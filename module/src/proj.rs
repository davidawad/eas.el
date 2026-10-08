// SPDX-License-Identifier: GPL-3.0-or-later
//! Projections as eas-geo-proj.el builds them: scale, translate, centre,
//! rotation, preclip, resampling and postclip; albersUsa; fitting.

use crate::clip::clip_antimeridian;
use crate::clip_circle::clip_circle;
use crate::clip_rect::clip_rectangle;
use crate::geom::{stream_geometry, stream_polygon, FrontKey, Node};
use crate::math::{lmax, lmin, rem, Rotation, EPS, PI, RAD};
use crate::raw::{self, RawFn};
use crate::resample::{replay, Project, ProjectRc, RadiansRotate, Resample};
use crate::sink::BoundsSink;
use crate::stream::{BoxStream, Shared, Stream};
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

/// The projection closure of `eas-geo-proj--simple'.
pub struct ProjectFn {
    raw: RawFn,
    alpha0: bool,
    dx: f64,
    dy: f64,
    ksx: f64,
    ksy: f64,
    ca: f64,
    sa: f64,
    sx: f64,
    sy: f64,
}

impl Project for ProjectFn {
    #[inline]
    fn project(&self, l: f64, p: f64) -> (f64, f64) {
        let (x, y) = (self.raw)(l, p);
        if self.alpha0 {
            (self.dx + self.ksx * x, self.dy - self.ksy * y)
        } else {
            let rx = self.sx * x;
            let ry = self.sy * y;
            ((self.ca * rx - self.sa * ry) + self.dx, (self.dy - self.sa * rx) - self.ca * ry)
        }
    }
}

/// The resolved arguments of `eas-geo-proj--simple'.
#[derive(Clone)]
pub struct Params {
    pub scale: f64,
    pub translate: (f64, f64),
    pub center: (f64, f64),
    pub rotate: [f64; 3],
    pub clip_angle: Option<f64>,
    pub clip_extent: Option<[f64; 4]>,
    /// 0 none, 1 mercator, 2 transverse.
    pub reclip: u8,
    pub precision: f64,
    pub reflect: (bool, bool),
    pub angle: f64,
}

pub enum Outline {
    Interrupt(Vec<(f64, f64)>),
    Berghaus,
    Armadillo,
    Butterfly,
}

/// `eas-geo-proj--simple'.
pub struct Simple {
    rotation: Rotation,
    pub front_key: FrontKey,
    pub project: ProjectRc,
    pub delta2: f64,
    circle: Option<f64>,
    extent: Option<[f64; 4]>,
}

impl Simple {
    pub fn new(raw: RawFn, a: &Params) -> Simple {
        let k = a.scale;
        let (x, y) = a.translate;
        let sx = if a.reflect.0 { -1.0 } else { 1.0 };
        let sy = if a.reflect.1 { -1.0 } else { 1.0 };
        let lam = rem(a.center.0, 360.0) * RAD;
        let phi = rem(a.center.1, 360.0) * RAD;
        let r = [rem(a.rotate[0], 360.0), rem(a.rotate[1], 360.0), rem(a.rotate[2], 360.0)];
        let rotation = Rotation::new(r[0] * RAD, r[1] * RAD, r[2] * RAD);
        let alpha = rem(a.angle, 360.0) * RAD;
        let ca = alpha.cos() * k;
        let sa = alpha.sin() * k;
        let alpha0 = alpha == 0.0;
        let (cx, cy) = {
            let (rx0, ry0) = raw(lam, phi);
            if alpha0 {
                (0.0 + k * sx * rx0, 0.0 - k * sy * ry0)
            } else {
                let rx = sx * rx0;
                let ry = sy * ry0;
                ((ca * rx - sa * ry) + 0.0, (0.0 - sa * rx) - ca * ry)
            }
        };
        let project = ProjectFn {
            raw,
            alpha0,
            dx: x - cx,
            dy: y - cy,
            ksx: k * sx,
            ksy: k * sy,
            ca,
            sa,
            sx,
            sy,
        };
        let extent = if a.reclip == 0 {
            a.clip_extent
        } else {
            let kk = PI * k;
            let tt = project.project(0.0, 0.0);
            Some(match (a.clip_extent, a.reclip) {
                (None, _) => [tt.0 - kk, tt.1 - kk, tt.0 + kk, tt.1 + kk],
                (Some(e), 1) => [lmax(tt.0 - kk, e[0]), e[1], lmin(tt.0 + kk, e[2]), e[3]],
                (Some(e), _) => [e[0], lmax(tt.1 - kk, e[1]), e[2], lmin(tt.1 + kk, e[3])],
            })
        };
        let circle = a.clip_angle.filter(|c| *c > 0.0);
        Simple {
            rotation,
            front_key: FrontKey(r, circle),
            project: Arc::new(project),
            delta2: a.precision * a.precision,
            circle,
            extent,
        }
    }

    fn preclip(&self, sink: BoxStream) -> BoxStream {
        match self.circle {
            Some(c) => clip_circle(c * RAD, sink),
            None => clip_antimeridian(sink),
        }
    }

    pub fn postclip(&self, sink: BoxStream) -> BoxStream {
        match self.extent {
            Some(e) => clip_rectangle(e[0], e[1], e[2], e[3], sink),
            None => sink,
        }
    }

    /// The :stream of the projection (no outline).
    pub fn pipeline(&self, sink: BoxStream) -> BoxStream {
        let resample = Box::new(Resample::new(self.project.clone(), self.delta2, self.postclip(sink)));
        Box::new(RadiansRotate { rotation: self.rotation, sink: self.preclip(resample) })
    }

    /// The :front half, recorded by `eas-geo-proj--events'.
    pub fn front(&self, sink: BoxStream) -> BoxStream {
        Box::new(RadiansRotate { rotation: self.rotation, sink: self.preclip(sink) })
    }
}

/// A projection: simple (with its outline and the unrotated projection
/// drawing it) or albersUsa's three insets.
pub enum Proj {
    Simple(Arc<Simple>, Option<(Arc<Outline>, Arc<Simple>)>),
    AlbersUsa(Vec<Simple>),
}

struct OutlineStream<S: Stream + 'static> {
    inner: BoxStream,
    outline: Arc<Outline>,
    flat: Arc<Simple>,
    sink: Shared<S>,
}

impl<S: Stream + 'static> Stream for OutlineStream<S> {
    fn point(&mut self, x: f64, y: f64, m: u8) {
        self.inner.point(x, y, m)
    }
    fn line_start(&mut self) {
        self.inner.line_start()
    }
    fn line_end(&mut self) {
        self.inner.line_end()
    }
    fn polygon_start(&mut self) {
        self.inner.polygon_start()
    }
    fn polygon_end(&mut self) {
        self.inner.polygon_end()
    }
    fn sphere(&mut self) {
        let mut s = self.flat.pipeline(Box::new(self.sink.clone()));
        match &*self.outline {
            Outline::Interrupt(ring) => stream_polygon(std::slice::from_ref(ring), &mut *s),
            Outline::Berghaus => crate::raw_extra::berghaus_sphere(&mut *s),
            Outline::Armadillo => crate::raw_extra::armadillo_sphere(&mut *s),
            Outline::Butterfly => crate::polyhedral::butterfly_sphere(&mut *s),
        }
    }
}

struct Multiplex(Vec<BoxStream>);

impl Stream for Multiplex {
    fn point(&mut self, x: f64, y: f64, m: u8) {
        self.0.iter_mut().for_each(|s| s.point(x, y, m))
    }
    fn line_start(&mut self) {
        self.0.iter_mut().for_each(|s| s.line_start())
    }
    fn line_end(&mut self) {
        self.0.iter_mut().for_each(|s| s.line_end())
    }
    fn polygon_start(&mut self) {
        self.0.iter_mut().for_each(|s| s.polygon_start())
    }
    fn polygon_end(&mut self) {
        self.0.iter_mut().for_each(|s| s.polygon_end())
    }
    fn sphere(&mut self) {
        self.0.iter_mut().for_each(|s| s.sphere())
    }
}

impl Proj {
    /// The projection TYPE with resolved parameters A (`eas-geo-proj').
    pub fn new(type_name: &str, parallels: (f64, f64), a: &Params) -> Option<Proj> {
        let raw = raw::make_raw(type_name, parallels)?;
        let main = Arc::new(Simple::new(raw, a));
        let outline = match type_name {
            "interruptedSinusoidal" | "interruptedMollweide" | "interruptedMollweideHemispheres" => {
                Some(Outline::Interrupt(raw::interrupt_sphere(type_name)?))
            }
            "berghaus" => Some(Outline::Berghaus),
            "armadillo" => Some(Outline::Armadillo),
            "polyhedralButterfly" => Some(Outline::Butterfly),
            _ => None,
        };
        let outline = match outline {
            None => None,
            Some(o) => {
                let flat = Params {
                    rotate: [0.0, 0.0, 0.0],
                    reclip: 0,
                    reflect: (false, false),
                    ..a.clone()
                };
                let raw = raw::make_raw(type_name, parallels)?;
                Some((Arc::new(o), Arc::new(Simple::new(raw, &flat))))
            }
        };
        Some(Proj::Simple(main, outline))
    }

    /// `eas-geo-proj--albers-usa' at scale K, translate X Y, PRECISION.
    pub fn albers_usa(k: f64, x: f64, y: f64, precision: f64) -> Proj {
        let e = EPS;
        let sub = |par: (f64, f64), rot: f64, center: (f64, f64), scale: f64, tx: f64, ty: f64, b: [f64; 4]| {
            let a = Params {
                scale,
                translate: (tx, ty),
                center,
                rotate: [rot, 0.0, 0.0],
                clip_angle: None,
                clip_extent: Some(b),
                reclip: 0,
                precision,
                reflect: (false, false),
                angle: 0.0,
            };
            Simple::new(raw::conic_equal_area(par.0 * RAD, par.1 * RAD), &a)
        };
        let lower = sub(
            (29.5, 45.5),
            96.0,
            (-0.6, 38.7),
            k,
            x,
            y,
            [x - 0.455 * k, y - 0.238 * k, x + 0.455 * k, y + 0.238 * k],
        );
        let alaska = sub(
            (55.0, 65.0),
            154.0,
            (-2.0, 58.5),
            0.35 * k,
            x - 0.307 * k,
            y + 0.201 * k,
            [(x - 0.425 * k) + e, (y + 0.120 * k) + e, (x - 0.214 * k) - e, (y + 0.234 * k) - e],
        );
        let hawaii = sub(
            (8.0, 18.0),
            157.0,
            (-3.0, 19.9),
            k,
            x - 0.205 * k,
            y + 0.212 * k,
            [(x - 0.214 * k) + e, (y + 0.166 * k) + e, (x - 0.115 * k) - e, (y + 0.234 * k) - e],
        );
        Proj::AlbersUsa(vec![lower, alaska, hawaii])
    }

    /// The projection's :stream into SINK.
    pub fn stream<S: Stream + 'static>(&self, sink: &Shared<S>) -> BoxStream {
        match self {
            Proj::Simple(main, None) => main.pipeline(Box::new(sink.clone())),
            Proj::Simple(main, Some((outline, flat))) => Box::new(OutlineStream {
                inner: main.pipeline(Box::new(sink.clone())),
                outline: outline.clone(),
                flat: flat.clone(),
                sink: sink.clone(),
            }),
            Proj::AlbersUsa(subs) => {
                Box::new(Multiplex(subs.iter().map(|s| s.pipeline(Box::new(sink.clone()))).collect()))
            }
        }
    }

    /// `eas-geo-proj-stream': NODE into SINK, replaying its recorded
    /// spherical stream when it has one.
    pub fn stream_node<S: Stream + 'static>(&self, node: &Node, sink: &Shared<S>) {
        if let Proj::Simple(main, _) = self {
            if main.delta2 > 0.0 && node.sphere_free {
                let events = node.recorded(main.front_key, |rec| main.front(Box::new(rec)));
                if let Some(ev) = events {
                    let mut post = main.postclip(Box::new(sink.clone()));
                    replay(&ev, &*main.project, main.delta2, &mut *post);
                    return;
                }
            }
        }
        let mut s = self.stream(sink);
        stream_geometry(&node.geom, &mut *s);
    }

    /// The bounds `eas-geo-proj-fit' measures of NODES.
    pub fn fit_bounds(&self, nodes: &[Arc<Node>]) -> [f64; 4] {
        let sink = Shared(Rc::new(RefCell::new(BoundsSink::default())));
        let mut shared: Option<BoxStream> = None;
        for n in nodes {
            if n.sphere_free {
                self.stream_node(n, &sink);
            } else {
                let s = shared.get_or_insert_with(|| self.stream(&sink));
                stream_geometry(&n.geom, &mut **s);
            }
        }
        drop(shared);
        let b = sink.0.borrow().0;
        b
    }
}

