// SPDX-License-Identifier: GPL-3.0-or-later
//! Adaptive resampling, rotation and recorded streams (eas-geo-stream.el).

use crate::math::{asin, cartesian, Rotation, EPS, RAD};
use crate::stream::{BoxStream, Stream};
use std::sync::Arc;

/// A projection's forward function at its scale and translate.
pub trait Project {
    fn project(&self, l: f64, p: f64) -> (f64, f64);
}

pub type ProjectRc = Arc<dyn Project + Send + Sync>;

const MAX_DEPTH: i32 = 16;

fn cos_min_distance() -> f64 {
    (30.0 * RAD).cos()
}

/// `eas-geo--resample-far'.
#[inline]
fn far(delta4: f64, depth: i32, x0: f64, y0: f64, x1: f64, y1: f64) -> bool {
    depth > 0 && {
        let dx = x1 - x0;
        let dy = y1 - y0;
        dx * dx + dy * dy > delta4
    }
}

/// A segment end: projected x y, longitude and unit vector.
#[derive(Clone, Copy)]
struct End {
    x: f64,
    y: f64,
    l: f64,
    a: f64,
    b: f64,
    c: f64,
}

/// `eas-geo--resample-line'.
#[allow(clippy::too_many_arguments)]
fn resample_line(
    project: &dyn Project,
    delta2: f64,
    cmin: f64,
    e0: End,
    e1: End,
    depth: i32,
    sink: &mut dyn Stream,
) {
    let dx = e1.x - e0.x;
    let dy = e1.y - e0.y;
    let d2 = dx * dx + dy * dy;
    if d2 > 4.0 * delta2 && depth > 0 {
        let depth = depth - 1;
        let a = e0.a + e1.a;
        let b = e0.b + e1.b;
        let c = e0.c + e1.c;
        let m = (a * a + b * b + c * c).sqrt();
        let c = c / m;
        let p2 = asin(c);
        let l2 = if ((c.abs() - 1.0).abs() < EPS) || ((e0.l - e1.l).abs() < EPS) {
            (e0.l + e1.l) / 2.0
        } else {
            b.atan2(a)
        };
        let (x2, y2) = project.project(l2, p2);
        let dx2 = x2 - e0.x;
        let dy2 = y2 - e0.y;
        let dz = dy * dx2 - dx * dy2;
        if dz * dz / d2 > delta2
            || ((dx * dx2 + dy * dy2) / d2 - 0.5).abs() > 0.3
            || e0.a * e1.a + e0.b * e1.b + e0.c * e1.c < cmin
        {
            let a = a / m;
            let b = b / m;
            let delta4 = 4.0 * delta2;
            let e2 = End { x: x2, y: y2, l: l2, a, b, c };
            if far(delta4, depth, e0.x, e0.y, x2, y2) {
                resample_line(project, delta2, cmin, e0, e2, depth, sink);
            }
            sink.point(x2, y2, 0);
            if far(delta4, depth, x2, y2, e1.x, e1.y) {
                resample_line(project, delta2, cmin, e2, e1, depth, sink);
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum PointMode {
    Plain,
    Line,
    RingFirst,
}

/// `eas-geo--resample-stream' (DELTA2 > 0), or the plain projecting
/// transformer of `eas-geo-resample' when DELTA2 is not positive.
pub struct Resample {
    project: ProjectRc,
    delta2: f64,
    cmin: f64,
    sink: BoxStream,
    plain_only: bool,
    e00: End,
    e0: End,
    point_mode: PointMode,
    ring_start: bool,
    ring_end: bool,
}

impl Resample {
    pub fn new(project: ProjectRc, delta2: f64, sink: BoxStream) -> Resample {
        let zero = End { x: 0.0, y: 0.0, l: 0.0, a: 0.0, b: 0.0, c: 0.0 };
        Resample {
            project,
            delta2,
            cmin: cos_min_distance(),
            sink,
            plain_only: !(delta2 > 0.0),
            e00: zero,
            e0: End { x: f64::NAN, y: f64::NAN, ..zero },
            point_mode: PointMode::Plain,
            ring_start: false,
            ring_end: false,
        }
    }

    fn line_point(&mut self, l: f64, p: f64) {
        let c = cartesian(l, p);
        let (x, y) = self.project.project(l, p);
        let e1 = End { x, y, l, a: c[0], b: c[1], c: c[2] };
        resample_line(&*self.project, self.delta2, self.cmin, self.e0, e1, MAX_DEPTH, &mut *self.sink);
        self.e0 = e1;
        self.sink.point(x, y, 0);
    }

    fn plain_line_start(&mut self) {
        self.e0.x = f64::NAN;
        self.point_mode = PointMode::Line;
        self.sink.line_start();
    }

    fn plain_line_end(&mut self) {
        self.point_mode = PointMode::Plain;
        self.sink.line_end();
    }
}

impl Stream for Resample {
    fn point(&mut self, x: f64, y: f64, _m: u8) {
        if self.plain_only {
            let (px, py) = self.project.project(x, y);
            self.sink.point(px, py, 0);
            return;
        }
        match self.point_mode {
            PointMode::Plain => {
                let (px, py) = self.project.project(x, y);
                self.sink.point(px, py, 0);
            }
            PointMode::Line => self.line_point(x, y),
            PointMode::RingFirst => {
                let l00 = x;
                self.line_point(x, y);
                self.e00 = End { l: l00, ..self.e0 };
                self.point_mode = PointMode::Line;
            }
        }
    }
    fn line_start(&mut self) {
        if self.plain_only {
            return self.sink.line_start();
        }
        if self.ring_start {
            self.plain_line_start();
            self.point_mode = PointMode::RingFirst;
            self.ring_end = true;
        } else {
            self.plain_line_start();
        }
    }
    fn line_end(&mut self) {
        if self.plain_only {
            return self.sink.line_end();
        }
        if self.ring_end {
            resample_line(&*self.project, self.delta2, self.cmin, self.e0, self.e00, MAX_DEPTH, &mut *self.sink);
            self.ring_end = false;
        }
        self.plain_line_end();
    }
    fn polygon_start(&mut self) {
        self.sink.polygon_start();
        if !self.plain_only {
            self.ring_start = true;
        }
    }
    fn polygon_end(&mut self) {
        self.sink.polygon_end();
        if !self.plain_only {
            self.ring_start = false;
        }
    }
    fn sphere(&mut self) {
        self.sink.sphere();
    }
}

/// `eas-geo-radians-rotate': degrees to radians, then ROTATION.
pub struct RadiansRotate {
    pub rotation: Rotation,
    pub sink: BoxStream,
}

impl Stream for RadiansRotate {
    fn point(&mut self, x: f64, y: f64, _m: u8) {
        let (l, p) = self.rotation.forward(x * RAD, y * RAD);
        self.sink.point(l, p, 0);
    }
    fn line_start(&mut self) {
        self.sink.line_start()
    }
    fn line_end(&mut self) {
        self.sink.line_end()
    }
    fn polygon_start(&mut self) {
        self.sink.polygon_start()
    }
    fn polygon_end(&mut self) {
        self.sink.polygon_end()
    }
    fn sphere(&mut self) {
        self.sink.sphere()
    }
}

/// A recorded event (`eas-geo-recorder').
pub enum Event {
    PolygonStart,
    PolygonEnd,
    Sphere,
    Point(f64, f64),
    /// [L P A B C] per point.
    Line(Vec<[f64; 5]>),
}

/// `eas-geo-recorder'.
#[derive(Default)]
pub struct Recorder {
    events: Vec<Event>,
    line: Vec<(f64, f64)>,
    open: bool,
}

impl Recorder {
    /// The events, or None when a line was left open.
    pub fn result(self) -> Option<Vec<Event>> {
        if self.open {
            None
        } else {
            Some(self.events)
        }
    }
}

impl Stream for Recorder {
    fn point(&mut self, l: f64, p: f64, _m: u8) {
        if self.open {
            self.line.push((l, p));
        } else {
            self.events.push(Event::Point(l, p));
        }
    }
    fn line_start(&mut self) {
        self.open = true;
        self.line.clear();
    }
    fn line_end(&mut self) {
        let flat = self
            .line
            .iter()
            .map(|&(l, p)| {
                let c = p.cos();
                [l, p, c * l.cos(), c * l.sin(), p.sin()]
            })
            .collect();
        self.events.push(Event::Line(flat));
        self.open = false;
        self.line.clear();
    }
    fn polygon_start(&mut self) {
        self.events.push(Event::PolygonStart);
    }
    fn polygon_end(&mut self) {
        self.events.push(Event::PolygonEnd);
    }
    fn sphere(&mut self) {
        self.events.push(Event::Sphere);
    }
}

/// `eas-geo-replay': EVENTS through d3's resampling of PROJECT into SINK.
pub fn replay(events: &[Event], project: &dyn Project, delta2: f64, sink: &mut dyn Stream) {
    let cmin = cos_min_distance();
    let zero = End { x: 0.0, y: 0.0, l: 0.0, a: 0.0, b: 0.0, c: 0.0 };
    let mut e00 = zero;
    let mut e0 = End { x: f64::NAN, y: f64::NAN, ..zero };
    let mut ring = false;
    let delta4 = 4.0 * delta2;
    let depth = MAX_DEPTH;
    for ev in events {
        match ev {
            Event::Line(flat) => {
                e0.x = f64::NAN;
                sink.line_start();
                for (i, pt) in flat.iter().enumerate() {
                    let (x, y) = project.project(pt[0], pt[1]);
                    let e1 = End { x, y, l: pt[0], a: pt[2], b: pt[3], c: pt[4] };
                    if far(delta4, depth, e0.x, e0.y, x, y) {
                        resample_line(project, delta2, cmin, e0, e1, depth, sink);
                    }
                    e0 = e1;
                    sink.point(x, y, 0);
                    if ring && i == 0 {
                        e00 = e0;
                    }
                }
                if ring && far(delta4, depth, e0.x, e0.y, e00.x, e00.y) {
                    resample_line(project, delta2, cmin, e0, e00, depth, sink);
                }
                sink.line_end();
            }
            Event::Point(l, p) => {
                let (x, y) = project.project(*l, *p);
                sink.point(x, y, 0);
            }
            Event::PolygonStart => {
                sink.polygon_start();
                ring = true;
            }
            Event::PolygonEnd => {
                sink.polygon_end();
                ring = false;
            }
            Event::Sphere => sink.sphere(),
        }
    }
}
