// SPDX-License-Identifier: GPL-3.0-or-later
//! GeoJSON geometries and `eas-geo-stream-geometry'.

use crate::resample::{Event, Recorder};
use crate::stream::{Shared, Stream};
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

pub type Ring = Vec<(f64, f64)>;

pub enum Geom {
    Empty,
    Sphere,
    Point(f64, f64),
    MultiPoint(Ring),
    LineString(Ring),
    MultiLineString(Vec<Ring>),
    Polygon(Vec<Ring>),
    MultiPolygon(Vec<Vec<Ring>>),
    Collection(Vec<Arc<Node>>),
}

/// What a recording depends on besides the geometry: the rotation (three
/// angles, degrees) and the clip angle (`eas-geo-proj--simple's :front-key).
#[derive(Clone, Copy)]
pub struct FrontKey(pub [f64; 3], pub Option<f64>);

impl PartialEq for FrontKey {
    fn eq(&self, o: &Self) -> bool {
        self.0.iter().zip(o.0.iter()).all(|(a, b)| a.to_bits() == b.to_bits())
            && self.1.map(f64::to_bits) == o.1.map(f64::to_bits)
    }
}

type Recorded = Option<Arc<Vec<Event>>>;

/// A geometry and its recorded spherical streams (`eas-geo-proj--recorded').
pub struct Node {
    pub geom: Geom,
    pub sphere_free: bool,
    records: Mutex<Vec<(FrontKey, Recorded)>>,
}

impl Node {
    pub fn new(geom: Geom) -> Node {
        let sphere_free = match &geom {
            Geom::Sphere => false,
            Geom::Collection(c) => c.iter().all(|n| n.sphere_free),
            _ => true,
        };
        Node { geom, sphere_free, records: Mutex::new(Vec::new()) }
    }

    /// The events this geometry sends through FRONT (rotation and
    /// preclip) into a recorder, made once per KEY.
    pub fn recorded(&self, key: FrontKey, front: impl FnOnce(Shared<Recorder>) -> crate::stream::BoxStream) -> Recorded {
        if let Some((_, r)) = self.records.lock().unwrap().iter().find(|(k, _)| *k == key) {
            return r.clone();
        }
        let rec = Shared(Rc::new(RefCell::new(Recorder::default())));
        {
            let mut s = front(rec.clone());
            stream_geometry(&self.geom, &mut *s);
        }
        let events = Rc::try_unwrap(rec.0).ok().and_then(|r| r.into_inner().result()).map(Arc::new);
        self.records.lock().unwrap().push((key, events.clone()));
        events
    }
}

/// `eas-geo--line'.
pub fn stream_line(coords: &[(f64, f64)], s: &mut dyn Stream, closed: bool) {
    let n = coords.len().saturating_sub(if closed { 1 } else { 0 });
    s.line_start();
    for &(x, y) in &coords[..n] {
        s.point(x, y, 0);
    }
    s.line_end();
}

/// `eas-geo--polygon'.
pub fn stream_polygon(rings: &[Ring], s: &mut dyn Stream) {
    s.polygon_start();
    for r in rings {
        stream_line(r, s, true);
    }
    s.polygon_end();
}

/// `eas-geo-stream-geometry'.
pub fn stream_geometry(g: &Geom, s: &mut dyn Stream) {
    match g {
        Geom::Empty => {}
        Geom::Sphere => s.sphere(),
        Geom::Point(x, y) => s.point(*x, *y, 0),
        Geom::MultiPoint(ps) => {
            for &(x, y) in ps {
                s.point(x, y, 0);
            }
        }
        Geom::LineString(c) => stream_line(c, s, false),
        Geom::MultiLineString(ls) => {
            for c in ls {
                stream_line(c, s, false);
            }
        }
        Geom::Polygon(rs) => stream_polygon(rs, s),
        Geom::MultiPolygon(ps) => {
            for p in ps {
                stream_polygon(p, s);
            }
        }
        Geom::Collection(c) => {
            for n in c {
                stream_geometry(&n.geom, s);
            }
        }
    }
}
