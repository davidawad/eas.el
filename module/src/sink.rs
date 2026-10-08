// SPDX-License-Identifier: GPL-3.0-or-later
//! The path and bounds sinks and the planar measures (eas-geo-stream.el).

use crate::math::{lmax, lmin, pow};
use crate::stream::Stream;

/// `eas-geo--simplify-run'.
const SIMPLIFY_RUN: u32 = 6;

/// What `eas-geo-path-sink' collects.
#[derive(Default, Debug, Clone)]
pub struct PathResult {
    /// (CLOSED, FLAT-XY) in stream order.
    pub paths: Vec<(bool, Vec<f64>)>,
    /// [CX CY R].
    pub circles: Vec<[f64; 3]>,
    pub polygons: u32,
}

/// `eas-geo-path-sink'.
pub struct PathSink {
    pub out: PathResult,
    radius: f64,
    tol2: Option<f64>,
    polygon: bool,
    line: bool,
    pts: Vec<Vec<f64>>,
    a: Option<(f64, f64)>,
    b: Option<(f64, f64)>,
    run: u32,
    all: Vec<f64>,
    nall: u32,
}

impl PathSink {
    pub fn new(radius: f64, tolerance: Option<f64>) -> PathSink {
        PathSink {
            out: PathResult::default(),
            radius,
            tol2: tolerance.filter(|t| *t > 0.0).map(|t| t * t),
            polygon: false,
            line: false,
            pts: Vec::new(),
            a: None,
            b: None,
            run: 0,
            all: Vec::new(),
            nall: 0,
        }
    }

    fn top(&mut self) -> &mut Vec<f64> {
        if self.pts.is_empty() {
            self.pts.push(Vec::new());
        }
        self.pts.last_mut().unwrap()
    }
}

impl Stream for PathSink {
    fn point(&mut self, x: f64, y: f64, _m: u8) {
        if !self.line {
            self.out.circles.push([x, y, self.radius]);
            return;
        }
        let Some(tol2) = self.tol2 else {
            let t = self.top();
            t.push(x);
            t.push(y);
            return;
        };
        if self.nall < 16 {
            self.all.push(x);
            self.all.push(y);
            self.nall += 1;
        }
        match (self.a, self.b) {
            (None, _) => {
                let t = self.top();
                t.push(x);
                t.push(y);
                self.a = Some((x, y));
            }
            (Some(_), None) => self.b = Some((x, y)),
            (Some((ax, ay)), Some((bx, by))) => {
                let drop = self.run < SIMPLIFY_RUN && {
                    let dx = x - ax;
                    let dy = y - ay;
                    let ex = bx - ax;
                    let ey = by - ay;
                    let z = dx * ey - dy * ex;
                    let d2 = dx * dx + dy * dy;
                    if d2 > 0.0 {
                        z * z < tol2 * d2
                    } else {
                        ex * ex + ey * ey < tol2
                    }
                };
                if drop {
                    self.b = Some((x, y));
                    self.run += 1;
                } else {
                    let t = self.top();
                    t.push(bx);
                    t.push(by);
                    self.a = Some((bx, by));
                    self.b = Some((x, y));
                    self.run = 0;
                }
            }
        }
    }
    fn line_start(&mut self) {
        self.line = true;
        self.a = None;
        self.b = None;
        self.run = 0;
        self.all.clear();
        self.nall = 0;
        self.pts.push(Vec::new());
    }
    fn line_end(&mut self) {
        self.line = false;
        if let Some((bx, by)) = self.b {
            let t = self.top();
            t.push(bx);
            t.push(by);
        }
        let mut flat = self.pts.pop().unwrap_or_default();
        if self.tol2.is_some()
            && self.polygon
            && flat.len() < 8
            && self.nall < 16
            && self.all.len() > flat.len()
        {
            flat = self.all.clone();
        }
        self.all.clear();
        if !flat.is_empty() {
            self.out.paths.push((self.polygon, flat));
        }
    }
    fn polygon_start(&mut self) {
        self.polygon = true;
    }
    fn polygon_end(&mut self) {
        self.polygon = false;
        self.out.polygons += 1;
    }
    fn sphere(&mut self) {}
}

/// `eas-geo-bounds-sink'.
pub struct BoundsSink(pub [f64; 4]);

impl Default for BoundsSink {
    fn default() -> Self {
        BoundsSink([f64::INFINITY, f64::INFINITY, f64::NEG_INFINITY, f64::NEG_INFINITY])
    }
}

impl Stream for BoundsSink {
    fn point(&mut self, x: f64, y: f64, _m: u8) {
        let b = &mut self.0;
        if x < b[0] {
            b[0] = x;
        }
        if x > b[2] {
            b[2] = x;
        }
        if y < b[1] {
            b[1] = y;
        }
        if y > b[3] {
            b[3] = y;
        }
    }
    fn line_start(&mut self) {}
    fn line_end(&mut self) {}
    fn polygon_start(&mut self) {}
    fn polygon_end(&mut self) {}
    fn sphere(&mut self) {}
}

/// `eas-geo-ring-area'.
pub fn ring_area(flat: &[f64]) -> f64 {
    let m = flat.len();
    let mut sum = 0.0;
    if m > 1 {
        let (mut x0, mut y0) = (flat[0], flat[1]);
        let mut i = 0;
        while i < m {
            i += 2;
            let (x1, y1) = if i < m { (flat[i], flat[i + 1]) } else { (flat[0], flat[1]) };
            sum += x0 * y1 - y0 * x1;
            x0 = x1;
            y0 = y1;
        }
    }
    sum / 2.0
}

/// `eas-geo--ring-centroid' of PATHS.
fn ring_centroid(paths: &[&(bool, Vec<f64>)]) -> Option<(f64, f64)> {
    let (mut x2, mut y2, mut z2) = (0.0, 0.0, 0.0);
    for (closed, flat) in paths.iter().map(|p| (p.0, &p.1)) {
        if !closed {
            continue;
        }
        let m = flat.len();
        if m > 1 {
            let (mut ax, mut ay) = (flat[0], flat[1]);
            let mut i = 0;
            while i < m {
                i += 2;
                let (bx, by) = if i < m { (flat[i], flat[i + 1]) } else { (flat[0], flat[1]) };
                let z = ay * bx - ax * by;
                x2 += z * (ax + bx);
                y2 += z * (ay + by);
                z2 += 3.0 * z;
                ax = bx;
                ay = by;
            }
        }
    }
    if z2 != 0.0 {
        Some((x2 / z2, y2 / z2))
    } else {
        None
    }
}

/// `eas-geo--path-centroid-1'.
fn path_centroid_1(paths: &[&(bool, Vec<f64>)], circles: &[[f64; 3]]) -> Option<(f64, f64)> {
    let (mut x2, mut y2, mut z2) = (0.0, 0.0, 0.0);
    let (mut x1, mut y1, mut z1) = (0.0, 0.0, 0.0);
    let (mut x0, mut y0, mut z0) = (0.0, 0.0, 0i64);
    for (closed, flat) in paths.iter().map(|p| (p.0, &p.1)) {
        let n = flat.len() / 2;
        for i in 0..n {
            x0 += flat[2 * i];
            y0 += flat[2 * i + 1];
            z0 += 1;
        }
        let count = if closed { n as i64 } else { n as i64 - 1 };
        for i in 0..count.max(0) as usize {
            let j = (i + 1) % n;
            let (ax, ay) = (flat[2 * i], flat[2 * i + 1]);
            let (bx, by) = (flat[2 * j], flat[2 * j + 1]);
            let len = (pow(bx - ax, 2.0) + pow(by - ay, 2.0)).sqrt();
            let z = ay * bx - ax * by;
            x1 += len * ((ax + bx) / 2.0);
            y1 += len * ((ay + by) / 2.0);
            z1 += len;
            if closed {
                x2 += z * (ax + bx);
                y2 += z * (ay + by);
                z2 += 3.0 * z;
            }
        }
    }
    for c in circles {
        x0 += c[0];
        y0 += c[1];
        z0 += 1;
    }
    if z2 != 0.0 {
        Some((x2 / z2, y2 / z2))
    } else if z1 != 0.0 {
        Some((x1 / z1, y1 / z1))
    } else if z0 != 0 {
        Some((x0 / z0 as f64, y0 / z0 as f64))
    } else {
        None
    }
}

/// `eas-geo-path-centroid'.
pub fn path_centroid(paths: &[&(bool, Vec<f64>)], circles: &[[f64; 3]]) -> Option<(f64, f64)> {
    ring_centroid(paths).or_else(|| path_centroid_1(paths, circles))
}

/// `eas-geoshape--anchor': the largest ring's centroid, else the path's.
pub fn anchor(r: &PathResult) -> Option<(f64, f64)> {
    let mut best: Option<&(bool, Vec<f64>)> = None;
    let mut best_a = 0.0;
    for p in &r.paths {
        if p.0 {
            let a = ring_area(&p.1).abs();
            if a > best_a {
                best = Some(p);
                best_a = a;
            }
        }
    }
    match best {
        Some(b) => path_centroid(&[b], &[]),
        None => path_centroid(&r.paths.iter().collect::<Vec<_>>(), &r.circles),
    }
}

/// `eas-geoshape--relative': paths and circles relative to AX AY, and
/// the box [X0 Y0 X1 Y1] relative to it.
pub struct Relative {
    pub paths: Vec<(bool, Vec<f64>)>,
    pub circles: Vec<[f64; 3]>,
    pub bbox: [f64; 4],
}

pub fn relative(r: &PathResult, ax: f64, ay: f64) -> Relative {
    let (mut x0, mut y0, mut x1, mut y1) =
        (f64::INFINITY, f64::INFINITY, f64::NEG_INFINITY, f64::NEG_INFINITY);
    let mut see = |x: f64, y: f64| {
        x0 = lmin(x0, x);
        y0 = lmin(y0, y);
        x1 = lmax(x1, x);
        y1 = lmax(y1, y);
    };
    let paths = r
        .paths
        .iter()
        .map(|(closed, flat)| {
            let mut rel = vec![0.0; flat.len()];
            let mut i = 0;
            while i + 1 < flat.len() {
                see(flat[i], flat[i + 1]);
                rel[i] = flat[i] - ax;
                rel[i + 1] = flat[i + 1] - ay;
                i += 2;
            }
            (*closed, rel)
        })
        .collect();
    let circles = r
        .circles
        .iter()
        .map(|c| {
            let rr = c[2];
            see(c[0] - rr, c[1] - rr);
            see(c[0] + rr, c[1] + rr);
            [c[0] - ax, c[1] - ay, rr]
        })
        .collect();
    Relative { paths, circles, bbox: [x0 - ax, y0 - ay, x1 - ax, y1 - ay] }
}
