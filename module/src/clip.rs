// SPDX-License-Identifier: GPL-3.0-or-later
//! d3-geo's generic clip of eas-geo-clip.el: the clip buffer, the rejoin
//! of cut rings along the clip edge, the spherical point-in-polygon test
//! and the antimeridian clip.  The small circle lives in clip_circle.rs,
//! the rectangle in clip_rect.rs.
//!
//! Every expression keeps the Elisp's operand order so the results are
//! bit-identical.

use crate::math::{asin, cartesian, cross, normalize, point_equal, rem, sign, EPS, EPS2, HALF_PI, PI, QUARTER_PI, TAU};
use crate::stream::{BoxStream, Stream};

/// A buffered point: X Y and its flag (0 for nil).
pub(crate) type P = (f64, f64, u8);

/// `eas-geo-clip-buffer': collects lines of points.
#[derive(Default)]
pub(crate) struct ClipBuffer {
    lines: Vec<Vec<P>>,
}

impl ClipBuffer {
    /// The buffer's `:rejoin': the last line takes the first's points.
    pub(crate) fn rejoin(&mut self) {
        if self.lines.len() > 1 {
            let first = self.lines.remove(0);
            self.lines.last_mut().unwrap().extend(first);
        }
    }

    /// The buffer's `:result': the lines in order, the buffer emptied.
    pub(crate) fn result(&mut self) -> Vec<Vec<P>> {
        std::mem::take(&mut self.lines)
    }
}

impl Stream for ClipBuffer {
    fn point(&mut self, x: f64, y: f64, m: u8) {
        // The Elisp signals on a point outside a line; drop it.
        if let Some(line) = self.lines.last_mut() { line.push((x, y, m)) }
    }
    fn line_start(&mut self) { self.lines.push(Vec::new()) }
    fn line_end(&mut self) {}
    fn polygon_start(&mut self) {}
    fn polygon_end(&mut self) {}
    fn sphere(&mut self) {}
}

/// An edge walk: FROM TO (None for the whole edge), DIRECTION, SINK.
pub(crate) type Interp<'a> = dyn FnMut(Option<(f64, f64)>, Option<(f64, f64)>, f64, &mut dyn Stream) + 'a;

/// A clip intersection (`eas-geo--ix'): point X, segment Z (an index
/// into the segments, None on the clip edge), other O, entry E, then
/// visited, next and previous (indices into the nodes).
struct Ix {
    x: P,
    z: Option<usize>,
    o: usize,
    e: bool,
    visited: bool,
    next: usize,
    prev: usize,
}

/// Stable merge sort of V under the strict predicate LT, as Emacs's
/// stable `sort' with `(< (compare a b) 0)'.  For a consistent order
/// every stable sort agrees; a NaN can make the order inconsistent, and
/// then the result may differ from Emacs's timsort (never a panic).
fn stable_sort(v: &mut Vec<usize>, lt: &dyn Fn(usize, usize) -> bool) {
    if v.len() < 2 { return }
    let mut right = v.split_off(v.len() / 2);
    stable_sort(v, lt);
    stable_sort(&mut right, lt);
    let left = std::mem::take(v);
    let (mut i, mut j) = (0, 0);
    while i < left.len() && j < right.len() {
        if lt(right[j], left[i]) { v.push(right[j]); j += 1 } else { v.push(left[i]); i += 1 }
    }
    v.extend_from_slice(&left[i..]);
    v.extend_from_slice(&right[j..]);
}

/// `eas-geo--link': RING's nodes into a ring.
fn link(nodes: &mut [Ix], ring: &[usize]) {
    for i in 0..ring.len() {
        let (a, b) = (ring[i], ring[(i + 1) % ring.len()]);
        nodes[a].next = b;
        nodes[b].prev = a;
    }
}

/// `eas-geo-clip-rejoin' of SEGMENTS into SINK.  COMPARE orders the
/// intersections along the clip edge, START-INSIDE says whether the clip
/// region's start lies inside and INTERPOLATE walks the clip edge.
pub(crate) fn rejoin(mut segments: Vec<Vec<P>>, compare: &dyn Fn(&P, &P) -> f64, mut start_inside: bool,
                     interpolate: &mut Interp, sink: &mut dyn Stream) {
    let (mut nodes, mut subject, mut clip) = (Vec::<Ix>::new(), Vec::new(), Vec::new());
    // A subject node at X on segment Z (entry E) and its clip twin.
    let mut pair = |nodes: &mut Vec<Ix>, x: P, z: usize, e: bool| {
        let i = nodes.len();
        nodes.push(Ix { x, z: Some(z), o: i + 1, e, visited: false, next: 0, prev: 0 });
        nodes.push(Ix { x, z: None, o: i, e: !e, visited: false, next: 0, prev: 0 });
        subject.push(i);
        clip.push(i + 1);
    };
    for si in 0..segments.len() {
        if segments[si].len() < 2 { continue }
        let n = segments[si].len() - 1;
        let (p0, p1) = (segments[si][0], segments[si][n]);
        if point_equal((p0.0, p0.1), (p1.0, p1.1)) && p0.2 == 0 && p1.2 == 0 {
            sink.line_start();
            for q in &segments[si][..n] {
                sink.point(q.0, q.1, 0);
            }
            sink.line_end();
            continue;
        }
        if point_equal((p0.0, p0.1), (p1.0, p1.1)) {
            // handle degenerate cases by moving the point
            segments[si][n].0 += 2.0 * EPS;
        }
        pair(&mut nodes, p0, si, true);
        pair(&mut nodes, segments[si][n], si, false);
    }
    if subject.is_empty() { return }
    let nr = &nodes;
    stable_sort(&mut clip, &|a, b| compare(&nr[a].x, &nr[b].x) < 0.0);
    link(&mut nodes, &subject);
    link(&mut nodes, &clip);
    for &c in &clip {
        start_inside = !start_inside;
        nodes[c].e = start_inside;
    }
    let start = subject[0];
    let mut done = false;
    let xy = |p: P| Some((p.0, p.1));
    while !done {
        let mut current = start;
        let mut subj = true;
        while !done && nodes[current].visited {
            current = nodes[current].next;
            done = current == start;
        }
        if done { break }
        let mut points = nodes[current].z;
        sink.line_start();
        loop {
            nodes[current].visited = true;
            let o = nodes[current].o;
            nodes[o].visited = true;
            let (nx, pv) = (nodes[current].next, nodes[current].prev);
            if nodes[current].e {
                if !subj {
                    interpolate(xy(nodes[current].x), xy(nodes[nx].x), 1.0, sink);
                } else if let Some(si) = points {
                    for q in &segments[si] {
                        sink.point(q.0, q.1, 0);
                    }
                }
                current = nx;
            } else {
                if !subj {
                    interpolate(xy(nodes[current].x), xy(nodes[pv].x), -1.0, sink);
                } else if let Some(si) = nodes[pv].z {
                    for q in segments[si].iter().rev() {
                        sink.point(q.0, q.1, 0);
                    }
                }
                current = pv;
            }
            current = nodes[current].o;
            points = nodes[current].z;
            subj = !subj;
            if nodes[current].visited { break }
        }
        sink.line_end();
    }
}

/// `eas-geo--longitude'.
#[inline]
fn longitude(l: f64) -> f64 {
    if l.abs() <= PI { l } else { sign(l) * (rem(l.abs() + PI, TAU) - PI) }
}

/// `eas-geo-polygon-contains': non-nil if POINT is inside the spherical
/// POLYGON (rings of (L P), radians).
pub fn polygon_contains(polygon: &[Vec<(f64, f64)>], point: (f64, f64)) -> bool {
    let (lam, mut phi) = (longitude(point.0), point.1);
    let sin_phi = phi.sin();
    let normal = [lam.sin(), -(lam.cos()), 0.0];
    let (mut angle, mut winding, mut sum) = (0.0, 0i64, 0.0);
    if sin_phi == 1.0 {
        phi = HALF_PI + EPS;
    } else if sin_phi == -1.0 {
        phi = (-HALF_PI) - EPS;
    }
    for ring in polygon {
        let m = ring.len();
        if m == 0 { continue }
        let mut p0 = ring[m - 1];
        let mut l0 = longitude(p0.0);
        let ph0 = p0.1 / 2.0 + QUARTER_PI;
        let (mut s0, mut c0) = (ph0.sin(), ph0.cos());
        for &p1 in ring.iter() {
            let l1 = longitude(p1.0);
            let ph1 = p1.1 / 2.0 + QUARTER_PI;
            let (s1, c1) = (ph1.sin(), ph1.cos());
            let delta = l1 - l0;
            let sgn = if delta >= 0.0 { 1.0 } else { -1.0 };
            let abs_delta = sgn * delta;
            let anti = abs_delta > PI;
            let k = s0 * s1;
            sum += (k * sgn * abs_delta.sin()).atan2(c0 * c1 + k * abs_delta.cos());
            angle += if anti { delta + sgn * TAU } else { delta };
            if (anti != (l0 >= lam)) != (l1 >= lam) {
                let arc = normalize(cross(cartesian(p0.0, p0.1), cartesian(p1.0, p1.1)));
                let ix = normalize(cross(normal, arc));
                let flip = anti != (delta >= 0.0);
                let phi_arc = (if flip { -1.0 } else { 1.0 }) * asin(ix[2]);
                if phi > phi_arc || (phi == phi_arc && (arc[0] != 0.0 || arc[1] != 0.0)) {
                    winding += if flip { 1 } else { -1 };
                }
            }
            (l0, s0, c0, p0) = (l1, s1, c1, p1);
        }
    }
    (angle < -EPS || (angle < EPS && sum < -EPS2)) != ((winding & 1) == 1)
}

/// A line cutter (`eas-geo--antimeridian-line', the circle's clip-line)
/// writing into the sink it is handed on each call.
pub(crate) trait LineCutter {
    fn line_start(&mut self, s: &mut dyn Stream);
    fn point(&mut self, s: &mut dyn Stream, l: f64, p: f64);
    fn line_end(&mut self, s: &mut dyn Stream);
    fn clean(&self) -> i32;
}

/// What `eas-geo-clip' is made of: VISIBLE, CLIP-LINE, INTERPOLATE, START.
pub(crate) trait ClipSpec {
    type Cutter: LineCutter;
    fn visible(&self, l: f64, p: f64) -> bool;
    fn cutter(&self) -> Self::Cutter;
    fn interpolate(&self, from: Option<(f64, f64)>, to: Option<(f64, f64)>, direction: f64, sink: &mut dyn Stream);
    fn start(&self) -> (f64, f64);
}

/// Which of the Elisp's point, point-line and point-ring is the point.
#[derive(Clone, Copy, PartialEq)]
enum PointMode { Point, Line, Ring }

/// `eas-geo--compare-intersection'.
fn compare_intersection(a: &P, b: &P) -> f64 {
    (if a.0 < 0.0 { a.1 - HALF_PI - EPS } else { HALF_PI - a.1 })
        - (if b.0 < 0.0 { b.1 - HALF_PI - EPS } else { HALF_PI - b.1 })
}

/// The stream `eas-geo-clip' makes of SPEC into SINK.
/// POLYGON and RING hold their points in order; RING_LINES says the
/// line-start and line-end are ring-start and ring-end.
pub(crate) struct Clip<C: ClipSpec> {
    spec: C,
    sink: BoxStream,
    line: C::Cutter,
    ring_buffer: ClipBuffer,
    ring_sink: C::Cutter,
    started: bool,
    polygon: Vec<Vec<(f64, f64)>>,
    segments: Vec<Vec<P>>,
    ring: Vec<(f64, f64)>,
    point_mode: PointMode,
    ring_lines: bool,
}

impl<C: ClipSpec> Clip<C> {
    pub(crate) fn new(spec: C, sink: BoxStream) -> Self {
        let (line, ring_sink) = (spec.cutter(), spec.cutter());
        let (ring_buffer, polygon, segments, ring) = (ClipBuffer::default(), Vec::new(), Vec::new(), Vec::new());
        let (started, point_mode, ring_lines) = (false, PointMode::Point, false);
        Clip { spec, sink, line, ring_buffer, ring_sink, started, polygon, segments, ring, point_mode, ring_lines }
    }

    fn start_polygon(&mut self) {
        if !self.started { self.sink.polygon_start(); self.started = true }
    }

    fn ring_end(&mut self) {
        if let Some(&first) = self.ring.first() {
            self.ring_sink.point(&mut self.ring_buffer, first.0, first.1);
        }
        self.ring_sink.line_end(&mut self.ring_buffer);
        let clean = self.ring_sink.clean();
        let mut segs = self.ring_buffer.result();
        let n = segs.len();
        self.polygon.push(std::mem::take(&mut self.ring));
        if n == 0 { return }
        if clean & 1 == 1 {
            let seg = &segs[0];
            if seg.len() > 1 {
                let m = seg.len() - 1;
                self.start_polygon();
                self.sink.line_start();
                for q in &seg[..m] {
                    self.sink.point(q.0, q.1, 0);
                }
                self.sink.line_end();
            }
        } else {
            if n > 1 && clean & 2 == 2 {
                let first = segs.remove(0);
                segs.last_mut().unwrap().extend(first);
            }
            self.segments.extend(segs.into_iter().filter(|sg| sg.len() > 1));
        }
    }
}

impl<C: ClipSpec> Stream for Clip<C> {
    fn point(&mut self, l: f64, p: f64, _m: u8) {
        match self.point_mode {
            PointMode::Point => if self.spec.visible(l, p) { self.sink.point(l, p, 0) },
            PointMode::Line => self.line.point(&mut *self.sink, l, p),
            PointMode::Ring => {
                self.ring.push((l, p));
                self.ring_sink.point(&mut self.ring_buffer, l, p);
            }
        }
    }
    fn line_start(&mut self) {
        if self.ring_lines {
            self.ring_sink.line_start(&mut self.ring_buffer);
            self.ring.clear();
        } else {
            self.point_mode = PointMode::Line;
            self.line.line_start(&mut *self.sink);
        }
    }
    fn line_end(&mut self) {
        if self.ring_lines {
            self.ring_end();
        } else {
            self.point_mode = PointMode::Point;
            self.line.line_end(&mut *self.sink);
        }
    }
    fn polygon_start(&mut self) {
        self.point_mode = PointMode::Ring;
        self.ring_lines = true;
        self.segments.clear();
        self.polygon.clear();
    }
    fn polygon_end(&mut self) {
        self.point_mode = PointMode::Point;
        self.ring_lines = false;
        let segs = std::mem::take(&mut self.segments);
        let inside = polygon_contains(&self.polygon, self.spec.start());
        if !segs.is_empty() {
            self.start_polygon();
            let spec = &self.spec;
            let mut interp = |f: Option<(f64, f64)>, t: Option<(f64, f64)>, d: f64, s: &mut dyn Stream| {
                spec.interpolate(f, t, d, s)
            };
            rejoin(segs, &compare_intersection, inside, &mut interp, &mut *self.sink);
        } else if inside {
            self.start_polygon();
            self.sink.line_start();
            self.spec.interpolate(None, None, 1.0, &mut *self.sink);
            self.sink.line_end();
        }
        if self.started { self.sink.polygon_end(); self.started = false }
        self.segments.clear();
        self.polygon.clear();
    }
    fn sphere(&mut self) {
        self.sink.polygon_start();
        self.sink.line_start();
        self.spec.interpolate(None, None, 1.0, &mut *self.sink);
        self.sink.line_end();
        self.sink.polygon_end();
    }
}

// Antimeridian (d3 clip/antimeridian.js)

/// `eas-geo--antimeridian-intersect'.
fn antimeridian_intersect(l0: f64, p0: f64, l1: f64, p1: f64) -> f64 {
    let s = (l0 - l1).sin();
    if s.abs() > EPS {
        let (c0, c1) = (p0.cos(), p1.cos());
        ((p0.sin() * c1 * l1.sin() - p1.sin() * c0 * l0.sin()) / (c0 * c1 * s)).atan()
    } else {
        (p0 + p1) / 2.0
    }
}

/// `eas-geo--antimeridian-line'.
pub(crate) struct AntimeridianLine {
    l0: f64,
    p0: f64,
    sign0: f64,
    clean: i32,
}

impl LineCutter for AntimeridianLine {
    fn line_start(&mut self, s: &mut dyn Stream) {
        s.line_start();
        self.clean = 1;
    }
    fn point(&mut self, s: &mut dyn Stream, l1: f64, p1: f64) {
        let mut l1 = l1;
        let sign1 = if l1 > 0.0 { PI } else { -PI };
        let delta = (l1 - self.l0).abs();
        if (delta - PI).abs() < EPS {
            // crosses a pole
            self.p0 = if (self.p0 + p1) / 2.0 > 0.0 { HALF_PI } else { -HALF_PI };
            s.point(self.l0, self.p0, 0);
            s.point(self.sign0, self.p0, 0);
            s.line_end();
            s.line_start();
            s.point(sign1, self.p0, 0);
            s.point(l1, self.p0, 0);
            self.clean = 0;
        } else if self.sign0.to_bits() != sign1.to_bits() && delta >= PI {
            // crosses the antimeridian
            if (self.l0 - self.sign0).abs() < EPS {
                self.l0 -= self.sign0 * EPS;
            }
            if (l1 - sign1).abs() < EPS {
                l1 -= sign1 * EPS;
            }
            self.p0 = antimeridian_intersect(self.l0, self.p0, l1, p1);
            s.point(self.sign0, self.p0, 0);
            s.line_end();
            s.line_start();
            s.point(sign1, self.p0, 0);
            self.clean = 0;
        }
        (self.l0, self.p0) = (l1, p1);
        s.point(l1, p1, 0);
        self.sign0 = sign1;
    }
    fn line_end(&mut self, s: &mut dyn Stream) {
        s.line_end();
        (self.l0, self.p0) = (f64::NAN, f64::NAN);
    }
    fn clean(&self) -> i32 { 2 - self.clean }
}

struct Antimeridian;

impl ClipSpec for Antimeridian {
    type Cutter = AntimeridianLine;
    fn visible(&self, _l: f64, _p: f64) -> bool { true }
    fn cutter(&self) -> AntimeridianLine {
        AntimeridianLine { l0: f64::NAN, p0: f64::NAN, sign0: f64::NAN, clean: 1 }
    }
    fn interpolate(&self, from: Option<(f64, f64)>, to: Option<(f64, f64)>, direction: f64, s: &mut dyn Stream) {
        match (from, to) {
            (Some(from), Some(to)) => {
                if (from.0 - to.0).abs() > EPS {
                    let lam = if from.0 < to.0 { PI } else { -PI };
                    let phi = direction * lam / 2.0;
                    s.point(-lam, phi, 0);
                    s.point(0.0, phi, 0);
                    s.point(lam, phi, 0);
                } else {
                    s.point(to.0, to.1, 0);
                }
            }
            _ => {
                let (phi, pi) = (direction * HALF_PI, PI);
                for (x, y) in [(-pi, phi), (0.0, phi), (pi, phi), (pi, 0.0), (pi, -phi), (0.0, -phi),
                               (-pi, -phi), (-pi, 0.0), (-pi, phi)] {
                    s.point(x, y, 0);
                }
            }
        }
    }
    fn start(&self) -> (f64, f64) { (-PI, -HALF_PI) }
}

/// `eas-geo-clip-antimeridian' into SINK.
pub fn clip_antimeridian(sink: BoxStream) -> BoxStream {
    Box::new(Clip::new(Antimeridian, sink))
}

#[cfg(test)]
#[path = "clip_tests.rs"]
mod tests;
