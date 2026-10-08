// SPDX-License-Identifier: GPL-3.0-or-later
//! d3-geo's small-circle clip (clip/circle.js, circle.js) of
//! eas-geo-clip.el: `eas-geo-clip-circle' and `eas-geo-circle-stream'.
//!
//! Every expression keeps the Elisp's operand order so the results are
//! bit-identical.

use crate::clip::{Clip, ClipSpec, LineCutter};
use crate::math::{acos, add3, cartesian, cross, dot, normalize, point_equal, rem, scale3, spherical, EPS, PI, RAD, TAU};
use crate::stream::{BoxStream, Stream};

/// `eas-geo--circle-radius': the signed angle of POINT relative to
/// [COS-RADIUS 0 0].
fn circle_radius(cos_radius: f64, point: (f64, f64)) -> f64 {
    let mut p = cartesian(point.0, point.1);
    p[0] -= cos_radius;
    let p = normalize(p);
    let r = acos(-p[1]);
    rem((if -p[2] < 0.0 { -r } else { r }) + TAU - EPS, TAU)
}

/// `eas-geo-circle-stream': the clip circle of RADIUS from T0 to T1 into
/// SINK; DELTA the angular step, DIRECTION its sign, T0 None for whole.
pub fn circle_stream(
    sink: &mut dyn Stream,
    radius: f64,
    delta: f64,
    direction: f64,
    t0: Option<(f64, f64)>,
    t1: Option<(f64, f64)>,
) {
    let cr = radius.cos();
    let sr = radius.sin();
    let step = direction * delta;
    let (t0, t1) = match (t0, t1) {
        (Some(a), Some(b)) => {
            let mut t0 = circle_radius(cr, a);
            let t1 = circle_radius(cr, b);
            if if direction > 0.0 { t0 < t1 } else { t0 > t1 } {
                t0 += direction * TAU;
            }
            (t0, t1)
        }
        _ => (radius + direction * TAU, radius - step / 2.0),
    };
    let mut tt = t0;
    while if direction > 0.0 { tt > t1 } else { tt < t1 } {
        let pt = spherical([cr, (-sr) * tt.cos(), (-sr) * tt.sin()]);
        sink.point(pt.0, pt.1, 0);
        tt -= step;
    }
}

/// The circle's parameters, shared by its visible, code, intersect and
/// clip-line.
#[derive(Clone, Copy)]
struct Circle {
    radius: f64,
    cr: f64,
    delta: f64,
    small: bool,
    not_hemisphere: bool,
}

impl Circle {
    fn visible(&self, l: f64, p: f64) -> bool {
        l.cos() * p.cos() > self.cr
    }

    fn code(&self, l: f64, p: f64) -> i32 {
        let r = if self.small { self.radius } else { PI - self.radius };
        let mut c = 0;
        if l < -r {
            c |= 1;
        } else if l > r {
            c |= 2;
        }
        if p < -r {
            c |= 4;
        } else if p > r {
            c |= 8;
        }
        c
    }

    /// The circle's intersect of A B; the TWO form returns both points.
    fn intersect(&self, a: (f64, f64), b: (f64, f64), two: bool) -> Inter {
        let cr = self.cr;
        let pa = cartesian(a.0, a.1);
        let pb = cartesian(b.0, b.1);
        let n1 = [1.0, 0.0, 0.0];
        let n2 = cross(pa, pb);
        let n2n2 = dot(n2, n2);
        let n1n2 = n2[0];
        let det = n2n2 - n1n2 * n1n2;
        if det == 0.0 {
            return if two { Inter::None } else { Inter::One(a) };
        }
        let c1 = cr * n2n2 / det;
        let c2 = (-cr) * n1n2 / det;
        let u = cross(n1, n2);
        let aa = add3(scale3(n1, c1), scale3(n2, c2));
        let w = dot(aa, u);
        let uu = dot(u, u);
        let t2 = w * w - uu * (dot(aa, aa) - 1.0);
        if t2 < 0.0 {
            return Inter::None;
        }
        let tt = t2.sqrt();
        let q = spherical(add3(scale3(u, (-w - tt) / uu), aa));
        if !two {
            return Inter::One(q);
        }
        let (mut l0, mut l1, mut p0, mut p1) = (a.0, b.0, a.1, b.1);
        if l1 < l0 {
            std::mem::swap(&mut l0, &mut l1);
        }
        let d = l1 - l0;
        let polar = (d - PI).abs() < EPS;
        let meridian = polar || d < EPS;
        if !polar && p1 < p0 {
            std::mem::swap(&mut p0, &mut p1);
        }
        let ok = if meridian {
            if polar {
                (p0 + p1 > 0.0) != (q.1 < if (q.0 - l0).abs() < EPS { p0 } else { p1 })
            } else {
                p0 <= q.1 && q.1 <= p1
            }
        } else {
            (d > PI) != (l0 <= q.0 && q.0 <= l1)
        };
        if ok {
            Inter::Two(q, spherical(add3(scale3(u, (-w + tt) / uu), aa)))
        } else {
            Inter::None
        }
    }
}

enum Inter {
    None,
    One((f64, f64)),
    Two((f64, f64), (f64, f64)),
}

/// The circle's clip-line.  V0 and V00 are Elisp t/nil, nil at first.
pub(crate) struct CircleLine {
    k: Circle,
    point0: Option<(f64, f64)>,
    c0: i32,
    v0: bool,
    v00: bool,
    clean: i32,
}

impl LineCutter for CircleLine {
    fn line_start(&mut self, _s: &mut dyn Stream) {
        self.v00 = false;
        self.v0 = false;
        self.clean = 1;
    }
    fn point(&mut self, s: &mut dyn Stream, l: f64, p: f64) {
        let k = self.k;
        let point1 = (l, p);
        let v = k.visible(l, p);
        let c = if k.small {
            if v {
                0
            } else {
                k.code(l, p)
            }
        } else if v {
            k.code(l + if l < 0.0 { PI } else { -PI }, p)
        } else {
            0
        };
        if self.point0.is_none() {
            self.v0 = v;
            self.v00 = v;
            if v {
                s.line_start();
            }
        }
        // The Elisp then flags point1 when (intersect point0 point1)
        // misses or meets an end; that flag is never read nor sent.
        if v != self.v0 {
            self.clean = 0;
            let point2 = if v {
                s.line_start();
                match k.intersect(point1, self.point0.unwrap_or(point1), false) {
                    Inter::One(q) => {
                        s.point(q.0, q.1, 0);
                        Some(q)
                    }
                    _ => None, // the Elisp signals here (d3 throws)
                }
            } else {
                let r = match k.intersect(self.point0.unwrap_or(point1), point1, false) {
                    Inter::One(q) => {
                        s.point(q.0, q.1, 2);
                        Some(q)
                    }
                    _ => None, // the Elisp signals here (d3 throws)
                };
                s.line_end();
                r
            };
            self.point0 = point2;
        } else if k.not_hemisphere && v != k.small {
            if let Some(point0) = self.point0 {
                if c & self.c0 == 0 {
                    if let Inter::Two(t0, t1) = k.intersect(point1, point0, true) {
                        self.clean = 0;
                        if k.small {
                            s.line_start();
                            s.point(t0.0, t0.1, 0);
                            s.point(t1.0, t1.1, 0);
                            s.line_end();
                        } else {
                            s.point(t1.0, t1.1, 0);
                            s.line_end();
                            s.line_start();
                            s.point(t0.0, t0.1, 3);
                        }
                    }
                }
            }
        }
        if v && self.point0.is_none_or(|p0| !point_equal(p0, point1)) {
            s.point(point1.0, point1.1, 0);
        }
        self.point0 = Some(point1);
        self.v0 = v;
        self.c0 = c;
    }
    fn line_end(&mut self, s: &mut dyn Stream) {
        if self.v0 {
            s.line_end();
        }
        self.point0 = None;
    }
    fn clean(&self) -> i32 {
        self.clean | if self.v00 && self.v0 { 2 } else { 0 }
    }
}

impl ClipSpec for Circle {
    type Cutter = CircleLine;
    fn visible(&self, l: f64, p: f64) -> bool {
        Circle::visible(self, l, p)
    }
    fn cutter(&self) -> CircleLine {
        CircleLine { k: *self, point0: None, c0: 0, v0: false, v00: false, clean: 1 }
    }
    fn interpolate(&self, from: Option<(f64, f64)>, to: Option<(f64, f64)>, direction: f64, s: &mut dyn Stream) {
        circle_stream(s, self.radius, self.delta, direction, from, to);
    }
    fn start(&self) -> (f64, f64) {
        if self.small {
            (0.0, -self.radius)
        } else {
            (-PI, self.radius - PI)
        }
    }
}

/// `eas-geo-clip-circle' of RADIUS (radians) into SINK.
pub fn clip_circle(radius: f64, sink: BoxStream) -> BoxStream {
    let cr = radius.cos();
    let k = Circle { radius, cr, delta: 2.0 * RAD, small: cr > 0.0, not_hemisphere: cr.abs() > EPS };
    Box::new(Clip::new(k, sink))
}
