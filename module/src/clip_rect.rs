// SPDX-License-Identifier: GPL-3.0-or-later
//! d3-geo's rectangle clip (clip/rectangle.js, clip/line.js) of
//! eas-geo-clip.el: `eas-geo-clip-rectangle' and its Liang-Barsky
//! segment clip.
//!
//! Every expression keeps the Elisp's operand order so the results are
//! bit-identical.

use crate::clip::{rejoin, ClipBuffer, P};
use crate::math::{lmax, lmin, EPS};
use crate::stream::{BoxStream, Stream};

/// `eas-geo--clip-segment': clip A B to the box X0 Y0 X1 Y1 in place;
/// false when the segment lies outside.
fn clip_segment(a: &mut (f64, f64), b: &mut (f64, f64), x0: f64, y0: f64, x1: f64, y1: f64) -> bool {
    let ax = a.0;
    let ay = a.1;
    let dx = b.0 - ax;
    let dy = b.1 - ay;
    let mut t0 = 0.0;
    let mut t1 = 1.0;
    // LOWER: the edge bounds from below (x0/y0).  False means out.
    let mut edge = |r: f64, d: f64, lower: bool| -> bool {
        if d == 0.0 {
            !(if lower { r > 0.0 } else { r < 0.0 })
        } else {
            let r = r / d;
            if lower == (d < 0.0) {
                if r < t0 {
                    return false;
                }
                if r < t1 {
                    t1 = r;
                }
            } else {
                if r > t1 {
                    return false;
                }
                if r > t0 {
                    t0 = r;
                }
            }
            true
        }
    };
    if !(edge(x0 - ax, dx, true) && edge(x1 - ax, dx, false) && edge(y0 - ay, dy, true) && edge(y1 - ay, dy, false)) {
        return false;
    }
    if t0 > 0.0 {
        *a = (ax + t0 * dx, ay + t0 * dy);
    }
    if t1 < 1.0 {
        *b = (ax + t1 * dx, ay + t1 * dy);
    }
    true
}

/// The box and the functions on it.
#[derive(Clone, Copy)]
struct Rect {
    x0: f64,
    y0: f64,
    x1: f64,
    y1: f64,
}

impl Rect {
    fn visible(&self, x: f64, y: f64) -> bool {
        self.x0 <= x && x <= self.x1 && self.y0 <= y && y <= self.y1
    }

    fn corner(&self, p: (f64, f64), direction: f64) -> i32 {
        let pos = direction > 0.0;
        if (p.0 - self.x0).abs() < EPS {
            if pos {
                0
            } else {
                3
            }
        } else if (p.0 - self.x1).abs() < EPS {
            if pos {
                2
            } else {
                1
            }
        } else if (p.1 - self.y0).abs() < EPS {
            if pos {
                1
            } else {
                0
            }
        } else if pos {
            3
        } else {
            2
        }
    }

    fn compare_point(&self, a: (f64, f64), b: (f64, f64)) -> f64 {
        let ca = self.corner(a, 1.0);
        let cb = self.corner(b, 1.0);
        if ca != cb {
            (ca - cb) as f64
        } else if ca == 0 {
            b.1 - a.1
        } else if ca == 1 {
            a.0 - b.0
        } else if ca == 2 {
            a.1 - b.1
        } else {
            b.0 - a.0
        }
    }

    fn interpolate(&self, from: Option<(f64, f64)>, to: Option<(f64, f64)>, direction: f64, s: &mut dyn Stream) {
        let mut a = 0;
        let mut a1 = 0;
        let walk = match (from, to) {
            (Some(from), Some(to)) => {
                a = self.corner(from, direction);
                a1 = self.corner(to, direction);
                a != a1 || ((self.compare_point(from, to) < 0.0) != (direction > 0.0))
            }
            _ => true,
        };
        if walk {
            let dir = direction as i32;
            loop {
                s.point(if a == 0 || a == 3 { self.x0 } else { self.x1 }, if a > 1 { self.y1 } else { self.y0 }, 0);
                a = (a + dir + 4).rem_euclid(4);
                if a == a1 {
                    break;
                }
            }
        } else if let Some(to) = to {
            s.point(to.0, to.1, 0);
        }
    }
}

/// The stream `eas-geo-clip-rectangle' makes into SINK.
struct ClipRect {
    r: Rect,
    sink: BoxStream,
    buffer: ClipBuffer,
    to_buffer: bool,
    /// None is the Elisp's nil segments; Some its one list of segments.
    segments: Option<Vec<Vec<P>>>,
    /// The rings, each point in order.
    polygon: Vec<Vec<(f64, f64)>>,
    in_polygon: bool,
    x__: f64,
    y__: f64,
    v__: bool,
    x_: f64,
    y_: f64,
    v_: bool,
    first: bool,
    clean: bool,
    line_mode: bool,
}

/// `(max -1e9 (min 1e9 X))'.
#[inline]
fn clamp(x: f64) -> f64 {
    lmax(-1e9, lmin(1e9, x))
}

impl ClipRect {
    fn active(&mut self) -> &mut dyn Stream {
        if self.to_buffer {
            &mut self.buffer
        } else {
            &mut *self.sink
        }
    }

    fn polygon_inside(&self) -> i64 {
        let Rect { x0, y1, .. } = self.r;
        let mut winding = 0;
        for pts in &self.polygon {
            let m = pts.len();
            if m == 0 {
                continue;
            }
            let (mut b0, mut b1) = pts[0];
            for &(pb0, pb1) in &pts[1..] {
                let a0 = b0;
                let a1 = b1;
                b0 = pb0;
                b1 = pb1;
                if a1 <= y1 {
                    if b1 > y1 && (b0 - a0) * (y1 - a1) > (b1 - a1) * (x0 - a0) {
                        winding += 1;
                    }
                } else if b1 <= y1 && (b0 - a0) * (y1 - a1) < (b1 - a1) * (x0 - a0) {
                    winding -= 1;
                }
            }
        }
        winding
    }

    fn line_point(&mut self, x: f64, y: f64) {
        let (mut x, mut y) = (x, y);
        let v = self.r.visible(x, y);
        if self.in_polygon {
            if let Some(ring) = self.polygon.last_mut() {
                ring.push((x, y));
            }
        }
        if self.first {
            self.x__ = x;
            self.y__ = y;
            self.v__ = v;
            self.first = false;
            if v {
                let a = self.active();
                a.line_start();
                a.point(x, y, 0);
            }
        } else if v && self.v_ {
            self.active().point(x, y, 0);
        } else {
            let mut a = (clamp(self.x_), clamp(self.y_));
            let mut b = (clamp(x), clamp(y));
            self.x_ = a.0;
            self.y_ = a.1;
            x = b.0;
            y = b.1;
            let Rect { x0, y0, x1, y1 } = self.r;
            if clip_segment(&mut a, &mut b, x0, y0, x1, y1) {
                let v_ = self.v_;
                let s = self.active();
                if !v_ {
                    s.line_start();
                    s.point(a.0, a.1, 0);
                }
                s.point(b.0, b.1, 0);
                if !v {
                    s.line_end();
                }
                self.clean = false;
            } else if v {
                let s = self.active();
                s.line_start();
                s.point(x, y, 0);
                self.clean = false;
            }
        }
        self.x_ = x;
        self.y_ = y;
        self.v_ = v;
    }
}

impl Stream for ClipRect {
    fn point(&mut self, x: f64, y: f64, _m: u8) {
        if self.line_mode {
            self.line_point(x, y);
        } else if self.r.visible(x, y) {
            self.active().point(x, y, 0);
        }
    }
    fn line_start(&mut self) {
        self.line_mode = true;
        if self.in_polygon {
            self.polygon.push(Vec::new());
        }
        self.first = true;
        self.v_ = false;
        self.x_ = f64::NAN;
        self.y_ = f64::NAN;
    }
    fn line_end(&mut self) {
        if self.segments.is_some() {
            self.line_point(self.x__, self.y__);
            if self.v__ && self.v_ {
                self.buffer.rejoin();
            }
            let res = self.buffer.result();
            if let Some(segs) = self.segments.as_mut() {
                segs.extend(res);
            }
        }
        self.line_mode = false;
        if self.v_ {
            self.active().line_end();
        }
    }
    fn polygon_start(&mut self) {
        self.to_buffer = true;
        self.segments = Some(Vec::new());
        self.polygon.clear();
        self.in_polygon = true;
        self.clean = true;
    }
    fn polygon_end(&mut self) {
        let start_inside = self.polygon_inside() != 0;
        let clean_inside = self.clean && start_inside;
        let segs = self.segments.take().unwrap_or_default();
        if clean_inside || !segs.is_empty() {
            let r = self.r;
            let sink = &mut *self.sink;
            sink.polygon_start();
            if clean_inside {
                sink.line_start();
                r.interpolate(None, None, 1.0, sink);
                sink.line_end();
            }
            if !segs.is_empty() {
                let compare = |a: &P, b: &P| r.compare_point((a.0, a.1), (b.0, b.1));
                let mut interp = |f: Option<(f64, f64)>, t: Option<(f64, f64)>, d: f64, s: &mut dyn Stream| {
                    r.interpolate(f, t, d, s)
                };
                rejoin(segs, &compare, start_inside, &mut interp, sink);
            }
            sink.polygon_end();
        }
        self.to_buffer = false;
        self.segments = None;
        self.polygon.clear();
        self.in_polygon = false;
    }
    fn sphere(&mut self) {
        self.sink.sphere();
    }
}

/// `eas-geo-clip-rectangle' to X0 Y0 X1 Y1 into SINK.
pub fn clip_rectangle(x0: f64, y0: f64, x1: f64, y1: f64, sink: BoxStream) -> BoxStream {
    Box::new(ClipRect {
        r: Rect { x0, y0, x1, y1 },
        sink,
        buffer: ClipBuffer::default(),
        to_buffer: false,
        segments: None,
        polygon: Vec::new(),
        in_polygon: false,
        x__: f64::NAN,
        y__: f64::NAN,
        v__: false,
        x_: 0.0,
        y_: 0.0,
        v_: false,
        first: false,
        clean: true,
        line_mode: false,
    })
}
