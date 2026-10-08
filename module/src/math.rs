// SPDX-License-Identifier: GPL-3.0-or-later
//! Constants and helpers with Emacs Lisp's exact float semantics.
//!
//! Every function here mirrors one in eas-geo-stream.el.  Results must be
//! bit-identical to the Elisp: no reassociation, the same libm calls, and
//! Elisp's `min'/`max' (NaN wins, the first argument wins ties).

pub const PI: f64 = std::f64::consts::PI;
pub const EPS: f64 = 1e-6;
pub const EPS2: f64 = 1e-12;
pub const HALF_PI: f64 = PI / 2.0;
pub const QUARTER_PI: f64 = PI / 4.0;
pub const TAU: f64 = 2.0 * PI;
pub const RAD: f64 = PI / 180.0;

/// Elisp `(max A B)': a NaN argument wins, else B only when greater.
#[inline]
pub fn lmax(a: f64, b: f64) -> f64 {
    if a.is_nan() {
        a
    } else if b.is_nan() || b > a {
        b
    } else {
        a
    }
}

/// Elisp `(min A B)': a NaN argument wins, else B only when smaller.
#[inline]
pub fn lmin(a: f64, b: f64) -> f64 {
    if a.is_nan() {
        a
    } else if b.is_nan() || b < a {
        b
    } else {
        a
    }
}

/// Elisp `(expt X Y)' of a float X: libm's pow.  A constant Y must not
/// reach LLVM, which would fold pow(x, 2.0) into x * x: glibc's pow is
/// not always correctly rounded, so the two can differ by an ulp.
#[inline]
pub fn pow(x: f64, y: f64) -> f64 {
    x.powf(std::hint::black_box(y))
}

/// `eas-geo-asin': d3's clamped asin.
#[inline]
pub fn asin(x: f64) -> f64 {
    if x > 1.0 {
        HALF_PI
    } else if x < -1.0 {
        -HALF_PI
    } else {
        x.asin()
    }
}

/// `eas-geo-acos': d3's clamped acos.
#[inline]
pub fn acos(x: f64) -> f64 {
    if x > 1.0 {
        0.0
    } else if x < -1.0 {
        PI
    } else {
        x.acos()
    }
}

/// `eas-geo-sign' as a float: 1, -1 or 0.
#[inline]
pub fn sign(x: f64) -> f64 {
    if x > 0.0 {
        1.0
    } else if x < 0.0 {
        -1.0
    } else {
        0.0
    }
}

/// `eas-geo-rem': JavaScript's A % B as (- a (* b (ftruncate (/ a b)))).
#[inline]
pub fn rem(a: f64, b: f64) -> f64 {
    a - b * (a / b).trunc()
}

/// Elisp `fround': round half to even.
#[inline]
pub fn fround(x: f64) -> f64 {
    x.round_ties_even()
}

/// `eas-geo-cartesian'.
#[inline]
pub fn cartesian(lam: f64, phi: f64) -> [f64; 3] {
    let c = phi.cos();
    [c * lam.cos(), c * lam.sin(), phi.sin()]
}

/// `eas-geo-spherical'.
#[inline]
pub fn spherical(v: [f64; 3]) -> (f64, f64) {
    (v[1].atan2(v[0]), asin(v[2]))
}

#[inline]
pub fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

#[inline]
pub fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]
}

#[inline]
pub fn scale3(v: [f64; 3], k: f64) -> [f64; 3] {
    [k * v[0], k * v[1], k * v[2]]
}

#[inline]
pub fn add3(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
}

/// `eas-geo-normalize': V scaled to unit length (V itself when null).
#[inline]
pub fn normalize(v: [f64; 3]) -> [f64; 3] {
    let l = dot(v, v).sqrt();
    if l > 0.0 {
        scale3(v, 1.0 / l)
    } else {
        v
    }
}

/// `eas-geo-point-equal'.
#[inline]
pub fn point_equal(a: (f64, f64), b: (f64, f64)) -> bool {
    (a.0 - b.0).abs() < EPS && (a.1 - b.1).abs() < EPS
}

/// `eas-geo--wrap-lambda'.
#[inline]
pub fn wrap_lambda(lam: f64) -> f64 {
    if lam.abs() > PI {
        lam - fround(lam / TAU) * TAU
    } else {
        lam
    }
}

/// D3's rotateRadians (`eas-geo-rotation'), forward direction only.
#[derive(Clone, Copy, Debug)]
pub struct Rotation {
    dl: f64,
    has_dl: bool,
    has_pg: bool,
    cdp: f64,
    sdp: f64,
    cdg: f64,
    sdg: f64,
}

impl Rotation {
    /// DL DP DG in radians.
    pub fn new(dl: f64, dp: f64, dg: f64) -> Rotation {
        let dl = rem(dl, TAU);
        Rotation {
            dl,
            has_dl: dl != 0.0,
            has_pg: dp != 0.0 || dg != 0.0,
            cdp: dp.cos(),
            sdp: dp.sin(),
            cdg: dg.cos(),
            sdg: dg.sin(),
        }
    }

    #[inline]
    fn pg(&self, l: f64, p: f64) -> (f64, f64) {
        let c = p.cos();
        let x = l.cos() * c;
        let y = l.sin() * c;
        let z = p.sin();
        let k = z * self.cdp + x * self.sdp;
        (
            (y * self.cdg - k * self.sdg).atan2(x * self.cdp - z * self.sdp),
            asin(k * self.cdg + y * self.sdg),
        )
    }

    #[inline]
    pub fn forward(&self, l: f64, p: f64) -> (f64, f64) {
        match (self.has_dl, self.has_pg) {
            (true, true) => {
                let a = wrap_lambda(l + self.dl);
                self.pg(a, p)
            }
            (true, false) => (wrap_lambda(l + self.dl), p),
            (false, true) => self.pg(l, p),
            (false, false) => (wrap_lambda(l), p),
        }
    }
}
