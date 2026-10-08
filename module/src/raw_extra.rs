// SPDX-License-Identifier: GPL-3.0-or-later
//! The d3-geo-projection raw projections of eas-geo-raw.el, the elliptic
//! integrals behind guyou, square() and quincuncial(), and the armadillo
//! and Berghaus outlines.  Operand order follows the Elisp exactly.

use crate::math::{acos, asin, fround, lmax, pow, sign, EPS, HALF_PI, PI, QUARTER_PI, RAD, TAU};
use crate::raw::{azimuthal_equal_area, azimuthal_equidistant, RawFn};
use crate::stream::Stream;
use std::sync::OnceLock;

/// `eas-geo--sinci': X / sin X, 1 at 0.
#[inline]
fn sinci(x: f64) -> f64 {
    if x == 0.0 {
        1.0
    } else {
        x / x.sin()
    }
}

pub fn aitoff(x: f64, y: f64) -> (f64, f64) {
    let cy = y.cos();
    let x = x / 2.0;
    let s = sinci(acos(cy * x.cos()));
    (2.0 * cy * x.sin() * s, y.sin() * s)
}

pub fn winkel3(l: f64, p: f64) -> (f64, f64) {
    let c = aitoff(l, p);
    ((c.0 + l / HALF_PI) / 2.0, (c.1 + p) / 2.0)
}

pub fn hammer(l: f64, p: f64) -> (f64, f64) {
    let c = azimuthal_equal_area(l / 2.0, p);
    (2.0 * c.0, c.1)
}

pub fn mollweide(l: f64, p: f64) -> (f64, f64) {
    let mut p = p;
    let cp = PI;
    let cps = cp * p.sin();
    let mut i = 30;
    let mut delta = 1.0f64;
    while delta.abs() > EPS && i > 0 {
        delta = ((p + p.sin()) - cps) / (1.0 + p.cos());
        p -= delta;
        i -= 1;
    }
    let th = p / 2.0;
    let cth = th.cos();
    let y = 2.0f64.sqrt() * th.sin();
    (2.0f64.sqrt() / HALF_PI * l * cth, y)
}

pub fn sinusoidal(l: f64, p: f64) -> (f64, f64) {
    (l * p.cos(), p)
}

pub fn wagner6(l: f64, p: f64) -> (f64, f64) {
    (l * (1.0 - (3.0 * p * p) / (PI * PI)).sqrt(), p)
}

pub fn eckert1(l: f64, p: f64) -> (f64, f64) {
    let a = (8.0 / (3.0 * PI)).sqrt();
    (a * l * (1.0 - p.abs() / PI), a * p)
}

pub fn collignon(l: f64, p: f64) -> (f64, f64) {
    let a = (1.0 - p.sin()).sqrt();
    let sp = PI.sqrt();
    (2.0 / sp * l * a, sp * (1.0 - a))
}

pub fn baker(l: f64, p: f64) -> (f64, f64) {
    let p0 = p.abs();
    let s2 = 2.0f64.sqrt();
    if p0 < QUARTER_PI {
        (l, (QUARTER_PI + p / 2.0).tan().ln())
    } else {
        (
            l * p0.cos() * (2.0 * s2 - 1.0 / p0.sin()),
            sign(p) * (2.0 * s2 * (p0 - QUARTER_PI) - (p0 / 2.0).tan().ln()),
        )
    }
}

pub fn bottomley(l: f64, p: f64) -> (f64, f64) {
    let s = 0.5;
    let rho = HALF_PI - p;
    let eta = if rho != 0.0 { (l * s * rho.sin()) / rho } else { rho };
    ((rho * eta.sin()) / s, HALF_PI - rho * eta.cos())
}

pub fn littrow(l: f64, p: f64) -> (f64, f64) {
    (l.sin() / p.cos(), p.tan() * l.cos())
}

pub fn wiechel(l: f64, p: f64) -> (f64, f64) {
    let cp = p.cos();
    let sp = l.cos() * cp;
    let s1 = 1.0 - sp;
    let l2 = (l.sin() * cp).atan2(-p.sin());
    let cl = l2.cos();
    let sl = l2.sin();
    let cp2 = lmax(0.0, 1.0 - sp * sp).sqrt();
    (sl * cp2 - cl * s1, -(cl * cp2) - sl * s1)
}

fn airy_b() -> f64 {
    let beta = HALF_PI;
    let tb = (beta / 2.0).tan();
    (2.0 * (beta / 2.0).cos().ln()) / (tb * tb)
}

pub fn airy(x: f64, y: f64) -> (f64, f64) {
    static B: OnceLock<f64> = OnceLock::new();
    let b = *B.get_or_init(airy_b);
    let cosz = y.cos() * x.cos();
    let k = -((if 1.0 - cosz != 0.0 { ((1.0 + cosz) / 2.0).ln() / (1.0 - cosz) } else { -0.5 })
        + b / (1.0 + cosz));
    (k * y.cos() * x.sin(), k * y.sin())
}

pub fn berghaus(l: f64, p: f64) -> (f64, f64) {
    let k = (2.0 * PI) / 5.0;
    let pt = azimuthal_equidistant(l, p);
    if l.abs() <= HALF_PI {
        return pt;
    }
    let theta = pt.1.atan2(pt.0);
    let r = (pow(pt.0, 2.0) + pow(pt.1, 2.0)).sqrt();
    let theta0 = k * fround((theta - HALF_PI) / k) + HALF_PI;
    let d = theta - theta0;
    let alpha = d.sin().atan2(2.0 - d.cos());
    let theta = (theta0 + asin((PI / r) * alpha.sin())) - alpha;
    (r * theta.cos(), r * theta.sin())
}

// Elliptic projections

/// `eas-geo--agm' of M: (AS, BS, DIVISOR).
fn agm(m: f64) -> (Vec<f64>, Vec<f64>, f64) {
    let mut a = 1.0f64;
    let mut b = (1.0 - m).sqrt();
    let mut c = m.sqrt();
    let mut i = 0;
    let mut as_ = Vec::new();
    let mut bs = Vec::new();
    while c.abs() > EPS {
        as_.push(a);
        bs.push(b);
        c = (a + b) / 2.0;
        b = (a * b).sqrt();
        a = c;
        c = (a - b) / 2.0;
        i += 1;
    }
    (as_, bs, 2.0f64.powf(i as f64) * a)
}

/// `eas-geo-elliptic-f': F(PHI|M).
pub fn elliptic_f(phi: f64, m: f64) -> f64 {
    if m == 0.0 {
        return phi;
    }
    if m == 1.0 {
        return (phi / 2.0 + QUARTER_PI).tan().ln();
    }
    let (as_, bs, div) = agm(m);
    let mut phi = phi;
    for i in 0..as_.len() {
        let q = (phi / PI).trunc();
        if phi - PI * q != 0.0 {
            let mut dphi = ((bs[i] * phi.tan()) / as_[i]).atan();
            if dphi < 0.0 {
                dphi += PI;
            }
            phi = phi + dphi + q * PI;
        } else {
            phi = phi + phi;
        }
    }
    phi / div
}

/// `eas-geo-elliptic-fi': F(PHI + i PSI|M) as (RE, IM).
pub fn elliptic_fi(phi: f64, psi: f64, m: f64) -> (f64, f64) {
    let r = phi.abs();
    let a = psi.abs();
    let sh = (a.exp() - (-a).exp()) / 2.0;
    if r != 0.0 {
        let csc = 1.0 / r.sin();
        let cot2 = 1.0 / (r.tan() * r.tan());
        let b = -(cot2 + m * sh * sh * csc * csc + -1.0 + m);
        let c = (m - 1.0) * cot2;
        let cotl2 = (-b + (b * b - 4.0 * c).sqrt()) / 2.0;
        (
            elliptic_f((1.0 / cotl2.sqrt()).atan(), m) * sign(phi),
            elliptic_f(lmax(0.0, (cotl2 / cot2 - 1.0) / m).sqrt().atan(), 1.0 - m) * sign(psi),
        )
    } else {
        (0.0, elliptic_f(sh.atan(), 1.0 - m) * sign(psi))
    }
}

/// `eas-geo--guyou': [K_ K F(pi/2|K^2) sqrt(K_) K^2].
fn guyou_consts() -> &'static [f64; 5] {
    static G: OnceLock<[f64; 5]> = OnceLock::new();
    G.get_or_init(|| {
        let s2 = 2.0f64.sqrt();
        let k_ = (s2 - 1.0) / (s2 + 1.0);
        let k = (1.0 - k_ * k_).sqrt();
        [k_, k, elliptic_f(HALF_PI, k * k), k_.sqrt(), k * k]
    })
}

pub fn guyou(l: f64, p: f64) -> (f64, f64) {
    let g = guyou_consts();
    let kk = g[2];
    let psi = (QUARTER_PI + p.abs() / 2.0).tan().ln();
    let r = (-psi).exp() / g[3];
    let x = r * (-l).cos();
    let y = r * (-l).sin();
    let x2 = x * x;
    let y1 = y + 1.0;
    let tt = 1.0 - x2 - y * y;
    let at0 = 0.5 * ((if x >= 0.0 { HALF_PI } else { -HALF_PI }) - tt.atan2(2.0 * x));
    let at1 = -0.25 * (tt * tt + 4.0 * x2).ln() + 0.5 * (y1 * y1 + x2).ln();
    let fi = elliptic_fi(at0, at1, g[4]);
    (-fi.1, (if p >= 0.0 { 1.0 } else { -1.0 }) * (0.5 * kk - fi.0))
}

/// `eas-geo-raw-square' of RAW.
pub fn square(raw: fn(f64, f64) -> (f64, f64)) -> RawFn {
    let dx = raw(HALF_PI, 0.0).0 - raw(-HALF_PI, 0.0).0;
    Box::new(move |l, p| {
        let s = if l > 0.0 { -0.5 } else { 0.5 };
        let pt = raw(l + s * PI, p);
        (pt.0 - s * dx, pt.1)
    })
}

/// `eas-geo-raw-quincuncial' of RAW.
pub fn quincuncial(raw: fn(f64, f64) -> (f64, f64)) -> RawFn {
    let dx = raw(HALF_PI, 0.0).0 - raw(-HALF_PI, 0.0).0;
    let r = 0.5f64.sqrt();
    Box::new(move |l, p| {
        let tt = l.abs() < HALF_PI;
        let pt = raw(
            if tt {
                l
            } else if l > 0.0 {
                l - PI
            } else {
                l + PI
            },
            p,
        );
        let x = (pt.0 - pt.1) * r;
        let y = (pt.0 + pt.1) * r;
        if tt {
            (x, y)
        } else {
            let d = dx * r;
            let s = if (x > 0.0) != (y > 0.0) { -1.0 } else { 1.0 };
            (s * x - sign(y) * d, s * y - sign(x) * d)
        }
    })
}

/// `eas-geo--armadillo': [S0 C0 TAN0 K].
fn armadillo_consts() -> [f64; 4] {
    let phi0 = 20.0 * RAD;
    let s0 = phi0.sin();
    let c0 = phi0.cos();
    [s0, c0, phi0.tan(), ((1.0 + s0) - c0) / 2.0]
}

pub fn armadillo(l: f64, p: f64) -> (f64, f64) {
    static A: OnceLock<[f64; 4]> = OnceLock::new();
    let [s0, c0, tan0, k] = *A.get_or_init(armadillo_consts);
    let cp = p.cos();
    let l = l / 2.0;
    let cl = l.cos();
    (
        (1.0 + cp) * l.sin(),
        (if p > -cl.atan2(tan0) - 1e-3 { 0.0 } else { -10.0 }) + k + p.sin() * c0
            + -((1.0 + cp) * s0 * cl),
    )
}

/// `eas-geo-raw--armadillo-sphere': the outline (degrees) into SINK.
pub fn armadillo_sphere(sink: &mut dyn Stream) {
    let tan0 = (20.0 * RAD).tan();
    let mut lam = -180.0f64;
    sink.polygon_start();
    sink.line_start();
    while lam < 180.0 {
        sink.point(lam, 90.0, 0);
        lam += 90.0;
    }
    loop {
        lam -= 3.0 * 0.5f64.sqrt();
        if lam < -180.0 {
            break;
        }
        sink.point(lam, -((lam * RAD) / 2.0).cos().atan2(tan0) / RAD, 0);
    }
    sink.line_end();
    sink.polygon_end();
}

/// `eas-geo-proj--berghaus-sphere': the five-lobed outline into SINK.
pub fn berghaus_sphere(sink: &mut dyn Stream) {
    let lobes = 5;
    let e = 1e-2;
    let cr = -(e * RAD).cos();
    let sr = (e * RAD).sin();
    let delta = 360.0 / lobes as f64;
    let delta0 = TAU / lobes as f64;
    let mut phi = 90.0 - 180.0 / lobes as f64;
    let mut phi0 = HALF_PI;
    sink.polygon_start();
    sink.line_start();
    for _ in 0..lobes {
        sink.point((sr * phi0.cos()).atan2(cr) / RAD, asin(sr * phi0.sin()) / RAD, 0);
        if phi < -90.0 {
            sink.point(-90.0, -180.0 - phi - e, 0);
            sink.point(-90.0, (-180.0 - phi) + e, 0);
        } else {
            sink.point(90.0, phi + e, 0);
            sink.point(90.0, phi - e, 0);
        }
        phi -= delta;
        phi0 -= delta0;
    }
    sink.line_end();
    sink.polygon_end();
}
