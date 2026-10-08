// SPDX-License-Identifier: GPL-3.0-or-later
//! Raw map projections of eas-geo-raw.el: the d3-geo ones, the
//! interrupted projections and the registry.  The d3-geo-projection
//! ones live in raw_extra.rs, the butterfly in polyhedral.rs.
//!
//! Every expression keeps the Elisp's operand order (Elisp arithmetic is
//! left-associative) so the results are bit-identical.

use crate::math::{acos, lmax, EPS, HALF_PI, RAD};
use crate::raw_extra as ex;
use std::sync::Arc;

/// A raw projection: (LAMBDA PHI) radians to (X Y) at unit scale.
pub type RawFn = Box<dyn Fn(f64, f64) -> (f64, f64) + Send + Sync>;

// d3-geo

/// `eas-geo-raw-azimuthal' with radial SCALE.
#[inline]
fn azimuthal(scale: impl Fn(f64) -> f64, x: f64, y: f64) -> (f64, f64) {
    let cx = x.cos();
    let cy = y.cos();
    let k = scale(cx * cy);
    if k == f64::INFINITY {
        (2.0, 0.0)
    } else {
        (k * cy * x.sin(), k * y.sin())
    }
}

/// `eas-geo-raw-azimuthal-equal-area'.
pub fn azimuthal_equal_area(x: f64, y: f64) -> (f64, f64) {
    azimuthal(
        |c| if c == -1.0 { f64::INFINITY } else { (2.0 / (1.0 + c)).sqrt() },
        x,
        y,
    )
}

/// `eas-geo-raw-azimuthal-equidistant'.
pub fn azimuthal_equidistant(x: f64, y: f64) -> (f64, f64) {
    azimuthal(
        |c| {
            let c = acos(c);
            if c == 0.0 {
                0.0
            } else {
                c / c.sin()
            }
        },
        x,
        y,
    )
}

pub fn equirectangular(l: f64, p: f64) -> (f64, f64) {
    (l, p)
}

pub fn mercator(l: f64, p: f64) -> (f64, f64) {
    (l, ((HALF_PI + p) / 2.0).tan().ln())
}

pub fn transverse_mercator(l: f64, p: f64) -> (f64, f64) {
    (((HALF_PI + p) / 2.0).tan().ln(), -l)
}

pub fn orthographic(x: f64, y: f64) -> (f64, f64) {
    (y.cos() * x.sin(), y.sin())
}

pub fn gnomonic(x: f64, y: f64) -> (f64, f64) {
    let cy = y.cos();
    let k = x.cos() * cy;
    ((cy * x.sin()) / k, y.sin() / k)
}

pub fn stereographic(x: f64, y: f64) -> (f64, f64) {
    let cy = y.cos();
    let k = 1.0 + x.cos() * cy;
    ((cy * x.sin()) / k, y.sin() / k)
}

pub fn equal_earth(l: f64, p: f64) -> (f64, f64) {
    let a1 = 1.340264;
    let a2 = -0.081106;
    let a3 = 0.000893;
    let a4 = 0.003796;
    let m = 3.0f64.sqrt() / 2.0;
    let th = crate::math::asin(m * p.sin());
    let t2 = th * th;
    let t6 = t2 * t2 * t2;
    (
        (l * th.cos()) / (m * (a1 + 3.0 * a2 * t2 + t6 * (7.0 * a3 + 9.0 * a4 * t2))),
        th * (a1 + a2 * t2 + t6 * (a3 + a4 * t2)),
    )
}

pub fn natural_earth1(l: f64, p: f64) -> (f64, f64) {
    let p2 = p * p;
    let p4 = p2 * p2;
    (
        l * (0.8707 + -0.131979 * p2 + p4 * (-0.013791 + p4 * (0.003971 * p2 - 0.001529 * p4))),
        p * (1.007226 + p2 * (0.015085 + p4 * (-0.044475 + 0.028874 * p2 + -0.005916 * p4))),
    )
}

/// `eas-geo-raw-conic-equal-area' for parallels Y0 Y1 (radians).
pub fn conic_equal_area(y0: f64, y1: f64) -> RawFn {
    let sy0 = y0.sin();
    let n = (sy0 + y1.sin()) / 2.0;
    if n.abs() < EPS {
        let cp = y0.cos();
        Box::new(move |l, p| (l * cp, p.sin() / cp))
    } else {
        let c = 1.0 + sy0 * (2.0 * n - sy0);
        let r0 = c.sqrt() / n;
        Box::new(move |x, y| {
            let r = lmax(0.0, c - 2.0 * n * y.sin()).sqrt() / n;
            let x = x * n;
            (r * x.sin(), r0 - r * x.cos())
        })
    }
}

#[inline]
fn tany(y: f64) -> f64 {
    ((HALF_PI + y) / 2.0).tan()
}

/// `eas-geo-raw-conic-conformal' for parallels Y0 Y1 (radians).
pub fn conic_conformal(y0: f64, y1: f64) -> RawFn {
    let cy0 = y0.cos();
    let n = if y0 == y1 {
        y0.sin()
    } else {
        (cy0 / y1.cos()).ln() / (tany(y1) / tany(y0)).ln()
    };
    if n == 0.0 {
        return Box::new(mercator);
    }
    let f = (cy0 * tany(y0).powf(n)) / n;
    Box::new(move |x, y| {
        let mut y = y;
        if f > 0.0 {
            if y < -HALF_PI + EPS {
                y = -HALF_PI + EPS;
            }
        } else if y > HALF_PI - EPS {
            y = HALF_PI - EPS;
        }
        let r = f / tany(y).powf(n);
        (r * (n * x).sin(), f - r * (n * x).cos())
    })
}

/// `eas-geo-raw-conic-equidistant' for parallels Y0 Y1 (radians).
pub fn conic_equidistant(y0: f64, y1: f64) -> RawFn {
    let cy0 = y0.cos();
    let n = if y0 == y1 { y0.sin() } else { (cy0 - y1.cos()) / (y1 - y0) };
    if n.abs() < EPS {
        return Box::new(equirectangular);
    }
    let g = cy0 / n + y0;
    Box::new(move |x, y| {
        let gy = g - y;
        let nx = n * x;
        (gy * nx.sin(), g - gy * nx.cos())
    })
}

// Interrupted projections

/// Lobes in degrees: (north, south), each a list of three-vertex lobes.
type Lobes = (Vec<[(f64, f64); 3]>, Vec<[(f64, f64); 3]>);

fn lobes_of(flat: &[[(i32, i32); 3]], north: usize) -> Lobes {
    let v: Vec<[(f64, f64); 3]> = flat
        .iter()
        .map(|l| {
            [
                (l[0].0 as f64, l[0].1 as f64),
                (l[1].0 as f64, l[1].1 as f64),
                (l[2].0 as f64, l[2].1 as f64),
            ]
        })
        .collect();
    (v[..north].to_vec(), v[north..].to_vec())
}

/// `eas-geo-raw-lobes' of NAME.
fn lobes(name: &str) -> Option<Lobes> {
    match name {
        "interruptedSinusoidal" => Some(lobes_of(
            &[
                [(-180, 0), (-110, 90), (-40, 0)],
                [(-40, 0), (0, 90), (40, 0)],
                [(40, 0), (110, 90), (180, 0)],
                [(-180, 0), (-110, -90), (-40, 0)],
                [(-40, 0), (0, -90), (40, 0)],
                [(40, 0), (110, -90), (180, 0)],
            ],
            3,
        )),
        "interruptedMollweide" => Some(lobes_of(
            &[
                [(-180, 0), (-100, 90), (-40, 0)],
                [(-40, 0), (30, 90), (180, 0)],
                [(-180, 0), (-160, -90), (-100, 0)],
                [(-100, 0), (-60, -90), (-20, 0)],
                [(-20, 0), (20, -90), (80, 0)],
                [(80, 0), (140, -90), (180, 0)],
            ],
            2,
        )),
        "interruptedMollweideHemispheres" => Some(lobes_of(
            &[
                [(-180, 0), (-90, 90), (0, 0)],
                [(0, 0), (90, 90), (180, 0)],
                [(-180, 0), (-90, -90), (0, 0)],
                [(0, 0), (90, -90), (180, 0)],
            ],
            2,
        )),
        _ => None,
    }
}

/// `eas-geo-raw-interrupt': RAW interrupted along LOBES (degrees).
fn interrupt(raw: Arc<dyn Fn(f64, f64) -> (f64, f64) + Send + Sync>, lobes: &Lobes) -> RawFn {
    let rad = |h: &Vec<[(f64, f64); 3]>| -> Vec<[(f64, f64); 3]> {
        h.iter()
            .map(|l| {
                let mut o = [(0.0, 0.0); 3];
                for (k, pt) in l.iter().enumerate() {
                    o[k] = (pt.0 * RAD, pt.1 * RAD);
                }
                o
            })
            .collect()
    };
    let north = rad(&lobes.0);
    let south = rad(&lobes.1);
    Box::new(move |l, p| {
        let sign = if p < 0.0 { -1.0 } else { 1.0 };
        let lobe = if p < 0.0 { &south } else { &north };
        let n = lobe.len() - 1;
        let mut i = 0;
        while i < n && l > lobe[i][2].0 {
            i += 1;
        }
        let lb = &lobe[i];
        let mid = lb[1].0;
        let top = lb[0].1;
        let pt = raw(l - mid, p);
        let off = raw(mid, if sign * p > sign * top { top } else { p });
        (pt.0 + off.0, pt.1)
    })
}

/// The single ring of `eas-geo-raw-interrupt-sphere' for type NAME.
pub fn interrupt_sphere(name: &str) -> Option<Vec<(f64, f64)>> {
    let lobes = lobes(name)?;
    let e = EPS;
    let interp = |pts: [(f64, f64); 4], m: i32, out: &mut Vec<(f64, f64)>| {
        let mut p0 = pts[0];
        for p1 in pts {
            let dx = (p1.0 - p0.0) / m as f64;
            let dy = (p1.1 - p0.1) / m as f64;
            for j in 0..m {
                out.push((p0.0 + j as f64 * dx, p0.1 + j as f64 * dy));
            }
            p0 = p1;
        }
        out.push(pts[3]);
    };
    let mut out = Vec::new();
    for lb in &lobes.0 {
        let (l0, p0, p1, l2, p2) = (lb[0].0, lb[0].1, lb[1].1, lb[2].0, lb[2].1);
        interp(
            [(l0 + e, p0 + e), (l0 + e, p1 - e), (l2 - e, p1 - e), (l2 - e, p2 + e)],
            30,
            &mut out,
        );
    }
    for lb in lobes.1.iter().rev() {
        let (l0, p0, p1, l2, p2) = (lb[0].0, lb[0].1, lb[1].1, lb[2].0, lb[2].1);
        interp(
            [(l2 - e, p2 - e), (l2 - e, p1 + e), (l0 + e, p1 + e), (l0 + e, p0 - e)],
            30,
            &mut out,
        );
    }
    Some(out)
}

// The registry

/// The raw projection of d3 type NAME, PARALLELS (degrees) for conics.
pub fn make_raw(name: &str, parallels: (f64, f64)) -> Option<RawFn> {
    let (y0, y1) = (parallels.0 * RAD, parallels.1 * RAD);
    let b = |f: fn(f64, f64) -> (f64, f64)| -> Option<RawFn> { Some(Box::new(f)) };
    match name {
        "albers" | "conicEqualArea" => Some(conic_equal_area(y0, y1)),
        "conicConformal" => Some(conic_conformal(y0, y1)),
        "conicEquidistant" => Some(conic_equidistant(y0, y1)),
        "azimuthalEqualArea" => b(azimuthal_equal_area),
        "azimuthalEquidistant" => b(azimuthal_equidistant),
        "equalEarth" => b(equal_earth),
        "equirectangular" => b(equirectangular),
        "gnomonic" => b(gnomonic),
        "mercator" => b(mercator),
        "naturalEarth1" => b(natural_earth1),
        "orthographic" => b(orthographic),
        "stereographic" => b(stereographic),
        "transverseMercator" => b(transverse_mercator),
        "airy" => b(ex::airy),
        "aitoff" => b(ex::aitoff),
        "armadillo" => b(ex::armadillo),
        "guyou" => Some(ex::square(ex::guyou)),
        "polyhedralButterfly" => Some(crate::polyhedral::butterfly_raw()),
        "peirceQuincuncial" => Some(ex::quincuncial(ex::guyou)),
        "baker" => b(ex::baker),
        "berghaus" => b(ex::berghaus),
        "bottomley" => b(ex::bottomley),
        "collignon" => b(ex::collignon),
        "eckert1" => b(ex::eckert1),
        "hammer" => b(ex::hammer),
        "littrow" => b(ex::littrow),
        "mollweide" => b(ex::mollweide),
        "sinusoidal" => b(ex::sinusoidal),
        "wagner6" => b(ex::wagner6),
        "wiechel" => b(ex::wiechel),
        "winkel3" => b(ex::winkel3),
        "interruptedSinusoidal" => Some(interrupt(Arc::new(ex::sinusoidal), &lobes(name)?)),
        "interruptedMollweide" | "interruptedMollweideHemispheres" => {
            Some(interrupt(Arc::new(ex::mollweide), &lobes(name)?))
        }
        _ => None,
    }
}

#[cfg(test)]
#[path = "raw_tests.rs"]
mod raw_tests;
