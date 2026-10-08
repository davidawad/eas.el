// SPDX-License-Identifier: GPL-3.0-or-later
//! Bit-exactness tests for raw.rs, raw_extra.rs and polyhedral.rs against
//! values printed by Emacs (eas-geo-raw.el, eas-geo-polyhedral.el).
//! Set EAS_RAW_REF to a file of the same "TYPE P0 P1 L P X Y" lines to
//! check a larger set.

use crate::raw::{interrupt_sphere, make_raw, RawFn};
use crate::stream::Stream;
use std::collections::HashMap;

/// Parse a float as Emacs prints it with %S.
fn num(s: &str) -> f64 {
    match s {
        "1.0e+INF" => f64::INFINITY,
        "-1.0e+INF" => f64::NEG_INFINITY,
        "0.0e+NaN" => f64::NAN,
        "-0.0e+NaN" => -f64::NAN,
        _ => s.parse().unwrap_or_else(|_| panic!("bad number {s}")),
    }
}

/// Same bits, or both NaN of the same sign.
fn same(a: f64, b: f64) -> bool {
    if a.is_nan() || b.is_nan() {
        a.is_nan() && b.is_nan() && a.is_sign_negative() == b.is_sign_negative()
    } else {
        a.to_bits() == b.to_bits()
    }
}

/// Check every line of DATA; return (count, mismatches).
fn check_raw(data: &str) -> (usize, Vec<String>) {
    let mut cache: HashMap<String, RawFn> = HashMap::new();
    let mut bad = Vec::new();
    let mut n = 0;
    for line in data.lines().filter(|l| !l.trim().is_empty()) {
        let w: Vec<&str> = line.split_whitespace().collect();
        let key = format!("{} {} {}", w[0], w[1], w[2]);
        let raw = cache
            .entry(key)
            .or_insert_with(|| make_raw(w[0], (num(w[1]), num(w[2]))).expect("known type"));
        let (x, y) = raw(num(w[3]), num(w[4]));
        n += 1;
        if !same(x, num(w[5])) || !same(y, num(w[6])) {
            bad.push(format!("{line} => {x:?} {y:?}"));
        }
    }
    (n, bad)
}

#[test]
fn raw_projections_match_elisp() {
    let (n, bad) = check_raw(RAW);
    assert!(n > 2000);
    assert!(bad.is_empty(), "{} mismatches:\n{}", bad.len(), bad.join("\n"));
}

#[test]
fn raw_projections_match_elisp_reference_file() {
    let Ok(path) = std::env::var("EAS_RAW_REF") else {
        crate::stream::test_skip(
            "raw_projections_match_elisp_reference_file",
            "EAS_RAW_REF names no reference file (the embedded points are checked by raw_projections_match_elisp)",
        );
        return;
    };
    let data = std::fs::read_to_string(path).unwrap();
    let (n, bad) = check_raw(&data);
    eprintln!("{n} reference points");
    assert!(bad.is_empty(), "{} mismatches:\n{}", bad.len(), bad.join("\n"));
}

#[test]
fn unknown_type_is_none() {
    assert!(make_raw("albersUsa", (0.0, 0.0)).is_none());
    assert!(make_raw("nope", (0.0, 0.0)).is_none());
    assert!(interrupt_sphere("mollweide").is_none());
}

/// A sink recording events as the Elisp test printed them.
struct Rec(Vec<String>);

impl Stream for Rec {
    fn point(&mut self, x: f64, y: f64, m: u8) {
        assert_eq!(m, 0);
        self.0.push(format!("P {} {}", x.to_bits(), y.to_bits()));
    }
    fn line_start(&mut self) {
        self.0.push("LS".into());
    }
    fn line_end(&mut self) {
        self.0.push("LE".into());
    }
    fn polygon_start(&mut self) {
        self.0.push("PS".into());
    }
    fn polygon_end(&mut self) {
        self.0.push("PE".into());
    }
    fn sphere(&mut self) {
        self.0.push("SPHERE".into());
    }
}

/// SPH's events for NAME, points as bits.
fn expected(name: &str) -> Vec<String> {
    SPH.lines()
        .filter_map(|l| l.strip_prefix(name).and_then(|r| r.strip_prefix(' ')))
        .map(|r| {
            let w: Vec<&str> = r.split_whitespace().collect();
            if w[0] == "P" {
                format!("P {} {}", num(w[1]).to_bits(), num(w[2]).to_bits())
            } else {
                w[0].to_string()
            }
        })
        .collect()
}

fn check_sphere(name: &str, f: fn(&mut dyn Stream)) {
    let mut r = Rec(Vec::new());
    f(&mut r);
    let e = expected(name);
    assert!(e.len() > 4, "{name}");
    assert_eq!(r.0, e, "{name}");
}

#[test]
fn armadillo_sphere_matches_elisp() {
    check_sphere("armadillo", crate::raw_extra::armadillo_sphere);
}

#[test]
fn butterfly_sphere_matches_elisp() {
    check_sphere("butterfly", crate::polyhedral::butterfly_sphere);
}

#[test]
fn berghaus_sphere_matches_elisp() {
    check_sphere("berghaus", crate::raw_extra::berghaus_sphere);
}

#[test]
fn interrupt_spheres_match_elisp() {
    for name in ["interruptedSinusoidal", "interruptedMollweide", "interruptedMollweideHemispheres"] {
        let got: Vec<String> = interrupt_sphere(name)
            .unwrap()
            .iter()
            .map(|p| format!("P {} {}", p.0.to_bits(), p.1.to_bits()))
            .collect();
        let e = expected(name);
        assert!(e.len() > 100, "{name}");
        assert_eq!(got, e, "{name}");
    }
}

/// "TYPE P0 P1 L P X Y" lines from Emacs.
/// Raw projections at sample points: "TYPE P0 P1 L P X Y" per line, from Emacs.
const RAW: &str = include_str!("../tests/data/raw-reference.txt");

/// Sphere outlines as Emacs streams them, from Emacs.
const SPH: &str = include_str!("../tests/data/sphere-reference.txt");
