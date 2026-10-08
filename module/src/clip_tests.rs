// SPDX-License-Identifier: GPL-3.0-or-later
//! Tests of clip.rs, clip_circle.rs and clip_rect.rs against the Elisp.
//!
//! The reference tests run batch Emacs on eas-geo-clip.el (../src, or
//! $EAS_SRC) with the generator below: it prints every event fed into a
//! clip and every event the clip sends on, floats with `%S' (round-trip
//! precision).  The Rust clip is fed the same input events and must send
//! the same events, bit for bit.  Without Emacs they skip and say so.

use super::*;
use crate::clip_circle::clip_circle;
use crate::clip_rect::clip_rectangle;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, Copy)]
enum Ev {
    P(f64, f64, u8),
    Ls,
    Le,
    Ps,
    Pe,
    Sph,
}

fn same(a: f64, b: f64) -> bool {
    a.to_bits() == b.to_bits() || (a.is_nan() && b.is_nan())
}

fn ev_eq(a: &Ev, b: &Ev) -> bool {
    match (a, b) {
        (Ev::P(x, y, m), Ev::P(u, v, n)) => same(*x, *u) && same(*y, *v) && m == n,
        (Ev::Ls, Ev::Ls) | (Ev::Le, Ev::Le) | (Ev::Ps, Ev::Ps) | (Ev::Pe, Ev::Pe) | (Ev::Sph, Ev::Sph) => true,
        _ => false,
    }
}

struct Rec(Rc<RefCell<Vec<Ev>>>);

impl Stream for Rec {
    fn point(&mut self, x: f64, y: f64, m: u8) {
        self.0.borrow_mut().push(Ev::P(x, y, m))
    }
    fn line_start(&mut self) {
        self.0.borrow_mut().push(Ev::Ls)
    }
    fn line_end(&mut self) {
        self.0.borrow_mut().push(Ev::Le)
    }
    fn polygon_start(&mut self) {
        self.0.borrow_mut().push(Ev::Ps)
    }
    fn polygon_end(&mut self) {
        self.0.borrow_mut().push(Ev::Pe)
    }
    fn sphere(&mut self) {
        self.0.borrow_mut().push(Ev::Sph)
    }
}

fn feed(s: &mut dyn Stream, evs: &[Ev]) {
    for e in evs {
        match *e {
            Ev::P(x, y, m) => s.point(x, y, m),
            Ev::Ls => s.line_start(),
            Ev::Le => s.line_end(),
            Ev::Ps => s.polygon_start(),
            Ev::Pe => s.polygon_end(),
            Ev::Sph => s.sphere(),
        }
    }
}

/// Run a clip made by MAKE on INPUT; the events it sends.
fn run(make: impl FnOnce(BoxStream) -> BoxStream, input: &[Ev]) -> Vec<Ev> {
    let acc = Rc::new(RefCell::new(Vec::new()));
    let mut clip = make(Box::new(Rec(acc.clone())));
    feed(&mut *clip, input);
    drop(clip);
    Rc::try_unwrap(acc).map(|c| c.into_inner()).unwrap_or_default()
}

fn pf(s: &str) -> f64 {
    if s.contains("NaN") {
        if s.starts_with('-') {
            -f64::NAN
        } else {
            f64::NAN
        }
    } else if s.contains("INF") {
        if s.starts_with('-') {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        }
    } else {
        s.parse().unwrap_or_else(|_| panic!("bad float {s}"))
    }
}

fn parse_ev(t: &[&str]) -> Ev {
    match t[0] {
        "p" => Ev::P(pf(t[1]), pf(t[2]), t[3].parse().unwrap()),
        "ls" => Ev::Ls,
        "le" => Ev::Le,
        "ps" => Ev::Ps,
        "pe" => Ev::Pe,
        "sph" => Ev::Sph,
        x => panic!("bad event {x}"),
    }
}

/// The Elisp reference: QUICK for the hand-made cases, else the world.
fn generate(quick: bool) -> Option<String> {
    let src = std::env::var("EAS_SRC").unwrap_or_else(|_| concat!(env!("CARGO_MANIFEST_DIR"), "/../src").to_string());
    if !std::path::Path::new(&src).join("eas-geo-clip.el").exists() {
        crate::stream::test_skip("clip reference tests", &format!("no eas-geo-clip.el under {src}"));
        return None;
    }
    let dir = std::env::temp_dir().join(format!("eas-geo-clip-test-{}-{}", std::process::id(), quick));
    std::fs::create_dir_all(&dir).ok()?;
    let gen = dir.join("gen.el");
    let out = dir.join("out.txt");
    std::fs::write(&gen, GEN_EL).ok()?;
    let status = std::process::Command::new("emacs")
        .args(["-Q", "--batch", "-L", &src, "-l"])
        .arg(&gen)
        .arg("--eval")
        .arg(format!("(gen-run {:?} {})", out.to_str()?, if quick { "t" } else { "nil" }))
        .status();
    match status {
        Ok(s) if s.success() => {}
        Ok(s) => panic!("emacs failed: {s}"),
        Err(e) => {
            crate::stream::test_skip("clip reference tests", &format!("no emacs ({e})"));
            return None;
        }
    }
    let text = std::fs::read_to_string(&out).ok();
    let _ = std::fs::remove_dir_all(&dir);
    text
}

struct Case {
    name: String,
    kind: String,
    args: Vec<f64>,
    input: Vec<Ev>,
    output: Vec<Ev>,
}

fn parse_cases(text: &str) -> Vec<Case> {
    let mut cases: Vec<Case> = Vec::new();
    for line in text.lines() {
        let t: Vec<&str> = line.split_whitespace().collect();
        match t.first().copied() {
            Some("case") => cases.push(Case {
                name: t[1].to_string(),
                kind: t[2].to_string(),
                args: t[3..].iter().map(|s| pf(s)).collect(),
                input: Vec::new(),
                output: Vec::new(),
            }),
            Some("i") => cases.last_mut().unwrap().input.push(parse_ev(&t[1..])),
            Some("o") => cases.last_mut().unwrap().output.push(parse_ev(&t[1..])),
            _ => {}
        }
    }
    cases
}

/// Compare every case; the number of events checked.
fn check_cases(cases: &[Case]) -> usize {
    let mut bad = Vec::new();
    let mut events = 0;
    for c in cases {
        let a = c.args.clone();
        let got = match c.kind.as_str() {
            "anti" => run(clip_antimeridian, &c.input),
            "circle" => run(|s| clip_circle(a[0], s), &c.input),
            "rect" => run(|s| clip_rectangle(a[0], a[1], a[2], a[3], s), &c.input),
            k => panic!("bad kind {k}"),
        };
        events += c.output.len();
        let n = got.len().min(c.output.len());
        if let Some(i) = (0..n).find(|&i| !ev_eq(&got[i], &c.output[i])) {
            bad.push(format!("{}: event {i}: rust {:?} elisp {:?}", c.name, got[i], c.output[i]));
        } else if got.len() != c.output.len() {
            bad.push(format!("{}: rust {} events, elisp {}", c.name, got.len(), c.output.len()));
        }
    }
    assert!(bad.is_empty(), "{} of {} cases differ:\n{}", bad.len(), cases.len(), bad.join("\n"));
    events
}

#[test]
fn clips_match_elisp_on_hand_made_cases() {
    let Some(text) = generate(true) else { return };
    let cases = parse_cases(&text);
    assert_eq!(cases.len(), 51);
    let n = check_cases(&cases);
    eprintln!("{} cases, {n} events identical", cases.len());
}

#[test]
fn clips_match_elisp_on_the_rotated_world() {
    let Some(text) = generate(false) else { return };
    let cases = parse_cases(&text);
    assert_eq!(cases.len(), 40);
    let n = check_cases(&cases);
    eprintln!("{} cases, {n} events identical", cases.len());
    // polygon_contains on the world's polygons.
    let mut polys: Vec<Vec<Vec<(f64, f64)>>> = Vec::new();
    let mut queries = 0;
    let mut bad = Vec::new();
    for line in text.lines() {
        let t: Vec<&str> = line.split_whitespace().collect();
        match t.first().copied() {
            Some("kpoly") => polys.push(Vec::new()),
            Some("kring") => polys.last_mut().unwrap().push(Vec::new()),
            Some("kp") => polys.last_mut().unwrap().last_mut().unwrap().push((pf(t[1]), pf(t[2]))),
            Some("kq") => {
                queries += 1;
                let want = t[3] == "1";
                let poly = polys.last().unwrap();
                if polygon_contains(poly, (pf(t[1]), pf(t[2]))) != want {
                    bad.push(format!("polygon {} point {} {}", polys.len() - 1, t[1], t[2]));
                }
            }
            _ => {}
        }
    }
    assert!(queries > 3000);
    assert!(bad.is_empty(), "polygon_contains differs:\n{}", bad.join("\n"));
}

#[test]
fn antimeridian_sphere_walks_the_whole_edge() {
    let got = run(clip_antimeridian, &[Ev::Sph]);
    assert_eq!(got.len(), 13);
    assert!(matches!(got[0], Ev::Ps) && matches!(got[12], Ev::Pe));
    assert!(ev_eq(&got[2], &Ev::P(-PI, HALF_PI, 0)));
}

#[test]
fn rectangle_cuts_a_line() {
    let got = run(|s| clip_rectangle(0.0, 0.0, 10.0, 10.0, s), &[Ev::Ls, Ev::P(-5.0, 5.0, 0), Ev::P(15.0, 5.0, 0), Ev::Le]);
    let want = [Ev::Ls, Ev::P(0.0, 5.0, 0), Ev::P(10.0, 5.0, 0), Ev::Le];
    assert_eq!(got.len(), want.len());
    assert!(got.iter().zip(want.iter()).all(|(a, b)| ev_eq(a, b)));
}

#[test]
fn stable_sort_keeps_ties_in_order() {
    let keys = [3.0, 1.0, 2.0, 1.0, 3.0, 0.5, 2.0];
    let mut idx: Vec<usize> = (0..keys.len()).collect();
    stable_sort(&mut idx, &|a, b| keys[a] < keys[b]);
    assert_eq!(idx, vec![5, 1, 3, 2, 6, 0, 4]);
}

const GEN_EL: &str = r##";;; gen.el --- reference events for the Rust clip tests -*- lexical-binding: t; -*-
(require 'eas-geo-clip)
(require 'eas-topojson)

(defvar gen-buf (generate-new-buffer " gen"))
(defun gen-emit (s) (with-current-buffer gen-buf (insert s "\n")))
(defun gen-f (x) (format "%S" (float x)))

(defun gen-sink (tag &optional next)
  "A stream printing events under TAG, forwarding them to NEXT."
  (eas-geo-stream--make
   :point (lambda (x y &optional m)
            (gen-emit (format "%s p %s %s %d" tag (gen-f x) (gen-f y) (or m 0)))
            (when next (eas-geo-point next x y m)))
   :line-start (lambda () (gen-emit (concat tag " ls")) (when next (eas-geo--call line-start next)))
   :line-end (lambda () (gen-emit (concat tag " le")) (when next (eas-geo--call line-end next)))
   :polygon-start (lambda () (gen-emit (concat tag " ps")) (when next (eas-geo--call polygon-start next)))
   :polygon-end (lambda () (gen-emit (concat tag " pe")) (when next (eas-geo--call polygon-end next)))
   :sphere (lambda () (gen-emit (concat tag " sph")) (when next (eas-geo--call sphere next)))))

(defvar gen-clips
  `(("anti" anti) ("c60" circle ,(* 60 eas-geo-rad)) ("c90" circle ,(* 90 eas-geo-rad))
    ("c142" circle ,(* 142 eas-geo-rad)) ("c179" circle ,(* 179.999 eas-geo-rad))
    ("c30" circle ,(* 30 eas-geo-rad)) ("c10" circle ,(* 10 eas-geo-rad))
    ("r1" rect -1.0 -0.6 1.3 0.9) ("rall" rect -3.2 -1.6 3.2 1.6) ("rsmall" rect 0.0 0.0 0.2 0.2)))

(defun gen-make-clip (spec sink)
  (pcase (car spec)
    ('anti (funcall (eas-geo-clip-antimeridian) sink))
    ('circle (funcall (eas-geo-clip-circle (nth 1 spec)) sink))
    ('rect (funcall (apply #'eas-geo-clip-rectangle (cdr spec)) sink))))

(defun gen-header (name spec)
  (gen-emit (format "case %s %s %s" name (car spec) (mapconcat #'gen-f (cdr spec) " "))))

(defun gen-star (cl cp n r1 r2 sx)
  "A closed star polygon ring around CL CP (degrees)."
  (let (pts)
    (dotimes (i (1+ n))
      (let* ((a (/ (* 2 float-pi (mod i n)) n)) (r (if (cl-oddp i) r1 r2)))
        (push (vector (+ cl (* sx r (cos a))) (+ cp (* r (sin a)))) pts)))
    (vconcat (nreverse pts))))

(defvar gen-hand
  (list
   '(:type "Polygon" :coordinates [[[170 10] [-170 10] [-170 -10] [170 -10] [170 10]]])
   '(:type "Polygon" :coordinates [[[0 80] [90 80] [180 80] [-90 80] [0 80]]])
   '(:type "Polygon" :coordinates [[[-170 75] [-50 75] [70 75] [-170 75]]])
   '(:type "Polygon" :coordinates [[[10 10] [20 10] [20 20] [10 20] [10 10]]])
   '(:type "Polygon" :coordinates [[[0 -60] [-90 -60] [180 -60] [90 -60] [0 -60]]])
   '(:type "Polygon" :coordinates [[[0 -60] [90 -60] [180 -60] [-90 -60] [0 -60]]])
   '(:type "Polygon" :coordinates [[[-120 -40] [120 -40] [120 40] [-120 40] [-120 -40]]
                                   [[-10 -10] [-10 10] [10 10] [10 -10] [-10 -10]]])
   '(:type "Polygon" :coordinates [[[-179 -89] [179 -89] [179 89] [-179 89] [-179 -89]]])
   '(:type "Polygon" :coordinates [[[0 0] [180 0] [0 90] [0 0]]])
   '(:type "Polygon" :coordinates [[[-30 -30] [30 -30] [30 30] [-30 30] [-30 -30]]])
   '(:type "Polygon" :coordinates [[[-80 -70] [80 -70] [80 70] [-80 70] [-80 -70]]])
   '(:type "Polygon" :coordinates [[[-30 -30] [-30 30] [30 30] [30 -30] [-30 -30]]])
   '(:type "MultiPolygon" :coordinates [[[[100 0] [110 0] [110 10] [100 10] [100 0]]]
                                        [[[-100 0] [-60 0] [-60 50] [-100 50] [-100 0]]]])
   '(:type "LineString" :coordinates [[170 0] [-170 5] [10 89] [-170 -80] [0 0] [180 0] [-180 10]])
   '(:type "LineString" :coordinates [[0 -90] [0 90]])
   '(:type "LineString" :coordinates [[-50 20] [50 20] [130 20] [170 -20] [-100 -40] [-20 70]])
   '(:type "MultiLineString" :coordinates [[[-100 0] [100 0]] [[-60 -50] [60 50] [150 -30]]])
   '(:type "MultiPoint" :coordinates [[0 0] [179 0] [100 50] [-150 -70] [0.2 0.1]])
   '(:type "Point" :coordinates [5 5])
   '(:type "Sphere")))

(defun gen-hand-all ()
  (append gen-hand
          (list (list :type "Polygon" :coordinates (vector (gen-star 0 0 36 30 75 1.5)))
                (list :type "Polygon" :coordinates (vector (gen-star 150 10 40 20 50 2)))
                (list :type "Polygon" :coordinates (vector (gen-star -20 -60 24 10 25 3)))
                (list :type "LineString" :coordinates (gen-star 60 0 50 10 85 2)))))

(defun gen-raw-feed (s)
  "Radian inputs with exact pi longitudes, straight into S."
  (let ((pi float-pi) (h eas-geo-half-pi))
    (cl-flet ((ring (pts) (eas-geo--call line-start s)
                (dolist (p pts) (eas-geo-point s (car p) (cadr p)))
                (eas-geo--call line-end s)))
      (eas-geo--call polygon-start s)
      (ring `((,pi 0.1) (,(- pi) 0.2) (,(- pi) -0.3) (,pi -0.2)))
      (eas-geo--call polygon-end s)
      (eas-geo--call polygon-start s)
      (ring `((0.0 ,h) (1.0 0.5) (2.0 0.4)))
      (eas-geo--call polygon-end s)
      (eas-geo--call polygon-start s)
      (ring `((3.0 0.2) (,(- pi) 0.25) (-3.0 0.2) (-3.0 -0.2) (3.0 -0.2)))
      (eas-geo--call polygon-end s)
      (eas-geo--call polygon-start s)
      (ring `((0.0 0.0) (0.0 0.0) (0.0 0.0)))
      (eas-geo--call polygon-end s)
      (eas-geo--call polygon-start s)
      (ring `((0.5 ,(- h)) (1.5 -1.0) (2.5 -1.2)))
      (eas-geo--call polygon-end s)
      (ring `((-1.0 0.0) (,pi 0.0) (-1.0 0.3) (2.0 ,h) (-2.0 -1.0)))
      (ring `((0.1 0.1)))
      (ring nil)
      (eas-geo--call polygon-start s)
      (ring `((1.0 0.2) (1.2 0.2) (1.2 0.4)))
      (ring `((-1.0 0.2) (-1.2 0.2) (-1.2 0.4)))
      (eas-geo--call polygon-end s))))

(defun gen-planar-feed (s)
  "Planar inputs for the rectangle, straight into S."
  (cl-flet ((ring (pts) (eas-geo--call line-start s)
              (dolist (p pts) (eas-geo-point s (float (car p)) (float (cadr p))))
              (eas-geo--call line-end s)))
    (dolist (poly '(((-10 -10) (110 -10) (110 60) (-10 60))
                    ((-10 -10) (-10 60) (110 60) (110 -10))
                    ((10 10) (20 10) (20 20))
                    ((-50 25) (50 -30) (150 25) (50 80))
                    ((-5e9 10) (5e9 20) (50 1e10))
                    ((0 0) (100 0) (100 50) (0 50))
                    ((200 200) (300 200) (300 300))
                    ((50 -10) (60 25) (70 -10) (80 25) (90 -10) (95 70) (5 70))))
      (eas-geo--call polygon-start s)
      (ring poly)
      (eas-geo--call polygon-end s))
    (eas-geo--call polygon-start s)
    (ring '((-20 -20) (120 -20) (120 70) (-20 70)))
    (ring '((40 10) (40 40) (60 40) (60 10)))
    (eas-geo--call polygon-end s)
    (ring '((-10 25) (110 25)))
    (ring '((-10 -10) (50 25) (50 26) (200 300) (-1e12 20) (30 30)))
    (ring '((0 50) (100 50) (100 0)))
    (eas-geo-point s 50.0 25.0)
    (eas-geo-point s 500.0 25.0)
    (eas-geo--call sphere s)))

(defvar gen-quick nil)

(defun gen-run (file quick)
  (setq gen-quick quick)
  (let* ((topo (eas-json-read-file (expand-file-name "../examples/data/vega/world-110m.json"
                                                     (file-name-directory (locate-library "eas-geo-clip")))))
         (world (eas-topojson-features topo "countries"))
         (hand (gen-hand-all))
         (rots '((0.0 0.0 0.0) (0.3 0.2 0.1) (1.0 -0.4 2.0) (3.0 1.2 -0.5))))
    (dolist (spec gen-clips)
      (let ((i 0))
        (dolist (rot rots)
          (dolist (src (if gen-quick (list (cons "hand" hand)) (list (cons "world" (append world nil)))))
            (gen-header (format "%s-%s-%d" (car spec) (car src) i) (cdr spec))
            (let* ((clip (gen-make-clip (cdr spec) (gen-sink "o")))
                   (head (eas-geo-radians-rotate (car (apply #'eas-geo-rotation rot)) (gen-sink "i" clip))))
              (dolist (g (cdr src)) (eas-geo-stream-object g head))))
          (setq i (1+ i))))
      (when quick
        (gen-header (format "%s-raw" (car spec)) (cdr spec))
        (gen-raw-feed (gen-sink "i" (gen-make-clip (cdr spec) (gen-sink "o"))))))
    (when quick
     (let ((rspec '(rect 0.0 0.0 100.0 50.0)))
      (gen-header "rplanar" rspec)
      (gen-planar-feed (gen-sink "i" (gen-make-clip rspec (gen-sink "o"))))))
    ;; polygon-contains on the rotated world.
    (unless quick
     (let* ((rot (car (eas-geo-rotation 0.3 0.2 0.1)))
           (pts (list (vector 0.0 0.0) (vector (- float-pi) (- eas-geo-half-pi)) (vector 0.5 0.3)
                      (vector 2.0 -1.0) (vector -2.5 0.7) (vector float-pi 0.0) (vector 0.0 eas-geo-half-pi)
                      (vector 1.0 (- eas-geo-half-pi)) (vector 4.0 0.2) (vector -0.3 0.9)))
           rings ring polys)
      (let ((rec (eas-geo-stream--make
                  :point (lambda (x y &optional _m) (push (vector x y) ring))
                  :line-start (lambda () (setq ring nil))
                  :line-end (lambda () (push (vconcat (nreverse ring)) rings))
                  :polygon-start (lambda () (setq rings nil))
                  :polygon-end (lambda () (push (nreverse rings) polys)))))
        (seq-doseq (g world) (eas-geo-stream-object g (eas-geo-radians-rotate rot rec))))
      (dolist (poly (nreverse polys))
        (gen-emit "kpoly")
        (dolist (r poly)
          (gen-emit "kring")
          (seq-doseq (p r) (gen-emit (format "kp %s %s" (gen-f (aref p 0)) (gen-f (aref p 1))))))
        (dolist (q (let* ((r (car poly)) (n (length r)) (sl 0.0) (sp 0.0))
                     (seq-doseq (p r) (setq sl (+ sl (aref p 0)) sp (+ sp (aref p 1))))
                     (append (list (aref r 0) (vector (/ sl n) (/ sp n))
                                   (vector (- (/ sl n) float-pi) (- (/ sp n))))
                             pts)))
          (gen-emit (format "kq %s %s %d" (gen-f (aref q 0)) (gen-f (aref q 1))
                            (if (eas-geo-polygon-contains poly q) 1 0)))))))
    (with-current-buffer gen-buf (write-region nil nil file nil 'silent))))
"##;
