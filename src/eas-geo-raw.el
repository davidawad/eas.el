;;; eas-geo-raw.el --- raw map projections, d3-geo and d3-geo-projection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 (eas-geo-proj.el builds projections from these).  Each
;; raw projection maps (LAMBDA PHI) in radians to [X Y] at unit scale,
;; north up, with d3's exact formulas.  `eas-geo-raw-types' describes
;; each type as d3 constructs it: default scale, clip angle, centre,
;; rotation and, for conics, the raw built from the two parallels.
;;
;; d3-geo: albers, azimuthalEqualArea, azimuthalEquidistant,
;; conicConformal, conicEqualArea, conicEquidistant, equalEarth,
;; equirectangular, gnomonic, mercator, naturalEarth1, orthographic,
;; stereographic, transverseMercator (albersUsa is a composite,
;; eas-geo-proj.el).  d3-geo-projection, as the Vega gallery registers
;; them: airy, aitoff, baker, berghaus, bottomley, collignon, eckert1,
;; hammer, littrow, mollweide, sinusoidal, wagner6, wiechel, winkel3
;; and the interrupted sinusoidal, Mollweide and Mollweide hemispheres.

;;; Code:

(require 'cl-lib)
(require 'eas-geo-stream)
(require 'eas-geo-polyhedral)

(defconst eas-geo--quarter-pi (/ float-pi 4) "Quarter of pi.")

(defun eas-geo--xy (x y) "The point [X Y]." (vector x y))

;;; d3-geo

(defun eas-geo-raw-azimuthal (scale)
  "D3's azimuthalRaw with radial SCALE (a function of cos x cos y)."
  (lambda (x y)
    (let* ((cx (cos x)) (cy (cos y)) (k (funcall scale (* cx cy))))
      (if (= k 1.0e+INF) (eas-geo--xy 2 0) (eas-geo--xy (* k cy (sin x)) (* k (sin y)))))))

(defconst eas-geo-raw-azimuthal-equal-area
  (eas-geo-raw-azimuthal (lambda (c) (if (= c -1) 1.0e+INF (sqrt (/ 2 (+ 1 c))))))
  "D3's azimuthalEqualAreaRaw.")

(defconst eas-geo-raw-azimuthal-equidistant
  (eas-geo-raw-azimuthal (lambda (c) (let ((c (eas-geo-acos c))) (if (= c 0) 0 (/ c (sin c))))))
  "D3's azimuthalEquidistantRaw.")

(defun eas-geo-raw-equirectangular (l p) "D3's equirectangularRaw of L P." (eas-geo--xy l p))

(defun eas-geo-raw-mercator (l p)
  "D3's mercatorRaw of L P."
  (eas-geo--xy l (log (tan (/ (+ eas-geo-half-pi p) 2)))))

(defun eas-geo-raw-transverse-mercator (l p)
  "D3's transverseMercatorRaw of L P."
  (eas-geo--xy (log (tan (/ (+ eas-geo-half-pi p) 2))) (- l)))

(defun eas-geo-raw-orthographic (x y) "D3's orthographicRaw of X Y." (eas-geo--xy (* (cos y) (sin x)) (sin y)))

(defun eas-geo-raw-gnomonic (x y)
  "D3's gnomonicRaw of X Y."
  (let* ((cy (cos y)) (k (* (cos x) cy))) (eas-geo--xy (/ (* cy (sin x)) k) (/ (sin y) k))))

(defun eas-geo-raw-stereographic (x y)
  "D3's stereographicRaw of X Y."
  (let* ((cy (cos y)) (k (+ 1 (* (cos x) cy)))) (eas-geo--xy (/ (* cy (sin x)) k) (/ (sin y) k))))

(defun eas-geo-raw-equal-earth (l p)
  "D3's equalEarthRaw of L P."
  (let* ((a1 1.340264) (a2 -0.081106) (a3 0.000893) (a4 0.003796) (m (/ (sqrt 3) 2))
         (th (eas-geo-asin (* m (sin p)))) (t2 (* th th)) (t6 (* t2 t2 t2)))
    (eas-geo--xy (/ (* l (cos th)) (* m (+ a1 (* 3 a2 t2) (* t6 (+ (* 7 a3) (* 9 a4 t2))))))
                 (* th (+ a1 (* a2 t2) (* t6 (+ a3 (* a4 t2))))))))

(defun eas-geo-raw-natural-earth1 (l p)
  "D3's naturalEarth1Raw of L P."
  (let* ((p2 (* p p)) (p4 (* p2 p2)))
    (eas-geo--xy (* l (+ 0.8707 (* -0.131979 p2) (* p4 (+ -0.013791 (* p4 (- (* 0.003971 p2) (* 0.001529 p4)))))))
                 (* p (+ 1.007226 (* p2 (+ 0.015085 (* p4 (+ -0.044475 (* 0.028874 p2) (* -0.005916 p4))))))))))

(defun eas-geo-raw-conic-equal-area (y0 y1)
  "D3's conicEqualAreaRaw for parallels Y0 Y1 (radians)."
  (let* ((sy0 (sin y0)) (n (/ (+ sy0 (sin y1)) 2)))
    (if (< (abs n) eas-geo-eps)
        (let ((cp (cos y0))) (lambda (l p) (eas-geo--xy (* l cp) (/ (sin p) cp))))
      (let* ((c (+ 1 (* sy0 (- (* 2 n) sy0)))) (r0 (/ (sqrt c) n)))
        (lambda (x y)
          (let ((r (/ (sqrt (max 0 (- c (* 2 n (sin y))))) n)) (x (* x n)))
            (eas-geo--xy (* r (sin x)) (- r0 (* r (cos x))))))))))

(defun eas-geo-raw-conic-conformal (y0 y1)
  "D3's conicConformalRaw for parallels Y0 Y1 (radians)."
  (let* ((tany (lambda (y) (tan (/ (+ eas-geo-half-pi y) 2))))
         (cy0 (cos y0))
         (n (if (= y0 y1) (sin y0) (/ (log (/ cy0 (cos y1))) (log (/ (funcall tany y1) (funcall tany y0))))))
         (f (and (/= n 0) (/ (* cy0 (expt (funcall tany y0) n)) n))))
    (if (= n 0) #'eas-geo-raw-mercator
      (lambda (x y)
        (if (> f 0) (when (< y (+ (- eas-geo-half-pi) eas-geo-eps)) (setq y (+ (- eas-geo-half-pi) eas-geo-eps)))
          (when (> y (- eas-geo-half-pi eas-geo-eps)) (setq y (- eas-geo-half-pi eas-geo-eps))))
        (let ((r (/ f (expt (funcall tany y) n))))
          (eas-geo--xy (* r (sin (* n x))) (- f (* r (cos (* n x))))))))))

(defun eas-geo-raw-conic-equidistant (y0 y1)
  "D3's conicEquidistantRaw for parallels Y0 Y1 (radians)."
  (let* ((cy0 (cos y0)) (n (if (= y0 y1) (sin y0) (/ (- cy0 (cos y1)) (- y1 y0)))))
    (if (< (abs n) eas-geo-eps) #'eas-geo-raw-equirectangular
      (let ((g (+ (/ cy0 n) y0)))
        (lambda (x y) (let ((gy (- g y)) (nx (* n x)))
                        (eas-geo--xy (* gy (sin nx)) (- g (* gy (cos nx))))))))))

;;; d3-geo-projection

(defun eas-geo--sinci (x) "X / sin X, 1 at 0." (if (= x 0) 1 (/ x (sin x))))

(defun eas-geo-raw-aitoff (x y)
  "D3's aitoffRaw of X Y."
  (let* ((cy (cos y)) (x (/ x 2.0)) (s (eas-geo--sinci (eas-geo-acos (* cy (cos x))))))
    (eas-geo--xy (* 2 cy (sin x) s) (* (sin y) s))))

(defun eas-geo-raw-winkel3 (l p)
  "D3's winkel3Raw of L P."
  (let ((c (eas-geo-raw-aitoff l p)))
    (eas-geo--xy (/ (+ (aref c 0) (/ l eas-geo-half-pi)) 2) (/ (+ (aref c 1) p) 2))))

(defun eas-geo-raw-hammer (l p)
  "D3's hammerRaw with B = 2 of L P."
  (let ((c (funcall eas-geo-raw-azimuthal-equal-area (/ l 2.0) p)))
    (eas-geo--xy (* 2 (aref c 0)) (aref c 1))))

(defun eas-geo-raw-mollweide (l p)
  "D3's mollweideRaw of L P."
  (let* ((cp float-pi) (cps (* cp (sin p))) (i 30) (delta 1.0))
    (while (and (> (abs delta) eas-geo-eps) (> i 0))
      (setq delta (/ (- (+ p (sin p)) cps) (+ 1 (cos p))) p (- p delta) i (1- i)))
    (let ((th (/ p 2)))
      (eas-geo--xy (* (/ (sqrt 2) eas-geo-half-pi) l (cos th)) (* (sqrt 2) (sin th))))))

(defun eas-geo-raw-sinusoidal (l p) "D3's sinusoidalRaw of L P." (eas-geo--xy (* l (cos p)) p))

(defun eas-geo-raw-wagner6 (l p)
  "D3's wagner6Raw of L P."
  (eas-geo--xy (* l (sqrt (- 1 (/ (* 3 p p) (* float-pi float-pi))))) p))

(defun eas-geo-raw-eckert1 (l p)
  "D3's eckert1Raw of L P."
  (let ((a (sqrt (/ 8 (* 3 float-pi))))) (eas-geo--xy (* a l (- 1 (/ (abs p) float-pi))) (* a p))))

(defun eas-geo-raw-collignon (l p)
  "D3's collignonRaw of L P."
  (let ((a (sqrt (- 1 (sin p)))) (sp (sqrt float-pi)))
    (eas-geo--xy (* (/ 2 sp) l a) (* sp (- 1 a)))))

(defun eas-geo-raw-baker (l p)
  "D3's bakerRaw of L P."
  (let ((p0 (abs p)) (s2 (sqrt 2)))
    (if (< p0 eas-geo--quarter-pi)
        (eas-geo--xy l (log (tan (+ eas-geo--quarter-pi (/ p 2)))))
      (eas-geo--xy (* l (cos p0) (- (* 2 s2) (/ 1 (sin p0))))
                   (* (eas-geo-sign p) (- (* 2 s2 (- p0 eas-geo--quarter-pi)) (log (tan (/ p0 2)))))))))

(defun eas-geo-raw-bottomley (l p)
  "D3's bottomleyRaw with sinPsi 0.5 of L P."
  (let* ((s 0.5) (rho (- eas-geo-half-pi p)) (eta (if (/= rho 0) (/ (* l s (sin rho)) rho) rho)))
    (eas-geo--xy (/ (* rho (sin eta)) s) (- eas-geo-half-pi (* rho (cos eta))))))

(defun eas-geo-raw-littrow (l p)
  "D3's littrowRaw of L P."
  (eas-geo--xy (/ (sin l) (cos p)) (* (tan p) (cos l))))

(defun eas-geo-raw-wiechel (l p)
  "D3's wiechelRaw of L P."
  (let* ((cp (cos p)) (sp (* (cos l) cp)) (s1 (- 1 sp))
         (l2 (atan (* (sin l) cp) (- (sin p)))) (cl (cos l2)) (sl (sin l2))
         (cp2 (sqrt (max 0 (- 1 (* sp sp))))))
    (eas-geo--xy (- (* sl cp2) (* cl s1)) (- (- (* cl cp2)) (* sl s1)))))

(defconst eas-geo--airy-b
  (let* ((beta eas-geo-half-pi) (tb (tan (/ beta 2)))) (/ (* 2 (log (cos (/ beta 2)))) (* tb tb)))
  "D3's airyRaw constant B for beta pi/2.")

(defun eas-geo-raw-airy (x y)
  "D3's airyRaw with beta pi/2 of X Y."
  (let* ((b eas-geo--airy-b)
         (cosz (* (cos y) (cos x)))
         (k (- (+ (if (/= (- 1 cosz) 0) (/ (log (/ (+ 1 cosz) 2)) (- 1 cosz)) -0.5) (/ b (+ 1 cosz))))))
    (eas-geo--xy (* k (cos y) (sin x)) (* k (sin y)))))

(defun eas-geo-raw-berghaus (l p)
  "D3's berghausRaw with 5 lobes of L P."
  (let* ((k (/ (* 2 float-pi) 5)) (pt (funcall eas-geo-raw-azimuthal-equidistant l p)))
    (if (<= (abs l) eas-geo-half-pi) pt
      (let* ((theta (atan (aref pt 1) (aref pt 0)))
             (r (sqrt (+ (expt (aref pt 0) 2) (expt (aref pt 1) 2))))
             (theta0 (+ (* k (fround (/ (- theta eas-geo-half-pi) k))) eas-geo-half-pi))
             (d (- theta theta0))
             (alpha (atan (sin d) (- 2 (cos d))))
             (theta (- (+ theta0 (eas-geo-asin (* (/ float-pi r) (sin alpha)))) alpha)))
        (eas-geo--xy (* r (cos theta)) (* r (sin theta)))))))

;;; Elliptic projections (d3-geo-projection elliptic.js, guyou.js, square.js, quincuncial)

(defun eas-geo-elliptic-f (phi m)
  "F(PHI|M), the incomplete elliptic integral of the first kind."
  (cond
   ((= m 0) phi)
   ((= m 1) (log (tan (+ (/ phi 2) eas-geo--quarter-pi))))
   (t (let ((a 1.0) (b (sqrt (- 1 m))) (c (sqrt m)) (i 0))
        (while (> (abs c) eas-geo-eps)
          (if (/= (eas-geo-rem phi float-pi) 0)
              (let ((dphi (atan (/ (* b (tan phi)) a))))
                (when (< dphi 0) (setq dphi (+ dphi float-pi)))
                (setq phi (+ phi dphi (* (ftruncate (/ phi float-pi)) float-pi))))
            (setq phi (+ phi phi)))
          (setq c (/ (+ a b) 2) b (sqrt (* a b)) a c c (/ (- a b) 2) i (1+ i)))
        (/ phi (* (expt 2.0 i) a))))))

(defun eas-geo-elliptic-fi (phi psi m)
  "F(PHI + i PSI|M) as [RE IM] (Abramowitz and Stegun 17.4.11)."
  (let ((r (abs phi)) (sh (let ((a (abs psi))) (/ (- (exp a) (exp (- a))) 2))))
    (if (/= r 0)
        (let* ((csc (/ 1 (sin r))) (cot2 (/ 1 (* (tan r) (tan r))))
               (b (- (+ cot2 (* m sh sh csc csc) -1 m)))
               (c (* (- m 1) cot2))
               (cotl2 (/ (+ (- b) (sqrt (- (* b b) (* 4 c)))) 2)))
          (vector (* (eas-geo-elliptic-f (atan (/ 1 (sqrt cotl2))) m) (eas-geo-sign phi))
                  (* (eas-geo-elliptic-f (atan (sqrt (max 0 (/ (- (/ cotl2 cot2) 1) m)))) (- 1 m)) (eas-geo-sign psi))))
      (vector 0 (* (eas-geo-elliptic-f (atan sh) (- 1 m)) (eas-geo-sign psi))))))

(defconst eas-geo--guyou
  (let* ((s2 (sqrt 2)) (k_ (/ (- s2 1) (+ s2 1))) (k (sqrt (- 1 (* k_ k_)))))
    (vector k_ k (eas-geo-elliptic-f eas-geo-half-pi (* k k)) (sqrt k_) (* k k)))
  "D3's guyouRaw constants: [K_ K F(pi/2|K^2) sqrt(K_) K^2].")

(defun eas-geo-raw-guyou (l p)
  "D3's guyouRaw of L P."
  (let* ((kk (aref eas-geo--guyou 2))
         (psi (log (tan (+ (/ float-pi 4) (/ (abs p) 2)))))
         (r (/ (exp (- psi)) (aref eas-geo--guyou 3)))
         (x (* r (cos (- l)))) (y (* r (sin (- l))))
         (x2 (* x x)) (y1 (+ y 1)) (tt (- 1 x2 (* y y)))
         (at (vector (* 0.5 (- (if (>= x 0) eas-geo-half-pi (- eas-geo-half-pi)) (atan tt (* 2 x))))
                     (+ (* -0.25 (log (+ (* tt tt) (* 4 x2)))) (* 0.5 (log (+ (* y1 y1) x2))))))
         (fi (eas-geo-elliptic-fi (aref at 0) (aref at 1) (aref eas-geo--guyou 4))))
    (eas-geo--xy (- (aref fi 1)) (* (if (>= p 0) 1 -1) (- (* 0.5 kk) (aref fi 0))))))

(defun eas-geo-raw-square (raw)
  "D3-geo-projection's square() of RAW."
  (let ((dx (- (aref (funcall raw eas-geo-half-pi 0) 0) (aref (funcall raw (- eas-geo-half-pi) 0) 0))))
    (lambda (l p)
      (let* ((s (if (> l 0) -0.5 0.5)) (pt (funcall raw (+ l (* s float-pi)) p)))
        (eas-geo--xy (- (aref pt 0) (* s dx)) (aref pt 1))))))

(defun eas-geo-raw-quincuncial (raw)
  "D3-geo-projection's quincuncial() of RAW."
  (let ((dx (- (aref (funcall raw eas-geo-half-pi 0) 0) (aref (funcall raw (- eas-geo-half-pi) 0) 0)))
        (r (sqrt 0.5)))
    (lambda (l p)
      (let* ((tt (< (abs l) eas-geo-half-pi))
             (pt (funcall raw (cond (tt l) ((> l 0) (- l float-pi)) (t (+ l float-pi))) p))
             (x (* (- (aref pt 0) (aref pt 1)) r)) (y (* (+ (aref pt 0) (aref pt 1)) r)))
        (if tt (eas-geo--xy x y)
          (let ((d (* dx r)) (s (if (not (eq (> x 0) (> y 0))) -1 1)))
            (eas-geo--xy (- (* s x) (* (eas-geo-sign y) d)) (- (* s y) (* (eas-geo-sign x) d)))))))))

(defconst eas-geo--armadillo
  (let* ((phi0 (* 20 eas-geo-rad)) (s0 (sin phi0)) (c0 (cos phi0)))
    (vector s0 c0 (tan phi0) (/ (- (+ 1 s0) c0) 2)))
  "D3's armadilloRaw constants at parallel 20 degrees: [S0 C0 TAN0 K].")

(defun eas-geo-raw-armadillo (l p)
  "D3's armadilloRaw with parallel 20 degrees of L P."
  (let* ((s0 (aref eas-geo--armadillo 0)) (c0 (aref eas-geo--armadillo 1))
         (tan0 (aref eas-geo--armadillo 2)) (k (aref eas-geo--armadillo 3)) (cp (cos p)) (l (/ l 2.0)) (cl (cos l)))
    (eas-geo--xy (* (+ 1 cp) (sin l))
                 (+ (if (> p (- (- (atan cl tan0)) 1e-3)) 0 -10)
                    k (* (sin p) c0) (- (* (+ 1 cp) s0 cl))))))

(defun eas-geo-raw--armadillo-sphere (sink)
  "Stream d3's armadillo outline (degrees, unrotated) into SINK."
  (let ((tan0 (tan (* 20 eas-geo-rad))) (lam -180))
    (eas-geo--call polygon-start sink)
    (eas-geo--call line-start sink)
    (while (< lam 180) (eas-geo-point sink lam 90) (setq lam (+ lam 90)))
    (while (>= (setq lam (- lam (* 3 (sqrt 0.5)))) -180)
      (eas-geo-point sink lam (/ (- (atan (cos (/ (* lam eas-geo-rad) 2)) tan0)) eas-geo-rad)))
    (eas-geo--call line-end sink)
    (eas-geo--call polygon-end sink)))

;;; Interrupted projections (d3-geo-projection interrupted/index.js)

(defun eas-geo-raw-interrupt (raw lobes)
  "RAW interrupted along LOBES (degrees), d3's interrupt forward."
  (let ((lobes (mapcar (lambda (hemi) (mapcar (lambda (l) (mapcar (lambda (pt) (vector (* (aref pt 0) eas-geo-rad) (* (aref pt 1) eas-geo-rad))) l)) hemi)) lobes)))
    (lambda (l p)
      (let* ((sign (if (< p 0) -1 1)) (lobe (nth (if (< p 0) 1 0) lobes))
             (i 0) (n (1- (length lobe))))
        (while (and (< i n) (> l (aref (nth 2 (nth i lobe)) 0))) (setq i (1+ i)))
        (let* ((lb (nth i lobe)) (mid (aref (nth 1 lb) 0)) (top (aref (nth 0 lb) 1))
               (pt (funcall raw (- l mid) p))
               (off (funcall raw mid (if (> (* sign p) (* sign top)) top p))))
          (eas-geo--xy (+ (aref pt 0) (aref off 0)) (aref pt 1)))))))

(defun eas-geo-raw-interrupt-sphere (lobes)
  "The outline polygon (degrees) of an interrupted projection with LOBES."
  (let* ((e eas-geo-eps) out
         (interp (lambda (pts m)
                   (let ((res nil) (p0 (car pts)))
                     (dolist (p1 pts)
                       (let ((dx (/ (- (aref p1 0) (aref p0 0)) m)) (dy (/ (- (aref p1 1) (aref p0 1)) m)))
                         (dotimes (j m) (push (vector (+ (aref p0 0) (* j dx)) (+ (aref p0 1) (* j dy))) res)))
                       (setq p0 p1))
                     (push (car (last pts)) res)
                     (nreverse res)))))
    (dolist (lb (nth 0 lobes))
      (let ((l0 (aref (nth 0 lb) 0)) (p0 (aref (nth 0 lb) 1)) (p1 (aref (nth 1 lb) 1))
            (l2 (aref (nth 2 lb) 0)) (p2 (aref (nth 2 lb) 1)))
        (setq out (append out (funcall interp (list (vector (+ l0 e) (+ p0 e)) (vector (+ l0 e) (- p1 e))
                                                    (vector (- l2 e) (- p1 e)) (vector (- l2 e) (+ p2 e)))
                                       30)))))
    (dolist (lb (reverse (nth 1 lobes)))
      (let ((l0 (aref (nth 0 lb) 0)) (p0 (aref (nth 0 lb) 1)) (p1 (aref (nth 1 lb) 1))
            (l2 (aref (nth 2 lb) 0)) (p2 (aref (nth 2 lb) 1)))
        (setq out (append out (funcall interp (list (vector (- l2 e) (- p2 e)) (vector (- l2 e) (+ p1 e))
                                                    (vector (+ l0 e) (+ p1 e)) (vector (+ l0 e) (- p0 e)))
                                       30)))))
    (list :type "Polygon" :coordinates (vector (vconcat out)))))

(defconst eas-geo-raw--lobes-sinusoidal
  '(((-180 0) (-110 90) (-40 0)) ((-40 0) (0 90) (40 0)) ((40 0) (110 90) (180 0))
    ((-180 0) (-110 -90) (-40 0)) ((-40 0) (0 -90) (40 0)) ((40 0) (110 -90) (180 0)))
  "Interrupted sinusoidal lobes: three north, three south.")

(defun eas-geo-raw--lobes (flat north)
  "FLAT lobe triples, the first NORTH of them northern, as d3's lobes."
  (let ((v (mapcar (lambda (l) (mapcar (lambda (p) (vector (car p) (cadr p))) l)) flat)))
    (list (seq-take v north) (seq-drop v north))))

(defconst eas-geo-raw-lobes
  (list (cons "interruptedSinusoidal" (eas-geo-raw--lobes eas-geo-raw--lobes-sinusoidal 3))
        (cons "interruptedMollweide"
              (eas-geo-raw--lobes '(((-180 0) (-100 90) (-40 0)) ((-40 0) (30 90) (180 0))
                                    ((-180 0) (-160 -90) (-100 0)) ((-100 0) (-60 -90) (-20 0))
                                    ((-20 0) (20 -90) (80 0)) ((80 0) (140 -90) (180 0)))
                                  2))
        (cons "interruptedMollweideHemispheres"
              (eas-geo-raw--lobes '(((-180 0) (-90 90) (0 0)) ((0 0) (90 90) (180 0))
                                    ((-180 0) (-90 -90) (0 0)) ((0 0) (90 -90) (180 0)))
                                  2)))
  "Lobes (degrees) of the interrupted projections.")

;;; The registry

(defvar eas-geo-raw--butterfly nil
  "The polyhedral butterfly's (RAW . OUTLINE), built on first use.")

(defun eas-geo-raw--butterfly ()
  "The polyhedral butterfly's (RAW . OUTLINE)."
  (or eas-geo-raw--butterfly (setq eas-geo-raw--butterfly (eas-geo-polyhedral-butterfly))))

(defun eas-geo-raw-polyhedral-butterfly (l p)
  "D3's polyhedralButterfly raw projection of L P."
  (funcall (car (eas-geo-raw--butterfly)) l p))

(defun eas-geo-raw--butterfly-sphere (sink)
  "Stream the polyhedral butterfly's outline (degrees) into SINK."
  (funcall (cdr (eas-geo-raw--butterfly)) sink))

(defconst eas-geo-raw-types
  `(("albers" :conic eas-geo-raw-conic-equal-area :parallels [29.5 45.5] :scale 1070
     :translate [480 250] :rotate [96 0] :center [-0.6 38.7])
    ("azimuthalEqualArea" :raw ,eas-geo-raw-azimuthal-equal-area :scale 124.75 :clip-angle ,(- 180 1e-3))
    ("azimuthalEquidistant" :raw ,eas-geo-raw-azimuthal-equidistant :scale 79.4188 :clip-angle ,(- 180 1e-3))
    ("conicConformal" :conic eas-geo-raw-conic-conformal :parallels [30 30] :scale 109.5)
    ("conicEqualArea" :conic eas-geo-raw-conic-equal-area :parallels [0 60] :scale 155.424 :center [0 33.6442])
    ("conicEquidistant" :conic eas-geo-raw-conic-equidistant :parallels [0 60] :scale 131.154 :center [0 13.9389])
    ("equalEarth" :raw eas-geo-raw-equal-earth :scale 177.158)
    ("equirectangular" :raw eas-geo-raw-equirectangular :scale 152.63)
    ("gnomonic" :raw eas-geo-raw-gnomonic :scale 144.049 :clip-angle 60)
    ("mercator" :raw eas-geo-raw-mercator :scale ,(/ 961 eas-geo-tau) :reclip mercator)
    ("naturalEarth1" :raw eas-geo-raw-natural-earth1 :scale 175.295)
    ("orthographic" :raw eas-geo-raw-orthographic :scale 249.5 :clip-angle ,(+ 90 eas-geo-eps))
    ("stereographic" :raw eas-geo-raw-stereographic :scale 250 :clip-angle 142)
    ("transverseMercator" :raw eas-geo-raw-transverse-mercator :scale 159.155 :reclip transverse
     :rotate-gamma 90)
    ;; d3-geo-projection, as the Vega gallery registers them
    ("airy" :raw eas-geo-raw-airy :scale 179.976 :clip-angle 147)
    ("aitoff" :raw eas-geo-raw-aitoff :scale 158.837)
    ("armadillo" :raw eas-geo-raw-armadillo :scale 218.695 :center [0 28.0974] :sphere armadillo)
    ("guyou" :raw ,(eas-geo-raw-square #'eas-geo-raw-guyou) :scale 151.496)
    ("polyhedralButterfly" :raw eas-geo-raw-polyhedral-butterfly :scale 101.858 :center [0 45] :angle -30
     :sphere polyhedral)
    ("peirceQuincuncial" :raw ,(eas-geo-raw-quincuncial #'eas-geo-raw-guyou) :scale 111.48
     :rotate [-90 -90 45] :clip-angle ,(- 180 1e-3))
    ("baker" :raw eas-geo-raw-baker :scale 112.314)
    ("berghaus" :raw eas-geo-raw-berghaus :scale 87.8076 :center [0 17.1875] :clip-angle ,(- 180 1e-3)
     :sphere berghaus)
    ("bottomley" :raw eas-geo-raw-bottomley :scale 158.837)
    ("collignon" :raw eas-geo-raw-collignon :scale 95.6464 :center [0 30])
    ("eckert1" :raw eas-geo-raw-eckert1 :scale 165.664)
    ("hammer" :raw eas-geo-raw-hammer :scale 169.529)
    ("littrow" :raw eas-geo-raw-littrow :scale 144.049 :clip-angle ,(- 90 1e-3))
    ("mollweide" :raw eas-geo-raw-mollweide :scale 169.529)
    ("sinusoidal" :raw eas-geo-raw-sinusoidal :scale 152.63)
    ("wagner6" :raw eas-geo-raw-wagner6 :scale 152.63)
    ("wiechel" :raw eas-geo-raw-wiechel :scale 124.75 :rotate [0 -90 45] :clip-angle ,(- 180 1e-3))
    ("winkel3" :raw eas-geo-raw-winkel3 :scale 158.837)
    ("interruptedSinusoidal" :interrupt eas-geo-raw-sinusoidal :scale 152.63 :rotate [-20 0])
    ("interruptedMollweide" :interrupt eas-geo-raw-mollweide :scale 169.529)
    ("interruptedMollweideHemispheres" :interrupt eas-geo-raw-mollweide :scale 169.529 :rotate [20 0]))
  "Projection types: (NAME . PLIST) as d3 constructs them.
:raw is the raw function (or a symbol naming it); :conic a function of
the parallels (radians) returning one; :interrupt a raw interrupted
along the type's `eas-geo-raw-lobes'.  :scale, :clip-angle (degrees),
:center, :rotate, :parallels and :translate are d3's defaults; :reclip
names mercator's automatic clip extent; :sphere an outline of its own.")

(defun eas-geo-raw-type (name)
  "The registry entry of projection type NAME, or nil."
  (cdr (assoc name eas-geo-raw-types)))

(defun eas-geo-raw-names ()
  "Every projection type drawn natively, albersUsa included."
  (cons "albersUsa" (mapcar #'car eas-geo-raw-types)))

(provide 'eas-geo-raw)
;;; eas-geo-raw.el ends here
