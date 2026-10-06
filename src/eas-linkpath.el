;;; eas-linkpath.el --- link geometry between two points: line, curve, diagonal, orthogonal -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega's linkpath: the path from a source point to a
;; target point, for the edges of node-link diagrams.  SHAPE is one of
;; `eas-linkpath-shapes' and ORIENT one of `eas-linkpath-orients':
;;
;;   line        a straight segment
;;   arc         a half circle over the segment
;;   curve       a cubic leaning off the segment
;;   diagonal    a cubic leaving and entering along the orient's axis
;;               (the classic tree link)
;;   orthogonal  two axis-parallel legs (radial: an arc, then a spoke)
;;
;; With orient "radial", x is an angle in radians (0 along +x, growing
;; clockwise on screen) and y a radius, both about the origin.
;;
;; The API, for any mark that wants a link:
;;
;;   (eas-linkpath-path SHAPE ORIENT SX SY TX TY)    Vega's SVG path data
;;   (eas-linkpath-points SHAPE ORIENT SX SY TX TY)  the same path as a
;;       polyline ((X Y) ...), curves sampled, for line marks and text
;;
;; and the x-eas domain transform, which turns each link row into the
;; vertex rows a Vega-Lite line mark draws (detail by link, order by
;; step):
;;
;;   {"x-eas:transform": "linkpath", "shape": "diagonal",
;;    "orient": "horizontal", "sourceX": "source.x", "sourceY": "source.y",
;;    "targetX": "target.x", "targetY": "target.y", "origin": [0, 0],
;;    "as": ["x", "y", "step", "link"]}
;;
;; Dotted fields read into nested objects (treelinks's source and
;; target rows); ORIGIN translates every point (Vega places a radial
;; path with the mark's x and y).  Each vertex row keeps its link row's
;; scalar fields; nested objects are left out.

;;; Code:

(require 'eas-core)
(require 'eas-transform-domain)

(defconst eas-linkpath-shapes '("line" "arc" "curve" "diagonal" "orthogonal")
  "Link shapes, as Vega names them.")

(defconst eas-linkpath-orients '("vertical" "horizontal" "radial")
  "Link orientations, as Vega names them.")

(defconst eas-linkpath-samples 12
  "Polyline segments per cubic or per half turn of arc.")

;;; Geometry

(defun eas-linkpath--num (v)
  "V as JavaScript prints a number."
  (cond ((and (floatp v) (= v (ffloor v)) (< (abs v) 1e15)) (format "%d" (truncate v)))
        (t (number-to-string v))))

(defun eas-linkpath--radial (a r)
  "The point at angle A (radians) and radius R, as (X Y)."
  (list (* r (cos a)) (* r (sin a))))

(defun eas-linkpath--wrap (d)
  "Angle D wrapped into (-pi, pi]."
  (let ((w (- d (* 2 float-pi (ffloor (/ (+ d float-pi) (* 2 float-pi)))))))
    (if (<= w (- float-pi)) (+ w (* 2 float-pi)) w)))

(defun eas-linkpath--kind (shape orient)
  "The geometry SHAPE takes under ORIENT, Vega's lookup: a symbol."
  (unless (member shape eas-linkpath-shapes)
    (eas-signal "INVALID_INPUT" (format "linkpath shape %S is not one of %s" shape
                                        (string-join eas-linkpath-shapes ", "))
                :path "shape"))
  (unless (member orient eas-linkpath-orients)
    (eas-signal "INVALID_INPUT" (format "linkpath orient %S is not one of %s" orient
                                        (string-join eas-linkpath-orients ", "))
                :path "orient"))
  (intern (if (or (equal orient "radial") (member shape '("diagonal" "orthogonal")))
              (concat shape "-" orient)
            shape)))

(defun eas-linkpath--cubics (kind sx sy tx ty)
  "Path of link KIND from SX SY to TX TY as segments.
Each is (line X Y), (cubic X1 Y1 X2 Y2 X Y) or (arc R CX CY A0 DA X Y)
\(a circular arc about CX CY), after a start point: (START SEGMENTS)."
  (pcase kind
    ((or 'line-radial 'arc-radial 'curve-radial)
     (let ((s (eas-linkpath--radial sx sy)) (e (eas-linkpath--radial tx ty)))
       (eas-linkpath--cubics (intern (string-remove-suffix "-radial" (symbol-name kind)))
                             (nth 0 s) (nth 1 s) (nth 0 e) (nth 1 e))))
    ('line (list (list sx sy) (list (list 'line tx ty))))
    ('arc (let* ((dx (- tx sx)) (dy (- ty sy)) (rr (/ (sqrt (+ (* dx dx) (* dy dy))) 2.0))
                 (cx (/ (+ sx tx) 2.0)) (cy (/ (+ sy ty) 2.0)))
            (list (list sx sy) (list (list 'arc rr cx cy (atan (- sy cy) (- sx cx)) float-pi tx ty)))))
    ('curve (let* ((dx (- tx sx)) (dy (- ty sy)) (ix (* 0.2 (+ dx dy))) (iy (* 0.2 (- dy dx))))
              (list (list sx sy) (list (list 'cubic (+ sx ix) (+ sy iy) (+ tx iy) (- ty ix) tx ty)))))
    ('orthogonal-horizontal (list (list sx sy) (list (list 'line sx ty) (list 'line tx ty))))
    ('orthogonal-vertical (list (list sx sy) (list (list 'line tx sy) (list 'line tx ty))))
    ('orthogonal-radial
     (let ((s (eas-linkpath--radial sx sy)) (e (eas-linkpath--radial tx sy)) (f (eas-linkpath--radial tx ty)))
       (list s (list (list 'arc sy 0 0 sx (eas-linkpath--wrap (- tx sx)) (nth 0 e) (nth 1 e))
                     (list 'line (nth 0 f) (nth 1 f))))))
    ('diagonal-horizontal (let ((m (/ (+ sx tx) 2.0)))
                            (list (list sx sy) (list (list 'cubic m sy m ty tx ty)))))
    ('diagonal-vertical (let ((m (/ (+ sy ty) 2.0)))
                          (list (list sx sy) (list (list 'cubic sx m tx m tx ty)))))
    ('diagonal-radial
     (let ((mr (/ (+ sy ty) 2.0)))
       (list (eas-linkpath--radial sx sy)
             (list (append '(cubic) (eas-linkpath--radial sx mr) (eas-linkpath--radial tx mr)
                           (eas-linkpath--radial tx ty))))))))

(defun eas-linkpath-path (shape orient sx sy tx ty)
  "Vega's SVG path data for a SHAPE link under ORIENT from SX SY to TX TY.
See the commentary for the shapes and orients."
  (let* ((kind (eas-linkpath--kind shape orient)) (n #'eas-linkpath--num)
         (geo (eas-linkpath--cubics kind sx sy tx ty)) (start (car geo)))
    (concat
     "M" (funcall n (nth 0 start)) "," (funcall n (nth 1 start))
     (pcase kind
       ;; Vega writes these two with their own commands.
       ('orthogonal-horizontal (concat "V" (funcall n ty) "H" (funcall n tx)))
       ('orthogonal-vertical (concat "H" (funcall n tx) "V" (funcall n ty)))
       (_ (mapconcat
           (lambda (seg)
             (pcase seg
               (`(line ,x ,y) (concat "L" (funcall n x) "," (funcall n y)))
               (`(cubic ,x1 ,y1 ,x2 ,y2 ,x ,y)
                (format "C%s,%s %s,%s %s,%s" (funcall n x1) (funcall n y1) (funcall n x2) (funcall n y2)
                        (funcall n x) (funcall n y)))
               (`(arc ,r ,_cx ,_cy ,_a0 ,da ,x ,y)
                (if (eq kind 'orthogonal-radial)
                    (format "A%s,%s 0 0,%d %s,%s" (funcall n r) (funcall n r) (if (> da 0) 1 0)
                            (funcall n x) (funcall n y))
                  (format "A%s,%s %s 0 1 %s,%s" (funcall n r) (funcall n r)
                          (funcall n (/ (* 180 (atan (- y (nth 1 start)) (- x (nth 0 start)))) float-pi))
                          (funcall n x) (funcall n y))))))
           (cadr geo) ""))))))

(defun eas-linkpath-points (shape orient sx sy tx ty)
  "The SHAPE link under ORIENT from SX SY to TX TY as a polyline ((X Y) ...).
Cubics take `eas-linkpath-samples' segments and arcs as many per half
turn, so line marks draw what `eas-linkpath-path' describes."
  (let* ((geo (eas-linkpath--cubics (eas-linkpath--kind shape orient) sx sy tx ty))
         (pts (list (car geo))) (k eas-linkpath-samples))
    (dolist (seg (cadr geo))
      (let ((p (car pts)))
        (pcase seg
          (`(line ,x ,y) (push (list x y) pts))
          (`(cubic ,x1 ,y1 ,x2 ,y2 ,x ,y)
           (cl-loop for i from 1 to k
                    for u = (/ i (float k)) for v = (- 1 u)
                    do (push (list (+ (* v v v (nth 0 p)) (* 3 v v u x1) (* 3 v u u x2) (* u u u x))
                                   (+ (* v v v (nth 1 p)) (* 3 v v u y1) (* 3 v u u y2) (* u u u y)))
                             pts)))
          (`(arc ,r ,cx ,cy ,a0 ,da ,x ,y)
           (let ((m (max 1 (ceiling (* k (/ (abs da) float-pi))))))
             (cl-loop for i from 1 below m
                      for a = (+ a0 (* da (/ i (float m))))
                      do (push (list (+ cx (* r (cos a))) (+ cy (* r (sin a)))) pts))
             (push (list x y) pts))))))
    (nreverse pts)))

;;; The transform

(defun eas-linkpath--get (row field)
  "ROW's FIELD, reading into nested objects at each dot."
  (let ((v row))
    (dolist (part (split-string field "\\.") v)
      (setq v (and (eas-object-p v) (plist-get v (eas-key part)))))))

(defun eas-linkpath--scalars (row)
  "ROW without its nested objects and arrays."
  (cl-loop for (k v) on row by #'cddr
           unless (or (vectorp v) (and (consp v) (keywordp (car v))))
           append (list k v)))

(defun eas-linkpath--rows (rows params)
  "The linkpath transform: each of ROWS as the vertex rows of its link.
PARAMS: shape, orient, sourceX..targetY fields, origin and as."
  (let* ((shape (plist-get params :shape)) (orient (plist-get params :orient))
         (fields (mapcar (lambda (k) (plist-get params k)) '(:sourceX :sourceY :targetX :targetY)))
         (origin (plist-get params :origin)) (ox (aref origin 0)) (oy (aref origin 1))
         (as (mapcar #'eas-key (append (plist-get params :as) nil)))
         out)
    (eas-linkpath--kind shape orient)
    (seq-do-indexed
     (lambda (row i)
       (let ((coords (mapcar (lambda (f) (eas-linkpath--get row f)) fields)))
         (when (seq-every-p #'numberp coords)
           (let ((base (eas-linkpath--scalars row)))
             (cl-loop for (x y) in (apply #'eas-linkpath-points shape orient coords) for j from 0
                      do (push (append (list (nth 0 as) (+ ox x) (nth 1 as) (+ oy y) (nth 2 as) j (nth 3 as) i)
                                       base)
                               out))))))
     rows)
    (vconcat (nreverse out))))

(eas-register-transform
 "linkpath"
 :doc "Each link row as the vertex rows of its path (line, arc, curve, diagonal, orthogonal; radial), for a line mark."
 :schema '(:shape (:type "string" :default "line" :doc "line, arc, curve, diagonal or orthogonal")
           :orient (:type "string" :default "vertical" :doc "vertical, horizontal or radial (x angle, y radius)")
           :sourceX (:type "string" :default "source.x" :doc "the source's x (dots read nested objects)")
           :sourceY (:type "string" :default "source.y")
           :targetX (:type "string" :default "target.x")
           :targetY (:type "string" :default "target.y")
           :origin (:type "array" :default [0 0] :doc "[x y] added to every point (a radial layout's centre)")
           :as (:type "array" :default ["x" "y" "step" "link"]
                :doc "fields for each vertex's x and y, its step along the link and the link's index"))
 :fn #'eas-linkpath--rows)

(provide 'eas-linkpath)
;;; eas-linkpath.el ends here
