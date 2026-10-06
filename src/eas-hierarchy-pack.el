;;; eas-hierarchy-pack.el --- circle packing of hierarchies -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega's pack transform, as an x-eas domain transform
;; over id/parent rows (eas-hierarchy.el):
;;
;;   {"x-eas:transform": "pack", "field": "size", "size": [W, H],
;;    "padding": 0, "radius": null, "as": ["x", "y", "r", "depth", "children"]}
;;
;; d3's pack: a leaf's radius is sqrt(value) (or the RADIUS field), each
;; node's children are packed by Wang et al.'s front-chain placement and
;; wrapped in their smallest enclosing circle (Welzl's algorithm, shuffled
;; by d3's linear congruential generator so the result is d3's to the
;; last bit), then the whole is scaled to fit SIZE, centred.

;;; Code:

(require 'eas-core)
(require 'eas-hierarchy)

;;; Circles: a vector [X Y R], the nodes' own slots once placed

(defun eas-hierarchy--lcg ()
  "The linear congruential generator of d3: a function returning [0, 1)."
  (let ((s 1))
    (lambda () (setq s (mod (+ (* 1664525 s) 1013904223) 4294967296)) (/ s 4294967296.0))))

(defsubst eas-hierarchy--cx (c) "C's x." (aref c 0))
(defsubst eas-hierarchy--cy (c) "C's y." (aref c 1))
(defsubst eas-hierarchy--cr (c) "C's radius." (aref c 2))

(defun eas-hierarchy--encloses-not (a b)
  "Non-nil when circle A does not enclose circle B."
  (let ((dr (- (eas-hierarchy--cr a) (eas-hierarchy--cr b)))
        (dx (- (eas-hierarchy--cx b) (eas-hierarchy--cx a))) (dy (- (eas-hierarchy--cy b) (eas-hierarchy--cy a))))
    (or (< dr 0) (< (* dr dr) (+ (* dx dx) (* dy dy))))))

(defun eas-hierarchy--encloses-weak (a b)
  "Non-nil when circle A encloses circle B, within a relative epsilon."
  (let ((dr (+ (- (eas-hierarchy--cr a) (eas-hierarchy--cr b))
               (* (max (eas-hierarchy--cr a) (eas-hierarchy--cr b) 1) 1e-9)))
        (dx (- (eas-hierarchy--cx b) (eas-hierarchy--cx a))) (dy (- (eas-hierarchy--cy b) (eas-hierarchy--cy a))))
    (and (> dr 0) (> (* dr dr) (+ (* dx dx) (* dy dy))))))

(defun eas-hierarchy--encloses-weak-all (a basis)
  "Non-nil when circle A weakly encloses every circle in BASIS."
  (cl-every (lambda (b) (eas-hierarchy--encloses-weak a b)) basis))

(defun eas-hierarchy--enclose2 (a b)
  "The smallest circle enclosing circles A and B."
  (let* ((x1 (eas-hierarchy--cx a)) (y1 (eas-hierarchy--cy a)) (r1 (eas-hierarchy--cr a))
         (x2 (eas-hierarchy--cx b)) (y2 (eas-hierarchy--cy b)) (r2 (eas-hierarchy--cr b))
         (x21 (- x2 x1)) (y21 (- y2 y1)) (r21 (- r2 r1)) (l (sqrt (+ (* x21 x21) (* y21 y21)))))
    (vector (/ (+ x1 x2 (* (/ x21 l) r21)) 2) (/ (+ y1 y2 (* (/ y21 l) r21)) 2) (/ (+ l r1 r2) 2))))

(defun eas-hierarchy--enclose3 (a b c)
  "The smallest circle enclosing circles A, B and C (each touching it)."
  (let* ((x1 (eas-hierarchy--cx a)) (y1 (eas-hierarchy--cy a)) (r1 (eas-hierarchy--cr a))
         (x2 (eas-hierarchy--cx b)) (y2 (eas-hierarchy--cy b)) (r2 (eas-hierarchy--cr b))
         (x3 (eas-hierarchy--cx c)) (y3 (eas-hierarchy--cy c)) (r3 (eas-hierarchy--cr c))
         (a2 (- x1 x2)) (a3 (- x1 x3)) (b2 (- y1 y2)) (b3 (- y1 y3)) (c2 (- r2 r1)) (c3 (- r3 r1))
         (d1 (- (+ (* x1 x1) (* y1 y1)) (* r1 r1)))
         (d2 (+ (- d1 (* x2 x2) (* y2 y2)) (* r2 r2)))
         (d3 (+ (- d1 (* x3 x3) (* y3 y3)) (* r3 r3)))
         (ab (- (* a3 b2) (* a2 b3)))
         (xa (- (/ (- (* b2 d3) (* b3 d2)) (* ab 2)) x1))
         (xb (/ (- (* b3 c2) (* b2 c3)) ab))
         (ya (- (/ (- (* a3 d2) (* a2 d3)) (* ab 2)) y1))
         (yb (/ (- (* a2 c3) (* a3 c2)) ab))
         (qa (- (+ (* xb xb) (* yb yb)) 1))
         (qb (* 2 (+ r1 (* xa xb) (* ya yb))))
         (qc (- (+ (* xa xa) (* ya ya)) (* r1 r1)))
         (r (- (if (> (abs qa) 1e-6)
                   (/ (+ qb (sqrt (- (* qb qb) (* 4 qa qc)))) (* 2 qa))
                 (/ qc qb)))))
    (vector (+ x1 xa (* xb r)) (+ y1 ya (* yb r)) r)))

(defun eas-hierarchy--enclose-basis (basis)
  "The circle enclosing the one, two or three circles of BASIS."
  (pcase (length basis)
    (1 (copy-sequence (car basis)))
    (2 (eas-hierarchy--enclose2 (nth 0 basis) (nth 1 basis)))
    (_ (eas-hierarchy--enclose3 (nth 0 basis) (nth 1 basis) (nth 2 basis)))))

(defun eas-hierarchy--extend-basis (basis p)
  "The support set of BASIS plus circle P, as d3's extendBasis."
  (if (eas-hierarchy--encloses-weak-all p basis) (list p)
    (or (cl-loop for b in basis
                 when (and (eas-hierarchy--encloses-not p b)
                           (eas-hierarchy--encloses-weak-all (eas-hierarchy--enclose2 b p) basis))
                 return (list b p))
        (cl-loop for (bi . rest) on basis
                 thereis (cl-loop for bj in rest
                                  when (and (eas-hierarchy--encloses-not (eas-hierarchy--enclose2 bi bj) p)
                                            (eas-hierarchy--encloses-not (eas-hierarchy--enclose2 bi p) bj)
                                            (eas-hierarchy--encloses-not (eas-hierarchy--enclose2 bj p) bi)
                                            (eas-hierarchy--encloses-weak-all (eas-hierarchy--enclose3 bi bj p) basis))
                                  return (list bi bj p)))
        (eas-signal "ENGINE_FAILED" "pack: no enclosing circle found"))))

(defun eas-hierarchy-enclose (circles random)
  "The smallest circle enclosing CIRCLES ([X Y R] vectors), shuffled by RANDOM."
  (let* ((v (vconcat circles)) (m (length v)) (i 0) (basis nil) (e nil))
    ;; d3's shuffle: Fisher-Yates from the end.
    (while (> m 0)
      (let ((j (floor (* (funcall random) m))))
        (setq m (1- m))
        (cl-rotatef (aref v m) (aref v j))))
    (while (< i (length v))
      (let ((p (aref v i)))
        (if (and e (eas-hierarchy--encloses-weak e p))
            (setq i (1+ i))
          (setq basis (eas-hierarchy--extend-basis basis p)
                e (eas-hierarchy--enclose-basis basis)
                i 0))))
    e))

;;; Packing siblings (d3's packSiblingsRandom)

(defun eas-hierarchy--place (b a c)
  "Place circle C tangent to circles B and A."
  (let* ((dx (- (aref b 0) (aref a 0))) (dy (- (aref b 1) (aref a 1))) (d2 (+ (* dx dx) (* dy dy))))
    (if (/= d2 0)
        (let ((a2 (expt (+ (aref a 2) (aref c 2)) 2)) (b2 (expt (+ (aref b 2) (aref c 2)) 2)))
          (if (> a2 b2)
              (let* ((x (/ (- (+ d2 b2) a2) (* 2 d2))) (y (sqrt (max 0 (- (/ b2 d2) (* x x))))))
                (aset c 0 (- (aref b 0) (* x dx) (* y dy)))
                (aset c 1 (+ (- (aref b 1) (* x dy)) (* y dx))))
            (let* ((x (/ (- (+ d2 a2) b2) (* 2 d2))) (y (sqrt (max 0 (- (/ a2 d2) (* x x))))))
              (aset c 0 (- (+ (aref a 0) (* x dx)) (* y dy)))
              (aset c 1 (+ (aref a 1) (* x dy) (* y dx))))))
      (aset c 0 (+ (aref a 0) (aref c 2)))
      (aset c 1 (aref a 1)))))

(defun eas-hierarchy--intersects (a b)
  "Non-nil when circles A and B overlap."
  (let ((dr (- (+ (aref a 2) (aref b 2)) 1e-6)) (dx (- (aref b 0) (aref a 0))) (dy (- (aref b 1) (aref a 1))))
    (and (> dr 0) (> (* dr dr) (+ (* dx dx) (* dy dy))))))

(defun eas-hierarchy--score (node next)
  "Squared distance from the origin of the weighted midpoint of NODE and NEXT."
  (let* ((a node) (b next) (ab (+ (aref a 2) (aref b 2)))
         (dx (/ (+ (* (aref a 0) (aref b 2)) (* (aref b 0) (aref a 2))) ab))
         (dy (/ (+ (* (aref a 1) (aref b 2)) (* (aref b 1) (aref a 2))) ab)))
    (+ (* dx dx) (* dy dy))))

(defun eas-hierarchy-pack-siblings (circles random)
  "Pack CIRCLES ([X Y R] vectors, placed in place) around the origin.
RANDOM shuffles the enclosing pass.  Return the enclosing radius."
  (let* ((cs (vconcat circles)) (n (length cs)))
    (cond
     ((= n 0) 0)
     ((= n 1) (aset (aref cs 0) 0 0) (aset (aref cs 0) 1 0) (aref (aref cs 0) 2))
     (t
      (let ((a (aref cs 0)) (b (aref cs 1)))
        (aset a 0 (- (aref b 2))) (aset a 1 0) (aset b 0 (aref a 2)) (aset b 1 0)
        (if (= n 2) (+ (aref a 2) (aref b 2))
          ;; The front chain is a ring of circles: next and previous by index into the hash tables.
          (let ((next (make-hash-table :test 'eq)) (prev (make-hash-table :test 'eq)) (c (aref cs 2)) (i 3))
            (eas-hierarchy--place b a c)
            (puthash a b next) (puthash c b prev) (puthash b c next) (puthash a c prev)
            (puthash c a next) (puthash b a prev)
            (while (< i n)
              (setq c (aref cs i))
              (eas-hierarchy--place a b c)
              (let ((j (gethash b next)) (k (gethash a prev))
                    (sj (aref b 2)) (sk (aref a 2)) (retry nil))
                (catch 'found
                  (while t
                    (if (<= sj sk)
                        (if (eas-hierarchy--intersects j c)
                            (progn (setq b j) (puthash a b next) (puthash b a prev) (setq retry t) (throw 'found nil))
                          (setq sj (+ sj (aref j 2)) j (gethash j next)))
                      (if (eas-hierarchy--intersects k c)
                          (progn (setq a k) (puthash a b next) (puthash b a prev) (setq retry t) (throw 'found nil))
                        (setq sk (+ sk (aref k 2)) k (gethash k prev))))
                    (when (eq j (gethash k next)) (throw 'found nil))))
                (unless retry
                  ;; Insert c between a and b, then find the pair closest to the centroid.
                  (puthash c a prev) (puthash c b next) (puthash a c next) (puthash b c prev)
                  (setq b c)
                  (let ((best (eas-hierarchy--score a (gethash a next))) (cur c))
                    (while (not (eq (setq cur (gethash cur next)) b))
                      (let ((sc (eas-hierarchy--score cur (gethash cur next))))
                        (when (< sc best) (setq a cur best sc))))
                    (setq b (gethash a next)))
                  (setq i (1+ i)))))
            (let* ((chain (let ((out (list b)) (cur b))
                            (while (not (eq (setq cur (gethash cur next)) b)) (push cur out))
                            (nreverse out)))
                   (e (eas-hierarchy-enclose chain random)))
              (seq-doseq (circle cs)
                (aset circle 0 (- (aref circle 0) (aref e 0)))
                (aset circle 1 (- (aref circle 1) (aref e 1))))
              (aref e 2)))))))))

;;; pack

(defun eas-hierarchy-pack (root params)
  "Lay ROOT out as d3's pack under PARAMS (size, padding, radius)."
  (let* ((size (or (plist-get params :size) [1 1])) (dx (aref size 0)) (dy (aref size 1))
         (padding (or (plist-get params :padding) 0))
         (radius (let ((r (plist-get params :radius))) (and (stringp r) (eas-key r))))
         (random (eas-hierarchy--lcg))
         (circles (make-hash-table :test 'eq)))
    (eas-hierarchy-each-before
     root (lambda (n)
            (puthash n (vector 0.0 0.0 0.0) circles)
            (unless (eas-hierarchy-node-children n)
              (let ((v (if radius (plist-get (eas-hierarchy-node-row n) radius)
                         (sqrt (max 0 (eas-hierarchy-node-value n))))))
                (aset (gethash n circles) 2 (if (numberp v) (max 0 (float v)) 0.0))))))
    (let ((pack-children
           (lambda (pad k)
             (lambda (n)
               (when-let* ((cs (eas-hierarchy-node-children n)))
                 (let ((rs (mapcar (lambda (c) (gethash c circles)) cs)) (r (* pad k)))
                   (when (/= r 0) (dolist (c rs) (aset c 2 (+ (aref c 2) r))))
                   (let ((e (eas-hierarchy-pack-siblings rs random)))
                     (when (/= r 0) (dolist (c rs) (aset c 2 (- (aref c 2) r))))
                     (aset (gethash n circles) 2 (+ e r))))))))
          (translate
           (lambda (k)
             (lambda (n)
               (let ((c (gethash n circles)) (p (eas-hierarchy-node-parent n)))
                 (aset c 2 (* (aref c 2) k))
                 (when p
                   (let ((pc (gethash p circles)))
                     (aset c 0 (+ (aref pc 0) (* k (aref c 0))))
                     (aset c 1 (+ (aref pc 1) (* k (aref c 1)))))))))))
      (let ((rc (gethash root circles)))
        (aset rc 0 (/ dx 2.0)) (aset rc 1 (/ dy 2.0)))
      (if radius
          (progn (eas-hierarchy-each-after root (funcall pack-children padding 0.5))
                 (eas-hierarchy-each-before root (funcall translate 1)))
        ;; Pack once unpadded, again with the padding scaled to the
        ;; packing's size, then scale the whole to fit, as d3 does.
        (eas-hierarchy-each-after root (funcall pack-children 0 1))
        (let ((k (/ (aref (gethash root circles) 2) (min dx dy))))
          (eas-hierarchy-each-after root (funcall pack-children padding k)))
        (eas-hierarchy-each-before root (funcall translate (/ (min dx dy) (* 2.0 (aref (gethash root circles) 2)))))))
    (eas-hierarchy-each-before
     root (lambda (n) (let ((c (gethash n circles)))
                        (setf (eas-hierarchy-node-x n) (aref c 0) (eas-hierarchy-node-y n) (aref c 1)
                              (eas-hierarchy-node-r n) (aref c 2)))))))

(eas-register-transform
 "pack"
 :doc "Circle-packing layout of id/parent rows, each circle's area its value (Vega's pack)."
 :schema (append eas-hierarchy-common-schema
                 '(:size (:type "array" :doc "[width height] the packing fits (default [1 1])")
                   :padding (:type "number" :default 0 :doc "gap between sibling circles")
                   :radius (:type "string" :doc "a field holding leaf radii instead of sqrt(value)")
                   :as (:type "array" :default ["x" "y" "r" "depth" "children"] :doc "output fields")))
 :fn (lambda (rows params)
       (eas-hierarchy-layout rows params #'eas-hierarchy-pack
                             (list (cons "x" #'eas-hierarchy-node-x) (cons "y" #'eas-hierarchy-node-y)
                                   (cons "r" #'eas-hierarchy-node-r) (cons "depth" #'eas-hierarchy-node-depth)
                                   (cons "children" #'eas-hierarchy-children-count)))))

(provide 'eas-hierarchy-pack)
;;; eas-hierarchy-pack.el ends here
