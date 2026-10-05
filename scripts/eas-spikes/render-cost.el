;;; render-cost.el --- spike: Lisp-side cost of one redraw -*- lexical-binding: t; -*-

;; Usage: emacs -Q --batch -l render-cost.el
;; Measures, per N in (100 1000 10000):
;;   svg-build   building an svg.el DOM of N rect marks plus one crosshair
;;   svg-change  moving the crosshair and re-serializing (the per-pointer-move
;;               Lisp cost before librsvg rasterizes anything)
;;   map-build   building an image :map list of N rect areas
;;   bisect      x-sorted bisect hit-test over N items (1000 queries)
;; and for a 100x30 grid:
;;   grid-insert erase + insert a fully propertized grid into a buffer
;; Rasterization, image-cache churn and display are NOT measured here:
;; they need a GUI frame with librsvg.

(require 'svg)
(require 'benchmark)

(defun spike--ms (reps fn)
  "Mean milliseconds of REPS calls of FN, after one warm-up call."
  (funcall fn)
  (garbage-collect)
  (let ((elapsed (car (benchmark-call fn reps))))
    (/ (* 1000.0 elapsed) reps)))

(defun spike--svg (n)
  "Build an SVG of N bars and one crosshair line; return (SVG . LINE)."
  (let ((svg (svg-create 800 400))
        (w (/ 780.0 n)))
    (dotimes (i n)
      (svg-rectangle svg (+ 10 (* i w)) (- 390 (% (* i 37) 300)) (max 0.5 (* 0.8 w))
                     (% (* i 37) 300) :fill "#4c78a8"))
    (svg-line svg 400 0 400 400 :stroke "#888" :id "crosshair")
    svg))

(defun spike--map (n)
  "Build a :map list of N rect areas."
  (let ((w (/ 780.0 n)) areas)
    (dotimes (i n)
      (let ((x0 (round (+ 10 (* i w)))))
        (push `((rect . ((,x0 . 10) . (,(+ x0 (max 1 (round w))) . 390)))
                ,(intern (format "eas-item-%d" i))
                (help-echo ,(format "item %d" i) pointer hand))
              areas)))
    areas))

(defun spike--bisect (xs x)
  "Index of the element of sorted vector XS nearest X."
  (let ((lo 0) (hi (1- (length xs))))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (< (aref xs mid) x) (setq lo (1+ mid)) (setq hi mid))))
    (if (and (> lo 0) (< (- x (aref xs (1- lo))) (- (aref xs lo) x))) (1- lo) lo)))

(dolist (n '(100 1000 10000))
  (let* ((reps (if (= n 10000) 5 20))
         (svg (spike--svg n))
         (line (car (dom-by-id svg "crosshair")))
         (xs (vconcat (number-sequence 0 (1- n)))))
    (princ (format "n=%-6d svg-build=%8.2fms svg-change=%8.2fms map-build=%8.2fms bisect-1000q=%6.3fms bytes=%d\n"
                   n
                   (spike--ms reps (lambda () (spike--svg n)))
                   (spike--ms reps (lambda ()
                                     (dom-set-attribute line 'x1 (random 800))
                                     (dom-set-attribute line 'x2 (random 800))
                                     (with-temp-buffer (svg-print svg) (buffer-size))))
                   (spike--ms reps (lambda () (spike--map n)))
                   (spike--ms reps (lambda () (dotimes (_ 1000) (spike--bisect xs (random n)))))
                   (with-temp-buffer (svg-print svg) (buffer-size))))))

(let ((rows 30) (cols 100))
  (with-temp-buffer
    (princ (format "grid %dx%d insert=%.3fms\n" cols rows
                   (spike--ms 50
                              (lambda ()
                                (erase-buffer)
                                (dotimes (r rows)
                                  (dotimes (c cols)
                                    (insert (propertize (string (+ #x2800 (% (* r c) 255)))
                                                        'eas-datum (+ (* r cols) c)
                                                        'help-echo "datum"
                                                        'face 'default)))
                                  (insert "\n"))))))))

;; Alternatives for the renderer: build the DOM list directly (svg.el's
;; `svg-rectangle' appends with `dom-append-child', O(n) per mark), and
;; serialize with one `mapconcat' instead of `svg-print'.

(defun spike--svg-direct (n)
  "Build the same DOM as `spike--svg' by consing children in one pass."
  (let ((w (/ 780.0 n)) children)
    (dotimes (i n)
      (push (dom-node 'rect `((x . ,(+ 10 (* i w))) (y . ,(- 390 (% (* i 37) 300)))
                              (width . ,(max 0.5 (* 0.8 w))) (height . ,(% (* i 37) 300))
                              (fill . "#4c78a8")))
            children))
    (push (dom-node 'line '((x1 . 400) (y1 . 0) (x2 . 400) (y2 . 400)
                            (stroke . "#888") (id . "crosshair")))
          children)
    (apply #'dom-node 'svg '((width . 800) (height . 400)
                             (xmlns . "http://www.w3.org/2000/svg"))
           (nreverse children))))

(defun spike--serialize (node)
  "Serialize DOM NODE to an SVG string."
  (if (stringp node) node
    (concat "<" (symbol-name (dom-tag node))
            (mapconcat (lambda (a) (format " %s=\"%s\"" (car a) (cdr a)))
                       (dom-attributes node) "")
            ">" (mapconcat #'spike--serialize (dom-children node) "")
            "</" (symbol-name (dom-tag node)) ">")))

(dolist (n '(100 1000 10000))
  (let ((svg (spike--svg-direct n)))
    (princ (format "n=%-6d direct-build=%7.2fms mapconcat-serialize=%7.2fms\n"
                   n
                   (spike--ms 10 (lambda () (spike--svg-direct n)))
                   (spike--ms 10 (lambda () (length (spike--serialize svg))))))))

;; A continuous series as ONE <path> of N vertices, the way the scene's
;; line/area marks are drawn: serialize cost per redraw.
(dolist (n '(100 1000 10000))
  (let ((svg (svg-create 800 400)))
    (svg-node svg 'path
              :d (mapconcat (lambda (i) (format "%s%.1f,%.1f" (if (= i 0) "M" "L")
                                                (* i (/ 780.0 n)) (float (% (* i 37) 300))))
                            (number-sequence 0 (1- n)) "")
              :stroke "#4c78a8" :fill "none")
    (princ (format "n=%-6d one-path svg-print=%7.2fms\n" n
                   (spike--ms 10 (lambda () (with-temp-buffer (svg-print svg) (buffer-size))))))))
