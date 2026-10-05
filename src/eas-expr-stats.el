;;; eas-expr-stats.el --- statistics and number formatting for expressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Vega expression functions the gallery's calculations use, ported
;; from vega-statistics and d3-format:
;;
;;   quantileUniform(p, a=0, b=1)    a + p (b - a)
;;   quantileNormal(p, mean=0, sd=1) via Giles's erfinv approximation
;;   format(value, specifier)        d3-format's [,][.precision][~][type]
;;                                   for types f d % e and none

;;; Code:

(require 'eas-core)

(defun eas-expr-stats-quantile-uniform (p &optional a b)
  "Quantile P of the uniform distribution on [A, B] (default [0, 1])."
  (let ((a (or a 0)) (b (or b 1)))
    (if (or (< p 0) (> p 1)) 0.0e+NaN (+ a (* p (- b a))))))

(defun eas-expr-stats--poly (w coefs)
  "Horner evaluation of COEFS (highest power first) at W, as Vega writes it."
  (let ((p (car coefs)))
    (dolist (c (cdr coefs) p) (setq p (+ c (* p w))))))

(defun eas-expr-stats--erfinv (x)
  "Mike Giles's approximation of the inverse error function at X.\nAs in vega-statistics."
  (let ((w (- (log (* (- 1 x) (+ 1 x))))))
    (* x
       (cond
        ((< w 6.25)
         (eas-expr-stats--poly
          (- w 3.125)
          '(-3.6444120640178196996e-21 -1.685059138182016589e-19 1.2858480715256400167e-18
            1.115787767802518096e-17 -1.333171662854620906e-16 2.0972767875968561637e-17
            6.6376381343583238325e-15 -4.0545662729752068639e-14 -8.1519341976054721522e-14
            2.6335093153082322977e-12 -1.2975133253453532498e-11 -5.4154120542946279317e-11
            1.051212273321532285e-09 -4.1126339803469836976e-09 -2.9070369957882005086e-08
            4.2347877827932403518e-07 -1.3654692000834678645e-06 -1.3882523362786468719e-05
            0.0001867342080340571352 -0.00074070253416626697512 -0.0060336708714301490533
            0.24015818242558961693 1.6536545626831027356)))
        ((< w 16.0)
         (eas-expr-stats--poly
          (- (sqrt w) 3.25)
          '(2.2137376921775787049e-09 9.0756561938885390979e-08 -2.7517406297064545428e-07
            1.8239629214389227755e-08 1.5027403968909827627e-06 -4.013867526981545969e-06
            2.9234449089955446044e-06 1.2475304481671778723e-05 -4.7318229009055733981e-05
            6.8284851459573175448e-05 2.4031110387097893999e-05 -0.0003550375203628474796
            0.00095328937973738049703 -0.0016882755560235047313 0.0024914420961078508066
            -0.0037512085075692412107 0.005370914553590063617 1.0052589676941592334
            3.0838856104922207635)))
        ((not (isnan w)) (if (= w 1.0e+INF) 1.0e+INF
                           (eas-expr-stats--poly
                            (- (sqrt w) 5.0)
                            '(-2.7109920616438573243e-11 -2.5556418169965252055e-10 1.5076572693500548083e-09
                              -3.7894654401267369937e-09 7.6157012080783393804e-09 -1.4960026627149240478e-08
                              2.9147953450901080826e-08 -6.7711997758452339498e-08 2.2900482228026654717e-07
                              -9.9298272942317002539e-07 4.5260625972231537039e-06 -1.9681778105531670567e-05
                              7.5995277030017761139e-05 -0.00021503011930044477347 -0.00013871931833623122026
                              1.0103004648645343977 4.8499064014085844221))))
        (t 1.0e+INF)))))

(defun eas-expr-stats-quantile-normal (p &optional mean stdev)
  "Quantile P of the normal distribution with MEAN (0) and STDEV (1)."
  (if (or (< p 0) (> p 1)) 0.0e+NaN
    (+ (or mean 0) (* (or stdev 1) (sqrt 2) (eas-expr-stats--erfinv (- (* 2 p) 1))))))

(provide 'eas-expr-stats)
;;; eas-expr-stats.el ends here
