;;; eas-link-bus.el --- named param buses: linked views across buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.6).  Views in different buffers, windows or specs
;; join a named bus.  When a dispatch changes one of the bus's params in
;; a member (a hover, a brush, a zoom of a param bound to scales), every
;; other member receives a link event/v1 (eas-link.el) for that param:
;;
;;   (eas-link-join "ohlc:TSM" "tickers" '("crosshair" "zoom"))
;;   (eas-link-join "ohlc:ASML" "tickers" '("crosshair" "zoom"))
;;   hover TSM's chart -> ASML's crosshair sits on the same day
;;
;; Link events are logged by the receiver and never forwarded again, so
;; a bus cannot echo, and a receiver's log replays without the sender.
;; The bus remembers the last store of each param, and a view that joins
;; late catches up at once.  Closed views leave their buses on the next
;; broadcast.  A bus with no param names carries every param.

;;; Code:

(require 'eas-core)
(require 'eas-describe)
(require 'eas-view)
(require 'eas-link)

(cl-defstruct (eas-link-bus (:constructor eas-link-bus--make) (:copier nil))
  name params members last)

(defvar eas-link-buses (make-hash-table :test 'equal)
  "Param buses by name.")

(defvar eas-link--delivering nil
  "Non-nil while a bus delivers, so receivers do not broadcast.")

(defun eas-link--id (view)
  "VIEW's id (VIEW is an id or a view)."
  (eas-view-id (eas-view-get view)))

(defun eas-link-get (bus)
  "BUS (a name or bus) or signal NOT_FOUND."
  (cond ((eas-link-bus-p bus) bus)
        ((gethash bus eas-link-buses))
        (t (eas-signal "NOT_FOUND" (format "No bus %S; buses: %s" bus
                                           (if (eas-link-bus-names) (string-join (eas-link-bus-names) ", ") "none"))
                       :bus bus))))

(defun eas-link-bus-names ()
  "Names of the live buses, sorted."
  (let (names) (maphash (lambda (k _) (push k names)) eas-link-buses) (sort names #'string<)))

(defun eas-link-describe-bus (bus)
  "BUS as JSON-ready data."
  (let ((bus (eas-link-get bus)))
    (eas-link--prune bus)
    (list :name (eas-link-bus-name bus)
          :params (if (eas-link-bus-params bus) (vconcat (eas-link-bus-params bus)) :null)
          :members (vconcat (eas-link-bus-members bus))
          :last (vconcat (mapcar (lambda (entry) (list :param (car entry) :store (or (cdr entry) :null)))
                                 (reverse (eas-link-bus-last bus)))))))

(defun eas-link-buses-of (view)
  "Names of the buses VIEW belongs to."
  (let ((id (eas-link--id view)))
    (seq-filter (lambda (name) (member id (eas-link-bus-members (gethash name eas-link-buses))))
                (eas-link-bus-names))))

(defun eas-link-join (view bus &optional params)
  "Make VIEW a member of BUS (a name), carrying PARAMS (names; nil = all).
Creates BUS on first use; PARAMS given again replace the bus's set.
VIEW catches up with the bus's last stores.  Returns the bus as data."
  (let* ((id (eas-link--id view))
         (bus (or (gethash bus eas-link-buses)
                  (puthash bus (eas-link-bus--make :name bus) eas-link-buses))))
    (when params (setf (eas-link-bus-params bus) (mapcar (lambda (p) (format "%s" p)) (append params nil))))
    (unless (member id (eas-link-bus-members bus))
      (setf (eas-link-bus-members bus) (append (eas-link-bus-members bus) (list id)))
      (let ((eas-link--delivering t))
        (dolist (entry (reverse (eas-link-bus-last bus)))
          (when (eas-link--carries-p bus (car entry))
            (eas-link--deliver id (list :type "link" :param (car entry) :store (or (cdr entry) :null)
                                        :from (eas-link-bus-name bus)))))))
    (eas-link-describe-bus bus)))

(defun eas-link-leave (view &optional bus)
  "Take VIEW out of BUS (default: every bus); drop buses left empty."
  (let ((id (if (eas-view-p view) (eas-view-id view) view)))
    (dolist (name (if bus (list (eas-link-bus-name (eas-link-get bus))) (eas-link-bus-names)))
      (let ((b (gethash name eas-link-buses)))
        (setf (eas-link-bus-members b) (delete id (eas-link-bus-members b)))
        (unless (eas-link-bus-members b) (remhash name eas-link-buses))))))

(defun eas-link--prune (bus)
  "Drop BUS members that are no longer live views."
  (setf (eas-link-bus-members bus)
        (seq-filter (lambda (id) (gethash id eas-views)) (eas-link-bus-members bus))))

(defun eas-link--carries-p (bus name)
  "Non-nil when BUS carries param NAME."
  (or (null (eas-link-bus-params bus)) (member name (eas-link-bus-params bus))))

(defun eas-link--deliver (id event)
  "Dispatch link EVENT to live view ID; nil when it is gone or static."
  (when-let* ((view (gethash id eas-views)))
    (when (eas-view-interactive view)
      (eas-dispatch view event))))

(defun eas-link--broadcast (view event old-state old-scene)
  "After VIEW took EVENT, send its changed bus params to the other members.
OLD-STATE and OLD-SCENE are VIEW's before EVENT (`eas-view-dispatch-functions')."
  (unless (or eas-link--delivering (equal (plist-get event :type) "link"))
    (let ((id (eas-view-id view)))
      (dolist (name (eas-link-buses-of view))
        (let* ((bus (gethash name eas-link-buses))
               (changes (eas-link-changes old-scene old-state (eas-view-scene view) (eas-view-state view)
                                          (eas-link-bus-params bus))))
          (eas-link--prune bus)
          (dolist (change changes)
            (let ((param (plist-get change :param)) (store (plist-get change :store)))
              (setf (eas-link-bus-last bus)
                    (cons (cons param (and (not (eq store :null)) store))
                          (assoc-delete-all param (eas-link-bus-last bus))))
              (let ((eas-link--delivering t))
                (dolist (other (eas-link-bus-members bus))
                  (unless (equal other id)
                    (eas-link--deliver other (append change (list :from id)))))))))))))

(add-hook 'eas-view-dispatch-functions #'eas-link--broadcast)

(cl-defun eas-link-open (source bus &rest args &key params &allow-other-keys)
  "Open SOURCE as a view (ARGS as in `eas-view-open') and join it to BUS.
PARAMS names the params BUS carries.  Returns the view."
  (let ((view (apply #'eas-view-open source (eas--plist-without args :params))))
    (eas-link-join view bus params)
    view))

(defun eas-link--describe ()
  "The describe section for linked views."
  (list :link (list :event "{\"type\": \"link\", \"param\": NAME, \"store\": STORE|null, \"from\"?: VIEW}"
                    :within-spec ["params at a concat (with \"views\") are one selection in every named view"
                                  "a param bound to scales held by several views zooms them together"
                                  "scale.domain {\"param\": NAME} follows an interval selection"]
                    :buses (vconcat (eas-link-bus-names))
                    :verbs ["eas-link-join" "eas-link-leave" "eas-link-open" "eas-link-describe-bus"])))

(add-hook 'eas-describe-functions #'eas-link--describe)

(provide 'eas-link-bus)
;;; eas-link-bus.el ends here
