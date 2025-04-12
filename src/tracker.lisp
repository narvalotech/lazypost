(defvar *root-path* "/home/jon/repos/lazypost")
(defparameter *use-db* t)

(ql:quickload :local-time)

(defun project-file (path-to-file)
  (concatenate 'string *root-path* "/" path-to-file))

(defun make-postcard (lid
                      src-country dst-country
                      src-email dst-email
                      message
                      &key image delivery-date sent-date)
  (list
     :lid lid
     :src-country src-country
     :dst-country dst-country
     :src-email src-email
     :dst-email dst-email
     :delivery-date delivery-date
     :sent-date sent-date
     :text message
     :image image))

(ql:quickload :sqlite)

(defun make-letter-from-db-row (row)
  (destructuring-bind (id
                       lid
                       delivery-date
                       sent-date
                       dst-country
                       src-country
                       dst-email
                       src-email
                       message
                       media-name
                       media) row
    (declare (ignore id media media-name))
    (make-postcard
     lid
     src-country
     dst-country
     src-email
     dst-email
     message
     :image nil
     :delivery-date delivery-date
     :sent-date sent-date)))

(defun dump-db (db)
  (sqlite:execute-to-list db "SELECT * FROM outbox"))

;; Operate on copy to not induce errors into LazyPost itself
(uiop:copy-file (project-file "data/db.sqlite")
                (project-file "data/db-copy.sqlite"))

(defparameter *db-path* (project-file "data/db-copy.sqlite"))

(defun get-all-letters ()
  (mapcar 'make-letter-from-db-row
          (sqlite:with-open-database (db *db-path*)
            (dump-db db))))

(defun percent-days (postcard)
  (let* ((delivery (local-time:parse-timestring (getf postcard :delivery-date)))
         (sent (local-time:parse-timestring (getf postcard :sent-date)))
         (travel-time-days
           (round
            (/ (local-time:timestamp-difference delivery sent) 60 60 24)))
         (left-days
           (max 0
                (round
                 (/ (local-time:timestamp-difference (local-time:now) sent) 60 60 24)))))
    (/ left-days travel-time-days)))

(defun get-delivery-date (postcard)
  (local-time:parse-timestring (getf postcard :delivery-date)))

(ql:quickload :read-csv)

(defun read-country-db ()
  (let ((csv
          (with-open-file (stream (project-file "src/coutries-and-states.csv"))
            (read-csv:parse-csv stream #\,))))
    (loop for country in (cdr csv)
          collect
          (list
           :code (nth 0 country)
           :name (nth 3 country)
           :is-state (equalp "1" (nth 4 country))
           :lat (read-from-string (nth 1 country))
           :lon (read-from-string (nth 2 country))))))

(defparameter *country-db* (read-country-db))

(defun find-country (db name)
  (find-if (lambda (country) (search name (getf country :name) :test #'equalp)) db))

(find-country *country-db* "Lou")
 ; => (:CODE "LA" :NAME "Louisiana" :IS-STATE T :LAT 31.244823 :LON -92.14503)

(find-country *country-db* "Norway")
 ; => (:CODE "NO" :NAME "Norway" :IS-STATE NIL :LAT 60.472023 :LON 8.468946)

(defun d2r (degrees)
  "Converts degrees to radians"
  (* degrees (/ pi 180)))

(d2r 180)
 ; => 3.141592653589793d0

(defun r2d (radians)
  (* radians (/ 180 pi)))

(r2d (/ pi 2))
 ; => 90.0d0

;; From https://stackoverflow.com/a/3694410
(defun distance? (lat1 lon1 lat2 lon2)
  (let* ((theta (- lon1 lon2))
         (dist (+
                (* (sin (d2r lat1))
                   (sin (d2r lat2)))
                (* (cos (d2r lat1))
                   (cos (d2r lat2))
                   (cos (d2r theta))))))
    (setf dist (acos dist))
    (setf dist (r2d dist))
    (setf dist (* dist 60 1.1515))
    ;; convert to kilometers
    (setf dist (* dist 1.609344))
    dist))

(defun country-distance (c1 c2)
  "Calculate straight-line distance between two countries"
  (let ((lat1 (getf c1 :lat))
        (lon1 (getf c1 :lon))
        (lat2 (getf c2 :lat))
        (lon2 (getf c2 :lon)))
    (distance? lat1 lon1 lat2 lon2)))

(country-distance
 (find-country *country-db* "Norway")
 (find-country *country-db* "Hawaii"))
 ; => 10963.836852735742d0

(ql:quickload :sketch)

(defun draw-dot (x y &key (color sketch:+orange+))
  (sketch:with-pen (sketch:make-pen :fill color)
    (sketch:circle x y 5)))

(defparameter *img-width* 800)
(defparameter *img-height* (truncate (* *img-width* .65)))

(defun xy (x y)
  (list :x x :y y))

(defun decode-xy (width height pos)
  (let ((lon (getf pos :lon))
        (lat (getf pos :lat)))
  (xy (+ (/ width 2) (* (/ lon 180) (/ width 2)))
      (- (/ height 2) (* (/ lat 90) (/ height 2))))))

(defun draw-pos (width height pos &key (color sketch:+orange+))
  (let ((coordinates (decode-xy width height pos)))

  (draw-dot (getf coordinates :x)
            (getf coordinates :y)
            :color color)))

(defun scale (a b factor)
  (truncate
   (+ a (* factor (- b a)))))

(defun draw-line (width height start end
                  &key (color sketch:+red+) (factor 1) (weight 4))

  (let ((pos-start (decode-xy width height start))
        (pos-end (decode-xy width height end)))

    (let ((x1 (getf pos-start :x))
          (y1 (getf pos-start :y))
          (x2 (scale (getf pos-start :x) (getf pos-end :x) factor))
          (y2 (scale (getf pos-start :y) (getf pos-end :y) factor)))

      (sketch:with-pen (sketch:make-pen :stroke color :weight weight)
        (sketch:line x1 y1 x2 y2)))))

(defun draw-distance (width height pos1 pos2 percent)
  (draw-pos width height pos1)
  (draw-pos width height pos2 :color sketch:+gray+)
  (draw-line width height pos1 pos2 :color sketch:+gray+ :weight 2)
  (draw-line width height pos1 pos2 :factor percent :color sketch:+orange+))

(mapcar
 (lambda (postcard)
   (list
    (* 100 (percent-days postcard))
    (find-country *country-db* (getf postcard :src-country))
    (find-country *country-db* (getf postcard :dst-country))
    ))
 (get-all-letters))

;; --------------
;; Override SKETCH's init to provide the :hidden flag
(defvar *headless* nil)
(when *headless*
  (in-package #:sketch)
  (defmethod initialize-instance :after ((instance sketch) &rest initargs &key &allow-other-keys)
    (apply #'prepare instance initargs)
    (setf (sketch-%window instance)
          (make-instance 'sketch-window
                         :title (sketch-title instance)
                         :w (sketch-width instance)
                         :h (sketch-height instance)
                         :fullscreen (sketch-fullscreen instance)
                         :resizable (sketch-resizable instance)
                         :sketch instance
                         :flags '(:hidden)))
    (initialize-environment instance)
    (initialize-gl instance)
    ;; These will have been added in the call to PREPARE.
    (with-slots ((fs %delayed-init-funs)) instance
      (loop for f across fs
            do (funcall f))
      (setf fs (make-array 0 :adjustable t :fill-pointer t))))

  (in-package :cl-user)
  )
;; --------------

(sketch:defsketch image-test
    ((sketch:title "Letter Tracker")
     (sketch:width (+ 0 *img-width*))
     (sketch:height (+ 0 *img-height*))
     (sketch:copy-pixels t)
     (save-image nil)
     (pic (sketch:load-resource (project-file "src/plate.jpg"))))

  (sketch:draw pic :width *img-width* :height *img-height*)

  (ignore-errors
   (mapcar
    (lambda (postcard)
      (draw-distance *img-width* *img-height*
                     (find-country *country-db* (getf postcard :src-country))
                     (find-country *country-db* (getf postcard :dst-country))
                     (percent-days postcard)))
    (get-all-letters)))

  (when save-image
    (setf save-image nil)
    (sketch:save-png "map.png"))
  )

(defparameter *sketch* (make-instance 'image-test))
(setf (slot-value *sketch* 'save-image) t)
