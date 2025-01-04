(defun get-form ()
  "this is a form")

(defparameter *the-post* nil)

(defun read-postcard (postcard)
  (let ((text (getf postcard :text))
        (src-city (getf postcard :src-city))
        (dst-city (getf postcard :dst-city))
        (src-email (getf postcard :src-email))
        (dst-email (getf postcard :dst-email)))
    (list text src-city dst-city src-email dst-email)))

(defun pprint-postcard (postcard)
  (destructuring-bind (text
                       src-city
                       dst-city
                       src-email
                       dst-email)
      (read-postcard postcard)
    (with-output-to-string (s)
      "Send a delayed postcard. Only accepts text."
      (format s "~A -> ~A~%" src-city dst-city)
      (format s "~A -> ~A~%" src-email dst-email)
      (format s "~A" text))))

(defun send-postcard (postcard)
  "Send a delayed postcard. Only accepts text."
  (push postcard *the-post*))

(defun deliver-postcard (postcard)
  (format t "##### You've got mail. #####~%~A~%~%" (pprint-postcard postcard)))

(progn
  (setf *the-post* nil)
  (send-postcard
   '(:text
     "Hello from Oslo!
Made a new friend, Ole nordmann.

xoxo Matt"

     :src-city "Oslo, Norway"
     :dst-city "Denver, USA"

     :src-email "matt.smith@gmail.com"
     :dst-email "john.smith@hostmail.com"

     :sent-date "2024-12-24"
     :delivery-date "2025-01-10"
     ))
  (send-postcard
   '(:text
     "Greetings from New York!
Hope everything is alright.

- John"

     :src-city "New York, USA"
     :dst-city "Denver, USA"

     :src-email "john.smith@hostmail.com"
     :dst-email "matt.smith@gmail.com"

     :sent-date "2024-12-18"
     :delivery-date "2024-12-25"
     ))
  )

(defparameter *date-count* 0)

(ql:quickload :local-time)

(defun make-fake-date (time)
  (incf *date-count*)
  (local-time:timestamp+ time *date-count* :day))

(defun get-delivery-date (postcard)
  (local-time:parse-timestring (getf postcard :delivery-date)))

(defun send-scheduled-postcards (db time)
  (let ((fake-date (make-fake-date time)))
    (format t "Delivering today's letters (~A)...~%" fake-date)
    (loop for postcard in db
          append
          (when postcard
            (if (local-time:timestamp>= fake-date (get-delivery-date postcard))
                (progn (deliver-postcard postcard) nil)
                (list postcard))))))

(ql:quickload :bordeaux-threads)

(defparameter *send-interval-s* 1)

(progn
  (setf *date-count* 0)
  (bt:make-thread
   (lambda ()
     (loop while t do
       (let ((current-time (local-time:now)))
         (setf *the-post*
               (send-scheduled-postcards *the-post* current-time))
         (sleep *send-interval-s*))))
   :name "Postman thread"))

(ql:quickload "read-csv")

(defun read-country-db ()
  (let ((csv
          (with-open-file (stream "~/repos/lazypost/coutries-and-states.csv")
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
  (find-if (lambda (country) (search name (getf country :name))) db))

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

;; ~30 days for a letter to go from US to Germany by boat
(defparameter *days-per-km* (/ 30 8237))

(defun delivery-time (distance-km)
  "Estimate delivery time (in days) given a distance (in km)"
  (* *days-per-km* distance-km))

(delivery-time
 (country-distance
  (find-country *country-db* "Norway")
  (find-country *country-db* "Hawaii")))
 ; => 39.93141988370429d0

(delivery-time
 (country-distance
  (find-country *country-db* "Norway")
  (find-country *country-db* "Sweden")))
 ; => 2.044138247852018d0
