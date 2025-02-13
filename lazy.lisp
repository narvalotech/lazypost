(defvar *root-path* "/home/jon/repos/lazypost")
(defparameter *use-db* t)

(defun project-file (path-to-file)
  (parse-namestring
   (concatenate 'string *root-path* "/" path-to-file)))

(defun make-postcard (src-country dst-country
                      src-email dst-email
                      message
                      &key image delivery-date sent-date)
  (declare (ignore image))              ; TODO: add image support
  (list
     :src-country src-country
     :dst-country dst-country
     :src-email src-email
     :dst-email dst-email
     :delivery-date delivery-date
     :sent-date sent-date
     :text message))

(ql:quickload :sqlite)

(defun create-db-table (db)
  (sqlite:execute-non-query
   db
   "create table outbox (id integer primary key,
    delivery_date text not null,
    sent_date text not null,
    dst_country text not null,
    src_country text not null,
    dst_email text not null,
    src_email text not null,
    message text,
    media blob)"))

(defun insert-letter-in-db (db postcard)
  (destructuring-bind (&key
                         text
                         src-country
                         dst-country
                         src-email
                         dst-email
                         delivery-date
                         sent-date
                       &allow-other-keys)
      postcard
    (sqlite:execute-non-query
     db
     "insert into outbox (delivery_date,
                          sent_date,
                          dst_country,
                          src_country,
                          dst_email,
                          src_email,
                          message,
                          media) values (?, ?, ?, ?, ?, ?, ?, ?)"
     delivery-date sent-date
     dst-country src-country
     dst-email src-email
     text nil)))

(defun make-letter-from-db-row (row)
  (destructuring-bind (id
                       delivery-date
                       sent-date
                       dst-country
                       src-country
                       dst-email
                       src-email
                       message
                       media) row
    (declare (ignore id))
    (make-postcard
     src-country
     dst-country
     src-email
     dst-email
     message
     :image media
     :delivery-date delivery-date
     :sent-date sent-date)))

(defun delete-db-row (db id)
  (sqlite:execute-non-query
   db
   "DELETE FROM outbox WHERE id = ?" id))

(defun pop-letters-from-db (db iso-date-str)
  "Get all the letters that have a delivery date < the date string param."
  (mapcar (lambda (letter)
            (delete-db-row db (nth 0 letter))
            (make-letter-from-db-row letter))
          (sqlite:execute-to-list
           db
           "SELECT * FROM outbox WHERE delivery_date <= DATE(?)"
           iso-date-str)))

(defun dump-db (db)
  (sqlite:execute-to-list db "SELECT * FROM outbox"))

(defun init-db (path)
  (sqlite:with-open-database (db path)
    (ignore-errors (create-db-table db))))

(defparameter *db-path* (project-file "db.sqlite"))

;; TODO: move to common init code
(when *use-db*
  (init-db *db-path*))

;; Some tests
;; (sqlite:with-open-database (db *db-path*)
;;   (loop for i from 0 to 10 do
;;     (insert-letter-in-db db (add-dates (generate-letter))))
;;   (dump-db db))

;; (sqlite:with-open-database (db *db-path*)
;;   (pop-letters-from-db
;;    db
;;    (format-date
;;     (local-time:timestamp+ (local-time:now) 15 :day))))

;; (sqlite:with-open-database (db *db-path*)
;;   (dump-db db))

(declaim (optimize (debug 3) (space 0) (speed 0)))

(defparameter *the-post* nil)

(defun pprint-postcard (postcard)
  (destructuring-bind (&key
                         text
                         src-country
                         dst-country
                         src-email
                         dst-email
                       &allow-other-keys)
      postcard
    (with-output-to-string (s)
      (format s "~A -> ~A~%" src-country dst-country)
      (format s "~A -> ~A~%" src-email dst-email)
      (format s "~A" text))))

(ql:quickload :local-time)

(defun postcard-valid? (postcard)
  (and
   (local-time:parse-timestring
    (getf postcard :delivery-date) :fail-on-error nil)))

(defun push-to-outbox (postcard)
  (if *use-db*
      (sqlite:with-open-database (db *db-path*)
          (insert-letter-in-db db postcard))
      (push postcard *the-post*)))

(defun send-postcard (postcard)
  "Send a delayed postcard. Only accepts text."
  ;; FIXME: validate postcard before adding it
  (when (postcard-valid? postcard)
    (format t "Queue: ~A -> ~A~%"
            (getf postcard :src-email)
            (getf postcard :dst-email))
    (push-to-outbox postcard)))

(defparameter *date-count* 0)

(defun make-fake-date (time)
  (incf *date-count*)
  (local-time:timestamp+ time *date-count* :day))

(defun get-delivery-date (postcard)
  (local-time:parse-timestring (getf postcard :delivery-date)))

(defun pull-from-the-post (date)
  (let ((to-send '()))
    (setf *the-post*
          (loop for postcard in *the-post*
                append
                (when postcard
                  (if (local-time:timestamp>= date (get-delivery-date postcard))
                      (progn (push postcard to-send) nil)
                      (list postcard)))))
    to-send))

(ql:quickload :cl-smtp)

(defparameter *use-smtp* nil)

(defun send-email-smtp (from name to subject message &optional attachments)
  (cl-smtp:send-email *smtp-server* *smtp-user* to subject message
                      :attachments attachments
                      :cc from
                      :reply-to from
                      :display-name name
                      :authentication (list :login *smtp-user* *smtp-pass*)
                      :ssl :tls
                      ))

(defun deliver-postcard-real (postcard)
 (destructuring-bind (&key
                         text
                         src-email
                         dst-email
                       &allow-other-keys)
      postcard
   (when *use-smtp*
     (send-email-smtp
      src-email "The Lazypost Company"
      dst-email "Digital postcard"
      text))))

(defun deliver-postcard-fake (postcard)
  (format t "##### You've got mail. #####~%~A~%~%"
          (pprint-postcard postcard)))

(defun format-date (time)
  (local-time:format-timestring
   nil
   time
   :format
   '(:year "-"
     (:month 2) "-"
     (:day 2))))

(defun pull-from-outbox (time)
  (if *use-db*
      (sqlite:with-open-database (db *db-path*)
        (pop-letters-from-db db (format-date time)))
      (pull-from-the-post time)))

(defun deliver-postcard (postcard)
  (if *use-smtp*
      (deliver-postcard-real postcard)
      (deliver-postcard-fake postcard)))

(defun send-scheduled-postcards (time)
  (let ((fake-date (make-fake-date time)))
    (format t "Delivering today's letters (~A)...~%" fake-date)
    (mapcar #'deliver-postcard (pull-from-outbox fake-date))))

(ql:quickload :bordeaux-threads)

(defparameter *send-interval-s* 1)

;; (progn
;;   (setf *date-count* 0)
;;   (bt:make-thread
;;    (lambda ()
;;      (loop while t do
;;        (let ((current-time (local-time:now)))
;;          (send-scheduled-postcards current-time)
;;          (sleep *send-interval-s*))))
;;    :name "Postman thread"))

(ql:quickload "read-csv")

(defun read-country-db ()
  (let ((csv
          (with-open-file (stream (project-file "coutries-and-states.csv"))
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

;; ~30 days for a letter to go from US to Germany by boat
(defparameter *days-per-km* (/ 30 8237))

(defun delivery-time (distance-km)
  "Estimate delivery time (in days) given a distance (in km)"
  (round (* *days-per-km* distance-km)))

(delivery-time
 (country-distance
  (find-country *country-db* "Norway")
  (find-country *country-db* "Hawaii")))
 ; => 40, -0.06858011629571337d0

(delivery-time
 (country-distance
  (find-country *country-db* "Norway")
  (find-country *country-db* "Sweden")))
 ; => 2, 0.04413824785201781d0

(defun rand-country (db)
  (let* ((len (length db))
         (index (random len)))
    (getf (nth index db) :name)))

(rand-country *country-db*)
 ; => "Philippines"

(defparameter *names*
  '("Todd"
    "Julian"
    "Peter"
    "Susan"
    "Tasha"
    "Annie"
    "Sarah"))

(defparameter *greetings*
  '(("Hello" "See ya")
    ("Hola" "Hasta la proxima")
    ("Greetings" "Love")))

(defun generate-letter ()
  (let ((src-country (rand-country *country-db*))
        (dst-country (rand-country *country-db*))
        (src-name (nth (random (length *names*)) *names*))
        (dst-name (nth (random (length *names*)) *names*))
        (greeting (nth (random (length *greetings*)) *greetings*)))
    (list
     :src-country src-country
     :dst-country dst-country
     :src-email (format nil "~A@lazypost.net" src-name)
     :dst-email (format nil "test-~A@rico.live" dst-name)
     :text
     (format nil "~A from ~A!~%~%~A, ~A"
             (nth 0 greeting)
             src-country
             (nth 1 greeting)
             dst-name))))

(generate-letter)
;  => (:SRC-COUNTRY "Nebraska" :DST-COUNTRY "South Dakota" :SRC-EMAIL
;  "Peter@lazypost.net" :DST-EMAIL "Sarah@lazypost.net" :TEXT
;  "Hello from Nebraska!

; See ya, Sarah")

(defun add-dates (postcard)
  (let ((datestr (format-date
                  (local-time:timestamp+
                   (local-time:now)
                   (delivery-time
                    (country-distance
                     (find-country *country-db* (getf postcard :src-country))
                     (find-country *country-db* (getf postcard :dst-country))))
                   :day))))
    (setf (getf postcard :sent-date) (format-date (local-time:now)))
    (setf (getf postcard :delivery-date) datestr)
    postcard))

(add-dates (generate-letter))
;  => (:DELIVERY-DATE "2025-02-05" :SENT-DATE "2025-01-04" :SRC-COUNTRY "Mali"
;  :DST-COUNTRY "British Indian Ocean Territory" :SRC-EMAIL "Tasha@lazypost.net"
;  :DST-EMAIL "Susan@lazypost.net" :TEXT "Greetings from Mali!

; Love, Susan")

;; (loop for i from 0 to 10 collect
;;       (send-postcard (add-dates (generate-letter))))

(defun handle-send (env)
  (declare (ignore env))
  (list
   200
   '(:content-type "text/html")
   (project-file "front/index.html")
   ))

(defun handle-error (&optional context)
  (list 400 '(:content-type "text/plain; charset=utf-8")
        (if context
            (list context)
            '("[400] ilo li sona ala"))))

(defun handle-static (env)
  ;; TODO: check that request targets localhost
  (let* ((filename (getf env :path-info))
         (ttt (search ".." filename)))
    (if ttt
        (handle-error)                  ; 100% secure, nothing to see here
        (list
         200
         nil
         (project-file filename)))))

(ql:quickload :http-body)

(defun param-is-binary? (param)
  ;; http-body returns a cons if the parameter is text and a list
  ;; if the param is binary or image.
  (consp (cdr param)))

(defun param-is-text? (param)
  (not (param-is-binary? param)))

(defun param-is-image? (param)
  (and (param-is-binary? param)
       (equalp (nth 3 param) "image/jpeg")))

(defun parse-param (param)
  (let ((param-type
          (cond ((param-is-text? param) :text)
                ((param-is-image? param) :image)
                (t :unknown))))
    (list param-type param)))

;; TODO: error on country not found
(defun form-params-ok? (parsed)
  ;; FIXME: validate parameters server-side
  ;; (format t "~A~%" parsed)
  (declare (ignore parsed))
  t)

(defun read-param (name params)
  (loop for param in params do
    (let ((pname (car (cadr param)))
          (pval (cdr (cadr param))))
      (when (equalp name pname) (return pval)))))

(defun postcard-sent ()
  (list 200 '(:content-type "text/plain; charset=utf-8")
        ;; TODO: Add delivery delay?
        '("Your postcard will be delivered in a few days.")))

(defun postcard-not-sent (&optional context)
  (handle-error context))

(defun error-if-not-valid (postcard)
  (unless
      (find-country *country-db* (getf postcard :src-country))
    (error "Country not found in database: ~A" (getf postcard :src-country)))

  (unless
      (find-country *country-db* (getf postcard :dst-country))
    (error "Country not found in database: ~A" (getf postcard :dst-country))))

(defun post-post (env)
  (let* ((params (http-body:parse (getf env :content-type)
                                  (getf env :content-length)
                                  (getf env :raw-body)))
         (parsed (mapcar #'parse-param params)))
    ;; (format t "processing: ~A~%" parsed)
    (if (not (form-params-ok? parsed))
        (postcard-not-sent)
        (handler-case
            (progn
              (let ((postcard (make-postcard
                               (read-param "country-sender" parsed)
                               (read-param "country-recipient" parsed)
                               (read-param "email-sender" parsed)
                               (read-param "email-recipient" parsed)
                               (read-param "message" parsed))))
                (error-if-not-valid postcard)
                (send-postcard (add-dates postcard))
                (postcard-sent)))
          ;; TODO: add invalid country as a custom error
          (t (c)
            (progn
              (format t "Got exception: ~a~%" c)
              (postcard-not-sent (format nil "~a" c))))))))

(defun response (env)
  ;; (format t "query-string: ~A~%" (getf env :query-string))
  ;; (break)

  (cond
    ((eql :post (getf env :request-method))
     (post-post env))

    ((eql :get (getf env :request-method))
     (cond
       ((search "/front/" (getf env :path-info) :test #'equalp) (handle-static env))
       ((equalp "/send" (getf env :path-info)) (handle-send env))
       (t (handle-error))))

    (t (handle-error))))

(ql:quickload :clack)

(defvar *handler* (clack:clackup 'response :address "0.0.0.0" :port 8000))

