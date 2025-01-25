(defun get-form ()
  "this is a form")

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
      (read-postcard postcard)
    (with-output-to-string (s)
      "Send a delayed postcard. Only accepts text."
      (format s "~A -> ~A~%" src-country dst-country)
      (format s "~A -> ~A~%" src-email dst-email)
      (format s "~A" text))))

(defun postcard-valid? (postcard)
  (and
   (local-time:parse-timestring
    (getf postcard :delivery-date) :fail-on-error nil)))

(defun send-postcard (postcard)
  "Send a delayed postcard. Only accepts text."
  ;; FIXME: validate postcard before adding it
  (when (postcard-valid? postcard)
    (push postcard *the-post*)))

(defun deliver-postcard (postcard)
  (format t "##### You've got mail. #####~%~A~%~%" (pprint-postcard postcard)))

(progn
  (setf *the-post* nil)
  (send-postcard
   '(:text
     "Hello from Oslo!
Made a new friend, Ole nordmann.

xoxo Matt"

     :src-country "Oslo, Norway"
     :dst-country "Denver, USA"

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

     :src-country "New York, USA"
     :dst-country "Denver, USA"

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

;; (progn
;;   (setf *date-count* 0)
;;   (bt:make-thread
;;    (lambda ()
;;      (loop while t do
;;        (let ((current-time (local-time:now)))
;;          (setf *the-post*
;;                (send-scheduled-postcards *the-post* current-time))
;;          (sleep *send-interval-s*))))
;;    :name "Postman thread"))

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
     ;; TODO: use country instead of city
     :src-country src-country
     :dst-country dst-country
     :src-email (format nil "~A@lazypost.net" src-name)
     :dst-email (format nil "~A@lazypost.net" dst-name)
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

(defun format-date (time)
  (local-time:format-timestring
   nil
   time
   :format
   '(:year "-"
     (:month 2) "-"
     (:day 2))))

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

;; (loop for i from 0 to 40 collect
;;       (send-postcard (add-dates (generate-letter))))

;; TODO: use something with less deps than spinneret
(ql:quickload :spinneret)

(defmacro form-image (class name text)
  (spinneret:with-html-string
    (:div :class class
     (:label :for name text)
     (:input :type :file
             :name name :id name
             :accept "image/jpeg"))))

(defmacro form-item (class name text &key is-email)
  (spinneret:with-html-string
    (:div :class class
     (:label :for name text)
     (:input :type (if is-email :email :text)
             :name name :id name :required t))))

(defun handle-send (env)
  (declare (ignore env))
  (list
   200
   '(:content-type "text/html")
   (list
    (spinneret:with-html-string
      (:doctype)
      (:head
       (:title "The Lazy Post Company"))
      (:body
       (:form
        :action "#" :method :post :class "form-example" :enctype "multipart/form-data"
        (:raw
         (form-item "form-example" "destination" "Destination country")
         (form-item "form-example" "origin" "Origin country")
         (form-item "form-example" "email-from" "Your email" :is-email t)
         (form-item "form-example" "email-to" "Destination email" :is-email t)
         (form-item "form-example" "message" "Message") ; TODO: use `textbox' for resizing
         (form-image "form-example" "picture" "Picture"))
        (:div
         :class "form-example"
         (:input :type :submit :value "Send"))))
      ))))

(defun handle-error ()
  (list 400 '(:content-type "text/plain; charset=utf-8")
        '("[400] ilo ala sona")))

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

(defun make-postcard (src-country dst-country
                      src-email dst-email
                      message &optional image)
  (declare (ignore image))              ; TODO: add image support
  (list
     :src-country src-country
     :dst-country dst-country
     :src-email src-email
     :dst-email dst-email
     :text message))

;; TODO: error on country not found
(defun form-params-ok? (parsed)
  ;; FIXME: validate parameters server-side
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
        '("Your postcard has been sent. It will be delivered in a few days.")))

(defun postcard-not-sent ()
  (handle-error))

(defun post-post (env)
  (let* ((params (http-body:parse (getf env :content-type)
                                  (getf env :content-length)
                                  (getf env :raw-body)))
         (parsed (mapcar #'parse-param params)))
    (format t "processing: ~A~%" parsed)
    (if (not (form-params-ok? parsed))
        (postcard-not-sent)
        (handler-case
            (progn
              (send-postcard
               (add-dates
                (make-postcard
                 (read-param "origin" parsed)
                 (read-param "destination" parsed)
                 (read-param "email-from" parsed)
                 (read-param "email-to" parsed)
                 (read-param "message" parsed))))
              (postcard-sent))
          (t (c)
            (progn
              (format t "Got exception: ~a~%" c)
              (postcard-not-sent)))))))

(defun response (env)
  ;; (format t "query-string: ~A~%" (getf env :query-string))
  ;; (break)

  (when (eql :post (getf env :request-method))
    (post-post env))

  (unless (eql :get (getf env :request-method))
    (handle-error))

  ;; Handle GET requests
  (cond ((equalp "/send" (getf env :path-info))
         (handle-send env))
        (t (handle-error))))

(ql:quickload :clack)

(defvar *handler* (clack:clackup 'response :address "0.0.0.0" :port 8000))

(ql:quickload :cl-smtp)

(defun send-email (from name to subject message &optional attachments)
  (cl-smtp:send-email *smtp-server* *smtp-user* to subject message
                      :attachments attachments
                      :cc from
                      :reply-to from
                      :display-name name
                      :authentication (list :login *smtp-user* *smtp-pass*)
                      :ssl :tls
                      ))

;; (send-email "random@rico.live"
;;             "Mailey McEmailface"
;;             "test@rico.live"
;;             "Postcard from Mailey"
;;             "O hai")
