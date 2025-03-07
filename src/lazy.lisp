(defvar *root-path* "/home/jon/repos/lazypost")
(defparameter *use-db* t)

(ql:quickload :local-time)

(defparameter *log-levels*
  '(:tra (5 "TRAC")
    :dbg (4 "DEBG")
    :inf (3 "INFO")
    :wrn (2 "WARN")
    :err (1 "ERRR")
    ))

(defparameter *current-log-level* :dbg)

(defun yeet-log-message (severity)
  (let ((current-severity (nth 0 (getf *log-levels* *current-log-level*)))
        (msg-severity (nth 0 (getf *log-levels* severity))))
    (> msg-severity current-severity)))

(yeet-log-message :dbg)
 ; => T
(yeet-log-message :inf)
 ; => NIL
(yeet-log-message :wrn)
 ; => NIL

(defun poor-mans-log (severity message)
  (unless (yeet-log-message severity)
    (format *error-output*  "[~A] ~A: ~A~%"
            (local-time:now)
            (nth 1 (getf *log-levels* severity))
            message)))

(defun log-trace (message) (poor-mans-log :tra message))
(defun log-dbg (message) (poor-mans-log :dbg message))
(defun log-inf (message) (poor-mans-log :inf message))
(defun log-wrn (message) (poor-mans-log :wrn message))
(defun log-err (message) (poor-mans-log :err message))

(defun project-file (path-to-file)
  (parse-namestring
   (concatenate 'string *root-path* "/" path-to-file)))

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

(defun create-db-table (db)
  (sqlite:execute-non-query
   db
   "create table outbox (id integer primary key,
    lid text not null,
    delivery_date text not null,
    sent_date text not null,
    dst_country text not null,
    src_country text not null,
    dst_email text not null,
    src_email text not null,
    message text,
    media_name text,
    media blob)"))

(defun get-image-name (image)
  (if image (nth 1 image) nil))

(defun get-image-data (image)
  (if image (nth 0 image) nil))

(defun make-image (name data)
  (when (and name data)
    (list data name)))

(defun insert-letter-in-db (db postcard)
  (destructuring-bind (&key
                         lid
                         text
                         src-country
                         dst-country
                         src-email
                         dst-email
                         delivery-date
                         sent-date
                         image
                       &allow-other-keys)
      postcard
    (sqlite:execute-non-query
     db
     "insert into outbox (lid,
                          delivery_date,
                          sent_date,
                          dst_country,
                          src_country,
                          dst_email,
                          src_email,
                          message,
                          media_name,
                          media) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     lid
     delivery-date sent-date
     dst-country src-country
     dst-email src-email
     text
     (get-image-name image) (get-image-data image))))

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
    (declare (ignore id))
    (make-postcard
     lid
     src-country
     dst-country
     src-email
     dst-email
     message
     :image (make-image media-name media)
     :delivery-date delivery-date
     :sent-date sent-date)))

(defun delete-db-row-from-lid (db lid)
  (sqlite:execute-non-query
   db
   "DELETE FROM outbox WHERE lid = ?" lid))

(defun delete-db-row (db id)
  (sqlite:execute-non-query
   db
   "DELETE FROM outbox WHERE id = ?" id))

(defun peek-letters-from-db (db iso-date-str)
  "Get all the letters that have a delivery date < the date string param."
  (mapcar (lambda (letter)
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

(defparameter *db-path* (project-file "data/db.sqlite"))

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
    (log-inf (format nil  "[~A] Queue: ~A -> ~A"
                     (getf postcard :lid)
                     (getf postcard :src-email)
                     (getf postcard :dst-email)))
    (push-to-outbox postcard)))

(defun get-delivery-date (postcard)
  (local-time:parse-timestring (getf postcard :delivery-date)))

(defun pull-lid-from-the-post (lid)
  ;; WOW this function is badly named lol
  (setf *the-post*
        (remove-if (lambda (letter)
                     (equalp (getf letter :lid) lid))
                   *the-post*)))

(defun peek-from-the-post (date)
  (loop for postcard in *the-post*
        append
        (when (and
               postcard
               (local-time:timestamp>= date (get-delivery-date postcard)))
          (list postcard))))

(ql:quickload :cl-smtp)

(defvar *use-smtp* nil)
(defvar *smtp-server* nil)
(defvar *smtp-user* nil)
(defvar *smtp-pass* nil)

(defun send-email-smtp (from name to subject message &optional attached-file)
  (cl-smtp:send-email *smtp-server* *smtp-user* to subject message
                      :attachments attached-file
                      :cc from
                      :reply-to from
                      :display-name name
                      :authentication (list :login *smtp-user* *smtp-pass*)
                      :ssl :tls
                      ))

(ql:quickload :sendgrid)

(defvar *use-sendgrid* nil)

;; TODO: use BT:WITH-TIMEOUT on this. I've seen it get stuck sometimes.
(defun send-email-sendgrid (from name to subject message &optional attached-file)
  (if attached-file
      (sendgrid:send-email :to to
                           :from "service@lazypost.net"
                           :reply-to (list (cons "email" from)
                                           (cons "name" from))
                           :from-name name
                           :subject subject
                           :content message
                           :send-at 0
                           :attachments t
                           :file (namestring attached-file)
                           :filename "picture.jpg")

      (sendgrid:send-email :to to
                           :from "service@lazypost.net"
                           :reply-to (list (cons "email" from)
                                           (cons "name" from))
                           :from-name name
                           :subject subject
                           :content message
                           :send-at 0)))

(defun send-postcard-fake (postcard)
  (log-inf (format nil  "##### You've got mail. #####~%~A~%"
                   (pprint-postcard postcard))))

(defun make-subject (postcard)
  (format nil "Digital postcard from ~A"
          (getf postcard :src-country)))

(defparameter *temp-image-file* (project-file "data/picture.jpg"))

(defun write-to-temporary-file (input-vector)
  "Make a temp file and write the input (vector) to it."
  ;; We can re-use the same file:
  ;; - we don't want disk space to balloon up
  ;; - we will be sending from a single thread
  (with-open-file (stream *temp-image-file*
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'unsigned-byte)
    (write-sequence input-vector stream))
  *temp-image-file*)

(defun destroy-letter (lid)
  (log-dbg (format nil "Deleting letter ~A" lid))
  (if *use-db*
      (sqlite:with-open-database (db *db-path*)
        (delete-db-row-from-lid db lid))
      (pull-lid-from-the-post lid)))

(defparameter *send-timeout* 3)

(defvar *send-page-path* "/send")

(defun message-footer (src-email)
  (format nil "~%
------------------
This postcard was sent by ~A through lazypost.net
To send one back, go to lazypost.net~A

If you think this is SPAM, please forward this email
to abuse@lazypost.net
"
          src-email
          *send-page-path*))

(defun append-footer (text src-email)
  (concatenate 'string
               text (message-footer src-email)))

(defun deliver-postcard (postcard)
  (destructuring-bind (&key
                         lid
                         text
                         src-email
                         dst-email
                         image
                       &allow-other-keys)
      postcard
    (log-inf (format nil  "[~A] Send: ~A -> ~A" lid src-email dst-email))

    (bt:with-timeout (*send-timeout*)
      (let ((attached-file
              (when image
                (write-to-temporary-file (nth 0 image)))))
        (cond
          (*use-sendgrid* (send-email-sendgrid
                           src-email "The Lazypost Company"
                           dst-email (make-subject postcard)
                           (append-footer text src-email)
                           attached-file))

          (*use-smtp* (send-email-smtp
                       src-email "The Lazypost Company"
                       dst-email (make-subject postcard)
                       (append-footer text src-email)
                       attached-file))

          (t (send-postcard-fake postcard))
          )))

    (destroy-letter lid)))

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
        (peek-letters-from-db db (format-date time)))
      (peek-from-the-post time)))

(defparameter *date-count* 0)

(defun make-fake-date (time)
  (incf *date-count*)
  (local-time:timestamp+ time *date-count* :day))

(defun send-scheduled-postcards-realquick (time)
  (let ((fake-date (make-fake-date time)))
    (log-trace (format nil  "Delivering letters: ~A..." fake-date))
    (mapcar #'deliver-postcard (pull-from-outbox fake-date))))

(defun send-scheduled-postcards (time)
  (log-trace (format nil  "Delivering letters: ~A..." time))
  (mapcar #'deliver-postcard (pull-from-outbox time)))

(ql:quickload :bordeaux-threads)

(defvar *send-interval-s* 3)

(defvar *air-mail* t)
(defparameter *stop-send-thread* nil)

(defun sleep-one-eye-open (seconds)
  (let ((small-interval .5)
        (slept 0))
    (loop while (and (not *stop-send-thread*)
                     (< slept seconds))
          do
             (progn
               (sleep small-interval)
               (incf slept small-interval))))
  (not *stop-send-thread*))

(defun spawn-send-thread (periodic-fn)
  (setf *date-count* 0)
  (bt:make-thread
   (lambda ()
     (loop while (sleep-one-eye-open *send-interval-s*) do
       (funcall periodic-fn)
       (let ((current-time (local-time:now)))
         (handler-case
             (if *air-mail*
                 (send-scheduled-postcards-realquick current-time)
                 (send-scheduled-postcards current-time))
           (t (c) (log-err (format nil "Unable to send: ~A~%" c)))))))
   :name "Postman thread"))

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

(defun static-file (filename)
  (project-file (concatenate 'string "front/" filename)))

(defun handle-static (env)
  ;; TODO: check that request targets localhost
  (let* ((filename (getf env :path-info))
         (ttt (search ".." filename)))
    (if ttt
        (handle-error)                  ; 100% secure, nothing to see here
        (list
         200
         nil
         (static-file filename)))))

(ql:quickload :http-body)

(ql:quickload :ironclad)

(defun sha256 (data)
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :sha256
                             (ironclad:ascii-string-to-byte-array data))))

;; (sha256 "test")
 ; => "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"

(defun compute-challenge (salt secret)
  (sha256 (format nil "~A~A" salt secret)))

;; (compute-challenge 1230980145 123456)
 ; => "02fd164f30603436624743f4001ed38cd33f2eed0e7f4ba3d5c4660e61e710a9"

(defun get-unix-time ()
  (local-time:timestamp-to-unix
   (local-time:now)))

;; (get-unix-time)
 ; => 1740847677 (31 bits, #x67C33A3D)

(defun calculate-challenge-difficulty (ip)
  ;; For now, we hardcode it.
  ;; TODO: increase exponentially by looking at previous requests from IP.
  ;; TODO: lower difficulty when using mobile Or distribute small challenges,
  ;; until a specific measured time is reached on the server.
  (declare (ignore ip))
  4)

(defun pad-number (number digits)
  (loop while (> 1 (/ number (expt 10 (- digits 1))))
        do (setf number (* number 10)))
  number)

(pad-number 12345 6)
 ; => 123450 (17 bits, #x1E23A)

(pad-number 123456 6)
 ; => 123456 (17 bits, #x1E240)

(defun generate-secret (level)
  "Generate a secret number with LEVEL amount of digits"
  (pad-number (random (expt 10 level)) level))

(generate-secret 6)
 ; => 113500 (17 bits, #x1BB5C)

(defun make-challenge (ip hash time salt secret)
  (list
   :ip ip
   :hash hash
   :time time
   :salt salt
   :secret secret))

(defun generate-challenge (ip)
  (let* ((difficulty (calculate-challenge-difficulty ip))
         (time (get-unix-time))
         (secret (generate-secret difficulty))
         (salt (sxhash (format nil "~A~A~A" ip time (random 1000)))))

    (log-dbg (format nil "Generating challenge: IP ~A TS ~A SECRET ~A"
                     ip time secret))

    (make-challenge
     ip
     (compute-challenge salt secret)
     time
     salt
     secret)))

(generate-challenge "127.0.0.1")
 ; => (:IP "127.0.0.1" :HASH
 ; "430621b3a0072fcdf4c8413eefad55e7e2d4f940aa5d1f27882f29a199551665" :TIME
 ; 1740851506 :SALT 2184513279468597411 :SECRET 991550)

(ql:quickload :cl-json)

(defun challenge->json (hash salt)
  (json:encode-json-to-string
   (list
    (cons "challenge"
          (list
           (cons "hash" hash)
           (cons "salt" salt))))))

(challenge->json "abc" "1234")
 ; => "{\"challenge\":{\"hash\":\"abc\",\"salt\":\"1234\"}}"

(defun store-challenge (table ip unix-time salt secret)
  (setf (gethash salt table)
        (make-challenge
         ip "" unix-time salt secret)))

(defparameter *challenges*
  (make-hash-table))

(defun long-ago (old now)
  (local-time:timestamp<
   old
   (local-time:timestamp- now 10 :minute)))

(defun delete-expired-challenge (key value)
  (let* ((challenge-unixtime (getf value :time))
         (challenge-ts
           (local-time:unix-to-timestamp challenge-unixtime)))

    (when (long-ago challenge-ts
                    (local-time:now))
      (log-dbg (format nil "Deleting old challenge: IP ~A secret ~A"
                       (getf value :ip)
                       (getf value :secret)))
      (remhash key *challenges*))))

;; (maphash #'delete-expired-challenge *challenges*)

(defun delete-expired-challenges ()
  (maphash #'delete-expired-challenge *challenges*))

(defun generate-and-store-challenge (ip)
  (let ((challenge (generate-challenge ip)))
    (destructuring-bind (&key time salt secret &allow-other-keys)
        challenge

      ;; TODO: perform garbage collection here?
      (store-challenge *challenges* ip time salt secret)

      challenge)))

(defun handle-challenge-request (env)
  (let ((ip (getf env :remote-addr)))
    (destructuring-bind (&key hash salt &allow-other-keys)
        (generate-and-store-challenge ip)

      (log-inf (format nil "Serving challenge for ~A" ip))

      (list
       200
       '(:content-type "text/json")
       (list (challenge->json hash (format nil "~A" salt)))))))

(defun verify-challenge-response (client-ip salt answer)
  (let ((challenge (gethash salt *challenges*)))
    (when challenge
      (remhash salt *challenges*)       ; No brute-forcing here..
      (destructuring-bind (&key time secret ip &allow-other-keys)
          challenge
        (and
         (not (long-ago (local-time:unix-to-timestamp time)
                        (local-time:now)))
         (equalp ip client-ip)
         (= secret answer))))))

(defun param-is-binary? (param)
  ;; http-body returns a cons if the parameter is text and a list
  ;; if the param is binary or image.
  (consp (cdr param)))

(defun param-is-text? (param)
  (not (param-is-binary? param)))

(defun param-is-image? (param)
  (and (param-is-binary? param)
       (equalp (nth 3 param) "image/jpeg")))

(defun stream->vector (stream &key
                                (element-type '(unsigned-byte 8))
                                (initial-size 1024))
  "Read the entire stream into a vector of the specified element-type.
   Returns the filled vector."
  ;; I didn't write this. Phind did.
  (let ((buffer (make-array initial-size
                            :element-type element-type
                            :adjustable t
                            :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (vector-push-extend byte buffer))
    buffer))

(defun vector->sarray (input)
  (let ((simple-array (make-array (length input)
                                  :element-type '(unsigned-byte 8))))
    (replace simple-array input)
    simple-array))

(defun parse-param (param)
  (let ((param-type
          (cond ((param-is-text? param) :text)
                ((param-is-image? param) :image)
                (t :unknown))))

    ;; The param as passed from CLACK is a FLEXI-STREAM of type input vector. We
    ;; want to extract the vector as soon as possible to not have to deal with
    ;; it later in the call tree.
    ;;
    ;; Note: sometimes, it's a FLEXI-STREAM of another kind, but not already
    ;; present in memory. It seems to depend on the file size. So we can't
    ;; really depend on the internal structure, and have to read from a
    ;; "generic" stream.
    (when (eql param-type :image)
      (let ((data (vector->sarray (stream->vector (nth 1 param))))
            (filename (nth 2 param))
            (param-name (nth 0 param)))
        (setf param (list param-name data filename))))

    (list param-type param)))

(defun read-param (params type name)
  (loop for param in params do
    (let ((ptype (car param))
          (pname (car (cadr param)))
          (pval (cdr (cadr param))))
      (when (and (equalp type ptype) (equalp name pname))
        (return pval)))))

(defun postcard-sent ()
  (list
   200
   '(:content-type "text/html")
   (project-file "front/sent.html")))

(defun postcard-not-sent (&optional context)
  (handle-error context))

(defun looks-like-mail-address? (email)
  (let ((at-position (search "@" email)))
    (and
     at-position
     (> at-position 0)
     (< at-position (- (length email) 1)))))

(defvar *allowed-addresses* nil)

(defun address-allowed? (email)
  (if *allowed-addresses*
      (not (null (find-if (lambda (seq) (search seq email)) *allowed-addresses*)))
      t))

(defun valid-email (email)
  "Minimum validation"
  (and (looks-like-mail-address? email)
       (address-allowed? email)))

(valid-email "test@example.com")
 ; => T
(valid-email "t@a")
 ; => T
(valid-email "@example.com")
 ; => NIL
(valid-email "test@")
 ; => NIL
(valid-email "test")
 ; => NIL

(defun image-too-big (image)
  (< (length (car image)) 2000000))

(defun correct-answer? (response)
  (destructuring-bind (&key ip salt answer)
      response
    (verify-challenge-response ip
                               (parse-integer salt)
                               (parse-integer answer))))

(defun error-if-not-valid (postcard challenge-rsp)
  (destructuring-bind (&key
                         src-country
                         dst-country
                         src-email
                         dst-email
                         image
                       &allow-other-keys)
      postcard

    (unless (correct-answer? challenge-rsp)
      (error "Incorrect or stale challenge answer"))

    (unless
        (find-country *country-db* src-country)
      (error "Country not found in database: ~A" src-country))

    (unless
        (find-country *country-db* dst-country)
      (error "Country not found in database: ~A" dst-country))

    (unless (valid-email src-email) (error "Invalid email: ~A" src-email))
    (unless (valid-email dst-email) (error "Invalid email: ~A" dst-email))
    (unless (image-too-big image) (error "Image too large (>2MB)"))
    ))

(defun generate-letter-id ()
  (format nil "~A-~A"
          (get-universal-time)
          (random 1000000)))

(generate-letter-id)
 ; => "3948711498-551011"

(defun post-post (env)
  (let* ((params (http-body:parse (getf env :content-type)
                                  (getf env :content-length)
                                  (getf env :raw-body)))
         (parsed (mapcar #'parse-param params))
         (challenge-rsp (list :ip (getf env :remote-addr)
                              :salt (read-param parsed :text "s")
                              :answer (read-param parsed :text "a"))))
    ;; (log-dbg (format nil  "processing: ~A%" parsed))
    (handler-case
        (progn
          (let ((postcard (make-postcard
                           (generate-letter-id)
                           (read-param parsed :text "country-sender")
                           (read-param parsed :text "country-recipient")
                           (read-param parsed :text "email-sender")
                           (read-param parsed :text "email-recipient")
                           (read-param parsed :text "message")
                           :image (read-param parsed :image "picture"))))
            (error-if-not-valid postcard challenge-rsp)
            (send-postcard (add-dates postcard))
            (postcard-sent)))
      ;; TODO: add invalid country as a custom error
      (t (c)
        (progn
          (log-err (format nil "Got exception when processing ~a: ~a" params c))
          (postcard-not-sent (format nil "~a" c)))))))

(defun response (env)
  ;; (log-dbg (format nil  "query-string: ~A" (getf env :query-string)))
  ;; (break)

  (cond
    ((eql :post (getf env :request-method))
     (post-post env))

    ((eql :get (getf env :request-method))
     (alexandria:switch ((getf env :path-info) :test #'equalp)

       ("/request-challenge" (handle-challenge-request env))

       (*send-page-path* (handle-send env))

       ;; TODO: change this to redirect to about page when we have it
       ("/index.html" (handle-error))

       (t (handle-static env))
       ))

    (t (handle-error))))

(ql:quickload :clack)

(defvar *port* 8000)

(defvar *webapp* (clack:clackup 'response
                                :address "0.0.0.0" :port *port*
                                :debug nil))

(defvar *send-thread* (spawn-send-thread #'delete-expired-challenges))

(ql:quickload :trivial-signal)

(defun handle-termination (signo)
  (log-inf (format nil "Graceful shutdown (SIG~A)"
                   (trivial-signal:signal-name signo)))

  (log-inf "Stopping webapp")
  (clack:stop *webapp*)

  (log-inf "Stopping email thread")
  (setf *stop-send-thread* t)
  (bt:join-thread *send-thread*)

  (log-inf "Done")
  (finish-output *error-output*)

  (uiop:quit 0)
  )

;; Simulate SIGTERM
;; (handle-termination 15)

;; Install the handler
(log-inf "Application started")

(defun install-handlers-and-run ()
  (trivial-signal:signal-handler-bind ((:term #'handle-termination)
                                       (:int #'handle-termination))
    (loop while t do (sleep 1))))
