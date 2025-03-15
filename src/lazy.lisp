(defvar *root-path* "/home/jon/repos/lazypost")
(defparameter *use-db* t)

(ql:quickload :local-time)
(ql:quickload :bordeaux-threads)

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

(defvar *db-lock* (bt:make-lock))

(defun init-db (path)
  (bt:with-lock-held (*db-lock*)
    (sqlite:with-open-database (db path)
      (ignore-errors (create-db-table db)))))

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
  (bt:with-lock-held (*db-lock*)
    (if *use-db*
        (sqlite:with-open-database (db *db-path*)
          (insert-letter-in-db db postcard))
        (push postcard *the-post*))))

(defun send-postcard (postcard)
  "Send a delayed postcard. Only accepts text."
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
  (bt:with-lock-held (*db-lock*)
    (if *use-db*
        (sqlite:with-open-database (db *db-path*)
          (delete-db-row-from-lid db lid))
        (pull-lid-from-the-post lid))))

(defparameter *send-timeout* 3)

(defvar *send-page-path* "/send")

(defun how-fast? (postcard)
  (let* ((delivery (local-time:parse-timestring (getf postcard :delivery-date)))
         (sent (local-time:parse-timestring (getf postcard :sent-date)))
         (travel-time-days
           (round
            (/ (local-time:timestamp-difference delivery sent) 60 60 24))))
    (if (zerop travel-time-days)
        "less than a day"
        (format nil "only ~A days" travel-time-days))))

(how-fast? '(:delivery-date "2025-03-10" :sent-date "2025-02-27"))
 ; => "only 11 days"

(how-fast? '(:delivery-date "2025-02-27" :sent-date "2025-02-27"))
 ; => "less than a day"

(defun message-footer (postcard)
  (destructuring-bind (&key
                         src-email
                         src-country
                         dst-country
                       &allow-other-keys)
      postcard
    (format nil "~%
------------------
This postcard was sent by ~A through lazypost.net
To send one back, go to lazypost.net~A

It travelled from ~A to ~A in ~A!

If you think this is SPAM, please forward this email
to abuse@lazypost.net
"
            src-email
            *send-page-path*
            src-country dst-country (how-fast? postcard))))

(defun append-footer (text postcard)
  (concatenate 'string
               text (message-footer postcard)))

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
                           (append-footer text postcard)
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
  (bt:with-lock-held (*db-lock*)
    (if *use-db*
        (sqlite:with-open-database (db *db-path*)
          (peek-letters-from-db db (format-date time)))
        (peek-from-the-post time))))

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

(defvar *send-interval-s* 3)

(defvar *air-mail* t)
(defparameter *stop-app-threads* nil)

(defun sleep-one-eye-open (seconds)
  (let ((small-interval .5)
        (slept 0))
    (loop while (and (not *stop-app-threads*)
                     (< slept seconds))
          do
             (progn
               (sleep small-interval)
               (incf slept small-interval))))
  (not *stop-app-threads*))

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

(defun generate-iv (bytes)
  (ironclad:make-random-salt bytes))

(defun make-aes-cipher (key &optional (iv (generate-iv 12)))
  (declare (type (simple-array (unsigned-byte 8) (*)) key iv))
  (ironclad:make-authenticated-encryption-mode
   :gcm
   :key key
   :cipher-name :aes
   :initialization-vector iv))

(defun encrypt (key message)
  (declare (type string key message))
  (let* ((iv-len 12)
         (iv (generate-iv iv-len))
         (cipher (make-aes-cipher (string-to-octets key) iv))
         (encrypted (ironclad:encrypt-message cipher (string-to-octets message)))
         (result (make-array (+ iv-len (length encrypted))
                             :element-type '(unsigned-byte 8))))
    (replace result iv)
    (replace result encrypted :start1 iv-len)
    (ironclad:byte-array-to-hex-string result)))

(defvar *challenge-key* "really-topsecret")

(encrypt *challenge-key* "try this, hackers!")
 ; => "81638a350d38ef23f269408a8a2bcb7a8ae4eea40560c3d132902d2c64dd"

(defun decrypt (key encrypted)
  (declare (type string key encrypted))
  (let* ((encrypted-bytes (ironclad:hex-string-to-byte-array encrypted))
         (iv-len 12)
         (iv (subseq encrypted-bytes 0 iv-len))
         (cipher (make-aes-cipher (string-to-octets key) iv)))
    ;; Should I use in-place decryption?
    (octets-to-string
     (ironclad:decrypt-message cipher (subseq encrypted-bytes iv-len)))))

(decrypt *challenge-key*
         (encrypt *challenge-key* "try this, hackers!"))
 ; => "try this, hackers!"

;; Scheme:
;; - generate salt:
;;   - unixtime + delta + ip (fixed-length)
;; - encrypt salt
;; - add nonce to encrypted salt
;; - generate challenge (salt + number)
;; - send enc-salt + hash to client
;;
;; (optimization)
;; ironclad can write to result buf directly if length constant
;;
;; on RX
;; - read nonce from salt
;; - decrypt salt
;; - check integrity. fail if not
;; - check hash. fail if not
;; - (propose new challenge if time delta to small)
;; - accept challenge, process letter.

(defun make-adjustable-string (s)
  (make-array (length s)
              :fill-pointer (length s)
              :adjustable t
              :initial-contents s
              :element-type (array-element-type s)))

(defun string-split (str &key (what-char #\  ))
  "Splits a string across WHAT-CHAR. Defaults to ascii space."
  (let ((tokens)
        (token (make-adjustable-string "")))
    (loop for c across str
          do (if (equal c what-char)
                 (progn
                   (push (copy-seq token) tokens)
                   (setf (fill-pointer token) 0))
                 (progn
                   (vector-push-extend c token))))
    (push token tokens)
    (nreverse tokens)))

(defun format-ip (ip)
  (format nil "~{~2,'0X~}"
          (mapcar #'parse-integer
                  (string-split ip :what-char #\.))))

(format-ip "192.168.0.12")
 ; => "C0A8000C"

(defun encode-salt (ip time delta r)
  (format nil "~A.~A.~A.~A" (format-ip ip) time delta r))

(encode-salt "192.168.0.2" (get-unix-time) 450 987)
 ; => "C0A80002.1741672965.450.987"

;; I don't really have to decode the IP, do I?
;; I can just compare the encoded strings I think.
;; TODO: profile and optimize this out when I do the other IP rework.
(defun decode-ip (ip-string)
  (format nil "~{~A~^.~}"
          (mapcar (lambda (byte) (parse-integer byte :radix 16))
                  (loop for i from 0 to 6 by 2
                        collect (subseq ip-string i (+ i 2))))))

(decode-ip "C0A80002")
 ; => "192.168.0.2"

(defun decode-salt (salt)
  (destructuring-bind (encoded-ip time delta r)
      (string-split salt :what-char #\.)
  (list
   (decode-ip encoded-ip)
   (parse-integer time)
   delta
   r)))

(decode-salt
 (encode-salt "192.168.0.2" (get-unix-time) 450 987))
 ; => ("192.168.0.2" "1741674000" "450" "987")

(defun generate-challenge (ip)
  (let* ((difficulty (calculate-challenge-difficulty ip))
         (time (get-unix-time))
         (delta "0000")
         (secret (generate-secret difficulty))
         (salt
           (encrypt *challenge-key*
                    (encode-salt ip time delta (random 1000)))))

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
 ; "0e0fc3f4c2b68d4a1b5dc1a0fdf239024f8baeff52a0f1da9f96e6e8980df5b5" :TIME
 ; 1741672748 :SALT
 ; "43c6b4e9ffeb76faccce8d55d43239db27dc9cef8249d92da9c77982dcf128a63aed704de29c2b8e"
 ; :SECRET 2114)

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

(defun long-ago (old now)
  (local-time:timestamp<
   old
   (local-time:timestamp- now 10 :minute)))

(defun generate-and-store-challenge (ip)
  ;; Hey! That's a lie, it doesn't store them!
  (generate-challenge ip))

(defun extract-ip (env)
  (let ((forwarded-ip
          (gethash "x-real-ip" (getf env :headers))))
    (if forwarded-ip
        forwarded-ip
        (getf env :remote-addr))))

(defun handle-challenge-request (env)
  (let ((ip (extract-ip env)))
    (destructuring-bind (&key hash salt &allow-other-keys)
        (generate-and-store-challenge ip)

      (log-inf (format nil "Serving challenge for ~A" ip))

      (list
       200
       '(:content-type "text/json")
       (list (challenge->json hash (format nil "~A" salt)))))))

(defun decrypt-salt (salt)
  (decode-salt
   (decrypt *challenge-key* salt)))

(decrypt-salt
 (encrypt *challenge-key*
          (encode-salt "192.168.0.2" (get-unix-time) 450 987)))
 ; => ("192.168.0.2" "1741674405" "450" "987")

(defun verify-challenge-response (client-ip hash salt answer)
  (handler-case
      (destructuring-bind (ip time delta r)
          (decrypt-salt salt)
        (declare (ignore delta r))
        (and
         (equalp ip client-ip)
         (not (long-ago (local-time:unix-to-timestamp time)
                        (local-time:now)))
         (equalp hash (compute-challenge salt answer))))

    (t (c)
      (declare (ignore c))
      (progn
        ;; TODO: profile this log message when under load
        (log-dbg (format nil "Failed challenge for ~A" client-ip))
        nil))))

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
  (destructuring-bind (&key ip hash salt answer)
      response
    (verify-challenge-response ip
                               hash
                               salt
                               (parse-integer answer))))

;; This table stores a mapping of hash <-> letter ID It could be stored in the
;; outbox database, but for flexibility we don't do that right now.
(defparameter *used-challenges* (make-hash-table :test #'equalp))

(defun find-past-challenge (challenge-rsp)
  (gethash (getf challenge-rsp :hash) *used-challenges*))

(defun store-used-challenge (postcard challenge-rsp)
  (let ((hash (getf challenge-rsp :hash))
        (lid (getf postcard :lid)))
    (setf (gethash hash *used-challenges*)
          lid)))

(defparameter *banned-ips-writelock* (bt:make-lock))
(defparameter *banned-ips* nil)

(defun clear-banned-ips ()
  (bt:with-lock-held (*banned-ips-writelock*)
    (let ((amount (length *banned-ips*)))
      (unless (zerop amount)
        (log-dbg (format nil "Forgiving ~A IPs" amount))))

    (setf *banned-ips*
          (make-array 100
                      :element-type '(unsigned-byte 32)
                      :adjustable t
                      :fill-pointer 0))))

(clear-banned-ips)

(defvar *forgive-interval-s* 30)

(defun spawn-forgive-thread ()
  (bt:make-thread
   (lambda ()
     (loop while (sleep-one-eye-open *forgive-interval-s*) do
       (setf *used-challenges* (make-hash-table :test #'equalp))
       (clear-banned-ips)))
   :name "Forgive thread"))

(defun decode-be-uint (bytes)
  (loop for i from 0
        for byte in (reverse bytes)
        summing
        (ash byte (* i 8))))

(defun ip->u32 (ip-str)
  (decode-be-uint
   (mapcar #'parse-integer
           (string-split ip-str :what-char #\.))))

(defun ban-ip (ip-str)
  (log-dbg (format nil "Banning ~A" ip-str))
  (bt:with-lock-held (*banned-ips-writelock*)
    (vector-push-extend
     (ip->u32 ip-str) *banned-ips*)))

(defun ip-is-banned? (ip-str)
  (let ((ip (ip->u32 ip-str)))
    (find-if (lambda (banned-ip) (= banned-ip ip))
              *banned-ips*)))

(define-condition reused-challenge-error
                  (error)
                  ((lid :initarg :lid)))

(defun error-if-not-valid (postcard challenge-rsp)
  (destructuring-bind (&key
                         src-country
                         dst-country
                         src-email
                         dst-email
                         image
                       &allow-other-keys)
      postcard

    (when (find-past-challenge challenge-rsp)
      (error 'reused-challenge-error
             :lid (find-past-challenge challenge-rsp)))

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
         (challenge-rsp (list :ip (extract-ip env)
                              :hash (read-param parsed :text "h")
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
            (store-used-challenge postcard challenge-rsp)
            (send-postcard (add-dates postcard))
            (postcard-sent)))

      (reused-challenge-error (err)
        (log-wrn (format nil "~A attempted to re-use challenge" (extract-ip env)))
        (destroy-letter (slot-value err 'lid))
        (ban-ip (extract-ip env))
        (postcard-not-sent "Heeey, no cheatin'"))

      ;; TODO: add invalid country as a custom error
      (t (c)
        (progn
          (log-err (format nil "Got exception when processing ~a: ~a" params c))
          (postcard-not-sent (format nil "~a" c)))))))

(defun handle-homepage (&optional context)
  (list 200 '(:content-type "text/plain; charset=utf-8")
        (if context
            (list context)
            '("Nothing here yet.."))))

(defun response (env)
  ;; (log-dbg (format nil  "query-string: ~A" (getf env :query-string)))
  ;; (break)

  ;; yougetnothing.jpg
  (when (ip-is-banned? (extract-ip env))
    (return-from response (list 400 nil)))

  (cond
    ((eql :post (getf env :request-method))
     (post-post env))

    ((eql :get (getf env :request-method))
     (alexandria:switch ((getf env :path-info) :test #'equalp)

       ("/request-challenge" (handle-challenge-request env))

       (*send-page-path* (handle-send env))

       ;; TODO: change this to redirect to about page when we have it
       ("/index.html" (handle-error))

       ("/" (handle-homepage))

       (t (handle-static env))
       ))

    (t (handle-error))))

(ql:quickload :clack)

(defvar *port* 8000)

(defvar *webapp* (clack:clackup 'response
                                :address "0.0.0.0" :port *port*
                                :debug nil))

(defvar *send-thread* (spawn-send-thread (lambda () nil)))
(defvar *forgive-thread* (spawn-forgive-thread))

(ql:quickload :trivial-signal)

(defun handle-termination (signo)
  (log-inf (format nil "Graceful shutdown (SIG~A)"
                   (trivial-signal:signal-name signo)))

  (log-inf "Stopping webapp")
  (clack:stop *webapp*)

  (log-inf "Stopping email thread")
  (setf *stop-app-threads* t)
  (bt:join-thread *send-thread*)

  (log-inf "Stopping forgive thread")
  (bt:join-thread *forgive-thread*)

  (log-inf "Done")
  (finish-output *error-output*)

  (uiop:quit 0)
  )

;; Simulate SIGTERM
;; (handle-termination 15)

;; Install the handler
(log-inf (format nil "Application started (port ~A)" *port*))

(defun install-handlers-and-run ()
  (trivial-signal:signal-handler-bind ((:term #'handle-termination)
                                       (:int #'handle-termination))
    (loop while t do (sleep 1))))
