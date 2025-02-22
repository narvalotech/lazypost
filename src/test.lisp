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

(add-dates (generate-letter))
;  => (:DELIVERY-DATE "2025-02-05" :SENT-DATE "2025-01-04" :SRC-COUNTRY "Mali"
;  :DST-COUNTRY "British Indian Ocean Territory" :SRC-EMAIL "Tasha@lazypost.net"
;  :DST-EMAIL "Susan@lazypost.net" :TEXT "Greetings from Mali!

; Love, Susan")

;; (loop for i from 0 to 10 collect
;;       (send-postcard (add-dates (generate-letter))))
