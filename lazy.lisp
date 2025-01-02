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
  (format t "~A~%" (pprint-postcard postcard)))

(send-postcard
 '(:text
   "Hello from Oslo!
Made a new friend, Ole nordmann.

xoxo Matt"

   :src-city "Oslo, Norway"
   :dst-city "Denver, USA"

   :src-email "matt.smith@gmail.com"
   :dst-email "john.smith@hostmail.com"
   ))
; Oslo, Norway -> Denver, USA
; matt.smith@gmail.com -> john.smith@hostmail.com
; Hello from Oslo!
; Made a new friend, Ole nordmann.

; xoxo Matt
;  => NIL

(defun send-scheduled-postcards (db time)
  (format t "Sending...~%"))

(ql:quickload :local-time)

(defun send-thread-cycle (db)
  (let ((current-time (local-time:now)))
    (send-scheduled-postcards db current-time)
    (sleep 5)))

(ql:quickload :bordeaux-threads)

(defparameter *send-thread*
  (bt:make-thread
   (lambda () (loop while t do
                    (send-thread-cycle *the-post*)))
   :name "Postman thread"))
(bt:join-thread *send-thread*)
