(defun get-form ()
  "this is a form")

(defparameter *the-post* nil)

(defun send-postcard (text
                      src-city
                      dst-city
                      src-email
                      dst-email)
  "Send a delayed postcard. Only accepts text."
  (format t "~A -> ~A~%" src-city dst-city)
  (format t "~A -> ~A~%" src-email dst-email)
  (format t "~A~%" text)
  )

(send-postcard
 "Hello from Oslo!
Made a new friend, Ole nordmann.

xoxo Matt"

 "Oslo, Norway"
 "Denver, USA"

 "matt.smith@gmail.com"
 "john.smith@hostmail.com"
 )
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
