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
