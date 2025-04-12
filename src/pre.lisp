(require 'asdf)

;; (require :slynk)
(ql:quickload :slynk)

(defun slynk-listener-thread-p (thread)
  "Check if the given thread is a Slynk listener thread."
  (let ((thread-name (sb-thread:thread-name thread)))
    (format t "thread: ~A~%" thread-name)
    (and thread-name
         (search "slynk" thread-name :test #'equalp))))

(defun slynk-server-running-p ()
  "Check if there is any active Slynk listener thread."
  (some #'slynk-listener-thread-p (sb-thread:list-all-threads)))

(unless (slynk-server-running-p)
  (format t "Starting SLYNK server~%")
  (slynk:create-server :port 42069 :dont-close t))
