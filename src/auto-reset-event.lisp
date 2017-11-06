;;;wiimote - Communicate with Nitendo Wiimote Controller from CL
;;;Written in 2013 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:wiimote)

(defclass auto-reset-event ()
  ((lock
    :initform (bordeaux-threads:make-lock)
    :reader wait-lock)
   (condition
    :initform (bordeaux-threads:make-condition-variable)
    :reader wait-condition)
   (event-set
    :initform nil
    :accessor event-set)))

(defun wait-one (event &optional (timeout nil))
  (bordeaux-threads:with-lock-held ((wait-lock event))
    (if (event-set event)
	(progn
	  (setf (event-set event) nil)
	  t)
	(if (bordeaux-threads:condition-wait (wait-condition event) (wait-lock event) :timeout timeout)
	    (progn
	      (setf (event-set event) nil)
	      t)
	    nil))))

(defun set-event (event)
  (bordeaux-threads:with-lock-held ((wait-lock event))
    (setf (event-set event) t)
    (bordeaux-threads:condition-notify (wait-condition event))))
