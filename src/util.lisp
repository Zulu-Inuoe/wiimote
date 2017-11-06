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

(defmacro defcenum (name cl:&body pairs)
  (flet ((make-name (&rest args)
	   (intern (apply #'concatenate 'string (mapcar #'string args)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(mapcar
	  (lambda (p)
	    `(defparameter
		 ,(make-name name "." (first p))
	       ,(second p)))
	  pairs)
       ',name)))
