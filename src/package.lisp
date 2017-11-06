;;;wiimote - Communicate with Nitendo Wiimote Controller from CL
;;;Written in 2013 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defpackage #:wiimote
  (:use
   #:anaphora
   #:cl
   #:hid
   #:trivial-garbage)
  (:export
   #:make-wiimote
   #:get-status
   #:get-calibration
   #:register-state-listener
   #:unregister-state-listener
   #:connect-wiimote
   #:disconnect-wiimote
   #:read-data
   #:write-data
   #:set-report-mode

   #:wiimote-state

   #:x
   #:y
   #:z

   #:accel-state
   #:accel-values

   #:battery-state

   #:button-state
   #:a
   #:b
   #:one
   #:two
   #:plus
   #:minus
   #:home
   #:up
   #:down
   #:left
   #:right

   #:led-state
   #:led1
   #:led2
   #:led3
   #:led4

   #:rumbling
   ))
