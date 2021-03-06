;;;wiimote - Communicate with Nitendo Wiimote Controller from CL
;;;Written in 2013 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defsystem #:wiimote
  :name "wiimote"
  :description "Common Lisp interaction library for Nintendo's Wiimote Controller."
  :version "0.0.0"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "util")
   (:file "package")
   (:file "async-generator")
   (:file "auto-reset-event")
   (:file "wiimote"))
  :depends-on
  (#:anaphora
   #:bordeaux-threads
   #:hid
   #:trivial-garbage))
