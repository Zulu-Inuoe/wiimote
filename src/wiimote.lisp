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

(defparameter *dev-handle* nil)
(defparameter *dev-stream* nil)
(defparameter *wiimote-vid* #x057e)
(defparameter *wiimote-pid* #x0306)
(defparameter *report-length* 22)

(defcenum input-report
  (status #x20)
  (read-data #x21)
  (write-data #x22)
  (buttons #x30)
  (buttons-accel #x31)
  (ir-accel #x33)
  (buttons-extension #x34)
  (extension-accel #x35)
  (ir-extension-accel #x37))

(defcenum output-report
  (leds #x11)
  (type #x12)
  (ir #x13)
  (status #x15)
  (write-data #x16)
  (read-data #x17)
  (ir2 #x1a))

(defclass wiimote ()
  ((serial-number
    :initform nil
    :accessor serial-number
    :initarg :serial-number
    :documentation "Serial number for the wiimote. If nil, then a connection has yet to be established.")
   (handle
    :initform nil
    :accessor handle
    :documentation "HID handle for the wiimote. Used for I/O.")
   (output-buffer
    :initform (make-array *report-length*)
    :reader output-buffer
    :documentation "Buffer used to store output reports for the wiimote.")
   (read-buffer
    :accessor read-buffer
    :documentation "Buffer used to store read data from the wiimote.")
   (read-stream
    :initarg read-stream
    :accessor read-stream
    :documentation "The asynchronous generator that polls the wiimote.")
   (state-listeners
    :initform nil
    :accessor state-listeners
    :documentation "A list functions called when the wiimote state is updated.")
   (read-event
    :initform (make-instance 'auto-reset-event)
    :reader read-event
    :documentation "An event used to signal that requested data is available.")
   (read-address
    :accessor read-address
    :documentation "The numerical address of the last read request.")
   (read-size
    :accessor read-size
    :documentation "The number of bytes requested in the last read request.")
   (wiimote-state
    :initform (make-instance 'wiimote-state)
    :reader wiimote-state
    :documentation "The parsed state of the wiimote.")))

(defclass wiimote-state ()
  ((battery
    :accessor battery)
   (button-state
    :initform (make-instance 'button-state)
    :reader button-state)
   (accel-calibration-state
    :initform (make-instance 'accel-calibration-state)
    :reader accel-calibration-state)
   (accel-state
    :initform (make-instance 'accel-state)
    :reader accel-state)
   (led-state
    :initform (make-instance 'led-state)
    :reader led-state)
   (rumbling
    :initform nil
    :accessor rumbling)))

(defclass led-state ()
  ((led1
    :accessor led1)
   (led2
    :accessor led2)
   (led3
    :accessor led3)
   (led4
    :accessor led4)))

(defclass button-state ()
  ((a
    :accessor a)
   (b
    :accessor b)
   (plus
    :accessor plus)
   (home
    :accessor home)
   (minus
    :accessor minus)
   (one
    :accessor one)
   (two
    :accessor two)
   (up
    :accessor up)
   (down
    :accessor down)
   (left
    :accessor left)
   (right
    :accessor right)))

(defclass vec3 ()
  ((x
    :initarg :x
    :initform 0
    :accessor x)
   (y
    :initarg :y
    :initform 0
    :accessor y)
   (z
    :initarg :z
    :initform 0
    :accessor z)))

(defclass accel-calibration-state ()
  ((x0
    :initform 0
    :accessor x0)
   (y0
    :initform 0
    :accessor y0)
   (z0
    :initform 0
    :accessor z0)
   (xg
    :initform 0
    :accessor xg)
   (yg
    :initform 0
    :accessor yg)
   (zg
    :initform 0
    :accessor zg)))

(defclass accel-state ()
  ((raw-values
    :initform (make-instance 'vec3)
    :reader raw-values)
   (accel-values
    :initform (make-instance 'vec3)
    :reader accel-values)))

(defun rumble-bit (wiimote)
  (if (rumbling (wiimote-state wiimote))
      #x01
      #x00))

(defun make-wiimote-data-handler (wiimote)
  (lambda (data)
    (when (parse-input-report wiimote data)
      (dolist (l (state-listeners wiimote))
	(funcall l (wiimote-state wiimote))))))

(defun make-wiimote-generator (wiimote)
  (lambda ()
    (aif (hid-read (handle wiimote) *report-length*)
	(values it t)
	(values nil nil))))

(defun make-wiimote (&optional serial-number)
  (make-instance 'wiimote :serial-number serial-number))

(defun register-state-listener (wiimote fn)
  (push fn (state-listeners wiimote)))

(defun unregister-state-listener (wiimote fn)
  (setf (state-listeners wiimote) (delete fn (state-listeners wiimote))))

(defun connect-wiimote (wiimote)
  (awhen (hid-open *wiimote-vid* *wiimote-pid*)
    (setf (handle wiimote) it
	  (read-stream wiimote) (make-instance
				 'async-generator
				 :generator (make-wiimote-generator wiimote)
				 :callback (make-wiimote-data-handler wiimote)))
    (start-async-generator (read-stream wiimote))
    (read-calibration wiimote)
    (get-status wiimote)
    t))

(defun disconnect-wiimote (wiimote)
  (stop-async-generator (read-stream wiimote))
  (hid-close (handle wiimote)))

(defun bit-on-p (buf byte bit)
  (not (zerop (logand (aref buf byte) (ash 1 bit)))))

(defun parse-buttons (button-state buf)
  (with-slots (a b minus home plus one two up down left right) button-state
    (setf a (bit-on-p buf 2 3)
	  b (bit-on-p buf 2 2)
	  minus (bit-on-p buf 2 4)
	  home (bit-on-p buf 2 7)
	  plus (bit-on-p buf 1 4)
	  one (bit-on-p buf 2 1)
	  two (bit-on-p buf 2 0)
	  up (bit-on-p buf 1 3)
	  down (bit-on-p buf 1 2)
	  left (bit-on-p buf 1 0)
	  right (bit-on-p buf 1 1))))


(defun normalize-value (raw zero-point gravity)
  (float (/ (- raw zero-point) (- gravity zero-point))))

(defun parse-accel (state buf)
  (with-slots ((xr x) (yr y) (zr z)) (raw-values (accel-state state))
    (setf xr (aref buf 3)
	  yr (aref buf 4)
	  zr (aref buf 5))
    (with-slots ((xt x) (yt y) (zt z)) (accel-values (accel-state state))
      (with-slots (x0 y0 z0 xg yg zg) (accel-calibration-state state)
	(setf xt (normalize-value xr x0 xg)
	      yt (normalize-value yr y0 yg)
	      zt (normalize-value zr z0 zg))))))

(defun parse-read-data (wiimote data)
  (when (bit-on-p data 3 3)
    (error "Error reading data from Wiimote, something about a byte not existing"))
  (when (not (zerop (logand (aref data 3) #x07)))
    (error "Error reading data from Wiimote: Attempt to read from write-only registers."))

  ;;Copy over the data into the read-buffer
  (let ((size (1+ (ash (aref data 3) -4)))
	(offset (logior (ash (aref data 4) 8) (aref data 5))))
    (loop
      :repeat size
      :for src :from 6
      :for dst :from (- offset (read-address wiimote))
      :do (setf (aref (read-buffer wiimote) dst) (aref data src)))

    (when (= (+ (read-address wiimote) (read-size wiimote))
	     (+ offset size))
      (set-event (read-event wiimote)))))

(defun parse-input-report (wiimote data)
  (case (aref data 0)
    (#.input-report.buttons
     (parse-buttons (button-state (wiimote-state wiimote)) data)
     t)
    (#.input-report.buttons-accel
     (parse-buttons (button-state (wiimote-state wiimote)) data)
     (parse-accel (wiimote-state wiimote) data)
     t)
    (#.input-report.ir-accel
     (parse-buttons (button-state (wiimote-state wiimote)) data)
     (parse-accel (wiimote-state wiimote) data)
     t)
    (#.input-report.read-data
     (parse-buttons (button-state (wiimote-state wiimote)) data)
     (parse-read-data wiimote data)
     t)
    (#.input-report.status
     (parse-buttons (button-state (wiimote-state wiimote)) data)
     (setf (battery (wiimote-state wiimote)) (aref data 6))
     (with-slots (led1 led2 led3 led4) (led-state (wiimote-state wiimote))
       (setf led1 (bit-on-p data 3 4)
	     led2 (bit-on-p data 3 5)
	     led3 (bit-on-p data 3 6)
	     led4 (bit-on-p data 3 7)))
     t)))

(defun clear-report (wiimote)
  (dotimes (i *report-length*)
    (setf (aref (output-buffer wiimote) i) 0)))

(defparameter *use-alt-mode* t)

(defun write-report (wiimote)
  (prog1
      (cond
	(*use-alt-mode*
	 (hid-set-output-report (handle wiimote) (output-buffer wiimote)))
	(t
	 (hid-write (handle wiimote) (output-buffer wiimote))))
    (sleep 0.1)))

(defun get-status (wiimote)
  (clear-report wiimote)

  (setf (aref (output-buffer wiimote) 0) output-report.status
	(aref (output-buffer wiimote) 1) (rumble-bit wiimote))

  (write-report wiimote))

(defun read-data (wiimote address size)
  (clear-report wiimote)

  (setf (read-buffer wiimote) (make-array size)
	(read-address wiimote) address
	(read-size wiimote) size)

  (flet ((write-buffer (i val)
	   (setf (aref (output-buffer wiimote) i) val)))
    (write-buffer 0 output-report.read-data)
    (write-buffer 1 (logior
		     (ldb (byte 8 24) address)
		     (rumble-bit wiimote)))
    (write-buffer 2 (ldb (byte 8 16) address))
    (write-buffer 3 (ldb (byte 8 8) address))
    (write-buffer 4 (ldb (byte 8 0) address))
    (write-buffer 5 (ldb (byte 8 8) size))
    (write-buffer 6 (ldb (byte 8 0) size)))

  (assert (write-report wiimote))

  (unless (wait-one (read-event wiimote) 1)
    (error "Error reading data from Wiimote..."))

  (read-buffer wiimote))

(defun write-data (wiimote address buf &aux (len (length buf)))
  (clear-report wiimote)

  (flet ((write-buffer (i val)
	   (setf (aref (output-buffer wiimote) i) val)))
    (write-buffer 0 output-report.write-data)
    (write-buffer 1 (logior (ldb (byte 8 24) address) (rumble-bit wiimote)))
    (write-buffer 2 (ldb (byte 8 16) address))
    (write-buffer 3 (ldb (byte 8 8) address))
    (write-buffer 4 (ldb (byte 8 0) address))
    (write-buffer 5 len)

    (dotimes (i len)
      (write-buffer (+ i 6) (aref buf i))))

  (assert (write-report wiimote)))

(defun read-calibration (wiimote)
  (let ((buf (read-data wiimote #x0016 7)))
    (with-slots (x0 y0 z0 xg yg zg) (accel-calibration-state (wiimote-state wiimote))
      (setf x0 (aref buf 0)
	    y0 (aref buf 1)
	    z0 (aref buf 2)
	    xg (aref buf 4)
	    yg (aref buf 5)
	    zg (aref buf 6)))))

(defun set-report-mode (wiimote mode continuous)
  (clear-report wiimote)

  (setf (aref (output-buffer wiimote) 0) output-report.type
	(aref (output-buffer wiimote) 1) (logior
					  (rumble-bit wiimote)
					  (if continuous #x04 #x00))
	(aref (output-buffer wiimote) 2) mode)
  (assert (write-report wiimote)))
