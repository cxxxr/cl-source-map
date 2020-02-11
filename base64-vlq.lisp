(defpackage :cl-source-map/base64-vlq
  (:use :cl)
  (:export :encode))
(in-package :cl-source-map/base64-vlq)

(defparameter +vlq-base-shift+ 5)
(defparameter +vlq-base+ (ash 1 +vlq-base-shift+))
(defparameter +vlq-base-mask+ (1- +vlq-base+))
(defparameter +vlq-continuation-bit+ +vlq-base+)

(defun encode-base64 (number)
  (aref "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" number))

(defun to-vlq-signed (value)
  (if (minusp value)
      (1+ (ash (- value) 1))
      (ash value 1)))

(defun encode (value)
  (with-output-to-string (out)
    (let ((vlq (to-vlq-signed value)))
      (loop :for digit := (logand vlq +vlq-base-mask+)
            :do (setf vlq (ash vlq (- +vlq-base-shift+)))
                (write-char (encode-base64
                             (if (plusp vlq)
                                 (logior digit +vlq-continuation-bit+)
                                 digit))
                            out)
            :while (plusp vlq)))))
