(defpackage :cl-source-map/util
  (:use :cl)
  (:export :missing
           :when-let))
(in-package :cl-source-map/util)

(defun missing (name)
  (error "Missing ~A" name))

(defmacro when-let ((var value) &body body)
  `(let ((,var ,value))
     (when ,var ,@body)))
