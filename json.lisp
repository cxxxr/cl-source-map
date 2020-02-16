(defpackage :cl-source-map/json
  (:use :cl)
  (:export :with-json
           :with-json-object
           :emit-key-value
           :emit-array
           :emit-string
           :emit-value))
(in-package :cl-source-map/json)

(defvar *stream*)
(defvar *starting-emit-object*)

(defmacro with-json ((&key (stream *standard-output*)) &body body)
  `(let ((*stream* ,stream))
     ,@body))

(defmacro with-json-object (() &body body)
  `(let ((*starting-emit-object* t))
     (write-char #\{ *stream*)
     ,@body
     (write-char #\} *stream*)))

(defmacro emit-key-value (key value)
  `(progn
     (if *starting-emit-object*
         (setq *starting-emit-object* nil)
         (write-char #\, *stream*))
     (emit-string ,key)
     (write-char #\: *stream*)
     ,value))

(defun emit-array (sequence)
  (write-char #\[ *stream*)
  (let ((first t))
    (map nil
         (lambda (elt)
           (if first
               (setq first nil)
               (write-char #\, *stream*))
           (emit-value elt))
         sequence))
  (write-char #\] *stream*))

(defparameter *char-replacements*
  '((#\\ . "\\\\")
    (#\" . "\\\"")
    (#\Backspace . "\\b")
    (#\Page . "\\f")
    (#\Newline . "\\n")
    (#\Return . "\\r")
    (#\Tab . "\\t")))

(defun emit-string (string)
  (write-char #\" *stream*)
  (dotimes (i (length string))
    (let* ((char (aref string i))
           (replacement (assoc char *char-replacements*)))
      (if replacement
          (write-string (cdr replacement) *stream*)
          (write-char char *stream*))))
  (write-char #\" *stream*))

(defun emit-value (value)
  (typecase value
    (string (emit-string value))
    (array (emit-array value))
    (list (emit-array value))
    (otherwise (princ value *stream*))))
