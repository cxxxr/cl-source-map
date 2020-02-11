(defpackage :cl-source-map/source-map-generator
  (:use :cl)
  (:export :source-map-generator
           :set-source-content))
(in-package :cl-source-map/source-map-generator)

(defclass source-map-generator ()
  ((file
    :initform nil
    :initarg :file
    :reader .file)
   (source-root
    :initform nil
    :initarg :source-root
    :reader .source-root)
   (skip-validation
    :initform nil
    :initarg :skip-validation
    :reader .skip-validation)
   (sources
    :initform '()
    :accessor .sources)
   (names
    :initform '()
    :accessor .names)
   (mappings
    :initform '()
    :accessor .mappings)
   (source-contents
    :initform (make-hash-table :test 'equal)
    :accessor .source-contents)))

(defgeneric set-source-content (source-map-generator source-file source-content))

(defun ensure-source-file (source-map-generator source-file)
  (namestring
   (if (.source-root source-map-generator)
       (merge-pathnames source-file (.source-root source-map-generator))
       source-file)))

(defmethod set-source-content ((this source-map-generator) source-file source-content)
  (let ((source-file (ensure-source-file this source-file)))
    (if source-content
        (setf (gethash source-file (.source-contents this))
              source-content)
        (remhash source-file (.source-contents this)))))
