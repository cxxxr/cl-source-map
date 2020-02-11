(defpackage :cl-source-map/source-map-generator
  (:use :cl)
  (:export :source-map-generator
           :add-source-content
           :remove-source-content))
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

(defgeneric add-source-content (source-map-generator source-file source-content))
(defgeneric remove-source-content (source-map-generator source-file))

(defun ensure-source-file (source-map-generator source-file)
  (if (.source-root source-map-generator)
      (merge-pathnames source-file (.source-root source-map-generator))
      (pathname source-file)))

(defmethod add-source-content ((this source-map-generator) source-file source-content)
  (let ((source-file (ensure-source-file this source-file)))
    (setf (gethash source-file (.source-contents this))
          source-content)))

(defmethod remove-source-content ((this source-map-generator) source-file)
  (let ((source-file (ensure-source-file this source-file)))
    (remhash source-file (.source-contents this))))
