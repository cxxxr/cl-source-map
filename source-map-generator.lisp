(defpackage :cl-source-map/source-map-generator
  (:use :cl
        :cl-source-map/util)
  (:local-nicknames (:mapping-list
                     :cl-source-map/mapping-list))
  (:local-nicknames (:mapping
                     :cl-source-map/mapping))
  (:export :source-map-generator
           :set-source-content))
(in-package :cl-source-map/source-map-generator)

(defgeneric set-source-content (source-map-generator source-file source-content))
(defgeneric add-mapping (source-map-generator mapping))

(defclass source-map-generator ()
  ((file
    :initform nil
    :initarg :file
    :reader .file)
   (source-root
    :initform nil
    :initarg :source-root
    :reader .source-root)
   #+(or)
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
    :initform (make-instance 'mapping-list:mapping-list)
    :accessor .mappings)
   (source-contents
    :initform (make-hash-table :test 'equal)
    :accessor .source-contents)))

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

(defmethod add-mapping ((this source-map-generator) mapping)
  ;; TODO: validate arguments
  (macrolet ((push! (this accessor object)
               `(pushnew ,object (,accessor ,this) :test #'equal)))
    (when-let (source (mapping:mapping-source mapping))
      (push! this .sources source))
    (when-let (name (mapping:mapping-name mapping))
      (push! this .names name)))
  (mapping-list:add-mapping (.mappings this) mapping))
