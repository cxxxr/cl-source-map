(defpackage :cl-source-map/source-map-generator
  (:use :cl
        :cl-source-map/util)
  (:local-nicknames (:mapping-list
                     :cl-source-map/mapping-list)
                    (:mapping
                     :cl-source-map/mapping)
                    (:base64-blq
                     :cl-source-map/base64-vlq))
  (:export :source-map-generator
           :set-source-content
           :add-mapping
           :serialize-mappings))
(in-package :cl-source-map/source-map-generator)

(defgeneric set-source-content (source-map-generator source-file source-content))
(defgeneric add-mapping (source-map-generator mapping))
(defgeneric serialize-mappings (source-map-generator))

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
   ;; TODO: sourcesとnamesをもっと効率の良いデータ構造にする
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
               `(unless (member ,object (,accessor ,this) :test #'equal)
                  (setf (,accessor ,this)
                        (nconc (,accessor ,this) (list ,object))))))
    (when-let (source (mapping:mapping-source mapping))
      (push! this .sources source))
    (when-let (name (mapping:mapping-name mapping))
      (push! this .names name)))
  (mapping-list:add-mapping (.mappings this) mapping))

(defmethod serialize-mappings ((this source-map-generator))
  (with-output-to-string (out)
    (loop :with previous-generated-column := 0
          :and previous-generated-line := 1
          :and previous-original-column := 0
          :and previous-original-line := 0
          :and previous-name-index := 0
          :and previous-source-index := 0
          :for i :from 0
          :for mapping :in (mapping-list:to-list (.mappings this))
          :for previous-mapping := nil :then mapping
          :do (block continue
                (cond ((/= (mapping:mapping-generated-line mapping)
                           previous-generated-line)
                       (loop :until (= (mapping:mapping-generated-line mapping)
                                       previous-generated-line)
                             :do (write-char #\; out)
                                 (incf previous-generated-line)))
                      ((> i 0)
                       (unless (mapping-list:compare-by-generated-position-inflated
                                mapping
                                previous-mapping)
                         (return-from continue))
                       (write-char #\, out))))
              (write-string (base64-blq:encode (- (mapping:mapping-generated-column mapping)
                                                  previous-generated-column))
                            out)
              (setf previous-generated-column (mapping:mapping-generated-column mapping))
              (when (mapping:mapping-source mapping)
                (let ((source-index (position (mapping:mapping-source mapping) (.sources this)
                                              :test #'equal)))
                  (write-string (base64-blq:encode (- source-index previous-source-index))
                                out)
                  (setf previous-source-index source-index))
                (write-string (base64-blq:encode (- (mapping:mapping-original-line mapping)
                                                    1
                                                    previous-original-line))
                              out)
                (setf previous-original-line (1- (mapping:mapping-original-line mapping)))
                (write-string (base64-blq:encode (- (mapping:mapping-original-column mapping)
                                                    previous-original-column))
                              out)
                (setf previous-original-column (mapping:mapping-original-column mapping))
                (when (mapping:mapping-name mapping)
                  (let ((name-index (position (mapping:mapping-name mapping) (.names this)
                                              :test #'equal)))
                    (write-string (base64-blq:encode (- name-index previous-name-index))
                                  out)
                    (setf previous-name-index name-index)))))))
