(defpackage :cl-source-map/mapping
  (:use :cl)
  (:import-from :cl-source-map/util
                :missing)
  (:export :mapping
           :mapping-generated-line
           :mapping-generated-column
           :mapping-original-line
           :mapping-original-column
           :mapping-source
           :mapping-name))
(in-package :cl-source-map/mapping)

(deftype line-number ()
  '(integer 1 *))

(deftype column ()
  '(integer 0 *))

(defstruct (mapping (:constructor mapping))
  (generated-line (missing :generated-line) :read-only t :type line-number)
  (generated-column 0 :read-only t :type column)
  (original-line nil :read-only t :type (or null line-number))
  (original-column nil :read-only t :type (or null column))
  (source nil :read-only t :type (or null string))
  (name nil :read-only t :type (or null string)))
