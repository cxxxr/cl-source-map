(defpackage :cl-source-map/mapping-list
  (:use :cl
        :cl-source-map/mapping)
  (:export :mapping-list
           :add-mapping))
(in-package :cl-source-map/mapping-list)

(defun compare-by-generated-position-inflated (mapping-a mapping-b)
  (flet ((cmp (cmp)
           (unless (zerop cmp)
             (return-from compare-by-generated-position-inflated cmp)))
         (strcmp (str1 str2)
           (declare (type (or string null) str1 str2))
           (cond ((equal str1 str2)    0)
                 ((null str1)          1)
                 ((null str2)         -1)
                 ((string> str1 str2)  1)
                 (t                   -1))))
    (cmp (- (mapping-generated-line mapping-a)
            (mapping-generated-line mapping-b)))
    (cmp (- (mapping-generated-column mapping-a)
            (mapping-generated-column mapping-b)))
    (cmp (strcmp (mapping-source mapping-a)
                 (mapping-source mapping-b)))
    (cmp (- (mapping-original-line mapping-a)
            (mapping-original-line mapping-b)))
    (cmp (- (mapping-original-column mapping-a)
            (mapping-original-column mapping-b)))
    (strcmp (mapping-name mapping-a)
            (mapping-name mapping-b))))

(defun generated-position-after-p (mapping-a mapping-b)
  (let ((line-a (mapping-generated-line mapping-a))
        (line-b (mapping-generated-line mapping-b))
        (column-a (mapping-generated-column mapping-a))
        (column-b (mapping-generated-column mapping-b)))
    (or (< line-a line-b)
        (and (= line-a line-b) (<= column-a column-b))
        ;; ???:
        ;; compare-by-generated-position-inflated関数の中でsource以降の値が比較される場合、
        ;; generated-lineとgenerated-columnが同じ値である必要があるが
        ;; この上の条件式でgenerated-lineとgenerated-columnが同じ値ならここに辿りつかない
        ;; なのでsource以降の条件式は評価されることがないデッドコードになるはず
        (<= (compare-by-generated-position-inflated mapping-a mapping-b)
            0))))

(defgeneric add-mapping (mapping-list mapping))

(defclass mapping-list ()
  ((list
    :initform '()
    :accessor .list)
   (sorted
    :initform t
    :accessor .sorted)
   (last
    :initform nil
    :accessor .last)))

(defmethod add-mapping ((this mapping-list) mapping)
  (push mapping (.list this))
  (if (or (null (.last this))
          (generated-position-after-p (.last this) mapping))
      (setf (.last this) mapping)
      (setf (.sorted this) nil)))
