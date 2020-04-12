(defpackage :cl-source-map/mapping-list
  (:use :cl
        :cl-source-map/mapping
        :cl-source-map/util)
  (:export :compare-by-generated-position-inflated
           :mapping-list
           :add-mapping
           :to-list))
(in-package :cl-source-map/mapping-list)

(defun compare-by-generated-position-inflated (mapping-a mapping-b)
  (labels ((cmp (cmp)
             (unless (zerop cmp)
               (return-from compare-by-generated-position-inflated cmp)))
           (cmp-aux (cmp x y)
             (cond ((equal x y)       0)
                   ((null x)          1)
                   ((null y)         -1)
                   ((funcall cmp x y) 1)
                   (t                -1)))
           (strcmp (str1 str2)
             (declare (type (or string null) str1 str2))
             (cmp-aux #'string> str1 str2))
           (intcmp (int1 int2)
             (declare (type (or integer null) int1 int2))
             (cmp-aux #'> int1 int2)))
    (cmp (- (mapping-generated-line mapping-a)
            (mapping-generated-line mapping-b)))
    (cmp (- (mapping-generated-column mapping-a)
            (mapping-generated-column mapping-b)))
    (cmp (strcmp (mapping-source mapping-a)
                 (mapping-source mapping-b)))
    (cmp (intcmp (mapping-original-line mapping-a)
                 (mapping-original-line mapping-b)))
    (cmp (intcmp (mapping-original-column mapping-a)
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
        #+(or)
        (<= (compare-by-generated-position-inflated mapping-a mapping-b)
            0))))

(defgeneric add-mapping (mapping-list mapping))
(defgeneric to-list (mapping-list))

(defclass mapping-list ()
  ((list
    :initform (make-tlist)
    :accessor .list)
   (sorted
    :initform t
    :accessor .sorted)
   (last
    :initform nil
    :accessor .last)))

(defmethod add-mapping ((this mapping-list) mapping)
  (tlist-add-right (.list this) mapping)
  (if (or (null (.last this))
          (generated-position-after-p (.last this) mapping))
      (setf (.last this) mapping)
      (setf (.sorted this) nil))
  mapping)

(defmethod to-list ((this mapping-list))
  (if (.sorted this)
      (tlist-to-list (.list this))
      (let ((sorted-list
              (sort (copy-list (tlist-to-list (.list this)))
                    (lambda (x y)
                      (>= 0 (compare-by-generated-position-inflated x y))))))
        (setf (.list this) (list-to-tlist sorted-list)
              (.sorted this) t)
        sorted-list)))
