(defpackage :cl-source-map/util
  (:use :cl)
  (:export :missing
           :when-let
           :make-tlist
           :tlist-left
           :tlist-right
           :tlist-empty-p
           :tlist-add-left
           :tlist-add-right
           :tlist-rem-left
           :tlist-update
           :tlist-to-list
           :list-to-tlist))
(in-package :cl-source-map/util)

(defun missing (name)
  (error "Missing ~A" name))

(defmacro when-let ((var value) &body body)
  `(let ((,var ,value))
     (when ,var ,@body)))

(declaim (inline make-tlist
                 tlist-left
                 tlist-right
                 tlist-empty-p
                 tlist-update
                 tlist-to-list))
(defun make-tlist () (cons nil nil))
(defun tlist-left (tl) (caar tl))
(defun tlist-right (tl) (cadr tl))
(defun tlist-empty-p (tl) (null (car tl)))
(defun tlist-update (tl) (setf (cdr tl) (last (car tl))))
(defun tlist-to-list (tl) (car tl))

(defun list-to-tlist (list)
  (let ((tl (cons list nil)))
    (tlist-update tl)
    tl))

(defun tlist-add-left (tl it)
  (let ((x (cons it (car tl))))
    (if (tlist-empty-p tl)
        (setf (cdr tl) x))
    (setf (car tl) x)))

(defun tlist-add-right (tl it)
  (let ((x (cons it nil)))
    (if (tlist-empty-p tl)
        (setf (car tl) x)
        (setf (cddr tl) x))
    (setf (cdr tl) x)))

(defun tlist-rem-left (tl)
  (if (tlist-empty-p tl)
      (error "Remove from empty tlist")
      (let ((x (car tl)))
        (setf (car tl) (cdar tl))
        (if (tlist-empty-p tl)
            (setf (cdr tl) nil))
        (car x))))
