(defpackage :cl-source-map/tests/main
  (:use :cl
        :rove
        :cl-source-map/mapping
        :cl-source-map/source-map-generator)
  (:local-nicknames (:generator
                     :cl-source-map/source-map-generator))
  (:local-nicknames (:mapping-list
                     :cl-source-map/mapping-list)))
(in-package :cl-source-map/tests/main)

(deftest source-map-generator.make-instance
  (let ((gen (make-instance 'source-map-generator)))
    (ok (typep gen 'source-map-generator))))

(deftest source-map-generator.set-source-content
  (let ((gen (make-instance 'source-map-generator)))
    (set-source-content gen "hoge.lisp" "aaa")
    (ok (= 1 (hash-table-count (generator::.source-contents gen))))
    (ok (equal "aaa" (gethash "hoge.lisp" (generator::.source-contents gen))))

    (set-source-content gen "hoge.lisp" "bbb")
    (ok (= 1 (hash-table-count (generator::.source-contents gen))))
    (ok (equal "bbb" (gethash "hoge.lisp" (generator::.source-contents gen))))

    (set-source-content gen "hoge.lisp" nil)
    (ok (= 0 (hash-table-count (generator::.source-contents gen))))

    (set-source-content gen "hoge.lisp" "ldfj")
    (ok (= 1 (hash-table-count (generator::.source-contents gen))))
    (ok (equal "ldfj" (gethash "hoge.lisp" (generator::.source-contents gen))))

    (set-source-content gen "piyo.lisp" "lasdj")
    (ok (= 2 (hash-table-count (generator::.source-contents gen))))
    (ok (equal "lasdj" (gethash "piyo.lisp" (generator::.source-contents gen)))))

  (let ((gen (make-instance 'source-map-generator :source-root "/root/")))
    (set-source-content gen "hoge.lisp" "a")
    (ok (equal "a" (gethash "/root/hoge.lisp" (generator::.source-contents gen))))
    (ok (= 1 (hash-table-count (generator::.source-contents gen))))))

(deftest mapping-list.generated-position-after-p
  (ok (mapping-list::generated-position-after-p
       (mapping :generated-line 1)
       (mapping :generated-line 2)))
  (ok (mapping-list::generated-position-after-p
       (mapping :generated-line 2)
       (mapping :generated-line 2)))
  (ok (mapping-list::generated-position-after-p
       (mapping :generated-line 2 :generated-column 2)
       (mapping :generated-line 2 :generated-column 3)))
  (ok (mapping-list::generated-position-after-p
       (mapping :generated-line 2 :generated-column 2)
       (mapping :generated-line 2 :generated-column 2)))
  (ng (mapping-list::generated-position-after-p
       (mapping :generated-line 2 :generated-column 2)
       (mapping :generated-line 2 :generated-column 1)))
  (ng (mapping-list::generated-position-after-p
       (mapping :generated-line 3)
       (mapping :generated-line 2))))

(deftest source-map-generator.add-mapping
  (let ((gen (make-instance 'source-map-generator)))
    (add-mapping gen (mapping :generated-line 1))
    ))
