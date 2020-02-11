(defpackage :cl-source-map/tests/main
  (:use :cl
        :rove
        :cl-source-map/source-map-generator)
  (:local-nicknames (:generator
                     :cl-source-map/source-map-generator)))
(in-package :cl-source-map/tests/main)

(deftest source-map-generator
  (testing "make-instance source-map-generator"
    (let ((gen (make-instance 'source-map-generator)))
      (ok (typep gen 'source-map-generator))))
  (testing "add-source-content"
    (let ((gen (make-instance 'source-map-generator)))
      (add-source-content gen "hoge.lisp" "aaa")
      (ok (= 1 (hash-table-count (generator::.source-contents gen))))
      (ok (equal "aaa" (gethash #p"hoge.lisp" (generator::.source-contents gen))))

      (add-source-content gen "hoge.lisp" "bbb")
      (ok (= 1 (hash-table-count (generator::.source-contents gen))))
      (ok (equal "bbb" (gethash #p"hoge.lisp" (generator::.source-contents gen))))

      (remove-source-content gen "hoge.lisp")
      (ok (= 0 (hash-table-count (generator::.source-contents gen))))

      (add-source-content gen "hoge.lisp" "ldfj")
      (ok (= 1 (hash-table-count (generator::.source-contents gen))))
      (ok (equal "ldfj" (gethash #p"hoge.lisp" (generator::.source-contents gen))))

      (add-source-content gen "piyo.lisp" "lasdj")
      (ok (= 2 (hash-table-count (generator::.source-contents gen))))
      (ok (equal "lasdj" (gethash #p"piyo.lisp" (generator::.source-contents gen)))))

    (let ((gen (make-instance 'source-map-generator :source-root "/root/")))
      (add-source-content gen "hoge.lisp" "a")
      (ok (equal "a" (gethash #p"/root/hoge.lisp" (generator::.source-contents gen))))
      (ok (= 1 (hash-table-count (generator::.source-contents gen)))))))
