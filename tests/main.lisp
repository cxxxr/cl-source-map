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
  (testing "set-source-content"
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
      (ok (= 1 (hash-table-count (generator::.source-contents gen)))))))
