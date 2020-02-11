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

(deftest compare-by-generated-position-inflated
  (ok (= -1 (mapping-list::compare-by-generated-position-inflated
             (mapping :generated-line 1)
             (mapping :generated-line 2))))
  (ok (= 0 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2)
            (mapping :generated-line 2))))
  (ok (= -1 (mapping-list::compare-by-generated-position-inflated
             (mapping :generated-line 2 :generated-column 2)
             (mapping :generated-line 2 :generated-column 3))))
  (ok (= 0 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2 :generated-column 2)
            (mapping :generated-line 2 :generated-column 2))))
  (ok (= 1 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2 :generated-column 2)
            (mapping :generated-line 2 :generated-column 1))))
  (ok (= 1 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 3)
            (mapping :generated-line 2))))
  (ok (= -1 (mapping-list::compare-by-generated-position-inflated
             (mapping :generated-line 2 :source "aaa")
             (mapping :generated-line 2 :source "abb"))))
  (ok (= 1 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2 :source "bbb")
            (mapping :generated-line 2 :source "abb"))))
  (ok (= 0 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2 :source "abb")
            (mapping :generated-line 2 :source "abb"))))
  (ok (= -1 (mapping-list::compare-by-generated-position-inflated
             (mapping :generated-line 2 :source "aaa" :original-line 1)
             (mapping :generated-line 2 :source "aaa" :original-line 2))))
  (ok (= 1 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2 :source "aaa" :original-line 2)
            (mapping :generated-line 2 :source "aaa" :original-line 1))))
  (ok (= 0 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2 :source "aaa" :original-line 1)
            (mapping :generated-line 2 :source "aaa" :original-line 1))))
  (ok (= -1 (mapping-list::compare-by-generated-position-inflated
             (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 1)
             (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 2))))
  (ok (= 1 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 2)
            (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 1))))
  (ok (= 0 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 1)
            (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 1))))
  (ok (= -1 (mapping-list::compare-by-generated-position-inflated
             (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 1 :name "abc")
             (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 1 :name "abd"))))
  (ok (= 1 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 1 :name "abd")
            (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 1 :name "abc"))))
  (ok (= 0 (mapping-list::compare-by-generated-position-inflated
            (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 1 :name "abc")
            (mapping :generated-line 2 :source "aaa" :original-line 1 :original-column 1 :name "abc")))))

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

(deftest mapping-list.add-mapping
  (let ((mappings (make-instance 'mapping-list:mapping-list))
        (mapping0 (mapping :generated-line 1 :generated-column 0))
        (mapping1 (mapping :generated-line 1 :generated-column 3))
        (mapping2 (mapping :generated-line 3 :generated-column 0)))

    (mapping-list:add-mapping mappings mapping1)
    (ok (equal mapping1 (mapping-list::.last mappings)))
    (ok (mapping-list::.sorted mappings))

    (mapping-list:add-mapping mappings mapping2)
    (ok (equal mapping2 (mapping-list::.last mappings)))
    (ok (mapping-list::.sorted mappings))

    (mapping-list:add-mapping mappings mapping0)
    (ok (equal mapping2 (mapping-list::.last mappings)))
    (ng (mapping-list::.sorted mappings))

    (ok (equal (mapping-list:to-list mappings)
               (list mapping0 mapping1 mapping2)))

    (ok (mapping-list::.sorted mappings))

    (ok (equal (mapping-list:to-list mappings)
               (list mapping0 mapping1 mapping2)))))

(deftest source-map-generator.add-mapping
  (let ((gen (make-instance 'source-map-generator))
        (mapping1 (mapping :generated-line 1)))
    (add-mapping gen mapping1)
    (ok (null (generator::.names gen)))
    (ok (null (generator::.sources gen)))
    (ok (equal (mapping-list:to-list (generator::.mappings gen))
               (list mapping1)))))
