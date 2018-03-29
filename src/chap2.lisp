(in-package :cl-user)
(defpackage paip-bali-chap2
  (:use :cl))
(in-package :paip-bali-chap2)

;;; Exercise 2.1

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (let ((written-phrases))
    (cond ((listp phrase)
           (mappend #'generate phrase))
          ((setf written-phrases (rewrites phrase))
           (generate (random-elt written-phrases)))
          (t (list phrase)))))
