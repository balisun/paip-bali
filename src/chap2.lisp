(in-package :cl-user)
(defpackage paip-bali-chap2
  (:use :cl))
(in-package :paip-bali-chap2)


(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

;;; Exercise 2.1

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (let ((written-phrases))
    (cond ((listp phrase)
           (mappend #'generate phrase))
          ((setf written-phrases (rewrites phrase))
           (generate (random-elt written-phrases)))
          (t (list phrase)))))

;;; Exercise 2.2

(defun generate (phrase)
  "Generate a random sentence or phrase."
  (let ((written-phrases))
    (cond ((listp phrase)
           (mappend #'generate phrase))
          ((null (setf written-phrases (rewrites phrase)))
           (list phrase))
          (written-phrases
           (generate (random-elt written-phrases))))))


;;; exercise 2.4

;; by my notion of x-product
(defun x-product (fn v1 v2)
  "my notion of x-product"
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) (funcall fn x y))
                                v2))
          v1))

(defun combine-all (xlist ylist)
  (apply #'append (x-product #'list xlist ylist)))


;;by PAIP's notion of x-product

(defun x-product (fn v1 v2)
  "PAIP's notion of x-product"
  (mappend #'(lambda (x) (mapcar #'(lambda (y) (funcall fn x y))
                                v2))
          v1))


(defun combine-all (xlist ylist)
  (x-product #'append xlist ylist))
