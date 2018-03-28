(in-package :cl-user)
(defpackage paip-bali
  (:use :cl))
(in-package :paip-bali)

;;; Exercise 1.1

(defun last-name (name)
  (if (member (first (last name)) *name-notes*)
      (last-name (butlast name))
      (first (last name))))

(defparameter *name-notes* '(MD Jr))

;;; Exercise 1.2

(defun power (base n)
  "calculate base^n, n is any interger."
  (cond ((= n 0) 1)
        ((= n 1) base)
        ((< n 0)
         (if (= base 0) "0 / 0, error!"
             (/ (power base (+ n 1)) base)))
        (t (* base (power base (- n 1))))))

;;; Exercise 1.3

;; recursive version
(defun count-atom-r (target)
  "count the number of atoms in a list. nil is either an atom & a lisp so it's included. the tick ' will be evaluated to QUOTE, so total count +1."
  (cond ((equal nil target) 1)
        ((listp target) (cond ((= 1 (length target)) (count-atom (first target)))
                              (t (+ (count-atom (first target)) (count-atom (rest target))))))
        (t 1)))


;; mapcar version
(defun count-atom-m (target)
  "count the number of atoms in a list. nil is either an atom & a lisp so it's included. the tick ' will be evaluated to QUOTE, so total count +1."
  (cond ((equal nil target) 1)
        ((listp target) (apply #'+ (mapcar #'count-atom target)))
        (t 1)))

;;;Exercise 1.4

(defun count-expn (expn target)
  (cond ((equal expn target) 1) ; expn = target, expn can be either a list or an atom
        ((listp target) (apply #'+ (mapcar #'(lambda (t1) (count-expn expn t1)) target)))
        (t 0)))

CL-USER> (count-expn 'a '(a a))
2
CL-USER> (count-expn '(a a) '(a a))
1
CL-USER> (count-expn '(a a) '(b a a))
0
CL-USER> (count-expn nil '(b a a))
0
CL-USER> (count-expn nil '(b a a nil))
1
CL-USER> (count-expn nil '(b a a nil '(nil nil)))
3
CL-USER> (count-expn '(a b) '(b '(a b) a a nil '(nil nil)))
1
CL-USER> (count-expn '(a b) '(b '(a b '(a b)) a a nil '(nil nil)))
1



;;; Exercise 1.5

(defun dot-product (v1 v2)
  (apply #'+ (mapcar #'* v1 v2)))
