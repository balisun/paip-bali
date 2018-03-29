;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

;;; ==============================

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

;;; ==============================

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

;;; my trial: failed, due to endless evaluation of the Adj*
(defmacro Adj* ()
  (one-of `(nil ,(append (Adj) (Adj*)))))


(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

;;; my trial
(defmacro PP* ()
  (one-of `(,(append (PP) (PP*)) nil)))


(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))

;;; ==============================

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammers.")

(setf *grammar* *simple-grammar*)

;;; ==============================

(defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

;;; ==============================

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))


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
  "Generate a random sentence or phrase"
  (let ((written-phrases))
    (cond ((listp phrase)
           (mappend #'generate phrase))
          ((null (setf written-phrases (rewrites phrase)))
           (list phrase))
          (written-phrases
           (generate (random-elt written-phrases))))))



;;; ==============================

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

;;; ==============================

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))


;;; ==============================

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))



(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

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
