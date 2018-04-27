(in-package :cl-user)
(defpackage chap3-20q
  (:use :cl)
  (:export
   #:init-20q
   #:show-memory
   #:write-memory))
(in-package :chap3-20q)

;;; Exercise 3.5

(defvar *words-memorized* '("a" "or" "yes" "six" "bali" "five" "orange" "camera" "avocado" "987654321"
                            "1234567890" "symbolic AI" "Wittgenstein" "human-like AI" "connectionism"
                            "abcdefghijklmno" "Ludwig von Wittgenstein"))

(defun show-memory ()
  *words-memorized*)

(defun write-memory (str-list)
  (setf *words-memorized* (sort (remove-duplicates str-list)
                                #'(lambda (str1 str2)
                                    (< (length str1)
                                       (length str2))))))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun get-last (the-list)
  (first (last the-list)))

(defun get-mid (the-list)
  (nth (1- (floor (/ (length the-list)
                     2)))
       the-list))

(defun give-up ()
  (setf *words-memorized* (sort (push (prompt-read "pls enter the word in your mind")
                                      *words-memorized*)
                                #'(lambda (str1 str2)
                                    (< (length str1)
                                       (length str2))))) ;setf forgotten!!

(defun refine-guessing-length (words lengths lowest highest)
  (list (remove-if #'(lambda (str)
                       (let ((str-length (length str)))
                         (or (> str-length highest)
                             (< str-length lowest))))
                   words)
        (remove-if #'(lambda (n)
                       (or (> n highest)
                           (< n lowest)))
                   lengths)))

(defun guess-length (possible-words words-lengths)
  (if words-lengths
      (if (> (length words-lengths)
             3) ;use cdddr?
          (let ((the-shortest (first words-lengths))
                (the-mid (get-mid words-lengths)))
            (if (y-or-n-p "is the word length between ~a & ~a latters?" the-shortest the-mid) ;if the length within the lower half?
                (apply #'guess-length (refine-guessing-length possible-words words-lengths the-shortest the-mid))
                (let ((the-longest (get-last words-lengths))
                      (the-mid (find-if #'(lambda (n)
                                            (> n the-mid))
                                        words-lengths)))
                  (if (y-or-n-p "is the word length between ~a & ~a latters?" the-mid the-longest) ;if the length within the higher half?
                      (apply #'guess-length (refine-guessing-length possible-words words-lengths the-mid the-longest))
                      nil))))
          (let ((the-shortest (first words-lengths)))
            (if (y-or-n-p "is the length of the word ~a latters?" the-shortest)
                (first (refine-guessing-length possible-words words-lengths the-shortest the-shortest))
                                        ;(guess-spelling (first (refine-guessing-length possible-words words-lengths the-shortest the-shortest)))
                (apply #'guess-length (refine-guessing-length possible-words words-lengths (+ 1 the-shortest) (get-last words-lengths))))))
      nil))

'( 3 5 7 10 12 13 15)
2 "or"
11 ""
18

ans 11
3~15 y
<=10 n
<=15 y
>=12 n => giveup

(defun guess-spelling (the-words)
  #+nil(print (list :the-words the-words))
  (if (null the-words)
      (give-up)
      (if (y-or-n-p "is the word \"~a?\"" (first the-words))
          (format t "The computer wins!")
          (guess-spelling (rest the-words)))))

(defun init-20q ()
  (guess-spelling (guess-length (when (> (length *words-memorized*)
                                         1)  ;sort *words-memorized* if there are morn than 1 words in it.
                                  (sort *words-memorized* #'(lambda (str1 str2) ;should use let!!
                                                              (< (length str1)
                                                                 (length str2)))))
                                (remove-duplicates (mapcar #'length *words-memorized*))))) ;make a list of the length of the memorized words.

(defun init-20q ()
  (guess-spelling (guess-length (when (cdr *words-memorized*)
                                  (sort *words-memorized* #'(lambda (str1 str2) ;should use let!!
                                                              (< (length str1)
                                                                 (length str2)))))
                                (remove-duplicates (mapcar #'length *words-memorized*))))) ;make a list of the length of the memorized words.
