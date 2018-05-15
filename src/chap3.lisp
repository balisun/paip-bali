;;; trainning session:
;;; Exercise 3.6 symbol-value


;;;list*
(defun list*-bali (&rest args)
  (cond ((null args)
         "error")
        ((= 1 (length args))
         (first args))
        (t
         (apply #'list*-bali
                (append (butlast (butlast args))
                        (list (cons (first (last (butlast args)))
                                    (first (last args)))))))))

(defun list*% (a &rest r)
 (if r
     (cons a (apply #'list*% r))
     a))

;;; Exercise 3.1
(let* ((x 6)
       (y (* x x)))
  (+ x y))

((lambda (x)
   (lambda (y)
     (+ x y))
   (* x x))
 6) ; =>36

(funcall #'(lambda (x)
             (funcall #'(lambda (y)
                         (+ x y))
                      (* x x)))
         6)


(let ((x 6))
  (let ((y (* x x)))
    (+ x y)))

((lambda (x)
   (let ((y (* x x)))
     (+ x y)))
 6)

((lambda (x)
   ((lambda (y)
      (+ x y))
    (* x x)))
 6)

(funcall #'(lambda (x)
             (funcall #'(lambda (y)
                          (+ x y))
                      (* x x)))
         6)

;;; Exercise 3.2
cons = list* w/ only 2 arguments.

;;; Exercise 3.3

(defun print-dot-pair0 (obj)
  "it seems that princ is for dealing with strings only (escape double quotes) ?"
  (labels ((dp-str (ob)
             (if (consp ob)
                 (concatenate 'string
                              "("
                              (write-to-string (dp-str (car ob)))
                              " . "
                              (dp-str (cdr ob))
                              ")")
                 (write-to-string ob))))
    (princ (dp-str obj))))

(print-dot-pair0 '(((a b c d))))(((A B C D)) . NIL)"(((A B C D)) . NIL)"

(defun print-dot-pair1 (obj)
  "revised to be tail-recursive."
  (labels ((dp-str (ob &optional (prefix "") (suffix ""))
             (if (consp ob)
                 (dp-str (cdr ob)
                         (concatenate 'string
                                      prefix
                                      "("
                                      (write-to-string (dp-str (car ob)))
                                      " . ")
                         (concatenate 'string
                                      ")"
                                      suffix))
                 (concatenate 'string
                              prefix
                              (write-to-string ob)
                              suffix))))           
    (princ (dp-str obj))))

(defun print-dot-pair2 (obj)
  (if (consp obj)
      (progn
        (princ "(")
        (print-dot-pair2 (car obj))
        (princ " . ")
        (print-dot-pair2 (cdr obj))
        (princ ")")
        obj)
      (princ obj)))

;;;Answer 3.3
(defun dprint (x) 
"Print an expression in dotted pair notation. use cond for implicit progn."
  (cond ((atom x)
         (princ x)) 
        (t (princ "(")
           (dprint (first x)) 
           (pr-rest (rest x)) 
           (princ ")") 
           X))) 

(defun pr-rest (x) 
  (princ " . ") 
  (dprint x)) 

;;; Exercise 3.4
(defun print-dp-list (obj)
  (if (atom obj)
      (princ obj)
      (progn
        (princ "(")
        (print-dp-list (car obj))
        (let ((the-cdr (cdr obj)))
          (cond ((null the-cdr))
                ((atom the-cdr)
                 (princ " . ")
                 (princ the-cdr))
                (t
                 (princ " ")
                 (print-dp-list the-cdr))))                 
        (princ ")")
        obj)))

(defun print-dp-list1 (obj)
  (if (atom obj)
      (princ obj)
      (progn
        (princ "(")
        (print-dp-list1 (car obj))
        (print-cdr (cdr obj))
        (princ ")")
        obj)))

(defun print-cdr (the-cdr)
  (cond ((null the-cdr))
        ((atom the-cdr)
         (princ " . ")
         (princ the-cdr))
        (t
         (princ " ")
         (print-dp-list1 (car the-cdr))
         (print-cdr (cdr the-cdr)))))
               

;;; Answer 3.4
(defun dprint2 (x) 
"Print an expression in dotted pair notation. use cond for implicit progn."
  (cond ((atom x)
         (princ x)) 
        (t (princ "(")
           (dprint2 (first x)) 
           (pr-rest2 (rest x)) 
           (princ ")") 
           X))) 

(defun pr-rest2 (x) 
  (cond ((null x)) 
        ((atom x) (princ " . ") (princ x)) 
        (t (princ " ") (dprint2 (first x)) (pr-rest2 (rest x))))) 

;;; Exercise 3.5 - pls find chap3-20q.lisp

;;; Exercise 3.6
Q:
; (setf a 'global-a) => lexical
(defvar *b* 'global-b) ;=> dynamical

(defun fn () *b*)

(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))

my answer:
(local-a local-b local-b local-a global-b)
slime:
(LOCAL-A LOCAL-B LOCAL-B GLOBAL-A LOCAL-B)

(symbol-value 'a)
;;the function symbol-value always treats its arguments as special veriables
;;because 'a is not defined as a dynnamic variable, let create a new lexical binding for a & shadow dynamical binding of *b*?
;;what indeed happen when (setf a ...) without defvar in advance? a special but not dynamic variable?

CL-USER> (defvar *b* 'global-b) ;=> dynamical

(defun fn () *b*)

(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))
; Evaluation aborted on #<UNBOUND-VARIABLE A {100343D753}>.
CL-USER> (symbol-value 'a)
; Evaluation aborted on #<UNBOUND-VARIABLE A {10036EE803}>.
CL-USER> (setf (symbol-value 'a) 'bla) => can be used in let
BLA
CL-USER> (symbol-value 'a)
BLA

=> can be used in let

(set 'a 'sss)
(setf (symbol-value 'a) 'bla)

;;;Exercise 3.8

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  ;;original version
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun find-all-1 (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "for KCL. remove duplicated key word & value in keyword-args"
  (flet ((remove-original-key (the-key args)
           (let ((n1 (search (list the-key)
                            args)))
             (append (subseq args 0 n1)
                     (subseq args (+ n1 2) (length args))))))
    (if test-not
        (apply #'remove item sequence 
               :test-not (complement test-not)
               (remove-original-key :test-not keyword-args))
        (apply #'remove item sequence
               :test (complement test)
               (remove-original-key :test keyword-args)))))

;;;Exercise 3.9

CL-USER> (reduce #'list nil)
NIL
CL-USER> (reduce #'list '(1))
1
CL-USER> (reduce #'list '(1 2))
(1 2)
CL-USER> (reduce #'list '(1 2 3))
((1 2) 3)


(defun length-by-reduce (the-list)
  (if (null the-list)
      0
      (reduce #'(lambda (sum e2)
                  (1+ sum))
              (print (replace the-list
                              '(1)
                              :start1 0
                              :end1 1)))))

(defun length-by-reduce1 (the-list)
  (reduce #'(lambda (sum e2)
              (1+ sum))
          (print (cons 0 the-list))))

;;; Exercise 3.11
acons

;;Exercise 3.12

(defun refine-cases (words-list)
  (format nil "~@(~{~a ~}~a.~)"
          (butlast words-list)
          (car (last words-list))))
