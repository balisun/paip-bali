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
                              (write-to-string (car ob))
                              " . "
                              (dp-str (cdr ob))
                              ")")
                 (write-to-string ob))))
    (princ (dp-str obj))))

(defun print-dot-pair1 (obj)
  "revised to be tail-recursive."
  (labels ((dp-str (ob &optional (prefix "") (suffix ""))
             (if (consp ob)
                 (dp-str (cdr ob)
                         (concatenate 'string
                                      prefix
                                      "("
                                      (write-to-string (car ob))
                                      " . ")
                         (concatenate 'string
                                      ")"
                                      suffix))
                 (concatenate 'string
                              prefix
                              (write-to-string ob)
                              suffix))))           
    (princ (dp-str obj))))

;;; Exercise 3.4
(defun print-opt (obj &optional (dot-pair-p nil))
  (if dot-pair-p
      (print-dot-pair1 obj)
      (print obj)))

;;; Exercise 3.5

(defvar *words-memorized* '("bali" "orange"))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun get-last (the-list)
  (first (last the-list)))

(defun get-mid (the-list)
  (nth (floor (/ (length the-list)
                 2))
       the-list))
               
(defun give-up ()
  (sort (push (prompt-read "pls enter the word in your mind")
              *words-memorized*)
        #'(lambda (str1 str2)
            (< (length str1)
               (length str2)))))

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
             3)
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
      nil)))



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
                                  (sort *words-memorized* #'(lambda (str1 str2)
                                                              (< (length str1)
                                                                 (length str2)))))
                                (remove-duplicates (mapcar #'length *words-memorized*))))) ;make a list of the length of the memorized words.

;;; Exercise 3.6
Q:
(setf a 'global-a)
(defvar *b* 'global-b)

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
              (replace the-list
                       '(1)
                       :start1 0
                       :end1 1))))
;;; Exercise 3.11
acons

;;Exercise 3.12

(defun refine-cases (words-list)
  (format nil "~@(~{~a ~}~a.~)"
          (butlast words-list)
          (car (last words-list))))
