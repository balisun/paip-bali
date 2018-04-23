;;; trainning session:
;;; Exercise 3.6 symbol-value



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
             funcall #'(lambda (y)
                         (+ x y))
             (* x x))
         6) ; => error!!o


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

(defun get-last (list)
  (first (last list)))

(defun get-mid (list))
               
(defun init-20q ()
  (guess-spelling (guess-length (when (> (length *words-memorized*)
                                         1)  ;sort *words-memorized* if there are morn than 1 words in it.
                                  (sort *words-memorized* #'(lambda (str1 str2)
                                                              (< (length str1)
                                                                 (length str2)))))
                                (remove-duplicates (mapcar #'length *words-memorized*))))) ;make a list of the length of the memorized words.

(defun give-up ()
  (push (prompt-read "pls enter the word in your mind: ")
        *words-memorized*))

(defun refine-guessing-length (words lengths shortest highest)
  (list (remove-if #'(lambda (str)
                       (let ((str-length (length str)))
                         (or (> str-length highest)
                             (< str-length lowest))))
                   possible-words)
        (remove-if #'(lambda (n)
                       (or (> n highest)
                           (< n lowest)))
                   words-lengths)))

(defun guess-length (possible-words words-lengths)
  (if words-lengths
      (if (> (length words-lengths)
             3)
          (let ((the-shortest (first words-lengths))
                (the-mid (get-mid words-lengths)))
            (if (y-or-n-p "is the word length between ~a & ~a latters?" the-shortest the-mid) ;if the length within the lower half?
                (guess-length (refine-guessing-length possible-words words-lengths the-shortest the-mid))
                (let ((the-longest (get-last words-lengths))
                      (the-mid (find-if #'(lambda (n)
                                            (> n the-mid))
                                        words-lengths)))
                  (if (y-or-n-p "is the word length between ~a & ~a latters?" the-shortest the-mid) ;if the length within the higher half?
                      (guess-length (refine-guessing-length possible-words words-lengths the-mid the-shortest))
                      (give-up)))))
          (length=shortest?)
          nil)))

(defun guess-spelling (possible-words))

(defun next-question (possible-words words-lengths &optional word-gussed)
  "ask questions to the answerer and return 'I got it, the word in your mind is ____ !!' after answerer replies 'it'."
  (cond ((null possible-words) ;no words left.
         (push (prompt-read "pls enter the word in your mind: ")
               *words-memorized*))
        ((/= word-length-high word-length-low) ;don't know the length yet
         (if (y-or-n-p "is the word length between ~a & ~a latters?" word-length-low word-length-high)
             (next-question (delete-if #'(lambda (w)
                                           (>= (length w)
                                               word-length-high))
                                       possible-words
                                       :from-end t)
                            word-length-low
                            (length (first (last possible-words))))
             (next-question possible-words
                              new-length-high
                              (if (> 1 (- word-length-high new-length-high))
                                  word-length-high
                                  new-length-high)))))
        (word-gussed ;guessing spelling.
         (let* ((reply-sum (ask-for-reply))
                (the-reply (first reply-sum))
                (the-answer (second reply-sum)))
           (cond (eql :it the-reply)
               (:it (format t
                            "I got it,I got it, the word in your mind is ~a !!"
                            (second reply-sum)))
               (:yes (next-question (yes-modified-input-args)))
               (:no (next-question (no-modified-input-args)))
               (:end return "quit!"))))))



        ;; ((null word-length-high) ;whether know the upper bound of the length of the word.
        ;;  (let ((new-length-high
        ;;          (* 10 word-length-low)))
        ;;    (if (y-or-n-p "is the word shorter than ~a latters?" new-length-high)
        ;;        (next-question possible-words word-length-low new-length-high)
        ;;        (next-question possible-words new-length-high))))
        ;; ((/= word-length-high word-length-low) ;don't know the length yet
        ;;  (let ((new-length-high
        ;;          (round (/ (+ word-length-high word-length-high)
        ;;                    2))))
        ;;    (if (y-or-n-p "is the word shorter than ~a latters?" new-length-high)
        ;;        (next-question possible-words
        ;;                       word-length-low
        ;;                       (if (> 1 (- new-length-high word-length-low))
        ;;                           word-length-low
        ;;                           new-length-high))
        ;;        (next-question possible-words
        ;;                       new-length-high
        ;;                       (if (> 1 (- word-length-high new-length-high))
        ;;                           word-length-high
        ;;                           new-length-high)))))



(defun think-status (possible-words &key word-length word-gussed)
  "return a status indicator by the remaining list of memorized words."
  )

(defun ask-for-reply (guess-status)
  "when not giv-ing-up, generate the question by the possible words, prompt form answerer to return reply 'yes' 'no' 'it' & the answer."
  (format t "" (case guess-status
                 (length-range-case length-range-question)
                 (a-length-case a-length-question)
                 (nth-latter-case nth-latter-question)))
  (prompt-from-answerer)
  (list the-reply the-answer))      
      

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
