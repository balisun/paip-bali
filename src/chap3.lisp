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

(defun init-20q ()
  (apply #'next-question (cons *words-memorized*
                               (funcall #'(lambda (words)
                                            (list (length (first (sort words #'(lambda (str1 str2)
                                                                                 (< (length str1)
                                                                                    (length str2))))))
                                                  (length (first (last words)))))
                                        *words-memorized*))))

(defun next-question (possible-words word-length-low word-length-high &optional word-gussed)
  "ask questions to the answerer and return 'I got it, the word in your mind is ____ !!' after answerer replies 'it'."
  (cond ((null possible-words) ;no words left.
         (push (prompt-read "pls enter the word in your mind: ")
               *words-memorized*))
        ((or (null (and word-length-high word-length-low)) ;one of the length bound not yet specified
             (/= word-length-high word-length-low)) ;don't know the length yet
         (ask-for-length))
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
my answer:
(local-a local-b local-b local-a global-b)
slime:
(LOCAL-A LOCAL-B LOCAL-B GLOBAL-A LOCAL-B)

