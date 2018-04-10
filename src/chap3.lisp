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
