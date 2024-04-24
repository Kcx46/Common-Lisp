;This code makes several operations using a polynomial as an input:
;zero(x)= 0
;p(x)= + 4x^3 + 3x^2 + 2x + 1 ;;q(x)= + 3x^2 + 5 
;p(x) + q(x) = + 4x^3 + 6x^2 + 2x + 6 
;p(x) * q(x) = + 12x^5 + 9x^4 + 26x^3 + 18x^2 + 10x + 5 
;p(q(x))= + 108x^6 + 567x^4 + 996x^2 + 586 
;0-p(x)= -4x^3 -3x^2 -2x -1 
;p(3)=142
;p '(x)= + 12x^2 + 6x + 2 
;p''(x)= + 24x + 6 ;

;I decleared some global variables
(defvar *polynomial* '())
(defvar *polynomial2* '())
(defvar *polynomial-result* '())
(defvar *polynomial-zero* '())



;functions declarations
(defun add-term (coeficiente grado)
  (push (cons coeficiente grado) *polynomial*)
  (setf *polynomial* (sort *polynomial* #'> :key #'cdr)))

(defun add-term2 (coeficiente grado)
  (push (cons coeficiente grado) *polynomial2*)
  (setf *polynomial2* (sort *polynomial2* #'> :key #'cdr)))

(defun add-term-zero (coeficiente grado)
  (push (cons coeficiente grado) *polynomial-zero*)
  (setf *polynomial-zero* (sort *polynomial-zero* #'> :key #'cdr)))
                                                                                                                                                                                                                                                                     
(defun add-term-poly (poly term)
  (let ((exponent (cdr term)))
    (push term poly)
    (setq poly (sort poly #'> :key #'cdr))
    poly))

(defun print-polynomial ()
  (format t "p(x)= ")
  (dolist (par *polynomial*)
    (cond
       ((> (cdr par) 1) (if (> (car par) 0)
                            (format t "+ ~dx^~d " (car par) (cdr par))
                          (format t "~dx^~d " (car par) (cdr par))))

       ((= (cdr par) 1)  (if (> (car par) 0)
                             (format t "+ ~dx " (car par))
                           (format t "~dx " (car par))))

       ((= (cdr par) 0) (if (> (car par) 0)
                            (format t "+ ~d " (car par))
                          (format t "~d " (car par)))))))

(defun print-polynomial2 ()
  (format t "q(x)= ")
  (dolist (par *polynomial2*)
    (cond
       ((> (cdr par) 1) (if (> (car par) 0)
                            (format t "+ ~dx^~d " (car par) (cdr par))
                          (format t "~dx^~d " (car par) (cdr par))))

       ((= (cdr par) 1)  (if (> (car par) 0)
                             (format t "+ ~dx " (car par))
                           (format t "~dx " (car par))))

       ((= (cdr par) 0) (if (> (car par) 0)
                            (format t "+ ~d " (car par))
                          (format t "~d " (car par)))))))
        


(defun suma-poly (poly1 poly2)
  (let ((result '()))
    (dolist (x poly1)
      (let ((sum (car x)))
        (dolist (y poly2)
          (when (= (cdr x) (cdr y))
            (setq sum (+ sum (car y)))))
        (push (cons sum (cdr x)) result)))
    (setq *polynomial-result* (sort result #'> :key #'cdr))))



(defun negative-poly (poly)
  (let ((result '()))
    (dolist (x poly)
      (let ((rest (car x)))
        (setq rest (cons (- 0 (car x))
                         (cdr x)))
        (push rest result))
        (setq *polynomial-result* (reverse result)))))
        

      
(defun print-suma ()
    (format t "p(x) + q(x) = ")
  (dolist (par *polynomial-result*)
    (cond
      ((> (cdr par) 1) (if (> (car par) 0)
                            (format t "+ ~dx^~d " (car par) (cdr par))
                          (format t "~dx^~d " (car par) (cdr par))))

       ((= (cdr par) 1)  (if (> (car par) 0)
                             (format t "+ ~dx " (car par))
                           (format t "~dx " (car par))))

       ((= (cdr par) 0) (if (> (car par) 0)
                            (format t "+ ~d " (car par))
                          (format t "~d " (car par)))))))

(defun print-resta (poly)
    (format t "0-p(x)= ")
  (dolist (par poly)
    (cond
      ((> (cdr par) 1) (if (> (car par) 0)
                            (format t "+ ~dx^~d " (car par) (cdr par))
                          (format t "~dx^~d " (car par) (cdr par))))

       ((= (cdr par) 1)  (if (> (car par) 0)
                             (format t "+ ~dx " (car par))
                           (format t "~dx " (car par))))

       ((= (cdr par) 0) (if (> (car par) 0)
                            (format t "+ ~d " (car par))
                          (format t "~d " (car par)))))))
(defun print-resta2 (poly)
    (format t "0-p(x)= ")
  (dolist (par poly)
    (cond
      ((> (cdr par) 1) (if (> (car par) 0)
                            (format t "+ ~dx^~d " (car par) (cdr par))
                          (format t "~dx^~d " (car par) (cdr par))))

       ((= (cdr par) 1)  (if (> (car par) 0)
                             (format t "+ ~dx " (car par))
                           (format t "~dx " (car par))))

       ((= (cdr par) 0) (if (> (car par) 0)
                            (format t "+ ~d " (car par))
                          (format t "~d " (car par)))))))




(defun inside-sum5 (poly)
  (let ((result (copy-list poly))) 
    (dolist (x poly)
      (dolist (y poly)
        (when (and (= (cdr x) (cdr y)) 
                   (not (= (car x) (car y))))
          (let ((suma (cons (+ (car x) (car y))
                           (cdr y))))
            (push suma result))
          (setq result (remove x result :test #'equal))))) 
    (setq *polynomial-result* (remove-duplicates result :test #'equal))
    (sort *polynomial-result* #'> :key #'cdr)))


                

(defun print-multiply ()
   (format t "p(x) * q(x) = ")
  (dolist (par *polynomial-result*)
    (cond
      ((> (cdr par) 1) (if (> (car par) 0)
                            (format t "+ ~dx^~d " (car par) (cdr par))
                          (format t "~dx^~d " (car par) (cdr par))))

       ((= (cdr par) 1)  (if (> (car par) 0)
                             (format t "+ ~dx " (car par))
                           (format t "~dx " (car par))))

       ((= (cdr par) 0) (if (> (car par) 0)
                            (format t "+ ~d " (car par))
                          (format t "~d " (car par)))))))


(defun evaluation2 (evaluator)
  "Horner"
  (let ((result 0))
    (dolist (term *polynomial*)
      (setq result (+ (* result evaluator) (car term))))
    (setq *polynomial-result* result)
    (format t "p(~a)=~a" evaluator *polynomial-result*)))

;;COMPOSE!!!!
(defvar *polynomial-result2* '())
(defvar *polynomial-result3* '())
(defvar *polynomial-result4* '())
(defvar zero 0)                  


(defun compose-poly (poly1 poly2)
  (let ((result '()))
    (dolist (term poly1)
      (let ((modified-poly1 (list (cons (car term) 0))))   
        (setq result (suma-poly4 (multiply-polynomials4 poly2 result) modified-poly1))
        (push result *polynomial-result*)
        (setq *polynomial-result* (remove-duplicates result :test #'equal))))))


(defun suma-poly4 (poly1 poly2)
  (let ((result '()))
    (dolist (x poly1)
      (let ((sum (car x)))
        (dolist (y poly2)
          (when (= (cdr x) (cdr y))
            (setq sum (+ sum (car y)))))
        (push (cons sum (cdr x)) result)))
    (dolist (y poly2)
      (unless (some (lambda (x) (= (cdr x) (cdr y))) poly1)
        (push y result)))
    (setq *polynomial-result* (sort result #'> :key #'cdr))))





(defun multiply-polynomials4 (poly1 poly2)
  (let ((result '()))
    (dolist (x poly1)
      (dolist (y poly2)
        (let ((product (cons (* (car x) (car y))
                             (+ (cdr x) (cdr y)))))
          (push product result))))
    (setq *polynomial-result3* (sort result #'> :key #'cdr))
    (inside-sum5 *polynomial-result3*)))







(defun ciclo ()
  (dolist (x *polynomial*)
    (format t "~a~%" x)))

(defun inside-sum6 (poly1)
  (let ((result (copy-list poly1))) 
    (dolist (x poly1)
      (dolist (y poly1)
        (when (and (= (cdr x) (cdr y)) 
                   (not (= (car x) (car y))))
          (let ((suma (cons (+ (car x) (car y))
                           (cdr y))))
            (push suma result))
          (setq result (remove x result :test #'equal))))) 
    (setq *polynomial-result4* (remove-duplicates result :test #'equal))
    (sort *polynomial-result4* #'> :key #'cdr)))

;;COMPOSEEEE!!
                                                                                                                                                                                                                                        

(defun multiply-polynomials (poly1 poly2)
  (let ((result '()))
    (dolist (x poly1)
      (dolist (y poly2)
        (let ((product (cons (* (car x) (car y))
                             (+ (cdr x) (cdr y)))))
          (push product result))))
    (setq *polynomial-result* (sort result #'> :key #'cdr))))
      
(defun derivacion (poly)
  (let ((result '()))
    (dolist (x poly)
      (let ((deriv (cons (* (car x) (cdr x))
                         (- (cdr x) 1))))
      (push deriv result)
      (setq *polynomial-result*(reverse result))))
      ))

(defun print-deriv (poly)
   (format t "p '(x)= ")
  (dolist (par poly)
    (cond
      ((> (cdr par) 1) (if (> (car par) 0)
                            (format t "+ ~dx^~d " (car par) (cdr par))
                          (format t "~dx^~d " (car par) (cdr par))))

       ((= (cdr par) 1)  (if (> (car par) 0)
                             (format t "+ ~dx " (car par))
                           (format t "~dx " (car par))))

       ((= (cdr par) 0) (if (> (car par) 0)
                            (format t "+ ~d " (car par))
                          (format t "~d " (car par)))))))
(defun print-compose (poly)
   (format t "p(q(x))= ")
  (dolist (par poly)
    (cond
      ((> (cdr par) 1) (if (> (car par) 0)
                            (format t "+ ~dx^~d " (car par) (cdr par))
                          (format t "~dx^~d " (car par) (cdr par))))

       ((= (cdr par) 1)  (if (> (car par) 0)
                             (format t "+ ~dx " (car par))
                           (format t "~dx " (car par))))

       ((= (cdr par) 0) (if (> (car par) 0)
                            (format t "+ ~d " (car par))
                          (format t "~d " (car par)))))))


(defun print-segunda-deriv (poly)
   (format t "p''(x)= ")
  (dolist (par poly)
    (cond
      ((> (cdr par) 1) (if (> (car par) 0)
                            (format t "+ ~dx^~d " (car par) (cdr par))
                          (format t "~dx^~d " (car par) (cdr par))))

       ((= (cdr par) 1)  (if (> (car par) 0)
                             (format t "+ ~dx " (car par))
                           (format t "~dx " (car par))))

       ((= (cdr par) 0) (if (> (car par) 0)
                            (format t "+ ~d " (car par))
                          (format t "~d " (car par)))))))

(defun print-zero ()
   (format t "zero(x)= ")
  (dolist (par *polynomial-zero*)
    (cond
      ((> (cdr par) 1) (if (> (car par) 0)
                            (format t "+ ~dx^~d " (car par) (cdr par))
                          (format t "~dx^~d " (car par) (cdr par))))

       ((= (cdr par) 1)  (if (> (car par) 0)
                             (format t "+ ~dx " (car par))
                           (format t "~dx " (car par))))

       ((= (cdr par) 0) (if (= (car par) 0)
                            (format t "0 " (car par))
                          (format t "~d " (car par)))))))

             

(setq *polynomial-result* nil)
(setq *polynomial* nil)
(setq *polynomial2* nil)

;functions calls
(add-term 4 3)
(add-term 3 2)
(add-term 2 1) 
(add-term 1 0)


(add-term2 3 2)
(add-term2 5 0)


(add-term-zero 0 0)



(print-zero)
(format t "~%")
(print-polynomial)
(format t "~%")
(print-polynomial2)
(format t "~%")
(suma-poly4 *polynomial* *polynomial2*)
(print-suma)
(setq *polynomial-result* nil)
(format t "~%")
(multiply-polynomials *polynomial* *polynomial2*)
(inside-sum5 *polynomial-result*)
(print-multiply)
(format t "~%")
(compose-poly *polynomial* *polynomial2*)
(print-compose *polynomial-result*)
(negative-poly *polynomial*)                                   
(suma-poly *polynomial-result* *polynomial-zero*)
(format t "~%")
(print-resta *polynomial-result*)
(format t "~%")
(evaluation2 3)
(format t "~%")
(derivacion *polynomial*)
(print-deriv *polynomial-result*)
(format t "~%")

(derivacion *polynomial-result*)
(print-segunda-deriv *polynomial-result*)




(setq *polynomial-result* nil)
(setq *polynomial* nil)
(setq *polynomial2* nil)
(setq *polynomial-result2* nil)
(setq *polynomial-result3* nil)
(setq *polynomial-result4* nil)
(setq *polynomial-zero* nil)
