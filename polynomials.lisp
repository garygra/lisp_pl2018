;;; Define the Polynomial Class
;;; using CLOS
(defclass polynomial ()       ; Name of the class
  ((poly :accessor poly       ; (slot :accessor_func func_name
         :initform nil        ;       :initial value
         :initarg :poly)      ;       :initialization argument
   (deg :accessor poly-deg
        :initform 0
        :initarg :deg)
   )
)

;;; Make a polynomial taking advantage of program = data
;;; new polynomial term of the form a * x^b:
(defun make-polynomial (a b)
    (MAKE-INSTANCE 'polynomial :poly  `(+ (* ,a (expt x ,b))) :deg b) ; generic function to create an instance
)

;;; Returns the degree of a given polynomial
(defmethod degree ((p polynomial)) ; method for the polynomial class
    (labels ( (degree-1 (poly max) 
                (cond  ( (null poly) max )  ;the end of p has been reached
                        ( (> (caddr (caddar poly)) max) (degree-1 (cdr poly) (caddr (caddar poly))) ) ;There is a term of higher degree
                        (t (degree-1 (cdr poly) max))  ; Eval the next term
                )
               )
             )
            (degree-1 (cdr (slot-value p 'poly)) 0)
     )
    
    
)

(defmethod print-object ((poly polynomial) out) ; Define a print-object method for polynomial class
    (labels ((to-string (stack term poly)       ; func to determine what part of the term we are on
                (cond  ((null term) (to-string-1  stack "" )) ;end of term
                       ((atom term) term)       ; term is not a list (number or var)
                       ((eql (car term) '*)     ; Term is (* n list) 
                            (to-string (push    ; push into a stack 
                                `(,(to-string stack (cadr term) poly) ,(to-string stack (caddr term) poly) ) stack) 
                                 (car poly) (cdr poly) ) )
                       ((eql (car term) 'expt) `(,(cadr term),(caddr term))) ; We are in the last part of term (expt var m)
                 )
              )
              (to-string-1 (stack res )  ; func to go from stack to string
                           (let* ((coef (caar stack))     ;get the parts of terms
                                 (var  (caadar stack))
                                 (exp  (car (cdadar stack)))
                                 (str  (if (or (null coef)      ; prevent str from being nil 
                                               (equal res "") )
                                               (if ( null coef ) 
                                                   res 
                                                   (format nil "~:[-~;~]" (>= coef 0)) )       ; Set the sign for the next
                                               (format nil "~a ~:[- ~;+ ~]" res (>= coef 0)))) ; term (empty if first term)
                                 )
                                 
                                 (cond  ; Test all posible cases
                                       ((null stack) (if (equal res "") "0" res )) ; The poly is 0
                                       ((= coef 0) (to-string-1 (cdr stack) (format nil "~a" res))) ; x^0
                                       ((= exp 0)  (to-string-1 (cdr stack) (format nil "~a~a" str (abs coef) ))) ; 0x
                                       ((and (= exp 1) (= (abs coef) 1)) (to-string-1 (cdr stack) (format nil "~a~a" str var ))) ; 1x^1
                                       ((= (abs coef) 1) (to-string-1 (cdr stack) (format nil "~a~a^~a" str var exp ) ) ) ; 1x^n or -1x^n
                                       ((= exp 1) (to-string-1 (cdr stack) (format nil "~a~a~a" str (abs coef) var))) ; x^1
                                       (t (to-string-1 (cdr stack) (format nil "~a~a~a^~a" str (abs coef) var exp ))) ; nx^m
                                )
                         )
             ) )
            ; send the res string to out
           (format out "~a" (to-string '() (cadr (slot-value poly 'poly)) (cddr (slot-value poly 'poly) )  ) )
     )
    
    

      
)

(defmethod plus ((p1 polynomial) (p2 polynomial))
    
    (labels (
             (plus-1 (p1 p2 res)    
                   (let ( (coef1 (cadar p1))   ; Get the parts of term1
                          (var1  (cadr(caddar p1)))
                          (expt1 (caddr (caddar p1)))
                          (coef2 (cadar p2))   ; Get the parts of term2
                          (var2  (cadr(caddar p2)))
                          (expt2 (caddr (caddar p2)))
                         )
                        
                        (cond ((and (null p1) (null p2)) (cons '+ (reverse res)) ) ; The end of both polys
                              ((null p1) (plus-1 nil (cdr p2) (push `(* ,coef2 (expt ,var2 ,expt2)) res))) ; End of p1 but no p2
                              ((null p2) (plus-1 nil (cdr p1) (push `(* ,coef1 (expt ,var1 ,expt1)) res))) ; End of p2 but no p1
                              ((= expt1 expt2) ; expts are equal ==> add
                                (plus-1 (cdr p1) (cdr p2) (push `(* ,(+ coef1 coef2) (expt ,var1 ,expt1)) res ) ))
                              ((< expt1 expt2) ; expt1 is smaller ==> get next term1
                                (plus-1 (cdr p1) p2  (push `(* ,coef1 (expt ,var1 ,expt1)) res)) )
                              ((> expt1 expt2) ; expt2 is smaller ==> get next term2
                                (plus-1 p1 (cdr p2)  (push `(* ,coef2 (expt ,var2 ,expt2)) res)) )
                         )
                    )
               ) 
             
             )
            (let ((poly_res (MAKE-INSTANCE 'polynomial ) )   ; Create a new instance of poly - the returned val
                  (poly (plus-1 (cdr (slot-value p1 'poly)) (cdr (slot-value p2 'poly)) '() )) ; poly <= p1 + p2 
                   )
                 (setf (poly poly_res)  poly)     ; poly_res.poly <= poly
                 (setf (poly-deg  poly_res) (degree poly_res)) ;poly_res.degree <= degree(poly_res)
                 poly_res
                ) 
            
      )
)

(defmethod minus ((p1 polynomial) (p2 polynomial))
    (labels ( (minus-1 (p1 p2 res)
                   (let ( (coef1 (cadar p1))
                          (var1  (cadr(caddar p1)))
                          (expt1 (caddr (caddar p1)))
                          (coef2 (cadar p2))
                          (var2  (cadr(caddar p2)))
                          (expt2 (caddr (caddar p2)))
                         )
                        ; Almost the same as plus
                        (cond ((and (null p1) (null p2)) (cons '+ (reverse res)) )
                              ((null p1) (minus-1 nil (cdr p2) (push `(* ,(- coef2) (expt ,var2 ,expt2)) res)))
                              ((null p2) (minus-1 nil (cdr p1) (push `(* ,coef1 (expt ,var1 ,expt1)) res)))
                              ((= expt1 expt2)  
                                (minus-1 (cdr p1) (cdr p2) (push `(* ,(- coef1 coef2) (expt ,var1 ,expt1)) res ) ))
                              ((< expt1 expt2) 
                                (minus-1 (cdr p1) p2  (push `(* ,coef1 (expt ,var1 ,expt1)) res)) )
                              ((> expt1 expt2) 
                                (minus-1 p1 (cdr p2)  (push `(* ,(- coef2) (expt ,var2 ,expt2)) res)) )
                         )
                    )
               ) 
             
             )
            (let ((poly_res (MAKE-INSTANCE 'polynomial ) )
                  (poly (minus-1 (cdr (slot-value p1 'poly)) (cdr (slot-value p2 'poly)) '() ))
                   )
                 (setf (poly poly_res) poly)
                 (setf (poly-deg  poly_res) (degree poly_res))
                 poly_res
                ) 
      )
)

;;; func to consolidate terms (those that have tha same expt)
(defun poly-shortener (p term p_aux ) ; p is a list, term is one term of p (not in p) and p_aux is the poly result 
                      (let* ((poly_aux1 (MAKE-INSTANCE 'polynomial :poly (cons '+ `(,term) )))
                            (res (plus p_aux poly_aux1)) 
                           )
                           (if (null p) res (poly-shortener (cdr p) (car p) res) )
                      )
             )

(defmethod times ((p1 polynomial) (p2 polynomial))
    (labels ((times-1 (p1 p2 res) 
                  ;iterate over p2
                  (cond ((null p2) res) ; end of p2 reached
                        (t  (times-1 p1 (cdr p2) (times-2 p1 (car p2) res))) ;recursive call to times-1, res is times-2 recursive call
                            ) 
             )
            (times-2 (p1 term2 res)
                     (let ((coef1 (cadar p1))
                           (var1  (cadr(caddar p1)))
                           (expt1 (caddr (caddar p1)))
                           (coef2 (cadr term2))
                           (var2  (cadr (caddr term2)))
                           (expt2 (caddr(caddr term2)))
                          )
                          ; iterate over p1
                         (cond ((null p1) res) ; end of p1 reached
                               (t (times-2 (cdr p1)  ;recursive call to times-2
                                           term2 
                                           (cons `(* ,(* coef1 coef2) (expt ,var1 ,(+ expt1 expt2))) res)))
                          )
                      )
             )
             (times-3 (p term p_aux )
                      (let* ((poly_aux1 (MAKE-INSTANCE 'polynomial :poly (cons '+ `(,term) )))
                            (res (plus p_aux poly_aux1)) 
                           )
                           (if (null p) res (times-3 (cdr p) (car p) res) )
                      )
                
                      
             )
            
            )
            (let ( (res_tmp (times-1 (cdr (slot-value p1 'poly)) (cdr (slot-value p2 'poly)) '() ))
                  )
                 (poly-shortener (cdr res_tmp) (car res_tmp) (make-polynomial 0 0)  )
             )
            )
    
    
    )

(defmethod compose ((p1 polynomial) (p2 polynomial))
    (labels ((compose-1 (p1 p2 res) ;; p1 is a list, p2 is a polynomial as is res
                (cond ((null p1) res) ; end of p1 reached
                      (t (compose-1 (cdr p1) p2 (plus (make-polynomial (cadar p1) 0) (times p2 res))))
                 )
             ) )
             (compose-1 (reverse (cdr  (slot-value p1 'poly))) p2 (make-polynomial 0 0))
     )
    
 )

(defmethod evaluate ((p1 polynomial) val)
             (eval (subst val 'x (poly p1) ))
)

(defmethod differentiate ((p polynomial))
    (labels ((differentiate-1 (p1 res)
             (let ( (coef (cadar p1))    ;get the parts of term
                    (var (cadr (caddar p1)))
                    (exp (caddr (caddar p1)))
                          )            
                     (cond ((null p1) res)  
                           ((= exp 0) (differentiate-1 (cdr p1) (cons `(* 0 (expt ,var 0)) res)))
                           (t (differentiate-1 (cdr p1) (cons `(* ,(* coef exp) (expt ,var ,(- exp 1))) res) ))
                          )
                      )
             )
         ) 
            (let ( (res_tmp (differentiate-1 (cdr (slot-value p 'poly)) '() ))
                  )
                 (poly-shortener (cdr res_tmp) (car res_tmp) (make-polynomial 0 0)  )
             )
    )
 )
    
(defvar *zero* (make-polynomial 0 0))
(defvar *p1* (make-polynomial 4 3))
(defvar *p2* (make-polynomial 3 2))
(defvar *p3* (make-polynomial 1 0))
(defvar *p4* (make-polynomial 2 1))
(defvar *p* (plus *p1* (plus *p2* (plus *p3* *p4*))))

(defvar *q1* (make-polynomial 3 2))
(defvar *q2* (make-polynomial 5 0))
(defvar *q* (plus *q1* *q2*))

(defvar *r* (plus *p* *q*))
(defvar *s* (times *p* *q*))
(defvar *tt* (compose *p* *q*))


(format t "zero(x)     = ~s~C" *zero* #\linefeed)
(format t "p(x)        = ~s~C" *p* #\linefeed)
(format t "q(x)        = ~s~C" *q* #\linefeed)
(format t "p(x) + q(x) = ~s~C" *r* #\linefeed)
(format t "p(x) * q(x) = ~s~C" *s* #\linefeed)
(format t "p(q(x))     = ~s~C" *tt* #\linefeed)
(format t "0 - p(x)    = ~s~C" (minus *zero* *p*) #\linefeed)
(format t "p(3)        = ~s~C" (evaluate *p* 3) #\linefeed)
(format t "p'(x)       = ~s~C" (differentiate *p*) #\linefeed )
(format t "p''(x)      = ~s~C" (differentiate (differentiate *p*)) #\linefeed )
