#lang racket

(provide (all-defined-out))
(define file1 (vector-ref (current-command-line-arguments) 0))
(define in (file->list file1))
;(define N (read in))
;(define D (read in))
;(define K (read in)) 
;(define eps (read in))
;(define minpts (read in))
(define start (take in 5))
(define in1 (drop in 5))
(define N (car start))
(define s2 (cdr start))
(define D (car s2))
(define s3 (cdr s2))
(define K (car s3))
(define s4 (cdr s3))
(define eps (car s4))
(define s5 (cdr s4))
(define minpts (car s5))


;(write in1)


(define (splitlist li i c)
  ;(when (empty? li) '())
  (if(<= i N)(append (list (take li c)) (splitlist (drop li c) (+ i 1) c)) '()))

(define in2 (splitlist in1 1 D))

(define (ques1 li j)
  (if (<= j N)(append (list (append (list j) (list (car li)))) (ques1 (cdr li) (+ j 1))) '()))
  
(define step1 (ques1 in2 1))
(display step1)
(display "\n")


(define (encore li li2)
  ;(write "HELLO")
  ;(write li)
  ;(write li2)
  (if (not(empty? li))  (+ (expt (- (car li) (car li2)) 2) (encore (cdr li) (cdr li2))) 0) )

(define (newfunc li li2)
  ;(write li)
  ;(write li2)
  (if (= (car li) (car li2)) +inf.0 (encore (car (cdr li)) (car (cdr li2)))))

(define (calc li li2 j)
  ;use li and step1
  
  (if (<= j N) (append (list (append  (list (car (car li2))) (list (sqrt (newfunc li (car li2)))))) (calc li (cdr li2) (+ j 1))) '()))

(define (sim_mat li i)
  ;(write "HARSHA")
  (if (<= i N) (append (list (calc (car li) step1 1)) (sim_mat (cdr li) (+ i 1))) '()))

(define step21 (sim_mat step1 1))



(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

(define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)

(define step2 (modify_precision step21))
(display step2)
(display "\n")

(define (sort1 li)
  (sort li (lambda (a b) (< (car (cdr a)) (car (cdr b))))))

(define (sort2 li)
  (sort li (lambda (a b) (> (car (cdr a)) (car (cdr b))))))
;(write(sort1 (car step2)))

(define (chooseNN li i)
  (if (<= i K) (append (list (car (car li))) (chooseNN (cdr li) (+ i 1))) '()))

(define (kfunc li)
  (list (sort (chooseNN (take li K) 1) <) ))
  
(define (knn li i)
  (if (<= i N) (append (kfunc (sort1 (car li))) (knn (cdr li) (+ i 1))) '()))

(define step3 (knn step2 1))

(display step3)
(display "\n")

(define (sfunc3 li1 li2)
  ;(write li1)
  ;(write li2)
  (cond
    [(empty? li1) 0]
    [(empty? li2) 0]
    [(= (car li1) (car li2)) (+ 1 (sfunc3 (cdr li1) (cdr li2)))]
        [(< (car li1) (car li2)) (+ 0 (sfunc3 (cdr li1) li2))]
        [(< (car li2) (car li1)) (+ 0 (sfunc3 (cdr li2) li1))]))

(define (sfunc2 ele i)
  (if (member i (list-ref step3 (- ele 1))) (list (append (list ele) (list (sfunc3 (list-ref step3 (- i 1)) (list-ref step3 (- ele 1)) )))) '()))

(define (sfunc li i)
  (if (not (empty? li)) (sort2(append (sfunc2 (car li) i) (sfunc (cdr li) i))) '()))
  

(define (shared li i)
  (if (<= i N) (append (list (sfunc (car li) i)) (shared (cdr li) (+ i 1))) '()))


(define step4 (shared step3 1))
(write step4)
(display "\n")

(define (dfunc1 li)
  (if (not(empty? li)) (+ (if ( >=  (car (cdr(car li))) eps) 1 0) (dfunc1 (cdr li))) 0))

(define (density i)
  (if (<= i N) (append (list(dfunc1(list-ref step4 (- i 1)))) (density (+ i 1))) '()))

(define step5 (density 1))

(display step5)
(display "\n")

(define (corefunc ele i)
  (if (>= ele minpts) i 0))

(define (core li i)
  ;(write li)
  (if (<= i N) (append (list (corefunc (car li) i)) (core (cdr li) (+ i 1))) '()))

(define step6temp (sort (core step5 1) <))

(define (remzero li)
  (if (not(empty? li))(if (not(= (car li) 0)) (append (list(car li)) (remzero (cdr li) )) (remzero (cdr li))) '()))

(define step6 (remzero step6temp))

(display step6)
(display "\n")

(define (find x li)
  (cond ((null? li) #f)
        ((eq? x (car li)) #t)
        (#t (find x (cdr li)))))



(define (step72 lix li li2 li3)

  (if (not(empty? li)) (if (member (car (car li)) step6)
                           (if (not (member (car (car li)) li2))
                               (if (not(member (car (car li)) lix)) (step72 lix (cdr li) li2 (append (list (car (car li))) li3)) (step72 lix (cdr li) li2 li3))
                               (step72 lix (cdr li) li2 li3))
                           (step72 lix (cdr li) li2 li3)) li3))

(define (step71 lix li i)

  (if (<= i (length li)) (step71 lix (append (step72 lix (list-ref step4 (- (list-ref li (- i 1)) 1)) li (list)) li) (+ i 1)) li))

(define (step70 li i c)

  (if (< i (length step6))  (if(not (member (list-ref step6 (- i 1)) li))
                                       (append (list(append  (list c)  (list(sort (step71 li (list(list-ref step6 (- i 1))) 1) <))))
                                       (step70 (append li (step71 li (list(list-ref step6 (- i 1)))1)) (+ i 1) (+ c 1)))
                                       (append '() (step70 li (+ i 1) c))) '()))
(define step7 (step70 (list) 1 1))
(display step7)
(display "\n")
(define (noise1 i li)
  ;(write "H")
  ;(write li)
  ;(write (car (cdr (car li))))
  (if (not(empty? li)) (if (>= (car (cdr (car li))) eps) '() (noise1 i (cdr li)))(list i)))

(define (noise i)
  (if (<= i N) (if (not(member i step6)) (append (noise1 i (list-ref step4 (- i 1))) (noise (+ i 1))) (append '() (noise (+ i 1)))) '()))

(define step8 (noise 1))

(display step8)
(display "\n")
(define templist (append step6 step8))

(define (border i)
  (if (<= i N) (if (member i templist) (append '() (border (+ i 1))) (append (list i) (border (+ i 1)))) '()))

(define step9 (border 1))

(display step9)
(display "\n")
(define (searchb ele li)
  
  (if (not(empty? li)) (if (= (car (car li)) ele) (car (cdr (car li))) (searchb ele (cdr li))) 0))
  

(define (findborder ele i val x)
  ;(write x)
  (if (<= i (length step6)) (if ( > (searchb ele (list-ref step4 (- (list-ref step6 (- i 1)) 1))) val)
                                (findborder ele (+ i 1) (searchb ele (list-ref step4 (- (list-ref step6 (- i 1)) 1))) (- (list-ref step6 (- i 1)) 1))
                                (findborder ele (+ i 1) val x)) x ))
                                                                                                             

(define (final i)
  (if (<= i (length step9)) (append (list (findborder (list-ref step9 (- i 1)) 1 0 0)) (final (+ i 1))) '()))

(define step10 (final 1))

(display step10)
(display "\n")