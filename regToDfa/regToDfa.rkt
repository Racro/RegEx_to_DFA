#lang racket
(require parser-tools/lex
         parser-tools/yacc)
(require "declarations.rkt")
(require "utilities.rkt")

(define (buildNullable-dummy t)
  (cond [(Literal? t) #f]
        [(Then? t) (and (buildNullable-dummy (Then-t1 t)) (buildNullable-dummy (Then-t2 t)))]
        [(Or? t) (or (buildNullable-dummy (Or-t1 t)) (buildNullable-dummy (Or-t2 t)))]
        [(Star? t) #t]
        [else #t]))

(define (buildFirst-dummy t)
  (cond [(Literal? t) (list (Literal-n t))]
        [(Or? t) (append (buildFirst-dummy (Or-t1 t)) (buildFirst-dummy (Or-t2 t)))]
        [(Then? t) (if (buildNullable-dummy (Then-t1 t)) (append (buildFirst-dummy (Then-t1 t)) (buildFirst-dummy (Then-t2 t)))
                       (buildFirst-dummy (Then-t1 t)))]
        [(Star? t) (buildFirst-dummy (Star-t t))]
        [else (list )]))

(define (buildLast-dummy t)
  (cond [(Literal? t) (list (Literal-n t))]
        [(Or? t) (append (buildLast-dummy (Or-t1 t)) (buildLast-dummy (Or-t2 t)))]
        [(Then? t) (if (buildNullable-dummy (Then-t2 t)) (append (buildLast-dummy (Then-t1 t)) (buildLast-dummy (Then-t2 t)))
                       (buildLast-dummy (Then-t2 t)))]
        [(Star? t) (buildLast-dummy (Star-t t))]
        [else (list )]))

(define (buildNullable t)
  (cond [(Literal? t) (list (cons (Literal-n t) #f))]
        [(Then? t) (append (buildNullable (Then-t1 t)) (buildNullable (Then-t2 t))
                           (list (cons (Then-n t) (buildNullable-dummy t))))]
        [(Or? t) (append (buildNullable (Or-t1 t)) (buildNullable (Or-t2 t))
                         (list (cons (Or-n t) (buildNullable-dummy t))))]
        [(Star? t) (append (buildNullable (Star-t t)) (list (cons (Star-n t) #t)))]
        [else (list (cons (Epsilon-n t) #t))]))

(define (buildFirst t)
  (cond [(Literal? t) (list (cons (Literal-n t) (buildFirst-dummy t)))]
        [(Or? t) (append (buildFirst (Or-t1 t)) (buildFirst (Or-t2 t))
                         (list (cons (Or-n t) (buildFirst-dummy t))))]
        [(Then? t) (append (buildFirst (Then-t1 t)) (buildFirst (Then-t2 t))
                           (list (cons (Then-n t) (buildFirst-dummy t))))]
        [(Star? t) (append (buildFirst (Star-t t)) (list (cons (Star-n t) (buildFirst-dummy t))))]
        [else (list (cons (Epsilon-n t) (buildFirst-dummy t)))]))

(define (buildLast t)
  (cond [(Literal? t) (list (cons (Literal-n t) (buildLast-dummy t)))]
        [(Or? t) (append (buildLast (Or-t1 t)) (buildLast (Or-t2 t))
                         (list (cons (Or-n t) (buildLast-dummy t))))]
        [(Then? t) (append (buildLast (Then-t1 t)) (buildLast (Then-t2 t))
                           (list (cons (Then-n t) (buildLast-dummy t))))]
        [(Star? t) (append (buildLast (Star-t t)) (list (cons (Star-n t) (buildLast-dummy t))))]
        [else (list (cons (Epsilon-n t) (buildLast-dummy t)))]))
         
(define (buildFollow t)
  (define (helper l)
    (cond [(null? (cdr l)) (list )]
          (else (append (list (cons (Literal-n (car l)) (remove-duplicates (flatten (list (follow (Literal-n (car l)) t))))))
                        (helper (cdr l))))))
  (eliminate-single (helper (list-of-literals t))))

(define (follow n t)
  (cond [(or (Literal? t) (Epsilon? t)) (list )]
        [(Then? t) (let ([th1 (Then-t1 t)]
                         [th2 (Then-t2 t)])
                     (if (check-in-list n (buildLast-dummy th1))
                         (append (follow n th1) (follow n th2) (list (buildFirst-dummy th2)))
                         (append (follow n th1) (follow n th2))))]
        [(Star? t) (let ([st (Star-t t)])
                     (if (check-in-list n (buildLast-dummy st))
                         (append (follow n st) (follow n st) (list (buildFirst-dummy st)))
                         (follow n st)))]
        [(Or? t) (append (follow n (Or-t1 t)) (follow n (Or-t2 t)))]))

(define (flatten l)
  (cond [(null? l) (list )]
        [(list? (car l)) (append (flatten (car l)) (flatten (cdr l)))]
        (else (cons (car l) (flatten (cdr l))))))
        
(define (list-of-literals t)
  (cond [(Literal? t) (list t)]
        [(Or? t) (append (list-of-literals (Or-t1 t)) (list-of-literals (Or-t2 t)))]
        [(Then? t) (append (list-of-literals (Then-t1 t)) (list-of-literals (Then-t2 t)))]
        [(Star? t) (list-of-literals (Star-t t))]
        [else (list )]))

(define (green-nodes t)
  (buildFirst-dummy t))

(define (check-in-list x l)
  (cond [(null? l) #f]
        [(= (car l) x) #t]
        (else (check-in-list x (cdr l)))))

(define (symbols t)
  (define (helper tr)
    (cond [(Literal? tr) (list (Literal-c tr))]
          [(Then? tr) (append (helper (Then-t1 tr)) (helper (Then-t2 tr)))]
          [(Or? tr) (append (helper (Or-t1 tr)) (helper (Or-t2 tr)))]
          [(Star? tr) (helper (Star-t tr))]
          [else null]))
  (remove-duplicates (helper t)))

(define (filter l1 l2)
  (define (pass l3 l4) 
    (cond [(null? l4) (cons l3 (filter (cdr l1) l2))]
          [(equal? l3 (car l4)) (filter (cdr l1) l2)]
          (else (pass l3 (cdr l4)))))
  (cond [(null? l1) (list )]
        (else (pass (car l1) l2))))

(define (remove-duplicate l)
  (cond [(null? l) l]
        [(null? (cdr l)) l]
        [(equal? (car l) (car (cdr l))) (remove-duplicate (cdr l))]
        (else (cons (car l) (remove-duplicate (cdr l))))))

(define (buildGraph regexp)
  (define t (maketree regexp))
  (define (red-nodes l)
    (cond [(null? l) (list )]
          [(= (last (car l)) (Literal-n (Then-t2 t))) (cons (car l) (red-nodes (cdr l)))]
          (else (red-nodes (cdr l)))))

  (define LOL (list-of-literals t))

  (define (give-literal k)
    (define (literal-list l)
      (cond [(= k (Literal-n (car l))) (Literal-c (car l))]
            (else (literal-list (cdr l)))))
    (literal-list LOL))
  
  (define BF (buildFollow t))

  (define (followpos i t)
    (define (helper2 l)
      (cond [(null? l) (list )]
            [(= i (car (car l))) (cdr (car l))]
            (else (helper2 (cdr l)))))
    (helper2 BF))
  (define (identify-similar k l)
    (define (ans1 l1)
      (cond [(null? l1) (list )]
            [(equal? (give-literal k) (give-literal (car l1))) (append (followpos (car l1) t) (ans1 (cdr l1)))]
            (else (ans1 (cdr l1)))))
    (define (update-list l2)
      (cond [(null? l2) (list )]
            [(equal? (give-literal k) (give-literal (car l2))) (update-list (remove (car l2) l2))]
            (else (cons (car l2) (update-list (cdr l2))))))
    (cons (remove-duplicates (ans1 l)) (update-list l)))

  (define (form-trans l-ini l-new l-new-dummy ans)    ;ans='() , l-new-dummy='() , l-new= '()
    (define (update-ans l)
      (cond [(null? l) (form-trans (cdr l-ini) l-new l-new-dummy ans)]
            [(equal? (give-literal (car l)) "#") (update-ans (cdr l))]
            (else (let([a (identify-similar (car l) l)])
                    (begin (set! ans (cons (Trans (car l-ini) (give-literal (car l)) (car a))
                                           ans))
                           (set! l-new (cons (car a) l-new))
                           (set! l-new-dummy (cons (car l-ini) l-new-dummy))
                           (update-ans (cdr a)))))))
    (cond [(null? l-ini) (let([f (filter (remove-duplicate l-new) l-new-dummy)]
                              [rd (remove-duplicate l-new-dummy)])
                           (if (null? f)
                               (append (list rd) (list ans) (list (red-nodes rd)))
                               (form-trans f (list ) l-new-dummy ans)))]
          (else (if (equal? (give-literal (caar l-ini)) "#")
                    (begin (set! l-new-dummy (cons (car l-ini) l-new-dummy))
                           (update-ans (car l-ini)))
                    (update-ans (car l-ini))))))
  (define mix (form-trans (list (green-nodes t)) (list ) (list ) (list )))
  (Graph (green-nodes t) (car mix) (car (cdr mix)) (car (cdr (cdr mix)))  (symbols t)))

(define (eliminate-single l)
  (cond [(null? l) l]
        [(= (length (car l)) 1) (eliminate-single (cdr l))]
        (else (cons (car l) (eliminate-single (cdr l))))))
