(library (rpn)
  (export rpn
          rpnv
          rpnl
          rpnlv
          :
          :v
          dup
          swap
          rot
          skim
          drop
          to-top
          to-bot
          tuck
          over
          double
          define-stack-operation
          rif
          ev
          dump
          p)
  (import (chezscheme))

(define key)

(define stack-in)
(define stack-out)

(define fn-in)
(define fn-out)

(define-syntax define-stack-operation
  (lambda (stx)
    (syntax-case stx ()
      [(_ name in out parser-expr)
       (identifier? #'name)
       #'(begin
           (define-syntax name
             (lambda (stx)
               (syntax-violation 'name "invalid use of keyword" stx)))
           (define-property name key
             (let ([parser parser-expr])
               (unless (procedure? parser)
                 (assertion-violation 'define-stack-operation "invalid parser" parser))
               parser))
           (define-property name stack-in
             (let ([stack-count-lambda in])
               (unless (procedure? stack-count-lambda)
                 (assertion-violation 'define-stack-operation "invalid stack count lambda" stack-count-lambda))
               stack-count-lambda))
           (define-property name stack-out
             (let ([stack-count-lambda out])
               (unless (procedure? stack-count-lambda)
                 (assertion-violation 'define-stack-operation "invalid stack count lambda" stack-count-lambda))
               stack-count-lambda)))])))

(define-syntax mapply
  (lambda (stx)
    (syntax-case stx ()
      [(_ f (arg ...))
      #'(f arg ...)])))

(define-syntax mlapply
  (lambda (stx)
    (syntax-case stx ()
      [(_ (a ...) (b b2 ...))
      #'(b a ... b2 ...)])))

(define-stack-operation double
  (lambda (old) 0)
  (lambda (old new) (* old 2))
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
      (with-syntax ([a (generate-temporaries #'stack)])
        #`(let-values ([a (mapply values stack)]) (macro c #,(append #'a #'a) args ...)))
        (syntax-violation 'tuck "stack is empty, can't double" #'stack))])))

(define-stack-operation tuck 
  (lambda (old) (- old 2))
  (lambda (old new) (+ new 3))
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (> (length #'stack) 2)
      (with-syntax ([(a) (generate-temporaries '(tmp))])
        #`(let ([a #,(car #'stack)]) (macro c #,(cons #'a (cons (cadr #'stack) (cons #'a (cddr #'stack)))) args ...)))
        (syntax-violation 'tuck "stack is less than size 2, can't top" #'stack))])))

(define-stack-operation over
  (lambda (old) (- old 2))
  (lambda (old new) (+ new 3))
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (> (length #'stack) 1)
      (with-syntax ([(a) (generate-temporaries '(tmp))])
        #`(let ([a #,(cadr #'stack)]) (macro c #,(cons #'a (cons (car #'stack) (cons #'a (cddr #'stack)))) args ...)))
        (syntax-violation 'over "stack is less than size 2, can't over" #'stack))])))

(define-stack-operation to-bot
  (lambda (old) old)
  (lambda (old new) new)
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
        #`(macro c #,(reverse (cons (car #'stack) (reverse (cdr #'stack)))) args ...)
        (syntax-violation 'to-bot "stack is empty, can't bot" #'stack))])))

(define-stack-operation to-top
  (lambda (old) old)
  (lambda (old new) new)
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
        #`(macro c #,(cons (car (reverse #'stack)) (reverse (cdr (reverse #'stack)))) args ...)
        (syntax-violation 'to-top "stack is empty, can't top" #'stack))])))

(define-stack-operation skim
  (lambda (old) 0)
  (lambda (old new) 1)
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
        #`(macro c #,(list (car #'stack)) args ...)
        (syntax-violation 'skim "stack is empty, can't skim" #'stack))])))

(define-stack-operation dup
  (lambda (old) (- old 1))
  (lambda (old new) (+ new 2))
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
      (with-syntax ([(a) (generate-temporaries '(tmp))])
        #`(let ([a #,(car #'stack)]) (macro c #,(cons #'a (cons #'a (cdr #'stack))) args ...)))
        (syntax-violation 'dup "stack is empty, can't dup" #'stack))])))

(define-stack-operation swap
  (lambda (old) (- old 2))
  (lambda (old new) (+ new 2))
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack arg ...)
     (if (>= (length #'stack) 2)
        #`(macro c #,(cons (cadr #'stack) (cons (car #'stack) (cddr #'stack))) arg ...)
        (syntax-violation 'swap "stack is less than size 2, cant swap" #'stack))])))

(define-stack-operation drop
  (lambda (old) (- old 1))
  (lambda (old new) new)
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
        #`(macro c #,(cdr #'stack) args ...)
        (syntax-violation 'drop "stack is empty, can't drop" #'stack))])))

(define-stack-operation rot
  (lambda (old) (- old 3))
  (lambda (old new) (+ new 3))
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (>= (length #'stack) 3)
      (with-syntax ([cc (caddr #'stack)]
                    [b (cadr #'stack)]
                    [a (car #'stack)])
        #`(macro c #,(cons #'cc (cons #'a (cons #'b (cdddr #'stack)))) args ...))
        (syntax-violation 'rot "stack is less than size 3 can't rot" #'stack))])))

(define-stack-operation rif
  (lambda (old) (- old 3))
  (lambda (old new) (+ new 1))
  (lambda (stx)
    (define take
        (lambda (lst n)
          (let loop ((result '()) (i n) (lst lst))
            (if (or (null? lst) (<= i 0))
                (reverse result)
                (loop (cons (car lst) result) (- i 1) (cdr lst))))))
    (define drop
      (lambda (lis k)
        (let iter ((lis lis) (k k))
          (if (zero? k) lis (iter (cdr lis) (- k 1))))))
    (syntax-case stx ()
    [(macro in _ c stack args ...)
     (if (>= (length #'stack) (syntax->datum #'in))
      (with-syntax ([co (caddr #'stack)]
                    [t (cadr #'stack)]
                    [f (car #'stack)]
                    [rest (take (cdddr #'stack) (- (syntax->datum #'in) 3))])
        #`(macro c #,(cons #'(mapply (if co t f) rest) (drop (cdddr #'stack) (- (syntax->datum #'in) 3))) args ...))
        (syntax-violation 'iff "stack is less than size specified input, can't iff" #'stack))]
    [(macro c stack args ...)
     (if (>= (length #'stack) 3)
      (with-syntax ([co (caddr #'stack)]
                    [t (cadr #'stack)]
                    [f (car #'stack)])
        #`(macro c #,(cons #'(if co t f) (cdddr #'stack)) args ...))
        (syntax-violation 'iff "stack is less than size 3, can't iff" #'stack))])))

(define-stack-operation ev
  (lambda (old) (- old 1))
  (lambda (old new) (+ new 1))
  (lambda (stx)
    (define take
        (lambda (lst n)
          (let loop ((result '()) (i n) (lst lst))
            (if (or (null? lst) (<= i 0))
                (reverse result)
                (loop (cons (car lst) result) (- i 1) (cdr lst))))))
    (define drop
      (lambda (lis k)
        (let iter ((lis lis) (k k))
          (if (zero? k) lis (iter (cdr lis) (- k 1))))))
    (syntax-case stx ()
    [(macro in _ c stack args ...)
     (if (>= (length #'stack) (syntax->datum #'in))
      (with-syntax ([f (take #'stack (syntax->datum #'in))]
                    [rest (drop #'stack (syntax->datum #'in))])
        #`(macro c #,(cons #'f #'rest) args ...))
        (syntax-violation 'iff "stack is less than size specified input, can't iff" #'stack))]
    [(macro c stack args ...)
     (if (>= (length #'stack) 1)
      (with-syntax ([fn (car #'stack)])
        #`(macro c #,(cons #'(fn) (cdr #'stack)) args ...))
        (syntax-violation 'ev "stack is less than size 1, can't ev" #'stack))])))

(define-syntax rpn-backend
  (lambda (stx)
    (lambda (lookup)
      (define make-n
        (lambda (n)
          (let loop ([result '()]
                    [n n])
            (if (<= n 0)
              result
              (loop (cons 'tmp result) (sub1 n))))))
      (define take
        (lambda (lst n)
          (let loop ((result '()) (i n) (lst lst))
            (if (or (null? lst) (<= i 0))
                (reverse result)
                (loop (cons (car lst) result) (- i 1) (cdr lst))))))
      (define drop
        (lambda (lis k)
        (let iter ((lis lis) (k k))
          (if (zero? k) lis (iter (cdr lis) (- k 1))))))
      (syntax-case stx ()
        [(_ c (st ...))
         #'(c (st ...))]
        [(_ c (st ...) fn arg ...)
         (and (identifier? #'fn)
              (number? (lookup #'fn #'fn-in))
              (number? (lookup #'fn #'fn-out)))
         #`(rpn-backend c (st ...) (#,(lookup #'fn #'fn-in) #,(lookup #'fn #'fn-out) fn) arg ...)]
        [(_ c (st ...) so arg ...)
         (and (identifier? #'so)
              (procedure? (lookup #'so #'key)))
         (let ([parser (lookup #'so #'key)])
            (parser #'(rpn-backend c (st ...) arg ...)))]
        [(_ c (st ...) (num so) arg ...)
         (and (identifier? #'so)
              (number? (syntax->datum #'num))
              (procedure? (lookup #'so #'key)))
         (let ([parser (lookup #'so #'key)])
            (parser #'(rpn-backend num #f c (st ...) arg ...)))]
        [(_ c (st ...) (num out so) arg ...)
         (and (identifier? #'so)
              (number? (syntax->datum #'num))
              (number? (syntax->datum #'out))
              (procedure? (lookup #'so #'key)))
         (let ([parser (lookup #'so #'key)])
            (parser #'(rpn-backend num out c (st ...) arg ...)))]
        [(_ c (st ...) (num fn) arg ...)
          (integer? (syntax->datum #'num))
          (if (>= (length #'(st ...)) (syntax->datum #'num))
            #`(rpn-backend c #,(drop #'(st ...) (syntax->datum #'num))
                           (mapply fn #,(reverse (take #'(st ...) (syntax->datum #'num)))) arg ...)
            (syntax-violation 'rpn-backend "stack insufficient size" #'(st ...)))]
        [(_ c (st ...) (num out fn) arg ...)
         (and (integer? (syntax->datum #'num))
              (integer? (syntax->datum #'out)))
         (if (>= (length #'(st ...)) (syntax->datum #'num))
           (if (= 1 (syntax->datum #'out))
             #`(rpn-backend c (st ...) (num fn) arg ...)
             (with-syntax ([tmp (generate-temporaries (make-n (syntax->datum #'out)))])
             #`(let-values ([tmp (mapply fn #,(reverse (take #'(st ...) (syntax->datum #'num))))])
                 (rpn-backend c #,(append (reverse #'tmp) (drop #'(st ...) (syntax->datum #'num))) arg ...))))
           (syntax-violation 'rpn-backend "stack insufficient size" #'(st ...)))]
        [(_ c (st ...) sym arg* ...)
         (find (lambda (x) (eq? (syntax->datum #'sym) x)) '(+ - * / expt))
         (if (>= (length #'(st ...)) 2)
           (with-syntax ([f (car #'(st ...))]
                         [s (cadr #'(st ...))]
                         [rest (cddr #'(st ...))])
                 (if (and (number? (syntax->datum #'f))
                          (number? (syntax->datum #'s)))
                   #`(rpn-backend c #,(cons (eval (syntax->datum #'(sym s f))) #'rest) arg* ...)
                   #`(rpn-backend c #,(cons #'(sym s f) #'rest) arg* ...)))
           (syntax-violation 'rpn-backend "stack insufficient size" #'(st ...)))]
        [(_ c (st ...) x arg* ...)
         #`(rpn-backend c #,(cons #'x #'(st ...)) arg* ...)]))))

(define-syntax pop
  (lambda (stx)
    (syntax-case stx ()
     [(_ (result))
      #'result]
      [stack
       (syntax-violation 'rpn "stack effect is not 1" #'stack)])))

(define-syntax values-apply
  (lambda (stx)
    (syntax-case stx ()
     ;[(_ ())
     ; (syntax-violation 'rpn "stack effect is not 1" #'stack)]
      [(_ (result ...))
       #`(mapply values #,(reverse #'(result ...)))])))

(define-syntax rpnv
  (lambda (stx)
    (syntax-case stx ()
      [(_ arg ...)
       #'(rpn-backend values-apply () arg ...)])))

(define-syntax rpn
  (lambda (stx)
    (syntax-case stx ()
      [(_ arg ...)
       #'(rpn-backend pop () arg ...)])))

(define-syntax arg-count
  (lambda (stx)
    (lambda (lookup)
      (define make-n
          (lambda (n)
            (let loop ([result '()]
                      [n n])
              (if (<= n 0)
                result
                (loop (cons 'tmp result) (sub1 n))))))
      (syntax-case stx ()
      [(_ i f p c)
       (identifier? #'i)
       #`(begin
          (define i (lambda p (mlapply p f)))
          (define-property i fn-in
             (let ([in-num #,(length (syntax->datum #'p))])
               (unless (number? in-num)
                 (assertion-violation 'rpn-fn "invalid in stack effect" in-num))
               in-num))
          (define-property i fn-out
             (let ([out-num (syntax->datum #'c)])
               (unless (number? out-num)
                 (assertion-violation 'rpn-fn "invalid out stack effect" out-num))
               out-num)))]
      [(_ i f p c)
       (syntax->datum #'i)
       #`(#,(length (syntax->datum #'p)) c (mapply f p))]
      [(_ i f p c)
       (not (syntax->datum #'i))
       #`(lambda p (mlapply p f))]
      [(_ i f p c a arg ...)
       (and (identifier? #'a)
            (number? (lookup #'a #'fn-in))
            (number? (lookup #'a #'fn-out)))
        (let* ([in (lookup #'a #'fn-in)]
               [out (lookup #'a #'fn-out)]
               [pc (- (syntax->datum #'c) in)])
          (with-syntax ([new-p (if (< pc 0) (append (generate-temporaries (make-n (- 0 pc))) #'p) #'p)]
                        [new-c (if (< pc 0) out (+ out pc))])
            #`(arg-count i f new-p new-c arg ...)))]
      [(_ i f p c a arg ...)
       (and (identifier? #'a)
            (procedure? (lookup #'a #'stack-in))
            (procedure? (lookup #'a #'stack-out)))
        (let* ([in (lookup #'a #'stack-in)]
               [out (lookup #'a #'stack-out)]
               [pc (in (syntax->datum #'c))])
          (with-syntax ([new-p (if (< pc 0) (append (generate-temporaries (make-n (- 0 pc))) #'p) #'p)]
                        [new-c (if (< pc 0) (out (syntax->datum #'c) 0) (out (syntax->datum #'c) pc))])
            #`(arg-count i f new-p new-c arg ...)))]
      [(_ i f p c (num fn) arg ...)
        (integer? (syntax->datum #'num))
        (let ([pc (eval (syntax->datum #'(- c num)))])
          (with-syntax ([new-p (if (< pc 0) (append (generate-temporaries (make-n (- 0 pc))) #'p) #'p)]
                        [new-c (if (< pc 0) #'1 (+ 1 pc))])
            #`(arg-count i f new-p new-c arg ...)))]
      [(_ i f p c (num out fn) arg ...)
       (and (integer? (syntax->datum #'num))
            (integer? (syntax->datum #'out)))
       (let ([pc (eval (syntax->datum #'(- c num)))])
          (with-syntax ([new-p (if (< pc 0) (append (generate-temporaries (make-n (- 0 pc))) #'p) #'p)]
                        [new-c (if (< pc 0) #'out (+ pc (syntax->datum #'out)))])
            #`(arg-count i f new-p new-c arg ...)))]
      [(_ i f p c sym arg ...)
       (find (lambda (x) (eq? (syntax->datum #'sym) x)) '(+ - * / expt))
       (let ([pc (eval (syntax->datum #'(- c 2)))])
          (with-syntax ([new-p (if (< pc 0) (append (generate-temporaries (make-n (- 0 pc))) #'p) #'p)]
                        [new-c (if (< pc 0) #'1 (+ 1 pc))])
            #`(arg-count i f new-p new-c arg ...)))]
      [(_ i f p c a arg ...)
        #`(arg-count i f p #,(eval (syntax->datum #'(+ c 1))) arg ...)]))))

(define-syntax rpnl
  (lambda (stx)
    (syntax-case stx ()
    [(_ arg ...)
    #`(arg-count #f (rpn arg ...) () 0 arg ...)])))

(define-syntax rpnlv
  (lambda (stx)
    (syntax-case stx ()
    [(_ arg ...)
    #`(arg-count #f (rpnv arg ...) () 0 arg ...)])))
    
(define-syntax :
  (lambda (stx)
    (syntax-case stx ()
     [(_ name arg ...)
      #'(arg-count name (rpn arg ...) () 0 arg ...)])))
      
(define-syntax :v
  (lambda (stx)
    (syntax-case stx ()
     [(_ name arg ...)
      #'(arg-count name (rpnv arg ...) () 0 arg ...)])))

(define dump
  (lambda (l) (apply values l)))

(define print-forward (lambda (x) (display x) (newline) x))

(: p (1 1 print-forward)))
