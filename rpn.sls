(library (rpn)
  (export rpn
          rpnv
          dup
          swap
          skim
          to-top
          to-bot
          tuck
          over
          double
          define-stack-operation)
  (import (chezscheme))

(define key)

(define-syntax define-stack-operation
  (lambda (stx)
    (syntax-case stx ()
      [(_ name parser-expr)
       (identifier? #'name)
       #'(begin
           (define-syntax name
             (lambda (stx)
               (syntax-violation 'name "invalid use of keyword" stx)))
           (define-property name key
             (let ([parser parser-expr])
               (unless (procedure? parser)
                 (assertion-violation 'define-stack-operation "invalid parser" parser))
               parser)))])))

(define-syntax mapply
  (lambda (stx)
    (syntax-case stx ()
      [(_ f (arg ...))
      #'(f arg ...)])))

(define-stack-operation double
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
      (with-syntax ([a (generate-temporaries #'stack)])
        #`(let-values ([a (mapply values stack)]) (macro c #,(append #'a #'a) args ...)))
        (syntax-violation 'tuck "stack is empty, can't double" #'stack))])))

(define-stack-operation tuck
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (> (length #'stack) 1)
      (with-syntax ([(a) (generate-temporaries '(tmp))])
        #`(let ([a #,(car #'stack)]) (macro c #,(cons #'a (cons (cadr #'stack) (cons #'a (cddr #'stack)))) args ...)))
        (syntax-violation 'tuck "stack is empty, can't top" #'stack))])))

(define-stack-operation over
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (> (length #'stack) 1)
      (with-syntax ([(a) (generate-temporaries '(tmp))])
        #`(let ([a #,(cadr #'stack)]) (macro c #,(cons #'a #'stack) args ...)))
        (syntax-violation 'over "stack is less than size 2, can't over" #'stack))])))

(define-stack-operation to-bot
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
        #`(macro c #,(reverse (cons (car #'stack) (reverse (cdr #'stack)))) args ...)
        (syntax-violation 'to-bot "stack is empty, can't bot" #'stack))])))

(define-stack-operation to-top
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
        #`(macro c #,(cons (car (reverse #'stack)) (reverse (cdr (reverse #'stack)))) args ...)
        (syntax-violation 'to-top "stack is empty, can't top" #'stack))])))

(define-stack-operation skim
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
        #`(macro c #,(list (car #'stack)) args ...)
        (syntax-violation 'skim "stack is empty, can't skim" #'stack))])))

(define-stack-operation dup
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
      (with-syntax ([(a) (generate-temporaries '(tmp))])
        #`(let ([a #,(car #'stack)]) (macro c #,(cons #'a (cons #'a (cdr #'stack))) args ...)))
        (syntax-violation 'dup "stack is empty, can't dup" #'stack))])))

(define-stack-operation swap
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack arg ...)
     (if (>= (length #'stack) 2)
        #`(macro c #,(cons (cadr #'stack) (cons (car #'stack) (cddr #'stack))) arg ...)
        (syntax-violation 'swap "stack is less than size 2, cant swap" #'stack))])))

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
        [(_ c (st ...) so arg ...)
         (and (identifier? #'so)
              (procedure? (lookup #'so #'key)))
         (let ([parser (lookup #'so #'key)])
            (parser #'(rpn-backend c (st ...) arg ...)))]
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
          (with-syntax ([tmp (generate-temporaries (make-n (syntax->datum #'out)))])
          #`(let-values ([tmp (mapply fn #,(reverse (take #'(st ...) (syntax->datum #'num))))])
              (rpn-backend c #,(append (reverse #'tmp) (drop #'(st ...) (syntax->datum #'num))) arg ...)))
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
     [(_ ())
      (syntax-violation 'rpn "stack effect is not 1" #'stack)]
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
       #'(rpn-backend pop () arg ...)]))))
