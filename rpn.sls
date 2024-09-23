(library (rpn)
  (export rpn
          rpnv
          dup
          swap
          skim
          to-top
          over
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

(define-stack-operation over
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
      (with-syntax ([(a) (generate-temporaries '(tmp))])
        #`(let ([a #,(car #'stack)]) (macro c #,(cons #'a (cons (cadr #'stack) (cons #'a (cddr #'stack)))) args ...)))
        (syntax-violation 'top "stack is empty, cant top" #'stack))])))

(define-stack-operation to-top
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
        #`(macro c #,(cons (car (reverse #'stack)) (reverse (cdr (reverse #'stack)))) args ...)
        (syntax-violation 'top "stack is empty, cant top" #'stack))])))

(define-stack-operation skim
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
        #`(macro c #,(list (car #'stack)) args ...)
        (syntax-violation 'skim "stack is empty, cant skim" #'stack))])))

(define-stack-operation dup
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
      (with-syntax ([(a) (generate-temporaries '(tmp))])
        #`(let ([a #,(car #'stack)]) (macro c #,(cons #'a (cons #'a (cdr #'stack))) args ...)))
        (syntax-violation 'dup "stack is empty, cant dup" #'stack))])))

(define-stack-operation swap
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack arg ...)
     (if (>= (length #'stack) 2)
        #`(macro c #,(cons (cadr #'stack) (cons (car #'stack) (cddr #'stack))) arg ...)
        (syntax-violation 'swap "stack is less than size 2, cant swap" #'stack))])))

(define-syntax mapply
  (lambda (stx)
    (syntax-case stx ()
      [(_ f (arg ...))
      #'(f arg ...)])))

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