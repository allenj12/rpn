# rpn
Reverse polish notation mini DSL for chezscheme. The stack, and its effects all take place at complie time and are disolved by runtime.

## Usage
Their are two main macros rpn and rpnv. They are exactly the same except rpn expects 1 final result in the stack, it is a compile time error to have 0 or more than 1 results. rpnv allows multiple results and and returns theme using schemes values fn for multiple value return.

## Examples
If both arguments for numbers are supplied for (* - / + expt) it is calculated at compile time
```
(expand '(rpn 1 2 +)) 
3
```
dup has the expected effect but also making sure calculations are not repeated
```
(expand '(rpn x y + dup +))
(let ([g17 (+ x y)]) (+ g17 g17))
```
Multiple values with rpnv
```
(expand '(rpnv x y + z ))
(values (+ x y) z)
```
However if you try multiple values with rpn you will get a compile time error
```
(expand '(rpn x y + z ))
Exception in rpn: stack effect is not 1 (pop (z (+ x y)))
```
If you want to call a function that is not a basic math operator, the number of arguments needed from the stack must be supplied
```
(define add3
    (lambda (x y z)
      (+ x y z)))
(expand '(rpn 1 2 3 (3 add3)))
(add3 1 2 3)
```
If your function returns multiple values you can specify how many values that are returned to be put back onto the stack
```
(define dup3
    (lambda (x)
      (values x x x)))
(rpn 3 (1 3 dup3) * *)
27
```
You can define your own stack operators such as dup
```
(define-stack-operation dup
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
      (with-syntax ([(a) (generate-temporaries '(tmp))])
        #`(let ([a #,(car #'stack)]) (macro c #,(cons #'a (cons #'a (cdr #'stack))) args ...)))
        (syntax-violation 'dup "stack is empty, cant dup" #'stack))])))
```
A slightly bit more involved example
```
(expand '(rpn '(1 2 3) dup (1 car) swap (1 cadr) dup * swap /))
(let ([g44 '(1 2 3)])
  (let ([g45 (cadr g44)])
    (/ (* g45 g45) (car g44))))
  ```
