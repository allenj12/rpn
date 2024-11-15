# rpn
Reverse polish notation mini DSL for chezscheme. The stack, and its effects all take place at complie time and are disolved by runtime.

## Usage
Their are a couple of main macros rpn, rpnv, rpnl, rpnlv, :, :v. The ones that do not end in 'v' will complie time error if it does not return a single value, it is a compile time error to have 0 or more than 1 results. The ones that end in 'v' allows multiple results and and returns them using schemes values fn for multiple value return.

## Examples
More in depth examples can be found at: https://github.com/allenj12/rpn-examples/tree/main

If both arguments for numbers are supplied for (* - / + expt) it is calculated at compile time
```
(expand '(rpn 1 2 +)) 
3
```
dup has the expected effect but also ensure calculations are not repeated
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
You can define your own stack operators such as dup that do the work at compile time
```
(define-stack-operation dup
  (lambda (old) (- old 1)) ;;stack effect after taking args
  (lambda (old new) (+ new 2)) ;;stack effect after spitting values out,old is useful to operate on the whole stack if need be
  (lambda (stx)
    (syntax-case stx ()
    [(macro c stack args ...)
     (if (not (null? #'stack))
      (with-syntax ([(a) (generate-temporaries '(tmp))])
        #`(let ([a #,(car #'stack)]) (macro c #,(cons #'a (cons #'a (cdr #'stack))) args ...)))
        (syntax-violation 'dup "stack is empty, can't dup" #'stack))])))
```
Its optional to implement a stack operation that takes in user supplied stack values
```
;;friendly to lang if implementation
(define-stack-operation rif
  ;; ... code removed for showing
    (syntax-case stx ()
    [(macro in _ c stack args ...) ;; in is also supplied here and also out as _ since its not used
     (if (>= (length #'stack) (syntax->datum #'in))
      (with-syntax ([co (caddr #'stack)]
                    [t (cadr #'stack)]
                    [f (car #'stack)]
                    [rest (take (cdddr #'stack) (- (syntax->datum #'in) 3))])
        #`(macro c #,(cons #'(mapply (if co t f) rest) (drop (cdddr #'stack) (- (syntax->datum #'in) 3))) args ...))
        (syntax-violation 'iff "stack is less than size specified input, can't iff" #'stack))]
```
A slightly bit more involved example
```
(expand '(rpn '(1 2 3) dup (1 car) swap (1 cadr) dup * swap /))
(let ([g44 '(1 2 3)])
  (let ([g45 (cadr g44)])
    (/ (* g45 g45) (car g44))))
  ```
rpnl and rpnlv are the lambda functions and can be evaled with 'ev'
```
(rpn 1 2 (rpnl +) (3 ev))
3
```
: and :v define a top level function for you, functions defined with : and :v do not need their stack effects made explicit when calling
```
(: x2 dup *)
(rpn 4 x2)
16
```
Here is a more involved example involving factorial
```
(: factorial dup 1 (2 =) (rpnl 1 *) (rpnl dup 1 - factorial *) (4 rif))
(factorial 5)
120
```
Sometimes you might want to override the stack effect that is calculated for you for a function. For example your function might take arguments that you dont move or process. You can explicitly state stack effects as so.
```
(: some-func (3 -- 2) +) ;; This function expects and argument at the bottom of the stack it will still return at the bottom of the stack.
```
Note that the DSL is not really friendly to functions with side-effects. The way around this for the time being is to call 'SE' on a function you only want for the side effects, internally this uses 'dup' to force a let binding and drops binding from the stack keeping the expression without having a void/null on the stack. Their are downsides to this, but should cover most cases.
