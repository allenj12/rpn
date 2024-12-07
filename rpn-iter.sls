#!chezscheme
(library (rpn-iter)
    (export repeat1
            repeat2
            repeat3
            repeat4
            iter1
            iter2
            iter3
            iter4
            fold1
            fold2
            fold3
            fold4
            find1
            find2
            find3
            find4)
    (import (rpn)
            (rpn-extended-base)
            (chezscheme))

(: repeat1 dup 0 {2 <=}
                (rot skim)
                (1 {2 fx-} (({2 ev}) 2k1) 2u2 repeat1)
                3rif)

(: repeat2 dup 0 {2 <=}
                ({4 2} drop drop)
                (1 {2 fx-} (({3 2 ev}) 3k2) 3u3 repeat2)
                {7 2 rif})

(: repeat3 dup 0 {2 <=}
                ({5 3} drop drop)
                (1 {2 fx-} (({4 3 ev}) 4k3) 4u4 repeat3)
                {8 3 rif})

(: repeat4
  {6 4 (lambda (a1 a2 a3 a4 l c)
        (if (<= c 0)
          (values a1 a2 a3 a4)
          (let-values ([(o t th f) (l a1 a2 a3 a4)])
            (repeat4 o t th f l (fx- c 1)))))})

(: iter1 1 (2dup {2 <}
                ({5 1} drop drop drop swap drop)
                ({5 1} dup 1 {2 fx+} {5 5 rot} rot {5 5 rot} dup {6 6 rrot} {3 ev}
                               {4 4 rrot} {5 5 rot} dup {6 6 rrot} {6 ev})
                {8 rif})
               dup {6 6 rrot} {6 ev})

(: iter2 1 (2dup {2 <}
              ({6 2} drop drop drop rot drop)
              ({6 2} dup 1 {2 fx+} {6 6 rot} {6 6 rot} {4 4 rot} {6 6 rot} dup {7 7 rrot} {4 2 ev}
                              {5 5 rrot} {5 5 rrot} {6 6 rot} dup {7 7 rrot} {7 2 ev})
              {9 2 rif})
            dup {7 7 rrot} {7 2 ev})

(: iter3 1 (2dup {2 <}
              ({7 3} drop drop drop {4 4 rot} drop)
              ({7 3} dup 1 {2 fx+} {7 7 rot} {7 7 rot} {7 7 rot} {5 5 rot} {7 7 rot} dup {8 8 rrot} {5 3 ev}
                              {6 6 rot} {6 6 rot} {6 6 rot} {7 7 rot} dup {8 8 rrot} {8 3 ev})
              {10 3 rif})
            dup {8 8 rrot} {8 3 ev})

(: iter4 1 (2dup {2 <}
              ({8 4} drop drop drop {5 5 rot} drop)
              ({8 4} dup 1 {2 fx+} {8 8 rot} {8 8 rot} {8 8 rot} {8 8 rot} {6 6 rot} {8 8 rot} dup {9 9 rrot} {6 4 ev}
                              {7 7 rot} {7 7 rot} {7 7 rot} {8 8 rot} dup {9 9 rrot} {9 4 ev})
              {11 4 rif})
            dup {9 9 rrot} {9 4 ev})

(: fold1 dup {1 null?}
            ({3 1} drop drop)
            (rsplit {4 4 rrot} swap dup {4 4 rrot} {3 ev}
                        swap rot fold1)
            {6 rif})

(: fold2 dup {1 null?}
            ({4 2} drop drop)
            (rsplit {5 5 rrot} swap dup {5 5 rrot} {4 2 ev}
                        rot {4 4 rot} fold2)
            {7 2 rif})
            
(: fold3 dup {1 null?}
            ({5 3} drop drop)
            (rsplit {6 6 rrot} swap dup {6 6 rrot} {5 3 ev}
                        {4 4 rot} {5 5 rot} fold3)
            {8 3 rif})
            
(: fold4 dup {1 null?}
            ({6 4} drop drop)
            (rsplit {7 7 rrot} swap dup {7 7 rrot} {6 4 ev}
                        {5 5 rot} {6 6 rot} fold4)
            {9 4 rif})

(: find1 {3 1 (lambda (arg fn li)
                  (call/cc
                    (lambda (c)
                      (let loo ([a arg]
                                [f fn]
                                [l li])
                        (if (null? l)
                            (c a)
                            (let ([ret (f c a (car l))])
                              (loo ret f (cdr l))))))))})
  
(: find2 {4 2 (lambda (arg1 arg2 fn li)
                  (call/cc
                    (lambda (c)
                      (let loo ([a1 arg1]
                                [a2 arg2]
                                [f fn]
                                [l li])
                        (if (null? l)
                            (c a1 a2)
                            (let-values ([(na1 na2) (f c a1 a2 (car l))])
                              (loo na1 na2 f (cdr l))))))))})
                            
(: find3 {5 3 (lambda (arg1 arg2 arg3 fn li)
                  (call/cc
                    (lambda (c)
                      (let loo ([a1 arg1]
                                [a2 arg2]
                                [a3 arg3]
                                [f fn]
                                [l li])
                        (if (null? l)
                            (c a1 a2 a3)
                            (let-values ([(na1 na2 na3) (f c a1 a2 a3 (car l))])
                              (loo na1 na2 na3 f (cdr l))))))))})
                            
(: find4 {6 4 (lambda (arg1 arg2 arg3 arg4 fn li)
                  (call/cc
                    (lambda (c)
                      (let loo ([a1 arg1]
                                [a2 arg2]
                                [a3 arg3]
                                [a4 arg4]
                                [f fn]
                                [l li])
                        (if (null? l)
                            (c a1 a2 a3 a4)
                            (let-values ([(na1 na2 na3 na4) (f c a1 a2 a3 a4 (car l))])
                              (loo na1 na2 na3 na4 f (cdr l))))))))}))