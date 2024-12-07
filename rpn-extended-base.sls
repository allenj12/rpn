#!chezscheme
(library (rpn-extended-base)
  (export split
          rsplit
          2dup
          3dup
          4dup
          dup2
          dup3
          dup4
          4rot
          5rot
          6rot
          7rot
          8rot
          9rot
          4rrot
          5rrot
          6rrot
          7rrot
          8rrot
          9rrot
          nip
          2nip
          nip3
          nip4
          nip5
          trim
          1rif
          2rif
          3rif
          4rif
          5rif
          6rif
          1pand
          2pand
          3pand
          4pand
          1por
          2por
          3por
          4por
          1u1
          1u2
          1u3
          1u4
          2u1
          2u2
          2u3
          2u4
          3u1
          3u2
          3u3
          3u4
          4u1
          4u2
          4u3
          4u4
          1uu1
          1uu2
          1uu3
          1uu4
          2uu1
          2uu2
          2uu3
          2uu4
          3uu1
          3uu2
          3uu3
          3uu4
          4uu1
          4uu2
          4uu3
          4uu4
          1uuu1
          1uuu2
          1uuu3
          1uuu4
          2uuu1
          2uuu2
          2uuu3
          2uuu4
          3uuu1
          3uuu2
          3uuu3
          3uuu4
          4uuu1
          4uuu2
          4uuu3
          4uuu4
          1kkk1
          1kkk2
          1kkk3
          1kkk4
          2kkk1
          2kkk2
          2kkk3
          2kkk4
          3kkk1
          3kkk2
          3kkk3
          3kkk4
          4kkk1
          4kkk2
          4kkk3
          4kkk4
          1k1
          1k2
          1k3
          1k4
          2k1
          2k2
          2k3
          2k4
          3k1
          3k2
          3k3
          3k4
          4k1
          4k2
          4k3
          4k4
          1kk1
          1kk2
          1kk3
          1kk4
          2kk1
          2kk2
          2kk3
          2kk4
          3kk1
          3kk2
          3kk3
          3kk4
          4kk1
          4kk2
          4kk3
          4kk4
          2uku1
          2uku2
          3uku1
          3uku2
          2kuk1
          2kuk2
          3kuk1
          3kuk2)
  (import (chezscheme)
          (rpn))

(: split dup {1 cdr} swap {1 car})
(: rsplit dup {1 car} swap {1 cdr})

(: 2dup over over)
(: 3dup {3 6} double)
(: 4dup {4 8} double)

(: 4rot {4 4 rot})
(: 5rot {5 5 rot})
(: 6rot {6 6 rot})
(: 7rot {7 7 rot})
(: 8rot {8 8 rot})
(: 9rot {9 9 rot})

(: 4rrot {4 4 rrot})
(: 5rrot {5 5 rrot})
(: 6rrot {6 6 rrot})
(: 7rrot {7 7 rrot})
(: 8rrot {8 8 rrot})
(: 9rrot {9 9 rrot})

(: dup2 over swap)
(: dup3 rot dup 4rrot 4rrot)
(: dup4 4rot dup 5rrot 5rrot)

(: nip swap drop)
(: 2nip rot drop rot drop)
(: nip3 rot drop)
(: nip4 {4 4 rot} drop)
(: nip5 {5 5 rot} drop)

(: trim drop swap drop)

(: 1rif {4 rif})
(: 2rif {5 rif})
(: 3rif {6 rif})
(: 4rif {7 rif})
(: 5rif {8 rif})
(: 6rif {9 rif})

(: 1pand
    ({1 1} drop #f)
    1rif)

(: 2pand
    ({2 1} #f skim)
    2rif)

(: 3pand
    ({3 1} #f skim)
    3rif)
    
(: 4pand
    ({4 1} #f skim)
    4rif)

(: 1por swap dup
    ({2 1} swap skim) curry
    rot
    1rif)

(: 2por swap dup
    ({3 1} rot skim) curry
    rot
    2rif)
    
(: 3por swap dup
    ({4 1} 4rot skim) curry
    rot
    3rif)

(: 4por swap dup
    ({5 1} 5rot skim) curry
    rot
    4rif)
    
(: 1u1 {3 2} swap to-bot {2 ev} to-top)
(: 1u2 {3 3} swap to-bot {2 2 ev} to-top)
(: 1u3 {3 4} swap to-bot {2 3 ev} to-top)
(: 1u4 {3 3} swap to-bot {2 4 ev} to-top)

(: 2u1 {4 2} swap to-bot {3 ev} to-top)
(: 2u2 {4 3} swap to-bot {3 2 ev} to-top)
(: 2u3 {4 4} swap to-bot {3 3 ev} to-top)
(: 2u4 {4 5} swap to-bot {3 4 ev} to-top)

(: 3u1 {5 2} swap to-bot {4 ev} to-top)
(: 3u2 {5 3} swap to-bot {4 2 ev} to-top)
(: 3u3 {5 4} swap to-bot {4 3 ev} to-top)
(: 3u4 {5 5} swap to-bot {4 4 ev} to-top)

(: 4u1 {6 2} swap to-bot {5 ev} to-top)
(: 4u2 {6 3} swap to-bot {5 2 ev} to-top)
(: 4u3 {6 4} swap to-bot {5 3 ev} to-top)
(: 4u4 {6 5} swap to-bot {5 4 ev} to-top)

(: 1uu1 {4 3} rrot to-bot to-bot {2 ev} to-top to-top)
(: 1uu2 {4 4} rrot to-bot to-bot {2 2 ev} to-top to-top)
(: 1uu3 {4 5} rrot to-bot to-bot {2 3 ev} to-top to-top)
(: 1uu4 {4 6} rrot to-bot to-bot {2 4 ev} to-top to-top)

(: 2uu1 {5 3} rrot to-bot to-bot {3 ev} to-top to-top)
(: 2uu2 {5 4} rrot to-bot to-bot {3 2 ev} to-top to-top)
(: 2uu3 {5 5} rrot to-bot to-bot {3 3 ev} to-top to-top)
(: 2uu4 {5 6} rrot to-bot to-bot {3 4 ev} to-top to-top)

(: 3uu1 {6 3} rrot to-bot to-bot {4 ev} to-top to-top)
(: 3uu2 {6 4} rrot to-bot to-bot {4 2 ev} to-top to-top)
(: 3uu3 {6 5} rrot to-bot to-bot {4 3 ev} to-top to-top)
(: 3uu4 {6 6} rrot to-bot to-bot {4 4 ev} to-top to-top)

(: 4uu1 {7 3} rrot to-bot to-bot {5 ev} to-top to-top)
(: 4uu2 {7 4} rrot to-bot to-bot {5 2 ev} to-top to-top)
(: 4uu3 {7 5} rrot to-bot to-bot {5 3 ev} to-top to-top)
(: 4uu4 {7 6} rrot to-bot to-bot {5 4 ev} to-top to-top)

(: 1uuu1 {5 4} 4rrot to-bot to-bot to-bot {2 ev} to-top to-top to-top)
(: 1uuu2 {5 5} 4rrot to-bot to-bot to-bot {2 2 ev} to-top to-top to-top)
(: 1uuu3 {5 6} 4rrot to-bot to-bot to-bot {2 3 ev} to-top to-top to-top)
(: 1uuu4 {5 7} 4rrot to-bot to-bot to-bot {2 4 ev} to-top to-top to-top)

(: 2uuu1 {6 4} 4rrot to-bot to-bot to-bot {3 ev} to-top to-top to-top)
(: 2uuu2 {6 5} 4rrot to-bot to-bot to-bot {3 2 ev} to-top to-top to-top)
(: 2uuu3 {6 6} 4rrot to-bot to-bot to-bot {3 3 ev} to-top to-top to-top)
(: 2uuu4 {6 7} 4rrot to-bot to-bot to-bot {3 4 ev} to-top to-top to-top)

(: 3uuu1 {7 4} 4rrot to-bot to-bot to-bot {4 ev} to-top to-top to-top)
(: 3uuu2 {7 5} 4rrot to-bot to-bot to-bot {4 2 ev} to-top to-top to-top)
(: 3uuu3 {7 6} 4rrot to-bot to-bot to-bot {4 3 ev} to-top to-top to-top)
(: 3uuu4 {7 7} 4rrot to-bot to-bot to-bot {4 4 ev} to-top to-top to-top)

(: 4uuu1 {8 4} 4rrot to-bot to-bot to-bot {5 ev} to-top to-top to-top)
(: 4uuu2 {8 5} 4rrot to-bot to-bot to-bot {5 2 ev} to-top to-top to-top)
(: 4uuu3 {8 6} 4rrot to-bot to-bot to-bot {5 3 ev} to-top to-top to-top)
(: 4uuu4 {8 7} 4rrot to-bot to-bot to-bot {5 4 ev} to-top to-top to-top)

(: 1k1 {2 2} over to-bot {2 ev} to-top)
(: 1k2 {2 3} over to-bot {2 2 ev} to-top)
(: 1k3 {2 4} over to-bot {2 3 ev} to-top)
(: 1k4 {2 5} over to-bot {2 4 ev} to-top)

(: 2k1 {3 2} over to-bot {3 ev} to-top)
(: 2k2 {3 3} over to-bot {3 2 ev} to-top)
(: 2k3 {3 4} over to-bot {3 3 ev} to-top)
(: 2k4 {3 5} over to-bot {3 4 ev} to-top)

(: 3k1 {4 2} over to-bot {4 ev} to-top)
(: 3k2 {4 3} over to-bot {4 2 ev} to-top)
(: 3k3 {4 4} over to-bot {4 3 ev} to-top)
(: 3k4 {4 5} over to-bot {4 4 ev} to-top)

(: 4k1 {5 2} over to-bot {5 ev} to-top)
(: 4k2 {5 3} over to-bot {5 2 ev} to-top)
(: 4k3 {5 4} over to-bot {5 3 ev} to-top)
(: 4k4 {5 5} over to-bot {5 4 ev} to-top)

(: 1kk1 {3 4} rrot 2dup to-bot to-bot rot {2 ev} to-top to-top)
(: 1kk2 {3 5} rrot 2dup to-bot to-bot rot {2 2 ev} to-top to-top)
(: 1kk3 {3 6} rrot 2dup to-bot to-bot rot {2 3 ev} to-top to-top)
(: 1kk4 {3 7} rrot 2dup to-bot to-bot rot {2 4 ev} to-top to-top)

(: 2kk1 {3 3} rrot 2dup to-bot to-bot rot {3 ev} to-top to-top)
(: 2kk2 {3 4} rrot 2dup to-bot to-bot rot {3 2 ev} to-top to-top)
(: 2kk3 {3 5} rrot 2dup to-bot to-bot rot {3 3 ev} to-top to-top)
(: 2kk4 {3 6} rrot 2dup to-bot to-bot rot {3 4 ev} to-top to-top)

(: 3kk1 {4 3} rrot 2dup to-bot to-bot rot {4 ev} to-top to-top)
(: 3kk2 {4 4} rrot 2dup to-bot to-bot rot {4 2 ev} to-top to-top)
(: 3kk3 {4 5} rrot 2dup to-bot to-bot rot {4 3 ev} to-top to-top)
(: 3kk4 {4 6} rrot 2dup to-bot to-bot rot {4 4 ev} to-top to-top)

(: 4kk1 {5 3} rrot 2dup to-bot to-bot rot {5 ev} to-top to-top)
(: 4kk2 {5 4} rrot 2dup to-bot to-bot rot {5 2 ev} to-top to-top)
(: 4kk3 {5 5} rrot 2dup to-bot to-bot rot {5 3 ev} to-top to-top)
(: 4kk4 {5 6} rrot 2dup to-bot to-bot rot {5 4 ev} to-top to-top)

(: 1kkk1 {4 6} 4rrot 3dup to-bot to-bot to-bot 4rot {2 ev} to-top to-top to-top)
(: 1kkk2 {4 7} 4rrot 3dup to-bot to-bot to-bot 4rot {2 2 ev} to-top to-top to-top)
(: 1kkk3 {4 8} 4rrot 3dup to-bot to-bot to-bot 4rot {2 3 ev} to-top to-top to-top)
(: 1kkk4 {4 9} 4rrot 3dup to-bot to-bot to-bot 4rot {2 4 ev} to-top to-top to-top)

(: 2kkk1 {4 5} 4rrot 3dup to-bot to-bot to-bot 4rot {3 ev} to-top to-top to-top)
(: 2kkk2 {4 6} 4rrot 3dup to-bot to-bot to-bot 4rot {3 2 ev} to-top to-top to-top)
(: 2kkk3 {4 7} 4rrot 3dup to-bot to-bot to-bot 4rot {3 3 ev} to-top to-top to-top)
(: 2kkk4 {4 8} 4rrot 3dup to-bot to-bot to-bot 4rot {3 4 ev} to-top to-top to-top)

(: 3kkk1 {4 4} 4rrot 3dup to-bot to-bot to-bot 4rot {4 ev} to-top to-top to-top)
(: 3kkk2 {4 5} 4rrot 3dup to-bot to-bot to-bot 4rot {4 2 ev} to-top to-top to-top)
(: 3kkk3 {4 6} 4rrot 3dup to-bot to-bot to-bot 4rot {4 3 ev} to-top to-top to-top)
(: 3kkk4 {4 7} 4rrot 3dup to-bot to-bot to-bot 4rot {4 4 ev} to-top to-top to-top)

(: 4kkk1 {5 4} 4rrot 3dup to-bot to-bot to-bot 4rot {5 ev} to-top to-top to-top)
(: 4kkk2 {5 5} 4rrot 3dup to-bot to-bot to-bot 4rot {5 2 ev} to-top to-top to-top)
(: 4kkk3 {5 6} 4rrot 3dup to-bot to-bot to-bot 4rot {5 3 ev} to-top to-top to-top)
(: 4kkk4 {5 7} 4rrot 3dup to-bot to-bot to-bot 4rot {5 4 ev} to-top to-top to-top)

(: 2kuk1 (rcurry (1k1) rcurry 1u2) rcurry 3k3)
(: 2kuk2 (rcurry (1k2) rcurry 1u3) rcurry 3k4)

(: 3kuk1 (rcurry (2k1) rcurry 2u2) rcurry 4k3)
(: 3kuk2 (rcurry (2k2) rcurry 2u3) rcurry 4k4)

(: 2uku1 ((rcurry 1u1) rcurry 3k2) rcurry 3u3)
(: 2uku2 ((rcurry 1u2) rcurry 3k3) rcurry 3u4)

(: 3uku1 ((rcurry 2u1) rcurry 4k2) rcurry 4u3)
(: 3uku2 ((rcurry 2u2) rcurry 4k3) rcurry 4u4))
