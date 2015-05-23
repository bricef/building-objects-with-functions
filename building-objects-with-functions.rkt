#lang racket

(define (mkmap) `())

(define (second l)
  (first (rest l)))

(define (get-key kv)
  (first kv))

(define (get-val kv)
  (second kv))

(define (get-in m k)
  (cond 
    [(empty? m) m]
    [(eq? (first (first m)) k) (get-val (first m))]
    [else (get-in (rest m) k)]))

(define (concat l1 l2)
  (cond 
    [(empty? l1) l2]
    [l1 (cons (first l1) (concat (rest l1) l2))]))



(define (assoc m k v)
  (cond 
    [(empty? m) (list (list k v))]
    [(eq? (get-key (first m)) k) (cons (list k v) (rest m))]
    [else (cons (first m) (assoc (rest m) k v))]))

(define (dissoc m k)
  (cond
    [(empty? m) m]
    [(eq? (get-key (first m)) k) (rest m)]
    [else (cons (first m) (dissoc (rest m) k))]))

(define t (assoc (assoc '() "a" 1) "b" 2))
     
     