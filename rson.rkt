#lang racket

(require json racket/list racket/string)

(provide rget rget* rget-path rget-in rassoc rkeys rvals rupdate rset rdel rcopy
         rselect rfilter rmap rflatten)

;; rget: get value for key from hash or alist, with optional default
(define (rget obj key #:default [default #f])
  (cond
    [(hash? obj) (hash-ref obj key default)]
    [(list? obj) (let ([p (assoc key obj)]) (if p (cdr p) default))]
    [else default]))

;; rget*: get value for key from hash/alist, error if not found
(define (rget* obj key)
  (cond
    [(hash? obj) (hash-ref obj key)]
    [(list? obj) (cdr (assoc key obj))]
    [else (error 'rget* "Not a hash or alist")]))

;; rget-path: get value by path (list of keys)
(define (rget-path obj path #:default [default #f])
  (for/fold ([cur obj]) ([k path])
    (if (or (hash? cur) (list? cur))
        (rget cur k #:default default)
        default)))

;; rget-in: like rget-path, but variadic
(define (rget-in obj . path)
  (rget-path obj path))

;; rassoc: like assoc but for hash or alist
(define (rassoc obj key)
  (cond
    [(hash? obj) (and (hash-has-key? obj key) (cons key (hash-ref obj key)))]
    [(list? obj) (assoc key obj)]
    [else #f]))

;; rkeys: get keys from hash or alist
(define (rkeys obj)
  (cond
    [(hash? obj) (hash-keys obj)]
    [(list? obj) (map car obj)]
    [else '()]))

;; rvals: get values from hash or alist
(define (rvals obj)
  (cond
    [(hash? obj) (hash-values obj)]
    [(list? obj) (map cdr obj)]
    [else '()]))

;; rupdate: update key in hash/alist with function
(define (rupdate obj key f)
  (cond
    [(hash? obj) (hash-set obj key (f (hash-ref obj key #f)))]
    [(list? obj) (let ([p (assoc key obj)])
                   (if p (cons (cons key (f (cdr p))) (remove p obj))
                       (cons (cons key (f #f)) obj)))]
    [else obj]))

;; rset: set key to value in hash/alist
(define (rset obj key val)
  (cond
    [(hash? obj) (hash-set obj key val)]
    [(list? obj) (let ([p (assoc key obj)])
                   (if p (cons (cons key val) (remove p obj))
                       (cons (cons key val) obj)))]
    [else obj]))

;; rdel: delete key from hash/alist
(define (rdel obj key)
  (cond
    [(hash? obj) (hash-remove obj key)]
    [(list? obj) (remove* (list (assoc key obj)) obj)]
    [else obj]))

;; rcopy: deep copy hash/alist
(define (rcopy obj)
  (cond
    [(hash? obj) (for/hash ([(k v) (in-hash obj)]) (values k (rcopy v)))]
    [(list? obj) (map (λ (p) (cons (car p) (rcopy (cdr p)))) obj)]
    [else obj]))


;; rselect: select multiple keys from hash/alist, returns hash
(define (rselect obj keys)
  (cond
    [(hash? obj) (for/hash ([k keys] #:when (hash-has-key? obj k)) (values k (hash-ref obj k)))]
    [(list? obj) (for/hash ([k keys] #:when (assoc k obj)) (values k (cdr (assoc k obj))))]
    [else (hash)]))

;; rfilter: filter hash/alist by predicate (key val -> bool)
(define (rfilter pred obj)
  (cond
    [(hash? obj) (for/hash ([(k v) (in-hash obj)] #:when (pred k v)) (values k v))]
    [(list? obj) (filter (λ (p) (pred (car p) (cdr p))) obj)]
    [else obj]))

;; rmap: map function (key val) over hash/alist, returns hash/alist
(define (rmap f obj)
  (cond
    [(hash? obj) (for/hash ([(k v) (in-hash obj)]) (define-values (k2 v2) (f k v)) (values k2 v2))]
    [(list? obj) (map (λ (p) (define-values (k v) (f (car p) (cdr p))) (cons k v)) obj)]
    [else obj]))

;; rflatten: flatten nested hash/alist to list of (path value) pairs
(define (rflatten obj #:path [path '()])
  (cond
    [(hash? obj)
     (append* (for/list ([(k v) (in-hash obj)]) (rflatten v #:path (append path (list k)))))]
    [(list? obj)
     (if (and (pair? obj) (pair? (car obj)) (symbol? (caar obj)))
         (append* (for/list ([p obj]) (rflatten (cdr p) #:path (append path (list (car p))))))
         (list (cons path obj)))]
    [else (list (cons path obj))]))
