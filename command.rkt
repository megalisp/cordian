#lang racket

(require (for-syntax racket/base)
         racket/string
         "context.rkt"
         "utils.rkt"
         "event.rkt"
         "debug.rkt"
         (only-in racket-cord client-user))

(provide cmd set-cmd-prefix! command-meta-table register-command commands match-prefix command-table cmd-prefix start-command-dispatcher)

;; Command prefix logic (default is accordion, can be overridden)
(define cmd-prefix "🪗")
(define (set-cmd-prefix! p)
  (set! cmd-prefix p))

;; Match prefix with optional space
(define (match-prefix content)
  (let ([p cmd-prefix])
    (cond
      [(string-prefix? content p) p]
      [(string-prefix? content (string-append p " ")) p]
      [else #f])))

;; Command registration and handling
(define-syntax (cmd stx)
  (syntax-case stx ()
    [(_ name _ #:desc desc #:exam exam #:logic logic ...)
     (with-syntax ([ctx (datum->syntax stx 'ctx)]
                   [args (datum->syntax stx 'args)])
       #`(begin
           (define (name ctx . args)
             (let ([ctx ctx]
                   [args args])
               (begin #,@(map syntax-local-introduce (syntax->list #'(logic ...))))))
           (hash-set! command-meta-table 'name (hash 'desc desc 'exam (string-append cmd-prefix exam)))
           (register-command (symbol->string 'name) name)))]))

(define (commands ctx)
  (hash-keys command-table))

(define command-table (make-hash))
(define command-meta-table (make-hash))

(define (register-command name handler)
  (hash-set! command-table name handler))

(define (match-command prefix content)
  (define parts (string-split content))
  (define name (string-trim (car parts) prefix))
  (define args (cdr parts))
  (define cmd (hash-ref command-table name #f))
  (when cmd (apply cmd args)))

;; Command dispatcher: listens for messages and runs commands
(define (start-command-dispatcher client)
  (on-event 'raw-message-create client
    (lambda (_ws-client client payload)
      (define content (hash-ref payload 'content ""))
      (define prefix (match-prefix content))
      (define author-id (hash-ref (hash-ref payload 'author) 'id #f))
  (define self-id (hash-ref (client-user client) 'id #f))
      (when (and prefix (not (equal? author-id self-id)))
        (define name-and-args (string-trim (substring content (string-length prefix))))
        (define parts (string-split name-and-args))
        (when (pair? parts)
          (define name (car parts))
          (define args (cdr parts))
          (define ctx payload)
          (debug-log "DISPATCHER" "About to dispatch command: ~a with args: ~a" name args)
          (current-event ctx)
          (define handler (hash-ref command-table name #f))
          (when handler (apply handler ctx args)))))))
