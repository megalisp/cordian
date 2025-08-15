#lang racket

(require racket/base
         "event.rkt"
         "utils.rkt"
         "debug.rkt"
         "context.rkt")


(provide rec)

;; ============================================================================
;; ----------------------------------------------------------------------------
;; DISCLAIMER:
;; It is ambiguous if, and to what degree, that any kind of 'logging' system for 
;; Discord is against their Terms of Service (TOS) or other policies. Use this macro
;; and any logging functionality with caution. By using this code, you agree that
;; the creator(s) and contributor(s) of this project (cordian) is not responsible
;; for any legal repercussions, loss of account, or other consequences resulting
;; from its use. You are solely responsible for your actions and compliance with
;; Discord's TOS and applicable laws.
;; ----------------------------------------------------------------------------
;; Intended Use-Case:
;; Our primary use-case for the rec macro is to log messages from channels such as
;; "micro" where only the server owner can write to it & as-such consent to its use.
;; This is intended to act as a microblog or announcement feed, not as a tool for
;; mass surveillance or capturing private conversations. We believe this use-case 
;; is not against Discord's TOS, but you should always be careful to review and 
;; be assured you comply with Discord's policies yourself.
;; ============================================================================

(define-syntax (rec stx)
  (syntax-case stx ()
    [(_ #:chan-id chan-id #:outputs outputs . rest)
     (let* ([args (syntax->list #'rest)]
            [get-keyword
             (lambda (kw default)
               (let loop ([lst args])
                 (cond
                   [(null? lst) default]
                   [(and (pair? lst) (eq? (syntax-e (car lst)) kw))
                    (if (pair? (cdr lst)) (syntax-e (cadr lst)) default)]
                   [else (loop (cdr lst))])))]
            [only-user (get-keyword '#:only-user #f)]
            [excl-user (get-keyword '#:excl-user #f)]
            [disp-guild (get-keyword '#:disp-guild #t)]
            [disp-channel (get-keyword '#:disp-channel #t)]
            [disp-username (get-keyword '#:disp-username #t)]
            [newline (get-keyword '#:newline #f)]
            [disp-timestamp (get-keyword '#:disp-timestamp #f)]
            [buffer (get-keyword '#:buffer #f)])
       #`(let ()
           (define only-user #,only-user)
           (define excl-user #,excl-user)
           (define disp-guild (eq? #,disp-guild #t))
           (define disp-channel (eq? #,disp-channel #t))
           (define disp-username (eq? #,disp-username #t))
           (define newline (eq? #,newline #t))
           (define disp-timestamp (eq? #,disp-timestamp #t))
           (define buffer (eq? #,buffer #t))
           (define (rec-handler _ws-client client payload)
             (define msg-chan (hash-ref payload 'channel_id ""))
             (define msg-user (hash-ref (hash-ref payload 'author) 'id ""))
             (define msg-content (hash-ref payload 'content ""))
             (define msg-username (hash-ref (hash-ref payload 'author) 'username ""))
             (define msg-guild (hash-ref payload 'guild_id ""))
             (define msg-timestamp (or (hash-ref payload 'timestamp #f) (hash-ref payload 'edited_timestamp #f)))
             (define allow? (and (string=? msg-chan chan-id)
                                 (or (not only-user) (string=? msg-user only-user))
                                 (or (not excl-user) (not (string=? msg-user excl-user)))) )
             (define real-outputs
               (cond
                 [(string-suffix? ".rec" outputs) outputs]
                 [(string-suffix? ".log" outputs) (string-replace outputs ".log" ".rec")]
                 [else (string-append outputs ".rec")]))
             (define header
               (string-join
                 (filter values
                   (list
                     (and disp-timestamp msg-timestamp)
                     (and disp-guild (if (string? msg-guild) msg-guild ""))
                     (and disp-channel (if (string? msg-chan) msg-chan ""))
                     (and disp-username (if (string? msg-username) msg-username msg-user))))
                 " | "))
             (when allow?
               (with-output-to-file real-outputs
                 (lambda ()
                   (cond
                     [(and newline buffer)
                      (displayln "")
                      (displayln header)
                      (displayln msg-content)
                      (displayln "")]
                     [newline
                      (displayln header)
                      (displayln msg-content)]
                     [else
                      (displayln (string-append header (if (string=? header "") "" ": ") msg-content))]))
                 #:exists 'append)))
           (on-event 'raw-message-create (current-discord-client) rec-handler)
           'rec-registered))]))