
#lang racket
(require racket/string)


(provide author guild-info)

(define (author ctx)
  (hash-ref ctx 'author))


(define (guild-info ctx)
  ;; I think Discord message events don't actually include the guild name, just the guild ID.
  ;; So the name will show as "unknown" unless you cache it from something like raw-guild-create.
  ;; Looks like I'll probably have to implement this properly at some point (either with a cache or an API call)?
  (define guild-id (hash-ref ctx 'guild_id #f))
  (define guild-name (or (hash-ref ctx 'guild_name #f)
                         (hash-ref ctx 'name #f)
                         "<unknown>"))
  (if guild-id
      (hash 'id guild-id 'name guild-name)
      #f))
