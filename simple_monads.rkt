#lang racket
(require racket/generic)

;;; Set up monad interface

(define-generics monad
  (return monad val)
  (run monad)
  (>>= monad f))

;;; Result
(struct Result () #:transparent
  #:methods gen:monad
  [(define (return monad value)
     (Ok value))
   (define (run monad)
     (match monad
       [(Ok v) v]
       [(Error e) (error e)]))
   (define (>>= monad f)
     (match monad
       [(Ok v) (f v)]
       [(Error _) monad]))])

(struct Ok Result (value) #:transparent)
(struct Error (err) #:transparent)

;;; List monad
(struct Lst (vals) #:transparent
  #:methods gen:monad
  [(define (return monad vals)
     (Lst vals))
   (define (run monad)
     (Lst-vals monad))
   (define (>>= monad f)
     (map f (Lst-vals monad)))])

;;; Audit/trace/logging Monad
(struct AuditValue (value audit-log)
  #:transparent
  #:methods gen:monad
  [(define (return monad value)
     (AuditValue value '()))
   (define (run monad)
     (AuditValue-value monad))
   (define (>>= monad f)
     (match-let* ([(AuditValue val₁ log₁) monad]
                  [(AuditValue val₂ log₂) (f val₁)])
       (AuditValue val₂ (append log₂ log₁))))])

(define base-path "/Users/ashton/School/2022_Winter/cs_630/presentations/monads/")

(let ([file-thing (AuditValue "datasets/foo" '("initial file name"))])
  (>>= file-thing
       (λ (filename)
         (>>= (AuditValue (build-path base-path filename) '("prepended base path"))
              (λ (fullpath) (AuditValue (file->lines fullpath) '("fetched lines from file")))))))

;;; Use a macro to get a little sugar
(define-syntax monadic-do
  (syntax-rules (<-)
    [(_ [val-sym <- gen] body)
     (>>= gen (λ (val-sym) body))]
    [(_ [val-sym <- gen] [val-syms <- gens] ... body)
     (>>= gen (λ (val-sym) (monadic-do [val-syms <- gens] ... body)))]))

(monadic-do
 [filename <- (AuditValue "datasets/foo" '("initial file name"))]
 [full-path <- (AuditValue (build-path base-path filename) '("prepended base path"))]
 (AuditValue (file->lines full-path) '("fetched lines from file")))

;;; Debugging with: (expand #'(monadic-do [file-thing <- (+ 1 2)] [filename <- (+ file-thing 1)] (* filename 42)))
