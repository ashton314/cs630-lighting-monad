#lang racket

(require web-server/servlet
         web-server/dispatch
         web-server/servlet-env)

(define datasets-path "/Users/ashton/School/2022_Winter/cs_630/presentations/monads/datasets/")

(define-values (my-app req-url)
  (dispatch-rules
   [("") index]
   [("sum" (string-arg)) sum-dataset]
   [("find-even") find-even-dataset]))

(define (index req)
  (response/xexpr
   `(html (head (title "My app"))
          (body (p "Datasets")
                (ul
                 ,@(for/list ([f (in-directory datasets-path)])
                     (let ([ds (path->string (file-name-from-path f))])
                       `(li (a ([href ,(string-append "/sum/" ds)]) ,ds)))))
                (a ([href "/find-even"]) "Find datasets with even sums")))))

(define (sum-dataset req setname)
  (response/xexpr
   `(html (head (title "My app | sum | " ,setname))
          (body
           (p (a ([href "/"]) "Back"))
           (p "Summing data from this dataset: " ,setname)
           (p ,(unpack (thread (sum-dataset-file? setname)
                               (λ (sum)
                                 (ok (format "Dataset sum: ~a" sum))))))))))

(define (find-even-dataset req)
  (let (
        [datasets
         (for/list ([f (in-directory datasets-path)])
           (path->string (file-name-from-path f)))]
        )
    (response/xexpr
     `(html (head (title "My app | find evens"))
            (body
             (p (a ([href "/"]) "Back"))
             (p "Even datasets:")
             (ul
              ,@(for/list ([ds datasets])
                  `(li ,ds
                       ": "
                       ,(unpack (thread (sum-dataset-file? ds)
                                        (λ (sum) (ok (if (even? sum) "is even" "isn't even")))))))))))))

;;; A struct to help us with error handing
(struct ok (value) #:transparent)
(struct err (message) #:transparent)

;;; Yanks the thing inside of the struct, no matter what it is, and returns it
(define (unpack ok-or-err)
  (match ok-or-err
    [(ok value) value]
    [(err mss) mss]))

;;; thread :: ok-or-error, (val -> ok-or-error) -> ok-or-error
(define (thread ok-or-err fn)
  (match ok-or-err
    [(ok val) (fn val)]
    [(err message) (err message)]))

(define (sum-dataset-file? set-name)
  (let ([full-path (build-path datasets-path set-name)])
    (if (file-exists? full-path)
        (let ([file-lines (file->lines full-path)])
          (if (all? looks-like-number? file-lines)
              (ok (apply + (map string->number file-lines)))
              (err "bad line in file")))
        (err "no file"))))

(define (all? ? lst)
  (if (null? lst)
      #t
      (if (? (car lst))
          (all? ? (cdr lst))
          #f)))

;;; Does this string look like a number to you?
(define (looks-like-number? str)
  (not (not (string->number str))))

(serve/servlet my-app
               #:port 8080
               #:launch-browser? #f
               #:servlet-regexp #rx"")


