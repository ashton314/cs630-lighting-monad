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
           (p "Sum is " ,(number->string (sum-dataset-file setname)))))))

(define (find-even-dataset req)
  (let (
        [datasets '("foo" "baz")]
        ;; [datasets
        ;;  (for/list ([f (in-directory datasets-path)])
        ;;    (path->string (file-name-from-path f)))]
        )
    (response/xexpr
     `(html (head (title "My app | find evens"))
            (body
             (p (a ([href "/"]) "Back"))
             (p "Even datasets:")
             (ul
              ,@(for/list ([ds datasets])
                  `(li ,ds ,(if (even? (sum-dataset-file ds)) " is even" " isn't even")))))))))

(define (sum-dataset-file set-name)
  (let* ([lines (file->lines (build-path datasets-path set-name))]
         [numbers (map string->number lines)])
    (apply + numbers)))

(serve/servlet my-app
               #:port 8080
               #:launch-browser? #f
               #:servlet-regexp #rx"")


