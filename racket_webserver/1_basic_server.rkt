#lang racket

(require web-server/servlet
         web-server/dispatch
         web-server/servlet-env)

(define datasets-path "/Users/ashton/Scratch/datasets/")

(define-values (my-app req-url)
  (dispatch-rules
   [("") index]
   [("sum" (string-arg)) sum-dataset]))

(define (index req)
  (response/xexpr
   `(html (head (title "My app"))
          (body (p "Datasets")
                (ul
                 ,@(for/list ([f (in-directory datasets-path)])
                     (let ([ds (path->string (file-name-from-path f))])
                       `(li (a ([href ,(string-append "/sum/" ds)]) ,ds)))))))))

(define (sum-dataset req setname)
  (response/xexpr
   `(html (head (title "My app | sum dataset | " setname))
          (body
           (p (a ([href "/"]) "Back"))
           (p "Summing data from this dataset: " ,setname)
           (p "Sum is " ,(let* ([lines (file->lines (build-path datasets-path setname))]
                                [numbers (map string->number lines)])
                           (number->string (apply + numbers))))))))

(serve/servlet my-app
               #:port 8080
               #:launch-browser? #f
               #:servlet-regexp #rx"")


