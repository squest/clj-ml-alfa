#lang racket

(require plot)

(define (chart str-part-name)
  (define data0
    (file->list (string-append "class-users-sma-3dim-"
                               str-part-name
                               "-view-0.edn")))
  (define data1
    (file->list (string-append "class-users-sma-3dim-"
                               str-part-name
                               "-view-1.edn")))
  (define data2
    (file->list (string-append "class-users-sma-3dim-"
                               str-part-name
                               "-view-2.edn")))
  (for [(i (map (λ(x) (length (first x)))
                (list data0 data1 data2)))]
    (println i))
  (plot3d
   (list
    (points3d (first data0) #:size 3 #:color "red")
    (points3d (first data1) #:size 3 #:color "blue")
    (points3d (first data2) #:size 3 #:color "green"))))