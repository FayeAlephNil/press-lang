#lang racket

(require racket/match)
(require racket/trace)

(define (my-eval expr env)
  (match expr
    [`inc
     (cons (lambda (arg) (+ 1 arg)) env)]
    ['#f (cons #f env)]
    ['#t (cons #t env)]
    [`(if ,bool ,body1 ,body2)
     (let* ([result (my-eval bool env)]
            [do-it? (car result)]
            [new-env (cdr result)])
           (if do-it?
               (my-eval body1 new-env)
               (my-eval body2 new-env)))]
    ;[`(proc ,name)
    ; (cons (lambda (arg) (apply (eval name) arg)) env)]
    ;[`(list ,@(list-rest a ... lst))
    ; (cons (car (cons a lst)) env)]
    [`()
     (cons '() env)]
    [x
     #:when (number? x)
     (cons x env)]
    [x
     #:when (symbol? x)
     (cons (env x) env)]
    [`(def ,x ,body)
     (cons '() (letrec ([new-env
                         (lambda (y) (if (eq? x y)
                          (car (my-eval body new-env))
                          (env y)))
                         ]) new-env))]
    [`(lambda (,x) ,body)
     (cons (lambda (arg) (car (my-eval body
                (lambda (y)
                  (if (eq? x y)
                      arg
                      (env y)))))) env)]
    [`(,op ,args)
     (cons ((car (my-eval op env)) (car (my-eval args env))) env)]
    ))

(define (eval-all evaluator env result . exprs)
  (if (empty? exprs)
      (cons result env)
      (let* ([eval-res (evaluator (car exprs) env)]
             [new-result (car eval-res)]
             [new-env (cdr eval-res)])
        (apply eval-all (cons evaluator (cons new-env (cons new-result (cdr exprs))))))))

(define (my-eval-all env result . exprs)
  (apply eval-all (cons my-eval (cons env (cons result exprs)))))

(define (empty-env x)
  (error (string-append "Symbol not found in environment: " (symbol->string x))))

(define basic-env
  (cdr
   (my-eval-all empty-env '()
                '(def + (proc +))
                '(def - (proc -))
                '(def * (proc *)))
   ))

(define lambda-env
  (cdr
   (my-eval-all empty-env '()
                '(def id (lambda (x) x))
                '(def const (lambda (x)
                              (lambda (y)
                                x)))
                '(def zero (lambda (f)
                             (lambda (x)
                               x))
                   )
                '(def succ (lambda (n)
                             (lambda (f)
                               (lambda (x)
                                 (f ((n f) x))))))
                '(def one (succ zero))
                '(def two (succ one))
                '(def three (succ two))
                '(def add (lambda (n)
                            (lambda (m)
                              (lambda (f)
                               (lambda (x)
                                 ((m f) ((n f) x)))))))
                '(def mult (lambda (n)
                             (lambda (m)
                               ((n (add m)) zero))))
                '(def exp (lambda (n)
                             (lambda (m)
                               (m n))))
                '(def is-zero? (lambda (n)
                                ((n (lambda (x) #f)) #t)))
                '(def pred (lambda (n)
                             (lambda (f)
                               (lambda (x)
                                 (((n (lambda (g)
                                      (lambda (h)
                                        (h (g f)))))
                                   (const x)) id)))))
                '(def church->num (lambda (n)
                                    ((n inc) 0)))
                )))

(define factorial-impl
  '(def fact
     (lambda (n)
       (if (is-zero? n)
           one
           ((mult n) (fact (pred n))))
       )
    ))
