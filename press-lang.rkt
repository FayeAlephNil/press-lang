#lang racket


(require racket/match)
(require racket/trace)
(require predicates)

(define (basic-eval evalu expr env)
  (match expr
    [x
     #:when (symbol? x)
     (cons
      (if (hash-has-key? env x)
          (hash-ref env x)
          (error (string-append "Variable not declared: " (symbol->string x))))
      env)]
    [`(,(or 'lambda 'λ) ,symblist ,body)
     #:when (and (list? symblist) ((all? symbol?) symblist))
     (cons (lambda (args)
             (if (= (length args) (length symblist))
                 (let*
                     ([proc (lambda (symb arg result)
                              (hash-set result symb arg))]
                      [new-env (foldl proc env symblist args)])
                   (car (evalu evalu body new-env)))
                 (error
                  (string-append
                   "Argument list does not match the desired number, you gave "
                   (number->string (length args))
                   " but " (number->string (length symblist)) " were expected"))))
             env)]
    [`(,(or 'lambda 'λ) ,x ,body)
     #:when (symbol? x)
     (cons (lambda (args)
             (car (evalu evalu body
                         (hash-set env x args))))
           env)]
    [(list op args ...)
     (cons
      ((car (evalu evalu op env))
       (map (lambda (arg) (car (evalu evalu arg env))) args))
      env)]
    [boi (error (string-append "Syntax Error, No Match for: " (~a boi)))]))

(define (decor-full f evalu)
  (lambda (evalu2 expr env)
    (let* ([result (f evalu2 expr env)]
           [go-on? (car result)]
           [new-expr (cadr result)]
           [new-env (cddr result)]
           )
      (if go-on?
          (evalu evalu2 new-expr new-env)
          (cons new-expr new-env)))))

(define (decor-literal p? evalu)
  (decor-full
   (lambda (evalu2 expr env)
     (if (p? expr)
         (cons #f (cons expr env))
         (cons #t (cons expr env))))
   evalu))

(define (decor-macro-evalu-env p? f evalu)
  (decor-full
   (lambda (evalu2 expr env)
     (if (p? expr)
         (let* ([result (f evalu expr env)]
                [new-expr (car result)]
                [new-env (cdr result)]
                )
           (cons #t (cons new-expr new-env)))
         (cons #t (cons expr env))))
   evalu))

(define (decor-macro-evalu p? f evalu)
  (decor-full
   (lambda (evalu2 expr env)
     (if (p? expr)
         (cons #t (cons (f evalu expr env) env))
         (cons #t (cons expr env))))
   evalu))

(define (add-nums evalu)
  (decor-literal number? evalu))

(define (add-bools evalu)
  (decor-literal boolean? evalu))

(define (add-quote evalu)
  (decor-full
   (lambda (evalu2 expr env)
     (match expr
       [(list 'quote quoted)
        (cons #f (cons quoted env))]
       [boi
        (cons #t (cons boi env))]))
   evalu))

(define (add-eval evalu)
  (decor-full
   (lambda (evalu2 expr env)
     (match expr
       [(list 'eval to-eval-expr)
        (let ([to-eval (car (evalu2 evalu2 to-eval-expr env))])
          (cons #f (evalu2 evalu2 to-eval env)))]
       [boi
        (cons #t (cons boi env))]))
  evalu))

(define (eval-all evalu env result . exprs)
  (if (empty? exprs)
      (cons result env)
      (let* ([eval-res (evalu evalu (car exprs) env)]
             [new-result (car eval-res)]
             [new-env (cdr eval-res)])
        (apply eval-all (cons evalu (cons new-env (cons new-result (cdr exprs))))))))

(define (basic-eval-all env result . exprs)
  (apply eval-all (cons basic-eval (cons env (cons result exprs)))))

(define my-eval
  (add-nums
   (add-bools
    (add-quote
     (add-eval
      basic-eval)))))

(define (my-eval-all env result . exprs)
  (apply eval-all (cons my-eval (cons env (cons result exprs)))))

(define empty-env (hash))
