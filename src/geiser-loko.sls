(library (geiser-loko)
  (export geiser:eval
          geiser:completions
          geiser:module-completions
          geiser:autodoc
          geiser:no-values
          geiser:load-file
          geiser:newline
          geiser:macroexpand)

  (import (loko)
          (loko system $primitives)
          (loko system r7rs))

  (define (with-output-to-string thunk)
    (let ((output (open-output-string)))
      (parameterize ((current-output-port* output))
        (thunk))
      (let ((res (get-output-string output)))
        (close-port output)
        res)))

  (define (write-to-string x)
    (with-output-to-string
      (lambda ()
        (write x))))

  ;; TODO get the environment of the provided module.
  (define (geiser:eval module form . rest)
    rest
    (let ((output (open-output-string))
          (result (parameterize ((current-output-port* output))
                    (eval form
                          (interaction-environment)))))
      (write
       `((result ,(write-to-string result))
         (output . ,(get-output-string output))))
      (newline)
      (close-output-port output)))

  (define (geiser:completions prefix . rest)
    rest
    (sort-list string-ci<?
               (filter (lambda (el)
                         (string-prefix? prefix el))
                       (map write-to-string
                            (environment-symbols (interaction-environment))))))

  (define (geiser:module-completions prefix . rest)
    rest
    (filter (lambda (pstring)
              (substring? prefix el))
            (map write-to-string (installed-libraries))))

  (define (operator-arglist operator)
    ;; TODO find out whether $procedure-info can be used for the arglist.
    '())

  (define (geiser:autodoc ids . rest)
    rest
    (cond ((null? ids) '())
          ((not (list? ids))
           (geiser:autodoc (list ids)))
          ((not (symbol? (car ids)))
           (geiser:autodoc (cdr ids)))
          (else
           (map (lambda (id)
                  (operator-arglist id)
                  ids)))))

  (define (geiser:no-values)
    #f)

  (define (geiser:load-file filename)
    (load filename))

  (define (geiser:newline)
    #f)

  (define (geiser:macroexpand form . rest)
    rest
    (with-output-to-string
      (lambda ()
        (pretty-print
         (syntax->datum (expand form)))))))
