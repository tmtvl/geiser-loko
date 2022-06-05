(library (geiser-loko)
  (export geiser:eval
          geiser:completions
          geiser:module-completions
          geiser:autodoc
          geiser:no-values
          geiser:load-file
          geiser:newline
          geiser:macroexpand)

  (import (loko))

  (define (last-index-of str-list char idx last-idx)
    (if (null? str-list)
        last-idx
        (last-index-of (cdr str-list)
                       char
                       (+ 1 idx)
                       (if (char=? char
                                   (car str-list))
                           idx
                           last-idx))))

  (define (obj-file-name name)
    (let ((idx (last-index-of (string->list name)
                              #\.
                              0
                              -1)))
      (if (= idx -1)
          (string-append name ".so")
          (string-append (substring name 0 idx)
                         ".so"))))

  (define (geiser:load-file filename)
    (let ((output-filename (obj-file-name filename)))
      (load output-filename)))

  (define string-prefix?
    (lambda (x y)
      (let ((n (string-length x)))
        (and (fx<=? n (string-length y))
             (let prefix? ((i 0))
               (or (fx=? i n)
                   (and (char=? (string-ref x i)
                                (string-ref y i))
                        (prefix? (fx+ i 1)))))))))

  (define (geiser:completions prefix . rest)
    rest
    (list-sort string-ci<?
               (filter (lambda (el)
                         (string-prefix? prefix el))
                       (map write-to-string
                            (environment-symbols (interaction-environment))))))

  (define (write-to-string x)
    (call-with-string-output-port
     (lambda (port)
       (write x port))))

  (define (geiser:eval module form . rest)
    rest
    (let ((output-string (open-output-string)))
      (write
       (call/cc
        (lambda (k)
          (with-exception-handler
              (lambda (e)
                (k `((result "")
                     (output . ,(string-append
                                 (get-output-string output-string)
                                 "\n"
                                 (call-with-string-output-port
                                   (lambda (port)
                                     (display e port)))))
                     (error (key . geiser-debugger)))))
            (lambda ()
              (call-with-values
                  ;; evaluate form, allow for multiple return values,
                  ;; and capture output in output-string.
                  (lambda ()
                    (parameterize ((current-output-port output-string))
                      (if module
                          (eval form
                                (environment module))
                          (eval form))))
                (lambda result
                  `((result ,(call-with-string-output-port
                              (lambda (port)
                                (pretty-print
                                 (if (null? (cdr result))
                                     (car result)
                                     result)
                                 port))))
                    (output .
                            ,(get-output-string output-string))))))))))
      (newline)
      (close-output-port output-string)))

  (define (geiser:module-completions prefix . rest)
    (define (substring? s1 s2)
      (let ((n1 (string-length s1))
            (n2 (string-length s2)))
        (let loop2 ((i2 0))
          (let loop1 ((i1 0)
                      (j i2))
            (if (fx=? i1 n1)
                i2
                (and (not (fx=? j n2))
                     (if (char=? (string-ref s1 i1)
                                 (string-ref s2 j))
                         (loop1 (fx+ i1 1)
                                (fx+ j 1))
                         (loop2 (fx+ i2 1)))))))))
    (filter (lambda (el)
              (substring? prefix el))
            (map write-to-string (installed-libraries))))

  (define (geiser:autodoc ids . rest)
    (cond ((null? ids) '())
          ((not (list? ids))
           (geiser:autodoc (list ids)))
          ((not (symbol? (car ids)))
           (geiser:autodoc (cdr ids)))
          (else
           ids)))

  (define (geiser:no-values)
    #f)

  (define (geiser:newline)
    #f)

  (define (geiser:macroexpand form . rest)
    (call-with-string-output-port
      (lambda (port)
        (pretty-print
         (syntax->datum (expand form))
         port)))))
