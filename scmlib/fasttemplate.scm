(define (fasttemplate-expand)
  (define (apply-chain ori-target . procs)
    (let loop ((target ori-target)
               (proc procs))
      (if (null? proc)
        target
        (loop ((car proc) target)
              (cdr proc)))))

  (define (read-all file-name)
    (call-with-input-file
      file-name
      (lambda (in)
        (let loop ((char-ls '())
                   (c (read-char in)))
          (if (eof-object? c)
            (list->string (reverse char-ls))
            (loop (cons c char-ls) (read-char in)))))))

  (define (template-path name)
    (string-append (app-get-tool-dir) "template\\" name ".txt"))

  (define (read-template)
    (let ((template-name (app-input-box "template name")))
      (if (string? template-name)
        (let ((file-name (template-path template-name)))
          (if (file-exists? file-name)
            (read-all file-name)
            (exit)))
        (exit))))

  (define (expand-var template)
    (define (delete-_input_ template)
      (regexp-replace-all
        #/\{\{_input_:(\w+)\}\}/
        template
        "{{_var_:$1}}"))

    (define (collect-input-tag template)
      (define (self v) v)
      (let ((unsafe-tag-ls '()))
        (regexp-replace-all
          #/\{\{_input_:(\w+)\}\}/
          template
          (lambda (m)
            (set! unsafe-tag-ls (cons (rxmatch-substring m 1)
                                      unsafe-tag-ls))
            (rxmatch-substring m)))
        (reverse unsafe-tag-ls)))

    (define (input-value-with-dialog from-tag)
      (let ((to-tag (app-input-box from-tag)))
        (if to-tag
          (cons from-tag to-tag)
          (exit))))

    (define (substitute-tag template tag-ls)
      (let loop ((tags (map input-value-with-dialog tag-ls))
                 (substituted-template template))
        (if (null? tags)
          substituted-template
          (loop (cdr tags)
                (regexp-replace-all
                  (string->regexp (string-append
                                    "\\{\\{_var_:"
                                    (caar tags)
                                    "\\}\\}"))
                  substituted-template
                  (cdar tags))))))

    (substitute-tag
      (delete-_input_ template)
      (collect-input-tag template)))

  (define (expand-name template)
    (regexp-replace-all
      #/\{\{_name_\}\}/
      template
      (lambda (m)
        (regexp-replace-all
          #/.*\\([^.]*)(\..*)?$/
          (editor-get-filename)
          "$1"))))

  (define (expand-expr template)
    (regexp-replace-all
      #/\{\{_expr_:(.+)\}\}/
      template
      (lambda (m)
        (eval (read (open-input-string (rxmatch-substring m 1)))))))

  (define (append-indent template)
    (define (get-current-indent)
      (let ((indent (rxmatch-substring
                      (rxmatch
                        #/^\t+/
                        (editor-get-row-string (editor-get-cur-row))))))
        (if indent
          (string-length indent)
          #f)))
    (let ((indent (get-current-indent)))
      (if indent
        (regexp-replace-all
          #/\A\t+/
          (regexp-replace-all
            #/^/
            template
            (make-string indent #\tab))
          "")
        template)))

  (define (goto-_cursor_)
    (editor-search-string "\\{\\{_cursor_\\}\\}")
    (editor-delete-selected-string))

  (let ((template (read-template)))
    (if template
      (begin
        (editor-paste-string
          (apply-chain
            template
            expand-var
            expand-name
            expand-expr
            append-indent))
        (goto-_cursor_)))))
