(define (load-wara-fasttemplate)
  (define (apply-chain ori-target . proc-ls)
    (let loop ((target ori-target)
               (proc-ls proc-ls))
      (if (null? proc-ls)
        target
        (loop ((car proc-ls) target)
              (cdr proc-ls)))))

  (define (read-all name)
    (call-with-input-file
      name
      (lambda (in)
        (let loop ((char-ls '())
                   (char (read-char in)))
          (if (eof-object? char)
            (list->string (reverse char-ls))
            (loop (cons char char-ls) (read-char in)))))))

  (define (template-path name)
    (string-append (app-get-tool-dir) "template\\" name ".txt"))

  (define (read-template)
    (let* ((name (app-input-box "template name"))
	   (path (template-path name)))
      (if (file-exists? path)
	(read-all path)
	#f)))

  (define (expand-var template)
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
                                    "\\{\\{_(var|input)_:"
                                    (caar tags)
                                    "\\}\\}"))
                  substituted-template
                  (cdar tags))))))

    (substitute-tag
      template
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

  (define (fasttemplate-expand)
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

  (define fasttemplate-key
    (if (symbol-bound? 'fasttemplate-key)
      fasttemplate-key
       "Ctrl+d"))

  (app-set-key fasttemplate-key fasttemplate-expand))

(load-wara-fasttemplate)
