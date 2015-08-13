(define (load-wara-fasttemplate)
  (define (apply-chain ori-target . proc-ls)
    (let loop ((target ori-target)
               (proc-ls proc-ls))
      (if (null? proc-ls)
        target
        (loop ((car proc-ls) target)
              (cdr proc-ls)))))

  (define (get-basename path)
    (regexp-replace-all #/.*\\([^.]*)(\..*)?$/ path "$1"))

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
    (let ((var-table (make-hash-table 'string=?)))
      (regexp-replace-all
        #/\{\{_(?:var|input)_:(\w+)\}\}/
        template
        (lambda (m)
          (let ((tag (rxmatch-substring m 1)))
            (if (not (hash-table-exists? var-table tag))
              (let ((var (app-input-box tag)))
                (if (not (string? var))
                  (exit)
                  (hash-table-put! var-table tag var))))
            (hash-table-get var-table tag ""))))))

  (define (expand-name template)
    (regexp-replace-all
      #/\{\{_name_\}\}/
      template
      (lambda (m)
        (get-basename (editor-get-filename)))))

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

  (define (move-to-end-of-line)
    (editor-set-row-col
      (editor-get-cur-row)
      (string-length (editor-get-row-string (editor-get-cur-row)))))

  (define (goto-_cursor_)
    (editor-search-string "\\{\\{_cursor_\\}\\}")
    (editor-delete-selected-string))

  (define (fasttemplate-expand)
    (let ((template (read-template)))
      (if template
        (begin
	  (move-to-end-of-line)
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
