;a non-compliant xml subset
(define parse-wiki-xml (lambda (raw-xml)
  ;if num is <0, return zero, else return num
  (define negative-to-zero (lambda (num)
    (if (< num 0)
      0
      num
    )
  ))
  (define extract-inside-tag (lambda (port tag-name)
    (define extract-inside-tag-tail (lambda (p inside)
      (let* (
        [cc (get-char p)]
        [all (string-append inside (string cc))]
        [l (string-length all)]
        [tl (- l (string-length tag-name) 3)]
      )
        (if (string=? (substring all (negative-to-zero tl) l) (string-append "</" tag-name ">"))
          (values (substring all 0 tl) p)
          (extract-inside-tag-tail p all)
        )
      )
    ))
    (extract-inside-tag-tail port "")
  ))
  ;current content is only string, since element content will be recursively gotten
  (define parse-wiki-xml-tail (lambda (port xml-tree current-tag current-content)
    ;cc = current char
    (let ([cc (get-char port)])
      (cond
        [
          (eof-object? cc)
          xml-tree
        ]
        [
          (char=? cc #\<)
          (if current-tag
            (if (char=? (lookahead-char port) #\/)
              ;tag end
              (parse-wiki-xml-tail port (cons (cons current-tag current-content) xml-tree) #f #f)
              ;is advanced-port needed?
              (let-values ([(section advanced-port) (extract-inside-tag port current-tag)])
                ;(display section)
                ;grab the section of text that is the element content
                ;(cons vara varb) becomes (vara . varb)
                (parse-wiki-xml-tail advanced-port (cons (cons current-tag (parse-wiki-xml (string-append (string #\<) section))) xml-tree) #f #f)
              )
            )
            ;new tag is starting
            (parse-wiki-xml-tail port xml-tree "" #f)
          )
        ]
        [
          (and (not current-tag) (not current-content))
          ;probably just end tag name, just skip
          (parse-wiki-xml-tail port xml-tree current-tag current-content)
        ]
        [
          ;if space but no content yet, ignore (change: added (not current-tag)
          (and (char=? cc #\space) (not current-content) (not current-tag))
          (parse-wiki-xml-tail port xml-tree current-tag #f)
        ]
        [
          (and (char=? cc #\>) (not current-content))
          (if (char=? (car (reverse (string->list current-tag))) #\/)
            (parse-wiki-xml-tail port (cons (cons current-tag #t) xml-tree) #f #f)
            ;tag name is done, now content?
            (parse-wiki-xml-tail port xml-tree current-tag "")
          )
        ]
        [
          ;add to tag name
          ;(and (char=? cc #\>) (not current-content))
          (not current-content)
          (parse-wiki-xml-tail port xml-tree (string-append current-tag (string cc)) current-content)
        ]
        [
          ;add to string content
          current-content
          (parse-wiki-xml-tail port xml-tree current-tag (string-append current-content (string cc)))
        ]
        #|[
          else
          ;probably just end tag name, just skip
          (parse-wiki-xml-tail port xml-tree current-tag current-content)
        ]|#
      )
    )
  ))
  (parse-wiki-xml-tail (open-string-input-port raw-xml) '() #f #f)
))


(define substring-try (lambda (str start end)
  (substring str start (if (< (string-length str) end) (string-length str) end))
))

(define get-value-from-key (lambda (pairs key starts)
  (define get-value-from-key-tail (lambda (pairs-m)
    (if (= (length pairs-m) 0)
      #f ;not found
      (if (or (and (string=? key (car (car pairs-m))) (not starts)) (and (string=? key (substring-try (car (car pairs-m)) 0 (string-length key))) starts))
        (cdr (car pairs-m))
        (get-value-from-key-tail (cdr pairs-m))
      )
    )
  ))
  (get-value-from-key-tail pairs)
))

;there is more information that we do NOT care about
(define-record-type last-edit (fields timestamp username comment))
(define extract-last-edit (lambda (xml-tree)
  (let ([revision (get-value-from-key (cdr (car xml-tree)) "revision" #f)])
    (make-last-edit (get-value-from-key revision "timestamp" #f) (get-value-from-key (get-value-from-key revision "contributor" #f) "username" #f) (get-value-from-key revision "comment" #f))
  )
))

(define extract-text (lambda (xml-tree)
  (get-value-from-key (get-value-from-key (cdr (car xml-tree)) "revision" #f) "text" #t)
))

(define text-to-term (lambda (raw-xml)
  ;
  ;placeholder
  raw-xml
))
