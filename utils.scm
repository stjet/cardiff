(define split-by-char (lambda (split-string split-char)
  (define split-by-char-tail (lambda (index current-string split-list)
    (if (= index (string-length split-string))
      (reverse (cons current-string split-list))
      (let ([current-char (string-ref split-string index)])
        (if (char=? split-char current-char)
          (split-by-char-tail (+ index 1) "" (cons current-string split-list))
          (split-by-char-tail (+ index 1) (string-append current-string (string current-char)) split-list)
        )
      )
    )
  ))
  (split-by-char-tail 0 "" '())
))

(define join-by-char (lambda (string-list join-char)
  (define join-by-char-tail (lambda (string-list current-string)
    (if (= 1 (length string-list))
      (string-append current-string (car string-list))
      (join-by-char-tail (cdr string-list) (string-append current-string (car string-list) (string join-char)))
    )
  ))
  (join-by-char-tail string-list "")
))

(define starts-with (lambda (find-in query)
  (if (< (string-length find-in) (string-length query))
    #f
    (string=? (substring find-in 0 (string-length query)) query)
  )
))

;65 to 90, 97 to 122
(define is-alphabet (lambda (char)
  (let ([unicode (char->integer char)])
    (or (and (<= 65 unicode) (>= 90 unicode)) (and (<= 97 unicode) (>= 122 unicode)))
  )
))

;first first char or "nonalphabet`
(define get-split-name (lambda (title)
  (let ([first-char (car (string->list title))])
    (if (is-alphabet first-char)
      (string (char-downcase first-char))
      "nonalphabet"
    )
  )
))

(define file-to-bytevector (lambda (file-name)
  (get-bytevector-all (open-file-input-port file-name))
))

;ffi stuff is hard! so I decided to not do it
;https://sourceware.org/bzip2/manual/manual.html
(define decompress (lambda (bytes)
  (let-values ([(stdin stdout _stderr _pid) (open-process-ports "bzip2 --decompress --stdout")])
    (put-bytevector stdin bytes)
    (flush-output-port stdin)
    (close-port stdin)
    (bytevector->string (get-bytevector-all stdout) (make-transcoder (utf-8-codec) (eol-style lf)))
  )
))

#|
;does not lowercase
(define found-in-string (lambda (find-in query)
  (define found-in-string-tail (lambda (index)
    (if (< (string-length find-in) (+ index (string-length query)))
      #f
      ;(display (substring find-in index (+ index (string-length query))))
      (if (string=? (substring find-in index (+ index (string-length query))) query)
        #t
        (found-in-string-tail (+ index 1))
      )
    )
  ))
  (found-in-string-tail 0)
))
|#
