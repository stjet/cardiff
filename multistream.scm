(load "utils.scm")

;index examples:
;673:96:floccinaucinihilipilification
;673:72:Appendix:English pronunciation
;bytes to seek into:article id:title
(define-record-type index-line (fields byte-seek article-id title))
(define parse-index-line (lambda (line)
  (let ([parts (split-by-char line #\:)])
    (make-index-line (string->number (car parts)) (string->number (car (cdr parts))) (join-by-char (cdr (cdr parts)) #\:))
  )
))
(define index-line->string (lambda (line-record)
  (string-append (number->string (index-line-byte-seek line-record)) ":" (number->string (index-line-article-id line-record)) ":" (index-line-title line-record))
))

;todo: lowercase
;if return-all-matches is #f, return list with one item (exact match) or 0 (not found)
(define index-search (lambda (base-path query return-all-matches)
  (define index-search-tail (lambda (file matches)
    (let ([line (get-line file)])
      (if (eof-object? line)
        matches
        (let* ([line-record (parse-index-line line)] [line-title (index-line-title line-record)])
          (if return-all-matches
            (if (starts-with line-title query)
              (index-search-tail file (cons line-record matches))
              (index-search-tail file matches)
            )
            (if (string=? line-title query)
              (cons line-record '())
              (index-search-tail file matches)
            )
          )
        )
      )
    )
  ))
  (index-search-tail (open-input-file (string-append base-path "/" (get-split-name query) ".txt")) '())
))

(define get-article (lambda (multistream-path base-path line-record)
  ;extract article from the decompressed stream
  ;(decompressed stream contains 100 articles
  ;a little naive
  (define extract-article (lambda (stream-text article-id)
    ;look for <page> with matching page id, and return it
    ;something else can handle only displaying the <revision>, because outside the <revision> there is important info like last edited date
    ;<page>
    ;<title></title>
    ;<ns></ns>
    ;<id></id>
    (define extract-article-tail (lambda (port current-page correct-page)
      (let* (
        [line (strip-leading-whitespace (get-line port))]
      )
        (cond
          [
            (string=? line "<page>")
            (extract-article-tail port "<page>" #f) ;#f or correct-page, doesn't matter
          ]
          [
            (string=? line (string-append "<id>" (number->string article-id) "</id>"))
            (extract-article-tail port (string-append current-page line) #t)
          ]
          [
            (and (string=? line "</page>") correct-page)
            (string-append current-page "</page>")
          ]
          [
            else
            (extract-article-tail port (string-append current-page line "\n") correct-page)
          ]
        )
      )
    ))
    (extract-article-tail (open-string-input-port stream-text) "" #f)
  ))
  ;get size of stream (next offset - offset)
  (define find-stream-size (lambda (byte-seek)
    (define find-stream-size-tail (lambda (file found)
      (let ([line (get-line file)])
        (if found
          (if (eof-object? line)
            0 ;returning 0 means the size is until end of file
            (- (string->number line) byte-seek)
          )
          (find-stream-size-tail file (= (string->number line) byte-seek))
        )
      )
    ))
    (find-stream-size-tail (open-input-file (string-append base-path "/_offsets.txt")) #f)
  ))
  (let* (
    [line-byte-seek (index-line-byte-seek line-record)]
    [multistream-port (open-file-input-port multistream-path)]
  )
    (set-port-position! multistream-port line-byte-seek) ;is there a better way? :(
    ;get the slice of bytes from the offset/byte-seek to the offset + stream size
    ;then pass those bytes into decompress func
    ;finally extract the article from the decompressed text
    (extract-article (decompress (get-bytevector-n multistream-port (find-stream-size line-byte-seek))) (index-line-article-id line-record))
  )
))

