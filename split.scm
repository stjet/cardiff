(load "utils.scm")
(load "multistream.scm")

;split index files into many smaller files
;also write all the offsets to a file, in order to get size of one of the compressed stream later on
(define split-index (lambda (index-file output-dir)
  (define split-index-tail (lambda (index-port offset)
    (let ([line (get-line index-port)])
      (if (not (eof-object? line))
        (let* (
          [line-record (parse-index-line line)]
          [line-title (index-line-title line-record)]
          [line-byte-seek (index-line-byte-seek line-record)]
          [index-output-port (open-file-output-port (string-append output-dir "/" (get-split-name line-title) ".txt") (file-options no-fail no-truncate append) (buffer-mode line) (make-transcoder (utf-8-codec) (eol-style lf)))]
          [offset-output-port (open-file-output-port (string-append output-dir "/_offsets.txt") (file-options no-fail no-truncate append) (buffer-mode line) (make-transcoder (utf-8-codec) (eol-style lf)))]
        )
          (display (index-line->string line-record) index-output-port)
          (newline index-output-port)
          (close-port index-output-port)
          (if (not (= offset line-byte-seek))
            (begin
              (display (number->string line-byte-seek) offset-output-port)
              (newline offset-output-port)
              (close-port offset-output-port)
            )
          )
          (split-index-tail index-port line-byte-seek)
        )
      )
    )
  ))
  (split-index-tail (open-input-file index-file) 0)
))
