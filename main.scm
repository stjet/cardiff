(load "multistream.scm")
(load "xml-to-term.scm")

;cannot pass in an empty list I guess
(define list-results (lambda (results)
  (display (index-line-title (car results)))
  (if (> (length results) 1)
    (begin
      (newline)
      (list-results (cdr results))
    )
  )
))


(let ([command (car (cdr (command-line)))] [query (join-by-char (cdr (cdr (command-line))) #\space)])
  (cond
    [
      (string=? command "entry")
      (let ([match (index-search "./enwiktionary" query #f)])
        (if (= (length match) 0)
          (display "Exact match not found\n")
          (let ([xml-tree (parse-wiki-xml (get-article "enwiktionary-latest-pages-articles-multistream.xml.bz2" "./enwiktionary" (car match)))])
            ;(display (extract-last-edit xml-tree))
            (display (text-to-term (extract-text xml-tree)))
          )
        )
      )
    ]
    [
      (string=? command "search")
      (let ([matches (index-search "./enwiktionary" query #t)])
        (if (= (length matches) 0)
          (display "No matches found\n")
          (list-results matches)
        )
      )
    ]
    [
      else
      (display "Do `cardiff entry (query)` or `cardiff search (query)`\n")
    ]
  )
)

