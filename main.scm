(load "multistream.scm")
(load "xml-to-html.scm")

;any arguments passed become article title to do exact search for
(let ([xml-tree (parse-wiki-xml (get-article "enwiktionary-latest-pages-articles-multistream.xml.bz2" "./enwiktionary" (car (index-search "./enwiktionary" (join-by-char (cdr (command-line)) #\space) #f))))])
  (display xml-tree)
  (display (extract-last-edit xml-tree))
)
