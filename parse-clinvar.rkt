#lang scheme

;; Convert a TSV ClinVar summary file into S-expression format
(let ((source (open-input-file "./variant_summary.txt")))
  (let ((dest (open-output-file "./variant_summary_sexpr.rkt")))
    (write-char #\( dest)
    (write-char #\( dest)
    (write-char #\" dest)
    (let loop ((nextChar (read-char source)))
      (cond
        ((eof-object? nextChar) (begin (write-char #\" dest)  (write-char #\) dest) (write-char #\) dest) (close-output-port dest)))
        ((char=? nextChar #\newline)
         (begin (write-char #\" dest)  (write-char #\) dest) (write-char #\newline dest) (write-char #\newline dest) (write-char #\( dest) (write-char #\" dest) (loop (read-char source))))
        ((char=? nextChar #\tab) (begin (write-char #\" dest) (write-char #\tab dest) (write-char #\" dest) (loop (read-char source))))
        (else (begin
                (write-char nextChar dest)
                (loop (read-char source))))))))
                
