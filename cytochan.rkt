#lang racket

(provide 
 clinVar
 cng
 cngClinVarSplit
 chrom9cngClinVar
 chrom9InRangecngClinVar
 write-tsv
 dangerousChrom9InRangecngClinVar
 mostlyHarmlessChrom9InRangecngClinVar
 example-genes
 genes-with-evidence-of-benignness
 genes-with-evidence-of-dangerousness&benignness
 )

(require
  "example-genes.rkt")


(define clinVar
  (read (open-input-file "./variant_summary_sexpr.rkt")))



(display "loaded clinVar")
(newline)

;; copy number gain
(define cng (filter (lambda (e) (and (list? e) (>= (length e) 2) (equal? (cadr e) "copy number gain"))) clinVar))

(display "filtered cng")
(newline)

(define cngClinVarSplit
  (cons (car clinVar)
        (map (lambda (e)
               (match e
                 [`(,_1 ,_2 ,_3 ,_4 ,gene-names . ,rest)
                  (let ((list-of-gene-name (regexp-split #rx";" gene-names)))
                    `(,_1 ,_2 ,_3 ,_4 ,list-of-gene-name . ,rest))]
                 [else e]))
             (cdr cng))))

(display "split cng")
(newline)







(define chrom9cngClinVar
  (filter
   (lambda (e)
     (let ((chromosome (list-ref e 18)))
       (equal? "9" chromosome)))
   cng))


(define chrom9InRangecngClinVar
  (filter
   (lambda (e)
     (let ((start (list-ref e 19))
           (end (list-ref e 20)))
       (or
        ;; overlap the beginning
        (and (< (string->number start) 203861)
             (>= (string->number end) 203861))
        ;;completely contained in the range
        (and (>= (string->number start) 203861)
             (<= (string->number end) 68330127))
        ;; overlap the end
        (and (< (string->number start) 68330127)
             (>= (string->number end) 68330127))
        )))
   chrom9cngClinVar))

(define write-tsv
  (lambda (db)
    (let ((op (open-output-file "./report.tsv" #:exists 'replace)))
      (for-each
       (lambda (e)
         (for-each
          (lambda (v)           
            (write v op)
            (write-char #\tab op))
          e)
         (newline op))
       db)
      (close-output-port op))))

(write-tsv chrom9InRangecngClinVar)


(define dangerousChrom9InRangecngClinVar (filter (lambda (e)
                                                   (or 
                                                    (equal? (list-ref e 6) "Pathogenic")
                                                    (equal? (list-ref e 6) "Likely pathogenic")))
                                                 chrom9InRangecngClinVar))

(define mostlyHarmlessChrom9InRangecngClinVar (filter (lambda (e)
                                                        (or 
                                                         (equal? (list-ref e 6) "Benign")
                                                         (equal? (list-ref e 6) "Likely benign")))
                                                      chrom9InRangecngClinVar))

(define genes-with-evidence-of-benignness
  (map
   (lambda (gene)
     (let ((gene-start (list-ref gene 1))
           (gene-end (list-ref gene 2)))
       (let ((benign-variants
              (filter
               (lambda (e)
                 (let ((variant-start (string->number (list-ref e 19)))
                       (variant-end  (string->number (list-ref e 20))))
                   (and (> gene-start variant-start)
                        (< gene-end variant-end))))
               mostlyHarmlessChrom9InRangecngClinVar)))
         (if (null? benign-variants)
             (list gene "no benign or likely benign variants containing gene")
             (list gene benign-variants)))))
   example-genes))

(map (lambda (ls) (list (car ls) (if (list? (cadr ls)) (length (cadr ls)) (cadr ls)))) genes-with-evidence-of-benignness)




(define genes-with-evidence-of-dangerousness&benignness
  (map
   (lambda (gene)
     (let ((gene-start (list-ref gene 1))
           (gene-end (list-ref gene 2)))
       (let ((dangerous-variants
              (filter
               (lambda (e)
                 (let ((variant-start (string->number (list-ref e 19)))
                       (variant-end  (string->number (list-ref e 20))))
                   (and (> gene-start variant-start)
                        (< gene-end variant-end))))
               dangerousChrom9InRangecngClinVar))
             (benign-variants
              (filter
               (lambda (e)
                 (let ((variant-start (string->number (list-ref e 19)))
                       (variant-end  (string->number (list-ref e 20))))
                   (and (> gene-start variant-start)
                        (< gene-end variant-end))))
               mostlyHarmlessChrom9InRangecngClinVar)))
         (append gene (list
                        (if (null? dangerous-variants)
                           "no dangerous or likely dangerous variants containing gene"
                           dangerous-variants)
                       (if (null? benign-variants)
                           "no benign or likely benign variants containing gene"
                           benign-variants))))))
   example-genes))


(map (lambda (ls) (list (car ls)
                        (if (list? (list-ref ls 3)) (length (list-ref ls 3)) (list-ref ls 3))
                        (if (list? (list-ref ls 4)) (length (list-ref ls 4)) (list-ref ls 4))
                        ))
     genes-with-evidence-of-dangerousness&benignness)
