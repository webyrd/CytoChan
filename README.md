# CytoChan
Cytogenetic Genomic Analysis in Racket


Currently works on ClinVar data.

To use, download ClinVar summary data in TSV format (named `variant_summary.txt`) to this directory. Then run `parse-clinvar.rkt` in Racket to generate `variant_summary_sexpr.rkt`, which is the same data, but in S-expression format.

Then load `cytochan.rkt` in Racket to generate an output report in TSV format.