# CytoChan
Cytogenetic Genomic Analysis in Racket


*** FOR RESEARCH PURPOSES ONLY ***

Mei-Jan Chen, Jacob Cordover, and William E. Byrd

Hugh Kaul Precision Medicine Institute, University of Alabama at Birmingham


Currently works on ClinVar data.

Usage:

First, download the ClinVar summary data in tab-separated value (TSV) format from:

ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz

or from the the web page:

https://www.ncbi.nlm.nih.gov/clinvar/docs/ftp_primer/

Then, uncompress the `variant_summary.txt.gz` file, and place the resulting `variant_summary.txt` file in the same directory as this README file.

Then, run `parse-clinvar.rkt` in Racket to generate `variant_summary_sexpr.rkt`, which is the same data, but in S-expression format.

Then, update `example-genes.rkt` to include the genes of interest.

Finally, load `cytochan.rkt` in Racket to generate an output report in TSV format.
