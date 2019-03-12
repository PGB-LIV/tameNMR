
# test runs for the PLS-DA tool

rm -rf ../outputs/plsda_out

Rscript ../../tameNMR/Stats/PLSDA.R --input=../test_data/data.csv --output=../outputs/plsda_out.html --outdir=../outputs/plsda_out --factorFile=../test_data/fact.txt --factorCol=2

