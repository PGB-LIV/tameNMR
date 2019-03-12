
# test runs for t-test tool

rm -rf ../outputs/ttest_out

Rscript ../../tameNMR/Stats/Ttest.R --input=../test_data/data.csv --output=../outputs/ttest_out.html --outdir=../outputs/ttest_out --factorFile=../test_data/fact.txt --factorCol=1 --tails=two.sided --paired=N --conf_level=0.05


