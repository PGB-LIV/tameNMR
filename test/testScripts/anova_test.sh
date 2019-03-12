
# test runs for the anova tool
rm -rf ../outputs/anova_out

Rscript ../../tameNMR/Stats/Anova.R --input=../test_data/data.csv --output=../outputs/anova_out.html --outdir=../outputs/anova_out --factorFile=../test_data/fact.txt --factorCol=2 --conf_level=0.05 --adjust=BH

