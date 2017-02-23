
# test runs for the anova tool

Rscript ../../tameNMR/Stats/Anova.R --input=../test_data/data.csv --output=../outputs/anova_out --outdir=../outputs/anova_out --factorFile=../test_data/fact.csv --factorCol=2 --conf_level=0.05 --adjust=BH

