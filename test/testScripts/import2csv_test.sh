
# test for import2csv.py
echo "Testing import2csv.py"
python ../../tameNMR/Import/import2csv.py /home/arturas/Projects/Galaxy/tameNMR/test/test_data/CPMG_exp.zip /home/arturas/Projects/Galaxy/tameNMR/test/outputs/import2csvOut.csv data

#Rscript ../../tameNMR/Stats/Ttest.R --input=../test_data/data.csv --output=../outputs/ttest_out_ --outdir=../outputs/ttest_out --factorFile=../test_data/fact.csv --factorCol=1 --tails=two.sided --paired=N --conf_level=0.05


