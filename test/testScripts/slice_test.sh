
# test for SliceSpectra.R

echo "Testing SliceSpectra.R"
Rscript ../../tameNMR/ProcessSpectra/SliceSpectra.R --input=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/Out_Import2csv.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_slice.csv --retainPpm=10-0 --remWater=Y

#Rscript ../../tameNMR/Stats/Ttest.R --input=../test_data/data.csv --output=../outputs/ttest_out_ --outdir=../outputs/ttest_out --factorFile=../test_data/fact.csv --factorCol=1 --tails=two.sided --paired=N --conf_level=0.05


