
# test for PeakPick.R

echo "Testing peakPick.R"
Rscript ../../tameNMR/ProcessSpectra/PeakPick.R --input=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_slice.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_peaks.csv

#Rscript ../../tameNMR/Stats/Ttest.R --input=../test_data/data.csv --output=../outputs/ttest_out_ --outdir=../outputs/ttest_out --factorFile=../test_data/fact.csv --factorCol=1 --tails=two.sided --paired=N --conf_level=0.05


