
# test for Normalise.R

echo "Testing Normalise.R"
Rscript ../../tameNMR/ProcessSpectra/Normalise.R --input=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_aligned.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_norm_PQN.csv --type=PQN

Rscript ../../tameNMR/ProcessSpectra/Normalise.R --input=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_aligned.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_norm_totArea.csv --type=totInt

Rscript ../../tameNMR/ProcessSpectra/Normalise.R --input=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_aligned.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_norm_refPeak.csv --type=refPeak --param=0.32-0.33


#Rscript ../../tameNMR/Stats/Ttest.R --input=../test_data/data.csv --output=../outputs/ttest_out_ --outdir=../outputs/ttest_out --factorFile=../test_data/fact.csv --factorCol=1 --tails=two.sided --paired=N --conf_level=0.05


