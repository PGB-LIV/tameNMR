
# test for Normalise.R

echo "Testing Normalise.R"
Rscript ../../tameNMR/ProcessSpectra/Normalise.R --input=../test_data/out_aligned.csv --output=../outputs/out_norm_PQN.csv --type=PQN

Rscript ../../tameNMR/ProcessSpectra/Normalise.R --input=../test_data/out_aligned.csv --output=../outputs/out_norm_totArea.csv --type=totInt

Rscript ../../tameNMR/ProcessSpectra/Normalise.R --input=../test_data/out_aligned.csv --output=../outputs/out_norm_refPeak.csv --type=refPeak --param=0.32-0.33

