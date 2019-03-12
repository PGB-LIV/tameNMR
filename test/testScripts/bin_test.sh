
# test for Bin.R

echo "Testing BinSpectra.R"
echo "testing uniform binning"
Rscript ../../tameNMR/ProcessSpectra/BinSpectra.R --input=../test_data/out_norm_PQN.csv --output=../outputs/out_binned_uniform.csv --pattern=../test_data/bins_uniform.csv

echo "testing binning with bruker bin table"
Rscript ../../tameNMR/ProcessSpectra/BinSpectra.R --input=../test_data/out_norm_PQN.csv --output=../outputs/out_binned_bruker.csv --pattern=../test_data/bins_bruker.csv

echo "testing binning with custom bin table"
Rscript ../../tameNMR/ProcessSpectra/BinSpectra.R --input=../test_data/out_norm_PQN.csv --output=../outputs/out_binned_fromCsv.csv --pattern=../test_data/bins_fromCsv.csv

#echo "testing intelligent binning"
#Rscript ../../tameNMR/ProcessSpectra/BinSpectra.R --input=../test_data/out_norm_PQN.csv --output=../outputs/out_binned_intelligent.csv --pattern=../test_data/bins_intelligent.csv




