
# test for Bin.R

echo "Testing BinSpectra.R"

Rscript ../../tameNMR/ProcessSpectra/BinSpectra.R --input=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_norm_PQN.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_binned_uniform.csv --method=uniform --binSize=0.05

#Rscript ../../tameNMR/ProcessSpectra/BinSpectra.R --input=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_norm_PQN.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_binned_uniform.csv --method=uniform --pattern=/home/arturas/Projects/Galaxy/tameNMR/tameNMR/test/pattern.txt

#Rscript ../../tameNMR/ProcessSpectra/BinSpectra.R --input=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_norm_PQN.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_binned_uniform.csv --method=uniform --binSize=0.05



