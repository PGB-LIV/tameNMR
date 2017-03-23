
# test for quantilePlot.R

echo "Testing QuantilePlot.R"
Rscript ../../tameNMR/Plot/QuantilePlot.R --input=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_norm_PQN.csv --outDir=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_quantiles --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_quantiles.html --ppmInt=4-3 --pltMean=Y

