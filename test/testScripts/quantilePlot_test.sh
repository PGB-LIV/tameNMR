
# test for quantilePlot.R
rm -rf ../outputs/out_quantiles

echo "Testing QuantilePlot.R"
Rscript ../../tameNMR/Plot/QuantilePlot.R --input=../test_data/out_norm_PQN.csv --outDir=../outputs/out_quantiles --output=../outputs/out_quantiles.html --ppmInt=4-3 --pltMean=Y

