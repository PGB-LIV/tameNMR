
# test runs for the PCA tool
rm -rf ../outputs/pca_out

Rscript ../../tameNMR/Stats/PCA.R --input=../test_data/data.csv --output=../outputs/pca_out.html --outdir=../outputs/pca_out --factorFile=../test_data/fact.txt --factorCol=2 --pcs="1-2,2-3,1-3" --scale=Y --showScores=Y --showLoadings=Y --showVarAcc=Y

