
# test runs for the PCA tool

Rscript ../../tameNMR/Stats/PCA.R --input=../test_data/data.csv --output=../outputs/pca_out --outdir=../outputs/pca_out --factorFile=../test_data/fact.csv --factorCol=2 --pcs="1-2,2-3,1-3" --scale=Y --showScores=Y --showLoadings=Y --showVarAcc=Y

