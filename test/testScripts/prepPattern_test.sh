
# test for prepPattern.R

echo "Testing PrepPattern.R"
echo "..making uniform bin table"
Rscript ../../tameNMR/Import/prepPattern.R --method=uniform --dataSet=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_norm_PQN.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/bins_uniform.csv --binSize=0.05

echo "..making bin table from Bruker pattern file"
#Rscript ../../tameNMR/Import/prepPattern.R --method=brukerPattern --pattern=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/brukerPattern.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/bins_bruker.csv

echo "..making bin table from csv table"
#Rscript ../../tameNMR/Import/prepPattern.R --method=csvTable --pattern=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/customPattern.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/bin_fromcsv.csv

echo "..making intelligent bin table"
Rscript ../../tameNMR/Import/prepPattern.R --method=intelligent --dataSet=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_norm_PQN.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/bins_intelligent.csv




