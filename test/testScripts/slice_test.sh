
# test for SliceSpectra.R

echo "Testing SliceSpectra.R"
Rscript ../../tameNMR/ProcessSpectra/SliceSpectra.R --input=../test_data/Out_Import2csv.csv --output=../outputs/out_slice.csv --retainPpm=10-0 --remWater=Y

