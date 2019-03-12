
# test for Align.R

echo "Testing align.R"
Rscript ../../tameNMR/ProcessSpectra/Align.R --inData=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_slice.csv --inPeaks=/home/arturas/Projects/Galaxy/tameNMR/test/test_data/out_peaks.csv --output=/home/arturas/Projects/Galaxy/tameNMR/test/outputs/out_aligned.csv --retainPpm=10-0 --remWater=Y

