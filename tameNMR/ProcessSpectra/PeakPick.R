#!/usr/bin/env Rscript

# NMR data normalization tool for Galaxy

# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Peak-picking of NMR spectra

      Arguments:
      --input=path - input file path
      --output=path - output file path

      Example:
      ./PeakPick.R --input=inputFilePath --output=outputFilePath\n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

suppressMessages(library(speaq))
#source('lib.R')

# ----------------- functions -------------------------------

readNMRTabFile <- function(inpath){
  data = read.table(inpath, header=T, sep='\t', stringsAsFactors = F)
  data = as.matrix(data)
  data
}

writeNMRPeakFile <- function(data, outpath){
  write.table(data, file=outpath, row.names=F, col.names = T, sep='\t')
}

# Window function that finds a decent baseline threshold for minimum peak levels
min_window = function(data, wwidth=100){
  n_points = nrow(data)
  starts = seq(1, n_points, by=round(wwidth/5))
  starts = starts[(starts+wwidth) < nrow(data)]
  windows = do.call(rbind, lapply(starts, function(x) colMeans(data[x:(x+wwidth-1),])))
  idx_min = which.min(rowMeans(windows))
  max(windows[idx_min,])
}

# ---------------------------------------------------------

# --- Read the data ---
data = readNMRTabFile(args[['input']])

# -- Perform peak picking
#baselineThresh <- 10 * max(apply(abs(data[1:200, 2:ncol(data)]),1,max)) / 10^round(log10(mean(data[1:200,2:ncol(data)])))
#X <- t(data[,2:ncol(data)]) / 10^round(log10(mean(data[1:200,2:ncol(data)])))
X = data[,2:ncol(data)]
#X = X / 10^round(log10(mean(X)))
baselineThresh = 5 * min_window(X)

#SCALECONST = 10^round(log10(mean(data[1:200,])))
#X <- t(data[,2:ncol(data)])  / SCALECONST
#baselineThresh <- 2 * max(apply(abs(X[,1:200]),2,max))
peakList <- detectSpecPeaks(t(X),
                            nDivRange = c(128),
                            scales = seq(1, 16, 2),
                            baselineThresh = baselineThresh,
                            SNR.Th = -1,
                            verbose=FALSE)

peakList <- lapply(peakList, function(x) data[x,1])
len <- sapply(peakList, length)
len <- max(len) - len
peaks <- mapply(function(x,y) c(x,rep(NA,y)), peakList, len)
colnames(peaks) <- colnames(data)[2:ncol(data)]

# --- Write the results
writeNMRPeakFile(as.data.frame(peaks), args[['output']])
