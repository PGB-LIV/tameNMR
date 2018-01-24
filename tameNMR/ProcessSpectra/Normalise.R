#!/usr/bin/env Rscript

# NMR data normalization tool for Galaxy


# --- Normalisation functions ---
PQN <- function(data, loc = "median"){
  if (loc == "mean") {
    locFunc <- mean
  } else if (loc == 'median'){
    locFunc <- median
  } else {
    cat(sprintf("non such location metric %d", loc))
  }

  #if(ncol(data)>nrow(data)) data <- t(data)
  #data = abs(data)
  data_ = data[,2:ncol(data)]
  reference <- apply(data_,1,locFunc)
  # sometimes reference produces 0s so we turn them into 1s before division
  # so spectrum stays unchanged
  reference[reference==0] <- 1

  quotient <- data_/reference
  quotient.withLocFunc <- apply(quotient,2,locFunc)

  pqn.data <- t(t(data_)/quotient.withLocFunc)
  cbind(data[,1],pqn.data)
}

# normalisation to total integral
totInt <- function(data) {
  data_ = data[,2:ncol(data)]
  
  meanInt = trapezoid(apply(abs(data_),1,mean))
  scalingFactor = meanInt / apply(abs(data_),2,trapezoid)
  data_ = t(t(data_) * scalingFactor)
  data_ = cbind(data[,1],data_)
  #colnames(data_) = names(data)
  data_
}

# normalisation to reference peak
refPeak <- function(data, par) {
  data_ = data[,2:ncol(data)]
  bin = as.numeric(strsplit(par,'-')[[1]])
  refPeakInt = apply(data_[data[,1] >= min(bin) & data[,1] <= max(bin),], 2, trapezoid)
  # adjust the integral for mean peak to preserve scale of spectra in the dataset
  refPeaksAdj = refPeakInt/mean(refPeakInt)
  data_ = t(t(data_)/refPeaksAdj)
  data_ = cbind(data[,1], data_)
  #colnames(data_) = names(data)
  data_
}

trapezoid = function(vect){
  sum(vect[1:(length(vect)-1)] + vect[2:length(vect)] ) / (2 * (length(vect)-1))
}

writeNMRTabFile <- function(data, outpath){
  write.table(data, file=outpath, row.names=F, col.names=T, sep='\t')
}

# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Normalisation of NMR spectra

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --type=type - the type of normalisation:
                PQN: (default) probabilistic quatient normalization
                totInt: normalization by the total integral
                refPeak: normalization by reference peak integral

      Example:
      ./Normalise.R --input=inputFilePath --output=outputFilePath --type=PQN --param=\n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x), "=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# --- Read the data ---
data = read.table(args[['input']], header=T, sep='\t', stringsAsFactors = F)
data_ = as.matrix(data)

# --- Normalise the data ---
if('type' %in% names(args)){
  normFunc = switch(args[['type']],
                    "PQN" = PQN,
                    "totInt" = totInt,
                    "refPeak" = refPeak)
} else {
  normFunc = PQN
}

if (args[['type']] == 'refPeak'){
  dataTemp = refPeak(data_, args[['param']])
} else { dataTemp = normFunc(data_) }

#names(dataTemp) = names(data)[2:ncol(data)]
#data_ = cbind(data[,1], dataTemp)
colnames(dataTemp) = names(data)

# --- Write the data
writeNMRTabFile(dataTemp, args[['output']])
