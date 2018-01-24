
#!/usr/bin/Rscript

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

  data_ = t(data)
  reference <- apply(data_,1,locFunc)
  # sometimes reference produces 0s so we turn them into 1s before division
  # so spectrum stays unchanged
  reference[reference==0] <- 1

  quotient <- data_/reference
  quotient.withLocFunc <- apply(quotient,2,locFunc)

  pqn.data <- t(t(data_)/quotient.withLocFunc)
  pqn.data
}

# normalisation to total sum
totInt <- function(data) {
  
  meanInt = sum(apply(data,2,mean))
  scalingFactor = meanInt / apply(data,1,sum)
  data = data * scalingFactor
  data
}

# normalisation to reference peak
refPeak <- function(data, par) {
 # Normalisation by selected peak
  normPeak = data[,par]
  data = data / normPeak * mean(normPeak)
  data
}

# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Normalisation of a data table

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --type=type - the type of normalisation:
                PQN: (default) probabilistic quatient normalization
                totInt: normalization by the total sum
                refPeak: normalization by reference peak value

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

colnames(dataTemp) = names(data)
rownames(dataTemp) = rownames(data)

# --- Write the data
write.table(data, file=outpath, row.names=T, col.names=T, sep='\t')
