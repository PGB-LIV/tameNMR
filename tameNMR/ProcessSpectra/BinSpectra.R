
# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Binning of NMR spectra

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --method=method - (uniform, intelligent, custom) the intervals of ppm to retain
      --binSize=sizeInPpm - (optional, required when method=uniform)
      --pattern=path - (optional, required when method=custom) path to a pattern file

      Example:
      ./BinSpectra.R --input=inputFilePath --output=outputFilePath --method=uniform --binSize=0.05\n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]


# ----------------- functions -------------------------------

readNMRTabFile <- function(inpath){
  data = read.table(inpath, header=T, sep='\t', stringsAsFactors = F)
  data = as.matrix(data)
  data
}



# -------------------- Functions for binning NMR spectra --------------------

# -- Uniform binning
uniformBinning <- function(data, ppms, wndw){

  # find the interval width in points
  windowInP <- ceiling( wndw / abs(ppms[1] - ppms[2]) )

  # generate the interval starts/ends
  starts <- seq(1, nrow(data), by=windowInP)
  ends <- seq(windowInP, nrow(data), by=windowInP)
  if(length(starts) != length(ends)) starts = starts[1:length(ends)]
  binSize = ends[1] - starts[1] + 1

  # bin data (ppm values are averages within each interval)
  dataColNames = names(data)
  data = as.matrix(data) * 1.0
  dataBinned = do.call('rbind', lapply(1:length(starts), function(i) apply(data[starts[i]:ends[i],],2,sum)/binSize))

  ppmBins = paste(round(ppms[starts],4), round(ppms[ends],4), sep='-')

  allBinned = as.data.frame(t(dataBinned))
  rownames(allBinned) <- dataColNames
  names(allBinned) <- ppmBins
  allBinned
}

# -- Pattern file-based binning
customBinning <- function(data, ppms, pattern){
  ppmInterval2Pos <- function(ppms, interval){
    bin = which(ppms>=min(interval) & ppms<=max(interval))
    c(min(bin),max(bin))
  }

  fixDupes = function(labels, dupes){
    unis = unique(labels[dupes])
    for(lab in unis){
      pos = which(labels == lab)
      labels[pos] <- paste(lab, '_', 1:length(pos), sep='')
    }
    labels
  }

  # find and modify duplicated bin labels
  dupes = duplicated(pattern[,3])
  if (any(dupes))
    pattern[,3] = fixDupes(pattern[,3], which(dupes))

  data = data * 1.0
  #convert ppms to positions in the data matrix
  bins = do.call('rbind', lapply(1:nrow(pattern), function(i) ppmInterval2Pos(ppms, pattern[i,1:2])))
  binSize = abs(bins[,2] - bins[,1]) + 1
  dataInt = do.call('cbind', lapply(1:nrow(bins), function(i) apply(data[bins[i,1]:bins[i,2],], 2, sum)/binSize[i]))
  dataInt = as.data.frame(dataInt)

  dupes = duplicated(colnames(data))
  if (any(dupes)){
    tempNames = fixDupes(colnames(data), which(dupes))
  } else {
    tempNames = colnames(data)
  }

  rownames(dataInt) = tempNames
  names(dataInt) = pattern[,3]
  dataInt
}

# -- Intelligent Binning (paper)
intelligentBinning <- function(data){
  # TODO
  print('Intelligent binning is not yet implemented')
  data[1:2,1:2]
}
# -------------------------------------------------------------------

# -- read data
data = readNMRTabFile(args[['input']])

# -- bin the spectra
if('method' %in% names(args)){
  if (args[['method']]=='uniform'){
    if ('binSize' %in% names(args)) {
      binSize = as.numeric(args[['binSize']])
      data = uniformBinning(data[,2:ncol(data)], data[,1], binSize)}
    else {
      binSize = 0.02 # default value
      data = uniformBinning(data[,2:ncol(data)], data[,1], binSize)}
  } else if (args[['method']] == 'custom'){
    if('pattern' %in% names(args)){
      patternFile = read.table(args[['pattern']], sep="\t", header=F, stringsAsFactors = F)
      data = customBinning(data[,2:ncol(data)], data[,1], patternFile)
    } else {
      data = intelligentBinning(data)
    }
  } else if (args[['method']] == 'intelligent'){
    data = intelligentBinning(data)
  } else {
    cat(sprintf('Method %s is not available. Try --help \n', args[['method']]))
  }
}
# -- write the output
write.table(data, file=args[['output']], row.names=T, col.names=T, sep='\t')
