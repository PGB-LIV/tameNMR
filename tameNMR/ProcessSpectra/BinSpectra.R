
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
      --pattern=path - path to a pattern file

      Example:
      ./BinSpectra.R --input=inputFilePath --output=outputFilePath --pattern=path \n\n")
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

# -- Pattern file-based binning
patternBasedBinning <- function(data, ppms, pattern){
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
# -------------------------------------------------------------------

# -- read data
if('input' %in% names(args)){
data = readNMRTabFile(args[['input']])
} else stop('Data file not given. See ./BinPattern --help')

if('pattern' %in% names(args)){
  patternFile = read.table(args[['pattern']], sep="\t", header=F, stringsAsFactors = F)
} else stop('Pattern file not given. See ./BinPattern --help')
  
data = patternBasedBinning(data[,2:ncol(data)], data[,1], patternFile)

# -- write the output
write.table(data, file=args[['output']], row.names=T, col.names=T, sep='\t')
