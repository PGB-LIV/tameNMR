
# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Normalization of NMR spectra

      Arguments:
      --inData=path - input file path
      --inPeaks=path - input file path
      --output=path - output file path

      Example:
      ./Align.R --inData=inputFilePath --inPeaks=peaksFilePath --output=outputFilePath\n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

suppressMessages(library(speaq))
# ----------------- functions -------------------------------

readNMRTabFile <- function(inpath){
  data = read.table(inpath, header=T, sep='\t', stringsAsFactors = F)
  data = as.matrix(data)
  data
}

writeNMRTabFile <- function(data, outpath){
  write.table(data, file=outpath, row.names=F, col.names=T, sep='\t')
}

# Convert ppm peak positions to positions in the array
ppm2pos <- function(peakList, ppms){
  getPos <- function(peak) which.min(abs(ppms-peak))
  do.call('c', lapply(peakList, getPos))
}

RangeScale<-function(x){
  if(max(x) == min(x)){
    x;
  }else{
    (x - mean(x))/(max(x)-min(x));
  }
}
# ---------------------------------------------------------

# -- Read data and peaks files
data <- readNMRTabFile(args[['inData']])
peaks <- readNMRTabFile(args[['inPeaks']])

# -- convert peaks file to list
peakList <- as.list(as.data.frame(peaks))
peakList <- lapply(peakList, function(x) x[!is.na(x)])
peakList <- lapply(peakList, function(x) ppm2pos(x, data[,1]))

# -- align the spectra
resFindRef <- findRef(peakList)
refInd <- resFindRef$refInd
cat("\n Order of spectrum for reference \n")

for (i in 1:length(resFindRef$orderSpec)) {
  cat(paste(i, ":",resFindRef$orderSpec[i],sep=""), " ")
  if (i %% 10 == 0) cat("\n")
}

data_ = data[,2:ncol(data)]
data_ = apply(data_, 2, RangeScale) * 100

# alignment through clustering
maxShift = 50;
Y <- dohCluster(t(data_),
                peakList = peakList,
                refInd = refInd,
                maxShift = maxShift,
                acceptLostPeak = TRUE, verbose=FALSE)
#list(data=Y,peakList=peakList)
Y <- t(Y)

# -- Write the results
writeNMRTabFile(cbind(data[,1],Y), args[['output']])
