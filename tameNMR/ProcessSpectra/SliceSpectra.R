
# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Slicing of NMR spectra

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --retainPpm=0-4,6-10 - the intervals of ppm to retain
      --remWater=Y - should the water signal (4.3-4.9 ppm) be removed

      Example:
      ./SliceSpectra.R --input=inputFilePath --output=outputFilePath --reatainPpm=0-4,6.5-10 --remWater=Y\n\n")
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

# Convert ppm peak positions to positions in the array
ppm2pos <- function(peakList, ppms){
  getPos <- function(peak) which.min(abs(ppms-peak))
  do.call('c', lapply(peakList, getPos))
}

parsePpmIntervals <- function(ppmInts){
  intervals <- strsplit(ppmInts, ',')[[1]]
  if (length(intervals) == 1){
    intervals = as.numeric(strsplit(intervals,'-')[[1]])
  } else {
    intervals <- do.call('rbind', lapply(intervals, function(x) strsplit(x,'-')[[1]]))
    intervals <- cbind(as.numeric(intervals[,1]),as.numeric(intervals[,2]))
    intervals <- do.call('rbind', lapply(1:nrow(intervals), function(i) { if(intervals[i,1] > intervals[i,2]) {rev(intervals[i,])} else {intervals[i,]}} ))
    intervals <- merge.Ints(intervals)
  }
  intervals
}

merge.Ints = function(inMat){
  # merge overlapping intervals
  inMat = do.call('rbind', lapply(1:nrow(inMat), function(x) sort(inMat[x,])))
  inMat = inMat[order(inMat[,1]),]
  out = matrix(nrow=0, ncol=2)
  
  curr = inMat[1,]
  for(i in 2:(nrow(inMat))){
    if(is.overlap(curr, inMat[i,])) curr = merge2Ints(curr, inMat[i,])
    else {
      out = rbind(out,curr)
      curr = inMat[i,]
    }
  } 
  out = rbind(out, curr) # add last curr to the result
  unname(out)
}

is.overlap = function(x,y){
  # checks if two 2d vectors overlap
  if (any(is.na(x)) | any(is.na(y))) return(FALSE)
  else{
    if (x[2] < x[1]) x = c(x[2],x[1])
    if (y[2] < y[1]) y = c(y[2],y[1])
    return(!((x[2] < y[1]) | (y[2] < x[1])))
  }
}

merge2Ints = function(x,y){
  # merge two 2d intervals
  tmp = c(x,y)
  c(min(tmp), max(tmp)) 
}

# ---------------------------------------------------------

# -- read data
data <- readNMRTabFile(args[['input']])

# -- parse the intervals and subset the data
intervals <- parsePpmIntervals(args[['retainPpm']])

if(args[['remWater']]=='Y') data = data[!(data[,1] <= 4.9 & data[,1] >= 4.3),]


if (!is.null(dim(intervals))){
  data_ = do.call('rbind', lapply(intervals, function(x) data[data[,1] >= min(x) & data[,1] <= max(x),]))
  colnames(data_) = c('ppm', colnames(data)[2:ncol(data)])
} else {
  data_ = data[data[,1] >= min(intervals) & data[,1] <= max(intervals),]
  colnames(data_) = c('ppm', colnames(data)[2:ncol(data)])
}


# -- write the data
write.table(data_, file=args[['output']], row.names=F, col.names=T, sep='\t')
