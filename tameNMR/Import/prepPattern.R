
# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Formatting of pattern files

      Arguments:
      --method=format - {brukerPattern, csvTable, uniform, intelligent}
      --dataSet=path - input data path (required for uniform and intelligent binning)
      --pattern=path - path to the pattern file (required for csvTable and uniform binning)
      --output=path - output bin table file path
      --binSize=0.02 - width of the bins in ppm (only for uniform binning)

      Example:
        ./prepPattern.R --method=uniform --output=pathToOutput  --dataSet=pathToData --binSize=0.02")
  
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

suppressMessages(require(speaq))
suppressMessages(require(rSFA))
# -------------------- Functions for bin tables for NMR spectra --------------------

# -- Uniform binning
makeUniformBinTable <- function(dataSet, wndw){

  ppms = dataSet[,1]
  data = dataSet[,2:ncol(dataSet)]
  # find the interval width in points
  windowInP <- ceiling( wndw / abs(ppms[1] - ppms[2]) )

  # generate the interval starts/ends
  starts <- seq(1, nrow(data), by=windowInP)
  ends <- seq(windowInP, nrow(data), by=windowInP)
  if(length(starts) != length(ends)) starts = starts[1:length(ends)]

  ppmBins = data.frame(left = round(ppms[starts],4),
                       right = round(ppms[ends],4),
                       binLabel = as.character(round((ppms[starts]+ppms[ends])/2,3)),
                       stringsAsFactors = F)
  ppmBins
}

# -- Intelligent Binning (paper)
makeIntelliBinTable <- function(data){
  data_ = data[,2:ncol(data)]
  meanSpec = apply(data_,1,median)
  
  #Detect peaks
  baselineThresh <- 4 * abs(max(meanSpec[1:200])) # determine threshold
  peakList <- detectSpecPeaks(matrix(meanSpec,nrow = 1),
                              nDivRange = c(128),
                              scales = seq(1, 16, 2),
                              baselineThresh = baselineThresh,
                              SNR.Th = -1,
                              verbose=FALSE)

  # find peak boarders
  ## finding derivatives
  deriv = sfaTimediff(matrix(meanSpec,ncol=1))
  
  ## finding peak edges as the nearest 0 in the derivatives 
  ## on both sides of the peak position
  find.peak.edges = function(peak, derivs){
    zeros = which(abs(derivs)<=0.005)
    leftEdge = min(peak-zeros[zeros<peak])
    rightEdge = min(zeros[zeros>peak]-peak)
    c(peak-leftEdge, peak+rightEdge)
  }
  
  bins = do.call('rbind', lapply(peakList[[1]], function(x) c(x, find.peak.edges(x, deriv))))
  bins = cbind(data[bins[,2],1], data[bins[,3],1], round((data[bins[,2],1]+data[bins[,3],1])/2,3))
  bins = as.data.frame(bins, stringsAsFactors=F)
  colnames(bins) = c('left', 'right', 'binLabel')
  bins
}

# -- Bruker pattern file
readBrukerPatternFile <- function(path){
  pf = read.table(path, sep="", header=F, skip=9, stringsAsFactors = F)
  pf = pf[,c(3,4,6)]
  colnames(pf) = c('left', 'right', 'binLabel')
  pf
}

# -- csv table pattern file
readcsvTable = function(path){
  pattern = read.csv(args[['input']], header=F, stringsAsFactors = F, sep=',')
  if(ncol(pattern) < 3) stop('Invalid csv table. Make sure that the table has no headers, 3 columns and is comma-delimited')
  else {
    colnames(pattern) = c('left', 'right', 'binLabel') 
    return(pattern)
    }
}

readNMRTabFile <- function(inpath){
  data = read.table(inpath, header=T, sep='\t', stringsAsFactors = F)
  data = as.matrix(data)
  data
}
# ================================================== 

if (args[['method']] == "brukerPattern"){
  if('pattern' %in% names(args)) pattern = readBrukerPatternFile(args[['pattern']])
} else if (args[['method']] == 'csvTable'){
  if('pattern' %in% names(args)) pattern = readcsvTable(args[['pattern']])
} else if (args[['method']] == 'uniform'){
  if('binSize' %in% names(args) & 'dataSet' %in% names(args)) {
    data = readNMRTabFile(args[['dataSet']])
    pattern = makeUniformBinTable(data, as.numeric(args[['binSize']]))
    } else stop('Not enough parameters. See ./prepPattern --help')
} else if (args[['method']] == 'intelligent'){
  if('dataSet' %in% names(args)) {
    data = readNMRTabFile(args[['dataSet']])
    pattern = makeIntelliBinTable(data)
    } else stop('Not enough parameters. See ./prepPattern --help')
} else {
  stop(sprintf('No such method %s \n', args[['method']]))
}

write.table(pattern, file=args[['output']], col.names = F, row.names = F, sep='\t')