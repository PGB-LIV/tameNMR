#!/usr/bin/env Rscript

# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Global alignment tool

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --alignTo=Glucose - peak to align to (currently only Glucose available)

      Example:
      ./GlobalAlign.R --input=inputFilePath --output=outputFilePath --alignTo=path \n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# ================= Functions ======================
min_window = function(data, wwidth=100){
  n_points = nrow(data)
  starts = seq(1, n_points, by=round(wwidth/5))
  starts = starts[(starts+wwidth) < nrow(data)]
  windows = do.call(rbind, lapply(starts, function(x) colMeans(data[x:(x+wwidth-1),])))
  idx_min = which.min(rowMeans(windows))
  max(windows[idx_min,])
}

# ==================== Global spectra alignment ====================


GlobalAlign = function(data, peak2align){

  low_ppm = peak2align[1]
  high_ppm = peak2align[2]
  pos_ppm = peak2align[3]
  isDoublet = peak2align[4] == 'doublet'
  
  data_ = as.matrix(data[,2:ncol(data)])
  ppms = data[,1]
  # alignment
  low = which.min(abs(ppms-low_ppm))
  high = which.min(abs(ppms-high_ppm))
  
  outList = alignGlucoseLeftPeak(data_, ppms, posPPM = pos_ppm, low, high)

  out = cbind(outList$ppms, outList$data)
  out
}

findLeftDoubletPeak <- function(pList, spec){
  amplitudes = spec[pList]
  maxPeakPos = which.max(amplitudes)
  maxPeakPos2 = which.max(amplitudes[-maxPeakPos])
  min(pList[maxPeakPos],pList[maxPeakPos2])
}

offsetSpectra = function(spec, ppms, posOfPeak, targetPos=5.20475){
  currPosPpm = ppms[posOfPeak]
  offset = targetPos - currPosPpm
  ppmsTemp = ppms+offset
  out = spec[ppmsTemp>=min(ppms) & ppmsTemp<=max(ppms)]
  pointOffset = length(spec) - length(out)
  out = c(rep(1,pointOffset), out)
  out
}

suppressMessages(require(speaq))
alignDoubletLeftPeak = function(data, ppms, posPPM, low, high){
  # aligns left doublet peak to the given ppm value
  # data - a dataframe with data in columns
  # ppms - a vector with the ppm scale (length = nrow(data))
  # posPPM - a ppms value to which to align left doublet peak
  # low/high -  the range where to look for doublet peaks

  
  #PeakPicking
  baselineThresh = 5 * min_window(data)
  peakList <- detectSpecPeaks(t(data[high:low,]),
                              nDivRange = c(128),
                              scales = seq(1, 16, 2),
                              baselineThresh = baselineThresh,
                              SNR.Th = -1,
                              verbose=FALSE)
  
  #removeNoGlucSamples (' maybe error instead?')
  rems = which(sapply(peakList, length)<2)
  if(length(rems) != 0){
    for (i in rems) print(paste('removing',i, sep='-'))
    data = data[,-rems]
    peakList = peakList[-rems]
  }

  # left glucose peak position in the subset
  leftGluPeaks = sapply(1:ncol(data), function(i) {findLeftGlucPeak(peakList[[i]], data[high:low,i])})
  # left glucose peak position in the whole spectra
  leftGluPeaksAbs = leftGluPeaks + high - 1

  data_aligned = do.call('cbind', lapply(1:ncol(data), function(i) offsetSpectra(data[,i], ppms, leftGluPeaksAbs[i], targetPos=posPPM)))
  #ppms = seq(max(ppms),min(ppms),length=(nrow(data_aligned)))
  
  if(length(rems)==0){
    colnames(data_aligned) = colnames(data)
  } else {
    colnames(data_aligned) = colnames(data)[-rems]
  }
  out = list(data=data_aligned, ppms = ppms, removed = rems)
  out
}

# ================================================== 


if('input' %in% names(args)){
  # import data
  data = read.table(args[['input']], header=T, sep='\t', stringsAsFactors = F)
  data = as.matrix(data)
} else stop('Data file not given. See ./BinPattern --help')

#if('alignTo' %in% names(args)){
#} else stop('Aligning to Glucose peak at ...')

# peak positions for various molecules
peakList = list(
  tsp = c(-0.2,0.01, 0, 'singlet'), 
  glucose = c(5.12, 5.3, 5.22475, 'doublet'),
  chloroform = c(1.336, 1.37, 1.4861, 'doublet'))

peak = args[['alignTo']]
if (peak %in% names(peakList)) peak = peakList[['peak']]

data = GlobalAlign(data, peak)

# -- write the output
write.table(data, file=args[['output']], row.names=T, col.names=T, sep='\t')
