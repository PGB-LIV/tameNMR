
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

# ==================== Global spectra alignment ====================

GlobalAlign = function(data){
  
  data_ = as.matrix(data[,2:ncol(data)])
  ppms = data[,1]
  # alignment
  low = which.min(abs(ppms-5.12))
  high = which.min(abs(ppms-5.3))
  
  outList = alignGlucoseLeftPeak(data_, ppms, posPPM = 5.20475, low, high)
  # removed [47,48,71,87]
  #patients1 = patients1[-c(47,48,71,87)]
  #conditions1 = conditions1[-c(47,48,71,87)]
  
  #data2_ = alignGlucoseLeftPeak(data2, ppms2, 5.20475, low, high)
  # no spectra removed
  
  #dataFinal = cbind(data_$data, data2_$data)
  #ppmsFinal = data_$ppms + 0.003 # better bin alignment
  out = cbind(outList$ppms, outList$data)
  out
}

findLeftGlucPeak <- function(pList, spec){
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

require(speaq)
alignGlucoseLeftPeak = function(data, ppms, posPPM, low, high){
  # aligns left glucose peak to the given ppm value
  # data - a dataframe with data in columns
  # ppms - a vector with the ppm scale (length = nrow(data))
  # posPPM - a ppms value to which to align left glucose peak
  # low/high -  the range where to look for glucose peaks

  #PeakPicking
  baselineThresh <- 2 * max(apply(abs(data[1:200,]),1,max)) / 10^round(log10(mean(data[1:200,])))
  #baselineThresh = 160000
  peakList <- detectSpecPeaks(t(data[high:low,]) / 10^round(log10(mean(data[1:200,]))),
                              nDivRange = c(128),
                              scales = seq(1, 16, 2),
                              baselineThresh = baselineThresh,
                              SNR.Th = -1,
                              verbose=FALSE)

  #removeNoGlucSamples
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

data = GlobalAlign(data)

# -- write the output
write.table(data, file=args[['output']], row.names=T, col.names=T, sep='\t')
