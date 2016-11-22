args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      PlotNMRSig - NMR spectra with significant bins

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --outDir=path - output folder
      --ppmInt=interval
      --bins=binTable
      --pvals=binPvals

      Example:
      ./plotNMRSig.R --input=inputFilePath --output=outputFilePath --outDir=outputDirPath --ppmInt=10-0 --bins=Binfile --pvals=PvalTable\n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# import data
data = read.table(args[['input']], header=T, sep='\t', stringsAsFactors = F)
data = as.matrix(data)

data_ = data[,2:ncol(data)]
scale = data[,1]

toplt = strsplit(args[['ppmInt']], '-')[[1]]
toplt = sort(as.numeric(toplt), decreasing = T)

readPatternFile <- function(path){
  pf <- read.table(path, sep="", header=F, skip=9, stringsAsFactors = F)
  pf[,c(3,4,6)]
}

bins = readPatternFile(args[['bins']])
if(grep('.dat',args[['pvals']])) { args[['pvals']]=gsub('.dat$','',args[['pvals']]) }
pvals = read.table(paste(args[['pvals']],'/pvals.csv',sep=''), header=T, sep=',')
pvals_ = pval[,2]

# -------------------- Function definitions --------------------
suppressMessages(library(RColorBewer))
plotNMRSignif = function(rawData, ppms, bins, pvals, toPlot=c(0,10)){
  meanData = apply(rawData, 1, mean)

  # intervals of pvals - 0, 0.001, 0.01, 0.05
  pvals_cut = as.character(cut(pvals, breaks = c(0,0.001,0.01,0.05,1)))
  levs =c("(0.05,1]","(0.01,0.05]","(0,0.001]","(0.001,0.01]")

  ppm_to_point <- function(ppm_scale, ppm){
    which.min(abs(ppm_scale-ppm))
  }

  binsToPoints = function(ppms, bins){
    do.call('c', lapply(1:nrow(bins), function(i) which(ppms >= min(bins[i,]) & ppms <= max(bins[i,]))))
  }

  dataNew = do.call('rbind', lapply(levs, function(lev) {
    dat = meanData
    # turn all points except for bins of each significance level to NA
    if(lev %in% pvals_cut){
      dat[-binsToPoints(ppms, bins[pvals_cut == lev, 1:2])] = NA
    } else {
      dat = rep(NA, length(dat))
    }
    dat}
  ))

  # make an empty spectrum and fill in gaps between buckets
  emptySpec = rep(NA, ncol(dataNew))
  emptyCols = apply(dataNew, 2, function(x) all(is.na(x)))
  emptySpec[emptyCols] = meanData[emptyCols]

  dataNew = rbind(emptySpec, dataNew)
  dataNew<- dataNew[,ppm_to_point(ppms,toPlot[1]):ppm_to_point(ppms,toPlot[2])]

  ppms<- ppms[ppm_to_point(ppms,toPlot[1]):ppm_to_point(ppms,toPlot[2])]
  N = ncol(dataNew)
  levs = c('Unbinned', gsub(",", " - ", levs))

  ticks <- floor(seq(1,ncol(dataNew),length=7))
  colsBrew = brewer.pal(11, 'Spectral')
  cols = c('grey', colsBrew[c(8,5,3,1)])
  xlim_ = c(0,N)
  ylim_ = c(min(dataNew, na.rm = T), 1.05*max(dataNew, na.rm = T))

  plot(NULL, xlim = xlim_, ylim=ylim_ , axes=F, xlab='ppm',ylab='', main='Bin significance (ANOVA)')
  axis(1, at=ticks, labels=round(ppms[ticks],2), srt=45)

  for(i in 1:nrow(dataNew)){
    lines(1:N, dataNew[i,], col=cols[i], lwd=2)
  }
  legend("top", legend=levs, fill = cols, cex=0.75, box.col='white', title = 'p-value', ncol=length(levs))
}

makeHTML <- function(plt){
  # files - a list of files to display (files within each entry are displayed next to each other?)
  css.H1 <- '\"text-align: center;font-family:verdana; font-size:30px\"'
  css.textDiv <- '\"text-align=: center; font-family:verdana; font-size:10px; padding-top:35px;padding-bottom=25px\"'

  html <- c('<!DOCTYPE html>',
            '<html>',
            '<head>',
            '</head>',
            '<body')

  html <- c(html,
            paste('<h1 style=',css.H1,'> NMR plot</h1>',sep=''),
            '<hr>')

  html <- c(html,
            '<div align=\"center\" >',
            paste('<img src=\"', plt[1], '\" width=\"1200\"\">', sep=''),
            '<hr>',
            '</div>')

  html <- c(html,
            '</body>',
            '</html>')

  html
}

#-------------------- Results --------------------
plt = paste(args[['outDir']],'/NMRSigPlot.png',sep='')
plt1 = 'NMRSigPlot.png'
if(!dir.exists(args[['outDir']])) dir.create(args[['outDir']], showWarnings = F)

png(plt, width=30, height=18, units= 'in', res=300)
plotNMRSignif(data_, scale, bins, pvals, toPlot = toplt)
dev.off()

htmlCode <- makeHTML(plt1)
# write outputs

htmlFile <- file(args[['output']])
writeLines(htmlCode, htmlFile)
close(htmlFile)
