#!/usr/bin/env Rscript

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
      --fact=path - path to groups file
      --factCol=1 - number of groups file column to use
      --plotDiff=F - if plot difference (only with 2 groups)
      --avgFun=mean - {mean, median} which function to use for averaging
      --test=ANOVA - which test used in to obtain pvalues (used in the plot title)
      --colourbar=discrete {discrete, continuous} - draw discrete (5 intervals) or continuous colourbar

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

bins = read.table(args[['bins']], header=F, stringsAsFactors = F, sep='\t')
if (is.numeric(bins$V3)) {
  bins$V3 = paste0('X',as.character(bins$V3)) 
  bins$V3 = sapply(bins$V3, function(x) gsub('-','.',x))
}

if(grep('.dat',args[['pvals']])) { args[['pvals']]=gsub('.dat$','',args[['pvals']]) }
pvals = read.table(paste(args[['pvals']],'_files/pvals.txt',sep=''), header=T, sep='\t', stringsAsFactors = F)
pvals_ = pvals[,'adj_p_vals']
test = args[['test']]

if('fact' %in% names(args) & 'factCol' %in% names(args)) {
  groups = read.csv(args[['fact']], header=T, stringsAsFactors = F)
  groups = groups[,as.numeric(args[['factCol']])]
  if(length(groups) != ncol(data_)) groups=NULL
  } else groups=NULL
if('plotDiff' %in% names(args)){
  plotDiff = ifelse(args[['plotDiff']]=='Y', TRUE, FALSE)
  } else plotDiff = F
if('avgFun' %in% names(args)) {
  avgFun = args[['avgFun']]
} else {
    avgFun = 'mean'
  }

# -------------------- Function definitions --------------------
suppressMessages(library(RColorBrewer))
suppressMessages(library(viridis))

plotNMRSignif = function(data, ppms, bins, pvals, toPlot=c(0,10), test='ANOVA', avgFun = 'mean', groups=NULL, plotDiff=F){
  
  # calculate spectrum for plotting
  if(plotDiff){
    if(!is.null(groups)){
      if(length(unique(groups))==2){
        uniGrp = unique(groups)
        avg = ifelse(avgFun=='mean', mean, median)
        avgA = apply(data[,groups == uniGrp[1]], 1, avg)
        avgB = apply(data[,groups == uniGrp[2]], 1, avg)
        spec = avgA-avgB
      } else stop(sprintf('Cannot plot difference of %d groups.\n', length(unique(groups))))
    } else stop('Cannot calculate difference without groups.')
  } else {
    avg = ifelse(avgFun=='mean', mean, median)
    spec = apply(data, 1, avg)
  }
  
  # intervals of pvals - 0, 0.001, 0.01, 0.05
  pvals[pvals==1] = 0.9999
  pvals_cut = as.character(cut(pvals, breaks = c(0,0.001,0.01,0.05,1)))
  pvals_cut[is.na(pvals_cut)] = "(0,0.001]"
  levs =c("(0.05,1]","(0.01,0.05]","(0.001,0.01]","(0,0.001]")
  
  ppm_to_point <- function(ppm_scale, ppm){
    which.min(abs(ppm_scale-ppm))
  }
  
  binsToPoints = function(ppms, bins){
    do.call('c', lapply(1:nrow(bins), function(i) which(ppms >= min(bins[i,]) & ppms <= max(bins[i,]))))
  }
  
  dataNew = do.call('rbind', lapply(levs, function(lev) {
    dat = spec
    # turn all points except for bins of each significance level to NA
    if(lev %in% pvals_cut){
      sigIntervals = binsToPoints(ppms, bins[pvals_cut == lev, 1:2])
      dat[-sigIntervals] = NA
    } else {
      dat = rep(NA, length(dat))
    }
    dat}
  ))
  
  # make an empty spectrum and fill in gaps between buckets
  emptySpec = rep(NA, ncol(dataNew))
  emptyCols = apply(dataNew, 2, function(x) all(is.na(x)))
  emptySpec[emptyCols] = spec[emptyCols]
  dataNew = rbind(emptySpec, dataNew)
  dataNew <- dataNew[,ppm_to_point(ppms,toPlot[1]):ppm_to_point(ppms,toPlot[2])]
  
  ppms <- ppms[ppm_to_point(ppms,toPlot[1]):ppm_to_point(ppms,toPlot[2])]
  N = ncol(dataNew)
  levs = c('Unbinned', gsub(",", " - ", levs))
  
  ticks <- floor(seq(1,ncol(dataNew),length=7))
  #colsBrew = brewer.pal(11, 'Spectral')
  #cols = c('grey', colsBrew[c(8,5,3,1)])
  colsBrew = plasma(5)[1:4]
  cols = c('grey', colsBrew)
  xlim_ = c(0,N)
  ylim_ = c(min(dataNew, na.rm = T), 1.05*max(dataNew, na.rm = T))
  
  plot(NULL, xlim = xlim_, ylim=ylim_ , axes=F, xlab='ppm',ylab='', main=sprintf('Bin significance plot (%s)', test))
  axis(1, at=ticks, labels=round(ppms[ticks],2), srt=45)
  axis(2)
  abline(h=0, col='black', lty=4)
  
  for(i in 1:nrow(dataNew)){
    lines(1:N, dataNew[i,], col=cols[i], lwd=4)
  }
  legend("top", legend=levs, fill = cols, cex=1.75, box.col='white', title = 'p-value', ncol=length(levs))
}

plotNMRSignif_2 = function(data, ppms, bins, pvals, toPlot=c(0,10), test='ANOVA', avgFun = 'mean', groups=NULL, plotDiff=F){
  
  # calculate spectrum for plotting
  if(plotDiff){
    if(!is.null(groups)){
      if(length(unique(groups))==2){
        uniGrp = unique(groups)
        avg = ifelse(avgFun=='mean', mean, median)
        avgA = apply(data[,groups == uniGrp[1]], 1, avg)
        avgB = apply(data[,groups == uniGrp[2]], 1, avg)
        spec = avgA-avgB
        } else stop(sprintf('Cannot plot difference of %d groups.\n', length(unique(groups))))
    } else stop('Cannot calculate difference without groups.')
  } else {
    avg = ifelse(avgFun=='mean', mean, median)
    spec = apply(data, 1, avg)
  }

  # intervals of pvals - 0, 0.001, 0.01, 0.05
  mapP2Line = function(pval, seq){
    for(i in 1:length(seq)){
      if(pval <= seq[i]) return(i) 
    }
    return(length(seq))
  }

  ppm_to_point <- function(ppm_scale, ppm){
    which.min(abs(ppm_scale-ppm))
  }

  binsToPoints = function(ppms, bins){
    do.call('c', lapply(1:nrow(bins), function(i) which(ppms >= min(bins[i,]) & ppms <= max(bins[i,]))))
  }

  dataNew = do.call('rbind', lapply(1:nrow(bins), function(i) {
    dat = spec
    dat[-binsToPoints(ppms, bins[i, 1:2])] = NA
    dat}
  ))

  colsOfBinsIdx = sapply(pvals, mapP2Line, seq(0,1,length=101))
  # make an empty spectrum and fill in gaps between buckets
  emptySpec = rep(NA, ncol(dataNew))
  emptyCols = apply(dataNew, 2, function(x) all(is.na(x)))
  emptySpec[emptyCols] = spec[emptyCols]

  dataNew = rbind(emptySpec, dataNew)
  dataNew<- dataNew[,ppm_to_point(ppms,toPlot[1]):ppm_to_point(ppms,toPlot[2])]

  ppms<- ppms[ppm_to_point(ppms,toPlot[1]):ppm_to_point(ppms,toPlot[2])]
  N = ncol(dataNew)
  #levs = c('Unbinned', gsub(",", " - ", levs))

  ticks <- floor(seq(1,ncol(dataNew),length=7))
  #colsBrew = brewer.pal(11, 'Spectral')
  #cols = c('grey', colsBrew[c(8,5,3,1)])
  colsBrew = rev(plasma(120)[1:100])
  cols = c('grey', colsBrew[colsOfBinsIdx])
  bw <- length(spec)/100
  xlim_ = c(-bw,N)
  ylim_ = c(min(dataNew, na.rm = T), 1.05*max(dataNew, na.rm = T))

  plot(NULL, xlim = xlim_, ylim=ylim_ , axes=F, xlab='ppm',ylab='', main=sprintf('Bin significance plot (%s)', test))
  axis(1, at=ticks, labels=round(ppms[ticks],2), srt=45)
  #axis(2)
  #abline(h=0, col='black', lty=4)
  segments(x0 = 0, y0 = 0, x1 = length(spec),y1 = 0, col = 'black', lty = 4)

  for(i in 1:nrow(dataNew)){
    lines(1:N, dataNew[i,], col=cols[i], lwd=2)
  }
  #legend("top", legend=levs, fill = cols, cex=0.75, box.col='white', title = 'p-value', ncol=length(levs))
  
  #draw custom legend
  fillColours = colsBrew
  ylimit = ylim_
  c <- (ylimit[2]-ylimit[1])/length(fillColours)
  bot <- abs(ylimit[1])

  for (i in 1:length(fillColours)){
    polygon( c(-0.5*bw,-0.2*bw,-0.2*bw,-0.5*bw), c((i-1)*c-bot,(i-1)*c-bot,i*c-bot,i*c-bot),
             col = fillColours[i],
             border = fillColours[i])}

  maxC <- length(fillColours)*c
  segments(c(-0.5*bw,-0.5*bw),c(-bot,maxC-bot),
          c(-0.2*bw,-0.2*bw),c(-bot,maxC-bot),
          col = "black")
  segments(c(-0.5*bw,-0.2*bw),c(-bot,-bot),c(-0.5*bw,-0.2*bw),c(maxC-bot,maxC-bot))
  text(rep(-0.6*bw, 2), c(-bot,maxC-bot), c("0","1"), cex = 0.8)
  #text(x = -0.65*bw, y=(maxC-bot)/2, labels = 'p-value', cex=0.75, srt=90, pos=2)
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

make.MDoutput = function(plts){
  output = ''
  header = ''#'## NMR significant bins\n'
  
  intro = ''
  prePlt1 = ''
  plt1 = sprintf('![](%s)\n', plts[[1]])
  
  output = c(header, intro, prePlt1,plt1)
  output
}
#-------------------- Results --------------------
outdir = args[['outDir']]
plt = paste(outdir,'/NMRSigPlot.png',sep='')
plt1 = 'NMRSigPlot.png'
if(!dir.exists(args[['outDir']])) dir.create(args[['outDir']], showWarnings = F)

png(plt, width=30, height=18, units= 'in', res=300)
if('colourbar' %in% names(args) & args[['colourbar']]=='discrete'){
  plotNMRSignif(data_, scale, bins[,1:2], pvals_, toPlot = toplt, test=test, groups=groups, plotDiff=plotDiff, avgFun = avgFun)
} else {
    
  plotNMRSignif_2(data_, scale, bins[,1:2], pvals_, toPlot = toplt, test=test, groups=groups, plotDiff=plotDiff, avgFun = avgFun)
}
dev.off()

style = 'img {height: 720px; width: 1200px; }'

mdEncoded = make.MDoutput(list(plt))
writeLines(mdEncoded, paste(outdir, "/results.Rmd", sep=''))
MDfile = markdown::markdownToHTML(file = paste(outdir,"/results.Rmd", sep=''), stylesheet = style)

htmlFile = file(args[['output']])
writeLines(MDfile, htmlFile)
close(htmlFile)

#htmlCode <- makeHTML(plt1)
# write outputs

#htmlFile <- file(args[['output']])
#writeLines(htmlCode, htmlFile)
#close(htmlFile)
