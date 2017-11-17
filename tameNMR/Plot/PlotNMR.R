
args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      PlotNMR - NMR spectra plotting tool

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --outDir=path - output folder
      --ppmInt=interval - ppm interval to plot
      --spread=N {Y,N} - spread the spectra (otherwise overlap, default)
      --group=Y {Y,N} - group spectra
      --fact=path - path to factor file
      --factCol=1 - which column in fact contains groups
      --aggr=Y {Y,N} - if aggregate spectra
      --aggrAvg=median {mean,median} - what function to use for aggregating spectra
      --plotBins=Y {Y,N} - if to plot bins
      --bins=path - path to bins file

      Example:
      ./plotNMR.R --input=inputFilePath --output=outputFilePath --outDir=outputDirPath --ppmInt=10-0 spread=Y \n\n")
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

if('spread' %in% names(args)){
  spreadPar = ifelse(args[['spread']] == 'Y', T, F)
} else spreadPar = F

if('group' %in% names(args)){
  groupPar = ifelse(args[['group']] == 'Y', T, F)
} else groupPar = F

if(groupPar & 'fact' %in% names(args) & 'factCol' %in% names(args)) {
  groupsPar = read.table(args[['fact']], header=T, sep='\t', row.names=1)
  groupsCol = as.numeric(args[['factCol']])
  groupsPar = groupsPar[,groupsCol]
} else {
  groupPar = F
  groupsPar = NULL
}

if('aggr' %in% names(args)) {
  aggrPar = ifelse(args[['aggr']] == 'Y', T, F)
} else aggrPar = F

if('aggrAvg' %in% names(args)) {
  aggrAvgPar = args[['aggrAvg']]
} else aggrAvgPar = 'median'

if('plotBins' %in% names(args)) {
  plotBinsPar = ifelse(args[['plotBins']] == 'Y', T, F)
} else plotBinsPar = F

if('bins' %in% names(args)) {
  binsPar = read.table(args[['bins']], header=T, sep='\t', stringsAsFactors = F)
} else binsPar = NULL

# -------------------- Function definitions --------------------
# Plot NMR spectra

plotNMRspectra = function(data, scale, toPlot=c(10, 0), 
                        spread = F,
                        group = F, groups = NULL,
                        aggregate = F, aggAvg = 'median',
                        plotBins=F, bins=NULL,
                        #diff = F, diffAvg = 'median', TODO: implement plotting of difference spectra
                        showLegend=F, labs=c(), ...){

  
  if (ncol(data)>6) showLegend = F
  
  # subset data to the range in toPlot
  data = as.matrix(data)
  data = data[ppm_to_point(scale,toPlot[1]):ppm_to_point(scale,toPlot[2]),]
  scale = scale[ppm_to_point(scale,toPlot[1]):ppm_to_point(scale,toPlot[2])]
  if(toPlot[2] > toPlot[1]) toPlot = rev(toPlot) # toPlot is left to right larger to smaller
  
  
  if(group & !is.null(groups)){
    if(length(as.character(unique(groups))) < 2) {group = F; groups = NULL} # don't use groups if only 1 present
  } else {group = F; groups=NULL}
  
  if(plotBins & !is.null(bins)) {
    bins = bins[apply(bins,1, function(x) all(c(x[1:2] < toPlot[1], x[1:2] > toPlot[2]))),]
    if(nrow(bins) == 0) {plotBins=F; bins=NULL}
    } else plotBins = F
  
  # aggregate data if required 
  if(aggregate){
    if(aggAvg == 'median') {
      data = aggregateByGroup(data, groups, median)
    } else if (aggAvg == 'mean') {
      data = aggregateByGroup(data, groups, mean)
    } else {
      cat(sprintf('No such average method %s, defaulting to median. \n', aggAvg))
      data = aggregateByGroup(data, groups, mean)
    }
  }
  # make colours
  if(aggregate & group){
    groups = as.factor(unique(as.character(groups)))
    cols = rainbow(ncol(data))
  } else if(group) {
    groups = as.factor(groups)
    cols = rainbow(length(levels(groups)))[groups]
  } else if(aggregate){
      cols = rainbow(ncol(data))
  } else {
      cols = rainbow(ncol(data))
  }
  
  if(spread){
    if(group) plotGroupsSpread(data, scale, groups, cols, plotBins=plotBins, bins=bins)
    else plotSpectraSpread(data, scale, cols, plotBins=plotBins, bins=bins)
  } else plotSpectraOlap(data, scale, cols, plotBins=plotBins, bins=bins)
}

ppm_to_point = function(ppm_scale, ppm){
  # convert ppm values to point values in spectra
  which.min(abs(ppm_scale-ppm))
}

aggregateByGroup = function(data, groups, avgF = median){
  # aggregate spectra by group if groups given, else just aggregate into 1 spectrum
  if(!is.null(groups)){
    groups = as.character(groups)
    grps = unique(groups)
    out = do.call('cbind', lapply(grps, function(grp) apply(data[,groups == grp], 1, avgF)))
    colnames(out) = grps
  } else {
    out = apply(data, 1, avgF)
    out = matrix(out, ncol = 1)
    print(dim(out))
    colnames(out) = 'Avg.Spectrum'
  }
  out
}

plotSpectraOlap = function(data, scale, cols, labs = NULL, showLegend = T, plotBins=NULL, bins=NULL, ...){
  # Plot NMR spectra ovelapped
  
  ylim = c(0,max(data)*1.05)
  xlim = c(0, nrow(data))
  N <- nrow(data)
  if(is.null(labs)) labs = colnames(data) 
  if(ncol(data)>8) showLegend = F
  
  ticks <- floor(seq(1,nrow(data),length=7))
  plot(NULL, xlim=xlim, ylim=ylim, axes=F, xlab="ppm", ylab="", ...)
  axis(1, at=ticks, labels=round(scale[ticks],2), srt=45)
  
  for (i in 1:ncol(data)){
    lines(1:N, data[,i], col=cols[i])
  }
  if(showLegend){
    if (!is.null(labs)>0){
      legend(xlim[1], ylim[2], legend=labs,fill = cols, cex=0.75, box.col='white')
    } else {
      legend(xlim[1], ylim[2], legend=colnames(data),fill = cols, cex=0.75, box.col='white')
    }
  }
  
  if(plotBins & !is.null(bins)) {
    plotBins(data, scale, bins, type='overlap', ylim=ylim)
  }

}

plotSpectraSpread = function(data, scale, cols, labs = NULL, showLegend = F, plotBins=NULL, bins=NULL, ...){
  # Plot NMR spectra spread
  
  offset = max(data)
  ylim = c(0, offset*ncol(data))#+offset*1.05)
  xlim = c(0, nrow(data))
  N <- nrow(data)
  
  if(is.null(labs)) labs = colnames(data) 
   
  x.ticks = floor(seq(1,nrow(data),length=7))
  y.ticks = (offset * (0:(ncol(data)-1))) + 0.3 * offset
  plot(NULL, xlim=xlim, ylim=ylim, axes=F, xlab="ppm", ylab="", ...)
  axis(1, at=x.ticks, labels=round(scale[x.ticks],2), srt=45)
  #if(plotBins) abline(h=0, lty=4, col='grey')
  #axis(2, at=y.ticks, labels=labs, las=2)
  
  for (i in 1:ncol(data)){
    lines(1:N, data[,i] + (i-1)*offset, col=cols[i])
  }
  text(x=floor(range(xlim)/100), y=y.ticks, labels = labs, cex = 0.6)
  #if(!is.null(groups)) legend('topright', legend=levels(groups), fill = rainbow(length(levels(groups))), cex=0.75, box.col='white')
  if(plotBins & !is.null(bins)) {
    plotBins(data, scale, bins, type='spread', ylim=ylim)
  }
}


plotGroupsSpread = function(data, scale, groups, cols, labs = NULL, showLegend = F, plotBins=NULL, bins=NULL, ...){
  # Plot NMR spectra grouped and spread
  groups = as.character(groups)
  
  offset = max(data)
  ylim = c(0, offset*length(unique(groups)))#+offset*1.05)
  xlim = c(0, nrow(data))
  N <- nrow(data)
  
  x.ticks = floor(seq(1,nrow(data),length=7))
  y.ticks = (offset * (0:(ncol(data)-1))) + 0.3 * offset
  plot(NULL, xlim=xlim, ylim=ylim, axes=F, xlab="ppm", ylab="", ...)
  axis(1, at=x.ticks, labels=round(scale[x.ticks],2), srt=45)
  #if(plotBins) abline(h=0, lty=4, col='grey')
  #axis(2, at=y.ticks, labels=labs, las=2)
  grps = unique(groups)
  for (g in 1:length(grps)){
    abline(h=(g-1)*offset, col='lightgrey')
    for (i in which(groups==grps[g])){
      lines(1:N, data[,i] + (g-1)*offset, col=cols[i])
    }
      
  }
  
  if(is.null(labs)) labs = grps 
  text(x=floor(range(xlim)/100), y=y.ticks, labels = labs, cex = 0.6)
  #if(!is.null(groups)) legend('topright', legend=levels(groups), fill = rainbow(length(levels(groups))), cex=0.75, box.col='white')
  if(plotBins & !is.null(bins)) {
    plotBins(data, scale, bins, type='spread', ylim=ylim)
  }
}
  

plotBins = function(data, scale, bins, type, ylim){
  labels = bins[,3]
  if(type=='spread') y = 0.95 * ylim[2] 
  else y = 0.5 * ylim[2] 
  
  for(i in 1:nrow(bins)){
    x1 = ppm_to_point(scale,bins[i,1])
    x2 = ppm_to_point(scale,bins[i,2])
    rect(x1,0,x2,y, col=rgb(0.8,0.8,0.8,0.5),border = NA)
    segments(x1, y, x2, y, lty = 4, lwd = 0.5)
    segments(x1, 0, x2, 0, lty = 4, lwd = 0.5)
    segments(x1,y*1.01,x1,0, lty = 4, lwd = 0.5)
    segments(x2,y*1.01,x2,0, lty = 4, lwd = 0.5)
    text(x=mean(c(x1,x2)), y=y*1.05, labels=labels[i], srt=60)
  }
  
}
# ================================================== 

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
  header = ''#'## NMR spectra\n'
  
  intro = ''
  prePlt1 = ''
  plt1 = sprintf('![](%s)\n', plts[[1]])
  
  output = c(header, intro, prePlt1,plt1)
  output
}

outdir = args[['outDir']]
plt = paste(outdir,'/NMRplot.png',sep='')
plt1 = 'NMRplot.png'
if(!dir.exists(outdir)) dir.create(outdir, showWarnings = F)

png(plt, width=15, height=9, units= 'in', res=300)
plotNMRspectra(data_, scale, toPlot=toplt, 
               spread = spreadPar,
               group = groupPar, groups = groupsPar,
               aggregate = aggrPar, aggAvg = aggrAvgPar,
               plotBins=plotBinsPar, bins = binsPar)
dev.off()

style = 'img {height: 720px; width: 1200px; }'

mdEncoded = make.MDoutput(plt)
writeLines(mdEncoded, paste(outdir, "/results.Rmd", sep=''))
MDfile = markdown::markdownToHTML(file = paste(outdir,"/results.Rmd", sep=''), stylesheet = style)

htmlFile = file(args[['output']])
writeLines(MDfile, htmlFile)
close(htmlFile)

#htmlCode <- makeHTML(plt1)

#htmlFile <- file(args[['output']])
#writeLines(htmlCode, htmlFile)
#close(htmlFile)
