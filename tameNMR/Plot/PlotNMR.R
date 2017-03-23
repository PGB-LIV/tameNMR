
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
      --type=overlap {overlap,spread,mean,median,diff} - type of plot
      --plotBins=Y {Y,N} - if to plot bins
      --bins=path - path to bins file
      --fact=path - path to factor file
      --factCol=1 - which column in fact contains groups
      --diffAvg=mean {mean, median} (only with type=diff) - which way to average

      Example:
      ./plotNMR.R --input=inputFilePath --output=outputFilePath --outDir=outputDirPath --ppmInt=10-0 type=overlap\n\n")
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

if('bins' %in% names(args)) bins = read.csv(args[['bins']], header=T, stringsAsFactors = F)

# -------------------- Function definitions --------------------

plotNMR.new = function(data, scale, toPlot=c(10, 0), 
                        plotBins=F, bins=NULL,
                        type='overlap', # cana also be {spread, mean, median, difference
                        groups=NULL, diffAvg = 'mean',
                        showLegend=F, labs=c(), ...){

  ppm_to_point = function(ppm_scale, ppm){
    which.min(abs(ppm_scale-ppm))
  }

  if (ncol(data)>6) showLegend = F
  
  data = as.matrix(data)
  data = data[ppm_to_point(scale,toPlot[1]):ppm_to_point(scale,toPlot[2]),]
  scale = scale[ppm_to_point(scale,toPlot[1]):ppm_to_point(scale,toPlot[2])]
  if(!is.null(groups) & length(as.character(unique(groups)))<2) groups = NULL # don't use groups if only 1 present

  
  if(type == 'overlap'){ # <- normal overlapped spectra
    
    ylim = c(0,max(data)*1.05)
    xlim = c(0, nrow(data))
    N <- nrow(data)
    if(!is.null(groups)){
      groups = as.factor(groups)
      cols = rainbow(length(levels(groups)))[groups]
      } else {
        cols = rainbow(ncol(data))
    }
  
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
  } else if(type == 'spread'){ # <- spread spectra for better visualisation of alignment
    
    offset = max(data)
    ylim = c(0, offset*ncol(data)+offset*1.05)
    xlim = c(0, nrow(data))
    N <- nrow(data)
    if(!is.null(groups)){
      groups = as.factor(groups)
      cols = rainbow(length(levels(groups)))[groups]
      } else {
        cols = rainbow(ncol(data))
    }
    if(is.null(labs)) labs = colnames(data) 
     
    x.ticks = floor(seq(1,nrow(data),length=7))
    y.ticks = (offset * (0:(ncol(data)-1))) + 0.3 * offset
    plot(NULL, xlim=xlim, ylim=ylim, axes=F, xlab="ppm", ylab="", ...)
    axis(1, at=x.ticks, labels=round(scale[x.ticks],2), srt=45)
    if(plotBins) abline(h=0, lty=4, col='grey')
    #axis(2, at=y.ticks, labels=labs, las=2)
    
    for (i in 1:ncol(data)){
      lines(1:N, data[,i] + (i-1)*offset, col=cols[i])
    }
    text(x=floor(range(xlim)/100), y=y.ticks, labels = labs, cex = 0.6)
    if(!is.null(groups)) legend('topright', legend=levels(groups), fill = rainbow(length(levels(groups))), cex=0.75, box.col='white')
    
  } else if(type == 'mean'){ # <- mean spectrum (good for looking at bins)
    if(!is.null(groups)){
      groups = as.character(groups)
      uniGroups = unique(groups)
      data = do.call('cbind', lapply(uniGroups, function(grp)  apply(data[,which(groups==grp)],1,mean)))
    } else {
      data = matrix(apply(data,1,mean), ncol = 1)
    }
    
    ylim = c(0,max(data)*1.05)
    xlim = c(0, nrow(data))
    N <- nrow(data)
    if(is.null(labs)) labs = colnames(data) 
    cols = rainbow(ncol(data))
    x.ticks = floor(seq(1,nrow(data),length=7))
    plot(NULL, xlim=xlim, ylim=ylim, axes=F, xlab="ppm", ylab="", ...)
    axis(1, at=x.ticks, labels=round(scale[x.ticks],2), srt=45)
    axis(2)
    abline(h=0, lty=4, col='black')
    for (i in 1:ncol(data)){
      lines(1:N, data[,i], col=cols[i])
    }
    if(!is.null(groups)) legend('topright', legend=uniGroups, fill = cols, cex=0.75, box.col='white')
      #text(x=floor(range(xlim)/100), y=y.ticks, labels = labs, cex = 0.6)
    
  } else if(type == 'median'){ # <- median spectrum (good for looking at bins)
    if(!is.null(groups)){
      groups = as.character(groups)
      uniGroups = unique(groups)
      data = do.call('cbind', lapply(1:length(uniGroups), function(i)  apply(data[,groups==uniGroups[i]],1,median)))
    } else {
      data = matrix(apply(data,2,median), ncol = 1)
    }
    
    ylim = c(0,max(data)*1.05)
    xlim = c(0, nrow(data))
    N <- nrow(data)
    if(is.null(labs)) labs = colnames(data) 
    cols = rainbow(ncol(data))
    x.ticks = floor(seq(1,nrow(data),length=7))
    plot(NULL, xlim=xlim, ylim=ylim, axes=F, xlab="ppm", ylab="", ...)
    axis(1, at=x.ticks, labels=round(scale[x.ticks],2), srt=45)
    axis(2)
    abline(h=0, lty=4, col='black')
    for (i in 1:ncol(data)){
      lines(1:N, data[,i], col=cols[i])
    }
    if(!is.null(groups)) legend('topright', legend=uniGroups, fill = cols, cex=0.75, box.col='white')
      #text(x=floor(range(xlim)/100), y=y.ticks, labels = labs, cex = 0.6)
    
  } else if(type == 'difference'){ # <- difference spectrum
    groups = as.character(groups)
    if(length(unique(groups))==2){
      uniGrp = unique(groups)
      avg = ifelse(diffAvg=='mean', mean, median)
      avgA = apply(data[,groups == uniGrp[1]], 1, avg)
      avgB = apply(data[,groups == uniGrp[2]], 1, avg)
      diffSpec = avgA-avgB
      
      ylim = c(min(diffSpec) - (abs(min(diffSpec) * 0.05)), max(diffSpec)*1.05)
      xlim = c(0, length(diffSpec))
      N <- length(diffSpec)
      cols = rainbow(1)
      x.ticks = floor(seq(1, length(diffSpec), length=7))
      plot(NULL, xlim=xlim, ylim=ylim, axes=F, xlab="ppm", ylab="", main=paste('Difference plot ( ', uniGrp[1], ' - ', uniGrp[2],' )', sep=''), ...)
      axis(1, at=x.ticks, labels=round(scale[x.ticks],2), srt=45)
      axis(2)
      abline(h=0, lty=4, col='grey')
      lines(1:N, diffSpec, col=cols)
      
    } else stop('Difference plot only works with 2 groups.\n')
    
  } else stop(sprintf('No such type %s\n', type))
  
  
  # Plotting bin boundaries and labels
  if(plotBins){
    scale_min = min(scale)
    scale_max = max(scale)
    binsToPlot = intersect(which(bins[,2] > scale_min & bins[,2] < scale_max),
                           which(bins[,2] > scale_min & bins[,2] < scale_max))
    labels = bins[binsToPlot,1]
    if(type=='spread') y = 0.9 * ylim[2] 
    else y = 0.5 * ylim[2] 
    
    for(i in 1:nrow(bins)){
      x1 = ppm_to_point(scale,bins[i,2])
      x2 = ppm_to_point(scale,bins[i,3])
      segments(x1, y, x2, y)
      text(x=mean(x1,x2), y=y*1.10, labels=labels[i], srt=45)
      segments(x1,y*1.05,x1,0)
      segments(x2,y*1.05,x2,0)
    }
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

plt = paste(args[['outDir']],'/NMRplot.png',sep='')
plt1 = 'NMRplot.png'
if(!dir.exists(args[['outDir']])) dir.create(args[['outDir']], showWarnings = F)

png(plt, width=30, height=18, units= 'in', res=300)
if(plotBins){
  if('bins' %in% names(args)){
    plotNMR(data_, scale, toPlot=toplt, plotBins=T, bins = bins)
  } else {
    stop('Bins not specified.')
  }
} else {
  plotNMR(data_, scale, toPlot=toplt)
}
dev.off()

htmlCode <- makeHTML(plt1)
# write outputs

htmlFile <- file(args[['output']])
writeLines(htmlCode, htmlFile)
close(htmlFile)
