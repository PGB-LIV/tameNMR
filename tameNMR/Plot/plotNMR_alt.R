




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
