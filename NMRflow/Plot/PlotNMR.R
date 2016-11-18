
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
      --ppmInt=interval

      Example:
      ./plotNMR.R --input=inputFilePath --output=outputFilePath --outDir=outputDirPath --ppmInt=10-0 \n\n")
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

# -------------------- Function definitions --------------------
plotNMR <- function(data, scale, toPlot=c(10, 0), showLegend=F, labs=c(), ...){

  ppm_to_point <- function(ppm_scale, ppm){
    which.min(abs(ppm_scale-ppm))
  }

  if (ncol(data)>6) showLegend = F

  data<- data[ppm_to_point(scale,toPlot[1]):ppm_to_point(scale,toPlot[2]),]
  scale<- scale[ppm_to_point(scale,toPlot[1]):ppm_to_point(scale,toPlot[2])]

  ylim = c(0,max(data)*1.05)
  xlim = c(0, nrow(data))
  N <- nrow(data)

  if (ncol(data)==1){
    cols <- 'blue'
    } else if (ncol(data) == 2){
    cols <- c('blue', 'red')
    } else {
    cols <- rainbow(ncol(data))
  }

  # Tick marks - TODO -  make them clever
  #tick_marks <- seq(max(scale),min(scale), length=11)
  ticks <- floor(seq(1,nrow(data),length=7))
  plot(NULL, xlim=xlim, ylim=ylim, axes=F, xlab="ppm", ylab="", ...)
  axis(1, at=ticks, labels=round(scale[ticks],2), srt=45)

  for (i in 1:ncol(data)){
    lines(1:N, data[,i], col=cols[i])
  }
  if(showLegend){
    if (length(labs)>0){
      legend(xlim[1], ylim[2], legend=labs,fill = cols, cex=0.75, box.col='white')
    } else {
      legend(xlim[1], ylim[2], legend=colnames(data),fill = cols, cex=0.75, box.col='white')
    }
  }
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

plt = paste(args[['outDir']],'/NMRplot.png',sep='')
plt1 = 'NMRplot.png'
if(!dir.exists(args[['outDir']])) dir.create(args[['outDir']], showWarnings = F)

png(plt, width=30, height=18, units= 'in', res=300)
plotNMR(data_, scale, toPlot=toplt)
dev.off()

htmlCode <- makeHTML(plt1)
# write outputs

htmlFile <- file(args[['output']])
writeLines(htmlCode, htmlFile)
close(htmlFile)
