
args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      QuantilePlot - Quantile plot of NMR spectra

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --outDir=path - output folder
      --ppmInt=interval - which part (ppm range) to plot
      --pltMean=Y - plot mean (alternatively pltMean=N plots median spectrum)

      Example:
      ./QuantilePlot.R --input=inputFilePath --output=outputFilePath --outDir=outputDirPath --ppmInt=10-0 --pltMean=Y")
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

# ==================== Plotting function ==================== 
#PlotQuantiles
plotQuantiles<-function(data,quantiles = seq(0,1,0.05),
                        ylimit = c(0,0),
                        ppmRange = c(10,0),
                        ppmPlot = c(10,0),
                        plotMean = FALSE,
                        mainTitle="default"){
   
   #Plot NMR spectra as quantile gradient.
   #Input: data     -   a matrix of numbers where every row is a  spectrum.
   #       ylimt    -   a 2 number vector specifying the range on the Y axis.
   #                    if default (0,0) is left, range is calculated from data.
   #                    Enter in the form c(Ymin,Ymax).
   #       ppmRange -   a 2 number vector specifying the ppm range of the data.
   #                    Enter in the form c(a,b) where a > b.
   #       ppmPlot  -   a 2 number vector specifying the ppm range to be plotted.
   #                    Enter in the form c(a,b) where a > b.
   #       plotMean -   a boolean value (TRUE/FALSE). If TRUE the mean spectrum is plotted,
   #                    else median is plotted.
   #       mainTitle -  a string representing the main title of the plot. If
   #                    default value is left will plot with name
   #                    "NMR Quantile Plot [plot ppm range]"
   
   
   #make values for x axis
   ppms <- seq(ppmRange[1],ppmRange[2],len=ncol(data))
   
   #calculate ppms for each point
   ppIndex <- which((ppms<=ppmPlot[1]) & (ppms>=ppmPlot[2]))
   
   #calculate the needed quantiles from data
   plotVals<-apply(data[,ppIndex] ,2,function(x){quantile(x,quantiles)} )
   
   ppms <- ppms[ppIndex]
   x <- 1:ncol(plotVals)
   ints <- floor(seq(1,ncol(plotVals), length=6))
   
   #create colour palettes
   colours<-heat.colors(n=floor(length(quantiles)/2))
   fillColours<-c(colours,rev(colours))
   
   #generate an empty plot
   if (ylimit[1] == 0){ ylimit[1] <- min(plotVals) }
   if (ylimit[2] == 0){ ylimit[2] <- max(plotVals) }
   
   if (mainTitle == "default"){
      mainTitle = paste("NMR Quantile Plot [",
                        round(ppmPlot[1],2),"-",round(ppmPlot[2],2)," ppm ]",sep="")}
   
   bw <- ncol(plotVals)/100
   if(ppmPlot[1] - ppmPlot[2] <= 0.1){
      digs <- 3
   } else {
      digs <-2}
   
   #Make an empty plot
   plot(NULL, xlim=c(-4*bw, ncol(plotVals)), ylim=ylimit,
        main = mainTitle,
        xlab = "ppm",
        ylab = "",
        xaxt = "n", yaxt="n")
   axis(1, at = ints, labels = round(ppms[ints], digits=digs), cex=1.5)
   
   #plot the areas
   for (i in 1:(nrow(plotVals)-1)){
      polygon(c(x,rev(x)),c(plotVals[i,],rev(plotVals[i+1,])),
              col=fillColours[i],
              border=fillColours[i])
   }
   
   #plot the mean or median
   if (plotMean){
      lines(x,apply(data[,ppIndex],2,mean),col="black", lwd=2)
   } else {
      lines(x,plotVals[ceiling(nrow(plotVals)/2),],col="black", lwd=2)
   }
   
   #draw custom legend
   c <- (ylimit[2]-ylimit[1])/length(fillColours)
   bot <- abs(ylimit[1])
 
   for (i in 1:length(fillColours)){
      polygon( c(-2*bw,-bw,-bw,-2*bw), c((i-1)*c-bot,(i-1)*c-bot,i*c-bot,i*c-bot),
               col = fillColours[i],
               border = fillColours[i])}
   
   maxC <- length(fillColours)*c
   segments(rep(-3*bw,5),c(-bot,maxC*0.25-bot,maxC*0.5-bot,maxC*0.75-bot,maxC-bot),
            rep(-bw,5),c(-bot,maxC*0.25-bot,maxC*0.5-bot,maxC*0.75-bot,maxC-bot),
            col = "black")
   segments(c(-2*bw,-bw),c(-bot,-bot),c(-2*bw,-bw),c(maxC-bot,maxC-bot))
   
   text(rep(-4*bw, 5), c(-bot,maxC*0.25-bot,maxC*0.5-bot,maxC*0.75-bot,maxC-bot),
        c("0%","25%","50%","75%","100%"), cex = 1)
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
            paste('<h1 style=',css.H1,'> NMR quantiles plot</h1>',sep=''),
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
  header = '## NMR quantile plot\n'
  
  intro = ''
  prePlt1 = ''
  plt1 = sprintf('![](%s)\n', plts[[1]])
  
  output = c(header, intro, prePlt1,plt1)
  output
}

outdir = args[['outDir']]
plt = paste(outdir,'/QuantilePlot.png',sep='')
plt1 = 'QuantilePlot.png'
if(!dir.exists(outdir)) dir.create(outdir, showWarnings = F)

pltMean = ifelse(args[['pltMean']]=='Y', TRUE, FALSE)
png(plt, width=15, height=9, units= 'in', res=300)
plotQuantiles(data = t(data_), ppmRange = c(min(scale), max(scale)), ppmPlot = toplt, plotMean = pltMean)
dev.off()

style = 'img {height: 720px; width: 1200px; }'

mdEncoded = make.MDoutput(plt)
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
