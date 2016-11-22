
args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      PLS-DA

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --outDir=path - output folder
      --factorFile=path - a path to a factorfile
      --factorCol=num - a which column to use

      Example:
      ./plsda.R --input=inputFilePath --output=outputFilePath --outDir=outputDirPath --factorFile=factorFilePath --factorCol=num \n\n")
  q(save="no")
}

suppressMessages(require(pls))
suppressMessages(require(ggplot2))
# -------------------functions --------------------

make.factorMat = function(fact){
  fact = as.character(fact)
  unFact = unique(fact)
  N = length(unFact)
  out = do.call('rbind', lapply(fact, function(currFact) { y=rep(0,N); y[which(unFact == currFact)] = 1 ; y}))
  colnames(out) <- unFact
  out
}

PCA_Scores_Plot <- function(scores, groups, useLabels=F, labels = "", pcs=c(1,2), legendName="Groups", outdir){

 if(useLabels & length(labels) != nrow(data)){
  print("Warning: The labels not given or given incorrectly. Using rownames.")
  labels <- rownames(data)
  }

  if (length(groups)!=nrow(scores$x)) {print(paste('groups length',length(groups),' while nrow', nrow(scores), sep="") ) }
  pcdf<-data.frame(pc1=scores[,pcs[1]], pc2=scores[,pcs[2]])
  if(useLabels) pcdf$labels<-labels

  #perc_accounted <- ((pc$sdev)^2/sum((pc$sdev)^2)*100)[pcs]
  perc_accounted <- c(0,0) #TODO: make this calculation correct for pls

  label_offset_x <- 0.035 * (range(pcdf$pc1)[2] - range(pcdf$pc1)[1])
  label_offset_y <- 0.035 * (range(pcdf$pc2)[2] - range(pcdf$pc2)[1])
  groups = as.factor*(groups)
  .e <- environment()
  p <- ggplot(data=pcdf, aes(x=pc1, y=pc2), environment=.e) + geom_point(size=5, aes(fill=groups), colour='black', pch=21)

  if(useLabels)  p <- p + geom_text(aes(x=pc1+label_offset_x, y=pc2+label_offset_y, label=labels))

  p <- p+#guides(color=FALSE)+
  theme_bw()+
  guides(fill = guide_legend(title = legendName))+
  xlab(paste("PC",pcs[1], " (", round(perc_accounted[1],2), "%)", sep=""))+
  ylab(paste("PC",pcs[2], " (", round(perc_accounted[2],2), "%)", sep=""))+
  ggtitle("PCA scores plot")

  fileName <- paste('PC_',pcs[1],'-',pcs[2],'_scores.png', sep='')
  filePath <- paste(outdir,'/',fileName, sep='')
  ggsave(filePath, p)
  fileName
}
# -------------------------------------------------


parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# import data

data = read.table(args[['input']], header=T, sep='\t', row.names=1, stringsAsFactors = F)
factorFile = read.table(args[['factorFile']], header=T, sep=',', stringsAsFactors = T, row.names=1)
factor = factorFile[,as.numeric(args[['factorCol']])]

factor_  = make.factorMat(factor)

comp.num = nrow(data) - 1;
if(comp.num > 8) comp.num = 8

res = plsr(factor_ ~ as.matrix(data), method='oscorespls', ncomp=comp.num)

makeHTML = function(res){

  # files - a list of files to display (files within each entry are displayed next to each other?)
  css.H1 <- '\"text-align: center;font-family:verdana; font-size:30px\"'
  css.textDiv <- '\"text-align=: center; font-family:verdana; font-size:10px; padding-top:35px;padding-bottom=25px\"'

  html <- c('<!DOCTYPE html>',
            '<html>',
            '<head>',
            '</head>',
            '<body>')


  html <- c(html,
            '</body>',
            '</html>')

  html
}

htmlCode <- makeHTML(res)

htmlFile <- file(args[['output']])
writeLines(htmlCode, htmlFile)
close(htmlFile)

# write outputs