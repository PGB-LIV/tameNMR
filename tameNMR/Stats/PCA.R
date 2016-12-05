
# PCA tool for Galaxy

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Principal component analysis and visualisation

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --outdir=path - folder for storing outputs
      --factorFile=fact - a factor to use for grouping
      --factorCol=factCol - a factor to use for grouping
      --pcs=1-2,2-3 - principal components to visualize
      --showScores=Y - whether to show scores plot
      --showLoadings=Y - whether to show loadings plot
      --showVarAcc=Y - whether to show 'variance accounted for' plot

      Example:
      ./PCA.R --input=inputFilePath --output=outputFilePath \n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

suppressMessages(library(ggplot2))

PCA_VarAcc_Plot <- function(pc, outdir){

  perc_accounted <- (pc$sdev^2/sum(pc$sdev^2)*100)
  perc_with_cumsum <- data.frame(pc = as.factor(1:length(perc_accounted)),
                                  perc_acc = perc_accounted,
                                 perc_cumsum = cumsum(perc_accounted))
  p<-ggplot(data = perc_with_cumsum, aes(x=pc, y=perc_cumsum))+
      geom_bar(stat='identity', col='black', fill='white')+
      geom_hline(yintercept = 95, col='red')+
      geom_hline(yintercept = 0, col='black')+
      xlab('PC')+
      ylab('% Variance')+
      ggtitle('% Variance accounted for by principle components')+
      theme_bw()

  fileName <- 'VarAcc.png'
  filePath <- paste(outdir,'/',fileName, sep='')
  ggsave(filePath, p)
  fileName
}

PCA_Scores_Plot <- function(pc, groups, useLabels=F, labels = "", pcs=c(1,2), legendName="Groups", outdir){

 if(useLabels & length(labels) != nrow(data)){
  print("Warning: The labels not given or given incorrectly. Using rownames.")
  labels <- rownames(data)
  }

  if (length(groups)!=nrow(pc$x)) {print(paste('groups length',length(groups),' while nrow', nrow(pc), sep="") ) }
  pcdf<-data.frame(pc1=pc$x[,pcs[1]], pc2=pc$x[,pcs[2]])
  if(useLabels) pcdf$labels<-labels
  perc_accounted <- ((pc$sdev)^2/sum((pc$sdev)^2)*100)[pcs]

  label_offset_x <- 0.035 * (range(pcdf$pc1)[2] - range(pcdf$pc1)[1])
  label_offset_y <- 0.035 * (range(pcdf$pc2)[2] - range(pcdf$pc2)[1])
  groups = as.factor(groups)
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

PCA_Loadings_Plot <- function(pc, groups, useLabels=F, labels = "", pcs=c(1,2), legendName="Groups", outdir){

   if(useLabels & length(labels) != nrow(pc$rotation)){
     print("Warning: loadings labels not given or given incorrectly. Using the column names.")
     labels <- colnames(data)
    }

  pcdf<-data.frame(load1=pc$rotation[,pcs[1]], load2=pc$rotation[,pcs[2]], var=labels)

  label_offset_x <- 0.035 * (range(pcdf$load1)[2] - range(pcdf$load1)[1])
  label_offset_y <- 0.035 * (range(pcdf$load2)[2] - range(pcdf$load2)[1])

  .e <- environment()

  p <- ggplot(data=pcdf, aes(x=load1, y=load2), environment=.e) + geom_point()

  if(useLabels) p <- p + geom_text(aes(x=load1+label_offset_x, y=load2+label_offset_y, label=labels))

  p <- p+
  xlab(paste("Loadings for PC",pcs[1],sep=""))+
  ylab(paste("Loadings for PC",pcs[2],sep=""))+
  ggtitle("PCA loadings plot")+
  theme_bw()

  fileName <- paste('PC_',pcs[1],'-',pcs[2],'_loadings.png', sep='')
  filePath <- paste(outdir,'/', fileName, sep='')
  ggsave(filePath, p)
  fileName
}

makeHTML <- function(files){
  # files - a list of files to display (files within each entry are displayed next to each other?)
  css.H1 <- '\"text-align: center;font-family:verdana; font-size:30px\"'
  css.textDiv <- '\"text-align=: center; font-family:verdana; font-size:20px; padding-top:35px;padding-bottom=25px\"'

  html <- c('<!DOCTYPE html>',
            '<html>',
            '<head>',
            '</head>',
            '<body')

  html <- c(html,
            paste('<h1 style=',css.H1,'> Principal component analysis </h1>',sep=''),
            '<hr>')
  html <- c(html,
            paste('<img src=\"', files[[1]], '\" style=\"width:500px\">', sep=''),
            #'<div style=\"text-align=: center; font-family:verdana; font-size:20px\">Barplot showing cumulative variance accounted for by each principal component.</div>',
            '<hr>')
  for(i in 2:length(files)){
    pic1 = paste('<img src=\"', files[[i]][1], '\" style=\"width:500px;\">', sep='')
    pic2 = paste('<img src=\"', files[[i]][2], '\" style=\"width:500px;\">', sep='')
    html <- c(html,
              paste('<div style=',css.textDiv,'>Scores and loadings plots for PCs ', strsplit(files[[i]][1],'_')[[1]][2],' </div>',sep=''),
              '<div>',
              #strsplit(files[[i]][1],'_')[[1]][2],
              pic1,
              pic2,
              '</div>')
  }

  html <- c(html,
            '</body>',
            '</html>')

  html
}

parsePCs <- function(pcs){
  # parse the pcs vector into 2col matrix
  pcs <-strsplit(pcs,',')[[1]]
  pcs <- do.call('rbind', lapply(pcs, function(x) as.numeric(strsplit(x, '-')[[1]])))
  pcs
}

data = read.table(args[['input']], header=T, row.names=1, stringsAsFactors = F, sep='\t')
factorFile = read.table(args[['factorFile']], header=T, sep=',', stringsAsFactors = T, row.names=1)
colnum <- as.numeric(args[['factorCol']])
factor = factorFile[,colnum]

if(!dir.exists(args[['outdir']])) dir.create(args[['outdir']], showWarnings = F)

if(args[['scale']]=='Y'){
  pc<-prcomp(data, scale = T)
 } else {
  pc <- prcomp(data)
 }

pcs = parsePCs(args[['pcs']])

plts = list()

plts[[1]] = suppressMessages(PCA_VarAcc_Plot(pc=pc, outdir = args[['outdir']]))
for(i in 1:nrow(pcs)){
  temp = c(suppressMessages(PCA_Scores_Plot(pc=pc, groups=factor, pcs=pcs[i,], outdir = args[['outdir']])),
           suppressMessages(PCA_Loadings_Plot(pc=pc, groups=factor, pcs=pcs[i,], outdir = args[['outdir']])))
  plts[[i+1]] = temp
}

#plts[[1]] = PCA_VarAcc_Plot(pc=pc, groups=factor, outdir = args[['outdir']])
#for(i in 1:nrow(pcs)){
#  tmp1 = PCA_Scores_Plot(pc=pc, groups=factor, pcs=pcs[i,], outdir = args[['outdir']])
#  tmp2 = PCA_Loadings_Plot(pc=pc, groups=factor, pcs=pcs[i,], outdir = args[['outdir']])
#  plts[[i+1]] = c(tmp1, tmp2)
#}

#plts = suppressMessages(do_PCA_Plot(pc=pc, groups=factor, outdir = args[['outdir']]))

htmlCode <- makeHTML(plts)

htmlFile <- file(args[['output']])
writeLines(htmlCode, htmlFile)
close(htmlFile)



