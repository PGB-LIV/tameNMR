
args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      One-way anova for multivariate data

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --outdir=path - folder for storing outputs
      --factorFile=fact - a factor to use for grouping
      --factorCol=factCol - a factor to use for grouping

      Example:
      ./anova.R --input=inputFilePath --output=outputFilePath \n\n")
  q(save="no")
}

suppressMessages(library(ggplot2))

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

data = read.table(args[['input']], header=T, sep='\t', row.names=1, stringsAsFactors = F)
factorFile = read.table(args[['factorFile']], header=T, sep=',', stringsAsFactors = T, row.names=1)
fac = factorFile[,as.numeric(args[['factorCol']])]
adjust = args[['adjust']]

calc_aov_Pval <- function(data, fac){
  fit = aov(data ~ fac)
  summary(fit)[[1]][["Pr(>F)"]][1] # extractig p-val
}

plot_anova <- function(res, outdir){
  res$names = rownames(res)
  names(res) <- c('p_val', 'adj_pval', 'names')
  p <- ggplot(data=res, aes(x=as.factor(names), y=-log10(adj_pval)))+
    geom_bar(stat=identity)+
    theme_bw()+
    xlab('Bins')+
    ylab('Adjusted p-value (-log10)')

  fileName <- 'AnovaPlot.png'
  filePath <- paste(outdir,'/',fileName, sep='')
  ggsave(filePath, p)
  fileName
}

makeHTML <- function(res, files){
  # files - a list of files to display (files within each entry are displayed next to each other?)
  css.H1 <- '\"text-align: center;font-family:verdana; font-size:30px\"'
  css.textDiv <- '\"text-align=: center; font-family:verdana; font-size:10px; padding-top:35px;padding-bottom=25px\"'

  html <- c('<!DOCTYPE html>',
            '<html>',
            '<head>',
            '</head>',
            '<body')

  html <- c(html,
            paste('<h1 style=',css.H1,'> ANOVA-test Results </h1>',sep=''),
            '<hr>')

  html <- c(html,
            '<div align=\"center\" >',
            paste('<img src=\"', files[1], '\" style=\"width:500px\">', sep=''),
            '<hr>',
            '</div>')

  html <- c(html,
            '<div align=\"center\" >',
            '<table style=\"border:1px solid black; border-collapse:collapse;\">',
            '<tr style=\"border:1px solid black; border-collapse:collapse;font-size:15px;\">',
            #paste('<th>Variable</th>','<th>',names(res)[1],'</th>','<th>',names(res)[2],'</th>', '<th>',names(res)[3],'</th>','<th>',names(res)[4],'</th>', sep=''),
            '<th>Variable</th><th>p-value</th><th>adjusted p-value</th>',
            '</tr>')

  for(i in 1:nrow(res)){
    if(res[i,2]<=0.05){
      html <- c(html,'<tr style=\"color:#FF0000;font-size:12px;\">')
    } else {
      html <- c(html,'<tr style=\"font-size:13px;\">')
    }
    html <- c(html,paste('<th>',row.names(res)[i],'</th>','<th>', ifelse(res[i,1]==0, '<0.001', res[i,1]),'</th>', '<th>',ifelse(res[i,2]==0, '<0.001', res[i,2]),'</th>', sep=''))
    html <- c(html,'</tr>')
  }

  html <- c(html,
            '</table>',
            '</div>')

  html <- c(html,
            '</body>',
            '</html>')

  html
}

pvals = do.call('c', lapply(1:ncol(data), function(i) calc_aov_Pval(data[,i], fac)))
adj.p_vals = p.adjust(pvals, method=adjust)
res = data.frame(p_vals = round(pvals,3), adj.p_vals=round(adj.p_vals,3))
rownames(res) <- names(data)
names(res) = c('p-values', 'adjusted p-values')

if(!dir.exists(args[['outDir']])) dir.create(args[['outDir']], showWarnings = F)

plots = suppressMessages(plot_anova(res, args[['outDir']]))

htmlCode <- makeHTML(res, plots)

htmlFile <- file(args[['output']])
writeLines(htmlCode, htmlFile)
close(htmlFile)

#write.table(res[res,2]<0.05, file=paste(args[['outDir']],'.csv',sep=''),sep=',', row.names=T, col.names=T)
write.table(res, file=paste(args[['outDir']],'/pvals.csv',sep=''),sep=',', row.names=T, col.names=T)
