
args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      T-tests for multivariate data

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --outdir=path - folder for storing outputs
      --factorFile=fact - a factor to use for grouping
      --factorCol=factCol - a factor to use for grouping

      Example:
      ./ttest.R --input=inputFilePath --output=outputFilePath \n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

data = read.table(args[['input']], header=T, sep='\t', row.names=1, stringsAsFactors = F)
factorFile = read.table(args[['factorFile']], header=T, sep=',', stringsAsFactors = T, row.names=1)
factor = factorFile[,as.numeric(args[['factorCol']])]
way = args[['way']]
if (args[['paired']] == 'Y') {paired=T} else {paired=F}
conf.level = as.numeric(args[['conf_level']])
adjust = args[['adjust']]

ttest = function(data, factor, paired, way, conf.level){
  out = t.test(data~factor, paired=paired, alternative = way, conf.level = conf.level)
  return(round(c(out$p.value, out$conf.int[1], out$conf.int[2]),3))
}

plot.Pvals = function(res, main='P_values (T-Test)', showLabels = F){
  # Plot P values as -log10(p)
  labels = rownames(res)
  X = 1:nrow(res)
  sig = res[,"adj_p_val"] <= 0.05
  cols = ifelse(sig, "steelblue","navy")
  
  Y = -log10(res[,"adj_p_val"])
  plot(X,Y, col=cols, pch=16, main=main, xlab='Bins', ylab="-log10(p-val)")#, axes=F)
  if(showLabels) {
    Xsig = X[sig]
    Ysig = Y[sig]
    text(Xsig+0.02*max(Xsig),Ysig+0.02*max(Ysig), labels = rownames(res)[sig])
    }
  abline(h=-log10(0.05))
}

makeHTML <- function(res, pvalsPlot){
  # files - a list of files to display (files within each entry are displayed next to each other?)
  css.H1 <- '\"text-align: center;font-family:verdana; font-size:30px\"'
  css.textDiv <- '\"text-align=: center; font-family:verdana; font-size:10px; padding-top:35px;padding-bottom=25px\"'

  html <- c('<!DOCTYPE html>',
            '<html>',
            '<head>',
            '</head>',
            '<body')

  html <- c(html,
            paste('<h1 style=',css.H1,'> T-test Results </h1>',sep=''),
            '<hr><hr>')
  
  html <- c(html,
            paste('<img src=\"', pvalsPlot, '\" style=\"width:500px\">', sep=''),
            #'<div style=\"text-align=: center; font-family:verdana; font-size:20px\">Barplot showing cumulative variance accounted for by each principal component.</div>',
            '<hr>')
  
  html <- c(html,
            '<div align=\"center\" >',
            '<table style=\"border:1px solid black; border-collapse:collapse;\">',
            '<tr style=\"border:1px solid black; border-collapse:collapse;font-size:15px;\">',
            '<th>Variable</th><th>p-value</th><th>confidence interval</th><th>adjusted p-value</th>',
            '</tr>')

  for(i in 1:nrow(res)){
    if(res[i,4]<=0.05){
      html <- c(html,'<tr style=\"color:#FF0000;font-size:12px;\">')
    } else {
      html <- c(html,'<tr style=\"font-size:13px;\">')
    }
    html <- c(html,paste('<th>',row.names(res)[i],'</th>','<th>', ifelse(res[i,1]==0, '<0.001', res[i,1]),'</th>','<th>',res[i,2],' - ',res[i,3],'</th>','<th>',ifelse(res[i,4]==0, '<0.001', res[i,4]),'</th>', sep=''))
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

res = do.call('rbind', lapply(1:ncol(data), function(i) ttest(data[,i], factor, paired, way, conf.level)))
res = as.data.frame(res)
res$adj.p_val = round(p.adjust(res[,1], method=adjust),3)
rownames(res) <- colnames(data)
names(res) <- c('p_val', 'conf_int_1','conf_int_2','adj_p_val')

pvalsPlot = paste(args[['output']],'/pvals.png',sep='')
png(pvalsPlot)
plot.Pvals(res)
dev.off()

htmlCode <- makeHTML(res)

htmlFile <- file(args[['output']])
writeLines(htmlCode, htmlFile)
close(htmlFile)

#write.table(res, file=args[[output]], row.names=T, col.names=T)