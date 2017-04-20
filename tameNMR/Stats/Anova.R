
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
suppressMessages(library(ggrepel))

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

data = read.table(args[['input']], header=T, sep='\t', row.names=1, stringsAsFactors = F)
factorFile = read.table(args[['factorFile']], header=T, sep='\t', stringsAsFactors = T, row.names=1)
fac = as.factor(factorFile[,as.numeric(args[['factorCol']])])
adjust = args[['adjust']]
outdir = args[['outdir']]
conf.level = as.numeric(args[['conf_level']])

calc_aov_Pval <- function(data, fac){
  fit = aov(data ~ fac)
  summary(fit)[[1]][["Pr(>F)"]][1] # extractig p-val
}

plot.Pvals = function(res, showLabels = F, sigLvl = 0.05, main='P_values (ANOVA)'){
  
  labels = rownames(res)
  X = factor(1:nrow(res))
  sig = res[,"adj.p-val"] <= sigLvl
  cols = ifelse(sig, "steelblue","navy")
  #cols = factor(ifelse(sig, "significant","non-significant"))
  Y = -log10(res[,"adj.p-val"])
  pltTemp = data.frame(X=X, Y=Y, cols=cols)
  rownames(pltTemp) = labels
  
  p = ggplot(data=pltTemp, aes(x=X, y=Y))+
    geom_point(col=cols)+
    geom_abline(intercept = -log10(sigLvl), slope = 0, col='red')
  
  if(showLabels) p = p + geom_text_repel(aes(x = X, y=Y, label=rownames(res)))
  
  p = p+ 
    theme_bw()+
    ggtitle('P-values')+
    xlab('Bins')+
    ylab('-log10( p-value )')+
    theme(axis.ticks.x=element_blank(), 
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5))
  p
}

plot_anova <- function(res){
  res$names = rownames(res)
  names(res) <- c('p_val', 'adj_pval', 'names')
  p <- ggplot(data=res, aes(x=as.factor(names), y=-log10(adj_pval)))+
    geom_bar(stat=identity)+
    theme_bw()+
    xlab('Bins')+
    ylab('Adjusted p-value (-log10)')

  p
}

plot.ANOVA = function(res, main='Significant Bins (ANOVA)', showLabels = F){
  labels = rownames(res)
  X = 1:nrow(res)
  sig = res[,2] <= 0.05
  cols = ifelse(sig, "steelblue","navy")
  
  Y = -log10(res[,2])
  plot(X,Y, col=cols, pch=16, main=main, xlab='Bins', ylab="-log10(p-val)")#, axes=F)
  if(showLabels) {
    Xsig = X[sig]
    Ysig = Y[sig]
    text(Xsig+0.02*max(Xsig),Ysig+0.02*max(Ysig), labels = rownames(res)[sig])
    }
  abline(h=-log10(0.05))
}

do_anova_Multi = function(data, groups, adjustMethod='fdr', thresh=0.05){
  aov.res = apply(data,2,function(x) aov(x~groups))
  anova.res = lapply(aov.res, anova)
  res<-do.call('rbind', lapply(anova.res, function(x) { c(x["F value"][1,], x["Pr(>F)"][1,])}))
  res = cbind(res,p.adjust(res[,2],method = adjustMethod))
  colnames(res) = c('F-stat','p-val','adj.p-val')
  #sigs = which(res[,2]<=thresh)
  
  posthoc.res<-lapply(aov.res, TukeyHSD, conf.level=1-thresh)
  tukey_pvals = extract.pVals.Tukey(posthoc.res, thresh)
  
  return(list(anova_pvals = res, posthoc_res = posthoc.res, tukey_pvals = tukey_pvals, anova_res = anova.res))
}

extract.pVals.Tukey = function(tukey.res, thresh=0.05){
  do.call('rbind', lapply(tukey.res, function(x) x[1][[1]][,'p adj']))
}

make.MDoutput = function(res, plots, conf.level){
  output = ''
  #header = '## T-test results\n'
  header = paste('## One-way ANOVA test results\n',
                 '### ',
                 Sys.Date(), '\n','---\n', sep='')
  
  intro = paste('**Total ANOVA tests performed:** ',nrow(res$anova_pvals),'\n\n',
                '**Number of significant variables:** ', sum(res$anova_pvals[,'adj.p-val']<=conf.level),
                '\n\n',sep='')
  prePlt1 = paste('P-values are plotted on a negative log scale  - larger values on the plot correspond to lower p-values.',
                  sprintf('The line corresponds to the given significance level ( %.3f ).', conf.level),
                  'The points are colored to help distinguish the statistically significant results.\n', sep='')
  plt1 = sprintf('![](%s)\n', plots[1])
  
  preTabl = paste('The Following table contains the p-values (raw and adjusted).',
                  '\n\n',
                  sep='')
  tableOfRes = c('<center>\n', printPvalTableInMD(res$anova_pvals), '\n </center> \n')
  
  preTabl2 = paste('Results of Tukey post-hoc analysis.',
                  '\n\n',
                  sep='')
  tableOfRes2 = '' #c('<center>\n', printPvalTableInMD(res$tukey_pvals), '\n </center> \n')
  
  output = c(output,header, intro, prePlt1, plt1, preTabl, tableOfRes)
  output
}

printTableInMD = function(tabl){
  makeLine = function(line){
    i = paste(line, collapse=' | ')
    c(i,' \n')
  }
  
  firstLine = makeLine(colnames(tabl))
  sepBar = c(rep('---|', ncol(tabl)-1),'---\n')
  res = c(firstLine, sepBar, do.call('c', lapply(1:nrow(tabl), function(x) makeLine(tabl[x,]))))
  paste(res, collapse=' ')
}

printPvalTableInMD = function(tabl){
  makeLine = function(line, rn){
    i = paste(line, collapse=' | ')
    c(paste(rn,' | ',sep=''), i,' \n')
}
  
  #firstLine = makeLine(colnames(tabl))
  firstLine = 'bin | p-value | adjusted p-value \n'
  #tabl = as.matrix(tabl)
  tabl_ = cbind(rownames(tabl), tabl[,2], tabl[,3])
  #sepBar = c(rep('---|', ncol(tabl)-1),'---\n')
  sepBar = ':--- | :---: | :---: | :---:\n'
  res = c(firstLine, sepBar, do.call('c', lapply(1:nrow(tabl_), function(x) makeLine(tabl_[x,], rownames(tabl)[x]))))
  paste(res, collapse=' ')
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

resAnova = do_anova_Multi(data, fac)
TukeyFilter = resAnova$tukey_pvals <= 0.05
SigBins = apply(TukeyFilter, 2, sum)


#pvalsPlot = paste(args[['output']],'/pvals.png',sep='')
#plot.ANOVA(resAnova$anova_pvals)

res = data.frame(bins = names(data), 
                 p_vals = round(resAnova$anova_pvals[,2],3), 
                 adj.p_vals=round(resAnova$anova_pvals[,3],3))
#rownames(res) = names(data)
#names(res) = c('p-values', 'adj.p-val')

if(!dir.exists(outdir)) dir.create(outdir , showWarnings = F)

plots = c()
fileName <- 'AnovaPlot.png'
filePath <- paste(outdir,'/',fileName, sep='')
p = suppressMessages(plot.Pvals(resAnova$anova_pvals))
suppressMessages(ggsave(filename = fileName, plot = p, path = outdir))
plots = c(plots,filePath)

write.table(res, file=paste(outdir,'/pvals.txt',sep=''), sep='\t', row.names=F, col.names=T)

mdEncoded <- make.MDoutput(resAnova, plots, conf.level)
writeLines(mdEncoded, paste(outdir, "/results.Rmd", sep=''))
MDTEST = markdown::markdownToHTML(file = paste(outdir,"/results.Rmd", sep=''))

htmlFile <- file(args[['output']])
writeLines(MDTEST, htmlFile)
close(htmlFile)

#knitr::knit2html(input = paste(outdir,"/results.Rmd", sep=''), output = outdir, quiet = T)

