#!/usr/bin/env Rscript

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
      --tails=two.tailed - a string denoting the type of the t-test ('greater','less','two.sided')
      --paired=Y - if the data is paired (Y/N)
      --factorFile2=fact - a factor to use for grouping pairs of observations
      --factorCol2=factCol - a factor column to use for grouping pairs of observations

      Example:
      ./ttest.R --input=inputFilePath --output=outputFilePath \n\n")
  q(save="no")
}

#knitr::knit2html(input = '~/Desktop/test2.rmd', output = '~/Desktop/out.html', quiet = T)

suppressMessages(require(ggplot2))
suppressMessages(require(ggrepel))
suppressMessages(require(knitr))

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# reading data and parameters
data = read.table(args[['input']], header=T, sep='\t', row.names=1, stringsAsFactors = F)
factorFile = read.table(args[['factorFile']], header=T, sep='\t', stringsAsFactors = T, row.names=1)
factor_ = factorFile[,as.numeric(args[['factorCol']])]

tails = args[['tails']]
if (args[['paired']] == 'Y') {
  paired=T
  factorFile2 = read.table(args[['factorFile2']], header=T, sep='\t', stringsAsFactors = T, row.names=1)
  factPairs = factorFile2[,as.numeric(args[['factorCol2']])]
  groupsP = as.numeric(factPairs)
  
} else {
  paired=F
  groupsP = NULL
  }

conf.level = as.numeric(args[['conf_level']])
adjust = args[['adjust']]
outdir = args[['outdir']]

calc_fc = function(data, grp, log2fc=F){
  # calculates fold changes of each column by the 2-group factor
  grpU = unique(as.character(grp))
  fcs = sapply(1:ncol(data), function(i) mean(data[grp==grpU[1],i])/mean(data[grp==grpU[2],i]))
  if (log2fc) fcs = log2(fcs)
  list(fcs=fcs, ratioLab=paste0(grpU[1],'/',grpU[2]))
}

ttest = function(data, factor_, paired = F, groupsP = NULL, tails = 'two.sided', conf.level = 0.05){
  if(!paired) out = t.test(data~factor_, paired=paired, alternative = tails, conf.level = conf.level)
  else {
    factorTmp = as.character(factor_)
    factorTmpU = unique(factorTmp)
    grp1 = data[groupsP > 0,]
    grp1_f = groupsP[groupsP > 0]
    grp2 = data[groupsP < 0,]
    grp2_f = groupsP[groupsP < 0] * -1
    grp1 = grp1[order(grp1_f),]
    grp2 = grp2[order(grp2_f),]
    out = t.test(grp1, grp2, paired=T, alternative = tails, conf.level = conf.level)
  }
  return(round(c(out$p.value, out$conf.int[1], out$conf.int[2]),3))
}

ttest_all = function(data, factor_, paired = F, groupsP = NULL, tails = 'two.sided', conf.level = 0.05, adjust = 'BH'){
  
  res = do.call('rbind', lapply(1:ncol(data), function(i) ttest(data[,i], factor_, paired, groupsP, tails, conf.level)))
  res = as.data.frame(res)
  res$adj_p_vals = round(p.adjust(res[,1], method=adjust),3)
  rownames(res) <- colnames(data)
  names(res) <- c('p_val', 'conf_int_1','conf_int_2','adj_p_vals')
  res
}

plot.Pvals = function(res, showLabels = F, sigLvl = 0.05, main='P_values (T-Test)'){
  
  labels = rownames(res)
  X = factor(1:nrow(res))
  sig = res[,"adj_p_vals"] <= sigLvl
  cols = ifelse(sig, "steelblue","navy")
  Y = -log10(res[,"adj_p_vals"])
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

plot.Bars = function(data, fact, pvals, xlabName='Bins', ylabName='Mean intensity', main='Bin means'){
  dataAgg = aggregate(data, list(fact), mean)
  fact_ = as.character(dataAgg[,1])
  dataAgg = as.matrix(dataAgg[,2:ncol(dataAgg)])
  meanMax = apply(dataAgg, 2, max)
  
  pltTemp = data.frame(means = c(dataAgg[1,], dataAgg[2,]), 
                       groups = rep(fact_, each=ncol(dataAgg)), 
                       names = rep(colnames(data),2),
                       sig = rep(make.SigStars(pvals),2),
                       sigHeight = rep(meanMax, 2) * 1.05)
  
  
  p = ggplot(data = pltTemp, aes(x=names, y=means, group=factor(groups), fill=factor(groups)))+
    geom_bar(stat='identity', position='dodge')+
    scale_fill_discrete(guide = guide_legend(title = "Groups"))+
    geom_text(aes(x = names, y = sigHeight, label = sig))
  
  p = p+
    theme_bw()+
    theme(axis.text.x = element_text(angle=60, hjust=1),
          plot.title = element_text(hjust = 0.5))+
    ggtitle(main)+
    xlab(xlabName)+
    ylab(ylabName)
  
  p
}

plot_volcano = function(fc, pval, labels=NULL, coff = 0.05, title='', logged=F){
    
    if (!logged) fc = log2(fc)
    if (is.null(labels)) labels = ''
    data = data.frame(fc = fc, 
                      pval = (-log10(pval)), 
                      labels = labels,
                      colour = 'steelblue',
                      stringsAsFactors = F)
    coff = -log10(coff)
    data$labels[data$pval < coff] = ''
    data$colour[data$pval >= coff] = 'red'
    
    p = ggplot(data, aes(x=fc, y=pval, label=labels))+
        geom_point(aes(fill=colour), col='black', pch=21, size=4)+
        geom_hline(yintercept=coff, linetype='dashed', col='red')
    
    if(!is.null(labels)) p = p + geom_text_repel()
    
    p = p+ 
        scale_fill_discrete(guide=F)+
        theme_bw(base_size=18) +
        xlab('Fold-change (log)')+
        ylab('P-value (-log10)')+
        ggtitle(title)+
        theme(plot.title = element_text(hjust = 0.5))
    p
}

make.SigStars = function(pvals, lvls = c(0.05, 0.01, 0.001), sgns = c('','*','**','***')){
  getSigLvl = function(x){
    for(i in 1:length(lvls)){
      if(x>lvls[i]) return(sgns[i])
    }
    return(sgns[length(lvls)+1])
  }
  # map over whole vector
  sapply(pvals, getSigLvl)
}

make.MDoutput = function(res, plots, conf.level, adjust){
  output = ''
  #header = '## T-test results\n'
  header = paste('## T-test results\n',
                 '### ',
                 Sys.Date(), '\n','---\n', sep='')
  
  intro = paste('**Total t-test performed:** ',nrow(res),'\n\n',
                '**Correction for multiple testing:** ', adjust,'\n\n',
                '**Number of significant variables:** ', sum(res[,'adj_p_vals']<=conf.level),'\n\n',
                sep='')
  prePlt1 = paste('P-values are plotted on a negative log scale  - larger values on the plot correspond to lower p-values. ',
                  sprintf('The line corresponds to the given significance level ( %.4f ). ', round(conf.level,3)),
                          ' The points are colored to help distinguish the statistically significant results.', sep='')
  plt1 = paste('![](',plots[['p_vals']],')\n', sep='')
  
  prePlt2 = paste('Variables are plotted as bars coloured by group.',
                  'The significance is denoted by the stars drawn above each pair of bars.',
                  sep='')
  plt2 = paste('![](',plots[['bars']],')\n', sep='')
  
  prePlt3 = paste('A volcano plot of the t-tests.')
  plt3 = paste('![](',plots[['volcano']],')\n', sep='')
  
  preTabl = paste('The Following table contains the p-values (raw and adjusted) as well as confidence intervals for the mean difference.',
                  '\n\n',
                  sep='')
  tableOfRes = c('<center>\n', printPvalTableInMD(res), '\n </center> \n')
  
  output = c(output,header, intro, prePlt1, plt1, prePlt2, plt2, prePlt3, plt3,preTabl, tableOfRes)
  
}

#formatConfLvl = function(conf){
#  if(conf<=0.01)
#}

fixCorMetName = function(name){
    names = list(holm = 'Holm method', hochberg = 'Hochberg method', hommel = 'Hommel method',
                 bonferoni = 'Bonferroni method', BH = 'Benjamini-Hochberg method',
                 BY = 'Benjamini-Yakutieli method', 'fdr' = 'Benjamini-Hochberg method',
                 none='None')
    names[name]
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
  makeLine = function(line){
    tmp = paste(line, collapse=' | ')
    c(tmp,' \n')
  }
  firstLine = 'bin | p-value | confidence interval | adjusted p-value \n'
  tabl_ = cbind(as.character(tabl[,1]), as.character(tabl[,2]), paste(tabl[,3], tabl[,4],sep='  :  '), as.character(tabl[,5]))
  sepBar = ':--- | :---: | :---: | :---:\n'
  res = c(firstLine, sepBar, do.call('c', lapply(1:nrow(tabl_), function(x) makeLine(tabl_[x,]))))
  paste(res, collapse=' ')
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

if(!dir.exists(args[['outdir']])) dir.create(args[['outdir']], showWarnings = F)

# calculations
res = ttest_all(data, factor_, paired, groupsP, tails, conf.level)

# plotting
plots = list()

if (nrow(res)>50){
  p1 = plot.Pvals(res[1:50,], showLabels = T, sigLvl = conf.level)
} else{
  p1 = plot.Pvals(res, showLabels = T, sigLvl = conf.level)
}
fileName = 'p_Vals.png'
suppressMessages(ggsave(path = outdir, filename = fileName, plot = p1))
plots[['p_vals']] = paste(outdir, '/', fileName, sep='')

# Only plot 50 values if there are more
if (ncol(data)>50){
  p2 = plot.Bars(data[,1:50], factor_, res[1:50,"adj_p_vals"])
} else {
  p2 = plot.Bars(data, factor_, res[,"adj_p_vals"])
}

fileName = 'meanBars.png'
suppressMessages(ggsave(path = outdir, filename = fileName, plot = p2))
plots[['bars']] = paste(outdir, '/', fileName, sep='')

# Plot volcano
fcs = calc_fc(data, factor_)
p3 = plot_volcano(fcs[['fcs']], res[,'adj_p_vals'], coff = 0.05, 
                  title = sprintf('Volcano plot ( %s , cut-off: %.2f)', fcs[['ratioLab']], conf.level), logged = F)

fileName = 'volcano.png'
suppressMessages(ggsave(path = outdir, filename = fileName, plot = p3))
plots[['volcano']] = paste(outdir, '/', fileName, sep='')

# generating other outputs

res = cbind.data.frame(names(data), res)
names(res) = c('bins',names(res)[2:ncol(res)])
write.table(res, file=paste(outdir,'/pvals.txt', sep=''), sep='\t', row.names=F, col.names=T)

mdEncoded <- make.MDoutput(res, plots, conf.level, fixCorMetName(adjust))
writeLines(mdEncoded, paste(outdir, "/results.Rmd", sep=''))
MDTEST = markdown::markdownToHTML(file = paste(outdir,"/results.Rmd", sep=''))

# TODO implement pdf generation
#if(args[['makepdf']] == 'T') knitr::knit2pdf(input = paste(outdir,"/results.Rmd", sep=''), output = paste(outdir,'/report.pdf',sep=''), quiet = T, compiler = 'texi2pdf')

htmlFile <- file(args[['output']])
writeLines(MDTEST, htmlFile)
close(htmlFile)
