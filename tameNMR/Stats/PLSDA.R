#!/usr/bin/env Rscript

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
suppressMessages(require(ellipse))
suppressMessages(require(reshape2))
# -------------------functions --------------------

make.factorMat = function(fact){
  fact = as.character(fact)
  unFact = unique(fact)
  N = length(unFact)
  out = do.call('rbind', lapply(fact, function(currFact) { y=rep(0,N); y[which(unFact == currFact)] = 1 ; y}))
  colnames(out) <- unFact
  out
}

# PLSDA functions

do_PLSDA = function(data, groups, comp.num=0, choice='Q2'){
  
  suppressMessages(require('pls'))
  suppressMessages(require('caret'))
  
  if(comp.num == 0) comp.num = dim(data)[1]-1
  if(comp.num > 8) {
    comp.num = 8
  }

  # prep data
  #if(length(unique(groups)) > 2) groups = scale(as.numeric(groups))[,1]
  #else groups = factor(groups)
  groups = scale(as.numeric(groups))[,1]
  datmat = as.matrix(data)
  
  # perform pls
  suppressWarnings(plsda.cls <- train(data, groups, "pls", trControl=trainControl(method= 'CV'), tuneLength=comp.num))

  # use the classifical regression to get R2 and Q2 measure
  plsda.reg <- plsr(groups ~ datmat, method ='oscorespls', ncomp=comp.num, validation= 'CV')
  fit.info <- pls::R2(plsda.reg, estimate = "all")$val[,1,]
  
  # combine accuracy, R2 and Q2
  accu <- plsda.cls$results[,2]
  all.info <- rbind(accu, fit.info[,-1]);
  rownames(all.info) <- c("Accuracy", "R2", "Q2");
  
  #plsObj = plsr(groups~data, method='oscorespls', ncomp=comp.num);
  # default use best number determined by Q2
  if(choice == 'Q2'){
    best.num <- which(all.info[3,] == max(all.info[3,]));
  } else if(choice == "R2"){
    best.num <- which(all.info[2,] == max(all.info[2,]));
  } else{
    best.num <- which(all.info[1,] == max(all.info[1,]));
  }
  
  varImp = calc.VarImp(plsda.cls, length(unique(groups)))
  VIPS = calc.VIP(plsda.reg, best.num)
  
  return(list(n.comp=best.num, varImp = varImp, VIPs = VIPS, plsda.reg=plsda.reg, plsda.cls=plsda.cls, diagnostics = all.info))
}

calc.VIP = function(pls, compNum){
  # calculate VIP http://mevik.net/work/software/VIP.R
  b <- c(pls$Yloadings)[1:compNum]
  T <- pls$scores[,1:compNum, drop = FALSE]
  SS <- b^2 * colSums(T^2)
  W <- pls$loading.weights[,1:compNum, drop = FALSE]
  Wnorm2 <- colSums(W^2);
  SSW <- sweep(W^2, 2, SS / Wnorm2, "*")
  vips <- sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS));
  if(compNum > 1){
    vip.mat <- as.matrix(t(vips));
  }else{
    vip.mat <- as.matrix(vips);
  }
  colnames(vip.mat) <- paste("Comp.", 1:ncol(vip.mat));
  vip.mat  
}

calc.VarImp = function(pls, group.num){
  
  coef.mat <- try(varImp(pls, scale=T)$importance);
  if(class(coef.mat) == "try-error") {
    coef.mat <- NULL;
  }else{
    if(group.num > 2){ # add an average coef for multiple class
      coef.mean<-apply(coef.mat, 1, mean);
      coef.mat <- cbind(coef.mean = coef.mean, coef.mat);
    }
    # rearange in decreasing order, keep as matrix, prevent dimesion dropping if only 1 col
    inx.ord<- order(coef.mat[,1], decreasing=T);
    coef.mat <- data.matrix(coef.mat[inx.ord, ,drop=FALSE]);
  }
  coef.mat
}

plot_PLSDA = function(plsdaObj, groups, comp=c(1,2), type='scores', legendName = 'Groups', n.bins=20, drawEllipse=T){
  #type = {scores, VIP, varImp} - only top <=n.bins plotted
     diag = plsdaObj$diagnostics[,plsdaObj$n.comp]
  
  if(type == 'scores'){
    tmpData = data.frame(comp1 = plsdaObj$plsda.reg$scores[,comp[1]], comp2 = plsdaObj$plsda.reg$scores[,comp[2]], group = groups)
    p = ggplot(data=tmpData, aes(x=comp1, y=comp2, col=groups))+
      geom_point(size=3)
    
    p = p+
    theme_bw(base_size = 14)+
    guides(col = guide_legend(title = legendName))
    
    if (drawEllipse){
      # Generate an ellipse for each groups of points 
      df_ell = do.call('rbind', lapply(levels(as.factor(groups)), function(grp) {
        ellipse(cor(tmpData[which(groups==grp),'comp1'], tmpData[which(groups==grp),'comp2']), 
                scale=c(sd(tmpData[which(groups==grp),'comp1']),sd(tmpData[which(groups==grp),'comp2'])), 
                centre=c(mean(tmpData[which(groups==grp),'comp1']),mean(tmpData[which(groups==grp),'comp2'])))
      }
      ))
      df_ell = as.data.frame(df_ell)
      grp = levels(as.factor(groups))
      grp = do.call('c', lapply(grp, function(x) rep(x, 100)))
      df_ell$group = grp
    }
    
    if (drawEllipse){
      
      #cols <- rainbow(length(unique(as.character(groups))))
      cols = gg_color_hue(length(unique(as.character(groups))))
      p <- p+
        geom_path(data=df_ell, aes(x=x, y=y, colour=group ), size=1, linetype=2)+
        scale_colour_manual(values=cols, guide=F)
      }
    
    p = p+ 
      xlab(paste('Component ', comp[1], sep=''))+
      ylab(paste('Component ', comp[2], sep=''))+
      ggtitle(bquote(PLS-DA~scores~(.(plsdaObj$n.comp)~comp.~R^2-.(round(diag[2],2))~Q^2-.(round(diag[3],2)))))+
      theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')
    p
    
  } else if(type == 'VIP'){
    
    tmpData = plsdaObj$VIPs
    if(ncol(tmpData)!=1) {tmpData = tmpData[order(tmpData[,plsdaObj$n.comp]),]}
    else {tmpData = data.frame(x=sort(tmpData))}
    n.vars = ifelse(nrow(tmpData) >= n.bins, n.bins, nrow(tmpData))
    tmpData = data.frame(vars = as.character(rownames(tmpData)[1:n.vars]), VIPs = tmpData[1:n.vars,plsdaObj$n.comp])
    
    #tmpData = plsdaObj$VIPs
    #tmpData = tmpData[order(tmpData[,plsdaObj$n.comp]),]
    #n.vars = ifelse(nrow(tmpData) >= n.bins, n.bins, nrow(tmpData))
    #tmpData = data.frame(vars = as.character(rownames(tmpData)[1:n.vars]), VIPs = tmpData[1:n.vars,plsdaObj$n.comp])
    
    tmpSegm = data.frame(x1=rep(0, nrow(tmpData)), y1=1:nrow(tmpData), x2=tmpData$VIPs, y2=1:nrow(tmpData))
    
    p = ggplot(data = tmpData, aes(x=reorder(vars, VIPs), y=VIPs))+
      geom_point(size=4)+
      geom_segment(data=tmpSegm, aes(x=y1 ,y=x1 , xend=y2 ,yend=x2), linetype='dotted', col='black')
    
    p = p+
    theme_bw(base_size = 14)+
    coord_flip()
    
    p = p+ 
      ylab('VIP score')+
      xlab('Variable')+
      theme(plot.title = element_text(hjust = 0.5))+
      ggtitle(paste('VIP scores for ',plsdaObj$n.comp,'-comp. PLS-DA model',sep=''))
    p
    
  } else if(type == 'varImp'){
    n.vars = ifelse(nrow(plsdaObj$varImp) > 20, 20 , nrow(plsdaObj$varImp))
    tmpData = data.frame(vars = as.character(rownames(plsdaObj$varImp)[1:n.vars]), varImp = plsdaObj$varImp[1:n.vars,1])
    
    p = ggplot(data = tmpData, aes(x=reorder(vars, varImp), y=varImp))+
      geom_point(size=4)
    
    p = p+
    theme_bw(base_size = 14)+
    coord_flip()
    
    p = p+ 
      ylab('Variable importance score')+
      xlab('Variable')+
      ggtitle(paste('Variable importance scores for ',plsdaObj$n.comp,'-comp. PLS-DA model',sep=''))
    p
    
  } else if(type == 'diagnostics'){
    diags = plsdaObj$diagnostics
    diags_ = t(as.matrix(diags))
    rownames(diags_) = colnames(diags)
    colnames(diags_) = rownames(diags)
    tempDF = as.data.frame(diags_)
    tempDF$components = 1:nrow(diags_)
    tempDF = melt(tempDF, id.vars = 'components')
    
    p = ggplot(data=tempDF, aes(x=components, y=value, group=variable, col=variable))+
      geom_line()+
      geom_point()
    
    p = p+
      ggtitle('PLS-DA diagnostics')+
      xlab('# of components')+
      ylab('Value')+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = 'bottom')+
      scale_color_discrete(guide=guide_legend(title='Measure'))
    p
  }
 p 
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

make.MDoutput = function(plts, n.comp){
  output = ''
  header = paste('## Partial Least-Squares Discriminant Analysis results\n',
                 '### ',
                 Sys.Date(), '\n','---\n', sep='')
  
  intro = paste('PLS-DA performed on the dataset using k-fold cross-validation.\n',
                sprintf('%d PLS component%s selected using Q^2 metric.', n.comp, ifelse(n.comp==1, ' was', 's were')), sep='')
  prePlt1 = 'Scores plot below shows the first two PLS components.'
  plt1 = sprintf('![](%s)\n', plts[1])
  prePlt2 = 'VIP plot showing up to 20 most influential variables.'
  plt2 = sprintf('![](%s)\n', plts[2])
  prePlt3 = 'Diagnostics plot showing the cross-validation diagnostic measures.'
  plt3 = sprintf('![](%s)\n', plts[3])
  
  output = c(header, intro, prePlt1, plt1, prePlt2, plt2, prePlt3, plt3)
  output
}
# -------------------------------------------------


parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# import data

data = read.table(args[['input']], header=T, sep='\t', row.names=1, stringsAsFactors = F)
factorFile = read.table(args[['factorFile']], header=T, sep='\t', stringsAsFactors = T, row.names=1)
grp = factorFile[,as.numeric(args[['factorCol']])]
grp = as.factor(grp)
outdir = args[['outDir']]

#factor_  = make.factorMat(factor)

comp.num = nrow(data) - 1;
if(comp.num > 8) comp.num = 8

#res = plsr(factor_ ~ as.matrix(data), method='oscorespls', ncomp=comp.num)
res = suppressMessages(do_PLSDA(as.matrix(data), grp, comp.num=comp.num))

# make output folder if it does not exist
if(!dir.exists(outdir)) dir.create(outdir, showWarnings = F)

# plotting
plots = c()

p1 = plot_PLSDA(res, grp, type='scores')
fileName = 'PLSDA_Scores.png'
suppressMessages(ggsave(path = outdir, filename = fileName, plot = p1))
plots = c(plots, paste(outdir, '/', fileName, sep=''))

# Only plot 50 values if there are more
p2 = plot_PLSDA(res, grp, type='VIP')
fileName = 'VIP.png'
suppressMessages(ggsave(path = outdir, filename = fileName, plot = p2))
plots = c(plots, paste(outdir, '/', fileName, sep=''))

p3 = plot_PLSDA(res, grp, type='diagnostics')
fileName = 'Diagnostics.png'
suppressMessages(ggsave(path = outdir, filename = fileName, plot = p3))
plots = c(plots, paste(outdir, '/', fileName, sep=''))
 
mdEncoded <- make.MDoutput(plots, res[[1]])
writeLines(mdEncoded, paste(outdir, "/results.Rmd", sep=''))
MDTEST = markdown::markdownToHTML(file = paste(outdir,"/results.Rmd", sep=''))

htmlFile <- file(args[['output']])
writeLines(MDTEST, htmlFile)
close(htmlFile)
#knitr::knit2html(input = paste(outdir,"/results.Rmd", sep=''), output = outdir, quiet = T)
