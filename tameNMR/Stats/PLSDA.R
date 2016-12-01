
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

# PLSDA functions

do_PLSDA = function(data, groups, comp.num=0, choice='Q2'){
  
  suppressMessages(require('pls'))
  suppressMessages(require('caret'))
  
  if(comp.num == 0) comp.num = dim(data)[1]-1
  if(comp.num > 8) {
    comp.num = 8
  }

  # prep data
  #if(length(unique(groups)) > 2) 
    groups = scale(as.numeric(groups))[,1]
  datmat = as.matrix(data)
  
  # perform pls
  plsda.cls <- train(data, groups, "pls", trControl=trainControl(method= 'CV'), tuneLength=comp.num)

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
  }else if(choice == "R2"){
    best.num <- which(all.info[2,] == max(all.info[2,]));
  }else{
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


plot_PLSDA = function(plsdaObj, groups, comp=c(1,2), type='scores', legendName = 'Groups', n.bins=20){
  
  #type = {scores, VIP, varImp} - only top <=n.bins plotted
     diag = plsdaObj$diagnostics[,plsdaObj$n.comp]
  
  if(type == 'scores'){
    tmpData = data.frame(comp1 = plsdaObj$plsda.reg$scores[,comp[1]], comp2 = plsdaObj$plsda.reg$scores[,comp[2]], group = groups)
    p = ggplot(data=tmpData, aes(x=comp1, y=comp2, col=groups))+
      geom_point(size=3)
    
    p = p+
    theme_bw(base_size = 14)+
    guides(col = guide_legend(title = legendName))
    
    
    p = p+ 
      xlab(paste('Component ', comp[1], sep=''))+
      ylab(paste('Component ', comp[2], sep=''))+
      ggtitle(bquote(PLS-DA~scores~(.(plsdaObj$n.comp)~comp.~R^2-.(round(diag[2],2))~Q^2-.(round(diag[3],2)))))
    
  } else if(type == 'VIP'){
    tmpData = plsdaObj$VIPs
    tmpData = tmpData[order(tmpData[,plsdaObj$n.comp]),]
    n.vars = ifelse(nrow(tmpData) >= n.bins, n.bins, nrow(tmpData))
    tmpData = data.frame(vars = as.character(rownames(tmpData)[1:n.vars]), VIPs = tmpData[1:n.vars,plsdaObj$n.comp])
    
    p = ggplot(data = tmpData, aes(x=reorder(vars, VIPs), y=VIPs))+
      geom_point(size=4)
    
    p = p+
    theme_bw(base_size = 14)+
    coord_flip()
    
    p = p+ 
      ylab('VIP score')+
      xlab('Variable')+
      ggtitle(paste('VIP scores for ',plsdaObj$n.comp,'-comp. PLS-DA model',sep=''))
    
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
    
  }
 p 
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
res = do_PLSDA(as.matrix(data), factor_, comp.num=comp.num)

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