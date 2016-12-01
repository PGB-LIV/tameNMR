
# PCA plots using bokeh

data = dataBinned
grp = newMeta$category

pc = prcomp(data, scale. = T, retx = T)

PCA_scores_plot_bokeh = function(pc, groups, pcs=c(1,2), legendName = 'Groups', outdir=NA){
  pcdf<-data.frame(pc1=pc$x[,pcs[1]], pc2=pc$x[,pcs[2]])
  pcdf$grp = as.factor(groups)
  varAcc = (pc$sdev^2/sum(pc$sdev^2)*100)
  varAcc = round(varAcc,1)
  
  p = figure(title = 'PCA scores plot') %>% 
    ly_points(pc1,pc2,data=pcdf, color=groups, hover=list(pc1,pc2)) %>%
    x_axis(label=paste('PC ',pcs[1],' (',varAcc[pcs[1]],'%)',sep=''), grid=F) %>%
    y_axis(label=paste('PC ',pcs[2],' (',varAcc[pcs[2]],'%)',sep=''), grid=F) %>%
    tool_lasso_select()
  p
}