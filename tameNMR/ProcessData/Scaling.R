#!/usr/bin/env Rscript

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Scaling

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --method=method - what scaling method to apply (auto, pareto, range)

      Example:
      ./scaling.R --input=inputFilePath --output=outputFilePath --method=auto\n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# normalize to zero mean and unit variance
AutoScale<-function(x){
  (x - mean(x))/sd(x, na.rm=T);
}

# normalize to zero mean but varaince/SE
ParetoScale<-function(x){
  (x - mean(x))/sqrt(sd(x, na.rm=T));
}

# normalize to zero mean but varaince/SE
RangeScale<-function(x){
  if(max(x) == min(x)){
    x;
  }else{
    (x - mean(x))/(max(x)-min(x));
  }
}

# import data

data = read.table(args[['input']], header=T, sep='\t', row.names=1, stringsAsFactors = F)
data_ = as.matrix(data)

# write outputs

if (args[['method']] == 'auto'){
  data_ = apply(data_, 2, AutoScale)
} else if (args[['method']] == 'pareto'){
  data_ = apply(data_, 2, ParetoScale)
} else if (args[['method']] == 'range') {
  data_ = apply(data_, 2, RangeScale)
} else if (args[['method']] == 'mean') {
  data_ = scale(data_, center=T, scale=F)
}

data_ = as.data.frame(data_)
rownames(data_) = rownames(data)
colnames(data_) = colnames(data)

write.table(data_, file=args[['output']], sep='\t', row.names=T, col.names=T)
