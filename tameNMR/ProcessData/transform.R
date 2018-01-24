#!/usr/bin/env Rscript

# Tool for data transformations

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Transform

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --method=method - what data transformation to apply (log, sqrt)

      Example:
      ./transform.R --input=inputFilePath --output=outputFilePath --method=log\n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# import data

data = read.table(args[['input']], header=T, sep='\t', row.names=1, stringsAsFactors = F)
data_ = as.matrix(data)

if('method' %in% names(args)) {
    method = args[['method']]
} else stop('No method provided')

# Transform data

if (args[['method']] == 'log'){
    if(sum(data_<=0) == 0) data_ = log(data_)
    else stop('Data contains numbers <= 0. Cannot calculate log.')
} else if (args[['method']] == 'sqrt'){
    if(sum(data_<=0) == 0) data_ = sqrt(data_)
    else stop('Data contains numbers < 0. Cannot calculate square root.')
} else stop(sprintf('No method %s', method))

data_ = as.data.frame(data_)
rownames(data_) = rownames(data)
colnames(data_) = colnames(data)

# write outputs
write.table(data_, file=args[['output']], sep='\t', row.names=T, col.names=T)
