
# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Global alignment tool

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --alignTo=Glucose - peak to align to (currently only Glucose available)

      Example:
      ./GlobalAlign.R --input=inputFilePath --output=outputFilePath --alignTo=path \n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# ================= Functions ======================


# ================================================== 


if('input' %in% names(args)){
data = readNMRTabFile(args[['input']])
} else stop('Data file not given. See ./BinPattern --help')

if('alignTo' %in% names(args)){
} else stop('Aligning to Glucose peak at ...')

data[,2:ncol(data)] = GlobalAlign(data[,2:ncol(data)], data[,1], ...)

# -- write the output
write.table(data, file=args[['output']], row.names=T, col.names=T, sep='\t')