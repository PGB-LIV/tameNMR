
# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Making a template for factors from a dataset

      Arguments:
      --input=path - input file path
      --output=path - output file path

      Example:
      ./makeFactTemp.R --input=inputFilePath --output=outputFilePath\n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

# prepare the template
data = read.table(args[['input']], header=T, row.names=1, stringsAsFactors = F, sep='\t')
out = data.frame(samples=rownames(data))
write.table(out, file=args[['output']], sep='\t', row.names = F, col.names = T)