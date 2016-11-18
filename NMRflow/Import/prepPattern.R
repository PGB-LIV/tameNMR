
# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Formatting of pattern files

      Arguments:
      --input=path - input file path
      --output=path - output file path
      --type=format - {BrukerPattern, csvTable}

      Example:
      ./prepPattern.R --input=inputFilePath --output=outputFilePath --type=BrukerPattern \n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]

readBrukerPatternFile <- function(path){
  pf <- read.table(path, sep="", header=F, skip=9, stringsAsFactors = F)
  pf[,c(3,4,6)]
}

if (args[['type']] == "BrukerPattern"){
  pattern = readBrukerPatternFile(args[['input']])
} else if (args[['type']] ==  'csvTable'){
  pattern = read.table(args[['input']], header=F, stringsAsFactors = F, sep=',')
} else if (args[['type']] ==  'tabularTable'){
  pattern = read.table(args[['input']], header=F, stringsAsFactors = F, sep='\t')
}

# TODO make sure that pattern file is valid

write.table(pattern, file=args[['output']], col.names = F, row.names = F, sep='\t')