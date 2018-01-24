
# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
  args <- c("--help")
}

if("--help" %in% args) {
  cat("
      Manuipulation of NMR spectra file

      Arguments:
      --input=path - input file path
      --input2=path - (optional) second input file path (for add/merge)
      --output=path - output file path
      --action=path - action to perform (add,remove,merge)
      --colName=name - (optional) column to be added/removed

      Example:
      ./add_rem_merge.R --input=inputFilePath --output=outputFilePath --action=rem --colName=samp_1 \n\n")
  q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]


# ----------------- functions -------------------------------

readNMRTabFile <- function(inpath){
  data = read.table(inpath, header=T, sep='\t', stringsAsFactors = F)
  data = as.matrix(data)
  data
}

addCol = function(dataFrom, dataTo, col){
  if(nrow(data1) == nrow(data2)){
    dataTo[,col] = dataFrom[,col]
  } else stop('Spectra are of different length.')
  dataTo
}

removeCol = function(data, col){
  data[,-which(names(data) == col)]
}

mergeData = function(data1, data2){
  if(nrow(data1) == nrow(data2)){
    #TODO: make sure the data columns have no duplicate names
    data = cbind(data1, data2)
  } else stop('Spectra are of different length.')
  data
}

# -------------------------------------------------------------------

# -- read data
if('input' %in% names(args)){
  data1 = readNMRTabFile(args[['input']])
} else stop('Data file not given. See ./add_rem_merge.R --help')

if('action' %in% names(args)){
  action = argsp[['action']]
} else stop('Action not specified. See ./add_rem_merge.R --help')

if('input2' %in% names(args)){
  data2 = readNMRTabFile(args[['input']])
} else if(action %in% c('add','merge')) stop('Data file not given. See ./add_rem_merge.R --help')

if('colName' %in% names(args)){
  colName = args[['colName']]
} else if(action %in% c('add','remove')) stop('Column name not given. See ./add_rem_merge.R --help')


if (action == 'remove'){
  output = removeCol(data1, colName)
} else if (action == 'add'){
  output = addCol(data1, data2, colName)
} else if (action == 'merge'){
  output = mergeData(data1, data2)
}

# -- write the output
write.table(output, file=args[['output']], row.names=T, col.names=T, sep='\t')
