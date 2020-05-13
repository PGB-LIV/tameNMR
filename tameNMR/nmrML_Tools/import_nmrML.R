
# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
    args <- c("--help")
}

if("--help" %in% args) {
    cat("
      Read processed data from nmrML files

      Arguments:
      --inData=path - input file path
      --output=path - output bin table file path

      Example:
        ./read_nmrML.R --inData=pathTonmrMLFile --output=pathToOutput")
    
    q(save="no")
}

parseArgs = function(x) strsplit(sub("^--", "", x),"=")
argsDF = as.data.frame(do.call('rbind', parseArgs(args)))
args = as.list(as.character(argsDF[,2]))
names(args) <- argsDF[,1]


library(XML)
library(caTools)
library(ggplot2)

input_file = args[['inData']]
output_path = args[['output']]


get_spectrum = function(root){
    
    SpecList = xmlElementsByTagName(root, "spectrumList", recursive = TRUE)$spectrumList
    spectra = xmlChildren(SpecList)
    xmlSpec = spectra[[1]]
    #spectra_lst = lapply(spectra, get_spectrum)
    
    # get number of points
    numODP = as.numeric(xmlGetAttr(xmlSpec, 'numberOfDataPoints'))
    # get the ppm scale
    xAxis = xmlChildren(xmlSpec)$xAxis
    ppm_start = as.numeric(xmlGetAttr(xAxis, 'startValue'))
    ppm_end = as.numeric(xmlGetAttr(xAxis, 'endValue'))
    ppm = seq(ppm_start, ppm_end, length.out = numODP)
    
    # get the spectrum
    specData = xmlValue(xmlChildren(xmlSpec)$spectrumDataArray)
    spec_raw = memDecompress(base64decode(specData, 'raw'), type='gzip')
    spectrum = readBin(spec_raw, 'double', numODP)
    
    return(data.frame('ppm'=ppm, 'spectrum'=spectrum))
}

tree = xmlTreeParse(input_file)
root = xmlRoot(tree)

fid = get_fid(root)
fid_DF = data.frame(xaxis=1:length(fid),Real=Re(fid), Imaginary=Im(fid))

spec = get_spectrum(root)

write.table(data, file=output_path, col.names = T, row.names = F, sep='\t')