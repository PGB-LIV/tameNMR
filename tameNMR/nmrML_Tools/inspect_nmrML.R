
# --- Parse the command line arguments ---

args <- commandArgs(TRUE)

if (length(args) < 1) {
    args <- c("--help")
}

if("--help" %in% args) {
    cat("
      Read and summarise data from nmrML files

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
outdir = args[['outDir']]
if(!dir.exists(outdir)) dir.create(outdir, showWarnings = F)

# ==================== Functions ====================  

get_fid = function(root){
  # extract fid data
  xml_fid = xmlElementsByTagName(root, "fidData", recursive = TRUE)$acquisition
  fid_str = xmlValue(xml_fid)
  
  # decompress and convert to complex form
  fid_raw = memDecompress(base64decode(fid_str, "raw"), type='gzip')
  fid = readBin(fid_raw, what='double', n=length(fid_raw)+1, size=4)
  fid_complex = complex(real=fid[c(T,F)], imaginary=fid[c(F,T)])
  fid_complex
}

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

get_acu_pars = function(root){}

get_proc_pars = function(root){}


# ===================================================  



tree = xmlTreeParse(input_file)
root = xmlRoot(tree)

fid = get_fid(root)
fid_DF = data.frame(xaxis=1:length(fid),Real=Re(fid), Imaginary=Im(fid))

spec = get_spectrum(root)

# plot fid Real
p_fid_real = ggplot(fid_DF, aes(x=xaxis, y=Real))+
  geom_line()+
  theme_bw()+
  ggtitle('FID (Real)')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('')+
  ylab('')

# plot fid Imaginary
p_fid_imag = ggplot(fid_DF, aes(x=xaxis, y=Imaginary))+
  geom_line()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle('FID (Imaginary)')+
  xlab('')+
  ylab('')

# plot spectrum processed
p_proc = ggplot(spec, aes(x=ppm, y=spectrum))+
  geom_line()+
  scale_x_reverse()+
  theme_bw()+
  ggtitle('Processed spectrum')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('ppm')+
  ylab('intensity')

make.MDoutput = function(plts){
  output = ''
  header = paste('## nmrML file summary\n',
                 '### ',
                 Sys.Date(), '\n','---\n', sep='')
  
  intro = ''
  prePlt1 = '\n'
  plt1 = sprintf('![](%s)\n', plts[[1]])
  
  prePlt2 = '\n'
  plt2 = sprintf('![](%s)\n', plts[[2]])
  
  prePlt3 = '\n'
  plt3 = sprintf('![](%s)\n', plts[[3]])
  
  output = c(header, intro, prePlt1, plt1, prePlt2, plt2, prePlt3, plt3)
  output
}


suppressMessages(ggsave(filename = 'fid_real.png', plot = p_fid_real, path = outdir))
suppressMessages(ggsave(filename = 'fid_im.png', plot = p_fid_imag, path = outdir))
suppressMessages(ggsave(filename = 'spec.png', plot = p_proc, path = outdir))

plts = list(paste0(outdir,'/fid_real.png'),
            paste0(outdir,'/fid_im.png'),
            paste0(outdir,'/spec.png'))

mdEncoded <- make.MDoutput(plts)
writeLines(mdEncoded, paste(outdir, "/results.Rmd", sep=''))
MDfile = markdown::markdownToHTML(file = paste(outdir,"/results.Rmd", sep=''))

htmlFile <- file(output_path)
writeLines(MDfile, htmlFile)
close(htmlFile)

