
""" A NMR data import into csv """

import os
import sys
import zipfile
import tarfile
import shutil
import traceback

#import nmrglue as ng
import pandas as pd
import numpy as np


# hacky way to suppress deprecation warnings
import warnings

warnings.filterwarnings("ignore")
import nmrglue as ng

def main(args):
    """ Main function that runs the wrapper"""

    source = args[2]

    try:
        if source == 'ftp':
            galaxyPath = getGalaxyPath(args[0],args[1])
            infile = os.path.join(galaxyPath,args[0])
        else:
            infile = args[0]

        outfile = args[1]
    except:
        print 'usage: import2csv.py <infile> <outfile>'
        sys.exit(2)

    try:
        infile = extractInfile(infile, outfile[:-4])
    except IOError as e:
        print e.message

    try:
        if os.path.isdir(infile):
            spectraDirs = os.listdir(infile)
        else:
            sys.exit(1)
    except IOError as e:
        print e.message

    importedFiles = []
    for spec in spectraDirs:
        if os.path.isdir(os.path.join(infile,spec)):
            importedFiles.append(readBruker(os.path.join(infile,spec)))


    writeCsv(importedFiles, outfile)

    # remove the unzipped folder
    try:
        shutil.rmtree(infile)
    except FileNotFoundError as e:
        print e.message
################################################################################
# functions #
################################################################################

def getGalaxyPath(infile, outfile):
    res = []
    infile = infile.split('/')
    outfile = outfile.split('/')
    if infile[0] in outfile:
        for i in outfile:
            if i != infile[0]:
                res.append(i)
            else:
                break
        return ('/'.join(res))
    else:
        sys.exit(2)


def extractInfile(infile, outFolder):
    """
    Extracts the archive at <infile> if it is a zip or tar.gz and returns a
    path to the folder containing contents of the archive, otherwise returns
    the path.

    Added support for zip files masqueraded as .dat for galaxy upload.
    """
    if infile.endswith('.zip') or infile.endswith('.dat'):
        try:
            with zipfile.ZipFile(infile,'r') as zf:
                zf.extractall(path=outFolder)
            return outFolder
        except IOError:
            traceback.print_exc()
            print 'could not unzip'

    elif(infile.endswith('.tar.gz') or infile.endswith('tar')):
        try:
            with tarfile.open(infile,'r') as tf:
                tf.extractall(path=outFolder)
            return outFolder
        except IOError as e:
            print e.message
    else:
        return infile

def readBruker(path):
    dic, data = ng.bruker.read(path)
    udic = ng.bruker.guess_udic(dic, data)
    uc = ng.fileiobase.uc_from_udic(udic)
    ppm_scale = uc.ppm_scale()

    with open(path+'/pdata/1/title') as fl:
        name = fl.readline().rstrip()

    name.replace(' ','_')
    dic, data = ng.bruker.read_pdata(path+'/pdata/1/')
    ppm_scale = np.linspace(max(ppm_scale), min(ppm_scale), len(data))
    return((name, ppm_scale, data))

def writeCsv(fileList, outFile):
    lengths = []
    for i in range(len(fileList)):
        lengths.append(fileList[i][2].shape[0])
    if lengths[:-1] == lengths[1:]:  # chscking if all lengths are the same
        dataDF = fileList2DataFrame(fileList)
        dataDF.to_csv(outFile, sep='\t', index=True, header=True)
    else:
        fileList = interpolateSpectra(fileList)
        dataDF = fileList2DataFrame(fileList)
        dataDF.to_csv(outFile, sep='\t', index=True, header=True)

#TODO: implement spectra interpolation
def interpolateSpectra(fileList):
    pass

def fileList2DataFrame(fileList):
    data = {x[0]:x[2] for x in fileList}
    ppm = fileList[0][1]
    dataDF = pd.DataFrame.from_dict(data, orient='columns')
    dataDF.index = ppm
    return(dataDF)



# not used anymore
#def calcPpms(dic, Npoints):
#    offset = (float(dic['acqus']['SW']) / 2) - (float(dic['acqus']['O1']) / float(dic['acqus']['BF1']))
#    start = float(dic['acqus']['SW']) - offset
#    end = -offset
#    step = float(dic['acqus']['SW']) / zero_fill_size
#    ppms = np.arange(start, end, -step)[:zero_fill_size]
#    return ppms

if __name__ == "__main__":
    main(sys.argv[1:])
