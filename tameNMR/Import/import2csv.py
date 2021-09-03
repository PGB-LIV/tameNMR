
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
        print('usage: import2csv.py <infile> <outfile>')
        sys.exit(2)

    try:
        infile = extractInfile(infile, outfile[:-4])
    except IOError as e:
        print(e.message)

    try:
        if os.path.isdir(infile):
            spectraDirs = os.listdir(infile)
        else:
            sys.exit(1)
    except IOError as e:
        print(e.message)

    importedFiles = []
    for spec in spectraDirs:
        if os.path.isdir(os.path.join(infile,spec)):
            importedFiles.append(readBruker(os.path.join(infile,spec)))


    writeCsv(importedFiles, outfile)

    # remove the unzipped folder
    try:
        shutil.rmtree(infile)
    except FileNotFoundError as e:
        print(e.message)
        
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
            print('could not unzip')

    elif(infile.endswith('.tar.gz') or infile.endswith('tar')):
        try:
            with tarfile.open(infile,'r') as tf:
                tf.extractall(path=outFolder)
            return outFolder
        except IOError as e:
            print (e.message)
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
    dataDF = fileList2DataFrame(fileList)
    dataDF.to_csv(outFile, sep='\t', index=True, header=True)

def fileList2DataFrame(fileList):
    
    fileList_ = align_scales(fileList)
    
    data = {x[0]:x[2] for x in fileList_}
    ppm = fileList_[0][1]
    dataDF = pd.DataFrame.from_dict(data, orient='columns')
    dataDF.index = ppm
    return(dataDF)

def align_scales(list_of_spectra):
    """
    Aligning all spectra scales in ppm taking into account offsets after processing in Topspin
    """
    
    ex_ppm = list_of_spectra[0][1]
    ppm_per_pt = (max(ex_ppm) - min(ex_ppm))/ex_ppm.shape[0]
    
    min_ppm, max_ppm = 666, -666
    
    for spec in list_of_spectra:
        if spec[1][0] > max_ppm:
            max_ppm = spec[1][0]
        if spec[1][-1] < min_ppm:
            min_ppm = spec[1][-1]
            
    n_pts = int(np.round((max_ppm - min_ppm)/ppm_per_pt))
    new_ppm = np.linspace(max_ppm, min_ppm, n_pts)
    
    print(new_ppm[0], new_ppm[-1])
    
    output = []
    for i in range(len(list_of_spectra)):
        output.append((list_of_spectra[i][0],
                       new_ppm,
                       extend_spec(list_of_spectra[i][2], list_of_spectra[i][1], new_ppm)))
    
    return output
    
def extend_spec(data, ppm_old, ppm_new):
    """
    Pad spectra with zeroes to fill a new ppm range
    """

    idx_max = np.argmin(abs(ppm_new - ppm_old[0]))
    
    pad_start = idx_max
    pad_end = (ppm_new.shape[0] - ppm_old.shape[0] - pad_start)
    
    if pad_end<0:
        pad_start = pad_start-1
        pad_end = 0

    return(np.concatenate((np.zeros(pad_start), data, np.zeros(pad_end)), axis=0))

if __name__ == "__main__":
    main(sys.argv[1:])
