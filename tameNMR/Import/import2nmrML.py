""" A wrapper for the data import using nmrML import tools"""

import os
import sys
import zipfile
import tarfile
import json
import shutil

import subprocess
#from xml.dom import core
#from xml.dom.html_builder import HtmlBuilder

def main(args):
    """ Main function that runs the wrapper"""

    absPath = '/home/arturas/Projects/galaxy/'
    pathToTools = os.path.split(os.path.realpath(__file__))[0]

    try:
        #infile = os.path.join(absPath,args[0])
        infile = args[0]
        outfile = args[1]
        #outDir = args[2]
        #vendor = args[3]
        proc = True if (args[2] == 'processed') else False
    except:
        print 'usage: import2nmrML.py <infile> <outfile> <proc>'
        sys.exit(2)

    try:
        infile = extractInfile(infile)
    except Exception as e:
        print e.message

    try:
        #if not os.path.exists(outDir):
        #    os.makedirs(outDir)
        spectraDirs = os.listdir(infile)
        importedFiles = []
        for spec in spectraDirs:
            if os.path.isdir(os.path.join(infile,spec)):
                callJnmrML(os.path.join(infile, spec), proc)
                importedFiles.append(spec)
                print(spec)
        filePaths = [os.path.join(infile, x + '.nmrML', ) for x in importedFiles]
        zipFiles(filePaths, outfile)
        shutil.rmtree(infile)

        # change this to zipping
        #with open(outfile,'w') as f:
            #f.write(genHtml(outfile))
       #     f.write(genHtml(importedFiles))

    except Exception as e:
        print e.message
################################################################################
# functions #
################################################################################

def extractInfile(infile):
    """
    Extracts the archive at <infile> if it is a zip or tar.gz and returns a
    path to the folder containing contents of the archive, otherwise returns
    the path.
    """

    if infile.endswith('.zip'):
        infile_new = '/'.join(infile.split('/')[:-1]) + '/nmrMLfiles'
        if not os.path.exists(infile_new):
            os.makedirs(infile_new)
        try:
            with zipfile.ZipFile(infile,'r') as zf:
                zf.extractall(path=infile_new)
            return infile_new
        except IOError as e:
            print e.message
    elif(infile.endswith('.tar.gz') or infile.endswith('tar')):
        infile_new = '/'.join(infile.split('/')[:-1]) + '/nmrMLfiles'
        if not os.path.exists(infile_new):
            os.makedirs(infile_new)
        try:
            with tarfile.open(infile,'r') as tf:
                tf.extractall(path=infile_new)
            return infile_new
        except IOError as e:
            print e.message
    else:
        return infile
    return(infile_new)

def zipFiles(files, outfile):
    """ zips the files and writes the output to outfile """

    for i in files:
        print i
    ziph = zipfile.ZipFile(outfile + '.zip', 'w', zipfile.ZIP_DEFLATED)
    for fl in files:
        ziph.write(fl)
    ziph.close()
    return(outfile)

def callJnmrML(specDir, proc):
    """calls the nmrML tools for each spectrum in the experiment"""

    pathToTools = ''
    nmrMLcreate = os.path.join(pathToTools,'JnmrML/converter/bin/nmrMLcreate')
    nmrMLproc = os.path.join(pathToTools,'JnmrML/converter/bin/nmrMLproc')
    outDir = '/'.join(specDir.split('/')[:-1])
    vendor = 'bruker'

    if(specDir.endswith('/')):
        name = specDir.split('/')[-2]
    else:
        name = specDir.split('/')[-1]

    outpath = os.path.join(outDir, name + '.nmrML')

    if proc:
        callCreate = ' '.join([nmrMLcreate, '-b','-z','-t',vendor,
            '-i', specDir ,'-o', outpath])

        callProc = ' '.join([nmrMLproc, '-b','-z','-t',vendor,
            '-d', os.path.join(specDir,'pdata/1/') ,'-o', outpath])

        callProc2 = ' '.join([nmrMLproc, '-b','-z','-t',vendor,
            '-d', os.path.join(specDir,'pdata/1/') ,
            '-i',outpath,
            '-o', outpath])

        try:
            subprocess.call(callCreate, shell=True)
            subprocess.call(callProc2, shell=True)
        except Exception as e:
            print e.message
    else:
        callCreate = ' '.join([nmrMLcreate, '-b','-z','-t',vendor,
            '-i', specDir ,'-o', outpath])
        try:
            subprocess.call(callCreate, shell=True)
        except Exception as e:
            print e.message

# Not used at the moment
def genHtml(filesImp):
    """ Generates HTML output with the list of files imported """
    template = '<html> <head> {0} </head> <body> {1} </body></html>'

    fileList = 'Files imported: <ul>'
    for n, file_ in enumerate(filesImp):
        fileList = fileList + '<li> {} </li>'.format(file_)
    fileList = fileList + ' </ul>'
    return template.format('',fileList)

if __name__ == "__main__":
    main(sys.argv[1:])
