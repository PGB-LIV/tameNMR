# Tools for Analysis of MEtabolomic NMR (tameNMR)

tameNMR is a suite of tools for processing and analysis of NMR data from metabolomics
experiments. It is designed as a set of command line programs to be used
as standalone tools or for design of automated workflows in Galaxy
(galaxyproject.org) framework.


## Structure
The package includes the following tools:

1. Import - Import of NMR data (from Bruker raw files)
    * Bruker to csv
    * Bruker to nmrML
2. ProcessSpectra
    * Normalisation
    * Peak Picking
    * Spectra alignment
    * Binning
3. ProcessData
    * Scaling
    * Make factor template (for grouping observations)
4. Stats - univariate and multivariate statistics
    * t-tests
    * one-way ANOVA
    * Principal component analysis (PCA)
    * Partial least squares discriminant analysis (PLS-DA)
5. Plots - various plotting tools for:
    * Raw NMR spectra
    * Quantiles of spectra
    * Significant bins (p-values from t-tests of ANOVA)

## Implementation details

Most of the tools are implemented in R and Python.
Following packages are required in order to run all the tools:

1. R
    * ggplot2
    * MassSpecWavelet
    * speaq
2. Python
    * numpy
    * nmrglue
    * pandas

## How to install a local instance of Galaxy with tameNMR (tested on Ubuntu 18.04)

NOTE: this is only meant for individual users running a local galaxy server on a workstation and is not guaranteed to work with multiple users.

### requires installation of galaxy server.

Make a folder for your galaxy instance

```bash
mkdir ~/galaxy
cd galaxy
```

Download and install Galaxy from Github:
(The current version runs on Galaxy version 17.05)

```bash
git clone -b release_17.05 https://github.com/galaxyproject/galaxy.git 
```
For more instructions on Galaxy please refer to: https://galaxyproject.org/admin/get-galaxy/

Make a folder in galaxy path called tameNMR (in this example the path is ~/galaxy/tameNMR)
Clone tameNMR repository and copy the required files to the galaxy instance:
```bash
cd ~/galaxy/tameNMR
git clone https://github.com/PGB-LIV/tameNMR

cp -r ~/galaxy/tameNMR/tameNMR ~/galaxy/galaxy/tools
```

run config file to configure without all galaxy links (to reduce impact and also copy welcome.html & tools folder):
```bash
cp ~/galaxy/tameNMR/config/tool-conf.xml ~/galaxy/galaxy/config/
cp ~/galaxy/tameNMR/static/* ~/galaxy/galaxy/static/
```

install R and a number of packages required for tameNMR
```bash
apt-get install r-base-core
R -e "install.packages(c('ellipse', 'markdown', 'knitr', 'viridis','ggrepel','pls', 'ggplot2'), repos='https://cran.ma.imperial.ac.uk/')"
R -e "source("https://bioconductor.org/biocLite.R");biocLite("MassSpecWavelet")"
R -e "install.packages('speaq', repos='https://cran.ma.imperial.ac.uk/')"

```

run galaxy (it may take ~60 seconds on first boot):
```bash
cd ~/galaxy/galaxy
sh run.sh
```


On the first run it will create a local virtual environment and run configuration.
You will now that it is running once the message in the terminal says: "serving at localhost:8080"
Kill the Galaxy instance by pressing Ctrl+C and continue the setup.

Install Python libraries into the local virtual environment in galaxy:

```bash
source ~/galaxy/galaxy/.venv/bin/activate
pip install pandas==0.19
pip install nmrglue
source deactivate
```

**In order for the outputs to appear correctly on the web interface you will need to allow formatting of the html files.**
Open file ~/galaxy/galaxy/config/galaxy.ini in a plain text editor.
(If the file does not exist there will be a file galaxy.ini.sample that you should make a copy of and rename to galaxy.ini)
In the galaxy.ini file find the line: #sanitize_all_html = True
and replace it with: sanitize_all_html = False
(remove the hashtag and change True to False)


Prepend a libPath (edit scripts in galaxy to point to correct r location):
```bash
sh ~/galaxy/prependRScripts.sh ~/galaxy/galaxy/tools/tameNMR
```
Run the Galaxy server again
```bash
cd ~/galaxy/galaxy
sh run.sh
```

Galaxy can now be accessed via web-browser using address:
**https://localhost:8080**

## Updating your local instance

```bash
cd ~/galaxy/tameNMR
git pull

cp -r ~/galaxy/tameNMR/tameNMR ~/galaxy/galaxy/tools/
sh ~/galaxy/prependRScripts.sh ~/galaxy/galaxy/tools/tameNMR

cp ~/galaxy/tameNMR/config/tool-conf.xml ~/galaxy/galaxy/config/
cp ~/galaxy/tameNMR/static/* ~/galaxy/galaxy/static/
```

In the development version the tools use csv files for input and output.
However we aim to use [nmrML] (http://nmrml.org) data format for all data
processing steps. This is currently being added into the tools.

The nmrML format (<http://nmrml.org>) has been developed as a uniform NMR
data storage and exchange standard and is being rapidly adopted in the field.

**This is a development version of tameNMR that changes quite often.**
