# *T*ools for *A*nalysis of *ME*tabolomic NMR (tameNMR)

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
    * Significant bins (mean value comparison)

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

In the development version the tools use csv files for input and output.
However we aim to use [nmrML] (http://nmrml.org) data format for all data
processing steps. This is currently being added into the tools.

The nmrML format (<http://nmrml.org>) has been developed as a uniform NMR
data storage and exchange standard and is being rapidly adopted in the field.

**This is a development version of tameNMR that changes quite often.**

**Stable version will be available soon.**
