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

## How to run a local instance of Galaxy with tameNMR using Docker (easy)
### (tested on Windows 10, Mac OS and Ubuntu 18.04)

This is the easiest way to get a local instance of Galaxy with tamrNMR tools preinstalled running on your machine. You only need to install a Docker client and run a line of code that will perform the whole setup for you. Instructions on how to install Docker on your machine can be found here: https://docs.docker.com/get-docker/ 

### 1. Install and Run Docker Desktop on Windows

**Note:**  Windows 10 Home is not supported by Docker Desktop and will only work with Docker Toolbox. This is an older version of Docker platform that can also be used for this. To find out your Windows version right-click the 'This PC' icon your desktop and choose properties. You will find your Windows version in the section "Windows Edition:"

#### Windows Professional/Enterprise/etc.
Choose Windows operating system on the website (https://docs.docker.com/get-docker/), click the "Download from Docker Hub" button at the top of the next page to begin downloading the installer file. The rest of the page describes installation in detail. Shortly - just run the downloaded file (you can find it in the latest downloads tab on your browser) and follow the instructions provided by the installer. Once the installation is complete you will have a Docker Desktop icon on your Desktop. Double click this icon to run a Docker client.

#### Windows Home
For Windows Home edition you will have to install Docker Toolbox. Go to the following website: https://github.com/docker/toolbox/releases.
Click the "DockerToolbox-version.exe" file, where "version" is the software version number (choose the one with the highest number). Download the file and run it (from the downloads location of the latest downloads tab on your browser). Follow the installation instructions. Once the installation is over you will have a new shortcut on your desktop. Double click the shortcut to run Docker Toolbox command line.

More detailed instructions can be found at: https://docs.docker.com/toolbox/toolbox_install_windows/ 

### 2. Install and Run Docker on Mac

Go to Docker downloads website https://docs.docker.com/get-docker/ and click on the Docker Desktop for Mac. Click the "Download from Docker Hub" button at the top of the next page to begin downloading the installer file (.img). Once the file is downloaded, double click it (from the downloads location or the latest downloads section on your browser). In the window that opens drag the "Docker.app" to the "Applications" folder to begin installation. Once the installation is finished run the Docker app from your applications folder.

The website contains detailed installation instructions if you have trouble getting it to work. 

### 3. Install and Run Docker on Linux

Docker installation on linux is somewhat more involved on linux. However there are detailed instructions how to do this on the following website: https://docs.docker.com/engine/install/ubuntu/.

### 4. Run Galaxy-tameNMR Docker container

Once you have the Docker client installed and running there is only one step left - running and instance og the Galaxy-tameNMR container. The first time you run the container it needs to be downloaded from the repository which might take some time. Any consecutive runs will be performed from the local copy and will be much faster.

It is worth running the container in Read-only mode at first as a test. This means that the container will be run fully in memory and won't be able to write any files on your computer. It will have full functionality but won't save any changes after it is turned off. We will show how to run the container with a designated folder on your machine for saving changes below.

#### Running Galaxy-tameNMR container in read-only mode

To run the container in read-only mode type the following command in your Docker command line (or any terminal emulator in linux):

```bash
docker run -d -p 8080:80 -p 8021:21 arturasg/galaxy-tamenmr
```
Here ```docker run``` tells Docker to run the container, ```-d``` specifies to run it in "daemon" mode meaning that it will run in the background, ```-p 8080:80``` and ```-p 8021:21``` specify that we would like to map the ports inside the container to be accessible from the outside so we can access the Galaxy instance running within the container and ```arturasg/galaxy-tamenmr``` refers to the id of the container. The first time the container won't be found locally and will be downloaded from the Docker repository instead.

If the command is successful you will see text in the console indicating the download and set-up process. Once the process is complete you will get back the control of the terminal and the container will be running in the background. To test that the container is running you can run the following command:

```bash
docker ps
``` 

You should get a list with one line and headers such as "CONTAINER ID", "IMAGE", "COMMAND" etc. Under each header will be an entry refering to your instance of the container. (If you only get the headers something must have gone wrong)

If your container is running you should be able to access the Galaxy in your browser at the following address:

```http://localhost:8080```

If the page doesn't load at first give it some time. It might take a minute for all the services to be activated (this depends on the speed of your computer).

**NOTE:** if you are running on **Windows 10 Home** edition and therefore in Docker Toolbox there is an additional step. Docker Toolbox runs a virtual machine in the background and uses its own ip address (instead of mapping to localhost). You will find it out by running the following command in your Docker command line:

```bash
docker-machine ip
```

The ip address (four numbers separated by dots, usually it is 192.168.99.100) is the address where you can access your galaxy instance. For example http://192.168.99.100:8080 or if yours is different replace the default address by the address you got from the ```docker-machine ip``` command.

#### Shutting the container down

If you want to run the container in different mode or to just turn it off from running in the background you can kill the process with the following command:

```bash
docker kill CONTAINER-ID
```
where CONTAINER-ID should be replaced by the id of your currently running container. To find it out use the ```docker ps``` command. The container id is the in the first column of the output (something like ```dfa7sb7fa3a2```). Once you run the ```docker kill container-id``` use the ```docker ps``` to confirm that the container is not running any more.

#### Running Galaxy-tameNMR container with mapped folder

In order to have a Galaxy instance that allows changes to be saved (user accounts, analysis results, histories, etc.) you need to map an external folder where Galaxy will be able to store its database and data files for the next run. To do this create a folder on your computer and find out the path to it. For example let's say we are working in Windows and the path to our new folder is "C:/Users/user1/Documents/Galaxy_data".

To run the Galaxy-tameNMR container and bind it to this folder we need to slightly modify the command from the "read-only" version:

```bash
docker run -d -p 8080:80 -p 8021:21 -v C:/Users/user1/Documents/Galaxy_data/:/export/ arturasg/galaxy-tamenmr
```

Here we added ```-v``` parameter with the value that consists of two parts separated by a colon. ```C:/Users/user1/Documents/Galaxy_data/``` refers to the path to your local folder while ```/export/``` refers to the folder inside the container where galaxy saves its data. The first time you run this the folder you created will be empty and Galaxy will populate it with its database, configuration files etc. From then on since the folder won't be empty Galaxy will know to use the data stored in that folder.


## How to install a local instance of Galaxy with tameNMR (advanced)
### (tested on Ubuntu 18.04)

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
cp ~/galaxy/tameNMR/extras/upload.py ~/galaxy/galaxy/tools/data_source/
```

install R and a number of packages required for tameNMR
```bash
apt-get install r-base-core
R -e "install.packages(c('ellipse', 'markdown', 'knitr', 'viridis','ggrepel','pls', 'ggplot2'), repos='https://cran.ma.imperial.ac.uk/')"
R -e "source("https://bioconductor.org/biocLite.R");biocLite("MassSpecWavelet")"
R -e "install.packages('speaq', repos='https://cran.ma.imperial.ac.uk/')"

```

You will likely be prompted to install the packages into a local folder. Note the path to that folder.
If this happens we need to tell R where the packages are so they can be used by Galaxy.
Check if the path you noted now contains a number of folders some of which are names of packages you have just installed.
If so open the file ~/galaxy/tameNMR/extras/prependLibPath.sh in a plain text editor and change the path in the 3rd line of the file:
Currently it says: *LIBPATH=".libPaths('/home/galaxy/R/x86_64-pc-linux-gnu-library/3.2/')"* 
Change the part in the brackets to the path you have installe your R packages to. It is likely to have a similar structure to the given one.
Now run this file - this will prepend an instruction to each R script in tameNMR pointing at the location of the additional packages. 

```bash
cd ~/galaxy/tameNMR/extras
sh prependLibPath ~/galaxy/galaxy/tools/tameNMR/
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


Prepend a libPath (edit scripts in galaxy to point to correct location of R libraries):
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
