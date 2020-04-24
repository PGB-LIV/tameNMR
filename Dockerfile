# Galaxy - tameNMR

FROM bgruening/galaxy-stable

MAINTAINER Arturas Grauslys, a.grauslys@liverpool.ac.uk

ENV GALAXY_CONFIG_BRAND Galaxy for NMR metabolomics

#USER $GALAXY_USER

WORKDIR /galaxy-central


# Install python packages
#RUN export PATH=$GALAXY_CONDA_PREFIX/condabin/:$GALAXY_CONDA_PREFIX/bin/:$PATH \
 RUN . /galaxy_venv/bin/activate \
 && pip install nmrglue pandas

#Install R and required packages
RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/" >> /etc/apt/sources.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
 && apt-get -y update \
 && apt-get -y install r-base r-base-dev libcurl4-gnutls-dev libcairo2-dev libxt-dev libxml2-dev libv8-dev libnlopt-dev libnlopt0 gdebi-core pandoc pandoc-citeproc software-properties-common make gcc gfortran g++ libgsl0-dev gsl-bin libssl-dev \ 
 && R -e "install.packages(c('BiocManager','ellipse', 'markdown', 'knitr', 'viridis','ggrepel','pls','gplots', 'devtools','markdown'), repos='https://cran.ma.imperial.ac.uk/')" \
 && R -e "BiocManager::install(c('MassSpecWavelet','impute'), update=FALSE)" \
 && R -e "install.packages('speaq', repos='https://cran.ma.imperial.ac.uk/')" \
 && cd $GALAXY_HOME \
 && git clone https://github.com/PGB-LIV/tameNMR \
 && cp -r tameNMR/tameNMR $GALAXY_ROOT/tools/ \
 && cp tameNMR/config/tool_conf.xml $GALAXY_ROOT/config/ \
 && cp tameNMR/static/* $GALAXY_CONFIG_DIR/web/ \
 && apt-get -y clean && rm -rf /var/lib/{cache,log}/ /usr/src/rnmr1d /var/tmp/*

 # Mark folders as imported from the host.
VOLUME ["/export/", "/data/", "/var/lib/docker"]

# Expose port 80 (webserver), 21 (FTP server), 8800 (Proxy)
EXPOSE :80
EXPOSE :21
EXPOSE :8800

# Autostart script that is invoked during container start
CMD ["/usr/bin/startup"]
