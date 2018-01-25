FROM ubuntu:latest as OS

MAINTAINER Michal Stolarczyk (mjs5kd@virginia.edu)



# Environment variables
#ENV DEBIAN_FRONTEND=noninteractive
#ENV DISPLAY=":1"
ENV PATH="/usr/local/bin/:/usr/local/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/bin:/sbin"
ENV PKG_CONFIG_PATH="/usr/lib64/pkgconfig:/usr/lib/pkgconfig:/usr/local/lib64/pkgconfig:/usr/local/lib/pkgconfig"
ENV LD_LIBRARY_PATH="/usr/lib64:/usr/lib:/usr/local/lib64:/usr/local/lib"

#ENV PACK_R="abind BH cba curl dendextend devtools doSNOW eigenfaces extrafont FactoMineR geometry ggplot2 gplots Hmisc httr klaR kohonen magic #Matrix matrixStats mda memoise MetStaT multcomp plotly plotrix R6 rCharts Rcpp rmarkdown rsm rstudioapi RUnit squash tools vegan xslx"
#ENV PACK_BIOC="mtbls2 Risa"
#ENV PACK_GITHUB="dragua/xlsx glibiseller/IPO jcapelladesto/geoRge rstudio/rmarkdown sneumann/MetShot"



# Add cran R backport
RUN apt-get -y update
RUN apt-get -y install apt-transport-https
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
RUN echo "deb https://mirrors.ebi.ac.uk/CRAN/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list

# Update & upgrade sources
RUN apt-get -y update
RUN apt-get -y dist-upgrade

# Install RStudio-related packages
RUN apt-get -y install wget r-base gdebi-core psmisc libapparmor1

# Install development files needed for general compilation
RUN apt-get -y install cmake ed freeglut3-dev g++ gcc git libcurl4-gnutls-dev libgfortran-4.8-dev libglu1-mesa-dev libgomp1 libssl-dev libxml2-dev python unzip xorg-dev

# Install libraries needed by Bioconductor
RUN apt-get -y install gdb libbz2-dev libdigest-sha-perl libexpat1-dev libgl1-mesa-dev libglu1-mesa-dev libgmp3-dev libgsl0-dev libgsl0-dbg libgsl2 liblzma-dev libnetcdf-dev libopenbabel-dev libpcre3-dev libpng12-dev libxml2-dev netcdf-bin openjdk-9-jdk-headless libglpk-dev libglpk-java python-dev python-pip



#RUN apt install -y libsundials-serial

# Install Xorg environment (needed for compiling some Bioc packages)
RUN apt-get -y install xauth xinit xterm xvfb

# Install libsbml (needed by Bioconductor rsbml)
RUN wget -O /tmp/libsbml.deb 'https://sourceforge.net/projects/sbml/files/libsbml/5.10.2/stable/Linux/64-bit/libSBML-5.10.2-Linux-x64.deb/download'
RUN dpkg -i /tmp/libsbml.deb
RUN rm /tmp/libsbml.deb
RUN pip install python-libsbml cobra

RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('igraph')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('shiny')"
RUN Rscript -e "install.packages('shinythemes')"
RUN Rscript -e "install.packages('shinyBS')"
RUN Rscript -e "install.packages('intergraph')"
RUN Rscript -e "install.packages('visNetwork')"
RUN Rscript -e "install.packages('xtable')"
RUN Rscript -e "install.packages('sna')"

# Install cmake to install SUNDIALS 
RUN apt-get install -y software-properties-common python-software-properties
RUN add-apt-repository ppa:george-edison55/cmake-3.x
RUN apt-get update
RUN apt-get install -y cmake

RUN wget -O /tmp/sundials.tar.gz 'https://computation.llnl.gov/projects/sundials/download/sundials-2.6.2.tar.gz'
#RUN tar -xzf /tmp/sundials.tar.gz
#RUN
#RUN cd sundials
#RUN cmake -DCMAKE_INSTALL_PREFIX=/home/myname/sundials/instdir -DEXAMPLES_INSTALL_PATH=/home/myname/sundials/instdir/examples ../srcdir
#RUN make
#RUN make install

#RUN wget -O /tmp/rsbml.tar.gz 'https://www.bioconductor.org/packages/release/bioc/src/contrib/rsbml_2.36.0.tar.gz'
#RUN wget -O /tmp/BiocGenerics.tar.gz 'https://bioconductor.org/packages/release/bioc/src/contrib/BiocGenerics_0.24.0.tar.gz'
#RUN wget -O /tmp/graph.tar.gz 'https://bioconductor.org/packages/release/bioc/src/contrib/graph_1.56.0.tar.gz'
#RUN R CMD INSTALL /tmp/BiocGenerics.tar.gz
#RUN R CMD INSTALL /tmp/graph.tar.gz
#RUN R CMD INSTALL /tmp/rsbml.tar.gz
#RUN Rscript -e "source('http://bioconductor.org/biocLite.R');biocLite("rsbml")"

