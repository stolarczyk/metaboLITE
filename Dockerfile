FROM ubuntu:xenial as OS

MAINTAINER Michal Stolarczyk (mjs5kd@virginia.edu)

# Add cran R backport
RUN apt-get -y update
RUN apt-get -y install apt-transport-https
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
RUN echo "deb https://mirrors.ebi.ac.uk/CRAN/bin/linux/ubuntu xenial/" >> /etc/apt/sources.list

# Update & upgrade sources
RUN apt-get -y update
RUN apt-get -y dist-upgrade

# Install RStudio-related packages
RUN apt-get -y install wget r-base gdebi-core psmisc libapparmor1

# Install development files needed for general compilation
RUN apt-get -y install cmake ed freeglut3-dev g++ gcc git libcurl4-gnutls-dev libgfortran-4.8-dev libglu1-mesa-dev libgomp1 libssl-dev libxml2-dev python unzip xorg-dev

# Install libraries needed by Bioconductor
RUN apt-get -y install gdb libbz2-dev libdigest-sha-perl libexpat1-dev libgl1-mesa-dev libglu1-mesa-dev libgmp3-dev libgsl0-dev libgsl0-dbg libgsl2 liblzma-dev libnetcdf-dev libopenbabel-dev libpcre3-dev libpng12-dev libxml2-dev netcdf-bin openjdk-9-jdk-headless libglpk-dev libglpk-java python-dev python-pip

# Install Xorg environment (needed for compiling some Bioc packages)
RUN apt-get -y install xauth xinit xterm xvfb
RUN apt-get -y install swig

# Install libsbml
RUN wget -O /tmp/libsbml.tar.gz 'https://sourceforge.net/projects/sbml/files/libsbml/5.10.2/stable/libSBML-5.10.2-core-plus-packages-src.tar.gz/download'
WORKDIR /tmp
RUN tar xfvz libsbml.tar.gz
WORKDIR /tmp/libSBML-5.10.2-Source
RUN ./configure --with-swig
RUN make
RUN make install
ENV LD_LIBRARY_PATH=/usr/local/lib

#Install libsbml R interface
RUN wget -O /tmp/libsBML_5.10.2_R.tar.gz https://sourceforge.net/projects/sbml/files/libsbml/5.10.2/stable/Linux/64-bit/R%20interface/libSBML_5.10.2_R_x86_64-pc-linux-gnu.tar.gz/download
WORKDIR /tmp
RUN R CMD INSTALL libsBML_5.10.2_R.tar.gz

#Install SUNDIALS library, needed for the rsbml packge configuration
RUN wget -O /tmp/sundials-2.6.2.tar.gz 'https://computation.llnl.gov/projects/sundials/download/sundials-2.6.2.tar.gz'
RUN tar xvzf /tmp/sundials-2.6.2.tar.gz
WORKDIR /tmp/sundials-2.6.2
RUN mkdir builddir
WORKDIR builddir
RUN cmake ..
RUN make
RUN make install

#Install rsbml package and its dependencies
RUN wget -O /tmp/rsbml.tar.gz 'https://www.bioconductor.org/packages/release/bioc/src/contrib/rsbml_2.38.0.tar.gz'
RUN wget -O /tmp/BiocGenerics.tar.gz 'https://bioconductor.org/packages/release/bioc/src/contrib/BiocGenerics_0.26.0.tar.gz'
RUN wget -O /tmp/graph.tar.gz 'https://bioconductor.org/packages/release/bioc/src/contrib/graph_1.58.0.tar.gz'
WORKDIR /tmp
RUN R CMD INSTALL BiocGenerics.tar.gz
RUN R CMD INSTALL graph.tar.gz
RUN R CMD INSTALL rsbml.tar.gz

#Install other R packages
RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('markdown')"
RUN Rscript -e "install.packages('igraph')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('shiny')"
RUN Rscript -e "install.packages('shinythemes')"
RUN Rscript -e "install.packages('shinyBS')"
RUN Rscript -e "install.packages('intergraph')"
RUN Rscript -e "install.packages('visNetwork')"
RUN Rscript -e "install.packages('xtable')"
RUN Rscript -e "install.packages('sna')"
RUN Rscript -e "install.packages('rPython')"
RUN Rscript -e "install.packages('DT')"
RUN Rscript -e "install.packages('sybil')"

# Installing python-libsbml and cobra
RUN pip install python-libsbml cobra

#Clone the R shinyapp git repository
RUN git clone https://github.com/michalstolarczyk/shinyapp.git

#Expose the port that the app will be listening on
EXPOSE 8080

#Run the app
CMD ["R","-e","shiny::runApp(appDir = '/tmp/shinyapp', port = 8080,launch.browser = F ,host = '0.0.0.0', quiet = FALSE, display.mode = 'auto',test.mode = FALSE)"]





