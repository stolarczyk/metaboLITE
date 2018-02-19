# **Toycon visualization app documentation**

Michal Stolarczyk <mjs5kd@virginia.edu>

# Table of contents
1. [Introduction](#introduction)
    1. [What is this?](#what)
    2. [Functionalities](#Functionalities)
2. [Installation](#Installation)
    1. [Docker container](#Docker)
    2. [UNIX](#UNIX)
        1. [libSBML](#libSBML)
        2. [Python](#Python)
        3. [R](#R)
    3. [Windows](#Windows)
        1. [libSBML](#libSBML_win)
        2. [Python](#Python_win)
        3. [R](#R_win)
3. [Usage](#Usage)

# Introduction<a name="introduction"></a>

## What is this?<a name="what"></a>

The *ToyCon visualization app* is an R shiny app that aids the basic understanding of concepts conveyed in the following publication: *iNRG: A toy network capturing central energy metabolism for use with constraint-based methods.*

## Functionalities<a name="Functionalities"></a>

- **Network layout visualization**, which is intended to graphically present the interconnections between metabolites and reactions in the [genome-scale metabolic reconstruction](https://en.wikipedia.org/wiki/Metabolic_network_modelling#Genome-Scale_Metabolic_Reconstruction). Nodes of the graph represent metabolites and reactions whereas egdes - connections between them. 
- **Network stoichiometry visualization**, which is intended to depict the stoichiometric coefficients in view of the aforesaid network representation of the genome-scale metabolic reconstruction. The width of the edges correspond to the stoichiometic coefficients that determine ratios of each metabolite in the reactions.
- **Reaction knockouts (KOs) impact visualization**, which is intended to present the influence of the specific reaction KOs on genome-scale metabolic reconstruction architecture and fluxes in the model. By means of the [Flux Balance Analysis](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3108565/pdf/nihms299330.pdf) (FBA) this functionality can be used to detect either essential (when removed render the model carry 0 flux through the [objective function](https://en.wikipedia.org/wiki/Flux_balance_analysis#Objective_function)) or nonessential (when removed do not influence the effective flux value through the objective function) reactions. Additionaly, knockouts can simulate the complete enzyme inhibition that catalyzes the reaction being knocked out.
- **Media changes impact visualization**, which is intended to deptict the incluence of growth media changes on the model growth and fluxes through reactions. The media changes are performed by constraining the exchange reactions in the model during the FBA simulation. For example in order to check the influence of oxygen shortage on the model growth one needs to lower the upper (and lower) flux [bound(s)](https://en.wikipedia.org/wiki/Flux_balance_analysis#Mathematical_description) of the oxygen exchange reaction.
- **Gene expression influenece visualization**, which is intended to depict the impact of the pseudo-gene expression changes in the model, which directly influences the flux that is carried by the reaction catalyzed by the enzyme encoded by the gene in question. It is indended to give the user an idea of how the algorithms for gene expression integration influence the fluxes in in the model.

# Installation<a name="Installation"></a>

## Docker container<a name="Docker"></a>
The most reliable, safest and **easiest installation approach** is to use the [Docker](https://www.docker.com/what-docker) platform due to its intrisic characterisitics. In order to do it user needs to build the image from Dockerfile or run the pre-built Docker image (both provided with this application in `docker/Dockerfile` and in the official Docker [repository](https://hub.docker.com/r/mstolarczyk/toyconapp/), respectively).

To follow either of these approaches install the Docker CE (community edition) software on your machine. The installation instructions can be found on the [Docker website](https://docs.docker.com/install/). 

After installation the docker image can pulled from the repository with a following [command](https://docs.docker.com/engine/reference/commandline/pull):

```docker pull [OPTIONS]```

e.g

``` sudo docker pull mstolarczyk/toyconapp ```

Check the image status:

``` sudo docker images```

or the docker image can be build with a following [command](https://docs.docker.com/engine/reference/commandline/build/):

```docker build [OPTIONS] PATH | URL | - ``` *Please note that image building may take 10-15 minutes*

e.g

```cd "path/to/the/Dockerfile/directory" ```

```sudo docker build -t toyconapp . ``` *Mind the dot at the end of the line!*

Check the image status:

``` sudo docker images```

Next, the container can be run using the built or loaded image with a following [command](https://docs.docker.com/engine/reference/commandline/run/):

```docker run [OPTIONS] IMAGE [COMMAND] [ARG...] ```

e.g

```sudo docker run -p 8080:8080 mstolarczyk/toyconapp ```

The set of two numbers separated by the colon after the `-p` flag specifies the port mapping. The application is programmed to listen on port 8080 (second number of the two) inside of the container. The port is then [exposed](https://docs.docker.com/engine/reference/builder/#expose) outside of the container and it is redirected to the same port in your OS/network with the command above.

Subsequently, to run the application go to your favourite web browser and paste:

``` localhost:8080 ```


## UNIX<a name="UNIX"></a>

The application is written in R programming language and uses [COBRApy](https://opencobra.github.io/cobrapy/) for under the hood [Flux Balance Analysis](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3108565/pdf/nihms299330.pdf) (FBA) simulations, reaction knockouts and constraining fluxes through exchange reactions.
Consequently an installation of [libSBML](http://sbml.org/Software/libSBML/), [R](https://www.r-project.org/) and [Python](https://www.python.org/) with functioning [cobra](https://pypi.python.org/pypi/cobra/) package is required.

### libSBML<a name="libSBML"></a>

Install system [libSBML](http://sbml.org/Software/libSBML/Downloading_libSBML) programming library. Necessary files and instructions for installation under UNIX are available [here](https://sourceforge.net/projects/sbml/files/libsbml/5.15.0/stable/)

Install [libSBML Python language interface](http://sbml.org/Software/libSBML/Downloading_libSBML#Python)

### Python<a name="Python"></a>

Install [Python](https://www.python.org/downloads/) programming language (version of your choice, tested on 2.7.12) with following packages:

- sys: [System-specific parameters and functions](https://docs.python.org/2/library/sys.html)

- cobra: [constraint-based reconstruction and analysis in python](https://pypi.python.org/pypi/cobra/0.10.0a1)

To install the packages using [pip](https://pypi.python.org/pypi/pip) package management system type in the terminal:

```sudo pip install <package name>```

e.g.:

```sudo pip install sys```

### R<a name="R"></a>

Install [R](https://www.r-project.org/) programming language (version of your choice, tested on 3.4.3) with following packages:

- rPython: [Package Allowing R to Call Python](https://CRAN.R-project.org/package=rPython)
    
   Prior to the rPython package installation please read these [instructions](https://cran.rproject.org/web/packages/rPython/INSTALL) to choose the appropriate Python version (with cobra package installed) in systems where several Python versions coexist.

   **Optionally:** Please note that in order to use the Python [virtual environemnt](https://virtualenv.pypa.io/en/stable/userguide/) **within Linux OS** you need to install the rPython package from command line after activation of the virtual environment. As follows:

   Activate the Python virtual environment

   ```source <path to Python virtual environemnt>/bin/activate```

   Install the rPython R package

   ```R CMD INSTALL <path to rPython package source>```

   Subsequently launch the application from command line after activating the Python virtual environment every time. As shown [here](#launch_commandline). 

- igraph: [Network Analysis and Visualization](https://CRAN.R-project.org/package=igraph)

- rsbml: [R support for SBML, using libsbml](https://www.bioconductor.org/packages/release/bioc/html/rsbml.html)

- shiny: [Web Application Framework for R](https://CRAN.R-project.org/package=shiny)

- shinythemes: [Themes for Shiny](https://CRAN.R-project.org/package=shinythemes)

- shinyBS: [Twitter Bootstrap Components for Shiny](https://CRAN.R-project.org/package=shinyBS)

- intergraph: [Coercion Routines for Network Data Objects](https://CRAN.R-project.org/package=intergraph)

- ggplot2: [Create Elegant Data Visualisations Using the Grammar of Graphics](https://CRAN.R-project.org/package=ggplot2)

- visNetwork: [Network Visualization using 'vis.js' Library](https://CRAN.R-project.org/package=visNetwork)

- xtable: [Export Tables to LaTeX or HTML](https://CRAN.R-project.org/package=xtable)

- dplyr: [A Grammar of Data Manipulation](https://CRAN.R-project.org/package=dplyr)

- sna: [Tools for Social Network Analysis](https://cran.r-project.org/web/packages/sna/)

Or you can use [this](scripts/install_packages.R) script to automatically install all required R packages (except from the rPython package, which requires special attention) after the language installation.

## Windows<a name="Windows"></a>

The application is written in R programming language and uses [COBRApy](https://opencobra.github.io/cobrapy/) for under the hood [Flux Balance Analysis](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3108565/pdf/nihms299330.pdf) (FBA) simulations, reaction knockouts and constraining fluxes through exchange reactions.
Consequently an installation of [libSBML](http://sbml.org/Software/libSBML/), [R](https://www.r-project.org/) and [Python](https://www.python.org/) with functioning [cobra](https://pypi.python.org/pypi/cobra/) package is required.

### libSBML<a name="libSBML_win"></a>

Install system [libSBML](http://sbml.org/Software/libSBML/Downloading_libSBML) programming library. Necessary files and instructions for installation under Windows are available [here](https://sourceforge.net/projects/sbml/files/libsbml/5.15.0/stable/Windows/64-bit/)

Install [libSBML Python language interface](http://sbml.org/Software/libSBML/Downloading_libSBML#Python)

### Python<a name="Python_win"></a>

Install [Python](https://www.python.org/downloads/) programming language (version of your choice, tested on 2.7.12) with following packages:

- sys: [System-specific parameters and functions](https://docs.python.org/2/library/sys.html)

- cobra: [constraint-based reconstruction and analysis in python](https://pypi.python.org/pypi/cobra/0.10.0a1)

To install the packages using [pip](https://pypi.python.org/pypi/pip) package management system type in the Command Prompt:

``` <absolute path to pip.exe> install <package_name>```

e.g.:

```C:/Python27/Scripts/pip.exe install sys``` (you may need to adjust the path accordingly)

### R<a name="R_win"></a>

Install [R](https://www.r-project.org/) programming language (version of your choice, tested on 3.4.3) with following packages:

- rPython-win: [Package Allowing R to Call Python](https://CRAN.R-project.org/package=rPython)

   The necessary instructions on how to install Windows version of this package are available on the package's [GitHub page](https://github.com/cjgb/rPython-win)

- igraph: [Network Analysis and Visualization](https://CRAN.R-project.org/package=igraph)

- rsbml: [R support for SBML, using libsbml](https://www.bioconductor.org/packages/release/bioc/html/rsbml.html)

- shiny: [Web Application Framework for R](https://CRAN.R-project.org/package=shiny)

- shinythemes: [Themes for Shiny](https://CRAN.R-project.org/package=shinythemes)

- shinyBS: [Twitter Bootstrap Components for Shiny](https://CRAN.R-project.org/package=shinyBS)

- intergraph: [Coercion Routines for Network Data Objects](https://CRAN.R-project.org/package=intergraph)

- ggplot2: [Create Elegant Data Visualisations Using the Grammar of Graphics](https://CRAN.R-project.org/package=ggplot2)

- visNetwork: [Network Visualization using 'vis.js' Library](https://CRAN.R-project.org/package=visNetwork)

- xtable: [Export Tables to LaTeX or HTML](https://CRAN.R-project.org/package=xtable)

- dplyr: [A Grammar of Data Manipulation](https://CRAN.R-project.org/package=dplyr)

- sna: [Tools for Social Network Analysis](https://cran.r-project.org/web/packages/sna/)

Or you can use [this](scripts/install_packages.R) script to automatically install all required R packages (except from the rPython package, which requires special attention) after the language installation.




# Usage<a name="Usage"></a>

Besides the installation of packages for both R and Python no further app installation is needed. Simply download the contents of this [repository](https://gitlab.com/mstolarczyk/shinyapp.git) and save them in the directory of your choice (represented as `"path/to/the/shinyapp/directory"` below).

To launch the app just run the following line of code in your favourite R IDE, e.g. [RStudio](https://www.rstudio.com/):

```shiny::runApp(appDir = "path/to/the/shinyapp/directory")```

or in the command line:<a name="launch_commandline"></a>

```R -e "shiny::runApp(appDir = "path/to/the/shinyapp/directory", launch.browser=TRUE)"```
