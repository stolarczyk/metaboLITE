# **Toycon visualization app documentation**

Michal Stolarczyk <mjs5kd@virginia.edu>

# Table of contents
1. [Introduction](#introduction)
    1. [What is this?](#what)
    2. [Functionalities](#Functionalities)
2. [Installation](#Installation)
    1. [UNIX](#UNIX)
        1. [libSBML](#libSBML)
        2. [Python](#Python)
        2. [R](#R)
    2. [Windows](#Windows)
        1. [libSBML](#libSBML_win)
        2. [Python](#Python_win)
        2. [R](#R_win)

# Introduction<a name="introduction"></a>

## What is this?<a name="what"></a>

The *ToyCon visualization app* is an R shiny app that aids the basic understanding of concepts conveyed in the ToyCon publication: *iNRG: A toy network capturing central energy metabolism for use with constraint-based methods.*

## Functionalities<a name="Functionalities"></a>

- Network layout visualization
- Network stoichiometry visualization
- Reaction knockouts (KOs) impact visualization
- Media changes impact visualization

# Installation<a name="Installation"></a>

The application is written in R programming language and uses [COBRApy](https://opencobra.github.io/cobrapy/) for under the hood [Flux Balance Analysis](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3108565/pdf/nihms299330.pdf) (FBA) simulations, reaction knockouts and constraining fluxes through exchange reactions.
Consequently an installation of [libsBML](http://sbml.org/Software/libSBML/), [R](https://www.r-project.org/) and [Python](https://www.python.org/) with functioning [cobra](https://pypi.python.org/pypi/cobra/) package is required.

## UNIX<a name="UNIX"></a>

### libSBML<a name="libSBML"></a>

Install system [libSBML](http://sbml.org/Software/libSBML/Downloading_libSBML) programming library. Necessary files and instructions for installation under UNIX are available [here](https://sourceforge.net/projects/sbml/files/libsbml/5.15.0/stable/)

Install [libSBML R language interface](http://sbml.org/Software/libSBML/Downloading_libSBML#R)

Install [libSBML Python language interface](http://sbml.org/Software/libSBML/Downloading_libSBML#Python)

### Python<a name="Python"></a>

Install [Python](https://www.python.org/downloads/) programming language (version of your choice, tested on 2.7.12) with following packages:

- sys: [System-specific parameters and functions](https://docs.python.org/2/library/sys.html)

- cobra: [constraint-based reconstruction and analysis in python](https://pypi.python.org/pypi/cobra/0.10.0a1)

### R<a name="R"></a>

Install R programming language (version of your choice, tested on 3.4.3) with following packages:

- rPython: [Package Allowing R to Call Python](https://CRAN.R-project.org/package=rPython)

Prior to the rPython package installation please read these [instructions](https://cran.r-project.org/web/packages/rPython/INSTALL) to choose the appropriate Python version (with cobra package installed) in systems where several Python versions coexist.

- igraph: [Network Analysis and Visualization](https://CRAN.R-project.org/package=igraph)

- libSBML: [application programming interface for the SBML format](http://sbml.org/Software/libSBML/Downloading_libSBML#R)

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

## Windows<a name="Windows"></a>

### libSBML<a name="libSBML_win"></a>

Install system [libSBML](http://sbml.org/Software/libSBML/Downloading_libSBML) programming library. Necessary files and instructions for installation under Windows are available [here](https://sourceforge.net/projects/sbml/files/libsbml/5.15.0/stable/Windows/64-bit/)

Install [libSBML R language interface](http://sbml.org/Software/libSBML/Downloading_libSBML#R)

Install [libSBML Python language interface](http://sbml.org/Software/libSBML/Downloading_libSBML#Python)

### Python<a name="Python_win"></a>

Install [Python](https://www.python.org/downloads/) programming language (version of your choice, tested on 2.7.12) with following packages:

- sys: [System-specific parameters and functions](https://docs.python.org/2/library/sys.html)

- cobra: [constraint-based reconstruction and analysis in python](https://pypi.python.org/pypi/cobra/0.10.0a1)

### R<a name="R_win"></a>

Install R programming language (version of your choice, tested on 3.4.3) with following packages:

- rPython-win: [Package Allowing R to Call Python](https://CRAN.R-project.org/package=rPython)

The necessary instructions on how to install Windows version of this package are available on the package's [GitHub page](https://github.com/cjgb/rPython-win)

- igraph: [Network Analysis and Visualization](https://CRAN.R-project.org/package=igraph)

- libSBML: [application programming interface for the SBML format](http://sbml.org/Software/libSBML/Downloading_libSBML#R)

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

