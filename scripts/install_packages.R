#This script is intended to help the user to automatically install the packages that are required by the ToyCon shiny app

# Function declaration ----------------------------------------------------

#The function installs the package only if it's missing. Either from CRAN or Bioconductor, depending of the availability.

install.packages.auto <- function(x) {
  for(package in x){
    package <- as.character(substitute(package)) 
    if(isTRUE(x %in% .packages(all.available=TRUE))) { 
      eval(parse(text = sprintf("require(\"%s\")", package)))
    } else { 
      eval(parse(text = sprintf("install.packages(\"%s\", dependencies = TRUE)", package)))
    }
    if(isTRUE(x %in% .packages(all.available=TRUE))) { 
      eval(parse(text = sprintf("require(\"%s\")", package)))
    } else {
      source("http://bioconductor.org/biocLite.R")
      eval(parse(text = sprintf("biocLite(\"%s\")", package)))
      eval(parse(text = sprintf("require(\"%s\")", package)))
    }
  }
}


# Packages installation ---------------------------------------------------

#The lines below install all the required packgages for the full functionality of the ToyCon shinyapp. 

required_packages = c("igraph","rsbml","shiny","intergraph","ggplot2","visNetwork","xtable","dplyr","sna","shinyBS","rPython","shinythemes")
install.packages.auto(x = required_packages)

