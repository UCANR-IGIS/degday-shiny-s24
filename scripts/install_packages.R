#########################################################################################
## GENERAL TIPS FOR INSTALLING PACKAGES
##
## Restarting R (Session > Restart R) is always a good idea before you install or update packages
##
## If you are prompted by the question, "Do you want to install from sources the package 
## which needs compilation?", choose "No".
##
## If you are working in RStudio Desktop (as opposed to Posit Cloud), running these commands 
## in R (rather than RStudio) can help resolve issues.
#########################################################################################

## Required packages from CRAN

cran_pkgs <- c("sf", "tidyverse", "conflicted", "here", "units", "zoo", "degday", "cimir", "httr2", "jsonlite", "usethis", 
              "shiny", "leaflet", "glue", "knitr", "DT", "plotly", "bslib", "openmeteo")

install.packages(cran_pkgs)

missing_pkgs <- cran_pkgs[!(cran_pkgs %in% installed.packages()[,"Package"])]

## Was it successful?

if (length(missing_pkgs)==0) {
  cat("ALL REQUIRED CRAN PACKAGES ARE INSTALLED :-) \n")
} else {
  cat("ONE OR MORE PACKAGES ARE NOT INSTALLED :-(\n", paste(missing_pkgs, collapse = ", "), "\n")
}

## Restart R (Session > Restart R), then install caladaptr from R-Universe

install.packages(c("caladaptr", "wrkshputils"), dependencies = TRUE,
                 repos = c(getOption("repos"), "https://ajlyons.r-universe.dev"))



