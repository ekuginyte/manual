# Load libraries
libraries <- c("tidyverse", "blogdown")
              
# FUNCTION to quietly install the libraries
#   INPUT:
#   libraries - names of libraries.
libraries_install <- function(libraries){
  for (i in libraries) {
    library(i, character.only = TRUE)
  }
}

# Install libraries
libraries_install(libraries)
              
# Create website
new_site(theme = "wowchemy/starter-academic") 

