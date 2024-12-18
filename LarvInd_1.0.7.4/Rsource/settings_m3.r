#------------------------------------------------------------------------------#
# Open required libraries
#------------------------------------------------------------------------------#

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
#packages = c("RODBC", "xlsx",
#             "data.table", "raster", "maptools", "rgdal", "ggplot2", "lubridate")

#packages <- c("writexl",
#             "data.table", "raster", "maptools", "rgdal", "ggplot2", "lubridate",
#             "mgcv", "MASS", "effects", "pROC", "ggplot2", "emmeans", "rgeos",
#             "sp", "gsubfn", "htmltools", "htmlTable")


## Now load or install&load all
#package.check <- lapply(
#  packages,
#  FUN = function(x) {
#    if (!require(x, character.only = TRUE)) {
#      install.packages(x, dependencies = TRUE)
#      library(x, character.only = TRUE)
#    }
#  }
#)

#------------------------------------------------------------------------------#
# Load personalised functions and settings
#------------------------------------------------------------------------------#
scripts <- c("base.functions.R", "summary.functions.R", "plot_envirvars.R",
  "plot_indexvars.R", "gridcreate.R", "derived.envirvars.R", "mapping_functions.R",
  "saveModel_outputs.R", "relative_folderpaths_m3.R")
  
scripts_checkload <- lapply(
  scripts, 
  FUN=function(x){
    if(file.exists(paste("Rsource/", x, sep=""))){
      source(paste("Rsource/", x, sep=""))
    }
    else{
      print(paste("Script  ", x, "  does not exist.", sep=""))
    }
  }
)

#source("Rsource/base.functions.R")
#source("Rsource/summary.functions.R")
#source("Rsource/plot_envirvars.R")
#source("Rsource/plot_indexvars.R")
#source("Rsource/functions.R")
#source("Rsource/mapping_functions.R")
#source("Rsource/saveModel_outputs.R")


#------------------------------------------------------------------------------#
# Version
#------------------------------------------------------------------------------#
source("Rsource/version.r")

#------------------------------------------------------------------------------#
# Load personalised functions
#------------------------------------------------------------------------------#
source("Rsource/base.functions.R")
source("Rsource/db.functions.R")

#------------------------------------------------------------------------------#
# Set relative folder paths 
# & create folders if missing
#------------------------------------------------------------------------------#
#source("Rsource/relative_folderpaths.R")

#------------------------------------------------------------------------------#
# Setting the databasefiles
#------------------------------------------------------------------------------#
databasefile <- paste(dbpath, databaseversion, sep="")  # ECOLARV database 
databasefileCTD <- paste(dbpathCTD, databaseCTD, sep="") # CTD database
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Set relative folder paths 
# & create folders if missing
#------------------------------------------------------------------------------#
#source("Rsource/relative_folderpaths_m3.R")

#------------------------------------------------------------------------------#
# Select species
#------------------------------------------------------------------------------#
# Load process specifications
#load(paste(propath, fprocess, sep="")) # No faria falta, ja se carrega a '00_set_input_files.r' 

#faocode <- with(process_specifs, FAO_X3A_Code[nomcientifico %in% species])
#sp <- gsub(" ", "_", species) # Species nº larvae variable (raw)

#faocode
#sp


# A ver si con esta línea funciona
#species <- process_specifs$nomcientifico

# Aquestes variables potser també podrien anar al "process_specifs"
abvar <- paste(tolower(faocode), "ab", sep="") # Species abundance variable
abgsvar <- paste(tolower(faocode), "ab_gs", sep="") # Species abundance variable standardised by gear
ABgsvar <- paste(faocode, "ab_gs", sep="")

#------------------------------------------------------------------------------#
# Load coastline
#------------------------------------------------------------------------------#
# Read .shp with coast
coastline <- readOGR("Rsource/sig/country.shp", verbose=FALSE)


#------------------------------------------------------------------------------#
# Dir create
#------------------------------------------------------------------------------#
sppath = paste(opath, faocode, "/",sep="")

if(!dir.exists(sppath)) dir.create(sppath)
#------------------------------------------------------------------------------#