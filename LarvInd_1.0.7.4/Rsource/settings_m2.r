#------------------------------------------------------------------------------#
# Proyecto: BLUEFIN, PANDORA                                                   #
# Autores: M.P. Tugores                                                        #
#                                                                              #
# Aim:                                                                         #
# Load functions and                                                           #
# Load script to set relative folder paths & create folders if missing         #
# Create databasefile and databasefileCTD objects,                             #
#     through pasting the databases name and paths                             #
#                                                                              #
# Date: 2023/11/24                                                             #
#------------------------------------------------------------------------------#

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
source("Rsource/relative_folderpaths.R")

#------------------------------------------------------------------------------#
# Setting the databasefiles
#------------------------------------------------------------------------------#
databasefile <- paste(dbpath, databaseversion, sep="")  # ECOLARV database 
databasefileCTD <- paste(dbpathCTD, databaseCTD, sep="") # CTD database
#------------------------------------------------------------------------------#