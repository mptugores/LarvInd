#------------------------------------------------------------------------------#
# Project: BLUEFIN, PANDORA                                                    #
# Authors: M.P. Tugores                                                        #
#                                                                              #
# Single/Multi-species abundance and length tables                             #
# with flexible filtering options                                              #
#                                                                              #
# 1.- Abundance table with CTD                                                 #
# 2.- Length frequency table                                                   #
#                                                                              #
# Last modified: 2023/11/24                                                    #
#------------------------------------------------------------------------------#
#**********                                                                    #
# Inputs:                                                                      #
#**********                                                                    #
# - Abundance database: "ecolarv_comunidades_actual.mdb"                       #
# - CTD database: "CTD_para_bluefin_actual.mdb"                                #
# - Database paths: 'dbpath', 'dbpathCTD'                                      #
#                                                                              #
# - Rsource/settings_m2.R                                                      #
# - Rsource/db.functions.R                                                     #
# - Rsource/base.functions.R                                                   #
# - Rsource/relative_folderpaths.R                                             #
#                                                                              #
#**********                                                                    #
# Outputs:                                                                     #
#**********                                                                    #
# 1) Process files:                                                            #
#    "data/processfiles/00_0_process_specifs_BFT_ALB_2021-11-19.RData"         #
#    "data/processfiles/00_1_ecolarv_DB_filt_BFT_2021-11-03.RData"             #
#                                                                              #
# 2) Abundance files:                                                          #
#    1. Raw Abundance table:                                                   #
#    Containing abundance data for the selected species and CTD data           #
#    'Abundance_df'                                                            #
#    "data/abundance_larv.index_multispecies_BFT_ALB_2021-11-19.csv"           #
#                                                                              #
#    2. Check files for the 'grupo_para_procesar':                             #
#    "data/processfiles/check1_Duplicated_collectors_Tunidos_2021-10-29.txt"   #
#    "data/processfiles/check1_Duplicated_stations_Tunidos_2021-10-29.txt"     #
#                                                                              #
#    3. Filtered & processed abundance tables (.csv and .xlsx):                #
#    One for each species:                                                     #
#    'abund_list': list                                                        #
#    "outtables/20211029_t_analisis_larvind_ALB_abs.csv"                       #
#                                                                              #
# 3) Length-frequency tables:                                                  #
#    1. Raw length-frequency table                                             #
#    'agQ23': data.frame                                                       #
#    "data/length.frec_abundance_340_BFT_2021-11-19.csv"                       #
#                                                                              #
#    2. Check files, for each species:                                         #
#    "data/processfiles/check2_on_length.frec_340_BFT_2021-10-29.csv"          #
#                                                                              #
#    3. Filtered and processed LF tables, one for each species:                #
#    'length_list': list                                                       #
#    "outtables/20211029_t_length_larvind_BFT.csv"                             #
#                                                                              #
#------------------------------------------------------------------------------#


rm(list=ls())

#******************************************************************************#
# 0. Load required libraries                                                   #
#******************************************************************************#

packages = c("RODBC", "writexl", 
              "data.table", 
              "lubridate", "lunar")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
#******************************************************************************#
# 1. Inputs                                                                    #
#******************************************************************************#

#-----------------------------------
# 1.1. Setting databases and paths 
#-----------------------------------
# Declare path ECOLARV database
dbpath=paste(getwd(), "/", sep="")
databaseversion="ecolarv_comunidades_actual.mdb"

# Declare path CTD database
dbpathCTD=paste(getwd(), "/", sep="")
databaseCTD="CTD_para_bluefin_actual.mdb"

# Independent table for CTD data
independentCTD=FALSE # If TRUE, a file containing only CTD data is generated

# Surface data (should CTD data from 5 m be retrieved?)
surf=FALSE

#------------------------------
# 1.2. Process specifications
#------------------------------
#------------------------------------------------------------------------------#
# 'input_process_specifs': object of class 'list'                              #
#                                                                              #
# Containing at least:                                                         #
# 1) grupo_para_procesar: the name of the group to which the species           #
#                         to be processed belongs to                           #  
# 2) tes_nomcientifico: the scientific names of the species being processed    #
#                                                                              #
# Additional variables (possible processing specifications):                   #
# to_tipo_estacion, to_fecha_sistematica, to_revisita                          #
#                                                                              #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# The names of objects in the list 'input_process_specifs'                     #
# MUST have the SAME NAME as the corresponding columns                         #
# in the ECOLARV database                                                      #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#                                                                              #
# *******   Listado especies de túnidos disponibles *********                  # 
# tuna.species <- c("Thunnus thynnus", "Thunnus alalunga", "Auxis rochei",     #
# "Euthynnuys alletteratus", "Katsuwonus pelamis", "Thunnus atlanticus",       #
# "LSV tunidos NI")                                                            #
#                                                                              # 
# *******   Listado de grupos para procesar (o "ttn_id_target_num") *********  #
# grupos_para_procesar:                                                        #
# 1: Tunidos                                                                   #
# 2: Ictio sin tunidos                                                         #
# 3: Paralarvas                                                                #
# 4: Decapodos                                                                 #
# 5: Phyllosomas                                                               #
# 6: Ephyras                                                                   #
# 7: Xiphiidae                                                                 #
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#   Examples with standard species:                                            #
#   Those classified as grupo_para_procesar="Tunidos" in the ecolarv database  #
#                                                                              #
# *******   Listado especies de túnidos disponibles *********                  # 
#   "Thunnus thynnus", "Thunnus alalunga", "Auxis rochei",                     #
#   "Euthynnuys alletteratus", "Katsuwonus pelamis", "Thunnus atlanticus",     #
#   "LSV tunidos NI")                                                          #
#------------------------------------------------------------------------------#

#-------------------------------
# 'Tunidos'
#-------------------------------

# Run just one of the three possible configurations (or any other you may want)

input_process_specifs <- list(grupo_para_procesar=c("Tunidos") 
  , tes_nomcientifico=c("Thunnus thynnus")
  , tc_estructura=c("B60", "B90")
  , tc_cont_estructura=c(1) 
  , tpt_larval_index=c(1,2)) 
 
#input_process_specifs <- list(grupo_para_procesar=c("Tunidos") 
#  , tes_nomcientifico=c("Thunnus thynnus", "Thunnus alalunga")
#  , tc_estructura=c("B60", "B90")
#  , tc_cont_estructura=c(1) 
#  , tpt_larval_index=c(1)) 

#input_process_specifs <- list(grupo_para_procesar=c("Tunidos") 
#  , tes_nomcientifico=c("Thunnus thynnus", "Thunnus alalunga", "Katsuwonus pelamis")
#  , tc_estructura=c("B60", "B90")
#  , tc_cont_estructura=c(1)
#  #, tpt_larval_index=c(1,2,3,4,NA)) 
#  , tpt_larval_index=c(1,2)) 
  
#input_process_specifs <- list(grupo_para_procesar=c("Tunidos") 
#  , tes_nomcientifico=c("Thunnus thynnus", "Thunnus alalunga")
#  , tc_estructura=c("B60", "B90")
#  , tc_cont_estructura=c(1) 
#  , tpt_larval_index=c(1,2))   
                                                  
#------------------------------------------------------------------------------#
#   Examples with standard species:                                            #
#  Those classified as grupo_para_procesar="Ictio sin tunidos" in the database #
#                                                                              #
# *******   Especies disponibles *********                                     # 
# "Xiphias gladius", etc.                                                      #
#------------------------------------------------------------------------------#
#-----------------------
# Xiphias gladius
#  and other species without tpt_larval_index declared
#-----------------------
input_process_specifs <- list(grupo_para_procesar=c("Xiphiidae") 
    , tes_nomcientifico=c("Xiphias gladius")
    , tc_estructura=c("B60", "B90", "WP2")
    , tc_cont_estructura=c(1)
    , tpt_larval_index=c()  # Let this empty for X. gladius; it will be retrieved in a latter step from Tunidos
)

# Mentre X. gladius estigui classificada sempre com 'Ictio sin tunidos'
# no cal, però tampoc molesta que al 'grupo_para_procesar' afegim categories on no hi és.
# No dona error i ho calcula perfectament bé.



################################################################################
# Run scripts                                                                  #
# Do not change the code below                                                 #
################################################################################

# Loading libraries, functions, relative paths, etc.
source("Rsource/settings_m2.R")  

# Format process_specifs:
formatPS(input_process_specifs, databasefile) # Si ja existeix, et podria demanar si el vols sobreescriure
# Applies format to 'input_process_specifs'
# Creates an object 'process_specifs' containing processing specifications
# Outputs: 
# - 'process_specifs': object of class 'list' containing a formated version 
#                      of the input object 'input_process_specifs'
# - "data/processfiles/00_0_process_specifs_BFT_ALB_2021-11-19.RData": file containing the
#                      formated 'process_specifs' object


#******************************************************************************#
# 2) Processing                                                                #
#******************************************************************************#

######### (2.1) Abundance ######################################################
#----------------------------------------
# (2.1) Get raw abundance data from ECOLARV
#----------------------------------------
source("Rsource/02_1_get_Abundance_table_withCTD.r") 
# Retrieves the abundance data from the ECOLARV database
# for the selected species and according to the settings stablished 
# in the object 'process_specifs'.
# It can be applied for one or several species
# It also adds CTD data for the retrieved stations from the CTD database.

# Outputs: 
# - "data/processfiles/00_1_ecolarv_DB_filt_BFT_2021-11-03.RData"             
# - 'Abundance_df': data.frame containing the abundance for the selected species 
#                   and CTD data. It contains one or more abundance columns, 
#                   one for each processed species, with the scientific name of the species
# - "data/abundance_larv.index_multispecies_BFT_ALB_2021-11-19.csv": same as 'Abundance_df'        

names(Abundance_df) # Have a look at the output

#---------------------------------------------------
# (2.1.a) Check duplicated collectors and stations
#---------------------------------------------------
source("Rsource/02_1a_check01_duplicated_collectors_stations.r") # Check no duplicated colectors and stations
# Outputs: 
# Printed information regarding the existence or not of duplicates collectors
# and stations for the selected 'grupo_para_procesar':
#   (a) In the R Console
#   (b) As .txt files:
#       - "data/processfiles/check1_Duplicated_collectors_Tunidos_2021-10-29.txt"   
#       - "data/processfiles/check1_Duplicated_stations_Tunidos_2021-10-29.txt"     

#--------------------------------------------------------------
# (2.1.b) Creates table of abundance for larval index analysis 
#--------------------------------------------------------------
source("Rsource/02_1b_apply_filters&create_species_vars.r")
# Applies further filters (remove subsurface hauls, MEDIAS0710, duplicated stations in gap years)
# Creates density variables for each species
# Applies species specific filters: removes years 2002 & 2003 for albacore

# Outputs:
# - 'abund_list': list object containing abundance data and other variables,
#          each element of the list being a data.frame with the abundance data 
#          for one of the processed species.
# - "/outtables/t_analisis_larvind_BFT_abs.csv": a file for each of the processed 
#          species, containing the respective abundance data.

str(abund_list)
names(abund_list)
# Después de aplicados los filtros se podría volver a comprobar si hay duplicados.
# Al menos en el caso de Xiphias.


######### (2.2) Length-frequency data ##########################################

#-------------------------------------------------
# (2.2) Get raw length-frequency data from ECOLARV 
#-------------------------------------------------
source("Rsource/02_2_get_LFD.r") 
# Retrieves the length-frequency data from ECOLAR database
# for the selected species and according to the settings stablished 
# in the object 'process_specifs'
# It can be applied for one or several species

# Outputs: 
# - 'outQ23': data.frame object containing the length-frequency data for all selected species
# - "/data/length.frec_abundance_340_BFT_2021-11-29.csv": table containing 
#            the length-frequency data for each of the analysed species.
#
names(outQ23)
str(outQ23)

# For species without length-frequency data like Xiphias gladius, 
# the following error is shown:
#
# Error in eval(ei, envir) : 
#  No length data in 'ecolarv' DB for the species processed!!

#----------------------------------------------------------
# (2.2.a) Check number of larvae equals sum of tll_numero 
#----------------------------------------------------------
source("Rsource/02_2a_check02_comprueba_nlarvas_y_tllnumero.r")
# Analyses if the amount of larvae in tg_nlarvae equals 
# the amount of larvae in tll_numero, for each of the selected species

# Output:
# - Printed information, in the R Console, regarding: 
#   (1) the range of the differences between those two variables (tg_nlarvae and tll_numero)
#   (2) It there is some haul in which the differences among the two variables is greater than 2.
# 
# - "data/processfiles/check2_on_length.frec_340_BFT_2021-10-29.csv": a file for each selected species
#       containing among other variables, the two variables to be compared and the difference among them.           
#

#----------------------------------------------------
# (2.2.b) Creates table of length-frequency data  
#         for larval index analysis
#----------------------------------------------------
# source("Rsource/02_2b_apply_filters_4_t_length.r") # previous versions script
# No threshold length

# source("Rsource/02_2b_apply_filters_4_t_length_1.r") # sets a threshold length
# BFT: 10.92 mm (maximum in the series 2001-2019, without 2018)
# ALB: 8.5 mm (maximum in the series 2001-2019, without 2018)

source("Rsource/02_2b_apply_filters_4_t_length_2.r")
# BFT: 8.5 mm (maximum of the 2001-2005 B60 hauls)
# ALB: 8.5 mm (maximum in the series 2001-2019, without 2018)
 
# Applies further filters (remove subsurface hauls, MEDIAS0710, duplicated stations in gap years)
# Applies species specific filters: removes years 2002 & 2003 for albacore
# It needs to be the same filters applied to the abundance tables in the script
# "Rsource/02_1a_check01_duplicated_collectors_stations.r"

# Outputs:
# - 'length_list': list object containing an element for each of the processed species
#                  each element is a data.frame with the processed length-frequency data
# - "/outtables/20211029_t_length_larvind_BFT_abs.csv": one for each species

str(length_list) # check the output
names(length_list)
#-------------------------------------------------------------------------------