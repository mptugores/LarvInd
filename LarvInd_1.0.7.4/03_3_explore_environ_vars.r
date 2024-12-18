#------------------------------------------------------------------------------#
# Environmental Data Exploration                                               #
#                                                                              #
# Script to perform data exploration for environmental variables               #
#     from in situ CTD data from ichthyoplankton TUNIBAL surveys               #
#                                                                              #
# Analysis performed:                                                          #
# 1) Boxplots of environmental variables  (classical style)                    #
# 2) Boxplots of environmental variables (Ottman style)                        #
# 3) Histogram of environmental variables weighted by Abundance                #
#                                                                              #
# Project: BLUEFIN, PANDORA                                                    #
# Authors: D. Alvarez, M.P. Tugores                                            #
#                                                                              #
# Last modified: 2021/12/01                                                    #
#------------------------------------------------------------------------------#
#                                                                              #
#**********                                                                    #
# Inputs:                                                                      #
#**********                                                                    #
#                                                                              #
# Scripts                                                                      #
# (1) "01_0_set_input_files.R": with objects containing input data file names  #
# (2) "Rsource/settings_m3.r": containing settings and all dependencies        #
#                                                                              #
# Files from "Module 2: Gettabs_LarvInd"                                       #
# (3) "data/processfiles/00_0_process_specifs_BFT_ALB_SKJ_2021-11-15.RData"    #
#                                                                              #
# From previous scripts in this module                                         #
# (4) Standardised abundance table with interpolated environ. vars (fanalisis):#
#     "outtables/BFT/20211115_t_abu_larvind_BFT_inter_insitu.csv"              #
#                                                                              #
#**********                                                                    #
# Outputs:                                                                     #
#**********                                                                    #
# 1. Boxplots of environmental variables                                       #
# - tiff: "boxplot_flumezcla_stand.tiff"                                       #
#                                                                              #
# 2. Boxplot of environmental variables (Ottman style)                         #
# - tiff: "boxplotOttman_flumezcla_stand_BFT.tiff"                             #
#                                                                              #
# 3. Histogram environmental variables weighted by Abudance                    #
# - tiff: "hist_envir__flumezcla_stand_PA_Ab_BFT.tiff"                         #
#                                                                              #
#------------------------------------------------------------------------------#

# Open table abundances and fishing data
rm(list=ls())

#############
# Inputs
#############
source("01_0_set_input_files.r"); species; fprocess
source("Rsource/settings_m3.R"); faocode; sp # Load functions, relative folder paths and settings

fanalisis <- paste(outdate, "_t_abu_larvind_BFT_allvars.csv", sep="")
fanalisis <- paste(outdate, "_t_abu_larvind_ALB_allvars.csv", sep="")
fcurr <- "03_2_explore_environ_vars.r"  # Current script name
                
# Check existence of required files
files2check <- c(paste(sppath, fanalisis, sep=""))
filecheck(files2check)

################################################################################
#                            PROCESSING                                        #
################################################################################

#################
# Read file
#################
t0 <- read.csv3(paste(sppath, fanalisis, sep=""), na.strings='NA')
# t0$date <- as.Date(t0$date)  
# con la función read.csv3 que usa fread de data.table no hace falta formatear las fechas

nrow(t0); names(t0)
head(t0)
str(t0)
summary(t0)

t1=t0; t1

################################################
# Generate standardised FMEZCLA variable
################################################
t1$flumezcla_stand <- t1$FMEZCLA/t1$prof_mld # FMEZCLA standardised by depth of the mixed layer

################################################
# Generate abundance standardised by volume
#     nlarvae_100m3 = 100* N larvae/volume
################################################
if(!any(names(t1)=="nlarvae_100m3")) t1$nlarvae_100m3 <- 100*(t1[,sp]/t1$volume)  # Abundance standardised by volume

################################################
# Variables to plot and labels for the y-axis
################################################

# Names of the variables to plot
var2plot <- c("TMEZCLA", "SMEZCLA", "jd", 
              "OMEZCLA", "FMEZCLA", 
              "prof_mld", "flumezcla_stand", 
              "residualtemp", "towdepth")
# Labels for the y-axis              
ylabs <- c("Temperature mixed layer depth (ºC)", "Salinity mixed layer depth (psu)", "Julian day", 
            "Oxygen mixed layer depth", "Fluorescence mixed layer depth", 
            "Depth of the mixed layer (m)", "Standardised Fluorescence", 
            "Residual Temperature mixed layer depth", "Towing Depth (cm)")

#------------------------------------------------
# 1. Boxplots of environmental variables by year
#------------------------------------------------

# Sample usage
# 1.a) Only one variable (TMEZCLA)
# boxplotEnv (t1, "year", "TMEZCLA", "Temperature")
# boxplotEnv (t1, "year", "TMEZCLA", "Temperature", savePlots=TRUE, path=figpath)  # Guardando en .tiff

# 1.b) For a group of variables, defined in var2plot
boxplotEnv(t1, "year", var2plot, ylabs) # Sin pseudónimo
boxplotEnv (t1, "year", var2plot, ylabs, savePlots=TRUE, path=figpath) # Guardando en .tiff  sin pseudónimo en el nombre del fichero
# boxplotEnv (t1, "year", var2plot, ylabs, savePlots=TRUE, saveID="v2") # Guardando en .tiff con un pseudónimo en el fichero

#----------------------------------------------------
#  2. Boxplot environmetal variables (Ottman style)
#----------------------------------------------------

# 2.a) Boxplots shown in the R Console
boxplotOttman(t1, "year", "TMEZCLA")
boxplotOttman(t1, "year", "FMEZCLA")
boxplotOttman(t1, "year", "SMEZCLA")
boxplotOttman(t1, "year", "OMEZCLA")
boxplotOttman(t1, "year", "predsal")
boxplotOttman(t1, "year", "predtemp")
boxplotOttman(t1, "year", "residualtemp")

# 2.b) Save one plot manually
# png(filename = paste(figpath, "boxplotOttman_SMEZCLA_dotplot.png", sep=""), width = 20, height = 15, units = "cm", res = 100)
#  boxplotOttman(t1, "year", "SMEZCLA")
# dev.off()

# 2.c) Save all plots in a loop
for (i in var2plot){
   png(filename = paste(figpath, "boxplotOttman_", i, "_", faocode, ".png", sep=""), width = 20, height = 15, units = "cm", res = 100)
   print(boxplotOttman(t1, "year", i))
   dev.off()
}

#------------------------------------------------------------------------
#  3.  Histogram environmental variables weighted by Abudance
#------------------------------------------------------------------------

faocode
sp

#################
# Sample usage, single variable
# histAb.environ(t1, "TMEZCLA", sp, pa=TRUE, ab=TRUE, savePlots=FALSE) # by deffault savePlots=FALSE, so, you don't need to specify it
# histAb.environ(t1, "SMEZCLA", sp, pa=TRUE, ab=TRUE) 
# histAb.environ(t1, "FMEZCLA", sp, pa=TRUE, ab=TRUE) 

#################
# Save all plots as .tiff in the outputfigures folder
histAb.environ (t1, var2plot, sp, pa=TRUE, ab=TRUE, savePlots=TRUE, path=figpath, saveID=faocode)

bandwidths <- c(0.5, 0.1, 4, 0.2, 0.2, 1, 0.01, 0.2, 5)

for (i in 1:length(var2plot)){
  histAb.environ (t1, var2plot[i], band_width=bandwidths[i], sp, pa=TRUE, ab=TRUE, 
  savePlots=TRUE, path=figpath, saveID=paste(faocode, bandwidths[i], sep="_"))
}



#--------------------------------------
# Save log file with settings 
#--------------------------------------
flogname <- paste("LarvInd_settings_log_", faocode, "_", substring(fabund, 1, 8), "_", Sys.Date(), ".txt", sep="") 
sink(file=paste(flogname), append=TRUE)
print(paste("--- '", fcurr, "' ---"))
print(paste("Current date : ", Sys.Date()))
print(paste("Abundance and length files processed on date : ", substring(fabund, 1,8)))
print(paste("Species: ", species))
print(paste("Abundance file (fabund): ", fabund))
print(paste("Length file (flen): ", flen))
print(paste("Processing file for abundance and length data (fprocess) :", fprocess))
print(paste("Abundance file for analysis (fanalisis) : ", fanalisis))
cat("\n")
sink()


#---------------------- End of script!! ---------------------------------------#