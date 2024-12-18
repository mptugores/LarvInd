#------------------------------------------------------------------------------#
# Data Exploration and Mapping                                                 #
#                                                                              #
# Script to perform data exploration on ichthyoplankton TUNIBAL surveys        #
#                                                                              #
# Analysis performed:                                                          #
# 1) Sample summary                                                            #
# 2) Volume filtered by fishing gear                                           #
# 3) Yearly maps of samples                                                    #
# 4) Yearly maps of samples with CTD                                           #
# 5) Yealy presence-absence maps for the selected species                      #
#                                                                              #
# Project: BLUEFIN, PANDORA                                                    #
# Author: D. Alvarez, M.P. Tugores                                             #
#                                                                              #
# Last modified: 2022/04/04                                                    #
#------------------------------------------------------------------------------#
#                                                                              #
#**********                                                                    #
# Inputs                                                                       #
#**********                                                                    #
#                                                                              #
# Scripts                                                                      #
# (1) "01_0_set_input_files.R": with objects containing input data file names  #
# (2) "Rsource/settings_m3.r": containing settings and all dependencies        #
#                                                                              #
# Files from "Module 2: Gettabs_LarvInd"                                       #
# (3) "outtables/20211115_t_abundance_larvind_BFT_abs.csv": abundance table    #
# (4) "data/processfiles/00_0_process_specifs_BFT_ALB_SKJ_2021-11-15.RData"    #
#                                                                              #
#                                                                              #
#**********                                                                    #
# Outputs                                                                      #
#**********                                                                    #
# 1) Summary of samples                                                        #
#    - printed in the R console                                                #
#                                                                              #
# 2) Boxplots of volume filtered by fishing gear                               #
#    - Printed in the R console                                                #
#    - Saved to .tiff:                                                         #
#        (1) "boxplot_volumes_B60&B90_BFT.tiff" (all the data)                 #
#        (2) "boxplot_volumes_B60&B90_BFT_onlyLI_1.tiff" (only larval index 1) # 
#                                                                              #
# 3) Yearly maps of samples                                                    #
#    - For larval_index=1, larval_index=2, larval_index=c(1,2):                #
#    - tiffs:                                                                  #
#        (1) "Maps_Samples_in_larvind_1_2001.tiff"                             #
#        (2) "Maps_Samples_in_larvind_2_2001.tiff"                             #
#        (3) "Maps_Samples_in_larvind_1&2_2001.tiff"                           #
#                                                                              #
# 4) Yearly maps of samples with CTD                                           #
#    - tiffs: (4) "Maps_Samples_withCTD_2001.tiff"                             #
#                                                                              #
# 5) Yearly maps of presence-absence data for selected species                 #
#    - tiffs: (5) "Maps_presence-absence_BFT_2001.tiff"                        #
#                                                                              #
#------------------------------------------------------------------------------#

rm(list=ls())

#############
# Inputs
#############
source("01_0_set_input_files.R") # script to set the following parameters:
species
fabund
# species: scientific name of the species to be analysed
# fabund: file containing the abundance data
# flen: file containing the length-frequency data for the selected species
# fprocess: file containing the process specifications that have been used to produce
#           the previous files

#############
# Current script name
#############
fcurr <- "01_1_explore_data_&_map_samples.r"

############################# WARNING ##########################################
#                     DON'T CHANGE the rest of the code !!!                    #
################################################################################

# Load settings and functions
source("Rsource/settings_m3.R"); sp; faocode

# Check existence of required files
files2check <- c(paste(opath, fabund, sep=""),
  paste(opath, flen, sep=""),
  paste(propath, fprocess, sep=""))

filecheck(files2check)

# Read abundance dataset
if(exists("t0")) rm(t0)                
t0 <- read.csv3(paste(opath, fabund, sep=""), na.strings="NA", dec='.', header=TRUE)
names(t0); nrow(t0)
str(t0)
summary(t0)

#***********************
# 1. Sample summary
#***********************
#resume número de muestras por año.... pivot tables in R 
# http://stackoverflow.com/questions/10220510/summary-statistics-by-two-or-more-factor-variables

# Number of samples by year
Nsamp_y <- tapply(t0$id_operation,t0$year,length)

# Number of samples by year and gear used
Nsamp_yg <- with(t0, 
  tapply(id_netcolector, list(gear,year), length)
)

# Number of samples by year and gear used and total amount of samples by year
# Data frame combining Nsamp_yg and Nsamp_y
Nsamp <- data.frame(t(Nsamp_yg), Total=as.vector(Nsamp_y)); Nsamp

# Number of samples by year and fishing type
Nsamp_ft <- with(t0, 
  tapply(id_netcolector, list(fishing_type,year), length)
); t(Nsamp_ft)


# Summary of towing depth by year, fishing type and gear used
TowDep_summary <- with(t0,
  aggregate(towdepth, with(t0, list(fishing_type, gear, year)), 
    function(x){c(min=min(na.omit(x)), mean=mean(na.omit(x)), max=max(na.omit(x)))})
); TowDep_summary # if there are NAs in towdepth, the 'summary' function does not work properly



#******************************************
#  2. Volume filtered by fishing gear
#******************************************
par(mar=c(7,7,3,3)) 
with(t0,
  boxplot(volume[gear=="B90"]~year[gear=="B90"], 
    xlab="Year", ylab="Volume (m3)", cex.main=2, cex.lab=2, cex.axis=1.5, cex.names=1.5, main="Bongo 90")
)

dev.new()
par(mar=c(7,7,3,3))
with(t0,
  boxplot(volume[gear=="B60"]~year[gear=="B60"], 
    xlab="Year", ylab="Volume (m3)", cex.main=2, cex.lab=2, cex.axis=1.5, cex.names=1.5, main="Bongo 60")
)

#--------------------------
# Saved to a .tiff file
#--------------------------
# As long as we have performed species specific filters in Module 2, the filtered volumes may
# differ from one species to the other.
tiff(paste(figpath,"boxplot_volumes_B60&B90_", faocode, ".tiff",sep=""), height=26, width=13*1.5, units = "cm",res=150)
  par(mfrow=c(2,1))
  boxplot(t0$volume[t0$gear=="B60"]~t0$year[t0$gear=="B60"], main="Volumenes B60", xlab="Year", ylab="Volume (m3)")
  boxplot(t0$volume[t0$gear=="B90"]~t0$year[t0$gear=="B90"], main="Volumenes B90", xlab="Year", ylab="Volume (m3)")
dev.off()

# Now, only including larval index 1
t1 <- t0[t0$larval_index==1,]
tiff(paste(figpath,"boxplot_volumes_B60&B90_", faocode, "_onlyLI_1.tiff",sep=""), height=26, width=13*1.5, units = "cm",res=150)
  par(mfrow=c(2,1))
  boxplot(t1$volume[t1$gear=="B60"]~t1$year[t1$gear=="B60"], main="Volumenes B60", xlab="Year", ylab="Volume (m3)")
  boxplot(t1$volume[t1$gear=="B90"]~t1$year[t1$gear=="B90"], main="Volumenes B90", xlab="Year", ylab="Volume (m3)")
dev.off()


#*****************************
# 3. Yearly maps of samples
#*****************************
# If save2tiff=FALSE maps printed to the R console
# If save2tiff=TRUE maps also saved to a .tiff file

# Map samples used in larval index 1  
mapSamples(t0, yearvar="year", larv_ind=c(1), path=figpath, 
   lonrange=c(0.5, 5.0), latrange=c(37.5, 41.0), save2tiff=TRUE) 

# Close all maps in the R Console
for(i in 1:length(dev.list())) dev.off()


if(any(t0$larval_index==2)) {
# Map samples used in larval index 2
mapSamples(t0, yearvar="year", larv_ind=c(2), path=figpath, 
   lonrange=c(0.5, 5.0), latrange=c(37.5, 41.0), save2tiff=TRUE)
# Close all maps in the R Console
for(i in 1:length(dev.list())) dev.off()

# Map samples used in larval index 1 & 2 
mapSamples(t0, yearvar="year", larv_ind=c(1,2), path=figpath, 
  lonrange=c(0.5, 5.0), latrange=c(37.5, 41.0), save2tiff=TRUE)
# Close all maps in the R Console
for(i in 1:length(dev.list())) dev.off()
}

#*****************************
# 4. Yearly maps of samples with CTD
#***************************** 
# If save2tiff=FALSE maps printed to the R console
# If save2tiff=TRUE maps also saved to a .tiff file

# Map samples with CTD 
mapSamples_withCTD (t0, yearvar="year", path=figpath, 
  lonrange=c(0.5, 5.0), latrange=c(37.5, 41.0), save2tiff=TRUE)

# Close all maps in the R Console
for(i in 1:length(dev.list())) dev.off()

#*****************************
# 5. Yearly presence-absence maps
#*****************************
# If save2tiff=FALSE maps printed to the R console
# If save2tiff=TRUE maps also saved to a .tiff file

# Map species presence-absence
mapSpecies_pa (t0, yearvar="year", path=figpath, 
  lonrange=c(0.5, 5.0), latrange=c(37.5, 41.0), save2tiff=TRUE) # Maps saved to a .tiff file

 # Close all maps in the R Console
for(i in 1:length(dev.list())) dev.off()

################################################################################
# Possible further developments:
# 
# 1) Sample summary saved as .txt or .csv files
# 2) Afegir a les funcions de mapping la possibilitat de modificar els noms dels .tiff de sortida (o potser no cal)
################################################################################

#-----------------------------------
# Save log file with settings
#-----------------------------------
flogname <- paste("LarvInd_settings_log_", faocode, "_", substring(fabund, 1, 8), "_", Sys.Date(), ".txt", sep="") 
sink(file=paste(flogname), append=TRUE)
print(paste("--- '", fcurr, "' ---"))
print(paste("Current date : ", Sys.Date()))
print(paste("Abundance and length files processed on date : ", substring(fabund, 1,8)))
print(paste("Species: ", species))
print(paste("Abundance file (fabund): ", fabund))
print(paste("Length file (flen): ", flen))
print(paste("Processing file for abundance and length data (fprocess) :", fprocess))
cat("\n")
sink()

#------------------------ End of script ---------------------------------------#