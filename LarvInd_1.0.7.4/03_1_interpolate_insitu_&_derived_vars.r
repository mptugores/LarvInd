#------------------------------------------------------------------------------#
# Script to interpolate in situ environmental data                             #
#     and generate residualtemp                                                #
#                                                                              #
# 1. Imputation of NA values (fill CTD data where missing)(impute.missing)     #
# 2. Compute residualtemp from CTD data: removes the effect of the day         #
#    of the year (jd)                                                          #
# 3. Compute environmental variables annual anomalies (Year_anom)              #
# 4. Save .csv & .xlsx with derived variables                                  #
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
# (4) Standardised abundance table (fanalisis):                                #
#     "outtables/BFT/20211115_t_abu_larvind_BFT_2mm_m2_mixgears.csv"           #
#                                                                              #
#                                                                              #
#**********                                                                    #
# Outputs:                                                                     #
#**********                                                                    #
# A. Table with summary statistics of the samplings                            #
# - csv: "20200728_t_abu_larvind_interp_insitu_BFT.csv"                        #
# - xlsx: "20200728_t_abu_larvind_interp_insitu_BFT.xlsx"                      #
#                                                                              #
#                                                                              #
# Project: BLUEFIN, PANDORA                                                    #
# Author: D. Alvarez, M.P. Tugores                                             #
#                                                                              #
# Last modified: 2021/12/21                                                    #
#------------------------------------------------------------------------------#


rm(list=ls())

#############
# Inputs
#############
source("01_0_set_input_files.R"); species; fprocess  # process specifications 
# that were used to retrieve abundance and length data
source("Rsource/settings_m3.R"); faocode; sp  # Load functions, relative paths and settings


fanalisis <- paste(outdate, "_t_abu_larvind_BFT_2mm_m2_mixgears.csv", sep="")
fanalisis <- paste(outdate, "_t_abu_larvind_ALB_2mm_m2_mixgears.csv", sep="") 
# Usar el fichero saliente del script "operational_vars" cuando esté implentado como toca                                          
fcurr <- "03_1_interpolate_insitu_&_derived_vars.r" # Current script name


# Check existence of required files
files2check <- c(paste(sppath, fanalisis, sep=""))
filecheck(files2check)

################################################################################
#                            PROCESSING                                        #
################################################################################

#################
# Read file
t0 <- read.csv3(paste(sppath, fanalisis, sep=""), na.strings='NA')
# if(!is.null(t0$date)) t0$date <- as.Date(t0$date)
# if(is.null(t0$date) & is.null(t0$year)) {
#  stop("Missing date or year information. Script is aborted! Please, revise input file.")  
# }
head(t0)
dim(t0); names(t0)
str(t0)
summary(t0)


################################################################################
# 1. Imputation of NA values
################################################################################
t1=t0  # we copy the database in case we wanted to compare data before and after imputing missing values


###################### assign environmental data unkown ########################
#
# Sample usage: Impute salinity data
fill.missing
# fill.missing(tdata=t1, environ="SMEZCLA", predvarname="predsal", type="gam1", impute=TRUE, equation=NULL) 
#
###################### assign environmental data unkown ########################


#############
# Salinity
#############

# Analyse NAs
countNAs(t1$SMEZCLA) # count the amount of NAs
tapply(t1$SMEZCLA, t1$year, countNAs) # check amount of NAs by year

# Impute missing values
t2 <- fill.missing (tdata=t1, environ="SMEZCLA", "predsal", impute=TRUE) 

# Visualise imputation model & predictions
plot(mod_SMEZCLA, all=TRUE, pages=1)
with(t2, plot(SMEZCLA, predsal))

tapply(t2$SMEZCLA, t2$year, countNAs) # check amount of NAs by year

#############
# Temperature
#############

# Analyse NAs
countNAs(t1$TMEZCLA)
tapply(t1$TMEZCLA, t1$year, countNAs)

# Impute missing values
t2 <- fill.missing (t2, "TMEZCLA", "predtemp", impute=TRUE)

# Visualise imputation model & predictions
plot(mod_TMEZCLA, all=TRUE, pages=1)
with(t2, plot(TMEZCLA, predtemp))

tapply(t2$TMEZCLA, t2$year, countNAs) # check amount of NAs by year

#############
# Fluorescence
#############
names(t2)[which(names(t2)=="suma_flu_zmezcla")] <- "FMEZCLA"

# Analyse NAs
countNAs(t2$FMEZCLA)
tapply(t2$FMEZCLA, t2$year, countNAs)

t2 <- fill.missing(t2, "FMEZCLA", "predflu")

# Visualise imputation model & predictions
plot(mod_FMEZCLA, all=TRUE, pages=1)

with(t2, plot(FMEZCLA, predflu))
with(t2, text(FMEZCLA, predflu, labels=year))

# Se podría hacer también con "OMEZCLA" y con "prof_mld"
# which(is.na(t1$OMEZCLA))
# which(is.na(t1$prof_mld))

################ END assign environmental data unkown ##########################

################################################################################
# 2. Remove Temperature trend by julian day
################################################################################

## ¿Posibilidad de hacer?:
## gam(TMEZCLA~s(jd, by=as.factor(year), k=3), data=t1, method="REML", select=TRUE)

## calulate redisual temp & tempres2 
templm <- lm(TMEZCLA~jd, data=t2) # fitting a linear model with the relationship
          
# Check model outputs
summary(templm)
plot(templm)

t2$residualtemp <- predict(templm, t2, type="response")-t2$TMEZCLA  # residualtemp
plot(t2$residualtemp, t2$jd)

t2$tempres2 <- t2$TMEZCLA - predict(templm, t2, type="response") # tempres2

###############################################################################

################################################################################
# 3. Annual anomalies of environmental variables  (Year_anom)
################################################################################

#-------------------------
# Function: "Year_anom"
#-------------------------
Year_anom
# Substract yearly mean from each value in every single year
# tdata: data.frame
# environ: name of the variable on which yearly anomaly is to be computed
# year.var: name of the variable containing the year; set to "year" by deffault

# with(t2, ave(SMEZCLA, year, FUN=function(x)mean(x)))
SALanom <- Year_anom(tdata=t2, environ="SMEZCLA")
Tanom <- Year_anom(tdata=t2, environ="TMEZCLA")

# Boxplot Salinity 
par (mfrow=c(1,2))
boxplot(SMEZCLA~year, data=t2)
boxplot(SALanom~year, data=t2)

# Boxplot Temperature
par (mfrow=c(1,2))
boxplot(TMEZCLA~year, data=t2)
boxplot(Tanom~year, data=t2)

t2$SALanom <- SALanom
t2$Tanom <- Tanom
################################################################################

#-------------------------------
# Save imputation models
#-------------------------------
save(mod_TMEZCLA, mod_SMEZCLA, mod_FMEZCLA, templm,  
  file=paste(envproc_path, substr(fanalisis, 1, 8), "_envir_data_imputation_mods_", faocode, ".RData", sep=""))

sink(file=paste(envproc_path, substr(fanalisis, 1, 8), "_envir_data_Summary_imputation_mods_", faocode, ".txt", sep=""))
  print("#-------------------------------------------------------")
  print("mod_TMEZCLA")
  print(summary(mod_TMEZCLA))
  print("#-------------------------------------------------------")
  print("mod_SMEZCLA")
  print(summary(mod_SMEZCLA))
  print("#-------------------------------------------------------")
  print("mod_FMEZCLA")
  print(summary(mod_FMEZCLA))
  print("#-------------------------------------------------------")
  print("templm")
  print(summary(templm))
sink()


#-------------------------------
# Save as .csv and .xlsx files 
#-------------------------------
tout=t2

outfile <- paste(substr(fanalisis, 1, 8), "t_abu_larvind", faocode, "interp_insitu",  sep="_")
outfile

save.csvxlsx(tout, path=sppath, file_out=outfile)

#-------------------------------------
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

#------------------------ End of script!! -------------------------------------#
