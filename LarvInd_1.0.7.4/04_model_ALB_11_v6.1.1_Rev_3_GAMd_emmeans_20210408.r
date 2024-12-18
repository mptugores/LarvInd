#------------------------------------------------------------------------------#
# NOTAS (Diego):                                                               #
# 2019-12-11                                                                   #
# gam con emmenas usando el mismo modelo que el laversion 09_v5                #
#                                                                              #
# v.1: clcula abundancias con gears estandarizador entre si..                  #
#      osea a partir de la variable bftab_gs                                   #
#                                                                              #
# definitive approach for processing as Walter, following article              #
# of Mathew Lauretta SCRS/2015/029 for                                         #
# combinig the two models binary and lognormal.                                #
#                                                                              #
# Goodman exact estimator:                                                     #
# var_i=var_p*cpue**2 + var_c*ppos**2 - var_p* var_c;                          #
#-ppos :is the annual estimate of the proportion positive (lsmeans in SAS)     #
#       for the proportion positive,                                           #
#-var_p: is the variance of the of the proportion positive = SE^2              #
#-cpue: is the back-calculated (lsmeans in SAS) lognormal component            #
#-var_c: is the variance for the lognormal component (=StdErr^2)               #
#                                                                              #
# The only question is WHY var_c is computed from the lsmeans                  #
# of the log transformed and it is not back calulated as it is for the CPUE... #
#                                                                              #
# calulations can be tracked on walter tables in file:                         #
# D:/AAD/proyectos/TUNABIT/subproyectos/larval_index_walter/BFT/20170130/      #
# sin b90ss/one catch curve 20170405_index calculationas manual.xlsx           #
#------------------------------------------------------------------------------#
#                                                                              #
# Analysis performed:                                                          #
# 1) Apply additional filters (if required)                                    #
# 2) Create definitive analysis table                                          #
# 3) Annual Mean CPUA (=nominalCPUA) summary table and plot                    #
# 4) Modelling delta GAM model                                                 #
# 5) Estimate larval index estimate (least-squares means)                      #
#    combining the two parts of the delta model                                #
# 6) Save outputs                                                              #
#                                                                              #
# Project: BLUEFIN, PANDORA                                                    #
# Author: D. Alvarez, M. Pilar                                                 #
#                                                                              #
# Last modified: 2022/02/22                                                    #
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
#     "outtables/BFT/20211115_t_abu_larvind_BFT_interp_insitu.csv"             #
#                                                                              #
# Output configuration                                                         #
# (5) modelID:                                                                 #
#     e.g. "SU_VPA" # Strict update of the VPA model; or any other             #
#              character string that help identify the model and its outputs   #
#     e.g. "SU_MSEboot" # Strict update MSE bootstrap                          #
#     e.g. "AC_deltagam": # Actualization of the delta gam model               #
#                                                                              #
#**********                                                                    #
# Outputs:                                                                     #
#**********                                                                    #
# 1) Summary table of mean CPUA by year (=nominal CPUA)                        #
#    - R object: summarytable                                                  #
#    - .csv and .xlsx files: containing the data.frame                         #
#     "outtables/BFT/20211115_summaryCPUAyear_ci_Ingram_BFT_2001-2019_SU_VPA.csv" 
#                                                                              #
# 2) Plot of mean CPUA by year                                                 #
#    - in the R console                                                        #
#    - as .png file (if desired):                                              #
#    "outfigures/20211115_meanCPUAyear_ciIngram_BFT_2001-2019_SU_VPA.png"      #
#                                                                              #
# 3) Histograms of the envirornmental variables                                #
#     - in the R console                                                       #
#                                                                              #
# 4) Binomial part of the model (results)                                      #
#                                                                              #
#    4.1) Plot of the partial effects                                          #
#     - in the R console                                                       #
#     - as .png file (if desired)                                              #
#     "outfigures/BFT_larval_index_SU_VPA_bin_effects_20211115.png"            #
#                                                                              #
#    4.2) Model fitting results (binomial part)                                #
#         4.2.1) ROC plot and AUC                                              #
#          - in the R console                                                  #
#          - as .png file (if desired)                                         #
#          "outfigures/BFT_larval_index_SU_VPA_bin_pROC_20211115.png"          #
#         4.2.2) Model fitting                                                 #
#          - in the R console                                                  #
#         4.2.3) Density plot of fitted positive and negative                  #
#          - in the R console                                                  #
#          - as .png file (if desired)                                         #
#          "outfigures/BFT_larval_index_SU_VPA_bin_fitting_density_20211115.png"#  
#                                                                              #
# 5) Histogram of the abundance variable                                       #
#     - in the R console                                                       #
#     - as .png file (if desired)                                              #
#     "model_outputs/SU_VPA/BFT_larval_index_SU_VPA_ABS_HIST_20211115.png"     #
#                                                                              #
# 6) Abundance part of the model (results) - only positive data                #
#                                                                              #
#    6.1) Plot of the partial effects                                          #
#     - in the R console                                                       #
#     - as .png file (if desired)                                              #
#     "outfigures/BFT_larval_index_SU_VPA_ABS_effects_20211115.png"            #
#                                                                              #
#    6.2) Plot of model residuals                                              #
#     - in the R console                                                       #
#     - as .png file (if desired)                                              #
#     "outfigures/BFT_prb_abundances_SU_VPA_RESIDUALS_20211115.png"            #
#                                                                              #
# 7) Parameters of the finally selected models                                 #
#    - .html and .xlsx files                                                   #
#    "model_outputs/SU_VPA/BFT_model_SU_VPA_parameters_20211115.html"          #
#    "model_outputs/SU_VPA/BFT_model_SU_VPA_parameters_20211115.xlsx"          #
#                                                                              #   
# 8) Larval index table calculated with emmeans                                #
#    - R object: index_table                                                   #
#    - .csv and .xlsx file                                                     #
#    "model_outputs/SU_VPA/20211115_larval_index_output_table_BFT_2019_v07_v03_SU_VPA.csv"#
#                                                                              #   
# 9) Plots of larval index                                                     #
#    9.1) Binomial part                                                        #
#     - in the R console                                                       #
#     - as .png file (if desired)                                              #
#     "outfigures/BFT_larvind_prob_presence_SU_VPA_emmeans_20211115.png"       #
#    9.2) Abundance part                                                       #
#     - in the R console                                                       #
#     - as .png file (if desired)                                              #
#     "outfigures/BFT_larvind_cpue_SU_VPA_emmeans_20211115.png"                #
#    9.3) Larval index                                                         #
#     - in the R console                                                       #
#     - as .png file (if desired)                                              #
#     "outfigures/20211115_larvalindex_BFT_SU_VPA_emmeans_ciIngram.png"        #
#------------------------------------------------------------------------------#

rm(list=ls())

############
# Inputs               
#############

source("01_0_set_input_files.R"); species; fprocess
source("Rsource/settings_m3.R") # Load functions, relative paths and settings

fanalisis <- paste(outdate, "_t_abu_larvind_ALB_allvars.csv", sep="")  ### Clar, no existeix. Me falten les passes anteriors

# Model name (modelID)
modelID <- "Rev_3_GAMd_em"   # Revised Delta GAM with emmeans
# Other recommended modelID names: "SU_MSE", "SU_VPA"   # Strict Update
# AC_deltagam: Actualization delta gam model
# "SU_MSEboot" Strict update of the MSE bootstrap model

# Current script name
fcurr <- "04_model_ALB_11_v6.1.1_Rev_3_GAMd_emmeans.r"

# Check the introduced fretro file is for the species being processed
if(length(grep(faocode, fanalisis))==0) stop("Check the input file. It may not be of the species being processed!!")
if(length(grep(faocode, fanalisis))==1) print("Ok!")

# Check existence of required files
files2check <- c(paste(sppath, fanalisis, sep=""))
filecheck(files2check)

################################################################################
#                            PROCESSING                                        #
################################################################################

# Read file
tdef <- read.csv3(paste(sppath, fanalisis, sep=""), na.strings="NA", header=T)
tdef$date <- as.Date(tdef$date)

#********************************************
# 1. Apply additional filters (if required)
#********************************************
#exclude gridi2 = 46, no represse3ntatico espacialemtne
which(tdef$gridid2==46)
#tdef=tdef[-which(tdef$gridid2==43),]
tdef=tdef[-which(tdef$gridid==44),]
#nrow(tdef)

#****************************************************
# 2. Create definitive analysis table  
#****************************************************
# Create standardised abundance variable in the same units than Walter (x10)
tdef$V1 = tdef[,abgsvar]*10
names(tdef)[names(tdef)=="V1"] <- ABgsvar
tapply(tdef[,ABgsvar],tdef$year,mean)

# Abundance only data
tabs=subset(tdef,lpres==1);str(tabs)

#****************************************************
# 3. Annual Mean CPUA summary table and plot (=nomialCPUA)
#****************************************************

############# To compare with the data without additional filters ##############
# Assess n larvas, nsamples and nominal index with cpua gear standardized      #
# on the final table (tdef)                                                    #
# after the application of addtional filters (if some has been applied)        #
################################################################################

# We can save it with the model pseudonym (modelID) 
# in case additional filters have been applied
sp
abgsvar
 
summarytable <- summaryCPUAyear (tab=tdef, year.var="year", date.var="date"
  ,abundance=sp, abundance2mm="n2mm", stand.abund=abgsvar 
  ,ci.method="non-normal"); summarytable 

# Plot
with(summarytable,
  plot.larvind(year, meanCPUA, LCI=LCInn, UCI=UCInn, my_ylab="Mean CPUA (2mm)+-CI")
)

#********************************************
# 4. Modelling: delta GAM
#********************************************

# Set format for factor variables
# tdef$fjd=as.factor(tdef$jd)
# tdef$fmonth=as.factor(tdef$month)
# tdef$fYear=as.factor(tdef$year)
# tdef$fgridid=as.factor(tdef$gridid)

#-----------------------
# Re-run the models
#----------------------
# mbin <- gam(lpres ~ as.factor(year)+s(jd, k=3)+s(SALanom, k=3)+ s(tempres2, k=3), family=binomial(link="logit"), method="REML", select=TRUE, data=tdef)
# mab <- gam(log(BFTab_gs) ~ as.factor(year)+s(lon, lat, k=9)+s(SALanom, k =3)+s(tempres2, k=3), family=gaussian, method="REML", select=TRUE, data=tabs)

# mbin <- gam(lpres~as.factor(year)+s(lat,lon)+s(jd,k=3)+s(SMEZCLA,k=3), data=tdef, family=binomial)# quito ournorm
# mab <- gam(log(ALBab_gs)~as.factor(year)+s(jd,k=3)+s(SMEZCLA,k=3)+s(residualtemp,k=3), data=tabs, family=gaussian)

# Standing alone target variable (required for script functioning)
ABgsvar
target <- tabs[,ABgsvar]


mbin <- gam(lpres~as.factor(year)+s(lat,lon)+s(jd,k=3)+s(SALanom,k=3), data=tdef, family=binomial, method="REML", select=TRUE)# quito ournorm
mab <- gam(log(ALBab_gs)~as.factor(year)+s(jd,k=3)+s(SALanom,k=3)+s(tempres2,k=3), data=tabs, family=gaussian, method="REML", select=TRUE)

# Provar SALanom, restemp2, ¿algo más?

#-----------------------
# Model responses
#-----------------------
# png(paste(modpath, faocode, "_", ModelID, "_pa_", outdate, ".png", sep=""), width=480, height=480*1.5)
par(mfrow=c(3,1))
labsize=1.8
vis.gam(mbin,view=c('lon','lat'), plot.type="contour",color="topo",too.far=.03,main='',cex.lab = labsize,type="link",xlab="Longitude(degrees)",ylab="Latitude(degrees)" )
plot(coastline, xlim=range(0,5),ylim=range(38.5,40), border="blue",col="gray",add=TRUE)
# plot(mbin,select=1,shade=T,scale=0,rug=T,res=F,cex.lab=labsize, cex.axis=labsize, xlab="Day of year",ylab="Partial effect")
plot(mbin, select=2, shade=T, scale=0, rug=T, res=F, cex.lab=labsize, cex.axis=labsize, xlab="Day of year", ylab="Partial effect")
plot(mbin, select=3, shade=T, scale=0, rug=T, res=F, cex.lab=labsize, cex.axis=labsize, xlab="Salinity anomaly", ylab="Partial effect")
dev.off()

# png(paste(modpath, faocode, "_", ModelID, "_abund_", outdate, ".png", sep=""), width=480, height=480*1.5)
par(mfrow=c(3,1))
labsize=1.8
# vis.gam(mab,view=c('lon','lat'), plot.type="contour",color="topo",too.far=.03,main='',cex.lab = labsize,type="link",xlab="Longitude(degrees)",ylab="Latitude(degrees)" )
# plot(coastline, xlim=range(0,5),ylim=range(38.5,40), border="blue",col="gray",add=TRUE)
plot(mab, select=1, shade=T, scale=0, rug=T, res=F, cex.lab=labsize, cex.axis=labsize, xlab="Day of year", ylab="Partial effect")
plot(mab, select=2, shade=T, scale=0, rug=T, res=F, cex.lab=labsize, cex.axis=labsize, xlab="Salinity anomaly", ylab="Partial effect")
plot(mab, select=3, shade=T, scale=0, rug=T, res=F, cex.lab=labsize, cex.axis=labsize, xlab="Residual temperature", ylab="Partial effect")
dev.off()

# Plot GAM smoothed terms
# plot.gam_smooth <- function(model, x, y, geo.int=TRUE, labsize=1.25, coastline){
#  if(geo.int==TRUE){
#    vis.gam(model, view=c(x, y), plot.type="contour", color="topo", too.far=0.03, cex.lab=labsize, type="link", xlab="Longitude(degrees)",ylab="Latitude(degrees)")
#    plot(coastline, xlim=range(0,5), ylim=range(38.5,40), border="blue", col="gray", add=TRUE)
#  }
#  if(geo.int==FALSE){
#    vis.gam(model, view=c(x, y), plot.type="contour", color="heat", too.far=0.03, cex.lab=labsize, type="link", xlab=x, ylab=y)
#  }
#}
 
#-----------------------
# Model performance
#-----------------------

tdef$fittedbin <- predict(mbin, type = 'response')
require(ggplot2)
ggplot(tdef, aes(fittedbin, fill = as.factor(lpres))) + geom_density(alpha = 0.2)

require(pROC); plot.roc(lpres ~ fittedbin, data = tdef,print.auc=TRUE)

#************************************************
# 5. Estimate larval index estimate (least-squares means)
#    Combining the two models
#************************************************
# require("lsmeans")  
# el cálculo sale idéntico tanto llamando a "lsmeans" como llamando a "emmeans"
ABgsvar
index_table <- index.table(mab=mab, mbin=mbin, tdef=tdef, tabs=tabs, 
  year.var="year", stand.abund=ABgsvar,  
  ci.method="non-normal", var.method="dependent", 
  two.stage=TRUE, error.method="emmeans")

index_table
summarytable

## OBSERVACIONES:
## meanCPUA (en summarytable) y nominalCPUA (en index_table) son lo mismo.
## ¿Estandarizar los nombres?

## Ojo! La tabla de salida 'index_table' es igual a la que se calculaba manualmente según 
## script de Diego,
## excepto que en 'index_table' el nominal CPUA y sus lower and upper CI están multiplicados por 10, 
## ya que, para hacer los modelos es esta variable la que se usa y, por tanto, 
## la que le indicamos en la función como variable de abundancia estandarizada (stand.abund).
## en cambio, en 'summarytable' tienen los valores originales ¿cómo merece más la pena guardarlo?


#******************************************************************************#
# 6. Save Outputs                                                              #
#******************************************************************************#
# 6.1) Nominal CPUA:                                                           #
#     6.1.1) 'summarytable' as .csv and .xlsx                                  #
#     6.1.2) Plot of meanCPUA plot with CI (.png)                              #
# 6.2) Model outputs:                                                          #
#    6.2.1) Model effects and performance of mbin and mab (.pdfs and/or .png)  #
#    6.2.2) Data and models used in the modelling (.RData)                     #
# 6.3) Estimated larval index                                                  #
#    6.3.1) 'index_table' as .csv and .xlsx                                    #
#    6.3.2) Plot of the larval index with CI (.png)                            #
#******************************************************************************#

# Create a folder that would contain model outputs
modpathid <- paste(modpath, faocode, "_", modelID, "/", sep=""); modpathid 
if(!dir.exists(modpathid)) dir.create(modpathid)


#-----------------------------------------------#
# 6.1. Nominal CPUA (meanCPUA)                  #
#-----------------------------------------------#
# check/retrieve required parameters
outdate
faocode
modelID
summarytable
ci <- "Ingram"; ci
tspan <- paste(min(summarytable$year), 
  max(summarytable$year), sep="-"); tspan # Time span of the dataset

# 6.1.1. Save 'summarytable'
fout1 <- paste(outdate, "summaryCPUAyear_ci", ci, "_", 
  faocode, "_", tspan, "_", modelID, sep=""); fout1  # Output file name
save.csvxlsx(summarytable, path=modpathid, file_out=fout1)

# 6.1.2. Plot to .png
fout2 <- paste(outdate, "_meanCPUA_ci", ci, "_", 
  faocode, "_", tspan, "_", modelID, sep=""); fout2 # Output file name
png(paste(figpath, fout2, ".png", sep=""))
  with(summarytable,
    plot.larvind(year, meanCPUA, LCI=LCInn, UCI=UCInn, my_ylab="Mean CPUA (2mm)+-CI")
  )
dev.off()  # The plot has already been saved in previous scripts.
# So here it is not necessary to save the plot again.
# Or we could do the same as with the "summaryCPUAyear"

#-----------------------------------------------#
# 6.2. Model outputs (for final models)         #
#-----------------------------------------------#
output <- saveModel_outputs (mab=mab, mbin=mbin, tdef=tdef,
    modelID=modelID, outdate=outdate, faocode=faocode, 
    pdfplots=TRUE, pngplots=TRUE, modelpathid)
    
# Outputs:                                                                   
# 1) Plots of model effects and performance (.pdf and/or .png)               
# 2) Data used for modelling and final models (.RData)                       
# 3) Summary of the models (.txt)                                            
# 4) Information regarding the models (.html and .xlsx)                      

#-----------------------------------------------#
# 6.3. Larval index                             #
#-----------------------------------------------#
outdate
faocode
modelID
index_table
ci

# 6.3.1. Save 'index_table' as .csv and .xlsx
index_table
faocode
modelID
fout3 <- paste(outdate, "_larval_index_outtable_", faocode, 
  "_", max(index_table$year), "_v07_v03_", modelID, sep=""); fout3
save.csvxlsx(index_table, path=modpathid, file_out=fout3)

# 6.3.2. Plot larval index

# Plot binomial part
fout4 <- paste(faocode, "_prob_presence_", modelID, "_lsmeans", sep=""); fout4
png(paste(modpathid, fout4, ".png", sep=""))
  with (index_table, plot(year, ppos, pch=19, xlab="Year"))
  with (index_table, lines(year, ppos))
dev.off()

# Plot abundance part
fout5 <- paste(faocode, "_cpue_", modelID, "_lsmeans", sep=""); fout5
png(paste(modpathid, fout5, ".png", sep=""))
  with (index_table, plot(year,cpue, pch=19, xlab="Year"))
  with (index_table, lines(year,cpue))
dev.off()

# Larval index
fout6 <- paste(outdate, "_larval_index_", 
  faocode, "_", modelID, "ci", ci, sep=""); fout6
png(paste(modpathid, fout6, ".png", sep=""))
  with(index_table, 
    plot.larvind(year, index, LCInn, UCInn, xaxis="vertical", my_ylab="Index")
  )
dev.off()

#-----------------------------------------------
# Save log file with settings 
#-----------------------------------------------
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
print(paste("ModelID : ", modelID))
cat("\n")
sink()

############ End of script!!! ##################################################