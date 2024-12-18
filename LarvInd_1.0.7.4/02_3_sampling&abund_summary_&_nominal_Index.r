#------------------------------------------------------------------------------#
# Summary of samples, abundance and nominal Index plots                        #
#                                                                              #
# Script to perform several statistics on samples, abundance and index plots   #
#                                                                              #
# Analysis performed:                                                          #
# 1) Sample summary                                                            #
# 2) Plot larval indicators                                                    #
# 3) Compute & save nominal CPUA                                               #
# 4) Compute & Plot CPUA  with SE                                              #
# 5) Boxplot Mean positive abundance                                           #
#                                                                              #
# Project: BLUEFIN, PANDORA                                                    #
# Author: D. Alvarez, M.P. Tugores                                             #
#                                                                              #
# Last modified: 2021/11/30                                                    #
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
# Other                                                                        #
# (4) fanalisis:                                                               #
#     "outtables/BFT/20211115_t_abundance_larvind_BFT_2mm_m2_mixgears.csv"     #
#                                                                              #
#**********                                                                    #
# Outputs:                                                                     #
#**********                                                                    #
# 1) Sample summary (.csv and .xlsx??)                                         #
#                                                                              #
# 2) Plots of the larval indicators (.tiff?; .png??)                           #
#                                                                              #
# 3) Nominal CPUA saved as... ¿¿??                                             #
#                                                                              #
# 4) Plot CPUA with SE (Console? File?)                                        #
#                                                                              #
# 5) Boxplot fo the mean positive abundance (.tiff?; .png??)                   #
#                                                                              #
# - png: “20200728_suma_nlarvae_original_lengths.png”                          #
#        “20200728_mean density per year nlarvae original per m3.png”          #
# B. Summary table of annual CPUA (summarytable)                               #
#    which can be saved as .csv and .xlsx or not                               #
# - csv:  "20200727_v07_08_nominal_index_BFT.csv"                              #
# - xlxs: "20200727_v07_08_nominal_index_BFT.xlsx"                             #
#------------------------------------------------------------------------------#

rm(list=ls())

#############
# Inputs
#############
source("01_0_set_input_files.R"); species; fprocess
source("Rsource/settings_m3.R"); faocode; sp  # Load functions, relative paths and settings

# File for analysis
fanalisis <- paste(outdate, "_t_abu_larvind_BFT_2mm_m2_mixgears.csv", sep="")
fanalisis <- paste(outdate, "_t_abu_larvind_ALB_2mm_m2_mixgears.csv", sep="")
fcurr <- "02_3_sampling&abund_summary_&_nominal_Index.r" # Current script name

# Check the introduced fretro file is for the species being processed
if(length(grep(faocode, fanalisis))==0) stop("Check the input file. It may not be of the species being processed!!")
if(length(grep(faocode, fanalisis))==1) print("Ok!")

# Check existence of required files
files2check <- c(paste(sppath, fanalisis, sep=""))
filecheck(files2check)


################################################################################
#                            PROCESSING                                        #
################################################################################

##################################
# Open table with abundances at 2mm (T2MM)
tlindex <- read.csv3(paste(sppath, fanalisis, sep=""), na.strings="NA",header=T)
ls()
str(tlindex)

# revisar como se hacen los Na
# tlindex[tlindex==-99]=NA # Ahora mismo, los NAs vienen chequeados de antes 
# y sólo hay -99 en la bottom_dep (de batimetría de la NOAA)
# tlindex[tlindex==-999]=NA

### samples by year
nsamples <- tapply(tlindex$id_netcolector,tlindex$year,length)
nsamples

### years in the data.frame
years <- levels(as.factor(tlindex$year))
years

### larval original density (N larvae/m3)
# If the variable does not exist in the input table, create it
if(!any(names(tlindex)=="nlarvae_m3")) tlindex$nlarvae_m3 <- tlindex[,sp]/tlindex$volume

#**************************************
# 1. Summary of the samples (table)
#**************************************

tout <- summarySAMPLING (tab=tlindex, year.var="year", 
  haul.date="fecha_estacion_llegada", abundance=sp, towdepth="towdepth", vol="volume"); tout
  # Function "summarySAMPLING" parameters
  # tab: data.frame containing data
  # year.var: name of the variable containing year
  # haul.date: name of the variable containing the date in which the haul was performed
  # abundance (="nlarvae"): name of the variable containing raw larval count
  # towdepth (m): depth of the tow
  # vol (=volume): name of the variable containing the filtered volume

# Plot ycpua: N larvae * towdepth/volume
plot(tout$year, tout$ycpua)

# Save 'summarytable'
fout0 <- paste(outdate, faocode, "summarySamplings", 
  paste(min(tout$year), max(tout$year), sep="-"), sep="_"); fout0

save.csvxlsx(tout, path=sppath, file_out=fout0)


#**************************************
# 2. Plot larval indicators
#**************************************

sp # "Thunnus_thynnus"
abgsvar #"bftab_gs"
#savePlots

vars2plot <- c(sp, "nlarvae_m3", abgsvar, abgsvar)  # Variable to plot
funs <- c("sum", "mean", "sum", "mean") # Function to be applied
nams <- c("Larval counts (N larvae)", "Mean_larvae_original_m3", 
  "Sum_CPUEnominal_nlarv2mm_m2", "Mean_CPUEnominal_nlarv2mm_m2") # File name
ylabs <- c("Larval counts (N larvae)", "Mean larvae original lengths (N larvae/m3)",
  "Sum larvae at 2mm/m2", "Mean larvae at 2mm/m2") # ylabs for the plots   
ci <- FALSE
ci.method <- NULL

# Plots por pantalla
plot.indices(tdata=tlindex, xvar="year", yvars=vars2plot, fun=funs, ylabs=ylabs) 

# Plots to a .png
plot.indices(tdata=tlindex, xvar="year", yvars=vars2plot, fun=funs, ylabs=ylabs, 
  savePlots=TRUE, figpath=figpath, ci=FALSE, nams=nams, saveID=NULL, outdate=outdate)   

# Se podría hacer como en anteriores scripts que se pueden combinar los dos

# Sólo larval index 1
plot.indices(tdata=tlindex[tlindex$larval_index==1,], xvar="year", yvars=vars2plot, fun=funs, ylabs=ylabs, 
  savePlots=TRUE, figpath=figpath, ci=FALSE, nams=nams, saveID="onlyLI_1", outdate=outdate)   

#**************************************
# 3. Compute & save nominal CPUA
#**************************************
summarytable <- summaryCPUAyear (tab=tlindex, year.var="year", date.var="date"
  ,abundance=sp, abundance2mm="n2mm", stand.abund=abgsvar 
  ,ci.method="non-normal"); summarytable
# Function "summaryCPUAyear" parameters
# tab: data.frame containing data
# year.var: name of the variable containing year
# abundance (="nlarvae"): name of the variable containing raw larval count
# abundance2mm (="n2mm"): N larvae standardised to larvae of 2 mm; 
#                         it can be ommitted if sum and mean n2mm are not to be computed
# stand.abund: name of the variable containing the standardised abundance (typically
#              standardised to gear and 2mm)
# ci.method: method to compute confidence intervals for the nominal index
#            available methods "Ingram_et_al_2010" or "Normal" 
# check/retrieve required parameters

outdate
faocode
ci <- "Ingram"; ci
tspan <- paste(min(summarytable$year), 
  max(summarytable$year), sep="-"); tspan # Time span of the dataset

# Save 'summarytable'
fout1 <- paste(outdate, "summaryCPUAyear_ci", ci, "_", 
  faocode, "_", tspan, sep=""); fout1  # Output file name
save.csvxlsx(summarytable, path=sppath, file_out=fout1)

#**************************************
# 4. Compute & Plot CPUA  with SE
#**************************************
abgsvar
faocode

vars2plot <- c(abgsvar) # Aquí hauria de dir meanCPUA
funs <- c("mean")
nams <- c("meanCPUA_2mm_m2") 
ylabs <- c("Mean CPUA (2mm)+-CI") # ylabs for plots
ci <- TRUE
ci.method <- "Ingram_etal_2010" 
 
plot.indices(tdata=tlindex, xvar="year", yvars=vars2plot, fun=funs, ylabs=ylabs, 
  ci=ci, ci.method=ci.method) 

# Plots in a .png
plot.indices(tdata=tlindex, xvar="year", yvars=vars2plot, fun=funs, ylabs=ylabs, 
  ci=ci, ci.method=ci.method, 
  savePlots=TRUE, figpath=figpath, nams=nams, saveID=NULL, outdate=outdate)       

# Only larval index 1
plot.indices(tdata=tlindex[tlindex$larval_index==1,], xvar="year", yvars=vars2plot, fun=funs, ylabs=ylabs, 
  ci=ci, ci.method=ci.method, 
  savePlots=TRUE, figpath=figpath, nams=nams, saveID="onlyLI_1", outdate=outdate)  
  
#**************************************
# 5. Compute Mean positive abundance
#**************************************
tlindex$nlarvae_100m3 <- tlindex$nlarvae_m3*100

# E2.- Mean positive abundance
tiff(paste(figpath,"boxplot_Abundance_", faocode, "_mean_pos.tiff",sep=""), 
  # height=26, width=13*1.5, units = "cm",res=150)
  height=13*1.5, width=13*1.5, units="cm", res=150)
  par(mar=c(7,7,3,3))
  boxplot(nlarvae_100m3~year, data=tlindex[,sp>0], xlab="Year", ylab="N larvae / m3", 
    cex.lab=2, cex.axis=1.5, cex.names=1.5, cex.main=2)
dev.off()

# E3.- Mean positive abundance without outliers 
cut=1000
t2 = tlindex[tlindex[,sp]>0 & tlindex[,sp]<cut,] # database without outliers
tiff(paste(figpath,"boxplot_Abundance_", faocode, "_mean_pos_no_outlayers.tiff",sep=""), 
  # height=26, width=13*1.5, units="cm", res=150)
  height=13*1.5, width=13*1.5, units="cm", res=150)
  par(mar=c(7,7,3,3))
  boxplot(nlarvae_100m3~year, data=t2, xlab="Year", ylab="N larvae / m3", 
    main=paste("abundances <",cut,sep=""), 
    cex.lab=2, cex.axis=1.5, cex.names=1.5, cex.main=2)
dev.off()

# Summary
sp
length(tlindex[,sp]) # nº registros Thunnus thynnus
length(tlindex[sp][tlindex[sp]>0]) # nº registros positivos
length(tlindex[sp][tlindex[sp]<cut]) # nº registros menores que 'cut'

#------------------------------------
# Save log file with settings 
#------------------------------------
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
#--------------------- End of script!!! ---------------------------------------#