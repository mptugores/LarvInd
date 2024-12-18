#------------------------------------------------------------------------------#
# Process exponential retrocalculation                                         #
#   to standardise the different length data into 2 mm                         #
#                                                                              #
# Analysis/steps performed:                                                    #
# 1) Check the amount of larvae < 1.5 mm and set to 1.6 [17/02/2022]           #
# 2) Plot an histogram (R Console)                                             #
# 3) Save excel with aggregated abundance by length-frequency                  #
# 4) Load/fit an exponential decay model (for length retrocalculation)         #
# 5) Plot length data and fitted/loaded retrocalc model                        #
# 6) Compute mean length per year  [may remain in the next script: 17/02/2022] #
#                                                                              #
# LO SIGUIENTE PONÍA EL SCRIPT DE DIEGO PERO NO VEO DÓNDE LO HAGA....          #
# 1- Compute the exponential curve from the histogram                          # 
#    (2 gears and only from 2 mm,                                              #
#    making <2mm=2mm asuming same age of the very small ones)                  #
# 2- Calculate the 2 mm for each length register                               #
# 3- Summarize the total caulculated at 2 mm for each colector                 #
#                                                                              #
#                                                                              #
# Project: BLUEFIN, PANDORA                                                    #
# Author: D. Alvarez-Berastegui, M.P. Tugores                                  #
#                                                                              #
# Last modified:                                                               #
#   - computation: 2017/03/16                                                  #
#   - script: 17/02/2022                                                       #
#------------------------------------------------------------------------------#
# Hist:                                                                        #
# 20170316: cambia la forma de calular la tabla de frecuencias, no usamos "rep"#
# 20220217: para el 2º histograma que sacamos en el paso 5) sí usamos "rep"    #
#           sinó estaba mal hecho                                              #
#                                                                              #
# 20161104                                                                     #
# diego alvarez                                                                #
# open length distributin table and compute the 2mm larvae table               #
################################ otros posibles modelos                        #
# 1: 0.00022 w e^-0.85                                                         #
# 2: 0.5 cte                                                                   #
###############################                                                #
#                                                                              #
# posibles soluciones a probar para el incremento excesivo                     #
# 1- filtrar las tallas de b90 maximas al maximo de B60 (8.5mm), dando ese     #
#    valor a tallas mayores                                                    #
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
# (3) "outtables/20211115_t_length_larvind_BFT.csv"                            #
# (4) "data/processfiles/00_0_process_specifs_BFT_ALB_SKJ_2021-11-15.RData"    #
#                                                                              #
# Other                                                                        #
# (5) fretro: retrocalculation model previously estimated                      #
#     "20180917_exponential_retrocalc_mod_allgears_BFT.RData"                  #
#                                                                              #
#                                                                              #
#**********                                                                    #
# Outputs:                                                                     #
#**********                                                                    #
# 1) Plots of the length-frequency and fitted exponential model                #
#    - tiffs:                                                                  #
#      “20200728_length_hist_&_exponenetial_model allgears from 2017 process.tiff” #
#      “20200728_length_hist_&_exponenetial_model allgears.tiff”               #
#                                                                              #
# 2) Values for the exponential curve                                          #
#    - xlsx:  “20200727_values_for_exponential_curve_allgears.xlsx”            #
#                                                                              #
# 3) File containing the mean length for each year                             #
#    - RData: "20200727_tmeanlengthsyear.RData"                                #
#------------------------------------------------------------------------------#

# clean objects
rm(list=ls())

#############
# Inputs
#############

#----------------
# Exponential retrocalculation model
#----------------
# IMPORTANT: for STRICT UPDATE: 
# use the lattest fit fretro model!!!


# For actualisations, you can set fretro NULL and fit a new retrocalculation model
fretro <- NULL  # Set to NULL if a new exponential model is to be computed

# For strict update use a previously fitted fretro model 
fretro <- "20180917_exponential_retrocalc_mod_allgears_BFT.RData"  # To use an old existing model from
fretro <- "20180917_exponential_retrocalc_mod_allgears_ALB.RData"  # To use an old existing model from

#----------------
# Current script name
#----------------
fcurr <- "02_1_process_exponential_retrocalc.r"

############################# WARNING ##########################################
#                     DON'T CHANGE the rest of the code !!!                    #
################################################################################

source("01_0_set_input_files.R"); species; fprocess; flen
source("Rsource/settings_m3.R"); sp # Load functions, relative paths and settings

# Check the introduced fretro file is for the species being processed
if(length(grep(faocode, fretro))==0) stop("Check the input file. It may not be of the species being processed!!")
if(length(grep(faocode, fretro))==1) print("Ok!")

# Check existence of required files
if (!is.null(fretro)) {
  files2check <- c(paste(mypath, fretro, sep=""))
  filecheck(files2check)
}

#-----------------------------------------------
# 0. Read length-frequency table from .csv file
#-----------------------------------------------
tt0 <- read.csv3(paste(opath, flen, sep=""))
names(tt0)
str(tt0)
nrow(tt0)

# Filter by species
if(length(levels(tt0$spp_name))>1) {
  tt <- tt0[tt0$spp_name==gsub(" ", "_", species),]; nrow(tt)
} # Currently not required (filtered beforehand)

if (length(levels(tt0$spp_name))==1) tt <- tt0

# Remove non-vector variables to be able to edit
tt$horallegada=NULL
tt$fecha_estacion_llegada=NULL
names(tt)
summary(tt)# comprobar que no hay NA ni -999

# Check NAs
# if some value in the data.frame is NA (-99, -999, -9999 or NA)
checkNAs(tt) 

# What would we do if there were NAs?????

#--------------------------------------------
# 1. Check N larvae with length < 1.5 mm & set to 1.5
#--------------------------------------------
# Check the amount of larvae with a length < 1.5 mm
tt[tt$length<1.5,] # Larvas de < 1.5 mm [1: 2005, 1: 2011, 1: 2015, 2: 2016]

length(tt$length[tt$length<1.5])  # Hay 5 larvas de < 1.5; For ALB, no larvae is < 1.5 mm 

# Si son pocas, las ponemos igual a 1.5
# ¿Cuántas son pocas? Ahora mismo para Bluefin hay 8
# tt$length[tt$length<1.5]=1.5  # Set to 1.5 
tt$length[tt$length<1.5]=1.6 
# [17/02/2022] Set to 1.6, para que no queden fuera del análisis
# ¿o mejor los omitimos?

#--------------------------------------------
# 2. Histogram of n larvae by length class
#--------------------------------------------
############# este histograma se genera a partir de los datos de frecuecnia con decimales
############# evita la necesidad de tener que aplicar la función rep de las versiones anteriores que forzaba a ajustar los decimales,
# brks <- seq(from=1.5, to=max(tt$length), by=1)  ## genera breaks e histograma a partir de la tabla de frecs
brks <- seq(from=1.5, to=max(tt$length)+1, by=1) ## +1 to avoid 10.9 larvae be set aside the calculations
lengthclass <- cut(tt$length, brks)
nlarvbyclass <- tapply(tt$frec, lengthclass, sum)

require(gsubfn) 
lt=colMeans(strapply(levels(lengthclass),"([0-9.]+),([0-9.]+)", function(...) as.numeric(c(...)), backref=-2, simplify=TRUE)) # calcula la media de un texto, previa transformacion en un número
texp=data.frame(length=lt, counts=nlarvbyclass)# comprobado calculo

if(any(is.na(texp))) texp=texp[!is.na(texp$counts),]
sum(texp$counts) # total amount of larvae

plot(lt, nlarvbyclass)
barplot(texp$counts, names.arg=texp$length, xlab="length(mm)", ylab="frequency")

# Check all data is used
sum(texp$counts)   
sum(tt$frec) 
sum(texp$counts)-sum(tt$frec)   ## diferencia -2


#---------------------------------------------------------------
# 3. Save excel with aggregated abundance by length-frequency
#    to get curve manually to test
#----------------------------------------------------------------
if(is.null(fretro)) {
  fileout <- paste(outdate, "_values_for_exponential_curve_allgears_", faocode, sep="")
  fileout
   
  save.csvxlsx(texp, path=sppath, file_out=fileout)  # Save as .csv and .xlsx
  
  plot(texp$length, texp$counts)   # in execl it gaves:
}   

#-------------------------------------------------------
# 4. Exponential decay model
#-------------------------------------------------------

# IMPORTANT: for STRICT UPDATE: 
# use the lattest fit fretro model!!!

#-------------------------------------------------------
#   4. a) Load a previously fit retrocalc model (if fretro != NULL)
if(!is.null(fretro)) load(fretro)

#-------------------------------------------------------
#   4. b)  Fit an exponential decay model (if fretro==NULL)
if(is.null(fretro)){
  ################## MODULO MODELO DE RETROCULUALO NO SE USA EN STRICT UPDATES
  # rm(mod)
  mod <- nls(counts ~ a*exp(b * length), data=texp, start = list(a=100000,b=-1))
  # mod
  #
  # a=9183.5756; b= -0.8998    ### Si corremos el modelo, debemos coger a y b del propio modelo, no??
  #
  # Correlation between data and predictions of the fitted model
  print(cor(fitted(mod),texp$counts,method="kendal"))
  # because we can not compute R2 for nls 
  # (http://blog.minitab.com/blog/adventures-in-statistics/why-is-there-no-r-squared-for-nonlinear-regression)
  retrofile=paste(outdate, "_exponential_retrocalc_mod_allgears_", faocode, ".RData", sep="")
  save(mod, file=retrofile)
  ################################################################################
}                           

#-------------------------------------------------------
# 5. Plot data versus the fitted model (or the model in the retro object)
#-------------------------------------------------------

if(!is.null(fretro)) {
  dateretro <- substr(fretro, 1, 8)
  nam1 <- paste(outdate, "_LengthData_&_ExpModel_allgears_", dateretro, "process_", faocode, ".tiff", sep=""); nam1
  nam2 <- paste(outdate, "_LengthHist_&_ExpModel_allgears_", dateretro, "process_", faocode, ".tiff", sep=""); nam2
  main1_2 <- paste("updated data and retrocalc model from ", dateretro, sep="")  
}

if(is.null(fretro)) {
  nam1 <- paste(outdate, "_LengthData_&_ExpModel_allgears_", outdate, "process_", faocode, ".tiff", sep=""); nam1
  nam2 <- paste(outdate, "_LengthHist_&_ExpModel_allgears_", outdate, "process_", faocode, ".tiff", sep=""); nam2
  main1_2 <- "updated data and retrocalc model"
}

# Save .tiff files
# nam1 <- paste(outdate, "_length_hist_&_exponential_model_allgears_from_2017_process_", faocode, ".tiff", sep=""); nam1
fname1 <- paste(figpath, nam1, sep="")

tiff(fname1, width=480*1.1)
  plot(texp$length, texp$counts, xlab="Length (mm)", ylab="Frequency", cex=2, col="red",
    ylim=c(0, max(texp$counts)), main=main1_2)
  points(texp$length, predict(mod, texp), cex=2, pch=20, col="blue")
  myseq <- seq(from=2, to=max(texp$length), length=100)
  lines(myseq, predict(mod, list(length=myseq)), col="blue", lwd=2)
dev.off()


## 17/02/2022 [Pilar]: working ok

# name2 <- paste(outdate, "_length_hist_&_exponential_model_allgears_", faocode, ".tiff", sep=""); name2
fname2 <- paste(figpath, nam2, sep="")

tiff(fname2, width=480*1.1)
  myhist=hist(rep(tt$length, round(tt$frec,0)), breaks=brks, plot=FALSE)
  plot(myhist, main=main1_2, xlab="Length (mm)", ylab="Frequency")
  points(texp$length, texp$counts, cex=2, col="red")
  points(texp$length, predict(mod, texp), cex=2, pch=20, col="blue")
  myseq <- seq(from=2, to=max(texp$length), length=100)
  lines(myseq, predict(mod, list(length=myseq)), col="blue",lwd=2)
dev.off()

#--------------------------------------------
# 6. Compute mean length per year 
#--------------------------------------------
# Aim: estimate lengths in collectors with no data in next script

a <- tt$length*tt$frec
b <- tapply(a, tt$year, function(x){sum(x)})
c <- tapply(tt$frec, tt$year, function(x){sum(x)})
meanlengthyear <- b/c

tmeanlengthsyear <- data.frame(year=as.numeric(names(meanlengthyear)), meanlength=meanlengthyear)
tmeanlengthsyear

# Save as an .RData file
fout2 <- paste(outdate, "_tmeanlengthsyear_", faocode, ".RData", sep="")
fout2
save(tmeanlengthsyear, file = fout2)

#--------------------------------
# Save log file with settings 
#--------------------------------
flogname <- paste("LarvInd_settings_log_", faocode, "_", substring(fabund, 1, 8), "_", Sys.Date(), ".txt", sep="") 
sink(file=paste(flogname), append=TRUE)
print(paste("--- '", fcurr, "' ---"))
print(paste("Current date : ", Sys.Date()))
print(paste("Abundance and length files processed on date : ", substring(fabund, 1,8)))
print(paste("Species: ", species))
print(paste("Abundance file (fabund): ", fabund))
print(paste("Length file (flen): ", flen))
print(paste("Processing file for abundance and length data (fprocess) :", fprocess))
# Save settings and retrocalculation model into the already existing log file
print("------------------------------------------------------------------")
if(!is.null(fretro)) {
  print(paste("Use existing retrocalculation file : ", fretro))
  print(paste(fretro))
  cat("\n")
  load(fretro); print(mod)
}
if(is.null(fretro)) {
  print("The newly generated retrocalculation model will be saved as :")
  print(retrofile)
  cat("\n")
  print(mod)
}
print("------------------------------------------------------------------")
cat("\n")
sink()
### WARNING! If you run the script more than once it will have duplicated information   



#--------------- End  of  script  !!! -----------------------------------------#