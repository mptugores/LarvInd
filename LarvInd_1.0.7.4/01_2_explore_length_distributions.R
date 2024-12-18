 #------------------------------------------------------------------------------#
# Length-frequency data exploration                                            #
#                                                                              #
# Script to check length-frequency data from ichthyoplankton TUNIBAL surveys   #
#                                                                              #
# Analysis performed:                                                          #
# 1) Summary of length data by year                                            #
# 2) Rounding error analysis                                                   #
# 3) Yearly boxplot of length data                                             #
# 4) Length-frequency histograms                                               #
# 5) Length-frequency by fishing gear                                          #
# 6) KS-test to check statistical difference                                   #
# 7) Density plots of length data                                              #
# 8) Annual histograms of length-frequency data                                #
#                                                                              #
# Author: D. Alvarez, M.P. Tugores                                             #
# Project: BLUEFIN, PANDORA                                                    #
#                                                                              #
# Last modified: 14/12/2021                                                    #
#                                                                              #
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
#                                                                              #
#**********                                                                    #
# Outputs:                                                                     #
#**********                                                                    #
# 1) Summary of length-frequency data                                          #
#    - csv: "20211115_BFT_summaryLengths.csv"                                  #
#    - xlsx: "20211115_BFT_summaryLengths.xlsx"                                #
#                                                                              #
# 2) Analysis of rounding in the R console                                     #
#                                                                              #
# 3) Boxplot length-frequency by year                                          #
#    - tiff: "boxplot_lengths_BFT_allgears.tiff"                               #
#                                                                              #
# 4) Histogram of length-frequency data                                        #
#    - tiff: "histt_lengths_BFT_allgears inc1.tiff"                            #
#                                                                              #
# 5) Histogram and histogram by gear                                           #
#    - tiff: "histt_lengths_BFT_B60 inc1.tiff                                  #
#         "histt_lengths_BFT_B90 inc1.tiff                                     #
#                                                                              #
# 6) KS-test of length-frequency data between gears (B60-B90) (in R Console)   #
#                                                                              #
# 7) Density plots of length data (in the R Console)                           #
#                                                                              #
# 8) Annual histograms of length-frequency for all gears                       #
#    - tiff: "20211115_histt_annual_lengths_BFT.tiff"                          #
#                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
# Observaciones:                                                               #
# 1. Formato hora no se lee bien (pero si no se usa...)                        #
# 2. Diego, compara redondeo R con redondeo Access; yo tengo hecha la función  #
#    de redondeo como Access; ¿cuál interesa dejar?                            # 
# 3. Ya no hace falta leer BD Access                                           #
# 4. No haría falta filtrar por especie; ya viene hecho de los scripts de      #
#    generación de la tabla de tallas                                          # 
#                                                                              #
#------------------------------------------------------------------------------#

rm(list=ls())

#############
# Inputs
#############
# Current script name
fcurr <- "01_2_explore_length_distributions.r"

############################# WARNING ##########################################
#                     DON'T CHANGE the rest of the code !!!                    #
################################################################################
source("01_0_set_input_files.R"); species; flen; fprocess

# Load functions, relative paths and settings
source("Rsource/settings_m3.R"); species; faocode

# Check existence of required files
files2check <- c(paste(opath, fabund, sep=""),
  paste(opath, flen, sep=""),
  paste(propath, fprocess, sep=""))

filecheck(files2check)

#---------------------------------------------
#  Read length-frequency table from .csv file
#---------------------------------------------
tt0 <- read.csv3(paste(opath, flen, sep=""))

# Filter by species (in case the file was not already filtered)
# Currently it is filtered beforehand
if(length(levels(tt0$spp_name))>1) {
  tt <- tt0[tt0$spp_name==gsub(" ", "_", species),]; nrow(tt)
}

if(length(levels(tt0$spp_name))==1) tt <- tt0

#******************************************
#  1. Summary of length data by year
#******************************************

# 2 plot length distributions per year for testing
# necesario expandir para pder hacer distribuciones de tallas
# importante redondear al prximo
tt2 <- tt[rep(1:nrow(tt), times=round(tt$frec,digits=0)), ];nrow(tt2) 

names(tt2)
rownames(tt2) <- NULL 
nrow(tt2)
tt2$frec <- 1

summarylengths <- data.frame(year=as.numeric(levels(as.factor(tt$year))), 
  minlength= tapply(tt$length,tt$year, min), 
  maxlength= tapply(tt2$length,tt2$year, max),
  meanlength= round(tapply(tt2$length,tt2$year, mean), 2))# comprobado calculo

if (species=="Thunnus thynnus") {
  perc_len8upper <- tapply(tt$length, tt$year, function(x){round(100*length(x[x>8])/length(x),2)})
  summarylengths <- data.frame(summarylengths, perc_upper8=perc_len8upper)
}

print(summarylengths)

#--------------------------
# Saved to .csv and .xlsx files
#--------------------------
outdate <- substr(flen, 1, 8)
filenam <- paste(outdate, "summaryLengths", faocode, sep="_")
filenam

save.csvxlsx(summarylengths, path=sppath, file_out=filenam)

#******************************************
#  3. Analysis of the rounding error
#******************************************
# Análisis del error al redondear
sum(tt$frec);sum(tt2$frec)
sum(tt$frec)-sum(tt2$frec)
sum(round(tt$frec, 0))-sum(tt2$frec)## para comparar el efecto del redondeo
###################################################################################################################### cont revising here
nrow(tt2)# comprobar que es igual a sum(tt2$frec) y que es igual a la misma operacion en torig2: sum(tt0$frec) y en la base de datos (=67867)
# al hacer la aproximacion en acces me sale 67845 aqui sale 67821
###################################################################################################################### porruq no es igual?
#el problmea es sobre todo por el año 2013 que tiene muchas frecuencias con decimales menores de 0.5
sum(tt$frec[tt$myyear==2013])
sum(round(tt$frec[tt$myyear==2013],digits=0))

# esto deberia afectar solo a los histogramas, no al cálculo del modelo de retrocalculo
### revisa por año
tapply(tt$frec,tt$year,sum)
tapply(tt2$frec,tt2$year,sum)

#******************************************
#  4. Yearly boxplot of length data
#******************************************
# Boxplot de tallas por año
tiff(paste(figpath, outdate,"_boxplot_lengths_", faocode, ".tiff", sep=""), height=20, width=26, units = "cm", res=150)
  par(mar=c(7,7,3,3))
  boxplot(tt2$length~tt2$year, ylab="larval length(mm)", xlab="Year", cex.lab=2, cex.axis=1.5, cex.names=1.5)
dev.off()

# Only for larval index 1
tt2_1 <- tt2[tt2$larval_index==1,]
tiff(paste(figpath, outdate,"_boxplot_lengths_", faocode, "_onlyLI_1.tiff", sep=""), height=20, width=26, units = "cm", res=150)
  par(mar=c(7,7,3,3))
  boxplot(tt2_1$length~tt2_1$year, ylab="larval length(mm)", xlab="Year", cex.lab=2, cex.axis=1.5, cex.names=1.5)
dev.off()


#******************************************
#  5. Length-frenquecy histograms
#******************************************
# Histograma de tallas
# clases=c(0.5,1,2,3,4,5,6,7,8,9,10,11,12)
clases=c(0.5, seq(from=1, to=round(max(tt2$length),0)+1, 1))
min(tt2$length)
max(tt2$length)

hist(tt2$length, xlab="length(mm)", breaks=clases,
  main="histogram larvae lengths (allyears and gears)") # for the whole time series

if(any(tt2$year==2002)){
hist(tt2$length[tt2$year==2002], xlab="length(mm)", breaks=clases,
  main="histogram larvae lengths (2002 and gears)") # for one single year
}

#--------------------------
# Saved to a .tiff file
#-------------------------- 
# Save length-frequency data as .tiff
tiff(paste(figpath, outdate,"_histt_lengths_", faocode, "_allgears.tiff", sep=""), width=480*1.1)
  hist(tt2$length, main="histogram larvae lengths (allyears and gears)", xlab="length(mm)", breaks=clases)
dev.off()

#******************************************
#  6. Length-frenquecy by fishing gear
#******************************************
# Histograma de tallas B60
tiff(paste(figpath, outdate,"_histt_lengths_", faocode, "_B60.tiff", sep=""), width=480*1.1)
hist(tt2$length[tt2$gear=="B60"], 
  main=paste("histogram larvae lengths B60 (", min(tt2$year), "-", max(tt2$year), ")", sep=""),
  xlab="length(mm)",breaks=clases)
dev.off()

# Histograma de tallas B90
tiff(paste(figpath, outdate,"_histt_lengths_", faocode, "_B90.tiff", sep=""), width=480*1.1)
hist(tt2$length[tt2$gear=="B90"], 
  main=paste("histogram larvae lengths B90 (", min(tt2$year), "-", max(tt2$year), ")", sep=""),
  xlab="length(mm)",breaks=clases)
dev.off()  # B90 parece demasiado selectiva hacia tallas intermedias

# Histograma de tallas B60-B90
tiff(paste(figpath, outdate,"_histt_lengths_", faocode, "_B60-B90.tiff", sep=""), width=480*1.1)
par(mfrow=c(1,2))
hist(tt2$length[tt2$gear=="B60"], 
  main=paste("histogram larvae lengths B60 (", min(tt2$year), "-", max(tt2$year), ")", sep=""),
  xlab="length(mm)",breaks=clases)
hist(tt2$length[tt2$gear=="B90"], 
  main=paste("histogram larvae lengths B90 (", min(tt2$year), "-", max(tt2$year), ")", sep=""),
  xlab="length(mm)",breaks=clases)
dev.off()  # B90 parece demasiado selectiva hacia tallas intermedias

#--------------------------------
# Now, only for larval index 1
#--------------------------------
# Histograma de tallas B60
tiff(paste(figpath, outdate,"_histt_lengths_", faocode, "_B60_onlyLI_1.tiff", sep=""), width=480*1.1)
hist(tt2_1$length[tt2_1$gear=="B60"], 
  main=paste("histogram larvae lengths B60 (", min(tt2_1$year), "-", max(tt2_1$year), ")", sep=""),
  xlab="length(mm)",breaks=clases)
dev.off()

# Histograma de tallas B90
tiff(paste(figpath, outdate,"_histt_lengths_", faocode, "_B90_onlyLI_1.tiff", sep=""), width=480*1.1)
hist(tt2_1$length[tt2_1$gear=="B90"], 
  main=paste("histogram larvae lengths B90 (", min(tt2_1$year), "-", max(tt2_1$year), ")", sep=""),
  xlab="length(mm)",breaks=clases)
dev.off()  # B90 parece demasiado selectiva hacia tallas intermedias

# Histograma de tallas B60-B90
tiff(paste(figpath, outdate,"_histt_lengths_", faocode, "_B60-B90_onlyLI_1.tiff", sep=""), width=480*1.1)
par(mfrow=c(1,2))
hist(tt2_1$length[tt2_1$gear=="B60"], 
  main=paste("histogram larvae lengths B60 (", min(tt2_1$year), "-", max(tt2_1$year), ")", sep=""),
  xlab="length(mm)",breaks=clases)
hist(tt2_1$length[tt2_1$gear=="B90"], 
  main=paste("histogram larvae lengths B90 (", min(tt2_1$year), "-", max(tt2_1$year), ")", sep=""),
  xlab="length(mm)",breaks=clases)
dev.off()  # B90 parece demasiado selectiva hacia tallas intermedias


#********************************************
#  7. KS-test to check statistical differene
#********************************************
# Son estadísticamente diferentes?
x <- tt2$length[tt2$gear=="B60"] 
y <- tt2$length[tt2$gear=="B90"]
ks.test(x, y) # Sí, lo son (p:  < 2.2e-16 significativo)

#******************************************
#  8. Density plots of length data
#******************************************
dD <- density(na.omit(tt2$length)) # returns the density data
plot(dD, main="all years") # plots the results 

rm(dD)
dD <- density(na.omit(tt2$length),n=100) # returns the density data
plot(dD, main="tt2") # plots the results 

#************************************************
#  9. Annual histograms of length-frequency data
#************************************************
# Histogramas anuales 
# Falta adaptación para cuando haya más de 15 años
tiff(paste(figpath, outdate,"_histt_annual_lengths_", faocode, ".tiff", sep=""), 
  width=480*2.2, height=480*1.4, res=100)
par(mfrow=c(3,5))
data1 <- tt2
for (i in levels(as.factor(data1$year))){
  tmp <- data1[data1$year==as.numeric(i),]
  ag1 <- aggregate(tmp$frec, by=list(tmp$length), function(x){round(sum(x), 0)})
  list1 <- list()
  for (j in 1:nrow(ag1)){
    list1[[j]] <- rep(ag1$Group.1[j], ag1$x[j])
  }
  output <- do.call(c, list1)
  hist(output, main=i,  xlab="TL(mm)", xlim=range(data1$length))
}
dev.off()

#--------------------------
# Histogramas anuales 
# Only for larval index 1
tiff(paste(figpath, outdate,"_histt_annual_lengths_", faocode, "onlyLI_1.tiff", sep=""), 
  width=480*2.2, height=480*1.4, res=100)
par(mfrow=c(3,5))
data1 <- tt2_1
for (i in levels(as.factor(data1$year))){
  tmp <- data1[data1$year==as.numeric(i),]
  ag1 <- aggregate(tmp$frec, by=list(tmp$length), function(x){round(sum(x), 0)})
  list1 <- list()
  for (j in 1:nrow(ag1)){
    list1[[j]] <- rep(ag1$Group.1[j], ag1$x[j])
  }
  output <- do.call(c, list1)
  hist(output, main=i,  xlab="TL(mm)", xlim=range(data1$length))
}
dev.off()


#---------------------------------------------
# Save log file with settings 
#---------------------------------------------
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

#-------------------------------- End script!!! -------------------------------#
