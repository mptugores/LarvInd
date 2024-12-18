#------------------------------------------------------------------------------#
# Script to calculate Nlarvae of 2 mm (all gears)                              #
#                                                                              #
# 1. open length distributin tabla from data species specific data base        #
# 2. open exponential model for backcalcuation                                 #
# 3. compute the abundance at reference length specified at sample             #
#    (only one combined decay curve)                                           #
# 4. open table t anlaisis larval index all vars                               #
# 5. paste tanalisis and ta baundnaces at 2mm                                  #
# 6. complete n2mm in samples with larvae but no lengths measured              #
# 7. compute variables density and abundance                                   #
# 8. compute varaible abundance(gear standardized)                             #
#    with the model published in iccat                                         #
# 9. export table _t_abs_2mm_m2_all_vars_mixedgears.csv                        #
#                                                                              #
# Project: BLUEFIN, PANDORA                                                    #
# Author: D. Alvarez, M.P. Tugores                                             #
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
# (3) "outtables/20211115_t_length_larvind_BFT.csv"                            #
# (4) "data/processfiles/00_0_process_specifs_BFT_ALB_SKJ_2021-11-15.RData"    #
#                                                                              #
# Other                                                                        #
# (5) fretro: "20180917_exponential_retrocalc_model_allgears_BFT.RData"        #
# (6) fmeanlength: "20211115_tmeanlengthsyear_BFT.RData"                       #
#                                                                              #
#                                                                              #
#**********                                                                    #
# Outputs:                                                                     #
#**********                                                                    #
# Saved in the directory (sppath) "outtables/BFT/"                             #
# 1) Length-frequency file with a variable of n larvae standardised to 2 mm    #
#    - csv:  "20200727_t_len_BFT_n2mm.csv"                                     #
#    - xlsx: "20200727_t_len_BFT_n2mm.xlsx"                                    #
#                                                                              #
# 2) Abundance file with an abundance variable standardised to 2 mm and        #
#    standardised between B60 and B90                                          #
#    - "20211115_t_abu_larvind_BFT_2mm_m2_mixgears.csv"                        #
#    - "20211115_t_abu_larvind_BFT_2mm_m2_mixgears.xlsx"                       #
#------------------------------------------------------------------------------#

# clean objects
rm(list=ls())

#############
# Inputs
#############
source("01_0_set_input_files.R"); species; flen; fprocess
source("Rsource/settings_m3.R") # Load functions, relative paths and settings

fretro <- "20180917_exponential_retrocalc_mod_allgears_BFT.RData"  ### USAMOOS EL MODELO ANTIGUO
fretro <- "20180917_exponential_retrocalc_mod_allgears_ALB.RData"  ### USAMOOS EL MODELO ANTIGUO
# fretro <- NULL # Set to NULL, if interested in computing a new retrocalculation model

fcurr <- "02_2_abund_standardisation_2mm_&_gears.r" # Current script name
fmeanlength <- paste(outdate, "_tmeanlengthsyear_BFT.RData", sep="") # Mean length for each year
fmeanlength <- paste(outdate, "_tmeanlengthsyear_ALB.RData", sep="") # Mean length for each year

# Check the introduced fretro file is for the species being processed
if(length(grep(faocode, fretro))==0) stop("Check the input file. It may not be of the species being processed!!")
if(length(grep(faocode, fretro))==1) print("Ok!")

if(length(grep(faocode, fmeanlength))==0) stop("Check the input file. It may not be of the species being processed!!")
if(length(grep(faocode, fmeanlength))==1) print("Ok!")


# Check existence of required files
files2check <- c(paste(mypath, fretro, sep=""),
  paste(mypath, fmeanlength, sep=""),
  paste(opath, flen, sep=""),
  paste(propath, fprocess, sep=""))

filecheck(files2check)

################################################################################
#                            PROCESSING                                        #
################################################################################

#*************************************************
# 1. Read length-frequency and abundance tables from .csv file
#*************************************************
# flen <- "length.frec_abundance_342_Thunnus thynnus_2020-10-23.csv"
tt0 <- read.csv3(paste(opath, flen, sep=""))
sp

nrow(tt0)
names(tt0)
str(tt0)
 
#******************************************
# 2. Filter the species of interest
#******************************************
tt <- tt0[tt0$spp_name==sp,]; nrow(tt)  # Currently (29/10/2021) not necessary

#****************************************************
# 3. Remove non vector varaibles to be able to edit
#****************************************************
# ¿Para qué queremos editar?
tt$to_horallegada=NULL
tt$to_fecha_estacion_llegada=NULL
names(tt)
summary(tt$length) # comprobar que no hay NA ni -999 en lengths t numero

## revise
# edit(tt)

#********************************************
# 4. Load exponential retrocalculation model
#********************************************
# load(file="20180917_exponenetial_retrocalculation_model_allgears_BFT.RData")
load(fretro)
ls()

#**************************************************************************
# 5. Compute the abundance at reference length (number of larvae of 2 mm)
#**************************************************************************
# compute number of larvae at 2mm for each length row in tt
# ratio is the number of larvae at 2mm/nuber of larvae at each length

referencelength=2# length at which we retrocalualte 
# compute n larvae at reference length for B90

# comparativa de modelos en D:\AAD\codigos\R_codes\bluefin\larval_index\diegolarvalindex\20170131\BFT_glmbased\outputtables
tt$n2mm <- round(tt$frec*((exp(coef(mod)[2]*referencelength))/(exp(coef(mod)[2]*tt$length))),digits=1)

referencelength2=3.5
tt$n35mm=round(tt$frec*((exp(coef(mod)[2]*referencelength2))/(exp(coef(mod)[2]*tt$length))),digits=1)

#----------------
# Check results
#----------------
#test, veo a que talla es la mitad del n larvas a 2mm ( mas o menos es 3)
#la decima parte a 4 mm (viendo la tabla original a partir de la cual se genera el modelo
#exp(coef(mod)[2]*referencelength)/exp(coef(mod)[2]*3)
#exp(coef(mod)[2]*referencelength)/exp(coef(mod)[2]*4)

# revisa resultado
plot(tt$n2mm, tt$frec)
text(tt$n2mm, tt$frec,tt$net_colector)

# revisa resultado
plot(tt$n2mm, tt$frec)
text(tt$n2mm, tt$frec,tt$year)

# revisa resultado
plot(tt$n2mm, tt$frec)
text(tt$n2mm, tt$frec,tt$length)

# quizas habria  que filtar larvas de un tamaño maximo (ver hist abajo) o quizas solo sacarla del modelo
#.... veo dispersion de una larva de 10.9 del colector 5714...  # Yo no lo veo
hist(tt$length, nclas=20)

# Check 
# edit(tt)

#-------------------------------
# Save length table with n larvae at 2 mm 
#     as a .csv and .xlxs
#-------------------------------
# to be able to check if necessary
fileout <- paste(outdate, "_t_length_larvind_", faocode, "_n2mm", sep="")
fileout
# "20211115_t_length_larvind_BFT_n2mm"

save.csvxlsx(tt, path=sppath, file_out=fileout)

#******************************************
# ESTO NO SÉ QUÉ HACE
#******************************************

# HAZ LA TABLA DE SALIDA AGRUPANDO NLARVA PARA CADA COLECTOR

# tt$colectorid=tt$tc_id_colector_autonum### 
tt$colectorid=tt$net_colector### 

#agrupa el numero de larvas a 2mm  (n2mm) en cada colector
# tot2mm=aggregate(tt$n2mm, by=list(Category=tt$tc_id_colector_autonum), FUN=sum);colnames(tot2mm)=c("tc_id_colector_autonum","n2mm");head(tot2mm)
tot2mm <- aggregate(n2mm~colectorid, data=tt, sum); head(tot2mm)

# Confirm no duplicates in 'colectorid'
# duplicated(tot2mm$tc_id_colector_autonum)
# duplicated (tot2mm$colectorid)
check.duplicates(datf="tot2mm", vari="colectorid")

# agrupamos tambien para "frec": la agrupacion de frec servira para comprobar luego al pegar a la tabla de abundancias
# totlarva <- aggregate(tt$frec, by=list(Category=tt$tc_id_colector_autonum), FUN=sum);colnames(totlarva)=c("tc_id_colector_autonum","testnlarv");head(totlarva)# para comprobar que es = a nlarva
totlarva <- aggregate(tt$frec, by=list(tt$colectorid), sum); colnames(totlarva)=c("colectorid","testnlarv");head(totlarva)# para comprobar que es = a nlarva

# Confirm no duplicates in 'colectorid'
check.duplicates(datf="totlarva", vari="colectorid")


# pegar las dos tablas 
#t2mm=merge(tot2y3mm,totlarva,by='tc_id_colector_autonum', all.x = TRUE)
t2mm <- merge(tot2mm, totlarva, by='colectorid', all.x = TRUE); head(t2mm)

## aqui faltaria pegar las tallas estandarizadas a 3.5 si queremos tambien-
##################
plot(t2mm$testnlarv, t2mm$n2mm)


# ahora pega la tabla de abundancias a 2mm a la de colectores para generar la tabla final de anlisis.

#******************************************
# 6. Read larval index all vars table
#******************************************

# ######  open table colectors
fabund # "20211027_t_analisis_larvind_BFT_abs.csv"

tcols0 <- read.csv3(paste(opath, fabund, sep=""), na="NA") 
summary(tcols0)
names(tcols0)
nrow(tcols0)

# test no colectors duplication
# which(duplicated(tcols0$id_netcolector)=="TRUE")# confirm no duplicates in colectors
check.duplicates("tcols0", "id_netcolector")


tcols=tcols0
nrow(tcols)
names(tcols)

#*********************************************
# 7. Paste tanalisis and tabundances at 2 mm
#*********************************************
### pega las dos tablas
#tlindex0=merge(tcols,t2mm,by.x='colector_id',by.y="colectorid", all.x = TRUE)
tlindex0 <- merge(tcols, t2mm, by.x='id_netcolector', by.y='colectorid', all.x=TRUE)####### modificado 20200714 para que coincidan nombres de campos
names(tlindex0)
head(tlindex0)


# fix(tlindex0)
# edit(tlindex0)            


#test pasting
nrow(tcols); nrow(tlindex0)
nrow(t2mm);nrow(tlindex0[which(!is.na(tlindex0$n2mm)),])# must be same as nrow(t2mm)
if(nrow(t2mm)!= nrow(tlindex0[which(!is.na(tlindex0$n2mm)),])) { 
  t2mm$TF <- t2mm$colectorid %in% tlindex0$id_netcolector
  print(t2mm [t2mm$TF==FALSE,])
}


names(tlindex0)
plot(tlindex0[,sp],tlindex0$testnlarv)###### debe ser una linea recta si n tunus en abundancias es igual a suma de frec
tlindex0$testnlarv=NULL # lo eliminamos despues de la comprobacion

### aqui mejor no removerlos si no rellenarlos pero eso hay que progrmarlos en funcin de la talla media antes

#*****************************************************************
# 8. Complete n2mm in samples with larvae but no length measured
#*****************************************************************

#****************************
# 8.1) For gap years, with absolutelly no length data we'll use the mean of the whole
# data series [BFT: only 2006 and 2008]

#--------------
# Global mean
#--------------
tab <- tt

a=tab$length*tab$frec
b=sum(a)
c=sum(tab$frec)
globalmean = b/c   # media de la serie histórica

globalmean

# Add mean length for all years without length data (ynolen)
load(paste(mypath, fmeanlength, sep="")) # Load annual mean data computed in the previous script

tlldf <- levels(as.factor(tmeanlengthsyear[,1])) 
abdf <- levels(as.factor(tlindex0$year))
ynolen <- as.numeric(setdiff(abdf, tlldf)) # years without length data

# For each year without length distributions (ynolen),
# adds a new row with the mean of the length distributions (globalmean)
# in the table 'tmeanlengthsyear'
for (i in ynolen){
  tmeanlengthsyear[nrow(tmeanlengthsyear)+1,] <- c(i, globalmean)
}

# Quizás habría que guardar el tmeanlength, para que quede constancia que en los 
# gap years hemos usado la media global

#****************************
# 8.2) For normal years,
#      with some sample without length information
#      Use the mean length of that particular year

#--------------
# Open table with mean length per year
# load("20200819_tmeanlengthsyear.RData") 
# load(fmeanlength)
# tmeanlengthsyear # this is the table with the mean lengths per year

#--------------
# Add a column with annual mean length
tlindex0 <- merge(tlindex0, tmeanlengthsyear, by="year", all.x=TRUE)

#--------------
# Idenfity gap samples
# Samples with number of larvae > 0 but standardised abundance to 2 mm (n2mm) is NA
# That means abundance exists in the abundance table
# but lengths are not available for that collector in the length table (hence, missing length information)

# Gapsamples
gapsamples=which(tlindex0[,sp]>0 & is.na(tlindex0$n2mm));length(gapsamples)
tlindex0[gapsamples,]

#--------------
# Effectively assign annual mean to gap samples
# tlindex0$n2mm[gapsamples]=round(tlindex0[gapsamples,sp]*((exp(coef(mod)[2]*referencelength))/(exp(coef(mod)[2]*v1))),digits=1)
tlindex0$n2mm[gapsamples]=round(tlindex0[gapsamples,sp]*((exp(coef(mod)[2]*referencelength))/(exp(coef(mod)[2]*tlindex0$meanlength[gapsamples]))),digits=1)

#--------------
# Check now we do not have NA in gapsamples
tlindex0[gapsamples,]

#****************************
# 8.3) Assign 0 to still NA "n2mm" 
#      This are hauls with 0 larvae of the species of interest and should not be NA 
#      or any other value
# till now "n2mm" in samples with no larvae were =NA... change that to "0"
summary(tlindex0$n2mm)
tlindex0$n2mm[is.na(tlindex0$n2mm)] <- 0
summary(tlindex0$n2mm)## now no NA in n2mm

# copare abs a 2mm con abs larvas:
names(tlindex0)
plot(tlindex0$n2mm,tlindex0[,sp])# comprobado pega bien

#####                            END MODULE                            #######
##############################################################################


#*********************************************
# 9. Compute variables density and abundance
#*********************************************
#####################################################################################################
##### 			MODULE compute densities and abundances                                  ####
#####  set towdepths, para hacer sto bien habria que meter towdepth en la tabla de abundancias   ####
tlindex=tlindex0
# tlindex$towdepthtemp=NA
# tlindex$towdepthtemp[which(tlindex$fishing_type=="deep_oblique")]=70
# tlindex$towdepthtemp[which(tlindex$fishing_type=="mixed_layer_oblique")]=30
# Incorporado al script 01_genera_tabla_abund
plot(tlindex$towdepth,tlindex$towdepthtemp) # Por tanto, el plot debería estar allí...?


#### abundance = towdepth* n2mm/volumen
# abvar: name of the abundance variable of the target species, e.g. bftab, albab, ...

tlindex$ab=(tlindex$towdepthtemp*tlindex$n2mm)/tlindex$volume
names(tlindex)[names(tlindex)=="ab"] <- abvar  

hist(tlindex[,abvar], main=abvar, xlab="")
summary(tlindex[,abvar])
summary(tlindex)

#************************************************************************
# 10. Compute abundances (gear standardised) 
# with the model published in ICCAT
#************************************************************************

###  bftab_gs= bftab gear standardized
# abundances of B60 are standardized to B90 catch following ecuation:
# B90 = 0.5823*B60*exp(B60*0.00115)
# example:
# B60= 70 -> B90=44.17795, as in excel of walter analisis b90 vs b60 bf0613 2013 walt's edits.xlsx

abgsvar <- paste(tolower(faocode), "ab_gs", sep="") # name of the abundance variable
# tlindex$ab_gs= NaN
# tlindex$ab_gs[tlindex$gear=="B90"]=tlindex$BFTab[tlindex$gear=="B90"]
# tlindex$ab_gs[tlindex$gear=="B60"]=0.5823*tlindex$BFTab[tlindex$gear=="B60"]*exp(tlindex$BFTab[tlindex$gear=="B60"]*0.00115)

tlindex$ab_gs= NaN
tlindex$ab_gs[tlindex$gear=="B90"]=tlindex[tlindex$gear=="B90", abvar]
tlindex$ab_gs[tlindex$gear=="B60"]=0.5823*tlindex[tlindex$gear=="B60", abvar]*exp(tlindex[tlindex$gear=="B60", abvar]*0.00115)
names(tlindex)[names(tlindex)=="ab_gs"] <- abgsvar

#####                            END MODULE                            #######
##############################################################################


# tapply(tlindex$BFTab_gs,tlindex$year,mean)
# tapply(tlindex$BFTab_gs,tlindex$year,max)
# tapply(tlindex$BFTab_gs[which(tlindex$BFTab_gs>0)],tlindex$year[which(tlindex$BFTab_gs>0)],mean)

tapply(tlindex[,abgsvar],tlindex$year,mean)
tapply(tlindex[,abgsvar],tlindex$year,max)

tlpos <- tlindex[which(tlindex[,abgsvar]>0),]
tapply(tlpos[,abgsvar], tlpos[,"year"], mean)

#************************************************************************
# 11. Export table 't_abu_larvind_BFT_2mm_m2_mixedgears.csv'
#     to .csv and .xlsx format
#************************************************************************
# export table

# filters:
tout=tlindex
names(tout)

fileout2 <- paste(outdate, "_t_abu_larvind_", faocode, "_2mm_m2_mixgears", sep="")
fileout2

save.csvxlsx(tout, path=sppath, file_out=fileout2)

# VALORAR SI NO GUARDAR EN NINGÚN CASO COMO .csv
# .csv: ocupa mucha menos memoria
# .xlsx: guardado y leído con librerías que no tiran de Java, ocupan más 
#        pero quizás no dan problemas con los decimales y las comas
#        OJO!! Se guardan los NAs como celdas vacías, fijarse si lo lee bien o no.

test=read.csv3(paste(sppath, fileout2, ".csv",sep=""), na.strings="-999")
names(test)


# Save log file with settings 
flogname <- paste("LarvInd_settings_log_", faocode, "_", substring(fabund, 1, 8), "_", Sys.Date(), ".txt", sep="") 
sink(file=paste(flogname), append=TRUE)
print(paste("--- '", fcurr, "' ---"))
print(paste("Current date : ", Sys.Date()))
print(paste("Abundance and length files processed on date : ", substring(fabund, 1,8)))
print(paste("Species: ", species))
print(paste("Abundance file (fabund): ", fabund))
print(paste("Length file (flen): ", flen))
print(paste("Processing file for abundance and length data (fprocess) :", fprocess))
print(paste("Annual mean length file (fmeanlength) : ", fmeanlength))
# Save settings and retrocalculation model into the already existing log file
print("------------- Script abu calculate nlarvae 2 mm ------------------")
print(paste("Finally selected model for retrocalculation : "))
print(paste(fretro))
cat("\n")
print(mod)
print("------------------------------------------------------------------")
cat("\n")
sink()
### WARNING! If you run the script more than once it will have duplicated information   

#-------------------------- End of script!!! ----------------------------------#
