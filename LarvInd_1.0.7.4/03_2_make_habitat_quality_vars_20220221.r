#------------------------------------------------------------------------------#
# Script to generate habitat quality variable                                  #
#                                                                              #
# Analysis performed:                                                          #
# 1) Add grid information like in Walter (Ref.?)
# 2) Generate a presence-absence GAM model including the interaction         #
#    of longitude and latitude, the day of the year, the residual temperature  #
#    and the salinity in the mixed layer using a quasibinomial family          #
# 3)	Performs the same model excluding one year at a time                     #
# 4)	Add the predicted values to the main abundance data.frame                #
# 5)	Saves the generated table                                                #
#                                                                              #
# Project: BLUEFIN, PANDORA                                                    #
# Authors: D. Alvarez                                                          #
#                                                                              #
# Last modified: 2022/02/21                                                    #
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
#**********                                                                    #
# Outputs:                                                                     #
#**********                                                                    #
#                                                                              #
# - csv: "20200727_t_predbin_BFT.csv"                                          #
# - xlxs: "20200727_t_predbin_BFT.xlsx"                                        #
#                                                                              #
#------------------------------------------------------------------------------#

rm(list=ls())

#############
# Inputs
#############
source("01_0_set_input_files.R"); species; fprocess
source("Rsource/settings_m3.R") # cargar funciones, relative paths and settings

# Name of the file containing standardised abundance
fanalisis <- paste(outdate, "_t_abu_larvind_BFT_interp_insitu.csv", sep="")
fanalisis <- paste(outdate, "_t_abu_larvind_ALB_interp_insitu.csv", sep="")
fgrid <- "20180917_cuadgrid_balsea_1DD.RData" 
fcurr <- "03_3_make_habitat_quality_vars.r" # Current script name

# Check existence of required files
files2check <- c(paste(sppath, fanalisis, sep=""), paste(mypath, fgrid, sep=""))
filecheck(files2check)

################################################################################
#                            PROCESSING                                        #
################################################################################

##################
# Read .csv table
##################
if (exists("t0")) rm(t0)
t0=read.csv3(paste(sppath, fanalisis, sep=""), na.strings='NA',header=T)
t0$date=as.Date(t0$date)

####################### 
# Assign id in sampling grid as Walter
##################
# Load cuadgrid
load(fgrid) # load an existent grid 'cuadgrid' into the R environment
ls()
cuadgrid

#------------
# Or create a new grid, if required
# createSpatialGrid (minlon, maxlon, minlat, maxlat, cellwidth, 
#  projection="+proj=longlat +datum=WGS84", basemap="Rsource/sig/country.shp", saveTiff=TRUE, saveShape=FALSE, 
#  mypath=getwd(), coastlinepath=getwd())
#------------
#  basemap: full path, name and extension of a shapefile that you want to use
#  as basemap when plotting
#  coastlinepath: ????
#  mypath: ????  

#assign the cell id to each sampling grid

t1 <- t0
require(sp)
coordinates(t1) <- c("lon", "lat")  # transform to a geographical object
proj4string(t1) <- proj4string(cuadgrid)
t0$gridid=over(t1,cuadgrid) # Add the grid id info to the 

plot(cuadgrid, axes=T, xlim=c(0,6), ylim=c(36,42))
points(t0$lon,t0$lat)
text(t0$lon,t0$lat,t0$gridid)# test that values are the same in one cell

#########################################################################
# OPERACPONAL 2:

# IMPORTANTE: LA TABLA ORIGINAL TIENE QUE LEER BIEN LOS na (-999) PARA QUE LLOS DATOS SELCCIONADOS SALGAN OK)
# ya he probado a ganar datos cambiando ekepixel por ekemean_060 se ganan muchos datos pero la mitad de r2-
# ahora pruebo a ganr datos sin quitar eke pixel


tmodel=t0
names(tmodel)


if (exists("temp1")) rm(temp1)
temp1=na.omit(tmodel[c("id_netcolector","lpres","year","gear","hournorm","lat","lon",
"jd","residualtemp","TMEZCLA","SMEZCLA","FMEZCLA")])

str(temp1)


plot(TMEZCLA~jd, data=temp1, xlab="Day of the Year", ylab="Temperature in the mix layer depth (ºC)")
cor(temp1$jd,temp1$TMEZCLA)



nrow(temp1)
#"fishingtype"
boxplot(SMEZCLA~year, data=temp1)
names(temp1)

tana=temp1

library(mgcv)

if(exists("mbin")) rm(mbin)
#mbin=gam(lpres ~ s(lon, lat)+as.factor(gear)+ s(jd,k=5)+s(residualtemp,k=5)+ s(SMEZCLA,k=5),data=tana,family=quasibinomial)
mbin=gam(lpres ~ s(lon, lat)+ s(jd,k=5)+s(residualtemp,k=5)+s(SMEZCLA,k=5), data=tana, family=quasibinomial)
plot(mbin,all=T)
summary(mbin)

# coastline <- readShapePoly("Rsource/sig/country.shp")
# already loaded

par(mfrow=c(2,2))
vis.gam(mbin,view=c('lon','lat'),plot.type="contour",color="topo",too.far=.03,main='',cex.lab = 1.5)
plot(coastline, xlim=range(0,5),ylim=range(38.5,40), border="blue",col="white",add=TRUE)
plot(mbin,select=2,shade=T,scale=0,rug=T,res=F,cex.lab = 1.5)
plot(mbin,select=3,shade=T,scale=0,rug=T,res=F,cex.lab = 1.5)
plot(mbin,select=4,shade=T,scale=0,rug=T,res=F,cex.lab = 1.5)
plot(mbin,select=5,shade=T,scale=0,rug=T,res=F,cex.lab = 1.5)

#quito fishing type (dejando gear) y dejo um porque el ggplot es mucho mejor.

# con smezcla norm sale peor

#tana$fittedbin=mbin$fitted.values
head(tana)

tana$fittedbin=NULL
tana$fittedbin=(round(predict.gam(mbin,tana,type="response"),digits=3))

d1=tana$fittedbin[tana$stage1bin==1]
d2=tana$fittedbin[tana$stage1bin==0]

require(ggplot2)
ggplot(tana, aes(fittedbin, fill = as.factor(lpres))) + geom_density(alpha = 0.2)

require(pROC)
g <- plot.roc(lpres ~ fittedbin, data = tana,print.auc=TRUE)


######## predice en todo  y luego con crospred para hacer variable de habitat en todos los puntos.
twalt=temp1# tabla sin na
twalt$survey=twalt$year
summary(twalt)
nrow(twalt)

# tsurvey=c(2001,2002,2003,2004,2005,2012,2013,2014,2015,2016)
tsurvey=c(2012,2013,2014,2015,2016)
l=length(tsurvey)

#tyear=c(2001,2002,2003,2004)

twalt$predbin=NA
head(twalt)

##################  inicio FOR........ start modelling one year with the other 5
for(i in 1:length(tsurvey)){
  ######## necesitamos 3 tablas una con todo na incluido = tmodel
  ######### otra sin na para hacer el modelo

  tsurvey[i]

  if(exists("tmodelo")) rm(tmodelo)
  if(exists("tpredict"))rm(tpredict)
  if(exists("opv3val"))rm(opv3val)

  #haz una tabla sin datos para la camapaña i

  #de la tabla anterior seleciona las campañas para generar el modelo
  tmodelo=(twalt[twalt$survey!=tsurvey[i],])################################ asi elegiremos solo los sin na pero de las variables a usar no de otras
  #fix(tmodelo)
  # de la tabla inicial haz una con los datos a validar
  tpredict=twalt[twalt$survey==tsurvey[i],]##############OJOOOOOOOOOO  aqui ya metemos la tabla de walter

  nrow(tmodelo)
  nrow(tpredict)

  summary(tpredict)
  if(exists("mbin")) rm(mbin)
  # m1=gam(lpres ~ s(lon, lat)+as.factor(gear)+ s(jd,k=5)+s(residualtemp,k=5)+ s(SMEZCLA,k=3),data=tmodelo,family=quasibinomial)
  m1=gam(lpres~s(lon, lat)+s(jd,k=5)+s(residualtemp,k=5)+s(SMEZCLA,k=3), data=tmodelo, family=quasibinomial)
  summary(m1)

  #predice los valores de qhabitat en la tabla de walter para el ´ño no usado
  if(exists("pred")) rm(pred)
  tpredict$predbin=(round(predict.gam(m1, tpredict, type="response"), digits=4))
  g <- plot.roc(lpres~predbin, data=tpredict, print.auc=TRUE)

  head(tpredict)

  #pegamos estos valores a twalt

  twalt$predbin[twalt$id_netcolector %in% tpredict$id_netcolector]=tpredict$predbin
  g <- plot.roc(lpres~predbin, data=twalt[twalt$survey==tsurvey[i],], print.auc=TRUE, main=tsurvey[i])#### como da lo mismo que antes es ok.
  Sys.sleep(1)

  plot(twalt[twalt$id_netcolector %in% tpredict$id_netcolector,]$id_netcolector,tpredict$id_netcolector)
} #################### End loop ################################################


head(twalt)

# hacemos el merge de twalt con tpredict y comparamamos

tpaste=twalt[c("id_netcolector","TMEZCLA","predbin")]# metemos um para usar de prediccion
names(tpaste)[names(tpaste)=="TMEZCLA"] <- "TMEZCLAtwalt"
head(tpaste)

if(exists("tout"))rm(tout)

tpaste2=merge(x=tana, y=tpaste, by ="id_netcolector", all.x=TRUE)
names(tpaste2)
#comprobamos que se ha pegado bien por um y umtwalt
tpaste3=tpaste2[c("id_netcolector","TMEZCLAtwalt","predbin","fittedbin","lpres")]# metemos um para usar de prediccion
names(tpaste3)


## tout es la tabla que se guarda con los datos de las preducciones y el campo tmezcla para comprobar futiros pegados
tout=tpaste3

with(tout, plot.roc(lpres, predbin, print.auc = TRUE,col="red"))
with(tout, plot.roc(lpres, fittedbin, print.auc = TRUE,col="red"))

#fix(tout)
nrow(tout)
names(tout)
head(tout)

#-------------------------
# Esto es lo que viene del script "03_4_generate_table_all_vars_4_larval_index_gvel.r"
# Pero creo que está también incluido en este script
# tpredbin <- read.csv3(paste(sppath, fpredbin, sep=""), na.strings='NA', dec='.',header=T)
tpredbin=tout
tpredbin=tpredbin[,c(1:4),];head(tpredbin)

t1.2=merge(x=t0, y=tpredbin, by="id_netcolector", all.x=TRUE)   # t1.1 es la tabla de abundancias; ahora t1.1 es t0
plot(t1.2$TMEZCLA,t1.2$TMEZCLAtwalt)### must be linear
t1.2$TMEZCLAtwalt=NULL
names(t1.2)
nrow(t1.2)

#------------------------
# cuando hagamos mejor las tablas de otras variables hacemos tout=t1.2... por ahora
tout=t0

names(tout)
nrow(tout)

#-------------------------
# Save as .csv and .xlsx
#-------------------------
fileout <- paste(substr(fanalisis, 1, 8), "_t_abu_larvind_", faocode, "_allvars", sep="")  
fileout

save.csvxlsx(tout, path=sppath, file_out=fileout)

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
print(paste("Abundance file for analysis (fanalisis) : ", fanalisis))
cat("\n")
sink()

#-------------------------- End of script!!! ----------------------------------#

