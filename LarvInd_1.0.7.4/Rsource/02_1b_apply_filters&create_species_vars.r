#-------------------------------------------------------------------------------
# Project: BLUEFIN, PANDORA
# Autors: M.P. Tugores, D. Álvarez-Berastegui
#
# Objective: Creates variables, format variables and apply filters to 
#            generate a table to analyse the Larval Index
#
# 1. Takes previously generated objects:
#    'Abundance_df': data.frame containing abundance information of one or several
#                    species (according to the settings we have stablished previously)
#    'process_specifs': object of class list containing the specifications we have
#                       set for the processing
# 2. Apply filters for surveys and fishing_type: 
#    2.1. Remove 'MEDIAS0710' 
#    2.2. Remove 'subsurface' hauls
# 3. Adaptations for gapyears
#    3.1. Fishing type NA changed to "NI"
#    3.2. Remove duplicated stations
# 6. Species specific variables and filters
#    6.1. Presence-absence for each species
#    6.2. Densities for each species
#    6.3. Remove 2002 and 2003 for Albacore
#
# 7. Remove, for every species, if there is some survey with n_larvas == 0
#    This filter has been removed to allow strict update of BFT ICCAT 2021
# 
# 8. Resume número de muestras
# 9. Genera Boxplot de volúmenes
# 10. Exports the output as .csv and .xlxs files
#
# Date: 2022/01/13
#-------------------------------------------------------------------------------

################## OBSERVATION 08/11/2021 ######################################
# Currently, points 8 and 9 are commented and hence they are not executed.
# We have to think if they are needed or not
################################################################################

################################################################################
# Inputs: 
# 'Abundance_df'
# 'process_specifs'
#
# Outputs:
# - tiff: "boxplot_volumes_B60&B90.tiff"  
# - csv:  "20200727_t_analisis_larvalindex_BFT_abs.csv" 
# - xlxs: "20200727_t_analisis_larvalindex_BFT_abs.xlxs" 
################################################################################

############################# NOTES## ##########################################
#                Includes adaptation to be used with GAP years                 #
#                     DON'T CHANGE the code !!!                                #
################################################################################


############################################
# 1. Takes 'Abundance_df'  
###########################################
t0 <- Abundance_df

############################################
# 2. Applies filters on surveys and fishing_type
############################################
# 2.1. Quitamos campañas medias que no tienen posiciones
t1=t0[!t0$survey=="MEDIAS0710",];nrow(t1) # Si no tenemos larval index 2, no temos medias
                                          # La definición actual 2021/04/09 de larval index 2 ya excluye MEDIAS
# 2.2. Excluimos subsurface
t1=t1[t1$fishing_type!="subsurface",]  # Remove subsurface B90 de 2004 y 2005,to avoid duplicated stations 

names(t1)
nrow(t1)
str(t1)

with(t1, tapply(id_netcolector, list(gear,year), length))
with(t1, tapply(id_netcolector, list(fishing_type,year), length))
aggregate(t1$towdepth, list(t1$fishing_type, t1$gear, t1$year), summary)
aggregate(t1$towdepth, list(t1$fishing_type, t1$gear, t1$year), function(x){summary(na.omit(x))}) # Hi ha una pesca de 2011 que no té towdepth

############################################
# 3. Adaptación para los gapyears
############################################
# This is intended for hauls outside the regular years,
# for whigh a fishing type has not already been set 26/01/2021

######### Set "fishing type" = "NI" for hauls with "fishing type" = <NA> #######
# This is already performed in previous scripts!!!
# levels(t1$fishing_type) <- c(levels(t1$fishing_type), "NI")
# t1$fishing_type [is.na(t1$fishing_type)] <- "NI" 

###################### Check remove duplicates  ################################
# WARNING!!!!
# Could this better be perfomed in previous scripts...?? 
# .... Better not because it is a kind of filter 
# .... but could lead to errors 
# .... if the previous files are used in some analysis.....
t2 <- t1; t2$station_id <- as.character(t2$station_id)
t2 <- split(t1, f=as.factor(t1$year))

# Check duplicates by year
lapply(t2, FUN= function(x){x[x$station_id %in% x$station_id[duplicated(x$station_id)],]})

gapyears <- c("2006", "2008", "2010", "2011")
x2modif <- which(names(t2) %in% gapyears)
for (i in x2modif){
    t2[[i]] <- do.call(rbind, by(t2[[i]], t2[[i]]$station_id, function(x) x[which.min(x$station_order), ] ))
}
t2 <- do.call(rbind, t2)
dim(t2)
t1 <- t2
################################################################################ 

############################################
# 4. Formatea fecha y NAs
###########################################

#### 4.1. Fecha as.Date
# class(t1$mydate)

# t1$mydate <- as.Date(t1$mydate)
# names(t1)[names(t1)=="mydate"] <- "date"
# head(t1)

#### 4.2. Set -9999 and -999 to NA
# if (length(which(t1==-999))>0) t1[t1 == -999]=NA   # No es necesario, ya lo detecta. 
# if(length(which(t1==-9999))>0) t1[t1 == -9999]=NA   # No es necesario, ya lo detecta
# Implementado en los scripts de generación de variables desde la BD 

# nrow(t1)
# names(t1)
# fix(t0)  # esta función estropea los formatos de según qué columnas!! No usar!!


############################################
# 5. Genera variables
###########################################

#### 5.1. Julian day
# library(date)
# t2$jd <- as.numeric(mdy.date(t2$month,t2$day,1960))
# require(lubridate)
# t1$jd <- yday(t1$date)
# Esto también podría estar en el script de generación de la tabla

#### 5.2. Categorise hour day in factor  
#  noche menor que 0.23 y mayor que 0.93  
# hamanece=0.27# sale de 6.5/24
# hanochece=0.89 # sale de 21.5 / 24
# deltacrepusculo=0.04 # sale de 1/24.. osea 1 hora de crepusculo

# cathour
# t1$cathour=NA
# t1$cathour[t1$hournorm>=(hamanece+deltacrepusculo) & t1$hournorm<=(hanochece-deltacrepusculo)]="day"
# t1$cathour[t1$hournorm<(hamanece-deltacrepusculo)| t1$hournorm>(hanochece+deltacrepusculo)]="night"
# t1$cathour[is.na(t1$cathour)]="crep"
# t1$cathour=as.factor(t1$cathour)

# require(ggplot2)                      
# qplot(t1$cathour,t1$hournorm)

# daynight
# hamanece=0.27# sale de 6.5/24
# hanochece=0.89 # sale de 21.5 / 24
# t1$daynight=NA
# t1$daynight [t1$hournorm>=(hamanece) & t1$hournorm<=(hanochece)]="day"
# t1$daynight [t1$hournorm<(hamanece) | t1$hournorm>(hanochece)]="night"
# t1$daynight

#---------------------------------
# Alternative way of defining day, night and dusk
#---------------------------------
# Faltaría definir lo que queremos que sea crepúsculo (que es dusk)

# myhour <- format(strptime(t1$hour,"%H:%M:%S"),'%H:%M:%S')
# myhour2 <- as.POSIXct(myhour, format="%H:%M:%S", tz="UTC")
# mydayhour <- as.POSIXct(paste(t1$date, myhour), format="%Y-%m-%d %H:%M:%S", tz="UTC") 

# library(suncalc)  # Estimates sunrise and sunset from lon, lat and time data
# keep = c("sunrise", "sunriseEnd", "sunsetStart", "sunset", "dusk", "nauticalDusk",
#   "night", "nadir", "nightEnd", "nauticalDawn", "dawn")  
# ss <- getSunlightTimes(data=t1, keep=keep)

# dn_f <- NULL
# dn_f [mydayhour>=ss$sunrise & mydayhour<ss$sunriseEnd] <- "dawn"
# dn_f [mydayhour>=ss$sunriseEnd & mydayhour<=ss$sunset] <- "day"
# dn_f [mydayhour>ss$sunset & mydayhour<ss$night] <- "dusk"
# dn_f [mydayhour>=ss$night | mydayhour<ss$sunrise] <- "night"

#### 5.3. Lunar phase
# require(lunar)
# t1$lunar_illum <- lunar.illumination(as.Date(t1$date))
# t1$lunar_phase <- lunar.phase(as.Date(t1$date), name=8) # 1: New, 2: Waxing crescent, 3: First quarter
# 4: Waxing gibbous, 5: Full, 6: Waning gibbous, 7: Third quarter, 8: Waning crescent

#### 5.4. NOAA Bathymetry  
# require(raster)
# bal <- raster(x=paste(getwd(),"/Rsource/sig/ETOPO1_BalearicSea.tif", sep=""))
# depth <- extract(bal, t1[c("lon", "lat")])
# t1 <- data.frame(t1, bottom_dep=depth)


#### 5.5. Set tow depth temp
# t1$towdepthtemp <- factor(t1$fishing_type, 
#  c("deep_oblique", "mixed_layer_oblique", "subsurface", "Oblicua", "NI"), 
#  # labels=c("70", "30", "5", "70", "70")) 
#  labels=c("70", "30", "5", "70", "30")) # Se modifica de 70 a 30 les NI
#  # ja que les profunditats de les pesques dels gapyears van de 8 a 45, mitjana 21
#t1$towdepthtemp <- as.numeric(levels(t1$towdepthtemp)[t1$towdepthtemp])


###############################################
# 6. Species specific variables and filters
###############################################
colgrupo <- which(names(t1)=="grupo_para_procesar")
nspecies <- length(process_specifs$nomcientifico)
# spnames <- gsub(" ", "_", process_specifs$nomcientifico)
spnames <- names(t1[(colgrupo+1):(colgrupo+nspecies)])
#nspecies <- length(spnames)

abund_list <- list()
for (i in 1:nspecies){
  abund_list[[i]] <- cbind(t1[1:colgrupo], t1[(colgrupo+nspecies+1):ncol(t1)], t1[colgrupo+i])
}
names(abund_list) <- spnames 

### 6.1. Variable presencia-ausencia para "species"
# Añade una variable = 1 si hay presencia
# t2$lpres=ifelse((t2$Thunnus_thynnus)>0,1,0)

for(i in 1:length(abund_list)){
  sp <- names(abund_list)[i]
  abund_list[[i]]$lpres <- NULL
  abund_list[[i]]$nlarvae_m3 <- NULL
  abund_list[[i]]$CPUAnom <- NULL
  abund_list[[i]]$CPUAnom_temp <- NULL
  
  abund_list[[i]]$lpres=ifelse(as.vector(abund_list[[i]][sp]>0), 1, 0)
  abund_list[[i]]$nlarvae_m3 <- abund_list[[i]][,sp]/abund_list[[i]]$volume
  abund_list[[i]]$CPUAnom <- abund_list[[i]][,sp]*abund_list[[i]]$towdepth/abund_list[[i]]$volume
  abund_list[[i]]$CPUAnom_temp <- abund_list[[i]][,sp]*abund_list[[i]]$towdepthtemp/abund_list[[i]]$volume
}

#### 6.2. Densities 
#faocode
#sp

#t1$nlarvae_m3 <- t2[,sp]/t1$volume
#t1$nlarvae_100m3 <- 100*t2[,sp]/t1$volume
#t1$CPUAnom <- t2[,sp]*t1$towdepth/t1$volume
#t1$CPUAnom_temp <- t2[,sp]*t1$towdepthtemp/t1$volume

# 6.3. Para ALB, quitamos el año 2002 y 2003 para albacora
# if(faocode=="ALB") t1 = t1[t1$year!= 2002 & t1$year!=2003,]
if (any(names(abund_list)=="Thunnus_alalunga")){
  ta <- which(names(abund_list)=="Thunnus_alalunga")
  tmp <- abund_list[[ta]]
  tmp <- tmp[tmp$year!=2002 & tmp$year!=2003,]
  abund_list[[ta]] <- tmp
}
# Para BFT, excluímos 2006 y 2010: HACER UNA QUESTION CON RESPUESTA!!!
# if(faocode=="BFT") t1=t1[t2$year!=2006 & t1$year!=2010,]
# names(t1)
# nrow(t1)
# str(t1)

#########################################################################
# 7. Remove, for every species, if there is some survey with n_larvas == 0
#########################################################################
# Intended for swordfish as it had larvae in 2010 or 2011 in WP2 or B60 
# (but not in the selected gears) and the year was appearing in the abundance table 
# as a 0 value in the amount of larvae

#for(i in 1:length(abund_list)){
#  df1 <- abund_list[[i]]
#  chec1 <- aggregate (df1[spnames[i]], by=list(df1$survey), FUN=sum)
#  if((length(chec1[,1][chec1[,2]==0])>=1)==TRUE) {
#    surveys2remove <- chec1[,1][chec1[,2]==0]
#    df1 <- df1[!df1$survey %in% surveys2remove,]
#  }
#  abund_list[[i]] <- df1
#  rm(df1, chec1)
#}

###########################################
# 8. Resume número de muestras
###########################################
#resume número de muestras por año.... pivot tables in R 
# http://stackoverflow.com/questions/10220510/summary-statistics-by-two-or-more-factor-variables

#for(i in 1:length(abund_list)){
#  print(names(abund_list)[i])
#  t0 <- abund_list[[i]]
#  print(tapply(t0$id_operation,t0$year,length))

#  print(with(t0, tapply(id_netcolector, list(gear,year), length)))
#  print(with(t0, tapply(id_netcolector, list(fishing_type,year), length)))

#  print(with(t0, 
#    aggregate(towdepth, with(t0, list(fishing_type, gear, year)), summary)
#    )
#  )

#  print(with(t0,
#    aggregate(towdepth, with(t0, list(fishing_type, gear, year)), function(x){summary(na.omit(x))})
#    )
#  )
#}

###########################################
#  9. Genera Boxplot de volúmenes
###########################################                
#for(i in 1:length(abund_list)){
#  faocode <- process_specifs$FAO_X3A_Code[process_specifs$nomcientifico==gsub("_", " ", names(abund_list)[i])]
#  tiff(paste(figpath,"boxplot_volumes_B60&B90_", faocode, ".tiff",sep=""), height =26,width=13*1.5,units = "cm",res=150)
#    par(mfrow=c(2,1))
#    boxplot(t1$volume[t1$gear=="B60"]~t1$year[t1$gear=="B60"],main="Volumenes B60")
#    boxplot(t1$volume[t1$gear=="B90"]~t1$year[t1$gear=="B90"],main="Volumenes B90")
#  dev.off()
#}

###########################################
# 10. Exporta tabla a .csv y .xlxs
###########################################

# cambiar esto con la última tabla
for(i in 1:length(abund_list)){
  faocode <- process_specifs$FAO_X3A_Code[process_specifs$nomcientifico==gsub("_", " ", names(abund_list)[i])]
  
  tout=abund_list[[i]]
  if(all(is.na(tout$larval_index))){ 
    tout$larval_index=tout$tpt_larval_index_bft
  }
  # Si hago esto, debería eliminar la columna tpt_larval_index_bft y no trabajar sobre ella
  names(tout)
  head(tout)
  
  print("----------------------------------------------------")
  print(paste("Number of collectors by year for  ", names(abund_list)[i]))
  print(tapply(tout$id_netcolector,tout$year,length))

  # CAMBIAR VARIABLE QUE ESTEN EN "env2"
  # outdate <- substr(gsub(".csv", "", fname), (nchar(fname)-13), (nchar(fname)-4))
  outdate <- gsub("-", "", Sys.Date())
  fileout <- paste(gsub("-", "", outdate), "_t_abundance_larvind_", faocode, "_abs", sep="")

  save.csvxlsx(tout, path=opath, file_out=fileout) 
  # write.table(tout,paste(opath, fileout, ".csv",sep=""), row.names=FALSE, quote = FALSE, sep=HCUsep, dec=HCUdec) 
  # library(xlsx)
  # write.xlsx(tout,paste(opath, fileout, ".xlsx",sep=""), row.names=FALSE, sheetName="Sheet1",showNA=TRUE)
  # require(writexl)
  # write_xlsx(tout, path=paste(opath, fileout, ".xlsx",sep=""))
  
  
}
#revisar que date en la tabla de salida es igual a day
################################################################################