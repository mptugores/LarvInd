#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN
# Autores: M.P. Tugores, D. Álvarez-Berastegui
#
# Generate Abundance table from: "ecolarv_comunidades_actual.accdb"
# Add info from CTD database
#
# Consultas Access:
# Q1: 00_selecciona_de_grupo_para_anlisis_spp_filas
# Q2: 01_colector_abundancia_sppfilas_conceros
# Q3: 03_abundancias con datos operación
# Q4: 04_larval_index_abs_y_ambientales_final 
#
# Date: 2023/11/24
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# INPUTS required
# 1) process_specifs
# 2) ecolarv_filt
# 3) databaseCTD
# 4) dpath: path were outputs will be saved
# 5) funciones cargadas!!!!!
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# OUTPUTS
# In the R console: 
#     'Abundance_df': objeto de clase lista con "n" objetos de clase data.frame. 
#                    Consta de 1 a n elementos, tantos como el nº de especies procesadas 
#	Ficheros .csv: en la carpeta "outputtables"
#     Contienen las abundancias de las especies procesadas, cuyo nombre será ej.:
#       a.	"abundance_larv.index_Thunnus thynnus_2020-06-11.csv" o
#       b.	 "abundance_larv.index_multispecies_340_342_2020-06-12.csv",
#-------------------------------------------------------------------------------

#-------------------------------
# Check inputs
#-------------------------------
# Case 0:   
# paths <- c("mypath", "dpath", "opath", "figpath", "propath")

# if(any(!exists(paths))) source("Rsource/relative_folderpaths.R") # Si no existe ninguno de los folderpaths

# Si existe alguno (raro que suceda, pero por si acaso), pero no los que vamos a usar, los cargamos a mano
# if (!exists("propath")) propath <- paste(getwd(), "/processfiles/", sep="")
# if (!exists("dpath")) dpath <- paste(getwd(), "/data/", sep="")

# rm(paths)

# Case 1:
if (!exists("process_specifs")){
  print("Process specifications are missing!!!")
  answer <- menu(c("Y", "N"), 
    title="Do you want to load '00_process_specifications.RData' file containing this data?")
  if (answer==1){
    tmp <- file.choose()
    load(tmp)
  }
  if (answer==2){
    stop("You should run the script '00_main_run_larval_index.r")
  }
rm(answer, tmp)
} 


# Case 2:   
if (!exists("databasefileCTD")){
  print("CTD database file is missing")
  answer3 <- menu(c("Y", "N"),
    title="Do you want to select the Access database")
  if(answer3==1){
    databasefileCTD <- file.choose ()
  }
  if(answer3==2){
    print("CTD data won't be appended to the abundance table")
  }
  rm(answer3)
}

# Funciona OK. Quizás modificar el funcionamiento.


#-------------------------------
# 0. Set database files
#-------------------------------
# Setting the databasefiles
databasefile <<- paste(dbpath, databaseversion, sep="")  # ECOLARV database 
databasefileCTD <<- paste(dbpathCTD, databaseCTD, sep="") # CTD database

#-------------------------------------------------------------------------------

#-------------------------------
# 1. Query 1: "00_selecciona_de_grupo_para_anlisis_spp_filas"
#-------------------------------

#-------------------------------
# 1.1. Retrieve tables of interest 
#-------------------------------

# Tables:
# t_operaciones, t_pescas, t_colectores, t_target_nombres, t_especies    
# t_proces_target, t_target_spp_composicion 

ecolarv_tables <- getTabsAbu (databasefile) # Retrieve tables of interest as a object of class 'list'

#-------------------------------
# 1.1.B) Filter the tables of interest by
#        'process_specifs'
#-------------------------------  
# Adaptation for non-standard species
if(is.null(process_specifs$larval_index)){
  tmp <- process_specifs[-which(names(process_specifs)=="larval_index")]  
}

if(!is.null(process_specifs$larval_index)) tmp <- process_specifs

# Effectively filtering
ecolarv_filt <- list()
for(k in 1:length(ecolarv_tables)){
  print(paste(names(ecolarv_tables)[k], " is being processed", sep=""))
  ecolarv_filt[[k]] <- filter.func(ecolarv_tables[[k]], tmp)
}
names(ecolarv_filt) <- names(ecolarv_tables)
print(lapply(ecolarv_filt, FUN=nrow))

ecolarv_filt$t_proces_target <- with (ecolarv_filt, t_proces_target[t_proces_target$id_target_num==t_target_nombres$id_target_num,])
# Este filtro OK, es correcto pero no arregla las pescas de Xiphias en 2010, cuando no hay ninguna de B60 ni B90.
# Tampoco reduce el tamaño del .RData, que sigue siendo de 1 KB.

#-------------------------------
# 1.1.C) Assign NA to values -999, -9999   if there is any  
#-------------------------------  
for(k in 1:length(ecolarv_filt)){
  if(length(which(ecolarv_filt[[k]]==-9999))>0) ecolarv_filt[[k]] [ecolarv_filt[[k]] == -9999] <- NA
  if(length(which(ecolarv_filt[[k]]==-999))>0) ecolarv_filt[[k]] [ecolarv_filt[[k]] == -999] <- NA
}

#-------------------------------
# 1.1.D) Guardar taules ecolarv filtrades
#-------------------------------
# Podríamos chequear cuáles se han filtrado y cuáles no
ecolarvfile <- paste(propath, "00_1_ecolarv_DB_filt_", paste(process_specifs$FAO_X3A_Code, collapse="_"), "_", Sys.Date(), ".RData", sep="")
save(ecolarv_filt, file=ecolarvfile)

rm(ecolarv_tables, k)  # Remove unnecessary objects
#-------------------------------------------------------------------------------

#-------------------------------
# 1.2. Join/Merge different DB tables
#-------------------------------
# 1) Merge t_operaciones and t_pescas     by    id1
# 2) Merge output1 with t_colectores      by    id2
# 3) Merge output2 with t_proces_target   by    tc_id_colector_autonum and tpt_id_colector_autonum
# 4) Merge output3 with t_target_nombres  by    tpt_id_target_num      and ttn_id_target_num
# 5) Merge output4 with t_target_spp_composicion   by tpt_id_target_num    and    tts_autonum
# 6) Merge output5 with t_especies        by    tts_id_target_num             and    tes_sppid

merge1 <- merge(ecolarv_filt$t_operaciones, ecolarv_filt$t_pescas[,c(1:4)], by="id1") # Añade información de las pescas a las operaciones
merge2 <- merge(merge1, ecolarv_filt$t_colectores[,-c(2)], by="id2") # Añade información de los colectores
merge3 <- merge(merge2, ecolarv_filt$t_proces_target, by="id_colector_autonum") # Añade información de los grupos a procesar 
merge4 <- merge(merge3, ecolarv_filt$t_target_nombres, by="id_target_num") # Pone info de los grupos a procesar
merge5 <- merge(merge4, ecolarv_filt$t_target_spp_composicion, by="id_target_num", 
  all.x=TRUE, all.y=TRUE)
merge6 <- merge(merge5, ecolarv_filt$t_especies, by="sppid")

dataQ1 <- merge6

rm(merge1, merge2, merge3, merge4, merge5, merge6) # remove unnecessary objects

#-------------------------------
# 2. Query 2 (Directamente en R)
#-------------------------------
# Acces: 01_colector_abundancia_sppfilas_conceros

# 2.1. Records with amount of larvas "n_larvas"==NA are set to 0 
# as they are hauls with absence of larvae of the selected species

# Para la esta query necesitamos también t_infogrupo

# Generate ID variable for the output of the filtered version of Query 1, "dataQ1.filt"  
dataQ1$id4 <- with(dataQ1, paste(id2, colectorn, sppid, sep="_"))

aggregate(dataQ1$grupo_para_procesar, by=list(dataQ1$grupo_para_procesar), length)
aggregate(dataQ1$grupo_para_procesar, by=list(dataQ1$sppid), length)


# Unir dataQ1 y la tabla t_infogrupo que hemos importado
dataQ2 <- merge(dataQ1, ecolarv_filt$t_infogrupo[c("id4", "nlarvas")], by="id4", all.x=TRUE)
head(dataQ2)

# Rename "tg_nlarvas" to "n_larvas" y poner ceros a los NA
names(dataQ2)[names(dataQ2)=="nlarvas"] <- "n_larvas"
dataQ2$n_larvas[dataQ2$n_larvas<0 & !is.na(dataQ2$n_larvas)] <- NA # Si hay NAs en formato -99, -999 o -9999 serán puestos a NA y después a cero
dataQ2$n_larvas[is.na(dataQ2$n_larvas)] <- 0

# Till here we have all hauls duplicated for each of the processed species
aggregate(dataQ2$grupo_para_procesar, by=list(dataQ2$grupo_para_procesar), length)
aggregate(dataQ2$grupo_para_procesar, by=list(dataQ2$sppid), length)

######################################################
# Hay algunas "tg_todasmedias" == NA, ¿por qué?
# Porque no se midieron
######################################################

# Now, we leave the data.frame with no duplicated hauls.
# Each haul will appear once with a column with the abundance of each species

sps <- process_specifs[grep("nomcientifico", names(process_specifs))]$nomcientifico

if(length(sps)==1){  # Processing for only 1 species: we only give format to the column name
  tmp.abu2 <- dataQ2
  names(tmp.abu2)[names(tmp.abu2)=="n_larvas"] <- sps
  tmp.abu2 <- tmp.abu2[-which(names(tmp.abu2) %in% c("autonum", "nomcientifico"))] # remove 2 columns not to be kept
}

# 
if (length(sps)>1){ # Processing for more than 1 species
  #tmp.abu2 <- NULL
  # Dividimos la tabla dataQ2 en dos:
  # 1)  tabla.info (información de las pescas)
  #     se quitan duplidados (tantos como número de especies seleccionadas * nº pescas)
  # 2)  tabla de abundancias
  
  colskeep <- c("id_target_num", "id_colector_autonum", "id2", "id1",
                 "campania", "codestacion", "orden_bd", "idoperacion_num",
                 "revisita", "fecha_sistematic", "tipo_estacion", "idIBAMAR",
                 "fecha_estacion_llegada", "horallegada", "hournorm", 
                 "latitudoperacion", "longitudoperacion", "id_pesca_num",
                 "tipo_pesca", "estructura", "cont_estructura", "flag_procesado",
                 "malla", "colectorn", "profundidad_max", "m3", 
                 "selec_analisis", "larval_index", "grupo_para_procesar")  
  
  #tmp.info <- dataQ2[colskeep]
  tmp.info <- dataQ2[,which(names(dataQ2) %in% colskeep)]
  # tablainfo <- tmp.info[duplicated(tmp.info),] # Eliminamos duplicados  (old, and I think wrong)
  tablainfo <- tmp.info[!duplicated(tmp.info),]
  
  # Check equal number of rows
  check.rows <- nrow(dataQ2)/length(sps)==nrow(tablainfo)
  if (check.rows==FALSE){
    stop("amount of rows in table info is not equal to rows in abundance tables")
  }
  
  if (check.rows==TRUE){
    tmp.abu <- dataQ2[c("nomcientifico", "n_larvas")] # Tabla abundancias
    tmp.abu$nomcientifico <- droplevels(as.factor(tmp.abu$nomcientifico)) # Remove unused levels
  
  # Format abunance table from long to wide
    tmp.sps <- split(tmp.abu, f=tmp.abu$nomcientifico)
    for(i in 1:length(tmp.sps)){
      tmp.sps[[i]] <- tmp.sps[[i]][,2]
    }
    tmp.table <- as.data.frame(do.call(cbind, tmp.sps)) 
    # Al hacer esto, quedan las columnas de n_larvas con el nombre científico como nombre de variable

  # Unimos las dos tablas anteriores reformateadas
  tmp.abu2 <- cbind(tablainfo, tmp.table)
  }
}

names(tmp.abu2)[which(names(tmp.abu2) %in% sps)] <- gsub(" ", "_", names(tmp.abu2)[which(names(tmp.abu2) %in% sps)])

aggregate(tmp.abu2$grupo_para_procesar, by=list(tmp.abu2$grupo_para_procesar), length)
#aggregate(tmp.abu2$grupo_para_procesar, by=list(tmp.abu2$sppid), length) # we have removed 'sppid' variable


#-------------------------------
# 3. Query 3:  Renombrar columnas: 
#-------------------------------
# Access: 03_abundancias con datos operación
tmp.abu3 <- tmp.abu2                                  

old.names <- c("idoperacion_num", "orden_bd", "campania", "horallegada", 
"codestacion", "latitudoperacion", "longitudoperacion", "id_colector_autonum",
"estructura", "m3", "profundidad_max", "tipo_pesca")

new.names <- c("id_operation", "station_order", "survey", "hour", 
"station_id", "lat", "lon", "id_netcolector", 
"gear", "volume", "towdepth", "fishing_type")

# old.names_id <- which(names(tmp.abu2) %in% old.names)  # Con el which no funciona porque proporciona los números
#                                                          ordenados de forma creciente!!!!
oldnams_id <- match(old.names, colnames(tmp.abu2))
names(tmp.abu3)[oldnams_id] <- new.names


# Format data of operations
tmp.abu3$mydate <- tmp.abu3$fecha_estacion_llegada
#tmp.abu3$year <-  format(as.Date(tmp.abu3$mydate), "%Y")
#tmp.abu3$month <-  format(as.Date(tmp.abu3$mydate), "%m")
#tmp.abu3$day <-  format(as.Date(tmp.abu3$mydate), "%d")
# tmp.abu3$mydate <- as.Date(tmp.abu3$mydate)

tmp.abu3$year <-  format(tmp.abu3$mydate, "%Y")
tmp.abu3$month <-  format(tmp.abu3$mydate, "%m")
tmp.abu3$day <-  format(tmp.abu3$mydate, "%d")



# Format time of operations
tmp.abu3$hour <- format(tmp.abu3$hour, "%H:%M:%S")

# Select columns to be kept
cols2keep <- c("id_operation", "id_netcolector", "survey", "station_order", "station_id",
  "larval_index", "fishing_type", "lat", "lon", "mydate", "year",
  "month", "day", "hour", "hournorm", "gear", 
  "cont_estructura", "flag_procesado", "malla",
  "towdepth", "volume", "fecha_estacion_llegada",
  "revisita", "fecha_sistematic", "tipo_estacion",
  "selec_analisis", 
  names(tmp.abu3)[c(which(names(tmp.abu3)=="grupo_para_procesar"): (which(names(tmp.abu3)=="mydate")-1))],
  "idIBAMAR") 

dataQ3 <- tmp.abu3
dataQ3 <- dataQ3[which(names(dataQ3)%in% cols2keep)]

# Pensar si hay que conservar más variables
# En teoría no, pero para las no estándard nos iría bien conservar más variables
# colskeep <- c("id_target_num", "id_colector_autonum", "id2", "id1",
#                 "campania", "codestacion", "orden_bd", "idoperacion_num",
#                 "revisita", "fecha_sistematic", "tipo_estacion", "idIBAMAR",
#                 "fecha_estacion_llegada", "horallegada", "hournorm", 
#                 "latitudoperacion", "longitudoperacion", "id_pesca_num",
#                 "tipo_pesca", "estructura", "cont_estructura", "flag_procesado",
#                 "malla", "colectorn", "profundidad_max", "m3", 
#                 "selec_analisis", "larval_index", "grupo_para_procesar")  


#-------------------------------
# 4. Adjustments for non-standard species
#-------------------------------
# Check if the species processed has all tpt_larval_index equal to NA
na.larvalindex <- all(is.na(dataQ3$larval_index))
anyNAlarvind <- any(is.na(dataQ3$larval_index))

# Then, apply the adjustments 
if (process_specifs$grupo_para_procesar=="Xiphiidae"){
#if (na.larvalindex==TRUE){
  print("Warning!!!! Non-standard species selected thus all 'tpt_larval_index' equal to NA!!! ")
  print("The 'larval_index' information will be retrieved from 'Tunidos' processing group")
  answer4 <- menu(c("Y", "N"),
  title="Do you want to proceed?")
  if (answer4==2){
    dataQ4 <- dataQ3
  } else {
  
    if (!exists("databasefile")){
    # Case 3:   
    print("ecolarv databasefile path is missing!!")
    answer3 <- menu(c("Y", "N"),
    title="Do you want to select the Access database")
      if(answer3==1){
      databasefile <- file.choose ()
      }
      if(answer3==2){
      print("Abundance table cannot be processed")
      stop()
      }
    } 
    
    #*****************
    # Prompt to select those values of larval index to be retrieved from Tunidos in the following step
    ANSWER <- menu (c("1", "2", "1 & 2", "All (1,2,3,4 & NA)"), 
      title="Which values of 'tpt_larval_index' do you want to include?")
      if(ANSWER==1) larvind <- c(1) 
      if(ANSWER==2) larvind <- c(2)
      if(ANSWER==3) larvind <- c(1,2)
      if(ANSWER==4) larvind <- c(1,2,3,4, NA) # No deixa emprar les pesques amb larval index == 5 (necessari per alguna cosa?) 
    print("Retrieving BFT larval index data")
    print ("Two filters will be applied: (1) According to your previous selection and (2) Using grupo_para_procesar=Tunidos.") 
    # End of prompt
    #*****************
    
    # Function to get larval index information from BFT, for non standard species
    tmp1.1 <- getBFTlarvalind(databasefile, larval_index=larvind) 
    print("'tpt_larval_index' for bluefin tuna will be added.") # A dia d'avui (24/11/2023) no l'empr per res
                                                                #, però ho deix per si de cas ho volguessim per alguna cosa
                                                                # Hauria de servir per comprovar coses de la BD
 
    # Add retrieved tpt_larval_index for Tunidos to Abundance_df
    merge7 <- merge(dataQ3, tmp1.1, by.x="id_netcolector", by.y="tpt_id_colector_autonum_bft", all.x=TRUE) 
    dataQ4 <- merge7
    
    # Avoid NA larval index in any not 'Tunidos' species (eg. Xiphias)  (13/01/2022)
    if (ANSWER<=3) dataQ4 <- dataQ4[!is.na(dataQ4$tpt_larval_index_bft),] 
    
  }
  
  # Actualize 'process_species' segons la selecció anterior
  print("'process_specifs' will be actualized with your selection and .RData file will be created or updated if already existed.")
  process_specifs$larval_index <- larvind
  process_specifs$info_tpt_larv_ind <- c("larval index data retrieved from Tunidos")
  # Creates or actualize .RData, if it already existed
  # processfile <- paste(propath, "00_0_process_specifications_", paste(process_specifs$sppid , collapse="_"), "_", Sys.Date(), ".RData", sep="")
  processfile <- paste(propath, "00_0_process_specifs_", paste(process_specifs$FAO_X3A_Code, collapse="_"), "_", Sys.Date(), ".RData", sep="")
  save(process_specifs, file=processfile)
}

#-------------------------------
# 5. Stop processing if some larval index has NA value
#-------------------------------
# if (na.larvalindex==FALSE & anyNAlarvind==TRUE){
#   print("Some record has larval index equal to NA")
#   print(dataQ3[anyNAlarvind,]) # Aquí podríem seleccionar només les columnes interessants
#   stop("Please, check the Access DataBase and re-run the script!!")
# }

#-------------------------------
# 6. Dealing with fishing_type == NA
#-------------------------------  

## For the "grupo_para_procesar" other than Tunidos or Xiphiidae, fishing type == NA
## won't be filtered

# if (na.larvalindex==FALSE){  # For standard species
if (process_specifs$grupo_para_procesar=="Tunidos"){   
  if (any(is.na(dataQ3$fishing_type))){
    x <- is.na(dataQ3$fishing_type)
    y1 <- (dataQ3$larval_index==1)
    y2 <- (dataQ3$larval_index==2)

    warn1 <- paste("Warning!! No abundance table will be created. ",
                  " There are standard survey hauls (larval index ==1)  ",
                  "without an assigned 'fishing type'. Please check 'ecolarv' database", 
                  sep="")
                  
    warn2 <- paste("Warning!! Non standard survey hauls have non-assigned 'fishing type'. ",
                  "NA values are replaced by 'NI', meaning non-identified. ", sep="")                
    if (nrow(dataQ3[x&y1,])>0) stop(warn1)
    if (nrow(dataQ3[x&y1,])==0) {
      print(warn2)
      # Assign NI to missing fishing_type when it is not from the standard survey
      levels(dataQ3$fishing_type) <- c(levels(dataQ3$fishing_type), "NI")
      dataQ3$fishing_type[is.na(dataQ3$fishing_type)] <- "NI" # "Not identified"
    }
  }
  dataQ4 <- dataQ3
}

#if (na.larvalindex==TRUE){  # For non-standard species
if (process_specifs$grupo_para_procesar=="Xiphiidae"){  # For non-standard species
  print("Processing non standard species")
  print("Some hauls have fishing type equal to NA. 'NI' is assigned instead!!")
  levels(dataQ4$fishing_type) <- c(levels(dataQ4$fishing_type), "NI")
  dataQ4$fishing_type[is.na(dataQ4$fishing_type)] <- "NI" # "Not identified" 
}

# A dataQ4 hi ha estacions repetides, és això correcte?
# Per què hi ha estacions repetides?
# POTSER QUE LES ESTACIONS DE IBAMAR SÓN MÉS GRANS QUE 

#-------------------------------
# 7. Create usefull variables
#-------------------------------

#### 7.1. Format fecha as.Date
# dataQ4$mydate <- as.Date(dataQ4$mydate)

#### 7.2 Renema "mydate" to "date"
names(dataQ4)[names(dataQ4)=="mydate"] <- "date"; head(dataQ4)


#### 7.3. Set -9999 and -999 to NA
if (length(which(dataQ4==-99))>0) dataQ4[dataQ4 == -99]=NA   
if (length(which(dataQ4==-999))>0) dataQ4[dataQ4 == -999]=NA   
if (length(which(dataQ4==-9999))>0) dataQ4[dataQ4 == -9999]=NA   

nrow(dataQ4)
names(dataQ4)

#### 7.4. Julian day
# library(date)
# t2$jd <- as.numeric(mdy.date(t2$month,t2$day,1960))
require(lubridate)
dataQ4$jd <- yday(dataQ4$date)

#### 7.5. Categorise hour day in factor  
#  noche menor que 0.23 y mayor que 0.93  
hamanece=0.27# sale de 6.5/24
hanochece=0.89 # sale de 21.5 / 24
deltacrepusculo=0.04 # sale de 1/24.. osea 1 hora de crepusculo

# cathour
dataQ4$cathour=NA
dataQ4$cathour[dataQ4$hournorm>=(hamanece+deltacrepusculo) & dataQ4$hournorm<=(hanochece-deltacrepusculo)]="day"
dataQ4$cathour[dataQ4$hournorm<(hamanece-deltacrepusculo)| dataQ4$hournorm>(hanochece+deltacrepusculo)]="night"
dataQ4$cathour[is.na(dataQ4$cathour)]="crep"
dataQ4$cathour=as.factor(dataQ4$cathour)

#require(ggplot2)                      
#qplot(dataQ4$cathour,dataQ4$hournorm)

# daynight
hamanece=0.27# sale de 6.5/24
hanochece=0.89 # sale de 21.5 / 24
dataQ4$daynight=NA
dataQ4$daynight [dataQ4$hournorm>=(hamanece) & dataQ4$hournorm<=(hanochece)]="day"
dataQ4$daynight [dataQ4$hournorm<(hamanece) | dataQ4$hournorm>(hanochece)]="night"
dataQ4$daynight

#---------------------------------
# Alternative way of defining day, night and dusk
#---------------------------------
# Faltaría definir lo que queremos que sea crepúsculo (que es dusk)

# myhour <- format(strptime(dataQ4$hour,"%H:%M:%S"),'%H:%M:%S')
# myhour2 <- as.POSIXct(myhour, format="%H:%M:%S", tz="UTC")
# mydayhour <- as.POSIXct(paste(dataQ4$date, myhour), format="%Y-%m-%d %H:%M:%S", tz="UTC") 

# library(suncalc)  # Estimates sunrise and sunset from lon, lat and time data
# keep = c("sunrise", "sunriseEnd", "sunsetStart", "sunset", "dusk", "nauticalDusk",
#   "night", "nadir", "nightEnd", "nauticalDawn", "dawn")  
# ss <- getSunlightTimes(data=dataQ4, keep=keep)

# dn_f <- NULL
# dn_f [mydayhour>=ss$sunrise & mydayhour<ss$sunriseEnd] <- "dawn"
# dn_f [mydayhour>=ss$sunriseEnd & mydayhour<=ss$sunset] <- "day"
# dn_f [mydayhour>ss$sunset & mydayhour<ss$night] <- "dusk"
# dn_f [mydayhour>=ss$night | mydayhour<ss$sunrise] <- "night"

#### 7.6. Lunar phase
require(lunar)
dataQ4$lunar_illum <- lunar.illumination(as.Date(dataQ4$date))
dataQ4$lunar_phase <- lunar.phase(as.Date(dataQ4$date), name=8) # 1: New, 2: Waxing crescent, 3: First quarter
# 4: Waxing gibbous, 5: Full, 6: Waning gibbous, 7: Third quarter, 8: Waning crescent

#### 7.7. NOAA Bathymetry  
require(raster)
bal <- raster(x=paste(getwd(),"/Rsource/sig/ETOPO1_BalearicSea.tif", sep="")) # load NOAA bathymetry from local drive
depth <- extract(bal, dataQ4[c("lon", "lat")]) # Extract bathymetry at sample stations
dataQ4 <- data.frame(dataQ4, bottom_dep=-(depth)) # Add bottom depth information to your original data.frame

#### 7.8 Slope analysis
proj4string(bal) <- "+proj=longlat +datum=WGS84 +no_defs" # Set projection to the raster bathymetry
slope_bal <- terrain(bal, opt="slope", unit="degrees") # Compute slope in degrees
slope_at_stations <- extract(slope_bal, dataQ4[c("lon", "lat")])  # Extract slope at sample stations
dataQ4 <- data.frame(dataQ4, slope=slope_at_stations) # Add slope information to your original data.frame

#### 7.9. Distance to the nearest coastal point
require(sp)
require(maps)

pts <- as.matrix(dataQ4[c("lon", "lat")])  # Locations of sample stations as a matrix
mp <- map("world", plot=FALSE) # get simple world map data from 'maps' library
xy.coast <- cbind(mp$x, mp$y)[!is.na(mp$x), ] # convert coords from the map to matrix and dump NA

## select a neighbourhood in which the calculations will be performed
xy.coast <- xy.coast[(xy.coast[,1]>=(-5) & xy.coast[,1]<=5 & xy.coast[,2]>=35 &xy.coast[,2]<=43),]  

## compute closest on land positions
closest.points <- matrix(0, ncol = 2, nrow = nrow(pts)) # container for all the nearest points matching the input

for (i in 1:nrow(pts)) {
   closest.points[i, 1:2] <- xy.coast[which.min(spDistsN1(xy.coast, pts[i, 1:2], longlat = TRUE)), ]
}

# Mapping the closest points on the coast
# map(mp, xlim=c(-5, 5), ylim=c(35, 43))
# points(pts, col="black", pch=19)
# points(closest.points, col="red", pch=19)

## compute minimum distance to coast from each of the sampling stations
dist2coast <- matrix(0, ncol=1, nrow=nrow(pts)) # container for distance to coast

for (i in 1:nrow(pts)){
  dist2coast[i] <- spDistsN1(t(as.matrix(pts[i,])), t(as.matrix(closest.points[i,])), longlat=TRUE)
}
dataQ4 <- data.frame(dataQ4, dist2coast=dist2coast) # Add distance to coast (Km) to your original data.frame

#### 7.10. Set tow depth temp
#dataQ4$fishing_type[is.na(dataQ4$fishing_type)] <- "NI"  #### Added for swordfish 2023/11/24 !!!!!!!!!!!!!!

dataQ4$towdepthtemp <- factor(dataQ4$fishing_type, 
  c("deep_oblique", "mixed_layer_oblique", "subsurface", "Oblicua", "NI"), 
  # labels=c("70", "30", "5", "70", "70")) 
  labels=c("70", "30", "5", "70", "30")) # Se modifica de 70 a 30 les NI
  # ja que les profunditats de les pesques dels gapyears van de 8 a 45, mitjana 21
dataQ4$towdepthtemp <- as.numeric(levels(dataQ4$towdepthtemp)[dataQ4$towdepthtemp])

#-------------------------------
# 8. Query 4:  Añade datos de CTD 
#-------------------------------
CTD_bluefin <- getCTDdata(databasefileCTD, year=FALSE, surface=surf)
Abundance_df <- merge(dataQ4, CTD_bluefin, by.x="idIBAMAR", by.y="ID_IBAMAR", all.x=TRUE) 

CTD_blue <- getCTDdata(databasefileCTD, year=TRUE)
CTD_Q4 <- merge(dataQ4["idIBAMAR"], CTD_blue,  by.x="idIBAMAR", by.y="ID_IBAMAR", all.x=TRUE)  

#-------------------------------
# 9. Save Abundance and CTD file
#-------------------------------
print("----------------------------------------------------")
print(paste("Number of collectors by year for 'grupo para procesar' == '", unique(Abundance_df$grupo_para_procesar), "'", sep=""))
print(tapply(Abundance_df$id_netcolector, Abundance_df$year,length))

# nsps <- gsub(" ", "_", process_specifs$nomcientifico)
nsps <- gsub(" ", "_", process_specifs$FAO_X3A_Code)

if(length(sps)==1){
    write.table(Abundance_df, paste(dpath, 
    "abundance_larv.index_", nsps, "_", Sys.Date(),".csv", sep=""), row.names=FALSE, sep=HCUsep, dec=HCUdec)

    if(independentCTD==TRUE){
      write.table(CTD_Q4, paste(dpath, 
      "CTD_larvind_", nsps, "_", Sys.Date(),".csv", sep=""), row.names=FALSE, sep=HCUsep, dec=HCUdec)
#    rm(tmp.abu3, tmp.abu2, cols2keep, dataQ1, dataQ2)   # remove unnecessary objects
    }
}                     


if (length(sps)>1){
    write.table(Abundance_df, paste(dpath, 
    "abundance_larv.index_multispecies_", 
    paste(process_specifs$FAO_X3A_Code, collapse="_"),
    "_", Sys.Date(),".csv", sep=""), row.names=FALSE, sep=HCUsep, dec=HCUdec)

    if(independentCTD==TRUE){
      write.table(CTD_Q4, paste(dpath, 
      "CTD_larvind_multispecies_", 
      paste(process_specifs$FAO_X3A_Code, collapse="_"),
      "_", Sys.Date(),".csv", sep=""), row.names=FALSE, sep=HCUsep, dec=HCUdec)
    }
    
#    rm(tmp.abu3, tmp.abu, tmp.abu2, tmp.sps, tmp.info, tmp.table, tablainfo, cols2keep, dataQ1,
#    dataQ2, i)  # remove unnecessary objects
}

# Per Thunnus thynnus i Thunnus alalunga, aquí Abundance_df té
# [1] 2099   35

# Però més amunt en tenia 4000 i pico, per què?; perquè hi havia totes les pesques duplicades per estar
# processant 2 espèciess

objs2remove <- c("warn1", "warn2", "anyNAlarvind", "x", "y1", "y2", "tmp.abu3", 
  "tmp.abu", "tmp.abu2", "tmp.sps", "tmp.info", "new.names",
  "tmp.table", "tablainfo", "cols2keep", "dataQ1", "dataQ2", "i", "dataQ3", "dataQ4",
  "CTD_bluefin", "CTD_blue",
  "pts", "mp", "xy.coast", "closest.points", "dist2coast",
  "hamanece", "hanochece", "bal", "depth")
        
target <- ls()[ls() %in% objs2remove]
rm(list=target)
rm(target)
  
#-----------End of the script !! -----------------------------------------------