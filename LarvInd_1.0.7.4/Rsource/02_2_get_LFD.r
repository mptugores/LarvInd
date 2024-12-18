#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN
#           Larval index
# 
# Autores: M.P. Tugores, D. Álvarez-Berastegui
#
# Análisis de frecuencia de tallas
#
# Consultas Access: 18_nmeasured  (se requiere para poder llevear a cabo la anterior)
#                   23_analisis_length_frecuencies_grouped_tablafinal
# Date: 2021/10/19              
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Script Inputs (needed to run the script): 
#
# Objetos R: process_specifs, databasefile, ecolarv_filt
# 
#      Del script:"00_main_run_larvalindex.r"
#        process_specifs: debe estar definido nombre científico 'tes_nomcientifico'
#        databasefile
#      Del script: "01_process_Abundance_table.r"
#        dataQ3: es el output del script 01, pero sin datos de CTD
#        t_especies: tabla de especies
#
# BEFORE RUNNING THE SCRIPT it should checked if all the needed objects/information
# is loaded all the 
# PROMPTS, STOP, etc SHOULD BE incorporated to avoid unwanted errors
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# INPUTS required
# 1) round2: function
# 2) mymerge: function
# 3) Abundance_df
# 4) ecolarv_filt
# 5) process_specifs
# 6) databasefile
# 7) dpath: paths to save outputs
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# OUTPUTS: 
#     Objetos R: agQ23, outQ23 (de clase 'list')
#                contienen las distribuciones de talla de las especies
#                Inicialmente agQ23 contenía todas las especies
#                y outQ23 sólo las de procesado. 
#                En esta versión, como filtramos los datos según 'process_specifs', 
#                previo a la realizacion de los análisis:
#                  agQ23 = outQ23
#
#     Ficheros .csv: "23_t_length.frec_abundance_340_Thunnus alalunga_2020-07-22.csv"
#                    "23_t_length.frec_abundance_340_Thunnus thynnus_2020-07-22.csv"
#-------------------------------------------------------------------------------
   
#-------------------------------
# Check inputs
#-------------------------------
if (!exists("databasefile")|!exists("dpath")){
  print("Either the path of the database or the path for output data are missing")
  stop("Please, run '00_main_run_larval_index.r'")
} 

if (!exists("round2") | !exists("mymerge")){
  source("Rsource/functions.R")  
}

if(!exists("Abundance_df") | !exists("ecolarv_filt")){
  print("You need to run script '01_process_Abundance_table_withCTD.r' before runing length frequency analysis")
}

if (exists("Abundance_df") & !exists("process_specifs")){
  print(paste("The object 'process_specifs' with the specifications for the processing", 
  "does not exist", sep=""))
  print("You need to load the file '.RData' containing the process_specifications")
  answer <- menu(c("Y","N"), title="Do you want to load the '.RData' file now?")
  if(answer==1){
    tmp <- file.choose()
    load(tmp)
  }
  if(answer==2){
    stop()
  }
}

#################################
# 1. Create Query 18_nmeasured
#################################

#---------------------------------
# 1.1. Retrieve length data
#---------------------------------
t_tallas <- getLFDdata(databasefile)
head(t_tallas)

#--------------------------------------------------------------
# 1.2. Join/Merge different DB tables
#      to add operaciones, pescas, colectores e infogrupo data
#--------------------------------------------------------------
# 1) Merge t_operaciones and t_pescas     by    id1
# 2) Merge output1 with t_colectores      by    id2
# 3) Merge output2 with t_infogrupo       by    id3
# 4) Merge output3 with t_tallas          by    id4
#--------------------------------------------------------------
merge1 <- mymerge(ecolarv_filt$t_operaciones, ecolarv_filt$t_pescas, by.x="id1", by.y="id1", all.y=TRUE) # Añade información de las pescas a las operaciones
merge2 <- mymerge(merge1, ecolarv_filt$t_colectores, by.x="id2", by.y="id2", all.y=TRUE) # Añade información de los colectores
merge3 <- mymerge(merge2, ecolarv_filt$t_infogrupo, by.x="id3", by.y="id3", all.y=TRUE) 
# We retain from t_tallas only the columns we are interested in
merge3 <- merge3[!is.na(merge3$id_unico),]
merge4 <- merge(merge3, t_tallas[c("id4", "tll_estructura", "nmeasured")], by="id4", all.y=TRUE) # Pone info de los grupos a procesar

#-------------
# Stop the code if all length data is zero 
#-------------
if(all(is.na(merge4$lengthdata))) stop("No length data in 'ecolarv' DB for the species processed!!")

#-------------
# Create a id5 
#-------------
# to aggregate measured larvae by several variables
merge4$id5 <- as.factor(paste(merge4$idoperacion, merge4$campania, merge4$codestacion, 
  merge4$orden_bd, merge4$estructura, merge4$cont_estructura, 
  merge4$colectorn, merge4$sppid, merge4$id_colector_autonum, sep="_"))

##################
#### Agrega nmeasured para cada especie de la o las
#### seleccionadas en el objeto process_specifs
##################

agQ18 <- with (merge4, aggregate(nmeasured, 
  by=list(id5, id_colector_autonum, sppid), 
  function(x){round2(sum(na.omit(x)), 0)}))
names(agQ18) <- c("id5",  "id_colector_autonum", "sppid", "nmeasured")
nmeasured_18 <- agQ18

rm (merge1, merge2, merge3, merge4) 
  
#################################
# 2. Create Query 23_analisis_length_frecuencies_grouped_tablafinal
#################################

#-------------
# Format columns
#-------------
ecolarv_filt$t_especies$nomcientifico <- with(ecolarv_filt$t_especies, gsub(" ", "_", nomcientifico)) 

# Chequear NAs
# summary(t_operacionesQ23)
# summary(t_colectoresQ23)
# summary(t_infogrupoQ23)
# summary(t_especiesQ23)
# summary(t_tallasQ23)

# Indicamos que las tallas con valor igual a -9999 son también NAs
# En la tabla de tallas existen valores reconocidos como NA
# pero que no eran -9999
# Y eliminamos todas las tallas que no tengan un valor no NA
t_tallas$tll_talla[t_tallas$tll_talla==(-9999)] <- NA
t_tallas <- na.omit(t_tallas)

#--------------
# IMPORTANTE: función 'round2'
#--------------
# Redondeamos las tallas a 1 dígito
# Usando una función específica "round2", que redondea igual que Access
t_tallas$tll_talla <- round2(t_tallas$tll_talla, 1) 

# Calcula la frecuencia
# para cada especie y para cada talla 
tallas.agr <- aggregate(t_tallas$tll_numero, 
  #by=list(t_tallas$id4, t_tallas$tll_talla, t_tallas$tll_sppid),function(x){round2(sum(x),1)})
  by=list(t_tallas$id4, t_tallas$tll_talla, t_tallas$tll_sppid),function(x){sum(round2(x,1))})
names(tallas.agr) <- c("id4", "length", "sppid", "frec")
# nchar(strsplit(as.character(tallas.agr$frec), "\\."))


# Si procesamos 1 especie, no hacemos nada
sps <- process_specifs$nomcientifico # especies a procesar
if(length(sps)==1){
  tallas.agr <- tallas.agr
}

# Si procesamos más de una especie, el objeto se convierte en una lista,
# cada elemento es una especie
if (length(sps)>1){
   names4agr <- levels(as.factor(tallas.agr$sppid))
   tallas.agr <- split(tallas.agr, f=tallas.agr$sppid)
   names(tallas.agr) <- names4agr
}
  
#---------------------------------
# 2.2. Join/Merge different DB tables
#---------------------------------
# tallas.agr se uniría con t_infogrupo, por un campo que sea id4
# el resultado, con t_colectores, por un campo igual a id4 pero sin sppid
# el resultado se unirá con t_operaciones mediante el campo id_operacion_num
# el resultado se unirá con dataQ3 para obtener tpt_larval_index, mediante la variable tc_colector_autonum y id_netcolector
# del 18_nmeasured no entenc què he de posar ni com, però si faig tot lo anterior, després ja ho veurem!!

m1.1 <- mymerge(ecolarv_filt$t_infogrupo, ecolarv_filt$t_especies, by.x="sppid", by.y="sppid", all.x=TRUE)
m2.1 <- mymerge(ecolarv_filt$t_operaciones, ecolarv_filt$t_colectores, by.x="idoperacion_num", by.y="id_operacion_num", all.y=TRUE)
m3.1 <- mymerge(m2.1, m1.1, by.x="id3", by.y="id3", all.y=TRUE)
m4.1 <- mymerge(m3.1, t_tallas, by.x="id4", by.y="id4", all.y=TRUE) 

species <- levels(as.factor(m4.1$sppid))
tmp_18 <- nmeasured_18[nmeasured_18$sppid %in% species,] # Com que ara filtram per les espècies a analitzar tmp_18 i nmeasured_18 són iguals

# A partir d'aquí feim els merge per cada espècie
# L'objecte dels merge es converteix en un objecte de classe llista
m4.1 <- split(m4.1, f=m4.1$sppid)
tmp_18 <- split(tmp_18, f=tmp_18$sppid)

m5.1 <- list()
for (i in 1:length(m4.1)){
  m5.1[[i]] <- mymerge(m4.1[[i]], tmp_18[[i]], by.x="id_colector_autonum", by.y="id_colector_autonum", all.x=TRUE)
}

m6.1 <- list()
for (i in 1:length(m5.1)){
  m6.1[[i]] <- merge(m5.1[[i]], ecolarv_filt$t_proces_target[c("id_colector_autonum", "larval_index")], by.x="id_colector_autonum", by.y="id_colector_autonum", all.x=TRUE)
}
names(m6.1) <- species 

#----------------------------
# Generar distribuciones de tallas
#----------------------------
# Genera el output definitivo de la consulta Q23
# agQ23: lista con tantos elementos como especies presentes en la BD

agQ23 <- list() # lista vacía donde se guardarán las LFD
names.agQ23 <- c("id_grupo_autonum", "larval_index", 
  "net_colector", "codestacion", 
  "survey", "gear", "idoperacion_num", "volume", "hournorm",
  "horallegada", "fecha_estacion_llegada", "towdepth", "spp_name",
  "length", "nsampled", "sppid", "orden_bd", "id2", "frec")

# Loop que genera la suma de individuos por talla y lo guarda como elementos de 
# agQ23  
for(i in 1:length(m6.1)){
  agQ23[[i]] <- with(m6.1[[i]], aggregate(tll_numero, 
    by=list(id_grupo_autonum, larval_index, 
    id_colector_autonum, codestacion,
    campania, estructura, idoperacion_num, m3, hournorm,
    horallegada, fecha_estacion_llegada, profundidad_max, nomcientifico, 
    tll_talla, nlarvas, tll_sppid, orden_bd, id2),
    function(x){sum(round2(x,1))})
  )
  names(agQ23[[i]]) <- names.agQ23 # posam noms a cada columna de cada element de la llista
}

names(agQ23) <- species  # posam nom a cada element de la llista
length(agQ23) # contiene tantos objetos como especies tengan tallas en la BD

rm (m1.1, m2.1, m3.1, m4.1, m5.1) 

# Añadiendo información del tipo de pesca
for(i in 1:length(agQ23)){
  agQ23[[i]] <- merge(agQ23[[i]], ecolarv_filt$t_pescas[,c(2,4)], by.x="id2", by.y="id2", all.x=TRUE)
  names(agQ23[[i]])[names(agQ23[[i]])=="tipo_pesca"] <- "fishing_type"
}

# Si hay tipo pesca == NA, ponlo como NI (for gapyears)
for(i in 1:length(agQ23)){
  if (any(is.na(agQ23[[i]]$tipo_pesca))){ 
    agQ23[[i]]$tipo_pesca[is.na(agQ23[[i]]$tipo_pesca)] <- 'NI'
  }
}

# Genero variable año
for(i in 1:length(agQ23)){
  agQ23[[i]]$year <- format(agQ23[[i]]$fecha_estacion_llegada, "%Y")
}



#-------------------------------
# Filtres
#-------------------------------
# Ja aplicats a l'inici. No caldrien
# Només es processen les espècies d'interés
# Seleccionamos sólo las especies para las que hemos llevado a cabo el procesado

# outQ23: lista con tantas especies como hayamos indicado en 'process_specifs'
# En realitat, outQ23 = agQ23
outQ23 <- agQ23[names(agQ23) %in% as.factor(process_specifs$sppid)]

for(i in 1:length(outQ23)){
  write.table(outQ23[[i]], paste(dpath, "length.frec_abundance_", species[i],
   "_", process_specifs$FAO_X3A_Code[process_specifs$sppid==species[i]], 
   "_", Sys.Date(), ".csv", sep=""), row.names=FALSE, sep=HCUsep, dec=HCUdec)
}
############# End of script!!! #################################################