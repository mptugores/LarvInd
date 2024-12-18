#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN, PANDORA
# Autores: M.P. Tugores
#
# Date: 2021/11/03
#-------------------------------------------------------------------------------


#-------------------------------
# 1.1. Conect ecolarv DB 
#      & load tables of interest in an object of class 'list'
#-------------------------------
getTabsAbu <- function(databasefile){
  library(RODBC)
  channel <- odbcConnectAccess2007 (databasefile)  # Conectar a la BD Access

  t_operaciones <- sqlQuery(channel,
    query=paste(
      "SELECT", 
      " to_campania & '_' & to_codestacion & '_' & to_orden_bd AS id1,",
      " to_campania, to_codestacion, to_orden_bd,",
      " to_idoperacion_num, to_revisita, to_fecha_sistematic, to_tipo_estacion,",
      " to_idIBAMAR, to_fecha_estacion_llegada, to_horallegada,",
      " Format((((Hour([to_horallegada]))*60)+(Minute([to_horallegada])))/1440,'0.0000')+0 AS hournorm,",  
      " to_latitudoperacion, to_longitudoperacion", 
      " FROM t_operaciones",
      sep="")
  )

  t_pescas <- sqlQuery(channel, 
    query=paste(
      "SELECT",
      " tp_campania & '_' & tp_codestacion & '_' & tp_orden_bd AS id1,",
      " tp_campania & '_' & tp_codestacion & '_' & tp_orden_bd & '_' & tp_estructura & '_' & tp_cont_estructura AS id2,",
      " tp_id_pesca_num, tp_tipo_pesca, tp_estructura",
      " FROM t_pescas",
      sep="")
  )
 
  t_colectores <- sqlQuery(channel, 
    query=paste(
      "SELECT",
      " tc_campania & '_' & tc_codestacion & '_' & tc_orden_bd & '_' & tc_estructura & '_' & tc_cont_estructura AS id2,",
      " tc_campania & '_' & tc_codestacion & '_' & tc_orden_bd & '_' & tc_estructura & '_' & tc_cont_estructura & '_' & tc_colectorn AS id3,",
      " tc_estructura, tc_cont_estructura,", 
      #" tc_id_colector_autonum, tc_id_operacion_num, tc_flag_procesado,",
      " tc_id_colector_autonum, tc_id_operacion_num,",
      " tc_malla, tc_colectorn, tc_profundidad_max, tc_m3", 
      " FROM t_colectores", 
      sep="")
  )

  t_target_nombres <- sqlQuery(channel, 
    query=paste(
      "SELECT ttn_id_target_num, ttn_nombre AS grupo_para_procesar",
      " FROM t_target_nombres",
      sep="")
  )

  t_especies <- sqlQuery (channel,
    query=paste(
      "SELECT tes_nomcientifico, tes_sppid",
      " FROM t_especies",
      sep="")
  )
     
  t_proces_target <- sqlQuery (channel,
    query=paste(
      "SELECT tpt_id_colector_autonum, tpt_id_target_num,",
      " tpt_selec_analisis, tpt_larval_index",
      " FROM t_proces_target", 
      sep="")
  )
 
  t_target_spp_composicion <- sqlQuery (channel,
    query=paste(
    "SELECT tts_autonum, tts_id_target_num, tts_sppid",
    " FROM t_target_spp_composicion",
    sep="")
  )

  t_infogrupo <- sqlQuery(channel, 
    query=paste(
      "SELECT",
      " tg_campania & '_' & tg_codestacion & '_' & tg_orden_bd & '_' & tg_estructura & '_' & tg_cont_estructura & '_' & tg_colectorn & '_' & tg_sppid AS id_unico,",
      " tg_campania & '_' & tg_codestacion & '_' & tg_orden_bd & '_' & tg_estructura & '_' & tg_cont_estructura & '_' & tg_colectorn & '_' & tg_sppid AS id4,",
      " tg_campania & '_' & tg_codestacion & '_' & tg_orden_bd & '_' & tg_estructura & '_' & tg_cont_estructura & '_' & tg_colectorn AS id3,",
      " tg_campania, tg_codestacion, tg_orden_bd, tg_estructura,",
      " tg_cont_estructura, tg_colectorn, tg_sppid,",
      " tg_nlarvas, tg_todasmedidas AS lengthdata, tg_id_grupo_autonum",
      " FROM t_infogrupo",
      sep="")  
  )
  odbcCloseAll()
  
  #-------------------------------
  # 1.3. Rename columns
  #-------------------------------  
  names(t_operaciones) <- gsub("to_", "", names(t_operaciones))
  names(t_pescas) <- gsub("tp_", "", names(t_pescas))
  names(t_colectores) <- gsub("tc_", "", names(t_colectores))
  names(t_infogrupo) <- gsub("tg_", "", names(t_infogrupo))
  names(t_especies) <- gsub("tes_", "", names(t_especies))
  names(t_proces_target) <- gsub("tpt_", "", names(t_proces_target))
  names(t_target_spp_composicion) <- gsub("tts_", "", names(t_target_spp_composicion))
  names(t_target_nombres) <- gsub("ttn_", "", names(t_target_nombres))

  #-------------------------------
  # 1.4. Put all tables in a list object
  #      and remove independent tables in the R console
  #------------------------------- 
  ecolarv_tables <- list(t_operaciones=t_operaciones,
    t_pescas=t_pescas, t_colectores=t_colectores, t_infogrupo=t_infogrupo,
    t_especies=t_especies, t_proces_target=t_proces_target, 
    t_target_spp_composicion=t_target_spp_composicion,
    t_target_nombres=t_target_nombres)
  
  return (ecolarv_tables)
}

#-------------------------------------------------------------------------------
# getBFTlarvalind: function to get larval index information from BFT
#
# Intended for non-standard species
# Date: 2020/10/01
#-------------------------------------------------------------------------------
                               
getBFTlarvalind <- function(databasefile, larval_index=c(1)) {
  library(RODBC)
  channel <- odbcConnectAccess2007 (databasefile)  # Conectar a la BD Access

  tmp.t_target_nombres <- sqlQuery(channel,
    query=paste(
    "SELECT ttn_id_target_num, ttn_nombre AS grupo_para_procesar",
    " FROM t_target_nombres",
    sep="")
  )
  tmp.t_proces_target <- sqlQuery (channel,
    query=paste(
    "SELECT tpt_id_colector_autonum,",
    " tpt_id_target_num,",
    # " tpt_selec_analisis,"
    " tpt_larval_index",
    " FROM t_proces_target",
    sep="")
  )
  odbcCloseAll()

  # Merge tables
  #-------------------------------
  tmp0 <- merge(tmp.t_proces_target, tmp.t_target_nombres, by.x="tpt_id_target_num", by.y="ttn_id_target_num", all.x=TRUE)
  tmp1 <- tmp0[,!names(tmp0)%in% "tpt_id_target_num"]
  names(tmp1) <- paste(names(tmp1), "bft", sep="_")

  # Filter by larval index==1 and grupo_para_procesar="Tunidos"
  #-------------------------------
  # filters <- list(tpt_larval_index_bft=1, grupo_para_procesar_bft="Tunidos")
  filters <- list(tpt_larval_index_bft=larval_index, grupo_para_procesar_bft="Tunidos")
  tmp1.1 <- filter.func(tmp1, process=filters)
  return(tmp1.1)
}


#-------------------------------------------------------------------------------
# getCTDdata: get & append CTD data to Abunance data.frames
# Date: 2020/10/01
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN
# Autores: M.P. Tugores
#
# Objective: Import tables from "CTD_para_bluefin_actual.accdb"
#
# 1) Check existence of inputs
# 2) Connect and import tables of interest from: "CTD_para_bluefin_actual.accdb"
# 3) Save as an .RData object
#
# Date: 2020/10/01
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# INPUTS
# 1) databasefileCTD: full path of the Access DB to be accessed
#-------------------------------------------------------------------------------
# OUTPUTS
# 1) CTD_bluefin: data frame
#-------------------------------------------------------------------------------

getCTDdata <- function(databasefileCTD, year=FALSE, surface=FALSE) {
  # databasefileCTD: full path of the Access data.base where CTD is found 
  library(RODBC)
  channel2 <- odbcConnectAccess2007 (databasefileCTD) 
  if(year==FALSE & surface==FALSE) {
  CTD_bluefin <- sqlQuery(channel2, 
    query= paste(
    "SELECT prof_mld, TMEZCLA, SMEZCLA, OMEZCLA, suma_flu_zmezcla, ID_IBAMAR",
    " FROM CTD_para_bluefin_actual",
    sep="")
    )
  }
  if(year==TRUE & surface==FALSE){
  CTD_bluefin <- sqlQuery(channel2, 
    query= paste(
    "SELECT fecha_vec_1 AS year, Gradminlat_e AS lat, Gradminlon_e AS lon, FECHA_formato_31 AS mdate,",
    " prof_mld, TMEZCLA, SMEZCLA, OMEZCLA, suma_flu_zmezcla, ID_IBAMAR",
    " FROM CTD_para_bluefin_actual",
    sep="")
    )
  }   
  if(year==FALSE & surface==TRUE) {
  CTD_bluefin <- sqlQuery(channel2, 
    query= paste(
    "SELECT prof_mld, TMEZCLA, SMEZCLA, OMEZCLA, suma_flu_zmezcla, ID_IBAMAR",
    " FROM CTD_para_bluefin_actual",
    sep="")
    )
  }
  if(year==TRUE & surface==TRUE){
  CTD_bluefin <- sqlQuery(channel2, 
    query= paste(
    "SELECT fecha_vec_1 AS year, Gradminlat_e AS lat, Gradminlon_e AS lon, FECHA_formato_31 AS mdate,",
    " prof_mld, TMEZCLA, SMEZCLA, OMEZCLA, Tem5, Sal5, OXI5, suma_flu_zmezcla, ID_IBAMAR",
    " FROM CTD_para_bluefin_actual",
    sep="")
    )
  }   
  odbcCloseAll()
  return(CTD_bluefin)
}


#-------------------------------------------------------------------------------
# getLFDdata: get all length data from ecolarv DB
# Date: 2021/11/03
#-------------------------------------------------------------------------------
getLFDdata <- function(databasefile){
  library(RODBC)
  #---------------------------------
  # 1.1. Load tables from Access
  #---------------------------------
  channel <- odbcConnectAccess2007 (databasefile)  # Conectar a la BD Access
  t_tallas <- sqlQuery(channel,
    query=paste(
      "SELECT",
      " tll_campania & '_' & tll_codestacion & '_' & tll_orden_bd",
      " & '_' & tll_estructura & '_' & tll_cont_estructura",
      " & '_' & tll_colectorn & '_' & tll_sppid AS id4,",
      " tll_estructura,",
      " tll_numero AS nmeasured,",
      " tll_sppid,",
      " tll_talla, tll_numero",
      " FROM t_tallas",
      sep="")
  )
  odbcCloseAll() # Cerramos conexión con la base de datos
  #-------------------------------
  # 1.1.b Assign NA for -999, -9999 values
  #     if there are some
  #-------------------------------  
  if(length(which(t_tallas==-9999))>0)  t_tallas [t_tallas == -9999] <- NA
  if(length(which(t_tallas==-999 ))>0)  t_tallas [t_tallas == -999]  <- NA
  if(length(which(t_tallas==-99 ))>0)   t_tallas [t_tallas == -99]   <- NA
  
  return(t_tallas)
}
#-------------------------------------------------------------------------------#