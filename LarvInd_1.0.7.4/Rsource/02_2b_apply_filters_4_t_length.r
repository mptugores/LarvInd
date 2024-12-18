#-------------------------------------------------------------------------------
# Project: BLUEFIN, PANDORA
# Autors: M.P. Tugores
#
# Objective: Perform filters on the length-frequency data retrieved from the
#            ECOLARV database
#
# 1. Takes 'agQ23'
# 2. Renames <NA> fishing_type to 'NI' (intended for gap years)
# 3. Applies filters regarding surveys and type of fishing: 
#    3.1. Remove 'MEDIAS0710' surveys  
#    3.2. Remove 'subsurface' hauls
# 4. Check and remove duplicates for gap years
# 5. Format date and NA values
# 6. Species specific filters
#    6.1. Removes 2002 and 2003 for Albacore
#    6.2. Sets a maximum size for BFT and ALB
# 
# Date: 2022/04/04
#-------------------------------------------------------------------------------

################################################################################
# Inputs: 
# 'agQ23': list object containing the length frequency data; one element for each
#          analysed species (generated in the script '00_2_get_LFD.r'
# 'process_specifs': list object containing the process specifications for
#                    data extracted from the ECOLARV database
#
# Outputs:
# - csv:  "20200727_t_length_larvind_BFT.csv" 
# - xlxs: "20200727_t_length_larvind_BFT.xlxs" 
################################################################################

############################# NOTES## ##########################################
#                Includes adaptation to be used with GAP years                 #
#                     DON'T CHANGE the code                                    #
################################################################################

for (i in 1:length(agQ23)) {
  ############################################
  # 1. Takes the object 'agQ23' 
  ###########################################
  t0 <- agQ23[[i]]    

  nrow(t0)

  ############################################
  # 2. Rename 'fishig_type' == <NA> to 'NI'
  ############################################
  # For hauls outside the regular years,
  # for which a fishing type has not been set in the ECOLARV database 08/11/2021

  t0$fishing_type <- as.factor(t0$fishing_type)
  levels(t0$fishing_type) <- c(levels(t0$fishing_type), "NI")
  t0$fishing_type [is.na(t0$fishing_type)] <- "NI" 

  #################################################
  # 3. Applies filters on surveys and fishing_type
  #################################################
  
  # 3.1. Remove MEDIAS surveys
  if(any(t0$survey=="MEDIAS0710")==TRUE) {
    t1=t0[!t0$survey=="MEDIAS0710",]
  } else {
  t1=t0
  }
                                            
  # 3.2. Remove subsurface hauls
  # Currently [08/11/2021], subsurface hauls are B90 from 2004 and 2005 
  if (any(na.omit(t1$fishing_type)=="subsurface")==TRUE){
    t1=t1[t1$fishing_type!="subsurface",]  
  } else {
  t1=t1
  }

  names(t1)
  nrow(t1)
  str(t1)

  with(t1, tapply(net_colector, list(gear,year), length))
  with(t1, tapply(net_colector, list(fishing_type,year), length))
  aggregate(t1$towdepth, list(t1$fishing_type, t1$gear, t1$year), summary)
  aggregate(t1$towdepth, list(t1$fishing_type, t1$gear, t1$year), function(x){summary(na.omit(x))}) # Hi ha una pesca de 2011 que no té towdepth

  ############################################
  # 4. Check and remove duplicates for gap years
  ############################################

  t2 <- t1
  # Rename and format variables to reuse previous code
  names(t2)[names(t2)=="codestacion"] <- "station_id"
  names(t2)[names(t2)=="orden_bd"] <- "station_order"
  t2$station_id <- as.character(t2$station_id)   
  # These previous three lines could be before t2=t1 if we want to change names of the variables in the output tables
  
  # Divide data by year
  t2 <- split(t1, f=as.factor(t1$year))

  # Check duplicates by year
  dupls <- lapply(t2, FUN= function(x){x[x$station_id %in% x$station_id[duplicated(x$station_id)],]})

  sum1 <- sum(do.call(c, lapply(dupls, FUN=nrow)))

  if(sum1>0){
    gapyears <- c("2006", "2008", "2010", "2011")
    x2modif <- which(names(t2) %in% gapyears)
    for (i in x2modif){
      t2[[i]] <- do.call(rbind, by(t2[[i]], t2[[i]]$station_id, function(x) x[which.min(x$station_order), ] ))
    }
    t2 <- do.call(rbind, t2)
    dim(t2)
    
    # Put the names of the variables back to their original name
    names(t1)[names(t1)=="station_id"] <- "codestacion"
    names(t1)[names(t2)=="station_order"] <- "orden_bd"
    t1 <- t2
  }

  # If there are no duplicates all the preceding code is not used and we continue working on
  # unmodified t1
  ############################################################################## 

  ############################################
  # 5. Format date and NA values
  ###########################################

  #### 4.1. Fecha as.Date
  class(t1$fecha_estacion_llegada)

  t1$fecha_estacion_llegada <- as.Date(t1$fecha_estacion_llegada)
  names(t1)[names(t1)=="fecha_estacion_llegada"] <- "date"
  head(t1)

  #### 4.2. Set -9999 and -999 to NA
  if (length(which(t1==-999))>0) t1[t1 == -999]=NA   # No es necesario, ya lo detecta. 
  if(length(which(t1==-9999))>0) t1[t1 == -9999]=NA   # No es necesario, ya lo detecta
  # Implementado en los scripts de generación de variables desde la BD 

  nrow(t1)
  names(t1)
  
  agQ23[[i]] <- t1
}

  ############################################
  # 6. Species specific filters
  ###########################################

  length_list <- agQ23

  # 6.3. Para ALB, quitamos el año 2002 y 2003 para albacora
  # if(faocode=="ALB") t1 = t1[t1$year!= 2002 & t1$year!=2003,]
  spnum <- process_specifs$sppid [process_specifs$nomcientifico %in% "Thunnus alalunga"]
  if (any(names(length_list)==spnum)){
    ta <- which(names(length_list)==spnum)
    tmp <- length_list[[ta]]
    tmp <- tmp[tmp$year!=2002 & tmp$year!=2003,]
    length_list[[ta]] <- tmp
  }

  # Para BFT, excluímos 2006 y 2010: HACER UNA QUESTION CON RESPUESTA!!!
  # SHOULD THIS BE IMPLEMENTED??
  # if(faocode=="BFT") t1=t1[t2$year!=2006 & t1$year!=2010,]
  # names(t1)
  # nrow(t1)
  # str(t1)
    
  
###########################################
# 9. Export the output as .csv and .xlxs
###########################################

# Output: length_list
# Exported as separated files for each species

for(i in 1:length(length_list)){
  faocode <- process_specifs$FAO_X3A_Code[process_specifs$sppid==names(length_list)[i]]
  
  tout2=length_list[[i]]
  if(all(is.na(tout2$larval_index))){ 
    tout2$larval_index=tout2$tpt_larval_index_bft
  }
  # Si hago esto, debería eliminar la columna tpt_larval_index_bft y no trabajar sobre ella
  names(tout2)
  head(tout2)
  print(tapply(tout2$net_colector, tout2$year, length))

  outdate <- gsub("-", "", Sys.Date())
  fileout2 <- paste(gsub("-", "", outdate), "_t_length_larvind_", faocode, sep="")

  save.csvxlsx(tout2, path=opath, file_out=fileout2)
  # write.table(tout2,paste(opath, fileout2, ".csv",sep=""), row.names=FALSE, quote = FALSE, sep=HCUsep, dec=HCUdec) 
  # require(writexl)
  # write.xlsx(tout2,paste(opath, fileout2, ".xlsx",sep=""), row.names=FALSE, sheetName="Sheet1",showNA=TRUE)
  # write_xlsx(tout2, path=paste(opath, fileout2, ".xlsx",sep="")) 
}
################################################################################