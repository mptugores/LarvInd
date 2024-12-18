#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN
# Autores: M.P. Tugores, D. Álvarez-Berastegui
#
# Objective: Check if nlarvas and tll_numero are equal
#
# Associated Acess Query: 
#     02_comprueba nlarvas y tll_numero
#
# Date: 2023/07/26
#-------------------------------------------------------------------------------

#------------------------------------------------------------------------
# INPUTS: 
#      m6.1, process_sppid  (R objects)
# obtained from running script "02_process_LFD.r"
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# OUTPUT: .csv files
# One for each of the processed species, in the folder '/processfiles/'
# Named as: 
#      'check2_on_length.frec_340_Thunnus thynnus_2020-06-22.csv'
#------------------------------------------------------------------------

#----------------
# Check inputs
#----------------
if(!exists("m6.1")){
  print("No 'm6.1' object is found")
  print("Before running this check, you need to run '02_process_LFD.r' script")
  answer <- menu(c("Y", "N"), title="Want to run '02_process_LFD.r' now?")
 
  if(answer==1){
  source("Rsource/00_2_process_LFD.r")
  }
  if(answer==2){
  stop()
  }
}

#--------------------------
# Check nlarvas y tllnumero
#--------------------------
m6.1_filt <- m6.1[names(m6.1) %in% as.factor(process_specifs$sppid)] 

check2 <- list()
for(i in 1:length(m6.1_filt)){
check2[[i]] <- with(m6.1_filt[[i]], aggregate(tll_numero, 
  by=list(id_grupo_autonum, campania, fecha_sistematic,
   estructura,tipo_estacion, revisita, cont_estructura, codestacion,
   sppid, orden_bd, colectorn, nlarvas),
  FUN=sum)
  )
  names(check2[[i]]) <- c("tg_id_grupo_autonum", "tc_campania", "to_fecha_sistematic",
   "tc_estructura","to_tipo_estacion", "to_revisita", "tc_cont_estructura", 
   "tg_codestacion", "sppid", "tg_orden_bd", "tg_colectorn", "tg_nlarvas", "SumaDetll_numero")  
}

for(i in 1:length(check2)){
  check2[[i]]$dif <- check2[[i]]$tg_nlarvas - check2[[i]]$SumaDetll_numero
}

for (i in 1:length(check2)){
  print(names(m6.1_filt)[i])
  print(process_specifs$nomcientifico[process_specifs$sppid %in% names(m6.1_filt)[i]])
  print("The differences between tg_nlarvas and SumaDetll_numero ranges between:")
  print(range(na.omit(with(check2[[i]], dif))))
  if(any(abs(range(na.omit(with(check2[[i]], dif))))>2)){
    print("For some haul the difference is bigger than 2")
    print("Revise the corresponding 'data/processfiles/check2_on_length.frec.csv' file.")
    print("Warning!! Values >2 for surveys before 2022")
    print(" ARE ASSUMED and ARE NOT TO BE CORRECTED")
    print(" to allow STRICT UPDATES of larval index.")  
  }
  print("--------------------------")
}

summary(check2[[1]]$tg_nlarvas-check2[[1]]$SumaDetll_numero)

#write.csv(check2340, file=paste(pepath, "check2_for340.csv", sep=""), row.names=FALSE)
#write.csv(check2, file=paste(pepath, "check2.csv", sep=""), row.names=FALSE)

#--------------------------
# Save the output as .csv
#--------------------------
for(i in 1:length(check2)){
  write.table(check2[[i]], paste(propath, "check2_on_length.frec_", process_specifs$sppid[i],
   "_", process_specifs$FAO_X3A_Code[i], "_", Sys.Date(), ".csv", sep=""), row.names=FALSE, sep=HCUsep, dec=HCUdec)
}
 
################################################################################