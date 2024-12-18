#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN
# Autores: M.P. Tugores, D. Álvarez-Berastegui
#
# Objective: Checks on Abundance data table
#
# Associated de Access Queries:
#     01 Comprueba no colectores repetidos
#     01 Comprueba no colectores repetidos
#
# Date: 2021/10/20
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# INPUTS required
# One of the outputs of the Abundance script: '01_process_Abundance_table.r'
#    1) EITHER: 'Abundance_df' located in the R console
#    2) OR: .csv Abundance file 
#           (e.g. 'abundance_larv.index_multispecies_340_342_2020-06-22.csv')
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# OUTPUTS: 
#     Texto en consola: indicando si hay o no duplicados en estaciones y colectores 
#     Ficheros .txt (en /processfiles/): 
#                    Duplicated colectors_Tunidos_2020-06-22.txt
#                    Duplicated stations_Tunidos_2020-06-22.txt
#-------------------------------------------------------------------------------

#----------------
# Check inputs
#----------------
# Abundance_df

if(!exists("Abundance_df")){
  print("No 'Abundance_df' object is found")
  answer <- menu(c("Y", "N"), title="Do you want to select an abundance file to check?")
 
  if(answer==1){
    tmp <- file.choose()
    library(data.table)
    Abundance_df <- data.frame(fread(tmp))
  
    print("You should choose a 'process_specifs.RData' also")
    ans <- menu(c("Y", "N"), title="Do you want to load a process_specifs .RData file?")
    if (ans==2){
      stop("Abundance_df won't be created!!")
    }
    if (ans==1){
      tmpspecifs <- file.choose()
      load(tmpspecifs)
    }
  }
  if(answer==2){
  stop()
  }
  
}

# Declare processing path:   
if (!exists("propath")){
   source("Rsource/relative_folderpaths.R") 
}

#---------------------------------------------------
# Group for processing
#---------------------------------------------------
group <- as.character(unique(Abundance_df$grupo_para_procesar))

#---------------------------------------------------
# Check duplicated colectors and stations
#---------------------------------------------------

# For standardized species (e.g. Thunnus tynnus),
# with no aditionally added larval index from Tunidos
checkbft <- names(Abundance_df)[which(names(Abundance_df)=="grupo_para_procesar_bft")]

if (length(checkbft)==0){
  # Check duplicated colectors
  check1.1 <- with(Abundance_df, 
    aggregate(id_operation, 
    by=list(id_netcolector, id_operation, survey, gear, grupo_para_procesar), 
    length)) 
  names(check1.1) <- c("id_netcolector", "id_operation", "survey", "gear", "grupo_para_procesar", "n")
  # Check duplicated stations
  check1.2 <- with(Abundance_df, 
    aggregate(id_operation,
    by=list(id_operation, survey, gear, grupo_para_procesar), 
    length))
  names(check1.2) <- c("id_operation", "survey", "gear", "grupo_para_procesar", "n")
}

# For non-standardized species (e.g. Xiphias gladius),
# with aditionally added larval index from Tunidos

# Sí está operativo, cuando analizamos, por ejemplo X. gladius (15/04/2021)

if (length(checkbft)==1){
  # Check duplicated colectors
  check1.1 <- with(Abundance_df, 
    aggregate(id_operation, 
    by=list(id_netcolector, id_operation, survey, gear, grupo_para_procesar_bft), 
    length))
  names(check1.1) <- c("id_netcolector", "id_operation", "survey", "gear", "grupo_para_procesar_bft", "n")
  # Check duplicated stations
  check1.2 <- with(Abundance_df, 
    aggregate(id_operation, 
    by=list(id_operation, survey, gear, grupo_para_procesar_bft), 
    length))
  names(check1.2) <- c("id_operation", "survey", "gear", "grupo_para_procesar_bft", "n")
}


#------------------------------------
# Get the output of the colectors check
#------------------------------------
errorfile1 <- paste("check1_Duplicated_collectors_", group, "_", Sys.Date(),".txt", sep="") 
if (max(check1.1$n)==1){
    print(paste("No duplicated collectors were found in ", group, "  !!", sep=""))
    sink(file=paste(propath, errorfile1, sep=""))
    print("No duplicated collectors were found!!")
    sink()
}

if (max(check1.1$n)>1){
    print(paste("There are duplicated collectors for ", group, sep=""))
    print(paste("Please, check '", errorfile1, "' file", sep=""))
    sink(file=paste(propath, errorfile1, sep=""))
    print(check1.1[check1.1$n>1,])
    sink()
}

#------------------------------------
# Get the output of the stations check
#------------------------------------
errorfile2 <- paste("check1_Duplicated_stations_", group, "_", Sys.Date(),".txt", sep="")   
if (max(check1.2$n)==1){
    print(paste("No duplicated stations were found in ", group, "  !!", sep=""))
    sink(file=paste(propath, errorfile2, sep=""))
    print("No duplicated stations were found!!")
    sink()
}

if (max(check1.2$n)>1){
    print("There are duplicated stations")
    print(paste("Please, check '", errorfile2, "' file", sep=""))
    sink(file=paste(propath, errorfile2, sep=""))
    print(check1.2[check1.2$n>1,])
    sink()
}

rm(check1.1, check1.2, errorfile1, errorfile2, group) 

################################################################################