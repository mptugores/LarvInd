#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN, PANDORA
# Autores: M.P. Tugores
#
# Date: 2022/04/01
#-------------------------------------------------------------------------------

#--------------- Check the existence of a list of objects in the R console ----#
#------------------ and append the existing ones into a data.frame ------------#
ifexists_data_frame <- function(x){
    # x: vector containing the names of the objects to be checked between colons,
    # for example: x=c("y", "z", "mydata")
    #   Objects in 'x' should be data.frames 
    #           with the same amount of rows 
    #           and 1 to "n" number of columns
    tf <- NULL; for (i in x) tf <- c(tf, exists(i))
    x2 <- na.omit(x[tf]); x2
    tflist <- list(); for (j in 1:length(x2)) tflist[[j]] <- get(x2[j])
    out <- do.call(data.frame, tflist)
    names(out) <- x2
    return(out)
  }

#------------------ Check the existence of a file in a specified folder -------#
filecheck <- function(x) {
  check1 <- sapply(
    x, 
    function(x){
      file.exists(x)
    }
  )
  if(all(check1)) print("All files exist and are placed in the corresponding folder.")
  if(!all(check1)) {
    print(names(check1)[FALSE])
    stop("Stop processing. Missing files.")
  }
}


#---------------------- Save .csv and .xlsx -----------------------------------# 
save.csvxlsx <- function(x, path, file_out){
  write.table(x, paste(path, file_out, ".csv",sep=""), quote=FALSE, row.names=FALSE, sep=HCUsep, dec=HCUdec) 
  require(writexl)
  write_xlsx(as.data.frame(x), path=paste(path, file_out, ".xlsx",sep="")) 
  #library(xlsx)
  #write.xlsx(summarylengths, paste(opath, fileout, ".xlsx",sep=""), sheetName="Sheet1", showNA=TRUE, row.names=FALSE)
}

#---------------------- Count & Identify NAs ----------------------------------# 
countNAs <- function(x){length(identifyNAs(x))}
identifyNAs <- function(x){which(is.na(x))}

checkNAs <- function(x){ 
  # Warnings if there are NA values  #THIS LINES COULD BE A FUNCTION
  if(any(is.na(x))==FALSE)  print("No NA values in the data.frame")
  if(any(x==(-999))==FALSE) print("No -999 values in the data.frame")
  if(any(x==(-9999))==FALSE) print("No -9999 values in the data.frame")
  
  if(any(x==(-9999))==TRUE) {
    print("Warning!! Some variable has -9999 values in the data")
    summary(x)
  }
  
    if(any(x==(-999))==TRUE) {
    print("Warning!! Some variable has -999 values in the data")
    summary(x)
  }

  if(any(is.na(x))==TRUE) {
    print("Warning!! Some variable has NA data")
    summary(x)
  }
} 

#---------------------- Reading Current User Settings -------------------------#
HCUsep <- readRegistry("Control Panel\\International", hive="HCU")$sList # Column separator
HCUdec <- Sys.localeconv()["decimal_point"]  # Decimal points

#-------------------------- Funcions ------------------------------------------#
# Read .csv file with specific settings
read.csv3 <- function(x, ...){
    if("data.table" %in% installed.packages()){
      library(data.table)
      output <- as.data.frame(fread(x, stringsAsFactors=TRUE))
    }
    if("data.table" %in% installed.packages()==FALSE){
      output <- read.csv(x, sep=",", dec=Sys.localeconv()["decimal_point"])
      if (ncol(output)==1){
        output <- read.csv(x, sep=";", dec=Sys.localeconv()["decimal_point"])
      }        
    }
    output 
}

# Function to print warnings in case of duplicates being found in a variable
check.duplicates <- function(datf, vari, out=TRUE){
  dataf <- get(datf)
  x <- dataf[vari]
  
  #print(x)
  if (any(duplicated(x))==FALSE) {
    ch_warn <- paste("Perfect! No duplicates found in variable '", vari, "' within '", datf, "' data.frame", sep="") # any x is duplicated
    print(ch_warn)
  }
  #if (!all(duplicated(x)==FALSE)) {
  if(any(duplicated(x))==TRUE) {
    ch_warn <- paste("Warning! Duplicates found in variable '", vari, "' within '", datf, "' data.frame", sep="") # if it is FALSE, there are duplicates
    print(ch_warn)
    if(out==TRUE) output <- dataf[duplicated(x),]
    return(output)
   } 
}

# Check the amount of decimal places
num.decimals <- function(x) {
    stopifnot(class(x)=="numeric")
    var1 <- grepl(".", x, fixed=TRUE)
    if(var1==TRUE){
    x <- sub("0+$","",x)
    x <- sub("^.+[.]","",x)
    return(nchar(x))
    }
    if(var1==FALSE){
    0
    }
}

# Redondeo estilo Access
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Merge que chequea las columnas de las tablas a unir
# y excluye las columnas duplicadas
mymerge <- function(x, y, by.x, by.y, all.x=TRUE, all.y=TRUE){
  tmp.y <- data.frame(y[by.y], y[c(!names(y) %in% names(x))])
  names(tmp.y)[1] <- by.y
  out <- merge(x, tmp.y, by.x=by.x, by.y=by.y, all.x=all.x, all.y=all.y)
  return(out)
}

#------------------------------------------------------
# filter.func () 
# Applies recursive filtering (should be renamed to 'recursive.filter')
#
# x: data.frame to be filtered
# process: object of class list with elements to process the filtering; names of the
#          elements should be equal to some of the column names of the data.frame if 
#          some filtering is expected to be produced
#------------------------------------------------------
# The function applies the filters in a recursive manner,
# the amount of objects in 'filters' will equal to the filters applied
# Finally, only the last element of the list is retrieved as it is the data.frame 
# on which all the filters have been applied
#------------------------------------------------------
filter.func <- function (x, process){
  # x: data.frame to be filtered
  # process: object of class list with elements to process the filtering
  filters <- list()
  
  cols2filt <- names(x)[names(x) %in% names(process)]   
  # names of the columns in the data.frame being processed also contained 
  # in the names of the objects in 'process'
  
  # A) If there are no columns with coincident names; no filtering is bein applied.
  # Hence, 'filters' will contain the input data.frame (i.e. 'x')
  if (length(cols2filt)==0){
    print(paste("None of the columns in the table being processed",
    " coincides with any of the names of the objects in the process specifications list.", sep=""))
    print("Hence, no filtering is being applied")
    filters[[1]] <- x
  }
  # B) If there are columns with coindicent names 
  if(length(cols2filt)>0){
    for (i in 1:length(cols2filt)){ 
      col.name <- cols2filt[i]
      proc.name <- process[names(process) %in% col.name] # retrieve the name column being processed
      if(i==1){
        tmp <- x  # In the first run, we are filtering on the 'x' data.frame
      }
      if (i>1) {
        tmp <- filters[[i-1]] # In the following runs, we will filter on the previously filtered data.frame
      }
      filt.column <- tmp[,which(names(tmp)==col.name)]  # select from the data.frame 'x' column to filter
      tmp.filt <- c(filt.column %in% proc.name[[1]]) # vector of TRUE/FALSE for filtering
      filters[[i]] <- tmp [tmp.filt,]
      
      if (nrow(filters[[i]])==0){
        print(paste("Filtering ", cols2filt[i], " on table being processed yields 0 rows selected", sep=""))
        print(paste("The values in the column ", col.name, " of the table being processed are the following", sep=""))
        print(levels(as.factor(filt.column)))
      }
      if(nrow(filters[[i]])==nrow(tmp)){
        print(paste("Filter ", i, " regarding ", cols2filt[i], " does not produce any effective filtering" , sep=""))
      }
      if(nrow(filters[[i]])!=nrow(tmp)){
        nrowsremoved <- nrow(tmp)-nrow(filters[[i]])
        print(paste("Filter ", i, " on ", cols2filt[i], " has removed ", nrowsremoved, " rows from the data.frame",  sep=""))      
      }
    } 
  }
  return(filters[[length(filters)]]) # Ens interessa el darrer dels elements, 
                             # que és el que tendrà tots els filtres aplicats 
}

#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN
# Autores: M.P. Tugores
# Objective: Format "process_specifs" object and file
# Date: 2021/10/07
#
# INPUTS
# 1) input_process_specifs: object of class list 
# 2) databasefile: manually introduced; full path and name of the Access DB
#
# Requirements
# a) propath (path containing the processing files) is defined; 
#    it can be automatically done with "Rsource/setttings.r"
#
# OUTPUTS
# 1) 
#-------------------------------------------------------------------------------

formatPS <- function(input_process_specifs, databasefile) {
  if(exists("process_specifs")){
    print(process_specifs)
    stop ("A formated 'process_specifs' object already exist")
  } 
  #----------------------------------------------------------------------------#
  # Generate process_specifs if it doesn't exist
  #----------------------------------------------------------------------------#
  if (!exists("process_specifs")){   
    # ifelse(exists("propath"), print("process_specifs will be saved in the processing path 'propath'"), stop("Processing path needs to be defined"))
    if(exists("propath")){
      print("process_specifs will be saved in the processing path 'propath'")
      print(propath)
    }
    if(!exists("propath")){
      stop("Processing path needs to be defined")
    }
    process_specifs <- input_process_specifs
    #-----------------------------------------------------
    # 1.2. Format process_specifs
    #      - Gets numeric codes of the selected species
    #      - and saves 'process_specifs' in a .RData
    #-----------------------------------------------------
    library(RODBC)
    channel <- odbcConnectAccess2007 (databasefile)  # Conectar a la BD Access
    t_especies <- sqlQuery (channel,
      query=paste(
        "SELECT tes_nomcientifico, tes_sppid",
        " FROM t_especies",
        sep="")
    )
    odbcCloseAll()
     
    process_sppid <- with (t_especies,
      tes_sppid[match(process_specifs$tes_nomcientifico, tes_nomcientifico)]
    )
    process_specifs$sppid <- process_sppid  # Introducimos el código de la especie en las especificaciones

    ######### Auto Retrieve FAO 3A Code
    fao <- read.table("Rsource/fao_codes/ASFIS_sp_2019.txt", sep=",", header=TRUE)
    #process_specifs$FAO_X3A_Code <- with(fao, 
    #            as.character(X3A_CODE[Scientific_name %in% process_specifs$tes_nomcientifico]))
    process_specifs$FAO_X3A_Code <- with(fao, 
      as.character(X3A_CODE[match(process_specifs$tes_nomcientifico, fao$Scientific_name)])
    )

    # Renombrar objetos de process_specifs
    length(process_specifs)
    names(process_specifs) <- gsub("tes_", "", gsub("tc_", "", gsub("tpt_", "", names(process_specifs)))) # Renombrar columnas para que tengan todas los mismos nombres

    Date <- Sys.Date()
    process_specifs$Date <- c(as.character(Date), gsub("-", "", Date))
    
    process_specifs$LarvInd_version <- larvind_v # retrieve from the script "version.r"

    process_specifs <<- process_specifs
    
    species <<- process_specifs$nomcientifico

    faocode <<- with(process_specifs, FAO_X3A_Code[nomcientifico %in% species])


    # Aquestes variables potser també podrien anar al "process_specifs"
    sp <- gsub(" ", "_", species) # Species nº larvae variable (raw)
    abvar <- paste(tolower(faocode), "ab", sep="") # Species abundance variable
    abgsvar <- paste(tolower(faocode), "ab_gs", sep="") # Species abundance variable standardised by gear
    ABgsvar <- paste(faocode, "ab_gs", sep="")
    # Guardar el objeto process_specifs como .RData
    # processfile <- paste(propath, "00_0_process_specifications_", paste(process_specifs$FAO_X3A_Code, collapse="_"), "_", Sys.Date(), ".RData", sep="")
    # processfile
    # "D:/BLUEFINproject/001_Indice_Larvario/20210622_Run_larval-index_R5.1.1_mirror/processfiles/00_0_process_specifications_BFT_ALB_2021-10-05.RData"
    # Better use the following line:s
    processfile <- paste(propath, "00_0_process_specifs_", paste(process_specifs$FAO_X3A_Code , collapse="_"), "_", Sys.Date(), ".RData", sep="")
    save(process_specifs, file=processfile)
    
    return(process_specifs)
  }
}
#------------------------------------------------------------------------------#
# Select species   POTSER AIXÒ S'HAURIA DE POSAR EN AQUEST SCRIPT!! 
#------------------------------------------------------------------------------#
# A ver si con esta línea funciona
# species <- process_specifs$nomcientifico

# faocode <- with(process_specifs, FAO_X3A_Code[nomcientifico %in% species])


# Aquestes variables potser també podrien anar al "process_specifs"
# sp <- gsub(" ", "_", species) # Species nº larvae variable (raw)
# abvar <- paste(tolower(faocode), "ab", sep="") # Species abundance variable
# abgsvar <- paste(tolower(faocode), "ab_gs", sep="") # Species abundance variable standardised by gear
# ABgsvar <- paste(faocode, "ab_gs", sep="")
#-------------------------------------------------------------------------------#