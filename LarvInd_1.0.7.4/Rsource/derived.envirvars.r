#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN, PANDORA
# Autores: M.P. Tugores
#
# Date: 2021/10/21
#-------------------------------------------------------------------------------


############# calculate salinity SALanom #######################################
# Function: yearly anomaly
Year_anom <- function(tdata, environ, year.var="year") {
  # For each value, the mean in the corresponding year is substracted
  # tdata: data.frame
  # environ: name of the variable on which yearly anomaly is to be computed
  tdata[,environ] - ave(tdata[,environ], tdata[,year.var], FUN=function(x)mean(x)) 
}
################################################################################

###########################
# Imputation of missing values
# versión 1.0
fill.missing <- function(tdata, environ, predvarname, type="gam1", impute=TRUE, equation=NULL){
  #-----------------------------------------------------------------------------  
  # Inputs:
  # 1) tdata: data.frame containing the following variables
  # 2) "environ": environmental variable on which to perform imputation, e.g. "SMEZCLA"
  # 3) "lon", "lat", "year": variables containing information on the longitude and latitude 
  # and year, respectively
  # 4) predvarname (e.g. "predsal"): name of the predicted environmental variable according to the imputation model
  #
  # Options:
  # 1) Impute TRUE or FALSE
  # - impute=TRUE: the model is run AND prediction of missing values is performed
  # - impute=FALSE: only the model is run but imputation is not performed
  #
  # 2) Equations predefined or used specified
  #    A. Predefined spatial imputation by means of "gam"
  #       type=="gam1", type=="gam2"
  #       - type="gam1" is the deffault: y~s(lon,lat)+as.factor(year)-1
  #       - type="gam2": y~s(lon,lat, by=as.factor(year))
  #    B. User can specify a different equation, by setting:
  #       - type=NULL and
  #       - equation="y~s(lon,lat)+as.factor(year)-1" (or any other equation the user wants to try)
  #
  # Outputs:
  # (1) data.frame: printed in the R Console (and can be saved as an R object)
  #                 contains the input dataset plus the predicted environmental variable 
  #                 which name has been set in 'predvarname', e.g. "predsal"
  # (2) "mod_SMEZCLA": R object in the GlobalEnv containing the fitted model
  #-----------------------------------------------------------------------------
  
  library(mgcv)
  t1 <- tdata  # temporal data.frame
  t1$y <- t1[,environ]
  
  # Check some model has been specified
  if(is.null(type) & is.null(equation)) stop("Missing equation. No imputation will be performed!")
 
  # Equations to perform NA imputation
  # A. Predefined equations
  if (!is.null(type)){
    if (type=="gam1") formula1 <- formula(y~s(lon,lat)+as.factor(year)-1)
    if (type=="gam2") formula1 <- formula(y~s(lon,lat, by=as.factor(year))) 
  }
  # B. User defined equation
  if (is.null(type)) formula1 <- as.formula(equation) 
    
  mod <- gam(formula1, data=t1, familiy=gaussian) # Running the model
   
  print(summary(mod)) # See model fitting on screen
  plot(mod, all=TRUE)  # Visualise model responses plot
  
  assign(paste("mod_", environ, sep=""), mod, .GlobalEnv) # 
  
  t1$V1 <- predict(mod,t1,type="response") # Prediction of missing values
    
  # Prediction of missing values
  if(impute==TRUE){
    print("Missing values are substituted by model predictions")  
    t1[identifyNAs(t1[,environ]), environ] <- t1$V1[identifyNAs(t1[,environ])]
  }
  
  if (impute==FALSE) print("Missing values are NOT imputed")  
  
  names(t1)[names(t1)=="V1"] <- predvarname
  t1 <- t1[names(t1)!="y"]  # remove the temporal variable

  return(t1)      
}
################################################################################



#-------------------------------------------------------------------------------#