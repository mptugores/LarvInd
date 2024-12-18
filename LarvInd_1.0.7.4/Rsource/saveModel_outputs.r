#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN, PANDORA
# Autores: M.P. Tugores
#
# Objective: Functions to retrieve and save model settings from GAM and/or glm
#
# Date: 2021/12/09
#-------------------------------------------------------------------------------



################################################################################
# Function: "saveModel_ouptputs"                                               #
# Saves model outputs for GAM and GLM models                                   #
################################################################################

# Presence-absence funciona tant per GAMs com per GLMs
saveModel_outputs <- function(mab=NULL, mbin=NULL, tdef,
  modelID, outdate, faocode, modelpathid, pdfplots=TRUE, pngplots=TRUE){
  #----------------------------------------------------------------------------#
  # Input parameters:                                                          #
  # mab: GLM or GAM finally selected abundance model (optional)                #
  # mbin: GLM or GAM finally selected presence-absence binomial model (optional)#
  # tdef: table containing the full dataset used for modelling                 #
  # modelID: short name to identify the model                                  #
  # outdate: date in which abundance and length data were retrieved            #
  # faocode: BFT, ALB, etc. to define the species name                         #
  # modelpathid: path where model results are to be saved                      #
  # pdfplots: TRUE/FALSE should the output plots be saved as a .pdf file       #
  # pngplots: TRUE/FALSE should the output plots be saved as .png files        #
  #----------------------------------------------------------------------------#
  # Outputs:                                                                   #
  # 1) Plots of model effects and performance (.pdf and/or .png)               #
  # 2) Data used for modelling and final models (.RData)                       #
  # 3) Summary of the models (.txt)                                            #
  # 4) Information regarding the models (.html and .xlsx)                      #
  #----------------------------------------------------------------------------#
    ###########################################
    # 1) Plots of Model effects and performance
    ###########################################
    #-----------------
    # In .pdf format
    #-----------------
    if (pdfplots==TRUE) {
      # mbin
      if(!is.null(mbin)){
        pdf(paste(modpathid, outdate, "_", modelID, "_bin_", class(mbin)[1], "_effects&perform_", faocode, ".pdf", sep=""), onefile=TRUE)
          # Plot 1: Model effects
          if(class(mbin)[1]=="gam"){
            require(mgcv); plot(mbin, all=TRUE) # Model effects
          }
          if(class(mbin)[1]=="glm") {  
            require(effects); plot(allEffects(mbin), ylab="Probability(released)", rug=FALSE) # Effects        
          }
          # Plot 2: Density presence-absence
          require(ggplot2)
          tdef$fittedbin <- predict(mbin, type = 'response')
          binfit <- ggplot(tdef, aes(fittedbin, fill = as.factor(lpres))) + geom_density(alpha = 0.2)
          print(binfit)
        
          # Plot 3: ROC plot and AUC
          require(pROC); g <- plot.roc(lpres ~ fittedbin, data = tdef, print.auc=TRUE)
        dev.off()
      }
      # mab
      if(!is.null(mab)){
        pdf(paste(modpathid, outdate, "_", modelID, "_abu_", class(mab)[1], "_effects&perform_", faocode, ".pdf", sep=""), onefile=TRUE)
          if(class(mab)[1]=="gam"){
            plot(mab, all=TRUE)   # Effects
            gam.check(mab)        # Model performance
          }
          if(class(mab)[1]=="glm"){
            ab.effects <- allEffects(logpos_glm)
            plot(ab.effects, ylab="Probability(released)", rug=FALSE) # Effects
            par(mfrow=c(2,2)); plot(mbin, 1); plot(mbin, 2); plot(mbin, 3); plot(mbin, 5) # Model performance
          }
          # Plot 3
          par(mfrow=c(1,1))
          hist(mab$residual, xlab="Residuals", main="Histogram of residuals")
        dev.off()
      }        
    }

    #-----------------
    # In .png format
    #-----------------
    if (pngplots==TRUE){
      
      # Binomial part (mbin)
      if(!is.null(mbin)){
        
        # Plot 1: Model effects
        # png(paste(modpathid, outdate, "_", modelID, "_bin_", class(mbin)[1], "_effects&perform_", faocode, "_1de3.png", sep=""))
        png(paste(modpathid, outdate, "_", modelID, "_bin_", class(mbin)[1], "_effects&perform_", faocode, "_1de3.png", sep=""), width=489*2.5, height=480*1.8)
        if(class(mbin)[1]=="gam"){
          plot(mbin, all=TRUE) # Model effects
        }
        if(class(mbin)[1]=="glm") {  
          require(effects); plot(allEffects(bin_glm), ylab="Probability(released)", rug=FALSE) # All effects on year         
        }
        dev.off() # End plot 1
        
        # Plot 2: Density presence-absence
        png(paste(modpathid, outdate, "_", modelID, "_bin_", class(mbin)[1], "_effects&perform_", faocode, "_2de3.png", sep=""))
          require(ggplot2)
          tdef$fittedbin <- predict(mbin, type = 'response')
          binfit <- ggplot(tdef, aes(fittedbin, fill = as.factor(lpres))) + geom_density(alpha = 0.2)
          print(binfit)
        dev.off()
        
        # Plot 3: ROC plot and AUC
        png(paste(modpathid, outdate, "_", modelID, "_bin_", class(mbin)[1], "_effects&perform_", faocode, "_3de3.png", sep=""))
          require(pROC)
          g <- plot.roc(lpres ~ fittedbin, data = tdef, print.auc=TRUE)
        dev.off()
      }
      
      # Abundance part (mab)
      if(!is.null(mab)){
        # Plot 1 out of 3
        # png(paste(modpathid, outdate, "_", modelID, "_abu_", class(mab)[1], "_effects&perform_", faocode, "_1de3.png", sep=""))   
        png(paste(modpathid, outdate, "_", modelID, "_abu_", class(mab)[1], "_effects&perform_", faocode, "_1de3.png", sep=""), width=489*2.5, height=480*1.8)   
          if(class(mab)[1]=="gam"){
            plot(mab, all=TRUE, main="Partial Effects") # Model partial effects
          }
          if(class(mab)[1]=="glm"){
            ab.effects <- allEffects(logpos_glm)
            plot(ab.effects, ylab="Probability(released)", rug=FALSE, main="All effects") # Model total effects on year
          }
        dev.off()
        
        # Plot 2: 
        png(paste(modpathid, outdate, "_", modelID, "_abu_", class(mab)[1], "_effects&perform_", faocode, "_2de3.png", sep=""))
          if(class(mab)[1]=="gam") {
            gam.check(mab)
          }
          if(class(mab)[1]=="glm"){
            par(mfrow=c(2,2))
            plot(mbin, 1); plot(mbin, 2); plot(mbin, 3); plot(mbin, 5) 
          }
        dev.off()
        
        # Plot 2: Histogram of residuals 
        png(paste(modpathid, outdate, "_", modelID, "_abu_", class(mab)[1], "_effects&perform_", faocode, "_3de3.png", sep=""))
          par(mfrow=c(1,1))
          hist(mab$residual, xlab="Residuals", main="Histogram of residuals")
        dev.off()
      }
    }


    #######################################
    # 2) .RData containing data used and the models
    #######################################
    if(is.null(mbin) & !is.null(mab)) {
      save(tdef, mab, file=paste(modpathid, outdate, "_", modelID, "_data&models.RData", sep=""))
    }
    if(!is.null(mbin) & is.null(mab)) {
      save(tdef, mbin, file=paste(modpathid, outdate, "_", modelID, "_data&models.RData", sep=""))
    }
    if(!is.null(mbin) & !is.null(mab)) {
      save(tdef, tabs, mbin, mab, file=paste(modpathid, outdate, "_", modelID, "_data&models.RData", sep=""))
    }
    sink(paste(modpathid, outdate, "_", modelID, "_summary_models.txt", sep=""))
      if(!is.null(mbin)) print(summary(mbin)) # mbin could be absent
      if(!is.null(mab)) print(summary(mab))
    sink()
  
    #######################################
    # 3) .html containing information regarding data used and the models
    #######################################
    if(is.null(mbin) & !is.null(mab)) {
      output <- get_Modparams (mbin=NULL, mab=mab, modelID=modelID, modpathid=modpathid, outdate=outdate, faocode=faocode)
    }
    if(!is.null(mbin) & is.null(mab)) {
      output <- get_Modparams (mbin=mbin, mab=NULL, modelID=modelID, modpathid=modpathid, outdate=outdate, faocode=faocode)
    }
    if(!is.null(mbin) & !is.null(mab)) {
      output <- get_Modparams (mbin=mbin, mab=mab, modelID=modelID, modpathid=modpathid, outdate=outdate, faocode=faocode)
    }
    
    #######################################
    # 4) .xlsx containing information regarding data used and the models
    ####################################### 
    require(writexl)
    out2 <- as.data.frame(cbind(names(output), t(output)))
    write_xlsx(out2, path=paste(modpathid, "/", faocode, "_model_", modelID, "_parameters.xlsx", sep=""), col_names=FALSE)
}


################################################################################
# Function: "get_Modparams"                                                    #
# To retrieve model parameters for GAM and GLM models                          #
################################################################################
get_Modparams <- function(mbin=NULL, mab=NULL, modelID, modpathid, outdate, faocode){
  #----------------------------------------------------------------------------#
  # mab: must exist in any case unless only presence-absence model is run      #
  # mbin: only if the model if 2-stage or only presence-absence                #
  # modelID:                                                                   #
  # outdate:                                                                   #
  # faocode:                                                                   #
  # modelpathid:                                                               #
  #----------------------------------------------------------------------------#
  require(htmltools)
  require(htmlTable)
  require(mgcv)
  
  #******************************
  # Binomial part of the model
  #******************************  
  if(!is.null(mbin)){
    callmbin <- paste(mbin$call) # Selected model
    classmbin <- callmbin[1] # Type of model
    formmbin <- callmbin[2] # Formula
    fitmetmbin <- mbin$method  # Fitting method  
    datambin <- callmbin [4]  # Dataset
    if(length(callmbin)==6) selectf <- callmbin [6]
    # fammbin <- gsub("\\s*\\([^\\)]+\\)", "", as.character(callmbin [3]))  # Family
    fammbin <- mbin$family$family
    linkmbin <- mbin$family$link
    
    if(classmbin=="gam"){
      ndatambin <- summary(mbin)$n # N
      yearsmbin <-  unique(mbin$xlevels[[1]]) # Time series    
    }
    if(classmbin=="glm"){
      ndatambin <- nrow(mbin$data) # Data used
      yearsmbin <- unique(mbin$data$year) # Time series
    }
    # Model fit
    mbinnulldev <- round(mbin$null.deviance, 1)
    mbindev <- round(mbin$deviance, 1)
    mbinaic <- round(mbin$aic, 1)
    mbindfresidual <- round(mbin$df.residual,0)
  }
  
  #******************************  
  # Abundance
  #******************************  
  if(!is.null(mab)){
    callmab <- paste(mab$call) # Selected model
    classmab <- callmab[1] # Type of model
    formmab <- callmab[2] # Formula
    fitmetmab <- mab$method  # Fitting method  
    datamab <- callmab [4]  # Dataset

    if(length(callmab)==6) selectf <- callmab [6]
    # fammab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(callmab [3]))  # Family
    fammab <- mab$family$family
    linkmab <- mab$family$link
    
    if(classmab=="gam"){
      ndatamab <- summary(mab)$n # N
      yearsmab <-  unique(mab$xlevels[[1]]) # Time series    
    }
    if(classmab=="glm"){
      ndatamab <- nrow(mab$data) # Data used
      yearsmab <- unique(mab$data$year) # Time series
    }
    
    # Model fit
    mabnulldev <- round(mab$null.deviance, 1)
    mabdev <- round(mab$deviance, 1)
    mabaic <- round(mab$aic, 1)
    mabdfresidual <- round(mab$df.residual, 0)
  }
  
  #******************************
  # Formatting the outcome
  #******************************
  
  # Info from a binomial model
  if(!is.null(mbin)){
    x <- t(unlist(c(c("Presence-Absence"),
      #if(classmbin=="glm")c(callmbin), 
      if(classmbin=="gam" | classmbin=="glm")c(classmbin, formmbin, fammbin, linkmbin, datambin), 
      c(fitmetmbin, ndatambin, mbinnulldev, mbindev, mbinaic, mbindfresidual))))
  }
  # Info from an abundance model
  if(!is.null(mab)){
    y <- t(unlist(c(c("Presence-Absence"),
      #if(classmab=="glm") c(callmab), 
      if(classmab=="gam"| classmbin=="glm") c(classmab, formmab, fammab, linkmab, datamab), 
      c(fitmetmab, ndatamab, mabnulldev, mabdev, mabaic, mabdfresidual))))     
  }
  
  # General information
  pcname <- Sys.info()["nodename"]  # PC
  user <- Sys.info()["effective_user"]  # User  
  path <- getwd() # Working directory
  
  # General info variables 
  V0 <- c(paste("'", pcname, "'", sep=""), "")
  V00 <- c(paste("'", user, "'", sep=""), "")
  V1 <- c(paste("'", path, "'", sep=""), "")
  V2 <- c(modelID, "")
  V3 <- c("---------", "---------")

  output <- data.frame(V0=V0, V00=V00, V1=V1, V2=V2, V3=V3)
  #---------------
  # 2-stage model
  #---------------
  if(!is.null(mbin) & !is.null(mab)) output <- data.frame(output, rbind(x, y))

  #---------------
  # Only presence-absence
  #---------------
  if(!is.null(mbin) & is.null(mab)) output <- data.frame(output, x)    
  
  #---------------
  # Only abundance model
  #---------------s
  if(is.null(mbin) & !is.null(mab)) output <- data.frame(output, y)
  
  # Add names for the different parameters
  if(classmbin=="gam"){
  nams <- c("PC name: ", "User: ", "Path: ", "ModelID: ", "", "Model type: ", " ", "Formula: ", "Family: ", "Link: ", 
    "Dataset: ", 
    #"Link function: ", 
    "Fit method: ", 
    # "select (T/F)", 
    "N: ", "Null deviance: ", "Deviance: ", 
    "AIC: ", "Df residual: ")
  }
  if(classmab=="glm"){
  nams <- c("PC name: ", "User: ", "Path: ", "ModelID: ", "", "Model type: ", " ", "Formula: ", "Family: ", "Dataset: ", 
    #"Link function: ", 
    "Fit method: ", 
    #"select (T/F)", 
    "N: ", "Null deviance: ", "Deviance: ", 
    "AIC: ", "Df residual: ")
  }
  names(output) <- nams
  
  # Save output as .html file
  save_html(html=htmlTable(t(output)), 
    file=paste(modpathid, "/", outdate, "_", faocode, "_model_", modelID, "_parameters.html", sep=""), 
    background = "white", libdir = "lib") 
  
  return(output) # Generate the output 
}  
############################## End of script !!!!###############################