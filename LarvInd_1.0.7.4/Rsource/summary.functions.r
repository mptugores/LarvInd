#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN, PANDORA
# Autores: M.P. Tugores
#
# Objective: Functions to produce summary tables of samplings, CPUA, etc
#
# Date: 2021/12/03
#-------------------------------------------------------------------------------

################################################################################
# *********************** Summary sampling *************************
# Assess n larvae, nsamples and nominal index with cpua gear standardized
# to be compared with data from data base 
################################################################################

summarySAMPLING <- function (tab, year.var, haul.date, abundance, towdepth, vol
  #, saveSummary=TRUE, saveID=NULL, opath=opath, outdate
  ){
  #----------------------------------------------------------------------------#
  # Parameters of the function: "summarySAMPLING"
  # tab: data.frame containing data
  # year.var: name of the variable containing year
  # haul.date: name of the variable containing the date in which the haul was performed
  # abundance (="nlarvae"): name of the variable containing raw larval count
  # towdepth (m): depth of the tow
  # vol (=volume): name of the variable containing the filtered volume
  #----------------------------------------------------------------------------#
  tab <- tab[order(tab[,haul.date]),]
  year=na.omit(unique(tab[year.var]))
  year=year[,1]
    
  t1 <- tab[tab[,abundance]>0,]
  inidate=as.Date(tapply(tab[,haul.date], tab$year, min), origin="1970-01-01")
  enddate=as.Date(tapply(tab[,haul.date], tab$year, max), origin="1970-01-01")
  
  n_stations=tapply(tab[,abundance],tab[,year.var],length) # Number of stations
    
  n_posit <- tapply(t1[,abundance],t1[,year.var], length)  # Number of positive stations
  
  nstations <- data.frame(year=names(n_stations), n_stations)
  nposit <- data.frame(year=names(n_posit), n_posit)
  
  data_df <- merge(nstations, nposit, by="year", all.x=TRUE)
  data_df$n_posit[is.na(data_df$n_posit)]<-0
  
  ratio_positives=with(data_df, 100*(n_posit/n_stations))
  
  n_larvae=tapply(tab[,abundance],tab[,year.var],sum)  # number of total larvae
  mean_larvae=tapply(tab[,abundance],tab[,year.var],mean)
  
  tab$cpua <- tab[,towdepth]*(tab[,abundance]/tab[,vol])
  ycpua=tapply(tab$cpua,tab[,year.var],mean)
  
  outputtable <- data.frame(year=year, inidate=inidate, enddate=enddate, 
    n_stations=data_df$n_stations, ratio_posit=ratio_positives, 
    n_larvae=n_larvae, mean_larvae=mean_larvae, ycpua=ycpua)
  outputtable <- outputtable[order(outputtable$year),]
  
  #if(saveSummary==TRUE){
  #  fout1 <- paste(outdate, faocode, "summarySamplings",
  #    paste(min(outputtable$year), max(outputtable$year), sep="-"), sep="_")
  #  if(!is.null(saveID)) fout1 <- paste(fout1, saveID, sep="_")
  #  write.table(outputtable, paste(sppath, fout1, ".csv",sep=""), quote = FALSE, row.names=FALSE, sep=HCUsep, dec=HCUdec) 
  #  #library(xlsx)
    #write.xlsx(outputtable, paste(opath, fout1, ".xlsx",sep=""), sheetName="Sheet1", showNA=TRUE, row.names=FALSE)  
  #  require(writexl)
  #  write_xlsx(outputtable, paste(sppath, fout1, ".xlsx",sep=""))
  #}
      
  return(outputtable)
  
}

################################################################################
# *********************** CPUA summary *************************
# Assess n larvae, nsamples and nominal index with cpua gear standardized
# to be compared with data from data base 
################################################################################

################################################################################
# Function "summaryCPUAyear"
# Purpose: compute CPUA mean annual summary table (=nominal CPUA)
#
# Inputs:
# - tab: data.frame containing data
# - year.var: name of the variable containing year
# - abundance (="nlarvae"): name of the variable containing larval counts
# - abundance2mm (="n2mm"): N larvae standardised to larvae of 2 mm; 
#                         it can be ommitted if sum and mean n2mm are not to be computed
# - stand.abund: name of the variable containing the standardised abundance (typically
#              standardised to gear and 2mm)
# - ci.method: method to compute confidence intervals for the nominal index
#            available methods "Walter&Lamking2010" or "Normal"
# - plot.cpua: if TRUE the mean CPUA is plotted within the R Console 
#
# Output: data.frame
# Variables of the output table: 
#         year, nsamples, larval.counts, n2mm_sum, n2mm_mean,
#         meanCPUA, CPUAsd, CPUAse, CPUAcv, LCInn, UCInn
################################################################################

summaryCPUAyear <- function (tab, year.var, abundance, abundance2mm=NULL, stand.abund 
  , ci.method="Ingram_etal_2010" 
  #, plot.cpua=TRUE 
  #, saveSummary=TRUE, saveID=NULL, opath=opath, outdate
  ){
  #----------------------------------------------------------------------------#
  # tab: data.frame containing data
  # year.var: name of the variable containing year
  # abundance ("nlarvae"): name of the variable containing raw larval count
  # abundance2mm ("n2mm"): N larvae standardised to larvae of 2 mm; 
  #                        (can be ommitted, if sum and mean n2mm 
  #                         are not to be computed)
  # stand.abund (abgsvar): name of the variable containing the standardised abundance 
  #                        (typically standardised to gear and 2mm)
  # ci.method: method to compute confidence intervals for the nominal index
  #            available methods "Walter&Lamking2010" or "Normal"
  # plot.cpua: if TRUE, meanCPUA (yearly mean stand.abund) is plot in the R Console; if FALSE, not plotted   
  # saveSummary: TRUE/FALSE saves the output table as .csv file
  # saveID: if not null, added to the name of the .csv file generated
  # opath: path where the .csv file will be saved 
  # outdate:  
  #----------------------------------------------------------------------------#
  year=na.omit(unique(tab[year.var])); year=year[,1]
  year=year[order(year)]
  nsamples=tapply(tab[,stand.abund],tab[,year.var],length)
  larval.counts=tapply(tab[,abundance],tab[,year.var],sum)  # number of total larvae

  meanCPUA=tapply(tab[,stand.abund],tab[,year.var],mean)
  CPUA_sd=tapply(tab[,stand.abund],tab[,year.var],sd)
  CPUA_SE=CPUA_sd/(sqrt(nsamples))
  CPUAcv=CPUA_SE/meanCPUA

  if(!is.null(abundance2mm)){
    n2mm_sum=tapply(tab[,abundance2mm], tab[,year.var], sum) # n2mm_sum
    n2mm_mean=tapply(tab[,abundance2mm], tab[,year.var], mean) # n2mm_mean
  }
  
  if(ci.method=="Ingram_etal_2010"){                
    # Ingram et al. (2010) approach for non normal
    C1= exp(2*sqrt(log(1+CPUAcv^2)));
    LCInn=(meanCPUA/C1);
    UCInn=(meanCPUA*C1);
    ci="Ingram"
  }
  
  if(ci.method=="Normal"){
    # Normal distribution
    C1= exp(2*sqrt(log(1+CPUAcv^2)));
    LCInn=meanCPUA-1.96*CPUA_sd/sqrt(nsamples)
    UCInn=meanCPUA+1.96*CPUA_sd/sqrt(nsamples)
    ci=ci.method
  }
  
  # if (ci.method== )    # other methods???
  if(is.null(abundance2mm)) {
    outputtable <- data.frame(year=year, nsamples=nsamples, larval.counts=larval.counts,
    meanCPUA=meanCPUA, CPUAsd=CPUA_sd, CPUAse=CPUA_SE, CPUAcv=CPUAcv, 
    LCInn=LCInn, UCInn=UCInn)
  }
  
  if(!is.null(abundance2mm)){
  outputtable <- data.frame(year=year, nsamples=nsamples, larval.counts=larval.counts,
    n2mm_sum=n2mm_sum, n2mm_mean=n2mm_mean, 
    meanCPUA=meanCPUA, CPUAsd=CPUA_sd, CPUAse=CPUA_SE, CPUAcv=CPUAcv, 
    LCInn=LCInn, UCInn=UCInn)
  }
  
  #if(plot.cpua==TRUE){
  #  plot(year, meanCPUA, pch=19, xlab="Year", ylab="Mean CPUA (2mm)+-CI", ylim=c(min(LCInn),max(UCInn)))
  #  lines(as.numeric(year), meanCPUA, col="blue")
  #  lines(year, LCInn, col="red", lty=2, lwd=2.5)
  #  lines(year, UCInn, col="red", lty=2, lwd=2.5)
  #}
  #if(saveSummary==TRUE){
    # fileout <- paste(substr(fname, 1, 8), "_v07_09_Summary_samplings_", faocode, sep="")
    # if(is.null(saveID)) fout1 <- paste(substr(fname, 1, 8), "_", faocode, "_v5.6_summaryCPUAyear_ciIngram_",min(summarytable$year), "-", max(summarytable$year), sep="")
    #fout1 <- paste(outdate, "summaryCPUAyear_ci", ci, "_", faocode,
    #  paste(min(outputtable$year), max(outputtable$year), sep="-"), sep="")
    #if(!is.null(saveID)) fout1 <- paste(fout1, saveID, sep="_")
    #write.table(outputtable, paste(sppath, fout1, ".csv",sep=""), quote = FALSE, row.names=FALSE, sep=HCUsep, dec=HCUdec) 
    #library(xlsx)
    #write.xlsx(outputtable, paste(opath, fout1, ".xlsx",sep=""), sheetName="Sheet1", showNA=TRUE, row.names=FALSE)  
    #require(writexl)
    #write_xlsx(outputtable, paste(sppath, fout1, ".xlsx",sep=""))
  #}
  return(outputtable) 
}
################################################################################

### New version
summaryCPUAyear <- function (tab, year.var, date.var
  , abundance, abundance2mm=NULL, stand.abund 
  , ci.method="non-normal" 
  #, plot.cpua=TRUE 
  #, saveSummary=TRUE, saveID=NULL, opath=opath, outdate
  ){
  #----------------------------------------------------------------------------#
  # tab: data.frame containing data
  # year.var: name of the variable containing year
  # date.var: name of the variable containing date information for each sample
  # abundance ("nlarvae"): name of the variable containing raw larval count
  # abundance2mm ("n2mm"): N larvae standardised to larvae of 2 mm; 
  #                        (can be ommitted, if sum and mean n2mm 
  #                         are not to be computed)
  # stand.abund (abgsvar): name of the variable containing the standardised abundance 
  #                        (typically standardised to gear and 2mm)
  # ci.method: method to compute confidence intervals for the nominal index
  #            available methods: 
  #            - "non-normal": from Ingram et al., 2010 assuming data with non-normal pdf
  #            - "normal": assuming normal pdf
  # plot.cpua: if TRUE, meanCPUA (yearly mean stand.abund) is plot in the R Console; if FALSE, not plotted   
  # saveSummary: TRUE/FALSE saves the output table as .csv file
  # saveID: if not null, added to the name of the .csv file generated
  # opath: path where the .csv file will be saved 
  # outdate:  
  #----------------------------------------------------------------------------#
  year=na.omit(unique(tab[year.var])); year=year[,1]
  year=year[order(year)]
  nsamples=tapply(tab[,stand.abund],tab[,year.var],length)
  
  mindate <- aggregate(tab[,date.var],by=list(tab[,year.var]), min) [,2]
  maxdate <- aggregate(tab[,date.var],by=list(tab[,year.var]), max) [,2]
  
  larval.counts=tapply(tab[,abundance],tab[,year.var],sum)  # number of total larvae
  
  tabs <- tab[c(tab[abundance]>0),]
  mindatepresence <- aggregate(tabs[,date.var], by=list(tabs[,year.var]), min) [,2]
  maxdatepresence <- aggregate(tabs[,date.var], by=list(tabs[,year.var]), max) [,2]
  
  meanCPUA=tapply(tab[,stand.abund],tab[,year.var],mean)
  CPUAsd=tapply(tab[,stand.abund],tab[,year.var],sd)
  CPUAse=CPUAsd/(sqrt(nsamples))
  CPUAcv=CPUAse/meanCPUA

  if(!is.null(abundance2mm)){
    n2mm_sum=tapply(tab[,abundance2mm], tab[,year.var], sum) # n2mm_sum
    n2mm_mean=tapply(tab[,abundance2mm], tab[,year.var], mean) # n2mm_mean
  }
  
  if(ci.method=="non-normal" | ci.method=="both"){                
    # Ingram et al. (2010) approach for non normal
    C1= exp(2*sqrt(log(1+CPUAcv^2)));
    LCInn=(meanCPUA/C1);
    UCInn=(meanCPUA*C1);
    #ci="Ingram"
  }
  
  if(ci.method=="normal" | ci.method=="both"){
    # Normal distribution
    # C1= exp(2*sqrt(log(1+CPUAcv^2)));
    LCI=meanCPUA-1.96*CPUAsd/sqrt(nsamples)
    UCI=meanCPUA+1.96*CPUAsd/sqrt(nsamples)
    #ci=ci.method
  }
  
  ifexists_data_frame <- function(x){
    tf <- NULL; for (i in x) tf <- c(tf, exists(i))
    x2 <- na.omit(x[tf]); x2
    tflist <- list(); for (j in 1:length(x2)) tflist[[j]] <- get(x2[j])
    out <- do.call(data.frame, tflist)
    names(out) <- x2
    return(out)
  }  # necessit que la funció estigui aquí perquè trobi els objectes que necessita
  # [hauria d'apendre com es fa per fer-ho ok]
  
  objs <- c("year", "nsamples"
    , "mindate", "maxdate"
    , "larval.counts", "mindatepresence", "maxdatepresence"
    , "n2mm_sum", "n2mm_mean"
    , "meanCPUA", "CPUAsd", "CPUAse", "CPUAcv"
    , "LCI", "UCI", "LCInn", "UCInn"
  )
  outputtable <- ifexists_data_frame(objs)
  return(outputtable) 
}
################################################################################

################################################################################
# Index table function
# Function to retrieve index.table either using "bootstrap" or "emmeans"
################################################################################
index.table <- function(mab, mbin, tdef, tabs, year.var, stand.abund,
  ci.method="Ingram_etal_2010", var.method="Lauretta", 
  two.stage=TRUE, method="emmeans", nboot=10) {
  #----------------------------------------------------------------------------#
  # mbin: binomial part of the model
  # mab: abundance model
  # tdef: data.frame
  # tabs: data.frame containing only positive stations for target species
  # year.var: name of the variable containing year information
  # stand.abund: abundance standardised, typically to 2 mm and to gears
  # Confidence interval calculation methods (ci.method): "emmeans", "bootstrap"
  # Variance calculation methods (var.method): "lauretta", "walter"
  # Variance methods only apply for two stage modelling
  # nboot: number of repetitions when using bootstrap
  #----------------------------------------------------------------------------#
    year=na.omit(as.numeric(levels(as.factor(tdef[,year.var]))))
    year=year[order(year)]
    nsamples=tapply(tdef[,year.var], tdef[,year.var], length)
    
  ###########################
  ###### 0. Mean CPUA
  ###########################
    # meanCPUA
    nominalCPUA=tapply(tdef[,stand.abund],tdef[,year.var],mean)
    CPUA_sd=tapply(tdef[,stand.abund],tdef[,year.var],sd)
    CPUA_SE=CPUA_sd/(sqrt(nsamples))
    CPUAcv=CPUA_SE/nominalCPUA

    if(ci.method=="Ingram_etal_2010"){                
      # Ingram et al., 2010 approach for non normal
      C1= exp(2*sqrt(log(1+CPUAcv^2)));
      CPUA_LCInn=(nominalCPUA/C1);
      CPUA_UCInn=(nominalCPUA*C1);
    }
  

  ###############################
  ###### 1. Least means squares
  ###############################
  if (method=="emmeans"){
    if(two.stage==FALSE){
      require(emmeans)
      mab_lsm <- emmeans(mab, formula(paste("~", year.var, sep="")), type="response", data=tabs); print(mab_lsm)
      cpue = summary(mab_lsm, type="response")$response
      var_c=summary(mab_lsm, type="response")$SE^2  # Qué pasa con la back-transformación de la varianza
      index=cpue; var_i=var_c  # Variance
    }
    
    if (two.stage==TRUE){
      ############################################################
      ##################### COMBINE THE 2 MODELS emmeans#########
      #############################################################
      require(emmeans)
      bin_lsm <- emmeans(mbin, formula(paste("~", year.var, sep="")), type="response", data=tdef); print(bin_lsm)
      mab_lsm <- emmeans(mab, formula(paste("~", year.var, sep="")), type="response", data=tabs); print(mab_lsm)

      ppos = summary(bin_lsm, type="response")$prob;ppos
      var_p=(summary(bin_lsm, type="response")$SE)^2

      cpue = summary(mab_lsm, type="response")$response
      var_c=summary(mab_lsm, type="response")$SE^2  # Qué pasa con la back-transformación de la varianza

      index=ppos*cpue

      if (var.method=="Lauretta") var_i = var_p*(cpue^2) + (var_c)*(ppos^2) - (var_p*var_c) ######### Lauretta
      if (var.method=="Walter")   var_i = var_p*(cpue^2) + (var_c)*(ppos^2) ######### Walter approximation
    }
    
    # Confidence interval for normal distributions
    se_i=sqrt(var_i)
    cv_i=se_i/index
    LCI=index-1.96*se_i;
    UCI=index+1.96*se_i

    ########################### Walter & Lamking (2010) approach for non normal
    if (ci.method=="Ingram_etal_2010") {
      C2= exp(2*sqrt(log(1+cv_i^2)))
      LCInn=(index/C2)
      UCInn=(index*C2)
    }

  index_table=data.frame(year
    , nsamples
    , index, se_i, UCI, LCI, UCInn, LCInn
    , cpue, var_c, ppos, var_p, cv_i, nominalCPUA=nominalCPUA 
    , nominalCPUA_LCI=CPUA_LCInn, nominalCPUA_UCI=CPUA_UCInn
    )
} # close least means squares

  ###########################
  ###### 2. bootstrap method
  ###########################

  if (method=="bootstrap"){
    tvars=tdef[, ! names(tdef) %in% c("year","fYear","fittedbin"), drop = F]; head(tvars)
    tyears=tdef[c("year")];tapply(tdef$year, tdef$year, length)
    # pego una distribicion random del resto de variables.

    # rm(t1)
    boot.mean = NULL
    boot.SE=NULL
    nrep=nboot
    for(i in 1:nrep){
      t1 = cbind(tyears,tvars[sample(nrow(tvars)),])
      # precide esda tabla
      py = predict(mbin,t1,type="response")*exp(predict(mab,t1,type="response"))
      t2 = cbind(t1,py)
      mu = tapply(t2$py,t2$year,mean)
      SE1 = tapply(t2$py,t2$year,mean)/sqrt(tapply(t2$py,t2$year,length))
      boot.mean = rbind(boot.mean,mu)
      boot.SE = rbind(boot.SE,SE1)
    }

    index=apply(boot.mean,2,mean)
    se_i=apply(boot.SE,2,mean)

    cv_i=se_i/index
    UCI=index+1.96*se_i
    LCI=index-1.96*se_i

    ########################### Ingram et al., 2010 approach for non normal
    if (ci.method=="Ingram_etal_2010") {
      C2= exp(2*sqrt(log(1+cv_i^2)));
      LCInn=(index/C2);
      UCInn=(index*C2);
    }
  #####################################################################
 
  index_table=data.frame(year=year
    , nsamples
    , index, se_i, UCI, LCI, UCInn, LCInn,
    cv_i, nominalCPUA=nominalCPUA, nominalCPUA_LCI=CPUA_LCInn,
    nominalCPUA_UCI=CPUA_UCInn)
} # close bootstrap
return(index_table)
}
################################################################################


#### New Version
################################################################################
# Index table function
# Function to retrieve index.table either using "bootstrap" or "emmeans"
################################################################################
index.table <- function(mab, mbin, tdef, tabs, year.var, stand.abund,
  error.method="emmeans", ci.method="non-normal", nboot=10,
  two.stage=TRUE, var.method="dependent") {
  #----------------------------------------------------------------------------#
  # mbin: binomial part of the model
  # mab: abundance model
  # tdef: data.frame containing all dataset
  # tabs: data.frame containing only positive stations for target species
  # year.var: name of the variable containing year information
  # stand.abund: abundance standardised, typically to 2 mm and to gears
  # error.method: error estimation method 
  #   - "emmeans": least squares means
  #   - "bootstrap"
  # nboot: if error.method=="bootstrap", number of repetitions 
  # ci.method: Confidence interval (CI) calculation methods
  #   - "normal": CI assuming a normal distribution 
  #   - "non-normal": approximation of CI estimation for non-normally distributed data
  #                   as proposed in Ingram et al., 2010
  #   - "both": normal and non-normal CI are computed
  # two.stage: TRUE/FALSE
  #   - TRUE: both presence-absence and abundance modelled in two separate models
  #   - FALSE: only presence-absence OR abundance are modelled
  # var.method: if two.stage=TRUE, variance calculation assumes or not independence
  #             between presence-absence and abundance processes 
  #   - "dependent": Lauretta
  #   - "independent": Walter
  #----------------------------------------------------------------------------#
  
  year=na.omit(as.numeric(levels(as.factor(tdef[,year.var]))))
  year=year[order(year)]
  nsamples=tapply(tdef[,year.var], tdef[,year.var], length)
  
  ###########################
  ###### 0. Mean/Nominal CPUA
  ###########################
  # meanCPUA
  nominalCPUA=tapply(tdef[,stand.abund],tdef[,year.var],mean)
  CPUA_sd=tapply(tdef[,stand.abund],tdef[,year.var],sd)
  CPUA_SE=CPUA_sd/(sqrt(nsamples))
  CPUAcv=CPUA_SE/nominalCPUA
 
  if(ci.method=="non-normal" | ci.method=="both"){                
    # Ingram et al., 2010 aproximation for non normal
    C1= exp(2*sqrt(log(1+CPUAcv^2)));
    CPUA_LCInn=(nominalCPUA/C1);
    CPUA_UCInn=(nominalCPUA*C1);
  }
  if(ci.method=="normal" | ci.method=="both"){
    CPUA_LCI=nominalCPUA-1.96*CPUA_SE;
    CPUA_UCI=nominalCPUA+1.96*CPUA_SE
  }
  
  ###############################
  ###### 1. Least means squares
  ###############################
  if (error.method=="emmeans"){
    require(emmeans)
    #---------------------
    # Single stage model
    #---------------------
    if(two.stage==FALSE){
      # Abundance only
      if(!is.null(mab)){
        mab_lsm <- emmeans(mab, formula(paste("~", year.var, sep="")), type="response", data=tabs); print(mab_lsm)
        cpue = summary(mab_lsm, type="response")$response
        var_c=summary(mab_lsm, type="response")$SE^2  # Qué pasa con la back-transformación de la varianza
        index=cpue; var_i=var_c  # Variance
      }
      # Binomial only
      if(!is.null(mbin)){
        bin_lsm <- emmeans(mbin, formula(paste("~", year.var, sep="")), type="response", data=tdef); print(bin_lsm)
        ppos = summary(bin_lsm, type="response")$prob;ppos
        var_p=(summary(bin_lsm, type="response")$SE)^2
        index=ppos; vari_i=var_p
      }
    } # close single stage
    
    #---------------------
    # Two stage model (combine the two models)
    #---------------------
    if (two.stage==TRUE){
      # Binomial part
      bin_lsm <- emmeans(mbin, formula(paste("~", year.var, sep="")), type="response", data=tdef); print(bin_lsm)
      ppos = summary(bin_lsm, type="response")$prob;ppos
      var_p=(summary(bin_lsm, type="response")$SE)^2
      print("var_p")
      print(var_p)

      # Abundance part
      mab_lsm <- emmeans(mab, formula(paste("~", year.var, sep="")), type="response", data=tabs); print(mab_lsm)
      cpue = summary(mab_lsm, type="response")$response
      var_c=summary(mab_lsm, type="response")$SE^2  # Qué pasa con la back-transformación de la varianza
      print("var_c")
      print(var_c)


      # Index
      index=ppos*cpue 

      # Variance of the index
      if (var.method=="dependent") var_i = var_p*(cpue^2) + (var_c)*(ppos^2) - (var_p*var_c) ######### Lauretta
      if (var.method=="independent") var_i = var_p*(cpue^2) + (var_c)*(ppos^2) ######### Walter approximation
    } # close two stage
    
    #---------------------
    # Standard error (se_i) and coefficient of variation (cv_i) for the Index
    #---------------------
    # Both for single stage and two stage models
    se_i=sqrt(var_i)
    cv_i=se_i/index 
  } # close least means squares
  ###########################
  
  ###########################
  ###### 2. bootstrap method
  ###########################
  if (error.method=="bootstrap"){
    tvars=tdef[, ! names(tdef) %in% c("year","fYear","fittedbin"), drop = F]; head(tvars)
    tyears=tdef[c("year")];tapply(tdef$year, tdef$year, length)
    # pego una distribicion random del resto de variables.

    # rm(t1)
    boot.mean = NULL
    boot.SE=NULL
    nrep=nboot
    for(i in 1:nrep){
      t1 = cbind(tyears,tvars[sample(nrow(tvars)),])
      # precide esda tabla
      py = predict(mbin,t1,type="response")*exp(predict(mab,t1,type="response"))
      t2 = cbind(t1,py)
      mu = tapply(t2$py,t2$year,mean)
      SE1 = tapply(t2$py,t2$year,mean)/sqrt(tapply(t2$py,t2$year,length))
      boot.mean = rbind(boot.mean,mu)
      boot.SE = rbind(boot.SE,SE1)
    }

    # Estimate index, standard error and coefficient of variation
    index=apply(boot.mean,2,mean)
    se_i=apply(boot.SE,2,mean)
    cv_i=se_i/index
  } # close bootstrap
  ###########################
  
  ###########################
  # Confidence interval for normal distributions
  ###########################
  if(ci.method=="normal" | ci.method=="both"){
      LCI=index-1.96*se_i;
      UCI=index+1.96*se_i
     }
    
  ########################### 
  # Ingram et al., 2010 approach for non normal
  ###########################
    if (ci.method=="non-normal" | ci.method=="both") {
      C2= exp(2*sqrt(log(1+cv_i^2)))
      LCInn=(index/C2)
      UCInn=(index*C2)
    }
  
  ###########################
  ##### Index table
  ###########################
  
  ifexists_data_frame <- function(x){
    tf <- NULL; for (i in x) tf <- c(tf, exists(i))
    x2 <- na.omit(x[tf]); x2
    tflist <- list(); for (j in 1:length(x2)) tflist[[j]] <- get(x2[j])
    out <- do.call(data.frame, tflist)
    names(out) <- x2
    return(out)
  }  # necessit que la funció estigui aquí, sinó no troba els objectes perquè són
  # temporals 
  # [hauria d'apendre com es fa per fer-ho ok]
  
  #index_table=data.frame(year
  #  , nsamples
  #  , index, UCI, LCI
  #  , UCInn, LCInn
  #  , ppos, var_p, cpue, var_c, 
  #  , se_i, cv_i, 
  #  , nominalCPUA=nominalCPUA 
  #  , nominalCPUA_LCI=CPUA_LCInn, nominalCPUA_UCI=CPUA_UCInn
  #  )
  objs <- c("year", "nsamples"
    , "mindate", "maxdate"
    , "index", "UCI", "LCI"
    , "UCInn", "LCInn"
    , "ppos", "var_p", "cpue", "var_c" 
    , "se_i", "cv_i" 
    , "nominalCPUA" 
    , "CPUA_LCInn", "CPUA_UCInn"
    )
  index_table <- ifexists_data_frame(objs)
  
return(index_table)
}
################################################################################

#-------------------------------------------------------------------------------#