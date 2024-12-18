#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN, PANDORA
# Autores: M.P. Tugores
#
# Objective: Plot index with or without Confidence Intervals
#
# Date: 2021/04/21
#-------------------------------------------------------------------------------


# La función 'plot.larvind' no será necesaria ELIMINAR!!!
# Function to plot the larval index 
#plot.larvind <- function(year, index, LCI, UCI){
#  plot(year,index, pch=15,ylim=c(min(LCI),max(UCI)), 
#    xlab="Year",ylab="Index",col="blue",xaxt="n",cex.lab=1.5)
#  axis(1, at = seq(min(year), max(year), by = 1), las=2)
#  lines(year,index,col="blue",lwd=2.5)
#  lines(year,index,col="blue",lwd=2.5)
#  lines(year,LCI,col="red",lty=2,lwd=2.5)
#  lines(year,UCI,col="red",lty=2,lwd=2.5)
#}

# Function to plot the larval index
plot.larvind <- function(year, index, LCI, UCI, xaxis="horizontal", my_ylab="Index", ylim=NULL){
  if(is.null(ylim)) my_ylim=c(min(LCI),max(UCI))
  if(!is.null(ylim)) my_ylim=ylim
  if(xaxis=="horizontal"){
    # plot (year, index, pch=19, xlab="Year", ylab="Mean CPUA (2mm)+-CI", ylim=c(min(LCI),max(UCI)))  
    plot (year, index, pch=19, xlab="Year", ylab=my_ylab, ylim=my_ylim)
  }  
  if(xaxis=="vertical") {
    plot(year, index, pch=15, ylim=my_ylim, 
    xlab="Year",ylab=my_ylab,col="blue", xaxt="n",cex.lab=1.5)
    axis(1, at = seq(min(year), max(year), by = 1), las=2)
  }
  lines(year,index,col="blue",lwd=2.5)
  lines(year,LCI,col="red",lty=2,lwd=2.5)
  lines(year,UCI,col="red",lty=2,lwd=2.5)
}



#*******************************************************************************
# 1.1. Function to plot larval indices (old)
#plot.indices <- function(tdata, x, y, fun="mean", yunits="n larvae(m3) original lengths"){
#  if(fun=="mean") y=tapply(tdata[,y], tdata[,x], mean)
#  if(fun=="sum") y=tapply(tdata[,y], tdata[,x], sum)
#  x=as.numeric(names(y))
#  ytext=paste(fun, yunits, collapse="_")  
#  plot(x, y, pch=19, ylab=yunits, xlab=("Year"), main="")
#  lines(x, y, col="red")
#}

#*******************************************************************************
# 1.1. Function to plot larval indices  (new)
#plot.indices <- function(tdata, xvar, yvar, fun="mean", yunits="n larvae(m3) original lengths", 
#  ci=FALSE, ci.method=NULL){
#  nsamples=tapply(tdata[,yvar],tdata[,xvar],length)
  
#  if(fun=="sum") y=tapply(tdata[,yvar], tdata[,xvar], sum)
#  if(fun=="mean") y=tapply(tdata[,yvar], tdata[,xvar], mean)
  
#  x=as.numeric(names(y))
#  ytext=paste(fun, yunits, collapse=" ") 
  
#  if(ci==FALSE){
#    plot(x, y, pch=19, ylab=yunits, xlab=("Year"), main="")
#    lines(x, y, col="red")
#  }
  
#  if (ci==TRUE) {
#    if (fun=="sum") stop("Confidence interval for the function 'sum' is not supported")
#    if (fun=="mean"){
#    index = y
#    index_sd=tapply(tdata[,yvar],tdata[,xvar],sd)
#    index_SE=index_sd/(sqrt(nsamples))
#    index_cv=index_SE/index
    
#    if(ci.method=="Ingram_etal_2010"){   # Ingram et al. (2010) for non normal             
#      C1= exp(2*sqrt(log(1+index_cv^2)));
#      LCInn=(index/C1);
#      UCInn=(index*C1);
      
#      plot (x, y, pch=19, xlab="Year", ylab=yunits, ylim=c(min(LCInn),max(UCInn)))
#      lines(x, y, col="blue",lwd=2.5)
#      lines(x, LCInn, col="red", lty=2,lwd=2.5)
#      lines(x, UCInn, col="red", lty=2,lwd=2.5)
#    }
  
#    if(ci.method=="Normal"){ # Normal distribution
#      LCI=index-1.96*index_sd/sqrt(nsamples)
#      UCI=index+1.96*index_sd/sqrt(nsamples)
      
#      plot (x, y, pch=19, xlab="Year", ylab=yunits, ylim=c(min(LCInn),max(UCInn)))
#      lines(x, y, col="blue",lwd=2.5)
#      lines(x, LCInn, col="red", lty=2,lwd=2.5)
#      lines(x, UCInn, col="red", lty=2,lwd=2.5)
#      }    
    #if(ci.method=="both"){
    #  C1= exp(2*sqrt(log(1+index_cv^2)))
    #  LCI=index-1.96*index_sd/sqrt(nsamples)
    #  UCI=index+1.96*index_sd/sqrt(nsamples)
    #  LCInn=(index/C1)
    #  UCInn=(index*C1)
    #}
    
#    } # close fun=="mean"
#  } # close ci==TRUE
#}



#*******************************************************************************
# 1.1. Function to plot larval indices  (new con loop & save)
plot.indices <- function(tdata, xvar, yvars, ylabs, fun, ci=FALSE, ci.method=NULL, 
  savePlots=FALSE, figpath, nams, saveID=NULL, outdate){

for (i in 1:length(yvars)){
  if(savePlots==FALSE) dev.new()
  if(savePlots==TRUE) {
    if(is.null(saveID)) nams_i <- paste(figpath, outdate, "_", faocode, "_", nams[i], sep="") # File name
    if(!is.null(saveID)) nams_i <- paste(figpath, outdate, "_", faocode, "_", nams[i], "_", saveID, sep="") # File name
    
    if(ci==FALSE) {
      nams_i <- nams_i  
    } else {
    if(ci==TRUE & ci.method=="Ingram_etal_2010") nams_i <- paste(nams_i, "_Ingram", sep="")
    if(ci==TRUE & ci.method=="Normal") nams_i <- paste(nams_i, "_Normal", sep="")
    }
    
    nams_i <- paste(nams_i, ".png", sep="")
    png(nams_i, width = 15, height = 15, units = "cm", res = 100) # Create a .png file
  }  
  # plot.indices(tdata, xvar, yvars[i], funs[i], yunits=ylabs[i], ci=ci, ci.method=ci.method)
  yvar_i <- yvars[i]
  fun_i <- funs[i]
  yunits_i <- ylabs[i]
  
  nsamples=tapply(tdata[,yvar_i],tdata[,xvar],length)
  
  if(fun_i=="sum") y=tapply(tdata[,yvar_i], tdata[,xvar], sum)
  if(fun_i=="mean") y=tapply(tdata[,yvar_i], tdata[,xvar], mean)
  
  if(all(is.na(y))) stop(paste("The variable ", yvar_i, " you are willing to plot is all NA", sep=""))
  
  x=as.numeric(names(y))
  # ytext=paste(fun, yunits_i, collapse=" ") 
  ytext=paste(fun_i, yunits_i, collapse=" ")
  
  if(ci==FALSE){
    plot(x, y, pch=19, ylab=yunits_i, xlab=("Year"), main="")
    lines(x, y, col="red")
  }
  
  if (ci==TRUE) {
    if (fun_i=="sum") stop("Confidence interval for the function 'sum' is not supported")
    if (fun_i=="mean"){
    index = y
    index_sd=tapply(tdata[,yvar_i],tdata[,xvar],sd)
    index_SE=index_sd/(sqrt(nsamples))
    index_cv=index_SE/index
    
    if(ci.method=="Ingram_etal_2010"){   # Ingram et al. (2010) for non normal             
      C1= exp(2*sqrt(log(1+index_cv^2)));
      LCInn=(index/C1);
      UCInn=(index*C1);
      
      plot (x, y, pch=19, xlab="Year", ylab=yunits_i, ylim=c(min(LCInn),max(UCInn)))
      lines(x, y, col="blue",lwd=2.5)
      lines(x, LCInn, col="red", lty=2,lwd=2.5)
      lines(x, UCInn, col="red", lty=2,lwd=2.5)
    }
  
    if(ci.method=="Normal"){ # Normal distribution
      LCI=index-1.96*index_sd/sqrt(nsamples)
      UCI=index+1.96*index_sd/sqrt(nsamples)
      
      plot (x, y, pch=19, xlab="Year", ylab=yunits_i, ylim=c(min(LCInn),max(UCInn)))
      lines(x, y, col="blue",lwd=2.5)
      lines(x, LCInn, col="red", lty=2,lwd=2.5)
      lines(x, UCInn, col="red", lty=2,lwd=2.5)
      }    
    #if(ci.method=="both"){
    #  C1= exp(2*sqrt(log(1+index_cv^2)))
    #  LCI=index-1.96*index_sd/sqrt(nsamples)
    #  UCI=index+1.96*index_sd/sqrt(nsamples)
    #  LCInn=(index/C1)
    #  UCInn=(index*C1)
    #}
    
    } # close fun=="mean"
  } # close ci==TRUE

  if(savePlots==TRUE) dev.off()
  #if(savePlots==FALSE & length(yvars)>1) {
  #  if(i<length(yvars)) {
  #    dev.new()
  #  }
  #} # close savePlots==FALSE  
} # close loop

} # close function

# plot.indices(tdata=tlindex, xvar="year", yvars=vars2plot, fun=funs, ylabs=ylabs)  # Funciona OK
# plot.indices(tdata=tlindex, xvar="year", yvars=vars2plot, fun=funs, ylabs=ylabs, ci=ci, ci.method=ci.method) # Funciona OK
# plot.indices(tdata=tlindex, xvar="year", yvars=vars2plot, fun=funs, ylabs=ylabs, savePlots=TRUE, figpath=figpath, nams=nams, saveID=NULL) # Funciona OK
# plot.indices(tdata=tlindex, xvar="year", yvars=vars2plot, fun=funs, ylabs=ylabs, 
#   ci=ci, ci.method=ci.method, savePlots=TRUE, figpath=figpath, nams=nams, saveID=NULL)  # Funciona OK    

#-------------------------------------------------------------------------------#