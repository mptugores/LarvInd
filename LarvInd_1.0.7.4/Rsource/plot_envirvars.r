#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN, PANDORA
# Autores: M.P. Tugores
#
# Date: 2022/03/03
#-------------------------------------------------------------------------------

######
# Boxplot Environmental variables

boxplotEnv <- function(tdata, year.var, vars2plot, ylabs=NULL, savePlots=FALSE, path=NULL, saveID=NULL) {
  t1 <- tdata
  for (i in vars2plot){
    if(is.null(saveID)) tiff_name <- paste(path,"boxplot_", i, ".tiff",sep="")
    if(!is.null(saveID)) tiff_name <- paste(path,"boxplot_", i, "_", saveID, ".tiff",sep="")
    if(savePlots==FALSE) dev.new()
    if(savePlots==TRUE) tiff(tiff_name, height=26, width=26, units="cm", res=150)
    par(mar=c(7,7,3,3))
    if(!is.null("ylabs")) ylabs_i <- ylabs[match(i, vars2plot)] 
    boxplot(t1[,i]~t1[,year.var], data=t1, xlab="Year", ylab=ylabs_i, 
    cex.lab=2, cex.axis=1.5, cex.names=1.5)
    if(savePlots==TRUE) dev.off()
  }
}


######
# Function: plot histogram of environmental variables

# hist.environ <- function(tabl, envir.var, species.var){
#  d1 <- density(tabl[tabl[,species.var]>0, envir.var])
#  d2 <- density(tabl[tabl[,species.var]==0, envir.var])
#  hist1 <- hist(tabl[,envir.var], plot=FALSE)
#  x <- max(d1$y, d2$y, hist1$density)
  # hist(tabl[,envir.var], freq=FALSE, ylim=c(0, x), main="Temperature", xlab=envir.var)
#  hist(tabl[,envir.var], freq=FALSE, ylim=c(0, x), main="", xlab=envir.var)
#  lines(d1, col="red")
#  lines(d2, col="blue")

#  v1 <- "present"
#  v2 <- "absent"
#  legend("topleft", inset=0.02, legend=c(v1, v2), col=c("red", "blue"), lty=1, box.lty=0)
#}  


# hist.environ <- function(tabl, envir.var, species.var){
#   d1 <- density(tabl[tabl[,species.var]>0, envir.var])
#   d2 <- density(tabl[tabl[,species.var]==0, envir.var])
#   hist1 <- hist(tabl[,envir.var], plot=FALSE)
#   x <- max(d1$y, d2$y, hist1$density)
  # hist(tabl[,envir.var], freq=FALSE, ylim=c(0, x), main="Temperature", xlab=envir.var)
#   hist(tabl[,envir.var], freq=FALSE, ylim=c(0, x), main=species.var, xlab=envir.var)
#   lines(d1, col="cyan3", lwd=2)
#   lines(d2, col="darkgrey", lty=1, lwd=2)

#   v1 <- "presence"
#   v2 <- "absence"
#   legend("topleft", inset=0.02, legend=c(v1, v2), col=c("red", "blue"), lty=1, box.lty=0)
# }  

# histAb.environ <- function(tabl, envir.var, species.var, pa=TRUE, ab=TRUE){
#  d1 <- density(tabl[tabl[,species.var]>0, envir.var])
#  d2 <- density(tabl[tabl[,species.var]==0, envir.var])
#  d3 <- density(tabl[,envir.var])
#  hist1 <- hist(tabl[,envir.var], plot=FALSE)
#  x <- max(d1$y, d2$y, hist1$density)
  
#  hist(tabl[,envir.var], freq=FALSE, ylim=c(0, x), xlab=envir.var, main=species.var, col="grey88", border="white")
#  if (pa==TRUE) lines(d1, col="cyan3", lwd=2)
#  if (pa==TRUE) lines(d2, col="darkgrey", lty=1, lwd=2)
#  if (ab==TRUE) lines(d3, col="brown1", lwd=2)

  # v1 <- expression(italic("X. gladius")*" presence")
#  if (pa==TRUE) v1 <- expression(" presence")
#  if (pa==TRUE) v2 <- expression(" absence")
#  if (ab==TRUE) v3 <- expression(" n. larvae")
#  if (pa==TRUE & ab==TRUE) legend("topleft", inset=0.02, legend=c(v1, v2, v3), col=c("cyan3", "darkgrey", "brown1"), lty=1, lwd=2, box.lty=0)
#  if (pa==FALSE & ab==TRUE) legend("topleft", inset=0.02, legend=c(v3), col=c("brown1"), lty=1, lwd=2, box.lty=0)
#  if (pa==TRUE & ab==FALSE) legend("topleft", inset=0.02, legend=c(v1, v2), col=c("cyan3", "darkgrey"), lty=1, lwd=2, box.lty=0)
#  if (pa!=TRUE & ab!=TRUE) warning("The histogram plotted is for the whole dataset!!")
#} 

histAb.environ <- 
function(tdata, vars2plot, species.var, band_width=NULL, pa=TRUE, ab=TRUE, savePlots=FALSE, path=NULL, saveID=NULL){ 
  #if (savePlots==FALSE  & onepage==FALSE) dev.new()
  for(i in vars2plot){
    t1 <- tdata
    if(any(is.na(t1[,i]))){
      t1 <- t1[!is.na(t1[ ,i]),]
      print(paste("Warning!! The environmental variable ", i, " has NA values!! They will be removed"))
    }
    t2 <- t1[rep(1:nrow(t1), times=round(t1[,species.var],digits=0)), ]
    if(is.null(band_width)){
      d1 <- density(t1[t1[,species.var]>0, i])
      d2 <- density(t1[t1[,species.var]==0, i])
      d3 <- density(t2[,i])
    }
    if(!is.null(band_width)){
      d1 <- density(t1[t1[,species.var]>0, i], bw=band_width)
      d2 <- density(t1[t1[,species.var]==0, i], bw=band_width)
      d3 <- density(t2[,i], bw=band_width)  
    }
    hist1 <- hist(t1[,i], plot=FALSE)
    x <- max(d1$y, d2$y, d3$y, hist1$density)
    
    # Setting the name of the .tiff file, in case you want to save it 
    if (pa==TRUE) value <- "PA"
    if (ab==TRUE) value  <- "Ab"
    if (pa==TRUE & ab==TRUE) value  <- "PA_Ab"
    
    tiff_name <- paste("hist_envir_", i, value, sep="_") 
    if(is.null(saveID))  tiff_name <- paste(path, tiff_name, ".tiff",sep="")
    if(!is.null(saveID)) tiff_name <- paste(path, tiff_name, "_", saveID, ".tiff",sep="")
    
    if(savePlots==FALSE) dev.new()
    if(savePlots==TRUE) tiff(tiff_name, height=26, width=26, units="cm", res=150)
    
    # Plot histogram
    hist(t1[,i], freq=FALSE, ylim=c(0, x), xlab=i, main=species.var, col="grey88", border="white")
    # Plot density lines
    if (pa==TRUE) lines(d1, col="cyan3", lwd=2)
    if (pa==TRUE) lines(d2, col="darkgrey", lty=1, lwd=2)
    if (ab==TRUE) lines(d3, col="brown1", lwd=2)

    # Plot legend
    if (pa==TRUE) v1 <- expression(" presence")
    if (pa==TRUE) v2 <- expression(" absence")
    if (ab==TRUE) v3 <- expression(" n. larvae")
    if (pa==TRUE & ab==TRUE) legend("topleft", inset=0.02, legend=c(v1, v2, v3), col=c("cyan3", "darkgrey", "brown1"), lty=1, lwd=2, box.lty=0)
    if (pa==FALSE & ab==TRUE) legend("topleft", inset=0.02, legend=c(v3), col=c("brown1"), lty=1, lwd=2, box.lty=0)
    if (pa==TRUE & ab==FALSE) legend("topleft", inset=0.02, legend=c(v1, v2), col=c("cyan3", "darkgrey"), lty=1, lwd=2, box.lty=0)
    if (pa!=TRUE & ab!=TRUE) warning("The histogram plotted is for the whole dataset!!")
    
    if (savePlots==TRUE) dev.off()
  }
}

######
# Boxplot Ottman

boxplotOttman <- function(tabl, year.var, environ) {
  #year <- tabl[,year.var]
  #envir <- tabl[,environ.var])
  t2 <- tabl
  t2$fyear <- as.factor(t2[,year.var])
  names(t2)[names(t2)==environ] <- "y"
  #t2$environ.var <- t2[,environ.var]
  ggplot(data=t2, aes(fyear, y, colour = fyear)) +
    labs(x=year.var, y=environ, colour=year.var) +
    geom_boxplot() +
    geom_jitter() + 
    theme_bw()
}

# Així funcionaria:
# boxplotOttman(tabl=t1, year.var="year", environ="TMEZCLA")
# Faltaria veure com tornees a posar que la y en comptes de y se digui TMEZCLA
#-------------------------------------------------------------------------------#