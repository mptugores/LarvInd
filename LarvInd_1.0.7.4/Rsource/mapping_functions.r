################################################################################
#***************************
# Functions to map samples
#***************************

mapSamples <- function(tabl, yearvar="year", larv_ind=NULL, path=figpath, 
  lonrange=c(0.5, 5.0), latrange=c(37.5, 41.0), save2tiff=FALSE, ...){
  #----------------------------------------------------------------------------#
  # Yearly map of the samples                                                  #
  #----------------------------------------------------------------------------#
  # tabl: data.frame with the geographical locations of the samples
  # yearvar: numeric variable containing the year in which the data were collected
  # larv_ind: numeric variable with the value of the 'tpt_larval_index' to be plotted
  #           deffault is set to NULL; then all data is plot together
  # path: directory where the .tiff maps will be saved if save2tiff=TRUE ('figpath' by deffault)
  # longrange and latrange: longitude and latitude ranges for the plotted map
  # save2tiff: parameter to determine if the maps will be plotted in the R console (FALSE) or
  #            printed to .tiff files
  #----------------------------------------------------------------------------#
  
  if(is.null(larv_ind))tabl <- tabl
  if(!is.null(larv_ind)) tabl <- tabl[tabl$larval_index %in% larv_ind, ] # Select tpt_larval_index: 1, 2 or c(1,2)
  
  tyears <- unique(tabl[,yearvar])
  larvid <- paste(larv_ind, collapse="&")
  labsize=1.2 
  
  for (i in tyears){ # Loop for each year 

    # Save as .tiff if selected
    tiffname <- paste(path, "Maps_Samples_in_larvind_", larvid, "_", i, ".tiff", sep="")
    if(save2tiff==TRUE) tiff(tiffname, width=480*1.1)
    if(save2tiff==FALSE) dev.new()
  
    selec <- c(tabl[yearvar]==i)
    tab <- tabl[selec,]
    
    # Plot all data together
    if (is.null(larv_ind)){
      with(tab, plot(lon, lat, main=i, xlim=lonrange, ylim=latrange, 
        xlab="Longitude", ylab="Latitude", pch=19, cex.lab = labsize, cex.main=2, col="blue", ...))################
    }

    # if larv_ind is not null
    if (!is.null(larv_ind)){
      # Plot larval index 1
      if (all(length(larv_ind)==1 & larv_ind==1)){
        with(tab, plot(lon, lat, main=i, xlim=lonrange, ylim=latrange, 
          xlab="Longitude", ylab="Latitude", pch=19, cex.lab = labsize, cex.main=2, col="blue", ...))################
      }
  
      # Plot larval index 2
      if (all(length(larv_ind)==1 & larv_ind==2)){
        with(tab, plot(lon, lat, main=i, xlim=lonrange, ylim=latrange, 
          xlab="Longitude", ylab="Latitude", pch=19, cex.lab = labsize, cex.main=2, col="lightgrey", ...))################
      }
  
      # Plot larval index 1 & 2
      if (all(length(larv_ind)==2 & all(larv_ind==c(1,2)))) {
        tplot1=tab[tab$larval_index==1,]
        tplot2=tab[tab$larval_index==2,] # plotea estaciones fecha no sistemáticas
  
        with(tplot1, 
          plot(lon, lat, main=i, xlim=lonrange, ylim=latrange,
            xlab="Longitude", ylab="Latitude", pch=19, cex.lab = labsize, cex.main=2, col="blue", ...)################
        )
        with(tplot2, 
          points(lon, lat, main=i, xlim=lonrange, ylim=latrange,
          xlab="Longitude", ylab="Latitude", pch=19, cex.lab=labsize, cex.main=2, col="lightgrey", ...)################
        )
    
    # Location of the legend
    xleg = round(0.7*max(lonrange),1)
    yleg = round(0.84*max(latrange)+(max(latrange)-min(latrange)), 0)
  
    legend(xleg, yleg, legend=c("larval index == 1", "larval index == 2"), 
         pch=19, col=c("blue", "lightgrey"))
    }
  } # close when larval index is not null
    
  plot(coastline, border="black", col="grey", axes=TRUE, las=1, add=TRUE, ...)
  
  # Close save as .tiff when selected
  if(save2tiff==TRUE) dev.off()   
  } # Close year loop
  if(save2tiff==TRUE) mapSamples(tabl=tabl, yearvar=yearvar, larv_ind=larv_ind, path=path, 
  lonrange=lonrange, latrange=latrange, save2tiff=FALSE) 
}

#***********************************
# Function to map samples with CTD
#***********************************
mapSamples_withCTD <- function(tabl, yearvar="year", path=figpath, 
  lonrange=c(0.5, 5.0), latrange=c(37.5, 41.0), labsize=1.5, save2tiff=FALSE, ...){
  #----------------------------------------------------------------------------#
  # Yearly map of the samples with CTD                                         #
  #----------------------------------------------------------------------------#
  # tabl: data.frame with the geographical locations of the samples
  # yearvar: numeric variable containing the year in which the data were collected
  # larv_ind: numeric variable with the value of the 'tpt_larval_index' to be plotted
  # path: directory where the .tiff maps will be saved if save2tiff=TRUE ('figpath' by deffault)
  # longrange and latrange: longitude and latitude ranges for the plotted map
  # labsize: size of the labels of the plot
  # save2tiff: parameter to determine if the maps will be plotted in the R console (FALSE) or
  #            printed to .tiff files
  #----------------------------------------------------------------------------#
  
  tabl <- na.omit(tabl[c("year","gear","SMEZCLA","lat","lon")])
  tyears <- unique(tabl$year)
  
  for (i in tyears) {  
    tab <- tabl[tabl[yearvar]==i,]
    tiffname <- paste(path, "Maps_Samples_withCTD_", i, ".tiff", sep="")
    if(save2tiff==FALSE) dev.new()
    if(save2tiff==TRUE) tiff(tiffname, width=480*1.1)
    with(tab, 
      plot(lon, lat, main=paste(i," fishing samples with CTD",sep=""),
      xlim=lonrange, ylim=latrange, xlab="Longitude", ylab="Latitude", pch=19, cex.lab=labsize, col="orange", ...)
    )
    plot(coastline, border="black", col="grey", axes=TRUE, las=1, add=TRUE, ...)
    if(save2tiff==TRUE) dev.off()
  } # Close year loop
  if(save2tiff==TRUE) mapSamples_withCTD(tabl=tabl, yearvar=yearvar, path=path, lonrange=lonrange, latrange=latrange, labsize=labsize, save2tiff=FALSE)
}

#*******************************************
# Function to map species presence-absence
#*******************************************
mapSpecies_pa <- function(tabl, yearvar="year", presabsvar="lpres", path=figpath, 
  lonrange=c(0.5, 5.0), latrange=c(37.5, 41.0), save2tiff=FALSE,...){
  #----------------------------------------------------------------------------#
  # Yearly map of the species presence-absence                                 #
  #----------------------------------------------------------------------------#
  # tabl: data.frame with the geographical locations of the samples
  # yearvar: numeric variable containing the year in which the data were collected
  # predabsvar: name of the variable containing the presence-absence information
  # path: directory where the .tiff maps will be saved if save2tiff=TRUE ('figpath' by deffault)
  # longrange and latrange: longitude and latitude ranges for the plotted map
  # labsize: size of the labels of the plot
  # save2tiff: parameter to determine if the maps will be plotted in the R console (FALSE) or
  #            printed to .tiff files
  #----------------------------------------------------------------------------#
  
  #tab <- tabl
  #myear <- as.factor(tab$year)
  tyears <- unique(tabl$year)
  
  #larvid <- paste(larv_ind, collapse=" & ")
  labsize=1.2 
  for (i in tyears) { 
    tab <- tabl[tabl[yearvar]==i,]
    # Save as .tiff if selected
    tiffname <- paste(path, "Maps_presence-absence_", faocode, "_", i, ".tiff", sep="")
    if(save2tiff==FALSE) dev.new()
    if(save2tiff==TRUE) tiff(tiffname, width=480*1.1)

    # Plot larval index 1 & 2
    tplot1=tab[tab[presabsvar]==1,]
    tplot2=tab[tab[presabsvar]==0,] 
      
    with(tplot1, 
      plot(lon, lat, main=i, xlim=lonrange, ylim=latrange,
        xlab="Longitude", ylab="Latitude", pch=19, cex.lab = labsize, cex.main=2, col="darkgreen", ...)################
    )
    with(tplot2, 
      points(lon, lat, main=i, xlim=lonrange, ylim=latrange,
        xlab="Longitude", ylab="Latitude", pch=19, cex.lab=labsize, cex.main=2, col="lightgrey", ...)################
    )
    # Location of the legend
    xleg = round(0.80*max(lonrange),1)
    yleg = round(0.84*max(latrange)+(max(latrange)-min(latrange)), 0)
  
    legend(xleg, yleg, legend=c("presence", "absence"), 
         pch=19, col=c("darkgreen", "lightgrey"))
 
    plot(coastline, border="black",col="grey", axes=TRUE, las=1, add=TRUE, ...)
  
    # Close save as .tiff when selected
    if(save2tiff==TRUE) dev.off()
    } # Close year loop
  if(save2tiff==TRUE) mapSpecies_pa (tabl=tabl, yearvar=yearvar, presabsvar=presabsvar, path=path, 
  lonrange=lonrange, latrange=latrange, save2tiff=FALSE)
}

################################################################################