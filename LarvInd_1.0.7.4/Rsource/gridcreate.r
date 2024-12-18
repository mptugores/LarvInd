#-------------------------------------------------------------------------------
# Proyecto: BLUEFIN, PANDORA
# Autores: M.P. Tugores
#
# Date: 2021/10/21
#-------------------------------------------------------------------------------

#-------------------------------------------
# Function createSpatialGrid 
#-------------------------------------------
# creates an object 'cuadgrid', also saved as an .RData 
# which contains a SpatialPolygons with the specifications set in the function

createSpatialGrid <- function(minlon, maxlon, minlat, maxlat, cellwidth, 
  projection="+proj=longlat +datum=WGS84", basemap="Rsource/sig/country.shp", saveTiff=TRUE, saveShape=FALSE, 
  # basemap: full path, name and extension of a shapefile that you want to use
  # as basemap when plotting
  mypath=getwd(), coastlinepath=getwd()){
  
  library(rgdal)
  library(sp)
  library(raster)
    
  m <- matrix(c(maxlon, maxlon, minlon, minlon, maxlon, 
    minlat, maxlat, maxlat, minlat, minlat), nrow=5, ncol=2)
  
  p <- Polygon(m)
  ps <- Polygons(list(p),1)
  sps <- SpatialPolygons(list(ps))
  proj4string(sps) <- CRS("+proj=longlat +datum=WGS84")

  size <- cellwidth
  r <- raster(sps, resolution = size)
  cuadgrid <<- as(r, "SpatialPolygons")
  
  if(saveTiff==TRUE){
    coastline <- readOGR(basemap)
    tiff(paste(mypath, "/", gsub("-", "", Sys.Date()), "_sampling_location_grid.tiff", sep=""), width=480*1.1)
      plot(coastline,col="grey", xlim=c(minlon,maxlon), ylim=c(minlat,maxlat), axes = TRUE)
      plot(sps, bg = "light blue",add=T)
      plot(cuadgrid, add = T)
    dev.off()
  }
  
  #names(cuadgrid)

  save(cuadgrid, file=paste(mypath, "/",gsub("-", "", Sys.Date()), "_cuadgrid_balsea_1DD.RData", sep=""))

  # save the shapefile
  if(saveShape==TRUE){
          
    centroids <- coordinates(cuadgrid)
    x <- centroids[,1]
    y <- centroids[,2]
    cuadgrid.spoldf <<- SpatialPolygonsDataFrame(cuadgrid,
      data=data.frame(x=x, y=y, row.names=row.names(cuadgrid)))
    
    writeOGR(cuadgrid.spoldf, dsn = mypath, 
      layer = paste(gsub("-", "", Sys.Date()), '_poly_cuadgrid_balsea', sep=""), 
      driver = "ESRI Shapefile")  
      
  }
}
################################################################################

# Previous version:
# "20180917_cuadgrid_balsea.RData"
# crea el polugono de la zona cuadrado
# maxlat=42+0.1# sumamos 0.1 para que encajen mejora las muestras
# minlat=36
# minlon=-2+0.05
# maxlon=8
# m=matrix(c(maxlon,maxlon,minlon,minlon,maxlon,minlat,maxlat,maxlat,minlat,minlat),nrow=5,ncol=2)
# p = Polygon(m)
# ps = Polygons(list(p),1)
# sps = SpatialPolygons(list(ps))
# proj4string(sps) = CRS("+proj=longlat +datum=WGS84")

# size <- 0.5
# r <- raster(sps, resolution = size)
# cuadgrid <- as(r, "SpatialPolygons")

# tiff(paste(figpath,"sampling_location_grid.tiff", sep=""), width=480*1.1)
# plot(coastline,col="grey",xlim=c(minlon,maxlon),ylim=c(minlat,maxlat), axes = TRUE)
# plot(sps, bg = "light blue",add=T)
# plot(cuadgrid, add = T)
# dev.off()
# names(cuadgrid)


# save(cuadgrid,file="20180917_cuadgrid_balsea.RData")
################################################################################