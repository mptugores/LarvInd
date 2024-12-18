

##################### lee SST zonal hoy.  variable = sstZ.0.5 y pixel

# cargar funciones, relative paths and settings
source("Rsource/settings.R")

rm(sstZ05dd)#
sstZ05dd=read.txt('D:/AAD/proyectos/TUNABIT/subproyectos/operacionaltuna/imputdata/outsfromRprocessing/SST_tables/scale_05/20141010_2001_2011_SSTstats_hoy_scale_0.5.txt',sep=';',na.strings='-999',dec='.',header=T)
names(sstZ05dd)

(levels(as.factor(sstZ05dd$date)))

tsstZ05dd=sstZ05dd[c("operacionbd","mean","slope4c","cellvalue")]
str(tsstZ05dd)
tsstZ05dd$sst05dd=tsstZ05dd$mean
tsstZ05dd$sstslop05dd=tsstZ05dd$slope4c

tsstZ05dd$sstpixelhoy=tsstZ05dd$cellvalue


# My version should:
# (1) Read the ecolarv dataset
# It is read as a .csv file but probably it can be converted to a list using the 
# function split by 'mydate'  [this might help for a posterior parallelization]

# df.data <- read.csv('file.csv')
# l_data <- split (df.data[c("id_netcolector", "year", "mydate", "lon", "lat")], f=df.data$mydate)


# (2) Download in a '/temp/'  folder (or 'cmems_data') rooted in the place were
# the actual project is found
# The .nc files of the days for which we have in-situ CTD and/or larval data
# This should be parallelized.

# Using: foreach () or doParallel()

# (3) With the raster package, extract from the files the data of the locations
# of interest, i.e. where we have in-situ data.


############################################ paste gvel stats 03
fgvel <- "D:/AAD/proyectos/TUNABIT/subproyectos/operacionaltuna/imputdata/outsfromRprocessing/gvel_tables/20150224gvelSTATS_2001_2013_0.3.txt"

t1 # Should be the abundance data.frame

if(exists("tgvel030")) rm(tgvel030)#sst zonal hace 15 días.
tgvel030= tryCatch(read.table(fgvel,sep=';',na.strings='-999',dec='.',header=T), error=function(e) NULL)
if (is.null(tgvel030)){
  warning("Missing tgvel data")
  t1.1 <- t1
}
if (!is.null(tgvel030)){
  names(tgvel030)
  nrow(tgvel030)

  if(exists("gvel030"))rm(gvel030)
  gvel030=tgvel030[c("operacionbd","grad","slope8c","slope4c","mean","cellvalue")]
  names(gvel030)
  gvel030.2=gvel030
  names(gvel030.2) <- c("id_operation","gvelgrad_030","gvelslope8c_030","gvelslope4c_030","gvelmean030","gvelpix" ) 
  summary(gvel030.2)

  nrow(gvel030.2)
  boxplot(gvel030.2$gvelpix)

  t1.1 <- merge(x = t1, y = gvel030.2, by = "id_operation", all.x=TRUE)
  names(t1.1)
  plot(t1.1$um,t1.1$gvelpix)

  boxplot(t1.1$gvelpix~t1.1$year)
}

#-------------------------
# Save as .csv and .xlsx
#-------------------------
tout=t1.1
fileout <- paste(substr(fanalisis, 1, 8), "_t_abu_larvind_", faocode, "_operationalvars", sep="") 
fileout

save.csvxlsx(tout, path=sppath, file_out=fileout)
################################################################################