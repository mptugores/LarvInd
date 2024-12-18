#------------------------------------------------------------------------------#
# Set relative folder paths 
# & create folders if missing
#------------------------------------------------------------------------------#

# mypath <- getwd()
mypath <- paste(getwd(), "/", sep="")

# Names of the objects containing the paths to be created
namepaths <- c("dpath", "opath", "figpath", "propath", "modpath", "envproc_path")

# Full paths of the directories to be createds 
paths <- c(paste(mypath,"data/",sep=""), 
  paste(mypath,"outtables/",sep=""),
  paste(mypath, "outfigures/", sep=""),
  paste(mypath,"data/processfiles/", sep=""),
  paste(mypath, "model_outputs/", sep=""),
  paste(mypath, "data/processfiles/envirdata_process/", sep="")
  )

for (i in 1:length(namepaths)){
  if(!exists(namepaths[i])) assign(namepaths[i], paths[i])
  if(!dir.exists(get(namepaths[i]))) dir.create(get(namepaths[i]))
}

rm(namepaths, paths, i)

# Observación: si ya hemos usado la BD en los anteriores scripts, igual no 
# hace falta aquí tener la dbpath cargada!!!
#------------------------------------------------------------------------------#