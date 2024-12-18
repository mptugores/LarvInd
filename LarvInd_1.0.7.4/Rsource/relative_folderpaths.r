#------------------------------------------------------------------------------#
# Proyecto: BLUEFIN, PANDORA                                                   #
# Autores: M.P. Tugores                                                        #
#                                                                              #
# Aim of the script:                                                           #
# Set relative folder paths & create folders, if missing                       #
#                                                                              #
# Date: 2021/11/26                                                             #
#------------------------------------------------------------------------------#

mypath <- getwd()

# Names of the objects containing the paths to be created
namepaths <- c("dpath", "opath", "propath")

# Full paths of the directories to be createds 
paths <- c(paste(mypath,"/data/",sep=""), 
  paste(mypath,"/outtables/",sep=""), 
  paste(mypath,"/data/processfiles/", sep=""))

for (i in 1:length(namepaths)){
  if(!exists(namepaths[i])) assign(namepaths[i], paths[i])
  if(!dir.exists(namepaths[i])) dir.create(get(namepaths[i]))
}

rm(namepaths, paths)
#------------------------------------------------------------------------------#