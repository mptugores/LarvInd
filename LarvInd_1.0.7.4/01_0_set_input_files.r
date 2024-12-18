################################################################################
# Script to set the input files for
#  the scripts to Estimate Larval Index
################################################################################

#****************************************
# Set objects to retrieve 
# input files (02/09/2022)
#****************************************
species <- "Thunnus alalunga"

# IMPORTANT:

# The previous script '00_Get_tabs_fromAccess.r' 
# generates the following files:

# fabund and flen: must exist in the dir "/outtables/"
# fprocess: must exist in the dir "/data/processfiles/" 

fabund <- "20220901_t_abundance_larvind_ALB_abs.csv"
flen <- "20220901_t_length_larvind_ALB.csv"
fprocess <- "00_0_process_specifs_BFT_ALB_2022-09-01.RData"

# Setting output date for files to be saved from the length data file
outdate <- substr(fabund, 1, 8)

# faocode
faocode <- "ALB"
################################################################################