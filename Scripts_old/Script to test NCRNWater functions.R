## compile NETN data from MS Access DB

NETN<-compileNETNdata(export = FALSE, surface= FALSE)# create NETN list object with water and metadata elements;and  includes all depth measurements

NETN[[1]] %>% View() # view water data

NETN[[2]] %>% View() # view meta data

## return list of only surface measurements (median of top 2 m of water); use for trend analysis

NETN.surf<-compileNETNdata(export = FALSE, surface = TRUE)

# import water data into R package from dir

## first compile data from DB, grab only surface values, and export to folder
compileNETNdata(export = TRUE, surface = TRUE)

# then ikport data in R package 
NCRN<- importNCRNWater("./Data/NCRN/")

NETN <- importNCRNWater("./Data/NETN/")


## Test some of the functions

# Plots 


waterseries(NETN, sitecode="NETN_ACAD_BRBK", charname="pH", assessment = F)
