### This function connects to NETN's WQ backend and queries all of the chemistry data for all sites (streams, lakes and ponds) and returns a df
#### Version 0.1 Aaron Weed 11/4/2016



NETN_Nutdata<-function(x){
  library(RODBC)
  con <- odbcConnect("NETNWQ")# Connect to database that I named under USER DSN in ODBC Data Source Admin
  # examples
  sqlTables(con, tableType = "TABLE")$TABLE_NAME # Access data table names
  sqlColumns(con, "tluParkCode")$COLUMN_NAME#Access column names of specifc table
  
  ###################### Import data and lookup tables used for the query
  #tbl_Tree_Data, tbl_Trees, tlu_Crown_Classes, tbl_Events, tbl_Locations, tlu_Plants
  
  # import dataframes of each tables within the DB
  loc <- sqlFetch(con, "tblLocations");names(loc)
  park <- sqlFetch(con, "tluParkCode");names(park)
  event <- sqlFetch(con, "tblNETNEvents"); names(event)
  sampdata<-sqlFetch(con, "tblNETNSamples");names(sampdata)
  chem<-sqlFetch(con, "tblChem");names(chem)
  ysi<-sqlFetch(con, "tblYSIcdf");names(ysi)
  
  odbcClose(con)
  
  ####### clean up some tables ########
  ysi<-ysi[ysi$Site < "899", ]
  chem<-chem[!chem$QCtype == "BLANK", ]; chem$QCtype<-droplevels(chem$QCtype)
  levels(chem$QCtype)
  
  ######################################## Join various dataframes be realtions to create queries
  library(plyr)
  
  # bind loc to samples
  temp<-join(loc, sampdata, by="LocationID")
  names(temp)
  
  # Add location data to df: bind loc to temp 
  temp1<-join( event,temp,  by="EventID")
  names(temp1)
  
## query for nutrient/acid data   
  ### append chemistry data to locations and events
  temp2<-join( chem, temp1, by="SampEventCode")
  names(temp2)
  
  ## subset df
  temp3<-temp2[,c( "ParkCode","LocationType","NETNCode","Description","SampType", "StartDate", "QCtype","LabCode","DEPTH","TYPE", "FPH",
                   "eqPH","Color_Flag","ACOLOR","TCOLOR","TColor_Flag","COND",
                    "ANC_ueqL","Ca_ueqL","Mg_ueqL","K_ueqL","K_Flag","Na_ueqL","Cl_ueqL","Cl_Flag"  ,     
                   "SO4_ueqL","SO4_Flag","NO3_ueq/L","NO3_ueq/L_Flag", "NO2+NO3_mg/L","NO2+NO3_Flag", "NO2_mg/L","NO2_Flag",      
                    "T_Dis_N_mgL","T_Dis_N_Flag","NH4_mg/L","NH4_Flag","NH3_mg/L","NH3_Flag","TN_mg/L","TN_Flag",       
                    "TP_ug/L","TP_Flag","T_Dis_P_ugL","T_Dis_P_Flag","PO4_ug/L","PO4_Flag","CHLA_ugL","CHLA_Flag" ,    
                    "DOC_mgL","DOC_Flag","DIC_mgL","Si_mgL","Al_ugL","Al_Flag","FPHMETH","CONDMETH" ,     
                    "COLORMETH","ALKMETH")]
  
  #head(temp3)
  #remove rows without site data
  temp3<-temp3[!is.na(temp3$Description) ,]
  
  #sort 
  temp3<-temp3[order(temp3$ParkCode,temp3$NETNCode, temp3$StartDate) ,]
  
  temp3

}