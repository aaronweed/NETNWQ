## This functions connects to NETN's backend database and creates a flat file of the discrete data collected from the YSI (Profile data) and the binnual nutrient sample
## query then writes the flat file to the R vizualizer folder

## produced to recreate Bill Gawley's ALLStreamData Access query (NETN_H20 version 3.0 and 4.0 Acceses databases)
## updated to reflect changes to the database BE new's schema made in Dec 2016.

## Version 0.1 Aaron Weed


AllStreamData<-function(x){
  library(RODBC)
  con <- odbcConnect("NETNWQ")# Connect to database that I named under USER DSN in ODBC Data Source Admin
  # Z:\PROJECTS\MONITORING\Lake_Pond_Stream\WaterMonitoring_NETN\5_Data\Database\Database_Versions\NETN_WQ_Database_2017\NETN_H2Ov4\Backend\Current\NETN_H2Ov4_BE_20170921.accdb
  # examples
  sqlTables(con, tableType = "TABLE")$TABLE_NAME # Access data table names
  sqlColumns(con, "tluParkCode")$COLUMN_NAME#Access column names of specifc table
  
  ###################### Import data and lookup tables used for the query
  #tbl_Tree_Data, tbl_Trees, tlu_Crown_Classes, tbl_Events, tbl_Locations, tlu_Plants
  
  # import dataframes of each tables within the DB
  loc <- sqlFetch(con, "Location");names(loc)#Location information on NETN water monitoring sites.
  park <- sqlFetch(con, "tluParkCode");names(park) #Park codes and supplementary informatin for NETN park units.
  event <- sqlFetch(con, "Event"); names(event)# Times and dates of water monitoring events.
  sampdata<-sqlFetch(con, "Sample");names(sampdata) #Date, location and supplementary information on NETN water monitoring events.
  ysi<-sqlFetch(con, "WQInSitu");names(ysi) #Measurements of in situ water quality taken with YSI sonde.
  flow<-sqlFetch(con, "StreamDischarge");names(flow) #Discharge (flow) measurements for NETN streams.
  chem<-sqlFetch(con, "Chemistry");names(chem) # Lake, pond, and stream laboratory water chemistry results.
  turb<-sqlFetch(con, "Turbidity");names(turb)#Measurements of turbidity collected with LaMotte 2020 meter.
  
  odbcClose(con)
  
  ######################################## Join various dataframes be realtions to create queries
  library(plyr)
  
  # bind loc to samples
  intersect(names(loc),names(sampdata))
  temp<-join(loc, sampdata, by="LocationID")
  #names(temp)
  
  # Add location data to df: bind loc to temp 
  temp1<-join( event,temp,  by="EventID")
  #names(temp1)
  
  ## query for core parm data   
  ### append ysi data to locations and events
  temp2<-join(temp1, ysi, by="SampEventCode")
  names(temp2)
  
  temp2$Year<-as.factor(temp2$year)
  temp2$DO_mg.L<-temp2$"DO_mg/L"
  
  ### append flow data to table
  temp2a<-join(temp2, flow[,c("SampEventCode","DISCHARGE","TotalArea","TotalVel")], by= "SampEventCode")
  temp2a$Discharge<-temp2a$DISCHARGE
  
  ## subset df
  temp3<-temp2a[,c( "ParkCode","LocationType","NETNCode","Description","SampType", "StartDate","Year" ,"Time","Depth_m", "Temp_C","SpCond" ,
                   "DO_mg.L","Do_sat" , "pH", "BP_mmHg","Discharge","TotalArea","TotalVel")]
  
  levels(temp3$LocationType)
  temp3$Type[temp3$LocationType %in% "Stream"] = "stream"
  temp3$Type[!temp3$LocationType %in% "Stream"] = "lk_pnd"

  #head(temp3)
  #remove rows without site data
  temp3<-temp3[!is.na(temp3$Description) ,]
  
  #sort 
  temp3<-temp3[order(temp3$ParkCode,temp3$NETNCode, temp3$StartDate) ,]
  
  temp3
  
  
}