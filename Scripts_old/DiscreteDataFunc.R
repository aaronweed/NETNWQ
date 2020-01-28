## This functions connects to NETN's backend database and creates a flat file of the discrete data collected from the YSI (Profile data)

# connects to the Database BE prior to the Dec 2016 version

NETN_discretedata<-function(x){
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
  ysi<-sqlFetch(con, "tblYSIcdf");names(ysi)
  flow<-sqlFetch(con, "tblStreamDischarge");names(flow)
  
  odbcClose(con)
  
  ####### clean up some tables ########
  ysi<-ysi[ysi$Site < "899", ]

  ######################################## Join various dataframes be realtions to create queries
  library(plyr)
  
  # bind loc to samples
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