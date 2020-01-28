
## Evaluate the lab-based and field-based methods for estimating nutrient in NETN streams/lakes
## Evaluation during the 2017 field season using Hanna Checker and test strips
# Pulls in lab-based data from 2017 and the HannaChecker Test strip data

##load packages

library(reshape2)
library(readxl)
library(readr)
library(ggplot2)
library(plyr)

## import th df:
## data exported from NETN Water DB in Dec 2016 from Bill Gawley


streams <- read_excel("~/NETN Protocols/NETN protocol analysis/Water quality/Assessment of Hanna Checker and test strip data/2017 Nutrient data/NETNStreams_2017_May-Sep.xlsx")
lakes <- read_excel("~/NETN Protocols/NETN protocol analysis/Water quality/Assessment of Hanna Checker and test strip data/2017 Nutrient data/NETNLakes_2017_May-Sep.xlsx")

checker <- as.data.frame(read_csv("~/NETN Protocols/NETN protocol analysis/Water quality/2017 Assessment of Hanna Checker and test strip data/checker_strips_analysisdata20171120.csv"))
  
  
###### DATA MANIPULATION ###########
####################################

#### Streams data manipulation

str(streams)

## subset df to include only reps "" and "1"

streams.sub<-subset(streams, is.na(streams$Rep) | streams$Rep %in% "1")

## create molten data frame (all values are represented for each site*time combination)
# needed to force as data frame for some reason for this to work.
names(streams.sub)
stream.melt<-melt(streams.sub, id.vars=c( "Park" ,"NETNCode", "Date", "WQ Depth (m)"), measure.vars=c("Temp (C)","DO (mg/L)", "Sonde pH","SpCond (uS/cm)","Discharge_cfs" , 
                                                                                                      "pH_Lab","ANC_ueqL", "TP_ugL" , "TN_mgL","NO2_mgL", "NH3_mgL" ,"NO3_ueqL" , 
                                                                                                      "NO3N_mgL_Calc","SO4_ueqL" , "Cl_ueqL" ))
                                                                                                
head(stream.melt)

## create month variable
stream.melt$Date<-as.Date(stream.melt$Date, "%Y-%m-%d")
stream.melt$month<-as.factor(format(stream.melt$Date,"%m"))
## create year variable
stream.melt$year<-as.factor(format(stream.melt$Date,"%Y"))
## create mon_year variable for spatial modeling (Sncf)
stream.melt$mon_year<-as.factor(format(stream.melt$Date,"%m%Y"))
# stream.melt$NETNCode<-as.factor(stream.melt$NETNCode)
# stream.melt$variable<-as.character(stream.melt$variable)


###############################
#### Lakes data manipulation####
##################################################

## create molten data frame (all values are represented for each site*time combination)
# needed to force as data frame for some reason for this to work.
names(lakes)
lake.melt<-melt(lakes, id.vars=c( "Park" ,"NETNCode", "Date", "SDepth (m)"), measure.vars=c("Avg Surface Temp (C)","Avg Surface DO (mg/L)" , "Avg Surface SpCond (uS/cm)", 
                                                                                            "pH_Lab","ANC_ueqL", "TP_ugL" , "TN_mgL","NO2_mgL" ,"NO3_ueqL" , 
                                                                                            "SO4_ueqL" , "Cl_ueqL" ))
head(lake.melt)

## create month variable
lake.melt$Date<-as.Date(lake.melt$Date,"%Y-%m-%d" )
lake.melt$month<-as.factor(format(lake.melt$Date,"%m"))
## create year variable
lake.melt$year<-as.factor(format(lake.melt$Date,"%Y"))
## create mon_year variable for spatial modeling (Sncf)
lake.melt$mon_year<-as.factor(format(lake.melt$Date,"%m%Y"))


###############################
#### Checker and field-based methods data ####
##################################################
## create molten data frame (all values are represented for each site*time combination)


names(checker)
field.melt<-melt(checker, id.vars=c( "NETNCode", "Date", "Sample Time", "Test Time", "Stream_Lake Temp","Bottle Temp", "Rain Event"  ), measure.vars=c(7:19))
levels(field.melt$variable)
head(field.melt)
field.melt$Date<-as.Date(field.melt$Date, "%m/%d/%Y")
field.melt$month<-as.factor(format(field.melt$Date,"%m"))
## create year variable
field.melt$year<-as.factor(format(field.melt$Date,"%Y"))
## create mon_year variable for spatial modeling (Sncf)
field.melt$mon_year<-as.factor(format(field.melt$Date,"%m%Y"))


###################################
#### Combine dataset for comparison analysis
#####################################
# combine lake and stream data
cols<-intersect(colnames(streams), colnames(lakes))
lab<-as.data.frame(rbind(streams[,cols], lakes[,cols]))

## create month variable
lab$month<-as.factor(format(lab$Date,"%m"))
## create year variable
lab$year<-as.factor(format(lab$Date,"%Y"))
## create mon_year variable for spatial modeling (Sncf)
lab$mon_year<-as.factor(format(lab$Date,"%m%Y"))


head(checker)
checker$Date<-as.Date(checker$Date, "%m/%d/%Y")
checker$month<-as.factor(format(checker$Date,"%m"))
## create year variable
checker$year<-as.factor(format(checker$Date,"%Y"))
## create mon_year variable for spatial modeling (Sncf)
checker$mon_year<-as.factor(format(checker$Date,"%m%Y"))

### Add field-based data to lab-based data
intersect(names(lab), names(checker))

final<-join(lab, checker, by=c("NETNCode", "mon_year"))

write.table(final,"Field_And_Lab_Based_com.csv", sep=",", row.names =F)

###############################
#### First assess relationships and patterns within a site and among variables of the FIELD-BASED METHODS
##################################################
levels(field.melt$variable)

data<-checker # select wide data
data<-field.melt # select long data
head(data)

### EFFECT OF BOTTLE TEMP ON CONC

p <- ggplot(data, aes(x= data$`Bottle Temp`, y = value))+ labs(x= "Bottle Temp (C)", y= "Concentration" )+
    geom_point(size =2, color = "blue") 

p<- (p+facet_wrap(~variable, ncol=4, scales ="free") +
       geom_smooth(method = "lm", se= TRUE)+ 
       theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
       theme(axis.text.x = element_text(color="black",angle = 0,  vjust=0,size = 16 * 0.8)) +
       theme(strip.text.x= element_text(size=9, face=c("bold.italic"))) +
       theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(legend.position = "top", legend.title = element_blank()) +
       theme(legend.key = element_rect(fill = "white", color = "white")) +
       theme(panel.background =  element_rect(fill="white", colour="black")) +
       theme(panel.grid.major = element_line(colour = "grey90")))
print(p)


### EFFECT OF STREAM/LAKE TEMP ON CONC

p <- ggplot(data, aes(x= data$`Stream_Lake Temp`, y = value))+ labs(x= "Stream/Lake Temp (C)", y= "Concentration" )+
  geom_point(size =2, color = "blue") 

p<- (p+facet_wrap(~variable, ncol=4, scales ="free") +
       geom_smooth(method = "lm", se= TRUE)+ 
       theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
       theme(axis.text.x = element_text(color="black",angle = 0,  vjust=0,size = 16 * 0.8)) +
       theme(strip.text.x= element_text(size=9, face=c("bold.italic"))) +
       theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(legend.position = "top", legend.title = element_blank()) +
       theme(legend.key = element_rect(fill = "white", color = "white")) +
       theme(panel.background =  element_rect(fill="white", colour="black")) +
       theme(panel.grid.major = element_line(colour = "grey90")))
print(p)

#### NUMBER OF SAMPLES WITH POSITIVE HITS TAKEN SO FAR PER SITE

SAMPS<- count(checker[,c("NETNCode")])
SAMPS

p <- ggplot(SAMPS, aes(x,freq))+ labs(y= "No. of Samples" ) +
      geom_boxplot(color = "black") + coord_flip()

p

### Variation in conc among sites
data$NETNCode<- as.factor(data$NETNCode)
levels(data$variable)
levels(data$NETNCode)

# taking out ACAD sites for ease of plotting
ACAD<-c( "ACBRWN","ACDKLI" ,"ACEAGL", "ACECHO", "ACJORD" ,"ACLKWO", "ACLONG", "ACLVYB", "ACROUN", "ACSEAL", "ACSTNL", "ACUBRK", "ACUHAD", "ACWHOL", "ACWOOD" )

N<-c("NH3-N mg/l", "NH3 mg/l" , "NO2-Nug/l(meter)","NO2 ug/l(meter)","NO2 mg/l(meter)","NO3(strip)")
P<-c("PO4mg/L","PO4µg/L" )
Cl<- c("Cl mg/L (meter)","Cl µeq/L (meter)","Cl quantab units (strip)" ,"Cl mg/L (strip)")

data2<-subset(data, NETNCode %in% ACAD & variable %in% Cl)
data2<-droplevels(data2)

### boxplot of the variation

p <- ggplot(data2, aes(x= variable, y = value))+ labs( y= "Concentration" )+
  geom_boxplot(color = "blue") 

p<- (p+facet_wrap(~NETNCode, ncol=4) +coord_flip()+
       #geom_smooth(span = 0.3)+ 
       theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 9,face="bold"))+
       theme(axis.text.x = element_text(color="black",angle = 0,  vjust=0,size = 9)) +
       theme(strip.text.x= element_text(size=9, face=c("bold.italic"))) +
       theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(legend.position = "top", legend.title = element_blank()) +
       theme(legend.key = element_rect(fill = "white", color = "white")) +
       theme(panel.background =  element_rect(fill="white", colour="black")) +
       theme(panel.grid.major = element_line(colour = "grey90")))
print(p)


### plot of the temporal variation

p <- ggplot(data2, aes(x= month, y = value, colour= NETNCode))+ labs( y= "Concentration" )+
  geom_point(size= 2) + geom_line(aes(group = NETNCode))

p<- (p+facet_wrap(~variable, ncol=3, scales= "free") +
       #geom_smooth(span = 0.3)+ 
       theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 9,face="bold"))+
       theme(axis.text.x = element_text(color="black",angle = 0,  vjust=0,size = 9)) +
       theme(strip.text.x= element_text(size=9, face=c("bold.italic"))) +
       theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(legend.position = "right", legend.title = element_blank()) +
       theme(legend.key = element_rect(fill = "white", color = "white")) +
       theme(panel.background =  element_rect(fill="white", colour="black")) +
       theme(panel.grid.major = element_line(colour = "grey90")))
print(p)

###############################
#### Assess relationships between FIELD-BASED and LAB-BASED DATA
##################################################
source("panel_cor.R")

names(final)

### Relationship between two vars
## NOte: field-based measurements are : "Stream_Lake Temp"         "Test Time"                "Time since"               "Bottle Temp"             
[37] "NH3-N mg/l"               "NH3 mg/l"                 "PO4mg/L"                  "PO4µg/L"                 
[41] "NO2-Nug/l(meter)"         "NO2 ug/l(meter)"          "NO2 mg/l(meter)"          "NO3(strip)"              
[45] "Cl mg/L (meter)"          "Cl µeq/L (meter)"         "Cl quantab units (strip)" "Cl mg/L (strip)"

data<-final

# Nitrite
N<-c("TN_mgL","NO2_mgL" ,"NO3_ueqL", "NH3-N mg/l", "NH3 mg/l" , "NO2-Nug/l(meter)","NO2 ug/l(meter)","NO2 mg/l(meter)","NO3(strip)")

dataN<-data[,c(N)]

plot(dataN$"NO2_mgL",dataN$"NO2 mg/l(meter)", xlab = "Lab NO2_mg_L", ylab= "Field NO2 mg_L", pch =16)

summary(lm(dataN$"NO2 mg/l(meter)"~ dataN$"NO2_mgL", na.action= na.exclude))

# Chloride

plot(data$"Cl_ueqL",data$"Cl mg/L (meter)", xlab = "Lab Cl_ueqL", ylab= "Field Cl_ueqL ", pch =16)




## compare all combos (not enough data right now, too may missing obs)


head(dataN)

pairs(dataN, upper.panel = panel.cor)




