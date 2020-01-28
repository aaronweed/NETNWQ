## view flow data

### flow vs flwo-level
df <- read_excel("Data/StreamDischargeData.xlsx")


xy<-select(df, Park, Discharge_cfs,`Flow Level`) %>% 
  na.omit()
# reorder factor levels
xy$`Flow Level`<-fct_relevel(xy$`Flow Level`,"Base", "Low","Medium", "Bank-Full","High", "Flood")


p <- ggplot(xy, aes(y=Discharge_cfs, x=`Flow Level`)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=1)+facet_wrap(~Park, scales = "free")
p


# flow vs stage

xy<-select(df, Park, Site, Discharge_cfs,Gage1_ft) %>% 
  na.omit() %>% 
  filter(!Park %in% "ACAD")

p <- ggplot(xy, aes(x=Discharge_cfs, y=Gage1_ft)) + 
  geom_point()+facet_wrap(~Site, scales = "free")

p

# Flow level vs stage

xy<-select(df, Park, Site, `Flow Level`,Gage1_ft) %>% 
  na.omit() 

p <- ggplot(xy, aes(y=Gage1_ft, x=`Flow Level`)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=1)+facet_wrap(~Park, scales = "free")
p
