install.packages('EDIutils')
library(EDIutils)


# Package ID: edi.233.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Interagency Ecological Program: Fish catch and water quality data from the Sacramento River floodplain and tidal slough, collected by the Yolo Bypass Fish Monitoring Program, 1998-2024..
# Data set creator:   Interagency Ecological Program -  
# Data set creator:  Lisa Vance - California Department of Water Resources 
# Data set creator:  Nicole Kwan - California Department of Water Resources 
# Contact:  Lisa Vance -  California Department of Water Resources  - lisa.vance@water.ca.gov
# Contact:  Nicole Kwan -  California Department of Water Resources  - Nicole.Kwan@water.ca.gov
# Contact:  Naoaki Ikemiyagi -  California Department of Fish and Wildlife  - 
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
 rm(list=ls())      

     



#Data table 2 Water Quality & Environmental Data#####

options(HTTPUserAgent="EDI_CodeGen")

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/233/5/4488201fee45953b001f70acf30f7734" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "EventID",     
                 "StationCode",     
                 "Datetime",     
                 "SampleDate",     
                 "WaterTemp",     
                 "SpecificConductance",     
                 "Conductivity",     
                 "Turbidity",     
                 "DO",     
                 "pH",     
                 "Secchi",     
                 "Tide",     
                 "WeatherCode",     
                 "VegetationRank",     
                 "SubstrateCode",     
                 "HabitatType",     
                 "MicrocystisRank",     
                 "MethodCode",     
                 "GearCode",     
                 "GearConditionCode",     
                 "SampleAltered",     
                 "FieldComments",     
                 "Flag_WQ",     
                 "Comment_WQ",     
                 "Duplicated"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$EventID)!="factor") dt2$EventID<- as.factor(dt2$EventID)
if (class(dt2$StationCode)!="factor") dt2$StationCode<- as.factor(dt2$StationCode)                                   
# attempting to convert dt2$Datetime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%m/%d/%Y %H:%M" 
tmp2Datetime<-as.POSIXct(dt2$Datetime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt2[dt2$Datetime != "",]) == length(tmp2Datetime[!is.na(tmp2Datetime)])){dt2$Datetime <- tmp2Datetime } else {print("Date conversion failed for dt2$Datetime. Please inspect the data and do the date conversion yourself.")}                                                                    

# attempting to convert dt2$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%m/%d/%Y"
tmp2SampleDate<-as.Date(dt2$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt2[dt2$SampleDate != "",]) == length(tmp2SampleDate[!is.na(tmp2SampleDate)])){dt2$SampleDate <- tmp2SampleDate } else {print("Date conversion failed for dt2$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt2$WaterTemp)=="factor") dt2$WaterTemp <-as.numeric(levels(dt2$WaterTemp))[as.integer(dt2$WaterTemp) ]               
if (class(dt2$WaterTemp)=="character") dt2$WaterTemp <-as.numeric(dt2$WaterTemp)
if (class(dt2$SpecificConductance)=="factor") dt2$SpecificConductance <-as.numeric(levels(dt2$SpecificConductance))[as.integer(dt2$SpecificConductance) ]               
if (class(dt2$SpecificConductance)=="character") dt2$SpecificConductance <-as.numeric(dt2$SpecificConductance)
if (class(dt2$Conductivity)=="factor") dt2$Conductivity <-as.numeric(levels(dt2$Conductivity))[as.integer(dt2$Conductivity) ]               
if (class(dt2$Conductivity)=="character") dt2$Conductivity <-as.numeric(dt2$Conductivity)
if (class(dt2$Turbidity)=="factor") dt2$Turbidity <-as.numeric(levels(dt2$Turbidity))[as.integer(dt2$Turbidity) ]               
if (class(dt2$Turbidity)=="character") dt2$Turbidity <-as.numeric(dt2$Turbidity)
if (class(dt2$DO)=="factor") dt2$DO <-as.numeric(levels(dt2$DO))[as.integer(dt2$DO) ]               
if (class(dt2$DO)=="character") dt2$DO <-as.numeric(dt2$DO)
if (class(dt2$pH)=="factor") dt2$pH <-as.numeric(levels(dt2$pH))[as.integer(dt2$pH) ]               
if (class(dt2$pH)=="character") dt2$pH <-as.numeric(dt2$pH)
if (class(dt2$Secchi)=="factor") dt2$Secchi <-as.numeric(levels(dt2$Secchi))[as.integer(dt2$Secchi) ]               
if (class(dt2$Secchi)=="character") dt2$Secchi <-as.numeric(dt2$Secchi)
if (class(dt2$Tide)!="factor") dt2$Tide<- as.factor(dt2$Tide)
if (class(dt2$WeatherCode)!="factor") dt2$WeatherCode<- as.factor(dt2$WeatherCode)
if (class(dt2$VegetationRank)!="factor") dt2$VegetationRank<- as.factor(dt2$VegetationRank)
if (class(dt2$SubstrateCode)!="factor") dt2$SubstrateCode<- as.factor(dt2$SubstrateCode)
if (class(dt2$HabitatType)!="factor") dt2$HabitatType<- as.factor(dt2$HabitatType)
if (class(dt2$MicrocystisRank)!="factor") dt2$MicrocystisRank<- as.factor(dt2$MicrocystisRank)
if (class(dt2$MethodCode)!="factor") dt2$MethodCode<- as.factor(dt2$MethodCode)
if (class(dt2$GearCode)!="factor") dt2$GearCode<- as.factor(dt2$GearCode)
if (class(dt2$GearConditionCode)!="factor") dt2$GearConditionCode<- as.factor(dt2$GearConditionCode)
if (class(dt2$SampleAltered)!="factor") dt2$SampleAltered<- as.factor(dt2$SampleAltered)
if (class(dt2$FieldComments)!="factor") dt2$FieldComments<- as.factor(dt2$FieldComments)
if (class(dt2$Flag_WQ)!="factor") dt2$Flag_WQ<- as.factor(dt2$Flag_WQ)
if (class(dt2$Comment_WQ)!="factor") dt2$Comment_WQ<- as.factor(dt2$Comment_WQ)
if (class(dt2$Duplicated)!="factor") dt2$Duplicated<- as.factor(dt2$Duplicated)

# Convert Missing Values to NA for non-dates

dt2$WaterTemp <- ifelse((trimws(as.character(dt2$WaterTemp))==trimws("NA")),NA,dt2$WaterTemp)               
suppressWarnings(dt2$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$WaterTemp))==as.character(as.numeric("NA"))),NA,dt2$WaterTemp))
dt2$SpecificConductance <- ifelse((trimws(as.character(dt2$SpecificConductance))==trimws("NA")),NA,dt2$SpecificConductance)               
suppressWarnings(dt2$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt2$SpecificConductance))
dt2$Conductivity <- ifelse((trimws(as.character(dt2$Conductivity))==trimws("NA")),NA,dt2$Conductivity)               
suppressWarnings(dt2$Conductivity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Conductivity))==as.character(as.numeric("NA"))),NA,dt2$Conductivity))
dt2$Turbidity <- ifelse((trimws(as.character(dt2$Turbidity))==trimws("NA")),NA,dt2$Turbidity)               
suppressWarnings(dt2$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Turbidity))==as.character(as.numeric("NA"))),NA,dt2$Turbidity))
dt2$DO <- ifelse((trimws(as.character(dt2$DO))==trimws("NA")),NA,dt2$DO)               
suppressWarnings(dt2$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DO))==as.character(as.numeric("NA"))),NA,dt2$DO))
dt2$pH <- ifelse((trimws(as.character(dt2$pH))==trimws("NA")),NA,dt2$pH)               
suppressWarnings(dt2$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$pH))==as.character(as.numeric("NA"))),NA,dt2$pH))
dt2$Secchi <- ifelse((trimws(as.character(dt2$Secchi))==trimws("NA")),NA,dt2$Secchi)               
suppressWarnings(dt2$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Secchi))==as.character(as.numeric("NA"))),NA,dt2$Secchi))
dt2$Tide <- as.factor(ifelse((trimws(as.character(dt2$Tide))==trimws("NA")),NA,as.character(dt2$Tide)))
dt2$WeatherCode <- as.factor(ifelse((trimws(as.character(dt2$WeatherCode))==trimws("NA")),NA,as.character(dt2$WeatherCode)))
dt2$VegetationRank <- as.factor(ifelse((trimws(as.character(dt2$VegetationRank))==trimws("NA")),NA,as.character(dt2$VegetationRank)))
dt2$SubstrateCode <- as.factor(ifelse((trimws(as.character(dt2$SubstrateCode))==trimws("NA")),NA,as.character(dt2$SubstrateCode)))
dt2$HabitatType <- as.factor(ifelse((trimws(as.character(dt2$HabitatType))==trimws("NA")),NA,as.character(dt2$HabitatType)))
dt2$MicrocystisRank <- as.factor(ifelse((trimws(as.character(dt2$MicrocystisRank))==trimws("NA")),NA,as.character(dt2$MicrocystisRank)))
dt2$MethodCode <- as.factor(ifelse((trimws(as.character(dt2$MethodCode))==trimws("NA")),NA,as.character(dt2$MethodCode)))
dt2$GearCode <- as.factor(ifelse((trimws(as.character(dt2$GearCode))==trimws("NA")),NA,as.character(dt2$GearCode)))
dt2$GearConditionCode <- as.factor(ifelse((trimws(as.character(dt2$GearConditionCode))==trimws("NA")),NA,as.character(dt2$GearConditionCode)))
dt2$SampleAltered <- as.factor(ifelse((trimws(as.character(dt2$SampleAltered))==trimws("NA")),NA,as.character(dt2$SampleAltered)))
dt2$FieldComments <- as.factor(ifelse((trimws(as.character(dt2$FieldComments))==trimws("NA")),NA,as.character(dt2$FieldComments)))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(EventID)
summary(StationCode)
summary(Datetime)
summary(SampleDate)
summary(WaterTemp)
summary(SpecificConductance)
summary(Conductivity)
summary(Turbidity)
summary(DO)
summary(pH)
summary(Secchi)
summary(Tide)
summary(WeatherCode)
summary(VegetationRank)
summary(SubstrateCode)
summary(HabitatType)
summary(MicrocystisRank)
summary(MethodCode)
summary(GearCode)
summary(GearConditionCode)
summary(SampleAltered)
summary(FieldComments)
summary(Flag_WQ)
summary(Comment_WQ)
summary(Duplicated) 
# Get more details on character variables

summary(as.factor(dt2$EventID)) 
summary(as.factor(dt2$StationCode)) 
summary(as.factor(dt2$Tide)) 
summary(as.factor(dt2$WeatherCode)) 
summary(as.factor(dt2$VegetationRank)) 
summary(as.factor(dt2$SubstrateCode)) 
summary(as.factor(dt2$HabitatType)) 
summary(as.factor(dt2$MicrocystisRank)) 
summary(as.factor(dt2$MethodCode)) 
summary(as.factor(dt2$GearCode)) 
summary(as.factor(dt2$GearConditionCode)) 
summary(as.factor(dt2$SampleAltered)) 
summary(as.factor(dt2$FieldComments)) 
summary(as.factor(dt2$Flag_WQ)) 
summary(as.factor(dt2$Comment_WQ)) 
summary(as.factor(dt2$Duplicated))
detach(dt2)               

#Data Table 6, Fish & Water Quality###########

inUrl6  <- "https://pasta.lternet.edu/package/data/eml/edi/233/5/807b8cfb5edfd14bd9cd812c124327af" 
infile6 <- tempfile()
try(download.file(inUrl6,infile6,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")


dt6 <-read.csv(infile6,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "EventID",     
                 "StationCode",     
                 "Datetime",     
                 "SampleDate",     
                 "WaterTemp",     
                 "SpecificConductance",     
                 "Conductivity",     
                 "Turbidity",     
                 "DO",     
                 "pH",     
                 "Secchi",     
                 "Tide",     
                 "WeatherCode",     
                 "VegetationRank",     
                 "SubstrateCode",     
                 "HabitatType",     
                 "MicrocystisRank",     
                 "MethodCode",     
                 "GearCode",     
                 "GearConditionCode",     
                 "SampleAltered",     
                 "FieldComments",     
                 "Flag_WQ",     
                 "Comment_WQ",     
                 "Duplicated",     
                 "SampleID",     
                 "OrganismCode",     
                 "IEPFishCode",     
                 "Count",     
                 "SeineVolume",     
                 "TrapStatus",     
                 "TrapHours"    ), check.names=TRUE)

unlink(infile6)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt6$EventID)!="factor") dt6$EventID<- as.factor(dt6$EventID)
if (class(dt6$StationCode)!="factor") dt6$StationCode<- as.factor(dt6$StationCode)                                   
# attempting to convert dt6$Datetime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%m/%d/%Y %H:%M" 
tmp6Datetime<-as.POSIXct(dt6$Datetime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt6[dt6$Datetime != "",]) == length(tmp6Datetime[!is.na(tmp6Datetime)])){dt6$Datetime <- tmp6Datetime } else {print("Date conversion failed for dt6$Datetime. Please inspect the data and do the date conversion yourself.")}                                                                    

# attempting to convert dt6$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%m/%d/%Y"
tmp6SampleDate<-as.Date(dt6$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt6[dt6$SampleDate != "",]) == length(tmp6SampleDate[!is.na(tmp6SampleDate)])){dt6$SampleDate <- tmp6SampleDate } else {print("Date conversion failed for dt6$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt6$WaterTemp)=="factor") dt6$WaterTemp <-as.numeric(levels(dt6$WaterTemp))[as.integer(dt6$WaterTemp) ]               
if (class(dt6$WaterTemp)=="character") dt6$WaterTemp <-as.numeric(dt6$WaterTemp)
if (class(dt6$SpecificConductance)=="factor") dt6$SpecificConductance <-as.numeric(levels(dt6$SpecificConductance))[as.integer(dt6$SpecificConductance) ]               
if (class(dt6$SpecificConductance)=="character") dt6$SpecificConductance <-as.numeric(dt6$SpecificConductance)
if (class(dt6$Conductivity)=="factor") dt6$Conductivity <-as.numeric(levels(dt6$Conductivity))[as.integer(dt6$Conductivity) ]               
if (class(dt6$Conductivity)=="character") dt6$Conductivity <-as.numeric(dt6$Conductivity)
if (class(dt6$Turbidity)=="factor") dt6$Turbidity <-as.numeric(levels(dt6$Turbidity))[as.integer(dt6$Turbidity) ]               
if (class(dt6$Turbidity)=="character") dt6$Turbidity <-as.numeric(dt6$Turbidity)
if (class(dt6$DO)=="factor") dt6$DO <-as.numeric(levels(dt6$DO))[as.integer(dt6$DO) ]               
if (class(dt6$DO)=="character") dt6$DO <-as.numeric(dt6$DO)
if (class(dt6$pH)=="factor") dt6$pH <-as.numeric(levels(dt6$pH))[as.integer(dt6$pH) ]               
if (class(dt6$pH)=="character") dt6$pH <-as.numeric(dt6$pH)
if (class(dt6$Secchi)=="factor") dt6$Secchi <-as.numeric(levels(dt6$Secchi))[as.integer(dt6$Secchi) ]               
if (class(dt6$Secchi)=="character") dt6$Secchi <-as.numeric(dt6$Secchi)
if (class(dt6$Tide)!="factor") dt6$Tide<- as.factor(dt6$Tide)
if (class(dt6$WeatherCode)!="factor") dt6$WeatherCode<- as.factor(dt6$WeatherCode)
if (class(dt6$VegetationRank)!="factor") dt6$VegetationRank<- as.factor(dt6$VegetationRank)
if (class(dt6$SubstrateCode)!="factor") dt6$SubstrateCode<- as.factor(dt6$SubstrateCode)
if (class(dt6$HabitatType)!="factor") dt6$HabitatType<- as.factor(dt6$HabitatType)
if (class(dt6$MicrocystisRank)!="factor") dt6$MicrocystisRank<- as.factor(dt6$MicrocystisRank)
if (class(dt6$MethodCode)!="factor") dt6$MethodCode<- as.factor(dt6$MethodCode)
if (class(dt6$GearCode)!="factor") dt6$GearCode<- as.factor(dt6$GearCode)
if (class(dt6$GearConditionCode)!="factor") dt6$GearConditionCode<- as.factor(dt6$GearConditionCode)
if (class(dt6$SampleAltered)!="factor") dt6$SampleAltered<- as.factor(dt6$SampleAltered)
if (class(dt6$FieldComments)!="factor") dt6$FieldComments<- as.factor(dt6$FieldComments)
if (class(dt6$Flag_WQ)!="factor") dt6$Flag_WQ<- as.factor(dt6$Flag_WQ)
if (class(dt6$Comment_WQ)!="factor") dt6$Comment_WQ<- as.factor(dt6$Comment_WQ)
if (class(dt6$Duplicated)!="factor") dt6$Duplicated<- as.factor(dt6$Duplicated)
if (class(dt6$SampleID)!="factor") dt6$SampleID<- as.factor(dt6$SampleID)
if (class(dt6$OrganismCode)!="factor") dt6$OrganismCode<- as.factor(dt6$OrganismCode)
if (class(dt6$IEPFishCode)!="factor") dt6$IEPFishCode<- as.factor(dt6$IEPFishCode)
if (class(dt6$Count)=="factor") dt6$Count <-as.numeric(levels(dt6$Count))[as.integer(dt6$Count) ]               
if (class(dt6$Count)=="character") dt6$Count <-as.numeric(dt6$Count)
if (class(dt6$SeineVolume)=="factor") dt6$SeineVolume <-as.numeric(levels(dt6$SeineVolume))[as.integer(dt6$SeineVolume) ]               
if (class(dt6$SeineVolume)=="character") dt6$SeineVolume <-as.numeric(dt6$SeineVolume)
if (class(dt6$TrapStatus)!="factor") dt6$TrapStatus<- as.factor(dt6$TrapStatus)
if (class(dt6$TrapHours)=="factor") dt6$TrapHours <-as.numeric(levels(dt6$TrapHours))[as.integer(dt6$TrapHours) ]               
if (class(dt6$TrapHours)=="character") dt6$TrapHours <-as.numeric(dt6$TrapHours)

# Convert Missing Values to NA for non-dates

dt6$WaterTemp <- ifelse((trimws(as.character(dt6$WaterTemp))==trimws("NA")),NA,dt6$WaterTemp)               
suppressWarnings(dt6$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$WaterTemp))==as.character(as.numeric("NA"))),NA,dt6$WaterTemp))
dt6$SpecificConductance <- ifelse((trimws(as.character(dt6$SpecificConductance))==trimws("NA")),NA,dt6$SpecificConductance)               
suppressWarnings(dt6$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt6$SpecificConductance))
dt6$Conductivity <- ifelse((trimws(as.character(dt6$Conductivity))==trimws("NA")),NA,dt6$Conductivity)               
suppressWarnings(dt6$Conductivity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Conductivity))==as.character(as.numeric("NA"))),NA,dt6$Conductivity))
dt6$Turbidity <- ifelse((trimws(as.character(dt6$Turbidity))==trimws("NA")),NA,dt6$Turbidity)               
suppressWarnings(dt6$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Turbidity))==as.character(as.numeric("NA"))),NA,dt6$Turbidity))
dt6$DO <- ifelse((trimws(as.character(dt6$DO))==trimws("NA")),NA,dt6$DO)               
suppressWarnings(dt6$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$DO))==as.character(as.numeric("NA"))),NA,dt6$DO))
dt6$pH <- ifelse((trimws(as.character(dt6$pH))==trimws("NA")),NA,dt6$pH)               
suppressWarnings(dt6$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$pH))==as.character(as.numeric("NA"))),NA,dt6$pH))
dt6$Secchi <- ifelse((trimws(as.character(dt6$Secchi))==trimws("NA")),NA,dt6$Secchi)               
suppressWarnings(dt6$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Secchi))==as.character(as.numeric("NA"))),NA,dt6$Secchi))
dt6$Tide <- as.factor(ifelse((trimws(as.character(dt6$Tide))==trimws("NA")),NA,as.character(dt6$Tide)))
dt6$WeatherCode <- as.factor(ifelse((trimws(as.character(dt6$WeatherCode))==trimws("NA")),NA,as.character(dt6$WeatherCode)))
dt6$VegetationRank <- as.factor(ifelse((trimws(as.character(dt6$VegetationRank))==trimws("NA")),NA,as.character(dt6$VegetationRank)))
dt6$SubstrateCode <- as.factor(ifelse((trimws(as.character(dt6$SubstrateCode))==trimws("NA")),NA,as.character(dt6$SubstrateCode)))
dt6$HabitatType <- as.factor(ifelse((trimws(as.character(dt6$HabitatType))==trimws("NA")),NA,as.character(dt6$HabitatType)))
dt6$MicrocystisRank <- as.factor(ifelse((trimws(as.character(dt6$MicrocystisRank))==trimws("NA")),NA,as.character(dt6$MicrocystisRank)))
dt6$MethodCode <- as.factor(ifelse((trimws(as.character(dt6$MethodCode))==trimws("NA")),NA,as.character(dt6$MethodCode)))
dt6$GearCode <- as.factor(ifelse((trimws(as.character(dt6$GearCode))==trimws("NA")),NA,as.character(dt6$GearCode)))
dt6$GearConditionCode <- as.factor(ifelse((trimws(as.character(dt6$GearConditionCode))==trimws("NA")),NA,as.character(dt6$GearConditionCode)))
dt6$SampleAltered <- as.factor(ifelse((trimws(as.character(dt6$SampleAltered))==trimws("NA")),NA,as.character(dt6$SampleAltered)))
dt6$FieldComments <- as.factor(ifelse((trimws(as.character(dt6$FieldComments))==trimws("NA")),NA,as.character(dt6$FieldComments)))
dt6$Count <- ifelse((trimws(as.character(dt6$Count))==trimws("NA")),NA,dt6$Count)               
suppressWarnings(dt6$Count <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$Count))==as.character(as.numeric("NA"))),NA,dt6$Count))
dt6$SeineVolume <- ifelse((trimws(as.character(dt6$SeineVolume))==trimws("NA")),NA,dt6$SeineVolume)               
suppressWarnings(dt6$SeineVolume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$SeineVolume))==as.character(as.numeric("NA"))),NA,dt6$SeineVolume))
dt6$TrapStatus <- as.factor(ifelse((trimws(as.character(dt6$TrapStatus))==trimws("NA")),NA,as.character(dt6$TrapStatus)))
dt6$TrapHours <- ifelse((trimws(as.character(dt6$TrapHours))==trimws("NA")),NA,dt6$TrapHours)               
suppressWarnings(dt6$TrapHours <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$TrapHours))==as.character(as.numeric("NA"))),NA,dt6$TrapHours))


# Here is the structure of the input data frame:
str(dt6)                            
attach(dt6)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(EventID)
summary(StationCode)
summary(Datetime)
summary(SampleDate)
summary(WaterTemp)
summary(SpecificConductance)
summary(Conductivity)
summary(Turbidity)
summary(DO)
summary(pH)
summary(Secchi)
summary(Tide)
summary(WeatherCode)
summary(VegetationRank)
summary(SubstrateCode)
summary(HabitatType)
summary(MicrocystisRank)
summary(MethodCode)
summary(GearCode)
summary(GearConditionCode)
summary(SampleAltered)
summary(FieldComments)
summary(Flag_WQ)
summary(Comment_WQ)
summary(Duplicated)
summary(SampleID)
summary(OrganismCode)
summary(IEPFishCode)
summary(Count)
summary(SeineVolume)
summary(TrapStatus)
summary(TrapHours) 
# Get more details on character variables

summary(as.factor(dt6$EventID)) 
summary(as.factor(dt6$StationCode)) 
summary(as.factor(dt6$Tide)) 
summary(as.factor(dt6$WeatherCode)) 
summary(as.factor(dt6$VegetationRank)) 
summary(as.factor(dt6$SubstrateCode)) 
summary(as.factor(dt6$HabitatType)) 
summary(as.factor(dt6$MicrocystisRank)) 
summary(as.factor(dt6$MethodCode)) 
summary(as.factor(dt6$GearCode)) 
summary(as.factor(dt6$GearConditionCode)) 
summary(as.factor(dt6$SampleAltered)) 
summary(as.factor(dt6$FieldComments)) 
summary(as.factor(dt6$Flag_WQ)) 
summary(as.factor(dt6$Comment_WQ)) 
summary(as.factor(dt6$Duplicated)) 
summary(as.factor(dt6$SampleID)) 
summary(as.factor(dt6$OrganismCode)) 
summary(as.factor(dt6$IEPFishCode)) 
summary(as.factor(dt6$TrapStatus))
detach(dt6)               



#EDA#######
dt6$year <- format(dt6$SampleDate, "%Y")

# Convert to numeric (optional)
dt6$year <- as.numeric(dt6$year)


for (i in seq(1998:2024)) {
  dt6Fish <- dt6 %>%
      summarize(sum(Count[year == i]))
}
ggplot(dt6, aes(x = ))

?unique
?count

dt6_1998 <- dt6 %>%
  filter(year == '1998')

dt6_2024 <- dt6 %>%
  filter(year == '2024')


sum(dt6$Count)

length(unique(dt6_1999$OrganismCode))
length(unique(dt6_2002$OrganismCode))

dt6_yc <- dt6 %>%
  group_by(year) %>%
  summarise(yc = sum(Count, na.rm = TRUE))



ggplot(dt6_yc, aes(x = year, y = yc)) + 
  geom_line()
    
  

