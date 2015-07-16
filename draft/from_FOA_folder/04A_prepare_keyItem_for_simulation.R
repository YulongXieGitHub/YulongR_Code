#
# 04A_prepare_keyItem_for_simulation.R 
#
# In Windows: need to invoke R i386 not X64
#
# DuctLeakage Rate and Ratio:
# IECC2009: ratio = 0.2  --> 0.2 X 1350 / (2400/100) = 11.25 cfm  (11.25 cfm / 100 sf)
# IECC2015: ratio = 0.08 --> 0.08 X 1200 / (2400/100) = 4 cfm     ( 4 cfm/100 sf)
#
# Input needed:
# "\\poncho\resstd\FOA\Phase1_Analysis\RCD_Analysis\00_auxi_files\MapTable.csv": 			map of code item and its short description from Mark haverlson 
# "\\poncho\resstd\FOA\Phase1_Analysis\RCD_Analysis\00_auxi_files\119_location_mapping_YLX.csv":	the list of the 119 Climate Cities used in Residential Analysis and their TMY3 EPW files
#
# May 8, 2015:
# There are three data issues which did not have in the Phase0 sample size design simulation but we need consider in the Phase 1 simulation
#
# (1) Cavity and Constinuous Insulation.
#     Wall insulation have two options (a) Cavity and (b) Cavity plus Continuous.
#     Although other (floor, ceiling) also have observation of both, we decided to only consider Cavity Insulation. i.e., treat cavity and continuous both as cavity insulation.
#
# (2) Mass wall and Frame wall: In Phase 0 we only considered Frame wall, not we need to habe mass wall simulated as they are observed.  Corresponding baseline models are needed for both types of walls.
#
# (3) Insulation quality will be taken as a degradation factor in the simulation.  Probably only for wall not for floor and ceiling.
#
# Input:
# 
# (1) array.states: 
#     list of states in FOA
# (2) 00_auxi_files/MapTable.csv: 
#     map the code item to their short description
# (3) list.keyItem: 
#     list of ket code items and expanded with insulation quality based on April 29 and May 1, 2015 email exchanges among Vrushali, Mark Haverson and YuLong
# (4) myIECC2009/myIECC2015: 
#     IECC code requirements
# (5) timeStamp.string: 
#     time of data retrival from database
# (6) Path.Data,paste(paste("AllData",timeStamp.string,sep="_"),".RDS": 
#     Data file retrieved from database 
#
# Output:
# (1) FL.KeyItem.Wide.RDS/FL.KeyItem.Wide.RDS
#     Key Code Item Arrays to be used for parm generation
#
# Maryland uses IECC2015: see Mark April 9, 2015 11:03 am.
# 
# April 19, 2015: add one more output on the database tables
#
# Created on April 13, 2015
#
# For each state, prepare two tables to list the values of the key code item in each unique building surveyed
# (1) one for all code items
# (2) two for key code items
#
# Query the FOA RCD database "checkweb_foa_04072015"
#
# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
#
# 	eliminate all stuff
# 
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# close all devices which have been opened
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}


# today's month, day and year
month <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
day   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
year  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]

hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]



#
# define the directory
#
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "/phome/resstd/FOA/Phase1_Analysis/RCD_Analysis/0_scripts"
	Path.Project <- "/phome/resstd/FOA/Phase1_Analysis/RCD_Analysis"
}else{
	Path.Current <- "Y:/FOA/Phase1_Analysis/RCD_Analysis/0_scripts"
	Path.Project <- "Y:/FOA/Phase1_Analysis/RCD_Analysis"
}
setwd(Path.Current)

# load packages
library("RODBC")
library(ggplot2)
library(reshape2)

# lis of states in the FOA
array.states <- c("AL","AR","GA","KY","MD","NC","PA","TX")
array.states <- c("AL","GA","KY","MD","NC","PA","TX","AR")
array.states <- c("MD","AL","GA","KY","NC","PA","TX","AR")


# -------------------------------------------------------------------------------------------------
# Select data from corresponding downloading
# -------------------------------------------------------------------------------------------------
timeStamp.string <- "2015Jul06"
timeStamp.string <- "2015Jun15"



# -------------------------------------------------------------------------------------------------
# 1. setup output and log directory
# -------------------------------------------------------------------------------------------------
# define path
Path.Data        <- paste(Path.Project,"01_extraction_for_QA",timeStamp.string,sep="/")		# Input data directory
Path.TMP         <- paste(Path.Project,"04A_prepare_keyItem_for_simulation",    sep="/")	# Output file directory			
Path.OUT         <- paste(Path.TMP,timeStamp.string,                           sep="/")		# Output file directory	for a particular batch of downloading from database		
Path.LOG         <- paste(Path.Project,"00_log",                               sep="/")		# log file directory	
Path.auxi        <- paste(Path.Project,"00_auxi_files",                        sep="/")		# Input auxiary file directory
if (!file.exists(Path.Data)){print(paste("NOT existing:",Path.Data," Check Why!",sep=""));die}
if (!file.exists(Path.TMP)) {print(paste("NOT existing:",Path.TMP,sep=""));dir.create(Path.TMP,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.OUT)) {print(paste("NOT existing:",Path.OUT,sep=""));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.LOG)) {print(paste("NOT existing:",Path.LOG,sep=""));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.auxi)){print(paste("NOT existing:",Path.auxi," Check Why!",sep=""));die}

# define files
FL.LOG          <- paste(Path.LOG,  paste("04A_prepare_keyItem_for_simulation_",timeStamp.string,".log",sep=""),sep="/")	
FL.MAP          <- paste(Path.auxi,"MapTable.csv",sep="/")					# a map between code item and its short description as well as the range if it is a numeric item
FL.allData.SUM  <- paste(Path.OUT,paste("AllData_",timeStamp.string,"_SUM.CSV",sep=""),sep="/")
if(file.exists(FL.LOG))        {print(paste(FL.LOG,         " exist. Delete it!"));file.remove(FL.LOG)}
if(!(file.exists(FL.MAP)))     {print(paste(FL.MAP," does not exist. Check why!"));die}
if(file.exists(FL.allData.SUM)){print(paste(FL.allData.SUM, " exist. Delete it!"));file.remove(FL.allData.SUM)}			
cat(paste("1A. Setup paths and files.\n",sep=""))
cat(paste("1A. Setup paths and files.\n",sep=""),file=FL.LOG, append=TRUE)								


source("multipleplot.R")
cat(paste("1B. insert a [multipleplot] function for ggplot2.\n",sep=""))
cat(paste("1B. insert a [multipleplot] function for ggplot2.\n",sep=""),file=FL.LOG,append=TRUE)	


cat(paste("Log file for data processing script [04A_prepare_keyItem_for_simulation.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
            "*                    [04A_prepare_keyItem_for_simulation.R]                *",
            "***************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)


# June 9, 2015: add the CrawlWallR (use 0 not confirm with the code book yet)
#
# IECC Core REquirement
#
IECC_2009 <- data.frame(CZ2 = c(30, 13,  4, 13, 0.65, 0.3, 50, 7,  0,  0,11.25,0),
		        CZ3 = c(30, 13,  8, 19, 0.50, 0.3, 50, 7, 13,  0,11.25,0),
		        CZ4 = c(38, 13,  8, 19, 0.35, 0.4, 50, 7, 13, 10,11.25,0),
		        CZ5 = c(38, 21, 13, 30, 0.35, 0.4, 50, 7, 13, 10,11.25,0),	# (1) even the code requirement is 20 for CZ 5 & 6, we use 21.  (2) the NR shgc for CZ 4|5|6, we use 0.4 ad default.  (3) for slab thickness, the default is 2 in.  Even they are 4 in for CZ6, we use 2 in.  Vrushali confirmed.
		        CZ6 = c(49, 21, 15, 30, 0.35, 0.4, 50, 7, 19, 10,11.25,0))
row.names(IECC_2009) <- c("CeilingR","FrameWallR","MassWallR","FloorR","WindowU","WindowSHGC","HighEffLamps","ACH50","BsmtWallR","SlabEdgeR","DuctTightness","CrawlWallR") 
IECC_2009[,"KeyCode"] <- row.names(IECC_2009)
id.vars <- "KeyCode"
measure.vars <- c("CZ2","CZ3","CZ4","CZ5","CZ6")
myIECC2009 <- melt(IECC_2009,id.vars = id.vars,measure.vars = measure.vars,variable.name = "ClimateZone",value.name = "CodeCompliance")
cat(paste("1C. add IECC2009 code requirement.\n",sep=""))
cat(paste("1C. add IECC2009 code requirement.\n",sep=""),file=FL.LOG,append=TRUE)	

# April 28, 2015: Table R402.1.1 at page R-29 of Chapter 4 of IECC2012 (IECC2015 does not change the prescriptive values)
#                 R4402.4.1.2 Testing: ACH at 50 pascl are 5 for CZ1 & 2 and 3 for CZ 3 to 8.
#                 R404.1. Lighting: 75% high efficiency lighting
#                 Note: CZ4C is the same as CZ5, since we do not have CZ4C in this FOA study, we did not make such distinction.
#                 For Frame Wall: R 20 still use 21 for simulation purpose.
IECC_2015 <- data.frame(CZ2 = c(38, 13,  4, 13, 0.40, 0.25, 75, 5,  0,  0,4.0,0),
		        CZ3 = c(38, 21,  8, 19, 0.35, 0.25, 75, 3, 13,  0,4.0,0),
		        CZ4 = c(49, 21,  8, 19, 0.35, 0.40, 75, 3, 13, 10,4.0,0),
		        CZ5 = c(49, 21, 13, 30, 0.32, 0.40, 75, 3, 19, 10,4.0,0),	# (1) even the code requirement is 20 for CZ 5 & 6, we use 21.  (2) the NR shgc for CZ 4|5|6, we use 0.4 ad default.  (3) for slab thickness, the default is 2 in.  Even they are 4 in for CZ6, we use 2 in.  Vrushali confirmed.
		        CZ6 = c(49, 21, 15, 30, 0.32, 0.40, 75, 3, 19, 10,4.0,0))
row.names(IECC_2015) <- c("CeilingR","FrameWallR","MassWallR","FloorR","WindowU","WindowSHGC","HighEffLamps","ACH50","BsmtWallR","SlabEdgeR","DuctTightness","CrawlWallR") 
IECC_2015[,"KeyCode"] <- row.names(IECC_2015)
id.vars <- "KeyCode"
measure.vars <- c("CZ2","CZ3","CZ4","CZ5","CZ6")
myIECC2015 <- melt(IECC_2015,id.vars = id.vars,measure.vars = measure.vars,variable.name = "ClimateZone",value.name = "CodeCompliance")
cat(paste("1C. add IECC2015 code requirement.\n",sep=""))
cat(paste("1C. add IECC2015 code requirement.\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# input the map between code item and its short description
# -------------------------------------------------------------------------------------------------
myMap <- read.table(file=FL.MAP,header=TRUE,sep=",",stringsAsFactors=FALSE)
row.names(myMap) <- myMap[,"ID"]
cat(paste("2A. a map between code item and its short description as well as rnage for numeric item is loaded.\n",sep=""))
cat(paste("2A. a map between code item and its short description as well as rnage for numeric item is loaded.\n",sep=""),file=FL.LOG, append=TRUE)								



# -------------------------------------------------------------------------------------------------
# list of key code items
# -------------------------------------------------------------------------------------------------
#       list.keyItem  <- c("FI1",     "FI17", "FI4",          "FI4b",        "FI6",     "FO1",      "FO4a",             "FO4b",           "FO7a",              "FO7b",            "FR10a",            "FR10b",          "FR2",    "FR3",       "IN1a",          "IN1b",        "IN3a",              "IN3b")
#       name.keyItem  <- c("CeilingR","ACH50","DuctTightness","DuctLeageExt","HighEffLamps","SlabEdgeR","BsmtWallR(Cavity)","BsmtWallR(Cont)","CrawlWallR(Cavity)","CrawlWallR(Cont)","MassWallR(Cavity)","MassWallR(Cont)","WindowU","WindowSHGC","FloorR(Cavity)","FloorR(Cont)","FrameWallR(Cavity)","FrameWallR(Cont)")
# names(name.keyItem) <- list.keyItem
# May 8, 2015: besides the original 18 key code items, insulation quality code item are retrieved as well which will be used to aling the observations.
#              9 more code items are added for the data preparartion.
#        list.keyItem  <- c("FI1",     "FI17", "FI4",          "FI4b",        "FI6",         "FO1",      "FO4a",             "FO4b",           "FO7a",              "FO7b",            "FR10a",            "FR10b",          "FR2",    "FR3",       "IN1a",          "IN1b",        "IN3a",              "IN3b",            "IQ1",           "IQ2",            "IQ3",           "IQ4",               "MIQ1",               "CSIQ1",                   "BG17")
#        name.keyItem  <- c("CeilingR","ACH50","DuctTightness","DuctLeageExt","HighEffLamps","SlabEdgeR","BsmtWallR(Cavity)","BsmtWallR(Cont)","CrawlWallR(Cavity)","CrawlWallR(Cont)","MassWallR(Cavity)","MassWallR(Cont)","WindowU","WindowSHGC","FloorR(Cavity)","FloorR(Cont)","FrameWallR(Cavity)","FrameWallR(Cont)","IQ(RoofCavity)","IQ(FloorCavity)","IQ(WallCavity)","IQ(BsmtWallCavity)","IQ(MassWallCavity)", "IQ(CrawlSpaceWallCavity)","PredominantFoundation")
#  names(name.keyItem) <- list.keyItem

# May 13, 2015: since we are created a few compund field for wall, ceiling and floor, we need have them being able to be mapped!!
#        list.keyItem  <- c("FI1",     "FI17", "FI4",          "FI4b",        "FI6",         "FO1",      "FO4a",             "FO4b",           "FO7a",              "FO7b",            "FR10a",            "FR10b",          "FR2",    "FR3",       "IN1a",          "IN1b",        "IN3a",              "IN3b",            "IQ1",           "IQ2",            "IQ3",           "IQ4",               "MIQ1",               "CSIQ1",                   "BG17",                 "CompWallR","CompCeilingR","CompBsmtWallR","CompFloorR","CompCrawlWallR")
#        name.keyItem  <- c("CeilingR","ACH50","DuctTightness","DuctLeageExt","HighEffLamps","SlabEdgeR","BsmtWallR(Cavity)","BsmtWallR(Cont)","CrawlWallR(Cavity)","CrawlWallR(Cont)","MassWallR(Cavity)","MassWallR(Cont)","WindowU","WindowSHGC","FloorR(Cavity)","FloorR(Cont)","FrameWallR(Cavity)","FrameWallR(Cont)","IQ(RoofCavity)","IQ(FloorCavity)","IQ(WallCavity)","IQ(BsmtWallCavity)","IQ(MassWallCavity)", "IQ(CrawlSpaceWallCavity)","PredominantFoundation","CompWallR","CompCeilingR","CompBsmtWallR","CompFloorR","CompCrawlWallR")
#  names(name.keyItem) <- list.keyItem

# May 19, 2015: we are trying to derive the foundation shares and heating system share from the filed observations!!
        list.keyItem  <- c("FI1",     "FI17", "FI4",          "FI4b",        "FI6",         "FO1",      "FO4a",             "FO4b",           "FO7a",              "FO7b",            "FR10a",            "FR10b",          "FR2",    "FR3",       "IN1a",          "IN1b",        "IN3a",              "IN3b",            "IQ1",           "IQ2",            "IQ3",           "IQ4",               "MIQ1",               "CSIQ1",                    "BG17",                 "EQ1",                      "EQ2",               "CompWallR","CompCeilingR","CompBsmtWallR","CompFloorR","CompCrawlWallR")
        name.keyItem  <- c("CeilingR","ACH50","DuctTightness","DuctLeageExt","HighEffLamps","SlabEdgeR","BsmtWallR(Cavity)","BsmtWallR(Cont)","CrawlWallR(Cavity)","CrawlWallR(Cont)","MassWallR(Cavity)","MassWallR(Cont)","WindowU","WindowSHGC","FloorR(Cavity)","FloorR(Cont)","FrameWallR(Cavity)","FrameWallR(Cont)","IQ(RoofCavity)","IQ(FloorCavity)","IQ(WallCavity)","IQ(BsmtWallCavity)","IQ(MassWallCavity)", "IQ(CrawlSpaceWallCavity)", "PredominantFoundation","PredominantHeatingSource", "heatingSystemType", "CompWallR","CompCeilingR","CompBsmtWallR","CompFloorR","CompCrawlWallR")
  names(name.keyItem) <- list.keyItem
cat(paste("2B. specify the key code items to a list.\n",sep=""))
cat(paste("2B. specify the key code items to a list.\n",sep=""),file=FL.LOG, append=TRUE)								





# May 8, 2015: Note the following mapping between Key Code item and their insulation quality
#              We do not have "CrawlSpace", "DuctTightness" and "DuctLeakageExt" in the model.
# "FI1",      	   "IN1a",           "IN3a",              "FO4a",              "FR10a",             "FO7a",                    "KW1",              "KW2")
# "CeilingR",	   "FloorR(Cavity)", "FrameWallR(Cavity)","BsmtWallR(Cavity)", "MassWallR(Cavity)", "CrawlWallR(Cavity)",      "KneeWallR(Cavity)","KneeWallR(Cont)")
# "IQ1",           "IQ2",            "IQ3",               "IQ4",               "MIQ1",              "CSIQ1",                   "KW5")
# "IQ(RoofCavity)","IQ(FloorCavity)","IQ(WallCavity)",    "IQ(BsmtWallCavity)","IQ(MassWallCavity)","IQ(CrawlSpaceWallCavity)","IQ(KneeWall)")
# There are only insulation quality for "FI1", "IN1a", "IN3a", "FO4a", "FR10a", "FO7a" will be used



# loopping through the state and all state data
cat(paste("2C. loopping through states.\n",sep=""))
cat(paste("2C. loopping through states.\n",sep=""),file=FL.LOG, append=TRUE)								

# ---------------------------------------------------------------------------------------------------------
# Looping through states
# ---------------------------------------------------------------------------------------------------------
for (this.state in array.states)
{
	if (this.state == "MD")			# Maryland use IECC2015 instead of IECC2009
	{
		vertical.line.string <- "Vertical Line: IECC2015 Code Requirement"
		maximum_supply_cfm     <- 1200	
	}else{
		vertical.line.string <- "Vertical Line: IECC2009 Code Requirement"
		maximum_supply_cfm     <- 1350	
	}
	
	
	# -------------------------------------------------------------------------------------------------
	# A. specify the output file names for this state
	# -------------------------------------------------------------------------------------------------
	if (this.state == "AllData")
	{
		string.thisState                <- paste("[",this.state,"]",sep="")
		FL.IN.RDS                       <- paste(Path.Data,paste(paste("AllData",timeStamp.string,sep="_"),".RDS",                 sep=""),sep="/")
		FL.AllItem.COMP.RDS             <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_AllItem_Complete.RDS",sep=""),sep="/")
		FL.AllItem.COMP.CSV             <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_AllItem_Complete.CSV",sep=""),sep="/")

		FL.SUM.ALL.CSV                  <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_sum_allItem.csv", sep=""),sep="/")
		FL.SUM.KEY.CSV                  <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_sum_keyitems.csv",sep=""),sep="/")

		FL.AllItem.RvNA.Long.RDS        <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_AllItem_NARemoved_Long.RDS",sep=""),sep="/")
		FL.AllItem.RvNA.Long.CSV        <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_AllItem_NARemoved_Long.CSV",sep=""),sep="/")
		
		FL.AllItem.RvNA.Wide_Before.RDS <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_AllItem_NARemoved_Wide_Before.RDS",sep=""),sep="/")
		FL.AllItem.RvNA.Wide_Before.CSV <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_AllItem_NARemoved_Wide_Before.CSV",sep=""),sep="/")

		FL.AllItem.RvNA.Wide_After.RDS  <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_AllItem_NARemoved_Wide_After.RDS", sep=""),sep="/")
		FL.AllItem.RvNA.Wide_After.CSV  <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_AllItem_NARemoved_Wide_After.CSV", sep=""),sep="/")

		FL.found.htgSys.share.OBJ       <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_found.htgSys.share.Rdata", sep=""),sep="/")
		FL.found.htgSys.share.CSV       <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_found.htgSys.share.CSV", sep=""),sep="/")
		
		FL.KeyItem.CSV                  <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_KeyItem.CSV",     sep=""),sep="/")
		FL.KeyItem.Wide.RDS             <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_KeyItem_Wide.RDS",sep=""),sep="/")
		FL.KeyItem.Long.RDS             <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_KeyItem_Long.RDS",sep=""),sep="/")
		FL.KeyItem.Wide.CSV             <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_KeyItem_Wide.CSV",sep=""),sep="/")
		FL.KeyItem.Long.CSV             <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_KeyItem_Long.CSV",sep=""),sep="/")
		FL.KeyItem.PDF                  <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_KeyItem.PDF",     sep=""),sep="/")
		FL.KeyItem.SUM                  <- paste(Path.OUT ,paste(paste("AllData",timeStamp.string,sep="_"),"_KeyItem_sum.CSV", sep=""),sep="/")
	}else{
		string.thisState                <- paste("State [",this.state,"]",sep="")
		FL.IN.RDS                       <- paste(Path.Data,paste(paste("State",this.state,timeStamp.string,sep="_"),".RDS",                 sep=""),sep="/")
		FL.AllItem.COMP.RDS             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_AllItem_Complete.RDS",sep=""),sep="/")
		FL.AllItem.COMP.CSV             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_AllItem_Complete.CSV",sep=""),sep="/")
		
		FL.SUM.ALL.CSV                  <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_sum_allItem.csv", sep=""),sep="/")
		FL.SUM.KEY.CSV                  <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_sum_keyitems.csv",sep=""),sep="/")
		
		FL.AllItem.RvNA.Long.RDS        <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_AllItem_NARemoved_Long.RDS",sep=""),sep="/")
		FL.AllItem.RvNA.Long.CSV        <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_AllItem.NARemoved_Long.CSV",sep=""),sep="/")

		FL.AllItem.RvNA.Wide_Before.RDS <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_AllItem_NARemoved_Wide_Before.RDS",sep=""),sep="/")
		FL.AllItem.RvNA.Wide_Before.CSV <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_AllItem_NARemoved_Wide_Before.CSV",sep=""),sep="/")

		FL.AllItem.RvNA.Wide_After.RDS  <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_AllItem_NARemoved_Wide_After.RDS", sep=""),sep="/")
		FL.AllItem.RvNA.Wide_After.CSV  <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_AllItem_NARemoved_Wide_After.CSV", sep=""),sep="/")

		FL.found.htgSys.share.OBJ       <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_found.htgSys.share.Rdata", sep=""),sep="/")
		FL.found.htgSys.share.CSV       <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_found.htgSys.share.CSV", sep=""),sep="/")
		
		FL.KeyItem.CSV                  <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem.CSV",     sep=""),sep="/")
		FL.KeyItem.Wide.RDS             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Wide.RDS",sep=""),sep="/")
		FL.KeyItem.Long.RDS             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Long.RDS",sep=""),sep="/")
		FL.KeyItem.Wide.CSV             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Wide.CSV",sep=""),sep="/")
		FL.KeyItem.Long.CSV             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Long.CSV",sep=""),sep="/")
		FL.KeyItem.PDF                  <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem.PDF",     sep=""),sep="/")	
		FL.KeyItem.SUM                  <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_sum.CSV", sep=""),sep="/")	
		
		# July 8, 2015: add an one page plot to display all numeric field of the field observations in a state
		FL.NumericItem.PDF              <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_NumericItem.PDF", sep=""),sep="/")	
		FL.NumericItem.CSV              <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_NumericItem.CSV", sep=""),sep="/")	
		FL.NumericItem.RDS              <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_NumericItem.RDS", sep=""),sep="/")
		FL.NumericItem.JPG              <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_NumericItem.JPG", sep=""),sep="/")

		# July 9, 2015: data summary
		FL.DataSummary.CSV              <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_DataSummary.CSV", sep=""),sep="/")	
		FL.DataSummary.RDS              <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_DataSummary.RDS", sep=""),sep="/")

		
	}
	if  (file.exists(FL.SUM.ALL.CSV ))                {print(paste(FL.SUM.ALL.CSV,                  " exist. Delete it!"));file.remove(FL.SUM.ALL.CSV)}
	if  (file.exists(FL.SUM.KEY.CSV))                 {print(paste(FL.SUM.KEY.CSV,                  " exist. Delete it!"));file.remove(FL.SUM.KEY.CSV)}
	
	if (!file.exists(FL.IN.RDS))                      {print(paste("NOT existing:",FL.IN.RDS," Check Why!",sep=""));die}	
	if  (file.exists(FL.AllItem.COMP.RDS))            {print(paste(FL.AllItem.COMP.RDS,             " exist. Delete it!"));file.remove(FL.AllItem.COMP.RDS)}
	if  (file.exists(FL.AllItem.COMP.CSV))            {print(paste(FL.AllItem.COMP.CSV,             " exist. Delete it!"));file.remove(FL.AllItem.COMP.CSV)}		# Key code item data with minimal cleaning
	if  (file.exists(FL.AllItem.RvNA.Long.RDS))       {print(paste(FL.AllItem.RvNA.Long.RDS,        " exist. Delete it!"));file.remove(FL.AllItem.RvNA.Long.RDS)}
	if  (file.exists(FL.AllItem.RvNA.Long.CSV))       {print(paste(FL.AllItem.RvNA.Long.CSV,        " exist. Delete it!"));file.remove(FL.AllItem.RvNA.Long.CSV)}		# Key code item data with minimal cleaning
	if  (file.exists(FL.AllItem.RvNA.Wide_After.RDS)) {print(paste(FL.AllItem.RvNA.Wide_After.RDS,  " exist. Delete it!"));file.remove(FL.AllItem.RvNA.Wide_After.RDS)}
	if  (file.exists(FL.AllItem.RvNA.Wide_Before.RDS)){print(paste(FL.AllItem.RvNA.Wide_Before.RDS, " exist. Delete it!"));file.remove(FL.AllItem.RvNA.Wide_Before.RDS)}
	if  (file.exists(FL.found.htgSys.share.OBJ))      {print(paste(FL.found.htgSys.share.OBJ,       " exist. Delete it!"));file.remove(FL.found.htgSys.share.OBJ)}
	if  (file.exists(FL.AllItem.RvNA.Wide_After.CSV)) {print(paste(FL.AllItem.RvNA.Wide_After.CSV,  " exist. Delete it!"));file.remove(FL.AllItem.RvNA.Wide_After.CSV)}	# Key code item data with minimal cleaning
	if  (file.exists(FL.AllItem.RvNA.Wide_Before.CSV)){print(paste(FL.AllItem.RvNA.Wide_Before.CSV, " exist. Delete it!"));file.remove(FL.AllItem.RvNA.Wide_Before.CSV)}	# Key code item data with minimal cleaning
	if  (file.exists(FL.found.htgSys.share.CSV))      {print(paste(FL.found.htgSys.share.CSV,       " exist. Delete it!"));file.remove(FL.found.htgSys.share.CSV)}		# Key code item data with minimal cleaning
	if  (file.exists(FL.KeyItem.CSV))                 {print(paste(FL.KeyItem.CSV,                  " exist. Delete it!"));file.remove(FL.KeyItem.CSV)}
	if  (file.exists(FL.KeyItem.Wide.RDS))            {print(paste(FL.KeyItem.Wide.RDS,             " exist. Delete it!"));file.remove(FL.KeyItem.Wide.RDS)}		# The wide format of the key code item data
	if  (file.exists(FL.KeyItem.Long.RDS))            {print(paste(FL.KeyItem.Long.RDS,             " exist. Delete it!"));file.remove(FL.KeyItem.Long.RDS)}		# The long format of the key code item data
	if  (file.exists(FL.KeyItem.Wide.CSV))            {print(paste(FL.KeyItem.Wide.CSV,             " exist. Delete it!"));file.remove(FL.KeyItem.Wide.CSV)}		# The wide format of the key code item data
	if  (file.exists(FL.KeyItem.Long.CSV))            {print(paste(FL.KeyItem.Long.CSV,             " exist. Delete it!"));file.remove(FL.KeyItem.Long.CSV)}		# The long format of the key code item data
	if  (file.exists(FL.KeyItem.PDF))                 {print(paste(FL.KeyItem.PDF,                  " exist. Delete it!"));file.remove(FL.KeyItem.PDF)}
	if  (file.exists(FL.KeyItem.SUM))                 {print(paste(FL.KeyItem.SUM,                  " exist. Delete it!"));file.remove(FL.KeyItem.SUM)}
	
	if  (file.exists(FL.NumericItem.PDF))             {print(paste(FL.NumericItem.PDF,              " exist. Delete it!"));file.remove(FL.NumericItem.PDF)}			# July 8, 2015: add one page plot to show the distributions of all numeric code items 
	if  (file.exists(FL.NumericItem.CSV))             {print(paste(FL.NumericItem.CSV,              " exist. Delete it!"));file.remove(FL.NumericItem.CSV)}			# July 8, 2015: add one page plot to show the distributions of all numeric code items 
	if  (file.exists(FL.NumericItem.RDS))             {print(paste(FL.NumericItem.RDS,              " exist. Delete it!"));file.remove(FL.NumericItem.RDS)}			# July 8, 2015: add one page plot to show the distributions of all numeric code items 
	if  (file.exists(FL.NumericItem.JPG))             {print(paste(FL.NumericItem.JPG,              " exist. Delete it!"));file.remove(FL.NumericItem.JPG)}			# July 8, 2015: add one page plot to show the distributions of all numeric code items 

	if  (file.exists(FL.DataSummary.CSV))             {print(paste(FL.DataSummary.CSV,              " exist. Delete it!"));file.remove(FL.DataSummary.CSV)}			# July 9, 2015: add data summary output
	if  (file.exists(FL.DataSummary.RDS))             {print(paste(FL.DataSummary.RDS,              " exist. Delete it!"));file.remove(FL.DataSummary.RDS)}			# July 8, 2015: add data summary output

	
	cat(paste("3. ",string.thisState,": Specify the file name for this state.\n",sep=""))
	cat(paste("3. ",string.thisState,": Specify the file name for this state.\n",sep=""),file=FL.LOG, append=TRUE)								

	# -------------------------------------------------------------------------------------------------
	# open pdf file for plotting
	# -------------------------------------------------------------------------------------------------
	pdf(file = FL.KeyItem.PDF,paper="special", width=17, height=11,bg = "transparent")	# device 2
	cat(paste("4. ",string.thisState,": open [",FL.KeyItem.PDF,"] for plots.\n",sep=""))
	cat(paste("4. ",string.thisState,": open [",FL.KeyItem.PDF,"] for plots.\n",sep=""),file=FL.LOG, append=TRUE)		

	# -------------------------------------------------------------------------------------------------
	# B. uploading state data from the RDS file into [myData.thisState]
	# -------------------------------------------------------------------------------------------------
	myData.thisState <- readRDS(file=FL.IN.RDS)
	cat(paste("5A. ",string.thisState,": data has been read into [myData.thisState].\n",sep=""))
	cat(paste("5A. ",string.thisState,": data has been read into [myData.thisState].\n",sep=""),file=FL.LOG, append=TRUE)								


	myData.thisState <- myData.thisState[!(is.na(myData.thisState[,"PV_codeitem_value"])),]			# keep only non NA entries in the code item value field
	myData.thisState <- myData.thisState[myData.thisState[,"PV_codeitem_value"] != "",]				# remove "" entries
	myData.thisState <- myData.thisState[myData.thisState[,"PV_codeitem_value"] != "NA",]				# remove "" entries
	myData.thisState <- myData.thisState[myData.thisState[,"PV_codeitem_value"] != "NR",]				# remove "" entries
	myData.thisState <- myData.thisState[myData.thisState[,"PV_codeitem_value"] != "unassigned",]			# remove "" entries  KY has value of "unassigned" in IQ3, IQ4, IQ2, MIQ1, CSIQ1


	myData.thisState <- droplevels(myData.thisState)

	myObs.Num <- data.frame(tapply(myData.thisState[,"PV_codeitem_value"],myData.thisState[,"CIA_codeitem_code"],length))
	names(myObs.Num) <- "NoObs"
	myObs.Num[,"CodeItem"] <- row.names(myObs.Num)
	

	# use both code name and its description as field name
	code.tmp  <- myObs.Num[,"CodeItem"]	# the code fields
	idx.matched <- match(code.tmp,myMap[,"ID"])
	code.desc  <- myMap[idx.matched,"Short.Name"]
	myObs.Num[,"Description"] <- code.desc


	saveRDS(myObs.Num,file=FL.DataSummary.RDS)
	cat(paste("Data Overview at ",string.thisState,"(",timeStamp.string,"),",sep=""),file=FL.DataSummary.CSV,append=TRUE)
	write.table(myObs.Num,file=FL.DataSummary.CSV,sep=",",append=TRUE)
	cat(paste("5B. ",string.thisState,": output summary of cod eitems.\n",sep=""))
	cat(paste("5B. ",string.thisState,": output summary of cod eitems.\n",sep=""),file=FL.LOG, append=TRUE)								

	# -------------------------------------------------------------------------------------------------
	# BB. uploading state data from the RDS file into [myData.thisState]
	# -------------------------------------------------------------------------------------------------
	myData.thisState <- readRDS(file=FL.IN.RDS)
	cat(paste("5C. ",string.thisState,": data has been read into [myData.thisState].\n",sep=""))
	cat(paste("5C. ",string.thisState,": data has been read into [myData.thisState].\n",sep=""),file=FL.LOG, append=TRUE)								



	# -------------------------------------------------------------------------------------------------
	# summarize data for this state
	# -------------------------------------------------------------------------------------------------
	cat(paste("Data Overview at ",string.thisState,"\n\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
	cat(paste("",dim(myData.thisState)[1],", total records in the database retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
	
	cat(paste("Data Overview at ",string.thisState,"\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
	cat(paste("",dim(myData.thisState)[1],", total records in the database retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
	mySum.tmp1 <- summary(myData.thisState)
	cat(paste(",",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
	write.table(mySum.tmp1,,file=FL.SUM.ALL.CSV,sep=",",append=TRUE)
	cat(paste("\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
	cat(paste("6. ",string.thisState,": summary of [myData.thisState].\n",sep=""))
	cat(paste("6. ",string.thisState,": summary of [myData.thisState].\n",sep=""),file=FL.LOG, append=TRUE)								
	
	
	
	

	
	# -------------------------------------------------------------------------------------------------
	# -------------------------------------------------------------------------------------------------
	# -------------------------------------------------------------------------------------------------
	#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
	# May 8, 2015: do clean on (1) double quotation sign, (2) back slash sign in the "CIA_codeitem_comment", "PV_codeitem_value" fields
	#              this is a generic cleaning for data of all states
	# -------------------------------------------------------------------------------------------------
	# -------------------------------------------------------------------------------------------------
	# -------------------------------------------------------------------------------------------------
	# clean the "CIA_codeitem_comments" field
	myData.thisState.cleaned <- myData.thisState
	myData.thisState.cleaned[,"CIA_codeitem_comments"] <- gsub("\""," inch",myData.thisState.cleaned[,"CIA_codeitem_comments"])	# double quotation sign replaced with inch
	myData.thisState.cleaned[,"CIA_codeitem_comments"] <- gsub("'", " ",    myData.thisState.cleaned[,"CIA_codeitem_comments"])	# single quotation sign replaced with a space
	myData.thisState.cleaned[,"CIA_codeitem_comments"] <- gsub("@", " at",  myData.thisState.cleaned[,"CIA_codeitem_comments"])	# @                sign replaced with at
	myData.thisState.cleaned[,"CIA_codeitem_comments"] <- gsub("\\\\", " ", myData.thisState.cleaned[,"CIA_codeitem_comments"])	# backslash        sign replaced with a space (note: if there is a single backslash in the database, the retrieved data will have double backslash to escape it.  Therefore we need to replace two backslashes.

	# this is a specific replacement: the single entry  having "Heat Pum[\\p" in the codeItem_value field in AL (code ID: 29537 (can fin dit by using [Select * from PRESCRIPTIVE_VALUES where CHECKLIST_ITEM_ANSWER LIKE '29537';])
	myData.thisState.cleaned[,"PV_codeitem_value"]     <- gsub("Heat Pum\\[\\\\\\p", "Heat Pump",  myData.thisState.cleaned[,"PV_codeitem_value"])	# backslash        sign replaced with a space (note: if there is a single backslash in the database, the retrieved data will have double backslash to escape it.  Therefore we need to replace two backslashes.
	cat(paste("7. ",string.thisState,": done some cleaning on [myData.thisState].\n",sep=""))
	cat(paste("7. ",string.thisState,": done some cleaning on [myData.thisState].\n",sep=""),file=FL.LOG, append=TRUE)		

	# July 8, 2015: For MD: the cleaning above changed 11 comments cells.  Not essential.
	# write.csv(myData.thisState,file="tbd1.csv",col.names=TRUE)
	# write.csv(myData.thisState.cleaned,file="tbd2.csv",col.names=TRUE)

	# -------------------------------------------------------------------------------------------------
	#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
	# convert "1" to "I" for the insulation quality field: eh., there is a "11" in "KW5" ID 8954
	#              this is a generic cleaning for data of all states
	# -------------------------------------------------------------------------------------------------
	# clean the insulation quality entries in the "CIA_codeitem_code" field
	myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "IQ1",  "PV_codeitem_value"] <- gsub("^1$","I",gsub("^2$","II",gsub("^3$","III",myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "IQ1",  "PV_codeitem_value"])))
	myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "IQ2",  "PV_codeitem_value"] <- gsub("^1$","I",gsub("^2$","II",gsub("^3$","III",myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "IQ2",  "PV_codeitem_value"])))
	myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "IQ3",  "PV_codeitem_value"] <- gsub("^1$","I",gsub("^2$","II",gsub("^3$","III",myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "IQ3",  "PV_codeitem_value"])))
	myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "IQ4",  "PV_codeitem_value"] <- gsub("^1$","I",gsub("^2$","II",gsub("^3$","III",myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "IQ4",  "PV_codeitem_value"])))
	myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "CSIQ1","PV_codeitem_value"] <- gsub("^1$","I",gsub("^2$","II",gsub("^3$","III",myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "CSIQ1","PV_codeitem_value"])))
	myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "MIQ1", "PV_codeitem_value"] <- gsub("^1$","I",gsub("^2$","II",gsub("^3$","III",myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "MIQ1", "PV_codeitem_value"])))
	myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "KW5",  "PV_codeitem_value"] <- gsub("^1$","I",gsub("^2$","II",gsub("^3$","III",myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "KW5",  "PV_codeitem_value"])))
	
	# July 9, 2015: in AL "Sa01", the KW5 insulation quality has a value of "11" probably should be "II".
	if (this.state == "AL")
	{
			myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "KW5",  "PV_codeitem_value"] <- gsub("^11$","II",myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "KW5",  "PV_codeitem_value"])
	}	
	cat(paste("8. ",string.thisState,": replace number with Roman character for the insulation quality code items.\n",sep=""))
	cat(paste("8. ",string.thisState,": replace number with Roman character for the insulation quality code items.\n",sep=""),file=FL.LOG, append=TRUE)	

	# July 8, 2015: For MD: the cleaning above did not change more at MD.  
	# write.csv(myData.thisState,file="tbd1.csv",col.names=TRUE)
	# write.csv(myData.thisState.cleaned,file="tbd2.csv",col.names=TRUE)
	# write.csv(myData.thisState.cleaned,file="tbd3.csv",col.names=TRUE)


	# -------------------------------------------------------------------------------------------------
	#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
	# May 19, 2015: since we decide to use BG17 to derive foundation shares and use EQ1 and EQ2 to derived heating system shares for the state
	#               standardize the entries in the database
	# 
	#              this is a clean option for AL.  Other states may have more which need to be standardized.
	# -------------------------------------------------------------------------------------------------		
	# standardize the value of EQ1 in the "CIA_codeitem_code" field: this is only based on the AL data at this moment
	if (this.state == "AL")
	{
		  myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "EQ1",  "PV_codeitem_value"] <-  gsub(".*Elec.*","electricity",myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "EQ1",  "PV_codeitem_value"],ignore.case=TRUE)
		  myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "EQ1",  "PV_codeitem_value"] <-  gsub(".*Gas.*", "gas",        myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "EQ1",  "PV_codeitem_value"],ignore.case=TRUE)
	}
	cat(paste("9. ",string.thisState,": stanadrdize the EQ1 entries.\n",sep=""))
	cat(paste("9. ",string.thisState,": stanadrdize the EQ1 entries.\n",sep=""),file=FL.LOG, append=TRUE)	
	
	# standardize the value of EQ2 in the "CIA_codeitem_code" field: this is only based on the AL data at this moment
	# This change is an AL specific
	if (this.state == "AL")
	{	
		  myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "EQ2",  "PV_codeitem_value"] <-  gsub(".*Electric.*",  "ElectricResistance",myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "EQ2",  "PV_codeitem_value"],ignore.case=TRUE)
		  myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "EQ2",  "PV_codeitem_value"] <-  gsub(".*Heat.*Pump.*","HeatPump",          myData.thisState.cleaned[myData.thisState.cleaned[,"CIA_codeitem_code"] == "EQ2",  "PV_codeitem_value"],ignore.case=TRUE)
	}
	cat(paste("10. ",string.thisState,": stanadrdize the EQ2 entries.\n",sep=""))
	cat(paste("10. ",string.thisState,": stanadrdize the EQ2 entries.\n",sep=""),file=FL.LOG, append=TRUE)	



	# -------------------------------------------------------------------------------------------------
	#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
	# May 13, 2015: there are leading and tailing space in the "county name" field.
	#               this is a generic cleaning for data of all states
	# -------------------------------------------------------------------------------------------------
	# remving the leading and tailing space in the entries of the "SPL_county", "SPL_bldg_name", "PV_codeitem_value" fields
	myData.thisState.cleaned[,"SPL_county"]        <- gsub("\\s*$","",gsub("^\\s*","",myData.thisState.cleaned[,"SPL_county"]))		# removing the leading and tailing space
	myData.thisState.cleaned[,"SPL_bldg_name"]     <- gsub("\\s*$","",gsub("^\\s*","",myData.thisState.cleaned[,"SPL_bldg_name"]))		# removing the leading and tailing space
	myData.thisState.cleaned[,"PV_codeitem_value"] <- gsub("\\s*$","",gsub("^\\s*","",myData.thisState.cleaned[,"PV_codeitem_value"]))	# removing the leading and tailing space (like [AL] - [IQ1] show two "II" and "II"
	cat(paste("11. ",string.thisState,": remove the leading and tailing space in the [County] and [Bldg] field.\n",sep=""))
	cat(paste("11. ",string.thisState,": remove the leading and tailing space in the [County] and [Bldg] field.\n",sep=""),file=FL.LOG, append=TRUE)	

	# July 8, 2015: For MD: the cleaning above did change 118 rows of the "SPL_county", "PV_codeitem_value" fields, but invisible in Beyond Compare..  
	# write.csv(myData.thisState,file="tbd1.csv",col.names=TRUE)
	# write.csv(myData.thisState.cleaned,file="tbd2.csv",col.names=TRUE)
	# write.csv(myData.thisState.cleaned,file="tbd3.csv",col.names=TRUE)
	# write.csv(myData.thisState.cleaned,file="tbd4.csv",col.names=TRUE)



	# ------------------------------------------------------------------------------------------------------------
	# save the cleaned data for this state including the entries where "PV_codeitem_value" is NA
	# 1. saveRDS(myData.thisState.cleaned)	to [FL.AllItem.COMP.RDS] like "State_AL_2015May21_AllItem_Complete.RDS"
	#                                          [FL.AllItem.COMP.CSV] like "State_AL_2015May21_AllItem_Complete.CSV"
	# ------------------------------------------------------------------------------------------------------------
	cat(paste("Cleaned Data of State (",this.state,"),",sep=""),file=FL.AllItem.COMP.CSV,append=TRUE)
	write.table(myData.thisState.cleaned,file=FL.AllItem.COMP.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	saveRDS(myData.thisState.cleaned,file=FL.AllItem.COMP.RDS)
	cat(paste("12. ",string.thisState,": cleaned data have been saved out to [",FL.AllItem.COMP.CSV,"] and [",FL.AllItem.COMP.RDS,"].\n",sep=""))
	cat(paste("12. ",string.thisState,": cleaned data have been saved out to [",FL.AllItem.COMP.CSV,"] and [",FL.AllItem.COMP.RDS,"].\n",sep=""),file=FL.LOG, append=TRUE)		

	# #################################################################################################
	# important data frames
	# Up to this point we have [myData.thisState] and [myData.thisState.cleaned]
	#
	# [myData.thisState]:		data directly retrived from the database without any cleaning
	# [myData.thisState.cleaned]: 	data with some generic cleaning
	# NA, empty entries of the "PV_codeitem_value" field have been kept uptil this point!
	# #################################################################################################
	
	
	
	# -------------------------------------------------------------------------------------------------
	# remove "NA" entries in the "PV_codeitem_value" field.  check if there is any data for this state after cleaning
	# -------------------------------------------------------------------------------------------------
	if (dim(myData.thisState.cleaned)[1] > 0)
	{
		# -------------------------------------------------------------------------------------------------
		#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
		# clean the data (C) keep only non-NA data in the "PV_codeitem_value" field
		#                (D) standardize the CZ
		#                (E) Change the lower case to upper case
		# -------------------------------------------------------------------------------------------------	
		#
		# C. [myData.nonNA.cleaned]    <- [[myData.thisState.cleaned]]	Only kept the entries with non-NA in the "PV_codeitem_value" field
		#    [myData.nonNA.notCleaned] <- [myData.thisState]: 		Only kept the entries with non-NA in the "PV_codeitem_value" field
		#
		# not cleaned data [myData.nonNA.notCleaned] <-- [myData.thisState]
		myData.nonNA.notCleaned <- myData.thisState[!(is.na(myData.thisState[,"PV_codeitem_value"])),]						# keep only non NA entries in the code item value field
		myData.nonNA.notCleaned <- myData.nonNA.notCleaned[myData.nonNA.notCleaned[,"PV_codeitem_value"] != "",]				# remove "" entries
		myData.nonNA.notCleaned <- myData.nonNA.notCleaned[myData.nonNA.notCleaned[,"PV_codeitem_value"] != "NA",]				# remove "" entries
		myData.nonNA.notCleaned <- myData.nonNA.notCleaned[grep("^\\s$",myData.nonNA.notCleaned[,"PV_codeitem_value"],perl=TRUE,invert=TRUE),]	# remove " " entries		
		cat(paste("21. ",string.thisState,": only entries with non-NA values in the [PV_codeitem_value] field are kept in [myData.nonNA.notCleaned] for the un-cleaned data.\n",sep=""))
		cat(paste("21. ",string.thisState,": only entries with non-NA values in the [PV_codeitem_value] field are kept in [myData.nonNA.notCleaned] for the un-cleaned data.\n",sep=""),file=FL.LOG, append=TRUE)		

		# cleaned data [myData.nonNA.cleaned] <- [myData.thisState.cleaned]
		myData.nonNA.cleaned <- myData.thisState.cleaned[!(is.na(myData.thisState.cleaned[,"PV_codeitem_value"])),]				# keep only non NA entries in the code item value field
		myData.nonNA.cleaned <- myData.nonNA.cleaned[myData.nonNA.cleaned[,"PV_codeitem_value"] != "",]						# remove "" entries
		myData.nonNA.cleaned <- myData.nonNA.cleaned[myData.nonNA.cleaned[,"PV_codeitem_value"] != "NA",]					# remove "" entries
		myData.nonNA.cleaned <- myData.nonNA.cleaned[grep("^\\s$",myData.nonNA.cleaned[,"PV_codeitem_value"],perl=TRUE,invert=TRUE),]		# remove " " entries
		cat(paste("22. ",string.thisState,": only entries with non-NA values in the [PV_codeitem_value] field are kept in [myData.nonNA.cleaned] for the cleaned data.\n",sep=""))
		cat(paste("22. ",string.thisState,": only entries with non-NA values in the [PV_codeitem_value] field are kept in [myData.nonNA.cleaned] for the cleaned data.\n",sep=""),file=FL.LOG, append=TRUE)		

		#
		# July 8, 2015: (1) by keeping only the non NA entries, the rows reduced from 30902 to 6999 at MD
		#               (2) There are 49 rows showing difference in "SPL_county", "PV_codeitem_value" fields which I believe are from the previous cleaning not from above manipulation. 
		# write.csv(myData.nonNA.notCleaned,file="tbd5.csv",col.names=TRUE)
		# write.csv(myData.nonNA.cleaned,   file="tbd6.csv",col.names=TRUE)		

		#
		# re-assign shorted names 
		# [myData.clean]    <--- [myData.nonNA.cleaned]
		# [myData.notClean] <--- [myData.nonNA.notCleaned]
		#
		if (this.state == "AllData")
		{
			#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
			myData.clean    <- subset(myData.nonNA.cleaned,   subset = (SPL_state != "HI"))
			myData.notClean <- subset(myData.nonNA.notCleaned,subset = (SPL_state != "HI"))
			cat(paste("23. remove Hawaii data then assign [myData.nonNA.cleaned] to [myData.clean].\n",sep=""))
			cat(paste("23. remove Hawaii data then assign [myData.nonNA.cleaned] to [myData.clean].\n",sep=""),file=FL.LOG, append=TRUE)	
		}else{
			myData.clean    <- myData.nonNA.cleaned	
			myData.notClean <- myData.nonNA.notCleaned
			cat(paste("24. assign [myData.nonNA.cleaned] to [myData.clean].\n",sep=""))
			cat(paste("24. assign [myData.nonNA.cleaned] to [myData.clean].\n",sep=""),file=FL.LOG, append=TRUE)			
		}
		cat(paste("25. ",string.thisState,": Assign [myData.nonNA.cleaned] to [myData.clean].\n",sep=""))
		cat(paste("25. ",string.thisState,": Assign [myData.nonNA.cleaned] to [myData.clean].\n",sep=""),file=FL.LOG, append=TRUE)		

		# summarize data
		cat(paste("",dim(myData.clean)[1],", non-NA records of the [value] field in the [PRESCRIPTIVE_VALUES] TABLE retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
		
		cat(paste("",dim(myData.clean)[1],", non-NA records of the [value] field in the [PRESCRIPTIVE_VALUES] TABLE retrieved on [",timeStamp.string,"] for ",string.thisState,"\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		mySum.tmp2 <- summary(myData.clean)
		cat(paste(",",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		write.table(mySum.tmp2,,file=FL.SUM.ALL.CSV,sep=",",append=TRUE)
		cat(paste("\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		cat(paste("26. ",string.thisState,": summary of [myData.clean].\n",sep=""))
		cat(paste("26. ",string.thisState,": summary of [myData.clean].\n",sep=""),file=FL.LOG, append=TRUE)								


		# #################################################################################################
		# important data frames
		# Up to this point we have [myData.clean] and [myData.notClean]
		#
		# [myData.nonNA.notCleaned] derived from [myData.thisState]:		by keeping non-empty and non-NA entries in the "PV_codeitem_value" field
		# [myData.nonNA.cleaned]    derived from [myData.thisState.cleaned]: 	by keeping non-empty and non-NA entries in the "PV_codeitem_value" field
		# 
		# data frames with short names are assigned to them
		# [myData.clean]    <- [myData.nonNA.cleaned]     <- [myData.thisState.cleaned] 
		# [myData.notClean] <- [myData.nonNA.notCleaned]  <- [myData.thisState]
		# #################################################################################################



		# -------------------------------------------------------------------------------------------------
		#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
		# D. Do more clean on [myData.clean], the cleaned data: change all CZ 2 to 2A, 3 to 3A, 
		# -------------------------------------------------------------------------------------------------
		# standard the name of CZ
		myData.clean[,"SPL_CZ"] <- sub("^2$","2A",sub("^3$","3A",sub("^4$","4A",sub("^5$","5A",myData.clean[,"SPL_CZ"]))))
		cat(paste("27. ",string.thisState,": standardize the climate zone term.\n",sep=""))
		cat(paste("27. ",string.thisState,": standardize the climate zone term.\n",sep=""),file=FL.LOG, append=TRUE)		

		# -------------------------------------------------------------------------------------------------
		# E. standardize the CZ field
		# -------------------------------------------------------------------------------------------------
		# create a "ClimateZone" field
		myData.clean[,"ClimateZone"] <- paste("CZ",toupper(myData.clean[,"SPL_CZ"]),sep="")
		cat(paste("28. ",string.thisState,": Add CZ infront of CZ.\n",sep=""))
		cat(paste("28. ",string.thisState,": Add CZ infront of CZ.\n",sep=""),file=FL.LOG, append=TRUE)	


		#
		# July 8, 2015: (1) by keeping only the non NA entries, the rows reduced from 30902 to 6999 at MD
		#               (2) There are 49 rows showing difference in "SPL_county", "PV_codeitem_value" fields which I believe are from the previous cleaning not from above manipulation. 
		# write.csv(myData.clean,   file="tbd7.csv",col.names=TRUE)
		# write.csv(myData.notClean,file="tbd8.csv",col.names=TRUE)	
		# July 8, 2015: MD has duplicates: Need to check AL for make sure no problems	All duplicated entries are found in the "wall2" field at MD. Ignore it.	
		# 
		# for (bldg in (unique(myData.clean[,"SPL_bldg_name"])))
		# {
		# 	A <- myData.clean[myData.clean[,"SPL_bldg_name"] == bldg,"CIA_codeitem_code"]
		# 	B <- paste((A[duplicated(A)]),collapse="-")
		# 	C <- sum(duplicated(A));
		# 	if(C>0){
		# 		D<-"Yes"
		# 	}else{
		# 		D<-"No"
		# 	};
		# 	cat(paste(bldg,B,C,D,"\n",sep=","))
		# }
		
		#
		# July 8, 2015:
		#
		if (this.state == "MD")
		{
			myData.clean    <- subset(myData.clean,   subset=(CIA_codeitem_code != "Wall2"))
			myData.notClean <- subset(myData.notClean,subset=(CIA_codeitem_code != "Wall2"))
		}
		
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# turn [myData.clean]    into [myData.clean.bldg.wide]:    Make a big table to list all code items in term of building before and after clean for QA purpose
		# turn [myData.notClean] into [myData.notClean.bldg.wide]: Make a big table to list all code items in term of building before and after clean for QA purpose
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		myData.clean.bldg.wide    <- as.data.frame(dcast(myData.clean,   SPL_bldg_name ~ CIA_codeitem_code,value.var="PV_codeitem_value"))
		myData.notClean.bldg.wide <- as.data.frame(dcast(myData.notClean,SPL_bldg_name ~ CIA_codeitem_code,value.var="PV_codeitem_value"))
		cat(paste("29. ",string.thisState,": prepared [myData.clean.bldg.wide] and [myData.notClean.bldg.wide]\n",sep=""))
		cat(paste("29. ",string.thisState,": prepared [myData.clean.bldg.wide] and [myData.notClean.bldg.wide]\n",sep=""),file=FL.LOG, append=TRUE)


		# #################################################################################################
		# important data frames
		# Up to this point we have [myData.clean.bldg.wide] and [myData.notClean.bldg.wide] since we need to provide information on what need to be cleaned in the database
		#
		# [myData.clean.bldg.wide]    <- [myData.clean]    <- [myData.nonNA.cleaned]     <- [myData.thisState.cleaned] 
		# [myData.notClean.bldg.wide] <- [myData.notClean] <- [myData.nonNA.notCleaned]  <- [myData.thisState]
		# #################################################################################################
		#
		# July 8, 2015: Standardize the EQ1 and EQ2 fields for MD
		#
		# 1.  105 gas (EQ1) - furnace (EQ2)
		# 2.  42 NA of EQ1.  Two of them shold assign to "gas" which are: bldg "AC" and "AX"
		# 3.  18 electricity.  Two need confirmation.  @@4: EQ2 assigned "electric resistance strip heat" to "electric resistance"; MoCO.10, EQ2 assigned "geothermal heat pump" to "heat pump"
		#   
		#     BQ17
		# 4.  89 bsmt
		# 5.  2 crawlspace: one has IN1a (FloorR) and one has FO7a (CrawlWallr)
		# 6.  73 slab: 
		#        have 23 out of 25 slabEgdeR (FO1), 
		#        have 24 out of 41 FloorR(Cavity) IN1a.
		# 7.  1 missing
		# 8.  70  FI1 CeilingR: 30, 38 and 49 three levels with 70 quality I and II
		# 9.  45  FI17 ACH50: 1.92 to 11.8
		# 10. 67  FI4  DuctTightness: 1.8 to 19
		# 11. 52  FI6  HighEff-Lump: 0 to 100
		# 12. 112 FR2  Windows_U
		# 13. 113 FR3  Windows SHGC (one more SHGC at bldg "W")
		# 14. 25  FO1  SlabEdgeR with two levels 10 and 12.5: 
		#         23 of the 25 have a slab foundation
		#         2  of the 25 have the bsmt foundation
		#         question: bldgs "C" and "MD.CAR.4" has bsmt foundation but with SlabEgdeR values of 10
		# 15. 35  FO4a BsmtWallR(Cavity) with 0, 11, 13, 15, 19, 21 six levels.  All of the foundation type is bsmt.  Good!
		#     there are 27 corresponding FO4b Continuous Wall insulation to these 35 Cavity Insulation.
		#     there are 34 corresponding IQ4  cavity insulation quality to these 35 Cavity Insulation.
		#     question: bldg "MD.PG.23" does not have a insulation quality IQ4 for FO4a field.
		# 16. 36  FO4b BsmtWallRCont with 0,5,10, 11,20 five levels.  All of the 36 entries have a corresponding ID4 values which is for FO4a.
		#     there are 27 corresponding FO4a Continuous Wall insulation to these 36 continuous Insulation FO4b.
		#     there are 9  corresponding IQ4  cavity insulation quality to these 36 continuous Insulation.  
		#     Question: in other words, there are 9 entries with Continuous Insulation FO4b and IQ4 supposed tfor Cavity FO4a (the blldgs are: @@9, MD.Dorch.1, MD.PG.15,MD.PG.24,MD.PG.8, MD.PG.9,MoCO.25,MoCO.7,MoCO.9)
		# 17. 43 IQ4: questions, there are only 35 FO4a.
		# 18. 41 IN1a (FloorR) with 19, 30, 35, 38, 60 five levels. 
		#     There are 33 0s of IN1b corresponding to these 41 IN1a.
		#     There are 41 IQ2 corresponding to the 41 IN1a.
		#     There are 16 bsmt, 24 slab and 1 crawlspace corresponding to these 41 IN1a (FloorRCavity)
		#     the 16 bsmt with FLoorr can be treated as unheated bsmt.
		#     but the foundation type of the 24 slab conflicts with the IN1a (floorR values)
		# 19. 33 IN1b. All are 0 and all have a corresponding IN1a.
		# 20. 1  FO7a, FO7b, CSIQ1 (CrawlSpaceWall)
		# 21. 4  FR10a (two 13 and two 15)
		#     4  FR10b (all 0)
		#     4  MIQ1  (all I)
		# 22. 42  IN3a (13, 15, 19, 20 and 21 five levels)
		#     there are 35 corresponding IN3b (0, 3, 5 three levels)
		#     there are 42 IQ3 (I, II, II three levels))
		# 23. 48 IN3b (0, 3, 5 three levels)
		#     there are 37 corresponding IN3a
		#     
		#     there are 11 IN3b with values but no IN3a neither IQ3 corresponded.
		#     there are 5  IN3a with values but no IN3b corresponded.
		#     there are 37 IN3a and IN3b co-existed.
		if (this.state == "MD")
		{			
			# standardize the term names
			  myData.clean.bldg.wide[,"EQ1"] <-  gsub(".*Elec.*","electricity",myData.clean.bldg.wide[,"EQ1"],ignore.case=TRUE)
			  myData.clean.bldg.wide[,"EQ1"] <-  gsub(".*Gas.*", "gas",        myData.clean.bldg.wide[,"EQ1"],ignore.case=TRUE)
			  
			  myData.clean.bldg.wide[,"EQ2"] <-  gsub(".*furnace.*","Furnace",myData.clean.bldg.wide[,"EQ2"],ignore.case=TRUE)
			  myData.clean.bldg.wide[,"EQ2"] <-  gsub(".*heat pump.*","HeatPump",myData.clean.bldg.wide[,"EQ2"],ignore.case=TRUE)
			  myData.clean.bldg.wide[,"EQ2"] <-  gsub(".*electric resistance.*","ElectricResistance",myData.clean.bldg.wide[,"EQ2"],ignore.case=TRUE)
			  
			  # MD specific cleaning on EQ1
			  myData.clean.bldg.wide[myData.clean.bldg.wide[,"SPL_bldg_name"] == "AC","EQ1"] <- "gas"
			  myData.clean.bldg.wide[myData.clean.bldg.wide[,"SPL_bldg_name"] == "AX","EQ1"] <- "gas"
			  
			  # MD specific cleaning on EQ2
			  myData.clean.bldg.wide[myData.clean.bldg.wide[,"SPL_bldg_name"] == "@@4",    "EQ2"] <- "ElectricResistance"
			  myData.clean.bldg.wide[myData.clean.bldg.wide[,"SPL_bldg_name"] == "MoCO.10","EQ2"] <- "HeatPump"		
			  
			
			# MD foundation type standardize
			myData.clean.bldg.wide[,"BG17"]       <- gsub("^crawlspace$","Crawlspace",gsub("^basement$","Basement",gsub("^slab-on-grade$","Slab",myData.clean.bldg.wide[,"BG17"],ignore.case=TRUE)))
			  
		}	

		# -----------------------------------------------------------------------------------------------------------------------------------
		# State Specific Cleaning (what we did here on all code item array [myData.clean.bldg.wide]                      <-- [myData.clean] 
		#                          needs to be repeated for key item array [myTable.bldg.wide]      <-- [myData.keyCode] <-- [myData.clean]             
		# -----------------------------------------------------------------------------------------------------------------------------------		
		if (this.state == "AL")
		{
			# -------------------------------------------------------------------------------------------------
			#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
			# May 19, 2015: for "AL" (1) assign values to two NA in EQ1 based on values of EQ2: clean EQ1 and EQ2 in [myData.clean.bldg.wide]
			# -------------------------------------------------------------------------------------------------
			# AL specific cleaning:  these cleanings are based on multiple fields so they occurred in the wide format of the data frames
			# (a) EQ1 == "NA" and EQ2 == "HeatPump": re-assign EQ1 == "electricity"			
			myData.clean.bldg.wide[(is.na(myData.clean.bldg.wide[,"EQ1"]) & (!is.na(myData.clean.bldg.wide[,"EQ2"]) & myData.clean.bldg.wide[,"EQ2"] == "HeatPump")),   "EQ1"] <- "electricity"

			# (b) EQ1 == "NA" and EQ2 == "Furnace": re-assign EQ1 == "gas"
			myData.clean.bldg.wide[(is.na(myData.clean.bldg.wide[,"EQ1"]) & (!is.na(myData.clean.bldg.wide[,"EQ2"]) & myData.clean.bldg.wide[,"EQ2"] == "Furnace")),    "EQ1"] <- "gas"
			
			# (c) EQ1 == "electricity" and EQ2 == "NA": re-assign EQ2 == "ElectricResistance"
			myData.clean.bldg.wide[(is.na(myData.clean.bldg.wide[,"EQ2"]) & (!is.na(myData.clean.bldg.wide[,"EQ1"]) & myData.clean.bldg.wide[,"EQ1"] == "electricity")),"EQ2"] <- "ElectricResistance"			
			cat(paste("30. ",string.thisState,": prepared [myData.clean.bldg.wide] and [myData.notClean.bldg.wide]\n",sep=""))
			cat(paste("30. ",string.thisState,": prepared [myData.clean.bldg.wide] and [myData.notClean.bldg.wide]\n",sep=""),file=FL.LOG, append=TRUE)

			# 
			# may 20, 2015: more AL specific cleaning
			#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
			#		
			# AL specific cleaning:  these cleanings are based on multiple fields so they occurred in the wide format of the data frames
			# 1. delete the 12.6 FrameWallR (Cont) for the entry where there are 12.6 for both cavity and continuous wall R
			index_row <- (!(is.na(myData.clean.bldg.wide[,"IN3a"])) & (myData.clean.bldg.wide[,"IN3a"] == "12.6")) & (!(is.na(myData.clean.bldg.wide[,"IN3a"])) & (myData.clean.bldg.wide[,"IN3a"] == "12.6"))
			myData.clean.bldg.wide[index_row,"IN3b"] <- ""


			# 2. remove all "0" values of IN1a (FloorR IN1a and IN1b) when the foundation type (BQ17) is "Slab"
			index_row <- (!(is.na(myData.clean.bldg.wide[,"BG17"])) & (myData.clean.bldg.wide[,"BG17"] == "Slab"))
			myData.clean.bldg.wide[index_row,"IN1a"] <- ""		
			myData.clean.bldg.wide[index_row,"IN1b"] <- ""		

			# 3. remove all "I" values of MIQ1 (MassWall) and re-assign "I" to "IQ3" (FrameWall)
			index_row <- (!(is.na(myData.clean.bldg.wide[,"MIQ1"])) & (myData.clean.bldg.wide[,"MIQ1"] == "I"))
			myData.clean.bldg.wide[index_row,"MIQ1"] <- ""		
			myData.clean.bldg.wide[index_row,"IQ3"]  <- "I"	
			cat(paste("30A. ",string.thisState,": cleaning [myData.clean.bldg.wide]\n",sep=""))
			cat(paste("30A. ",string.thisState,": cleaning [myData.clean.bldg.wide]\n",sep=""),file=FL.LOG, append=TRUE)
			
			#
			# May 28, 2014: more clean.  Reset the insulation quality values to NA if there is no insulation values
			#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
			#
			if ("FI1" %in% names(myData.clean.bldg.wide))
			{
				idx.2.mv.FI1_IQ1 <- (is.na(myData.clean.bldg.wide[,"FI1"]) | myData.clean.bldg.wide[,"FI1"] == "" | myData.clean.bldg.wide[,"FI1"] == "NA" | myData.clean.bldg.wide[,"FI1"] == "NR") &  (!(is.na(myData.clean.bldg.wide[,"IQ1"])) & myData.clean.bldg.wide[,"IQ1"] != "" & myData.clean.bldg.wide[,"IQ1"] != "NA" & myData.clean.bldg.wide[,"IQ1"] != "NR")				
				myData.clean.bldg.wide[idx.2.mv.FI1_IQ1,"IQ1"] <- NA
				cat(paste("30AA. ",string.thisState,": ",sum(idx.2.mv.FI1_IQ1)," [IQ1] are re-set to NA since there are no corrresponding [FI1] values in [myData.clean.bldg.wide].\n",sep=""))
				cat(paste("30AA. ",string.thisState,": ",sum(idx.2.mv.FI1_IQ1)," [IQ1] are re-set to NA since there are no corrresponding [FI1] values in [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)					
			}

			if ("FO4a" %in% names(myData.clean.bldg.wide))
			{
				idx.2.mv.FO4a_IQ4 <- (is.na(myData.clean.bldg.wide[,"FO4a"]) | myData.clean.bldg.wide[,"FO4a"] == "" | myData.clean.bldg.wide[,"FO4a"] == "NA" | myData.clean.bldg.wide[,"FO4a"] == "NR") &  (!(is.na(myData.clean.bldg.wide[,"IQ4"])) & myData.clean.bldg.wide[,"IQ4"] != "" & myData.clean.bldg.wide[,"IQ4"] != "NA" & myData.clean.bldg.wide[,"IQ4"] != "NR")
				myData.clean.bldg.wide[idx.2.mv.FO4a_IQ4,"IQ4"] <- NA
				cat(paste("30BB. ",string.thisState,": ",sum(idx.2.mv.FO4a_IQ4)," [IQ4] are re-set to NA since there are no corrresponding [FO4a] values in [myData.clean.bldg.wide].\n",sep=""))
				cat(paste("30BB. ",string.thisState,": ",sum(idx.2.mv.FO4a_IQ4)," [IQ4] are re-set to NA since there are no corrresponding [FO4a] values in [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)										
			}

			if ("IN1a" %in% names(myData.clean.bldg.wide))
			{
				idx.2.mv.IN1a_IQ2 <- (is.na(myData.clean.bldg.wide[,"IN1a"]) | myData.clean.bldg.wide[,"IN1a"] == "" | myData.clean.bldg.wide[,"IN1a"] == "NA" | myData.clean.bldg.wide[,"IN1a"] == "NR") &  (!(is.na(myData.clean.bldg.wide[,"IQ2"])) & myData.clean.bldg.wide[,"IQ2"] != "" & myData.clean.bldg.wide[,"IQ2"] != "NA" & myData.clean.bldg.wide[,"IQ2"] != "NR")
				myData.clean.bldg.wide[idx.2.mv.IN1a_IQ2,"IQ2"] <- NA
				cat(paste("30CC. ",string.thisState,": ",sum(idx.2.mv.IN1a_IQ2)," [IQ2] are re-set to NA since there are no corrresponding [IN1a] values in [myData.clean.bldg.wide].\n",sep=""))
				cat(paste("30CC. ",string.thisState,": ",sum(idx.2.mv.IN1a_IQ2)," [IQ2] are re-set to NA since there are no corrresponding [IN1a] values in [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)									
			}

			if ("IN3a" %in% names(myData.clean.bldg.wide))
			{
				idx.2.mv.IN3a <- (is.na(myData.clean.bldg.wide[,"IN3a"]) | myData.clean.bldg.wide[,"IN3a"] == "" | myData.clean.bldg.wide[,"IN3a"] == "NA" | myData.clean.bldg.wide[,"IN3a"] == "NR") &  (!(is.na(myData.clean.bldg.wide[,"IQ3"])) & myData.clean.bldg.wide[,"IQ3"] != "" & myData.clean.bldg.wide[,"IQ3"] != "NA" & myData.clean.bldg.wide[,"IQ3"] != "NR")
				myData.clean.bldg.wide[idx.2.mv.IN3a,"IQ3"] <- NA
				cat(paste("30DD. ",string.thisState,": ",sum(idx.2.mv.IN3a)," [IQ3] are re-set to NA since there are no corrresponding [IN3a] values in [myData.clean.bldg.wide].\n",sep=""))
				cat(paste("30DD. ",string.thisState,": ",sum(idx.2.mv.IN3a)," [IQ3] are re-set to NA since there are no corrresponding [IN3a] values in [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)									
			}

			if ("FR10a" %in% names(myData.clean.bldg.wide))
			{
				idx.2.mv.FR10a_MIQ1 <- (is.na(myData.clean.bldg.wide[,"FR10a"]) | myData.clean.bldg.wide[,"FR10a"] == "" | myData.clean.bldg.wide[,"FR10a"] == "NA" | myData.clean.bldg.wide[,"FR10a"] == "NR") &  (!(is.na(myData.clean.bldg.wide[,"MIQ1"])) & myData.clean.bldg.wide[,"MIQ1"] != "" & myData.clean.bldg.wide[,"MIQ1"] != "NA" & myData.clean.bldg.wide[,"MIQ1"] != "NR")
				myData.clean.bldg.wide[idx.2.mv.FR10a_MIQ1,"MIQ1"] <- NA
				cat(paste("30EE. ",string.thisState,": ",sum(idx.2.mv.FR10a_MIQ1)," [MIQ1] are re-set to NA since there are no corrresponding [FR10a] values in [myData.clean.bldg.wide].\n",sep=""))
				cat(paste("30EE. ",string.thisState,": ",sum(idx.2.mv.FR10a_MIQ1)," [MIQ1] are re-set to NA since there are no corrresponding [FR10a] values in [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)										
			}
			cat(paste("30B. ",string.thisState,": cleaning [myData.clean.bldg.wide]\n",sep=""))
			cat(paste("30B. ",string.thisState,": cleaning [myData.clean.bldg.wide]\n",sep=""),file=FL.LOG, append=TRUE)
			
		}
		cat(paste("30C. ",string.thisState,": further cleaning the AL data on [myData.clean.bldg.wide].\n",sep=""))
		cat(paste("30C. ",string.thisState,": further cleaning the AL data on [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)
		


		# -----------------------------------------------------------------------------------------------------------------------------------
		# JUNE 16, 2015	
		# State Specific Cleaning (what we did here on all code item array [myData.clean.bldg.wide]                      <-- [myData.clean] 
		#                          needs to be repeated for key item array [myTable.bldg.wide]      <-- [myData.keyCode] <-- [myData.clean]             
		# -----------------------------------------------------------------------------------------------------------------------------------		
		if (this.state == "AL")
		{
			# row 3,4
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("SC01","Au04","Au28","MaCo04"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"EQ1"] <- "gas"
			myData.clean.bldg.wide[idx.rows,"EQ2"] <- "Furnace"	# Change "HeatPump" to "Furnace"
			cat(paste("30D. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30D. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)
			
			# row 5
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Ho07"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"EQ1"] <- "gas"		# Change "electricity" to "gas"
			myData.clean.bldg.wide[idx.rows,"EQ2"] <- "Furnace"
			cat(paste("30E. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30E. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)
				
			# row 6: no change was proposed by the AL team for this conflict
			# if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			# idx.rows <- match(c("Au01","Au25","Ma01"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			# myData.clean.bldg.wide[idx.rows,"EQ1"] <- "gas"
			# myData.clean.bldg.wide[idx.rows,"EQ2"] <- "Furnace"
			# cat(paste("30F. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			# cat(paste("30F. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)
				
			# row 7a: 
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Md01","Gv01","MaCo03"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"EQ1"] <- "NA"
			myData.clean.bldg.wide[idx.rows,"EQ2"] <- "NA"
			cat(paste("30G. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30G. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			
			
			# row 7b: 
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Do02","MaCo01","MaCo02"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"EQ1"] <- "electricity"
			myData.clean.bldg.wide[idx.rows,"EQ2"] <- "HeatPump"
			cat(paste("30H. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30H. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			
			
			# row 10: 
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Hu10"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IN1a"] <- "NA"	# Change R=0 to NA
			myData.clean.bldg.wide[idx.rows,"IN1b"] <- "NA" # Change R=0 to NA
			cat(paste("30I. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30I. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			
					

			# row 12a: reverse the change made earlier
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Sa01"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IN1a"] <- "25"	# reverse the deleted R=25 and IQ=II
			myData.clean.bldg.wide[idx.rows,"IQ2"]  <- "II"
			cat(paste("30J. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30J. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			
			
			# row 12b: reverse the change made earlier 
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("SC02"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IN1a"] <- "19"	# reverse the deleted R=19 and IQ=II
			myData.clean.bldg.wide[idx.rows,"IQ2"]  <- "II"
			cat(paste("30K. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30K. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

			# row 14:  
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Ve02","Ve03"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IN1a"] <- "NA"	# Change from R=0 to NA
			myData.clean.bldg.wide[idx.rows,"IN1b"] <- "NA" # Change from R=0 to NA
			cat(paste("30L. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30L. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			


			# row 16:  
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("MaCo06"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IN3a"] <- "15"	# Change from R=14.4 to R=15
			cat(paste("30M. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30M. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			
												
			# row 17:  
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Gv01"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IQ2"] <- "I"	# Change from NA to I
			cat(paste("30N. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30N. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			
				
			# row 18:  
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Gv01"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IN3a"] <- "15"	# Change from 14.4 to 15
			cat(paste("30O. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30O. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			
			
			# row 20:  
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Pe02"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IN3a"] <- "NA"	# Change from 11 to NA
			cat(paste("30P. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30P. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			
			
			# row 21:  
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Ho05"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IN3a"] <- "15"	# Change from 14 to 15
			cat(paste("30Q. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30Q. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

			# row 25:  
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Ve02","Ve03"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IN1a"] <- "NA"	# Change from 0 to NA
			myData.clean.bldg.wide[idx.rows,"IN1b"] <- "NA"	# Change from 0 to NA
			cat(paste("30R. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30R. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

			# row 28:  
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Au30"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IQ3"] <- "II"	# Change from NA to II
			cat(paste("30S. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30S. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			


			
			# row 29:  
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Pe02"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IN3a"] <- "NA"	# Change from 11 to NA
			cat(paste("30T. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30T. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			
				
			# row 30:  
			if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
			idx.rows <- match(c("Ve03"),myData.clean.bldg.wide[,"SPL_bldg_name"])
			myData.clean.bldg.wide[idx.rows,"IQ4"] <- "II"	# Change from NA to II
			cat(paste("30U. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""))
			cat(paste("30U. ",string.thisState,": implementing AL team feadback on cleaning [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			
							
		}
		cat(paste("30V. ",string.thisState,": implementing changes based on AL team feedback [myData.clean.bldg.wide].\n",sep=""))
		cat(paste("30V. ",string.thisState,": implementing changes based on AL team feedback [myData.clean.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)
		
		
		
		# use both code name and its description as field name
		names.code  <- names(myData.clean.bldg.wide)[-1]	# the code fields
		idx.matched <- match(names.code,myMap[,"ID"])
		names.desc  <- myMap[idx.matched,"Short.Name"]
		names(myData.clean.bldg.wide) <- c("SPL_bldg_name",paste(names.code,"(",names.desc,")",sep=""))
		
		names.code  <- names(myData.notClean.bldg.wide)[-1]	# the code fields
		idx.matched <- match(names.code,myMap[,"ID"])
		names.desc  <- myMap[idx.matched,"Short.Name"]
		names(myData.notClean.bldg.wide) <- c("SPL_bldg_name",paste(names.code,"(",names.desc,")",sep=""))		
		cat(paste("31. ",string.thisState,": use both code name and description for field names in [myData.clean.bldg.wide] and [myData.notClean.bldg.wide]\n",sep=""))
		cat(paste("31. ",string.thisState,": use both code name and description for field names in [myData.clean.bldg.wide] and [myData.notClean.bldg.wide]\n",sep=""),file=FL.LOG, append=TRUE)

		# =========================================================================================
		# Output the wide format of all code items before cleaning [myData.clean.bldg.wide]		
		# save [myData.clean.bldg.wide] into "FL.AllItem.RvNA.Wide_After.CSV" and "FL.AllItem.RvNA.Wide_After.RDS" like "State_AL_2015May21_AllItem_NARemoved_Wide_After.RDS" and "State_AL_2015May21_AllItem_NARemoved_Wide_After.CSV"
		# =========================================================================================		
		cat(paste("wide format of all data,",sep=""),file=FL.AllItem.RvNA.Wide_After.CSV,append=TRUE)
		write.table(myData.clean.bldg.wide, file=FL.AllItem.RvNA.Wide_After.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
		saveRDS(myData.clean.bldg.wide,file=FL.AllItem.RvNA.Wide_After.RDS)

		# =========================================================================================
		# Output the wide format of all code items before cleaning [myData.notClean.bldg.wide		
		# save [myData.notClean.bldg.wide] into "FL.AllItem.RvNA.Wide_Before.CSV" and "FL.AllItem.RvNA.Wide_Before.RDS" like "State_AL_2015May21_AllItem_NARemoved_Wide_Before.RDS" and "State_AL_2015May21_AllItem_NARemoved_Wide_Before.CSV"
		# =========================================================================================
		cat(paste("wide format of all data,",sep=""),file=FL.AllItem.RvNA.Wide_Before.CSV,append=TRUE)
		write.table(myData.notClean.bldg.wide, file=FL.AllItem.RvNA.Wide_Before.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
		saveRDS(myData.notClean.bldg.wide,file=FL.AllItem.RvNA.Wide_Before.RDS)
		cat(paste("32. ",string.thisState,": output a wide format of all code items in term of bldg.\n",sep=""))
		cat(paste("32. ",string.thisState,": output a wide format of all code items in term of bldg.\n",sep=""),file=FL.LOG, append=TRUE)
		


		# #################################################################################################
		# important data frames
		# draft cleaned version of the data are outputted
		# [myData.clean.bldg.wide] and [myData.notClean.bldg.wide]: note more cleaning occurred on this wide format data frame due to the specific cleaning based on mutliple fields.
		#
		# [myData.clean.bldg.wide]    <- [myData.clean]    <- [myData.nonNA.cleaned]     <- [myData.thisState.cleaned] 
		# [myData.notClean.bldg.wide] <- [myData.notClean] <- [myData.nonNA.notCleaned]  <- [myData.thisState]
		# #################################################################################################






		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# F. [myData.keyCode]: kept only the key code items in [myData.clean]
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		myData.keyCode <- myData.clean[seq(1:dim(myData.clean)[1])[myData.clean[,"CIA_codeitem_code"] %in% list.keyItem],]
		
		# summarize data
		cat(paste("",dim(myData.keyCode)[1],", non-NA records of the [Key Code Items] in the [value] field in the [PRESCRIPTIVE_VALUES] retrieved on [",timeStamp.string,"] for ",string.thisState,"\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
		
		cat(paste("",dim(myData.keyCode)[1],", non-NA records of the [Key Code Items] in the [value] field in the [PRESCRIPTIVE_VALUES] retrieved on [",timeStamp.string,"] for ",string.thisState,"\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		mySum.tmp3 <- summary(myData.keyCode)
		cat(paste(",",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		write.table(mySum.tmp3,,file=FL.SUM.ALL.CSV,sep=",",append=TRUE)
		cat(paste("\n\n",sep=""),file=FL.SUM.ALL.CSV,append=TRUE)
		cat(paste("33. ",string.thisState,": summary of [myData.keyCode].\n",sep=""))
		cat(paste("33. ",string.thisState,": summary of [myData.keyCode].\n",sep=""),file=FL.LOG, append=TRUE)								

		
		myData.keyCode <- droplevels(myData.keyCode)
		no.keyCode.all <- length(list.keyItem)
		no.keyCode.act <- length(unique(myData.keyCode[,"CIA_codeitem_code"]))
		cat(paste("34. ",string.thisState,": [myData.keyCode] keep only those entries of the key code items in [myData.clean] and there are [",no.keyCode.act,"] of [",no.keyCode.all,"] key code items appeared.\n",sep=""))
		cat(paste("34. ",string.thisState,": [myData.keyCode] keep only those entries of the key code items in [myData.clean] and there are [",no.keyCode.act,"] of [",no.keyCode.all,"] key code items appeared.\n",sep=""),file=FL.LOG, append=TRUE)	


		# #################################################################################################
		# important data frames
		# only keep the key code items
		# 
		# [myData.keyCode]            <- [myData.clean]
		#
		# Why we did get [myData.keyCode] from [myData.clean.bldg.wide] after turing to long format, because all these internal cleanign shold not exist eventually and the cleaning should occur in the database itself!!!
		# 
		# Note: the further cleaning on the wide format [myData.clean.bldg.wide] was not carried over to [myData.keyCode]
		# [myData.clean.bldg.wide]    <- [myData.clean]    <- [myData.nonNA.cleaned]     <- [myData.thisState.cleaned] 
		# [myData.notClean.bldg.wide] <- [myData.notClean] <- [myData.nonNA.notCleaned]  <- [myData.thisState]
		# #################################################################################################






		if (dim(myData.keyCode)[1] > 0)
		{

			#
			# G. convert the key code item to character (May 8, 2015: should not convert to numeric any more since we have included some insulation quality code items which are not numeric)
			#
			if(is.factor(myData.keyCode[,"PV_codeitem_value"])){myData.keyCode[,"PV_codeitem_value"] <- as.character(myData.keyCode[,"PV_codeitem_value"])}
		      # if(is.character(myData.keyCode[,"PV_codeitem_value"])){myData.keyCode[,"PV_codeitem_value"] <- as.numeric(myData.keyCode[,"PV_codeitem_value"])}
			cat(paste("41. ",string.thisState,": convert the PV_codteitem_value field in [myData.keyCode] to character if it is not.\n",sep=""))
			cat(paste("41. ",string.thisState,": convert the PV_codteitem_value field in [myData.keyCode] to character if it is not.\n",sep=""),file=FL.LOG, append=TRUE)	

			# ------------------------------------------------------------------------------------------------------------
			# H. save the cleaned data for this state
			# 2. saveRDS(myData.keyCode) to [FL.AllItem.RvNA.Long.RDS] like "State_AL_2015May21_KeyItem_Long.RDS"
			#                               [FL.AllItem.RvNA.Long.CSV] like "State_AL_2015May21_KeyItem_Long.CSV"
			# ------------------------------------------------------------------------------------------------------------
			cat(paste("Cleaned Data of State (",this.state,"),",sep=""),file=FL.AllItem.RvNA.Long.CSV,append=TRUE)
			write.table(myData.keyCode,file=FL.AllItem.RvNA.Long.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			saveRDS(myData.keyCode,file=FL.AllItem.RvNA.Long.RDS)
			cat(paste("42. ",string.thisState,": cleaned data have been saved out to [",FL.AllItem.RvNA.Long.CSV,"] and [",FL.AllItem.RvNA.Long.RDS,"].\n",sep=""))
			cat(paste("42. ",string.thisState,": cleaned data have been saved out to [",FL.AllItem.RvNA.Long.CSV,"] and [",FL.AllItem.RvNA.Long.RDS,"].\n",sep=""),file=FL.LOG, append=TRUE)		



			#
			# I. make a table of the key code item values in term of the unique building name
			#    where [myTable.bldg.wide] is the one we need for next step
			#
			myTable.state     <- as.data.frame(dcast(myData.keyCode,SPL_state     ~ CIA_codeitem_code,value.var="PV_codeitem_value"))
			myTable.CZ        <- as.data.frame(dcast(myData.keyCode,ClimateZone   ~ CIA_codeitem_code,value.var="PV_codeitem_value"))	
			myTable.bldg.wide <- as.data.frame(dcast(myData.keyCode,SPL_bldg_name ~ CIA_codeitem_code,value.var="PV_codeitem_value"))
			myTable.county    <- as.data.frame(dcast(myData.keyCode,SPL_county    ~ CIA_codeitem_code,value.var="PV_codeitem_value"))
			cat(paste("43. ",string.thisState,": make a series of tables from [myData.keyCode].\n",sep=""))
			cat(paste("43. ",string.thisState,": make a series of tables from [myData.keyCode].\n",sep=""),file=FL.LOG, append=TRUE)



			# #################################################################################################
			# important data frames
			# [myTable.bldg.wide] is a version of [myData.clean.bldg.wide] which only consists of key code items
			# 
			# [myTable.bldg.wide]         <- [myData.keyCode] <- [myData.clean]
			# [myData.clean.bldg.wide]                        <- [myData.clean]    <- [myData.nonNA.cleaned]     <- [myData.thisState.cleaned] 
			# [myData.notClean.bldg.wide]                     <- [myData.notClean] <- [myData.nonNA.notCleaned]  <- [myData.thisState]
			# #################################################################################################

			if (this.state == "MD")
			{			
				# standardize the term names
				myTable.bldg.wide[,"EQ1"] <-  gsub(".*Elec.*","electricity",myTable.bldg.wide[,"EQ1"],ignore.case=TRUE)
				myTable.bldg.wide[,"EQ1"] <-  gsub(".*Gas.*", "gas",        myTable.bldg.wide[,"EQ1"],ignore.case=TRUE)

				myTable.bldg.wide[,"EQ2"] <-  gsub(".*furnace.*","Furnace",myTable.bldg.wide[,"EQ2"],ignore.case=TRUE)
				myTable.bldg.wide[,"EQ2"] <-  gsub(".*heat pump.*","HeatPump",myTable.bldg.wide[,"EQ2"],ignore.case=TRUE)
				myTable.bldg.wide[,"EQ2"] <-  gsub(".*electric resistance.*","ElectricResistance",myTable.bldg.wide[,"EQ2"],ignore.case=TRUE)

				# MD specific cleaning on EQ1
				myTable.bldg.wide[myTable.bldg.wide[,"SPL_bldg_name"] == "AC","EQ1"] <- "gas"
				myTable.bldg.wide[myTable.bldg.wide[,"SPL_bldg_name"] == "AX","EQ1"] <- "gas"

				# MD specific cleaning on EQ2
				myTable.bldg.wide[myTable.bldg.wide[,"SPL_bldg_name"] == "@@4",    "EQ2"] <- "ElectricResistance"
				myTable.bldg.wide[myTable.bldg.wide[,"SPL_bldg_name"] == "MoCO.10","EQ2"] <- "HeatPump"	
				
				# MD foundation type standardize
				myTable.bldg.wide[,"BG17"]       <- gsub("^crawlspace$","Crawlspace",gsub("^basement$","Basement",gsub("^slab-on-grade$","Slab",myTable.bldg.wide[,"BG17"],ignore.case=TRUE)))
				  
			}


			# -------------------------------------------------------------------------------------------------------------------------------
			# State Specific Cleaning ((what we did here on key code item array [myTable.bldg.wide]      <-- [myData.keyCode] <-- [myData.clean] 
			#                   is to repeat what we did for the all item array [myData.clean.bldg.wide]                      <-- [myData.clean]
			# -------------------------------------------------------------------------------------------------------------------------------                                                           for key item array [myTable.bldg.wide]      <-- [myData.keyCode]            
			#
			if (this.state == "AL")
			{
				# -------------------------------------------------------------------------------------------------
				#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
				# May 19, 2015: for "AL" (1) assign values to two NA in EQ1 based on values of EQ2: clean EQ1 and EQ2 in [myTable.bldg.wide]
				# -------------------------------------------------------------------------------------------------
				# AL specific cleaning:  these cleanings are based on multiple fields so they occurred in the wide format of the data frames		
				# (a) EQ1 == "NA" and EQ2 == "HeatPump": re-assign EQ1 == "electricity"			
				myTable.bldg.wide[(is.na(myTable.bldg.wide[,"EQ1"]) & (!is.na(myTable.bldg.wide[,"EQ2"]) & myTable.bldg.wide[,"EQ2"] == "HeatPump")),   "EQ1"] <- "electricity"

				# (b) EQ1 == "NA" and EQ2 == "Furnace": re-assign EQ1 == "gas"
				myTable.bldg.wide[(is.na(myTable.bldg.wide[,"EQ1"]) & (!is.na(myTable.bldg.wide[,"EQ2"]) & myTable.bldg.wide[,"EQ2"] == "Furnace")),    "EQ1"] <- "gas"

				# (c) EQ1 == "electricity" and EQ2 == "NA": re-assign EQ2 == "ElectricResistance"
				myTable.bldg.wide[(is.na(myTable.bldg.wide[,"EQ2"]) & (!is.na(myTable.bldg.wide[,"EQ1"]) & myTable.bldg.wide[,"EQ1"] == "electricity")),"EQ2"] <- "ElectricResistance"			
				cat(paste("30. ",string.thisState,": prepared [myTable.bldg.wide] and [myData.notClean.bldg.wide]\n",sep=""))
				cat(paste("30. ",string.thisState,": prepared [myTable.bldg.wide] and [myData.notClean.bldg.wide]\n",sep=""),file=FL.LOG, append=TRUE)

				# 
				# may 20, 2015: more AL specific cleaning
				#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
				#		
				# AL specific cleaning:  these cleanings are based on multiple fields so they occurred in the wide format of the data frames
				# 1. delete the 12.6 FrameWallR (Cont) for the entry where there are 12.6 for both cavity and continuous wall R
				index_row <- (!(is.na(myTable.bldg.wide[,"IN3a"])) & (myTable.bldg.wide[,"IN3a"] == "12.6")) & (!(is.na(myTable.bldg.wide[,"IN3a"])) & (myTable.bldg.wide[,"IN3a"] == "12.6"))
				myTable.bldg.wide[index_row,"IN3b"] <- ""


				# 2. remove all "0" values of IN1a (FloorR IN1a and IN1b) when the foundation type (BQ17) is "Slab"
				index_row <- (!(is.na(myTable.bldg.wide[,"BG17"])) & (myTable.bldg.wide[,"BG17"] == "Slab"))
				myTable.bldg.wide[index_row,"IN1a"] <- ""		
				myTable.bldg.wide[index_row,"IN1b"] <- ""		

				# 3. remove all "I" values of MIQ1 (MassWall) and re-assign "I" to "IQ3" (FrameWall)
				index_row <- (!(is.na(myTable.bldg.wide[,"MIQ1"])) & (myTable.bldg.wide[,"MIQ1"] == "I"))
				myTable.bldg.wide[index_row,"MIQ1"] <- ""		
				myTable.bldg.wide[index_row,"IQ3"]  <- "I"		
				
				#
				# May 28, 2014: more clean.  Reset the insulation quality values to NA if there is no insulation values
				#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
				#
				if ("FI1" %in% names(myTable.bldg.wide))
				{
					idx.2.mv.FI1_IQ1 <- (is.na(myTable.bldg.wide[,"FI1"]) | myTable.bldg.wide[,"FI1"] == "" | myTable.bldg.wide[,"FI1"] == "NA" | myTable.bldg.wide[,"FI1"] == "NR") &  (!(is.na(myTable.bldg.wide[,"IQ1"])) & myTable.bldg.wide[,"IQ1"] != "" & myTable.bldg.wide[,"IQ1"] != "NA" & myTable.bldg.wide[,"IQ1"] != "NR")				
					myTable.bldg.wide[idx.2.mv.FI1_IQ1,"IQ1"] <- NA
					cat(paste("43AA. ",string.thisState,": ",sum(idx.2.mv.FI1_IQ1)," [IQ1] are re-set to NA since there are no corrresponding [FI1] values in [myTable.bldg.wide].\n",sep=""))
					cat(paste("43AA. ",string.thisState,": ",sum(idx.2.mv.FI1_IQ1)," [IQ1] are re-set to NA since there are no corrresponding [FI1] values in [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)					
				}
				
				if ("FO4a" %in% names(myTable.bldg.wide))
				{
					idx.2.mv.FO4a_IQ4 <- (is.na(myTable.bldg.wide[,"FO4a"]) | myTable.bldg.wide[,"FO4a"] == "" | myTable.bldg.wide[,"FO4a"] == "NA" | myTable.bldg.wide[,"FO4a"] == "NR") &  (!(is.na(myTable.bldg.wide[,"IQ4"])) & myTable.bldg.wide[,"IQ4"] != "" & myTable.bldg.wide[,"IQ4"] != "NA" & myTable.bldg.wide[,"IQ4"] != "NR")
					myTable.bldg.wide[idx.2.mv.FO4a_IQ4,"IQ4"] <- NA
					cat(paste("43BB. ",string.thisState,": ",sum(idx.2.mv.FO4a_IQ4)," [IQ4] are re-set to NA since there are no corrresponding [FO4a] values in [myTable.bldg.wide].\n",sep=""))
					cat(paste("43BB. ",string.thisState,": ",sum(idx.2.mv.FO4a_IQ4)," [IQ4] are re-set to NA since there are no corrresponding [FO4a] values in [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)										
				}
				
				if ("IN1a" %in% names(myTable.bldg.wide))
				{
					idx.2.mv.IN1a_IQ2 <- (is.na(myTable.bldg.wide[,"IN1a"]) | myTable.bldg.wide[,"IN1a"] == "" | myTable.bldg.wide[,"IN1a"] == "NA" | myTable.bldg.wide[,"IN1a"] == "NR") &  (!(is.na(myTable.bldg.wide[,"IQ2"])) & myTable.bldg.wide[,"IQ2"] != "" & myTable.bldg.wide[,"IQ2"] != "NA" & myTable.bldg.wide[,"IQ2"] != "NR")
					myTable.bldg.wide[idx.2.mv.IN1a_IQ2,"IQ2"] <- NA
					cat(paste("43CC. ",string.thisState,": ",sum(idx.2.mv.IN1a_IQ2)," [IQ2] are re-set to NA since there are no corrresponding [IN1a] values in [myTable.bldg.wide].\n",sep=""))
					cat(paste("43CC. ",string.thisState,": ",sum(idx.2.mv.IN1a_IQ2)," [IQ2] are re-set to NA since there are no corrresponding [IN1a] values in [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)									
				}
				
				if ("IN3a" %in% names(myTable.bldg.wide))
				{
					idx.2.mv.IN3a <- (is.na(myTable.bldg.wide[,"IN3a"]) | myTable.bldg.wide[,"IN3a"] == "" | myTable.bldg.wide[,"IN3a"] == "NA" | myTable.bldg.wide[,"IN3a"] == "NR") &  (!(is.na(myTable.bldg.wide[,"IQ3"])) & myTable.bldg.wide[,"IQ3"] != "" & myTable.bldg.wide[,"IQ3"] != "NA" & myTable.bldg.wide[,"IQ3"] != "NR")
					myTable.bldg.wide[idx.2.mv.IN3a,"IQ3"] <- NA
					cat(paste("43DD. ",string.thisState,": ",sum(idx.2.mv.IN3a)," [IQ3] are re-set to NA since there are no corrresponding [IN3a] values in [myTable.bldg.wide].\n",sep=""))
					cat(paste("43DD. ",string.thisState,": ",sum(idx.2.mv.IN3a)," [IQ3] are re-set to NA since there are no corrresponding [IN3a] values in [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)									
				}

				if ("FR10a" %in% names(myTable.bldg.wide))
				{
					idx.2.mv.FR10a_MIQ1 <- (is.na(myTable.bldg.wide[,"FR10a"]) | myTable.bldg.wide[,"FR10a"] == "" | myTable.bldg.wide[,"FR10a"] == "NA" | myTable.bldg.wide[,"FR10a"] == "NR") &  (!(is.na(myTable.bldg.wide[,"MIQ1"])) & myTable.bldg.wide[,"MIQ1"] != "" & myTable.bldg.wide[,"MIQ1"] != "NA" & myTable.bldg.wide[,"MIQ1"] != "NR")
					myTable.bldg.wide[idx.2.mv.FR10a_MIQ1,"MIQ1"] <- NA
					cat(paste("43EE. ",string.thisState,": ",sum(idx.2.mv.FR10a_MIQ1)," [MIQ1] are re-set to NA since there are no corrresponding [FR10a] values in [myTable.bldg.wide].\n",sep=""))
					cat(paste("43EE. ",string.thisState,": ",sum(idx.2.mv.FR10a_MIQ1)," [MIQ1] are re-set to NA since there are no corrresponding [FR10a] values in [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)										
				}														
			}
			cat(paste("43B. ",string.thisState,": further cleaning the AL data on [myTable.bldg.wide].\n",sep=""))
			cat(paste("43B. ",string.thisState,": further cleaning the AL data on [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)



			# -----------------------------------------------------------------------------------------------------------------------------------
			# JUNE 16, 2015	
			# State Specific Cleaning (what we did here on all code item array [myTable.bldg.wide]                      <-- [myData.clean] 
			#                          needs to be repeated for key item array [myTable.bldg.wide]      <-- [myData.keyCode] <-- [myData.clean]             
			# -----------------------------------------------------------------------------------------------------------------------------------		
			if (this.state == "AL")
			{
				# row 3,4
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("SC01","Au04","Au28","MaCo04"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"EQ1"] <- "gas"
				myTable.bldg.wide[idx.rows,"EQ2"] <- "Furnace"	# Change "HeatPump" to "Furnace"
				cat(paste("44D. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44D. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)

				# row 5
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Ho07"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"EQ1"] <- "gas"		# Change "electricity" to "gas"
				myTable.bldg.wide[idx.rows,"EQ2"] <- "Furnace"
				cat(paste("44E. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44E. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)

				# row 6: no change was proposed by the AL team for this conflict
				# if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				# idx.rows <- match(c("Au01","Au25","Ma01"),myTable.bldg.wide[,"SPL_bldg_name"])
				# myTable.bldg.wide[idx.rows,"EQ1"] <- "gas"
				# myTable.bldg.wide[idx.rows,"EQ2"] <- "Furnace"
				# cat(paste("44F. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				# cat(paste("44F. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)

				# row 7a: 
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Md01","Gv01","MaCo03"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"EQ1"] <- "NA"
				myTable.bldg.wide[idx.rows,"EQ2"] <- "NA"
				cat(paste("44G. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44G. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

				# row 7b: 
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Do02","MaCo01","MaCo02"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"EQ1"] <- "electricity"
				myTable.bldg.wide[idx.rows,"EQ2"] <- "HeatPump"
				cat(paste("44H. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44H. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

				# row 10: 
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Hu10"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IN1a"] <- "NA"	# Change R=0 to NA
				myTable.bldg.wide[idx.rows,"IN1b"] <- "NA" # Change R=0 to NA
				cat(paste("44I. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44I. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			


				# row 12a: reverse the change made earlier
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Sa01"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IN1a"] <- "25"	# reverse the deleted R=25 and IQ=II
				myTable.bldg.wide[idx.rows,"IQ2"]  <- "II"
				cat(paste("44J. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44J. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

				# row 12b: reverse the change made earlier 
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("SC02"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IN1a"] <- "19"	# reverse the deleted R=19 and IQ=II
				myTable.bldg.wide[idx.rows,"IQ2"]  <- "II"
				cat(paste("44K. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44K. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

				# row 14:  
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Ve02","Ve03"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IN1a"] <- "NA"	# Change from R=0 to NA
				myTable.bldg.wide[idx.rows,"IN1b"] <- "NA" # Change from R=0 to NA
				cat(paste("44L. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44L. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			


				# row 16:  
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("MaCo06"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IN3a"] <- "15"	# Change from R=14.4 to R=15
				cat(paste("44M. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44M. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

				# row 17:  
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Gv01"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IQ2"] <- "I"	# Change from NA to I
				cat(paste("44N. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44N. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

				# row 18:  
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Gv01"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IN3a"] <- "15"	# Change from 14.4 to 15
				cat(paste("44O. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44O. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

				# row 20:  
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Pe02"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IN3a"] <- "NA"	# Change from 11 to NA
				cat(paste("44P. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44P. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

				# row 21:  
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Ho05"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IN3a"] <- "15"	# Change from 14 to 15
				cat(paste("44Q. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44Q. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

				# row 25:  
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Ve02","Ve03"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IN1a"] <- "NA"	# Change from 0 to NA
				myTable.bldg.wide[idx.rows,"IN1b"] <- "NA"	# Change from 0 to NA
				cat(paste("44R. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44R. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			




				# row 28:  
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Au30"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IQ3"] <- "II"	# Change from "NA" to "II"
				cat(paste("44S. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44S. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			



				# row 29:  
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Pe02"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IN3a"] <- "NA"	# Change from 11 to NA
				cat(paste("44T. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44T. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

				# row 30:  
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Ve03"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"IQ4"] <- "II"	# Change from NA to II
				cat(paste("44U. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("44U. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)			

			}
			cat(paste("44V. ",string.thisState,": implementing changes based on AL team feedback [myTable.bldg.wide].\n",sep=""))
			cat(paste("44V. ",string.thisState,": implementing changes based on AL team feedback [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)


			# #################################################################################################
			# important data frames
			# like [myData.clean.bldg.wide], a AL specific cleaning was done on the wide format data frame [myTable.bldg.wide] due to the involvment of multiple items
			# a further cleaning version of [myTable.bldg.wide] 		(with only key code items)
			# it is a counterpart of        [myData.clean.bldg.wide]	(with      all code items)
			#
			# [myTable.bldg.wide]         <- [myData.keyCode]  <- [myData.clean]
			# [myData.clean.bldg.wide]    <- [myData.clean]    <- [myData.nonNA.cleaned]     <- [myData.thisState.cleaned] 
			# [myData.notClean.bldg.wide] <- [myData.notClean] <- [myData.nonNA.notCleaned]  <- [myData.thisState]
			# #################################################################################################




			# 
			# J. bldg name is unique to all the state, CZ, county, attached the "state", "CZ", "county", "User" to the [myTable.bldg.wide] table
			#
			var.bldg    <- "SPL_bldg_name"								# the only field which are not a key code item
			var.keyCode <- names(myTable.bldg.wide)[!(names(myTable.bldg.wide) %in% var.bldg)]	# all fields which are key code items			
			
			# use more meaningful field name for [myTable.state], [myTable.CZ], and [myTable.county]
			# skip this operation for [myTable.bldg.wide] because we need to manipulate more on it.
			var.keyCode.meaning   <- paste(var.keyCode,name.keyItem[match(var.keyCode,names(name.keyItem))],sep="-")	# MAPPING!!!!!!!!!!!
			names(myTable.state)  <- c("SPL_state",  var.keyCode.meaning)
			names(myTable.CZ)     <- c("ClimateZone",var.keyCode.meaning)
			names(myTable.county) <- c("SPL_county", var.keyCode.meaning)
			cat(paste("44. ",string.thisState,": use both keyCode and keyCode name as the fields in [myTable.state], [myTable.CZ], and [myTable.county].\n",sep=""))
			cat(paste("44. ",string.thisState,": use both keyCode and keyCode name as the fields in [myTable.state], [myTable.CZ], and [myTable.county].\n",sep=""),file=FL.LOG, append=TRUE)
			
			#
			# append more descriptive feilds to [myTable.bldg.wide]
			#
			# (a) initialize
			myTable.bldg.wide[,"State"]  <- "DoNotExist"
			myTable.bldg.wide[,"CZ"]     <- "DoNotExist"
			myTable.bldg.wide[,"County"] <- "DoNotExist"
			myTable.bldg.wide[,"User"]   <- "DoNotExist"

			# (b) assign value
			myTable.bldg.wide[,"State"]  <- myData.keyCode[match(myTable.bldg.wide[,"SPL_bldg_name"],myData.keyCode[,"SPL_bldg_name"]),"SPL_state"]
			myTable.bldg.wide[,"CZ"]     <- myData.keyCode[match(myTable.bldg.wide[,"SPL_bldg_name"],myData.keyCode[,"SPL_bldg_name"]),"SPL_CZ"]
			myTable.bldg.wide[,"County"] <- myData.keyCode[match(myTable.bldg.wide[,"SPL_bldg_name"],myData.keyCode[,"SPL_bldg_name"]),"SPL_county"]
			myTable.bldg.wide[,"User"]   <- myData.keyCode[match(myTable.bldg.wide[,"SPL_bldg_name"],myData.keyCode[,"SPL_bldg_name"]),"UP_mail"]

			# (c) check: there should be no "DoNotExist" at all
			myTmp <- (myTable.bldg.wide == "DoNotExist")[myTable.bldg.wide == "DoNotExist"]
			if (length(myTmp[!(is.na(myTmp))])){cat(paste("\n\n\nWhy there are still \"DoNoExist\" which are not replaced?\n\n\n",sep=""));die}

			# (d) switch the order of the fields in [myTable.bldg.wide]
			var.other <- names(myTable.bldg.wide)[!(names(myTable.bldg.wide) %in% var.keyCode)]	# fields which are not a key code item	[var.keyCode]
			myTable.bldg.wide <- myTable.bldg.wide[,c(var.other,var.keyCode)]
			cat(paste("45. ",string.thisState,": add [state], [CZ], [county], [user] to [myTable.bldg.wide].\n",sep=""))
			cat(paste("45. ",string.thisState,": add [state], [CZ], [county], [user] to [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)


			# --------------------------------------------------------------------------------------------------------
			# L. save [myTable.state], [myTable.CZ], [myTable.bldg.wide], [myTable.county] to [FL.KeyItem.CSV] like "State_AL_2015May21_KeyItem.CSV"
			# --------------------------------------------------------------------------------------------------------
			# (a) save [myTable.bldg.wide] which is the actual values of the key items
			cat(paste("key code item in unique bldg (actual value),",sep=""),file=FL.KeyItem.CSV,append=TRUE)	
			write.table(myTable.bldg.wide,file=FL.KeyItem.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			cat(paste("\n\n",sep=""),file=FL.KeyItem.CSV,append=TRUE)


			# (b) save [myTable.state] which is the count of the key items
			cat(paste("key code item in state (count),",sep=""),file=FL.KeyItem.CSV,append=TRUE)	
			write.table(myTable.state,file=FL.KeyItem.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			cat(paste("\n\n",sep=""),file=FL.KeyItem.CSV,append=TRUE)

			# (c) save [myTable.CZ] which is the count of the key items
			cat(paste("key code item in CZ (count),",sep=""),file=FL.KeyItem.CSV,append=TRUE)	
			write.table(myTable.CZ,file=FL.KeyItem.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			cat(paste("\n\n",sep=""),file=FL.KeyItem.CSV,append=TRUE)

			# (d) save [myTable.county] which is the count of the key items
			cat(paste("key code item in county (count),",sep=""),file=FL.KeyItem.CSV,append=TRUE)	
			write.table(myTable.county,file=FL.KeyItem.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			cat(paste("\n\n",sep=""),file=FL.KeyItem.CSV,append=TRUE)
			cat(paste("46. ",string.thisState,": [myTable.state], [myTable.CZ], [myTable.bldg.wide], [myTable.county] have been saved [",FL.KeyItem.CSV,"].\n",sep=""))
			cat(paste("46. ",string.thisState,": [myTable.state], [myTable.CZ], [myTable.bldg.wide], [myTable.county] have been saved [",FL.KeyItem.CSV,"].\n",sep=""),file=FL.LOG, append=TRUE)	

			# *****************************************************************************************************************
			# *****************************************************************************************************************
			# *****************************************************************************************************************
			# May 11, 2015: Make compound fields for Wall (including insulation quality, wall type, cavity and continuous.)
			# *****************************************************************************************************************
			# *****************************************************************************************************************
			# *****************************************************************************************************************
			# 1. a compound field of wall consists of (FR10a)-(MIQ1)-(FR10b)-(IN3a)-(IQ3)-(IN3b)
			# ia. first we need to have all these fields which do not necessary exist in all states
			var.keyCode.more <- var.keyCode		# append [var.keyCode] with keyCode which does not exist in the state into [var.keyCode.more]
			# (A) MassWall
			if(!("FR10a" %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"FR10a"] <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"FR10a")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields
			if(!("MIQ1"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"MIQ1"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"MIQ1")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
			if(!("FR10b" %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"FR10b"] <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"FR10b")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields
			cat(paste("47A. ",string.thisState,": to make compoud field for [MassWall] we need to insert component field if they do not exist.\n",sep=""))
			cat(paste("47A. ",string.thisState,": to make compoud field for [MassWall] we need to insert component field if they do not exist.\n",sep=""),file=FL.LOG, append=TRUE)	
			

			# (B) FrameWall
			if(!("IN3a"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"IN3a"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"IN3a")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields
			if(!("IQ3"   %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"IQ3"]   <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"IQ3")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields
			if(!("IN3b"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"IN3b"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"IN3b")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields
			cat(paste("47B. ",string.thisState,": to make compoud field for [FrameWall] we need to insert component field if they do not exist.\n",sep=""))
			cat(paste("47B. ",string.thisState,": to make compoud field for [FrameWall] we need to insert component field if they do not exist.\n",sep=""),file=FL.LOG, append=TRUE)	
			

			# (C) CrawlWall			
			if(!("FO7a"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"FO7a"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"FO7a")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
			if(!("FO7b"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"FO7b"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"FO7b")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
			if(!("CSIQ1"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"CSIQ1"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"CSIQ1")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
			cat(paste("47C. ",string.thisState,": to make compoud field for [CrawlWall] we need to insert component field if they do not exist.\n",sep=""))
			cat(paste("47C. ",string.thisState,": to make compoud field for [CrawlWall] we need to insert component field if they do not exist.\n",sep=""),file=FL.LOG, append=TRUE)	


			# (D) BsmtWall			
			if(!("FO4a"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"FO4a"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"FO4a")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
			if(!("FO4b"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"FO4b"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"FO4b")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
			if(!("IQ4"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"IQ4"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"IQ4")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
			cat(paste("47D. ",string.thisState,": to make compoud field for [BsmtWall] we need to insert component field if they do not exist.\n",sep=""))
			cat(paste("47D. ",string.thisState,": to make compoud field for [BsmtWall] we need to insert component field if they do not exist.\n",sep=""),file=FL.LOG, append=TRUE)	
			

			# (E) Floor			
			if(!("IN1a"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"IN1a"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"IN1a")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
			if(!("IN1b"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"IN1b"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"IN1b")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
			if(!("IQ2"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"IQ2"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"IQ2")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
			cat(paste("47E. ",string.thisState,": to make compoud field for [Floor] we need to insert component field if they do not exist.\n",sep=""))
			cat(paste("47E. ",string.thisState,": to make compoud field for [Floor] we need to insert component field if they do not exist.\n",sep=""),file=FL.LOG, append=TRUE)	
			
			
			# (F) Ceiling			
			if(!("FI1"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"FI1"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"FI1")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields					
			if(!("IQ1"  %in% names(myTable.bldg.wide))){
				myTable.bldg.wide[,"IQ1"]  <- "NR"
				var.keyCode.more <- c(var.keyCode.more,"IQ1")
			}	# do not use logic NA instead of using charaacter "NR" because the other fields are character fields			
						
			cat(paste("47F. ",string.thisState,": to make compoud field for [Ceiling] we need to insert component field if they do not exist.\n",sep=""))
			cat(paste("47F. ",string.thisState,": to make compoud field for [Ceiling] we need to insert component field if they do not exist.\n",sep=""),file=FL.LOG, append=TRUE)	
			
			cat(paste("47. ",string.thisState,": have inserted some fields into [myTable.bldg.wide] if they do not exists for a state.\n",sep=""))
			cat(paste("47. ",string.thisState,": have inserted some fields into [myTable.bldg.wide] if they do not exists for a state.\n",sep=""),file=FL.LOG, append=TRUE)	

			# =========================================================================================================================
			# =========================================================================================================================
			# =========================================================================================================================
			# 1b. make a compound field for wall consistsing both type of wall and both cavity and continous insulation values
			# =========================================================================================================================
			# =========================================================================================================================
			# =========================================================================================================================
			# 1. make a compound field for CompWallR consists of "FR10a" - "MIQ1" - "FR10b" - "IN3a" - "IQ3" - "IN3b" for both frame wall and mass wall
			var.other.more <- var.other			# append the list of the variables
			if("FR10a" %in% names(myTable.bldg.wide)){
				myTable.bldg.wide[,"CompWallR"]    <- paste(myTable.bldg.wide[,"FR10a"],myTable.bldg.wide[,"MIQ1"],myTable.bldg.wide[,"FR10b"],myTable.bldg.wide[,"IN3a"],myTable.bldg.wide[,"IQ3"],myTable.bldg.wide[,"IN3b"],sep="_")
				var.other.more <- c(var.other.more,"CompWallR")
				
				cat(paste("48A. ",string.thisState,": added a compound field [CompWallR] into [myTable.bldg.wide].\n",sep=""))
				cat(paste("48A. ",string.thisState,": added a compound field [CompWallR] into [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)					
			}
			
			
			# 2. make a compound field for CompCeilingR consists of "FI1)" - "IQ1" 
			if("FI1" %in% names(myTable.bldg.wide)){
				myTable.bldg.wide[,"CompCeilingR"] <- paste(myTable.bldg.wide[,"FI1"],myTable.bldg.wide[,"IQ1"],sep="_")
				var.other.more <- c(var.other.more,"CompCeilingR")
				
				cat(paste("48B. ",string.thisState,": added a compound field [CompCeilingR] into [myTable.bldg.wide].\n",sep=""))
				cat(paste("48B. ",string.thisState,": added a compound field [CompCeilingR]  into [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)					
			}
			
			
			# 3. make a compound field for CompBsmtWallR consists of R "FO4a" - "IQ4" - "FO4b"
			if("FO4a" %in% names(myTable.bldg.wide)){
				myTable.bldg.wide[,"CompBsmtWallR"] <- paste(myTable.bldg.wide[,"FO4a"],myTable.bldg.wide[,"IQ4"],myTable.bldg.wide[,"FO4b"],sep="_")	
				var.other.more <- c(var.other.more,"CompBsmtWallR")
				
				cat(paste("48C. ",string.thisState,": added a compound field [CompBsmtWallR] into [myTable.bldg.wide].\n",sep=""))
				cat(paste("48C. ",string.thisState,": added a compound field [CompBsmtWallR] into [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)					
			}
			
			
			# 4. make a compound field "CompFloorR" consists "IN1a" - "IQ2"- "IN1b"
			if("IN1a" %in% names(myTable.bldg.wide)){
				myTable.bldg.wide[,"CompFloorR"] <- paste(myTable.bldg.wide[,"IN1a"],myTable.bldg.wide[,"IQ2"],myTable.bldg.wide[,"IN1b"],sep="_")	
				var.other.more <- c(var.other.more,"CompFloorR")
				
				cat(paste("48D. ",string.thisState,": added a compound field [CompFloorR] into [myTable.bldg.wide].\n",sep=""))
				cat(paste("48D. ",string.thisState,": added a compound field [CompFloorR] into [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)					
			}
			
			
			# 5. make a compound field for "CompCrawlWallR" consists of "FO7a" - "CSIQ1" - "FO7b" 
			if("FO7a" %in% names(myTable.bldg.wide)){
				myTable.bldg.wide[,"CompCrawlWallR"] <- paste(myTable.bldg.wide[,"FO7a"],myTable.bldg.wide[,"CSIQ1"],myTable.bldg.wide[,"FO7b"],sep="_")	
				var.other.more <- c(var.other.more,"CompCrawlWallR")

				cat(paste("48E. ",string.thisState,": added a compound field [CompCrawlWallR] into [myTable.bldg.wide].\n",sep=""))
				cat(paste("48E. ",string.thisState,": added a compound field [CompCrawlWallR] into [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)					
			}
			
			
			# 6. foundation type
			myTable.bldg.wide[,"found.type"] <- myTable.bldg.wide[,"BG17"]
			var.other.more <- c(var.other.more,"found.type")
			cat(paste("48F. ",string.thisState,": added a [found.type] field into [myTable.bldg.wide].\n",sep=""))
			cat(paste("48F. ",string.thisState,": added a [found.type] field into [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)	



			# -----------------------------------------------------------------------------------------------------------------------------------
			# JUNE 16, 2015	
			# State Specific Cleaning (what we did here on all code item array [myData.clean.bldg.wide]                      <-- [myData.clean] 
			#                          needs to be repeated for key item array [myTable.bldg.wide]      <-- [myData.keyCode] <-- [myData.clean]             
			# -----------------------------------------------------------------------------------------------------------------------------------		
			if (this.state == "AL")
			{
			
				# RE-ASSIGN "heatedbsmt" AND "unheatedbsmt"
				
				# row 13,24
				if(exists("idx.rows")  && is.data.frame(get("idx.rows"))) {rm(idx.rows)}
				idx.rows <- match(c("Ho02","Ph02"),myTable.bldg.wide[,"SPL_bldg_name"])
				myTable.bldg.wide[idx.rows,"found.type"] <- "UnheatedBasement"	# Change from "HeatedBasement" to "UnheatedBasement"
				myTable.bldg.wide[idx.rows,"BG17"]       <- "UnheatedBasement"	# Change from "HeatedBasement" to "UnheatedBasement"
				cat(paste("48G. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""))
				cat(paste("48G. ",string.thisState,": implementing AL team feadback on cleaning [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)
			}
			cat(paste("48H. ",string.thisState,": implementing changes based on AL team feedback [myTable.bldg.wide].\n",sep=""))
			cat(paste("48H. ",string.thisState,": implementing changes based on AL team feedback [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)


			# 7. heating source
			myTable.bldg.wide[,"heatSource"] <- myTable.bldg.wide[,"EQ1"]
			var.other.more <- c(var.other.more,"heatSource")
			cat(paste("48I. ",string.thisState,": added a [heatSource]  fieldinto [myTable.bldg.wide].\n",sep=""))
			cat(paste("48I. ",string.thisState,": added a few more fields into [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)	

			# 8. heating system type
			myTable.bldg.wide[,"heatSyse"] <- myTable.bldg.wide[,"EQ2"]
			var.other.more <- c(var.other.more,"heatSyse")
			cat(paste("48J. ",string.thisState,": added a few more fields into [myTable.bldg.wide].\n",sep=""))
			cat(paste("48J. ",string.thisState,": added a few more fields into [myTable.bldg.wide].\n",sep=""),file=FL.LOG, append=TRUE)	
			
			
			
			# 9. put more meaningful field name
			var.keyCode.more.rev <- paste(var.keyCode.more,name.keyItem[match(var.keyCode.more,names(name.keyItem))],sep="-")	# MAPPING!!!!!!!!!!!
			cat(paste("49. ",string.thisState,": mapping fields.\n",sep=""))
			cat(paste("49. ",string.thisState,": mapping fields.\n",sep=""),file=FL.LOG, append=TRUE)	
			
			# ---------------------------------------------------------------------------------
			# 10. replace the field name
			#     [myTable.bldg.wide.rev] is the same as [myTable.bldg.wide] but the field names and order are changed
			# ---------------------------------------------------------------------------------
			myTable.bldg.wide.rev  <- myTable.bldg.wide[,c(var.other.more,var.keyCode.more)]
		  names(myTable.bldg.wide.rev) <-                    c(var.other.more,var.keyCode.more.rev)
			cat(paste("50A. ",string.thisState,": re-assigned [myTable.bldg.wide] to [myTable.bldg.wide.rev].\n",sep=""))
			cat(paste("50A. ",string.thisState,": re-assigned [myTable.bldg.wide] to [myTable.bldg.wide.rev].\n",sep=""),file=FL.LOG, append=TRUE)	
		  
		  
			
			# ----------------------------------------------------------------------------
			# 8. save [myTable.bldg.wide.rev] which is the actual values of the key items
			# ----------------------------------------------------------------------------
			# manually control the order of the fields to be outputted
			out.fields <- var.other.more
			if(length(grep("PredominantFoundation",   var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("PredominantFoundation",   var.keyCode.more.rev,value=TRUE))}
			if(length(grep("PredominantHeatingSource",var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("PredominantHeatingSource",var.keyCode.more.rev,value=TRUE))}
			if(length(grep("heatingSystemType",       var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("heatingSystemType",       var.keyCode.more.rev,value=TRUE))}
			if(length(grep("Ceiling|Roof",            var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("Ceiling|Roof",            var.keyCode.more.rev,value=TRUE))}
			if(length(grep("ACH50",                   var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("ACH50",                   var.keyCode.more.rev,value=TRUE))}
			if(length(grep("Duct",                    var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("Duct",                    var.keyCode.more.rev,value=TRUE))}
			if(length(grep("HighEffLamp",             var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("HighEffLamp",             var.keyCode.more.rev,value=TRUE))}
			if(length(grep("WindowU",                 var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("WindowU",                 var.keyCode.more.rev,value=TRUE))}
			if(length(grep("WindowSHGC",              var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("WindowSHGC",              var.keyCode.more.rev,value=TRUE))}
			if(length(grep("SlabEdgeR",               var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("SlabEdgeR",               var.keyCode.more.rev,value=TRUE))}			
			if(length(grep("BsmtWallR\\(Cavity",      var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("BsmtWallR\\(Cavity",      var.keyCode.more.rev,value=TRUE))}			
			if(length(grep("BsmtWallR\\(Cont",        var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("BsmtWallR\\(Cont",        var.keyCode.more.rev,value=TRUE))}			
			if(length(grep("IQ\\(BsmtWall",           var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("IQ\\(BsmtWall",           var.keyCode.more.rev,value=TRUE))}			
			if(length(grep("FloorR\\(Cavity",         var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("FloorR\\(Cavity",         var.keyCode.more.rev,value=TRUE))}
			if(length(grep("FloorR\\(Cont",           var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("FloorR\\(Cont",           var.keyCode.more.rev,value=TRUE))}
			if(length(grep("IQ\\(Floor",              var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("IQ\\(Floor",              var.keyCode.more.rev,value=TRUE))}
			if(length(grep("CrawlWallR\\(Cavity",     var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("CrawlWallR\\(Cavity",     var.keyCode.more.rev,value=TRUE))}
			if(length(grep("CrawlWallR\\(Cont",       var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("CrawlWallR\\(Cont",       var.keyCode.more.rev,value=TRUE))}
			if(length(grep("IQ\\(Crawl",              var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("IQ\\(Crawl",              var.keyCode.more.rev,value=TRUE))}			
			if(length(grep("MassWallR\\(Cavity",      var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("MassWallR\\(Cavity",      var.keyCode.more.rev,value=TRUE))}
			if(length(grep("MassWallR\\(Cont",        var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("MassWallR\\(Cont",        var.keyCode.more.rev,value=TRUE))}
			if(length(grep("IQ\\(MassWall",           var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("IQ\\(MassWall",           var.keyCode.more.rev,value=TRUE))}
			if(length(grep("FrameWallR\\(Cavity",     var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("FrameWallR\\(Cavity",     var.keyCode.more.rev,value=TRUE))}
			if(length(grep("FrameWallR\\(Cont",       var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("FrameWallR\\(Cont",       var.keyCode.more.rev,value=TRUE))}
			if(length(grep("IQ3-IQ\\(WallCavity\\)",  var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("IQ3-IQ\\(WallCavity\\)",  var.keyCode.more.rev,value=TRUE))}
			if(length(grep("KneeWall",                var.keyCode.more.rev))>0){out.fields <- c(out.fields,grep("KneeWall",                var.keyCode.more.rev,value=TRUE))}
			
			myTable.bldg.wide.rev.reOrder <- myTable.bldg.wide.rev[,out.fields]
			cat(paste("50B. ",string.thisState,": re-assigned [myTable.bldg.wide.rev] to [myTable.bldg.wide.rev.reOrder].\n",sep=""))
			cat(paste("50B. ",string.thisState,": re-assigned [myTable.bldg.wide.rev] to [myTable.bldg.wide.rev.reOrder].\n",sep=""),file=FL.LOG, append=TRUE)	

			# =========================================================================================
			# Output the wide format of key code items [myTable.bldg.wide.rev.reOrder]		
			# save [myTable.bldg.wide.rev.reOrder] to "FL.KeyItem.CSV" which is re-saved to "FL.KeyItem.Wide.CSV" below (like "State_AL_2015May21_KeyItem.CSV")
			# =========================================================================================			
			cat(paste("\n\nkey code item in unique bldg (actual value) [myTable.bldg.wide.rev],",sep=""),file=FL.KeyItem.CSV,append=TRUE)	
			write.table(myTable.bldg.wide.rev.reOrder,file=FL.KeyItem.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			cat(paste("\n\n",sep=""),file=FL.KeyItem.CSV,append=TRUE)
			
			# output the counts
			tmp.count <-  apply(myTable.bldg.wide.rev.reOrder[,out.fields][,-match(var.other.more,out.fields)],2,function(x) length(x[!is.na(x) & x!="NA" & x!="NR" & x!=""]))
			cat(c(rep(",",(length(var.other.more)+1)),paste(tmp.count,sep="",collpase=",")),file=FL.KeyItem.CSV,append=TRUE)

			cat(paste("51. ",string.thisState,": output [myTable.bldg.wide.rev].\n",sep=""))
			cat(paste("51. ",string.thisState,": output [myTable.bldg.wide.rev].\n",sep=""),file=FL.LOG, append=TRUE)	



			# ------------------------------------------------------------------------------------------------
			# ------------------------------------------------------------------------------------------------
			# ------------------------------------------------------------------------------------------------
			# ------------------------------------------------------------------------------------------------
			# [myTable.bldg.wide]     diff from [myTable.bldg.wide.rev]         on the field order and names
			# [myTable.bldg.wide.rev] diff from [myTable.bldg.wide.rev.reOrder] on the field order
			# K. saveRDS(myTable.bldg.wide.rev.reOrder) to [FL.KeyItem.Wide.RDS] like "State_AL_2015May21_KeyItem_Wide.RDS" used for parm preparartion!!!!
			#                                              [FL.KeyItem.Wide.CSV] like "State_AL_2015May21_KeyItem_Wide.CSV"
			#
			# FOR PARM PREPARATION
			# ------------------------------------------------------------------------------------------------
			# ------------------------------------------------------------------------------------------------
			# ------------------------------------------------------------------------------------------------
			# ------------------------------------------------------------------------------------------------
			cat(paste("key code item in unique bldg (actual value),",sep=""),file=FL.KeyItem.Wide.CSV,append=TRUE)	
			write.table(myTable.bldg.wide.rev.reOrder,file=FL.KeyItem.Wide.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)			
			saveRDS(myTable.bldg.wide.rev.reOrder,file=FL.KeyItem.Wide.RDS)
			cat(paste("52. ",string.thisState,": [myTable.bldg.wide] has been saved [",FL.KeyItem.Wide.RDS,"].\n",sep=""))
			cat(paste("52. ",string.thisState,": [myTable.bldg.wide] has been saved [",FL.KeyItem.Wide.RDS,"].\n",sep=""),file=FL.LOG, append=TRUE)	




			# #################################################################################################
			# important data frames
			# [myTable.bldg.wide.rev.reOrder], [myTable.bldg.wide.rev], [myTable.bldg.wide] they are the same except the order of the fields!!!
			#
			# [myTable.bldg.wide.rev.reOrder] <-- [myTable.bldg.wide.rev] <-- [myTable.bldg.wide]         <- [myData.keyCode]  <- [myData.clean]
			#                                                                 [myData.clean.bldg.wide]    <- [myData.clean]    <- [myData.nonNA.cleaned]     <- [myData.thisState.cleaned] 
			#                                                                 [myData.notClean.bldg.wide] <- [myData.notClean] <- [myData.nonNA.notCleaned]  <- [myData.thisState]
			# #################################################################################################




			
			# *********************************************************************************************
			# *********************************************************************************************
			# *********************************************************************************************
			# *********************************************************************************************
			# note: we have
			#                var.other      <--> var.keyCode
			#                var.other.more <-> var.keyCode.more  after added "CompWallR"         "CompCeilingR"      "CompBsmtWallR"     "CompFloorR" 	CompCrawlWallR"  "found.type"
			# re-set var.other and var.keyCode
			# *********************************************************************************************
			# *********************************************************************************************
			# *********************************************************************************************
			# *********************************************************************************************
			# *********************************************************************************************
			var.other.new   <- c(var.other,"found.type")
			var.keyCode.new <- c(var.keyCode.more,"CompWallR","CompCeilingR","CompBsmtWallR","CompFloorR")
			cat(paste("53. ",string.thisState,": need to re-define [var.other.new] and [var.keyCode.new] for melting.\n",sep=""))
			cat(paste("53. ",string.thisState,": need to re-define [var.other.new] and [var.keyCode.new] for melting.\n",sep=""),file=FL.LOG, append=TRUE)	

			# ---------------------------------------------------------------------------------------------
			# M. turn [myTable.bldg.wide] to [myTable.bldg.long]: Make long format data frame
			# ---------------------------------------------------------------------------------------------
			myTable.bldg.long <- melt(myTable.bldg.wide,id.vars = var.other.new,measure.vars = var.keyCode.new,variable.name = "KeyCodeItem",value.name = "KeyCodeValue")
			cat(paste("54. ",string.thisState,": turn [myTable.bldg.wide] into [myTable.bldg.long].\n",sep=""))
			cat(paste("54. ",string.thisState,": turn [myTable.bldg.wide] into [myTable.bldg.long].\n",sep=""),file=FL.LOG, append=TRUE)	

			# N. add a Code Item Description field
			# myTable.bldg.long[,"KeyCodeName"] <- myTable.bldg.long[,"KeyCodeItem"]
			myTable.bldg.long[,"KeyCodeName"] <- name.keyItem[as.character(myTable.bldg.long[,"KeyCodeItem"])]	# MAPPING!!!!!!!!!!!
			cat(paste("55. ",string.thisState,": add code description field [KeyCodeName] in [myTable.bldg.long].\n",sep=""))
			cat(paste("55. ",string.thisState,": add code description field [KeyCodeName] in [myTable.bldg.long].\n",sep=""),file=FL.LOG, append=TRUE)	

			# O. combine the Cavity and Continuous: heyCodeName is something like "BsmtWallR(Cont)" and want to ignore the content in the ().
			myTable.bldg.long[,"KeyCode"] <- myTable.bldg.long[,"KeyCodeName"]
			myTable.bldg.long[,"KeyCode"] <- sub("(.*)(\\(.*\\))","\\1",myTable.bldg.long[,"KeyCode"])
			cat(paste("56A. ",string.thisState,": add code name field after combing cavity and continuous insulation [KeyCode] in [myTable.bldg.long].\n",sep=""))
			cat(paste("56A. ",string.thisState,": add code name field after combing cavity and continuous insulation [KeyCode] in [myTable.bldg.long].\n",sep=""),file=FL.LOG, append=TRUE)	
			
			# July 8, 2015: add a field which combining the code label and the code name
			myTable.bldg.long[,"KeyCodeLabel"] <- paste(myTable.bldg.long[,"KeyCodeItem"],myTable.bldg.long[,"KeyCodeName"],sep="-")
			cat(paste("56B. ",string.thisState,": add [KeyCodeLabel] field in [myTable.bldg.long].\n",sep=""))
			cat(paste("56B. ",string.thisState,": add [KeyCodeLabel] field in [myTable.bldg.long].\n",sep=""),file=FL.LOG, append=TRUE)	

			# P. add a Climate Zone field
			myTable.bldg.long[,"ClimateZone"] <- paste("CZ",sub("(\\d+)(.*)","\\1",myTable.bldg.long[,"CZ"]),sep="")
			cat(paste("57. ",string.thisState,": add [ClimateZone] field without moisture regime in [myTable.bldg.long].\n",sep=""))
			cat(paste("57. ",string.thisState,": add [ClimateZone] field without moisture regime in [myTable.bldg.long].\n",sep=""),file=FL.LOG, append=TRUE)	

			# Q. merge [myTable.bldg.long] with [myIECC2009] or [myIECC2015] to transfer the code complinace requirement
			if (this.state == "MD")
			{
				myTable.bldg.long <- merge(myTable.bldg.long,myIECC2015,by =c("KeyCode","ClimateZone"),all.x=TRUE,all.y=FALSE)
			}else{
				myTable.bldg.long <- merge(myTable.bldg.long,myIECC2009,by =c("KeyCode","ClimateZone"),all.x=TRUE,all.y=FALSE)
			}
			cat(paste("58. ",string.thisState,": add code compliance value to [myTable.bldg.long].\n",sep=""))
			cat(paste("58. ",string.thisState,": add code compliance value to [myTable.bldg.long].\n",sep=""),file=FL.LOG, append=TRUE)	

			# ------------------------------------------------------------------------------------------------
			# R. saveRDS(myTable.bldg.long) to [FL.KeyItem.Long.RDS] like "State_AL_2015May21_KeyItem_Long.RDS".  This is the one to be used for parm preparation!!!!!  Note: the cleanig on FI4 (duct tightness) is not included!!!
			#                                  [FL.KeyItem.Long.CSV] like "State_AL_2015May21_KeyItem_Long.CSV"
			# ------------------------------------------------------------------------------------------------
			cat(paste("key code item in unique bldg (actual value),",sep=""),file=FL.KeyItem.Long.CSV,append=TRUE)	
			write.table(myTable.bldg.long,file=FL.KeyItem.Long.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)			
			saveRDS(myTable.bldg.long,file=FL.KeyItem.Long.RDS)
			cat(paste("59. ",string.thisState,": [myTable.bldg.long] has been saved [",FL.KeyItem.Long.RDS,"].\n",sep=""))
			cat(paste("59. ",string.thisState,": [myTable.bldg.long] has been saved [",FL.KeyItem.Long.RDS,"].\n",sep=""),file=FL.LOG, append=TRUE)	



			# #################################################################################################
			# important data frames
			# get the long format [myTable.bldg.long] from [myTable.bldg.wide] 
			#                     [myTable.bldg.long] is used for plotting!!!!!!!!
			# [myTable.bldg.long]                                         <-- [myTable.bldg.wide] 
			#
			# [myTable.bldg.wide.rev.reOrder] <-- [myTable.bldg.wide.rev] <-- [myTable.bldg.wide]         <- [myData.keyCode]  <- [myData.clean]
			#                                                                 [myData.clean.bldg.wide]    <- [myData.clean]    <- [myData.nonNA.cleaned]     <- [myData.thisState.cleaned] 
			#                                                                 [myData.notClean.bldg.wide] <- [myData.notClean] <- [myData.nonNA.notCleaned]  <- [myData.thisState]
			# #################################################################################################




		
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# PLOT AFTER CLEANNING  [myData.clean.bldg.wide]
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		
			# -----------------------------------------------------------------------------------------------
			# S. plotting original 18 code items
			# -----------------------------------------------------------------------------------------------
			list.codeItem <- sort(unique(myTable.bldg.long[,"KeyCodeItem"]))	# do not do all the KeyCode in the KeyCodeName field because some are raw and some ware combined.  Need customized as follow:
			for (this.codeItem in list.codeItem[grep("^Comp",list.codeItem,invert=TRUE)])
			{
				this.codeitem.name <- name.keyItem[this.codeItem]		# MAPPING!!!!!!!!!!!


				# find the corresponding short description and range if it is a numeric code item
				idx.matched <- match(this.codeItem,myMap[,"ID"])
				this.codeItem.desp <- myMap[idx.matched,"Short.Name"]
				this.codeItem.UL   <- myMap[idx.matched,"Upper.Bound"]
				this.codeItem.LL   <- myMap[idx.matched,"Lower.Bound"]
				cat(paste("60. ",string.thisState,": description and range of [",this.codeitem.name,"].\n",sep=""))
				cat(paste("60. ",string.thisState,": description and range of [",this.codeitem.name,"].\n",sep=""),file=FL.LOG, append=TRUE)	



				mySubset <- subset(myTable.bldg.long,subset=(KeyCodeItem == this.codeItem))
				cat(paste("61. ",string.thisState,": subsetting the data for [",this.codeitem.name,"].\n",sep=""))
				cat(paste("61. ",string.thisState,": subsetting the data for [",this.codeitem.name,"].\n",sep=""),file=FL.LOG, append=TRUE)
				
				# remove NA rows
				mySubset <- mySubset[!(is.na(mySubset[,"KeyCodeValue"])),]
				mySubset <- mySubset[mySubset[,"KeyCodeValue"] != "NA" & mySubset[,"KeyCodeValue"] != "NR" & mySubset[,"KeyCodeValue"] != "",]							
				cat(paste("62. ",string.thisState,": [",this.codeItem,"]:  detele the NA rows.\n",sep=""))
				cat(paste("62. ",string.thisState,": [",this.codeItem,"]:  detele the NA rows.\n",sep=""),file=FL.LOG, append=TRUE)

				#
				# June 10, 2015
				# 
				if (this.codeItem == "FI1")
				{
					mySubset[mySubset[,"KeyCodeValue"] == "30,11","KeyCodeValue"] <- "30"
					mySubset[,"KeyCodeValue"] <- as.numeric(as.character(mySubset[,"KeyCodeValue"])) 
				}
				
				mySubset.lvldropped <- droplevels(mySubset)
				



				# out put the record to a summary file
				no.records.nonNA <- length(!(is.na(mySubset.lvldropped[,"KeyCodeValue"])))
				cat(paste("CodeItem in Database",this.state,this.codeItem,this.codeitem.name,no.records.nonNA,"\n",sep=","),file=FL.allData.SUM, append=TRUE)	
				cat(paste("63. ",string.thisState,": [",this.codeitem.name,"]: number of non-NA records has been saved to [",FL.allData.SUM,"].\n",sep=""))
				cat(paste("63. ",string.thisState,": [",this.codeitem.name,"]: number of non-NA records has been saved to [",FL.allData.SUM,"].\n",sep=""),file=FL.LOG, append=TRUE)	

				# -------------------------------------------------------------------------------------------------
				#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
				# May 14, 2015: if the field is like FI4 with multiple entries, break down and calculate the average instead
				# -------------------------------------------------------------------------------------------------
				if (this.codeItem == "FI4")
				{
					# # May 15: assume multiple observation sof the same system, so take means
					# mySubset.lvldropped[,"KeyCodeValue.raw"]  <- mySubset.lvldropped[,"KeyCodeValue"]
					# mySubset.lvldropped[,"KeyCodeValue.form"] <- paste("mean(c(",mySubset.lvldropped[,"KeyCodeValue.raw"],"),na.rm=TRUE)",sep="")
					# 
					# for (index in seq(1:dim(mySubset.lvldropped)[1]))
					# {
					# 	mySubset.lvldropped[index,"KeyCodeValue"] <- eval(parse(text=mySubset.lvldropped[index,"KeyCodeValue.form"]))
					# }
					# string.multiple <- " Multiple entries are averaged!"
					
					#
					# May 19, 2015: based on May 18, 2015 discussion with J and Vrushali: assume multiple observations are taken from multiple systems of the same house, the multiple entries should then be split into multiple individual entries
					#
					mySubset.lvldropped[,"KeyCodeValue.raw"]  <- mySubset.lvldropped[,"KeyCodeValue"]
					idx.new.row                               <- dim(mySubset.lvldropped)[1]					
					index.multiple                            <- grep(",",mySubset.lvldropped[,"KeyCodeValue.raw"])	# the index where FI4 has multiple entries
					no.multiple                               <- length(index.multiple)				# the number of rows with multiple entries
					string.multiple                           <- paste(" [",no.multiple,"] obs with multiple entries are expanded!",sep="")
					for (index in index.multiple)		# the row index of the entries with multiple values
					{
						multiple.array <- unlist(strsplit(mySubset.lvldropped[index,"KeyCodeValue.raw"],","))
						mySubset.lvldropped[index,"KeyCodeValue"] <- multiple.array[1]				# the original row take the first of the multiple entries
						for (tmp.idx in seq(from=2,to=length(multiple.array)))
						{	
							idx.new.row <- idx.new.row + 1							# the rest of the multiple entries is added to the end of the [mySubset.lvldropped]
							mySubset.lvldropped[idx.new.row,]               <- mySubset.lvldropped[index,]	# everything else in the new row has the same value as the row of the the multiple entry
							mySubset.lvldropped[idx.new.row,"KeyCodeValue"] <- multiple.array[tmp.idx]	# replace the "KeyCodeValue" with the rest of the multplie entries
						}
					}
					
					# May 29, 2015: convert the observed leakage rate (cfm/100sf) into leakage ratio
					index.withValue <- (!(is.na(mySubset.lvldropped[,"KeyCodeValue"]))       & 
					                           (mySubset.lvldropped[,"KeyCodeValue"] !="NA") & 
					                           (mySubset.lvldropped[,"KeyCodeValue"] !="")    )
					# mySubset.lvldropped[index.withValue,"KeyCodeValue"] <- (as.numeric(mySubset.lvldropped[index.withValue,"KeyCodeValue"]) * 2400 / 100) / maximum_supply_cfm
					mySubset.lvldropped[index.withValue,"KeyCodeValue"] <- as.numeric(mySubset.lvldropped[index.withValue,"KeyCodeValue"])	# june 9, 2015: showing the measure unit cfm/100 sf instead of the simulation unit the leakage ratio 					
					
				}else{
					string.multiple <- ""
				}
				cat(paste("64. ",string.thisState,": [",this.codeitem.name,"]: Handling multiple entries as in duct tightness in FI4.\n",sep=""))
				cat(paste("64. ",string.thisState,": [",this.codeitem.name,"]: Handling multiple entries as in duct tightness in FI4.\n",sep=""),file=FL.LOG, append=TRUE)	


				no.data   <- dim(mySubset.lvldropped)[1] 
			      # no.Subset <- dim(mySubset.lvldropped)[1]
				no.Subset <- sum(!(is.na(mySubset.lvldropped[,"KeyCodeValue"])))	
				cat(paste("64B. ",string.thisState,": [",this.codeitem.name,"]: number of observations.\n",sep=""))
				cat(paste("64B. ",string.thisState,": [",this.codeitem.name,"]: number of observations.\n",sep=""),file=FL.LOG, append=TRUE)	
				
				
				if (no.Subset > 0)
				{

					if ((length(grep("[^0-9\\.]",mySubset.lvldropped[,"KeyCodeValue"]))) | (length(grep("(\\d*\\.+)(\\d*\\.+)(.*)$",mySubset.lvldropped[,"KeyCodeValue"]))))	
					{
						cat(paste("**** ",string.thisState,": [",this.codeItem,"]: is a character field.  No plotting at this time!\n",sep=""))
						cat(paste("**** ",string.thisState,": [",this.codeItem,"]: is a character field.  No plotting at this time!\n",sep=""),file=FL.LOG, append=TRUE)
					}else{
						# convert the numeric fiel first
						if(is.character(mySubset.lvldropped[,"KeyCodeValue"])){mySubset.lvldropped[,"KeyCodeValue"] <- as.numeric(mySubset.lvldropped[,"KeyCodeValue"])}
						if(is.factor(mySubset.lvldropped[,"KeyCodeValue"])){mySubset.lvldropped[,"KeyCodeValue"] <- as.numeric(as.character(mySubset.lvldropped[,"KeyCodeValue"]))}
					
						if (this.state == "AllData")
						{
							p.plotafter1A <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=State~.,colour=factor(KeyCodeValue),fill=factor(KeyCodeValue),geom="histogram") 
							p.plotafter1A <- p.plotafter1A + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter1A <- p.plotafter1A + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))			
							p.plotafter1A <- p.plotafter1A + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							p.plotafter1A <- p.plotafter1A + geom_vline(xintercept=c(this.codeItem.UL,this.codeItem.LL),colour="blue",linetype="longdash")
							# plot(p.plotafter1A)	

							p.plotafter1B <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=State~.,colour=ClimateZone,fill=ClimateZone,geom="histogram") 
							p.plotafter1B <- p.plotafter1B + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter1B <- p.plotafter1B + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))			
							p.plotafter1B <- p.plotafter1B + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter1B)	

							p.plotafter1C <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=State~.,colour=SPL_bldg_name,fill=SPL_bldg_name,geom="histogram") 
							p.plotafter1C <- p.plotafter1C + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter1C <- p.plotafter1C + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))			
							p.plotafter1C <- p.plotafter1C + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter1C)	

							p.plotafter1D <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=State~.,colour=County,fill=County,geom="histogram") 
							p.plotafter1D <- p.plotafter1D + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter1D <- p.plotafter1D + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))			
							p.plotafter1D <- p.plotafter1D + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter1D)	
							multiplot(p.plotafter1A,p.plotafter1B,p.plotafter1C,p.plotafter1D,cols=2)

							p.plotafter11A <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=ClimateZone~State,colour=factor(KeyCodeValue),fill=factor(KeyCodeValue),geom="histogram") 
							p.plotafter11A <- p.plotafter11A + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter11A <- p.plotafter11A + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))			
							p.plotafter11A <- p.plotafter11A + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter11A)	

							p.plotafter11B <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=ClimateZone~State,colour=ClimateZone,fill=ClimateZone,geom="histogram") 
							p.plotafter11B <- p.plotafter11B + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter11B <- p.plotafter11B + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))			
							p.plotafter11B <- p.plotafter11B + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter11B)	

							p.plotafter11C <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=ClimateZone~State,colour=SPL_bldg_name,fill=SPL_bldg_name,geom="histogram") 
							p.plotafter11C <- p.plotafter11C + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter11C <- p.plotafter11C + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))			
							p.plotafter11C <- p.plotafter11C + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter11C)	

							p.plotafter11D <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=ClimateZone~State,colour=County,fill=County,geom="histogram") 
							p.plotafter11D <- p.plotafter11D + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter11D <- p.plotafter11D + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))			
							p.plotafter11D <- p.plotafter11D + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter11D)


							multiplot(p.plotafter11A,p.plotafter11B,p.plotafter11C,p.plotafter11D,cols=2)
						}else{
							p.plotafter2A <- ggplot(data=mySubset.lvldropped,aes(x=KeyCodeValue,colour=factor(KeyCodeValue),fill=factor(KeyCodeValue))) + geom_histogram()
							p.plotafter2A <- p.plotafter2A + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right")
							p.plotafter2A <- p.plotafter2A + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))
							p.plotafter2A <- p.plotafter2A + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped) 
							# plot(p.plotafter2A)
							
							p.plotafter2B <- ggplot(data=mySubset.lvldropped,aes(x=KeyCodeValue,colour=factor(ClimateZone),fill=factor(ClimateZone))) + geom_histogram()
							p.plotafter2B <- p.plotafter2B + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter2B <- p.plotafter2B + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))
							p.plotafter2B <- p.plotafter2B + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter2B)

							p.plotafter2C <- ggplot(data=mySubset.lvldropped,aes(x=KeyCodeValue,colour=factor(County),fill=factor(County))) + geom_histogram()
							p.plotafter2C <- p.plotafter2C + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter2C <- p.plotafter2C + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))
							p.plotafter2C <- p.plotafter2C + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter2C)

							p.plotafter2D <- ggplot(data=mySubset.lvldropped,aes(x=KeyCodeValue,colour=factor(SPL_bldg_name),fill=factor(SPL_bldg_name))) + geom_histogram()
							p.plotafter2D <- p.plotafter2D + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter2D <- p.plotafter2D + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))
							p.plotafter2D <- p.plotafter2D + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter2D)	
							multiplot(p.plotafter2A,p.plotafter2B,p.plotafter2C,p.plotafter2D,cols=2)



							p.plotafter22A <- ggplot(data=mySubset.lvldropped,aes(x=KeyCodeValue,colour=factor(KeyCodeValue),fill=factor(KeyCodeValue))) + geom_histogram() + facet_wrap(~ ClimateZone,ncol = 1)
							p.plotafter22A <- p.plotafter22A + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter22A <- p.plotafter22A + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))
							p.plotafter22A <- p.plotafter22A + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter22A)

					              # p.plotafter22B <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=ClimateZone,fill=ClimateZone,geom="histogram",facets=ClimateZone~.) 
							p.plotafter22B <- ggplot(data=mySubset.lvldropped,aes(x=KeyCodeValue,colour=factor(ClimateZone),fill=factor(ClimateZone))) + geom_histogram() + facet_wrap(~ ClimateZone,ncol = 1)
							p.plotafter22B <- p.plotafter22B + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter22B <- p.plotafter22B + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))
							p.plotafter22B <- p.plotafter22B + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter22B)

					              # p.plotafter22C <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=SPL_bldg_name,fill=SPL_bldg_name,geom="histogram",facets=ClimateZone~.) 
							p.plotafter22C <- ggplot(data=mySubset.lvldropped,aes(x=KeyCodeValue,colour=factor(SPL_bldg_name),fill=factor(SPL_bldg_name))) + geom_histogram() + facet_wrap(~ ClimateZone,ncol = 1)
							p.plotafter22C <- p.plotafter22C + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right",legend.text=element_text(colour="black",size=10))	# ,legend.direction = "horizontal") 
							p.plotafter22C <- p.plotafter22C + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))
							p.plotafter22C <- p.plotafter22C + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter22C)

					              # p.plotafter22D <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=County,fill=County,geom="histogram",facets=ClimateZone~.) 
							p.plotafter22D <- ggplot(data=mySubset.lvldropped,aes(x=KeyCodeValue,colour=factor(County),fill=factor(County))) + geom_histogram() + facet_wrap(~ ClimateZone,ncol = 1)
							p.plotafter22D <- p.plotafter22D + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plotafter22D <- p.plotafter22D + labs(x=paste(this.codeItem,"(",this.codeitem.name,")",sep=" "),y="count",title=paste("(Individual Code Item) ",string.thisState,": ( ",this.codeItem,")-(",this.codeitem.name,")\n",vertical.line.string,"\nNo. Observations: ",no.Subset,string.multiple,sep=""))
							p.plotafter22D <- p.plotafter22D + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plotafter22D)			

							multiplot(p.plotafter22A,p.plotafter22B,p.plotafter22C,p.plotafter22D,cols=2)			
						}
					}		# end of judging if it is a numeric field
				}
			}
			cat(paste("65. ",string.thisState,": [plotting the original 18 key code items in the database.\n",sep=""))
			cat(paste("65. ",string.thisState,": [plotting the original 18 key code items in the database.\n",sep=""),file=FL.LOG, append=TRUE)	


			cat(paste("\n\n\n",sep=","),file=FL.allData.SUM, append=TRUE)	
			cat(paste("66. ",string.thisState,": add a separator line in the summary output file.\n",sep=""))
			cat(paste("66. ",string.thisState,": add a separator line in the summary output file.\n",sep=""),file=FL.LOG, append=TRUE)	
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# PLOT AFTER CLEANNING  [myData.clean.bldg.wide]
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
			# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




			# -------------------------------------------------------------------
			# T. combined Cavity and Continous Insulation
			# -------------------------------------------------------------------
			count <- 0
			for (this.codeItem in c("BsmtWallR","CrawlWallR","MassWallR","FloorR","FrameWallR"))
			{
				count <- count + 1

				mySubset <- subset(myTable.bldg.long,subset=(KeyCode == this.codeItem))
				
				# remove NA rows
				mySubset <- mySubset[!(is.na(mySubset[,"KeyCodeValue"])) & mySubset[,"KeyCodeValue"] != "NA"  & mySubset[,"KeyCodeValue"] != ""  & mySubset[,"KeyCodeValue"] != "NR",]
				cat(paste("71. ",string.thisState,": [",this.codeItem,"]:  detele the NA rows.\n",sep=""))
				cat(paste("71. ",string.thisState,": [",this.codeItem,"]:  detele the NA rows.\n",sep=""),file=FL.LOG, append=TRUE)
				
				
				
				mySubset.lvldropped <- droplevels(mySubset)

				no.records.nonNA <- length(!(is.na(mySubset.lvldropped[,"KeyCodeValue"])))
				cat(paste("Cobined CodeItem in Database",this.state,this.codeItem,"",no.records.nonNA,"\n",sep=","),file=FL.allData.SUM, append=TRUE)	
				cat(paste("72. ",string.thisState,": [",this.codeItem,"]: number of non-NA records has been saved to [",FL.allData.SUM,"].\n",sep=""))
				cat(paste("72. ",string.thisState,": [",this.codeItem,"]: number of non-NA records has been saved to [",FL.allData.SUM,"].\n",sep=""),file=FL.LOG, append=TRUE)	


				if (dim(mySubset.lvldropped)[1] > 0)
				{

					if ((length(grep("[^0-9\\.]",mySubset.lvldropped[,"KeyCodeValue"]))) | (length(grep("(\\d*\\.+)(\\d*\\.+)(.*)$",mySubset.lvldropped[,"KeyCodeValue"]))))	
					{
						cat(paste("**** ",string.thisState,": [",this.codeItem,"]: is a character field.  No plotting at this time!\n",sep=""))
						cat(paste("**** ",string.thisState,": [",this.codeItem,"]: is a character field.  No plotting at this time!\n",sep=""),file=FL.LOG, append=TRUE)
					}else{
						# convert the numeric field first
						if(is.character(mySubset.lvldropped[,"KeyCodeValue"])){mySubset.lvldropped[,"KeyCodeValue"] <- as.numeric(mySubset.lvldropped[,"KeyCodeValue"])}
						if(is.factor(mySubset.lvldropped[,"KeyCodeValue"])){mySubset.lvldropped[,"KeyCodeValue"] <- as.numeric(as.character(mySubset.lvldropped[,"KeyCodeValue"]))}
					
					
						if (this.state == "AllData")
						{
							p.plot4A <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=State~.,colour=KeyCodeName,fill=KeyCodeName,geom="histogram") 
							p.plot4A <- p.plot4A + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot4A <- p.plot4A + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot4A <- p.plot4A + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot4A)	

							p.plot4B <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=State~.,colour=CZ,fill=CZ,geom="histogram") 
							p.plot4B <- p.plot4B + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot4B <- p.plot4B + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot4B <- p.plot4B + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot4B)	

							p.plot4C <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=State~.,colour=SPL_bldg_name,fill=SPL_bldg_name,geom="histogram") 
							p.plot4C <- p.plot4C + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot4C <- p.plot4C + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot4C <- p.plot4C + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot4C)	

							p.plot4D <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=State~.,colour=County,fill=County,geom="histogram") 
							p.plot4D <- p.plot4D + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot4D <- p.plot4D + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot4D <- p.plot4D + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot4D)	

							multiplot(p.plot4A,p.plot4B,p.plot4C,p.plot4D,cols=2)

							p.plot44A <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=ClimateZone~State,colour=KeyCodeName,fill=KeyCodeName,geom="histogram") 
							p.plot44A <- p.plot44A + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot44A <- p.plot44A + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot44A <- p.plot44A + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot44A)	

							p.plot44B <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=ClimateZone~State,colour=CZ,fill=CZ,geom="histogram") 
							p.plot44B <- p.plot44B + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot44B <- p.plot44B + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot44B <- p.plot44B + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot44B)	

							p.plot44C <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=ClimateZone~State,colour=SPL_bldg_name,fill=SPL_bldg_name,geom="histogram") 
							p.plot44C <- p.plot44C + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot44C <- p.plot44C + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot44C <- p.plot44C + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot44C)	

							p.plot44D <- qplot(data=mySubset.lvldropped,KeyCodeValue,facets=ClimateZone~State,colour=County,fill=County,geom="histogram") 
							p.plot44D <- p.plot44D + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot44D <- p.plot44D + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot44D <- p.plot44D + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot44D)	

							multiplot(p.plot44A,p.plot44B,p.plot44C,p.plot44D,cols=2)

						}else{
							p.plot5A <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=KeyCodeName,fill=KeyCodeName,geom="histogram") 
							p.plot5A <- p.plot5A + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot5A <- p.plot5A + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot5A <- p.plot5A + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot5A)	

							p.plot5B <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=CZ,fill=CZ,geom="histogram") 
							p.plot5B <- p.plot5B + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot5B <- p.plot5B + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot5B <- p.plot5B + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot5B)	

							p.plot5C <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=SPL_bldg_name,fill=SPL_bldg_name,geom="histogram") 
							p.plot5C <- p.plot5C + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot5C <- p.plot5C + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot5C <- p.plot5C + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot5C)	

							p.plot5D <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=County,fill=County,geom="histogram") 
							p.plot5D <- p.plot5D + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot5D <- p.plot5D + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot5D <- p.plot5D + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot5D)				

							multiplot(p.plot5A,p.plot5B,p.plot5C,p.plot5D,cols=2)	

							p.plot55A <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=KeyCodeName,fill=KeyCodeName,geom="histogram",facets=ClimateZone~.) 
							p.plot55A <- p.plot55A + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot55A <- p.plot55A + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot55A <- p.plot55A + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot55A)	

							p.plot55B <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=CZ,fill=CZ,geom="histogram",facets=ClimateZone~.) 
							p.plot55B <- p.plot55B + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot55B <- p.plot55B + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot55B <- p.plot55B + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot55B)	

							p.plot55C <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=SPL_bldg_name,fill=SPL_bldg_name,geom="histogram",facets=ClimateZone~.) 
							p.plot55C <- p.plot55C + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot55C <- p.plot55C + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot55C <- p.plot55C + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot55C)	

							p.plot55D <- qplot(data=mySubset.lvldropped,KeyCodeValue,colour=County,fill=County,geom="histogram",facets=ClimateZone~.) 
							p.plot55D <- p.plot55D + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
							p.plot55D <- p.plot55D + labs(x=paste(this.codeItem,sep=""),y="count",title=paste("(Code Item with Cavity & Con Insulation) ",string.thisState,": (",this.codeItem,"\nVertical Line: IECC2009 Code Requirement",sep=""))
							p.plot55D <- p.plot55D + geom_vline(aes(xintercept=CodeCompliance), data = mySubset.lvldropped)
							# plot(p.plot55D)				

							multiplot(p.plot55A,p.plot55B,p.plot55C,p.plot55D,cols=2)			
						}
					}	# end of judging if it is a character field
				}
			}
			cat(paste("73. ",string.thisState,": [plotting the 9 key code items used for simulation by combining the Cavity and Continuous Insulation.\n",sep=""))
			cat(paste("73. ",string.thisState,": [plotting the 9 key code items used for simulation by combining the Cavity and Continuous Insulation.\n",sep=""),file=FL.LOG, append=TRUE)	










 			# -------------------------------------------------------------------
 			# U. Compound Fields created on Wall, Floor, Ceiling and CrawlWall
 			# -------------------------------------------------------------------
 			count <- 0
 			for (this.codeItem in list.codeItem[grep("^Comp",list.codeItem)])
 			{
 				count <- count + 1
 
 				mySubset <- subset(myTable.bldg.long,subset=(KeyCode == this.codeItem))
 				mySubset[,"flag"] <- gsub("NR","",gsub("NA","",gsub("_","",mySubset[,"KeyCodeValue"])))	# The "KeyCodeValue" field consists of concanated "NA" and "_".  If nothing left after replacement, it is an empty field which can be deleted.
 				
 				# remove NA rows
 				mySubset <- mySubset[mySubset[,"flag"] != "",]					# the empty entries in "KeyCodeValue" field (see above)
 				cat(paste("81. ",string.thisState,": [",this.codeItem,"]:  detele the NA rows.\n",sep=""))
 				cat(paste("81. ",string.thisState,": [",this.codeItem,"]:  detele the NA rows.\n",sep=""),file=FL.LOG, append=TRUE)
 				
 				
 				
 				mySubset.lvldropped <- droplevels(mySubset)
 				cat(paste("82. ",string.thisState,": [",this.codeItem,"]:  drop unused levels.\n",sep=""))
 				cat(paste("82. ",string.thisState,": [",this.codeItem,"]:  drop unused levels.\n",sep=""),file=FL.LOG, append=TRUE)
 				
 				
 				#
 				# expand the compound fields
 				#
 				if      (this.codeItem == "CompWallR")
 				{
 					mySubset.lvldropped[,"FR10a"] <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\1",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"MIQ1"]  <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\2",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"FR10b"] <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\3",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"IN3a"]  <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\4",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"IQ3"]   <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\5",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"IN3b"]  <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\6",mySubset.lvldropped[,"KeyCodeValue"])
 					
 				}else if(this.codeItem == "CompCeilingR")
 				{
 					mySubset.lvldropped[,"FI1"]  <- sub("(.*)_(.*)","\\1",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"IQ1"]  <- sub("(.*)_(.*)","\\2",mySubset.lvldropped[,"KeyCodeValue"])			
 				}else if(this.codeItem == "CompBsmtWallR")
 				{
 					mySubset.lvldropped[,"FO4a"] <- sub("(.*)_(.*)_(.*)","\\1",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"IQ4"]  <- sub("(.*)_(.*)_(.*)","\\2",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"FO4b"] <- sub("(.*)_(.*)_(.*)","\\3",mySubset.lvldropped[,"KeyCodeValue"])			
 				}else if(this.codeItem == "CompFloorR")
 				{
 					mySubset.lvldropped[,"IN1a"] <- sub("(.*)_(.*)_(.*)","\\1",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"IQ2"]  <- sub("(.*)_(.*)_(.*)","\\2",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"IN1b"] <- sub("(.*)_(.*)_(.*)","\\3",mySubset.lvldropped[,"KeyCodeValue"])					
 				}else if(this.codeItem == "CompCrawlWallR")
 				{
 					mySubset.lvldropped[,"FO7a"]  <- sub("(.*)_(.*)_(.*)","\\1",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"CSIQ1"] <- sub("(.*)_(.*)_(.*)","\\2",mySubset.lvldropped[,"KeyCodeValue"])
 					mySubset.lvldropped[,"FO7b"]  <- sub("(.*)_(.*)_(.*)","\\3",mySubset.lvldropped[,"KeyCodeValue"])					
 				}				
 				var.4.comp <- c("FR10a","MIQ1","FR10b","IN3a","IQ3","IN3b","FI1","IQ1","FO4a","IQ4","FO4b","IN1a","IQ2","IN1b","FO7a","CSIQ1","FO7b" )
 				var.part2  <- intersect(names(mySubset.lvldropped),var.4.comp)	# varying fields in [mySubset.lvldropped]
                           	var.part1  <- setdiff(names(mySubset.lvldropped),var.part2 )	# intact  fields in [mySubset.lvldropped]
 	
 				# fields to be plotted
 				var.4.plot  <- c("FR10a",            "FR10b",          "IN3a",              "IN3b",            "FI1",     "FO4a",             "FO4b",           "IN1a",          "IN1b",        "FO7a",              "FO7b" )
                           names(var.4.plot) <- c("MassWallR(Cavity)","MassWallR(Cont)","FrameWallR(Cavity)","FrameWallR(Cont)","CeilingR","BsmtWallR(Cavity)","BsmtWallR(Cont)","FloorR(Cavity)","FloorR(Cont)","CrawlWallR(Cavity)","CrawlWallR(Cont)")				
				cat(paste("66. ",string.thisState,": [",this.codeItem,"]:  expanded the compound fields.\n",sep=""))
 				cat(paste("66. ",string.thisState,": [",this.codeItem,"]:  expanded the compound fields.\n",sep=""),file=FL.LOG, append=TRUE)
                            
                           
                           	#
                           	# replace missing values in the expanded fields
                           	#
                           	for (this.var in var.4.comp)
                           	{
                           		if (!is.na(match(this.var, var.part2)))	# this.var is indeed a existing field in the expnaded data frame
                           		{
                           			if (this.var == "FR10a" | this.var == "FR10b" | this.var == "IN3a" | this.var == "IN3b" | this.var == "FI1" | this.var == "FO4a" | this.var == "FO4b" | this.var == "IN1a" | this.var == "IN1b" | this.var == "FO7a" | this.var == "FO7b")
                           			{
							if(is.character(mySubset.lvldropped[,this.var]))
							{
								mySubset.lvldropped[,this.var] <- gsub("NR","",gsub("NA","",mySubset.lvldropped[,this.var]))
								mySubset.lvldropped[,this.var] <-   as.numeric(mySubset.lvldropped[,this.var])
							}else if(is.factor(mySubset.lvldropped[,this.var]))
							{
								mySubset.lvldropped[,this.var] <- as.character(mySubset.lvldropped[,this.var])
								mySubset.lvldropped[,this.var] <- gsub("NR","",gsub("NA","",mySubset.lvldropped[,this.var]))
								mySubset.lvldropped[,this.var] <-   as.numeric(mySubset.lvldropped[,this.var])
							}
                           			}else{
							if(is.character(mySubset.lvldropped[,this.var]))
							{
								mySubset.lvldropped[,this.var] <-   as.factor(mySubset.lvldropped[,this.var])
							}                           			
                           			}
                           		}
                           	}
				cat(paste("83. ",string.thisState,": [",this.codeItem,"]:  turn the fields into numeric and character as it should be.\n",sep=""))
 				cat(paste("83. ",string.thisState,": [",this.codeItem,"]:  turn the fields into numeric and character as it should be.\n",sep=""),file=FL.LOG, append=TRUE)
                           	
                           	
    
 				#
 				# plotting
 				#
 				# A. Plot Component of CompWallR 
 				if (this.codeItem == "CompWallR")
 				{	
 					# A1. FR10a and MIQ1
 					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"FR10a"]))
 					if (no.obs.thisCode > 0)
 					{
 						var.x    <- "FR10a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "CZ"
 						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 

						
 						var.x    <- "FR10a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="") 						
 						var.col  <- "County"
 						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						
 						var.x    <- "FR10a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="") 						
 						var.col  <- "SPL_bldg_name"
 						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "FR10a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="") 						
 						var.col  <- "MIQ1"
 						var.fill <- "MIQ1"
						p.plot004 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot004 <- p.plot004 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot004 <- p.plot004 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
												
						multiplot(p.plot001,p.plot002,p.plot003,p.plot004,cols=2)						
					}
					cat(paste("84. ",string.thisState,": [",this.codeItem,"]:  plot component A1.\n",sep=""))
					cat(paste("84. ",string.thisState,": [",this.codeItem,"]:  plot component A1.\n",sep=""),file=FL.LOG, append=TRUE)
					

					# A2: IN3a and IQ3
 					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"IN3a"]))
 					if (no.obs.thisCode > 0)
 					{
 						var.x    <- "IN3a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="") 						
 						var.col  <- "CZ"
 						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 

						
 						var.x    <- "IN3a"
 					 	var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "County"
 						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						
 						var.x    <- "IN3a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "SPL_bldg_name"
 						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "IN3a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "IQ3"
 						var.fill <- "IQ3"
						p.plot004 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot004 <- p.plot004 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot004 <- p.plot004 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
												
						multiplot(p.plot001,p.plot002,p.plot003,p.plot004,cols=2)
					}
					cat(paste("85. ",string.thisState,": [",this.codeItem,"]:  plot component A2.\n",sep=""))
					cat(paste("85. ",string.thisState,": [",this.codeItem,"]:  plot component A2.\n",sep=""),file=FL.LOG, append=TRUE)					
					

					# A3: FR10b
					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"FR10b"]))
 					if (no.obs.thisCode > 0)
 					{
 						var.x    <- "FR10b"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "CZ"
 						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "FR10b"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "County"
 						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						
 						var.x    <- "FR10b"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "SPL_bldg_name"
 						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 

						
						multiplot(p.plot001,p.plot002,p.plot003,cols=2)
					}
					cat(paste("86. ",string.thisState,": [",this.codeItem,"]:  plot component A3.\n",sep=""))
					cat(paste("86. ",string.thisState,": [",this.codeItem,"]:  plot component A3.\n",sep=""),file=FL.LOG, append=TRUE)
					
					# A4: IN3b
 					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"IN3b"]))
 					if (no.obs.thisCode > 0)
 					{
 						var.x    <- "IN3b"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "CZ"
 						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "IN3b"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "County"
 						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						
 						var.x    <- "IN3b"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "SPL_bldg_name"
 						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						multiplot(p.plot001,p.plot002,p.plot003,cols=2)
					}
					cat(paste("87. ",string.thisState,": [",this.codeItem,"]:  plot component A4.\n",sep=""))
					cat(paste("87. ",string.thisState,": [",this.codeItem,"]:  plot component A4.\n",sep=""),file=FL.LOG, append=TRUE)
					
				# B. Plot Component of CompCeilingR 
 				}else if (this.codeItem == "CompCeilingR")
 				{
 					# B1. FI1 and IQ1
					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"FI1"]))
 					if (no.obs.thisCode > 0)
 					{
 						var.x    <- "FI1"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "CZ"
 						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "FI1"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "County"
 						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						
 						var.x    <- "FI1"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "SPL_bldg_name"
 						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "FI1"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "IQ1"
 						var.fill <- "IQ1"
						p.plot004 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot004 <- p.plot004 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot004 <- p.plot004 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
												
						multiplot(p.plot001,p.plot002,p.plot003,p.plot004,cols=2)
					} 	
					cat(paste("88. ",string.thisState,": [",this.codeItem,"]:  plot component B1.\n",sep=""))
					cat(paste("88. ",string.thisState,": [",this.codeItem,"]:  plot component B1.\n",sep=""),file=FL.LOG, append=TRUE)
					
				# C. Plot Component of CompFloorR 
 				}else if (this.codeItem == "CompFloorR")
 				{
					# C1. IN1a and IQ2
					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"IN1a"]))
 					if (no.obs.thisCode > 0)
 					{
 						var.x    <- "IN1a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "CZ"
 						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "IN1a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "County"
 						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						
 						var.x    <- "IN1a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "SPL_bldg_name"
 						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "IN1a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "IQ2"
 						var.fill <- "IQ2"
						p.plot004 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot004 <- p.plot004 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot004 <- p.plot004 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
												
						multiplot(p.plot001,p.plot002,p.plot003,p.plot004,cols=2) 
					}
					cat(paste("89. ",string.thisState,": [",this.codeItem,"]:  plot component C1.\n",sep=""))
					cat(paste("89. ",string.thisState,": [",this.codeItem,"]:  plot component C1.\n",sep=""),file=FL.LOG, append=TRUE)
					
					# C2. IN1b
					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"IN1b"]))
					if (no.obs.thisCode > 0)
					{
						var.x    <- "IN1b"
						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
						var.col  <- "CZ"
						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						var.x    <- "IN1b"
						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
						var.col  <- "County"
						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 



						var.x    <- "IN1b"
						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
						var.col  <- "SPL_bldg_name"
						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 

						multiplot(p.plot001,p.plot002,p.plot003,cols=2)
					}
					cat(paste("90. ",string.thisState,": [",this.codeItem,"]:  plot component C2.\n",sep=""))
					cat(paste("90. ",string.thisState,": [",this.codeItem,"]:  plot component C2.\n",sep=""),file=FL.LOG, append=TRUE)					
				# D. Plot Component of CompBsmtWallR 
 				}else if (this.codeItem == "CompBsmtWallR")
 				{
					# D1. FO4a and IQ4
					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"FO4a"]))
 					if (no.obs.thisCode > 0)
 					{
 						var.x    <- "FO4a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "CZ"
 						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "FO4a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "County"
 						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						
 						var.x    <- "FO4a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "SPL_bldg_name"
 						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "FO4a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "IQ4"
 						var.fill <- "IQ4"
						p.plot004 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot004 <- p.plot004 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot004 <- p.plot004 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
												
						multiplot(p.plot001,p.plot002,p.plot003,p.plot004,cols=2) 
					}
					cat(paste("91. ",string.thisState,": [",this.codeItem,"]:  plot component D1.\n",sep=""))
					cat(paste("91. ",string.thisState,": [",this.codeItem,"]:  plot component D1.\n",sep=""),file=FL.LOG, append=TRUE)
					
					# D2. FO4b
					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"FO4b"]))
					if (no.obs.thisCode > 0)
					{
						var.x    <- "FO4b"
						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
						var.col  <- "CZ"
						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						var.x    <- "FO4b"
						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
						var.col  <- "County"
						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 



						var.x    <- "FO4b"
						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
						var.col  <- "SPL_bldg_name"
						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 

						multiplot(p.plot001,p.plot002,p.plot003,cols=2)
					} 	
					cat(paste("92. ",string.thisState,": [",this.codeItem,"]:  plot component D2.\n",sep=""))
					cat(paste("92. ",string.thisState,": [",this.codeItem,"]:  plot component D2.\n",sep=""),file=FL.LOG, append=TRUE)					
				# E. Plot Component of CompCrawlWallR 
 				}else if (this.codeItem == "CompCrawlWallR")
 				{
					# E1. FO7a and CSIQ1
					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"FO7a"]))
 					if (no.obs.thisCode > 0)
 					{
 						var.x    <- "FO7a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "CZ"
 						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "FO7a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "County"
 						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						
 						var.x    <- "FO7a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "SPL_bldg_name"
 						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						
						
 						var.x    <- "FO7a"
 						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
 						var.col  <- "CSIQ1"
 						var.fill <- "CSIQ1"
						p.plot004 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot004 <- p.plot004 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot004 <- p.plot004 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
												
						multiplot(p.plot001,p.plot002,p.plot003,p.plot004,cols=2) 
					}
					cat(paste("93. ",string.thisState,": [",this.codeItem,"]:  plot component D1.\n",sep=""))
					cat(paste("93. ",string.thisState,": [",this.codeItem,"]:  plot component D1.\n",sep=""),file=FL.LOG, append=TRUE)
					
					# E2. FO7b
					no.obs.thisCode <- sum(!is.na(mySubset.lvldropped[,"FO7b"]))
					if (no.obs.thisCode > 0)
					{
						var.x    <- "FO7b"
						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
						var.col  <- "CZ"
						var.fill <- "CZ"
						p.plot001 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot001 <- p.plot001 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot001 <- p.plot001 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 


						var.x    <- "FO7b"
						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
						var.col  <- "County"
						var.fill <- "County"
						p.plot002 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot002 <- p.plot002 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot002 <- p.plot002 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 



						var.x    <- "FO7b"
						var.x.name <- name.keyItem[var.x]
 						var.string <- paste(var.x," (",var.x.name,")",sep="")
						var.col  <- "SPL_bldg_name"
						var.fill <- "SPL_bldg_name"
						p.plot003 <- ggplot(data=mySubset.lvldropped,aes_string(x=var.x,colour=var.col,fill=var.fill)) + geom_histogram() 
						p.plot003 <- p.plot003 + labs(x=paste(var.x," (",var.x.name,")",sep=""),y="No. Observations",title=paste("Compound Code Item (",this.codeItem,"): ",var.string," at ",string.thisState,"\n",no.obs.thisCode," observations",sep=""))
						p.plot003 <- p.plot003 + theme(axis.text.x = element_text(angle=0,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 

						multiplot(p.plot001,p.plot002,p.plot003,cols=2)
					} 	
					cat(paste("94. ",string.thisState,": [",this.codeItem,"]:  plot component D2.\n",sep=""))
					cat(paste("94. ",string.thisState,": [",this.codeItem,"]:  plot component D2.\n",sep=""),file=FL.LOG, append=TRUE)					
 				}
 				
 				
			}			# end of compound code items
 			cat(paste("95. ",string.thisState,": plotting the 9 key code items used for simulation by combining the Cavity and Continuous Insulation.\n",sep=""))
 			cat(paste("95. ",string.thisState,": plotting the 9 key code items used for simulation by combining the Cavity and Continuous Insulation.\n",sep=""),file=FL.LOG, append=TRUE)	




			
			
			# ---------------------------------------------------------------------------------
			# Table of counts: prepare tables of counts (observations) where a categorical variables has "at" "below" and "above" the code requirement.
			# ---------------------------------------------------------------------------------
			# June 9, 2015: a state specifi CLEAN: there is a "30,11" entry for "FI1" at "NC"
			#               temporaily delete the "11" after "30",
			#
			if (this.state == "NC")
			{
				myTable.bldg.long[myTable.bldg.long[,"SPL_bldg_name"]=="15708" & myTable.bldg.long[,"KeyCodeItem"] == "FI1","KeyCodeValue"] <- "30"
 				cat(paste("95B. ",string.thisState,": A NC specific cleaning.\n",sep=""))
 				cat(paste("95B. ",string.thisState,": A NC specific cleaning.\n",sep=""),file=FL.LOG, append=TRUE)	
			}
			
			rm(myTable.summary)
			individual.codeItem <- grep("^Comp|^IQ|^MIQ|^CSIQ|^FI4$|^KW|^EQ|^BG17$",unique(myTable.bldg.long[,"KeyCodeItem"]),value=TRUE,invert=TRUE)
			count.item <- 0
			for (this.item in individual.codeItem)
			{
				this.name <- name.keyItem[match(this.item,names(name.keyItem))]
				mySubset.thisItem <- subset(myTable.bldg.long,subset=(KeyCodeItem==this.item))
				mySubset.thisItem <- mySubset.thisItem[!(is.na(mySubset.thisItem[,"KeyCodeValue"])) & mySubset.thisItem[,"KeyCodeValue"] != "NA"  & mySubset.thisItem[,"KeyCodeValue"] != "NR" & mySubset.thisItem[,"KeyCodeValue"] != "",]
				no.obs <- dim(mySubset.thisItem)[1]
				
				count.item <- count.item + 1
									
				if (no.obs > 0)
				{
					mySubset.thisItem[,"Dist"] <- "At"

					if (this.name == "ACH50" | this.name == "WindowU" | this.name == "WindowSHGC")
					{
						mySubset.thisItem[as.numeric(mySubset.thisItem[,"KeyCodeValue"]) < mySubset.thisItem[,"CodeCompliance"],"Dist"] <- "Above"
						mySubset.thisItem[as.numeric(mySubset.thisItem[,"KeyCodeValue"]) > mySubset.thisItem[,"CodeCompliance"],"Dist"] <- "Below"
					}else if(length(grep("WallR|CeilingR|FloorR|SlabEdgeR|HighEffLamps",this.name,value=TRUE)))
					{
						mySubset.thisItem[as.numeric(mySubset.thisItem[,"KeyCodeValue"]) < mySubset.thisItem[,"CodeCompliance"],"Dist"] <- "Above"
						mySubset.thisItem[as.numeric(mySubset.thisItem[,"KeyCodeValue"]) > mySubset.thisItem[,"CodeCompliance"],"Dist"] <- "Below"
					}else{
						mySubset.thisItem[,"Dist"] <- mySubset.thisItem[,"KeyCodeValue"]
					}
				
					myTable.tmp <- as.data.frame(table(mySubset.thisItem[,"Dist"]))

					if (count.item == 1)
					{
						myTable.summary <- data.frame(codeItem   = this.item,
									      codeName   = this.name,
									      no.obs     = no.obs,
									      Below.Code = ifelse(length(myTable.tmp[myTable.tmp[,"Var1"] == "Below","Freq"]),myTable.tmp[myTable.tmp[,"Var1"] == "Below","Freq"],0),
									      At.Code    = ifelse(length(myTable.tmp[myTable.tmp[,"Var1"] == "At",   "Freq"]),myTable.tmp[myTable.tmp[,"Var1"] == "At",   "Freq"],0),
									      Above.Code = ifelse(length(myTable.tmp[myTable.tmp[,"Var1"] == "Above","Freq"]),myTable.tmp[myTable.tmp[,"Var1"] == "Above","Freq"],0))
					}else{
						myTable.summary <- rbind(myTable.summary,
						                         data.frame(codeItem   = this.item,
									            codeName   = this.name,
									            no.obs     = no.obs,
									            Below.Code = ifelse(length(myTable.tmp[myTable.tmp[,"Var1"] == "Below","Freq"]),myTable.tmp[myTable.tmp[,"Var1"] == "Below","Freq"],0),
									            At.Code    = ifelse(length(myTable.tmp[myTable.tmp[,"Var1"] == "At",   "Freq"]),myTable.tmp[myTable.tmp[,"Var1"] == "At",   "Freq"],0),
									            Above.Code = ifelse(length(myTable.tmp[myTable.tmp[,"Var1"] == "Above","Freq"]),myTable.tmp[myTable.tmp[,"Var1"] == "Above","Freq"],0)))
					}
				}else{
					if (count.item == 1)
					{
						myTable.summary <- data.frame(codeItem   = this.item,
									      codeName   = this.name,
									      no.obs     = 0,
									      Below.Code = 0,
									      At.Code    = 0,
									      Above.Code = 0)
					}else{
						myTable.summary <- rbind(myTable.summary,
						                         data.frame(codeItem   = this.item,
									            codeName   = this.name,
									            no.obs     = 0,
									            Below.Code = 0,
									            At.Code    = 0,
									            Above.Code = 0))
					}			
				}
			}
			cat(paste("Summary Of Key Items,",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
			write.table(myTable.summary,file=FL.SUM.KEY.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			cat(paste("\n\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
 			cat(paste("96. ",string.thisState,": Summary Tables.\n",sep=""))
 			cat(paste("96. ",string.thisState,": Summary Tables.\n",sep=""),file=FL.LOG, append=TRUE)	
			

			#
			# Frequency of foundation and heating system
			#
			for (this.item in c("BG17","EQ1","EQ2"))
			{
				this.name <- name.keyItem[match(this.item,names(name.keyItem))]
				mySubset.thisItem <- subset(myTable.bldg.long,subset=(KeyCodeItem==this.item))
				mySubset.thisItem <- mySubset.thisItem[!(is.na(mySubset.thisItem[,"KeyCodeValue"])) & mySubset.thisItem[,"KeyCodeValue"] != "NA"  & mySubset.thisItem[,"KeyCodeValue"] != "NR" & mySubset.thisItem[,"KeyCodeValue"] != "",]
				no.obs <- dim(mySubset.thisItem)[1]			
				myTable.tmp <- as.data.frame(table(mySubset.thisItem[,"KeyCodeValue"]))
				
				cat(paste("Summary Of ",this.item,",",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
				write.table(myTable.tmp,file=FL.SUM.KEY.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
				cat(paste("\n\n",sep=""),file=FL.SUM.KEY.CSV,append=TRUE)
				
				# write the foundation and htgsys fraction out
				cat(paste("fraction of ",this.item,",",sep=""),file=FL.found.htgSys.share.CSV,append=TRUE)
				write.table(myTable.tmp,file=FL.found.htgSys.share.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	
				cat(paste("97. ",string.thisState,": Summary Tables for Frequency of foundation and heating system.\n",sep=""))
	 			cat(paste("97. ",string.thisState,": Summary Tables for Frequency of foundation and heating system.\n",sep=""),file=FL.LOG, append=TRUE)	
			}

			
			#
			# Frequency of CZ in the state
			#
			myTable.found.share  <- as.data.frame(table(myTable.bldg.wide[,"BG17"]))
			myTable.fuel.share   <- as.data.frame(table(myTable.bldg.wide[,"EQ1"]))
			myTable.htgsys.share <- as.data.frame(table(myTable.bldg.wide[,"EQ2"]))
			myTable.CZ.share     <- as.data.frame(table(myTable.bldg.wide[,"CZ"]))
			save(myTable.found.share,myTable.fuel.share,myTable.htgsys.share,myTable.CZ.share,file=FL.found.htgSys.share.OBJ)

			cat(paste("foundation share in ",this.state,",",sep=""),file=FL.found.htgSys.share.CSV,append=TRUE)
			write.table(myTable.found.share,file=FL.found.htgSys.share.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			
			cat(paste("fuel share in ",this.state,",",sep=""),file=FL.found.htgSys.share.CSV,append=TRUE)
			write.table(myTable.fuel.share,file=FL.found.htgSys.share.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			
			cat(paste("heating system share in ",this.state,",",sep=""),file=FL.found.htgSys.share.CSV,append=TRUE)
			write.table(myTable.htgsys.share,file=FL.found.htgSys.share.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			
			cat(paste("CZ share in ",this.state,",",sep=""),file=FL.found.htgSys.share.CSV,append=TRUE)
			write.table(myTable.CZ.share,file=FL.found.htgSys.share.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			cat(paste("98. ",string.thisState,": shares of foundation, heating systems, CZ in the state are outputted.\n",sep=""))
			cat(paste("98. ",string.thisState,": shares of foundation, heating systems, CZ in the state are outputted.\n",sep=""),file=FL.LOG, append=TRUE)	
			
			
			
			# ***********************************************************************************
			# ***********************************************************************************
			# ***********************************************************************************
			# ***********************************************************************************
			# ***********************************************************************************
			# July 8, 2015: add the one page plot of all numeric code item here.
			#               a subset of the long format is used for this purpose
			# ***********************************************************************************
			# ***********************************************************************************
			# ***********************************************************************************
			# ***********************************************************************************
			# ***********************************************************************************
			field.numeric <- c("^ACH50$","^BsmtWallR\\(Cavity\\)$","^BsmtWallR\\(Cont\\)$","^CeilingR$","^CrawlWallR\\(Cavity\\)$","^CrawlWallR\\(Cont\\)$","^DuctTightness$","^FloorR\\(Cavity\\)$","^FloorR\\(Cont\\)$","^FrameWallR\\(Cavity\\)$","^FrameWallR\\(Cont\\)$","^HighEffLamps$","^IQ\\(CrawlSpaceWallCavity\\)$","^IQ\\(WallCavity\\)$","^IQ\\(BsmtWallCavity\\)$","^IQ\\(MassWallCavity\\)$","^IQ\\(RoofCavity\\)$","^IQ\\(FloorCavity\\)$","^MassWallR\\(Cavity\\)$","^MassWallR\\(Cont\\)$","^SlabEdgeR$","^WindowSHGC$","^WindowU")
			cat(paste("101. ",string.thisState,": the whole sets of field outputted to the csv file.\n",sep=""))
			cat(paste("101. ",string.thisState,": the whole sets of field outputted to the csv file.\n",sep=""),file=FL.LOG, append=TRUE)	
			
			
			# find the matching index
			no.field <- 0
			for (this.field in field.numeric)
			{
				
				tmp <- grep(this.field,myTable.bldg.long[,"KeyCodeName"])
				if (length(tmp)>0)
				{
					no.field <- no.field + 1
					if (no.field == 1)
					{
						index.num <- tmp
					}else{
						index.num <- c(index.num,tmp)
					}
				}
			}
			cat(paste("102. ",string.thisState,": found out thoese rows with these code items in [myData.bldg.long].\n",sep=""))
			cat(paste("102. ",string.thisState,": found out thoese rows with these code items in [myData.bldg.long].\n",sep=""),file=FL.LOG, append=TRUE)	
			
			myTable.bldg.long.num <- myTable.bldg.long[index.num,]
			cat(paste("103. ",string.thisState,": [myTable.bldg.long.num] is a subset of [myData.bldg.long] consistsing only the numeric fields.\n",sep=""))
			cat(paste("103. ",string.thisState,": [myTable.bldg.long.num] is a subset of [myData.bldg.long] consistsing only the numeric fields.\n",sep=""),file=FL.LOG, append=TRUE)
			
			myTable.bldg.long.num <- myTable.bldg.long.num[!(is.na(myTable.bldg.long.num[,"KeyCodeValue"])),]			# keep only non NA entries in the code item value field
			myTable.bldg.long.num <- myTable.bldg.long.num[myTable.bldg.long.num[,"KeyCodeValue"] != "",]				# remove "" entries
			myTable.bldg.long.num <- myTable.bldg.long.num[myTable.bldg.long.num[,"KeyCodeValue"] != "NA",]				# remove "" entries
			myTable.bldg.long.num <- myTable.bldg.long.num[myTable.bldg.long.num[,"KeyCodeValue"] != "NR",]				# remove "" entries
			myTable.bldg.long.num <- myTable.bldg.long.num[myTable.bldg.long.num[,"KeyCodeValue"] != "unassigned",]			# remove "" entries  KY has value of "unassigned" in IQ3, IQ4, IQ2, MIQ1, CSIQ1
			cat(paste("104. ",string.thisState,": [myTable.bldg.long.num] keep only non-NA rows in [myTable.bldg.long.num].\n",sep=""))
			cat(paste("104. ",string.thisState,": [myTable.bldg.long.num] keep only non-NA rows in [myTable.bldg.long.num].\n",sep=""),file=FL.LOG, append=TRUE)


			
			# AL: has multiple ducttightness values which needs to be expanded
			idx.new.row                               <- dim(myTable.bldg.long.num)[1]
			index.multiple                            <- grep(",",myTable.bldg.long.num[,"KeyCodeValue"])		# the index where FI4 has multiple entries
			no.multiple                               <- length(index.multiple)					# the number of rows with multiple entries
			for (index in index.multiple)										# the row index of the entries with multiple values
			{
				multiple.array <- unlist(strsplit(myTable.bldg.long.num[index,"KeyCodeValue"],","))
				myTable.bldg.long.num[index,"KeyCodeValue"] <- multiple.array[1]				# the original row take the first of the multiple entries
				for (tmp.idx in seq(from=2,to=length(multiple.array)))
				{	
					idx.new.row <- idx.new.row + 1								# the rest of the multiple entries is added to the end of the [tmpArray_DuctTightnessR]
					myTable.bldg.long.num[idx.new.row,]               <- myTable.bldg.long.num[index,]	# initialize the new row with the current row
					myTable.bldg.long.num[idx.new.row,"KeyCodeValue"] <- multiple.array[tmp.idx]		# replace the "KeyCodeValue" with the rest of the multplie entries
				}
			}			
			cat(paste("105. ",string.thisState,": expand any possible multiple entries.\n",sep=""))
			cat(paste("105. ",string.thisState,": expand any possible multiple entries.\n",sep=""),file=FL.LOG, append=TRUE)
			
			
			# convert Roman character to Integer
			myTable.bldg.long.num[,"KeyCodeValue"] <-  gsub("^NA$",NA,gsub("^I$","1",gsub("^II$","2",gsub("^III$","3",myTable.bldg.long.num[,"KeyCodeValue"]))))
			cat(paste("106. ",string.thisState,": convert Roman character to integer in [KeyCodeValue] field of [myTable.bldg.long.num].\n",sep=""))
			cat(paste("106. ",string.thisState,": convert Roman character to integer in [KeyCodeValue] field of [myTable.bldg.long.num].\n",sep=""),file=FL.LOG, append=TRUE)

			myTable.bldg.long.num[,"KeyCodeValue"] <-  as.numeric(myTable.bldg.long.num[,"KeyCodeValue"])
			cat(paste("107. ",string.thisState,": convert the [KeyCodeValue] field of [myTable.bldg.long.num] to numeric.\n",sep=""))
			cat(paste("107. ",string.thisState,": convert the [KeyCodeValue] field of [myTable.bldg.long.num] to numeric.\n",sep=""),file=FL.LOG, append=TRUE)
			
			# append the number of observations as additional fields
			myObs.Num <- data.frame(tapply(myTable.bldg.long.num[,"KeyCodeValue"],myTable.bldg.long.num[,"KeyCodeLabel"],length))
			names(myObs.Num) <- "NoObs"
			myObs.Num[,"KeyCodeLabel"] <- row.names(myObs.Num)
			
			myTable.bldg.long.num[,"NoObs"] <- NA
			myTable.bldg.long.num[,"NoObs"] <- myObs.Num[myTable.bldg.long.num[,"KeyCodeLabel"],"NoObs"]
			
			myTable.bldg.long.num[,"KeyCodeString"] <- paste(myTable.bldg.long.num[,"KeyCodeLabel"]," (no obs: ",myTable.bldg.long.num[,"NoObs"],")",sep="")
			cat(paste("108. ",string.thisState,": added [NoObs] and [KyCodeString] as additional columns.\n",sep=""))
			cat(paste("108. ",string.thisState,": added [NoObs] and [KyCodeString] as additional columns.\n",sep=""),file=FL.LOG, append=TRUE)

			
			
			
			
			# plot.obj <- ggplot(data=myTable.bldg.long.num) + geom_jitter(aes(x=CodeItem,y=value))   # note: geom_jitter also pertube the y axis	
			#geom_hline(data=IECC_2009,aes(y=CZ2))+
			plot.obj <- ggplot(data=myTable.bldg.long.num) + geom_point(aes(x=KeyCodeString,y=KeyCodeValue,color=ClimateZone),position=position_jitter(width=0.5,height=0))   # only jitter the x axis
			plot.obj <- plot.obj + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_text(colour="black")) 
			plot.obj <- plot.obj + labs(y="Code Item Observed Range",x="",title=paste(this.state,": Distributions of Key Code Items",sep=""))
			plot.obj <- plot.obj + geom_hline(aes(yintercept=CodeCompliance,color=ClimateZone), data =myTable.bldg.long.num) 		
			plot.obj <- plot.obj + facet_wrap(~KeyCodeString,scales="free")
			cat(paste("107: [",this.state,"]: generate the plot.\n",sep=""))
			cat(paste("107: [",this.state,"]: generate the plot.\n",sep=""),file=FL.LOG,append=TRUE)	
			
			#
			# July 8, 2015: output all files associated with the distribution of the numeirc code field
			#
			saveRDS(myTable.bldg.long.num,file=FL.NumericItem.RDS)
			
			cat(paste(this.state," Numeric Code Items,",sep=""),file=FL.NumericItem.CSV,appen=TRUE)
			write.table(myTable.bldg.long.num,file=FL.NumericItem.CSV,row.names=TRUE,col.names=TRUE,sep=",",append=TRUE)
			
			pdf(file = FL.NumericItem.PDF,paper="special", width=17, height=11,bg = "transparent")	# device 3
			dev.set(3)
				plot(plot.obj)
			dev.off(3)
			
			
			jpeg(file = FL.NumericItem.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")
			dev.set(3)
				plot(plot.obj)
			dev.off(3)
			
			
			#
		}else{	# end of non-zero [myData.keyCode]   data condition		
			cat(paste("201. there is no keyitem data retrieved for ",this.state," at ",timeStamp.string,".\n\n",sep=""))
			cat(paste("201. there is no keyitem data retriveed for ",this.state," at ",timeStamp.string,".\n\n",sep=""),file=FL.LOG, append=TRUE)								
		}

	}else{		# end of non-zero [myData.thisState.cleaned] data condition
		cat(paste("300. there is no data retrieved for ",this.state," at ",timeStamp.string,".\n\n",sep=""))
		cat(paste("300. there is no data retriveed for ",this.state," at ",timeStamp.string,".\n\n",sep=""),file=FL.LOG, append=TRUE)					
	}
	
	dev.off()	
}			# end of state loop
	
	    
# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n04A_prepare_keyItem_for_simulation.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n04A_prepare_keyItem_for_simulation.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [04A_prepare_keyItem_for_simulation.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [04A_prepare_keyItem_for_simulation.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

