#
# 04C_prepare_nonKeyItem.R 
#
# In Windows: need to invoke R i386 not X64
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
array.states <- c("AL","NC","MD","GA","KY","PA","TX","AR")
array.states <- c("MD","AL","GA","KY","NC","PA","TX","AR")
# array.states <- c("NC","AL","GA","KY","MD","PA","AR","TX")

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
Path.TMP         <- paste(Path.Project,"04C_prepare_nonKeyItem",               sep="/")		# Output file directory			
Path.OUT         <- paste(Path.TMP,timeStamp.string,                           sep="/")		# Output file directory	for a particular batch of downloading from database		
Path.LOG         <- paste(Path.Project,"00_log",                               sep="/")		# log file directory	
Path.auxi        <- paste(Path.Project,"00_auxi_files",                        sep="/")		# Input auxiary file directory
if (!file.exists(Path.Data)){print(paste("NOT existing:",Path.Data," Check Why!",sep=""));die}
if (!file.exists(Path.TMP)) {print(paste("NOT existing:",Path.TMP,sep=""));dir.create(Path.TMP,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.OUT)) {print(paste("NOT existing:",Path.OUT,sep=""));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.LOG)) {print(paste("NOT existing:",Path.LOG,sep=""));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.auxi)){print(paste("NOT existing:",Path.auxi," Check Why!",sep=""));die}

# define files
FL.LOG          <- paste(Path.LOG,  paste("04C_prepare_nonKeyItem_",timeStamp.string,".log",sep=""),sep="/")	
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


cat(paste("Log file for data processing script [04C_prepare_nonKeyItem.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
            "*                    [04C_prepare_nonKeyItem.R]                *",
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
	if (this.state == "MD")						# Maryland use IECC2015 instead of IECC2009
	{
		vertical.line.string <- "Vertical Line: IECC2015 Code Requirement"
		maximum_supply_cfm   <- 1200	
		code_supply_cfm      <- 0.08 * 1200 * 100 / 2400	# 4.00 cfm / 100 sf
	}else{
		vertical.line.string <- "Vertical Line: IECC2009 Code Requirement"
		maximum_supply_cfm   <- 1350	
		code_supply_cfm      <- 0.2 * 1350 * 100 / 2400		# 11.25 cfm / 100 sf
	}
	
	
	# -------------------------------------------------------------------------------------------------
	# A. specify the output file names for this state
	# -------------------------------------------------------------------------------------------------
	
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

	FL.found.htgSys.share.OBJ       <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_found.htgSys.share.Rdata",sep=""),sep="/")
	FL.found.htgSys.share.CSV       <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_found.htgSys.share.CSV",  sep=""),sep="/")

	FL.KeyItem.CSV                  <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem.CSV",     sep=""),sep="/")
	FL.KeyItem.Wide.RDS             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Wide.RDS",sep=""),sep="/")
	FL.KeyItem.Long.RDS             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Long.RDS",sep=""),sep="/")
	FL.KeyItem.Wide.CSV             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Wide.CSV",sep=""),sep="/")
	FL.KeyItem.Long.CSV             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Long.CSV",sep=""),sep="/")
	FL.KeyItem.SUM                  <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_sum.CSV", sep=""),sep="/")	
	
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
	if  (file.exists(FL.KeyItem.SUM))                 {print(paste(FL.KeyItem.SUM,                  " exist. Delete it!"));file.remove(FL.KeyItem.SUM)}
	
	FL.KeyItem.PDF             <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem.PDF",            sep=""),sep="/")	
	FL.KeyItem.1page.PDF       <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_1page.PDF",      sep=""),sep="/")
	FL.NonKeyItem.PDF          <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_NonKeyItem.PDF",         sep=""),sep="/")	
	FL.NonKeyItem.Selected.PDF <- paste(Path.OUT ,paste(paste("State",this.state,timeStamp.string,sep="_"),"_NonKeyItem_Selected.PDF",sep=""),sep="/")	
	
	if  (file.exists(FL.KeyItem.PDF))            {print(paste(FL.KeyItem.PDF," exist. Delete it!"))      ;file.remove(FL.KeyItem.PDF)}
	if  (file.exists(FL.KeyItem.1page.PDF))      {print(paste(FL.KeyItem.1page.PDF," exist. Delete it!"));file.remove(FL.KeyItem.1page.PDF)}
	if  (file.exists(FL.NonKeyItem.PDF))         {print(paste(FL.NonKeyItem.PDF," exist. Delete it!"))      ;file.remove(FL.NonKeyItem.PDF)}
	if  (file.exists(FL.NonKeyItem.PDF))         {print(paste(FL.NonKeyItem.PDF," exist. Delete it!"))      ;file.remove(FL.NonKeyItem.PDF)}
	if  (file.exists(FL.NonKeyItem.Selected.PDF)){print(paste(FL.NonKeyItem.Selected.PDF," exist. Delete it!"))      ;file.remove(FL.NonKeyItem.Selected.PDF)}
	
	cat(paste("3. ",string.thisState,": Specify the file name for this state.\n",sep=""))
	cat(paste("3. ",string.thisState,": Specify the file name for this state.\n",sep=""),file=FL.LOG, append=TRUE)								
	
	# -------------------------------------------------------------------------------------------------
	# open pdf file for plotting
	# -------------------------------------------------------------------------------------------------
	pdf(file = FL.KeyItem.PDF,            paper="special", width=17, height=11,bg = "transparent")	# device 2
	pdf(file = FL.KeyItem.1page.PDF,      paper="special", width=17, height=11,bg = "transparent")	# device 3
	pdf(file = FL.NonKeyItem.PDF,         paper="special", width=17, height=11,bg = "transparent")	# device 4
	pdf(file = FL.NonKeyItem.Selected.PDF,paper="special", width=17, height=11,bg = "transparent")	# device 5
	

	
	cat(paste("4. ",string.thisState,": open pdf files for plots.\n",sep=""))
	cat(paste("4. ",string.thisState,": open pdf files for plots.\n",sep=""),file=FL.LOG, append=TRUE)		

	# -------------------------------------------------------------------------------------------------
	# B. uploading state data from the RDS file into [myData.thisState]
	# -------------------------------------------------------------------------------------------------
	myData.thisState <- readRDS(file=FL.IN.RDS)
	cat(paste("5. ",string.thisState,": data has been read into [myData.thisState].\n",sep=""))
	cat(paste("5. ",string.thisState,": data has been read into [myData.thisState].\n",sep=""),file=FL.LOG, append=TRUE)								

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

		# -------------------------------------------------------------------------------------------------
		# turn [myData.clean]    into [myData.clean.bldg.wide]:    Make a big table to list all code items in term of building before and after clean for QA purpose
		# turn [myData.notClean] into [myData.notClean.bldg.wide]: Make a big table to list all code items in term of building before and after clean for QA purpose
		# -------------------------------------------------------------------------------------------------
		myData.clean.bldg.wide    <- as.data.frame(dcast(myData.clean,   SPL_bldg_name ~ CIA_codeitem_code,value.var="PV_codeitem_value"))
		myData.notClean.bldg.wide <- as.data.frame(dcast(myData.notClean,SPL_bldg_name ~ CIA_codeitem_code,value.var="PV_codeitem_value"))
		cat(paste("29. ",string.thisState,": prepared [myData.clean.bldg.wide] and [myData.notClean.bldg.wide]\n",sep=""))
		cat(paste("29. ",string.thisState,": prepared [myData.clean.bldg.wide] and [myData.notClean.bldg.wide]\n",sep=""),file=FL.LOG, append=TRUE)

			

		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&		
		# June 8, 2016: PLOT on [myData.notClean]  BEFORE CLEANING
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

		# -----------------------------------------------------------------------------------------------
		# S. plotting original 18 code items
		# -----------------------------------------------------------------------------------------------
		list.codeItem.keyItem             <- c("BG17","EQ1","EQ2","FI1","IQ1","FI17","FI4","FI6","FR2","FR3","FO1","FO4a","FO4b","IQ4","IN1a","IN1b","IQ2","FO7a","FO7b","CSIQ1","FR10a","FR10b","MIQ1","IN3a","IN3b","IQ3")
		list.codeItem.inData              <- names(myData.notClean.bldg.wide)[-1]
		list.codeItem.keyItem.inData      <- intersect(list.codeItem.inData,list.codeItem.keyItem)
		list.codeItem.nonKeyItem.inData   <- list.codeItem.inData[-match(list.codeItem.keyItem.inData,list.codeItem.inData)]		
		list.codeItem.nonKeyItem.Selected <- grep("^Comp",list.codeItem.nonKeyItem.inData,value=TRUE,invert=TRUE)

		
		cat(paste("31. ",string.thisState,": get the lable of key code  items in not cleaned data set [myData.notClean.bldg.wide]\n",sep=""))
		cat(paste("31. ",string.thisState,": get the lable of key code  items in not cleaned data set [myData.notClean.bldg.wide]\n",sep=""),file=FL.LOG, append=TRUE)

		for (this.batch in c("KeyCode","NonKeyCode","NonKeyCodeSelected"))
		{
		
			if (this.batch == "KeyCode")
			{
				list.codeItem.label.inData <- list.codeItem.keyItem.inData
				dev.label       <- 2
				dev.label.1page <- 3
			}else if (this.batch == "NonKeyCode")
			{
				list.codeItem.label.inData <- list.codeItem.nonKeyItem.inData
				dev.label       <- 4			
			}else if (this.batch == "NonKeyCodeSelected")
			{
				list.codeItem.label.inData <- list.codeItem.nonKeyItem.Selected
				dev.label       <- 5			
			}
			cat(paste("32. ",string.thisState,": use trhe proper list of code item label in the data set [myData.notClean.bldg.wide]\n",sep=""))
			cat(paste("32. ",string.thisState,": use trhe proper list of code item label in the data set [myData.notClean.bldg.wide]\n",sep=""),file=FL.LOG, append=TRUE)

			
			# 
			# loopping through the key cod eitems
			#
			command.string.plot <- "multiplot("	# this is only for key code item
			no.in.mplot <- 0
			for (this.codeItem.label in list.codeItem.label.inData)
			{
				# find the corresponding short description and range if it is a numeric code item
				idx.matched <- match(this.codeItem.label,myMap[,"ID"])
				this.codeItem.label.desp <- myMap[idx.matched,"Short.Name"]
				cat(paste("40. ",string.thisState,": description and range of [",this.codeItem.label,"]-[",this.codeItem.label.desp,"].\n",sep=""))
				cat(paste("40. ",string.thisState,": description and range of [",this.codeItem.label,"]-[",this.codeItem.label.desp,"].\n",sep=""),file=FL.LOG, append=TRUE)	


				mySubset.inData <- subset(myData.notClean,subset=(CIA_codeitem_code == this.codeItem.label))
				cat(paste("41. ",string.thisState,": subsetting the data for [",this.codeItem.label,"]-[",this.codeItem.label.desp,"].\n",sep=""))
				cat(paste("41. ",string.thisState,": subsetting the data for [",this.codeItem.label,"]-[",this.codeItem.label.desp,"].\n",sep=""),file=FL.LOG, append=TRUE)
				
				# remove NA rows
				mySubset.inData <- mySubset.inData[!(is.na(mySubset.inData[,"PV_codeitem_value"])),]
				mySubset.inData <- mySubset.inData[mySubset.inData[,"PV_codeitem_value"] != "NA" & mySubset.inData[,"PV_codeitem_value"] != "NR" & mySubset.inData[,"PV_codeitem_value"] != "",]							
				cat(paste("42. ",string.thisState,": [",this.codeItem.label,"]:  detele the NA rows.\n",sep=""))
				cat(paste("42. ",string.thisState,": [",this.codeItem.label,"]:  detele the NA rows.\n",sep=""),file=FL.LOG, append=TRUE)
				
				#
				# June 10, 2015
				# 
				if ((this.codeItem.label == "FI1") & (this.state == "NC"))
				{
					mySubset.inData[mySubset.inData[,"PV_codeitem_value"] == "30,11","PV_codeitem_value"] <- "30"
					mySubset.inData[,"PV_codeitem_value"] <- as.numeric(as.character(mySubset.inData[,"PV_codeitem_value"])) 
				}
				mySubset.inData.lvldropped <- droplevels(mySubset.inData)
				cat(paste("43. ",string.thisState,": drop the non-existing levels in [mySubset.inData] and re-save in [mySubset.inData.lvldropped].\n",sep=""))
				cat(paste("43. ",string.thisState,": drop the non-existing levels in [mySubset.inData] and re-save in [mySubset.inData.lvldropped].\n",sep=""),file=FL.LOG, append=TRUE)
				

				# out put the record to a summary file
				no.records.nonNA.inData <- length(!(is.na(mySubset.inData.lvldropped[,"PV_codeitem_value"])))
				cat(paste("CodeItem in Database",this.state,this.codeItem.label,this.codeItem.label.desp,no.records.nonNA.inData,"\n",sep=","),file=FL.allData.SUM, append=TRUE)	
				cat(paste("43B. ",string.thisState,": [",this.codeItem.label,"]-[",this.codeItem.label.desp,"]: number of non-NA records has been saved to [",FL.allData.SUM,"].\n",sep=""))
				cat(paste("43B. ",string.thisState,": [",this.codeItem.label,"]-[",this.codeItem.label.desp,"]: number of non-NA records has been saved to [",FL.allData.SUM,"].\n",sep=""),file=FL.LOG, append=TRUE)	

				# -------------------------------------------------------------------------------------------------
				#              CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN, CLEAN
				# May 14, 2015: if the field is like FI4 with multiple entries, break down and calculate the average instead
				# -------------------------------------------------------------------------------------------------
				if (this.codeItem.label == "FI4" & this.state == "AL")
				{
					# # May 15: assume multiple observation sof the same system, so take means
					# mySubset.inData.lvldropped[,"PV_codeitem_value.inData"]  <- mySubset.inData.lvldropped[,"PV_codeitem_value"]
					# mySubset.inData.lvldropped[,"PV_codeitem_value.form"] <- paste("mean(c(",mySubset.inData.lvldropped[,"PV_codeitem_value.inData"],"),na.rm=TRUE)",sep="")
					# 
					# for (index in seq(1:dim(mySubset.inData.lvldropped)[1]))
					# {
					# 	mySubset.inData.lvldropped[index,"PV_codeitem_value"] <- eval(parse(text=mySubset.inData.lvldropped[index,"PV_codeitem_value.form"]))
					# }
					# string.multiple <- " Multiple entries are averaged!"
					
					#
					# May 19, 2015: based on May 18, 2015 discussion with J and Vrushali: assume multiple observations are taken from multiple systems of the same house, the multiple entries should then be split into multiple individual entries
					#
					if (is.factor(mySubset.inData.lvldropped[,"PV_codeitem_value"]))
					{
						mySubset.inData.lvldropped[,"PV_codeitem_value"]     <- as.character(mySubset.inData.lvldropped[,"PV_codeitem_value"])
						mySubset.inData.lvldropped[,"PV_codeitem_value.inData"] <- mySubset.inData.lvldropped[,"PV_codeitem_value"]
					}else{
						mySubset.inData.lvldropped[,"PV_codeitem_value.inData"] <- mySubset.inData.lvldropped[,"PV_codeitem_value"]
					}

					idx.new.row                                        <- dim(mySubset.inData.lvldropped)[1]					
					index.multiple                                     <- grep(",",mySubset.inData.lvldropped[,"PV_codeitem_value.inData"])	# the index where FI4 has multiple entries
					no.multiple                                        <- length(index.multiple)				# the number of rows with multiple entries
					string.multiple                                    <- paste(" [",no.multiple,"] obs with multiple entries are expanded!",sep="")
					for (index in index.multiple)		# the row index of the entries with multiple values
					{
						multiple.array <- unlist(strsplit(mySubset.inData.lvldropped[index,"PV_codeitem_value.inData"],","))
						mySubset.inData.lvldropped[index,"PV_codeitem_value"] <- multiple.array[1]				# the original row take the first of the multiple entries
						for (tmp.idx in seq(from=2,to=length(multiple.array)))
						{	
							idx.new.row <- idx.new.row + 1							# the rest of the multiple entries is added to the end of the [mySubset.inData.lvldropped]
							mySubset.inData.lvldropped[idx.new.row,]               <- mySubset.inData.lvldropped[index,]	# everything else in the new row has the same value as the row of the the multiple entry
							mySubset.inData.lvldropped[idx.new.row,"PV_codeitem_value"] <- multiple.array[tmp.idx]	# replace the "PV_codeitem_value" with the rest of the multplie entries
						}
					}
					
					# May 29, 2015: convert the observed leakage rate (cfm/100sf) into leakage ratio
					index.withValue <- (!(is.na(mySubset.inData.lvldropped[,"PV_codeitem_value"]))       & 
					                           (mySubset.inData.lvldropped[,"PV_codeitem_value"] !="NA") & 
					                           (mySubset.inData.lvldropped[,"PV_codeitem_value"] !="")    )
					# mySubset.inData.lvldropped[index.withValue,"PV_codeitem_value"] <- (as.numeric(mySubset.inData.lvldropped[index.withValue,"PV_codeitem_value"]) * 2400 / 100) / maximum_supply_cfm
					mySubset.inData.lvldropped[index.withValue,"PV_codeitem_value"] <- as.numeric(mySubset.inData.lvldropped[index.withValue,"PV_codeitem_value"])	# june 9, 2015: showing the measure unit cfm/100 sf instead of the simulation unit the leakage ratio 
					
				}else{
					string.multiple <- ""
				}
				cat(paste("44. ",string.thisState,": [",this.codeItem.label,"]-[",this.codeItem.label.desp,"]: Handling multiple entries as in duct tightness in FI4.\n",sep=""))
				cat(paste("44. ",string.thisState,": [",this.codeItem.label,"]-[",this.codeItem.label.desp,"]: Handling multiple entries as in duct tightness in FI4.\n",sep=""),file=FL.LOG, append=TRUE)	


				no.data.inData   <- dim(mySubset.inData.lvldropped)[1] 
			      # no.Subset.inData <- dim(mySubset.inData.lvldropped)[1]
				no.Subset.inData <- sum(!(is.na(mySubset.inData.lvldropped[,"PV_codeitem_value"])))	
				cat(paste("44B. ",string.thisState,": [",this.codeItem.label,"]-[",this.codeItem.label.desp,"]: number of observations.\n",sep=""))
				cat(paste("44B. ",string.thisState,": [",this.codeItem.label,"]-[",this.codeItem.label.desp,"]: number of observations.\n",sep=""),file=FL.LOG, append=TRUE)	
				
				
				#
				# dynamic including plot into multiplot
				#
				myWork <- mySubset.inData.lvldropped
				names(myWork) <- sub("PV_codeitem_value","FieldValue",sub("SPL_CZ","ClimateZone",names(myWork)))
				if(!(is.factor(myWork[,"ClimateZone"]))){myWork[,"ClimateZone"] <- as.factor(myWork[,"ClimateZone"])}

				no.unique.lvl <- length(unique(myWork[,"FieldValue"])) 
				if (no.unique.lvl >= 10){xtick.size = 5}else{xtick.size = 10}
								
					
				if (no.Subset.inData > 0)
				{

					if ((length(grep("[^0-9\\.]",myWork[,"FieldValue"]))) | (length(grep("(\\d*\\.+)(\\d*\\.+)(.*)$",myWork[,"FieldValue"]))))	
					{
						cat(paste("**** ",string.thisState,": [",this.codeItem.label,"]: is a character field.  No plotting at this time!\n",sep=""))
						cat(paste("**** ",string.thisState,": [",this.codeItem.label,"]: is a character field.  No plotting at this time!\n",sep=""),file=FL.LOG, append=TRUE)
						
						p.plot1 <- ggplot(myWork, aes(x=factor(1), fill=factor(FieldValue))) + geom_bar(width=1) + coord_polar(theta="y")
						p.plot1 <- p.plot1 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot1 <- p.plot1 + labs(x=paste(this.codeItem.label,"(",this.codeItem.label.desp,")",sep=" "),y="count",title=paste(string.thisState,": ( ",this.codeItem.label,")-(",this.codeItem.label.desp,")\nNo. Observations: ",no.Subset.inData,sep=""))
						# plot(p.plot1)
									

						# bar chart in term of [climate zone]			
						p.plot4 <- qplot(data=myWork,FieldValue,colour=ClimateZone,fill=ClimateZone,geom="bar") 
						p.plot4 <- p.plot4 + theme(axis.text.x = element_text(angle=90,color="black",size=xtick.size),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot4 <- p.plot4 + labs(x=paste(this.codeItem.label,"(",this.codeItem.label.desp,")",sep=" "),y="count",title=paste(string.thisState,": ( ",this.codeItem.label,")-(",this.codeItem.label.desp,")\nNo. Observations: ",no.Subset.inData,sep=""))
					       # p.plot4 <- p.plot4 + theme(legend.title = element_text(colour="chocolate",size=14,face="bold")) + scale_color_discrete(name="This Color")
						# plot(p.plot4)	
						
						if (this.batch == "KeyCode")
						{
							dev.set(2)
						}else if(this.batch == "NonKeyCode")
						{
							dev.set(4)
						}else if(this.batch == "NonKeyCodeSelected")
						{
							dev.set(5)
						}
						multiplot(p.plot1,p.plot4,cols=2)	
		
		
					}else{
						# convert the numeric fiel first
						if(is.character(myWork[,"FieldValue"])){myWork[,"FieldValue"] <- as.numeric(myWork[,"FieldValue"])}
						if(is.factor(myWork[,"FieldValue"])){myWork[,"FieldValue"] <- as.numeric(as.character(myWork[,"FieldValue"]))}
					
	


						p.plot.before22A <- ggplot(data=myWork,aes(x=FieldValue,colour=factor(FieldValue),fill=factor(FieldValue))) + geom_histogram() + facet_wrap(~ ClimateZone,ncol = 1)
						p.plot.before22A <- p.plot.before22A + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot.before22A <- p.plot.before22A + labs(x=paste(this.codeItem.label,"(",this.codeItem.label.desp,")",sep=" "),y="count",title=paste(string.thisState,": ( ",this.codeItem.label,")-(",this.codeItem.label.desp,")\nNo. Observations: ",no.Subset.inData,sep=""))
					      #p.plot.before22A <- p.plot.before22A + geom_vline(aes(xintercept=CodeCompliance), data = myWork)
						# plot(p.plot.before22A)

					      # p.plot.before22B <- qplot(data=myWork,FieldValue,colour=ClimateZone,fill=ClimateZone,geom="histogram",facets=ClimateZone~.) 
						p.plot.before22B <- ggplot(data=myWork,aes(x=FieldValue,colour=factor(ClimateZone),fill=factor(ClimateZone))) + geom_histogram() + facet_wrap(~ ClimateZone,ncol = 1)
						p.plot.before22B <- p.plot.before22B + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
						p.plot.before22B <- p.plot.before22B + labs(x=paste(this.codeItem.label,"(",this.codeItem.label.desp,")",sep=" "),y="count",title=paste(string.thisState,": ( ",this.codeItem.label,")-(",this.codeItem.label.desp,")\nNo. Observations: ",no.Subset.inData,sep=""))
					      #p.plot.before22B <- p.plot.before22B + geom_vline(aes(xintercept=CodeCompliance), data = myWork)
						# plot(p.plot.before22B)


						if (this.batch == "KeyCode")
						{
							dev.set(2)
						}else if(this.batch == "NonKeyCode")
						{
							dev.set(4)
						}else if(this.batch == "NonKeyCodeSelected")
						{
							dev.set(5)
						}
						multiplot(p.plot.before22A,p.plot.before22B,cols=2)	


						#
						# dynamic plotting
						#
						if (this.codeItem.label == "FI17")
						{
							mplot.before.ach50 <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.ach50,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "FI1")
						{
							mplot.before.ceilingR <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.ceilingR,",sep="")
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "FI4")
						{
							mplot.before.leakageratio <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.leakageratio,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "FI6")
						{
							mplot.before.highefflump <- p.plot.before22A
							
							command.string.plot <- paste(command.string.plot,"mplot.before.highefflump,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "FO1")
						{
							mplot.before.slabedgeR <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.slabedgeR,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "FO4a")
						{
							mplot.before.bsmtwallCavityR <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.bsmtwallCavityR,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "FO4b")
						{
							mplot.before.bsmtwallContR <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.bsmtwallContR,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "FR2")
						{
							mplot.before.windowU <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.windowU,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "FR3")
						{
							mplot.before.windowSHGC <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.windowSHGC,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "IN1a")
						{
							mplot.before.FlooRCavity <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.FlooRCavity,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "IN1b")
						{
							mplot.before.FlooRCont <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.FlooRCont,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "IN3a")
						{
							mplot.before.FrameWallRCavity <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.FrameWallRCavity,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "IN3b")
						{
							mplot.before.FrameWallRCont <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.FrameWallRCont,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "FO4a")
						{
							mplot.before.BsmtWallRCavity <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.BsmtWallRCavity,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}else if(this.codeItem.label == "FO4b")
						{
							mplot.before.BsmtWallRCont <- p.plot.before22A
							command.string.plot <- paste(command.string.plot,"mplot.before.BsmtWallRCont,",sep="")	
							no.in.mplot <- no.in.mplot + 1
						}
					}		# end of judging if it is a numeric field
				}
				if (this.codeItem.label == "FI4")
				{
				# die
				}
				
			}	# end of the batch loop
			cat(paste("45. ",string.thisState,": [plotting the original 18 key code items in the database.\n",sep=""))
			cat(paste("45. ",string.thisState,": [plotting the original 18 key code items in the database.\n",sep=""),file=FL.LOG, append=TRUE)	

			
			if (this.batch == "KeyCode")
			{
				dev.set(3)
				if (no.in.mplot<=4)
				{
					ncols = 2
				} else if (no.in.mplot<=9)
				{
					ncols = 3
				} else
				{
					ncols = 4
				} 
				
				command.string.plot <- paste(command.string.plot,"cols = ",ncols,")",sep="")
				eval(parse(text=command.string.plot))	
			}
			
			
			# June 9, 2015: BEFORE, no change. During PNNL's meeting with DOE, the plots of FO4b, IN1b, FO1, FO4a, IN1a are removed. from the post-cleaned plot
			# multiplot(mplot.before.ach50,           mplot.before.highefflump,mplot.before.bsmtwallCavityR,mplot.before.bsmtwallContR,
			#           mplot.before.ceilingR,        mplot.before.FlooRCavity,mplot.before.FlooRCont,      mplot.before.slabedgeR,
			#           mplot.before.FrameWallRCavity,mplot.before.windowU,    mplot.before.windowSHGC,     mplot.before.leakageratio,cols=4)
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&		
		# PLOT on [myData.notClean]
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		}	# end of the batch plotting of "KeyItem" and "nonKeyItem"
	}		# end of the non-zero data
		


	dev.off(2)
	dev.off(3)
	dev.off(4)	
	dev.off(5)
}			# end of state loop
	
	    
# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n04C_prepare_nonKeyItem.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n04C_prepare_nonKeyItem.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [04C_prepare_nonKeyItem.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [04C_prepare_nonKeyItem.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

