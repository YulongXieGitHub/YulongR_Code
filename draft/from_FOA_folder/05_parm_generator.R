#
# 05_parm_generator.R
#
# July 7, 2015: for final AL EUI analysis (based on discussion in a meeting on July 6, 2015)
#               (a) stay with the fixed 1500 samples for each CZ
#               (b) not providing a state EUI cdf which is not possible for the fixed 1500 samples for each CZ
#               (c) providing a weighted histogram through weighted count in the bins
# Note: (1) "2015Jun15": 		the data downloaded date
#       (2) "AL_June29_2015_Final": 	simulation folder
#
# June 26, 2015: we want to have all insulation quality as separate parm variables.  Corresponding changes will be made on the templates as well.
#                name convention of the insulation quality parm variables are:  ins_qual_wall
#                                                                               ins_qual_ceiling
#                                                                               ins_qual_floor
#                                                                               ins_qual_bsmtwall
#                value should be 1, 2, 3
#                the second change is to use 1500 for each CZ as the randomly drawn numbers
#
#
# June 5, 2015: add duct leakage simulation preparation
#
# This script was modified on "1_parm_generator_MonteCarlo" of Phase 0 where random draw was conducted on all nine code items simultaneously based on the value range from Delphi Process.
# 
# randomly draw 1500 times see page 15 of analysis methodology document "\\Poncho\resstd\FOA\_documentsDOE Residential Energy Code Field Study Methodology_012815_v3.docx" 
#
# Input data:
# "119_location_mapping_YLX.csv"
#
# ----------------------------------------------------------------------------------------------------------------------------------------
# May 28, 2015: the duct tightness observation is in a unit of cfm/100 sf.  The code requirement is also in the same unit (e.g., IECC2012 is  4    cfm/100 sf).  But in the simulation we use leakage_ratio to represent it (e.g., for IECC2012, sf has 2400 sf.  the maximum supply cfm is 1200 cfm.  The leakage cfm is 4     cfm/100 * 2400 =  96 cfm.  So the leakage ration is  96 / 1200 = 0.08.
#                                                                                                                                IECC2009 is 11.25 cfm/100 cf                                                                      for IECC2009, sf has 2400 sf.  the maximum supply cfm is 1350 cfm.  The leakage cfm is 11.25 cfm/100 * 2400 = 270 cfm.  So the leakage ration is 270 / 1350 = 0.20.                  
#                                                                                                                                IECC2009 is 18.75 cfm/100 cf                                                                      for IECC2006, sf has 2400 sf.  the maximum supply cfm is 1500 cfm.  The leakage cfm is 18.75 cfm/100 * 2400 = 450 cfm.  So the leakage ration is 450 / 1500 = 0.30.  
# for cfm/100 sf observations, we will calculate the leakage ratio as: Y = X * 2400 / 100 (cfm) for IECC2009: Z = Y (cfm) /1350 (cfm)
#                                                                                               for IECC2009: Z = Y (cfm) /1200 (cfm)
# ----------------------------------------------------------------------------------------------------------------------------------------
# August 19, 2014
# August 20, 2014: change "." to "_" the field name and added foundation type as varying variable based on Vrushali's comment.
# August 22, 2014: sampling strategy revised (see J's August 21, 2014 ppt slides).  The change focus on detecting difference of each code item instead of detecting the whole building energy change. The run generation therefore changes.
# August 25, 2014
# This script is to analyze the simulation data from the Monte Carlo sample (about ~6000 - 7000 models)
# The models runs are conducted on 
# 1) nine measures which are 	i) "r_ceiling", ii) "r_wall", iii) "r_floor", iv) "u_window", v) "shgc_window", vi) "f_inc_hw", vii) "ach50", viii) "r_bsmtwall",and ix) "r_slab"
#    the value range of the nine code items vary from CZ (Mark provided value range estimates, New Expected Range 8-19-2014 v2 Delphi Input.xlsx, Tuesday, August 19, 2014 10:26 AM)
# 2) foundations: 		i) "heatedbsmt", ii) "unheatedbsmt", iii) "crawlspace", and iv) "slab"
# 3) four htgSys: 		i) "gasfurnace", ii) "electricfurnace", iii) "heatpump", and iv) "oilfurnace")
# 4) eight climate cities:	i) "Houston",ii) "Phoenix", iii) "Memphis", iv) "ElPaso", v) "Baltimore", vi) "Albuquerque", vii) "Chicago", and viii) "Burlington")
# 5) eight states:		i) PA, ii) MD, iii) NC, iv) AL, v) GA, vi) AR, vii) TX, viii) KY
#    map state to climate city:
#    i) PA --> 		         | ii) MD --> 		   | iii) NC -->              | iv) AL --> 2A Houston | v) GA --> 2A Houston   | vi) AR --> 	          | vii)TX --> 2A Houston     | vii) KY --> 		          | (3) 
#				 | 			   | 			      | 		      | 	               |  		          |            2B Phoenix     | 				  | (1)
#		  	   	 | 	   		   | 	         3A Memphis   | 	   3A Memphis | 	  3A Memphis   | 	    3A Memphis	  |            3A Memphis     | 				  | (5)
#				 | 			   | 			      | 		      | 		       |  			  |            3B El Paso     | 				  | (1)
#		  4A Baltimore	 | 	      4A Baltimore | 		 4A Baltimore | 		      | 	  4A Baltimore | 	    4A Baltimore  |                           | 		    4A Baltimore  | (6)
#		          	 | 			   | 			      | 		      | 		       | 			  | 	       4B Albuquerque | 				  | (1)
#		  5A Chicago	 | 	      5A Chicago   | 		 5A Chicago   | 		      | 		       | 			  | 			      | 				  | (3)
#		  6A Burlington	 | 			   | 			      | 		      | 		       | 			  | 			      | 				  | (1)
# 6) weights:
#    weights used in the previous 2015 IECC determination (May 17, 2014): 		\\poncho\resstd\determination\IECC2015\quantitative\analysis\documentation\weights\weights.xlsx.
#    weights used in the 2014 May-June for 2015 IECC determination (June 6, 2014): 	\\poncho\resstd\determination\IECC2015\quantitative\analysis\00_weights	   (several csv files generated: revised_weights_single_CZ_foundation.csv, revised_weights_single_CZ_htgsys.csv, revised_weights_CZ.csv and one excel file probably revised on the excel file received on May 17, 2014 from Vrushali: revised_weights_2014_june16_YLX.xlsx)
#   The following weights are taken from \\poncho\resstd\determination\IECC2015\quantitative\analysis\documentation\weights\weights.xlsx
#   

# eliminate all stuff
rm(list = ls(all = TRUE))

# **********************************************************************************
# building areas
# **********************************************************************************
footages <- c(2400,1200)	# place holder: need to verify
names(footages) <- c("singlefamily","multifamily")


# list of CZ in the state where sample was taken: see "Combining Data Across Climate Zones.docx" attached in Mark's April 27, 2015 10:55 am email.
# The following are taken from Ivy's sample plan files sent on April 27, 2015 11:05 AM where she provided smaple plan file of seven states without GA ("\\poncho\resstd\FOA\_documents\sample_plan\").
# The CZ to be sampled in the seven states are documented in  "\\poncho\resstd\FOA\Phase1_Analysis\RCD_Analysis\00_auxi_files\SampledCZ_in_States.xlsx"
# 
list.CZ.in.state <- data.frame(PA = list(                    "Baltimore","Chiacago"),
                               MD = list(                    "Baltimore"),			# unknow in Mark's April 27 2015 email
                               NC = list(          "Memphis","Baltimore"),
                               AL = list("Houston","Memphis"),
                               GA = list(          "Memphis"),
                               AR = list(          "Memphis","Baltimore"),
                               TX = list("Houston"                      ),
                               KY = list(          "Baltimore"))




# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

# setup start date and time, random seed
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# load libraries needed
library("reshape2")
library("lattice")
library("ggplot2")

#
col.array <- rep(c("cyan","light green","green","grey","magenta","cyan","pink","black"),5)
col.array <- rep(c(rgb(68,118,178,maxColorValue=255),rgb(215,228,189,maxColorValue=255),"green","grey","magenta","cyan","pink","black"),5)

# today's month, day and year in the format of "Thu Jun 16 08:48:36 2011", 5 fields separated by space
today.month  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
today.day    <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
today.year   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]
today.hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
today.minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
today.second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]

# number of samples
no.2draw <- 1500		# randomly draw 1500 sets of paraeters for a given state.


# -------------------------------------------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current  <- "/phome/resstd/FOA/Phase1_Analysis/RCD_Analysis/0_scripts"
	Path.Project  <- "/phome/resstd/FOA/Phase1_Analysis/RCD_Analysis"
	Path.Simu     <- "/phome/resstd/FOA/Phase1_Analysis/RCD_Analysis/simulation/singlefamily"
}else{
	Path.Current  <- "Y:/FOA/Phase1_Analysis/RCD_Analysis/0_scripts"
	Path.Project  <- "Y:/FOA/Phase1_Analysis/RCD_Analysis"
	Path.Simu     <- "Y:/FOA/Phase1_Analysis/RCD_Analysis/simulation/singlefamily"	
		
}

setwd(Path.Current)


# *********************************************************************************************************
# Select data from corresponding downloading
# *********************************************************************************************************
timeStamp.string <- "2015Jun15"

array.states <- c("AL","AR","GA","KY","MD","NC","PA","TX")
	
# define log path and output weight folder
Path.LOG  <- paste(Path.Project,"0_log",sep="/")
Path.TMP  <- paste(Path.Project,"05_parm_generator",sep="/")
Path.OUT  <- paste(Path.TMP,timeStamp.string,sep="/")
Path.Data <- paste(Path.Project,"04A_prepare_keyItem_for_simulation",timeStamp.string,sep="/")
Path.Auxi <- paste(Path.Project,"00_auxi_files",sep="/")

if (!file.exists(Path.TMP)) {print(paste("NOT existing:",Path.TMP));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.OUT)) {print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.LOG)) {print(paste("NOT existing:",Path.LOG));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Data)){print(paste("NOT existing:",Path.Data," Check Why!",sep=""));die}
if (!file.exists(Path.Auxi)){print(paste("NOT existing:",Path.Auxi," Check Why!",sep=""));die}

FL.LOG <- paste(Path.LOG, "05_parm_generator.log",       sep="/")
FL.EPW <- paste(Path.Auxi,"119_location_mapping_YLX.csv",sep="/")
if   (file.exists(FL.LOG)) {print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}	
if (!(file.exists(FL.EPW))){print(paste("NOT existing:",FL.EPW," Check Why!",sep=""));die}	
cat(paste("1: define a log/output folder.\n",sep=""))
cat(paste("1: define a log/output folder.\n",sep=""),file=FL.LOG,append=TRUE)

#
# load the weather file for the state to [myEPW.state] consists of state abb, CZ label, epw file
#
myEPW.state <- read.table(FL.EPW,header=TRUE,stringsAsFactors=FALSE,sep=",")
cat(paste("1B: weather file for the state are inputted.\n",sep=""))
cat(paste("1B: weather file for the state are inputted.\n",sep=""),file=FL.LOG,append=TRUE)

# define CZ
CZ.city  	<- c("Miami",     "Houston","Phoenix","Memphis","ElPaso","SanFrancisco","Baltimore","Albuquerque","Salem", "Chicago","Boise", "Burlington","Helena","Duluth","Fairbanks")
CZ.idx   	<- c( 1,           2,        2,        3,        3,       3,             4,          4,            4,       5,        5,       6,           6,       7,       8)
CZ.moist 	<- c("warm_humid","moist",  "dry",    "moist",  "dry",   "marine",      "moist",    "dry",        "marine","moist",  "dry",   "moist",     "dry",   "moist",  "moist")
CZ.label        <- c("A",         "A",      "B",      "A",      "B",     "C",           "A",        "B",          "C",     "A",      "B",     "A",          "B",    "A",      "A")
WZ.file         <- c("USA_FL_Miami.Intl.AP.722020_TMY3.epw",     "USA_TX_Houston-Bush.Intercontinental.AP.722430_TMY3.epw","USA_AZ_Phoenix-Sky.Harbor.Intl.AP.722780_TMY3.epw","USA_TN_Memphis.Intl.AP.723340_TMY3.epw","USA_TX_El.Paso.Intl.AP.722700_TMY3.epw","USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw","USA_MD_Baltimore-Washington.Intl.AP.724060_TMY3.epw","USA_NM_Albuquerque.Intl.AP.723650_TMY3.epw","USA_OR_Salem-McNary.Field.726940_TMY3.epw", "USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw","USA_ID_Boise.Air.Terminal.726810_TMY3.epw", "USA_VT_Burlington.Intl.AP.726170_TMY3.epw","USA_MT_Helena.Rgnl.AP.727720_TMY3.epw","USA_MN_Duluth.Intl.AP.727450_TMY3.epw","USA_AK_Fairbanks.Intl.AP.702610_TMY3.epw")
df.CZ <- data.frame(CZ.city      = CZ.city,
		    CZ.idx       = CZ.idx,
                    CZ.label     = paste(CZ.idx,CZ.label,sep=""),
                    moist.regiem = CZ.moist,
                    weather.file = WZ.file,stringsAsFactors = FALSE)
row.names(df.CZ) <- df.CZ[,"CZ.city"]                   
cat(paste("2: define some arrays.\n",sep=""))
cat(paste("2: define some arrays.\n",sep=""),file=FL.LOG,append=TRUE)

# varying parm variable
parm.main.varying <- c("r_ceiling",                   "r_wall",                              "r_floor",                 "u_window","shgc_window","f_inc_hw","ach50","r_bsmtwall",                "r_slab")			# June 26, 2015: changes to reflect we want separate parm variables for the insulation quality
parm.main.varying <- c("r_ceiling","ins_qual_ceiling","r_wall","ins_qual_wall","r_sheathing","r_floor","ins_qual_floor","u_window","shgc_window","f_inc_hw","ach50","r_bsmtwall","ins_qual_bsmtwall","r_slab","leakage_ratio")	# June 26, 2015: changes to reflect we want separate parm variables for the insulation quality
no.main.varying   <- length(parm.main.varying)

parm.duct.varying <- c("leakage_ratio","r_returnduct")	# June 5, 2015: for duct leakage simulation
no.duct.varying   <- length(parm.duct.varying)
cat(paste("3: the varying parm variables.\n",sep=""))
cat(paste("3: the varying parm variables.\n",sep=""),file=FL.LOG,append=TRUE)

# heating systems
htgSys      <- c("gasfurnace","electricfurnace","heatpump","oilfurnace")
foundations <- c("heatedbsmt","unheatedbsmt","crawlspace","slab")
cat(paste("4: the heat system and foundation types.\n",sep=""))
cat(paste("4: the heat system and foundation types.\n",sep=""),file=FL.LOG,append=TRUE)


no.htgSys     <- length(htgSys)
no.foundation <- length(foundations)
cat(paste("5: defined ",no.htgSys," htgSys (",paste(htgSys,collapse=","),") and ",no.foundation," foundations (",paste(foundations,collapse=","),").\n",sep=""))
cat(paste("5: defined ",no.htgSys," htgSys (",paste(htgSys,collapse=","),") and ",no.foundation," foundations (",paste(foundations,collapse=","),").\n",sep=""),file=FL.LOG,append=TRUE)


# IECC requirement for the nine code items
IECC_2009 <- data.frame(CZ2 = c(30, 13, 13, 0.65, 0.3, 0.5, 7,  0,  0),
			CZ3 = c(30, 13, 19, 0.50, 0.3, 0.5, 7, 13,  0),
			CZ4 = c(38, 13, 19, 0.35, 0.4, 0.5, 7, 13, 10),
			CZ5 = c(38, 21, 30, 0.35, 0.4, 0.5, 7, 13, 10),	# (1) even the code requirement is 20 for CZ 5 & 6, we use 21.  (2) the NR shgc for CZ 4|5|6, we use 0.4 ad default.  (3) for slab thickness, the default is 2 in.  Even they are 4 in for CZ6, we use 2 in.  Vrushali confirmed.
			CZ6 = c(49, 21, 30, 0.35, 0.4, 0.5, 7, 19, 10))
row.names(IECC_2009) <- c("r_ceiling","r_wall","r_floor","u_window","shgc_window","f_inc_hw","ach50","r_bsmtwall","r_slab") 
cat(paste("6A: specified the IECC 2009 reuirement for the nine code items.\n",sep=""))
cat(paste("6A: specified the IECC 2009 reuirement for the nine code items.\n",sep=""),file=FL.LOG,append=TRUE)

### myIECC2009.tmp <- IECC_2009
### row.names(myIECC2009.tmp) <- c("CeilingR","FrameWallR","FloorR","WindowU","WindowSHGC","F_INC_HW","ACH50","BsmtWallR","SlabEdgeR") 
### myIECC2009.tmp[,"KeyCode"] <- row.names(myIECC2009.tmp)
### id.vars <- "KeyCode"
### measure.vars <- c("CZ2","CZ3","CZ4","CZ5","CZ6")
### myIECC2009 <- melt(myIECC2009.tmp,id.vars = id.vars,measure.vars = measure.vars,variable.name = "ClimateZone",value.name = "CodeCompliance")
### cat(paste("6B. add IECC2009 code requirement to [myIECC2009].\n",sep=""))
### cat(paste("6B. add IECC2009 code requirement to [myIECC2009].\n",sep=""),file=FL.LOG,append=TRUE)




# April 28, 2015: Table R402.1.1 at page R-29 of Chapter 4 of IECC2012 (IECC2015 does not change the prescriptive values)
#                 R4402.4.1.2 Testing: ACH at 50 pascl are 5 for CZ1 & 2 and 3 for CZ 3 to 8.
#                 R404.1. Lighting: 75% high efficiency lighting
#                 Note: CZ4C is the same as CZ5, since we do not have CZ4C in this FOA study, we did not make such distinction.
#                 For Frame Wall: R 20 still use 21 for simulation purpose.
IECC_2015 <- data.frame(CZ2 = c(38, 13, 13, 0.40, 0.25, 75, 5,  0,  0),
			CZ3 = c(38, 21, 19, 0.35, 0.25, 75, 3, 13,  0),
			CZ4 = c(49, 21, 19, 0.35, 0.40, 75, 3, 13, 10),
			CZ5 = c(49, 21, 30, 0.32, 0.40, 75, 3, 19, 10),	# (1) even the code requirement is 20 for CZ 5 & 6, we use 21.  (2) the NR shgc for CZ 4|5|6, we use 0.4 ad default.  (3) for slab thickness, the default is 2 in.  Even they are 4 in for CZ6, we use 2 in.  Vrushali confirmed.
			CZ6 = c(49, 21, 30, 0.32, 0.40, 75, 3, 19, 10))
row.names(IECC_2015) <- c("r_ceiling","r_wall","r_floor","u_window","shgc_window","f_inc_hw","ach50","r_bsmtwall","r_slab") 
cat(paste("7A: specified the IECC 2015 reuirement for the nine code items.\n",sep=""))
cat(paste("7A: specified the IECC 2015 reuirement for the nine code items.\n",sep=""),file=FL.LOG,append=TRUE)

### myIECC2015.tmp <- IECC_2015
### row.names(myIECC2015.tmp) <- c("CeilingR","FrameWallR","FloorR","WindowU","WindowSHGC","F_INC_HW","ACH50","BsmtWallR","SlabEdgeR") 
### myIECC2015.tmp[,"KeyCode"] <- row.names(myIECC2015.tmp)
### id.vars <- "KeyCode"
### measure.vars <- c("CZ2","CZ3","CZ4","CZ5","CZ6")
### myIECC2015 <- melt(myIECC2015.tmp,id.vars = id.vars,measure.vars = measure.vars,variable.name = "ClimateZone",value.name = "CodeCompliance")
### cat(paste("7B. add IECC2015 code requirement to [myIECC2015].\n",sep=""))
### cat(paste("7B. add IECC2015 code requirement to [myIECC2015].\n",sep=""),file=FL.LOG,append=TRUE)







      list.keyItem.original <- c("FI1",     "FI17", "FI4",          "FI4b",        "FI6",     "FO1",      "FO4a",             "FO4b",           "FO7a",              "FO7b",            "FR10a",            "FR10b",          "FR2",    "FR3",       "IN1a",          "IN1b",        "IN3a",              "IN3b")
      name.keyItem.original <- c("CeilingR","ACH50","DuctTightness","DuctLeageExt","F_INC_HW","SlabEdgeR","BsmtWallR(Cavity)","BsmtWallR(Cont)","CrawlWallR(Cavity)","CrawlWallR(Cont)","MassWallR(Cavity)","MassWallR(Cont)","WindowU","WindowSHGC","FloorR(Cavity)","FloorR(Cont)","FrameWallR(Cavity)","FrameWallR(Cont)")
           keyItem.original <- data.frame(itemCode = list.keyItem.original,
      				          itemName = name.keyItem.original)
cat(paste("8A: specified the original key code items in the database.\n",sep=""))
cat(paste("8A: specified the original key code items in the database.\n",sep=""),file=FL.LOG,append=TRUE)

      list.keyItem.4simu <- c("FI1",     "FI17", "FI4",          "FI4b",        "FI6",     "FO1",      "FO4a",             "FO4b",           "FO7a",              "FO7b",            "FR10a",            "FR10b",          "FR2",    "FR3",       "IN1a",          "IN1b",        "IN3a",              "IN3b")
      name.keyItem.4simu <- c("CeilingR","ACH50","DuctTightness","DuctLeageExt","F_INC_HW","SlabEdgeR","BsmtWallR(Cavity)","BsmtWallR(Cont)","CrawlWallR(Cavity)","CrawlWallR(Cont)","MassWallR(Cavity)","MassWallR(Cont)","WindowU","WindowSHGC","FloorR(Cavity)","FloorR(Cont)","FrameWallR(Cavity)","FrameWallR(Cont)")
           keyItem.4simu <- data.frame(itemCode = list.keyItem.4simu,
      				       itemName = name.keyItem.4simu)
cat(paste("8B: specified the nine key code items to be used in the simulation.\n",sep=""))
cat(paste("8B: specified the nine key code items to be used in the simulation.\n",sep=""),file=FL.LOG,append=TRUE)


        list.keyItem  <- c("FI1",     "FI17", "FI4",          "FI4b",        "FI6",         "FO1",      "FO4a",             "FO4b",           "FO7a",              "FO7b",            "FR10a",            "FR10b",          "FR2",    "FR3",       "IN1a",          "IN1b",        "IN3a",              "IN3b",            "IQ1",           "IQ2",            "IQ3",           "IQ4",               "MIQ1",               "CSIQ1",                   "KW5",         "KW1",              "KW2",            "BG17",                 "EQ1",                      "EQ2",               "CompWallR","CompCeilingR","CompBsmtWallR","CompFloorR","CompCrawlWallR")
        name.keyItem  <- c("CeilingR","ACH50","DuctTightness","DuctLeageExt","HighEffLamps","SlabEdgeR","BsmtWallR(Cavity)","BsmtWallR(Cont)","CrawlWallR(Cavity)","CrawlWallR(Cont)","MassWallR(Cavity)","MassWallR(Cont)","WindowU","WindowSHGC","FloorR(Cavity)","FloorR(Cont)","FrameWallR(Cavity)","FrameWallR(Cont)","IQ(RoofCavity)","IQ(FloorCavity)","IQ(WallCavity)","IQ(BsmtWallCavity)","IQ(MassWallCavity)", "IQ(CrawlSpaceWallCavity)","IQ(KneeWall)","KneeWallR(Cavity)","KneeWallR(Cont)","PredominantFoundation","PredominantHeatingSource", "heatingSystemType", "CompWallR","CompCeilingR","CompBsmtWallR","CompFloorR","CompCrawlWallR")
  names(name.keyItem) <- list.keyItem


source("multipleplot.R")


# ---------------------------------------------------------------------------------------------------------
# shares
# ---------------------------------------------------------------------------------------------------------
# share of CZ in states: Sep 8, 2014 (Monday): Conference call changed the methodogy.  To meet deadline, 4 instead 6 CZ, 3 instead of 4 htgsys, 3 instead of foundation are included int he simulations
# May 27, 2015: still use this CZ share in the states
array.share.CZ.state <- data.frame(AL = c(0.181,0.819,0.000,0.000),
                                   AR = c(0.000,0.754,0.246,0.000),
                                   GA = c(0.197,0.726,0.077,0.000),
                                   KY = c(0.000,0.000,1.000,0.000),
                                   MD = c(0.000,0.000,1.000,0.000),
                                   NC = c(0.000,0.515,0.485,0.000),
                                   PA = c(0.000,0.000,0.234,0.766),
                                   TX = c(1.000,0.000,0.000,0.000))                      
row.names(array.share.CZ.state) <- c("Houston","Memphis","Baltimore","Chicago")
cat(paste("9A: share of CZ in the state.\n",sep=""))
cat(paste("9A: share of CZ in the state.\n",sep=""),file=FL.LOG,append=TRUE)



# share of heating systems in states
# May 27, 2015: this share will be replaced by the actual htgsys fractions in the observations
array.share.htgSys.state <- data.frame(AL = c(0.689,0.289,0.000,0.021),
                                       AR = c(0.375,0.481,0.000,0.145),
                                       GA = c(0.789,0.190,0.001,0.020),
                                       KY = c(0.689,0.289,0.000,0.021),
                                       MD = c(0.789,0.190,0.001,0.020),
                                       NC = c(0.789,0.190,0.001,0.020),
                                       PA = c(0.245,0.692,0.046,0.017),
                                       TX = c(0.375,0.481,0.000,0.145))
row.names(array.share.htgSys.state) <- c("heatpump","gasfurnace","oilfurnace","electricfurnace")
cat(paste("9B: share of htg system in the state.\n",sep=""))
cat(paste("9B: share of htg system in the state.\n",sep=""),file=FL.LOG,append=TRUE)

   
# share of foundation in states
# May 27, 2015: this share will be replaced by the actual foundation fractions in the observations
array.share.found.state <- data.frame(AL = c(0.441,0.086,0.106,0.367),
                                      AR = c(0.669,0.006,0.029,0.297),
                                      GA = c(0.571,0.066,0.097,0.267),
                                      KY = c(0.441,0.086,0.106,0.367),
                                      MD = c(0.280,0.307,0.183,0.230),
                                      NC = c(0.387,0.023,0.041,0.549),
                                      PA = c(0.289,0.246,0.328,0.137),
                                      TX = c(0.796,0.003,0.004,0.198))
row.names(array.share.found.state) <- c( "slab","heatedbsmt", "unheatedbsmt","crawlspace")
cat(paste("9C: share of foundations in the state.\n",sep=""))
cat(paste("9C: share of foundations in the state.\n",sep=""),file=FL.LOG,append=TRUE)

# normalize the share because they do not add up to 1
array.share.CZ.state     <- sweep(array.share.CZ.state,2,colSums(array.share.CZ.state),FUN="/")
array.share.htgSys.state <- sweep(array.share.htgSys.state,2,colSums(array.share.htgSys.state),FUN="/")
array.share.found.state  <- sweep(array.share.found.state,2,colSums(array.share.found.state),FUN="/")
cat(paste("9D: normalize to 1 to the shares of CZ, htgSys and foundations in the state.\n",sep=""))
cat(paste("9D: normalize to 1 to the shares of CZ, htgSys and foundations in the state.\n",sep=""),file=FL.LOG,append=TRUE)




# code items to be varying
code.items    <- c("ceiling","wall","floor","u_window","shgc_window","f_inc_hw","ach50","bsmtwall","slab")	
no.code.items <- length(code.items)
cat(paste("10: specified ",no.code.items," code items which are (",paste(code.items,collapse=","),").\n",sep=""))
cat(paste("10: specified ",no.code.items," code items which are (",paste(code.items,collapse=","),").\n",sep=""),file=FL.LOG,append=TRUE)

# field of parm csv file
field.to.parm.main <- c("ID","weatherfile","location","tag","doe_climate_zone","doe_moisture_regime","prototype","cfa_total","aspect_ratio","n_units","n_stories_per_unit","code","heating_fuel","system","wfr","fndn_type","afn_control","vent_sch","runslab","runbsmt","dhw_type",parm.main.varying)
field.to.parm.duct <- c("ID","weatherfile","location","tag","doe_climate_zone","doe_moisture_regime","prototype","cfa_total","aspect_ratio","n_units","n_stories_per_unit","code","heating_fuel","system","wfr","fndn_type","afn_control","vent_sch","runslab","runbsmt","dhw_type",parm.duct.varying,"identifier")
cat(paste("11: specified the headline of the parm csv file.\n",sep=""))
cat(paste("11: specified the headline of the parm csv file.\n",sep=""),file=FL.LOG,append=TRUE)

# default values for some common parm variable : taken from parm csv file of single family house undetr IECC 2015 determination folder: \\poncho\resstd\determination\IECC2015\quantitative\analysis\2015IECC
default.n_stories_per_unit         <- 2
default.aspect_ratio               <- 1.33
default.ventilation                <- "yes"
default.n_units                    <- 1
default.afn_control.main           <- "MultizoneWithoutDistribution"
default.afn_control.duct           <- "MultizoneWithDistribution"
default.supply_leak_ratio_of_total <- 0.5
default.vent_sch                   <- "always_avail"
default.semi_conditioned           <- "no"
default.dhw_type                   <- ""
default.wfr                        <- 0.15
default.dhw_type		   <- "storage"
cat(paste("12: default value for some parm variables.\n",sep=""))
cat(paste("12: default value for some parm variables.\n",sep=""),file=FL.LOG,append=TRUE)


cat(paste("13: start loopping through states.....................\n",sep=""))
cat(paste("13: start loopping through states.....................\n",sep=""),file=FL.LOG,append=TRUE)
for (this.state in array.states[1])
{
	# define the actual sample CZ in each state: this was taken from the sample plan but needs to be verified by the data retrieved from the database!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	this.code.state        <- "IECC_2009"
	this.code.state.string <- "IECC2009"
	maximum_supply_cfm     <- 1350				# maximum supply air is 1200 cfm at IECC 2012/2015 and 1350 for IECC2009 and 1500 cfm for IECC2006
	if       (this.state == "AL")
	{
		array.CZ  <- c("Houston","Memphis")
	  names(array.CZ) <- c("2A",     "3A")
	}else if (this.state == "AR")
	{
		array.CZ  <- c("Memphis","Baltimore")
	  names(array.CZ) <- c("3A",     "4A")		
	}else if (this.state == "GA")
	{
		array.CZ  <- c("Memphis")
	  names(array.CZ) <- c("3A")		
	}else if (this.state == "KY")
	{
		array.CZ  <- c("Baltimore")
	  names(array.CZ) <- c("4A")		
	}else if (this.state == "MD")
	{
		array.CZ  <- c("Baltimore")
	  names(array.CZ) <- c("4A")	

		this.code.state        <- "IECC_2015"
		this.code.state.string <- "IECC2015"
		maximum_supply_cfm     <- 1200			# maximum supply air is 1200 cfm at IECC 2012/2015 and 1350 for IECC2009 and 1500 cfm for IECC2006		
	}else if (this.state == "NC")
	{
		array.CZ  <- c("Memphis","Baltimore")
	  names(array.CZ) <- c("3A",     "4A")		
	}else if (this.state == "PA")
	{
		array.CZ  <- c("Baltimore","Chiacago")
	  names(array.CZ) <- c("4A",       "5A")		
	}else if (this.state == "TX")
	{
		array.CZ  <- c("Houston")
	  names(array.CZ) <- c("2A")		
	}
	cat(paste("21: [",this.state,"]: specified the atucally sampled CZ in the state.\n",sep=""))
	cat(paste("21: [",this.state,"]: specified the atucally sampled CZ in the state.\n",sep=""),file=FL.LOG,append=TRUE)
	


	# define sub-folder for each state
	Path.Out.State       <- paste(Path.OUT, this.state,sep="/")
	Path.main.Simu.State <- paste(Path.Simu,this.state,sep="/")
	Path.duct.Simu.State <- paste(Path.main.Simu.State,"duct.leakage",sep="/")
	if (!file.exists(Path.Out.State)) {print(paste("NOT existing:",Path.Out.State)); dir.create(Path.Out.State, showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.main.Simu.State)){print(paste("NOT existing:",Path.main.Simu.State));dir.create(Path.main.Simu.State,showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.duct.Simu.State)){print(paste("NOT existing:",Path.duct.Simu.State));dir.create(Path.duct.Simu.State,showWarnings=TRUE,recursive=TRUE)}
	cat(paste("22: [",this.state,"]: specified an output and a simulation folder for the state.\n",sep=""))
	cat(paste("22: [",this.state,"]: specified an output and a simulation folder for the state.\n",sep=""),file=FL.LOG,append=TRUE)
	
	# define main simulation subfolders
	Path.input.nobackup   <- paste(Path.main.Simu.State,"input.nobackup",  sep="/")
	Path.output.nobackup  <- paste(Path.main.Simu.State,"output.nobackup", sep="/")
	Path.scrap.nobackup   <- paste(Path.main.Simu.State,"scrap.nobackup",  sep="/")
	Path.summary.nobackup <- paste(Path.main.Simu.State,"summary.nobackup",sep="/")
	if (!file.exists(Path.input.nobackup))  {print(paste("NOT existing:",Path.input.nobackup));  dir.create(Path.input.nobackup,showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.output.nobackup)) {print(paste("NOT existing:",Path.output.nobackup)); dir.create(Path.output.nobackup,showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.scrap.nobackup))  {print(paste("NOT existing:",Path.scrap.nobackup));  dir.create(Path.scrap.nobackup,showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.summary.nobackup)){print(paste("NOT existing:",Path.summary.nobackup));dir.create(Path.summary.nobackup,showWarnings=TRUE,recursive=TRUE)}
	
	# define duct simulation subfolders
	Path.input.nobackup   <- paste(Path.duct.Simu.State,"input.nobackup",  sep="/")
	Path.output.nobackup  <- paste(Path.duct.Simu.State,"output.nobackup", sep="/")
	Path.scrap.nobackup   <- paste(Path.duct.Simu.State,"scrap.nobackup",  sep="/")
	Path.summary.nobackup <- paste(Path.duct.Simu.State,"summary.nobackup",sep="/")
	if (!file.exists(Path.input.nobackup))  {print(paste("NOT existing:",Path.input.nobackup));  dir.create(Path.input.nobackup,showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.output.nobackup)) {print(paste("NOT existing:",Path.output.nobackup)); dir.create(Path.output.nobackup,showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.scrap.nobackup))  {print(paste("NOT existing:",Path.scrap.nobackup));  dir.create(Path.scrap.nobackup,showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.summary.nobackup)){print(paste("NOT existing:",Path.summary.nobackup));dir.create(Path.summary.nobackup,showWarnings=TRUE,recursive=TRUE)}

	# define output file names
	FL.duct.parm.2keep <- paste(Path.Out.State,      "phase1_duct_parm.csv",       sep="/")
	FL.duct.parm.2sim  <- paste(Path.duct.Simu.State,"phase1_duct_parm.csv",       sep="/")
	FL.duct.parm.4chk  <- paste(Path.Out.State,      "phase1_duct_parm_4check.csv",sep="/")
	
	FL.main.parm.2keep <- paste(Path.Out.State,      "phase1_main_parm.csv",       sep="/")
	FL.main.parm.2sim  <- paste(Path.main.Simu.State,"phase1_main_parm.csv",       sep="/")
	FL.main.parm.4chk  <- paste(Path.Out.State,      "phase1_main_parm_4check.csv",sep="/")
	
	FL.pdf             <- paste(Path.Out.State,      "phase1_parm.pdf",            sep="/")
	FL.chk             <- paste(Path.Out.State,      "phase1_dbase_check.csv",     sep="/")
	FL.for.sample.csv  <- paste(Path.Out.State,       paste(this.state,"_",timeStamp.string,"_4sample.csv",sep=""),sep="/")
	FL.for.sample.rds  <- paste(Path.Out.State,       paste(this.state,"_",timeStamp.string,"_4sample.rds",sep=""),sep="/")
	FL.sampled.csv     <- paste(Path.Out.State,       paste(this.state,"_",timeStamp.string,"_sampled.csv",sep=""),sep="/")	
	FL.dbase.csv       <- paste(Path.Out.State,       paste(this.state,"_",timeStamp.string,"_dbase.csv",  sep=""),sep="/")
	FL.dbase.rds       <- paste(Path.Out.State,       paste(this.state,"_",timeStamp.string,"_dbase.rds",  sep=""),sep="/")
	FL.wgt.obj         <- paste(Path.Out.State,       paste(this.state,"_",timeStamp.string,"_wgt.RData",  sep=""),sep="/")
	if (file.exists(FL.duct.parm.2keep)){print(paste(FL.duct.parm.2keep,"exist.Delete it!"));file.remove(FL.duct.parm.2keep)}	
	if (file.exists(FL.duct.parm.2sim)) {print(paste(FL.duct.parm.2sim, "exist.Delete it!"));file.remove(FL.duct.parm.2sim)}	
	if (file.exists(FL.duct.parm.4chk)) {print(paste(FL.duct.parm.4chk, "exist.Delete it!"));file.remove(FL.duct.parm.4chk)}
	
	if (file.exists(FL.main.parm.2keep)){print(paste(FL.main.parm.2keep,"exist.Delete it!"));file.remove(FL.main.parm.2keep)}	
	if (file.exists(FL.main.parm.2sim)) {print(paste(FL.main.parm.2sim, "exist.Delete it!"));file.remove(FL.main.parm.2sim)}	
	if (file.exists(FL.main.parm.4chk)) {print(paste(FL.main.parm.4chk, "exist.Delete it!"));file.remove(FL.main.parm.4chk)}
	
	if (file.exists(FL.pdf))            {print(paste(FL.pdf,            "exist.Delete it!"));file.remove(FL.pdf)}	
	if (file.exists(FL.chk))            {print(paste(FL.chk,            "exist.Delete it!"));file.remove(FL.chk)}	
	if (file.exists(FL.for.sample.csv)) {print(paste(FL.for.sample.csv, "exist.Delete it!"));file.remove(FL.for.sample.csv)}	
	if (file.exists(FL.for.sample.rds)) {print(paste(FL.for.sample.rds, "exist.Delete it!"));file.remove(FL.for.sample.rds)}
	if (file.exists(FL.sampled.csv))    {print(paste(FL.sampled.csv,    "exist.Delete it!"));file.remove(FL.sampled.csv)}	
	if (file.exists(FL.dbase.csv))      {print(paste(FL.dbase.csv,      "exist.Delete it!"));file.remove(FL.dbase.csv)}	
	if (file.exists(FL.dbase.rds))      {print(paste(FL.dbase.rds,      "exist.Delete it!"));file.remove(FL.dbase.rds)}	
	if (file.exists(FL.wgt.obj))        {print(paste(FL.wgt.obj,        "exist.Delete it!"));file.remove(FL.wgt.obj)}	
	cat(paste("23: [",this.state,"]: define path and file names for the state.\n",sep=""))
	cat(paste("23: [",this.state,"]: define path and file names for the state.\n",sep=""),file=FL.LOG,append=TRUE)

	# open the device to plot
	pdf(file=FL.pdf,paper="special",width=17,height=11,bg="transparent")
	cat(paste("24: [",this.state,"]: open the pdf file for output plots.\n",sep=""))
	cat(paste("24: [",this.state,"]: open the pdf file for output plots.\n",sep=""),file=FL.LOG,append=TRUE)

	
	# put information into the check file
	cat(paste("24B: [",this.state,"]: .......................\n",sep=""))
	cat(paste("24B: [",this.state,"]: .......................\n",sep=""),file=FL.chk,append=TRUE)
	cat(paste("24C: [",this.state,"]: consists of samples from ",length(array.CZ)," CZ which are ",paste(array.CZ,sep=",",collapse=" and "),"\n",sep=""))
	cat(paste("24C: [",this.state,"]: consists of samples from ",length(array.CZ)," CZ which are ",paste(array.CZ,sep=",",collapse=" and "),"\n",sep=""),file=FL.chk,append=TRUE)




	# load the observation of this state
	# Will use [FL.KeyItem.Long.RDS]: there is a "KeyCode" field which are the none code item names to be used in the simulation.
	#                                        the "KeyCodename" field consists of the key code name used in the database
	FL.KeyItem.Wide.RDS       <- paste(Path.Data,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Wide.RDS",        sep=""),sep="/")
	FL.KeyItem.Long.RDS       <- paste(Path.Data,paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Long.RDS",        sep=""),sep="/")
	FL.found.htgSys.share.OBJ <- paste(Path.Data,paste(paste("State",this.state,timeStamp.string,sep="_"),"_found.htgSys.share.Rdata",sep=""),sep="/")
	cat(paste("25: [",this.state,"]: specify two RDS files for outputing.\n",sep=""))
	cat(paste("25: [",this.state,"]: specify two RDS files for outputing.\n",sep=""),file=FL.LOG,append=TRUE)



	# only if we have such a file...........
	if (file.exists(FL.KeyItem.Long.RDS))
	{
		# delete existing data frames which will be loaded from [FL.found.htgSys.share.OBJ]
		if(exists("myTable.found.share")  && is.data.frame(get("myTable.found.share"))) {rm(myTable.found.share)}
		if(exists("myTable.fuel.share")   && is.data.frame(get("myTable.fuel.share")))  {rm(myTable.fuel.share)}
		if(exists("myTable.htgsys.share") && is.data.frame(get("myTable.htgsys.share"))){rm(myTable.htgsys.share)}
		if(exists("myTable.CZ.share")     && is.data.frame(get("myTable.CZ.share")))    {rm(myTable.CZ.share)}
		load(file=FL.found.htgSys.share.OBJ)
		names(myTable.CZ.share)      <- sub("Var1","CZ",names(myTable.CZ.share))
		myTable.CZ.share[,"share"]   <- myTable.CZ.share[,"Freq"] / sum(myTable.CZ.share[,"Freq"],na.rm=TRUE)
		myTable.CZ.share[,"no.draw"] <- round(myTable.CZ.share[,"share"] * no.2draw,digits=0)
		cat(paste("31A: [",this.state,"]: load the htgsys, found and CZ shares in the states.\n",sep=""))
		cat(paste("31A: [",this.state,"]: load the htgsys, found and CZ shares in the states.\n",sep=""),file=FL.LOG,append=TRUE)
		
		
		myTable.bldg.wide   <- readRDS(file=FL.KeyItem.Wide.RDS)
		myTable.bldg.long   <- readRDS(file=FL.KeyItem.Long.RDS)		
		cat(paste("31B: [",this.state,"]: observation of key items are loaded.\n",sep=""))
		cat(paste("31B: [",this.state,"]: observation of key items are loaded.\n",sep=""),file=FL.LOG,append=TRUE)


		# do not use the wide format data for retrieve observations for simulation because there are two columns for some key code items.  Better use for diagonosis!
		# change the field name from "itemCode" to "itemName"
		      myData.Work.wide  <- myTable.bldg.wide
		names(myData.Work.wide) <- sub("FI1","CeilingR",sub("FO4a","BsmtWallR",sub("FO4b","BsmtWallR",sub("FI17","ACH50",sub("FI4","DuctTightness",sub("FI6","F_INC_HW",sub("FO1","SlabEdgeR",sub("FR2","WindowU",sub("FR3","WindowSHGC",sub("IN1a","FloorR",sub("IN1b","FloorR",sub("IN3a","FrameWallR",sub("IN3b","FrameWallR",names(myData.Work.wide))))))))))))))
		cat(paste("32: [",this.state,"]: loaded the wide format of data.\n",sep=""))
		cat(paste("32: [",this.state,"]: loaded the wide format of data.\n",sep=""),file=FL.LOG,append=TRUE)


		# ------------------------------------------------------------------------------------------------------------------------------------------------------------
		# [myData.Work.long] is the data frame we are going to use for the parm generation
		# ------------------------------------------------------------------------------------------------------------------------------------------------------------
		myData.Work.long <- myTable.bldg.long
		cat(paste("33: [",this.state,"]: use [myData.Work.long] which is [myTable.bldg.long] for processing.\n",sep=""))
		cat(paste("33: [",this.state,"]: use [myData.Work.long] which is [myTable.bldg.long] for processing.\n",sep=""),file=FL.LOG,append=TRUE)

		# output the observation retrieved from the database
		saveRDS(myData.Work.long,file=FL.dbase.rds)
		
		cat(paste(this.state," (wide format),",sep=""),file=FL.dbase.csv,append=TRUE)
		write.table(myData.Work.wide,,file=FL.dbase.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
		cat(paste("\n",sep=""),file=FL.dbase.csv,append=TRUE)

		cat(paste(this.state," (long format),",sep=""),file=FL.dbase.csv,append=TRUE)
		write.table(myData.Work.long,,file=FL.dbase.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
		cat(paste("\n",sep=""),file=FL.dbase.csv,append=TRUE)
		cat(paste("34: [",this.state,"]: output the data reading for diagonosis purpose.\n",sep=""))
		cat(paste("34: [",this.state,"]: output the data reading for diagonosis purpose.\n",sep=""),file=FL.LOG,append=TRUE)

		# for (this.item in sort(unique(myData.Work.long[,"KeyCode"])))
		# {
		# 	myTmp <- subset(myData.Work.long,subset=(KeyCode==this.item))
		# 	
		# 	# get the number of non-NA elements
		# 	myCount <- tapply(myTmp[,"KeyCodeValue"],list(myTmp[,"CZ"]),FUN=function(x) sum(is.finite(x)))
 		# 	string.count <- paste("CZ",names(myCount),"=",myCount,sep="",collapse=" and ")
		# 
		# 	p.plot0 <- qplot(data=myTmp,KeyCodeValue,facets=CZ~.,colour=factor(KeyCodeValue),fill=factor(KeyCodeValue),geom="histogram") 
		# 	p.plot0 <- p.plot0 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") 
		# 	p.plot0 <- p.plot0 + labs(x=this.item,y="count",title=paste("Observed (",this.item,") at (",this.state,"): No of Obs: ",string.count,"\n(vertical line indicates the code requirement)",sep=""))		
		# 	p.plot0 <- p.plot0 + geom_vline(aes(xintercept=CodeCompliance), data = myTmp)
		# 	plot(p.plot0)
		# }
		

		# split the number of the samples
		share.CZ.thisState <- array.share.CZ.state[array.share.CZ.state[,this.state] > 0,this.state,drop=FALSE]	
		no.2draw.CZ        <- round(share.CZ.thisState * no.2draw,digits=0)	# May 28, 2015: note: this CZ share will be replaced by the CZ share from the field observations.
				
		City.thisState     <- row.names(no.2draw.CZ)
		CZ.thisState       <- df.CZ[match(City.thisState,df.CZ[,"CZ.city"]),"CZ.label"]
		cat(paste("35: [",this.state,"]: use [myData.Work.long] which is [myTable.bldg.long] for processing.\n",sep=""))
		cat(paste("35: [",this.state,"]: use [myData.Work.long] which is [myTable.bldg.long] for processing.\n",sep=""),file=FL.LOG,append=TRUE)




		# use the CZ label to re-name the row of [no.2draw.CZ] which uses CZ.city (not state city but national city) as row names
		row.names(no.2draw.CZ) <- df.CZ[match(row.names(no.2draw.CZ),df.CZ[,"CZ.city"]),"CZ.label"]
		cat(paste("36: [",this.state,"]: use CZ label to replace CZ city name in [no.2draw.CZ].\n",sep=""))
		cat(paste("36: [",this.state,"]: use CZ label to replace CZ city name in [no.2draw.CZ].\n",sep=""),file=FL.LOG,append=TRUE)


		# ----------------------------------------------------------------------------------------------------------------------------------------------------
		# check the number of observations in the state level (verify if the state satify the 63 observation requirement)
		# ----------------------------------------------------------------------------------------------------------------------------------------------------
		tmpArray_r_CompCeiling  <- subset(myData.Work.long,subset=(KeyCode == "CompCeilingR"  & State == this.state),select=KeyCodeValue,drop=TRUE)
		tmpArray_r_CompWall	<- subset(myData.Work.long,subset=(KeyCode == "CompWallR"     & State == this.state),select=KeyCodeValue,drop=TRUE)
		tmpArray_r_CompFloor    <- subset(myData.Work.long,subset=(KeyCode == "CompFloorR"    & State == this.state),select=KeyCodeValue,drop=TRUE)
		tmpArray_u_window       <- subset(myData.Work.long,subset=(KeyCode == "WindowU"       & State == this.state),select=KeyCodeValue,drop=TRUE)
		tmpArray_shgc_window    <- subset(myData.Work.long,subset=(KeyCode == "WindowSHGC"    & State == this.state),select=KeyCodeValue,drop=TRUE)
		tmpArray_f_inc_hw       <- subset(myData.Work.long,subset=(KeyCode == "HighEffLamps"  & State == this.state),select=KeyCodeValue,drop=TRUE)
		tmpArray_ach50	        <- subset(myData.Work.long,subset=(KeyCode == "ACH50"         & State == this.state),select=KeyCodeValue,drop=TRUE)
		tmpArray_r_CompBsmtWall <- subset(myData.Work.long,subset=(KeyCode == "CompBsmtWallR" & State == this.state),select=KeyCodeValue,drop=TRUE)
		tmpArray_r_slab	        <- subset(myData.Work.long,subset=(KeyCode == "SlabEdgeR"     & State == this.state),select=KeyCodeValue,drop=TRUE)
		tmpArray_DuctTightness  <- subset(myData.Work.long,subset=(KeyCode == "DuctTightness" & State == this.state),select=KeyCodeValue,drop=TRUE)
		cat(paste("37: (",this.state,"): the observations of the nine code items are pulled out from the database!\n",sep=""))
		cat(paste("37: (",this.state,"): the observations of the nine code items are pulled out from the database!\n",sep=""),file=FL.LOG,append=TRUE)



		# remove the NAs
		# 1. ceiling
		tmpArray_r_CompCeilingR  <- tmpArray_r_CompCeiling[!(is.na(tmpArray_r_CompCeiling))]
		tmp_flag                 <- gsub("NR","",gsub("NA","",gsub("_","",tmpArray_r_CompCeilingR)))
		tmpArray_r_CompCeilingR  <- tmpArray_r_CompCeilingR[!(is.na(tmp_flag) | (tmp_flag == ""))]

		# 2. wall
		tmpArray_r_CompWallR     <- tmpArray_r_CompWall[!(is.na(tmpArray_r_CompWall))]
		tmp_flag                 <- gsub("NR","",gsub("NA","",gsub("_","",tmpArray_r_CompWallR)))
		tmpArray_r_CompWallR     <- tmpArray_r_CompWallR[!(is.na(tmp_flag) | (tmp_flag == ""))]	 

		# 3. floor
		tmpArray_r_CompFloorR    <- tmpArray_r_CompFloor[!(is.na(tmpArray_r_CompFloor))]
		tmp_flag                 <- gsub("NR","",gsub("NA","",gsub("_","",tmpArray_r_CompFloorR)))
		tmpArray_r_CompFloorR    <- tmpArray_r_CompFloorR[!(is.na(tmp_flag) | (tmp_flag == ""))] 

		# 4. u_window
		tmpArray_u_windowR       <- tmpArray_u_window[!(is.na(tmpArray_u_window))]
		tmp_flag                 <- gsub("NR","",gsub("NA","",gsub("_","",tmpArray_u_windowR)))
		tmpArray_u_windowR       <- tmpArray_u_windowR[!(is.na(tmp_flag) | (tmp_flag == ""))]

		# 5. shgc_window
		tmpArray_shgc_windowR    <- tmpArray_shgc_window[!(is.na(tmpArray_shgc_window))]
		tmp_flag                 <- gsub("NR","",gsub("NA","",gsub("_","",tmpArray_shgc_windowR)))
		tmpArray_shgc_windowR    <- tmpArray_shgc_windowR[!(is.na(tmp_flag) | (tmp_flag == ""))]

		# 6. f_inc_hw
		tmpArray_f_inc_hwR       <- as.numeric(tmpArray_f_inc_hw[!(is.na(tmpArray_f_inc_hw))])/100			# June 26, 2015: turn percentage to fraction
		tmp_flag                 <- gsub("NR","",gsub("NA","",gsub("_","",tmpArray_f_inc_hwR)))
		tmpArray_f_inc_hwR       <- tmpArray_f_inc_hwR[!(is.na(tmp_flag) | (tmp_flag == ""))]

		# 7. ach50
		tmpArray_ach50R          <- tmpArray_ach50[!(is.na(tmpArray_ach50))]
		tmp_flag                 <- gsub("NR","",gsub("NA","",gsub("_","",tmpArray_ach50R)))
		tmpArray_ach50R	         <- tmpArray_ach50R[!(is.na(tmp_flag) | (tmp_flag == ""))]

		# 8. bsmtwall
		tmpArray_r_CompBsmtWallR <- tmpArray_r_CompBsmtWall[!(is.na(tmpArray_r_CompBsmtWall))]
		tmp_flag                 <- gsub("NR","",gsub("NA","",gsub("_","",tmpArray_r_CompBsmtWallR)))
		tmpArray_r_CompBsmtWallR <- tmpArray_r_CompBsmtWallR[!(is.na(tmp_flag) | (tmp_flag == ""))]

		# 9. slab
		tmpArray_r_slabR         <- tmpArray_r_slab[!(is.na(tmpArray_r_slab))]
		tmp_flag                 <- gsub("NR","",gsub("NA","",gsub("_","",tmpArray_r_slabR)))
		tmpArray_r_slabR	 <- tmpArray_r_slabR[!(is.na(tmp_flag) | (tmp_flag == ""))] 

		# 10. duct tightness
		tmpArray_DuctTightnessR  <- tmpArray_DuctTightness[!(is.na(tmpArray_DuctTightness))]
		tmp_flag                 <- gsub("NR","",gsub("NA","",gsub("_","",tmpArray_DuctTightnessR)))
		tmpArray_DuctTightnessR	 <- tmpArray_DuctTightnessR[!(is.na(tmp_flag) | (tmp_flag == ""))]   
		cat(paste("38: (",this.state,"): the NA have been removed!\n",sep=""))
		cat(paste("38: (",this.state,"): the NA have been removed!\n",sep=""),file=FL.LOG,append=TRUE)

		#
		# There are multiple entries in the FI4 (DuctTightness filde, expand them
		#
		idx.new.row                               <- length(tmpArray_DuctTightnessR)
		index.multiple                            <- grep(",",tmpArray_DuctTightnessR)			# the index where FI4 has multiple entries
		no.multiple                               <- length(index.multiple)				# the number of rows with multiple entries
		for (index in index.multiple)									# the row index of the entries with multiple values
		{
			multiple.array <- unlist(strsplit(tmpArray_DuctTightnessR[index],","))
			tmpArray_DuctTightnessR[index] <- multiple.array[1]					# the original row take the first of the multiple entries
			for (tmp.idx in seq(from=2,to=length(multiple.array)))
			{	
				idx.new.row <- idx.new.row + 1							# the rest of the multiple entries is added to the end of the [tmpArray_DuctTightnessR]
				tmpArray_DuctTightnessR[idx.new.row] <- multiple.array[tmp.idx]			# replace the "KeyCodeValue" with the rest of the multplie entries
			}
		}
		cat(paste("39A: (",this.state,"): multiple extries in the FI4 (Duct Tightness filed) are expanded!\n",sep=""))
		cat(paste("39A: (",this.state,"): multiple extries in the FI4 (Duct Tightness filed) are expanded!\n",sep=""),file=FL.LOG,append=TRUE)
		
		# June 5, 2015: assign the unique values of [tmpArray_DuctTightnessR] to [unique_DuctTightnessR] to be used in the duct parm preparation
		if (length(tmpArray_DuctTightnessR)  > 0)
		{
			unique_DuctTightnessR <- sort(unique(tmpArray_DuctTightnessR)) 
			if(is.character(unique_DuctTightnessR)){unique_DuctTightnessR <- (as.numeric(unique_DuctTightnessR) * 2400 / 100) / maximum_supply_cfm}
		}else{
			if (this.state == "MD")
			{
				unique_DuctTightnessR <- 0.08
			}else{
				unique_DuctTightnessR <- 0.2
			}
		}
		cat(paste("39B: (",this.state,"): the unique value of field duct observations are put into [unique_DuctTightnessR] for duct parm preparation!\n",sep=""))
		cat(paste("39B: (",this.state,"): the unique value of field duct observations are put into [unique_DuctTightnessR] for duct parm preparation!\n",sep=""),file=FL.LOG,append=TRUE)


		cat(paste("40: (",this.state,"): ---- loopping through CZ in the states!\n",sep=""))
		cat(paste("40: (",this.state,"): ---- loopping through CZ in the states!\n",sep=""),file=FL.LOG,append=TRUE)

		no.CZ         <- 0
		no.TRB.main   <- 0
		no.PRO.main   <- 1
		idx.PRO.main  <- 0	# used to distiguish the first code item in all loops
		no.total.main <- 0
		
		# June 5, 2015: Consider the duct leakage simulation
		no.TRB.duct   <- 0
		no.PRO.duct   <- 1
		idx.PRO.duct  <- 0	# used to distiguish the first code item in all loops
		no.total.duct <- 0		
		for (this.CZ in CZ.thisState)
		{
			no.CZ           <- no.CZ + 1 
			this.CZ.idx     <- df.CZ[df.CZ[,"CZ.label"] == this.CZ,"CZ.idx"]
			this.CZ.label   <- df.CZ[df.CZ[,"CZ.label"] == this.CZ,"CZ.label"]
			this.moist      <- df.CZ[df.CZ[,"CZ.label"] == this.CZ,"moist.regiem"]

			this.CZ.city    <- myEPW.state[((myEPW.state[,"State.Abb"] == this.state) & (myEPW.state[,"CZ.revised"] == this.CZ)),"CZ.city"]
			this.weather    <- myEPW.state[((myEPW.state[,"State.Abb"] == this.state) & (myEPW.state[,"CZ.revised"] == this.CZ)),"TMY3"]

			# no.2draw.thisCZ <- no.2draw.CZ[this.CZ,this.state]
			no.2draw.thisCZ   <- myTable.CZ.share[myTable.CZ.share[,"CZ"] == this.CZ,"no.draw"]	# May 28, 2015: use the observation to derive the shares of CZ in the state
			
			# ***********************************************************************
			# June 26, 2015: dr aw the same number of samples for all CZ in the state
			# ***********************************************************************
			no.2draw.thisCZ <- no.2draw

			cat(paste("41: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): get the CZ.city, CZ.idx, CZ.label, weather file.\n",sep=""))
			cat(paste("41: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): get the CZ.city, CZ.idx, CZ.label, weather file.\n",sep=""),file=FL.LOG,append=TRUE)


			# put information into the check file
			cat(paste("42: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [",this.weather,"] at [",this.CZ.city,"] in the simulation.\n",sep=""))
			cat(paste("42: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [",this.weather,"] at [",this.CZ.city,"] in the simulation.\n",sep=""),file=FL.chk,append=TRUE)


			# IECC code requirement of the nine code items
			# IECC2012/2015 has a leakage rate 4cfm/100 sf.  The max supply cfm is 1200 cfm.  The sf footage is 2400 sf.  This leads to a leakage ratio of 0.08 <- (4 * 2400/100) / 1200 
			# similar logic to get leakage ratio of 0.2 for IECC2009 (max suppl cfm = 1350) and 0.3 for IECC2006 (max suppl cfm = 1500 csm)
			if (this.state == "MD")
			{
				IECC.r_ceiling     <- IECC_2015["r_ceiling",  paste("CZ",this.CZ.idx,sep="")]
				IECC.r_wall        <- IECC_2015["r_wall",     paste("CZ",this.CZ.idx,sep="")]
				IECC.r_floor       <- IECC_2015["r_floor",    paste("CZ",this.CZ.idx,sep="")]
				IECC.u_window      <- IECC_2015["u_window",   paste("CZ",this.CZ.idx,sep="")]
				IECC.shgc_window   <- IECC_2015["shgc_window",paste("CZ",this.CZ.idx,sep="")]
				IECC.f_inc_hw      <- IECC_2015["f_inc_hw",   paste("CZ",this.CZ.idx,sep="")]
				IECC.ach50         <- IECC_2015["ach50",      paste("CZ",this.CZ.idx,sep="")]
				IECC.r_bsmtwall    <- IECC_2015["r_bsmtwall", paste("CZ",this.CZ.idx,sep="")]
				IECC.r_slab        <- IECC_2015["r_slab",     paste("CZ",this.CZ.idx,sep="")]	
				IECC.leakage_ratio <- 0.08
				IECC.r_sheathing   <- 0
				IECC.identifier    <- "i15"
				
				this.code          <- "IECC_2015"
			}else{
				IECC.r_ceiling     <- IECC_2009["r_ceiling",  paste("CZ",this.CZ.idx,sep="")]
				IECC.r_wall        <- IECC_2009["r_wall",     paste("CZ",this.CZ.idx,sep="")]
				IECC.r_floor       <- IECC_2009["r_floor",    paste("CZ",this.CZ.idx,sep="")]
				IECC.u_window      <- IECC_2009["u_window",   paste("CZ",this.CZ.idx,sep="")]
				IECC.shgc_window   <- IECC_2009["shgc_window",paste("CZ",this.CZ.idx,sep="")]
				IECC.f_inc_hw      <- IECC_2009["f_inc_hw",   paste("CZ",this.CZ.idx,sep="")]
				IECC.ach50         <- IECC_2009["ach50",      paste("CZ",this.CZ.idx,sep="")]
				IECC.r_bsmtwall    <- IECC_2009["r_bsmtwall", paste("CZ",this.CZ.idx,sep="")]
				IECC.r_slab        <- IECC_2009["r_slab",     paste("CZ",this.CZ.idx,sep="")]
				IECC.leakage_ratio <- 0.2
				IECC.r_sheathing   <- 0
				IECC.identifier    <- "i09"
						   
				this.code          <- "IECC_2009"
			}
			cat(paste("43: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): the IECC 2009/2015 code requirements of the nine code items are extracted!\n",sep=""))
			cat(paste("43: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): the IECC 2009/2015 code requirements of the nine code items are extracted!\n",sep=""),file=FL.LOG,append=TRUE)




			cat(paste("50: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,") Baseline: loopping through htg systems!\n",sep=""))
			cat(paste("50: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,") Baseline: loopping through htg systems!\n",sep=""),file=FL.LOG,append=TRUE)
			# -----------------------------------------------------------------------------------------
			# Generate Baseline Runs 
			# -----------------------------------------------------------------------------------------
			for (this.htgSys in htgSys)
			{
				if (this.htgSys == "heatpump")
				{
					this.fuel   <- "electricity"
					this.system <- "heatpump"
				}else if (this.htgSys == "electricfurnace")
				{
					this.fuel   <- "electricity"
					this.system <- "acandfurnace"

				}else if (this.htgSys == "gasfurnace")
				{
					this.fuel   <- "naturalgas"
					this.system <- "acandfurnace"
				}else if (this.htgSys == "oilfurnace")
				{
					this.fuel   <- "oil"
					this.system <- "acandfurnace"
				}else{
					cat(paste("51: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,"): should not be here!\n",sep=""))
					cat(paste("51: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,"): should not be here!\n",sep=""),file=FL.LOG,append=TRUE)		
					die;
				}

				cat(paste("60: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): loopping through foundation!\n",sep=""))
				cat(paste("60: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): loopping through foundation!\n",sep=""),file=FL.LOG,append=TRUE)
				for (this.foundation in foundations)
				{
					cat(paste("61: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,")-(",this.foundation,"): looping through the combination of htg sys and foundation!\n",sep=""))
					cat(paste("61: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,")-(",this.foundation,"): looping through the combination of htg sys and foundation!\n",sep=""),file=FL.LOG,append=TRUE)



					# -----------------------------------------------------------------------------------
					# MAIN PARM: prepare TRB cases and put in [df.parm.TRB.main]
					# -----------------------------------------------------------------------------------
					no.TRB.main   <- no.TRB.main + 1
					no.total.main <- no.total.main + 1
					this.TRB.main <- paste(paste("TRB",no.TRB.main,sep=""),paste(this.CZ.city,this.CZ,sep="."),this.code.state.string,paste(this.htgSys,this.foundation,sep="."),"TRB",sep="_")

					df.main.tmp <- data.frame(ID                  = this.TRB.main,
							          prototype           = "singlefamily",
							          cfa_total           = 2400,
							          code                = this.code.state,
							          heating_fuel        = this.fuel,
							          location            = this.CZ.city,   
							          doe_climate_zone    = this.CZ.idx,     
							          doe_moisture_regime = this.moist,  
							          weatherfile         = this.weather,
							          tag                 = this.htgSys,  
							          system              = this.system,
							          fndn_type           = this.foundation, 
							  
							          ins_qual_ceiling    = 1,			# June 26, 2015: new parm variable for insulation quality
							          ins_qual_bsmtwall       = 1,			# June 26, 2015: new parm variable for insulation quality
							          ins_qual_floor      = 1,			# June 26, 2015: new parm variable for insulation quality
							          ins_qual_wall       = 1,			# June 26, 2015: new parm variable for insulation quality							 							 
							  
							          u_window            = IECC.u_window,
							          shgc_window         = IECC.shgc_window,
							          f_inc_hw            = IECC.f_inc_hw,
							          ach50               = IECC.ach50,							  
							          r_slab              = IECC.r_slab,
							          leakage_ratio       = IECC.leakage_ratio,	# May 27, 2015: additional variable added
							          r_ceiling           = IECC.r_ceiling,		# May 27, 2015: additional variable added
							          r_bsmtwall          = IECC.r_bsmtwall,	# May 27, 2015: additional variable added
							          r_floor             = IECC.r_floor,		# May 27, 2015: additional variable added
							          r_wall              = IECC.r_wall,		# May 27, 2015: additional variable added
							          r_sheathing         = IECC.r_sheathing	# May 27, 2015: additional variable added		
							  )			                        	# June 1, 2015: additional variable added		
							  

					if (no.TRB.main == 1){df.parm.TRB.main <- df.main.tmp}else{df.parm.TRB.main <- rbind(df.parm.TRB.main,df.main.tmp)}
					cat(paste("62: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,")-(",this.foundation,"): TRB case is prepared!\n",sep=""))
					cat(paste("62: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,")-(",this.foundation,"): TRB case is prepared!\n",sep=""),file=FL.LOG,append=TRUE)
					
					# June 05, 2015: preparae duct sealing simulation parm
					# -----------------------------------------------------------------------------------
					# DUCT PARM: prepare TRB cases and put in [df.parm.TRB.duct]
					# -----------------------------------------------------------------------------------
					# June 5, 2015: for duct leakage simulation:
					if (this.foundation == "slab")			# when foundation is "slab", we need to have "r_returnduct" and the R value is 8 for IECC2015 and 6 IECC2009
					{
						if (this.state == "MD")
						{
							IECC.r_returnduct <- "8"
							this.code         <- "IECC_2015"
						}else{
							IECC.r_returnduct <- "6"
							this.code         <- "IECC_2009"
						}
					}else{
						IECC.r_returnduct <- ""			# for foundation type other than "slab", there is no value for "r_returnduct"
					}
					cat(paste("63A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): the IECC 2009/2015 code requirements of the nine code items are extracted!\n",sep=""))
					cat(paste("63A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): the IECC 2009/2015 code requirements of the nine code items are extracted!\n",sep=""),file=FL.LOG,append=TRUE)

					# crate the duct parm					
					no.TRB.duct   <- no.TRB.duct + 1
					no.total.duct <- no.total.duct + 1
					this.TRB.duct <- paste(paste("TRB",no.TRB.duct,sep=""),paste(this.CZ.city,this.CZ,sep="."),this.code.state.string,paste(this.htgSys,this.foundation,sep="."),"TRB",sep="_")

					df.duct.tmp <- data.frame(ID          = this.TRB.duct,
							  prototype           = "singlefamily",
							  cfa_total           = 2400,
							  code                = this.code.state,
							  heating_fuel        = this.fuel,
							  location            = this.CZ.city,   
							  doe_climate_zone    = this.CZ.idx,     
							  doe_moisture_regime = this.moist,  
							  weatherfile         = this.weather,
							  tag                 = this.htgSys,  
							  system              = this.system,
							  fndn_type           = this.foundation, 
							  leakage_ratio       = IECC.leakage_ratio,	# May 27, 2015: additional variable added
							  r_returnduct        = IECC.r_returnduct,	# June 5, 2015: for duct leakage simulation
							  identifier          = IECC.identifier)	# June 5, 2015: for duct leakage simulation
							  

					if (no.TRB.duct == 1){df.parm.TRB.duct <- df.duct.tmp}else{df.parm.TRB.duct <- rbind(df.parm.TRB.duct,df.duct.tmp)}
					cat(paste("63B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,")-(",this.foundation,"): TRB case is prepared!\n",sep=""))
					cat(paste("63B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,")-(",this.foundation,"): TRB case is prepared!\n",sep=""),file=FL.LOG,append=TRUE)					
				}
			}
			cat(paste("63C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): TRB case is prepared!\n",sep=""))
			cat(paste("63C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): TRB case is prepared!\n",sep=""),file=FL.LOG,append=TRUE)



			# diagnosis of the retained data retrieve from the database
			# if there is no data retained, give a warning to be used for grab code requirement!!
			if (length(tmpArray_r_CompCeilingR)  ==0){tmpArray_r_CompCeilingR  <- IECC.r_ceiling;     warning.r_ceiling      <- TRUE}else{warning.r_ceiling     <- FALSE}
			if (length(tmpArray_r_CompWallR)     ==0){tmpArray_r_CompWallR	   <- IECC.r_wall;        warning.r_wall	 <- TRUE}else{warning.r_wall	    <- FALSE}
			if (length(tmpArray_r_CompFloorR)    ==0){tmpArray_r_CompFloorR	   <- IECC.r_floor;       warning.r_floor        <- TRUE}else{warning.r_floor	    <- FALSE}
			if (length(tmpArray_u_windowR)       ==0){tmpArray_u_windowR       <- IECC.u_window;      warning.u_window       <- TRUE}else{warning.u_window      <- FALSE}
			if (length(tmpArray_shgc_windowR)    ==0){tmpArray_shgc_windowR    <- IECC.shgc_window;   warning.shgc_window    <- TRUE}else{warning.shgc_window   <- FALSE}
			if (length(tmpArray_f_inc_hwR)       ==0){tmpArray_f_inc_hwR       <- IECC.f_inc_hw;      warning.f_inc_hw       <- TRUE}else{warning.f_inc_hw      <- FALSE}	# ;tmpArray_f_inc_hwR = as.numeric(tmpArray_f_inc_hwR) / 100}
			if (length(tmpArray_ach50R)          ==0){tmpArray_ach50R          <- IECC.ach50;         warning.ach50	         <- TRUE}else{warning.ach50	    <- FALSE}
			if (length(tmpArray_r_CompBsmtWallR) ==0){tmpArray_r_CompBsmtWallR <- IECC.r_bsmtwall;    warning.r_bsmtwall     <- TRUE}else{warning.r_bsmtwall    <- FALSE}
			if (length(tmpArray_r_slabR)         ==0){tmpArray_r_slabR	   <- IECC.r_slab;        warning.r_slab	 <- TRUE}else{warning.r_slab	    <- FALSE}  
			if (length(tmpArray_DuctTightnessR)  ==0){tmpArray_DuctTightnessR  <- IECC.leakage_ratio; warning.DuctTightness  <- TRUE}else{warning.DuctTightness <- FALSE}  
			cat(paste("71: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): flag a warning if the code item does not have a data!\n",sep=""))
			cat(paste("71: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): flag a warning if the code item does not have a data!\n",sep=""),file=FL.LOG,append=TRUE)


			#
			# May 28, 2015: AL specific decision: do not simulate the foundation (i.e., floor, bsmt, carwl space etc, use code default due to the lack of observations!!!)
			#
			if (this.state == "AL")
			{
				tmpArray_r_CompFloorR	 <- IECC.r_floor;       warning.r_floor    <- TRUE			
				tmpArray_r_CompBsmtWallR <- IECC.r_bsmtwall;    warning.r_bsmtwall <- TRUE			
				tmpArray_r_slabR	 <- IECC.r_slab;        warning.r_slab	   <- TRUE	
				cat(paste("71B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): AL: no effort on simulate the floor, slab and bsmt Rs!\n",sep=""))
				cat(paste("71B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): AL: no effort on simulate the floor, slab and bsmt Rs!\n",sep=""),file=FL.LOG,append=TRUE)				
			}


			
			# ---------------------------------------------------------------------------------
			# output some checking to the check file
			# ---------------------------------------------------------------------------------
			# 1. ceiling
			if (warning.r_ceiling)
			{
				cat(paste("01: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_ceiling] does not have observations in the database!!!\n",sep=""))
				cat(paste("01: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_ceiling] does not have observations in the database!!!\n",sep=""),file=FL.chk,append=TRUE)
			}else{
				cat(paste("01: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_ceiling] has [",length(tmpArray_r_CompCeilingR),"] data observations in the database.\n",sep=""))
				cat(paste("01: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_ceiling] has [",length(tmpArray_r_CompCeilingR),"] data observations in the database.\n",sep=""),file=FL.chk,append=TRUE)		
			}

			# 2. wall
			if (warning.r_wall)
			{
				cat(paste("02: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_wall] does not have observations in the database!!!\n",sep=""))
				cat(paste("02: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_wall] does not have observations in the database!!!\n",sep=""),file=FL.chk,append=TRUE)
			}else{
				cat(paste("02: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_wall] has [",length(tmpArray_r_CompWallR),"] data observations in the database.\n",sep=""))
				cat(paste("02: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_wall] has [",length(tmpArray_r_CompWallR),"] data observations in the database.\n",sep=""),file=FL.chk,append=TRUE)		
			}

			# 3. floor 
			if (warning.r_floor)
			{
				cat(paste("03: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_floor] does not have observations in the database!!!\n",sep=""))
				cat(paste("03: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_floor] does not have observations in the database!!!\n",sep=""),file=FL.chk,append=TRUE)
			}else{
				cat(paste("03: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_floor] has [",length(tmpArray_r_CompFloorR),"] data observations in the database.\n",sep=""))
				cat(paste("03: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_floor] has [",length(tmpArray_r_CompFloorR),"] data observations in the database.\n",sep=""),file=FL.chk,append=TRUE)		
			}

			# 4. u_window
			if (warning.u_window)
			{
				cat(paste("04: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [u_window] does not have observations in the database!!!\n",sep=""))
				cat(paste("04: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [u_window] does not have observations in the database!!!\n",sep=""),file=FL.chk,append=TRUE)
			}else{
				cat(paste("04: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [u_window] has [",length(tmpArray_u_windowR),"] data observations in the database.\n",sep=""))
				cat(paste("04: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [u_window] has [",length(tmpArray_u_windowR),"] data observations in the database.\n",sep=""),file=FL.chk,append=TRUE)		
			}

			# 5. shgc_window
			if (warning.shgc_window)
			{
				cat(paste("05: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [shgc_window] does not have observations in the database!!!\n",sep=""))
				cat(paste("05: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [shgc_window] does not have observations in the database!!!\n",sep=""),file=FL.chk,append=TRUE)
			}else{
				cat(paste("05: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [shgc_window] has [",length(tmpArray_shgc_windowR),"] data observations in the database.\n",sep=""))
				cat(paste("05: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [shgc_window] has [",length(tmpArray_shgc_windowR),"] data observations in the database.\n",sep=""),file=FL.chk,append=TRUE)		
			}

			# 6. f_inc_hw
			if (warning.f_inc_hw)
			{
				cat(paste("06: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [f_inc_hw] does not have observations in the database!!!\n",sep=""))
				cat(paste("06: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [f_inc_hw] does not have observations in the database!!!\n",sep=""),file=FL.chk,append=TRUE)
			}else{
				cat(paste("06: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [f_inc_hw] has [",length(tmpArray_f_inc_hwR),"] data observations in the database.\n",sep=""))
				cat(paste("06: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [f_inc_hw] has [",length(tmpArray_f_inc_hwR),"] data observations in the database.\n",sep=""),file=FL.chk,append=TRUE)		
			}

			# 7. ach50
			if (warning.ach50)
			{
				cat(paste("07: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [ach50] does not have observations in the database!!!\n",sep=""))
				cat(paste("07: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [ach50] does not have observations in the database!!!\n",sep=""),file=FL.chk,append=TRUE)
			}else{
				cat(paste("07: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [ach50] has [",length(tmpArray_ach50R),"] data observations in the database.\n",sep=""))
				cat(paste("07: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [ach50] has [",length(tmpArray_ach50R),"] data observations in the database.\n",sep=""),file=FL.chk,append=TRUE)		
			}

			# 8. bsmtwall
			if (warning.r_bsmtwall)
			{
				cat(paste("08: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_bsmtwall] does not have observations in the database!!!\n",sep=""))
				cat(paste("08: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_bsmtwall] does not have observations in the database!!!\n",sep=""),file=FL.chk,append=TRUE)
			}else{
				cat(paste("08: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_bsmtwall] has [",length(tmpArray_r_CompBsmtWallR),"] data observations in the database.\n",sep=""))
				cat(paste("08: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_bsmtwall] has [",length(tmpArray_r_CompBsmtWallR),"] data observations in the database.\n",sep=""),file=FL.chk,append=TRUE)		
			}

			# 9. slab
			if (warning.r_slab)
			{
				cat(paste("09: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_slab] does not have observations in the database!!!\n",sep=""))
				cat(paste("09: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_slab] does not have observations in the database!!!\n",sep=""),file=FL.chk,append=TRUE)
			}else{
				cat(paste("09: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_slab] has [",length(tmpArray_r_slabR),"] data observations in the database.\n",sep=""))
				cat(paste("09: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_slab] has [",length(tmpArray_r_slabR),"] data observations in the database.\n",sep=""),file=FL.chk,append=TRUE)		
			}
			
			# 10. duct tightness
			if (warning.DuctTightness)
			{
				cat(paste("10: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_slab] does not have observations in the database!!!\n",sep=""))
				cat(paste("10: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_slab] does not have observations in the database!!!\n",sep=""),file=FL.chk,append=TRUE)
			}else{
				cat(paste("10: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_slab] has [",length(tmpArray_DuctTightnessR),"] data observations in the database.\n",sep=""))
				cat(paste("10: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): [r_slab] has [",length(tmpArray_DuctTightnessR),"] data observations in the database.\n",sep=""),file=FL.chk,append=TRUE)		
			}			
			cat(paste("72: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): checked the number of observations of the nine code items!\n",sep=""))
			cat(paste("72: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): checked the number of observations of the nine code items!\n",sep=""),file=FL.LOG,append=TRUE)

		
			# ---------------------------------------------------------------------------------
			# write out the randowmly sampled data for this CZ of this state
			# ---------------------------------------------------------------------------------
			# 1. ceiling
			cat(paste("\n[ceiling]: ",length(tmpArray_r_CompCeilingR)," obs for sampling (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame(tmpArray_r_CompCeilingR),sep=",",file=FL.for.sample.csv,append=TRUE)
			
			# 2. wall
			cat(paste("\n[wall]: ",length(tmpArray_r_CompWallR)," obs for sampling (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame(tmpArray_r_CompWallR),sep=",",file=FL.for.sample.csv,append=TRUE)
			
			# 3. floor
			cat(paste("\n[floor]: ",length(tmpArray_r_CompFloorR)," obs for sampling (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame(tmpArray_r_CompFloorR),sep=",",file=FL.for.sample.csv,append=TRUE)
			
			# 4. u_window
			cat(paste("\n[u window]: ",length(tmpArray_u_windowR)," obs for sampling (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame(tmpArray_u_windowR),sep=",",file=FL.for.sample.csv,append=TRUE)
			
			# 5. shgc_window
			cat(paste("\n[shgc window]: ",length(tmpArray_shgc_windowR)," obs for sampling (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame(tmpArray_shgc_windowR),sep=",",file=FL.for.sample.csv,append=TRUE)
			
			# 6. f_inc_hw
			cat(paste("\n[hieff light]: ",length(tmpArray_f_inc_hwR)," obs for sampling (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame(tmpArray_f_inc_hwR),sep=",",file=FL.for.sample.csv,append=TRUE)
			
			# 7. ach50
			cat(paste("\n[ach50: ",length(tmpArray_ach50R)," obs for sampling (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame(tmpArray_ach50R),sep=",",file=FL.for.sample.csv,append=TRUE)
			
			# 8. bsmtwall
			cat(paste("\n[bsmtwall]: ",length(tmpArray_r_CompBsmtWallR)," obs for sampling (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame(tmpArray_r_CompBsmtWallR),sep=",",file=FL.for.sample.csv,append=TRUE)
			
			# 9. slab
			cat(paste("\n[slab]: ",length(tmpArray_r_slabR)," obs for sampling (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame(tmpArray_r_slabR),sep=",",file=FL.for.sample.csv,append=TRUE)
			
			# 10. duct tightness
			cat(paste("\n[leakage ratio]: ",length(tmpArray_DuctTightnessR)," obs for sampling (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame(tmpArray_DuctTightnessR),sep=",",file=FL.for.sample.csv,append=TRUE)
			write.table(data.frame("72B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): write out the ",no.2draw.thisCZ," samples drawn from the surveyed observatiosn!\n",sep=""))
			write.table(data.frame("72B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): write out the ",no.2draw.thisCZ," samples drawn from the surveyed observatiosn!\n",sep=""),file=FL.LOG,append=TRUE)


			#
			# june 26, 2015: add a RDS output for the surveyed observations for plotting
			#
			if (no.CZ == 1)		# only output this file once for each state because we draw the same data for each CZ in the state
			{
				myObs <- rbind(data.frame(value=as.character(tmpArray_r_CompCeilingR),item  = rep("CompoundCeilingR",length(tmpArray_r_CompCeilingR))),
				               data.frame(value=as.character(tmpArray_r_CompWallR),item     = rep("CompoundWallR",   length(tmpArray_r_CompWallR))),
				               data.frame(value=as.character(tmpArray_r_CompFloorR),item    = rep("CompoundFloorR",  length(tmpArray_r_CompFloorR))),
				               data.frame(value=as.character(tmpArray_u_windowR),item       = rep("u_window",        length(tmpArray_u_windowR))),
				               data.frame(value=as.character(tmpArray_shgc_windowR),item    = rep("shgc_window",     length(tmpArray_shgc_windowR))),
				               data.frame(value=as.character(tmpArray_f_inc_hwR),item       = rep("f_inc_hw",        length(tmpArray_f_inc_hwR))),
				               data.frame(value=as.character(tmpArray_ach50R),item          = rep("ach50",           length(tmpArray_ach50R))),
				               data.frame(value=as.character(tmpArray_r_CompBsmtWallR),item = rep("CompBsmtWallR",   length(tmpArray_r_CompBsmtWallR))),
				               data.frame(value=as.character(tmpArray_r_slabR),item         = rep("slabR",           length(tmpArray_r_slabR))),
				               data.frame(value=as.character(tmpArray_DuctTightnessR),item  = rep("DuctTightness",   length(tmpArray_DuctTightnessR))))
				               

				cat("observation used for random drawing,",file=FL.for.sample.csv,append=TRUE)
				write.table(myObs,file=FL.for.sample.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
				
				saveRDS(myObs,file=FL.for.sample.rds)				             
			}
			write.table(data.frame("72C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): write out the ",no.2draw.thisCZ," samples drawn from the surveyed observatiosn to files!\n",sep=""))
			write.table(data.frame("72C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): write out the ",no.2draw.thisCZ," samples drawn from the surveyed observatiosn to files!\n",sep=""),file=FL.LOG,append=TRUE)
			
			

			# --------------------------------------------------------------------------------------------------------------------------
			# May 27, 2015: if th compound item exits, the random draw is on the compound item which need to be broken down and get the components
			#               otherwise just repicate the code requirement
			#
			# randomly draw the fraction of the [no.2draw] combinations: [no.2draw.thisCZ]
			# for non-compound code items, ensure they are in numeric format
			# --------------------------------------------------------------------------------------------------------------------------
			# 1. randomly sample or assign code requirment to "sample.ceiling"
			if (warning.r_ceiling)
			{
				sample.ceiling <- rep(tmpArray_r_CompCeilingR,no.2draw.thisCZ)
			}else{
				sample.ceiling <- sample(tmpArray_r_CompCeilingR, no.2draw.thisCZ,replace = TRUE)
			}
			
			# 2. randomly sample or assign code requirment to "sample.wall"
			if (warning.r_wall)
			{			
				sample.wall <- rep(tmpArray_r_CompWallR,no.2draw.thisCZ)
			}else{
				sample.wall <- sample(tmpArray_r_CompWallR,no.2draw.thisCZ,replace = TRUE)
			}
			
			# 3. randomly sample or assign code requirment to "sample.floor"
			if (warning.r_floor)
			{			
				sample.floor <- rep(tmpArray_r_CompFloorR,no.2draw.thisCZ)
			}else{
				sample.floor <- sample(tmpArray_r_CompFloorR,no.2draw.thisCZ,replace = TRUE)
			}

			# 4. randomly sample or assign code requirment to "sample.u_window"
			if (warning.u_window)
			{			
				sample.u_window <- rep(tmpArray_u_windowR,no.2draw.thisCZ)
			}else{
				sample.u_window <- sample(tmpArray_u_windowR,no.2draw.thisCZ,replace = TRUE)
			}
			if(is.character(sample.u_window)){sample.u_window <- as.numeric(sample.u_window)}
			
			# 5. randomly sample or assign code requirment to "sample.shgc_window"
			if (warning.shgc_window)
			{			
				sample.shgc_window <- rep(tmpArray_shgc_windowR,no.2draw.thisCZ)
			}else{
				sample.shgc_window <- sample(tmpArray_shgc_windowR,no.2draw.thisCZ,replace = TRUE)
			}
			if(is.character(warning.shgc_window)){warning.shgc_window <- as.numeric(warning.shgc_window)}
			
			# 6. randomly sample or assign code requirment to "sample.f_inc_hw"
			if (warning.f_inc_hw)
			{			
				sample.f_inc_hw <- rep(tmpArray_f_inc_hwR,no.2draw.thisCZ)
			}else{
				sample.f_inc_hw <- sample(tmpArray_f_inc_hwR,no.2draw.thisCZ,replace = TRUE)	# June 1, 2015: the survey observation is the fraction of the high efficiency lump.  Use 1 - X to get the fraction of incadense lights
				sample.f_inc_hw <- 1 - sample.f_inc_hw
			}
			if(is.character(warning.f_inc_hw)){warning.f_inc_hw <- as.numeric(warning.f_inc_hw)}
			
			# 7. randomly sample or assign code requirment to "sample.ach50"
			if (warning.ach50)
			{			
				sample.ach50 <- rep(tmpArray_ach50R,no.2draw.thisCZ,no.2draw.thisCZ)
			}else{
				sample.ach50 <- sample(tmpArray_ach50R,no.2draw.thisCZ,replace = TRUE)
			}
			if(is.character(warning.ach50)){warning.ach50 <- as.numeric(warning.ach50)}
			
			# 8. randomly sample or assign code requirment to "sample.bsmtwall"
			if (warning.r_bsmtwall)
			{			
				sample.bsmtwall <- rep(tmpArray_r_CompBsmtWallR,no.2draw.thisCZ)
			}else{
				sample.bsmtwall <- sample(tmpArray_r_CompBsmtWallR,no.2draw.thisCZ,replace = TRUE)
			}
			
			# 9. randomly sample or assign code requirment to "sample.slab"
			if (warning.r_slab)
			{			
				sample.slab <- rep(tmpArray_r_slabR,no.2draw.thisCZ)
			}else{
				sample.slab <- sample(tmpArray_r_slabR,no.2draw.thisCZ,replace = TRUE)
			}
			if(is.character(sample.slab)){sample.slab <- as.numeric(sample.slab)}
			
			# 10. randomly sample or assign code requirment to "sample.leakage_ratio"
			if (warning.DuctTightness)
			{			
				sample.leakage_ratio <- rep(tmpArray_DuctTightnessR,no.2draw.thisCZ)				# [tmpArray_DuctTightnessR] consists of the code required leakage ratio 		
			}else{
				sample.leakage_rate <- sample(tmpArray_DuctTightnessR, no.2draw.thisCZ,replace = TRUE)		# [tmpArray_DuctTightnessR] consists of the observed leakage rate in cfm/100 sf 		
				sample.leakage_ratio <- (as.numeric(sample.leakage_rate) * 2400 / 100) / maximum_supply_cfm	# calculate the leakage ratio from the observed leakage rate (cfm/100 sf)
			}
			if(is.character(sample.leakage_ratio)){sample.leakage_ratio <- as.numeric(sample.leakage_ratio)}
			cat(paste("73: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): randomly drawn ",no.2draw.thisCZ," samples from the surveyed observatiosn for the nine code items!\n",sep=""))
			cat(paste("73: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): randomly drawn ",no.2draw.thisCZ," samples from the surveyed observatiosn for the nine code items!\n",sep=""),file=FL.LOG,append=TRUE)


			# ---------------------------------------------------------------------------------
			# write out the randowmly sampled data for this CZ of this state
			# ---------------------------------------------------------------------------------
			cat(paste("\n\n\nSampled Observations Before Compound Code Item processed!\n\n",sep=""),file=FL.sampled.csv,append=TRUE)
			# 1. ceiling
			cat(paste("\n[ceiling]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.ceiling,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 2. wall
			cat(paste("\n[wall]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.wall,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 3. floor
			cat(paste("\n[cfloor]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.floor,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 4. u_window
			cat(paste("\n[u window]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.u_window,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 5. shgc_window
			cat(paste("\n[shgc window]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.shgc_window,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 6. f_inc_hw
			cat(paste("\n[hieff light]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.f_inc_hw,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 7. ach50
			cat(paste("\n[ach50]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.ach50,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 8. bsmtwall
			cat(paste("\n[bsmtwall]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.bsmtwall,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 9. slab
			cat(paste("\n[slab]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.slab,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 10. duct tightness
			cat(paste("\n[leakage ratio]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.leakage_ratio,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			cat(paste("73B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): write out the ",no.2draw.thisCZ," samples drawn from the surveyed observatiosn!\n",sep=""))
			cat(paste("73B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): write out the ",no.2draw.thisCZ," samples drawn from the surveyed observatiosn!\n",sep=""),file=FL.LOG,append=TRUE)


			# ---------------------------------------------------------------------------------
			# SPLIT Compound Code Items!!!!!!!!!!!!!
			# note: for compound items, the random drawing is on the compound item which need to be split
			#       if the warning is TRUE, there is no observation for the compound item, the code requirement is replaced!
			#
			# generate [sample.bsmtwall.R] and [sample.ins_qual_bsmtwall] to be used instead of [sample.wall] which could be a compound code item!
			# ---------------------------------------------------------------------------------
			# 1. ceiling
			# generate [sample.ceiling.R] and [sample.ins_qual_ceiling] to be used instead of [sample.ceiling] which could be a compound code item!
			if(!(warning.r_ceiling))
			{
				# it is assumed that R and contR will not occur simulatenously, so the joint of both should still give the same length of array
				sample.ceiling.R  <- sub("(.*)_(.*)","\\1",sample.ceiling)
				sample.ceiling.IQ <- sub("(.*)_(.*)","\\2",sample.ceiling)
						
				no.I      <- sum(sample.ceiling.IQ=="I")									# there might be NA IQ for the  values,
				no.II     <- sum(sample.ceiling.IQ=="II")									# which will be replaced with the IQ value of the majority IQ level
				no.III    <- sum(sample.ceiling.IQ=="III")									# which coul dbe I, II or III
				# value.NA  <-  (no.I * 1 + no.II * 0.98 + no.III * 0.95) / sum(c(no.I,no.II,no.III))				# take the average IQ level
				# June 26, 2015: use the category with the most count
				max.category <- max(c(no.I,no.II,no.III),na.rm=TRUE)
				value.NA <- which(c(no.I,no.II,no.III) == max.category)
				# June 26, 2015: missing values replaced by the category of the majority observations.
				
				sample.ceiling.Mult     <- as.numeric(gsub("^1$|^I$",1,gsub("^2$|^II$",2,gsub("^3$|^III$",3,gsub("^NA$",value.NA,gsub("^NR$",value.NA,gsub("^$",value.NA,sample.ceiling.IQ)))))))	# convert the IQ to multiplier (I|1: 100%, II|2: 98%, III|3: 95%)
				sample.ins_qual_ceiling <- as.numeric(sample.ceiling.Mult)

				# the ceiling related [sample.ceiling.R], [sample.ceiling.Mult], [sample.ins_qual_ceiling]
				sample.ceiling.df <- cbind(sample.ceiling,sample.ceiling.R,sample.ceiling.IQ,sample.ceiling.Mult,sample.ins_qual_ceiling)
				
				cat(paste("74A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): derive [sample.ins_qual_ceiling] from [sample.ceiling]!\n",sep=""))
				cat(paste("74A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): derive [sample.ins_qual_ceiling] from [sample.ceiling]!\n",sep=""),file=FL.LOG,append=TRUE)	
			}else 
			{
				if (is.character(sample.ceiling)){sample.ceiling <- as.numeric(sample.ceiling)}
				if    (is.factor(sample.ceiling)){sample.ceiling <- as.numeric(as.chacater(sample.ceiling))}
				sample.ceiling.R        <- sample.ceiling
				sample.ins_qual_ceiling <- 1	# June 26, 2015
				cat(paste("74B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): ceiling is not a compound item!\n",sep=""))
				cat(paste("74B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): ceiling is not a compound item!\n",sep=""),file=FL.LOG,append=TRUE)	
				
			}
			if(is.character(sample.ceiling.R))   {sample.ceiling.R    <- as.numeric(sample.ceiling.R)}
			if(is.character(sample.ins_qual_ceiling)){sample.ins_qual_ceiling <- as.numeric(sample.ins_qual_ceiling)}
			cat(paste("74C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [sample.ceiling.R] for [ins_qual_ceiling] and use [sample.ins_qual_ceiling] for [r_ceiling]!\n",sep=""))
			cat(paste("74C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [sample.ceiling.R] for [ins_qual_ceiling] and use [sample.ins_qual_ceiling] for [r_ceiling]!\n",sep=""),file=FL.LOG,append=TRUE)	
	
			# 2. wall
			# generate [sample.framewall.R] and [sample.ins_qual_wall] and [sample.framesheathing.R] to be used instead of [sample.wall] which could be a compound code item!
			if(!(warning.r_wall))
			{
				# it is assumed that cavityR and contR will not occur simulatenously, so the joint of both should still give the same length of array
				sample.masswall.cavityR   <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\1",sample.wall)
				sample.masswall.cavityIQ  <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\2",sample.wall)
				sample.masswall.contR     <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\3",sample.wall)
				sample.framewall.cavityR  <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\4",sample.wall)
				sample.framewall.cavityIQ <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\5",sample.wall)
				sample.framewall.contR    <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\6",sample.wall)


				# assign mising for masswall IQ
				# June 26, 2015: handle possible missing value in the insulation quality
				no.I      <- sum(sample.masswall.cavityIQ=="I")									# there might be NA IQ for the  values,
				no.II     <- sum(sample.masswall.cavityIQ=="II")									# which will be replaced with the IQ value of the majority IQ level
				no.III    <- sum(sample.masswall.cavityIQ=="III")									# which coul dbe I, II or III
				# value.NA  <-  (no.I * 1 + no.II * 0.98 + no.III * 0.95) / sum(c(no.I,no.II,no.III))				# take the average IQ level
				# June 26, 2015: use the category with the most count
				max.category <- max(c(no.I,no.II,no.III),na.rm=TRUE)
				value.NA     <- which(c(no.I,no.II,no.III) == max.category)
				# June 26, 2015: missing values replaced by the category of the majority observations.


				# conversion of masswall IQ
				sample.masswall.cavityR  <- as.numeric(gsub("NR",NA,gsub("NA",NA,gsub("^$",NA,sample.masswall.cavityR))))
				sample.masswall.cavityIQ <- as.numeric(gsub("^I$|^1$",1,gsub("^II$|^2$",2,gsub("^III$|^3$",3,gsub("^NA$",value.NA,gsub("^NR$",value.NA,gsub("^$",value.NA,sample.masswall.cavityIQ)))))))
				sample.masswall.contR    <- as.numeric(gsub("NR",NA,gsub("NA",NA,gsub("^$",NA,sample.masswall.contR))))
				sample.masswall.cavityIQ[is.na(sample.masswall.cavityIQ)] <- mean(sample.masswall.cavityIQ,na.rm=TRUE)	
				
				# assign mising for framewall IQ
				# June 26, 2015: handle possible missing value in the insulation quality
				no.I      <- sum(sample.framewall.cavityIQ=="I")									# there might be NA IQ for the  values,
				no.II     <- sum(sample.framewall.cavityIQ=="II")									# which will be replaced with the IQ value of the majority IQ level
				no.III    <- sum(sample.framewall.cavityIQ=="III")									# which coul dbe I, II or III
				# value.NA  <-  (no.I * 1 + no.II * 0.98 + no.III * 0.95) / sum(c(no.I,no.II,no.III))				# take the average IQ level
				# June 26, 2015: use the category with the most count
				max.category <- max(c(no.I,no.II,no.III),na.rm=TRUE)
				value.NA     <- which(c(no.I,no.II,no.III) == max.category)
				# June 26, 2015: missing values replaced by the category of the majority observations.
				
				# conversion offramewall IQ
				sample.framewall.cavityR  <- as.numeric(gsub("NR",NA,gsub("NA",NA,gsub("^$",NA,sample.framewall.cavityR))))
				sample.framewall.cavityIQ <- as.numeric(gsub("^I$|^1$",1,gsub("^II$|^2$",2,gsub("^III$|^3$",3,gsub("^NA$",value.NA,gsub("^NR$",value.NA,gsub("^$",value.NA,sample.framewall.cavityIQ)))))))
				sample.framewall.contR    <- as.numeric(gsub("NR",NA,gsub("NA",NA,gsub("^$",NA,sample.framewall.contR))))
				sample.framewall.cavityIQ[is.na(sample.framewall.cavityIQ)] <- mean(sample.framewall.cavityIQ,na.rm=TRUE)	# replace the missing IQ with the mean IQ

			
				sample.masswall.R    <- sample.masswall.cavityR	
				sample.ins_qual_mswl <- sample.masswall.cavityIQ
				sample.massshield.R  <- sample.masswall.contR
				
				sample.framewall.R    <- sample.framewall.cavityR	
				sample.ins_qual_wall  <- sample.framewall.cavityIQ
				sample.framesheathing.R  <- sample.framewall.contR	
				sample.framesheathing.R[is.na(sample.framesheathing.R) | sample.framesheathing.R == "NA"] <- 0

				# the wall related [sample.masswall.R], [sample.masswall.Mult], [sample.ins_qual_mswl]
				sample.wall.df <- cbind(sample.masswall.cavityR,
				                        sample.masswall.cavityIQ,
				                        sample.masswall.contR,
				                        
				                        sample.framewall.cavityR,
				                        sample.framewall.cavityIQ,
				                        sample.framewall.contR,
				                        
				                        sample.masswall.R,
				                        sample.ins_qual_mswl,
				                        sample.massshield.R,
				                        
				                        sample.framewall.R,
				                        sample.ins_qual_wall,
				                        sample.framesheathing.R)
				
				cat(paste("75A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): derive [sample.ins_qual_mswl] from [sample.masswall]!\n",sep=""))
				cat(paste("75A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): derive [sample.ins_qual_mswl] from [sample.masswall]!\n",sep=""),file=FL.LOG,append=TRUE)			
			}else 
			{
				if (is.character(sample.wall)){sample.wall <- as.numeric(sample.wall)}
				if    (is.factor(sample.wall)){sample.wall <- as.numeric(as.chacater(sample.wall))}
				sample.framewall.R     <- sample.wall
				sample.ins_qual_wall   <- 1	# June 26, 2015
				sample.framesheathing.Reff <- rep(0,length(sample.wall))							
				cat(paste("75B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): wall is not a compound item!\n",sep=""))
				cat(paste("75B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): wall is not a compound item!\n",sep=""),file=FL.LOG,append=TRUE)	
							
			}			
			if(is.character(sample.framewall.R))   {sample.framewall.R  <- as.numeric(sample.framewall.R)}
			if(is.character(sample.ins_qual_wall)){sample.ins_qual_wall <- as.numeric(sample.ins_qual_wall)}								
			if(is.character(sample.framesheathing.R)) {sample.framesheathing.R  <- as.numeric(sample.framesheathing.R)}	
			cat(paste("75C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [sample.framewall.R] for [ins_qual_wall] and use [sample.ins_qual_wall] for [r_wall] and use [sample.framesheathing.R] for [r_sheathing]!\n",sep=""))
			cat(paste("75C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [sample.framewall.R] for [ins_qual_wall] and use [sample.ins_qual_wall] for [r_wall] and use [sample.framesheathing.R] for [r_sheathing]!\n",sep=""),file=FL.LOG,append=TRUE)	
				


			#
			# June 1st, 2015: (1) Wall R has to be discrete integer otherwise, the wall thickness and conductivity are wrong. So use (a) WallR and (b) "ins_qual_wall" separately
			#                 (2) observed wall 12.6 and 14.4 for AL are replaced with 13 and 15.
			#			
			sample.framewall.R    <- gsub(12.6,13,gsub(14.4,15,sample.framewall.R))
			cat(paste("75D: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [sample.framewall.R] for [ins_qual_wall] and use [sample.ins_qual_wall] for [r_wall] and use [sample.framesheathing.R] for [r_sheathing]!\n",sep=""))
			cat(paste("75D: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [sample.framewall.R] for [ins_qual_wall] and use [sample.ins_qual_wall] for [r_wall] and use [sample.framesheathing.R] for [r_sheathing]!\n",sep=""),file=FL.LOG,append=TRUE)	
			
			
			# 3. floor
			# generate [sample.floor.R] and [sample.ins_qual_floor] to be used instead of [sample.floor] which could be a compound code item!					
			if (!(warning.r_floor))
			{
				# it is assumed that cavityR and contR will not occur simulatenously, so the joint of both should still give the same length of array
				sample.floor.cavityR  <- sub("(.*)_(.*)_(.*)","\\1",sample.floor)
				sample.floor.cavityIQ <- sub("(.*)_(.*)_(.*)","\\2",sample.floor)
				sample.floor.contR    <- sub("(.*)_(.*)_(.*)","\\3",sample.floor)
				
				# concanate the cavity and continuous values.  Accumption: they are supplement each other,  Not occur at the same time!!!
				sample.floor.R  <- sample.floor.cavityR									# initial the "sample.floor.R" array with "cavity R"
				idx.cavityNA    <- is.na(sample.floor.R) | sample.floor.R == "NA" | sample.floor.R == "^$"		# find the index of cavity NA entries
				sample.floor.R[idx.cavityNA]   <- sample.floor.contR[idx.cavityNA]					# replace those cavity NA va;lues with the continuous R
				sample.floor.R <- as.numeric(sample.floor.R)								# convert to numeric
					
				sample.floor.IQ <- sample.floor.cavityIQ								# initial the IQ with the cavity IQ
				sample.floor.IQ[idx.cavityNA ] <- 1									# for continuous value, assign IQ with 1
					
				no.I      <- sum(sample.floor.IQ=="I")									# there might be NA IQ for the cavity values,
				no.II     <- sum(sample.floor.IQ=="II")									# which will be replaced with the IQ value of the majority IQ level
				no.III    <- sum(sample.floor.IQ=="III")								# which coul dbe I, II or III
				# value.NA  <-  (no.I * 1 + no.II * 0.98 + no.III * 0.95) / sum(c(no.I,no.II,no.III))			# take the average IQ level
				
				# June 26, 2015: use the category with the most count
				max.category <- max(c(no.I,no.II,no.III),na.rm=TRUE)
				value.NA <- which(c(no.I,no.II,no.III) == max.category)
				# June 26, 2015: missing values replaced by the category of the majority observations.
				
				
				sample.floor.Mult <- as.numeric(gsub("^1$|^I$",1,gsub("^2$|^II$",2,gsub("^3$|^III$",3,gsub("^NA$",value.NA,gsub("^NR$",value.NA,gsub("^$",value.NA,sample.floor.IQ)))))))	# convert the IQ to multiplier (I|1: 100%, II|2: 98%, III|3: 95%)
				sample.ins_qual_floor <- as.numeric(sample.floor.Mult)

				# the floor related [sample.floor.R], [sample.floor.Mult], [sample.ins_qual_floor]
				sample.floor.df <- cbind(sample.floor,sample.floor.cavityR,sample.floor.cavityIQ,sample.floor.contR,sample.floor.R,sample.floor.IQ,sample.floor.Mult,sample.ins_qual_floor)
				
				cat(paste("76A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): derive [sample.ins_qual_floor] from [sample.floor]!\n",sep=""))
				cat(paste("76A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): derive [sample.ins_qual_floor] from [sample.floor]!\n",sep=""),file=FL.LOG,append=TRUE)	
			}else 
			{
				if (is.character(sample.floor)){sample.floor <- as.numeric(sample.floor)}
				if    (is.factor(sample.floor)){sample.floor <- as.numeric(as.chacater(sample.floor))}
				sample.floor.R        <- sample.floor
				sample.ins_qual_floor <- 1	# June 26, 2015
				cat(paste("76B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): floor is not a compound item!\n",sep=""))
				cat(paste("76B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): floor is not a compound item!\n",sep=""),file=FL.LOG,append=TRUE)	
				
			}
			if(is.character(sample.floor.R))   {sample.floor.R    <- as.numeric(sample.floor.R)}
			if(is.character(sample.ins_qual_floor)){sample.ins_qual_floor <- as.numeric(sample.ins_qual_floor)}			
			cat(paste("76C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [sample.floor.R] for [ins_qual_floor] and use [sample.ins_qual_floor] for [r_floor]!\n",sep=""))
			cat(paste("76C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [sample.floor.R] for [ins_qual_floor] and use [sample.ins_qual_floor] for [r_floor]!\n",sep=""),file=FL.LOG,append=TRUE)	

			# 8. bsmtwall
			if (!(warning.r_bsmtwall))
			{
				# it is assumed that cavityR and contR will not occur simulatenously, so the joint of both should still give the same length of array
				sample.bsmtwall.cavityR  <- sub("(.*)_(.*)_(.*)","\\1",sample.bsmtwall)
				sample.bsmtwall.cavityIQ <- sub("(.*)_(.*)_(.*)","\\2",sample.bsmtwall)
				sample.bsmtwall.contR    <- sub("(.*)_(.*)_(.*)","\\3",sample.bsmtwall)
				
				# concanate the cavity and continuous values.  Accumption: they are supplement each other,  Not occur at the same time!!!
				sample.bsmtwall.R  <- sample.bsmtwall.cavityR									# initial the "sample.bsmtwall.R" array with "cavity R"
				idx.cavityNA       <- is.na(sample.bsmtwall.R) | sample.bsmtwall.R == "NA" | sample.bsmtwall.R == "^$"		# find the index of cavity NA entries
				sample.bsmtwall.R[idx.cavityNA]   <- sample.bsmtwall.contR[idx.cavityNA]					# replace those cavity NA va;lues with the continuous R
				sample.bsmtwall.R <- as.numeric(sample.bsmtwall.R)								# convert to numeric
					
				sample.bsmtwall.IQ <- sample.bsmtwall.cavityIQ									# initial the IQ with the cavity IQ
				sample.bsmtwall.IQ[idx.cavityNA ] <- 1										# for continuous value, assign IQ with 1
					
				no.I      <- sum(sample.bsmtwall.IQ=="I")									# there might be NA IQ for the cavity values,
				no.II     <- sum(sample.bsmtwall.IQ=="II")									# which will be replaced with the IQ value of the majority IQ level
				no.III    <- sum(sample.bsmtwall.IQ=="III")									# which coul dbe I, II or III
				# value.NA  <-  (no.I * 1 + no.II * 0.98 + no.III * 0.95) / sum(c(no.I,no.II,no.III))				# take the average IQ level
				
				# June 26, 2015: use the category with the most count
				max.category <- max(c(no.I,no.II,no.III),na.rm=TRUE)
				value.NA <- which(c(no.I,no.II,no.III) == max.category)
				# June 26, 2015: missing values replaced by the category of the majority observations.
				
				
				sample.bsmtwall.Mult <- as.numeric(gsub("^1$|^I$",1,gsub("^2$|^II$",2,gsub("^3$|^III$",3,gsub("^NA$",value.NA,gsub("^NR$",value.NA,gsub("^$",value.NA,sample.bsmtwall.IQ)))))))	# convert the IQ to multiplier (I|1: 100%, II|2: 98%, III|3: 95%)
				sample.ins_qual_bsmtwall <- as.numeric(sample.bsmtwall.Mult)	# June 26, 2015: new parm variable

				# the bsmtwall related [sample.bsmtwall.R], [sample.bsmtwall.Mult], [sample.ins_qual_bsmtwall]
				sample.bsmtwall.df <- cbind(sample.bsmtwall,sample.bsmtwall.cavityR,sample.bsmtwall.cavityIQ,sample.bsmtwall.contR,sample.bsmtwall.R,sample.bsmtwall.IQ,sample.bsmtwall.Mult,sample.ins_qual_bsmtwall)
				
				cat(paste("77A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): derive [sample.ins_qual_bsmtwall] from [sample.bsmtwall]!\n",sep=""))
				cat(paste("77A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): derive [sample.ins_qual_bsmtwall] from [sample.bsmtwall]!\n",sep=""),file=FL.LOG,append=TRUE)	
			}else 
			{
				if (is.character(sample.bsmtwall)){sample.bsmtwall <- as.numeric(sample.bsmtwall)}
				if    (is.factor(sample.bsmtwall)){sample.bsmtwall <- as.numeric(as.chacater(sample.bsmtwall))}
				sample.bsmtwall.R    <- sample.bsmtwall
				sample.ins_qual_bsmtwall <- 1	# June 26, 2015
				cat(paste("77B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): bsmtwall is not a compound item!\n",sep=""))
				cat(paste("77B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): bsmtwall is not a compound item!\n",sep=""),file=FL.LOG,append=TRUE)					
			}
			if(is.character(sample.bsmtwall.R))   {sample.bsmtwall.R    <- as.numeric(sample.bsmtwall.R)}
			if(is.character(sample.ins_qual_bsmtwall)){sample.ins_qual_bsmtwall <- as.numeric(sample.ins_qual_bsmtwall)}								
			cat(paste("77C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [sample.bsmtwall.R] for [ins_qual_bsmtwall] and use [sample.ins_qual_bsmtwall] for [r_bsmtwall]!\n",sep=""))
			cat(paste("77C: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): will use [sample.bsmtwall.R] for [ins_qual_bsmtwall] and use [sample.ins_qual_bsmtwall] for [r_bsmtwall]!\n",sep=""),file=FL.LOG,append=TRUE)	
							

			# ---------------------------------------------------------------------------------
			# write out the randowmly sampled data for this CZ of this state
			# ---------------------------------------------------------------------------------
			# 1-1. ceiling
			cat(paste("\n\n\nSampled Observations After Compound Code Item processed!\n\n",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste("\n[ceiling]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.ceiling.R,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)

			# 1-2. ceiling
			cat(paste("\n[ceiling eff]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.ins_qual_ceiling,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 2-3. wall
			cat(paste("\n[wall]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.framewall.R,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)

			# 2-4. wall
			cat(paste("\n[wall] eff cavity: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.framewall.R,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)

			# 2-5. wall
			cat(paste("\n[wall] cont: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.framesheathing.R,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 3-6. floor
			cat(paste("\n[cfloor]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.floor.R,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)

			# 3-7. floor
			cat(paste("\n[cfloor] eff: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.ins_qual_floor,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			
			# 4-8. u_window
			cat(paste("\n[u window]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.u_window,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 5-9. shgc_window
			cat(paste("\n[shgc window]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.shgc_window,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 6-10. f_inc_hw
			cat(paste("\n[hieff light]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.f_inc_hw,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 7-11. ach50
			cat(paste("\n[ach50]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.ach50,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 8-12. bsmtwall
			cat(paste("\n[bsmtwall]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.bsmtwall.R,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 8-13. bsmtwall
			cat(paste("\n[bsmtwall] eff: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.ins_qual_bsmtwall,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 9-14. slab
			cat(paste("\n[slab]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.leakage_ratio,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			
			# 10-15. duct tightness
			cat(paste("\n[leakage ratio]: sampled at (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"),",sep=""),file=FL.sampled.csv,append=TRUE)
			cat(paste(sample.leakage_ratio,sep=",",collapse=","),file=FL.sampled.csv,append=TRUE)
			cat(paste("78A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): write out the ",no.2draw.thisCZ," samples derived from the randomly drawn samples!\n",sep=""))
			cat(paste("78A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): write out the ",no.2draw.thisCZ," samples derived from the randomly drawn samples!\n",sep=""),file=FL.LOG,append=TRUE)
	
			# -----------------------------------------------------------------------------------------
			# this is the list of the sampled variable and the list of variables to be used for setting parm variables
			# -----------------------------------------------------------------------------------------
			#    "array from sampling"   "array derived from sampled array"    "parm variable to be assigned to"
			# 1.  sample.ceiling       	--> sample.ceiling.R        	--> ins_qual_ceiling    1.   
			#                          	--> sample.ins_qual_ceiling     --> r_ceiling    	2.       			
			# 2.  sample.wall	   	--> sample.framewall.R      	--> ins_qual_wall	3.
			#                          	--> sample.framewall.R   	--> r_wall		4.
			#                          	--> sample.framesheathing.R 	--> r_sheathing		5.	                   			                    
			# 3.  sample.floor	   	--> sample.floor.R          	--> ins_qual_floor 	6.
			#                          	--> sample.ins_qual_floor       --> r_floor  		7.		
			# 4.  sample.u_window      	--> sample.u_window         	--> u_window		8.	
			# 5.  sample.shgc_window   	--> sample.shgc_window      	--> shgc_window		9.	
			# 6.  sample.f_inc_hw      	--> sample.f_inc_hw         	--> f_inc_hw		10.	
			# 7.  sample.ach50         	--> sample.ach50            	--> ach50		11.	
			# 8.  sample.bsmtwall      	--> sample.bsmtwall.R       	--> ins_qual_bsmtwall	12.
			#                          	--> sample.ins_qual_bsmtwall    	--> r_bsmtwall	 	13.				                 
			# 9.  sample.slab          	--> sample.slab             	--> r_slab 		14.		
			# 10. sample.leakage_ratio 	--> sample.leakage_ratio    	--> leakage_ratio 	15.
			# -----------------------------------------------------------------------------------------
			
			
			# -----------------------------------------------------------------------------------------
			# plot of the randomly drawn data
			# -----------------------------------------------------------------------------------------
			p.plot2.ceiling.R          <- qplot(data=data.frame(sample.ceiling.R       ),sample.ceiling.R       ,colour=factor(sample.ceiling.R       ),fill=factor(sample.ceiling.R       ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.ceiling.R       ",y="count",title=paste("Sampled (r_ceiling)      at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.ceiling.R       ))),sep=""))		
			p.plot2.ceiling.ins_qual   <- qplot(data=data.frame(sample.ins_qual_ceiling),sample.ins_qual_ceiling,colour=factor(sample.ins_qual_ceiling),fill=factor(sample.ins_qual_ceiling),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.ins_qual_ceiling",y="count",title=paste("Sampled (r_ceiling eff)  at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.ins_qual_ceiling))),sep=""))		
			p.plot2.framewall.R        <- qplot(data=data.frame(sample.framewall.R     ),sample.framewall.R     ,colour=factor(sample.framewall.R     ),fill=factor(sample.framewall.R     ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.framewall.R     ",y="count",title=paste("Sampled (r_wall)         at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.framewall.R     ))),sep=""))		
			p.plot2.framewall.ins_qual <- qplot(data=data.frame(sample.ins_qual_wall   ),sample.ins_qual_wall   ,colour=factor(sample.ins_qual_wall   ),fill=factor(sample.ins_qual_wall   ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.ins_qual_wall  ", y="count",title=paste("Sampled (r_wall eff)     at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.ins_qual_wall   ))),sep=""))		
			p.plot2.framesheathing.R   <- qplot(data=data.frame(sample.framesheathing.R),sample.framesheathing.R,colour=factor(sample.framesheathing.R),fill=factor(sample.framesheathing.R),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.framesheathing.R",y="count",title=paste("Sampled (r_wall sheath)  at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.framesheathing.R))),sep=""))		
			p.plot2.floor.R            <- qplot(data=data.frame(sample.floor.R         ),sample.floor.R         ,colour=factor(sample.floor.R         ),fill=factor(sample.floor.R         ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.floor.R         ",y="count",title=paste("Sampled (r_floor)        at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.floor.R         ))),sep=""))		
			p.plot2.floor.ins_qual     <- qplot(data=data.frame(sample.ins_qual_floor  ),sample.ins_qual_floor  ,colour=factor(sample.ins_qual_floor  ),fill=factor(sample.ins_qual_floor  ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.ins_qual_floo   ",y="count",title=paste("Sampled (r_floor eff)    at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.ins_qual_floor  ))),sep=""))		
			p.plot2.u_window           <- qplot(data=data.frame(sample.u_window        ),sample.u_window        ,colour=factor(sample.u_window        ),fill=factor(sample.u_window        ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.u_window        ",y="count",title=paste("Sampled (u_window)       at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.u_window        ))),sep=""))		
			p.plot2.shgc_window        <- qplot(data=data.frame(sample.shgc_window     ),sample.shgc_window     ,colour=factor(sample.shgc_window     ),fill=factor(sample.shgc_window     ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.shgc_window     ",y="count",title=paste("Sampled (shgc_window)    at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.shgc_window     ))),sep=""))		
			p.plot2.f_inc_hw           <- qplot(data=data.frame(sample.f_inc_hw        ),sample.f_inc_hw        ,colour=factor(sample.f_inc_hw        ),fill=factor(sample.f_inc_hw        ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.f_inc_hw        ",y="count",title=paste("Sampled (f_inc_hw)       at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.f_inc_hw        ))),sep=""))		
			p.plot2.ach50              <- qplot(data=data.frame(sample.ach50           ),sample.ach50           ,colour=factor(sample.ach50           ),fill=factor(sample.ach50           ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.ach50           ",y="count",title=paste("Sampled (ach50)          at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.ach50           ))),sep=""))		
			p.plot2.bsmtwall.R         <- qplot(data=data.frame(sample.bsmtwall.R      ),sample.bsmtwall.R      ,colour=factor(sample.bsmtwall.R      ),fill=factor(sample.bsmtwall.R      ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.bsmtwall.R      ",y="count",title=paste("Sampled (r_bsmtwall)     at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.bsmtwall.R      ))),sep=""))		
			p.plot2.bsmtwall.ins_qual  <- qplot(data=data.frame(sample.ins_qual_bsmtwall   ),sample.ins_qual_bsmtwall   ,colour=factor(sample.ins_qual_bsmtwall   ),fill=factor(sample.ins_qual_bsmtwall   ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.ins_qual_bsmtwall   ",y="count",title=paste("Sampled (r_bsmtwall eff) at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.ins_qual_bsmtwall   ))),sep=""))		
			p.plot2.slab               <- qplot(data=data.frame(sample.slab            ),sample.slab            ,colour=factor(sample.slab            ),fill=factor(sample.slab            ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.slab            ",y="count",title=paste("Sampled (r_slab)         at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.slab            ))),sep=""))		
			p.plot2.leakage_ratio      <- qplot(data=data.frame(sample.leakage_ratio   ),sample.leakage_ratio   ,colour=factor(sample.leakage_ratio   ),fill=factor(sample.leakage_ratio   ),geom="histogram") + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="right") + labs(x="sample.leakage_ratio   ",y="count",title=paste("Sampled (r_slab)         at (",this.state,")-(",this.CZ,"): No of Sampled: ",sum(!(is.na(sample.leakage_ratio   ))),sep=""))		
			 
		 	multiplot(p.plot2.ceiling.R       ,
		 	          p.plot2.ceiling.ins_qual)
		 	          
		 	multiplot(p.plot2.framewall.R     ,	
				  p.plot2.framewall.ins_qual  ,
				  p.plot2.framesheathing.R)
				  
		 	multiplot(p.plot2.floor.R         ,
				  p.plot2.floor.ins_qual  )
				  
		 	multiplot(p.plot2.u_window        ,
				  p.plot2.shgc_window     )
				  
		 	multiplot(p.plot2.f_inc_hw        ,
				  p.plot2.ach50           )
				  
		 	multiplot(p.plot2.bsmtwall.R      ,
				  p.plot2.bsmtwall.ins_qual   )
				  
		 	multiplot(p.plot2.slab            ,
				  p.plot2.leakage_ratio   )
			cat(paste("78B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,") Random Sampling: loopping through htg systems!\n",sep=""))
			cat(paste("78B: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,") Random Sampling: loopping through htg systems!\n",sep=""),file=FL.LOG,append=TRUE)
			# -----------------------------------------------------------------------------------------
			# Generate Baseline Runs 
			# -----------------------------------------------------------------------------------------

			for (this.htgSys in htgSys)
			{
				if (this.htgSys == "heatpump")
				{
					this.fuel   <- "electricity"
					this.system <- "heatpump"
				}else if (this.htgSys == "electricfurnace")
				{
					this.fuel   <- "electricity"
					this.system <- "acandfurnace"

				}else if (this.htgSys == "gasfurnace")
				{
					this.fuel   <- "naturalgas"
					this.system <- "acandfurnace"
				}else if (this.htgSys == "oilfurnace")
				{
					this.fuel   <- "oil"
					this.system <- "acandfurnace"
				}else{
					cat(paste("79: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,"): should not be here!\n",sep=""))
					cat(paste("79: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,"): should not be here!\n",sep=""),file=FL.LOG,append=TRUE)		
					die;
				}


				for (this.foundation in foundations)
				{
					cat(paste("80: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,")-(",this.foundation,"): looping through the combination of htg sys and foundation!\n",sep=""))
					cat(paste("80: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,")-(",this.htgSys,")-(",this.foundation,"): looping through the combination of htg sys and foundation!\n",sep=""),file=FL.LOG,append=TRUE)

					# main parm
					idx.PRO.main <- idx.PRO.main + 1

					# 
					this.PRO.main <- paste(paste("PRO",seq(from=no.PRO.main,to = (no.PRO.main + no.2draw.thisCZ-1)),sep=""),paste(this.CZ.city,this.CZ,sep="."),this.code.state.string,paste(this.htgSys,this.foundation,sep="."),paste("PRO",seq(1:no.2draw.thisCZ),sep=""),sep="_")
					no.PRO.main <- no.PRO.main + no.2draw.thisCZ

					# parm data frame
					df.main.tmp <- data.frame(ID                  = this.PRO.main,
							          prototype           = rep("singlefamily", no.2draw.thisCZ),
							          cfa_total           = rep(2400,           no.2draw.thisCZ),
							          code                = rep(this.code.state,no.2draw.thisCZ),
							          heating_fuel        = rep(this.fuel,      no.2draw.thisCZ),
							          location            = rep(this.CZ.city,   no.2draw.thisCZ),   
							          doe_climate_zone    = rep(this.CZ.idx,    no.2draw.thisCZ),   
							          doe_moisture_regime = rep(this.moist,     no.2draw.thisCZ),  
							          weatherfile         = rep(this.weather,   no.2draw.thisCZ),
							          tag                 = rep(this.htgSys,    no.2draw.thisCZ),  
							          system              = rep(this.system,    no.2draw.thisCZ), 
							          fndn_type           = rep(this.foundation,no.2draw.thisCZ), 
							          
							          ins_qual_ceiling    = sample.ins_qual_ceiling,	# June 26, 2015: new parm variable for insulation quality
							          ins_qual_bsmtwall       = sample.ins_qual_bsmtwall,		# June 26, 2015: new parm variable for insulation quality
							          ins_qual_floor      = sample.ins_qual_floor,		# June 26, 2015: new parm variable for insulation quality						          
							          ins_qual_wall       = sample.ins_qual_wall,		# June 26, 2015: new parm variable for insulation quality
							          
							          u_window            = sample.u_window,
							          shgc_window         = sample.shgc_window,
							          f_inc_hw            = sample.f_inc_hw, 
							          ach50               = sample.ach50 ,
							          
							          r_slab              = sample.slab,
							          leakage_ratio       = sample.leakage_ratio,		# May 27, 2015: additional variable added
							          r_ceiling           = sample.ceiling.R,		# May 27, 2015: additional variable added
							          r_bsmtwall          = sample.bsmtwall.R,		# May 27, 2015: additional variable added
							          r_floor             = sample.floor.R,			# May 27, 2015: additional variable added
							          r_wall              = sample.framewall.R,		# May 27, 2015: additional variable added.  June 1, 2015: for wall, the effective wall R is separated to be wall R and a degrdation factor implemented through template because the wall thickness depends on discrete wall R values.
							          r_sheathing         = sample.framesheathing.R		# May 27, 2015: additional variable added
							          )	# June 1, 2015: the wall insulation degrdation factor is used as a separate parm variable.  Note empty default to 1 in the template.
							  


					if (idx.PRO.main == 1)
					{
						df.parm.PRO.main <- df.main.tmp
					}else{
						df.parm.PRO.main <- rbind(df.parm.PRO.main,df.main.tmp)
					}

					# June 05, 2015: preparae duct sealing simulation parm
					# -----------------------------------------------------------------------------------
					# DUCT PARM: prepare TRB cases and put in [df.parm.TRB.duct]
					# -----------------------------------------------------------------------------------
					# June 5, 2015: for duct leakage simulation:
					if (this.foundation == "slab")			# when foundation is "slab", we need to have "r_returnduct" and the R value is 8 for IECC2015 and 6 IECC2009
					{
						if (this.state == "MD")
						{
							IECC.r_returnduct <- "8"
							this.code         <- "IECC_2015"
						}else{
							IECC.r_returnduct <- "6"
							this.code         <- "IECC_2009"
						}
					}else{
						IECC.r_returnduct <- ""			# for foundation type other than "slab", there is no value for "r_returnduct"
					}
					cat(paste("63A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): the IECC 2009/2015 code requirements of the nine code items are extracted!\n",sep=""))
					cat(paste("63A: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): the IECC 2009/2015 code requirements of the nine code items are extracted!\n",sep=""),file=FL.LOG,append=TRUE)


					
					# -----------------------------------------------------------------					
					# June 5, 2015: duct parm: for each unique duct leakage ratio value, we will have a model for each CZ with 16 variants
					# -----------------------------------------------------------------
					idx.PRO.duct <- idx.PRO.duct + 1

					# number of unique duct leakage values
					no.unique.duct <- length(unique_DuctTightnessR)
					
					# 
					this.PRO.duct <- paste(paste("PRO.duct",seq(from=no.PRO.duct,to = (no.PRO.duct + no.unique.duct - 1)),sep=""),paste(this.CZ.city,this.CZ,sep="."),this.code.state.string,paste(this.htgSys,this.foundation,sep="."),paste("PRO.duct",seq(1:no.unique.duct),sep=""),sep="_")
					no.PRO.duct   <- no.PRO.duct + no.unique.duct

					# parm data frame
					df.duct.tmp <- data.frame(ID                  = this.PRO.duct,
							          prototype           = rep("singlefamily", no.unique.duct),
							          cfa_total           = rep(2400,           no.unique.duct),
							          code                = rep(this.code.state,no.unique.duct),
							          heating_fuel        = rep(this.fuel,      no.unique.duct),
							          location            = rep(this.CZ.city,   no.unique.duct),   
							          doe_climate_zone    = rep(this.CZ.idx,    no.unique.duct),   
							          doe_moisture_regime = rep(this.moist,     no.unique.duct),  
							          weatherfile         = rep(this.weather,   no.unique.duct),
							          tag                 = rep(this.htgSys,    no.unique.duct),  
							          system              = rep(this.system,    no.unique.duct), 
							          fndn_type           = rep(this.foundation,no.unique.duct), 
							          leakage_ratio       = unique_DuctTightnessR,			# June 5, 2015: for duct leakage simulation
							          r_returnduct        = IECC.r_returnduct,			# June 5, 2015: for duct leakage simulation
								  identifier          = IECC.identifier)			# June 5, 2015: for duct leakage simulation

							  


					if (idx.PRO.duct == 1)
					{
						df.parm.PRO.duct <- df.duct.tmp
					}else{
						df.parm.PRO.duct <- rbind(df.parm.PRO.duct,df.duct.tmp)
					}					
				}		# end of foundation loop
			}			# end of htgSys loop
			cat(paste("81: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): PRO.duct case is prepared!\n",sep=""))
			cat(paste("81: (",this.state,")-(",this.CZ.city,")-(",this.CZ.label,"): PRO.duct case is prepared!\n",sep=""),file=FL.LOG,append=TRUE)
		}				# end of climate city loop
		cat(paste("82: (",this.state,"): completed the CZ loop!\n",sep=""))
		cat(paste("82: (",this.state,"): completed the CZ loop!\n",sep=""),file=FL.LOG,append=TRUE)
		# *****************************************************************************************
		# here is the end of the CZ loop in the state
		# *****************************************************************************************

		# -----------------------------------------------------------------------------------------
		# add the default values for some common parm variables
		# -----------------------------------------------------------------------------------------
		df.parm.TRB.main[,"n_stories_per_unit"]         <- default.n_stories_per_unit        
		df.parm.TRB.main[,"aspect_ratio"]               <- default.aspect_ratio              
		df.parm.TRB.main[,"ventilation"]                <- default.ventilation               
		df.parm.TRB.main[,"n_units"]                    <- default.n_units                   
		df.parm.TRB.main[,"afn_control"]                <- default.afn_control.main               
		df.parm.TRB.main[,"supply_leak_ratio_of_total"] <- default.supply_leak_ratio_of_total
		df.parm.TRB.main[,"vent_sch"]                   <- default.vent_sch   
		df.parm.TRB.main[,"wfr"]                        <- default.wfr   
		df.parm.TRB.main[,"dhw_type"]                   <- default.dhw_type		
		
		df.parm.TRB.duct[,"n_stories_per_unit"]         <- default.n_stories_per_unit        
		df.parm.TRB.duct[,"aspect_ratio"]               <- default.aspect_ratio              
		df.parm.TRB.duct[,"ventilation"]                <- default.ventilation               
		df.parm.TRB.duct[,"n_units"]                    <- default.n_units                   
		df.parm.TRB.duct[,"afn_control"]                <- default.afn_control.duct               
		df.parm.TRB.duct[,"supply_leak_ratio_of_total"] <- default.supply_leak_ratio_of_total
		df.parm.TRB.duct[,"vent_sch"]                   <- default.vent_sch   		
		df.parm.TRB.duct[,"wfr"]                        <- default.wfr   
		df.parm.TRB.duct[,"dhw_type"]                   <- default.dhw_type		
		cat(paste("83A: (",this.state,"): set default group 1!\n",sep=""))
		cat(paste("83A: (",this.state,"): set default group 1!\n",sep=""),file=FL.LOG,append=TRUE)

		# "runslab" and "runbsmt"
		df.parm.TRB.main[grep("^crawlspace$|^slab$",        df.parm.TRB.main[,"fndn_type"]),"runslab"] <- "yes"
		df.parm.TRB.main[grep("^crawlspace$|^slab$",        df.parm.TRB.main[,"fndn_type"]),"runbsmt"] <- "no"
		df.parm.TRB.main[grep("^heatedbsmt$|^unheatedbsmt$",df.parm.TRB.main[,"fndn_type"]),"runslab"] <- "no"
		df.parm.TRB.main[grep("^heatedbsmt$|^unheatedbsmt$",df.parm.TRB.main[,"fndn_type"]),"runbsmt"] <- "yes"
		
		# "runslab" and "runbsmt"
		df.parm.TRB.duct[grep("^crawlspace$|^slab$",        df.parm.TRB.duct[,"fndn_type"]),"runslab"] <- "yes"
		df.parm.TRB.duct[grep("^crawlspace$|^slab$",        df.parm.TRB.duct[,"fndn_type"]),"runbsmt"] <- "no"
		df.parm.TRB.duct[grep("^heatedbsmt$|^unheatedbsmt$",df.parm.TRB.duct[,"fndn_type"]),"runslab"] <- "no"
		df.parm.TRB.duct[grep("^heatedbsmt$|^unheatedbsmt$",df.parm.TRB.duct[,"fndn_type"]),"runbsmt"] <- "yes"		

		df.parm.TRB.main[,"semi_conditioned"]           <- default.semi_conditioned                          

		cat(paste("83B: (",this.state,"): set default group 2!\n",sep=""))
		cat(paste("83B: (",this.state,"): set default group 2!\n",sep=""),file=FL.LOG,append=TRUE)
		
		# -----------------------------------------------------------------------------------------
		# PRO
		# -----------------------------------------------------------------------------------------
		df.parm.PRO.main[,"n_stories_per_unit"]         <- default.n_stories_per_unit        
		df.parm.PRO.main[,"aspect_ratio"]               <- default.aspect_ratio              
		df.parm.PRO.main[,"ventilation"]                <- default.ventilation               
		df.parm.PRO.main[,"n_units"]                    <- default.n_units                   
		df.parm.PRO.main[,"afn_control"]                <- default.afn_control.main               
		df.parm.PRO.main[,"supply_leak_ratio_of_total"] <- default.supply_leak_ratio_of_total
		df.parm.PRO.main[,"vent_sch"]                   <- default.vent_sch     
		df.parm.PRO.main[,"wfr"]                        <- default.wfr 
		df.parm.PRO.main[,"dhw_type"]                   <- default.dhw_type		
		
		df.parm.PRO.duct[,"n_stories_per_unit"]         <- default.n_stories_per_unit        
		df.parm.PRO.duct[,"aspect_ratio"]               <- default.aspect_ratio              
		df.parm.PRO.duct[,"ventilation"]                <- default.ventilation               
		df.parm.PRO.duct[,"n_units"]                    <- default.n_units                   
		df.parm.PRO.duct[,"afn_control"]                <- default.afn_control.duct               
		df.parm.PRO.duct[,"supply_leak_ratio_of_total"] <- default.supply_leak_ratio_of_total
		df.parm.PRO.duct[,"vent_sch"]                   <- default.vent_sch   		
		df.parm.PRO.duct[,"wfr"]                        <- default.wfr 
		df.parm.PRO.duct[,"dhw_type"]                   <- default.dhw_type		
		cat(paste("83C: (",this.state,"): set default group 3!\n",sep=""))
		cat(paste("83C: (",this.state,"): set default group 3!\n",sep=""),file=FL.LOG,append=TRUE)
		
		# "runslab" and "runbsmt"
		df.parm.PRO.main[grep("^crawlspace$|^slab$",        df.parm.PRO.main[,"fndn_type"]),"runslab"] <- "yes"
		df.parm.PRO.main[grep("^crawlspace$|^slab$",        df.parm.PRO.main[,"fndn_type"]),"runbsmt"] <- "no"
		df.parm.PRO.main[grep("^heatedbsmt$|^unheatedbsmt$",df.parm.PRO.main[,"fndn_type"]),"runslab"] <- "no"
		df.parm.PRO.main[grep("^heatedbsmt$|^unheatedbsmt$",df.parm.PRO.main[,"fndn_type"]),"runbsmt"] <- "yes"
		
		# "runslab" and "runbsmt"
		df.parm.PRO.duct[grep("^crawlspace$|^slab$",        df.parm.PRO.duct[,"fndn_type"]),"runslab"] <- "yes"
		df.parm.PRO.duct[grep("^crawlspace$|^slab$",        df.parm.PRO.duct[,"fndn_type"]),"runbsmt"] <- "no"
		df.parm.PRO.duct[grep("^heatedbsmt$|^unheatedbsmt$",df.parm.PRO.duct[,"fndn_type"]),"runslab"] <- "no"
		df.parm.PRO.duct[grep("^heatedbsmt$|^unheatedbsmt$",df.parm.PRO.duct[,"fndn_type"]),"runbsmt"] <- "yes"		

		df.parm.PRO.main[,"semi_conditioned"]           <- default.semi_conditioned          

		cat(paste("83D: (",this.state,"): default values are used for some common parm variables!\n",sep=""))
		cat(paste("83D: (",this.state,"): default values are used for some common parm variables!\n",sep=""),file=FL.LOG,append=TRUE)

		# -----------------------------------------------------------------------------------------
		# replace 0 with 0.00001 for "r_floor", "r_bsmtwall" and "r_slab"
		# -----------------------------------------------------------------------------------------
		df.parm.TRB.main[df.parm.TRB.main[,"r_floor"] == 0,"r_floor"] <- 0.00001
		df.parm.PRO.main[df.parm.PRO.main[,"r_floor"] == 0,"r_floor"] <- 0.00001


		df.parm.TRB.main[df.parm.TRB.main[,"r_bsmtwall"] == 0,"r_bsmtwall"] <- 0.00001
		df.parm.PRO.main[df.parm.PRO.main[,"r_bsmtwall"] == 0,"r_bsmtwall"] <- 0.00001

		df.parm.TRB.main[df.parm.TRB.main[,"r_slab"] == 0,"r_slab"] <- 0.00001
		df.parm.PRO.main[df.parm.PRO.main[,"r_slab"] == 0,"r_slab"] <- 0.00001
		cat(paste("83E: (",this.state,"): set default group 5!\n",sep=""))
		cat(paste("83E: (",this.state,"): set default group 5!\n",sep=""),file=FL.LOG,append=TRUE)



		# -----------------------------------------------------------------------------------------
		# replace 0 with 0.00001 for "ins_qual_floor", "ins_qual_bsmtwall" and "r_slab"
		# -----------------------------------------------------------------------------------------
		# df.parm.TRB.main[df.parm.TRB.main[,"ins_qual_floor"] == 0,"ins_qual_floor"] <- 0.00001
		# df.parm.PRO.main[df.parm.PRO.main[,"ins_qual_floor"] == 0,"ins_qual_floor"] <- 0.00001


		# df.parm.TRB.main[df.parm.TRB.main[,"ins_qual_bsmtwall"] == 0,"ins_qual_bsmtwall"] <- 0.00001
		# df.parm.PRO.main[df.parm.PRO.main[,"ins_qual_bsmtwall"] == 0,"ins_qual_bsmtwall"] <- 0.00001
		# cat(paste("83F: (",this.state,"): set default group 5!\n",sep=""))
		# cat(paste("83F: (",this.state,"): set default group 5!\n",sep=""),file=FL.LOG,append=TRUE)
		
		
		# write out the main parm csv file to analysis folder
		write.table(df.parm.TRB.main[,field.to.parm.main],file=FL.main.parm.2keep,sep=",",row.names=FALSE,col.names=TRUE, append=TRUE,quote=FALSE)
		write.table(df.parm.PRO.main[,field.to.parm.main],file=FL.main.parm.2keep,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)

		# write out the main parm csv file to the simulation folder
		write.table(df.parm.TRB.main[,field.to.parm.main],file=FL.main.parm.2sim,sep=",",row.names=FALSE,col.names=TRUE, append=TRUE,quote=FALSE)
		write.table(df.parm.PRO.main[,field.to.parm.main],file=FL.main.parm.2sim,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
		
		# write out the duct parm csv file to the analysis folder
		write.table(df.parm.TRB.duct[,field.to.parm.duct],file=FL.duct.parm.2keep,sep=",",row.names=FALSE,col.names=TRUE, append=TRUE,quote=FALSE)
		write.table(df.parm.PRO.duct[,field.to.parm.duct],file=FL.duct.parm.2keep,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)

		# write out the duct parm csv file to the simulation folder
		write.table(df.parm.TRB.duct[,field.to.parm.duct],file=FL.duct.parm.2sim,sep=",",row.names=FALSE,col.names=TRUE, append=TRUE,quote=FALSE)
		write.table(df.parm.PRO.duct[,field.to.parm.duct],file=FL.duct.parm.2sim,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
		
		cat(paste("84: (",this.state,"): runs of all CZs are written out to ",FL.main.parm.2keep,"] and [",FL.main.parm.2sim,"]!\n",sep=""))
		cat(paste("84: (",this.state,"): runs of all CZs are written out to ",FL.main.parm.2keep,"] and [",FL.main.parm.2sim,"]!\n",sep=""),file=FL.LOG,append=TRUE)

		#
		# split the ID field for checking purpose
		#
		# ---------------------------------------------------------------------------------
		# break up the "S.ID" to get what we need
		# ---------------------------------------------------------------------------------
		df.parm.TRB.main.4chk <- df.parm.TRB.main
		df.parm.PRO.main.4chk <- df.parm.PRO.main
		
		df.parm.TRB.duct.4chk <- df.parm.TRB.duct
		df.parm.PRO.duct.4chk <- df.parm.PRO.duct		
		
		# TRB main                                  
		df.parm.TRB.main.4chk[,"SS.type"]       <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\1",df.parm.TRB.main.4chk[,"ID"],perl=TRUE)			
		df.parm.TRB.main.4chk[,"SS.City"]       <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\2",df.parm.TRB.main.4chk[,"ID"],perl=TRUE)			
		df.parm.TRB.main.4chk[,"SS.Standard"]   <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\3",df.parm.TRB.main.4chk[,"ID"],perl=TRUE)			
		tmp1                               	<- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\4",df.parm.TRB.main.4chk[,"ID"],perl=TRUE)
		df.parm.TRB.main.4chk[,"SS.HeatSys"]    <- sub("(.*)\\.(.*)",             "\\1",tmp1,                        perl=TRUE)
		df.parm.TRB.main.4chk[,"SS.Foundation"] <- sub("(.*)\\.(.*)",             "\\2",tmp1,                        perl=TRUE)
		tmp2                               	<- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\5",df.parm.TRB.main.4chk[,"ID"],perl=TRUE)			
		df.parm.TRB.main.4chk[,"SS.itemNam"]    <- sub("(.*)-(.*)-(.*)",          "\\2",tmp2,                        perl=TRUE)
		df.parm.TRB.main.4chk[,"SS.itemLvl"]    <- sub("(.*)-(.*)-(.*)",          "\\3",tmp2,                        perl=TRUE)
		
		# PRO main                                  
		df.parm.PRO.main.4chk[,"SS.type"]       <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\1",df.parm.PRO.main.4chk[,"ID"],perl=TRUE)			
		df.parm.PRO.main.4chk[,"SS.City"]       <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\2",df.parm.PRO.main.4chk[,"ID"],perl=TRUE)			
		df.parm.PRO.main.4chk[,"SS.Standard"]   <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\3",df.parm.PRO.main.4chk[,"ID"],perl=TRUE)			
		tmp1                               	<- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\4",df.parm.PRO.main.4chk[,"ID"],perl=TRUE)
		df.parm.PRO.main.4chk[,"SS.HeatSys"]    <- sub("(.*)\\.(.*)",             "\\1",tmp1,                        perl=TRUE)
		df.parm.PRO.main.4chk[,"SS.Foundation"] <- sub("(.*)\\.(.*)",             "\\2",tmp1,                        perl=TRUE)
		tmp2                               	<- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\5",df.parm.PRO.main.4chk[,"ID"],perl=TRUE)			
		df.parm.PRO.main.4chk[,"SS.itemNam"]    <- sub("(.*)-(.*)-(.*)",          "\\2",tmp2,                        perl=TRUE)
		df.parm.PRO.main.4chk[,"SS.itemLvl"]    <- sub("(.*)-(.*)-(.*)",          "\\3",tmp2,                        perl=TRUE)
		
		# TRB duct                                  
		df.parm.TRB.duct.4chk[,"SS.type"]       <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\1",df.parm.TRB.duct.4chk[,"ID"],perl=TRUE)			
		df.parm.TRB.duct.4chk[,"SS.City"]       <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\2",df.parm.TRB.duct.4chk[,"ID"],perl=TRUE)			
		df.parm.TRB.duct.4chk[,"SS.Standard"]   <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\3",df.parm.TRB.duct.4chk[,"ID"],perl=TRUE)			
		tmp1                               	<- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\4",df.parm.TRB.duct.4chk[,"ID"],perl=TRUE)
		df.parm.TRB.duct.4chk[,"SS.HeatSys"]    <- sub("(.*)\\.(.*)",             "\\1",tmp1,                        perl=TRUE)
		df.parm.TRB.duct.4chk[,"SS.Foundation"] <- sub("(.*)\\.(.*)",             "\\2",tmp1,                        perl=TRUE)
		tmp2                               	<- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\5",df.parm.TRB.duct.4chk[,"ID"],perl=TRUE)			
		df.parm.TRB.duct.4chk[,"SS.itemNam"]    <- sub("(.*)-(.*)-(.*)",          "\\2",tmp2,                        perl=TRUE)
		df.parm.TRB.duct.4chk[,"SS.itemLvl"]    <- sub("(.*)-(.*)-(.*)",          "\\3",tmp2,                        perl=TRUE)
		
		# PRO duct                                  
		df.parm.PRO.duct.4chk[,"SS.type"]       <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\1",df.parm.PRO.duct.4chk[,"ID"],perl=TRUE)			
		df.parm.PRO.duct.4chk[,"SS.City"]       <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\2",df.parm.PRO.duct.4chk[,"ID"],perl=TRUE)			
		df.parm.PRO.duct.4chk[,"SS.Standard"]   <- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\3",df.parm.PRO.duct.4chk[,"ID"],perl=TRUE)			
		tmp1                               	<- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\4",df.parm.PRO.duct.4chk[,"ID"],perl=TRUE)
		df.parm.PRO.duct.4chk[,"SS.HeatSys"]    <- sub("(.*)\\.(.*)",             "\\1",tmp1,                        perl=TRUE)
		df.parm.PRO.duct.4chk[,"SS.Foundation"] <- sub("(.*)\\.(.*)",             "\\2",tmp1,                        perl=TRUE)
		tmp2                               	<- sub("(.*)_(.*)_(.*)_(.*)_(.*)","\\5",df.parm.PRO.duct.4chk[,"ID"],perl=TRUE)			
		df.parm.PRO.duct.4chk[,"SS.itemNam"]    <- sub("(.*)-(.*)-(.*)",          "\\2",tmp2,                        perl=TRUE)
		df.parm.PRO.duct.4chk[,"SS.itemLvl"]    <- sub("(.*)-(.*)-(.*)",          "\\3",tmp2,                        perl=TRUE)		
		
		# write out the parm csv file
		write.table(df.parm.TRB.main.4chk,file=FL.main.parm.4chk,sep=",",row.names=FALSE,col.names=TRUE, append=TRUE,quote=FALSE)
		write.table(df.parm.PRO.main.4chk,file=FL.main.parm.4chk,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)

		write.table(df.parm.TRB.duct.4chk,file=FL.duct.parm.4chk,sep=",",row.names=FALSE,col.names=TRUE, append=TRUE,quote=FALSE)
		write.table(df.parm.PRO.duct.4chk,file=FL.duct.parm.4chk,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)

		cat(paste("85: (",this.state,"): runs of all CZs are written out to ",FL.main.parm.2keep,"] and [",FL.main.parm.2sim,"]!\n",sep=""))
		cat(paste("85: (",this.state,"): runs of all CZs are written out to ",FL.main.parm.2keep,"] and [",FL.main.parm.2sim,"]!\n",sep=""),file=FL.LOG,append=TRUE)
		
		
		
		dev.off()
	}
}

# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n91. 05_parm_generator.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n91. 05_parm_generator.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\n92. Processing time for [05_parm_generator.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\n92. Processing time for [05_parm_generator.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

#
# put run related information into the log file
#
cat(paste("73. This run was conducted in ",.Platform$OS.type,"\n",sep=""));
cat(paste("73. This run was conducted in ",.Platform$OS.type,"\n",sep=""),file=FL.LOG,append=TRUE);

# get the version of R used for this computation and the latest version released
current.Rversion <- R.Version()$version.string
tmp = readLines("http://cran.r-project.org/sources.html")
rls = tmp[grep("latest release", tmp) + 1L]			# the version number is in the next line of 'The latest release'
latest.Rversion  <- gsub("(.*R-|\\.tar\\.gz.*)", "", rls)	# "The latest release: R-2.13.0.tar.gz"
if (latest.Rversion != current.Rversion)
{
	cat(paste("\n\n94. you may want to upgrade R from the version you are using [",current.Rversion,"] to the latest version of [",latest.Rversion,"]\n",sep=""));
	cat(paste("\n\n94. you may want to upgrade R from the version you are using [",current.Rversion,"] to the latest version of [",latest.Rversion,"]\n",sep=""),file=FL.LOG,append=TRUE);
}else{
	cat(paste("\n\n94. The R version you are using is the latest version released so far!\n",sep=""))
	cat(paste("\n\n94. The R version you are using is the latest version released so far!\n",sep=""),file=FL.LOG,append=TRUE)
}



# get the version information of the attached libraries
cat(paste("\n\nThe information of the packages you used for this calculation:\n"))
cat(paste("\n\nThe information of the packages you used for this calculation:\n"),file=FL.LOG,append=TRUE)
tmp <- sessionInfo()
pkg.loaded <- tmp$otherPkgs
no.pkg.loaded <- length(pkg.loaded)
for (i in seq(1,no.pkg.loaded))
{
	cat(paste(pkg.loaded[[i]]$Package,":",pkg.loaded[[i]]$Version," ",pkg.loaded[[i]]$Date,"\n",sep=" "))
	cat(paste(pkg.loaded[[i]]$Package,":",pkg.loaded[[i]]$Version," ",pkg.loaded[[i]]$Date,"\n",sep=" "),file=FL.LOG,append=TRUE)
}





