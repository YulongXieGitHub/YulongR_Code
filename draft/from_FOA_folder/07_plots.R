#
# 07_plots.R
#
# July 8, 2015: (1) function_wgtHistogram revised to taking the number of the rows as the count of runs.
#               (2) the observation distribution removed the jitter along the y axis.
#
# July 7, 2015: this script is used to plot (a) the observation distribution at each CZ of the state
#					    (b) the simulation EUI results per CZ and the whole state
#
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
library("scales")
library("plyr")

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
}else{
	Path.Current  <- "Y:/FOA/Phase1_Analysis/RCD_Analysis/0_scripts"
	Path.Project  <- "Y:/FOA/Phase1_Analysis/RCD_Analysis"		
}

setwd(Path.Current)


# *********************************************************************************************************
# Select data from corresponding downloading
# *********************************************************************************************************
timeStamp.string <- "2015Jun15"

array.states <- c("AL","AR","GA","KY","MD","NC","PA","TX")
	
# define log path and output weight folder
Path.LOG  <- paste(Path.Project,"0_log",sep="/")
Path.TMP  <- paste(Path.Project,"07_plots",sep="/")
Path.OUT  <- paste(Path.TMP,timeStamp.string,sep="/")
Path.Simu <- paste(Path.Project,"06B_main_data_analysis",timeStamp.string,sep="/")
Path.Data <- paste(Path.Project,"04A_prepare_keyItem_for_simulation",timeStamp.string,sep="/")
Path.Auxi <- paste(Path.Project,"00_auxi_files",sep="/")

if (!file.exists(Path.TMP)) {print(paste("NOT existing:",Path.TMP));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.OUT)) {print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.LOG)) {print(paste("NOT existing:",Path.LOG));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Data)){print(paste("NOT existing:",Path.Data," Check Why!",sep=""));die}
if (!file.exists(Path.Auxi)){print(paste("NOT existing:",Path.Auxi," Check Why!",sep=""));die}

FL.LOG <- paste(Path.LOG, "07_plots.log",       sep="/")
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


# IECC requirement for the nine code items
IECC_2009 <- data.frame(CZ2 = c(30, 13, 13, 0.65, 0.3, 0.5, 7,  0,  0),
			CZ3 = c(30, 13, 19, 0.50, 0.3, 0.5, 7, 13,  0),
			CZ4 = c(38, 13, 19, 0.35, 0.4, 0.5, 7, 13, 10),
			CZ5 = c(38, 21, 30, 0.35, 0.4, 0.5, 7, 13, 10),	# (1) even the code requirement is 20 for CZ 5 & 6, we use 21.  (2) the NR shgc for CZ 4|5|6, we use 0.4 ad default.  (3) for slab thickness, the default is 2 in.  Even they are 4 in for CZ6, we use 2 in.  Vrushali confirmed.
			CZ6 = c(49, 21, 30, 0.35, 0.4, 0.5, 7, 19, 10))
row.names(IECC_2009) <- c("r_ceiling","r_wall","r_floor","u_window","shgc_window","f_inc_hw","ach50","r_bsmtwall","r_slab") 
cat(paste("3: specified the IECC 2009 reuirement for the nine code items.\n",sep=""))
cat(paste("3: specified the IECC 2009 reuirement for the nine code items.\n",sep=""),file=FL.LOG,append=TRUE)



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
cat(paste("4: specified the IECC 2015 reuirement for the nine code items.\n",sep=""))
cat(paste("4: specified the IECC 2015 reuirement for the nine code items.\n",sep=""),file=FL.LOG,append=TRUE)





source("multipleplot.R")
# source("weighted_histogram.R")
source("function_wgtHistogram.R")
source("function_splitVar.R")
cat(paste("5: load functions.\n",sep=""))
cat(paste("5: load functions.\n",sep=""),file=FL.LOG,append=TRUE)


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
cat(paste("6: share of CZ in the state.\n",sep=""))
cat(paste("6: share of CZ in the state.\n",sep=""),file=FL.LOG,append=TRUE)



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
cat(paste("7: share of htg system in the state.\n",sep=""))
cat(paste("7: share of htg system in the state.\n",sep=""),file=FL.LOG,append=TRUE)

   
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
cat(paste("8: share of foundations in the state.\n",sep=""))
cat(paste("8: share of foundations in the state.\n",sep=""),file=FL.LOG,append=TRUE)

# normalize the share because they do not add up to 1
array.share.CZ.state     <- sweep(array.share.CZ.state,2,colSums(array.share.CZ.state),FUN="/")
array.share.htgSys.state <- sweep(array.share.htgSys.state,2,colSums(array.share.htgSys.state),FUN="/")
array.share.found.state  <- sweep(array.share.found.state,2,colSums(array.share.found.state),FUN="/")
cat(paste("9: normalize to 1 to the shares of CZ, htgSys and foundations in the state.\n",sep=""))
cat(paste("9: normalize to 1 to the shares of CZ, htgSys and foundations in the state.\n",sep=""),file=FL.LOG,append=TRUE)

# turn the city name to CZ label
row.names(array.share.CZ.state) <- gsub("Houston","2A",gsub("Memphis","3A",gsub("Baltimore","4A",gsub("Chicago","5A",row.names(array.share.CZ.state)))))


array.share.CZ.state["3A","AL"] <- 0.7765
array.share.CZ.state["2A","AL"] <- 0.2235
cat(paste("10: Use CZ shares derived by Mark Halverson.\n",sep=""))
cat(paste("10: Use CZ shares derived by Mark Halverson.\n",sep=""),file=FL.LOG,append=TRUE)

cat(paste("11: start loopping through states.....................\n",sep=""))
cat(paste("11: start loopping through states.....................\n",sep=""),file=FL.LOG,append=TRUE)
for (this.state in array.states[1])
{
	count.plot <- 0
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
	
	thisState.CZ.shares <- array.share.CZ.state[array.share.CZ.state[,this.state,drop=FALSE]>0,this.state,drop=FALSE]

	# define sub-folder for each state
	Path.Out.State  <- paste(Path.OUT, this.state,sep="/")
	Path.Simu.State <- paste(Path.Simu,this.state,sep="/")
	if (!file.exists(Path.Out.State)) {print(paste("NOT existing:",Path.Out.State)); dir.create(Path.Out.State, showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.Simu.State)){print(paste("NOT existing:",Path.Simu.State));dir.create(Path.Simu.State, showWarnings=TRUE,recursive=TRUE)}
	cat(paste("22: [",this.state,"]: specified an output and a simulation folder for the state.\n",sep=""))
	cat(paste("22: [",this.state,"]: specified an output and a simulation folder for the state.\n",sep=""),file=FL.LOG,append=TRUE)
	
	# define input file names
	FL.EUI.rds   <- paste(Path.Simu.State,paste("FOA_EUI_ductAdj.rds",sep=""),sep="/")	
	FL.Data.rds  <- paste(Path.Data,      paste(paste("State",this.state,timeStamp.string,sep="_"),"_KeyItem_Wide.RDS",sep=""),sep="/")	
	FL.Obs.rds   <- paste(Path.Data,      paste(paste("State",this.state,timeStamp.string,sep="_"),"_NumericItem.RDS", sep=""),sep="/")
	if (!file.exists(FL.EUI.rds)) {print(paste("NOT existing:",FL.EUI.rds,"   Check Why!",sep=""));die}
	if (!file.exists(FL.Data.rds)){print(paste("NOT existing:",FL.Data.rds,"  Check Why!",sep=""));die}
	if (!file.exists(FL.Obs.rds)) {print(paste("NOT existing:",FL.Obs.rds,"   Check Why!",sep=""));die}
	cat(paste("23: [",this.state,"]: specify input files.\n",sep=""))
	cat(paste("23: [",this.state,"]: specify input files.\n",sep=""),file=FL.LOG,append=TRUE)

	# define input file names	
	FL.hist.pdf       <- paste(Path.Out.State,paste(this.state,"_plots.pdf",sep=""),sep="/")
	FL.chk.4hist.csv  <- paste(Path.Out.State,paste(this.state,"_plots.csv",sep=""),sep="/")
	FL.data.4hist.rds <- paste(Path.Out.State,paste(this.state,"_data_4hist.rds",  sep=""),sep="/")
	FL.wgt.4hist.rds  <- paste(Path.Out.State,paste(this.state,"_weight_4hist.rds",sep=""),sep="/")
	if (file.exists(FL.hist.pdf)) {print(paste(FL.hist.pdf, "exist.Delete it!"));file.remove(FL.hist.pdf)}	
	if (file.exists(FL.chk.4hist.csv)) {print(paste(FL.chk.4hist.csv, "exist.Delete it!"));file.remove(FL.chk.4hist.csv)}	
	if (file.exists(FL.data.4hist.rds)){print(paste(FL.data.4hist.rds,"exist.Delete it!"));file.remove(FL.data.4hist.rds)}
	if (file.exists(FL.wgt.4hist.rds)) {print(paste(FL.wgt.4hist.rds, "exist.Delete it!"));file.remove(FL.wgt.4hist.rds)}
	cat(paste("24: [",this.state,"]: define path and file names for the state.\n",sep=""))
	cat(paste("24: [",this.state,"]: define path and file names for the state.\n",sep=""),file=FL.LOG,append=TRUE)

	# open the device to plot
	pdf(file=FL.hist.pdf,paper="special",width=17,height=11,bg="transparent")
	cat(paste("25: [",this.state,"]: open the pdf file for output plots.\n",sep=""))
	cat(paste("25: [",this.state,"]: open the pdf file for output plots.\n",sep=""),file=FL.LOG,append=TRUE)



	
	# =========================================================================================
	# load EUI data	
	# =========================================================================================
	myEUI <- readRDS(file=FL.EUI.rds)
	cat(paste("26: [",this.state,"]: simulated EUI data has been loaded.\n",sep=""))
	cat(paste("26: [",this.state,"]: simulated EUI data has been loaded.\n",sep=""),file=FL.LOG,append=TRUE)
	
	
	# add a state weighted average of the code compliance bldg
	thisState.baselineEUI  <- myEUI[diff(c(0,myEUI[,"EUI.tot.TRB"])) != 0,c("SS.CZ","EUI.tot.TRB"),drop=FALSE]
	thisState.baselineEUI[,"CZ.share"] <- thisState.CZ.shares[thisState.baselineEUI[,"SS.CZ"],this.state,drop=FALSE]
	thisState.weightedEUI <- weighted.mean(thisState.baselineEUI[,"EUI.tot.TRB"], w = thisState.baselineEUI[,"CZ.share"])
	myEUI[,"EUI.State.WgtTRB"]  <- thisState.weightedEUI	
	myEUI[,"EUI.State.WgtSimu"] <- myEUI[,"EUI.State.WtgMean.ductAdj"]
	thisState.WgtEUI.ductAdj    <- myEUI[1,"EUI.State.WtgMean.ductAdj"]
	cat(paste("27: [",this.state,"]: have added the weighted CZ & state EUI from code compilance and from simulation into the [myEUI] data frame.\n",sep=""))
	cat(paste("27: [",this.state,"]: have added the weighted CZ & state EUI from code compilance and from simulation into the [myEUI] data frame.\n",sep=""),file=FL.LOG,append=TRUE)
	
	#
	# prepare data for using Hathways's function to plot the weighted histogram
	# which needs (1) a data frame with at least two columns, column one is the value to be plotted hee is the EUI, column two in the a factor of the groups here is the CZ
	#             (2) a weighting data frame, where one colun has the same bame as the group column in the data frame and the other columns is the weights
	#
	# ********************************************************************************************
	#       myEUI.4.wgtHistogram  <- myEUI[,c("SS.CZ","EUI.tot.TRB","EUI.tot.CZ.mean.ductAdj","EUI.State.WgtTRB","EUI.State.WgtSimu","EUI.tot.PRO.ductAdj")]	
	# names(myEUI.4.wgtHistogram) <-        c("CZ",   "CZ.BaseEUI", "CZ.meanSimEUI",          "State.WgtBaseEUI","State.WgtSimEUI",  "EUI")


	      myEUI.4.wgtHistogram  <- myEUI[,c("SS.CZ","EUI.State.WgtTRB","EUI.State.WgtSimu","EUI.tot.PRO.ductAdj")]	
	names(myEUI.4.wgtHistogram) <-        c("CZ",   "State.WgtBaseEUI","State.WgtSimEUI",  "EUI")


	
	if(!(is.factor(myEUI.4.wgtHistogram[,"CZ"]))){myEUI.4.wgtHistogram[,"CZ"] <- as.factor(myEUI.4.wgtHistogram[,"CZ"])}
	
	
	myWgt <- data.frame(CZ = c("2A","3A"),weight = c(0.2235,0.7765))
	cat(paste("28: [",this.state,"]: prepare the data [myEUI.4.wgtHistogram] and [myWgt] for  plotting the weighted hisogram.\n",sep=""))
	cat(paste("28: [",this.state,"]: prepare the data [myEUI.4.wgtHistogram] and [myWgt] for  plotting the weighted hisogram.\n",sep=""),file=FL.LOG,append=TRUE)
	
	
	save(myEUI.4.wgtHistogram,file=FL.data.4hist.rds)
	save(myWgt,file=FL.wgt.4hist.rds)
	cat(paste("29: [",this.state,"]: saved [myEUI.4.wgtHistogram] and [myWgt] to rds files.\n",sep=""))
	cat(paste("29: [",this.state,"]: saved [myEUI.4.wgtHistogram] and [myWgt] to rds files.\n",sep=""),file=FL.LOG,append=TRUE)
	
	#
	# out put the data frame for exporting
	#
	plot.wgtPdf <- weighted_histogram(myEUI.4.wgtHistogram,value_column="EUI",group_column="CZ",myWgt,overlay_alpha=0.5)
	cat(paste("29B: [",this.state,"]: call function to plot weighted histogram.\n",sep=""))
	cat(paste("29B: [",this.state,"]: call function to plot weighted histogram.\n",sep=""),file=FL.LOG,append=TRUE)
	
	# CZ weighted code compliance EUI
	plot.wgtPdf <- plot.wgtPdf +  geom_vline(aes(xintercept=State.WgtBaseEUI), data =myEUI.4.wgtHistogram,colour="black") 		
	plot.wgtPdf <- plot.wgtPdf + annotate("text",label=round(myEUI.4.wgtHistogram[1,"State.WgtBaseEUI"],digits=2),x=myEUI.4.wgtHistogram[1,"State.WgtBaseEUI"],y=0,colour="black")	
	
	# CZ weighted simulated EUI
	plot.wgtPdf <- plot.wgtPdf + geom_vline(aes(xintercept=State.WgtSimEUI), data =myEUI.4.wgtHistogram,colour="magenta")		
	plot.wgtPdf <- plot.wgtPdf + annotate("text",label=round(myEUI.4.wgtHistogram[1,"State.WgtSimEUI"],digits=2),x=myEUI.4.wgtHistogram[1,"State.WgtSimEUI"],y=0,colour="magenta")
	plot.wgtPdf <- plot.wgtPdf + labs(x="EUI Value",y="Percentage",title=paste(paste("\nvertical black line indicates the weighted average of EUI for a IECC prescriptive code-compliant prototype\nvertical magenta line indicates the weighted average of simulated EUI of the state",sep=""),"\n",sep=""))
	plot(plot.wgtPdf)
	
	# output to a jpeg file
	count.plot <- count.plot + 1
	FL.JPG <- paste(Path.Out.State,paste(this.state,"_figure",count.plot,".jpg",sep=""),sep="/")
	jpeg(file = FL.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")
	dev.set(3)	
		plot(plot.wgtPdf)
	dev.off(3)	
	cat(paste("30: [",this.state,"]: the weighted average PDF plots of the state.\n",sep=""))
	cat(paste("30: [",this.state,"]: the weighted average PDF plots of the state.\n",sep=""),file=FL.LOG,append=TRUE)
	
	
	
	# ----------------------------------------------------------------------------------
	# plotting series I
	# ----------------------------------------------------------------------------------
	for (label.CZ in sort(unique(myEUI[,"SS.CZ"])))
	{
		if (label.CZ == "2A")
		{
			this.color <- "red"
		}else if (label.CZ == "3A")
		{
			this.color <- "cyan"
		}
		# specified color for plotting
	
		
		
		if(exists("mySubset.4plot")  && is.data.frame(get("mySubset.4plot"))) {rm(mySubset.4plot)}
		
		      mySubset.4plot  <- subset(myEUI,subset=(SS.CZ == label.CZ),select = EUI.tot.PRO.ductAdj,drop=FALSE)
		      mySubset.4plot  <- mySubset.4plot[order(mySubset.4plot[,"EUI.tot.PRO.ductAdj"]),,drop=FALSE]
		      mySubset.4plot[,"Index"] <- seq(from=1,to=dim(mySubset.4plot)[1])
		names(mySubset.4plot) <- sub("EUI.tot.PRO.ductAdj","EUI",names(mySubset.4plot))
		baseline.EUI <- myEUI[myEUI[,"SS.CZ"] == label.CZ,"EUI.tot.TRB"][1]		# the corresponding baseline EUI
		baseline.IDX <- seq(1:dim(mySubset.4plot)[1])[mySubset.4plot[,"EUI"] > baseline.EUI][1]

		CZ.mean.EUI <- myEUI[myEUI[,"SS.CZ"] == label.CZ,"EUI.tot.CZ.mean.ductAdj"][1]	# the corresponding CZ average EUI
		CZ.mean.IDX <- seq(1:dim(mySubset.4plot)[1])[mySubset.4plot[,"EUI"] > CZ.mean.EUI][1]


		# -------------------------------------------------------------------------- 
		# CDF 
		# --------------------------------------------------------------------------
		plot.obj.cdf <- xyplot(EUI~Index,data=mySubset.4plot,
				   horizontal = FALSE,
				   type       = "h",
				   layout     = c(0,1),
				   as.table   = TRUE,
				   between    = list(y=0.5),
				   xlab       = "Index of Models",
				   ylab       = "EUI (kBtu/sf)",
				   main       = paste("(",this.state,"): IECC Regulated end-use EUI (kBtu/sf) of ",dim(mySubset.4plot)[1]," pseudo houses sampled at (",label.CZ,")\nRed line indicates the EUI for a 2009 IECC prescriptive code-compliant prototype\nCyan line indicates the average EUI of the simulated models",sep=""),
				 # main       = paste("(",label.CZ,")-(",this.state,"): IECC Regulated end-use EUI of the models based on randow sampling of code items field observations",sep=""),
				   cex        = 0.5,
				   pch        = 19,
				   col        = "blue",

			   panel = function(x,y,...) {
				   panel.xyplot(x, y,...)
				   panel.abline(h=baseline.EUI,col=c("red"))
				   panel.abline(v=baseline.IDX,col=c("red"))
				   panel.text(0,baseline.EUI,adj=c(-0.25,-0.25),labels=paste("Code EUI of (",label.CZ,")=",round(baseline.EUI,digits=2)," (kBtu/sf)",sep=""),col="red")	
				   
				   panel.abline(h=CZ.mean.EUI,col=c("cyan"))
				   panel.abline(v=CZ.mean.IDX,col=c("cyan"))
				   panel.text(0,CZ.mean.EUI,adj=c(-0.25,-0.25),labels=paste("Average EUI of (",label.CZ,")=",round(CZ.mean.EUI,digits=2)," (kBtu/sf)",sep=""),col="cyan")}	
				   
				   )
				   
				   # plot(plot.obj.cdf)

			   	
		# assign a plot number of the plot
		command.string <- paste(paste("p.plot000",count.plot,sep="")," <- plot.obj.cdf",sep="") 
		eval(parse(text=command.string))		
		
		# plotting
		dev.set(2)
		plot(plot.obj.cdf)
		
		# output to a jpeg file
		count.plot <- count.plot + 1		
		FL.JPG <- paste(Path.Out.State,paste(this.state,"_figure",count.plot,".jpg",sep=""),sep="/")
		jpeg(file = FL.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")
		dev.set(3)	
			plot(plot.obj.cdf)
		dev.off(3)
		
		# -------------------------------------------------------------------------- 
		# PDF 
		# --------------------------------------------------------------------------
		plot.obj.pdf <- histogram(~EUI,data = mySubset.4plot,freq=TRUE,nint=50,
				           layout     = c(0,1),
				           border     = "black",
				           as.table   = TRUE,
				           between    = list(y=0.5),
				           xlab       = "EUI (kBtu/sf)",
				           main       = paste("(",this.state,"): IECC Regulated end-use EUI (kBtu/sf) of ",dim(mySubset.4plot)[1]," pseudo houses sampled at (",label.CZ,")\nRed line indicates the EUI for a 2009 IECC prescriptive code-compliant prototype\nCyan line indicates the average EUI of the simulated models",sep=""),
				           cex        = 0.5,
				           col        = "black",

			   panel = function(x,...) {
				   panel.histogram(x,...)
				   panel.abline(v=baseline.EUI,col=c("red"))
				   panel.text(baseline.EUI,0,adj=c(0.25,-0.25),labels=round(baseline.EUI,digits=2),col="red")	

				   panel.abline(v=CZ.mean.EUI,col=c("cyan"))
				   panel.text(CZ.mean.EUI,0,adj=c(0.25,-0.25),labels=round(CZ.mean.EUI,digits=2),col="cyan")}	
				   
				   )

		# assign a plot number of the plot
		command.string <- paste(paste("p.plot000",count.plot,sep="")," <- plot.obj.pdf",sep="") 
		eval(parse(text=command.string))		
		
		# plotting
		dev.set(2)
		plot(plot.obj.pdf)	
		
		# output to a jpeg file
		count.plot <- count.plot + 1
		FL.JPG <- paste(Path.Out.State,paste(this.state,"_figure",count.plot,".jpg",sep=""),sep="/")
		jpeg(file = FL.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")
		dev.set(3)	
			plot(plot.obj.pdf)
		dev.off(3)		
	}
	cat(paste("32: [",this.state,"]: output ",dim(myEUI)[1]," by ",dim(myEUI)[2]," [myEUI].\n",sep=""))
	cat(paste("32: [",this.state,"]: output ",dim(myEUI)[1]," by ",dim(myEUI)[2]," [myEUI].\n",sep=""),file=FL.LOG,append=TRUE)
	
	
	# in [myEUI], there is a field "EUI.tot.TRB" which has only 2 or 3 unique numbers, one for each CZ, retrieve the frst row to reach
	thisState.baselineEUI  <- myEUI[diff(c(0,myEUI[,"EUI.tot.TRB"])) != 0,c("SS.CZ","EUI.tot.TRB"),drop=FALSE]
	thisState.baselineEUI[,"CZ.share"] <- thisState.CZ.shares[thisState.baselineEUI[,"SS.CZ"],this.state,drop=FALSE]
	thisState.weightedEUI <- weighted.mean(thisState.baselineEUI[,"EUI.tot.TRB"], w = thisState.baselineEUI[,"CZ.share"])
	cat(paste("33: [",this.state,"]: baseline EUI in CZs [thisState.baselineEUI] and weighted baselineEUI of the state in [thisState.weightedEUI].\n",sep=""))
	cat(paste("33: [",this.state,"]: baseline EUI in CZs [thisState.baselineEUI] and weighted baselineEUI of the state in [thisState.weightedEUI].\n",sep=""),file=FL.LOG,append=TRUE)	






	# ----------------------------------------------------------------------------------
	# EndUse Histogram
	# ----------------------------------------------------------------------------------
	
	# creating a long format version of [myEUI] for plotting purpose: re-arrange the 6 variables of [EUI.elec.PRO],[EUI.gas.PRO],[EUI.tot.PRO],[EUI.elec.DIFF],[EUI.gas.DIFF],[EUI.tot.DIFF] 
	measure.vars.ductAdj   <- c(grep("EUI.gas.PRO|EUI.elec.PRO|EUI.tot.PRO",names(myEUI),value=TRUE))
	id.vars.ductAdj        <- names(myEUI)[-c(match(measure.vars.ductAdj,names(myEUI)))]
	# the non-match subset of the fields: including the 6 PRO fields (elec, gas, tot EUI and DIFF)	
	myEUI.long <- melt(myEUI,id.vars = id.vars.ductAdj,measure.vars = measure.vars.ductAdj,variable.name = "Variable",value.name = "value")
	cat(paste("54: [",this.state,"]: have a ",dim(myEUI)[1]," by ",dim(myEUI)[2]," [myEUI] and a ",dim(myEUI.long)[1]," by ",dim(myEUI.long)[2]," [myEUI.long] ready.\n",sep=""))
	cat(paste("54: [",this.state,"]: have a ",dim(myEUI)[1]," by ",dim(myEUI)[2]," [myEUI] and a ",dim(myEUI.long)[1]," by ",dim(myEUI.long)[2]," [myEUI.long] ready.\n",sep=""),file=FL.LOG,append=TRUE)


	
	# 11. IECC Regulated end-use Gas EUI
	if(exists("mySubset.4plot")  && is.data.frame(get("mySubset.4plot"))) {rm(mySubset.4plot)}
	mySubset.4plot <- subset(myEUI.long,subset=(Variable == "EUI.gas.PRO.ductAdj"))
	mySubset.4plot[,"y"] <- 0
	this.codeItem <- "IECC Regulated end-use Gas EUI"
	xlabel        <- "IECC Regulated End-Use Gas EUI"
	this.TRB      <- "EUI.gas.TRB"
	this.CZ       <- "SS.CZ"	
	names(mySubset.4plot) <- sub(this.CZ,"CZ",sub(this.TRB,"CodeCompliance",sub("value","KeyCodeValue",names(mySubset.4plot))))
	p.plot51 <- ggplot(data=mySubset.4plot,aes(x=KeyCodeValue,colour=factor(CZ),fill=factor(CZ))) + geom_histogram() + facet_wrap(~ CZ,ncol = 1)
	p.plot51 <- p.plot51 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="")
	p.plot51 <- p.plot51 + labs(x=paste(xlabel,"\nvertical black line indicates the EUI for a IECC prescriptive code-compliant prototype",sep=""),y="count",title=paste(paste(xlabel," at [",this.state,"]",sep=""),"\n",sep=""))
	p.plot51 <- p.plot51 + geom_vline(aes(xintercept=CodeCompliance), data =mySubset.4plot) 
	p.plot51 <- p.plot51 + geom_text(aes(x=CodeCompliance,y=y,label = round(CodeCompliance,digits=2)),data = mySubset.4plot,colour="red",size=5)	

	# 12. IECC Regulated end-use Elec EUI
	if(exists("mySubset.4plot")  && is.data.frame(get("mySubset.4plot"))) {rm(mySubset.4plot)}
	mySubset.4plot <- subset(myEUI.long,subset=(Variable == "EUI.elec.PRO.ductAdj"))
	mySubset.4plot[,"y"] <- 0
	this.codeItem <- "IECC Regulated end-use Elec EUI"
	xlabel        <- "IECC Regulated End-Use Elec EUI"
	this.TRB      <- "EUI.elec.TRB"
	this.CZ       <- "SS.CZ"	
	names(mySubset.4plot) <- sub(this.CZ,"CZ",sub(this.TRB,"CodeCompliance",sub("value","KeyCodeValue",names(mySubset.4plot))))
	p.plot52 <- ggplot(data=mySubset.4plot,aes(x=KeyCodeValue,colour=factor(CZ),fill=factor(CZ))) + geom_histogram() + facet_wrap(~ CZ,ncol = 1)
	p.plot52 <- p.plot52 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="")
	p.plot52 <- p.plot52 + labs(x=paste(xlabel,"\nvertical black line indicates the EUI for a IECC prescriptive code-compliant prototype",sep=""),y="count",title=paste(paste(xlabel," at [",this.state,"]",sep=""),"\n",sep=""))
	p.plot52 <- p.plot52 + geom_vline(aes(xintercept=CodeCompliance), data =mySubset.4plot) 
	p.plot52 <- p.plot52 + geom_text(aes(x=CodeCompliance,y=y,label = round(CodeCompliance,digits=2)),data = mySubset.4plot,colour="red",size=5)	
	
	
	# 13. IECC Regulated end-use Total EUI
	if(exists("mySubset.4plot")  && is.data.frame(get("mySubset.4plot"))) {rm(mySubset.4plot)}
	mySubset.4plot <- subset(myEUI.long,subset=(Variable == "EUI.tot.PRO.ductAdj"))
	mySubset.4plot[,"y"] <- 0
	this.codeItem <- "IECC Regulated end-use Total EUI"
	xlabel        <- "IECC Regulated End-Use EUI"
	this.TRB      <- "EUI.tot.TRB"
	this.CZ       <- "SS.CZ"	
	names(mySubset.4plot) <- sub(this.CZ,"CZ",sub(this.TRB,"CodeCompliance",sub("value","KeyCodeValue",names(mySubset.4plot))))
	p.plot53 <- ggplot(data=mySubset.4plot,aes(x=KeyCodeValue,colour=factor(CZ),fill=factor(CZ))) + geom_histogram() + facet_wrap(~ CZ,ncol = 1)
	p.plot53 <- p.plot53 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="")
	p.plot53 <- p.plot53 + labs(x=paste(xlabel,"\nvertical black line indicates the EUI for a IECC prescriptive code-compliant prototype",sep=""),y="count",title=paste(paste(xlabel," at [",this.state,"]",sep=""),"\n",sep=""))
	p.plot53 <- p.plot53 + geom_vline(aes(xintercept=CodeCompliance), data =mySubset.4plot) 
	p.plot53 <- p.plot53 + geom_text(aes(x=CodeCompliance,y=y,label = round(CodeCompliance,digits=2)),data = mySubset.4plot,colour="red",size=5)	

	# -------------------------------------------------------------------------------------------------------
	# 14. IECC Regulated end-use Total EUI
	#     June 29, 2015: add two vertical bars
	# -------------------------------------------------------------------------------------------------------
	if(exists("mySubset.4plot")  && is.data.frame(get("mySubset.4plot"))) {rm(mySubset.4plot)}
	mySubset.4plot <- subset(myEUI.long,subset=(Variable == "EUI.tot.PRO.ductAdj"))
	mySubset.4plot[,"y"] <- 0
	this.codeItem <- "IECC Regulated end-use Total EUI"
	xlabel        <- "IECC Regulated End-Use EUI"
	this.TRB      <- "EUI.tot.TRB"
	this.CZ       <- "SS.CZ"	
	names(mySubset.4plot) <- sub(this.CZ,"CZ",sub(this.TRB,"CodeCompliance",sub("value","KeyCodeValue",names(mySubset.4plot))))
	p.plot54 <- ggplot(data=mySubset.4plot,aes(x=KeyCodeValue,colour=factor(CZ),fill=factor(CZ))) + geom_histogram() + facet_wrap(~ CZ,ncol = 2)
	p.plot54 <- p.plot54 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="")
	p.plot54 <- p.plot54 + labs(x=paste(xlabel,"\nvertical black line indicates the EUI for a IECC prescriptive code-compliant prototype\nvertical green line indicates the weighted average of simulated EUI of the state",sep=""),y="count",title=paste(paste(xlabel," at [",this.state,"]",sep=""),"\n",sep=""))
	
	# CZ weighted code compliance EUI
	p.plot54 <- p.plot54 + geom_vline(xintercept=thisState.weightedEUI,colour="black") 		
	p.plot54 <- p.plot54 + annotate("text",label=round(thisState.weightedEUI,digits=2),x=thisState.weightedEUI,y=0,colour="black")	
	
	# CZ weighted simulated EUI
	p.plot54 <- p.plot54 + geom_vline(xintercept=thisState.WgtEUI.ductAdj,colour="green") 		
	p.plot54 <- p.plot54 + annotate("text",label=round(thisState.WgtEUI.ductAdj,digits=2),x=thisState.WgtEUI.ductAdj,y=0,colour="green")	





	# 15. IECC Regulated end-use Total EUI
	if(exists("mySubset.4plot")  && is.data.frame(get("mySubset.4plot"))) {rm(mySubset.4plot)}
	mySubset.4plot <- subset(myEUI.long,subset=(Variable == "EUI.tot.PRO.ductAdj"))
	mySubset.4plot[,"y"] <- 0
	this.codeItem <- "IECC Regulated end-use Total EUI"
	xlabel        <- "IECC Regulated End-Use EUI"
	this.TRB      <- "EUI.tot.TRB"
	this.CZ       <- "SS.CZ"	
	names(mySubset.4plot) <- sub(this.CZ,"CZ",sub(this.TRB,"CodeCompliance",sub("value","KeyCodeValue",names(mySubset.4plot))))
	p.plot55 <- ggplot(data=mySubset.4plot,aes(x=KeyCodeValue)) + geom_histogram(colour="black",fill="black")
	p.plot55 <- p.plot55 + theme(axis.text.x = element_text(angle=90,color="black",size=10),axis.text.y = element_text(color="black"),legend.position="")
	
	# CZ weighted code compliance EUI
	p.plot55 <- p.plot55 + labs(x=paste(xlabel,"\nvertical red line indicates the EUI for a IECC prescriptive code-compliant prototype\nvertical green line indicates the weighted average of simulated EUI of the state",sep=""),y="count",title=paste(paste(xlabel," at [",this.state,"]",sep=""),"\n",sep=""))
	p.plot55 <- p.plot55 + geom_vline(aes(xintercept=thisState.weightedEUI), colour="red", data =mySubset.4plot) 
	p.plot55 <- p.plot55 + annotate("text",label=round(thisState.weightedEUI,digits=2),x=thisState.weightedEUI,y=0,colour="red")	
	
	# CZ weighted simulated EUI
	p.plot55 <- p.plot55 + geom_vline(xintercept=thisState.WgtEUI.ductAdj,colour="green") 		
	p.plot55 <- p.plot55 + annotate("text",label=round(thisState.WgtEUI.ductAdj,digits=2),x=thisState.WgtEUI.ductAdj,y=0,colour="green")	




	dev.set(2)
	multiplot(p.plot51,p.plot52,p.plot53,cols=3)
      	plot(p.plot54)


	# output to a jpeg file
	count.plot <- count.plot + 1
	FL.JPG <- paste(Path.Out.State,paste(this.state,"_figure",count.plot,".jpg",sep=""),sep="/")
	jpeg(file = FL.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")
	dev.set(3)	
		multiplot(p.plot51,p.plot52,p.plot53,cols=3)
	dev.off(3)	
	

	# output to a jpeg file
	count.plot <- count.plot + 1
	FL.JPG <- paste(Path.Out.State,paste(this.state,"_figure",count.plot,".jpg",sep=""),sep="/")
	jpeg(file = FL.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")
	dev.set(3)	
		plot(p.plot54)
	dev.off(3)			

		
		
	cat(paste("140: [",this.state,"]: ranked EUI are plotted in terms of the CZs in the state.\n",sep=""))
	cat(paste("140: [",this.state,"]: ranked EUI are plotted in terms of the CZs in the state.\n",sep=""),file=FL.LOG,append=TRUE)





	
	# =========================================================================================
	# load observation data
	# =========================================================================================
	myOBS <- readRDS(file=FL.Obs.rds)
	cat(paste("41: [",this.state,"]: load the observations data.\n",sep=""))
	cat(paste("41: [",this.state,"]: load the observations data.\n",sep=""),file=FL.LOG,append=TRUE)	
	
	plot.obj <- ggplot(data=myOBS) + geom_point(aes(x=KeyCodeString,y=KeyCodeValue,color=ClimateZone),position=position_jitter(width=0.5,height=0))   # only jitter the x axis
	plot.obj <- plot.obj + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_text(colour="black")) 
	plot.obj <- plot.obj + labs(y="Code Item Observed Range",x="",title=paste(this.state,": Distributions of Key Code Items",sep=""))
	plot.obj <- plot.obj + geom_hline(aes(yintercept=CodeCompliance,color=ClimateZone), data =myOBS) 		
	plot.obj <- plot.obj + facet_wrap(~KeyCodeString,scales="free")
	cat(paste("44: [",this.state,"]: generate the plot.\n",sep=""))
	cat(paste("44: [",this.state,"]: generate the plot.\n",sep=""),file=FL.LOG,append=TRUE)	
	
	dev.set(2)
	plot(plot.obj)
	
	
	# output to a jpeg file
	count.plot <- count.plot + 1
	FL.JPG <- paste(Path.Out.State,paste(this.state,"_figure",count.plot,".jpg",sep=""),sep="/")
	jpeg(file = FL.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")
	dev.set(3)	
		plot(plot.obj)
	dev.off(3)
	cat(paste("45: [",this.state,"]: plotted.\n",sep=""))
	cat(paste("45: [",this.state,"]: plotted.\n",sep=""),file=FL.LOG,append=TRUE)	





	### # Hathaway's processing
	### myOBS.processed = split_variables(x=myOBS)
	### cat(paste("42: [",this.state,"]: processing by Hathaways function.\n",sep=""))
	### cat(paste("42: [",this.state,"]: processing by Hathaways function.\n",sep=""),file=FL.LOG,append=TRUE)	
	### 
	### # re-assign variable names
	### myOBS.processed[,"CodeItem"] <- myOBS.processed[,"variable_2"]
	### myOBS.processed[,"CodeItem"] <- gsub("^ach50$","ACH50(FI17)",
	###                                 gsub("^compbsmtwallr$","BsmtWallR(FO04)",
	###                                 gsub("^ceilingr$","CeilingR(FI1)",
	###                                 gsub("^ceilingr.1$","CeilingRInsQual(IQ1)",
	###                                 gsub("^floorr$","FloorR(IN1a)",
	###                                 gsub("^wallr.3$","WallR(IN3a))",
	###                                 gsub("^wallr.4$","WallRInsQual(IQ3)",
	###                                 gsub("^ducttightness$","DuctTightness(FI4)",
	###                                 gsub("^f_inc_hw$","f_inc_hw(FI6)",
	###                                 gsub("^slabr$","SlabR(FO1)",
	###                                 gsub("^u_windows$","WindowsU(FR2)",
	###                                 gsub("^shgc_windows$","WindowsSHGC(FR3)",myOBS.processed[,"CodeItem"]))))))))))))
	### cat(paste("43: [",this.state,"]: rename the code items.\n",sep=""))
	### cat(paste("43: [",this.state,"]: rename the code items.\n",sep=""),file=FL.LOG,append=TRUE)	
	### 
	### 
	### # plot.obj <- ggplot(data=myOBS.processed) + geom_jitter(aes(x=CodeItem,y=value))   # note: geom_jitter also pertube the y axis	
	### #geom_hline(data=IECC_2009,aes(y=CZ2))+
	### plot.obj <- ggplot(data=myOBS.processed) + geom_point(aes(x=CodeItem,y=value),position=position_jitter(width=0.5,height=0))   # only jitter the x axis
	### plot.obj <- plot.obj + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_text(colour="black")) 
	### plot.obj <- plot.obj + labs(y="Code Item Observed Range",x="",title=paste(this.state,": Distributions of Key Code Items",sep="")
	### )
	### plot.obj <- plot.obj + facet_wrap(~CodeItem,scales="free")
	### cat(paste("44: [",this.state,"]: generate the plot.\n",sep=""))
	### cat(paste("44: [",this.state,"]: generate the plot.\n",sep=""),file=FL.LOG,append=TRUE)	
	### 
	### dev.set(2)
	### plot(plot.obj)
	### 
	### 
	### # output to a jpeg file
	### count.plot <- count.plot + 1
	### FL.JPG <- paste(Path.Out.State,paste(this.state,"_figure",count.plot,".jpg",sep=""),sep="/")
	### jpeg(file = FL.JPG,width=12,height=8,units="in",res=1200,bg = "transparent")
	### dev.set(3)	
	### 	plot(plot.obj)
	### dev.off(3)
	### cat(paste("45: [",this.state,"]: plotted.\n",sep=""))
	### cat(paste("45: [",this.state,"]: plotted.\n",sep=""),file=FL.LOG,append=TRUE)	
	### 
	### 
	### ggsave("yulong_code_item_distribution_example.png",width=7,height=10)

	dev.off(2)
}

# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n91. 07_plots.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n91. 07_plots.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\n92. Processing time for [07_plots.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\n92. Processing time for [07_plots.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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





