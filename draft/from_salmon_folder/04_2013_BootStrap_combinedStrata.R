# ***********************************************************
# 04_2013_BootStrap_combinedStrata.R
# ***********************************************************
# October 1, 2013: 
# Based on "2012_HanfordReach_StrandingSurvey_BootStrap_combinedStrata_revised.R" under "C:\YuLong_Projects\FY2012_SalmonStrandingFieldSurvey_2012March_June\0_scripts\".
# ***********************************************************
# October 1st, 2012: received strata combining information from Chris
# --------------------------------------------------------------------------------------------
# The combinations are:
# --------------------------------------------------------------------------------------------
# Stratum 1:	3 strata including (section1|2|3_Fen27_Mar12)
# Stratum 2:	2 strata including (section1|2  _Mar13-Mar26)
# Stratum 3:	1 strata including (section3    _Mar13-Mar26)
# Stratum 4:	2 strata including (section1|2  _Mar27_Apr9)
# Stratum 5:	1 strata including (section3    _Mar27_Apr9)
# Stratum 6:	1 strata including (section1    _Apr10_Apr23)
# Stratum 7:	1 strata including (section2    _Apr10_Apr23)
# Stratum 8:	1 strata including (section3    _Apr10_Apr23)
# Stratum 9:	3 strata including (section1|2|3_Apr24_May07)
# Stratum 10:	3 strata including (section1|2|3_May08_May21)
# Stratum 11:	1 strata including (section1    _May22_Jun4)
# Stratum 12:	1 strata including (section2    _May22_Jun4)
# Stratum 13:	1 strata including (section3    _May22_Jun4)
# --------------------------------------------------------------------------------------------
#
# Sep 20, 2012: Modified on "2011_HanfordReach_StrandingSurvey_BootStrap_combinedStrata_revised.R"
#
# February 17, 2012: Discussed with Chris on Feb 15, 2012.  
# To be consistent with the entrapment analysis, betteer to leave the first two-week period without stranding mortality.
# so split the original combined stratum 1 (four weeks and three river section) into two strata
# so wqe will have 11 instead of combined strata
#
#
# start to code on Feb 1, 2012: 
# 	why: there are no single stranded fish in 14 out of the 24 (3 river section and 8 biweek period) strata.
#            0 straded fish may not make sense
#            combined adjacent spatial and temporal strata to ensure non-zero fish in the combined strata.
# The combinations are:
# --------------------------------------------------------------------------------------------
# Stratum 1:	3 strata including (section1|2|3_Mar1-Mar14)
# Stratum 2:	3 strata including (section1|2|3_Mar15-Mar28)
# Stratum 3:	6 strata including (section1|2|3_Mar29-Apr11) and (section1|2|3_Apr12-Apr25)
# Stratum 4:	1 strata including (section1_Apr26-May9)
# Stratum 5:	2 strata including (section2|3_Apr26-May9)
# Stratum 6:	2 strata including (section1|2|_May10-May23)
# Stratum 7:	1 strata including (section3_May10-May23)
# Stratum 8:	1 strata including (section1_May24-Jun6)
# Stratum 9:	1 strata including (section2_May24-Jun6)
# Stratum 10:	1 strata including (section3_May24-Jun6)
# Stratum 11:	3 strata including (section1|2|3_Jun7-Jun20)
# --------------------------------------------------------------------------------------------
#
# coded    on Sep 16, 2011 using hypothetical dewatered area data
# modified on Oct 26, 2011 to use real dewatered data received on Oct 26, 2011
# checked  on Nov 10, 2011
#
# two-stage sampling
#
# Primary units:   transect from 1 to 360.  The transects are the boundary lines of the quadrants.
# Secondary units: plots along each transect
#
# section	segment	lower boundary	upper boundary	site	site idx	transects idx	tot sites	
# upper		1	620		635		15	 1-15		  1-60
# upper		2	605		620		15	16-30		 61-120
# --------------------------------------------------------------------------------------------	30
# middle 	3 	595		605		10	31-40		121-160
# middle 	4 	588		595		7	41-47		161-188
# middle 	5 	581		588		7	48-54		189-216
# middle 	6 	575		581		6	55-60		217-240
# --------------------------------------------------------------------------------------------	30
# lower		7 	558		575		17	61-77		241-308
# lower 	8 	545		558		13	78-90		309-360
# --------------------------------------------------------------------------------------------	30
#
# option 5: 
# draw n.k transect from the n.k transects surveyed in the k-th stratum
# draw m.ik from the m.ik plots (if m.ik >= 3) or from the sum(m.ik) (with i from 1 to m.k) plots with replacement (if m.ik<3) 
# -------------------------------------------------------------------------------------------------
#
# methodology description:
# I. quantity description
# -------------------------------------------------------------------------------------------------
# strata:                                                                                        					k = 1,..., K
# Number of primary   unit (transect) for the k-th strata: N_k                       but the actually sampled primary   unit: n_k:	i = 1,..., n_k  (out of N_k)
# Number of secondary unit (plot)     for the i-th transect of the k-th strata: M_ik but the actually sampled secondary unit: m_ik:    	j = 1,..., m_ik (out of M_ik)
# the count of the stranded fish in the sampled plots (from field survey):						y_ijk, j=1,...,m_ik, i=1,...,n_k, k=1,...,K
# the dewatered area of the sampled plots (from field survey):								a_ijk, j=1,...,m_ik, i=1,...,n_k, k=1,...,K
#
# the dewatered area of the i-th primary unit of the k-th strata (from model):						A_ik (i=1,...,N_k),          the overall dewatered area of the i_th transect of the k_th strata, across all M_ik plots of the i-th transect.
# the dewatered area                          of the k-th strata (from model):						A_k = sum(i=1 to N_k)[A_ik], the overall dewatered area                      of the k-th strata, across all N_k transects                      
#
# II. estimate in the primary unit
# -------------------------------------------------------------------------------------------------
# The estimate of mean stranded fish rate (#/area) of the i-th transect of the k-th strata from the sampled data:	r_ik_hat =        sum(j=1 to m_ik)[y_ijk] / sum(j=1 to m_ik)[a_ijk]
# The estimate of mean stranded fish               of the i-th transect of the k-th strata: 				y_ik_hat = A_ik * sum(j=1 to m_ik)[y_ijk] / sum(j=1 to m_ik)[a_ijk]. Note: sample estimate is expanded by A_ik
#
# III. estimate in the strata
# -------------------------------------------------------------------------------------------------
# The estimate of mean stranded fish rate (#/area)                      of the k-th strata from the sampled data:	r_k_hat  =        sum(i=1 to n_k)[y_ik_hat] / sum(i=1 to n_k)[A_ik]
# The estimate of mean stranded fish                                    of the k-th strata: 				y_k_hat  = A_k  * sum(i=1 to n_k)[y_ik_hat] / sum(i=1 to n_k)[A_ik]. Note: sample estimate is expanded by A_k
#
# IV. estimate of the entire reach
# -------------------------------------------------------------------------------------------------
# The estimate of mean stranded fish of the entire Hanford Reach							y_hat    = sum(k=1 to K)[y_k_hat], k=1,...,K
#
#
# How to sum two vectors element by element but ignore the missing value?
#
# eliminate all stuff
rm(list = ls(all = TRUE))


# -------------------------------------------------------------------------------------------------
# 1. preparing
# -------------------------------------------------------------------------------------------------
# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}


# today's month, day and year in the format of "Thu Jun 16 08:48:36 2011", 5 fields separated by space
today.month  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
today.day    <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
today.year   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]
today.hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
today.minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
today.second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]


# -------------------------------------------------------------------------------------------------
# 2. change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2013_SalmonStrandingFieldSurvey/FY2013_SalmonStranding_Analysis/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2013_SalmonStrandingFieldSurvey/FY2013_SalmonStranding_Analysis/0_scripts"
}
setwd(Path.Current)

# -------------------------------------------------------------------------
# 3. load stuff defined in the "functions_strandingAnalysis.R"
# -------------------------------------------------------------------------------------------------
source(paste(Path.Current,"functions_strandingAnalysis.R",sep="/"))



# -------------------------------------------------------------------------------------------------
# 4. setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.data.in <- "../../Data_Received/data_2013_stranding_entrapment_fieldSurvey"		
Path.area.in <- "../02_DewateredArea_Flow2013"
Path.out     <- "../04_2013_BootStrap_combinedStrata"	
Path.log     <- "../0_log"								
if (!file.exists(Path.data.in)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.area.in)){print(paste("NOT existing:",Path.area.in));dir.create(Path.area.in,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.out))    {print(paste("NOT existing:",Path.out));    dir.create(Path.out,    showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));    dir.create(Path.log,    showWarnings=TRUE,recursive=TRUE)}



# ------------------------------------------------------------------------------------------------- 
# 5. define input data files
# ------------------------------------------------------------------------------------------------- 
FL.data.in <- paste(Path.data.in,"Stranding Data 2013_YLX.csv",     sep="/")		
FL.area.in <- paste(Path.area.in,"02_DewateredArea_Flow2013.Rdata", sep="/")			
if (!file.exists(FL.data.in)){print(paste("field stranding data file \"",FL.data.in," \"does not exist. Check why!"))}
if (!file.exists(FL.area.in)){print(paste("dewatered area data file \"", FL.area.in," \"does not exist. Check why!"))}


# ------------------------------------------------------------------------------------------------- 
# 6. create a LOG file 
# ------------------------------------------------------------------------------------------------- 
FL.LOG  <- paste(Path.log,"04_2013_BootStrap_combinedStrata.log",sep="/")	# OUTPUT Log file
FL.DIAG <- paste(Path.log,"04_2013_BootStrap_combinedStrata.diag",sep="/")	# OUTPUT diagnosis file
FL.DBG  <- paste(Path.log,"04_2013_BootStrap_combinedStrata.debug",sep="/")	# OUTPUT debug file
if (file.exists(FL.LOG)) {print(paste(FL.LOG, "exist.Delete it!"));file.remove(FL.LOG)}
if (file.exists(FL.DIAG)){print(paste(FL.DIAG,"exist.Delete it!"));file.remove(FL.DIAG)}
if (file.exists(FL.DBG)) {print(paste(FL.DBG, "exist.Delete it!"));file.remove(FL.DBG)}

# ------------------------------------------------------------------------------------------------- 
# 7. load libraries
# -------------------------------------------------------------------------------------------------
# library("psych")	# use the skew function of this package
library("lattice")
library("reshape")
library("chron")
library("cwhmisc")	# used for remove duplicate rows in a data frame
library(RODBC)
library(graphics)
# library(gplots)
# library("locfit")
# library("boot")
cat(paste("loaded necessary libraries","\n",sep=""))
cat(paste("loaded necessary libraries","\n",sep=""),file=FL.LOG,append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# 8. create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
# output related to direct input 
FL.rawData.out   <- paste(Path.out,"04_2013_BootStrap_combinedStrata_rawData.csv",sep="/")		# OUTPUT raw stranding data file
FL.rawArea.out   <- paste(Path.out,"04_2013_BootStrap_combinedStrata_rawArea.csv",sep="/")		# OUTPUT raw dewatered area file
FL.rawStat.out   <- paste(Path.out,"04_2013_BootStrap_combinedStrata_rawStat.csv",sep="/")		# OUTPUT data statistics file
FL.boot.out      <- paste(Path.out,"04_2013_BootStrap_combinedStrata_bootstrap.csv",sep="/")		# OUTPUT data statistics file
FL.boot.obj      <- paste(Path.out,"04_2013_BootStrap_combinedStrata_bootstrap.Rdata",sep="/")		# OUTPUT data statistics file
FL.pdf.out       <- paste(Path.out,"04_2013_BootStrap_combinedStrata.pdf",sep="/")			# OUTPUT statistics tables, one for each individual quantity
FL.procData.out  <- paste(Path.out,"04_2013_BootStrap_combinedStrata_procData.csv",sep="/")		# OUTPUT statistics tables, one for each individual quantity
if (file.exists(FL.rawData.out))   {print(paste(FL.rawData.out,"exist.Delete it!"));   file.remove(FL.rawData.out)}
if (file.exists(FL.rawArea.out))   {print(paste(FL.rawArea.out,"exist.Delete it!"));   file.remove(FL.rawArea.out)}
if (file.exists(FL.rawStat.out))   {print(paste(FL.rawStat.out,"exist.Delete it!"));   file.remove(FL.rawStat.out)}
if (file.exists(FL.boot.obj))      {print(paste(FL.boot.obj,"exist.Delete it!"));      file.remove(FL.boot.obj)}
if (file.exists(FL.boot.out))      {print(paste(FL.boot.out,"exist.Delete it!"));      file.remove(FL.boot.out)}
if (file.exists(FL.pdf.out))       {print(paste(FL.pdf.out,"exist.Delete it!"));       file.remove(FL.pdf.out)}
if (file.exists(FL.procData.out))  {print(paste(FL.procData.out,"exist.Delete it!"));  file.remove(FL.procData.out)}
cat(paste("defined output file names\n",sep=""))
cat(paste("defined output file names\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 9a. define biweekly periods: the biweekly period supplied by Nino.  Start at Wednesdays (differ from FY2013 which started on Fridays)
# -------------------------------------------------------------------------------------------------
biweek.idx.label     <- c( 1,            2,            3,            4,            5,            6,            7,           8           )
biweek.idx.names     <- c("Feb27-Mar12","Mar13-Mar26","Mar27-Apr09","Apr10-Apr23","Apr24-May07","May08-May21","May22-Jun4","Jun05-Jun18")
biweek.idx.fullNames <- c("Feb27-Mar12","Mar13-Mar26","Mar27-Apr09","Apr10-Apr23","Apr24-May07","May08-May21","May22-Jun4","Jun05-Jun18")
names(biweek.idx.fullNames) <- biweek.idx.names

firstday.1st_biweek      <- chron(dates="02/27/2013",times="0:0:0",   format=c('m/d/y','h:m:s'))	# 
first.sampleDay          <- chron(dates="03/02/2013",times="0:0:0",   format=c('m/d/y','h:m:s'))	# the date of first observation in the database (stranding and entrapment data spreadsheet)
last.sampleDay           <- chron(dates="06/09/2013",times="23:59:59",format=c('m/d/y','h:m:s'))	# the date of last  observation in the database (stranding and entrapment data spreadsheet) 
cat(paste("specified biweekly periods designation!","\n",sep=""))


# -------------------------------------------------------------------------------------------------
# 9b. setup output and log directory
# -------------------------------------------------------------------------------------------------
no.strata.comb <- 14
      strata.combs  <- paste("set",seq(1,no.strata.comb),sep="")
names(strata.combs) <- c("Feb27_Mar12_sec123","Mar13_Mar26_sec12","Mar13_Mar26_sec3","Mar27_Apr9_sec12","Mar27_Apr9_sec3","Apr10_Apr23_sec1","Apr10_Apr23_sec2","Apr10_Apr23_sec3","Apr24_May7_sec123","May8_May21_sec123","May22_June4_sec1","May22_June4_sec2","May22_June4_sec3","June5_June18_sec123")




# *************************************************************************************************
# 11. load dewatered area file:
# dewatered data from model is created by processing the dewatered data received and saved in a R object file
# the dewatered data are saved in the data frames [myArea.transect.long],[myArea.transect.wide],[myArea.section.long] and [myArea.section.wide]
# *************************************************************************************************
load(FL.area.in)
cat(paste("11a. loaded the model dewatered area data from [",FL.area.in,"]\n",sep=""))
cat(paste("11a. loaded the model dewatered area data from [",FL.area.in,"]\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# prepare dewatered area on the irregular strata: October 1st, 2012
# -------------------------------------------------------------------------------------------------
strata.comb <- rep("NA",dim(myArea.transect.long)[1])	# initialize an array for the strata.comb

# stratum 1: 
index1 <- (myArea.transect.long[,"biweek.idx"] == "Feb27-Mar12")
strata.comb[index1] <- "Feb27_Mar12_sec123"

# stratum 2: 
index2 <- (myArea.transect.long[,"biweek.idx"] == "Mar13-Mar26")  &  ((myArea.transect.long[,"section"] == 1) | (myArea.transect.long[,"section"] == 2))
strata.comb[index2] <- "Mar13_Mar26_sec12"
if(length(setdiff(index1,index2)) > 0){stop("stratum 1 and stratum 2 are not be mutural exclusive\n")}

# stratum 3: 
index3 <- (myArea.transect.long[,"biweek.idx"] == "Mar13-Mar26")  &  ((myArea.transect.long[,"section"] == 3))
strata.comb[index3] <- "Mar13_Mar26_sec3"
if(length(setdiff(index2,index3)) > 0){stop("stratum 2 and stratum 3 are not be mutural exclusive\n")}

# stratum 4: 
index4 <- (myArea.transect.long[,"biweek.idx"] == "Mar27-Apr09")  &  ((myArea.transect.long[,"section"] == 1) | (myArea.transect.long[,"section"] == 2))
strata.comb[index4] <- "Mar27_Apr9_sec12"
if(length(setdiff(index3,index4)) > 0){stop("stratum 3 and stratum 4 are not be mutural exclusive\n")}

# stratum 5: 
index5 <- (myArea.transect.long[,"biweek.idx"] == "Mar27-Apr09")  &  ((myArea.transect.long[,"section"] == 3))
strata.comb[index5] <- "Mar27_Apr9_sec3"
if(length(setdiff(index4,index5)) > 0){stop("stratum 4 and stratum 5 are not be mutural exclusive\n")}

# stratum 6:
index6 <- (myArea.transect.long[,"biweek.idx"] == "Apr10-Apr23") & (myArea.transect.long[,"section"] == 1)
strata.comb[index6] <- "Apr10_Apr23_sec1"
if(length(setdiff(index5,index6)) > 0){stop("stratum 5 and stratum 6 are not be mutural exclusive\n")}

# stratum 7:
index7 <- (myArea.transect.long[,"biweek.idx"] == "Apr10-Apr23") &  (myArea.transect.long[,"section"] == 2)
strata.comb[index7] <- "Apr10_Apr23_sec2"
if(length(setdiff(index6,index7)) > 0){stop("stratum 6 and stratum 7 are not be mutural exclusive\n")}

# stratum 8:
index8 <- (myArea.transect.long[,"biweek.idx"] == "Apr10-Apr23") &  (myArea.transect.long[,"section"] == 3)
strata.comb[index8] <- "Apr10_Apr23_sec3"
if(length(setdiff(index7,index8)) > 0){stop("stratum 7 and stratum 8 are not be mutural exclusive\n")}

# stratum 9:
index9 <- (myArea.transect.long[,"biweek.idx"] == "Apr24-May07") & ((myArea.transect.long[,"section"] == 1) | (myArea.transect.long[,"section"] == 2) | (myArea.transect.long[,"section"] == 3))
strata.comb[index9] <- "Apr24_May7_sec123"
if(length(setdiff(index8,index9)) > 0){stop("stratum 8 and stratum 9 are not be mutural exclusive\n")}

# stratum 10:
index10 <- (myArea.transect.long[,"biweek.idx"] == "May08-May21") & ((myArea.transect.long[,"section"] == 1) | (myArea.transect.long[,"section"] == 2) | (myArea.transect.long[,"section"] == 3))
strata.comb[index10] <- "May8_May21_sec123"
if(length(setdiff(index9,index10)) > 0){stop("stratum 9 and stratum 10 are not be mutural exclusive\n")}

# stratum 11:
index11 <- (myArea.transect.long[,"biweek.idx"] == "May22-Jun4")  & (myArea.transect.long[,"section"] == 1)
strata.comb[index11] <- "May22_June4_sec1"
if(length(setdiff(index10,index11)) > 0){stop("stratum 10 and stratum 11 are not be mutural exclusive\n")}

# stratum 12:
index12 <- (myArea.transect.long[,"biweek.idx"] == "May22-Jun4")  & (myArea.transect.long[,"section"] == 2)
strata.comb[index12] <- "May22_June4_sec2"
if(length(setdiff(index11,index12)) > 0){stop("stratum 11 and stratum 12 are not be mutural exclusive\n")}

# stratum 13:
index13 <- (myArea.transect.long[,"biweek.idx"] == "May22-Jun4")  & (myArea.transect.long[,"section"] == 3)
strata.comb[index13] <- "May22_June4_sec3"
if(length(setdiff(index12,index13)) > 0){stop("stratum 12 and stratum 13 are not be mutural exclusive\n")}

# stratum 14:
index14 <- (myArea.transect.long[,"biweek.idx"] == "Jun05-Jun18")  & ((myArea.transect.long[,"section"] == 1) | (myArea.transect.long[,"section"] == 2) | (myArea.transect.long[,"section"] == 3))
strata.comb[index14] <- "June5_June18_sec123"
if(length(setdiff(index13,index14)) > 0){stop("stratum 13 and stratum 14 are not be mutural exclusive\n")}
cat(paste("11b. prepared dewatered area data for irregular strata.\n",sep=""))
cat(paste("11b. prepared dewatered area data for irregular strata.\n",sep=""),file=FL.LOG,append=TRUE)


myArea.transect.long  <- cbind(myArea.transect.long,
                         strata.comb = as.character(strata.comb))	# add a combined strata field                        
rm(strata.comb,index1,index2,index3,index4,index5,index6,index7,index8,index9,index10,index11,index12,index13,index14)
cat(paste("11c. appended [myArea.transect.long] with combined strata.\n",sep=""))
cat(paste("11c. appended [myArea.transect.long] with combined strata.\n",sep=""),file=FL.LOG,append=TRUE)

#
# this new irregular strata label will lead to multiple occurrence of "transect" in a given transect due to the strata merging.
#
      myArea  <-  aggregate(myArea.transect.long[,"area.dewatered.m2"],list(myArea.transect.long[,"transect"],myArea.transect.long[,"strata.comb"]),sum,na.rm=TRUE)
names(myArea) <- c("transect","strata.comb","area.dewatered.m2")
cat(paste("11d. dewatered area has been read from [",FL.area.in,"] and recalculated for the irregular strata\n",sep=""))
cat(paste("11d. dewatered area has been read from [",FL.area.in,"] and recalculated for the irregular strata\n",sep=""),file=FL.LOG,append=TRUE)



# *************************************************************************************************
# 12. read in survey data
# *************************************************************************************************
# read in field stranding data
      myData.input  <- read.csv(file=FL.data.in,header=TRUE,sep=",",stringsAsFactors=FALSE)
names(myData.input) <- c("sampled","date","section","segment","transect","flowBand","time","wayPoint","north","east","substrate.dom","substrate.subDom","embedded","vegetation","area.sampled","morts","NoPM","Stickleback","SculpiNo","Dace","Sucker","Site")
cat(paste("12a. read in 2013 field stranding data from [",FL.data.in,"]\n",sep=""))
cat(paste("12a. read in 2013 field stranding data from [",FL.data.in,"]\n",sep=""),file=FL.LOG,append=TRUE)


# sampled has values of "Yes", "Yes", "NO DATA": seems like not necessary since I already made the changes in the original data file
myData.input[!is.na(myData.input[,"sampled"]) & (myData.input[,"sampled"]=="yes" | myData.input[,"sampled"] == "Yes"),"sampled"] <- "Yes"
myData.input[!is.na(myData.input[,"sampled"]) & (myData.input[,"sampled"]=="No"  | myData.input[,"sampled"] == "no"),"sampled"]  <- "No"
myData.input <- myData.input[myData.input[,"sampled"] == "Yes" | myData.input[,"sampled"] == "No",]	# filter the sample marked as "NO DATA"
cat(paste("12b. remove records with value other than \"Yes\" or \"No\"  or \"Other Species\" in the \"sampled\" field\n",sep=""))
cat(paste("12b. remove records with value other than \"Yes\" or \"No\"  or \"Other Species\" in the \"sampled\" field\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# check the difference between "Y" "sampled" and non-zero "area.sampled": to make sure the sampled "Y" records are exact the same as those records with non-zero area
FL.tmp1 <- paste(Path.log,"tmp1.csv",sep="/")
FL.tmp2 <- paste(Path.log,"tmp2.csv",sep="/")
tmp.A <- myData.input[(!is.na(myData.input[,"sampled"])  & (myData.input[,"sampled"]=="Yes")),]
tmp.B <- myData.input[(!is.na(myData.input[,"area.sampled"]) & (myData.input[,"area.sampled"] > 0)),]	 
if(setequal(tmp.A,tmp.B)){
	cat(paste("12c. all sampled plots has a non-zero dewatered area.  OK!\n",sep=""))
	cat(paste("12c. all sampled plots has a non-zero dewatered area.  OK!\n",sep=""),file=FL.LOG,append=TRUE)	
}else{
	cat(paste("12c. there are ",dim(tmp.A)[1] - dim(tmp.B)[1]," sampled plots have a zero dewatered area.  Check it!\n",sep=""))
	cat(paste("12c. there are ",dim(tmp.A)[1] - dim(tmp.B)[1]," sampled plots have a zero dewatered area.  Check it!\n",sep=""),file=FL.LOG,append=TRUE)	
	if (file.exists(FL.tmp1)){print(paste(FL.tmp1," exist.Delete it!"));file.remove(FL.tmp1)}
	if (file.exists(FL.tmp2)){print(paste(FL.tmp2," exist.Delete it!"));file.remove(FL.tmp2)}
	write.table(tmp.A,file=FL.tmp1,sep=",",row.names=TRUE,col.names =TRUE, append=TRUE)
	write.table(tmp.B,file=FL.tmp2,sep=",",row.names=TRUE,col.names =TRUE, append=TRUE)
	cat(paste("12c. it is supposed to have non-zero area for all records with \"Y\" for the \"sampled\" field.  Check [",FL.tmp1,"] and [",FL.tmp2,"] to verify\n",sep=""))
	cat(paste("12c. it is supposed to have non-zero area for all records with \"Y\" for the \"sampled\" field.  Check [",FL.tmp1,"] and [",FL.tmp2,"] to verify\n",sep=""),file=FL.LOG,append=TRUE)	
}
cat(paste("12c. A quick test has been conducted!\n",sep=""))
cat(paste("12c. A quick test has been conducted!\n",sep=""),file=FL.LOG,append=TRUE)	


# -------------------------------------------------------------------------------------------------
# 13. keep only the surveyed data: i.e., plot sample with non-zero area
# -------------------------------------------------------------------------------------------------
myData <- subset(myData.input,subset = area.sampled>0,c("date","time","section","segment","transect","flowBand","area.sampled","morts"))	# keep only fields needed
cat(paste("13a. retained only samples with non-zero survey area in [myData]\n",sep=""))
cat(paste("13a. retained only samples with non-zero survey area in [myData]\n",sep=""),file=FL.LOG,append=TRUE)

# assign 0 to missing values in the fish fields
myData[is.na(myData[,"morts"]), "morts"]  <- 0
cat(paste("13b. assigned 0 to NA entries in \"morts\" fields\n",sep=""))
cat(paste("13b. assigned 0 to NA entries in \"morts\" fields\n",sep=""),file=FL.LOG,append=TRUE)

# added a morts variable which is the sum of dead and alive fishes
myData <- cbind(myData,binary = ifelse(myData[,"morts"]>0,1,0))	# just flag the number of non-zero stranding samples
cat(paste("13c. added a \"binary\" field which is the binary representation of the morts (Chinook found)\n",sep=""))
cat(paste("13c. added a \"binary\" field which is the binary representation of the morts (Chinook found)\n",sep=""),file=FL.LOG,append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# 14a. split the date into year, month and day, and added a date.chron field
# -------------------------------------------------------------------------------------------------
tmp   <- unlist(strsplit(myData[,"date"],"/"))
month <- as.numeric(tmp[seq(from=1,to=length(tmp),by=3)])		# [day]
day   <- as.numeric(tmp[seq(from=2,to=length(tmp),by=3)])		# [month]
year  <- as.numeric(tmp[seq(from=3,to=length(tmp),by=3)])		# [year]

tmp   <- unlist(strsplit(myData[,"time"],":"))
hour  <- as.numeric(tmp[seq(from=1,to=length(tmp),by=2)])		# [hour]
minute<- as.numeric(tmp[seq(from=2,to=length(tmp),by=2)])		# [minute]



date.chron <- chron(dates = paste(month,day,year,sep="/"),
                    times = paste(hour,minute,rep("0",length(day)),sep=":"))
myData <- cbind(myData,month = month, day = day,year = year,date.chron = date.chron)
cat(paste("14a. added a \"date.chron\" and year, month and day fields\n",sep=""))
cat(paste("14a. added a \"date.chron\" and year, month and day fields\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 14b. add a bi-week index.  Note: Friday, March 2th, 2013 is the first sampling date and Feb 27, 2013 was the first day of the first biweekly period [firstday.1st_biweek]
# -------------------------------------------------------------------------------------------------
myData <- data.frame(myData,								
	      	     biweek.idx = as.numeric(ceiling((myData[,"date.chron"] - firstday.1st_biweek + (1/60)/24)/14)))		# plus (5/60)/48 which is half of a 5 minute interval 


myData[,"biweek.idx"] <- factor(myData[,"biweek.idx"],levels=biweek.idx.label,labels=biweek.idx.names,ordered=TRUE)		# convert week index from number to string like Nov9, etc
cat(paste("14b. added a \"biweek.idx\" field by considering Wednesday Feb 27, 2013 was the first day of the first biweekly period [firstday.1st_biweek]!\n",sep=""))
cat(paste("14b. added a \"biweek.idx\" field by considering Wednesday Feb 27, 2013 was the first day of the first biweekly period [firstday.1st_biweek]!\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 15. reassign "site", "segment", "section" based on "transect" because there are some errors in the "segment" designation was found in the spreadsheets.  (Mar 30, 2013, transect 115 was assigned to segment 3 instead of segment 2).
# -------------------------------------------------------------------------------------------------
# re-assign "site" from "transect" assuming "transect" in the database is accurate.
myData[,"site.new"] <- (myData[,"transect"]-1)%/%4 + 1
cat(paste("14a. added a [site.new] field\n",sep=""))
cat(paste("14a. added a [site.new] field\n",sep=""),file=FL.LOG,append=TRUE)

# re-assign "segment" from "site" assuming "transect" in the database is accurate.
myData[,"segment.new"] <- rep(0,dim(myData)[1])
myData[(myData[,"site.new"] >=  1 & myData[,"site.new"] <= 15),"segment.new"] <- 1
myData[(myData[,"site.new"] >= 16 & myData[,"site.new"] <= 30),"segment.new"] <- 2

myData[(myData[,"site.new"] >= 31 & myData[,"site.new"] <= 40),"segment.new"] <- 3
myData[(myData[,"site.new"] >= 41 & myData[,"site.new"] <= 47),"segment.new"] <- 4
myData[(myData[,"site.new"] >= 48 & myData[,"site.new"] <= 54),"segment.new"] <- 5
myData[(myData[,"site.new"] >= 55 & myData[,"site.new"] <= 60),"segment.new"] <- 6

myData[(myData[,"site.new"] >= 61 & myData[,"site.new"] <= 77),"segment.new"] <- 7
myData[(myData[,"site.new"] >= 78 & myData[,"site.new"] <= 90),"segment.new"] <- 8
cat(paste("14b. added a [segment.new] field based on [site.new] which is based on [transect].\n",sep=""))
cat(paste("14b. added a [segment.new] field based on [site.new] which is based on [transect].\n",sep=""),file=FL.LOG,append=TRUE)

# re-assign "section" from "segment" assuming "transect" in the database is accurate.
myData[,"section.new"] <- rep(0,dim(myData)[1])
myData[(myData[,"segment.new"] >= 1 & myData[,"segment.new"] <= 2),"section.new"] <- 1
myData[(myData[,"segment.new"] >= 3 & myData[,"segment.new"] <= 6),"section.new"] <- 2
myData[(myData[,"segment.new"] >= 7 & myData[,"segment.new"] <= 8),"section.new"] <- 3
cat(paste("15. added a [section.new] which is based on [segment.new] which is based on [site.new] which is based on [transect].\n",sep=""))
cat(paste("15. added a [section.new] which is based on [segment.new] which is based on [site.new] which is based on [transect].\n",sep=""),file=FL.LOG,append=TRUE)







# -------------------------------------------------------------------------------------------------
# 16. add a "strata" field:  by concanating the temporal strata field and the spaital strata field with a "_"
# -------------------------------------------------------------------------------------------------
myData  <- cbind(myData,
                 strata = as.character(paste(as.character(myData[,"biweek.idx"]),paste("section.new",myData[,"section.new"],sep=""),sep="_")))	# add a combined strata field
cat(paste("16. added a \"strata\" field which is the concatenation of biweek index and river section\n",sep=""))
cat(paste("16. added a \"strata\" field which is the concatenation of biweek index and river section\n",sep=""),file=FL.LOG,append=TRUE)




# -------------------------------------------------------------------------------------------------
# 17. ASSIGN IRREGULAR STRATA INDEX TO THE DATA: October 1st, 2012
# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
# prepare dewatered area on the irregular strata: October 1st, 2012
# -------------------------------------------------------------------------------------------------
strata.comb <- rep("NA",dim(myData)[1])	# initialize an array for the strata.comb


# stratum 1: 
index1 <- (myData[,"biweek.idx"] == "Feb27-Mar12")
strata.comb[index1] <- "Feb27_Mar12_sec123"

# stratum 2: 
index2 <- (myData[,"biweek.idx"] == "Mar13-Mar26")  &  ((myData[,"section.new"] == 1) | (myData[,"section.new"] == 2))
strata.comb[index2] <- "Mar13_Mar26_sec12"
if(length(setdiff(index1,index2)) > 0){stop("stratum 1 and stratum 2 are not be mutural exclusive\n")}

# stratum 3: 
index3 <- (myData[,"biweek.idx"] == "Mar13-Mar26")  &  ((myData[,"section.new"] == 3))
strata.comb[index3] <- "Mar13_Mar26_sec3"
if(length(setdiff(index2,index3)) > 0){stop("stratum 2 and stratum 3 are not be mutural exclusive\n")}

# stratum 4: 
index4 <- (myData[,"biweek.idx"] == "Mar27-Apr09")  &  ((myData[,"section.new"] == 1) | (myData[,"section.new"] == 2))
strata.comb[index4] <- "Mar27_Apr9_sec12"
if(length(setdiff(index3,index4)) > 0){stop("stratum 3 and stratum 4 are not be mutural exclusive\n")}

# stratum 5: 
index5 <- (myData[,"biweek.idx"] == "Mar27-Apr09")  &  ((myData[,"section.new"] == 3))
strata.comb[index5] <- "Mar27_Apr9_sec3"
if(length(setdiff(index4,index5)) > 0){stop("stratum 4 and stratum 5 are not be mutural exclusive\n")}


# stratum 6:
index6 <- (myData[,"biweek.idx"] == "Apr10-Apr23") & (myData[,"section.new"] == 1)
strata.comb[index6] <- "Apr10_Apr23_sec1"
if(length(setdiff(index5,index6)) > 0){stop("stratum 5 and stratum 6 are not be mutural exclusive\n")}

# stratum 7:
index7 <- (myData[,"biweek.idx"] == "Apr10-Apr23") &  (myData[,"section.new"] == 2)
strata.comb[index7] <- "Apr10_Apr23_sec2"
if(length(setdiff(index6,index7)) > 0){stop("stratum 6 and stratum 7 are not be mutural exclusive\n")}

# stratum 8:
index8 <- (myData[,"biweek.idx"] == "Apr10-Apr23") &  (myData[,"section.new"] == 3)
strata.comb[index8] <- "Apr10_Apr23_sec3"
if(length(setdiff(index7,index8)) > 0){stop("stratum 7 and stratum 8 are not be mutural exclusive\n")}

# stratum 9:
index9 <- (myData[,"biweek.idx"] == "Apr24-May07") & ((myData[,"section.new"] == 1) | (myData[,"section.new"] == 2) | (myData[,"section.new"] == 3))
strata.comb[index9] <- "Apr24_May7_sec123"
if(length(setdiff(index8,index9)) > 0){stop("stratum 8 and stratum 9 are not be mutural exclusive\n")}

# stratum 10:
index10 <- (myData[,"biweek.idx"] == "May08-May21") & ((myData[,"section.new"] == 1) | (myData[,"section.new"] == 2) | (myData[,"section.new"] == 3))
strata.comb[index10] <- "May8_May21_sec123"
if(length(setdiff(index9,index10)) > 0){stop("stratum 9 and stratum 10 are not be mutural exclusive\n")}

# stratum 11:
index11 <- (myData[,"biweek.idx"] == "May22-Jun4")  & (myData[,"section.new"] == 1)
strata.comb[index11] <- "May22_June4_sec1"
if(length(setdiff(index10,index11)) > 0){stop("stratum 10 and stratum 11 are not be mutural exclusive\n")}

# stratum 12:
index12 <- (myData[,"biweek.idx"] == "May22-Jun4")  & (myData[,"section.new"] == 2)
strata.comb[index12] <- "May22_June4_sec2"
if(length(setdiff(index11,index12)) > 0){stop("stratum 11 and stratum 12 are not be mutural exclusive\n")}

# stratum 13:
index13 <- (myData[,"biweek.idx"] == "May22-Jun4")  & (myData[,"section.new"] == 3)
strata.comb[index13] <- "May22_June4_sec3"
if(length(setdiff(index12,index13)) > 0){stop("stratum 12 and stratum 13 are not be mutural exclusive\n")}

# stratum 14:
index14 <- (myData[,"biweek.idx"] == "Jun05-Jun18")  & ((myData[,"section.new"] == 1) | (myData[,"section.new"] == 2) | (myData[,"section.new"] == 3))
strata.comb[index14] <- "June5_June18_sec123"
if(length(setdiff(index13,index14)) > 0){stop("stratum 13 and stratum 14 are not be mutural exclusive\n")}



myData  <- cbind(myData,
                         strata.comb = as.character(strata.comb))	# add a combined strata field                        
rm(strata.comb,index1,index2,index3,index4,index5,index6,index7,index8,index9,index10,index11,index12,index13,index14)
cat(paste("17. assigned the irregular strata index to the data\n",sep=""))
cat(paste("17. assigned the irregular strata index to the data\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 18a. output the processed dewatered area data (for checking purpose)
# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
# [myData] 		 is appended with the irregular strata label
# [myArea.transect.long] has been appended with the irregular strata names
# [myArea] 	 	 is an aggregated version of [myArea.transect.long"] aggregated on multiple trasects occurred during the strata combinations
# -------------------------------------------------------------------------------------------------
cat("processed area,",  file=FL.procData.out,append=TRUE)
write.table(myArea,     file=FL.procData.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
cat("\nprocessed data,",file=FL.procData.out,append=TRUE)
write.table(myData,     file=FL.procData.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
cat(paste("18a. the pre-processed raw 2013 Chinook stranding survey data has been outputted to \"",FL.rawData.out,"\"\n",sep=""))
cat(paste("18a. the pre-processed raw 2013 Chinook stranding survey data has been outputted to \"",FL.rawData.out,"\"\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 18b. output the processed raw data (for checking purpose)
# -------------------------------------------------------------------------------------------------
cat(paste("2012 Stranding data,",sep=""),file=FL.rawData.out,append=TRUE)
write.table(myData,file=FL.rawData.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
cat(paste("18b. the pre-processed raw 2012 Chinook stranding survey data has been outputted to \"",FL.rawData.out,"\"\n",sep=""))
cat(paste("18b. the pre-processed raw 2012 Chinook stranding survey data has been outputted to \"",FL.rawData.out,"\"\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 18c. output the model dewatered area data used in this script (for checking purpose)
# -------------------------------------------------------------------------------------------------
cat(paste("dewatered area on regular strata,",sep=""),file=FL.rawArea.out,append=TRUE)
write.table(myArea.transect.long,file=FL.rawArea.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)

cat(paste("\ndewatered area on ir-regular strata,",sep=""),file=FL.rawArea.out,append=TRUE)
write.table(myArea,file=FL.rawArea.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
cat(paste("18c. the dewatered area used in the bootstrap has been outputted to \"",FL.rawArea.out,"\"!\n",sep=""))
cat(paste("18c. the dewatered area used in the bootstrap has been outputted to \"",FL.rawArea.out,"\"!\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# 19. Statistics of the data used for boot straping
# -------------------------------------------------------------------------------------------------
# statistics on overall data across all strata
   transect.visited.perStrata <- tapply(myData[,"transect"],list(myData[,"strata.comb"]), FUN=unique)			# get the list of the unique transect surveyed in each strata
no.transect.visited.perStrata <- data.frame(no.transect.visited.perStrata = sapply(transect.visited.perStrata,length))	# get the number of the unique surveyed transects
        list.strata.perStrata <-  row.names(no.transect.visited.perStrata)						# list of unique stratas occurred in this data set 
cat(paste("19a. statistics: put the actual transect visited in a list which will be used\n",sep=""))
cat(paste("19a. statistics: put the actual transect visited in a list which will be used\n",sep=""),file=FL.LOG,append=TRUE)

    no.plots.perStrata <- data.frame(no.plots.perStrata     = tapply(myData[,"morts"],     list(myData[,"strata.comb"]), FUN=length))		# all "no.sampled" in the sampled strata
area.sampled.perStrata <- data.frame(area.sampled.perStrata = tapply(myData[,"area.sampled"],  list(myData[,"strata.comb"]), FUN=sum,na.rm=TRUE))	# all "area.sampled"   in the sampled strata # Sep 20, 2012: replace "area" with "area.sampled"
    fish.all.perStrata <- data.frame(fish.all.perStrata     = tapply(myData[,"morts"],     list(myData[,"strata.comb"]), FUN=sum,na.rm=TRUE))	# all "morts"      in the sampled strata
cat(paste("19b. statistics: count area and fish in the sampled strata\n",sep=""))
cat(paste("19b. statistics: count area and fish in the sampled strata\n",sep=""),file=FL.LOG,append=TRUE)

# combine to a single PlotsSampled Table
myTable.perStrata <- cbind(no.transect.visited.perStrata = no.transect.visited.perStrata,
                                      no.plots.perStrata =            no.plots.perStrata,
		                  area.sampled.perStrata =        area.sampled.perStrata,
		                      fish.all.perStrata =            fish.all.perStrata) 

cat(paste("all strata,",sep=""),file=FL.rawStat.out,append=TRUE)
write.table(myTable.perStrata,file=FL.rawStat.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
cat(paste("\n",sep=""),file=FL.rawStat.out,append=TRUE)
cat(paste("19c. statistics: the statistics based on data of all strata are outputted to \"",FL.rawStat.out,"\"\n",sep=""))
cat(paste("19c. statistics: the statistics based on data of all strata are outputted to \"",FL.rawStat.out,"\"\n",sep=""),file=FL.LOG,append=TRUE)

# number of plots sampled per biweek per section
myNoPlots <- data.frame(tapply(myData[,"morts"],list(myData[,"biweek.idx"],myData[,"section.new"]),FUN=length))
names(myNoPlots) <- sub("X","Section",names(myNoPlots))
cat(paste("plots visited,",sep=""),file=FL.rawStat.out,append=TRUE)
write.table(myNoPlots,file=FL.rawStat.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
cat(paste("\n",sep=""),file=FL.rawStat.out,append=TRUE)
cat(paste("19d. statistics: number of plots sampled per biweekly periods per river section are outputted to \"",FL.rawStat.out,"\"\n",sep=""))
cat(paste("19d. statistics: number of plots sampled per biweekly periods per river section are outputted to \"",FL.rawStat.out,"\"\n",sep=""),file=FL.LOG,append=TRUE)

# numer of transects (primary units) sampled per biweek per section
tmp <- data.frame(tapply(myData[,"transect"],list(myData[,"biweek.idx"],myData[,"section.new"]),FUN=unique))

# there are 3 sections and 8 two-week period, make it total 24 strata
myNoTransect <- data.frame(array(24,c(8,3)))
for (i in 1:8)
{
	for (j in 1:3)
	{
		myNoTransect[i,j] <- length(unlist(tmp[i,j]))
	}
}
row.names(myNoTransect) <- row.names(tmp)
    names(myNoTransect) <- sub("X","Section",names(tmp))
cat(paste("transect visited,",sep=""),file=FL.rawStat.out,append=TRUE)
write.table(myNoTransect,file=FL.rawStat.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
cat(paste("\n",sep=""),file=FL.rawStat.out,append=TRUE)
cat(paste("19e. statistics: for all strata\n",sep=""))
cat(paste("19e. statistics: for all strata\n",sep=""),file=FL.LOG,append=TRUE)


# statistics on data of each individual strata
cat(paste("statistics: the following are statistics for each stratum\n",sep=""))
cat(paste("statistics: the following are statistics for each stratum\n",sep=""),file=FL.LOG,append=TRUE)

#
# 20. unique strata
#
unique.strata <- sort(unique(myData[,"strata.comb"]))
cat(paste("there are ",length(unique.strata)," unique irregular strata\n",sep=""))

# statistics of each stratum
for (this.strata in unique.strata)
{
	cat(paste("get the subset data of the current strata \"",this.strata,"\"!\n",sep=""))
	
	myData.thisStrata <- subset(myData,subset = strata.comb == this.strata)
	
	# 
	no.plots.perTransect            <- data.frame(no.plots.perTransect     = tapply(myData.thisStrata[,"morts"],     list(myData.thisStrata[,"transect"]), FUN=length))		# all "no.sampled" in the sampled strata
	area.sampled.perTransect        <- data.frame(area.sampled.perTransect = tapply(myData.thisStrata[,"area.sampled"],  list(myData.thisStrata[,"transect"]), FUN=sum,na.rm=TRUE))	# all "area.sampled"   in the sampled strata # Sep 20, 2012: replace "area" with "area.sampled"
	fish.all.perTransect            <- data.frame(fish.all.perTransect     = tapply(myData.thisStrata[,"morts"],     list(myData.thisStrata[,"transect"]), FUN=sum,na.rm=TRUE))	# all "morts"      in the sampled strata
	cat(paste("count area and fish in the sampled strata\n",sep=""))
	cat(paste("count area and fish in the sampled strata\n",sep=""),file=FL.LOG,append=TRUE)

	# combine to a single PlotsSampled Table
	myPlots.perTransect.perStrata <- cbind(	       no.plots.perTransect=     no.plots.perTransect,
					   area.sampled.perTransect        = area.sampled.perTransect,
					       fish.all.perTransect        =     fish.all.perTransect) 

	cat(paste(this.strata,",",sep=""),file=FL.rawStat.out,append=TRUE)
	write.table(myPlots.perTransect.perStrata,file=FL.rawStat.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
	cat(paste("\n",sep=""),file=FL.rawStat.out,append=TRUE)
	cat(paste("20a. statistics: the statistics of strata ",this.strata," are outputted to \"",FL.rawStat.out,"\"\n",sep=""))
	cat(paste("20a. statistics: the statistics of strata ",this.strata," are outputted to \"",FL.rawStat.out,"\"\n",sep=""),file=FL.LOG,append=TRUE)
}

# statistics on data of each two-week time periods
for (this.biweek in biweek.idx.names)
{
	cat(paste("get the subset data of the current two-week period \"",this.biweek,"\"!\n",sep=""))
	
	myData.this.Biweek <- subset(myData,subset = biweek.idx == this.biweek)
	
	# 
	no.plots.perTransect            <- data.frame(no.plots.perTransect     = tapply(myData.this.Biweek[,"morts"],     list(myData.this.Biweek[,"transect"]), FUN=length))		# all "no.sampled" in the sampled strata
	area.sampled.perTransect        <- data.frame(area.sampled.perTransect = tapply(myData.this.Biweek[,"area.sampled"],  list(myData.this.Biweek[,"transect"]), FUN=sum,na.rm=TRUE))	# all "area.sampled"   in the sampled strata # Sep 20, 2012: replace "area" with "area.sampled"
	fish.all.perTransect            <- data.frame(fish.all.perTransect     = tapply(myData.this.Biweek[,"morts"],     list(myData.this.Biweek[,"transect"]), FUN=sum,na.rm=TRUE))	# all "morts"      in the sampled strata
	cat(paste("count area and fish in the sampled biweek period\n",sep=""))
	cat(paste("count area and fish in the sampled biweek period\n",sep=""),file=FL.LOG,append=TRUE)

	# combine to a single PlotsSampled Table
	myPlots.perTransect.perBiweek <- cbind(	       no.plots.perTransect=     no.plots.perTransect,
					   area.sampled.perTransect        = area.sampled.perTransect,
					       fish.all.perTransect        =     fish.all.perTransect) 

	cat(paste(this.biweek,",",sep=""),file=FL.rawStat.out,append=TRUE)
	write.table(myPlots.perTransect.perBiweek,file=FL.rawStat.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
	cat(paste("\n",sep=""),file=FL.rawStat.out,append=TRUE)
	cat(paste("20b. statistics: the statistics of biweek period ",this.biweek," are outputted to \"",FL.rawStat.out,"\"\n",sep=""))
	cat(paste("20b. statistics: the statistics of biweek period ",this.biweek," are outputted to \"",FL.rawStat.out,"\"\n",sep=""),file=FL.LOG,append=TRUE)	
}




# -------------------------------------------------------------------------------------------------
# 21. BOOTSTRAP
# -------------------------------------------------------------------------------------------------
cat(paste("\n\n------------------------------------------------------------------------------------\n",sep=""))
cat(paste("\n\n------------------------------------------------------------------------------------\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("21a. Start Bootstrap..........\n",sep=""))
cat(paste("21a. Start Bootstrap..........\n",sep=""),file=FL.LOG,append=TRUE)


# define parameters for bootstrap
 no.replicate <- 10000 							# number of times for bootstrap: hard coded as 10000

# -------------------------------------------------------------------------------------------------
# define an array to record the [data estimate] of each stratum which will be used to calculate the bootstrap BIAS
# -------------------------------------------------------------------------------------------------
tao.split.data <- data.frame(array(NA,dim=c(no.strata.comb,1)))		# "tao.split" is the estimated stranding fish in each of the temporal (biweek) - spatial (river-section) strata
tao.data <- 0
row.names(tao.split.data) <- names(strata.combs)			# row:    biweek period
    names(tao.split.data) <- paste("river")				# column: river section
# -------------------------------------------------------------------------------------------------


# #################################################################################
# bootstrap sampling: we fixed this as 10000 which means we repeat booting samping 10000 times
# #################################################################################
for (idx.replicate in seq(from=1,to=no.replicate,by=1))			# loop of bootstrapping
{	
	cat(paste("21b. we are running bootstrap replicate ",idx.replicate,"\n",sep=""))
	cat(paste("21b. we are running bootstrap replicate ",idx.replicate,"\n",sep=""),file=FL.LOG,append=TRUE)
	
	# estimate in the Hanford Reach
	tao <- 0							# "tao" is the estimated stranding fish of the entire Hanford Reach from current bootstrap sample
	
	# in order to see which spatial-temporal strata constributing to the total stranded fish in the Hanford Reach, a "no biweek period" by "no river section" table is made
	tao.split <- data.frame(array(NA,dim=c(no.strata.comb,1)))	# "tao.split" is the estimated stranding fish in each of the 10 irregular strata
	row.names(tao.split) <- names(strata.combs)			# row:    biweek period
	    names(tao.split) <- paste("river")				# column: river section
	

	# sampling indepedently on each strata and then aggregate the strata estimate to get a entire reach estimate
	for (strata in unique.strata)
	{
		cat(paste("21c. replicate [",idx.replicate,"] of stratum [",strata,"]\n",sep=""))
		cat(paste("21c. replicate [",idx.replicate,"] of stratum [",strata,"]\n",sep=""),file=FL.LOG,append=TRUE)
		
		# # retrieve back the biweek index (temporal) and section index (spatial) from the strata name which will be used to get the transect and dewatered area of the transects in the strata
		# if(is.factor(strata)){strata <- as.character(strata)}		
		# tmp   <- unlist(strsplit(strata,"_"))
		# strata.temporal <- tmp[1]				# the biweek index
		# strata.spatial  <- as.numeric(sub("section","",tmp[2]))	# the section index i.e., 1, 2, or 3
		
		# data index of current strata
		index.data <- (myData[,"strata.comb"]==strata)	
		index.area <- (myArea[,"strata.comb"]==strata)	
		
	
		
		if(sum(index.data) > 0)	# should not use length(index.data) because index.data is a logic array with TRUE/FALSE which always has the length of [myData]
		{
			# **************************************
			# the subset of data at current strata
			# **************************************			
			myData.strata <- subset(myData,subset = index.data)
			idx.for.boot  <- seq(from=1,to=dim(myData.strata)[1],by=1)	# pooling all plots sample index.data for boot strap drawing
			
		
			
			# list and number of primary units (transect) surveyed at current strata
			list.primary <- sort(unique(myData.strata[,"transect"]))				# list of the sampled primary units (i.e., transect) in current strata
			n.k <- length(list.primary)								# the number of sampled primary units in current starta
			
			# get the dewatere transect and their area information from [myArea]
			myArea.strata <- subset(myArea,subset = index.area)
			
			# the available primary units and their estimated dewatered area
			N.k <- sum(!is.na(myArea.strata[,"transect"]))			# the total available transects in this strata with non-zero dewatered area
			A.k <- sum(myArea.strata[,"area.dewatered.m2"],na.rm=TRUE)	# the total dewaztered area of this strata
						
			cat(paste("21d. [",strata,"]: actually surveyed n.k: [",n.k,"], out of N.k: [",N.k,"] available transects with total modeled dewatered area: [",A.k,"] m2!\n",sep=""))
			cat(paste("21d. [",strata,"]: actually surveyed n.k: [",n.k,"], out of N.k: [",N.k,"] available transects with total modeled dewatered area: [",A.k,"] m2!\n",sep=""),file=FL.LOG, append=TRUE)
			cat(paste("[",strata,"]: actually surveyed n.k: [",n.k,"], out of N.k: [",N.k,"] available transects with total modeled dewatered area: [",A.k,"] m2!\n",sep=""),file=FL.DIAG,append=TRUE)

			# bootstrap sampling n.k transect from the n.k sampled transect in [list.primary] with replacement 
			if(n.k == 1)
			{
				list.primary.boot <- list.primary						# draw random primary unit (transect) from the sampled primary unit (transect) with replacement	
				cat(paste("21e. [",strata,"]: we have only one transect ",list.primary," surveyed!\n",sep=""))
				cat(paste("21e. [",strata,"]: we have only one transect ",list.primary," surveyed!\n",sep=""),file=FL.LOG,append=TRUE)
				cat(paste("21e. [",strata,"]: we have only one transect ",list.primary," surveyed!\n",sep=""),file=FL.DBG,append=TRUE)
			}else{
				# ********************************
				# draw n.k from the n.k transect with replacement
				list.primary.boot <- sample(list.primary,n.k,replace=TRUE)			# draw random primary unit (transect) from the sampled primary unit (transect) with replacement	
				# ********************************
			}

			cat(paste("21f. [",strata,"]: current bootstrap sample drawn from the [",n.k,"] transects contains ",dim(myData.strata)[1]," samples!\n",sep=""))
			cat(paste("21f. [",strata,"]: current bootstrap sample drawn from the [",n.k,"] transects contains ",dim(myData.strata)[1]," samples!\n",sep=""),file=FL.LOG, append=TRUE)
			cat(paste("21f. [",strata,"]: current bootstrap sample drawn from the [",n.k,"] transects contains ",dim(myData.strata)[1]," samples!\n",sep=""),file=FL.DIAG,append=TRUE)

			# =========================================================================
			# to estimate the bias, we need the data estimate.  Since we have a drawing on the pooled plots, in this level we did not go to each transect
			# =========================================================================
			if (idx.replicate == 1)
			{
				cat("\n21g. start of bias estimate\n")
				cat("\n21g. start of bias estimate\n",file=FL.LOG,append=TRUE)			
				# define arrays for current strata
				A.ik.array.data <- c()
				r.ik.array.data <- c()
				y.ik.array.data <- c()

				# loopping through the primary units
				idx.data <- 1
				for (label.primary in list.primary)
				{
					# data of a given transect
					myData.site.data <- subset(myData.strata,subset = transect == label.primary)	# samples of the sampled secondary units (plots) in current primary unit (transect) of current strata


					## for debug purpose
					## cat(paste("\nstrata [",strata,"] & transect [",label.primary,"],",sep=""),file=FL.LOG,append=TRUE)
					## write.table(myData.site.data,file=FL.LOG,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
					## cat(paste("\n",sep=""),file=FL.LOG,append=TRUE)
					
					# find the m_ik of each transect 
					m.ik.data <- dim(myData.site.data)[1]						# surveyed plots of a surveyed transect in the current strata
	
					# *******************************************
					# looping through the secondary unit level
					# *******************************************
					if (m.ik.data < 3)
					{
						myData.transect.data <- myData.strata					# for transect with 1 or 2 plots, drawn 3 plots from the pooled plots of all transects in the stratum						
					}else{
						myData.transect.data <- myData.site.data				# for transect with 3 or more plots, drawn the plots from the transect
					}

					# stranding rate (fish/area) of current primary unit on a bootstrap sample
					sum.a.ik.data <- sum(myData.transect.data[,"area.sampled"], na.rm=TRUE)		# sum of the surveyed area       of current transect of current strata # Sep 20, 2012: replace "area" with "area.sampled"
					sum.y.ik.data <- sum(myData.transect.data[,"morts"],    na.rm=TRUE) 		# sum of observed stranded fishs of current transect of current strata
					    r.ik.data <- sum.y.ik.data / sum.a.ik.data					# stranded rate (# fishes/area)  of current transect of current strata

					# actual dewatered area of current transect of current strata
					A.ik <- myArea.strata[myArea.strata[,"transect"] == label.primary,"area.dewatered.m2"]			# get the dewater area of current transect in current strata				

					# estimated stranded fish of current transect of current strata
					y.ik.data <- r.ik.data * A.ik							# the data stranding rate is expanded by the dewatered area of the ith-transect of the k-th strata

					cat(paste("\t21h. [",strata,"] & [transect ",label.primary,"]: stranded fish: ",y.ik.data," based on a stranding fish rate of ",r.ik.data,"(",sum.y.ik.data,") / (",sum.a.ik.data,") and a dewatered area of ",A.ik," m2\n",sep=""))
					cat(paste("\21h. t[",strata,"] & [transect ",label.primary,"]: stranded fish: ",y.ik.data," based on a stranding fish rate of ",r.ik.data,"(",sum.y.ik.data,") / (",sum.a.ik.data,") and a dewatered area of ",A.ik," m2\n",sep=""),file=FL.LOG, append=TRUE)
					cat(paste("\21h. t[",strata,"] & [transect ",label.primary,"]: stranded fish: ",y.ik.data," based on a stranding fish rate of ",r.ik.data,"(",sum.y.ik.data,") / (",sum.a.ik.data,") and a dewatered area of ",A.ik," m2\n",sep=""),file=FL.DIAG,append=TRUE)

					# put them in the arrays				
					A.ik.array.data[idx.data] <- A.ik
					r.ik.array.data[idx.data] <- r.ik.data
					y.ik.array.data[idx.data] <- y.ik.data		

					if (is.na(A.ik) | is.na(r.ik.data) | is.na(y.ik.data))
					{
						cat(paste("21i. something not right. check why!\n",sep=""))
						cat(paste("21i. something not right. check why!\n",sep=""),file=FL.LOG,append=TRUE)
						die
					}

					# accumulate idx.data
					idx.data <- idx.data + 1
				}




				# esimate at the strata level
				  r.k.data <- sum(y.ik.array.data) / sum(A.ik.array.data)	# the stranded rate of current strata (# fishes/area)
				tao.k.data <- A.k * r.k.data					# the estimate strata stranding rate is expanded to the entire strata to get the strata estimate			
				cat(paste("21j. [",strata,"]: stranded fish: ",tao.k.data," based on a stranding fish rate of ",r.k.data,"(",sum(y.ik.array.data),") / (",sum(A.ik.array.data),") and a dewatered area of ",A.k," m2!\n",sep=""))
				cat(paste("21j. [",strata,"]: stranded fish: ",tao.k.data," based on a stranding fish rate of ",r.k.data,"(",sum(y.ik.array.data),") / (",sum(A.ik.array.data),") and a dewatered area of ",A.k," m2!\n",sep=""),file=FL.LOG, append=TRUE)
				cat(paste("21j. [",strata,"]: stranded fish: ",tao.k.data," based on a stranding fish rate of ",r.k.data,"(",sum(y.ik.array.data),") / (",sum(A.ik.array.data),") and a dewatered area of ",A.k," m2!\n",sep=""),file=FL.DIAG,append=TRUE)


				##### # using all plots data in the stratum to make a data estimate
				##### area.data <- sum(myData.strata[,"area.sampled"], na.rm=TRUE)	# sum of the sampled area  # Sep 20, 2012: replace "area" with "area.sampled"
				##### mort.data <- sum(myData.strata[,"morts"],    na.rm=TRUE) 	# sum of observed stranded fishs
				##### rate.data <- mort.data / area.data			# stranded rate (# fishes/area)  of current transect of current strata
				##### cat(paste("the data values: area=",area.data," m2; morts=",mort.data," dead fish and rate=",rate.data," fish/m2 for two-week period of [",strata,"]\n",sep=""))
				##### cat(paste("the data values: area=",area.data," m2; morts=",mort.data," dead fish and rate=",rate.data," fish/m2 for two-week period of [",strata,"]\n",sep=""),file=FL.LOG,append=TRUE)		
				##### 
				##### tao.k.data <- A.k * rate.data
				##### 
				##### cat(paste("[",strata,"]: stranded fish: ",tao.k.data," based on a stranding fish rate of ",rate.data," and a dewatered area of ",A.k," m2!\n",sep=""))
				##### cat(paste("[",strata,"]: stranded fish: ",tao.k.data," based on a stranding fish rate of ",rate.data," and a dewatered area of ",A.k," m2!\n",sep=""),file=FL.LOG, append=TRUE)
				##### cat(paste("[",strata,"]: stranded fish: ",tao.k.data," based on a stranding fish rate of ",rate.data," and a dewatered area of ",A.k," m2!\n",sep=""),file=FL.DIAG,append=TRUE)			
				
				# add the estimate of this strata to the estimate of the entire Hanford reach
				tao.data <- tao.data + tao.k.data

				# assign "tao.k.data" to the "tao.split.data" table for further checking the spatial and temporal contribution
				tao.split.data[strata,"river"] <- tao.k.data

				if (is.na(tao.k.data) | is.na(tao.data))
				{
					cat(paste("21k. something not right. check why!\n",sep=""))
					cat(paste("21k. something not right. check why!\n",sep=""),file=FL.LOG,append=TRUE)
				}  
				cat(paste("calculate [data.estimate] for [",strata,"]\n",sep=""))
				cat("21l. end of bias estimate\n\n")
				cat("21l. end of bias estimate\n\n",file=FL.LOG,append=TRUE)					
			}
			# -----------------------------------------------------------------
			
			
			# if (strata == "Mar29-Apr25_sec123"){die}
			
			# define arrays for current strata: the length of the arrays is the number of the transect in this strata
			A.ik.array <- c()
			r.ik.array <- c()
			y.ik.array <- c()
			
			# loopping through the primary unit randomly drawn (possible replicated)
			idx <- 1
			cat(paste("21m. randomly select transects with replacement for stratum [",strata,"]: (",paste(list.primary.boot,collapse=","),")\n",sep=""))
			cat(paste("21m. randomly select transects with replacement for stratum [",strata,"]: (",paste(list.primary.boot,collapse=","),")\n",sep=""),file=FL.LOG,append=TRUE)
			
			for (label.primary in list.primary.boot)
			{
				cat("21n. loopping on transect level:",label.primary," for stratum:[",strata,"]\n")
				cat("21n. loopping on transect level:",label.primary," for stratum:[",strata,"]\n",file=FL.LOG,append=TRUE)

				# data of a given transect in the boot sample
				myData.site <- subset(myData.strata,subset = transect == label.primary)		# samples of the sampled secondary units (plots) in current primary unit (transect) of current strata
				
				# find the m_ik of each transect 
				m.ik <- dim(myData.site)[1]							# surveyed plots of a surveyed transect in the current strata
				list.secondary <- seq(from=1,to=m.ik,by=1)					# construct a sample index.data for current transect of current strata
				
				# *******************************************
				# bootstrapping on the secondary unit level
				# *******************************************
				if (m.ik < 3)
				{
					# randomly draw m.ik sample from [myData.strata] with replacement (not drawing m.ik from the m.ik data in the i-th transect of the k-th strata, instead, data from all surveyed transects of the k-th strata are pooled)		
					# draw m.ik from sum(m.ik) with i from 1 to n.k (draw from the pool)
					# **********************
					idx.from.boot <- sample(idx.for.boot,3,replace=TRUE)			# note: m.ik sample is not drawn from m.ik samples of current transect in the strata INSTEAD from the pooled plots from all transects in the strata subset													
					myData.transect.boot <- myData.strata[idx.from.boot,]			# for transect with 1 or 2 plots, drawn 3 plots from the pooled plots of all transects in the stratum

					cat(paste("21o. m.ik<3: randomly select plots with replacement for transect [",label.primary,"] at stratum [",strata,"] from pooled data in the stratum: (",paste(idx.from.boot,coppapse=","),")\n",sep=""))
					cat(paste("21o. m.ik<3: randomly select plots with replacement for transect [",label.primary,"] at stratum [",strata,"] from pooled data in the stratum: (",paste(idx.from.boot,coppapse=","),")\n",sep=""),file=FL.LOG,append=TRUE)					
					# **********************
				}else{
					# ********************************
					# draw m.ik from the m.ik plots of the current transect witrh replacement
					list.secondary.boot  <- sample(list.secondary,m.ik,replace=TRUE)	# draw random sample from the index.data with replacement	
					myData.transect.boot <- myData.site[list.secondary.boot,]		# for transect with 3 or more plots, drawn the plots from the transect
					
					cat(paste("21p. m.ik>=3: randomly select plots with replacement for transect [",label.primary,"] at stratum [",strata,"] from current transect: (",paste(list.secondary.boot,collapse=","),")\n",sep=""))
					cat(paste("21p. m.ik>=3: randomly select plots with replacement for transect [",label.primary,"] at stratum [",strata,"] from current transect: (",paste(list.secondary.boot,collapse=","),")\n",sep=""),file=FL.LOG,append=TRUE)					
					# ********************************				
				}
				
				# stranding rate (fish/area) of current primary unit on a bootstrap sample
				sum.a.ik <- sum(myData.transect.boot[,"area.sampled"], na.rm=TRUE)	# sum of the surveyed area       of current transect of current strata # Sep 20, 2012: replace "area" with "area.sampled"
				sum.y.ik <- sum(myData.transect.boot[,"morts"],    na.rm=TRUE) 	# sum of observed stranded fishs of current transect of current strata
				r.ik <- sum.y.ik / sum.a.ik					# stranded rate (# fishes/area)  of current transect of current strata
				
				# actual dewatered area of current transect of current strata
				A.ik <- myArea.strata[myArea.strata[,"transect"] == label.primary,"area.dewatered.m2"]			# get the dewater area of current transect in current strata				
				
				# estimated stranded fish of current transect of current strata
				y.ik <- r.ik * A.ik						# the data stranding rate is expanded by the dewatered area of the ith-transect of the k-th strata
				
				cat(paste("\t21q. [",strata,"] & [transect ",label.primary,"]: stranded fish: ",y.ik," based on a stranding fish rate of ",r.ik,"(",sum.y.ik,") / (",sum.a.ik,") and a dewatered area of ",A.ik," m2\n",sep=""))
				cat(paste("\t21q. [",strata,"] & [transect ",label.primary,"]: stranded fish: ",y.ik," based on a stranding fish rate of ",r.ik,"(",sum.y.ik,") / (",sum.a.ik,") and a dewatered area of ",A.ik," m2\n",sep=""),file=FL.LOG, append=TRUE)
				cat(paste("\t21q. [",strata,"] & [transect ",label.primary,"]: stranded fish: ",y.ik," based on a stranding fish rate of ",r.ik,"(",sum.y.ik,") / (",sum.a.ik,") and a dewatered area of ",A.ik," m2\n",sep=""),file=FL.DIAG,append=TRUE)

				# put them in the arrays				
				A.ik.array[idx] <- A.ik
				r.ik.array[idx] <- r.ik
				y.ik.array[idx] <- y.ik		
				
				if (is.na(A.ik) | is.na(r.ik) | is.na(y.ik))
				{
					cat(paste("21r. something not right. check why!\n",sep=""))
					cat(paste("21r. something not right. check why!\n",sep=""),file=FL.LOG,append=TRUE)
					die
				}
				
				# accumulate idx
				idx <- idx + 1
			}

			cat("21s. work on the stratum level for stratum:[",strata,"]\n")
			cat("21s. work on the stratum level for stratum:[",strata,"]\n",file=FL.LOG,append=TRUE)
			# esimate at the strata level
			  r.k <- sum(y.ik.array) / sum(A.ik.array)	# the stranded rate of current strata (# fishes/area)
			tao.k <- A.k * r.k				# the estimate strata stranding rate is expanded to the entire strata to get the strata estimate			
			cat(paste("21t. [",strata,"]: stranded fish: ",tao.k," based on a stranding fish rate of ",r.k,"(",sum(y.ik.array),") / (", sum(A.ik.array),") and a dewatered area of ",A.k," m2!\n",sep=""))
			cat(paste("21t. [",strata,"]: stranded fish: ",tao.k," based on a stranding fish rate of ",r.k,"(",sum(y.ik.array),") / (", sum(A.ik.array),") and a dewatered area of ",A.k," m2!\n",sep=""),file=FL.LOG, append=TRUE)
			cat(paste("21t. [",strata,"]: stranded fish: ",tao.k," based on a stranding fish rate of ",r.k,"(",sum(y.ik.array),") / (", sum(A.ik.array),") and a dewatered area of ",A.k," m2!\n",sep=""),file=FL.DIAG,append=TRUE)
			
			# add the estimate of this strata to the estimate of the entire Hanford reach
			tao <- tao + tao.k
			
			# assign "tao.k" to the "tao.split" table for further checking the spatial and temporal contribution
			tao.split[strata,"river"] <- tao.k

			if (is.na(tao.k) | is.na(tao))
			{
				cat(paste("21u. something not right. check why!\n",sep=""))
				cat(paste("21u. something not right. check why!\n",sep=""),file=FL.LOG,append=TRUE)
			}			
			
		
			# if (strata == "Mar29-Apr25_sec123"){die}
		}
	}
	
	# now we have the estimate from all strata: do a quick check
	tao.split.sum <- sum(tao.split,na.rm=TRUE)
	if (abs(tao - tao.split.sum) > 1e-5)
	{
		cat(paste("21v. The total stranded Boot estimate ",tao," is NOT the same as the sum of the stranded estimate from each of the strata ",tao.split.sum,". Checking why!\n",sep=""))
		cat(paste("21v. The total stranded Boot estimate ",tao," is NOT the same as the sum of the stranded estimate from each of the strata ",tao.split.sum,". Checking why!\n",sep=""),file=FL.LOG, append=TRUE)
		die
	}
	

    
	# put the adjusted total stranded estimate of current boot replicate into an array	
	          tao.strata  <- data.frame(t(rbind(tao.split,reach=sum(tao.split[,"river"],na.rm=TRUE))))
	    names(tao.strata) <- c(row.names(tao.split),"reach") 
	row.names(tao.strata) <- paste("boot",idx.replicate,sep="")
	
	if(idx.replicate == 1)
	{
		tao.boot <- tao.strata
	}else{
		tao.boot <- rbind(tao.boot,tao.strata)
	}
	cat(paste("21w. put the adjusted total stranded estimate of current boot replicate into an array!\n",sep=""))
	cat(paste("21w. put the adjusted total stranded estimate of current boot replicate into an array!\n",sep=""),file=FL.LOG, append=TRUE)
	
	
	
	
	# -----------------------------------------------------------------
	# to estimate BIAS we need to have a data estimate
	# -----------------------------------------------------------------
	if (idx.replicate == 1)
	{
		cat(paste("have [data.estimate] for all strata\n",sep=""))
		
		
		# now we have the estimate from all strata: do a quick check
		tao.split.sum.data <- sum(tao.split.data,na.rm=TRUE)
		if (abs(tao.data - tao.split.sum.data) > 1e-5)
		{
			cat(paste("21x. The total stranded Data estimate ",tao.data," is NOT the same as the sum of the stranded estimate from each of the strata ",tao.split.sum.data,". Checking why!\n",sep=""))
			cat(paste("21x. The total stranded Data estimate ",tao.data," is NOT the same as the sum of the stranded estimate from each of the strata ",tao.split.sum.data,". Checking why!\n",sep=""),file=FL.LOG, append=TRUE)
			die
		}

		     TAO.data  <- data.frame(t(rbind(tao.split.data,reach=sum(tao.split.data[,"river"],na.rm=TRUE))))
	       names(TAO.data) <- c(row.names(tao.split.data),"reach")      
           row.names(TAO.data) <- paste("Data")		
	}	
	
	
	

	
	# -----------------------------------------------------------------
	# output the boot strap output
	# -----------------------------------------------------------------
	if (idx.replicate == 1)
	{
		cat(paste("output [data.estimate] and [bootstrap estimate] of all strata\n",sep=""))
		
		
		# write the data estimate
		cat(paste(",",sep=""),file=FL.boot.out,append=TRUE)
		write.table(TAO.data,file=FL.boot.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
	
		# write the bootstrap estimate
		cat(paste(",",sep=""),      file=FL.boot.out,append=TRUE)
		write.table(tao.strata,file=FL.boot.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
	}else{
		# write the bootstrap estimate
		write.table(tao.strata,file=FL.boot.out,sep=",",col.names =FALSE, row.names=TRUE,append=TRUE)
	}
	

	
	cat(paste("22y. the estimated stranded fish is ",tao," in the Hanford Reach from bootstrap replicate ",idx.replicate," is ",tao,"!\n",sep=""))
	cat(paste("22y. the estimated stranded fish is ",tao," in the Hanford Reach from bootstrap replicate ",idx.replicate," is ",tao,"!\n",sep=""),file=FL.LOG, append=TRUE)	
	cat(paste("22y. the estimated stranded fish is ",tao," in the Hanford Reach from bootstrap replicate ",idx.replicate," is ",tao,"!\n",sep=""),file=FL.DIAG,append=TRUE)	
}
names(tao.boot) <- sub("\\.","-",names(tao.boot))

cat(paste("22z. bootstrap has completed the ",no.replicate," drawings!\n",sep=""))
cat(paste("22z. bootstrap has completed the ",no.replicate," drawings!\n",sep=""),file=FL.LOG,append=TRUE)	


# -------------------------------------------------------------------------------------------------
# change the name for the entire reach
# -------------------------------------------------------------------------------------------------
names(TAO.data) <- sub("tao.reach.data","HanfordReach",names(TAO.data))
names(tao.boot) <- sub("tao.reach",     "HanfordReach",names(tao.boot))

# -------------------------------------------------------------------------------------------------
# calculate BAIS corrected estimate and BIAS corrected and accerlated confidence interval
# -------------------------------------------------------------------------------------------------
mean.tao.boot <- apply(tao.boot,2,FUN=mean,na.rm=TRUE)
BIAS.tao.boot <- mean.tao.boot - TAO.data
BIAS.corrected <- 2*TAO.data - mean.tao.boot		# bias corrected boot strap estimate
row.names(BIAS.corrected) <- "bias corrected boot strap estimate"

# calclate the fraction of bootstrap estimate which is smaller than the data estimate
DIFF.tao.boot <- tao.boot - rep(1,dim(tao.boot)[1]) %*% t(as.numeric(TAO.data))
FLAG.tao.boot <- DIFF.tao.boot
FLAG.tao.boot[FLAG.tao.boot > 0 & !(is.na(FLAG.tao.boot))] <- 0
FLAG.tao.boot[FLAG.tao.boot < 0 & !(is.na(FLAG.tao.boot))] <- 1

negative.count <- apply(FLAG.tao.boot,2,FUN=sum,na.rm=TRUE)
     all.count <- apply(FLAG.tao.boot,2,FUN=length)
negative.frac  <- negative.count/all.count

# calculate z0
z0 <- qnorm(negative.frac)
z0[z0==-Inf] <- NA



# calculate the two locations for the 95% percentile
      conf <- 0.95
     alpha <- (1+c(-conf,conf))/2
    zalpha <- qnorm(alpha)


# bias corrected alpha
adj.zalpha1 <- zalpha[1] + 2 * z0
adj.zalpha2 <- zalpha[2] + 2 * z0
adj.alpha1  <- pnorm(adj.zalpha1)
adj.alpha2  <- pnorm(adj.zalpha2)

# the position in the sorted boot array for percentile CI and bias corrected CI
idx.count <- 0
for (label in names(tao.boot))
{

	# get the bootstrap estimate, BIAS and the bias corrected bootstrap estimate
	data.estimate    <- as.numeric(TAO.data[label])
	boot.estimate    <- as.numeric(mean.tao.boot[label])
	bias.estimate    <- as.numeric(BIAS.tao.boot[label])
	boot.estimate.BC <- as.numeric(BIAS.corrected[label])
	
	idx.count <- idx.count + 1
	if (sum(is.na(tao.boot[,label])) != no.replicate)
	{
		# percentile CI
		loc.perc  <- norm.inter(tao.boot[,label],alpha)
		 CI.perc  <- data.frame(conf = conf,loc.LL = loc.perc[1,][1],LL = loc.perc[1,][2],loc.UL = loc.perc[2,][1],UL = loc.perc[2,][2])


		# bias corrected CI
		adj.alpha <- c(adj.alpha1[label],adj.alpha2[label])
		if (sum(is.na(adj.alpha)) == 2)
		{
			 CI.bias  <- data.frame(conf = NA,loc.LL = NA,LL = NA,loc.UL = NA,UL = NA)
		}else{		
			loc.bias  <- norm.inter(tao.boot[,label],adj.alpha)
			 CI.bias  <- data.frame(conf = conf,loc.LL = loc.bias[1,][1],LL = loc.bias[1,][2],loc.UL = loc.bias[2,][1],UL = loc.bias[2,][2])
		}		
		
	}else{
		CI.perc <- data.frame(conf = NA,loc.LL = NA,LL = NA,loc.UL = NA,UL = NA)
		CI.bias <- data.frame(conf = NA,loc.LL = NA,LL = NA,loc.UL = NA,UL = NA)
	}		

	# add a row name		
	command.string <- paste("row.names(CI.bias) <- \"",label,"\"",sep="")
	eval(parse(text=command.string))

	# add a row name
	command.string <- paste("row.names(CI.perc) <- \"",label,"\"",sep="")
	eval(parse(text=command.string))


	CI.perc <- cbind(CI.perc,
			 data.estimate    = data.estimate,    
			 boot.estimate    = boot.estimate,    
			 bias.estimate    = bias.estimate,    
			 boot.estimate.BC = boot.estimate.BC) 
	
	CI.bias <- cbind(CI.bias,
			 data.estimate    = data.estimate,    
			 boot.estimate    = boot.estimate,    
			 bias.estimate    = bias.estimate,    
			 boot.estimate.BC = boot.estimate.BC) 
	
	if (idx.count == 1)
	{
		CI.perc.all <- CI.perc
		CI.bias.all <- CI.bias
	}else{
		CI.perc.all <- rbind(CI.perc.all,CI.perc)
		CI.bias.all <- rbind(CI.bias.all,CI.bias)
	}
}




# -------------------------------------------------------------------------------------------------
# output the bootstrap results
# -------------------------------------------------------------------------------------------------
cat(paste("\n\n",sep=""),file=FL.boot.out,append=TRUE)
cat(paste("bootstrap,",sep=""),file=FL.boot.out,append=TRUE)
write.table(tao.boot,file=FL.boot.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)
cat(paste("bootstrap results of ",no.replicate," drawings has been outputted to \"",FL.boot.out,"\"\n",sep=""))
cat(paste("bootstrap results of ",no.replicate," drawings has been outputted to \"",FL.boot.out,"\"\n",sep=""),file=FL.LOG,append=TRUE)


pmn   <- apply(tao.boot,2,FUN = mean,na.rm=TRUE)
pmin  <- apply(tao.boot,2,FUN = min, na.rm=TRUE)
p025  <- apply(tao.boot,2,FUN = quantile,0.025,na.rm=TRUE)  
p500  <- apply(tao.boot,2,FUN = quantile,0.500,na.rm=TRUE)  
p975  <- apply(tao.boot,2,FUN = quantile,0.975,na.rm=TRUE) 
pmax  <- apply(tao.boot,2,FUN = max, na.rm=TRUE)
pstat <- rbind(pmn,pmin,p025,p500,p975,pmax)

cat(paste("\n\n",sep=""),file=FL.boot.out,append=TRUE)
cat(paste("bootstrap statistics,",sep=""),file=FL.boot.out,append=TRUE)
write.table(pstat,file=FL.boot.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)

cat(paste("\n\n",sep=""),file=FL.boot.out,append=TRUE)
cat(paste("percentile confidence intervals,",sep=""),file=FL.boot.out,append=TRUE)
write.table(CI.perc.all,file=FL.boot.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)

cat(paste("\n\n",sep=""),file=FL.boot.out,append=TRUE)
cat(paste("bias corrected confidence intervals,",sep=""),file=FL.boot.out,append=TRUE)
write.table(CI.bias.all,file=FL.boot.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)

cat(paste("\n\n",sep=""),file=FL.boot.out,append=TRUE)
cat(paste("bias corrected boot strap estimate,",sep=""),file=FL.boot.out,append=TRUE)
write.table(BIAS.corrected,file=FL.boot.out,sep=",",col.names =TRUE, row.names=TRUE,append=TRUE)




save(tao.boot,pstat,TAO.data,CI.perc.all,CI.bias.all,file=FL.boot.obj)
cat(paste("bootstrap statistics of ",no.replicate," drawings has been outputted to \"",FL.boot.out,"\" and saved to\"",FL.boot.obj,"\"\n",sep=""))
cat(paste("bootstrap statistics of ",no.replicate," drawings has been outputted to \"",FL.boot.out,"\" and saved to\"",FL.boot.obj,"\"\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------
# distribution of the total mortalities based on (no.replicate) samples each has (bootSample.szie) observations
# -------------------------------------------------------------------------
# the position in the sorted boot array for percentile CI and bias corrected CI

# open pdf file for outputting plots
pdf(file = FL.pdf.out,         paper="a4r",width=0,height=0)	# device 2


idx.count <- 0
for (label in names(tao.boot))
{
	if (label == "reach")
	{
		string.title <- paste("Hanford Reach (",no.replicate," replicates)",sep="")
		string.xaxis <- paste("Estimated Stranding Loss of Year 2013",sep="")
	}else
	{
		string.title <- paste("Hanford Reach (",label,"): (",no.replicate," replicates)",sep="")
		string.xaxis <- paste("Estimated Stranding Loss of Year 2013",sep="")
	}
	

	idx.count <- idx.count + 1
	if (sum(is.na(tao.boot[,label])) != no.replicate)
	{
		hist(tao.boot[,label], 
			    xlab = string.xaxis,
			    ylab = paste("Frequency",sep=""),
			    main = string.title,
			    sub = "",
			    col="red",border="red",
			    between = list(x=0.5,y=0.5),as.table = TRUE,
			    scales = list(x= "free",y="free"),
			    nclass = 400,table=TRUE,
			    freq=FALSE)	
		abline(v =  mean(tao.boot[,label]),         col="grey", lwd=2,lty=2)	#                     the mean
		abline(v =median(tao.boot[,label]),         col="black",lwd=2,lty=2)	#                     the median			
		abline(v =  CI.perc.all[label,c("LL","UL")],col="blue", lwd=2,lty=2)	# one sigma away from the mean
		abline(v =  CI.bias.all[label,c("LL","UL")],col="green",lwd=2,lty=2)	# one sigma away from the mean

		# add the average of BOOT mean as a vertical line
		# text(median(tao.boot[,label]),0,paste(round(median(tao.boot[,label]),digits=2),round(mean(tao.boot[,label]),digits=2),sep="|"),cex=1,col="black")		# -0.05 to place the text
		# text(CI.perc.all[label,c("LL")],0,round(CI.perc.all[label,"LL"]),digits=2,cex=1,col="blue")									# -0.05 to place the text
		# text(CI.perc.all[label,c("UL")],0,round(CI.perc.all[label,"UL"]),digits=2,cex=1,col="blue")									# -0.05 to place the text
		# text(CI.bias.all[label,c("LL")],0,round(CI.bias.all[label,"LL"]),digits=2,cex=1,col="green")									# -0.05 to place the text
		# text(CI.bias.all[label,c("UL")],0,round(CI.bias.all[label,"UL"]),digits=2,cex=1,col="green")									# -0.05 to place the text
		cat(paste("Histogram of the entire Hanfor Reach of the entire season\n",sep=""))
		cat(paste("Histogram of the entire Hanfor Reach of the entire season\n",sep=""),file=FL.LOG,append=TRUE)
	}		
}



dev.off();
cat(paste("a histogram has been prepared on the ",no.replicate," drawings!\n",sep=""))
cat(paste("a histogram has been prepared on the ",no.replicate," drawings!\n",sep=""),file=FL.LOG,append=TRUE)	



# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n04_2013_BootStrap_combinedStrata.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n04_2013_BootStrap_combinedStrata.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [04_2013_BootStrap_combinedStrata.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [04_2013_BootStrap_combinedStrata.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

#
# put run related information into the log file
#
cat(paste("This run was conducted in ",.Platform$OS.type,"\n",sep=""));
cat(paste("This run was conducted in ",.Platform$OS.type,"\n",sep=""),file=FL.LOG,append=TRUE);




# get the version of R used for this computation and the latest version released
current.Rversion <- R.Version()$version.string
tmp = readLines("http://cran.r-project.org/sources.html")
rls = tmp[grep("latest release", tmp) + 1L]			# the version number is in the next line of 'The latest release'
latest.Rversion  <- gsub("(.*R-|\\.tar\\.gz.*)", "", rls)	# "The latest release: R-2.13.0.tar.gz"
if (latest.Rversion != current.Rversion)
{
	cat(paste("\n\nyou may want to upgrade R from the version you are using [",current.Rversion,"] to the latest version of [",latest.Rversion,"]\n",sep=""));
	cat(paste("\n\nyou may want to upgrade R from the version you are using [",current.Rversion,"] to the latest version of [",latest.Rversion,"]\n",sep=""),file=FL.LOG,append=TRUE);
}else{
	cat(paste("\n\nThe R version you are using is the latest version released so far!\n",sep=""))
	cat(paste("\n\nThe R version you are using is the latest version released so far!\n",sep=""),file=FL.LOG,append=TRUE)
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





