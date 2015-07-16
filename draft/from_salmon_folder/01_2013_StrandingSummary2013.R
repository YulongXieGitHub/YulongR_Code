#
# 01_2013_StrandingSummary2013.R
#
# Input files:
# C:/yulong_projects/FY2013_SalmonStrandingFieldSurvey/Data_Received/data_2013_stranding_entrapment_fieldSurvey/Stranding Data 2013.xls
# C:/yulong_projects/FY2013_SalmonStrandingFieldSurvey/Data_Received/data_2013_stranding_entrapment_fieldSurvey/Stranding Data 2013_YLX.csv	# The csv version is:
#
#
#/Volumes/MurrayGeostats2/FY2013_SalmonStrandingFieldSurvey/FY2013_SalmonStranding_Analysis/0_scripts/02_DewateredArea_Flow2013.R
# Sep 9, 2013: "01_2013_StrandingSummary2013.R" based on "C:\YuLong_Projects\FY2012_SalmonStrandingFieldSurvey_2012March_June\0_scripts\2012_HanfordReach_StrandingSurvey_2012March_June_revised.R"
#
# To summarize the stranding data
#
# -----------------------------------------------------------------------
# Sep 12, 2012:
# Modified on "2011_HanfordReach_StrandingSurvey.R" 
# To apply on the 2012 Survey season data analysis
# 
# Data description:
# ------------------ several lines of the data in "Stranding Data 2013_YLX.csv"  ----------------------------
# sampled,Date,River SectioNo,SegmeNot,TraNosect,FB Yes_No,Time,WaypoiNot,Coord No,Coord E,Dom_Sub,SubDom_Sub,Embedded,VegetatioNo,Area Sampled,ChiNoook,NoPM,Stickleback,SculpiNo,Dace,Sucker,Site
# yes,3/2/2013,2,6,221,Yes,10:36,B221LS03,46.57275,119.34306,9,9,1,1,74.7,,,,,,,56
# yes,3/2/2013,2,6,221,Yes,10:15,B221LS01,46.57273,119.34286,8,9,3,1,78.5,,,,,,,56
# yes,3/2/2013,2,6,221,Yes,10:25,B221LS02,46.57272,119.34277,1,9,3,2,63.74,,,,,,,56
# col names		description
# -------------------------------------------------------------------------
# sampled		sampled					I added this column.  Originally the data are separated into two distinct portion.  The first 734 data I believe are data actually sampled (assigned "yes") and the last 288 samples are plots visited but not sampled (assigned "no")
# Date			** Date				
# River SectioNo	** River Section
# SegmeNot		** Segment
# TraNosect		** Transect
# FB Yes_No		** ??
# Time			** Time
# WaypoiNot		??
# Coord No		North
# Coord E		East
# Dom_Sub		substrate??
# SubDom_Sub		substrate??
# Embedded		??
# VegetatioNo		Vegetation Flag
# Area Sampled		** Area Sampled
# ChiNoook		** Number of Chinook
# NoPM			fish species. Northern pikeminnow??
# Stickleback		fish species
# SculpiNo		fish species
# Dace			fish species
# Sucker		fish species
# Site			site					I added this column which is calculated from the trunsect.
# -------------------------------------------------------------------------
#
#
# Gone through on Dec 5, 2011
#
# Created on August 3rd to process the 2011 Hanford Reach Salmon Stranding Field Survey data
# The data was received on Aug 1st, 2011 from Crhis in the following file:
# "C:\YuLong_Projects\FY2012_SalmonStrandingFieldSurvey\Data_Received\DataReceived_Aug1_2011\2011 Entrapment & Stranding Database SORT0720.xlsx"
# A working copy                                                                            "2011 Entrapment & Stranding Database SORT0720_ylx.xlsx" has been made 
# and a working tab "Stranding_workingCopy" for stranding data has been created in this working copy which is basically a simplified data set as the one in the "Stranding" tab.
# Some minor errors in the "Stranding" tab have been corrected in the "Stranding_workingCopy" tab.
#
# Modified on Nov 10, 2011
#
#
# The data in the "Stranding_workingCopy" tab is saved as a csv format in the "0_data" subfolder in the file name of 
# "2012_HanfordReach_StrandingSurvey_correctedOn_Dec20_2011.csv" and is used as the data set for the analysis.
#
# Data Description of each record in the data
# -------------------------------------------------------------------------------------------------
# Date: 	sampling date
# Time: 	{may have no use for our analysis}
# Site: 	which is the concatenation of transect and L/R indication
# Segment:	8 river segment
# Transect:	1 - 360 transect.  This is the primary unit of random sample selection.
# Shore:	{may have no use for our analysis}.  Left and Right, do not know what are "LI", "RI", etc. 
# Sampled:	a flag of "Sampled" or "Not Sampled" for the particular transcet and quardrant.
# Latitude:	{may have no use for our analysis}
# Longitude:	{may have no use for our analysis}
# Area:		the area of the transect/quardrant
# fish.alive:	alive fish found in the transect/quardrant
# fish.dead:	dead  fish found in the transect/quardrant.  Treated as dead in our analysis
# -------------------------------------------------------------------------------------------------
#
# The data was recorded on the transect/quardrant basis.
# 
# -------------------------------------------------------------------------------------------------
# After data inspection, the following correspondence has been established 
# between the header of the data file "2012_HanfordReach_StrandingSurvey_correctedOn_Dec20_2011.csv"
#     and the header in tab "Stranding Summary" of "2011 Entrapment & Stranding Database SORT0720.xlsx"
#
# col in the "Stranding Summary" Table		field in the data file of "2012_HanfordReach_StrandingSurvey_correctedOn_Dec20_2011.csv"
# ------------------------------------		-----------------------------------------------------------------
# (2) Site visited      (col 2): 		"transect" not the "site" field in the data file			note: Here Iste means Transect.  transect is from 1 to 360.  Site is an identification of transect added LEFT/RIGHT info, i.e., "quardrant/transect"
# (3) plot sampled, No  (col 3): 		if all "sampled" are "N" for the same "Transect", it counts as "No".	note: here "Plots" means "Quardrant". no  quardrant was sampled 
# (4) plot sampled, Yes (col 4): 		if any "sampled" are "Y" for the same "Transect", it counts as "Yes"	note: here "Plots" means "Quardrant". any quardrant not necessary all quardrant were sampled
# (5) plots             (col 5):                the number of "Y" records in "sampled" field.  				note: a quardrant of any transect is treated as a site (transect/quardrant). A transect can have multiple samples
# (6) area sampled      (col 6):                the sum of "area" when sampled is "Y". 					note: the area of each sampled plots (transect/quardrant) a sampled "Y" may have a 0 area such as 4/7/11 13:30 and 4/7/11 13:40
# (7) Chinook           (col 7): 
# -------------------------------------------------------------------------------------------------
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

# close all devices which have been opened
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
Path.in  <- "../../Data_Received/data_2013_stranding_entrapment_fieldSurvey"		# INPUT  directory
Path.out <- "../01_2013_StrandingSummary2013"						# OUTPUT directory
Path.log <- "../0_log"									# OUTPUT log  directory
if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.in)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}

# ------------------------------------------------------------------------------------------------- 
# 5. define input data files
# ------------------------------------------------------------------------------------------------- 
FL.data.csv.in <- paste(Path.in, "Stranding Data 2013_YLX.csv",sep="/")			# INPUT field stranding data file
if (!file.exists(FL.data.csv.in)){print(paste("field stranding data file \"",FL.data.csv.in," \"does not exist. Check why!"))}

# ------------------------------------------------------------------------------------------------- 
# 6. create a LOG file 
# ------------------------------------------------------------------------------------------------- 
FL.LOG  <- paste(Path.log,"01_2013_StrandingSummary2013.log",sep="/")			# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}
cat(paste("6. Paths and input files are defined!\n",sep=""))
cat(paste("6. Paths and input files are defined!\n",sep=""),file=FL.LOG,append=TRUE)


# ------------------------------------------------------------------------------------------------- 
# 7. load libraries
# -------------------------------------------------------------------------------------------------
library("lattice")
library("reshape")
library("chron")
library("cwhmisc")	# used for remove duplicate rows in a data frame
library("RODBC")
library("graphics")
library("reshape")
# library(gplots)
# library("locfit")
# library("boot")
cat(paste("7. loaded necessary libraries","\n",sep=""))
cat(paste("7. loaded necessary libraries","\n",sep=""),file=FL.LOG,append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# 8. create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
# output related to direct input 
FL.obsv.sum <- paste(Path.out,"01_2013_StrandingSummary2013_obsv_sum.csv",  sep="/")		
FL.data.sum <- paste(Path.out,"01_2013_StrandingSummary2013_data_sum.csv",  sep="/")		
FL.data.csv <- paste(Path.out,"01_2013_StrandingSummary2013_data_raw.csv",  sep="/")		
FL.tran.sum <- paste(Path.out,"01_2013_StrandingSummary2013_transect.csv",  sep="/")		
FL.data.obj <- paste(Path.out,"01_2013_StrandingSummary2013_data.Rdata",    sep="/")		


if (file.exists(FL.obsv.sum)) {print(paste(FL.obsv.sum, "exist.Delete it!"));file.remove(FL.obsv.sum)}
if (file.exists(FL.data.sum)) {print(paste(FL.data.sum, "exist.Delete it!"));file.remove(FL.data.sum)}
if (file.exists(FL.data.csv)) {print(paste(FL.data.csv, "exist.Delete it!"));file.remove(FL.data.csv)}
if (file.exists(FL.tran.sum)) {print(paste(FL.tran.sum, "exist.Delete it!"));file.remove(FL.tran.sum)}
if (file.exists(FL.data.obj)) {print(paste(FL.data.obj, "exist.Delete it!"));file.remove(FL.data.obj)}
cat(paste("8. defined output file names\n",sep=""))
cat(paste("8. defined output file names\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 9. define biweekly periods: the biweekly period supplied by Nino.  See Nino email on Sep 5, 2013 12:40 pm
# -------------------------------------------------------------------------------------------------
biweek.idx.label     <- c( 1,            2,            3,            4,            5,            6,            7,           8           )
biweek.idx.names     <- c("Feb27-Mar12","Mar13-Mar26","Mar27-Apr09","Apr10-Apr23","Apr24-May07","May08-May21","May22-Jun4","Jun05-Jun18")
biweek.idx.fullNames <- c("Feb27-Mar12","Mar13-Mar26","Mar27-Apr09","Apr10-Apr23","Apr24-May07","May08-May21","May22-Jun4","Jun05-Jun18")
names(biweek.idx.fullNames) <- biweek.idx.names

firstday.1st_biweek      <- chron(dates="02/27/2013",times="0:0:0",   format=c('m/d/y','h:m:s'))	# 
first.sampleDay          <- chron(dates="03/02/2013",times="0:0:0",   format=c('m/d/y','h:m:s'))	# variable not used: the date of first observation in the database (stranding and entrapment data spreadsheet)
last.sampleDay           <- chron(dates="06/09/2013",times="23:59:59",format=c('m/d/y','h:m:s'))	# variable not used: the date of last  observation in the database (stranding and entrapment data spreadsheet) 
cat(paste("9. The biweekly periods are defined!\n",sep=""))
cat(paste("9. The biweekly periods are defined!\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# 10. code section label
# -------------------------------------------------------------------------------------------------
section.label     <- c( 1,      2,       3)
section.names     <- c("Upper","Middle","Lower")


# -------------------------------------------------------------------------------------------------
# 11. Read in and processing the raw data
# -------------------------------------------------------------------------------------------------
# read in field stranding data
      myData.input  <- read.csv(file=FL.data.csv.in,header=TRUE,sep=",",stringsAsFactors=FALSE)
names(myData.input) <- c("sample.flag","date","section","segment","transect","FB","time","wayPoint","north","east","substrate.dominate","substrate.subDominate","embedded","vegetation","area.sampled","Chinook","NPM","StickleBack","Sculpin","Dace","Sucker","site.lab")                           
cat(paste("11a. read in 2013 stranding data from [",FL.data.csv.in,"]\n",sep=""))
cat(paste("11a. read in 2013 stranding data from [",FL.data.csv.in,"]\n",sep=""),file=FL.LOG,append=TRUE)

#
# change the NA in the fish count field into 0 when it is a sampled plot
#
index.Chinook      <- is.na(myData.input[,c("Chinook")])      & myData.input[,"sample.flag"]=="yes"
index.NPM          <- is.na(myData.input[,c("NPM")])          & myData.input[,"sample.flag"]=="yes"
index.StickleBack  <- is.na(myData.input[,c("StickleBack")])  & myData.input[,"sample.flag"]=="yes"
index.Sculpin      <- is.na(myData.input[,c("Sculpin")])      & myData.input[,"sample.flag"]=="yes"
index.Dace         <- is.na(myData.input[,c("Dace")])         & myData.input[,"sample.flag"]=="yes"
index.area.sampled <- is.na(myData.input[,c("area.sampled")]) & myData.input[,"sample.flag"]=="yes"

myData.input[index.Chinook,     c("Chinook")]      <- 0
myData.input[index.NPM,         c("NPM")]          <- 0
myData.input[index.StickleBack, c("StickleBack")]  <- 0
myData.input[index.Sculpin,     c("Sculpin")]      <- 0
myData.input[index.Dace,        c("Dace")]         <- 0
myData.input[index.area.sampled,c("area.sampled")] <- 0
cat(paste("11b. NA in the sampled plots are re-assigned to 0.\n",sep=""))
cat(paste("11b. NA in the sampled plots are re-assigned to 0.\n",sep=""),file=FL.LOG,append=TRUE)


# ------------------------------------------------------------------------------------------------- 
# 12. prepare date.chron field. split the date into year, month and day, and added a date.chron field
# -------------------------------------------------------------------------------------------------
tmp   <- unlist(strsplit(myData.input[,"date"],"/"))
month <- as.numeric(tmp[seq(from=1,to=length(tmp),by=3)])		# [day]
day   <- as.numeric(tmp[seq(from=2,to=length(tmp),by=3)])		# [month]
year  <- as.numeric(tmp[seq(from=3,to=length(tmp),by=3)]) 		# [year]

tmp   <- unlist(strsplit(myData.input[,"time"],":"))
hour  <- as.numeric(tmp[seq(from=1,to=length(tmp),by=2)])		# [hour]
minute<- as.numeric(tmp[seq(from=2,to=length(tmp),by=2)])		# [minute]

date.chron <- chron(dates = paste(month,day,year,sep="/"),
                    times = paste(hour,minute,rep("0",length(day)),sep=":"))
myData.input <- cbind(myData.input,month = month, day = day,year = year,date.chron = date.chron)
cat(paste("12. added a [date.chron] and [year], [month] and [day] fields\n",sep=""))
cat(paste("12. added a [date.chron] and [year], [month] and [day] fields\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 13. add a bi-week index.  Note: March 9th, 2012 Friday is the first sampling date
# -------------------------------------------------------------------------------------------------
myData.input <- data.frame(myData.input,						# use "date.chron.fake" only for testing the script		
	      		   biweek.idx = as.numeric(ceiling((myData.input[,"date.chron"] - firstday.1st_biweek + (1/60)/24)/14)))		# plus (5/60)/48 which is half of a 5 minute interval 

myData.input[,"biweek.idx"] <- factor(myData.input[,"biweek.idx"],levels=biweek.idx.label,labels=biweek.idx.names,ordered=TRUE)		# convert week index from number to string like Nov9, etc
cat(paste("13. added a [biweek.idx] field\n",sep=""))
cat(paste("13. added a [biweek.idx] field\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 14. reassign "site", "segment", "section" based on "transect" because there are some errors in the "segment" designation was found in the spreadsheets.  (Mar 30, 2013, transect 115 was assigned to segment 3 instead of segment 2).
# -------------------------------------------------------------------------------------------------
# re-assign "site" from "transect" assuming "transect" in the database is accurate.
myData.input[,"site.new"] <- (myData.input[,"transect"]-1)%/%4 + 1
cat(paste("14a. added a [site.new] field\n",sep=""))
cat(paste("14a. added a [site.new] field\n",sep=""),file=FL.LOG,append=TRUE)

# re-assign "segment" from "site" assuming "transect" in the database is accurate.
myData.input[,"segment.new"] <- rep(0,dim(myData.input)[1])
myData.input[(myData.input[,"site.new"] >=  1 & myData.input[,"site.new"] <= 15),"segment.new"] <- 1
myData.input[(myData.input[,"site.new"] >= 16 & myData.input[,"site.new"] <= 30),"segment.new"] <- 2

myData.input[(myData.input[,"site.new"] >= 31 & myData.input[,"site.new"] <= 40),"segment.new"] <- 3
myData.input[(myData.input[,"site.new"] >= 41 & myData.input[,"site.new"] <= 47),"segment.new"] <- 4
myData.input[(myData.input[,"site.new"] >= 48 & myData.input[,"site.new"] <= 54),"segment.new"] <- 5
myData.input[(myData.input[,"site.new"] >= 55 & myData.input[,"site.new"] <= 60),"segment.new"] <- 6

myData.input[(myData.input[,"site.new"] >= 61 & myData.input[,"site.new"] <= 77),"segment.new"] <- 7
myData.input[(myData.input[,"site.new"] >= 78 & myData.input[,"site.new"] <= 90),"segment.new"] <- 8
cat(paste("14b. added a [segment.new] field based on [site.new] which is based on [transect].\n",sep=""))
cat(paste("14b. added a [segment.new] field based on [site.new] which is based on [transect].\n",sep=""),file=FL.LOG,append=TRUE)

# re-assign "section" from "segment" assuming "transect" in the database is accurate.
myData.input[,"section.new"] <- rep(0,dim(myData.input)[1])
myData.input[(myData.input[,"segment.new"] >= 1 & myData.input[,"segment.new"] <= 2),"section.new"] <- 1
myData.input[(myData.input[,"segment.new"] >= 3 & myData.input[,"segment.new"] <= 6),"section.new"] <- 2
myData.input[(myData.input[,"segment.new"] >= 7 & myData.input[,"segment.new"] <= 8),"section.new"] <- 3
cat(paste("14c. added a [section.new] which is based on [segment.new] which is based on [site.new] which is based on [transect].\n",sep=""))
cat(paste("14c. added a [section.new] which is based on [segment.new] which is based on [site.new] which is based on [transect].\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 15. turn "section" to factor
# -------------------------------------------------------------------------------------------------
myData.input[,"section.new"] <- factor(myData.input[,"section.new"],levels=section.label,labels=section.names,ordered=TRUE)		# convert week index from number to string like Nov9, etc
cat(paste("15. turn [section.new] into a factor.\n",sep=""))
cat(paste("15. turn [section.new] into a factor.\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 15. Table.Frequency: the lumping in bootstrape is based on this.
# -------------------------------------------------------------------------------------------------
# to get the distribution of the number of plots sampled at each plot (actually combination of date and transect or combination of biweek idx and transect)
# to check if there are multiple visit to a transect within a given biweek.idx, do this on both [transect_biweek] and [transect_date].
# if no multiple visit to a transect, the results based on "biweek.idx" and "date" will be the same.
myData.input[,"transect_biweek"] <- paste(paste("transect",myData.input[,"transect"],sep=""),myData.input[,"biweek.idx"],sep="_")


# -------------------------------------------------------------------------------------------------
# 16. retain only necessary fields for summarizing (a) "area.sampled" and (b) "Chinook".  about cast/melt, see Yulong to Chris's email on Sep 12, 2103 9:54 am.
# -------------------------------------------------------------------------------------------------
id.vars         <- c("sample.flag","section.new","segment.new","site.new","transect","biweek.idx")	# where "area.sampled" and "Chinook" are used as "variable" and the other are used as identifiers
measure.vars    <- c("area.sampled","Chinook")						# where "area.sampled" and "Chinook" are used as "variable" and the other are used as identifiers
myAreaFish      <- myData.input[,c(id.vars,measure.vars)]		
myAreaFish.melt <- melt(myAreaFish,id.vars=id.vars,measure.vars=measure.vars,variable_name = "variable",na.rm=FALSE)
cat(paste("melt [myAreaFish] into [myAreaFish.melt].\n",sep=""))
cat(paste("melt [myAreaFish] into [myAreaFish.melt].\n",sep=""),file=FL.LOG,append=TRUE)

#
# summary of "area.sampled" and "Chinook": based on "area.sampled" == "yes" samples only
#
myArea.section.biweek  <- cast(myAreaFish.melt, section.new ~ biweek.idx, subset=variable=="area.sampled" & sample.flag == "yes",sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))
myArea.segment.biweek  <- cast(myAreaFish.melt, segment.new ~ biweek.idx, subset=variable=="area.sampled" & sample.flag == "yes",sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))
myArea.site.biweek     <- cast(myAreaFish.melt,    site.new ~ biweek.idx, subset=variable=="area.sampled" & sample.flag == "yes",sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))

myFish.section.biweek  <- cast(myAreaFish.melt, section.new ~ biweek.idx, subset=variable=="Chinook"      & sample.flag == "yes",sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))
myFish.segment.biweek  <- cast(myAreaFish.melt, segment.new ~ biweek.idx, subset=variable=="Chinook"      & sample.flag == "yes",sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))
myFish.site.biweek     <- cast(myAreaFish.melt,    site.new ~ biweek.idx, subset=variable=="Chinook"      & sample.flag == "yes",sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))

#
# summary of number of observations (a) "tot" | (b) "sampled" |  (c) "not sampled".  They summary should be the same if we are based on either "area.sampled" (myObs2) or "Chinook" (myObs1) data.
#
# the number of observations for "tot" samples based on "Chinook" data (myObs1)
myObs1.tot.section.biweek <- cast(myAreaFish.melt, section.new ~ biweek.idx, subset=variable=="Chinook",     length, margins=c("grand_row", "grand_col"))
myObs1.tot.segment.biweek <- cast(myAreaFish.melt, segment.new ~ biweek.idx, subset=variable=="Chinook",     length, margins=c("grand_row", "grand_col"))
myObs1.tot.site.biweek    <- cast(myAreaFish.melt,    site.new ~ biweek.idx, subset=variable=="Chinook",     length, margins=c("grand_row", "grand_col"))

# the number of observations for "Sampled" samples based on "Chinook" data (myObs1)
myObs1.yes.section.biweek <- cast(myAreaFish.melt, section.new ~ biweek.idx, subset=variable=="Chinook"      & sample.flag == "yes",length, margins=c("grand_row", "grand_col"))
myObs1.yes.segment.biweek <- cast(myAreaFish.melt, segment.new ~ biweek.idx, subset=variable=="Chinook"      & sample.flag == "yes",length, margins=c("grand_row", "grand_col"))
myObs1.yes.site.biweek    <- cast(myAreaFish.melt,    site.new ~ biweek.idx, subset=variable=="Chinook"      & sample.flag == "yes",length, margins=c("grand_row", "grand_col"))

# the number of observations for "Not Sampled" samples based on "Chinook" data (myObs1)
myObs1.no.section.biweek  <- cast(myAreaFish.melt, section.new ~ biweek.idx, subset=variable=="Chinook"      & sample.flag == "no",length, margins=c("grand_row", "grand_col"))
myObs1.no.segment.biweek  <- cast(myAreaFish.melt, segment.new ~ biweek.idx, subset=variable=="Chinook"      & sample.flag == "no",length, margins=c("grand_row", "grand_col"))
myObs1.no.site.biweek     <- cast(myAreaFish.melt,    site.new ~ biweek.idx, subset=variable=="Chinook"      & sample.flag == "no",length, margins=c("grand_row", "grand_col"))

# the number of observations for "tot" samples based on both "area.sampled" (myObs2)
myObs2.tot.section.biweek <- cast(myAreaFish.melt, section.new ~ biweek.idx, subset=variable=="area.sampled",length, margins=c("grand_row", "grand_col"))
myObs2.tot.segment.biweek <- cast(myAreaFish.melt, segment.new ~ biweek.idx, subset=variable=="area.sampled",length, margins=c("grand_row", "grand_col"))
myObs2.tot.site.biweek    <- cast(myAreaFish.melt,    site.new ~ biweek.idx, subset=variable=="area.sampled",length, margins=c("grand_row", "grand_col"))

# the number of observations for "Sampled" samples based on both "area.sampled" (myObs2)
myObs2.yes.section.biweek <- cast(myAreaFish.melt, section.new ~ biweek.idx, subset=variable=="area.sampled" & sample.flag == "yes",length, margins=c("grand_row", "grand_col"))
myObs2.yes.segment.biweek <- cast(myAreaFish.melt, segment.new ~ biweek.idx, subset=variable=="area.sampled" & sample.flag == "yes",length, margins=c("grand_row", "grand_col"))
myObs2.yes.site.biweek    <- cast(myAreaFish.melt,    site.new ~ biweek.idx, subset=variable=="area.sampled" & sample.flag == "yes",length, margins=c("grand_row", "grand_col"))

# the number of observations for "Not Sampled" samples based on both "area.sampled" (myObs2)
myObs2.no.section.biweek  <- cast(myAreaFish.melt, section.new ~ biweek.idx, subset=variable=="area.sampled" & sample.flag == "no",length, margins=c("grand_row", "grand_col"))
myObs2.no.segment.biweek  <- cast(myAreaFish.melt, segment.new ~ biweek.idx, subset=variable=="area.sampled" & sample.flag == "no",length, margins=c("grand_row", "grand_col"))
myObs2.no.site.biweek     <- cast(myAreaFish.melt,    site.new ~ biweek.idx, subset=variable=="area.sampled" & sample.flag == "no",length, margins=c("grand_row", "grand_col"))

cat(paste("summary of \"area.sampled\" and \"Chinook\" are completed.\n",sep=""))
cat(paste("summary of \"area.sampled\" and \"Chinook\" are completed.\n",sep=""),file=FL.LOG,append=TRUE)
# -------------------------------------------------------------------------------------------------
# the arrays generated in this block of code
# -------------------------------------------------------------------------------------------------
# myAreaFish          		
# myAreaFish.melt 		
# myArea.section.biweek  		
# myArea.segment.biweek  		
# myArea.site.biweek     		
# myFish.section.biweek  		
# myFish.segment.biweek  
# myFish.site.biweek
# myObs1.tot.section.biweek 	
# myObs1.tot.segment.biweek 	
# myObs1.tot.site.biweek    	
# myObs1.yes.section.biweek 	
# myObs1.yes.segment.biweek 	
# myObs1.yes.site.biweek    	
# myObs1.no.section.biweek  	
# myObs1.no.segment.biweek  	
# myObs1.no.site.biweek     	
# myObs2.tot.section.biweek 	
# myObs2.tot.segment.biweek 	
# myObs2.tot.site.biweek    	
# myObs2.yes.section.biweek 	
# myObs2.yes.segment.biweek 	
# myObs2.yes.site.biweek    	
# myObs2.no.section.biweek  	
# myObs2.no.segment.biweek  	
# myObs2.no.site.biweek     	
# -------------------------------------------------------------------------------------------------




# -------------------------------------------------------------------------------------------------
# 17. retain only necessary fields for summarizing (a) "visited Transect", (b) "sampled Transects" and (c) "not sampled Transects"
#     in summary table:
#         visited: plots visited no matter if it has been sampled, which will be calculated based on all the data
#         sampled: plots visited and sampled (with no-zero sampled area), which will be calculated based on the sampled ("yes") data only.
#     not sampled: plots visited but not sampled, which cannot be calculated based on the "no" data, because most transects in "no" subsets also have sampled in the "yes" sample.  only transects appears in "no" sample but not appears in the "yes" sample should be counted, 
#                  not sampled are calculated by the difference between the "visited" and the "sampled".
#     this assume that the first portion of 734 data in the stranding database are for visited and sampled and the last 288 data are visited but not sampled!
# -------------------------------------------------------------------------------------------------
fn_Transect <- function(x){tmp.no.Transect <- length(unique(x))}				# count the number of Transects

id.vars          <- c("sample.flag","section.new","segment.new","site.new","biweek.idx")	# where "transect" is used as "variable" and the other are used as identifiers
measure.vars     <- c("transect_biweek")							# where "transect" is used as "variable" and the other are used as identifiers
myTransect       <- myData.input[,c(id.vars,measure.vars)]		
myTransect.melt  <- melt(myTransect,id.vars=id.vars,measure.vars=measure.vars,variable_name = "variable",na.rm=FALSE)
cat(paste("melt [myTransect] into [myTransect.melt].\n",sep=""))
cat(paste("melt [myTransect] into [myTransect.melt].\n",sep=""),file=FL.LOG,append=TRUE)

# transects "Visited"
myTransect.tot.section.biweek <- cast(myTransect.melt, section.new ~ biweek.idx, subset=variable=="transect_biweek",fn_Transect, margins=c("grand_row", "grand_col"))
myTransect.tot.segment.biweek <- cast(myTransect.melt, segment.new ~ biweek.idx, subset=variable=="transect_biweek",fn_Transect, margins=c("grand_row", "grand_col"))
myTransect.tot.site.biweek    <- cast(myTransect.melt,    site.new ~ biweek.idx, subset=variable=="transect_biweek",fn_Transect, margins=c("grand_row", "grand_col"))
row.names(myTransect.tot.section.biweek) <- myTransect.tot.section.biweek[,1]
row.names(myTransect.tot.segment.biweek) <- myTransect.tot.segment.biweek[,1]
row.names(myTransect.tot.site.biweek)    <- myTransect.tot.site.biweek[,1]     
cat(paste("transect (visited) are counted.\n",sep=""))
cat(paste("transect (visited) are counted.\n",sep=""),file=FL.LOG,append=TRUE)

# transects "Sampled"
myTransect.yes.section.biweek <- cast(myTransect.melt, section.new ~ biweek.idx, subset=variable=="transect_biweek" & sample.flag == "yes",fn_Transect, margins=c("grand_row", "grand_col"))
myTransect.yes.segment.biweek <- cast(myTransect.melt, segment.new ~ biweek.idx, subset=variable=="transect_biweek" & sample.flag == "yes",fn_Transect, margins=c("grand_row", "grand_col"))
myTransect.yes.site.biweek    <- cast(myTransect.melt,    site.new ~ biweek.idx, subset=variable=="transect_biweek" & sample.flag == "yes",fn_Transect, margins=c("grand_row", "grand_col"))
row.names(myTransect.yes.section.biweek) <- myTransect.yes.section.biweek[,1]
row.names(myTransect.yes.segment.biweek) <- myTransect.yes.segment.biweek[,1]
row.names(myTransect.yes.site.biweek)    <- myTransect.yes.site.biweek[,1]     
cat(paste("transect (sampled) are counted.\n",sep=""))
cat(paste("transect (sampled) are counted.\n",sep=""),file=FL.LOG,append=TRUE)

# combine the "tot" and "yes" arrays to the calculation of "no" array.  "merge" with "TRUE" option is needed because the two arrays may have different entries.
myTransect.combine.section.biweek <- merge(myTransect.tot.section.biweek,myTransect.yes.section.biweek,by.x="section.new",by.y="section.new",all=TRUE,suffixes=c("(tot)","(yes)"))
myTransect.combine.segment.biweek <- merge(myTransect.tot.segment.biweek,myTransect.yes.segment.biweek,by.x="segment.new",by.y="segment.new",all=TRUE,suffixes=c("(tot)","(yes)"))
myTransect.combine.site.biweek    <- merge(myTransect.tot.site.biweek,   myTransect.yes.site.biweek,   by.x="site.new",   by.y="site.new",   all=TRUE,suffixes=c("(tot)","(yes)"))

# reorder
myTransect.combine.section.biweek <- myTransect.combine.section.biweek[c(4,3,2,1),]
myTransect.combine.segment.biweek <- myTransect.combine.segment.biweek[c(2,3,4,5,6,7,8,9,1),]
cat(paste("transect arrays (visitied) and (sampled) are combined.\n",sep=""))
cat(paste("transect arrays (visitied) and (sampled) are combined.\n",sep=""),file=FL.LOG,append=TRUE)

# replace "NA" in the combined data frame with 0s: if we need to calculate "no" as the difference between "tot" and "yes", we cannot have NA in the arrays.
myTransect.combine.section.biweek[is.na(myTransect.combine.section.biweek)] <- 0
myTransect.combine.segment.biweek[is.na(myTransect.combine.segment.biweek)] <- 0
myTransect.combine.site.biweek[is.na(myTransect.combine.site.biweek)] <- 0

# calcuate the "Not Sampled" arrays: tmp arrays are needed because the column names are different for the "tot" and "yes" columns.  We need to make them the same.
tmp.tot <- myTransect.combine.section.biweek[,grep("\\(tot\\)",names(myTransect.combine.section.biweek),value=TRUE)] 
tmp.yes <- myTransect.combine.section.biweek[,grep("\\(yes\\)",names(myTransect.combine.section.biweek),value=TRUE)]
names(tmp.tot) <- sub("\\(tot\\)","",names(tmp.tot))
names(tmp.yes) <- sub("\\(yes\\)","",names(tmp.yes))
myTransect.no.section.biweek <- cbind(section=myTransect.combine.section.biweek[,1],tmp.tot - tmp.yes)

tmp.tot <- myTransect.combine.segment.biweek[,grep("\\(tot\\)",names(myTransect.combine.segment.biweek),value=TRUE)] 
tmp.yes <- myTransect.combine.segment.biweek[,grep("\\(yes\\)",names(myTransect.combine.segment.biweek),value=TRUE)]
names(tmp.tot) <- sub("\\(tot\\)","",names(tmp.tot))
names(tmp.yes) <- sub("\\(yes\\)","",names(tmp.yes))
myTransect.no.segment.biweek <-  cbind(segment=myTransect.combine.segment.biweek[,1],tmp.tot - tmp.yes)

tmp.tot <- myTransect.combine.site.biweek[,grep("\\(tot\\)",names(myTransect.combine.site.biweek),value=TRUE)] 
tmp.yes <- myTransect.combine.site.biweek[,grep("\\(yes\\)",names(myTransect.combine.site.biweek),value=TRUE)]
names(tmp.tot) <- sub("\\(tot\\)","",names(tmp.tot))
names(tmp.yes) <- sub("\\(yes\\)","",names(tmp.yes))
myTransect.no.site.biweek <-  cbind(site=myTransect.combine.site.biweek[,1],tmp.tot - tmp.yes)
cat(paste("summary of number of \"transect\" are completed.\n",sep=""))
cat(paste("summary of number of \"transect\" are completed.\n",sep=""),file=FL.LOG,append=TRUE)
# -------------------------------------------------------------------------------------------------
# the arrays generated in this block
# -------------------------------------------------------------------------------------------------
# myTransect       			
# myTransect.melt  			
# myTransect.tot.section.biweek 	
# myTransect.tot.segment.biweek 	
# myTransect.tot.site.biweek    	
# myTransect.yes.section.biweek 	
# myTransect.yes.segment.biweek 	
# myTransect.yes.site.biweek    	
# myTransect.combine.section.biweek 	
# myTransect.combine.segment.biweek 	
# myTransect.combine.site.biweek    	
# myTransect.no.section.biweek 		
# myTransect.no.segment.biweek 		
# myTransect.no.site.biweek 		
# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# 18a. [FL.data.sum]: write the data out to files: [myArea.section.biweek],[myArea.segment.biweek],[myArea.site.biweek],[myFish.section.biweek],[myFish.segment.biweek],[myFish.site.biweek] to [FL.data.sum]
# -------------------------------------------------------------------------------------------------
# sampled area summary
cat(paste("[myArea.section.biweek],",sep=""),file=FL.data.sum,append=TRUE)
write.table(myArea.section.biweek,file=FL.data.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myArea.segment.biweek],",sep=""),file=FL.data.sum,append=TRUE)
write.table(myArea.segment.biweek,file=FL.data.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myArea.site.biweek],",sep=""),file=FL.data.sum,append=TRUE)
write.table(myArea.site.biweek,file=FL.data.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("summary of \"area sampled\" outputted.\n",sep=""))
cat(paste("summary of \"area sampled\" outputted.\n",sep=""),file=FL.LOG,append=TRUE)


# Chinook count summary
cat(paste("\n\n[myFish.section.biweek],",sep=""),file=FL.data.sum,append=TRUE)
write.table(myFish.section.biweek,file=FL.data.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myFish.segment.biweek],",sep=""),file=FL.data.sum,append=TRUE)
write.table(myFish.segment.biweek,file=FL.data.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myFish.site.biweek],",sep=""),file=FL.data.sum,append=TRUE)
write.table(myFish.site.biweek,file=FL.data.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("summary of \"Chinook\" count outputted.\n",sep=""))
cat(paste("summary of \"Chinook\" count outputted.\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# 18b. [FL.data.csv]: write the data out to files: [(myAreaFish],[(myAreaFish.melt],[myTransect],[myTransect.melt] to [FL.data.csv]
# -------------------------------------------------------------------------------------------------
# "area sampled" and "Chinook"
cat(paste("\n\n[myAreaFish],",sep=""),file=FL.data.csv,append=TRUE)
write.table(myAreaFish,file=FL.data.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myAreaFish.melt],",sep=""),file=FL.data.csv,append=TRUE)
write.table(myAreaFish.melt,file=FL.data.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("Data of \"area sampled\" and \"Chinook\" are outputted.\n",sep=""))
cat(paste("Data of \"area sampled\" and \"Chinook\" are outputted.\n",sep=""),file=FL.LOG,append=TRUE)


# "transects"
cat(paste("\n\n[myTransect],",sep=""),file=FL.data.csv,append=TRUE)
write.table(myTransect,file=FL.data.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myTransect.melt],",sep=""),file=FL.data.csv,append=TRUE)
write.table(myTransect.melt,file=FL.data.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("Data of \"transect\" are outputted.\n",sep=""))
cat(paste("Data of \"transect\" are outputted.\n",sep=""),file=FL.LOG,append=TRUE)




# -------------------------------------------------------------------------------------------------
# 18c. [FL.obsv.sum]: write the summary out to files for [observation distribution] into [FL.obsv.sum]
# -------------------------------------------------------------------------------------------------
cat(paste("\n\n[myObs1.tot.section.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs1.tot.section.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs1.tot.segment.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs1.tot.segment.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs1.tot.site.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs1.tot.site.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

# write the summary out to files
cat(paste("\n\n[myObs2.tot.section.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs2.tot.section.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs2.tot.segment.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs2.tot.segment.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs2.tot.site.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs2.tot.site.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

# write the summary out to files
cat(paste("\n\n[myObs1.yes.section.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs1.yes.section.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs1.yes.segment.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs1.yes.segment.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs1.yes.site.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs1.yes.site.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

# write the summary out to files
cat(paste("\n\n[myObs2.yes.section.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs2.yes.section.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs2.yes.segment.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs2.yes.segment.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs2.yes.site.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs2.yes.site.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

# write the summary out to files
cat(paste("\n\n[myObs1.no.section.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs1.no.section.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs1.no.segment.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs1.no.segment.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs1.no.site.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs1.no.site.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

# write the summary out to files
cat(paste("\n\n[myObs2.no.section.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs2.no.section.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs2.no.segment.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs2.no.segment.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myObs2.no.site.biweek],",sep=""),file=FL.obsv.sum,append=TRUE)
write.table(myObs2.no.site.biweek,file=FL.obsv.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("summary of observation distribution is outputted.\n",sep=""))
cat(paste("summary of observation distribution is outputted.\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# 18d. [FL.tran.sum]: write the summary out to files for [transects] into [FL.tran.sum]
# -------------------------------------------------------------------------------------------------
# total transects "visited"
cat(paste("\n\n[myTransect.tot.section.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.tot.section.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myTransect.tot.segment.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.tot.segment.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myTransect.tot.site.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.tot.site.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

# transects "sampled"
cat(paste("\n\n[myTransect.yes.section.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.yes.section.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myTransect.yes.segment.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.yes.segment.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myTransect.yes.site.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.yes.site.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

# combined the "visited" and the "sampled" transects
cat(paste("\n\n[myTransect.combine.section.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.combine.section.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myTransect.combine.segment.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.combine.segment.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myTransect.combine.site.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.combine.site.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

# transects "not sampled"
cat(paste("\n\n[myTransect.no.section.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.no.section.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myTransect.no.segment.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.no.segment.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\n[myTransect.no.site.biweek],",sep=""),file=FL.tran.sum,append=TRUE)
write.table(myTransect.no.site.biweek,file=FL.tran.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("summary of transects is outputted.\n",sep=""))
cat(paste("summary of transects is outputted.\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 19. save the arrays
# -------------------------------------------------------------------------------------------------
save(list = ls(all=TRUE),file=FL.data.obj)
cat(paste("save all objects created into [",FL.data.obj,"].\n",sep=""))
cat(paste("save all objects created into [",FL.data.obj,"].\n",sep=""),file=FL.LOG,append=TRUE)
 
 
 
# -------------------------------------------------------------------------------------------------
# 20. time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n01_2013_StrandingSummary2013.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n01_2013_StrandingSummary2013.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [01_2013_StrandingSummary2013.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [01_2013_StrandingSummary2013.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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



# -------------------------------------------------------------------------------------------------------------------------------
# format of input stranding data file C:/yulong_projects/FY2013_SalmonStrandingFieldSurvey/Data_Received/data_2013_stranding_entrapment_fieldSurvey/Stranding Data 2013_YLX.csv	# The csv version is:
# -------------------------------------------------------------------------------------------------------------------------------
# sampled,Date,River SectioNo,SegmeNot,TraNosect,FB Yes_No,Time,WaypoiNot,Coord No,Coord E,Dom_Sub,SubDom_Sub,Embedded,VegetatioNo,Area Sampled,ChiNoook,NoPM,Stickleback,SculpiNo,Dace,Sucker,Site
# yes,3/2/2013,2,6,221,Yes,10:36,B221LS03,46.57275,119.34306,9,9,1,1,74.7,,,,,,,56
# yes,3/2/2013,2,6,221,Yes,10:15,B221LS01,46.57273,119.34286,8,9,3,1,78.5,,,,,,,56
# yes,3/2/2013,2,6,221,Yes,10:25,B221LS02,46.57272,119.34277,1,9,3,2,63.74,,,,,,,56
# yes,3/3/2013,1,2,82,Yes,11:37,B082LS01,46.39145,119.36725,9,9,1,1,13.3,,,,,,,21
# yes,3/5/2013,2,3,159,Yes,12:07,B159RS01,46.40528,119.27645,5,7,2,1,18.9,,,,,,,40
# yes,3/5/2013,2,3,159,Yes,12:11,B159RS02,46.40533,119.27648,5,7,2,1,18.9,,,,,,,40
# yes,3/5/2013,2,3,159,Yes,12:16,B159RS03,46.40525,119.27643,5,7,2,1,18.9,,,,,,,40
# yes,3/5/2013,2,3,159,Yes,12:35,B159LS01,46.40616,119.27176,1,1,4,1,27.4,,,,,,,40
# yes,3/5/2013,2,3,159,Yes,12:37,B159LS02,46.40621,119.27173,1,1,4,1,18.9,,,,,,,40
# yes,3/5/2013,2,3,159,Yes,12:42,B159LS03,46.4061,119.27179,1,1,4,1,18.9,,,,,,,40
# yes,3/5/2013,2,4,169,Yes,9:49,B169RS01,46.39531,119.26266,6,8,2,1,31.9,1,,,,,,43
# yes,3/5/2013,2,4,169,Yes,10:10,B169RS02,46.39527,119.26262,6,8,2,1,34.9,,,,,,,43
# yes,3/5/2013,2,4,169,Yes,10:30,B169RS03,46.39537,119.26271,6,8,2,1,27.4,,,,,,,43
# yes,3/5/2013,2,4,169,Yes,11:15,B169LS01,46.39844,119.25908,1,1,4,1,70.4,,,,,,,43
# 



