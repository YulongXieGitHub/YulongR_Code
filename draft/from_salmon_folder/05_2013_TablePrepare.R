#
# 05_2013_TablePrepare.R
#
# October 1, 2013: use this script to prepare tables which are similar to the table we have in previous years.
#
# Sep 9, 2013: "05_2013_TablePrepare.R" based on "C:\YuLong_Projects\FY2012_SalmonStrandingFieldSurvey_2012March_June\0_scripts\2012_HanfordReach_StrandingSurvey_2012March_June_revised.R"
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
Path.out <- "../05_2013_TablePrepare"							# OUTPUT directory
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
FL.LOG  <- paste(Path.log,"05_2013_TablePrepare.log",sep="/")				# OUTPUT Log file
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
FL.QA.csv    <- paste(Path.out,"05_2013_StrandingSummary2013_data_QA.csv",sep="/")	
FL.data.obj  <- paste(Path.out,"05_2013_StrandingSummary2013_data.Rdata", sep="/")		
FL.table.csv <- paste(Path.out,"05_2013_StrandingSummary2013_Table.csv",  sep="/")		
if (file.exists(FL.QA.csv))   {print(paste(FL.QA.csv,   "exist.Delete it!"));file.remove(FL.QA.csv)}
if (file.exists(FL.data.obj)) {print(paste(FL.data.obj, "exist.Delete it!"));file.remove(FL.data.obj)}
if (file.exists(FL.table.csv)){print(paste(FL.table.csv,"exist.Delete it!"));file.remove(FL.table.csv)}
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
cat(paste("10. section label is defined!\n",sep=""))
cat(paste("10. section label is defined!\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 11. Read in and processing the raw data
# -------------------------------------------------------------------------------------------------
# read in field stranding data
      myData.input  <- read.csv(file=FL.data.csv.in,header=TRUE,sep=",",stringsAsFactors=FALSE)
names(myData.input) <- c("sample.flag","date","section","segment","transect","FB","time","wayPoint","north","east","substrate.dominate","substrate.subDominate","embedded","vegetation","area.sampled","Chinook","NPM","StickleBack","Sculpin","Dace","Sucker","site")                          
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
# 15a. check the "site", "segment", "section" designation in the data based against the re-assigned one based on "transect"
# -------------------------------------------------------------------------------------------------
idx.mismatch <- (myData.input[,"section.new"] != myData.input[,"section"]) | (myData.input[,"segment.new"] != myData.input[,"segment"]) | (myData.input[,"site.new"] != myData.input[,"site"])
 no.mismatch <- sum(idx.mismatch[idx.mismatch])
   myData.QA <- myData.input[idx.mismatch,]
if ( no.mismatch > 0)
{
	cat(paste("there are ",no.mismatch," mismatches in the [site], [segment], [section] designation in the database!\n",sep=""))	
	cat(paste("there are ",no.mismatch," mismatches in the [site], [segment], [section] designation in the database!\n",sep=""),file=FL.LOG,append=TRUE)
	cat(paste("there are ",no.mismatch," mismatches in the [site], [segment], [section] designation in the database!\n",sep=""),file=FL.QA.csv,append=TRUE)
}
cat(paste("mis-matched in terms of site/segment/section,",sep=""),file=FL.QA.csv,append=TRUE)
write.table(myData.QA,file=FL.QA.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("15a. any mis-match in the [site], [segment], [section] between what originally in the data and derived from [transect] are reported to [",FL.QA.csv,"].\n",sep=""))
cat(paste("15a. any mis-match in the [site], [segment], [section] between what originally in the data and derived from [transect] are reported to [",FL.QA.csv,"].\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 15b. check the inconsistency between "sampled with "yes" but with no sampled area value.
# -------------------------------------------------------------------------------------------------
idx.mismatch <- ((myData.input[,"sample.flag"] == "yes") & ((myData.input[,"area.sampled"] <= 0) | (is.na(myData.input[,"area.sampled"]))))
 no.mismatch <- sum(idx.mismatch[idx.mismatch])
   myData.QA <- myData.input[idx.mismatch,]
if (no.mismatch > 0)
{
	cat(paste("there are ",no.mismatch," mismatches entries with [yes] designation on sampled field but there is no area sampled!\n",sep=""))
		cat(paste("there are ",no.mismatch," mismatches entries with [yes] designation on sampled field but there is no area sampled!\n",sep=""),file=FL.LOG,append=TRUE)
		cat(paste("\n\nthere are ",no.mismatch," mismatches entries with [yes] designation on sampled field but there is no area sampled!\n",sep=""),file=FL.QA.csv,append=TRUE)

}
cat(paste("mis-matched in terms of missing or 0 sampled area,",sep=""),file=FL.QA.csv,append=TRUE)
write.table(myData.QA,file=FL.QA.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("15b. any mis-match in the [sample.flag] and [area.sample] are reported to [",FL.QA.csv,"].\n",sep=""))
cat(paste("15b. any mis-match in the [sample.flag] and [area.sample] are reported to [",FL.QA.csv,"].\n",sep=""),file=FL.LOG,append=TRUE)




# -------------------------------------------------------------------------------------------------
# 16. turn "section" to factor
# -------------------------------------------------------------------------------------------------
myData.input[,"section.new"] <- factor(myData.input[,"section.new"],levels=section.label,labels=section.names,ordered=TRUE)		# convert week index from number to string like Nov9, etc
cat(paste("16. turn [section.new] into a factor.\n",sep=""))
cat(paste("16. turn [section.new] into a factor.\n",sep=""),file=FL.LOG,append=TRUE)




# -------------------------------------------------------------------------------------------------
# 17. retain only useful fields of [myData.input] and put into [myData.work]
# -------------------------------------------------------------------------------------------------
myData.work <- myData.input[,c("sample.flag","date","site.new","section.new","segment.new","transect","time","area.sampled","Chinook","biweek.idx")]
cat(paste("17. retain only useful fields of [myData.input] and put into [myData.work].\n",sep=""))
cat(paste("17. retain only useful fields of [myData.input] and put into [myData.work].\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# 18. Table 4 at page 14: the lumping in bootstrape is based on this.
# -------------------------------------------------------------------------------------------------
# to get the distribution of the number of plots sampled at each plot (actually combination of date and transect or combination of biweek idx and transect)
# to check if there are multiple visit to a transect within a given biweek.idx, do this on both [transect_biweek] and [transect_date].
# if no multiple visit to a transect, the results based on "biweek.idx" and "date" will be the same.
#
myData.work[,"transect_biweek"] <- paste(paste("transect",myData.work[,"transect"],sep=""),myData.work[,"biweek.idx"],sep="_")
myData.work[,"transect_date"]   <- paste(paste("transect",myData.work[,"transect"],sep=""),myData.work[,"date"],sep="_")


myData.sampled <- myData.work[myData.work[,"sample.flag"]=="yes",]
no.plot.in.transect.all <- tapply(myData.sampled[,"Chinook"],myData.sampled[,"transect_date"],function(x){length(x)})	# the number of plots (i.e., samples)                                 in each of the transect sampled in each biweekly period
no.plot.in.transect.yes <- tapply(myData.sampled[,"Chinook"],myData.sampled[,"transect_date"],function(x){sum(x>0)})	# the number of plots (i.e., samples) with at least one fish stranded in each of the transect sampled in each biweekly period
no.plot.in.transect     <- cbind(no.plot.in.transect.all,no.plot.in.transect.yes)

freq.plot.in.transect.all <- tapply(no.plot.in.transect[,"no.plot.in.transect.all"],no.plot.in.transect[,"no.plot.in.transect.all"],function(x){length(x)})
freq.plot.in.transect.yes <- tapply(no.plot.in.transect[,"no.plot.in.transect.yes"],no.plot.in.transect[,"no.plot.in.transect.all"],function(x){sum(x>0)})

      Table.Frequency  <- data.frame(cbind(freq.plot.in.transect.all,freq.plot.in.transect.yes))     
names(Table.Frequency) <- c("No. of Transects","No. of Transects with non-zero mortality")
Table.Frequency[,"Plots in Transect"] <- as.numeric(row.names(Table.Frequency))
Table.Frequency[,"%"]                 <- 100*(Table.Frequency[,"No. of Transects"]/sum(Table.Frequency[,"No. of Transects"]))
Table.Frequency[,"No. of Plots"]      <- Table.Frequency[,"Plots in Transect"]*Table.Frequency[,"No. of Transects"]
Table.Frequency[,"cum%"]              <- cumsum(Table.Frequency[,"%"])

# sort according to the "Plots in Transect"
O <- order(Table.Frequency[,"Plots in Transect"],decreasing = TRUE)
Table.Frequency <- Table.Frequency[O,c("Plots in Transect","No. of Transects","%","cum%","No. of Transects with non-zero mortality","No. of Plots")]
Total  <- apply(Table.Frequency,2,sum)
Total[1] <- ""
Table.Frequency <- rbind(Table.Frequency,Total = Total)

cat(paste("Table.Frequncy,",sep=""),file=FL.table.csv,append=TRUE)
write.table(Table.Frequency,file=FL.table.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("18. The frequency table of the number of plots sampled per transect.\n",sep=""))
cat(paste("18. The frequency table of the number of plots sampled per transect.\n",sep=""),file=FL.LOG,append=TRUE)


# open pdf file for outputting plots
FL.pdf.out <- paste(Path.out,"05_2013_plots_surveyed.pdf",  sep="/")
if  (file.exists(FL.pdf.out)){print(paste(FL.pdf.out,"exist.Delete it!"));file.remove(FL.pdf.out)}
data.4.plot <- sort(no.plot.in.transect[,"no.plot.in.transect.all"])
pdf(file = FL.pdf.out,         paper="a4r",width=0,height=0)	
	# histogram
	hist(data.4.plot,main=paste("Histogram of plots surveyed in each transect"),xlab="Number of plots",nclass=15,col="cyan",border="black",freq=TRUE)

	# bar plots
	barplot(data.4.plot,main=paste("bar plot of plots surveyed in each transect"),ylab="Number of plots",col="cyan",border="black")
dev.off()




hist(no.plot.in.transect[,"no.plot.in.transect.all"],main=paste("Histogram of plots surveyed in each transect"),xlab="Number of plots",nclass=15,col="cyan",border="black",freq=TRUE)



# -------------------------------------------------------------------------------------------------
# 19. retain only necessary fields for summarizing (a) "area.sampled" and (b) "Chinook".  about cast/melt, see Yulong to Chris's email on Sep 12, 2103 9:54 am.
# -------------------------------------------------------------------------------------------------
id.vars         <- c("sample.flag","section.new","segment.new","site.new","transect","biweek.idx")	# where "area.sampled" and "Chinook" are used as "variable" and the other are used as identifiers
measure.vars    <- c("area.sampled","Chinook")								# where "area.sampled" and "Chinook" are used as "variable" and the other are used as identifiers
myAreaFish      <- myData.input[,c(id.vars,measure.vars)]		
myAreaFish.melt <- melt(myAreaFish,id.vars=id.vars,measure.vars=measure.vars,variable_name = "variable",na.rm=FALSE)
cat(paste("19. prepare [myAreaFish] and melt [myAreaFish] into [myAreaFish.melt].\n",sep=""))
cat(paste("19. prepare [myAreaFish] and melt [myAreaFish] into [myAreaFish.melt].\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 20. prepare Table7_page24 /Table8_page24  : add a new field [transect.biweek] to identify the unique transect sampled in the two week period which is the comboination of "transect" and "biweekly period". (note: the same transect can be sampled multiple time during different biweekly period)
# -------------------------------------------------------------------------------------------------
myAreaFish[,"transect.biweek"] <- paste(myAreaFish[,"transect"],"_",myAreaFish[,"biweek.idx"],sep="")

# 20a. Table7_page24
transect.visited           <- tapply(myAreaFish[,                                 "transect.biweek"],myAreaFish[,                                 "segment.new"],function(x){length(unique(x))})		# the number of transect sampled during all biweekly periods at a given segment
transect.sampled           <- tapply(myAreaFish[myAreaFish[,"sample.flag"]=="yes","transect.biweek"],myAreaFish[myAreaFish[,"sample.flag"]=="yes","segment.new"],function(x){length(unique(x))})		# the number of transect sampled during all biweekly periods at a given segment
transect.not.sampled       <- transect.visited - transect.sampled
plots.sampled.sample.flag  <- tapply(myAreaFish[,"sample.flag"], myAreaFish[,"segment.new"], function(x){length(x[x=="yes"])})		# the number of transect sampled during all biweekly periods at a given segment
plots.sampled.area.sampled <- tapply(myAreaFish[,"area.sampled"],myAreaFish[,"segment.new"], function(x){length(x[!(is.na(x)) & x>0])})	# the number of transect sampled during all biweekly periods at a given segment
area.sampled               <- tapply(myAreaFish[,"area.sampled"],myAreaFish[,"segment.new"], function(x){sum(x[!(is.na(x))])})
Chinook                    <- tapply(myAreaFish[,"Chinook"],     myAreaFish[,"segment.new"], function(x){sum(x[!(is.na(x))])})

Table7.transect.visited <- cbind(TransectsVisited = transect.visited,
                                 PlotsSampledNo   = transect.not.sampled,
                                 PlotsSampledYes  = transect.sampled,
                                 NumberOfPlots    = plots.sampled.sample.flag,
                                 NumberOfPlots    = plots.sampled.area.sampled,
                                 AreaSampled      = area.sampled,
                                 Chinook          = Chinook)
                                 
Table7.transect.visited <- rbind(Table7.transect.visited,
                                 Total = apply(Table7.transect.visited,2,sum,na.rm=TRUE))
cat(paste("\n\nTable 7 at page24,",sep=""),file=FL.table.csv,append=TRUE)
write.table(Table7.transect.visited,file=FL.table.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("20. Table7_page24 has been prepared.\n",sep=""))
cat(paste("20. Table7_page24 has been prepared.\n",sep=""),file=FL.LOG,append=TRUE)


# 20b. Table7_page24
transect.visited           <- tapply(myAreaFish[,                                 "transect.biweek"],myAreaFish[,                                 "section.new"],function(x){length(unique(x))})		# the number of transect sampled during all biweekly periods at a given segment
transect.sampled           <- tapply(myAreaFish[myAreaFish[,"sample.flag"]=="yes","transect.biweek"],myAreaFish[myAreaFish[,"sample.flag"]=="yes","section.new"],function(x){length(unique(x))})		# the number of transect sampled during all biweekly periods at a given segment
transect.not.sampled       <- transect.visited - transect.sampled
plots.sampled.sample.flag  <- tapply(myAreaFish[,"sample.flag"], myAreaFish[,"section.new"], function(x){length(x[x=="yes"])})		# the number of transect sampled during all biweekly periods at a given segment
plots.sampled.area.sampled <- tapply(myAreaFish[,"area.sampled"],myAreaFish[,"section.new"], function(x){length(x[!(is.na(x)) & x>0])})	# the number of transect sampled during all biweekly periods at a given segment
area.sampled               <- tapply(myAreaFish[,"area.sampled"],myAreaFish[,"section.new"], function(x){sum(x[!(is.na(x))])})
Chinook                    <- tapply(myAreaFish[,"Chinook"],     myAreaFish[,"section.new"], function(x){sum(x[!(is.na(x))])})

Table8.transect.visited <- cbind(TransectsVisited = transect.visited,
                                 PlotsSampledNo   = transect.not.sampled,
                                 PlotsSampledYes  = transect.sampled,
                                 NumberOfPlots    = plots.sampled.sample.flag,
                                 NumberOfPlots    = plots.sampled.area.sampled,
                                 AreaSampled      = area.sampled,
                                 Chinook          = Chinook)
                                 
Table8.transect.visited <- rbind(Table8.transect.visited,
                                 Total = apply(Table8.transect.visited,2,sum,na.rm=TRUE))
cat(paste("\n\nTable 8 at page24,",sep=""),file=FL.table.csv,append=TRUE)
write.table(Table8.transect.visited,file=FL.table.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("20. Table7_page24 / Table8_page24 has been prepared.\n",sep=""))
cat(paste("20. Table7_page24 / Table8_page24 has been prepared.\n",sep=""),file=FL.LOG,append=TRUE)






# -------------------------------------------------------------------------------------------------
# 21: Table10_page26
# -------------------------------------------------------------------------------------------------
Table10_page26 <- cast(myAreaFish.melt, section.new ~ biweek.idx, subset=variable=="Chinook" & sample.flag == "yes",sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))
names(Table10_page26) <- sub("(all)","Total",names(Table10_page26))		# replace "(all)" in field name with "Total"
cat(paste("Table10_page26 has been prepared.\n",sep=""))
cat(paste("Table10_page26 has been prepared.\n",sep=""),file=FL.LOG,append=TRUE)

 
 cat(paste("\n\nTable 10 at Page26,",sep=""),file=FL.table.csv,append=TRUE)
 write.table(Table10_page26,file=FL.table.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("21. Table10_page26 has been prepared.\n",sep=""))
cat(paste("21. Table10_page26 has been prepared.\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 22: Table12_page28
# -------------------------------------------------------------------------------------------------
Table12_page28 <- data.frame(
                  rbind(c("1",     "Feb27-Mar12",     "All sections of Hanford Reach"),
                        c("2",     "Mar13-Mar26",     "Upper and Middle sections"),
                        c("3",     "Mar13-Mar26",     "Lower section"),
                        c("4",     "Mar27-Apr9",      "Upper and Middle sections"),
                        c("5",     "Mar27-Apr9",      "Lower section"),
                        c("6",     "Apr10-Apr23",     "Upper section"),
                        c("7",     "Apr10-Apr23",     "Middle section"),
                        c("8",     "Apr10-Apr23",     "Lower section"),
                        c("9",     "Apr24-May7",      "All sections of Hanford Reach"),
                        c("10",    "May8-May21",      "All sections of Hanford Reach"),
                        c("11",    "May22-Jun4",      "Lower section"),
                        c("12",    "May22-Jun4",      "Upper section"),
                        c("13",    "May22-Jun4",      "Middle section"),
                        c("14",    "Jun5-Jun18",      "All sections of Hanford Reach")))
                        
names(Table12_page28) <- c("Strata","Sampling Periods","Hanford Reach Sections Included")                        
 cat(paste("\n\nTable 12 at Page28,",sep=""),file=FL.table.csv,append=TRUE)
 write.table(Table12_page28,file=FL.table.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("22. Table10_page26 has been prepared.\n",sep=""))
cat(paste("22. Table10_page26 has been prepared.\n",sep=""),file=FL.LOG,append=TRUE)

 

# -------------------------------------------------------------------------------------------------
# 23. save the arrays
# -------------------------------------------------------------------------------------------------
save(list = ls(all=TRUE),file=FL.data.obj)
cat(paste("save all objects created into [",FL.data.obj,"].\n",sep=""))
cat(paste("save all objects created into [",FL.data.obj,"].\n",sep=""),file=FL.LOG,append=TRUE)


 
# -------------------------------------------------------------------------------------------------
# 24. time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n05_2013_TablePrepare.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n05_2013_TablePrepare.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [05_2013_TablePrepare.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [05_2013_TablePrepare.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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


