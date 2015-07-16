#
# 02_DewateredArea_Flow2013.R
#
# Sep 3, 2013
#
# Sep 18, 2012: Modified on "2011_HanfordReach_DewateredArea.R".
#
# Revised on Feb 14, 2012 to get the dewatered area of Feb 27 and Feb 28, 2011.
#
# Gone through on Dec 5, 2011
#
# Modified on Nov 29, 2011 after received the revised biweekly periods
#
# Coded on Oct 26, 2011
#
# The dewatered area per transect in each of the two week periods have been saved in the array of "myArea.transect.long" and "myArea.transect.wide"
# The objects shold be accessed through load()
#
# received dewatered area from Chris on Oct 26, 2011
# The data is delimited by space and has the following fields:
#
# 1. Date
# 2. Time
# 3. Quandrant number (1-360, 0 is any part of the model outside the study area)  
# 4. In-river area, ha
# 5. Area dewatered since previous hour, ha
# 6. Area entrapped since previous hour, ha
# -------------------------------------------------------------------------------------------------
#
# section	segment	lower boundary	upper boundary	site	tot sites
# upper		1	620		635		15
# upper		2	605		620		15
# ---------------------------------------------------------	30
# middle 	3 	595		605		10	
# middle 	4 	588		595		7	
# middle 	5 	581		588		7	
# middle 	6 	575		581		6	
# ---------------------------------------------------------	30
# lower		7 	558		575		17	
# lower 	8 	545		558		13	
# ---------------------------------------------------------	30
#
# This script is used to turn the data into the following format:
# a 360 by 10 matrix
# rows:    360 transect
# columns: 9   two-week period and one "section" field
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
# 1. change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2013_SalmonStrandingFieldSurvey/FY2013_SalmonStranding_Analysis/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2013_SalmonStrandingFieldSurvey/FY2013_SalmonStranding_Analysis/0_scripts"
}
setwd(Path.Current)


# -------------------------------------------------------------------------------------------------
# 2a. biweekly periods
# -------------------------------------------------------------------------------------------------
biweek_designation <- "Nino"
if (biweek_designation == "Paul")
{
	biweek.idx.label     <- c( 1,           2,            3,             4,            5,            6,            7          )
	biweek.idx.names     <- c("Mar4-Mar17","Mar18-Mar31","Apr11-Apr114","Apr15-Apr28","Apr29-May12","May13-May26","May27-Jun9")
	biweek.idx.fullNames <- c("Mar4-Mar17","Mar18-Mar31","Apr11-Apr114","Apr15-Apr28","Apr29-May12","May13-May26","May27-Jun9")
	names(biweek.idx.fullNames) <- biweek.idx.names
	
	# get the first and last day
	firstday.1st_biweek   <- chron(dates="03/04/2013",times="0:0:0",   format=c('m/d/y','h:m:s'))	# split the data into two week intervals from Tuesday of March 1, 2011
	first.sampleDay.2keep <- chron(dates="03/04/2013",times="0:0:0",   format=c('m/d/y','h:m:s'))	# split the data into two week intervals from Tuesday of March 1, 2011
	last.sampleDay        <- chron(dates="06/09/2013",times="23:59:59",format=c('m/d/y','h:m:s'))	# split the data into two week intervals from Tuesday of March 1, 2011
}else if (biweek_designation == "Nino")
{
	biweek.idx.label     <- c( 1,             2,            3,            4,            5,            6,            7,           8           )
	biweek.idx.names     <- c("Feb27-Mar12","Mar13-Mar26","Mar27-Apr09","Apr10-Apr23","Apr24-May07","May08-May21","May22-Jun4","Jun05-Jun18")
	biweek.idx.fullNames <- c("Feb27-Mar12","Mar13-Mar26","Mar27-Apr09","Apr10-Apr23","Apr24-May07","May08-May21","May22-Jun4","Jun05-Jun18")
	names(biweek.idx.fullNames) <- biweek.idx.names
	
	# get the first and last day
	firstday.1st_biweek   <- chron(dates="02/27/2013",times="0:0:0",   format=c('m/d/y','h:m:s'))	# split the data into two week intervals from Tuesday of March 1, 2011
	first.sampleDay.2keep <- chron(dates="03/02/2013",times="0:0:0",   format=c('m/d/y','h:m:s'))	# split the data into two week intervals from Tuesday of March 1, 2011
	last.sampleDay        <- chron(dates="06/09/2013",times="23:59:59",format=c('m/d/y','h:m:s'))	# split the data into two week intervals from Tuesday of March 1, 2011
}

# -------------------------------------------------------------------------------------------------
# 2b. code section label
# -------------------------------------------------------------------------------------------------
section.label     <- c( 1,      2,       3)
section.names     <- c("Upper","Middle","Lower")


# -------------------------------------------------------------------------------------------------
# 3a. setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.in  <- "../../Data_Received/dewateredArea_2013_flow"	# INPUT  directory
Path.out <- "../02_DewateredArea_Flow2013"			# OUTPUT directory
Path.log <- "../0_log"						# OUTPUT log  directory
if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.in)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}

# 3b. define input data files
  FL.area.in <- paste(Path.in, "dewater_10m_2013_02_06.txt",sep="/")				# INPUT field stranding data file
  if (!file.exists(FL.area.in)){print(paste("field stranding data file \"",FL.area.in," \"does not exist. Check why!"))}
# Format of "dewater_10m_02_02.txt"
# date       time       transect        area.inRiver.ha area.dewatered.ha	area.entrapped.ha
# 02-01-2013 01:00:00   0 		2.869125e+02 	7.177340e-02 		0.000000e+00
# 02-01-2013 01:00:00   1 		2.462679e+01 	0.000000e+00 		0.000000e+00
# 02-01-2013 01:00:00   2 		1.109339e+01 	0.000000e+00 		0.000000e+00

# 3c. create a LOG file 
FL.LOG  <- paste(Path.log,"02_DewateredArea_Flow2013.log",sep="/")		# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}
cat(paste("3. Paths and input files are defined!\n",sep=""))
cat(paste("3. Paths and input files are defined!\n",sep=""),file=FL.LOG,append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# 4. load libraries
# -------------------------------------------------------------------------------------------------
library("lattice")
library("reshape")
library("chron")
library("cwhmisc")	# used for remove duplicate rows in a data frame
library(RODBC)
library(graphics)
library("reshape")
# library(gplots)
# library("locfit")
# library("boot")
cat(paste("4. loaded necessary libraries","\n",sep=""))
cat(paste("4. loaded necessary libraries","\n",sep=""),file=FL.LOG,append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# 5. create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
# output related to direct input 
FL.area_raw.out        <- paste(Path.out,"02_DewateredArea_Flow2013_raw.csv",       sep="/")		
FL.area_retain.out     <- paste(Path.out,"02_DewateredArea_Flow2013_retained.csv",  sep="/")		
FL.area_bySite.out     <- paste(Path.out,"02_DewateredArea_Flow2013_bySite.csv",    sep="/")	
FL.area_byTransect.out <- paste(Path.out,"02_DewateredArea_Flow2013_byTransect.csv",sep="/")
FL.area_sum.out        <- paste(Path.out,"02_DewateredArea_Flow2013_sum.csv",       sep="/")		
FL.area.obj            <- paste(Path.out,"02_DewateredArea_Flow2013.Rdata",         sep="/")		
if (file.exists(FL.area_raw.out))       {print(paste(FL.area_raw.out,       "exist.Delete it!"));file.remove(FL.area_raw.out)}
if (file.exists(FL.area_retain.out))    {print(paste(FL.area_retain.out,    "exist.Delete it!"));file.remove(FL.area_retain.out)}
if (file.exists(FL.area_bySite.out))    {print(paste(FL.area_bySite.out,    "exist.Delete it!"));file.remove(FL.area_bySite.out)}
if (file.exists(FL.area_byTransect.out)){print(paste(FL.area_byTransect.out,"exist.Delete it!"));file.remove(FL.area_byTransect.out)}
if (file.exists(FL.area_sum.out))       {print(paste(FL.area_sum.out,       "exist.Delete it!"));file.remove(FL.area_sum.out)}
if (file.exists(FL.area.obj))           {print(paste(FL.area.obj,           "exist.Delete it!"));file.remove(FL.area.obj)}
cat(paste("5. defined output dewatered area file names\n",sep=""))
cat(paste("5. defined output dewatered area file names\n",sep=""),file=FL.LOG,append=TRUE)



# ------------------------------------------------------------------------------------------------- 
# 6. read in field stranding data
# ------------------------------------------------------------------------------------------------- 
      myArea.input  <- read.table(file=FL.area.in,header=FALSE,sep="",stringsAsFactors=FALSE)
names(myArea.input) <- c("date","time","transect","area.inRiver.ha","area.dewatered.ha","area.entrapped.ha")
cat(paste("dewatered area has been read in.\n",sep=""))
cat(paste("dewatered area has been read in.\n",sep=""),file=FL.LOG,append=TRUE)

# convert ha into square meter
myArea.input[,"area.dewatered.m2"] <- myArea.input[,"area.dewatered.ha"] * 10000
cat(paste("6a. converted ha into square meter.\n",sep=""))
cat(paste("6a. converted ha into square meter.\n",sep=""),file=FL.LOG,append=TRUE)

# output a file sorted according to the transect
cat("wide format (sort on transect),",file=FL.area_raw.out,append=TRUE)
write.table(myArea.input[order(myArea.input[,"transect"],myArea.input[,"date"],myArea.input[,"time"]),],file=FL.area_raw.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("6b. outputted the data sorted according to transect into [FL.area_raw.out].\n",sep=""))
cat(paste("6b. outputted the data sorted according to transect into [FL.area_raw.out].\n",sep=""),file=FL.LOG,append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# 7. prepare biweekly periods lables and retain only data in the survey periods and study area
# -------------------------------------------------------------------------------------------------
# 7a. get year/month/day and prepare a chron.date field
tmp   <- unlist(strsplit(myArea.input[,"date"],"-"))
month <- as.numeric(tmp[seq(from=1,to=length(tmp),by=3)])	# [day]
day   <- as.numeric(tmp[seq(from=2,to=length(tmp),by=3)])	# [month]
year  <- as.numeric(tmp[seq(from=3,to=length(tmp),by=3)]) 	# [year]

date.chron <- chron(dates = paste(month,day,year,sep="/"),
                    times = rep("0:0:0",length(day)))
myArea.input <- cbind(myArea.input,month = month, day = day,year = year,date.chron = date.chron)
cat(paste("7a. added a \"date.chron\" and year, month and day fields\n",sep=""))
cat(paste("7a. added a \"date.chron\" and year, month and day fields\n",sep=""),file=FL.LOG,append=TRUE)

# 7b. retain only data within the survey period
myArea.retain <- myArea.input[((myArea.input[,"date.chron"] >= first.sampleDay.2keep) & (myArea.input[,"date.chron"] <= last.sampleDay)),] 
cat(paste("7b. only kept the data within the survey period.\n",sep=""))
cat(paste("7b. only kept the data within the survey period.\n",sep=""),file=FL.LOG,append=TRUE)

# 7c. add a bi-week index.  Note: 
myArea.retain <- data.frame(myArea.retain,						# use "date.chron.fake" only for testing the script		
	      		    biweek.idx = as.numeric(ceiling((myArea.retain[,"date.chron"] - firstday.1st_biweek + (1/60)/24)/14)))			# plus (5/60)/48 which is half of a 5 minute interval 

# 7d. turn the biweekly period into factors
myArea.retain[,"biweek.idx"] <- factor(myArea.retain[,"biweek.idx"],levels=biweek.idx.label,labels=biweek.idx.names,ordered=TRUE)		# convert week index from number to string like Nov9, etc
cat(paste("7d. added a \"biweek.idx\" field\n",sep=""))
cat(paste("7d. added a \"biweek.idx\" field\n",sep=""),file=FL.LOG,append=TRUE)

# 7e. exclude data with transect == 0 which is out of the study area
myArea.retain <- myArea.retain[myArea.retain[,"transect"] > 0,]
cat(paste("7e.exclude transect == 0 data.\n",sep=""))
cat(paste("7e. exclude transect == 0 data.\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# 8. adding "site", "segment" and "section"
# -------------------------------------------------------------------------------------------------
# 8a. adding "site"
myArea.retain[,"site"] <- (myArea.retain[,"transect"]-1)%/%4 + 1
cat(paste("8a. added a [site] field\n",sep=""))
cat(paste("8a. added a [site] field\n",sep=""),file=FL.LOG,append=TRUE)

# 8b. adding "segment" 
myArea.retain[,"segment"] <- rep(0,dim(myArea.retain)[1])
myArea.retain[(myArea.retain[,"transect"] >=   1 & myArea.retain[,"transect"] <=  60),"segment"] <- 1
myArea.retain[(myArea.retain[,"transect"] >=  61 & myArea.retain[,"transect"] <= 120),"segment"] <- 2

myArea.retain[(myArea.retain[,"transect"] >= 121 & myArea.retain[,"transect"] <= 160),"segment"] <- 3
myArea.retain[(myArea.retain[,"transect"] >= 161 & myArea.retain[,"transect"] <= 188),"segment"] <- 4
myArea.retain[(myArea.retain[,"transect"] >= 189 & myArea.retain[,"transect"] <= 216),"segment"] <- 5
myArea.retain[(myArea.retain[,"transect"] >= 217 & myArea.retain[,"transect"] <= 240),"segment"] <- 6

myArea.retain[(myArea.retain[,"transect"] >= 241 & myArea.retain[,"transect"] <= 308),"segment"] <- 7
myArea.retain[(myArea.retain[,"transect"] >= 309 & myArea.retain[,"transect"] <= 360),"segment"] <- 8
cat(paste("8b. added a [segment] field\n",sep=""))
cat(paste("8b. added a [segment] field\n",sep=""),file=FL.LOG,append=TRUE)

# 8c. adding "section" 
myArea.retain[,"section"] <- rep(0,dim(myArea.retain)[1])
myArea.retain[(myArea.retain[,"transect"] >=   1 & myArea.retain[,"transect"] <= 120),"section"] <- 1
myArea.retain[(myArea.retain[,"transect"] >= 121 & myArea.retain[,"transect"] <= 240),"section"] <- 2
myArea.retain[(myArea.retain[,"transect"] >= 241 & myArea.retain[,"transect"] <= 360),"section"] <- 3
cat(paste("8c. added a [section] field\n",sep=""))
cat(paste("8c. added a [section] field\n",sep=""),file=FL.LOG,append=TRUE)

# 8d. turning "section" into factor
myArea.retain[,"section"] <- factor(myArea.retain[,"section"],levels=section.label,labels=section.names,ordered=TRUE)		# convert week index from number to string like Nov9, etc
cat(paste("8d. turn [section] into a factor.\n",sep=""))
cat(paste("8d. turn [section] into a factor.\n",sep=""),file=FL.LOG,append=TRUE)

# 8e. output the summarized dewatered areas
cat(paste("retained dewatered area data,",sep=""),file=FL.area_retain.out,append=TRUE)
write.table(myArea.retain,file=FL.area_retain.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("8e. outputted the retained data into [",FL.area_retain.out,"].\n",sep=""))
cat(paste("8e. outputted the retained data into [",FL.area_retain.out,"].\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 9. use "melt" to turn a subset of [myArea.retain], i.e., [myDewateredArea] into [myDewateredArea.melt]: prepare a data frame easy for summarizing
# -------------------------------------------------------------------------------------------------
id.vars              <- c("section","segment","site","transect","biweek.idx")	# where "area.dewatered.m2" is used as "variable" and the other are used as identifiers
measure.vars         <- c("area.dewatered.m2")					# where "area.dewatered.m2" is used as "variable" and the other are used as identifiers
myDewateredArea      <- myArea.retain[,c(id.vars,measure.vars)]		
myDewateredArea.melt <- melt(myDewateredArea,id.vars=id.vars,measure.vars=measure.vars,variable_name = "variable",na.rm=FALSE)
cat(paste("9. melt [myDewateredArea] into [myDewateredArea.melt].\n",sep=""))
cat(paste("9. melt [myDewateredArea] into [myDewateredArea.melt].\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# 10. summarizing dewatered area in terms of "biweek" and "site"|"segment"|"section"
# -------------------------------------------------------------------------------------------------
myArea.section.biweek  <- cast(myDewateredArea.melt, section ~ biweek.idx, sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))
myArea.segment.biweek  <- cast(myDewateredArea.melt, segment ~ biweek.idx, sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))
myArea.site.biweek     <- cast(myDewateredArea.melt,    site ~ biweek.idx, sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))
myArea.transect.biweek <- cast(myDewateredArea.melt,transect ~ biweek.idx, sum,na.rm=TRUE, margins=c("grand_row", "grand_col"))
cat(paste("10. summary of \"area.dewatered.m2\" are completed.\n",sep=""))
cat(paste("10. summary of \"area.dewatered.m2\" are completed.\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 11. prepare a table in the format of table 9 of FY2012 report
# -------------------------------------------------------------------------------------------------
myData.tmp   <- as.data.frame(myArea.section.biweek[-dim(myArea.section.biweek)[1],-dim(myArea.section.biweek)[2]])	# remove the total row and column
id.vars      <- "section"
measure.vars <- names(myData.tmp)[-1]
myTable <- melt(myData.tmp,id.vars=id.vars,measure.vars=measure.vars,variable_name = "biweek.idx",na.rm=FALSE)
names(myTable) <- c("River Section","Two-Week Index","Area(m2)")
rm(myData.tmp)
cat(paste("11. prepare the section-biweek dewatered area into a table.\n",sep=""))
cat(paste("11. prepare the section-biweek dewatered area into a table.\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# 12. output the summarized dewatered areas
# -------------------------------------------------------------------------------------------------
cat(paste("\n\nArea(m2) Section-biweek,",sep=""),file=FL.area_sum.out,append=TRUE)
write.table(myTable,file=FL.area_sum.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\nArea(m2) Section-biweek,",sep=""),file=FL.area_sum.out,append=TRUE)
write.table(myArea.section.biweek,file=FL.area_sum.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\nArea(m2) Segment-biweek,",sep=""),file=FL.area_sum.out,append=TRUE)
write.table(myArea.segment.biweek,file=FL.area_sum.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\nArea(m2) Site-biweek,",sep=""),file=FL.area_sum.out,append=TRUE)
write.table(myArea.site.biweek,file=FL.area_sum.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)

cat(paste("\n\nArea(m2) Transect-biweek,",sep=""),file=FL.area_sum.out,append=TRUE)
write.table(myArea.transect.biweek,file=FL.area_sum.out,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("12a. outputted the summarized dewatered area into [",FL.area_sum.out,"].\n",sep=""))
cat(paste("12a. outputted the summarized dewatered area into [",FL.area_sum.out,"].\n",sep=""),file=FL.LOG,append=TRUE)

# also output the section-biweek dewatered area to a Table format file
write.table(myTable,file=FL.area_bySite.out,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("12b. output the section-biweek dewatered area into a table in [",FL.area_bySite.out,"].\n",sep=""))
cat(paste("12b. output the section-biweek dewatered area into a table in [",FL.area_bySite.out,"].\n",sep=""),file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# 13. prepare a data frame similar to "myArea.transect.long" which in previous years was prepared by "2012_HanfordReach_DewateredArea.R" and used by "2012_HanfordReach_StrandingSurvey_BootStrap_option5_modified.R"
# -------------------------------------------------------------------------------------------------
# The format of "myArea.transect.long" used in previous years
# TRANSECT long format	biweek.idx	transect	area.dewatered.m2	biweek.name	segment	section	site
# 9			Mar9-Mar22	1		50596.29785		Mar9-Mar22	1	1	1
# 10			Mar23-Apr5	1		71897.96994		Mar23-Apr5	1	1	1
# 11			Apr6-Apr19	1		69531.00894		Apr6-Apr19	1	1	1
myData.tmp           <- as.data.frame(myArea.transect.biweek[-dim(myArea.transect.biweek)[1],-dim(myArea.transect.biweek)[2]])	# remove the total row and column
id.vars              <- "transect"
measure.vars         <- names(myData.tmp)[-1]
myArea.transect.long <- melt(myData.tmp,id.vars=id.vars,measure.vars=measure.vars,variable_name = "biweek.idx",na.rm=FALSE)
names(myArea.transect.long) <- sub("value","area.dewatered.m2",names(myArea.transect.long))

# turn the factor to numeric for the "transect" field
if (is.factor(myArea.transect.long[,"transect"])){myArea.transect.long[,"transect"] <- as.numeric(as.character(myArea.transect.long[,"transect"]))}

# add "segment", "section", "site" and "biweek.name" fields
myArea.transect.long[,"site"] <- (myArea.transect.long[,"transect"]-1)%/%4 + 1
cat(paste("13a. added a [site] field to [myArea.transect.long].\n",sep=""))
cat(paste("13a. added a [site] field to [myArea.transect.long].\n",sep=""),file=FL.LOG,append=TRUE)

# adding "segment" 
myArea.transect.long[,"segment"] <- rep(0,dim(myArea.transect.long)[1])
myArea.transect.long[(myArea.transect.long[,"transect"] >=   1 & myArea.transect.long[,"transect"] <=  60),"segment"] <- 1
myArea.transect.long[(myArea.transect.long[,"transect"] >=  61 & myArea.transect.long[,"transect"] <= 120),"segment"] <- 2

myArea.transect.long[(myArea.transect.long[,"transect"] >= 121 & myArea.transect.long[,"transect"] <= 160),"segment"] <- 3
myArea.transect.long[(myArea.transect.long[,"transect"] >= 161 & myArea.transect.long[,"transect"] <= 188),"segment"] <- 4
myArea.transect.long[(myArea.transect.long[,"transect"] >= 189 & myArea.transect.long[,"transect"] <= 216),"segment"] <- 5
myArea.transect.long[(myArea.transect.long[,"transect"] >= 217 & myArea.transect.long[,"transect"] <= 240),"segment"] <- 6

myArea.transect.long[(myArea.transect.long[,"transect"] >= 241 & myArea.transect.long[,"transect"] <= 308),"segment"] <- 7
myArea.transect.long[(myArea.transect.long[,"transect"] >= 309 & myArea.transect.long[,"transect"] <= 360),"segment"] <- 8
cat(paste("13b. added a [segment] field to [myArea.transect.long].\n",sep=""))
cat(paste("13b. added a [segment] field to [myArea.transect.long].\n",sep=""),file=FL.LOG,append=TRUE)

# adding "section" 
myArea.transect.long[,"section"] <- rep(0,dim(myArea.transect.long)[1])
myArea.transect.long[(myArea.transect.long[,"transect"] >=   1 & myArea.transect.long[,"transect"] <= 120),"section"] <- 1
myArea.transect.long[(myArea.transect.long[,"transect"] >= 121 & myArea.transect.long[,"transect"] <= 240),"section"] <- 2
myArea.transect.long[(myArea.transect.long[,"transect"] >= 241 & myArea.transect.long[,"transect"] <= 360),"section"] <- 3
cat(paste("13c. added a [section] field to [myArea.transect.long].\n",sep=""))
cat(paste("13c. added a [section] field to [myArea.transect.long].\n",sep=""),file=FL.LOG,append=TRUE)

# adding "biweek.name"
myArea.transect.long[,"biweek.name"] <- myArea.transect.long[,"biweek.idx"]
cat(paste("13d. added a [biweek.name] field to [myArea.transect.long].\n",sep=""))
cat(paste("13d. added a [biweek.name] field to [myArea.transect.long].\n",sep=""),file=FL.LOG,append=TRUE)

# sort and organize in the same way as in previous year
O <- order(myArea.transect.long[,"section"],myArea.transect.long[,"segment"],myArea.transect.long[,"site"],myArea.transect.long["transect"],myArea.transect.long[,"biweek.idx"])
myArea.transect.long <- myArea.transect.long[O,c("biweek.idx","transect","area.dewatered.m2","biweek.name","segment","section","site")]


# also output the section-biweek dewatered area to a Table format file
write.table(myArea.transect.long,file=FL.area_byTransect.out,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE,quote=FALSE)
cat(paste("13e. output the section-biweek dewatered area into a table in [",FL.area_byTransect.out,"].\n",sep=""))
cat(paste("13e. output the section-biweek dewatered area into a table in [",FL.area_byTransect.out,"].\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 14. save everything into a R object file
# -------------------------------------------------------------------------------------------------
save(list = ls(all=TRUE),file=FL.area.obj)
cat(paste("14. save all objects created into [",FL.area.obj,"].\n",sep=""))
cat(paste("14. save all objects created into [",FL.area.obj,"].\n",sep=""),file=FL.LOG,append=TRUE)
 



# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n02_DewateredArea_Flow2013.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n02_DewateredArea_Flow2013.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [02_DewateredArea_Flow2013.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [02_DewateredArea_Flow2013.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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






















