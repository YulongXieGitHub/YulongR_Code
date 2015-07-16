#
# 10_report_ggplot_Hartigan_report.R 
#
# April 4, 2015: output the plots in jpg format to retain resolution.
#
# March 11, 2015: Meet Chris.
# He asked me to modify the label font of the figure.
#
# Feb 24-25, 2015
# Revise plot based on the request of Chris
#
#
# Nov 5, 2014: only output the essential plots
# October 15, 2014: use ggplt package for plotting.
#

# eliminate all stuff
rm(list = ls(all = TRUE))


# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
# http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/
# http://docs.ggplot2.org/current/
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# two y axis in ggplot2
# http://rpubs.com/kohske/dual_axis_in_ggplot2
# http://stackoverflow.com/questions/3099219/how-to-use-ggplot2-make-plot-with-2-y-axes-one-y-axis-on-the-left-and-another
# http://stackoverflow.com/questions/6625691/is-it-possible-to-switch-the-side-of-y-axis-breaks-and-labels-on-a-faceted-plot
# http://stackoverflow.com/questions/21981416/dual-y-axis-in-ggplot2-for-multiple-panel-figure
# https://github.com/hadley/ggplot2/wiki/Align-two-plots-on-a-page
# http://mandymejia.wordpress.com/2013/11/13/10-reasons-to-switch-to-ggplot-7/

# facet_wrap does not have a labbeller function but can specify the number of columns and rows
# facet_grid does have the labbeller function but no way to control the number of columns and rows.

# http://stackoverflow.com/questions/10151123/how-to-specify-columns-in-facet-grid-or-how-to-change-labels-in-facet-wrap

# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

library("ggplot2")
library(missMDA)
library(cluster)
library(HSAUR)
library(fpc)
library(chron)
library(grid)
library(gridExtra)
library(ggthemes)
library(latticeExtra)
library(reshape2)

# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

col.array  <- c("red","blue","green","magenta","cyan","yellow","black","purple","pink","brown","cadetblue","chartreuse","chocolate","cornflowerblue","darkcyan","darkgoldenrod2","deepskyblue","darkolivegreen1","firebrick1")
col.array2 <- c("red4","red1","blue4","blue1","green4","green1","deeppink4","deeppink1","deepskyblue4","deepskyblue1","turquoise4","turquoise1","firebrick4","firebrick1")

# today's month, day and year in the format of "Thu Jun 16 08:48:36 2011", 5 fields separated by space
today.month  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
today.day    <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
today.year   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]
today.hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
today.minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
today.second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]
cat(paste("1: specification.\n",sep=""))
# ------------------------------------------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Script  <- "/phome/resstd/mh_rule/vm/2014analysis/0_script"
	Path.Project <- "/phome/resstd/mh_rule/vm/2014analysis"
}else{
	Path.Script  <- "D:/YuLong_Projects/Yulong_Geology/FY2014_SFA/0_scripts"
	Path.Project <- "D:/YuLong_Projects/Yulong_Geology/FY2014_SFA"

	Path.Script  <- "c:/YuLong_Projects/FY2015/FY2014_SFA/0_scripts"
	Path.Project <- "c:/YuLong_Projects/FY2015/FY2014_SFA"
}



source("multiplot.R")


# **********************************************************************************
# log file
# **********************************************************************************
Path.LOG <- paste(Path.Project,"0_log",sep="/")
if (!file.exists(Path.LOG)){print(paste("NOT existing:",Path.LOG));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}
FL.LOG <- paste(Path.LOG,"10_report_ggplot_Hartigan_report.log",sep="/")
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}	
cat(paste("1: defined path and file names.\n",sep=""))
cat(paste("1: defined path and file names.\n",sep=""),file=FL.LOG,append=TRUE)

# specify data and results folder
Path.Data <- paste(Path.Project,"0_data",sep="/")
if (!file.exists(Path.Data)){print(paste("NOT existing:",Path.Data));cat(paste(Path.Data," does not exist for [",this.subfolder,"].  Check why!\n",sep=""));die}
cat(paste("2: defined path and file names.\n",sep=""))
cat(paste("2: defined path and file names.\n",sep=""),file=FL.LOG,append=TRUE)




# shallow wells in "TimeSeries_UHSpC_2011_ShallowWells.jpg"
Well.Shallow <- c("2-7","2-8","2-9","2-11","2-12","2-13","2-14","2-15","2-16","2-17","2-18","2-19","2-20","2-21","2-22","2-23","2-24","2-26","2-29","2-34","2-37","3-23","3-24","3-25","3-27","3-28","3-29","3-30","3-35")

# -----------------------------------------------------------------------------------------------------------------------------------
cat(paste("3: specify path and file of input files.\n",sep=""))
cat(paste("3: specify path and file of input files.\n",sep=""),file=FL.LOG,append=TRUE)


#
# read the extractale Uranium
#
FL.U.extract <- paste(Path.Data,"U_extractable.csv",sep="/")
my.U.extract <- read.csv(FL.U.extract,header=TRUE)
names(my.U.extract) <- c("TmpID","Well_ID","Easting","Notthing","extractable.U","Year")
my.U.extract[,"Well_ID"] <- sub("399-","",my.U.extract[,"Well_ID"])
my.U.extract[,"Well_ID"] <- sub("2-0","2-",my.U.extract[,"Well_ID"])


# loop for the three sets of data
for (this.year.alt in c(2011,2010,20100)[1])
{

	if (this.year.alt == 2010)
	{
		this.chron.cut.begin <- chron(dates  = "06/07/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		this.chron.cut.end   <- chron(dates  = "07/30/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		idx_for_abline <- c()
		files <- c("2-10_WLData_2010","UData_2010_AllWell.txt","SpCData_2010_AllWell.txt")
		this.year <- this.year.alt
		this.year.string <- this.year
		
		my.U.extract.thisYear <- my.U.extract[my.U.extract[,"Year"] == "2010",]
		
		date.max.WL <- as.Date("2011-06-26")
	}
	else if (this.year.alt == 2011)
	{
		this.chron.cut.begin <- chron(dates  = "05/06/11",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		this.chron.cut.end   <- chron(dates  = "08/06/11",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		idx_for_abline <- c(40,45,51)
		files <- c("2-10_WLData_2011","UData_2011_AllWell.txt","SpCData_2011_AllWell.txt")
		this.year <- this.year.alt
		this.year.string <- this.year
		
		my.U.extract.thisYear <- my.U.extract[my.U.extract[,"Year"] == "2011",]
		
		date.max.WL <- as.Date("2011-06-16")
	}
	else if (this.year.alt == 20100)
	{
		this.chron.cut.begin <- chron(dates  = "05/11/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		this.chron.cut.end   <- chron(dates  = "07/30/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		idx_for_abline <- c()
		files <- c("2-10_WLData_2010","UData_2010_AllWell.txt","SpCData_2010_AllWell.txt")
		this.year <- 2010
		this.year.string <- "2010B"
		
		my.U.extract.thisYear <- my.U.extract[my.U.extract[,"Year"] == "2010",]
		
		date.max.WL <- as.Date("2011-06-26")
	}
	cat(paste("3: year ",this.year.alt,": specify path and file of input files.\n",sep=""))
	cat(paste("3: year ",this.year.alt,": specify path and file of input files.\n",sep=""),file=FL.LOG,append=TRUE)
	
	Path.results <- paste(Path.Project,"10_report_ggplot_Hartigan_report",this.year.string,sep="/")
	if (!file.exists(Path.results)){print(paste("NOT existing:",Path.results));dir.create(Path.results,showWarnings=TRUE,recursive=TRUE)}


	FL.WL.OUT       <- paste(Path.results,paste("verify_WL_data_",this.year.string,"_complete.csv",sep=""),          sep="/")
	FL.U.OUT        <- paste(Path.results,paste("verify_U_data_",this.year.string,"_complete.csv",sep=""),           sep="/")
	FL.Sp.OUT       <- paste(Path.results,paste("verify_Sp_data_",this.year.string,"_complete.csv",sep=""),          sep="/")
	FL.Merged.U.WL  <- paste(Path.results,paste("verify_merged_",this.year.string,"_complete_U_WL.csv",sep=""),      sep="/")
	FL.Merged.Sp.WL <- paste(Path.results,paste("verify_merged_",this.year.string,"_complete_Sp_WL.csv",sep=""),     sep="/")
	FL.PDF          <- paste(Path.results,paste("verify_merged_",this.year.string,"_complete.pdf",sep=""),           sep="/")
	FL.OBJ          <- paste(Path.results,paste("verify_merged_",this.year.string,"_complete.Rdata",sep=""),         sep="/")
	FL.Imput.csv    <- paste(Path.results,paste("verify_merged_",this.year.string,"_complete_Imput.csv",sep=""),     sep="/")
	FL.Imput.obj    <- paste(Path.results,paste("verify_merged_",this.year.string,"_complete_Imput.Rdata",sep=""),   sep="/")
	FL.LOESS        <- paste(Path.results,paste("verify_merged_",this.year.string,"_complete_Loess.Rdata",sep=""),   sep="/")
	if (file.exists(FL.WL.OUT))      {print(paste(FL.WL.OUT,      "exist.Delete it!"));file.remove(FL.WL.OUT)}
	if (file.exists(FL.U.OUT))       {print(paste(FL.U.OUT,       "exist.Delete it!"));file.remove(FL.U.OUT)}
	if (file.exists(FL.Sp.OUT))      {print(paste(FL.Sp.OUT,      "exist.Delete it!"));file.remove(FL.Sp.OUT)}
	if (file.exists(FL.Merged.U.WL)) {print(paste(FL.Merged.U.WL, "exist.Delete it!"));file.remove(FL.Merged.U.WL)}
	if (file.exists(FL.Merged.Sp.WL)){print(paste(FL.Merged.Sp.WL,"exist.Delete it!"));file.remove(FL.Merged.Sp.WL)}
	if (file.exists(FL.PDF))         {print(paste(FL.PDF,         "exist.Delete it!"));file.remove(FL.PDF)}
	if (file.exists(FL.OBJ))         {print(paste(FL.OBJ,         "exist.Delete it!"));file.remove(FL.OBJ)}
	if (file.exists(FL.Imput.csv))   {print(paste(FL.Imput.csv,   "exist.Delete it!"));file.remove(FL.Imput.csv)}
	if (file.exists(FL.Imput.obj))   {print(paste(FL.Imput.obj,   "exist.Delete it!"));file.remove(FL.Imput.obj)}
	if (file.exists(FL.LOESS))       {print(paste(FL.LOESS,       "exist.Delete it!"));file.remove(FL.LOESS)}

	cat(paste("4: specify path and file names.\n",sep=""))
	cat(paste("4: specify path and file names.\n",sep=""),file=FL.LOG,append=TRUE)

	# OPEN PDF FILE
	pdf(file = FL.PDF,paper="special", width=17, height=11,bg = "transparent")


	# loop the files for reading
	for (this.file in files)
	{
		# WL data
		if ((this.file == "2-10_WLData_2011") | (this.file == "2-10_WLData_2010"))
		{
			field.names <- c("Date-Time","WL")
			# read data files
			FL.data    <- paste(Path.Data,paste(this.file,".csv",sep=""),sep="/")
			myData.raw <- read.table(file=FL.data,header=TRUE,sep=",",stringsAsFactors=FALSE)
			names(myData.raw) <- field.names
		# U data		
		}else if ((this.file == "UData_2011_AllWell.txt") | (this.file == "UData_2010_AllWell.txt"))
		{
			FL.data    <- paste(Path.Data,paste(this.file,sep=""),sep="/")
			myData.raw <- read.table(file=FL.data,header=TRUE,sep="",stringsAsFactors=FALSE)
			names(myData.raw) <- sub("\\.","-",sub("X","",names(myData.raw)))	# change the wellname from something like "X2.07" to "2-07"
			names(myData.raw) <- sub("-0","-",names(myData.raw))			# change the well name from something like "2-07" to "2-7"
		# Sp data
		}else if ((this.file == "SpCData_2011_AllWell.txt") | (this.file == "SpCData_2010_AllWell.txt"))
		{
			FL.data    <- paste(Path.Data,paste(this.file,sep=""),sep="/")
			myData.raw <- read.table(file=FL.data,header=TRUE,sep="",stringsAsFactors=FALSE)
			names(myData.raw) <- sub("\\.","-",sub("X","",names(myData.raw)))	# change the wellname from something like "X2.07" to "2-07"
			names(myData.raw) <- sub("-0","-",names(myData.raw))			# change the well name from something like "2-07" to "2-7"
		}
		cat(paste("4a. ",this.file," data has been read from [",FL.data,"] for year ",this.year,".\n",sep=""))
		cat(paste("4a. ",this.file," data has been read from [",FL.data,"] for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)

		# add auxilary columns to the U data frame
		if ((this.file == "UData_2011_AllWell.txt")  | (this.file == "UData_2010_AllWell.txt") )
		{	
			# only retain the wells in the jpg file
			# find the wells in [myData.raw] which is also in the shallow well lits in [Well.Shallow] list. 
			list.shallow.well <- Well.Shallow[match(names(myData.raw),Well.Shallow)[!is.na((match(names(myData.raw),Well.Shallow)))]]	# it is cubersome need better way to do the match.		
			myData.U <- myData.raw[,c("Dates",list.shallow.well)]	# match returns a vector of the positions of (first) matches of its first argument in its second. So anything matched (the well in [myData.raw] appears in the [Well.Shallow] list should be kept.  Anything matched not the position is needed for pulling the data out.

			# approximate the hour by omitting the minutes
			myData.U[,"Year"]  <- sub("(\\d+)-(\\d+)-(\\d+)","\\1",myData.U[,"Dates"])	
			myData.U[,"Month"] <- sub("(\\d+)-(\\d+)-(\\d+)","\\2",myData.U[,"Dates"])	
			myData.U[,"Day"]   <- sub("(\\d+)-(\\d+)-(\\d+)","\\3",myData.U[,"Dates"])	

			myData.U[,"date.chron"] <- chron(dates  = paste(myData.U[,"Month"],myData.U[,"Day"],myData.U[,"Year"],sep="/"),
							 times  = "00:00:00",
							 format = c('m/d/y','h:m:s'))

			# output the data for verification
			write.table(myData.U,file=FL.U.OUT,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
			cat(paste("4b. ",this.file," add [date.chron] and keep only shallow wells for year ",this.year,".\n",sep=""))
			cat(paste("4b. ",this.file," add [date.chron] and keep only shallow wells for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)

		# add auxilary columns to the Sp data frame
		}else if ((this.file == "SpCData_2011_AllWell.txt") | (this.file == "SpCData_2010_AllWell.txt"))
		{	
			# only retain the wells in the jpg file
			# find the wells in [myData.raw] which is also in the shallow well lits in [Well.Shallow] list. 
			list.shallow.well <- Well.Shallow[match(names(myData.raw),Well.Shallow)[!is.na((match(names(myData.raw),Well.Shallow)))]]	# it is cubersome need better way to do the match.		
			myData.Sp <- myData.raw[,c("Dates",list.shallow.well)]	# match returns a vector of the positions of (first) matches of its first argument in its second. So anything matched (the well in [myData.raw] appears in the [Well.Shallow] list should be kept.  Anything matched not the position is needed for pulling the data out.

			# approximate the hour by omitting the minutes
			myData.Sp[,"Year"]  <- sub("(\\d+)-(\\d+)-(\\d+)","\\1",myData.Sp[,"Dates"])	
			myData.Sp[,"Month"] <- sub("(\\d+)-(\\d+)-(\\d+)","\\2",myData.Sp[,"Dates"])	
			myData.Sp[,"Day"]   <- sub("(\\d+)-(\\d+)-(\\d+)","\\3",myData.Sp[,"Dates"])	

			myData.Sp[,"date.chron"] <- chron(dates  = paste(myData.Sp[,"Month"],myData.Sp[,"Day"],myData.Sp[,"Year"],sep="/"),
							  times  = "00:00:00",
							  format = c('m/d/y','h:m:s'))

			# output the data for verification
			write.table(myData.Sp,file=FL.Sp.OUT,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
			cat(paste("4c. ",this.file," add [date.chron] and keep only shallow wells for year ",this.year,".\n",sep=""))
			cat(paste("4c. ",this.file," add [date.chron] and keep only shallow wells for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)
		# add auxilary columns to the WL data frame
		}else if ((this.file == "2-10_WLData_2011") | (this.file == "2-10_WLData_2010"))
		{
			# split the date/time
			tmp      <- unlist(strsplit(myData.raw[,"Date-Time"],"\\s+"))
			tmp.Date <- tmp[seq(from=1,to=length(tmp),by=2)]
			tmp.Time <- tmp[seq(from=2,to=length(tmp),by=2)]

			myData.raw[,"Date"] <- tmp.Date
			myData.raw[,"Time"] <- tmp.Time

			myData.WL <- myData.raw

			myData.WL[,"date.chron"] <- chron(dates  = myData.WL[,"Date"],
							  times  = paste(myData.WL[,"Time"],":00",sep=""),
							  format = c('m/d/y','h:m:s'))

			# output the data for verification
			write.table(myData.WL,file=FL.WL.OUT,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
			cat(paste("4d. ",this.file," add [date.chron] and keep only shallow wells for year ",this.year,".\n",sep=""))
			cat(paste("4d. ",this.file," add [date.chron] and keep only shallow wells for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		
		}
	}
	cat(paste("4e. We have U data [myData.U], Sp data [myData.Sp] and WL data [myData.WL] for year ",this.year,".\n",sep=""))
	cat(paste("4e. We have U data [myData.U], Sp data [myData.Sp] and WL data [myData.WL] for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# use the date in column 1 as the row names
	row.names(myData.U)  <- myData.U[,1]	# use the date column as the row names
	row.names(myData.Sp) <- myData.Sp[,1]	# use the date column as the row names
	cat(paste("5. use date.chron filed as row names for year ",this.year,".\n",sep=""))
	cat(paste("5. use date.chron filed as row names for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# I. We should have [myData.U], [myData.Sp], [myData.WL] at this moment
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	#
	# only keep the WL data in the same time duration as the U and Sp data before merging
	#
	# 6. find the date range of the U and Sp data
	    date.range <- range(myData.U[,"date.chron"],na.rm=TRUE)
	myData.WL <- myData.WL[myData.WL[,"date.chron"] >= date.range[1] & myData.WL[,"date.chron"] <= date.range[2],]
	cat(paste("6. keep WL only in the U time duration for year ",this.year,".\n",sep=""))
	cat(paste("6. keep WL only in the U time duration for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	# 7A. merge the water table with the U data
	myData.Merged.U  <- merge(myData.U, myData.WL,by = c("date.chron"),all.x = TRUE)		
	
	# output the data for verification
	cat(paste("Merged data of U and WL,",sep=""),file=FL.Merged.U.WL,append=TRUE)
	write.table(myData.Merged.U,file=FL.Merged.U.WL,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	cat(paste("7A. merge WL and U data for year ",this.year,".\n",sep=""))
	cat(paste("7A. merge WL and U data for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# 7B. merge the water table with the Sp data
	myData.Merged.Sp <- merge(myData.Sp,myData.WL,by = c("date.chron"),all.x = TRUE)	# WL has mSpch more data than Sp and Sp so only keep WL when there are Sp and Sp
	# oSptpSpt the data for verification
	cat(paste("Merged data of Sp and WL,",sep=""),file=FL.Merged.Sp.WL,append=TRUE)
	write.table(myData.Merged.Sp,file=FL.Merged.Sp.WL,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	cat(paste("7B. merge WL and Sp data for year ",this.year,".\n",sep=""))
	cat(paste("7B. merge WL and Sp data for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		



	# =======================================================================
	# Prepare data frame for ggplot for U and Sp
	# =======================================================================
	# U
	myData.ggplot.wide.U <- myData.Merged.U
	id.vars      <- c("date.chron","Dates","Year","Month","Day","Date","Time","WL")
	rm.vars      <- "Date-Time"
	measure.vars <- names(myData.Merged.U)[!(names(myData.Merged.U) %in% c(id.vars,rm.vars))]
	myData.ggplot.wide.U <- myData.ggplot.wide.U[,c(id.vars,measure.vars)] 
	names(myData.ggplot.wide.U)[grep("-",names(myData.ggplot.wide.U))] <- paste("Well",grep("-",names(myData.ggplot.wide.U),value=TRUE),sep="")
	measure.vars <- paste("Well",measure.vars,sep="")
	
	myData.ggplot.long.U <- melt(myData.ggplot.wide.U,id.vars =id.vars,measure.vars = measure.vars,value.name="value")
	# myData.ggplot.long.U[,"Date"]  <- as.Date(myData.ggplot.long.U[,"Date"])			# m/d/yyy is not a unambiguous format
	myData.ggplot.wide.U[,"Dates"] <- as.Date(myData.ggplot.wide.U[,"Dates"])			# yyy-mm-dd is an unambiguous format
	myData.ggplot.long.U[,"Dates"] <- as.Date(myData.ggplot.long.U[,"Dates"])			# yyy-mm-dd is an unambiguous format

	# Sp
	myData.ggplot.wide.Sp <- myData.Merged.Sp
	id.vars      <- c("date.chron","Dates","Year","Month","Day","Date","Time","WL")
	rm.vars      <- "Date-Time"
	measure.vars <- names(myData.Merged.Sp)[!(names(myData.Merged.Sp) %in% c(id.vars,rm.vars))]
	myData.ggplot.wide.Sp <- myData.ggplot.wide.Sp[,c(id.vars,measure.vars)] 
	names(myData.ggplot.wide.Sp)[grep("-",names(myData.ggplot.wide.Sp))] <- paste("Well",grep("-",names(myData.ggplot.wide.Sp),value=TRUE),sep="")
	measure.vars <- paste("Well",measure.vars,sep="")
	
	myData.ggplot.long.Sp <- melt(myData.ggplot.wide.Sp,id.vars =id.vars,measure.vars = measure.vars,value.name="value")
	# myData.ggplot.long.Sp[,"Date"]  <- as.Date(myData.ggplot.long.Sp[,"Date"])			# m/d/yyy is not a SpnambigSpoSps format
	myData.ggplot.wide.Sp[,"Dates"] <- as.Date(myData.ggplot.wide.Sp[,"Dates"])			# yyy-mm-dd is an SpnambigSpoSps format
	myData.ggplot.long.Sp[,"Dates"] <- as.Date(myData.ggplot.long.Sp[,"Dates"])			# yyy-mm-dd is an SpnambigSpoSps format


	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# II. We should have [myData.ggplot.long.U], 
	#                    [myData.ggplot.wide.U], 
	#                    [myData.ggplot.long.Sp], 
	#                    [myData.ggplot.wide.Sp]
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


	# -------------------------------------------------------------------------------------------------
	# 8A. plot the data "U" and "WL": complete time series
	# -------------------------------------------------------------------------------------------------
	# U complete time series
	this.variable <- "U"
	p0 <- ggplot(data=myData.ggplot.long.U,aes(Dates,value,group=variable,color=variable) )
	p1 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Complete Time Series of ",this.variable,sep="")) +  scale_color_discrete(name="Well ID")

	p0 <- ggplot(data=myData.ggplot.long.U,aes(Dates,value,color=variable) ) 
	p2 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Complete Time Series of ",this.variable," at Year ", this.year,sep="")) +  scale_color_discrete(name="Well ID") + facet_wrap(~variable,ncol=6)
	cat(paste("8A. Plot the entire time series of U for year ",this.year,".\n",sep=""))
	cat(paste("8A. Plot the entire time series of U for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		
	
	# U truncated time series
	idx <- myData.ggplot.long.U[,"date.chron"] > this.chron.cut.begin & myData.ggplot.long.U[,"date.chron"] < this.chron.cut.end
	p0 <- ggplot(data=myData.ggplot.long.U[idx,],aes(Dates,value,group=variable,color=variable) )
	p3 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Truncated Time Series of ",this.variable,sep="")) +  scale_color_discrete(name="Well ID")

	p0 <- ggplot(data=myData.ggplot.long.U[idx,],aes(Dates,value,color=variable) ) 
	p4 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Truncated Time Series of ",this.variable," at Year ", this.year,sep="")) +  scale_color_discrete(name="Well ID") + facet_wrap(~variable,ncol=6)
	cat(paste("8B. Plot the time series of U starting at May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("8B. Plot the time series of U starting at May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# Sp complete time series
	this.variable <- "Sp"
	p0 <- ggplot(data=myData.ggplot.long.Sp,aes(Dates,value,group=variable,color=variable) )
	p5 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Complete Time Series of ",this.variable,sep="")) +  scale_color_discrete(name="Well ID")

	p0 <- ggplot(data=myData.ggplot.long.Sp,aes(Dates,value,color=variable) ) 
	p6 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Complete Time Series of ",this.variable," at Year ", this.year,sep="")) +  scale_color_discrete(name="Well ID") + facet_wrap(~variable,ncol=6)
	cat(paste("9A. Plot the entire time series of Sp for year ",this.year,".\n",sep=""))
	cat(paste("9A. Plot the entire time series of Sp for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		
	
	# Sp truncated time series
	idx <- myData.ggplot.long.Sp[,"date.chron"] > this.chron.cut.begin & myData.ggplot.long.Sp[,"date.chron"] < this.chron.cut.end
	p0 <- ggplot(data=myData.ggplot.long.Sp[idx,],aes(Dates,value,group=variable,color=variable) )
	p7 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Truncated Time Series of ",this.variable,sep="")) +  scale_color_discrete(name="Well ID")

	p0 <- ggplot(data=myData.ggplot.long.Sp[idx,],aes(Dates,value,color=variable) ) 
	p8 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Truncated Time Series of ",this.variable," at Year ", this.year,sep="")) +  scale_color_discrete(name="Well ID") + facet_wrap(~variable,ncol=6)
	cat(paste("9B. Plot the time series of Sp starting at May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("9B. Plot the time series of Sp starting at May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)	
	
	
	# plot out the plots: Nov 5, 2014: do not want to plot them out
	### print(p1)
	### print(p2)
	### print(p3)
	### print(p4)
	### print(p5)
	### print(p6)
	### print(p7)
	### print(p8)

	# -------------------------------------------------------------------------------------------------
	# 11. prepare cleaned data sets
	# -------------------------------------------------------------------------------------------------
	if (this.year.alt == 2010)
	{
		myData.U.set1 <- myData.Merged.U[(myData.Merged.U[,"date.chron"] >= chron(dates  = "06/07/10",times  = "00:00:00",format = c('m/d/y','h:m:s')) & 
		                                  myData.Merged.U[,"date.chron"] <= chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))),]
		list.well.set1 <- names(myData.U.set1)[!(names(myData.U.set1) %in% c("Dates","Year","Month","Day","date.chron","Date-Time","WL","Date","Time"))]                                  
	
	
		myData.U.set2 <- myData.Merged.U[(myData.Merged.U[,"date.chron"] >= chron(dates  = "05/11/10",times  = "00:00:00",format = c('m/d/y','h:m:s')) & 
		                                  myData.Merged.U[,"date.chron"] <= chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))),c("Dates","2-17","2-21","2-23","2-24","2-26","2-29","2-34","2-37","3-23","3-24","3-25","3-27","3-28","3-30","3-35","Year","Month","Day","date.chron","Date-Time","WL","Date","Time")]
		myData.U.set2 <- myData.U.set2[myData.U.set2[,"date.chron"] != chron(dates  = "05/30/10",times  = "00:00:00",format = c('m/d/y','h:m:s')),]
		list.well.set2 <- names(myData.U.set2)[!(names(myData.U.set2) %in% c("Dates","Year","Month","Day","date.chron","Date-Time","WL","Date","Time"))]    
		
		
		# use more wells
		myData.Merged.U <- myData.U.set1
		list.shallow.well.remained <- list.well.set1
		
		cat(paste("11. Deleted two wells for year ",this.year,".\n",sep=""))
		cat(paste("11. Deleted two wells for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)	
	}else if(this.year.alt == 20100)
	{
		myData.U.set2 <- myData.Merged.U[(myData.Merged.U[,"date.chron"] >= chron(dates  = "05/11/10",times  = "00:00:00",format = c('m/d/y','h:m:s')) & 
		                                  myData.Merged.U[,"date.chron"] <= chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))),c("Dates","2-17","2-21","2-23","2-24","2-26","2-29","2-34","2-37","3-23","3-24","3-25","3-27","3-28","3-30","3-35","Year","Month","Day","date.chron","Date-Time","WL","Date","Time")]
		myData.U.set2 <- myData.U.set2[myData.U.set2[,"date.chron"] != chron(dates  = "05/30/10",times  = "00:00:00",format = c('m/d/y','h:m:s')),]
		list.well.set2 <- names(myData.U.set2)[!(names(myData.U.set2) %in% c("Dates","Year","Month","Day","date.chron","Date-Time","WL","Date","Time"))]    
		
		
		# use more wells
		myData.Merged.U <- myData.U.set2
		list.shallow.well.remained <- list.well.set2
		
		cat(paste("11. Deleted two wells for year ",this.year,".\n",sep=""))
		cat(paste("11. Deleted two wells for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)	
	}else if (this.year.alt == 2011)
	{

		# -------------------------------------------------------------------------------------------------
		# 11. detele two wells 
		# -------------------------------------------------------------------------------------------------
		myData.Merged.U <- myData.Merged.U[myData.Merged.U[,"date.chron"] != chron(dates  = "07/30/11",times  = "00:00:00",format = c('m/d/y','h:m:s')),]
		myData.Merged.U <- myData.Merged.U[,names(myData.Merged.U)[!(names(myData.Merged.U) %in% c("2-34","2-37","3-35"))]]

		myData.Merged.Sp <- myData.Merged.Sp[myData.Merged.Sp[,"date.chron"] != chron(dates  = "07/30/11",times  = "00:00:00",format = c('m/d/y','h:m:s')),]
		myData.Merged.Sp <- myData.Merged.Sp[,names(myData.Merged.Sp)[!(names(myData.Merged.Sp) %in% c("2-34","2-37","3-35"))]]

		list.shallow.well.remained <- list.shallow.well[!(list.shallow.well %in% c("2-34","2-37","3-35"))]


		cat(paste("11. Deleted two wells for year ",this.year,".\n",sep=""))
		cat(paste("11. Deleted two wells for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)	
	}


	# Nov 5, 2014
	myData.Merged.U <- myData.Merged.U[myData.Merged.U[,"date.chron"] > this.chron.cut.begin & myData.Merged.U[,"date.chron"] < this.chron.cut.end,]
	cat(paste("11C. U data only after May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("11C. U data only after May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	myData.Merged.Sp <- myData.Merged.Sp[myData.Merged.Sp[,"date.chron"] > this.chron.cut.begin & myData.Merged.Sp[,"date.chron"] < this.chron.cut.end,]
	cat(paste("11D. U data only after May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("11D. U data only after May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	# =======================================================================
	# Prepare data frame for ggplot for U and Sp for the cleaned data
	# =======================================================================
	# U
	myData.ggplot.wide.U <- myData.Merged.U
	id.vars      <- c("date.chron","Dates","Year","Month","Day","Date","Time","WL")
	rm.vars      <- "Date-Time"
	measure.vars <- names(myData.Merged.U)[!(names(myData.Merged.U) %in% c(id.vars,rm.vars))]
	myData.ggplot.wide.U <- myData.ggplot.wide.U[,c(id.vars,measure.vars)] 
	names(myData.ggplot.wide.U)[grep("-",names(myData.ggplot.wide.U))] <- paste("Well",grep("-",names(myData.ggplot.wide.U),value=TRUE),sep="")
	measure.vars <- paste("Well",measure.vars,sep="")
	
	myData.ggplot.long.U <- melt(myData.ggplot.wide.U,id.vars =id.vars,measure.vars = measure.vars,value.name="value")
	# myData.ggplot.long.U[,"Date"]  <- as.Date(myData.ggplot.long.U[,"Date"])			# m/d/yyy is not a unambiguous format
	myData.ggplot.wide.U[,"Dates"] <- as.Date(myData.ggplot.wide.U[,"Dates"])			# yyy-mm-dd is an unambiguous format
	myData.ggplot.long.U[,"Dates"] <- as.Date(myData.ggplot.long.U[,"Dates"])			# yyy-mm-dd is an unambiguous format

	# Sp
	myData.ggplot.wide.Sp <- myData.Merged.Sp
	id.vars      <- c("date.chron","Dates","Year","Month","Day","Date","Time","WL")
	rm.vars      <- "Date-Time"
	measure.vars <- names(myData.Merged.Sp)[!(names(myData.Merged.Sp) %in% c(id.vars,rm.vars))]
	myData.ggplot.wide.Sp <- myData.ggplot.wide.Sp[,c(id.vars,measure.vars)] 
	names(myData.ggplot.wide.Sp)[grep("-",names(myData.ggplot.wide.Sp))] <- paste("Well",grep("-",names(myData.ggplot.wide.Sp),value=TRUE),sep="")
	measure.vars <- paste("Well",measure.vars,sep="")
	
	myData.ggplot.long.Sp <- melt(myData.ggplot.wide.Sp,id.vars =id.vars,measure.vars = measure.vars,value.name="value")
	# myData.ggplot.long.Sp[,"Date"]  <- as.Date(myData.ggplot.long.Sp[,"Date"])			# m/d/yyy is not a SpnambigSpoSps format
	myData.ggplot.wide.Sp[,"Dates"] <- as.Date(myData.ggplot.wide.Sp[,"Dates"])			# yyy-mm-dd is an SpnambigSpoSps format
	myData.ggplot.long.Sp[,"Dates"] <- as.Date(myData.ggplot.long.Sp[,"Dates"])			# yyy-mm-dd is an SpnambigSpoSps format



	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# III. We should have [myData.ggplot.long.U], 
	#                     [myData.ggplot.wide.U], 
	#                     [myData.ggplot.long.Sp], 
	#                     [myData.ggplot.wide.Sp] with the same set of wells and the same date/time points retained for the cluster analysis
	# Clustering analyss need a separate sets of data frame because we want to have a complete set of data
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



	# -------------------------------------------------------------------------------------------------
	# 12A. plot the data "U" and "WL": complete time series
	# -------------------------------------------------------------------------------------------------
	# U complete time series
	this.variable <- "U"
	p0 <- ggplot(data=myData.ggplot.long.U,aes(Dates,value,group=variable,color=variable) )
	p9 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Shallow Complete Time Series of ",this.variable,sep="")) +  scale_color_discrete(name="Well ID")

	p0 <- ggplot(data=myData.ggplot.long.U,aes(Dates,value,color=variable) ) 
	p10 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Shallow Complete Time Series of ",this.variable," at Year ", this.year,sep="")) +  scale_color_discrete(name="Well ID") + facet_wrap(~variable,ncol=6)
	cat(paste("12A. Plot the entire time series of U for year ",this.year,".\n",sep=""))
	cat(paste("12A. Plot the entire time series of U for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		
	
	# U truncated time series
	idx <- myData.ggplot.long.U[,"date.chron"] > this.chron.cut.begin & myData.ggplot.long.U[,"date.chron"] < this.chron.cut.end
	p0 <- ggplot(data=myData.ggplot.long.U[idx,],aes(Dates,value,group=variable,color=variable) )
	p11 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Shallow Truncated Time Series of ",this.variable,sep="")) +  scale_color_discrete(name="Well ID")

	p0 <- ggplot(data=myData.ggplot.long.U[idx,],aes(Dates,value,color=variable) ) 
	p12 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Shallow Truncated Time Series of ",this.variable," at Year ", this.year,sep="")) +  scale_color_discrete(name="Well ID") + facet_wrap(~variable,ncol=6)
	cat(paste("12B. Plot the time series of U starting at May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("12B. Plot the time series of U starting at May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# Sp complete time series
	this.variable <- "Sp"
	p0 <- ggplot(data=myData.ggplot.long.Sp,aes(Dates,value,group=variable,color=variable) )
	p13 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Shallow Complete Time Series of ",this.variable,sep="")) +  scale_color_discrete(name="Well ID")

	p0 <- ggplot(data=myData.ggplot.long.Sp,aes(Dates,value,color=variable) ) 
	p14 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Shallow Complete Time Series of ",this.variable," at Year ", this.year,sep="")) +  scale_color_discrete(name="Well ID") + facet_wrap(~variable,ncol=6)
	cat(paste("13A. Plot the entire time series of Sp for year ",this.year,".\n",sep=""))
	cat(paste("13A. Plot the entire time series of Sp for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		
	
	# Sp truncated time series
	idx <- myData.ggplot.long.Sp[,"date.chron"] > this.chron.cut.begin & myData.ggplot.long.Sp[,"date.chron"] < this.chron.cut.end
	p0 <- ggplot(data=myData.ggplot.long.Sp[idx,],aes(Dates,value,group=variable,color=variable) )
	p15 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Shallow Truncated Time Series of ",this.variable,sep="")) +  scale_color_discrete(name="Well ID")

	p0 <- ggplot(data=myData.ggplot.long.Sp[idx,],aes(Dates,value,color=variable) ) 
	p16 <- p0 + geom_line(alpha=1) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Shallow Truncated Time Series of ",this.variable," at Year ", this.year,sep="")) +  scale_color_discrete(name="Well ID") + facet_wrap(~variable,ncol=6)
	cat(paste("13B. Plot the time series of Sp starting at May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("13B. Plot the time series of Sp starting at May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# plot out the plots: Nov 5, 2014: do not want to plot them out
	### print(p9)
	### print(p10)
	### print(p11)
	### print(p12)
	### print(p13)
	### print(p14)
	### print(p15)
	### print(p16)









	# -------------------------------------------------------------------------------------
	# [myData.U]: only keep data after May 6, 20111 or even after May 12, 2011)
	# -------------------------------------------------------------------------------------
	myData.U <- myData.U[myData.U[,"date.chron"] > this.chron.cut.begin & myData.U[,"date.chron"] < this.chron.cut.end,]
	cat(paste("15A. U data only after May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("15A. U data only after May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	myData.Sp <- myData.Sp[myData.Sp[,"date.chron"] > this.chron.cut.begin & myData.Sp[,"date.chron"] < this.chron.cut.end,]
	cat(paste("15B. Sp data in the same time period as U data for year ",this.year,".\n",sep=""))
	cat(paste("15B. Sp data in the same time period as U data for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	# --------------------------------------------------------------------
	# [myData.U]: detele well 2-34, 2-37 and date 7/30 which have more missing
	# --------------------------------------------------------------------
	if (this.year.alt == 2011)
	{	
		myData.U <- myData.U[myData.U[,"date.chron"] != chron(dates  = "07/30/11",times  = "00:00:00",format = c('m/d/y','h:m:s')),]
		myData.U <- myData.U[,names(myData.U)[!(names(myData.U) %in% c("2-34","2-37","3-35"))]]
		
		# keep the Sp data in the same time period as U data
		myData.Sp <- myData.Sp[myData.Sp[,"date.chron"] != chron(dates  = "07/30/11",times  = "00:00:00",format = c('m/d/y','h:m:s')),]
		myData.Sp <- myData.Sp[,names(myData.Sp)[!(names(myData.Sp) %in% c("2-34","2-37","3-35"))]]
		
		chron.begin <- this.chron.cut.begin
		chron.end   <- this.chron.cut.end			
	}else if (this.year.alt == 2010)
	{
		myData.U <- myData.U[(myData.U[,"date.chron"] >= chron(dates  = "06/07/10",times  = "00:00:00",format = c('m/d/y','h:m:s')) & 
		                      myData.U[,"date.chron"] <= chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))),]
		
		# keep the Sp data in the same time period as U data
		myData.Sp <- myData.Sp[(myData.Sp[,"date.chron"] >= chron(dates  = "06/07/10",times  = "00:00:00",format = c('m/d/y','h:m:s')) & 
		                      myData.Sp[,"date.chron"] <= chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))),]
		                      		                     
		chron.begin <- chron(dates  = "06/07/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		chron.end   <- chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))		                      
	}else if (this.year.alt == 20100)
	{
		myData.U <- myData.U[(myData.U[,"date.chron"] >= chron(dates  = "05/11/10",times  = "00:00:00",format = c('m/d/y','h:m:s')) & 
		                      myData.U[,"date.chron"] <= chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))),c("Dates","2-17","2-21","2-23","2-24","2-26","2-29","2-34","2-37","3-23","3-24","3-25","3-27","3-28","3-30","3-35","Year","Month","Day","date.chron")]
		myData.U <- myData.U[myData.U[,"date.chron"] != chron(dates  = "05/30/10",times  = "00:00:00",format = c('m/d/y','h:m:s')),]

		# Oct 7, 2014: keep the Sp data in the same time period as U data: but delete 5/12/2010 and 5/15/2010 data which has too many NAs
		myData.Sp <- myData.Sp[(myData.Sp[,"date.chron"] >= chron(dates  = "05/11/10",times  = "00:00:00",format = c('m/d/y','h:m:s')) & 
		                      myData.Sp[,"date.chron"] <= chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))),c("Dates","2-17","2-21","2-23","2-24","2-26","2-29","2-34","2-37","3-23","3-24","3-25","3-27","3-28","3-30","3-35","Year","Month","Day","date.chron")]
		myData.Sp <- myData.Sp[myData.Sp[,"date.chron"] != chron(dates  = "05/12/10",times  = "00:00:00",format = c('m/d/y','h:m:s')) & myData.Sp[,"date.chron"] != chron(dates  = "05/15/10",times  = "00:00:00",format = c('m/d/y','h:m:s')),]
		
		chron.begin <- chron(dates  = "05/11/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		chron.end   <- chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
	}
	cat(paste("16. delete well 2-34, 2-37, 3-35 and date July 30 which have the most missing data for year ",this.year,".\n",sep=""))
	cat(paste("16. delete well 2-34, 2-37, 3-35 and date July 30 which have the most missing data for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# IV. We should have [myData.U], 
	#                    [myData.Sp] which are the basis for the clustering analysis
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


	#  
	myData.PCA_U1  <-   myData.U[,intersect(names(myData.U),  list.shallow.well)]
	myData.PCA_Sp1 <-   myData.Sp[,intersect(names(myData.Sp),list.shallow.well)]
	cat(paste("17. Use the wide format of U/Sp data for PCA for year ",this.year,".\n",sep=""))
	cat(paste("17. Use the wide format of U/Sp data for PCA for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# imputation
	myData.PCA_U1.imputed  <- imputePCA(myData.PCA_U1, ncp=6)
	myData.PCA_Sp1.imputed <- imputePCA(myData.PCA_Sp1,ncp=6)
	cat(paste("19. impute the U/Sp data for year ",this.year,".\n",sep=""))
	cat(paste("19. impute the U/Sp data for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# [myData.U.complete] & [myData.Sp.complete]: completed data by imputation
	myData.U.complete  <- myData.PCA_U1.imputed$completeObs
	myData.Sp.complete <- myData.PCA_Sp1.imputed$completeObs
	cat(paste("20. the complete U/Sp data after imputation for year ",this.year,".\n",sep=""))
	cat(paste("20. the complete U/Sp data after imputation for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# output the file for other usage
	cat(paste("U complete data for analysis,",sep=""),file=FL.Imput.csv,append=TRUE)
	write.table(myData.U.complete,file=FL.Imput.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	
	cat(paste("\n\nSp complete data for analysis,",sep=""),file=FL.Imput.csv,append=TRUE)
	write.table(myData.Sp.complete,file=FL.Imput.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	
	save(myData.U.complete,myData.Sp.complete,file=FL.Imput.obj)
	cat(paste("21. save the complete U/Sp data for other usage for year ",this.year,".\n",sep=""))
	cat(paste("21. save the complete U/Sp data for other usage for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	#  V. We should have [myData.U.complete], 
	#                    [myData.Sp.complete] which are the complete data for clustering analysis
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



	# -------------------------------------------------------------------------------------------------
	# get in the coordinate data
	# -------------------------------------------------------------------------------------------------
	Path.Cord <- paste(Path.Project,"0_data",   sep="/")
	if (!file.exists(Path.Cord)){print(paste("NOT existing:",Path.Cord," Check Why!\n"));die}
	FL.Cord <- paste(Path.Cord,"well_list_full.txt",sep="/")
	if (!(file.exists(FL.Cord))){print(paste(FL.Cord," does not exist. Check why!"));die}	
	cat(paste("22. get the coordinates for year ",this.year,".\n",sep=""))
	cat(paste("22. get the coordinates for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	# read the well coordinate data
	myXY <- read.table(file=FL.Cord,sep="\t",header=TRUE)
	names(myXY) <- c("WellID","X","Y","Elev(m)","top_screen_ft","bottom_screen_ft","HR_Depth")

	# remove "399-" from the wellID
	myXY[,"WellID"] <- gsub("399-","",myXY[,"WellID"])

	# change "#-0#" to "#-#'
	myXY[,"WellID"] <- sub("-0","-",myXY[,"WellID"])
	cat(paste("23. clean the coordinates for year ",this.year,".\n",sep=""))
	cat(paste("23. clean the coordinates for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	#
	# keep only those wells in [list.shallow.well.remained]
	#
	my.U.extract.thisYear <- my.U.extract.thisYear[my.U.extract.thisYear[,"Well_ID"] %in% list.shallow.well.remained,]
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# VI. We should have [myXY]                  the well coordinates
	#                    [my.U.extract.thisYear] the extractable U
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++












	
	# ----------------------------------------------------------------------------------------------------------------- 
	# Loopping for clustering
	# -----------------------------------------------------------------------------------------------------------------
	for (this.variable in c("Sp","U"))
	{
		if (this.variable == "Sp")
		{
			myDATA <- t(myData.Sp.complete)	# which is a 90 by 26 data frame
			myDATA.MERGED <- myData.Merged.Sp
			cluser.array <- c(2,3,4,5)
			this.unit <- "SpC (S/m)"
		}else if (this.variable == "U")
		{
			myDATA <- t(myData.U.complete)	# which is a 90 by 26 data frame
			myDATA.MERGED <- myData.Merged.U
			cluser.array <- c(4,5)
			this.unit <- "U (ug.L-1)"
		}
		
		# -----------------------------------------------------
		# write out intermediate data for verification purpose
		# -----------------------------------------------------
		FL.INT.CSV <- paste(Path.results,paste("intermediate_",this.year.string,"_",this.variable,".csv",sep=""),sep="/")
		if (file.exists(FL.INT.CSV)){print(paste(FL.INT.CSV, "exist.Delete it!"));file.remove(FL.INT.CSV)}
		cat(paste(this.year.string,":",this.variable," in [myDATA] with imputation,",sep=""),file=FL.INT.CSV,append=TRUE)
		write.table(myDATA,file=FL.INT.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
		cat(paste("\n\n",this.year.string,":",this.variable," in [myDATA.MERGED] without imputation,",sep=""),file=FL.INT.CSV,append=TRUE)
		write.table(myDATA.MERGED,file=FL.INT.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
		
		
		cat(paste("24. (",this.variable,") make [myDATA] for subsequential analysis for year ",this.year," for ",this.variable,".\n",sep=""))
		cat(paste("24. (",this.variable,") make [myDATA] for subsequential analysis for year ",this.year," for ",this.variable,".\n",sep=""),file=FL.LOG,append=TRUE)		

		# *********************************************************************************
		# 1. kMean
		# *********************************************************************************

		#
		# Loopping for different K for KMeans
		# 
		for (this.no.cls in cluser.array)
		{
			no.cls <- this.no.cls
			
			# check the location of the wells when no of cluster = 5
			# use 5 classes Kmean
			model.kMeans  <- kmeans(myDATA,centers=no.cls,nstart=10,iter.max=50,algorithm = "Hartigan-Wong")
			class.label   <- model.kMeans$cluster

			# put class index and well coordinate into a data frame
			myData.chron <- chron(dates = paste(sub("(.*)-(.*)-(.*)","\\2",colnames(myDATA)),sub("(.*)-(.*)-(.*)","\\3",colnames(myDATA)),sub("(.*)-(.*)-(.*)","\\1",colnames(myDATA)),sep="/"),
					      times = "0:0:0",
					      format = c('m/d/y','h:m:s'))
			myTmp <- cbind(WellID      = rownames(myDATA),						    
				       class.label = class.label,
				       as.data.frame(myDATA))

			# merge the cluster index with the XY coordinates
			myMerged <- merge(myTmp,myXY[,c("WellID","X","Y")],by=c("WellID"),all=FALSE)                     
			rownames(myMerged) <- myMerged[,"WellID"]
			cat(paste("25A. (",this.variable,") [",no.cls," classes]: Kmean for 5 clusters for year ",this.year,".\n",sep=""))
			cat(paste("25A. (",this.variable,") [",no.cls," classes]: Kmean for 5 clusters for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)	
		
			# prepare data for ggplot
			id.vars        <-c("WellID","class.label","X","Y")
			measure.vars   <- meaure.vars <- names(myMerged)[!(names(myMerged) %in% id.vars)]
			myData.4ggplot <- melt(myMerged,id.vars = id.vars,measure.vars = measure.vars,,value.name="value")
			myData.4ggplot[,"variable"] <- as.Date(myData.4ggplot[,"variable"] )
			
			# add a cut on the date based on maximum WL
			myData.4ggplot[,"from.WL.peak"] <- "before WL peak"
			myData.4ggplot[myData.4ggplot[,"variable"] > date.max.WL,"from.WL.peak"] <- "after WL peak"
			
			# ------------------------------------------------------------------------------------
			# ****************** re-assign class name ******************
			# ------------------------------------------------------------------------------------
			if (this.no.cls == 4)
			{
				if (this.variable == "Sp")
				{
					myData.4ggplot[,"class.label"] <- factor(sub("1","SW",sub("2","SE",sub("3","NE",sub("4","NW",myData.4ggplot[,"class.label"])))),levels=c("SW","SE","NE","NW"),labels=c("SW","SE","NE","NW"),ordered=TRUE)
				}else if (this.variable == "U")
				{
					myData.4ggplot[,"class.label"] <- factor(sub("1","SE",sub("2","NW",sub("3","SW",sub("4","NE",myData.4ggplot[,"class.label"])))),levels=c("SW","SE","NE","NW"),labels=c("SW","SE","NE","NW"),ordered=TRUE)
				}
				cat(paste("25B. (",this.variable,") [",no.cls," classes]: Reassign cluster labels.\n",sep=""))
				cat(paste("25B. (",this.variable,") [",no.cls," classes]: Reassign cluster labels.\n",sep=""),file=FL.LOG,append=TRUE)	
			}
			if(!(is.factor(myData.4ggplot[,"class.label"])))
			{
				myData.4ggplot[,"Cluster"] <- as.factor(myData.4ggplot[,"class.label"])
			}else{
				myData.4ggplot[,"Cluster"] <- myData.4ggplot[,"class.label"]
			}
			cat(paste("2511A. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""))
			cat(paste("2511A. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""),file=FL.LOG,append=TRUE)	
			
			# only keep four weels
			myData.4ggplot.4wells <- subset(myData.4ggplot,WellID %in% c("2-22","2-14","3-23","3-28"))


			
			# time seris colored by cluster label
			p0  <- ggplot(data=myData.4ggplot,aes(x=variable,y=value,group=WellID,color=Cluster)) 
			p13 <- p0 + geom_line() + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.unit) +  scale_color_discrete(name="Cluster") # title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep="")

			p13 <- p13 + theme(axis.text.x = element_text(angle=45,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
			p13 <- p13 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
			p13 <- p13 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 


			if (this.variable == "Sp")
			{
				p0  <- ggplot(data=myData.4ggplot.4wells,aes(x=variable,y=value,label=paste("Well",WellID,sep=""),group=WellID,color=Cluster,shape=factor(WellID))) 
				p13B <- p0   + geom_line() +geom_point() + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.unit) +  scale_color_discrete(name="Cluster") # ,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep="")
				p13B <- p13B + theme(axis.text.x = element_text(angle=45,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
				p13B <- p13B + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
				p13B <- p13B + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 
			}else if(this.variable == "U")
			{
			
				p0  <- ggplot(data=myData.4ggplot.4wells,aes(x=variable,y=value,label=paste("Well",WellID,sep=""),group=WellID,color=Cluster,shape=factor(WellID))) 
				p13B <- p0   + geom_line() +geom_point() + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y= expression(paste('U (',mu,gL^-1,')',sep=""))) +  scale_color_discrete(name="Cluster") # ,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep="")
				p13B <- p13B + theme(axis.text.x = element_text(angle=45,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
				p13B <- p13B + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
				p13B <- p13B + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 			
			}
			cat(paste("2511B. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""))
			cat(paste("2511B. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""),file=FL.LOG,append=TRUE)	


			# box plot of wells
			p0  <- ggplot(data=myData.4ggplot,aes(x=WellID,y=value,group=WellID,fill=Cluster,colour=Cluster)) 
			p14 <- p0 + geom_boxplot(notch=TRUE) 	# + scale_fill_manual(name = "Cluster",values=c("pink","cyan","green","magenta")[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep=""))
			p14 <- p14 + theme(axis.text.x = element_text(angle=90,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
			p14 <- p14 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
			p14 <- p14 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 
			
			# p14 <- p0 + geom_boxplot(notch=TRUE,colour=class.label) + scale_fill_manual(name = "Cluster",values=c("pink","cyan","green","magenta")[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep=""))
			
			
     			 # box plot of clusters
			p0  <- ggplot(data=myData.4ggplot,aes(x=Cluster,y=value,group=Cluster,fill=Cluster,colour=Cluster)) 
			p15 <- p0 + geom_boxplot(notch=TRUE) + labs(x="Cluster",y=this.unit) # + scale_fill_manual(name = "Cluster",values=c("pink","cyan","green","magenta")[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep=""))
			p15 <- p15 + theme(axis.text.x = element_text(angle=90,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
			p15 <- p15 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
			p15 <- p15 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 


			p0  <- ggplot(data=myData.4ggplot,aes(x=X,y=Y,label=paste("Well",WellID,sep=""),group=WellID,color=Cluster)) 
			p16 <- p0 + geom_point(size=2,shape=4) + geom_text(size=3,vjust=-0.5) + coord_equal() + theme(legend.position="right") + labs(x="Easting (m)",y="Northing (m)") 	# ,title=paste("Location of the wells in the clusters of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep=""))
			p16 <- p16 + theme(axis.text.x = element_text(angle=90,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
			p16 <- p16 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
			p16 <- p16 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 
			
			cat(paste("2511C. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""))
			cat(paste("2511C. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""),file=FL.LOG,append=TRUE)	

		
		
			# a series of plots for the cluster results
			if (this.no.cls == 4)
			{
      				multiplot(p13,p14,p15,p16,cols=2)
      			
      				plot(p13)
      				plot(p13B)
      				plot(p15)
      				plot(p16)
      				
      				# -----------------------------------------------------------------------------------------------------------
      				# April 4, 2015: output plots in jpdg format
      				#                Figure 7 and 8
      				# -----------------------------------------------------------------------------------------------------------
      				if (this.year.alt == "2011" & this.variable == "Sp" & this.no.cls == 4)
      				{      					
      					FL.Fig7b <- paste(Path.results,paste("Fig7B_SpC.jpg",sep=""),sep="/")				# p16
      					FL.Fig8b <- paste(Path.results,paste("Fig8B_SpC.jpg",sep=""),sep="/")				# p13B      					
					if (file.exists(FL.Fig7b)){print(paste(FL.Fig7b,"exist.Delete it!"));file.remove(FL.Fig7b)}
					if (file.exists(FL.Fig8b)){print(paste(FL.Fig8b,"exist.Delete it!"));file.remove(FL.Fig8b)}
					
					# Figure 7B
					jpeg(file = FL.Fig7b,width=12,height=8,units="in",res=1200,bg = "transparent")
					dev.set(3)	
						plot(p16)
					dev.off(3)
					
					# Figure 8B
					jpeg(file = FL.Fig8b,width=12,height=8,units="in",res=1200,bg = "transparent")
					dev.set(3)
						plot(p13B)
					dev.off(3)
      				}else if (this.year.alt == "2011" & this.variable == "U" & this.no.cls == 4)
      				{
      					FL.Fig7a <- paste(Path.results,paste("Fig7A_Uaq.jpg",sep=""),sep="/")				# p16
      					FL.Fig8a <- paste(Path.results,paste("Fig8A_Uaq.jpg",sep=""),sep="/")				# p13B      					
					if (file.exists(FL.Fig7a)){print(paste(FL.Fig7a,"exist.Delete it!"));file.remove(FL.Fig7a)}
					if (file.exists(FL.Fig8a)){print(paste(FL.Fig8a,"exist.Delete it!"));file.remove(FL.Fig8a)}   
					
					# Figure 7A
					jpeg(file = FL.Fig7a,width=12,height=8,units="in",res=1200,bg = "transparent")
					dev.set(3)	
						plot(p16)
					dev.off(3)
					
					# Figure 8A
					jpeg(file = FL.Fig8a,width=12,height=8,units="in",res=1200,bg = "transparent")
					dev.set(3)
						plot(p13B)
					dev.off(3)					
      				}
      			}
			cat(paste("2511D. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""))
			cat(paste("2511D. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""),file=FL.LOG,append=TRUE)	
      			

			# Oct 23, 2014: box plot of extractable uranium
			myExtracU.4ggplot <- my.U.extract.thisYear
			myExtracU.4ggplot[,"class.label"] <- class.label[myExtracU.4ggplot[,"Well_ID"]]
			
			# ------------------------------------------------------------------------------------
			# ****************** re-assign class name ******************
			# ------------------------------------------------------------------------------------
			if (this.no.cls == 4)
			{			
				if (this.variable == "Sp")
				{
					myExtracU.4ggplot[,"class.label"] <- factor(sub("1","SW",sub("2","SE",sub("3","NE",sub("4","NW",myExtracU.4ggplot[,"class.label"])))),levels=c("SW","SE","NE","NW"),labels=c("SW","SE","NE","NW"),ordered=TRUE)
				}else if (this.variable == "U")
				{
					myExtracU.4ggplot[,"class.label"] <- factor(sub("1","SE",sub("2","NW",sub("3","SW",sub("4","NE",myExtracU.4ggplot[,"class.label"])))),levels=c("SW","SE","NE","NW"),labels=c("SW","SE","NE","NW"),ordered=TRUE)
				}
			}
			myExtracU.4ggplot[,"Cluster"] <- myExtracU.4ggplot[,"class.label"]			
			cat(paste("2511C. (",this.variable,") [",no.cls," classes]: Reassign cluster labels.\n",sep=""))
			cat(paste("2511C. (",this.variable,") [",no.cls," classes]: Reassign cluster labels.\n",sep=""),file=FL.LOG,append=TRUE)				
		
			
			
			
			p0 <- ggplot(data=myExtracU.4ggplot,aes(x=Cluster,y=extractable.U,fill=Cluster))
			p.add1 <- p0  + geom_boxplot(notch=TRUE) + labs(x="Cluster",y="Extractable U (Kg)") + theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=2)) # + scale_fill_manual(name = "Cluster",values=c("pink","cyan","green","magenta")[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep="")) + labs(x="Cluster",y="Extractable U",title=paste("Year ", this.year,": Extractable U: ",no.cls," clusters of Kmeans",sep="")) + theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=2))  
			p.add1 <- p.add1 + theme(axis.text.x = element_text(angle=90,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
			p.add1 <- p.add1 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
			p.add1 <- p.add1 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 
			
			
			# Oct 23, 2014: boxplot of aqueous Uranium prior and post to the WL peak (always for Uranium no matter cluster was created by Sp or by U)
			if (this.variable == "U")
			{
				myAqueousU.4ggplot <- myData.ggplot.long.U
				myAqueousU.4ggplot[,"from.WL.peak"] <- "before WL peak"
				myAqueousU.4ggplot[myAqueousU.4ggplot[,"Dates"] > date.max.WL,"from.WL.peak"] <- "after WL peak"
				myAqueousU.4ggplot[,"class.label"] <- class.label[sub("Well","",myAqueousU.4ggplot[,"variable"])] 
			}else if (this.variable == "Sp")
			{
				mySp.4ggplot <- myData.ggplot.long.U
				mySp.4ggplot[,"from.WL.peak"] <- "before WL peak"
				mySp.4ggplot[mySp.4ggplot[,"Dates"] > date.max.WL,"from.WL.peak"] <- "after WL peak"
				mySp.4ggplot[,"class.label"] <- class.label[sub("Well","",mySp.4ggplot[,"variable"])] 
			
			}
			cat(paste("2511D. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""))
			cat(paste("2511D. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""),file=FL.LOG,append=TRUE)	
			
			# ------------------------------------------------------------------------------------
			# ****************** re-assign class label ******************
			# ------------------------------------------------------------------------------------
			if (this.no.cls == 4)
			{			
				if (this.variable == "Sp")
				{
					# myAqueousU.4ggplot[,"class.label"] <- factor(sub("1","SW",sub("2","SE",sub("3","NE",sub("4","NW",myAqueousU.4ggplot[,"class.label"])))),levels=c("SW","SE","NE","NW"),labels=c("SW","SE","NE","NW"),ordered=TRUE)
					  mySp.4ggplot[,"class.label"]       <- factor(sub("1","SW",sub("2","SE",sub("3","NE",sub("4","NW",mySp.4ggplot[,"class.label"])))),levels=c("SW","SE","NE","NW"),labels=c("SW","SE","NE","NW"),ordered=TRUE)
				}else if (this.variable == "U")
				{
					  myAqueousU.4ggplot[,"class.label"] <- factor(sub("1","SE",sub("2","NW",sub("3","SW",sub("4","NE",myAqueousU.4ggplot[,"class.label"])))),levels=c("SW","SE","NE","NW"),labels=c("SW","SE","NE","NW"),ordered=TRUE)
					# mySp.4ggplot[,"class.label"]       <- factor(sub("1","SE",sub("2","NW",sub("3","SW",sub("4","NE",mySp.4ggplot[,"class.label"])))),levels=c("SW","SE","NE","NW"),labels=c("SW","SE","NE","NW"),ordered=TRUE)
				}

				if (this.variable == "Sp")
				{
					mySp.4ggplot[,"Cluster"]     <- mySp.4ggplot[,"class.label"]
					mySp.4ggplot[,"cluster.WL"]  <- paste(paste(mySp.4ggplot[,"class.label"],sep=""),mySp.4ggplot[,"from.WL.peak"],sep="-")
					mySp.4ggplot[,"WellID.WL"]   <- paste(mySp.4ggplot[,"variable"],mySp.4ggplot[,"from.WL.peak"],sep="-")
					
					mySp.4ggplot[,"cluster.WL"]  <- factor(mySp.4ggplot[,"cluster.WL"],levels=c("SW-before WL peak","SW-after WL peak","SE-before WL peak","SE-after WL peak","NE-before WL peak","NE-after WL peak","NW-before WL peak","NW-after WL peak"),labels=c("SW-before WL peak","SW-after WL peak","SE-before WL peak","SE-after WL peak","NE-before WL peak","NE-after WL peak","NW-before WL peak","NW-after WL peak"),ordered=TRUE)
				}else if(this.variable == "U")
				{
					myAqueousU.4ggplot[,"Cluster"]     <- myAqueousU.4ggplot[,"class.label"]
					myAqueousU.4ggplot[,"cluster.WL"]  <- paste(paste(myAqueousU.4ggplot[,"class.label"],sep=""),myAqueousU.4ggplot[,"from.WL.peak"],sep="-")
					myAqueousU.4ggplot[,"WellID.WL"]   <- paste(myAqueousU.4ggplot[,"variable"],myAqueousU.4ggplot[,"from.WL.peak"],sep="-")

					myAqueousU.4ggplot[,"cluster.WL"]  <- factor(myAqueousU.4ggplot[,"cluster.WL"],levels=c("SW-before WL peak","SW-after WL peak","SE-before WL peak","SE-after WL peak","NE-before WL peak","NE-after WL peak","NW-before WL peak","NW-after WL peak"),labels=c("SW-before WL peak","SW-after WL peak","SE-before WL peak","SE-after WL peak","NE-before WL peak","NE-after WL peak","NW-before WL peak","NW-after WL peak"),ordered=TRUE)
				}
				cat(paste("2511E. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""))
				cat(paste("2511E. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""),file=FL.LOG,append=TRUE)	
			}
			
			
			# Oct 23, 2014: boxplot of all wells within cluster
			if (this.no.cls==4)
			{
				if (this.variable == "Sp")
				{
					p0 <- ggplot(data=mySp.4ggplot,aes(x=class.label,y=value,fill=Cluster))
					p.add2 <- p0  + geom_boxplot(notch=TRUE) + labs(x="Cluster",y=this.unit,title=paste("NOT UPDATED!!! Year ", this.year,": Sp: ",no.cls," clusters of Kmeans",sep="")) + theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=2))  + scale_fill_manual(name = "Cluster",values=c("pink","cyan","green","magenta")[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep=""))
					p.add2 <- p.add2 + theme(axis.text.x = element_text(angle=90,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
					p.add2 <- p.add2 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
					p.add2 <- p.add2 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 



					# boxplot of all wells within cluster (split by the maximum WL)
					p0 <- ggplot(data=mySp.4ggplot,aes(x=cluster.WL,y=value,fill=as.factor(cluster.WL)))
					p.add3 <- p0  + geom_boxplot(notch=TRUE) + labs(x="Categories",y=this.unit) + theme(axis.text.x = element_text(angle=45,size=10,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=2))  + scale_fill_manual(guide=FALSE,name = "Category",values=col.array2)   # title=paste("Year ", this.year,": Sp: ",no.cls," clusters of Kmeans (before/after max WL)",sep="")
					p.add3 <- p.add3 + theme(axis.text.x = element_text(angle=90,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
					p.add3 <- p.add3 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
					# p.add3 <- p.add3 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 
					

					# boxplot of individual well in the clusters (split by the maximum WL)
					p0 <- ggplot(data=mySp.4ggplot,aes(x=WellID.WL,y=value,fill=cluster.WL))
					p.add4 <- p0  + geom_boxplot(notch=TRUE) + labs(x="Categories",y=this.unit,title=paste("NOT UPDATED!!! Year ", this.year,": Sp: ",no.cls," clusters of Kmeans (before/after max WL)",sep="")) + theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=2)) + scale_fill_manual(guide=FALSE,name = "Category",values=col.array2)
					p.add4 <- p.add4 + theme(axis.text.x = element_text(angle=90,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
					p.add4 <- p.add4 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
					# p.add4 <- p.add4 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 
					
					cat(paste("2511F. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""))
					cat(paste("2511F. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""),file=FL.LOG,append=TRUE)	
				}else if (this.variable == "U")
				{
					p0 <- ggplot(data=myAqueousU.4ggplot,aes(x=class.label,y=value,fill=Cluster))
					p.add2 <- p0  + geom_boxplot(notch=TRUE) + labs(x="Cluster",y="Aqueous U (ugL-1)",title=paste("NOT UPDATED!!! Year ", this.year,": Aqueous U: ",no.cls," clusters of Kmeans",sep="")) + theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=2))  + scale_fill_manual(guide=FALSE,name = "Cluster",values=c("pink","cyan","green","magenta")[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep=""))
					p.add2 <- p.add2 + theme(axis.text.x = element_text(angle=90,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
					p.add2 <- p.add2 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
					p.add2 <- p.add2 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 
					

					# boxplot of all wells within cluster (split by the maximum WL)
					p0 <- ggplot(data=myAqueousU.4ggplot,aes(x=cluster.WL,y=value,fill=as.factor(cluster.WL)))
					p.add3 <- p0  + geom_boxplot(notch=TRUE) + labs(x="Category",y=expression(paste('Aqueous U (',mu,gL^-1,')',sep=""))) + theme(axis.text.x = element_text(angle=45,size=10,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=2))  + scale_fill_manual(guide=FALSE,name = "Category",values=col.array2)	# ,title=paste("Year ", this.year,": Aqueous U: ",no.cls," clusters of Kmeans (before/after max WL)",sep="")
					p.add3 <- p.add3 + theme(axis.text.x = element_text(angle=90,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
					p.add3 <- p.add3 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
					# p.add3 <- p.add3 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 
					

					# boxplot of individual well in the clusters (split by the maximum WL)
					p0 <- ggplot(data=myAqueousU.4ggplot,aes(x=WellID.WL,y=value,fill=cluster.WL))
					p.add4 <- p0  + geom_boxplot(notch=TRUE) + labs(x="Category",y="Aqueous U (ugL-1)",title=paste("NOT UPDATED!!! Year ", this.year,": Aqueous U: ",no.cls," clusters of Kmeans (before/after max WL)",sep="")) + theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=2)) + scale_fill_manual(guide=FALSE,name = "Category",values=col.array2)
					p.add4 <- p.add4 + theme(axis.text.x = element_text(angle=90,size=15,vjust=0.5,color="black"),axis.title.x = element_text(color="black",size=20,vjust=0)) 
					p.add4 <- p.add4 + theme(axis.text.y = element_text(angle=0,size=15,vjust=0.5,color="black"),axis.title.y = element_text(color="black",size=20,vjust=1)) 
					# p.add4 <- p.add4 + theme(legend.position="right",legend.text = element_text(color="black",size=15,vjust=2),legend.title = element_text(color="black",size=20,vjust=2)) 
					
					cat(paste("2511F. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""))
					cat(paste("2511F. (",this.variable,") [",no.cls," classes]: We are here.\n",sep=""),file=FL.LOG,append=TRUE)				
				}
				
			}


      			
      			# Oct 23, 2014: added boxplots on Oct 23, 2014
			if (this.no.cls == 4)
			{      		
				cat("pause at line 1080\n")
				# browser()
				
	      			multiplot(p.add1,p.add2,p.add3,p.add4,cols=2)
	      			plot(p.add1)
	      			plot(p.add3)
	      			
      				# -----------------------------------------------------------------------------------------------------------
      				# April 4, 2015: output plots in jpdg format
      				#                Figure 9 and 10
      				# -----------------------------------------------------------------------------------------------------------
      				if (this.year.alt == "2011" & this.variable == "Sp" & this.no.cls == 4)
      				{      	
      					FL.Fig9b <- paste(Path.results,paste("Fig9B_SpC.jpg",sep=""),sep="/")				# p.add3
					if (file.exists(FL.Fig9b)){print(paste(FL.Fig9b,"exist.Delete it!"));file.remove(FL.Fig9b)}

      					FL.Fig10 <- paste(Path.results,paste("Fig10_BEU_",this.variable,".jpg",sep=""),sep="/")		# p.add1
					if (file.exists(FL.Fig10)){print(paste(FL.Fig10,"exist.Delete it!"));file.remove(FL.Fig10)}
					
					# Figure 9B
					jpeg(file = FL.Fig9b,width=12,height=8,units="in",res=1200,bg = "transparent")
					dev.set(3)	
						plot(p.add3)
					dev.off(3)
					
 					# Figure 10
 					jpeg(file = FL.Fig10,width=12,height=8,units="in",res=1200,bg = "transparent")
 					dev.set(3)	
 						plot(p.add1)
					dev.off(3)
					
      				}else if (this.year.alt == "2011" & this.variable == "U" & this.no.cls == 4)
      				{
      				
      					FL.Fig9a <- paste(Path.results,paste("Fig9A_Uaq.jpg",sep=""),sep="/")				# p.add3
					if (file.exists(FL.Fig9a)){print(paste(FL.Fig9a,"exist.Delete it!"));file.remove(FL.Fig9a)}
					

      					FL.Fig10 <- paste(Path.results,paste("Fig10_BEU_",this.variable,".jpg",sep=""),sep="/")		# p.add1
					if (file.exists(FL.Fig10)){print(paste(FL.Fig10,"exist.Delete it!"));file.remove(FL.Fig10)}
					

					# Figure 9A
					jpeg(file = FL.Fig9a,width=12,height=8,units="in",res=1200,bg = "transparent")
					dev.set(3)	
						plot(p.add3)
					dev.off(3)
					
					# Figure 10
					jpeg(file = FL.Fig10,width=12,height=8,units="in",res=1200,bg = "transparent")
					dev.set(3)	
						plot(p.add1)
					dev.off(3)					
				}
	      		}


			# # plot for each cluster
			# for (this.class in seq(1:no.cls))
			# {
			# 	myData.4ggplot.thisCls <- myData.4ggplot
			# 	myData.4ggplot.thisCls[,"this.class"] <- 0
			# 	myData.4ggplot.thisCls[myData.4ggplot.thisCls[,"class.label"] == this.class,"this.class"] <- 1
			# 	
			# 	myData.4ggplot.sub <- myData.4ggplot.thisCls[myData.4ggplot.thisCls[,"this.class"] == 1,]
			# 
			# 	p0  <- ggplot(data=myData.4ggplot.sub,aes(x=variable,y=value,group=WellID,color=as.factor(WellID))) 
			# 	p.tmp1 <- p0 + geom_line() + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep="")) +  scale_color_discrete(name="Well ID") 
			# 	
			# 	p0  <- ggplot(data=myData.4ggplot.sub,aes(x=WellID,y=value,fill=as.factor(WellID))) 
			# 	p.tmp2 <- p0 + geom_boxplot(notch=TRUE) + scale_fill_manual(name = "Well ID",values=col.array)				
			# 
			# 	p0  <- ggplot(data=myData.4ggplot.sub,aes(x=class.label,y=value,fill=as.factor(class.label))) 
			# 	p.tmp3 <- p0 + geom_boxplot(notch=TRUE) + scale_fill_manual(name = "Cluster",values=c("pink","cyan","green","magenta")[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep="")) + theme(legend.position="none")
			# 
			# 	p0  <- ggplot(data=myData.4ggplot.thisCls,aes(x=X,y=Y,label=paste("Well",WellID,sep=""),group=WellID,color=as.factor(this.class))) 
			# 	p.tmp4 <- p0 + geom_point(size=2,shape=4) + geom_text(size=3,vjust=-0.5) + scale_color_manual(name="WellID",values=c("grey","red")) + coord_equal() + theme(legend.position="none") + labs(x="Easting",y="Northing",title=paste("Location of the wells in the clusters of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep=""))
			# 
			# 
			# 	p0  <- ggplot(data=myData.4ggplot.thisCls,aes(x=variable,y=value,group=as.factor(WellID),color=as.factor(this.class))) 
			# 	p.tmp5 <- p0 + geom_line() + scale_color_manual(name="WellID",values=c("grey","red")) + theme(legend.position="none") + labs(x="Date",y=this.variable,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep=""))
			# 
			# 	# plot out
			# 	# multiplot(p.tmp1,p.tmp2,p.tmp4,p.tmp5,cols=2)
			# }
			# 
			# for all cluster time series
			# p0  <- ggplot(data=myData.4ggplot.thisCls,aes(x=variable,y=value,group=as.factor(WellID),color=as.factor(class.label))) 
			# p21 <- p0 + geom_line() + scale_color_manual(name="Cluster",values=col.array)  + labs(x="Date",y=this.variable,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep=""))
			# 
			# for all clsuter: boxplots
			# p0  <- ggplot(data=myData.4ggplot,aes(x=class.label,y=value,fill=as.factor(class.label))) 
			# p22 <- p0 + geom_boxplot(notch=TRUE) + scale_fill_manual(name = "Cluster",values=col.array[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep="")) + theme(legend.position="none")
			# 
			# # for all clusters: map
			# p0  <- ggplot(data=myData.4ggplot,aes(x=X,y=Y,label=paste("Well",WellID,sep=""),group=WellID,color=as.factor(class.label))) 
			# p23 <- p0 + geom_point(size=2,shape=4) + geom_text(size=3,vjust=-0.5) + scale_color_manual(name="Cluster",values=col.array) + coord_equal()  + labs(x="Easting",y="Northing",title=paste("Location of the wells in the clusters of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep=""))
			# 
			# 
			# for (this.class in seq(1:no.cls))
			# {
			# 	myData.4ggplot.sub <- myData.4ggplot.thisCls[myData.4ggplot.thisCls[,"class.label"] == this.class,]
			# 
			# 	p0  <- ggplot(data=myData.4ggplot.sub,aes(x=variable,y=value,group=WellID,color=as.factor(WellID))) 
			# 	p.tmp <- p0 + geom_line() + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep="")) +  scale_color_discrete(name="Well ID") 
 			# 
			# 	command.string <- paste(paste("p.tmp",this.class,sep="")," <- p.tmp",sep="")
			# 	eval(parse(text=command.string))
			# 
			# }
			
			
			# plot out
			# if(no.cls == 2)
			# {
			# 	multiplot(p21,p22,p23,p.tmp1,p.tmp2,cols=3)
			# }else if(no.cls == 3)
			# {
			# 	multiplot(p21,p22,p23,p.tmp1,p.tmp2,p.tmp3,cols=3)
			# }else if(no.cls == 4)
			# {
			# 	multiplot(p21,p22,p23,p.tmp1,p.tmp2,p.tmp3,p.tmp4,cols=3)
			# }else if(no.cls == 5)
			# {
			# 	multiplot(p21,p22,p23,p.tmp1,p.tmp2,p.tmp3,p.tmp4,p.tmp5,cols=3)
			# }
			


			# # -----------------------------------------------------
			# # write out intermediate data for verification purpose
			# # -----------------------------------------------------
			# FL.INT.CSV <- paste(Path.results,paste("intermediate_",this.year.string,"_",this.variable,"_",this.no.cls,"cls.csv",sep=""),sep="/")
			# if (file.exists(FL.INT.CSV)){print(paste(FL.INT.CSV, "exist.Delete it!"));file.remove(FL.INT.CSV)}
			# cat(paste("Aqueous U at (",this.year.string,"):",this.variable," with ",this.no.cls," classes in [myAqueousU.4ggplot],",sep=""),file=FL.INT.CSV,append=TRUE)
			# write.table(myAqueousU.4ggplot,file=FL.INT.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			# 
			# cat(paste("\n\nExtractible U at (",this.year.string,"):",this.variable," with ",this.no.cls," classes in [myExtracU.4ggplot],",sep=""),file=FL.INT.CSV,append=TRUE)
			# write.table(myExtracU.4ggplot,file=FL.INT.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
			# 
			# browser()
		}
	}					# end of Sp and U split
	dev.off()
}						# end of year separation



# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n10_report_ggplot_Hartigan_report.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n10_report_ggplot_Hartigan_report.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [10_report_ggplot_Hartigan_report.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [10_report_ggplot_Hartigan_report.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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




# 
# 
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


