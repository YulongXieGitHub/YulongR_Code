#
# 04_report_ggplot.R 
#
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
# library(latticeExtra)

# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

col.array <- c("red","blue","green","magenta","cyan","yellow","black","purple","pink","brown","cadetblue","chartreuse","chocolate","cornflowerblue","darkcyan","darkgoldenrod2","deepskyblue","darkolivegreen1","firebrick1")

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
}



source("multiplot.R")


# **********************************************************************************
# log file
# **********************************************************************************
Path.LOG <- paste(Path.Project,"0_log",sep="/")
if (!file.exists(Path.LOG)){print(paste("NOT existing:",Path.LOG));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}
FL.LOG <- paste(Path.LOG,"4_report_ggplot.log",sep="/")
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


# loop for the three sets of data
for (this.year.alt in c(2011,2010,20100))
{

	if (this.year.alt == 2010)
	{
		this.chron.cut.begin <- chron(dates  = "06/07/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		this.chron.cut.end   <- chron(dates  = "07/30/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		idx_for_abline <- c()
		files <- c("2-10_WLData_2010","UData_2010_AllWell.txt","SpCData_2010_AllWell.txt")
		this.year <- this.year.alt
		this.year.string <- this.year
	}
	else if (this.year.alt == 2011)
	{
		this.chron.cut.begin <- chron(dates  = "05/06/11",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		this.chron.cut.end   <- chron(dates  = "08/06/11",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		idx_for_abline <- c(40,45,51)
		files <- c("2-10_WLData_2011","UData_2011_AllWell.txt","SpCData_2011_AllWell.txt")
		this.year <- this.year.alt
		this.year.string <- this.year
	}
	else if (this.year.alt == 20100)
	{
		this.chron.cut.begin <- chron(dates  = "05/11/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		this.chron.cut.end   <- chron(dates  = "07/30/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		idx_for_abline <- c()
		files <- c("2-10_WLData_2010","UData_2010_AllWell.txt","SpCData_2010_AllWell.txt")
		this.year <- 2010
		this.year.string <- "2010B"
	}
	cat(paste("3: year ",this.year.alt,": specify path and file of input files.\n",sep=""))
	cat(paste("3: year ",this.year.alt,": specify path and file of input files.\n",sep=""),file=FL.LOG,append=TRUE)
	
	Path.results <- paste(Path.Project,"4_report_ggplot",this.year.string,sep="/")
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
	measSpre.vars <- names(myData.Merged.Sp)[!(names(myData.Merged.Sp) %in% c(id.vars,rm.vars))]
	myData.ggplot.wide.Sp <- myData.ggplot.wide.Sp[,c(id.vars,measSpre.vars)] 
	names(myData.ggplot.wide.Sp)[grep("-",names(myData.ggplot.wide.Sp))] <- paste("Well",grep("-",names(myData.ggplot.wide.Sp),value=TRUE),sep="")
	measSpre.vars <- paste("Well",measSpre.vars,sep="")
	
	myData.ggplot.long.Sp <- melt(myData.ggplot.wide.Sp,id.vars =id.vars,measSpre.vars = measSpre.vars,value.name="value")
	# myData.ggplot.long.Sp[,"Date"]  <- as.Date(myData.ggplot.long.Sp[,"Date"])			# m/d/yyy is not a SpnambigSpoSps format
	myData.ggplot.wide.Sp[,"Dates"] <- as.Date(myData.ggplot.wide.Sp[,"Dates"])			# yyy-mm-dd is an SpnambigSpoSps format
	myData.ggplot.long.Sp[,"Dates"] <- as.Date(myData.ggplot.long.Sp[,"Dates"])			# yyy-mm-dd is an SpnambigSpoSps format


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
	
	
	# plot out the plots
	print(p1)
	print(p2)
	print(p3)
	print(p4)
	print(p5)
	print(p6)
	print(p7)
	print(p8)

	# -------------------------------------------------------------------------------------------------
	# 11. prepare three sets of data for 2010 U data
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
	measSpre.vars <- names(myData.Merged.Sp)[!(names(myData.Merged.Sp) %in% c(id.vars,rm.vars))]
	myData.ggplot.wide.Sp <- myData.ggplot.wide.Sp[,c(id.vars,measSpre.vars)] 
	names(myData.ggplot.wide.Sp)[grep("-",names(myData.ggplot.wide.Sp))] <- paste("Well",grep("-",names(myData.ggplot.wide.Sp),value=TRUE),sep="")
	measSpre.vars <- paste("Well",measSpre.vars,sep="")
	
	myData.ggplot.long.Sp <- melt(myData.ggplot.wide.Sp,id.vars =id.vars,measSpre.vars = measSpre.vars,value.name="value")
	# myData.ggplot.long.Sp[,"Date"]  <- as.Date(myData.ggplot.long.Sp[,"Date"])			# m/d/yyy is not a SpnambigSpoSps format
	myData.ggplot.wide.Sp[,"Dates"] <- as.Date(myData.ggplot.wide.Sp[,"Dates"])			# yyy-mm-dd is an SpnambigSpoSps format
	myData.ggplot.long.Sp[,"Dates"] <- as.Date(myData.ggplot.long.Sp[,"Dates"])			# yyy-mm-dd is an SpnambigSpoSps format

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

	# plot out the plots
	print(p9)
	print(p10)
	print(p11)
	print(p12)
	print(p13)
	print(p14)
	print(p15)
	print(p16)









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




	#  
	myData.PCA_U1  <-   myData.U[,intersect(names(myData.U),  list.shallow.well)]
	myData.PCA_Sp1 <-   myData.Sp[,intersect(names(myData.Sp),list.shallow.well)]
	
	
	
	cat(paste("17. Use the wide format of U/Sp data for PCA for year ",this.year,".\n",sep=""))
	cat(paste("17. Use the wide format of U/Sp data for PCA for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# # Estimate the number of dimensions for the Principal Component Analysis by cross-validation
	# nb <- estim_ncpPCA(myData.PCA_U1,ncp.min=0,ncp.max=10) 
	# plot(nb$criterion)
	# cat(paste("18. use missMDA function to estimate number of PC for imputation for year ",this.year,".\n",sep=""))
	# cat(paste("18. use missMDA function to estimate number of PC for imputation for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		
	# 

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
		}else if (this.variable == "U")
		{
			myDATA <- t(myData.U.complete)	# which is a 90 by 26 data frame
			myDATA.MERGED <- myData.Merged.U
			cluser.array <- c(4,5)
		}
		
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
			model.kMeans  <- kmeans(myDATA,centers=no.cls)
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
			
			# time seris colored by cluster label
			p0  <- ggplot(data=myData.4ggplot,aes(x=variable,y=value,group=WellID,color=as.factor(class.label))) 
			p13 <- p0 + geom_line() + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep="")) +  scale_color_discrete(name="Cluster") 

			# box plot of wells
			p0  <- ggplot(data=myData.4ggplot,aes(x=WellID,y=value,group=WellID,fill=as.factor(class.label))) 
			p14 <- p0 + geom_boxplot(notch=TRUE) + scale_fill_manual(name = "Cluster",values=c("pink","cyan","green","magenta")[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep=""))
			
      # box plot of clusters
			p0  <- ggplot(data=myData.4ggplot,aes(x=class.label,y=value,group=class.label,fill=as.factor(class.label))) 
			p15 <- p0 + geom_boxplot(notch=TRUE) + scale_fill_manual(name = "Cluster",values=c("pink","cyan","green","magenta")[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep=""))

			p0  <- ggplot(data=myData.4ggplot,aes(x=X,y=Y,label=paste("Well",WellID,sep=""),group=WellID,color=as.factor(class.label))) 
			p16 <- p0 + geom_point(size=2,shape=4) + geom_text(size=3,vjust=-0.5) + scale_color_manual(name="Cluster",values=col.array) + coord_equal() + theme(legend.position="none") + labs(x="Easterning",y="Northening",title=paste("Location of the wells in the clusters of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep=""))
			
      multiplot(p13,p14,p15,p16,cols=2)
      
			# plot for each cluster
			for (this.class in seq(1:no.cls))
			{
				myData.4ggplot.thisCls <- myData.4ggplot
				myData.4ggplot.thisCls[,"this.class"] <- 0
				myData.4ggplot.thisCls[myData.4ggplot.thisCls[,"class.label"] == this.class,"this.class"] <- 1
				
				myData.4ggplot.sub <- myData.4ggplot.thisCls[myData.4ggplot.thisCls[,"this.class"] == 1,]

				p0  <- ggplot(data=myData.4ggplot.sub,aes(x=variable,y=value,group=WellID,color=as.factor(WellID))) 
				p.tmp1 <- p0 + geom_line() + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep="")) +  scale_color_discrete(name="Well ID") 
				
				p0  <- ggplot(data=myData.4ggplot.sub,aes(x=WellID,y=value,fill=as.factor(WellID))) 
				p.tmp2 <- p0 + geom_boxplot(notch=TRUE) + scale_fill_manual(name = "Well ID",values=col.array)				

				p0  <- ggplot(data=myData.4ggplot.sub,aes(x=class.label,y=value,fill=as.factor(class.label))) 
				p.tmp3 <- p0 + geom_boxplot(notch=TRUE) + scale_fill_manual(name = "Cluster",values=c("pink","cyan","green","magenta")[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep="")) + theme(legend.position="none")

				p0  <- ggplot(data=myData.4ggplot.thisCls,aes(x=X,y=Y,label=paste("Well",WellID,sep=""),group=WellID,color=as.factor(this.class))) 
				p.tmp4 <- p0 + geom_point(size=2,shape=4) + geom_text(size=3,vjust=-0.5) + scale_color_manual(name="WellID",values=c("grey","red")) + coord_equal() + theme(legend.position="none") + labs(x="Easterning",y="Northening",title=paste("Location of the wells in the clusters of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep=""))


				p0  <- ggplot(data=myData.4ggplot.thisCls,aes(x=variable,y=value,group=as.factor(WellID),color=as.factor(this.class))) 
				p.tmp5 <- p0 + geom_line() + scale_color_manual(name="WellID",values=c("grey","red")) + theme(legend.position="none") + labs(x="Date",y=this.variable,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep=""))

				# plot out
				multiplot(p.tmp1,p.tmp2,p.tmp4,p.tmp5,cols=2)
			}
			
			# for all cluster time series
			p0  <- ggplot(data=myData.4ggplot.thisCls,aes(x=variable,y=value,group=as.factor(WellID),color=as.factor(class.label))) 
			p21 <- p0 + geom_line() + scale_color_manual(name="Cluster",values=col.array)  + labs(x="Date",y=this.variable,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep=""))

			# for all clsuter: boxplots
			p0  <- ggplot(data=myData.4ggplot,aes(x=class.label,y=value,fill=as.factor(class.label))) 
			p22 <- p0 + geom_boxplot(notch=TRUE) + scale_fill_manual(name = "Cluster",values=col.array[1:no.cls],labels=paste("Cluster",seq(1:no.cls),sep="")) + theme(legend.position="none")

			# for all clusters: map
			p0  <- ggplot(data=myData.4ggplot,aes(x=X,y=Y,label=paste("Well",WellID,sep=""),group=WellID,color=as.factor(class.label))) 
			p23 <- p0 + geom_point(size=2,shape=4) + geom_text(size=3,vjust=-0.5) + scale_color_manual(name="Cluster",values=col.array) + coord_equal()  + labs(x="Easterning",y="Northening",title=paste("Location of the wells in the clusters of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep=""))

			
			for (this.class in seq(1:no.cls))
			{
				myData.4ggplot.sub <- myData.4ggplot.thisCls[myData.4ggplot.thisCls[,"class.label"] == this.class,]

				p0  <- ggplot(data=myData.4ggplot.sub,aes(x=variable,y=value,group=WellID,color=as.factor(WellID))) 
				p.tmp <- p0 + geom_line() + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=this.variable,title=paste("Time Series of ",this.variable," at Year ", this.year,": ",no.cls," clusters of Kmeans",sep="")) +  scale_color_discrete(name="Well ID") 
 
				command.string <- paste(paste("p.tmp",this.class,sep="")," <- p.tmp",sep="")
				eval(parse(text=command.string))
	
			}
			
			
			# plot out
			if(no.cls == 2)
			{
				multiplot(p21,p22,p23,p.tmp1,p.tmp2,cols=3)
			}else if(no.cls == 3)
			{
				multiplot(p21,p22,p23,p.tmp1,p.tmp2,p.tmp3,cols=3)
			}else if(no.cls == 4)
			{
				multiplot(p21,p22,p23,p.tmp1,p.tmp2,p.tmp3,p.tmp4,cols=3)
			}else if(no.cls == 5)
			{
				multiplot(p21,p22,p23,p.tmp1,p.tmp2,p.tmp3,p.tmp4,p.tmp5,cols=3)
			}
		}
	}					# end of Sp and U split
	dev.off()
}						# end of year separation



### # ----------------------------------------------------
### # Kmean using base function in the stats package
### # ----------------------------------------------------
### Kmeans.results.3cls <- kmeans(myData.U.complete, 3)
### fitted.3cls <- fitted(Kmeans.results.3cls)
### resid.3cls <- myData.U.complete - fitted.3cls
### cat(paste("18. use kmeans in the [stats] package.\n",sep=""))
### cat(paste("18. use kmeans in the [stats] package.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### # Kmean using "cluster" package (hsaur, CLSUET
### km    <- kmeans(myData.U.complete,3)
### dissE <- daisy(myData.U.complete) 
### dE2   <- dissE^2
### sk2   <- silhouette(km$cl, dE2)
### plot(sk2)
### cat(paste("19. use kmeans in the [HSAUR] and [cluster] packages.\n",sep=""))
### cat(paste("19. use kmeans in the [HSAUR] and [cluster] packages.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### # plot Kmeans IN [cluster] and [fpc]
### # Kmeans clustre analysis
### clus <- kmeans(myData.U.complete, centers=3)
### plotcluster(myData.U.complete[,c(1,2)], clus$cluster)
### 
### clusplot(myData.U.complete[,c(1,2)], clus$cluster, color=TRUE, shade=TRUE, 
###          labels=2, lines=0)
###          
### with(myData.U.complete, pairs(myData.U.complete, col=c(1:3)[clus$cluster]))          
### 
### plot(myData.U.complete[,, col = Kmeans.results.3cls$cluster)
### points(cl$centers, col = 1:2, pch = 8, cex = 2)
### cat(paste("12. use kmeans in the [fpc] and [cluster] packages.\n",sep=""))
### cat(paste("12. use kmeans in the [fpc] and [cluster] packages.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### # PCA
### PCA.results <- princomp(myData.U.complete)
### biplot(PCA.results)
### cat(paste("20a. PCA biplots.\n",sep=""))
### cat(paste("20a. PCA biplots.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### 
### # loading plot
### par(mfrow=c(1,2))
### Loadings <- PCA.results$loadings
### plot(Loadings[,1],Loadings[,2],col="red",type="p",pch=16,xlab="PC1",ylab="PC2",main=paste("Year ",this.year,": wells in the PC space PC1-PC2",sep=""))
### text(Loadings[,1],Loadings[,2],row.names(Loadings),cex=0.75,col="black")
### 
### plot(Loadings[,1],Loadings[,3],col="blue",type="p",pch=16,xlab="PC1",ylab="PC3",main=paste("Year ",this.year,": wells in the PC space PC1-PC3",sep=""))
### text(Loadings[,1],Loadings[,3],row.names(Loadings),cex=0.75,col="black")
### cat(paste("21b. PCA loading plots.\n",sep=""))
### cat(paste("21b. PCA loading plots.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### #
### # use MCluster
### #
### myData.U.transport <- t(myData.U.complete)
### # use Mcluster
### cluster.U2 <- Mclust(myData.U.transport)
### no.cls     <- length(table(cluster.U2$classification))
### idx.class  <- cluster.U2$classification
### cat(paste("22A. Mcluster on the transported data.\n",sep=""))
### cat(paste("22A. Mcluster on the transported data.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### par(mfrow=c(3,no.cls/3))
### for (this.class in seq(1:no.cls))
### {
### 	idx.this.cls     <- names(idx.class)[idx.class==this.class]
### 	Value.this.cls       <- myData.Merged.U[,idx.this.cls]
### 	Value.range.this.cls <- range(Value.this.cls,na.rm=TRUE)
### 	
### 	
### 	plot(myData.Merged.U[,idx.this.cls[1]] ~ myData.Merged.U[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,Value.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",paste(idx.this.cls,collapse=","),sep=""))
### 	if (length(idx.this.cls) > 1)
### 	{
### 		for (count in seq(from=2,to=length(idx.this.cls)))
### 		{
### 			lines(myData.Merged.U[,idx.this.cls[count]] ~ myData.Merged.U[,"date.chron"],pch=16,col=col.array[count])
### 		}
### 	}
### 	
### }
### cat(paste("22B. overlay time series of the well in the classes.\n",sep=""))
### cat(paste("22B. overlay time series of the well in the classes.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### # use 3 classes Kmean
### kmeans.U2  <- kmeans(myData.U.transport,centers=3)
### idx.class  <- kmeans.U2$cluster
### no.cls     <- max(idx.class)
### cat(paste("23A. Kmean for 3 clusters.\n",sep=""))
### cat(paste("23A. Kmean for 3 clusters.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### par(mfrow=c(1,3))
### for (this.class in seq(1:no.cls))
### {
### 	idx.this.cls     <- names(idx.class)[idx.class==this.class]
### 	Value.this.cls       <- myData.Merged.U[,idx.this.cls]
### 	Value.range.this.cls <- range(Value.this.cls,na.rm=TRUE)
### 	
### 	
### 	plot(myData.Merged.U[,idx.this.cls[1]] ~ myData.Merged.U[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,Value.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",paste(idx.this.cls,collapse=","),sep=""))
### 	if (length(idx.this.cls) > 1)
### 	{
### 		for (count in seq(from=2,to=length(idx.this.cls)))
### 		{
### 			lines(myData.Merged.U[,idx.this.cls[count]] ~ myData.Merged.U[,"date.chron"],pch=16,col=col.array[count])
### 		}
### 	}
### 	
### }
### cat(paste("23B. overlay time series of the well in the classes of 3 cls kmean.\n",sep=""))
### cat(paste("23B. overlay time series of the well in the classes of 3 cls kmean.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### # use 4 classes Kmean
### kmeans.U2  <- kmeans(myData.U.transport,centers=4)
### idx.class  <- kmeans.U2$cluster
### no.cls     <- max(idx.class)
### cat(paste("24A. Kmean for 4 clusters.\n",sep=""))
### cat(paste("24A. Kmean for 4 clusters.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### par(mfrow=c(2,2))
### for (this.class in seq(1:no.cls))
### {
### 	idx.this.cls     <- names(idx.class)[idx.class==this.class]
### 	Value.this.cls       <- myData.Merged.U[,idx.this.cls]
### 	Value.range.this.cls <- range(Value.this.cls,na.rm=TRUE)
### 	
### 	
### 	plot(myData.Merged.U[,idx.this.cls[1]] ~ myData.Merged.U[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,Value.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",paste(idx.this.cls,collapse=","),sep=""))
### 	if (length(idx.this.cls) > 1)
### 	{
### 		for (count in seq(from=2,to=length(idx.this.cls)))
### 		{
### 			lines(myData.Merged.U[,idx.this.cls[count]] ~ myData.Merged.U[,"date.chron"],pch=16,col=col.array[count])
### 		}
### 	}
### 	
### }
### cat(paste("24B. overlay time series of the well in the classes of 4 cls kmean.\n",sep=""))
### cat(paste("24B. overlay time series of the well in the classes of 4 cls kmean.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### 
### 
### # use 5 classes Kmean
### kmeans.U2  <- kmeans(myData.U.transport,centers=5)
### idx.class  <- kmeans.U2$cluster
### no.cls     <- max(idx.class)
### cat(paste("25A. Kmean for 5 clusters.\n",sep=""))
### cat(paste("25A. Kmean for 5 clusters.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### par(mfrow=c(2,3))
### for (this.class in seq(1:no.cls))
### {
### 	idx.this.cls     <- names(idx.class)[idx.class==this.class]
### 	Value.this.cls       <- myData.Merged.U[,idx.this.cls]
### 	Value.range.this.cls <- range(Value.this.cls,na.rm=TRUE)
### 	
### 	
### 	plot(myData.Merged.U[,idx.this.cls[1]] ~ myData.Merged.U[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,Value.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",paste(idx.this.cls,collapse=","),sep=""))
### 	if (length(idx.this.cls) > 1)
### 	{
### 		for (count in seq(from=2,to=length(idx.this.cls)))
### 		{
### 			lines(myData.Merged.U[,idx.this.cls[count]] ~ myData.Merged.U[,"date.chron"],pch=16,col=col.array[count])
### 		}
### 	}
### 	
### }
### cat(paste("25B. overlay time series of the well in the classes of 5 cls kmean.\n",sep=""))
### cat(paste("25B. overlay time series of the well in the classes of 5 cls kmean.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### 
### 
### # use 6 classes Kmean
### kmeans.U2  <- kmeans(myData.U.transport,centers=6)
### idx.class  <- kmeans.U2$cluster
### no.cls     <- max(idx.class)
### cat(paste("26A. Kmean for 6 clusters.\n",sep=""))
### cat(paste("26A. Kmean for 6 clusters.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### par(mfrow=c(2,3))
### for (this.class in seq(1:no.cls))
### {
### 	idx.this.cls     <- names(idx.class)[idx.class==this.class]
### 	Value.this.cls       <- myData.Merged.U[,idx.this.cls]
### 	Value.range.this.cls <- range(Value.this.cls,na.rm=TRUE)
### 	
### 	
### 	plot(myData.Merged.U[,idx.this.cls[1]] ~ myData.Merged.U[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,Value.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",paste(idx.this.cls,collapse=","),sep=""))
### 	if (length(idx.this.cls) > 1)
### 	{
### 		for (count in seq(from=2,to=length(idx.this.cls)))
### 		{
### 			lines(myData.Merged.U[,idx.this.cls[count]] ~ myData.Merged.U[,"date.chron"],pch=16,col=col.array[count])
### 		}
### 	}
### 	
### }
### cat(paste("26B. overlay time series of the well in the classes of 6 cls kmean.\n",sep=""))
### cat(paste("26B. overlay time series of the well in the classes of 6 cls kmean.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### #
### # loess plot
### #
### for (this.degree in c(1,2))
### {
### 	for (this.family in c("gaussian","symmetric"))
### 	{
### 		for (this.span in seq(from=0.05,to=0.9,by=0.05))
### 		{
### 			par(mfrow=c(5,6))
### 			for (this.well in intersect(names(myData.Merged.U),list.shallow.well))
### 			{
### 				myData.thisWell <- myData.Merged.U[,c(this.well,"date.chron")]
### 				plot(myData.thisWell[,this.well] ~ myData.thisWell[,"date.chron"],pch=16,col="red",xlab="date",ylab="U",main = paste("Year ",this.year,": ",this.well,": span=(",this.span,") family=(",this.family,") degree = (",this.degree,")",sep=""))
### 				loess.fit <- loess(myData.thisWell[,this.well] ~ myData.thisWell[,"date.chron"], data =myData.thisWell,span=this.span,family = this.family,degree=this.degree)
### 				lines(myData.thisWell[,"date.chron"],predict(loess.fit),col="blue")
### 
### 				#
### 				myData.loess <- cbind(myData.thisWell[!(is.na(myData.thisWell[,this.well])),c("date.chron",this.well)],predict(loess.fit))
### 			}
### 		}
### 	}
### }
### cat(paste("27. loess fitting of U time series.\n",sep=""))
### cat(paste("27. loess fitting of U time series.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### #
### # OK use span = 0.15, degree=2, family = gaussian
### #
### this.degree <- 2
### this.family <- "gaussian"
### this.span <- 0.15
### 
### par(mfrow=c(5,6))
### count <- 0
### for (this.well in intersect(names(myData.Merged.U),list.shallow.well)[!(intersect(names(myData.Merged.U),list.shallow.well) %in% c("2-34","2-37","3-35"))])
### {
### 	count <- count + 1
### 	myData.thisWell <- myData.Merged.U[,c(this.well,"date.chron")]
### 	plot(myData.thisWell[,this.well] ~ myData.thisWell[,"date.chron"],pch=16,col="red",xlab="date",ylab="U",main = paste("Year ",this.year,": well ",this.well,sep=""))
### 	loess.fit <- loess(myData.thisWell[,this.well] ~ myData.thisWell[,"date.chron"], data =myData.thisWell,span=this.span,family = this.family,degree=this.degree)
### 	lines(myData.thisWell[,"date.chron"],predict(loess.fit),col="blue")
### 	abline(v=this.chron.cut.begin,lty=1,col="blue")
### 
### 	#
### 	myData.loess <- cbind(myData.thisWell[!(is.na(myData.thisWell[,this.well])),c("date.chron",this.well)],predict(loess.fit))
### 	names(myData.loess) <- sub("predict\\(loess\\.fit\\)",paste(this.well,".loess",sep=""),names(myData.loess))
### 
### 	
### 	if (count == 1)
### 	{
### 		myData.LOESS <- myData.loess
### 	}else{
### 		myData.LOESS <- merge(myData.LOESS,myData.loess)
### 	}
### }
### cat(paste("28. loess fitting of U time series based on a fixed set of parameters.\n",sep=""))
### cat(paste("28. loess fitting of U time series based on a fixed set of parameters.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### # output the LOESS data for other usage
### cat(paste("LOESS complete data for analysis,",sep=""),file=FL.LOESS,append=TRUE)
### write.table(myData.LOESS,file=FL.LOESS,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
### cat(paste("29. output the LOES data.\n",sep=""))
### cat(paste("29. output the LOES data.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### #
### # save the file for other usage
### save(myData.U.complete,myData.LOESS,file=FL.OBJ)
### cat(paste("30. save the complete data for other usage.\n",sep=""))
### cat(paste("30. save the complete data for other usage.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### 
### 
### 
### # ====================================================================================================
### # use the loess predict to do clustering
### # ====================================================================================================
### row.names(myData.LOESS) <- myData.LOESS[,"date.chron"]
### myData.LOESS.complete   <- myData.LOESS[myData.LOESS[,"date.chron"] > this.chron.cut.begin,grep("\\.loess",names(myData.LOESS),value=TRUE)]
### myData.LOESS.transport  <- t(myData.LOESS.complete)
### cat(paste("31. use loess fitted data for cluster abalysis.\n",sep=""))
### cat(paste("31. use loess fitted data for cluster abalysis.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### #
### # use MCluster
### #
### myData.LOESS.transport <- t(myData.LOESS.complete)
### # use Mcluster
### cluster.U2 <- Mclust(myData.LOESS.transport)
### no.cls     <- length(table(cluster.U2$classification))
### idx.class  <- cluster.U2$classification
### cat(paste("32A. UCluster.\n",sep=""))
### cat(paste("32A. UCluster.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### par(mfrow=c(3,no.cls/3))
### for (this.class in seq(1:no.cls))
### {
### 	idx.this.cls     <- names(idx.class)[idx.class==this.class]
### 	Value.this.cls       <- myData.LOESS.complete[,idx.this.cls]
### 	Value.range.this.cls <- range(Value.this.cls,na.rm=TRUE)
### 	
### 	
### 	plot(myData.LOESS[,idx.this.cls[1]] ~ myData.LOESS[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,Value.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",paste(idx.this.cls,collapse=",")),sep=""))
### 	if (length(idx.this.cls) > 1)
### 	{
### 		for (count in seq(from=2,to=length(idx.this.cls)))
### 		{
### 			lines(myData.LOESS[,idx.this.cls[count]] ~ myData.LOESS[,"date.chron"],pch=16,col=col.array[count])
### 		}
### 	}
### 	
### }
### cat(paste("32B. overlay time series of the well in the classes from UClsuter.\n",sep=""))
### cat(paste("32B. overlay time series of the well in the classes from UClsuter.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### # use 3 classes Kmean
### kmeans.U2  <- kmeans(myData.LOESS.transport,centers=3)
### idx.class  <- kmeans.U2$cluster
### no.cls     <- max(idx.class)
### cat(paste("33A. Kmean for 3 clusters.\n",sep=""))
### cat(paste("33A. Kmean for 3 clusters.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### par(mfrow=c(1,3))
### for (this.class in seq(1:no.cls))
### {
### 	idx.this.cls     <- names(idx.class)[idx.class==this.class]
### 	Value.this.cls       <- myData.LOESS.complete[,idx.this.cls]
### 	Value.range.this.cls <- range(Value.this.cls,na.rm=TRUE)
### 	
### 	
### 	plot(myData.LOESS[,idx.this.cls[1]] ~ myData.LOESS[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,Value.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",paste(idx.this.cls,collapse=",")),sep=""))
### 	if (length(idx.this.cls) > 1)
### 	{
### 		for (count in seq(from=2,to=length(idx.this.cls)))
### 		{
### 			lines(myData.LOESS[,idx.this.cls[count]] ~ myData.LOESS[,"date.chron"],pch=16,col=col.array[count])
### 		}
### 	}
### 	
### }
### cat(paste("33B. overlay time series of the well in the classes of 3 cls kmean.\n",sep=""))
### cat(paste("33B. overlay time series of the well in the classes of 3 cls kmean.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### # use 4 classes Kmean
### kmeans.U2  <- kmeans(myData.LOESS.transport,centers=4)
### idx.class  <- kmeans.U2$cluster
### no.cls     <- max(idx.class)
### cat(paste("34A. Kmean for 4 clusters.\n",sep=""))
### cat(paste("34A. Kmean for 4 clusters.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### par(mfrow=c(2,2))
### for (this.class in seq(1:no.cls))
### {
### 	idx.this.cls     <- names(idx.class)[idx.class==this.class]
### 	Value.this.cls       <- myData.LOESS.complete[,idx.this.cls]
### 	Value.range.this.cls <- range(Value.this.cls,na.rm=TRUE)
### 	
### 	
### 	plot(myData.LOESS[,idx.this.cls[1]] ~ myData.LOESS[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,Value.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",gsub("\\.loess","",paste(idx.this.cls,collapse=","))),sep=""))
### 	if (length(idx.this.cls) > 1)
### 	{
### 		for (count in seq(from=2,to=length(idx.this.cls)))
### 		{
### 			lines(myData.LOESS[,idx.this.cls[count]] ~ myData.LOESS[,"date.chron"],pch=16,col=col.array[count])
### 		}
### 	}
### 	
### }
### cat(paste("34B. overlay time series of the well in the classes of 4 cls kmean.\n",sep=""))
### cat(paste("34B. overlay time series of the well in the classes of 4 cls kmean.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### # use 5 classes Kmean
### kmeans.U2  <- kmeans(myData.LOESS.transport,centers=5)
### idx.class  <- kmeans.U2$cluster
### no.cls     <- max(idx.class)
### cat(paste("35A. Kmean for 5 clusters.\n",sep=""))
### cat(paste("35A. Kmean for 5 clusters.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### par(mfrow=c(2,3))
### for (this.class in seq(1:no.cls))
### {
### 	idx.this.cls     <- names(idx.class)[idx.class==this.class]
### 	Value.this.cls       <- myData.LOESS.complete[,idx.this.cls]
### 	Value.range.this.cls <- range(Value.this.cls,na.rm=TRUE)
### 	
### 	
### 	plot(myData.LOESS[,idx.this.cls[1]] ~ myData.LOESS[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,Value.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",gsub("\\.loess","",paste(idx.this.cls,collapse=","))),sep=""))
### 	if (length(idx.this.cls) > 1)
### 	{
### 		for (count in seq(from=2,to=length(idx.this.cls)))
### 		{
### 			lines(myData.LOESS[,idx.this.cls[count]] ~ myData.LOESS[,"date.chron"],pch=16,col=col.array[count])
### 		}
### 	}
### 	
### }
### cat(paste("35B. overlay time series of the well in the classes of 5 cls kmean.\n",sep=""))
### cat(paste("35B. overlay time series of the well in the classes of 5 cls kmean.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### 
### # use 6 classes Kmean
### kmeans.U2  <- kmeans(myData.LOESS.transport,centers=6)
### idx.class  <- kmeans.U2$cluster
### no.cls     <- max(idx.class)
### cat(paste("36A. Kmean for 6 clusters.\n",sep=""))
### cat(paste("36A. Kmean for 6 clusters.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### par(mfrow=c(2,3))
### for (this.class in seq(1:no.cls))
### {
### 	idx.this.cls     <- names(idx.class)[idx.class==this.class]
### 	Value.this.cls       <- myData.LOESS.complete[,idx.this.cls]
### 	Value.range.this.cls <- range(Value.this.cls,na.rm=TRUE)
### 	
### 	
### 	plot(myData.LOESS[,idx.this.cls[1]] ~ myData.LOESS[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,Value.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",paste(idx.this.cls,collapse=",")),sep=""))
### 	if (length(idx.this.cls) > 1)
### 	{
### 		for (count in seq(from=2,to=length(idx.this.cls)))
### 		{
### 			lines(myData.LOESS[,idx.this.cls[count]] ~ myData.LOESS[,"date.chron"],pch=16,col=col.array[count])
### 		}
### 	}
### 	
### }
### cat(paste("36B. overlay time series of the well in the classes of 6 cls kmean.\n",sep=""))
### cat(paste("36B. overlay time series of the well in the classes of 6 cls kmean.\n",sep=""),file=FL.LOG,append=TRUE)		
### 





# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n04_report_ggplot.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n04_report_ggplot.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [04_report_ggplot.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [04_report_ggplot.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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




### # Sp and WL data
### long.Sp  <- myData.Merged.U[,c("date.chron","WellName","Sp")]
### wide.Sp1 <- reshape(long.Sp,idvar=c("date.chron"),timevar = "WellName",direction="wide",v.name="Sp")
### wide.Sp2 <- reshape(long.Sp,idvar=c("WellName"),timevar = "date.chron",direction="wide",v.name="Sp")
### sum(!(is.na(wide.Sp1[,-1])))
### sum(!(is.na(wide.Sp2[,-1])))
### 
### long.WL <- myData.Merged.U[,c("date.chron","WellName","WL")]
### wide.WL1 <- reshape(long.WL,idvar=c("date.chron"),timevar = "WellName",direction="wide",v.name="WL")
### wide.WL2 <- reshape(long.WL,idvar=c("WellName"),timevar = "date.chron",direction="wide",v.name="WL")
### sum(!(is.na(wide.WL1[,-1])))
### sum(!(is.na(wide.WL2[,-1])))
### 
### 
### 
### 
### 
### # use latticeExtra function to overlay plots
### plot1 <- doubleYScale(plot.U, plot.Sp,add.ylab2 = TRUE)
### plot2 <- doubleYScale(plot.U, plot.WL,add.ylab2 = TRUE)
### plot(plot1)
### plot(plot2)
### cat(paste("14d. overlay time series of U and WL & U and Sp.\n",sep=""))
### cat(paste("14d. overlay time series of U and WL & U and Sp.\n",sep=""),file=FL.LOG,append=TRUE)		
### 
### 
### 
### # ----------------------------------------------------------------------------------------
### # In order to keep the number of NA, use [reshape] instead of [acast]
### # ----------------------------------------------------------------------------------------
### long.U <- myData.Merged.U[,c("date.chron","WellName","U")]
### wide.U1 <- reshape(long.U,idvar=c("date.chron"),timevar = "WellName",direction="wide",v.name="U")
### wide.U2 <- reshape(long.U,idvar=c("WellName"),timevar = "date.chron",direction="wide",v.name="U")
### write.csv(long.U, file="long_U.csv",sep=",",row.names=FALSE)
### write.csv(wide.U1,file="wide_U1.csv",sep=",",row.names=FALSE)
### write.csv(wide.U2,file="wide_U2.csv",sep=",",row.names=FALSE)
### 
### sum((is.na(wide.U1[,-1])))
### sum((is.na(wide.U2[,-1])))
### 
### long.Sp  <- myData.Merged.U[,c("date.chron","WellName","Sp")]
### wide.Sp1 <- reshape(long.Sp,idvar=c("date.chron"),timevar = "WellName",direction="wide",v.name="Sp")
### wide.Sp2 <- reshape(long.Sp,idvar=c("WellName"),timevar = "date.chron",direction="wide",v.name="Sp")
### sum(!(is.na(wide.Sp1[,-1])))
### sum(!(is.na(wide.Sp2[,-1])))
### 
### long.WL <- myData.Merged.U[,c("date.chron","WellName","WL")]
### wide.WL1 <- reshape(long.WL,idvar=c("date.chron"),timevar = "WellName",direction="wide",v.name="WL")
### wide.WL2 <- reshape(long.WL,idvar=c("WellName"),timevar = "date.chron",direction="wide",v.name="WL")
### sum(!(is.na(wide.WL1[,-1])))
### sum(!(is.na(wide.WL2[,-1])))
### 
### 
### # ----------------------------------------------------------------------------------------
### # The number of NA changed by using acast!!
### # ----------------------------------------------------------------------------------------
### # get wide format data
### # turn the data into a wide format
### myData.Wide.U  <- as.data.frame(acast(myData.Merged.U,date.chron~WellName,sum,value.var="U")) 	
### myData.Wide.Sp <- as.data.frame(acast(myData.Merged.U,date.chron~WellName,sum,value.var="Sp")) 	
### myData.Wide.WL <- as.data.frame(acast(myData.Merged.U,date.chron~WellName,sum,value.var="WL")) 
### 
### myData.Wide.U[,"date-time"]  <- row.names(myData.Wide.U)
### myData.Wide.Sp[,"date-time"] <- row.names(myData.Wide.Sp)
### myData.Wide.WL[,"date-time"] <- row.names(myData.Wide.WL)
### 
### myData.Wide.U[,"date"]       <- sub("\\(","",sub("(.*)\\s+(.*)","\\1",myData.Wide.U[,"date-time"]))
### myData.Wide.U[,"time"]       <- sub("\\)","",sub("(.*)\\s+(.*)","\\2",myData.Wide.U[,"date-time"]))
### myData.Wide.U[,"chron.date"] <-  chron(dates  = myData.Wide.U[,"date"],
### 		                       times  = myData.Wide.U[,"time"],
### 		                       format = c('m/d/y','h:m:s'))
### 		                       
### plot(myData.Wide.U[,"chron.date"] ~ myData.Wide.U[,"2-11"],type="l",lty=1,col="red")		                       
### 
### 
### cat(paste("\n\nU,",sep=""),file=FL.Merged.U.WL,append=TRUE)
### write.table(myData.Wide.U,file=FL.Merged.U.WL,row.names=TRUE,col.names=TRUE,append=TRUE)
### 
### cat(paste("\n\nU,",sep=""),file=FL.Merged.U.WL,append=TRUE)
### write.table(myData.Wide.Sp,file=FL.Merged.U.WL,row.names=TRUE,col.names=TRUE,append=TRUE)
### 
### cat(paste("\n\nU,",sep=""),file=FL.Merged.U.WL,append=TRUE)
### write.table(myData.Wide.WL,file=FL.Merged.U.WL,row.names=TRUE,col.names=TRUE,append=TRUE)
### 
### 
### 
### # http://stackoverflow.com/questions/23804752/adding-several-loess-lines-to-each-panel-in-lattice-plot
### # my.col1<- c("white", "darkgray", "black", "lightgray",  "ivory2")
### # my.col2<- c("white", "darkgray", "black", "lightgray",  "ivory2")
### # xyplot(U~date.chron | WellName,
### #        data=myData.Merged.U,
### #        layout=c(8,4), 
### #        # index.cond=list(c(1,2,3)),
### #        par.settings = list(
### #        # superpose.polygon = list(col=c(my.col1, my.col2))), 
### #        # superpose.line=list(col=c(my.col1, my.col2)),
### #         ylab="U", xlab="Date",
### #         scales=list(x=list(rot=45, alternating=1),y=list(relation="free")),
### #     auto.key=list(space="top", columns=1, cex=.8,between.columns = 1,font=3,
### #         rectangles=FALSE, points=TRUE),
### #     panel = panel.superpose,
### #     panel.groups = function(x,y,...) {
### #         panel.xyplot(x, y, ...)
### #         panel.loess(x, y, ...)
### #     }
### # )
### 
### 
### 
### 
### # plot.U.loess  <- xyplot(myData.Merged.U[,"U"]  ~ myData.Merged.U[,"date.chron"] | WellName,data=myData.Merged.U,type="l",
### #                         lty=1,col="green",main = "U", xlab="date",ylab="U (ug/l)", 
### #                         scales = list(x = list(rot = 45),y=list(relation="free")),
### # 			panel.loess(x, y, span = 2/3, degree = 1,
### # 				    family = c("symmetric", "gaussian"),
### # 				    evaluation = 50,
### # 				    lwd, lty, col, col.line, type,
### # 				    horizontal = FALSE,
### # 				    ..., identifier = "loess")                        
### #                         )
### # 
### # 
### # 
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




	p1 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() 
	p2 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + scale_x_discrete(limits=c("2011-03-01","2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01","2011-09-01"))
	p3 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + scale_x_discrete(limits=c("2011-03-01","2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01","2011-09-01"),labels=c("03-01","04-01","05-01","06-01","07-01","08-01","09-01"))
	p4 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + theme(axis.ticks = element_blank(),axis.text.x=element_blank())
	p5 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + theme(axis.ticks = element_blank(),axis.text.x=element_blank()) + scale_y_continuous(limits=c(0,300))
	p6 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + theme(axis.ticks = element_blank(),axis.text.x=element_blank()) + coord_cartesian(ylim=c(0,300))
	p7 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line()
	p8 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + geom_line(aes(x=Dates,y=WL,color="black"),data=myData.ggplot.long.U)
	
	this.variable <- "U"
	p9  <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + geom_line(aes(x=Dates,y=WL,color="black"),data=myData.ggplot.long.U) + ggtitle(paste("Time Series of ",this.variable,sep=""))
	p10 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + geom_line(aes(x=Dates,y=WL,color="black"),data=myData.ggplot.long.U) + ggtitle(paste("Time Series of ",this.variable,sep="")) + theme(plot.title=element_text(size=20,face="bold",vjust=2))
	p11 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + geom_line(aes(x=Dates,y=WL,color="black"),data=myData.ggplot.long.U) + ggtitle(paste("Time Series of ",this.variable,sep="")) + theme(plot.title=element_text(size=20,face="bold",vjust=2)) + labs(x="Date",y=expression(paste("U",sep="")),title="Time Series")
	p12 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + geom_line(aes(x=Dates,y=WL,color="black"),data=myData.ggplot.long.U) + ggtitle(paste("Time Series of ",this.variable,sep="")) + theme(axis.ticks.y = element_blank(),axis.text.y=element_blank())	
	p13 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + geom_line(aes(x=Dates,y=WL,color="black"),data=myData.ggplot.long.U) + ggtitle(paste("Time Series of ",this.variable,sep="")) + theme(axis.text.x = element_text(angle=90,size=5,vjust=0.5,color="magenta"),axis.title.x = element_text(color="cyan",size=20,vjust=2))	
	p14 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + geom_line(aes(x=Dates,y=WL,color="black"),data=myData.ggplot.long.U) + ggtitle(paste("Time Series of ",this.variable,sep="")) + theme(axis.text.x = element_text(angle=90,size=5,vjust=0.5,color="magenta"),axis.title.x = element_text(color="cyan",size=20,vjust=2)) + coord_equal()	
	p15 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(Month)))    + geom_line() + geom_line(aes(x=Dates,y=WL,color="black"),data=myData.ggplot.long.U) + ggtitle(paste("Time Series of ",this.variable,sep="")) + theme(axis.text.x = element_text(angle=90,size=5,vjust=0.5,color="magenta"),axis.title.x = element_text(color="cyan",size=20,vjust=2)) 
	p16 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(Month)))    + geom_line() + geom_line(aes(x=Dates,y=WL,color="black"),data=myData.ggplot.long.U) + ggtitle(paste("Time Series of ",this.variable,sep="")) + theme(axis.text.x = element_text(angle=90,size=5,vjust=0.5,color="magenta"),axis.title.x = element_text(color="cyan",size=20,vjust=2)) + theme(legend.title=element_text(colour="blue",size=16,face="bold")) + scale_color_discrete(name="Month")
	p17 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(Month)))    + geom_line(aes(color="Important Line")) + geom_point(aes(color="My Points"))
	p18 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(Month)))    + geom_line(aes(color="Important Line")) + geom_point(aes(color="My Points")) + scale_colour_manual(name="Legend",values=c("Important Line"="grey","My Points"="red"))
	p19 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(Month)))    + geom_line(aes(color="Important Line")) + geom_point(aes(color="My Points")) + scale_colour_manual(name="Legend",values=c("Important Line"="grey","My Points"="red")) + guides(color=guide_legend(override.aes=list(linetype=c(1,0),shape=c(NA,16))))
	p20 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,color="red")) + geom_line() + facet_wrap(~variable,scales="free") 
	p21 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,color="red")) + geom_line() + facet_wrap(~variable) +  ggtitle(paste("Time Series of ",this.variable,sep=""))
	p22 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,color="red")) + geom_line() + facet_wrap(Month~variable,scales="free") 
	p23 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,color="red")) + geom_line() + facet_wrap(Month~Year,scales="free") 
	p24 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,color="red")) + geom_line() + facet_wrap(Month~Year,scales="free") + theme_economist() + scale_colour_economist()
		
	p30 <- ggplot(myData.ggplot.long.U,aes(x=Dates,y=value,group=variable,colour=as.factor(variable))) + geom_line() + facet_grid(Wl~Dates)
		
die