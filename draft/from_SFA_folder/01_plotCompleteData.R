#
# 01_plotCompleteData.R 
#
# August 8, 2014
#

# eliminate all stuff
rm(list = ls(all = TRUE))



# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

library("reshape2")
library("lattice")
library(latticeExtra)
library(missMDA)
library(cluster)
library(HSAUR)
library(fpc)

# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

col.array <- c("red","blue","green","magenta","cyan","yellow","black","purple","pink","brown")

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


library(chron)



# **********************************************************************************
# log file
# **********************************************************************************
Path.LOG <- paste(Path.Project,"0_log",sep="/")
if (!file.exists(Path.LOG)){print(paste("NOT existing:",Path.LOG));dir.create(Path.LOG,showWarnings=TRUE,recursive=TRUE)}
FL.LOG <- paste(Path.LOG,"01_plotCompleteData.log",sep="/")
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}	
cat(paste("1: defined path and file names.\n",sep=""))
cat(paste("1: defined path and file names.\n",sep=""),file=FL.LOG,append=TRUE)

# specify data and results folder
Path.Data <- paste(Path.Project,"0_data",sep="/")




# shallow wells in "TimeSeries_UHSpC_2011_ShallowWells.jpg"
Well.Shallow <- c("2-7","2-8","2-9","2-11","2-12","2-13","2-14","2-15","2-16","2-17","2-18","2-19","2-20","2-21","2-22","2-23","2-24","2-26","2-29","2-34","2-37","3-23","3-24","3-25","3-27","3-28","3-29","3-30","3-35")

# -----------------------------------------------------------------------------------------------------------------------------------


for (this.year in c(2011,2010))
{
	Path.results <- paste(Path.Project,"1_results",this.year,sep="/")
	if (!file.exists(Path.Data)){print(paste("NOT existing:",Path.Data));cat(paste(Path.Data," does not exist for [",this.subfolder,"].  Check why!\n",sep=""));die}
	if (!file.exists(Path.results)){print(paste("NOT existing:",Path.results));dir.create(Path.results,showWarnings=TRUE,recursive=TRUE)}

	FL.WL.OUT       <- paste(Path.results,paste("verify_WL_data_",this.year,"_complete.csv",sep=""),          sep="/")
	FL.U.OUT        <- paste(Path.results,paste("verify_U_data_",this.year,"_complete.csv",sep=""),           sep="/")
	FL.Sp.OUT       <- paste(Path.results,paste("verify_Sp_data_",this.year,"_complete.csv",sep=""),          sep="/")
	FL.Merged.U.WL  <- paste(Path.results,paste("verify_merged_",this.year,"_complete_U_WL.csv",sep=""),      sep="/")
	FL.Merged.Sp.WL <- paste(Path.results,paste("verify_merged_",this.year,"_complete_Sp_WL.csv",sep=""),     sep="/")
	FL.PDF          <- paste(Path.results,paste("verify_merged_",this.year,"_complete.pdf",sep=""),           sep="/")
	FL.OBJ          <- paste(Path.results,paste("verify_merged_",this.year,"_complete.Rdata",sep=""),         sep="/")
	FL.Imput.csv    <- paste(Path.results,paste("verify_merged_",this.year,"_complete_Imput.csv",sep=""),     sep="/")
	FL.Imput.obj    <- paste(Path.results,paste("verify_merged_",this.year,"_complete_Imput.Rdata",sep=""),   sep="/")
	FL.LOESS        <- paste(Path.results,paste("verify_merged_",this.year,"_complete_Loess.Rdata",sep=""),   sep="/")
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
	cat(paste("2: specify path and file names for year ",this.year,".\n",sep=""))
	cat(paste("2: specify path and file names for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)

	# OPEN PDF FILE
	pdf(file = FL.PDF,paper="special", width=17, height=11,bg = "transparent")



	if (this.year == 2010)
	{
		files <- c("2-10_WLData_2010","UData_2010_AllWell.txt","SpCData_2010_AllWell.txt")
		this.chron.cut.begin <- chron(dates  = "06/07/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		this.chron.cut.end   <- chron(dates  = "07/30/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		idx_for_abline <- c()
	
	}else if (this.year == 2011)
	{
		files <- c("2-10_WLData_2011","UData_2011_AllWell.txt","SpCData_2011_AllWell.txt")	
		this.chron.cut.begin <- chron(dates  = "05/06/11",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		this.chron.cut.end   <- chron(dates  = "08/06/11",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		idx_for_abline <- c(40,45,51)
	}
	cat(paste("3: specify path and file of input files for year ",this.year,".\n",sep=""))
	cat(paste("3: specify path and file of input files for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)
	

	# loop the files
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
			cat(paste("4c. ",this.file," add [date.chron] and keep only shallow wells for year ",this.year," for year ",this.year,".\n",sep=""))
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
	cat(paste("4e. We have U data [myData.U], Sp data [myData.Sp] and WL data [myData.WL] for ",this.year," for year ",this.year,".\n",sep=""))
	cat(paste("4e. We have U data [myData.U], Sp data [myData.Sp] and WL data [myData.WL] for ",this.year," for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	# use the date in column 1 as the row names
	row.names(myData.U) <- myData.U[,1]	# use the date column as the row names
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
	myData.Merged.U <- merge(myData.U,myData.WL,by = c("date.chron"),all.x = TRUE)	# WL has much more data than U and Sp so only keep WL when there are U and Sp
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



	# 8. plot the data "U" and "WL"
	par(mfrow = c(5,6))
	par(mar=c(3, 4, 3, 4))
	for (this.well in list.shallow.well)
	{
		# plot the complete time series 
		plot(myData.Merged.U[,"date.chron"],myData.Merged.U[,this.well],type="l",lty=1,lwd=0.5,col="red",col.lab="red",xlab=list("Date",cex=0.75),ylab=list(cex=0.75,paste("U at (",this.well,")",sep="")),main = list(cex=1.5,paste("Year ",this.year,": U at (",this.well,")",sep="")))
		par(new=TRUE)
		plot(myData.Merged.U[,"date.chron"],myData.Merged.U[,"WL"],type="l",lty=1,lwd=0.5,col="blue",col.lab="blue",axes=FALSE,bty="n",xlab="",ylab="")
		axis(side=4,at=pretty(range(myData.Merged.U[,"WL"],na.rm=TRUE)))
		mtext(col="blue","Water Level",side=4,line=3,cex=0.75)
		abline(v=myData.Merged.U[idx_for_abline,"date.chron"],type="l",lty=2,col=c("green","black","green"))
	}
	par(mfrow = c(5,6))
	cat(paste("8A. Plot the entire time series of U for year ",this.year,".\n",sep=""))
	cat(paste("8A. Plot the entire time series of U for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	par(mar=c(3, 4, 3, 4))
	for (this.well in list.shallow.well)
	{
		# plot the complete time series after May 6, 2011
		idx <- myData.Merged.U[,"date.chron"] > this.chron.cut.begin & myData.Merged.U[,"date.chron"] < this.chron.cut.end
		plot(myData.Merged.U[idx,"date.chron"],myData.Merged.U[idx,this.well],type="l",lty=1,lwd=0.5,col="red",col.lab="red",xlab=list("Date",cex=0.75),ylab=list(cex=0.75,paste("U at (",this.well,")",sep="")),main = list(cex=1.5,paste("Year ",this.year,": U at (",this.well,")",sep="")))
		par(new=TRUE)
		plot(myData.Merged.U[idx,"date.chron"],myData.Merged.U[idx,"WL"],type="l",lty=1,lwd=0.5,col="blue",col.lab="blue",axes=FALSE,bty="n",xlab="",ylab="")
		axis(side=4,at=pretty(range(myData.Merged.U[,"WL"],na.rm=TRUE)))
		mtext(col="blue","Water Level",side=4,line=3,cex=0.75)
		abline(v=myData.Merged.U[idx_for_abline,"date.chron"],type="l",lty=2,col=c("green","black","green"))	

	}
	cat(paste("8B. Plot the time series of U starting at May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("8B. Plot the time series of U starting at May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		



	# 9. plot the data "Sp" and "WL"
	par(mfrow = c(5,6))
	par(mar=c(3, 4, 3, 4))
	for (this.well in list.shallow.well)
	{
		# the complete time series
		plot(myData.Merged.Sp[,"date.chron"],myData.Merged.Sp[,this.well],type="l",lty=1,lwd=0.5,col="red",col.lab="red",xlab=list("Date",cex=0.75),ylab=list(cex=0.75,paste("Sp at (",this.well,")",sep="")),main = list(cex=1.5,paste("Year ",this.year,": Sp at (",this.well,")",sep="")))
		par(new=TRUE)
		plot(myData.Merged.Sp[,"date.chron"],myData.Merged.Sp[,"WL"],type="l",lty=1,lwd=0.5,col="blue",col.lab="blue",axes=FALSE,bty="n",xlab="",ylab="")
		axis(side=4,at=pretty(range(myData.Merged.Sp[,"WL"],na.rm=TRUE)))
		mtext(col="blue","Water Level",side=4,line=3,cex=0.75)
		abline(v=myData.Merged.Sp[idx_for_abline,"date.chron"],type="l",lty=2,col=c("green","black","green"))
	}
	cat(paste("9A. Plot the entire time series of Sp for year ",this.year,".\n",sep=""))
	cat(paste("9A. Plot the entire time series of Sp for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	par(mfrow = c(5,6))
	par(mar=c(3, 4, 3, 4))
	for (this.well in list.shallow.well)
	{
		# the time series after May 6, 2011
		idx <- myData.Merged.Sp[,"date.chron"] > this.chron.cut.begin & myData.Merged.Sp[,"date.chron"] < this.chron.cut.end
		plot(myData.Merged.Sp[idx,"date.chron"],myData.Merged.Sp[idx,this.well],type="l",lty=1,lwd=0.5,col="red",col.lab="red",xlab=list("Date",cex=0.75),ylab=list(cex=0.75,paste("Sp at (",this.well,")",sep="")),main = list(cex=1.5,paste("Year ",this.year,": Sp at (",this.well,")",sep="")))
		par(new=TRUE)
		plot(myData.Merged.Sp[idx,"date.chron"],myData.Merged.Sp[idx,"WL"],type="l",lty=1,lwd=0.5,col="blue",col.lab="blue",axes=FALSE,bty="n",xlab="",ylab="")
		axis(side=4,at=pretty(range(myData.Merged.Sp[,"WL"],na.rm=TRUE)))
		mtext(col="blue","Water Level",side=4,line=3,cex=0.75)
		abline(v=myData.Merged.Sp[idx_for_abline,"date.chron"],type="l",lty=2,col=c("green","black","green"))	
	}
	cat(paste("9B. Plot the time series of Sp starting at May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("9B. Plot the time series of Sp starting at May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# 10. plot the data "U" and "Sp"
	par(mfrow = c(5,6))
	par(mar=c(3, 4, 3, 4))
	for (this.well in list.shallow.well)
	{
		# the complete time series
		plot(myData.Merged.U[,"date.chron"],myData.Merged.U[,this.well],type="l",lty=1,lwd=0.5,col="red",col.lab="red",xlab=list("Date",cex=0.75),ylab=list(cex=0.75,paste("U at (",this.well,")",sep="")),main = list(cex=1.5,paste("Year ",this.year,": U at (",this.well,")",sep="")))
		par(new=TRUE)
		plot(myData.Merged.Sp[,"date.chron"],myData.Merged.Sp[,this.well],type="l",lty=1,lwd=0.5,col="blue",col.lab="blue",axes=FALSE,bty="n",xlab="",ylab="")
		axis(side=4,at=pretty(range(myData.Merged.Sp[,this.well],na.rm=TRUE)))
		mtext(col="blue","Sp",side=4,line=3,cex=0.75)
		abline(v=myData.Merged.U[idx_for_abline,"date.chron"],type="l",lty=2,col=c("green","black","green"))

	}
	cat(paste("10A. Plot the entire time series of U and Sp for year ",this.year,".\n",sep=""))
	cat(paste("10A. Plot the entire time series of U and Sp for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	par(mfrow = c(5,6))
	par(mar=c(3, 4, 3, 4))
	for (this.well in list.shallow.well)
	{
		# the time series after May 6, 2011
		idx <- myData.Merged.U[,"date.chron"] > this.chron.cut.begin & myData.Merged.U[,"date.chron"] < this.chron.cut.end
		plot(myData.Merged.U[idx,"date.chron"],myData.Merged.U[idx,this.well],type="l",lty=1,lwd=0.5,col="red",col.lab="red",xlab=list("Date",cex=0.75),ylab=list(cex=0.75,paste("U at (",this.well,")",sep="")),main = list(cex=1.5,paste("Year ",this.year,": U at (",this.well,")",sep="")))
		par(new=TRUE)
		idx <- myData.Merged.Sp[,"date.chron"] > this.chron.cut.begin	 & myData.Merged.Sp[,"date.chron"] < this.chron.cut.end
		plot(myData.Merged.Sp[idx,"date.chron"],myData.Merged.Sp[idx,this.well],type="l",lty=1,lwd=0.5,col="blue",col.lab="blue",axes=FALSE,bty="n",xlab="",ylab="")
		axis(side=4,at=pretty(range(myData.Merged.Sp[,this.well],na.rm=TRUE)))
		mtext(col="blue","Sp",side=4,line=3,cex=0.75)
		abline(v=myData.Merged.U[idx_for_abline,"date.chron"],type="l",lty=2,col=c("green","black","green"))	

	}
	cat(paste("10B. Plot the time series of U and Sp starting at May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("10B. Plot the time series of U and Sp starting at May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		



	# -------------------------------------------------------------------------------------
	# [myData.U]: only keep data after May 6, 20111 or even after May 12, 2011)
	# -------------------------------------------------------------------------------------
	myData.U <- myData.U[myData.U[,"date.chron"] > this.chron.cut.begin & myData.U[,"date.chron"] < this.chron.cut.end,]
	cat(paste("11. U data only after May 7, 2011 for year ",this.year,".\n",sep=""))
	cat(paste("11. U data only after May 7, 2011 for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		



	# --------------------------------------------------------------------
	# [myData.U]: detele well 2-34, 2-37 and date 7/30 which have more missing
	# --------------------------------------------------------------------
	if (this.year == 2011)
	{
		myData.U <- myData.U[myData.U[,"date.chron"] != chron(dates  = "07/30/11",times  = "00:00:00",format = c('m/d/y','h:m:s')),]
		myData.U <- myData.U[,names(myData.U)[!(names(myData.U) %in% c("2-34","2-37","3-35"))]]
		chron.begin <- this.chron.cut.begin
		chron.end   <- this.chron.cut.end
		
	}else if (this.year == 2010)
	{
		myData.U <- myData.U[(myData.U[,"date.chron"] >= chron(dates  = "06/07/10",times  = "00:00:00",format = c('m/d/y','h:m:s')) & 
		                      myData.U[,"date.chron"] <= chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))),]
		chron.begin <- chron(dates  = "06/07/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))
		chron.end   <- chron(dates  = "07/29/10",times  = "00:00:00",format = c('m/d/y','h:m:s'))		                      
	}
	cat(paste("12. delete well 2-34, 2-37, 3-35 and date July 30 which have the most missing data for year ",this.year,".\n",sep=""))
	cat(paste("12. delete well 2-34, 2-37, 3-35 and date July 30 which have the most missing data for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	#  
	myData.PCA_U1 <-   myData.U[,intersect(names(myData.U),list.shallow.well)]
	cat(paste("13. Use the wide format of U data for PCA for year ",this.year,".\n",sep=""))
	cat(paste("13. Use the wide format of U data for PCA for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# Estimate the number of dimensions for the Principal Component Analysis by cross-validation
	nb <- estim_ncpPCA(myData.PCA_U1,ncp.min=0,ncp.max=10) 
	plot(nb$criterion)
	cat(paste("14. use missMDA function to estimate number of PC for imputation for year ",this.year,".\n",sep=""))
	cat(paste("14. use missMDA function to estimate number of PC for imputation for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# imputation
	myData.PCA_U1.imputed <- imputePCA(myData.PCA_U1,ncp=6)
	cat(paste("15. impute the U data for year ",this.year,".\n",sep=""))
	cat(paste("15. impute the U data for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# [myData.U.complete]: completed data by imputation
	myData.U.complete <- myData.PCA_U1.imputed$completeObs
	cat(paste("16. the complete data after imputation for year ",this.year,".\n",sep=""))
	cat(paste("16. the complete data after imputation for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# output the file for other usage
	cat(paste("U complete data for analysis,",sep=""),file=FL.Imput.csv,append=TRUE)
	write.table(myData.U.complete,file=FL.Imput.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	save(myData.U.complete,file=FL.Imput.obj)
	cat(paste("17. save the complete data for other usage for year ",this.year,".\n",sep=""))
	cat(paste("17. save the complete data for other usage for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# ----------------------------------------------------
	# Kmean using base function in the stats package
	# ----------------------------------------------------
	par(mfrow = c(2,2))
	par(mar=c(3, 4, 3, 4))	
	Kmeans.results.3cls <- kmeans(myData.U.complete, 3)
	fitted.3cls <- fitted(Kmeans.results.3cls)
	resid.3cls <- myData.U.complete - fitted.3cls
	cat(paste("18. use kmeans in the [stats] package for year ",this.year,".\n",sep=""))
	cat(paste("18. use kmeans in the [stats] package for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# Kmean using "cluster" package (hsaur, CLSUET
	km    <- kmeans(myData.U.complete,3)
	dissE <- daisy(myData.U.complete) 
	dE2   <- dissE^2
	sk2   <- silhouette(km$cl, dE2)
	plot(sk2)
	cat(paste("19. use kmeans in the [HSAUR] and [cluster] packages for year ",this.year,".\n",sep=""))
	cat(paste("19. use kmeans in the [HSAUR] and [cluster] packages for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

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
	### cat(paste("12. use kmeans in the [fpc] and [cluster] packages for year ",this.year,".\n",sep=""))
	### cat(paste("12. use kmeans in the [fpc] and [cluster] packages for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# PCA
	PCA.results <- princomp(myData.U.complete)
	biplot(PCA.results)
	cat(paste("20a. PCA biplots for year ",this.year,".\n",sep=""))
	cat(paste("20a. PCA biplots for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		



	# loading plot
	par(mfrow=c(1,2))
	Loadings <- PCA.results$loadings
	plot(Loadings[,1],Loadings[,2],col="red",type="p",pch=16,xlab="PC1",ylab="PC2",main=paste("Year ",this.year,": wells in the PC space PC1-PC2",sep=""))
	text(Loadings[,1],Loadings[,2],row.names(Loadings),cex=0.75,col="black")

	plot(Loadings[,1],Loadings[,3],col="blue",type="p",pch=16,xlab="PC1",ylab="PC3",main=paste("Year ",this.year,": wells in the PC space PC1-PC3",sep=""))
	text(Loadings[,1],Loadings[,3],row.names(Loadings),cex=0.75,col="black")
	cat(paste("21b. PCA loading plots for year ",this.year,".\n",sep=""))
	cat(paste("21b. PCA loading plots for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	#
	# use MCluster
	#
	myData.U.transport <- t(myData.U.complete)
	# use Mcluster
	cluster.U2 <- Mclust(myData.U.transport)
	no.cls     <- length(table(cluster.U2$classification))
	idx.class  <- cluster.U2$classification
	cat(paste("22A. Mcluster on the transported data for year ",this.year,".\n",sep=""))
	cat(paste("22A. Mcluster on the transported data for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	par(mfrow=c(3,no.cls/3))
	for (this.class in seq(1:no.cls))
	{
		idx.this.cls     <- names(idx.class)[idx.class==this.class]
		U.this.cls       <- myData.Merged.U[,idx.this.cls]
		U.range.this.cls <- range(U.this.cls,na.rm=TRUE)

		idx.date <- myData.Merged.U[,"date.chron"] >= chron.begin & myData.Merged.U[,"date.chron"] <= chron.end

		plot(myData.Merged.U[idx.date,idx.this.cls[1]] ~ myData.Merged.U[idx.date,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",paste(idx.this.cls,collapse=","),sep=""))
		if (length(idx.this.cls) > 1)
		{
			for (count in seq(from=2,to=length(idx.this.cls)))
			{
				lines(myData.Merged.U[idx.date,idx.this.cls[count]] ~ myData.Merged.U[idx.date,"date.chron"],pch=16,col=col.array[count])
			}
		}

	}
	cat(paste("22B. overlay time series of the well in the classes for year ",this.year,".\n",sep=""))
	cat(paste("22B. overlay time series of the well in the classes for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	# use 3 classes Kmean
	par(mfrow = c(2,2))
	par(mar=c(3, 4, 3, 4))		
	kmeans.U2  <- kmeans(myData.U.transport,centers=3)
	idx.class  <- kmeans.U2$cluster
	no.cls     <- max(idx.class)
	cat(paste("23A. Kmean for 3 clusters for year ",this.year,".\n",sep=""))
	cat(paste("23A. Kmean for 3 clusters for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	par(mfrow=c(1,3))
	for (this.class in seq(1:no.cls))
	{
		idx.this.cls     <- names(idx.class)[idx.class==this.class]
		U.this.cls       <- myData.Merged.U[,idx.this.cls]
		U.range.this.cls <- range(U.this.cls,na.rm=TRUE)

		idx.date <- myData.Merged.U[,"date.chron"] >= chron.begin & myData.Merged.U[,"date.chron"] <= chron.end

		plot(myData.Merged.U[idx.date,idx.this.cls[1]] ~ myData.Merged.U[idx.date,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",paste(idx.this.cls,collapse=","),sep=""))
		if (length(idx.this.cls) > 1)
		{
			for (count in seq(from=2,to=length(idx.this.cls)))
			{
				lines(myData.Merged.U[idx.date,idx.this.cls[count]] ~ myData.Merged.U[idx.date,"date.chron"],pch=16,col=col.array[count])
			}
		}

	}
	cat(paste("23B. overlay time series of the well in the classes of 3 cls kmean for year ",this.year,".\n",sep=""))
	cat(paste("23B. overlay time series of the well in the classes of 3 cls kmean for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	# use 4 classes Kmean
	kmeans.U2  <- kmeans(myData.U.transport,centers=4)
	idx.class  <- kmeans.U2$cluster
	no.cls     <- max(idx.class)
	cat(paste("24A. Kmean for 4 clusters for year ",this.year,".\n",sep=""))
	cat(paste("24A. Kmean for 4 clusters for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	par(mfrow=c(2,2))
	for (this.class in seq(1:no.cls))
	{
		idx.this.cls     <- names(idx.class)[idx.class==this.class]
		U.this.cls       <- myData.Merged.U[,idx.this.cls]
		U.range.this.cls <- range(U.this.cls,na.rm=TRUE)

		idx.date <- myData.Merged.U[,"date.chron"] >= chron.begin & myData.Merged.U[,"date.chron"] <= chron.end

		plot(myData.Merged.U[idx.date,idx.this.cls[1]] ~ myData.Merged.U[idx.date,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",paste(idx.this.cls,collapse=","),sep=""))
		if (length(idx.this.cls) > 1)
		{
			for (count in seq(from=2,to=length(idx.this.cls)))
			{
				lines(myData.Merged.U[idx.date,idx.this.cls[count]] ~ myData.Merged.U[idx.date,"date.chron"],pch=16,col=col.array[count])
			}
		}

	}
	cat(paste("24B. overlay time series of the well in the classes of 4 cls kmean for year ",this.year,".\n",sep=""))
	cat(paste("24B. overlay time series of the well in the classes of 4 cls kmean for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		




	# use 5 classes Kmean
	kmeans.U2  <- kmeans(myData.U.transport,centers=5)
	idx.class  <- kmeans.U2$cluster
	no.cls     <- max(idx.class)
	cat(paste("25A. Kmean for 5 clusters for year ",this.year,".\n",sep=""))
	cat(paste("25A. Kmean for 5 clusters for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	par(mfrow=c(2,3))
	for (this.class in seq(1:no.cls))
	{
		idx.this.cls     <- names(idx.class)[idx.class==this.class]
		U.this.cls       <- myData.Merged.U[,idx.this.cls]
		U.range.this.cls <- range(U.this.cls,na.rm=TRUE)

		idx.date <- myData.Merged.U[,"date.chron"] >= chron.begin & myData.Merged.U[,"date.chron"] <= chron.end

		plot(myData.Merged.U[idx.date,idx.this.cls[1]] ~ myData.Merged.U[idx.date,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",paste(idx.this.cls,collapse=","),sep=""))
		if (length(idx.this.cls) > 1)
		{
			for (count in seq(from=2,to=length(idx.this.cls)))
			{
				lines(myData.Merged.U[idx.date,idx.this.cls[count]] ~ myData.Merged.U[idx.date,"date.chron"],pch=16,col=col.array[count])
			}
		}

	}
	cat(paste("25B. overlay time series of the well in the classes of 5 cls kmean for year ",this.year,".\n",sep=""))
	cat(paste("25B. overlay time series of the well in the classes of 5 cls kmean for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		




	# use 6 classes Kmean
	kmeans.U2  <- kmeans(myData.U.transport,centers=6)
	idx.class  <- kmeans.U2$cluster
	no.cls     <- max(idx.class)
	cat(paste("26A. Kmean for 6 clusters for year ",this.year,".\n",sep=""))
	cat(paste("26A. Kmean for 6 clusters for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

	par(mfrow=c(2,3))
	for (this.class in seq(1:no.cls))
	{
		idx.this.cls     <- names(idx.class)[idx.class==this.class]
		U.this.cls       <- myData.Merged.U[,idx.this.cls]
		U.range.this.cls <- range(U.this.cls,na.rm=TRUE)

		idx.date <- myData.Merged.U[,"date.chron"] >= chron.begin & myData.Merged.U[,"date.chron"] <= chron.end

		plot(myData.Merged.U[idx.date,idx.this.cls[1]] ~ myData.Merged.U[idx.date,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",paste(idx.this.cls,collapse=","),sep=""))
		if (length(idx.this.cls) > 1)
		{
			for (count in seq(from=2,to=length(idx.this.cls)))
			{
				lines(myData.Merged.U[idx.date,idx.this.cls[count]] ~ myData.Merged.U[idx.date,"date.chron"],pch=16,col=col.array[count])
			}
		}

	}
	cat(paste("26B. overlay time series of the well in the classes of 6 cls kmean for year ",this.year,".\n",sep=""))
	cat(paste("26B. overlay time series of the well in the classes of 6 cls kmean for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


	#
	# loess plot
	#
	if (this.year == 2011)
	{
		for (this.degree in c(1,2))
		{
			for (this.family in c("gaussian","symmetric"))
			{
				for (this.span in seq(from=0.05,to=0.9,by=0.05))
				{
					par(mfrow=c(5,6))
					for (this.well in intersect(names(myData.Merged.U),list.shallow.well))
					{
						myData.thisWell <- myData.Merged.U[,c(this.well,"date.chron")]
						plot(myData.thisWell[,this.well] ~ myData.thisWell[,"date.chron"],pch=16,col="red",xlab="date",ylab="U",main = paste("Year ",this.year,": ",this.well,": span=(",this.span,") family=(",this.family,") degree = (",this.degree,")",sep=""))
						loess.fit <- loess(myData.thisWell[,this.well] ~ myData.thisWell[,"date.chron"], data =myData.thisWell,span=this.span,family = this.family,degree=this.degree)
						lines(myData.thisWell[,"date.chron"],predict(loess.fit),col="blue")

						#
						myData.loess <- cbind(myData.thisWell[!(is.na(myData.thisWell[,this.well])),c("date.chron",this.well)],predict(loess.fit))
					}
				}
			}
		}
	
		cat(paste("27. loess fitting of U time series for year ",this.year,".\n",sep=""))
		cat(paste("27. loess fitting of U time series for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


		#
		# OK use span = 0.15, degree=2, family = gaussian
		#
		this.degree <- 2
		this.family <- "gaussian"
		this.span <- 0.15

		par(mfrow=c(5,6))
		count <- 0
		for (this.well in intersect(names(myData.Merged.U),list.shallow.well)[!(intersect(names(myData.Merged.U),list.shallow.well) %in% c("2-34","2-37","3-35"))])
		{
			count <- count + 1
			myData.thisWell <- myData.Merged.U[,c(this.well,"date.chron")]
			plot(myData.thisWell[,this.well] ~ myData.thisWell[,"date.chron"],pch=16,col="red",xlab="date",ylab="U",main = paste("Year ",this.year,": well:",this.well,sep=""))
			loess.fit <- loess(myData.thisWell[,this.well] ~ myData.thisWell[,"date.chron"], data =myData.thisWell,span=this.span,family = this.family,degree=this.degree)
			lines(myData.thisWell[,"date.chron"],predict(loess.fit),col="blue")
			abline(v=this.chron.cut.begin,lty=1,col="blue")

			#
			myData.loess <- cbind(myData.thisWell[!(is.na(myData.thisWell[,this.well])),c("date.chron",this.well)],predict(loess.fit))
			names(myData.loess) <- sub("predict\\(loess\\.fit\\)",paste(this.well,".loess",sep=""),names(myData.loess))


			if (count == 1)
			{
				myData.LOESS <- myData.loess
			}else{
				myData.LOESS <- merge(myData.LOESS,myData.loess)
			}
		}
		cat(paste("28. loess fitting of U time series based on a fixed set of parameters for year ",this.year,".\n",sep=""))
		cat(paste("28. loess fitting of U time series based on a fixed set of parameters for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


		# output the LOESS data for other usage
		cat(paste("LOESS complete data for analysis,",sep=""),file=FL.LOESS,append=TRUE)
		write.table(myData.LOESS,file=FL.LOESS,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
		cat(paste("29. output the LOES data for year ",this.year,".\n",sep=""))
		cat(paste("29. output the LOES data for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		
	}	# only for 2011 data

	#
	# save the file for other usage
	#
	if (this.year == 2011)
	{
		save(myData.U.complete,myData.LOESS,file=FL.OBJ)
	}else if (this.year == 2010)
	{
		save(myData.U.complete,file=FL.OBJ)
	}
	cat(paste("30. save the complete data for other usage for year ",this.year,".\n",sep=""))
	cat(paste("30. save the complete data for other usage for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		





	# ====================================================================================================
	# use the loess predict to do clustering
	# ====================================================================================================
	if (this.year == 2011)
	{
		row.names(myData.LOESS) <- myData.LOESS[,"date.chron"]
		myData.LOESS.complete   <- myData.LOESS[myData.LOESS[,"date.chron"] > this.chron.cut.begin,grep("\\.loess",names(myData.LOESS),value=TRUE)]
		myData.LOESS.transport  <- t(myData.LOESS.complete)
		cat(paste("31. use loess fitted data for cluster abalysis for year ",this.year,".\n",sep=""))
		cat(paste("31. use loess fitted data for cluster abalysis for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


		#
		# use MCluster
		#
		myData.LOESS.transport <- t(myData.LOESS.complete)
		# use Mcluster
		cluster.U2 <- Mclust(myData.LOESS.transport)
		no.cls     <- length(table(cluster.U2$classification))
		idx.class  <- cluster.U2$classification
		cat(paste("32A. UCluster for year ",this.year,".\n",sep=""))
		cat(paste("32A. UCluster for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


		par(mfrow=c(3,no.cls/3))
		for (this.class in seq(1:no.cls))
		{
			idx.this.cls     <- names(idx.class)[idx.class==this.class]
			U.this.cls       <- myData.LOESS.complete[,idx.this.cls]
			U.range.this.cls <- range(U.this.cls,na.rm=TRUE)


			plot(myData.LOESS[,idx.this.cls[1]] ~ myData.LOESS[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",paste(idx.this.cls,collapse=",")),sep=""))
			if (length(idx.this.cls) > 1)
			{
				for (count in seq(from=2,to=length(idx.this.cls)))
				{
					lines(myData.LOESS[,idx.this.cls[count]] ~ myData.LOESS[,"date.chron"],pch=16,col=col.array[count])
				}
			}

		}
		cat(paste("32B. overlay time series of the well in the classes from UClsuter for year ",this.year,".\n",sep=""))
		cat(paste("32B. overlay time series of the well in the classes from UClsuter for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


		# use 3 classes Kmean
		kmeans.U2  <- kmeans(myData.LOESS.transport,centers=3)
		idx.class  <- kmeans.U2$cluster
		no.cls     <- max(idx.class)
		cat(paste("33A. Kmean for 3 clusters for year ",this.year,".\n",sep=""))
		cat(paste("33A. Kmean for 3 clusters for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

		par(mfrow=c(1,3))
		for (this.class in seq(1:no.cls))
		{
			idx.this.cls     <- names(idx.class)[idx.class==this.class]
			U.this.cls       <- myData.LOESS.complete[,idx.this.cls]
			U.range.this.cls <- range(U.this.cls,na.rm=TRUE)


			plot(myData.LOESS[,idx.this.cls[1]] ~ myData.LOESS[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",paste(idx.this.cls,collapse=",")),sep=""))
			if (length(idx.this.cls) > 1)
			{
				for (count in seq(from=2,to=length(idx.this.cls)))
				{
					lines(myData.LOESS[,idx.this.cls[count]] ~ myData.LOESS[,"date.chron"],pch=16,col=col.array[count])
				}
			}

		}
		cat(paste("33B. overlay time series of the well in the classes of 3 cls kmean for year ",this.year,".\n",sep=""))
		cat(paste("33B. overlay time series of the well in the classes of 3 cls kmean for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

		# use 4 classes Kmean
		kmeans.U2  <- kmeans(myData.LOESS.transport,centers=4)
		idx.class  <- kmeans.U2$cluster
		no.cls     <- max(idx.class)
		cat(paste("34A. Kmean for 4 clusters for year ",this.year,".\n",sep=""))
		cat(paste("34A. Kmean for 4 clusters for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

		par(mfrow=c(2,2))
		for (this.class in seq(1:no.cls))
		{
			idx.this.cls     <- names(idx.class)[idx.class==this.class]
			U.this.cls       <- myData.LOESS.complete[,idx.this.cls]
			U.range.this.cls <- range(U.this.cls,na.rm=TRUE)


			plot(myData.LOESS[,idx.this.cls[1]] ~ myData.LOESS[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",gsub("\\.loess","",paste(idx.this.cls,collapse=","))),sep=""))
			if (length(idx.this.cls) > 1)
			{
				for (count in seq(from=2,to=length(idx.this.cls)))
				{
					lines(myData.LOESS[,idx.this.cls[count]] ~ myData.LOESS[,"date.chron"],pch=16,col=col.array[count])
				}
			}

		}
		cat(paste("34B. overlay time series of the well in the classes of 4 cls kmean for year ",this.year,".\n",sep=""))
		cat(paste("34B. overlay time series of the well in the classes of 4 cls kmean for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		


		# use 5 classes Kmean
		kmeans.U2  <- kmeans(myData.LOESS.transport,centers=5)
		idx.class  <- kmeans.U2$cluster
		no.cls     <- max(idx.class)
		cat(paste("35A. Kmean for 5 clusters for year ",this.year,".\n",sep=""))
		cat(paste("35A. Kmean for 5 clusters for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

		par(mfrow=c(2,3))
		for (this.class in seq(1:no.cls))
		{
			idx.this.cls     <- names(idx.class)[idx.class==this.class]
			U.this.cls       <- myData.LOESS.complete[,idx.this.cls]
			U.range.this.cls <- range(U.this.cls,na.rm=TRUE)


			plot(myData.LOESS[,idx.this.cls[1]] ~ myData.LOESS[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",gsub("\\.loess","",paste(idx.this.cls,collapse=","))),sep=""))
			if (length(idx.this.cls) > 1)
			{
				for (count in seq(from=2,to=length(idx.this.cls)))
				{
					lines(myData.LOESS[,idx.this.cls[count]] ~ myData.LOESS[,"date.chron"],pch=16,col=col.array[count])
				}
			}

		}
		cat(paste("35B. overlay time series of the well in the classes of 5 cls kmean for year ",this.year,".\n",sep=""))
		cat(paste("35B. overlay time series of the well in the classes of 5 cls kmean for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		



		# use 6 classes Kmean
		kmeans.U2  <- kmeans(myData.LOESS.transport,centers=6)
		idx.class  <- kmeans.U2$cluster
		no.cls     <- max(idx.class)
		cat(paste("36A. Kmean for 6 clusters for year ",this.year,".\n",sep=""))
		cat(paste("36A. Kmean for 6 clusters for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)		

		par(mfrow=c(2,3))
		for (this.class in seq(1:no.cls))
		{
			idx.this.cls     <- names(idx.class)[idx.class==this.class]
			U.this.cls       <- myData.LOESS.complete[,idx.this.cls]
			U.range.this.cls <- range(U.this.cls,na.rm=TRUE)


			plot(myData.LOESS[,idx.this.cls[1]] ~ myData.LOESS[,"date.chron"],type="l",lty=1,col="red",xlab="date",ylab="U",ylim = c(0,U.range.this.cls[2]),main=paste("Year ",this.year,": this cls has ",length(idx.this.cls)," wells:",gsub("\\.loess","",paste(idx.this.cls,collapse=",")),sep=""))
			if (length(idx.this.cls) > 1)
			{
				for (count in seq(from=2,to=length(idx.this.cls)))
				{
					lines(myData.LOESS[,idx.this.cls[count]] ~ myData.LOESS[,"date.chron"],pch=16,col=col.array[count])
				}
			}

		}
		cat(paste("36B. overlay time series of the well in the classes of 6 cls kmean for year ",this.year,".\n",sep=""))
		cat(paste("36B. overlay time series of the well in the classes of 6 cls kmean for year ",this.year,".\n",sep=""),file=FL.LOG,append=TRUE)
	}	# only for 2011 data

	dev.off()
	
	# ----------------------------------------------------------------------
	# assign a named data frame to the current data frame
	# myData.WL --> myData2010 for year 2010 and myData.WL --> myData2011 for year 2011
	# myData.U  --> myData2010 for year 2010 and myData.U  --> myData2011 for year 2011
	# myData.Sp --> myData2010 for year 2010 and myData.Sp --> myData2011 for year 2011	
	command.string <- paste(paste("myData.WL",this.year,sep=""),"          <- myData.WL",sep=""); eval(parse(text=command.string))	
	command.string <- paste(paste("myData.U",this.year,sep=""), "          <- myData.U",sep="");  eval(parse(text=command.string))	
	command.string <- paste(paste("myData.Sp",this.year,sep=""),"          <- myData.Sp",sep=""); eval(parse(text=command.string))	
	command.string <- paste(paste("myData.Merged.U",this.year,sep=""),"    <- myData.Merged.U",sep=""); eval(parse(text=command.string))	
	command.string <- paste(paste("myData.Merged.Sp",this.year,sep=""),"   <- myData.Merged.Sp",sep=""); eval(parse(text=command.string))	
	command.string <- paste(paste("myData.U.complete",this.year,sep=""),"  <- myData.U.complete",sep=""); eval(parse(text=command.string))	
	command.string <- paste(paste("myData.U.transport",this.year,sep="")," <- myData.U.transport",sep=""); eval(parse(text=command.string))		
}	# end of year loop




# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n01_plotCompleteData.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n01_plotCompleteData.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [01_plotCompleteData.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [01_plotCompleteData.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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
### 		# assign a named data frame to the current data frame
### 		if (this.file %in% "WLdata)
### 		{
### 			this.file.string <- "WL"		
### 		}else if (this.file %in% "Udata")
### 		{
### 			this.file.string <- "U"
### 		}else if (this.file %in% "SpCdata")
### 		{
### 			this.file.string <- "Sp"
### 		}	
### 		object.name    <- paste("myData.",this.file.string,this.year,sep="")
### 		df.name        <- paste("myData.",this.file.string,          sep="")
### 		command.string <- paste(object.name," <- ",df.name,",sep="")	
### 		# eval(parse(text=command.string))
