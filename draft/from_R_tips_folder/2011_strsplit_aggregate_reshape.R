# data_arrange3.R
# 
# Load the data file which mainly is tab delimited
#

library("lattice")
library("chron")

#
# since the date filed consists of month in character
#
      months.str  <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
names(months.str) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

      months.num  <- c(1,2,3,4,5,6,7,8,9,10,11,12)
names(months.num) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


# Path.Data <- "\\pnl\Projects\JCPenney\JCPenney_existing\Colonial_Heights\Trend Data\Tracer ES\raw data\"
Path.Input  <- "/phome/nap/JC_Penney/Existing_BLDG_Sim/trend_data/0301-0323"
FL.LOG <- "log.log"
if (file.exists(FL.LOG)){print(paste(FL.LOG," exist. Delete it!"));file.remove(FL.LOG)}			# remove existing OUTPUT files

   input.files <- list.files(path=Path.Input,pattern=c("^jzhang\\.*.*txt_$"))						# the MAKE FILE
no.input.files <- length(input.files)

idx.file <- 0
for (idx in seq(1,no.input.files))
{
	idx.file <- idx.file + 1
	tmp <- read.table(file=paste(Path.Input,input.files[idx.file],sep="/"),stringsAsFactors=FALSE,header=TRUE,sep="\t")
	names(tmp) <- c("v1","v2","v3","v4","v5")
	
	if (idx.file == 1)
	{
		myData.tmp <- tmp
	}else{
		myData.tmp <- rbind(myData.tmp,tmp)
	}
	
	cat(paste(input.files[idx.file]," has been read in\n",sep=""))
	cat(paste(input.files[idx.file]," has been read in\n",sep=""),file=FL.LOG,append=TRUE)
}
	


# -----------------------------------------------------------------------
# the 1st field consists of variable names but also some thing else separated with a comma
# -----------------------------------------------------------------------
tmp      <- unlist(strsplit(myData.tmp[,"v1"],","))
variable <- as.character(tmp[seq(from=1,to=length(tmp),by=2)])		# [variable] name is the first part of the field
cat("get the 1st field: variable\n")



# -----------------------------------------------------------------------
# the 3rd field is date/time which consists of month in character
# -----------------------------------------------------------------------
    dates <- as.character(myData.tmp[,"v3"])
      tmp <- unlist(strsplit(dates," "))				# the date/time are in a format "Mar 01 2011 00:02" format
month.tmp <- as.character(tmp[seq(from=1,to=length(tmp),by=4)])		# the 1st of the 4 parts in the date/time string is [month] which is a character

month <- months.num[names(months.str[month.tmp])]			# convert the character month to numeric month [month]

day   <- as.numeric(tmp[seq(from=2,to=length(tmp),by=4)])		# [day]  is the 2nd of the 4 parts in the date/time string
year  <- as.numeric(tmp[seq(from=3,to=length(tmp),by=4)])		# [year] is the 3nd of the 4 parts in the date/time string
hr.min<- tmp[seq(from=4,to=length(tmp),by=4)]				# Hour and minute consists of the 4th of the 4 part in the date/time string and separated by a colon

tmp   <- unlist(strsplit(hr.min,":"))					# further split hour:minute to get hour and minute
hour  <- as.numeric(tmp[seq(from=1,to=length(tmp),by=2)])		# [hour]
minute<- as.numeric(sub(",","",tmp[seq(from=2,to=length(tmp),by=2)]))	# [minute]: need to remove the comma in the end of the string
cat("get the 3rd field: date/time\n")

# -----------------------------------------------------------------------
# the 5th field is values but mixed with units which consists of one or more parts separated by space
# -----------------------------------------------------------------------
tmp    <- gsub("(^[a-zA-Z]+$)","-999.999\\1",myData.tmp[,"v5"])		# in some file the values field only contains "On" or "Off" add -99999.99999 for place hold
tmp    <- gsub("([0-9]+\\.[0-9]+)(.)","\\1,\\2",tmp)			# need to join the number part and the rest part with a comma then split them
tmp    <- unlist(strsplit(tmp,","))					# the numeric part is the value we need
values <- as.numeric(tmp[seq(from=1,to=length(tmp),by=2)])		# the [values] we need
units  <-            tmp[seq(from=2,to=length(tmp),by=2)]		# the [units] we do not need


# convert the "ON" values to 1 and "Off" value to 0
values[units == "On"]  <- 1
values[units == "Off"] <- 0


cat("get the 5th field: values\n")


# -----------------------------------------------------------------------
# resonstruct the date from
# -----------------------------------------------------------------------
myData.raw <- data.frame(dates    = dates,				# original data in the date/time file
                         variable = variable,				# the [variable] name
                         values   = values,				# the [values] of the variables
                         month    = month,				# the [month]  of the date/time
                         year     = year,				# the [year]   of the date/time
                         day      = day,				# the [day]    of the date/time
                         hour     = hour,				# the [hour]   of the date/time
                         minute   = minute,				# the [minute] of the date/time
                         stringsAsFactors=FALSE)
cat("reconstruct the raw data\n")
                         
# create a chron date field                         
chron.date <- chron(dates = paste(myData.raw[,"month"],myData.raw[,"day"],   myData.raw[,"year"],sep="/"),
                    times = paste(myData.raw[,"hour"], myData.raw[,"minute"],0,              sep=":"))


# add a chron date object to the date frame
myData.raw <- cbind(myData.raw,
                    chron.date = chron.date)
                
# sort the data according to variable and date/time                
myData.raw.order  <- myData.raw[order(myData.raw[,"variable"],myData.raw[,"year"],myData.raw[,"month"],myData.raw[,"day"],myData.raw[,"hour"],myData.raw[,"minute"]),]
cat("the original data has been successfully read in and arranged in a nice format in [myData.raw] and sorted in [myData.raw.order]\n")

#
# write the sorted raw data out
#
var.list <- c("variable","year","month","day","hour","minute","values")

FL.OUT1 <- paste(Path.Input,"myData_raw_sorted.csv",sep="/")

cat(",",file=FL.OUT1,append=TRUE)
write.table(myData.raw.order[,c(var.list,"dates")],file=FL.OUT1,sep=",",col.names=TRUE,row.names=TRUE)
cat("the sorted raw data in [myData.raw.order] has been written out\n")

# -----------------------------------------------------------------------
# plot the time series of the raw data of the variables in the data file
# -----------------------------------------------------------------------
FL.PDF <- paste(Path.Input,"JCP_raw.PDF",sep="/")	
if (file.exists(FL.PDF)){print(paste(FL.PDF," exist. Delete it!"));file.remove(FL.PDF)}			# remove existing OUTPUT files

pdf(file = FL.PDF,paper="a4r", width=0, height=0)	# this should have a integer index of 2.  Note: the "null device" is always device 1



# all variables in a single plot
plot.obj <- xyplot(values ~ chron.date,data=myData.raw.order,
                   groups = variable,
                   layout = c(1,1),
                   type = "l",
                   lwd  = 0.5,
                   lty  = 1,
                   col  = "red",
                   xlab = list("Date/Time",cex=1.5),
                   ylab = list("Value",cex=1.5),
                   scale=list(y="free")
              )           
plot(plot.obj)              

# one plot for each variable
plot.obj <- xyplot(values ~ chron.date | as.factor(variable),data=myData.raw.order,
                   layout = c(1,1),
                   type = "l",
                   lwd  = 0.5,
                   lty  = 1,
                   col  = "red",
                   xlab = list("Date/Time",cex=1.5),
                   ylab = list("Value",cex=1.5),
                   scale=list(y="free")
              )           
plot(plot.obj) 

dev.off()
cat("the original data has been plotted\n")


# -----------------------------------------------------------------------
# convert the long format to wide format
# -----------------------------------------------------------------------
myData.wide <- reshape(myData.raw,v.names   = "values",		# field to be converted from long to wide
                                  idvar     = "dates",		# 
                                  timevar   = "variable",	# field with categories
                                  direction = "wide")

#
# write the wide formatted data out
#
FL.OUT2 <- paste(Path.Input,"myData_wide.csv",sep="/")
cat(",",file=FL.OUT2,append=TRUE)
write.table(myData.wide,file=FL.OUT2,sep=",",col.names=TRUE,row.names=TRUE)
cat("the wide formatted data in [myData.wide] has been written out\n")


# 
# this is the original date/time
#
    dates <- as.character(myData.wide[,"dates"])
      tmp <- unlist(strsplit(dates," "))			# the date/time are in a format "Mar 01 2011 00:02" format
month.tmp <- as.character(tmp[seq(from=1,to=length(tmp),by=4)])

month <- months.num[names(months.str[month.tmp])]

day   <- as.numeric(tmp[seq(from=2,to=length(tmp),by=4)])
year  <- as.numeric(tmp[seq(from=3,to=length(tmp),by=4)])
hr.min<- tmp[seq(from=4,to=length(tmp),by=4)]			# hour minute need further split

tmp   <- unlist(strsplit(hr.min,":"))				# the date/time are in a format "Mar 01 2011 00:02" format
hour  <- as.numeric(tmp[seq(from=1,to=length(tmp),by=2)])
minute<- as.numeric(sub(",","",tmp[seq(from=2,to=length(tmp),by=2)]))	# there is aq comma after the minute)
chron.date <- chron(dates = paste(myData.wide[,"month"],myData.wide[,"day"],myData.wide[,"year"],sep="/"),
                    times = paste(myData.wide[,"hour"], as.numeric(as.character(myData.wide[,"minute"])),0,sep=":"))

myData.raw.wide <- data.frame(myData.wide,month=month,year=year,day=day,hour=hour,minute=minute,chron.date=chron.date,stringsAsFactors=FALSE)



#
# sorted
#
myData.raw.wide.order <- myData.raw.wide[order(myData.raw.wide[,"year"],myData.raw.wide[,"month"],myData.raw.wide[,"day"],myData.raw.wide[,"hour"],myData.raw.wide[,"minute"]),]

#
# write the wide formatted data out
#
FL.OUT4 <- paste(Path.Input,"myData_wide_sorted.csv",sep="/")
cat(",",file=FL.OUT4,append=TRUE)
write.table(myData.raw.wide.order,file=FL.OUT4,sep=",",col.names=TRUE,row.names=TRUE)
cat("the sorted wide formatted data in [myData.raw.wide.order] has been written out\n")


# -----------------------------------------------------------------------
# bin the data to 5 minute interval 
# -----------------------------------------------------------------------
break1  <- c(seq(from=0,to=60,by=1))	# time interval 1  minutes
label1  <- c(seq(from=0,to=59,by=1))	# category labels when 1  minutes is used as time interval for averaging

break5  <- c(seq(from=0,to=60,by=5))	# time interval 5  minutes
label5  <- c(seq(from=0,to=55,by=5))	# category labels when 5  minutes is used as time interval for averaging

breakX  <- break5
labelX  <- label5
		
#
# convert minute field into minute category
#
cut.Xmin <- cut(myData.raw.order[,"minute"],breaks=breakX,labels=labelX,right=FALSE)	# convert to the "minute" field into categories

#
# append this minute category field into the data frame [myData.raw.order]
#
if ("agg.Xmin" %in% names(myData.raw.order)){myData.raw.order[,"agg.Xmin"] <- cut.Xmin}else{myData.raw.order <- data.frame(myData.raw.order,agg.Xmin=cut.Xmin)}

# aggregate on X (1 or 5 minutes) minutes
myData.agg <- aggregate(myData.raw.order[,c("values"),drop=FALSE],
			list(  variable = myData.raw.order[,"variable"],
				   year = myData.raw.order[,"year"],
				  month = myData.raw.order[,"month"],
				    day = myData.raw.order[,"day"],
				   hour = myData.raw.order[,"hour"],
				 minute = myData.raw.order[,"agg.Xmin"]), 
			      mean,na.rm=TRUE)


chron.date <- chron(dates = paste(myData.agg[,"month"],myData.agg[,"day"],myData.agg[,"year"],sep="/"),
                    times = paste(myData.agg[,"hour"], as.numeric(as.character(myData.agg[,"minute"])),0,sep=":"))


# add a chron date object
myData.agg <- cbind(myData.agg,chron.date = chron.date)

# -----------------------------------------------------------------------
# sort data before output
# -----------------------------------------------------------------------
var.list <- c("variable","year","month","day","hour","minute","values")

myData.agg.order <- myData.agg[order(myData.agg[,"variable"],myData.agg[,"year"],myData.agg[,"month"],myData.agg[,"day"],myData.agg[,"hour"],myData.agg[,"minute"]),]

#
# write the 5 minute aggregated raw data
#
FL.OUT5 <- paste(Path.Input,"myData_agg.csv",sep="/")
cat(",",file=FL.OUT5,append=TRUE)
write.table(myData.agg.order,file=FL.OUT5,sep=",",col.names=TRUE,row.names=TRUE)
cat("the 5-minutes aggregated long formatted data in [myData.agg.order] has been written out\n")



# -----------------------------------------------------------------------
# plot the time series of the raw data of the variables in the data file
# -----------------------------------------------------------------------
FL.PDF <- paste(Path.Input,"JCP_agg.PDF",sep="/")	
if (file.exists(FL.PDF)){print(paste(FL.PDF," exist. Delete it!"));file.remove(FL.PDF)}			# remove existing OUTPUT files

pdf(file = FL.PDF,paper="a4r", width=0, height=0)	# this should have a integer index of 2.  Note: the "null device" is always device 1



# all variables in a single plot
plot.obj <- xyplot(values ~ chron.date,data=myData.agg.order,
                   groups = variable,
                   layout = c(1,1),
                   type = "l",
                   lwd  = 0.5,
                   lty  = 1,
                   col  = "red",
                   xlab = list("Date/Time",cex=1.5),
                   ylab = list("Value",cex=1.5),
                   scale=list(y="free")
              )           
plot(plot.obj)              

# one plot for each variable
plot.obj <- xyplot(values ~ chron.date | as.factor(variable),data=myData.agg.order,
                   layout = c(1,1),
                   type = "l",
                   lwd  = 0.5,
                   lty  = 1,
                   col  = "red",
                   xlab = list("Date/Time",cex=1.5),
                   ylab = list("Value",cex=1.5),
                   scale=list(y="free")
              )           
plot(plot.obj) 

dev.off()
cat("the 5-minute aggregated data has been plotted\n")



# -----------------------------------------------------------------------
# convert the long format to wide format
# -----------------------------------------------------------------------
myData.agg.wide <- reshape(myData.agg.order,v.names   = "values",		# field to be converted from long to wide
                                            idvar     = "chron.date",		# 
                                            timevar   = "variable",		# field with categories
                                            # drop      = c(),
                                            direction = "wide")

#
# write the wide formatted data out
#
FL.OUT6 <- paste(Path.Input,"myData_agg_wide.csv",sep="/")
cat(",",file=FL.OUT6,append=TRUE)
write.table(myData.agg.wide,file=FL.OUT6,sep=",",col.names=TRUE,row.names=TRUE)
cat("the 5-minute aggregated wide formatted data in [myData.agg.wide] has been written out\n")






#write.table(file="myData_raw.csv", myData.raw.order[,c(var.list,"dates")],     sep=",",col.names=TRUE,row.names=TRUE)
#write.table(file="myData_agg.csv", myData.agg.order[,c(var.list,"chron.date")],sep=",",col.names=TRUE,row.names=TRUE)
#write.table(file="myData_wide.csv",myData.raw.wide.order,                      sep=",",col.names=TRUE,row.names=TRUE)
		     