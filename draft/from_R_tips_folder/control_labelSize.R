#
# new35b_CrownePlaza_BuildingElectricity_Utility2009_retrieve_revised_0225_2011.R 
# 
# modified on Nov 24, 2010 
# (1) to NOT read the 0s values by having added a condition in data retrival.  There are 0s in the metering data which shold be excluded.
# (2) for daily utility meter: using avg*scaling*24 to replace originally used sum to avoid impact of less than should-be number of points in some hours
#     this second change will change some of the daily values
#
# Revised on June 22, 2010 by re-calculating "DateTime" field based on "Year", "Month", "Day", "Hour", "Minute" and "Second" in order to avoid problems caused by the daylighting saving time change.
#
# This script is to retrieve
# meterID 1: the whole building electricity meter
# meterID 2: the outdoor temperature
# meterID 903: Hotel Occupancy
# meterID 904: Monitored GR Occupancy
#
# it will be used to plot against the whole building electricity consumption from the individual electricity meters
#
#
# Created on January 18, 2010 retrieve meterID 1 data (i.e., whole building electrocity data to compare the sum of electricity meters)
# Renamed    on Jan 31, 2010
# Feb 11, 2010: added a retrival on the outdoor temperature
# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "/phome/nap/CrownePlaza/Data_uploaded_till_Sep27_2010/CrownePlaza_allData/0_scripts"
}else{
	Path.Current <- "j:/CrownePlaza/Data_uploaded_till_Sep27_2010/CrownePlaza_allData/0_scripts"
}
setwd(Path.Current)

# -------------------------------------------------------------------------
# 	load stuff defined in the "0_CrownePlaza_FunctionR"
# -------------------------------------------------------------------------------------------------
source(paste(Path.Current,"0_CrownePlaza_Function.R",sep="/"))


# -------------------------------------------------------------------------------------------------
#	setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.out  <- "../new35b_CrownePlaza_BuildingElectricity_Utility2009_retrieve_revised_0225_2011"		# OUTPUT processed result directory
Path.log  <- "../new0_log"										# OUTPUT log  directory
if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# ------------------------------------------------------------------------------------------------- 
# 	create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
FL.LOG <- paste(Path.log,"new35b_CrownePlaza_BuildingElectricity_Utility2009_retrieve_revised_0225_2011.log",sep="/")	# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file for data processing script [new35b_CrownePlaza_BuildingElectricity_Utility2009_retrieve.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n*********************************************************************************************",
            "*      [new35b_CrownePlaza_BuildingElectricity_Utility2009_retrieve_revised_0225_2011.R]      *",
            "*********************************************************************************************\n",sep="\n"),file=FL.LOG, append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# 	define file names
# ------------------------------------------------------------------------------------------------- 
FL.Utility2009.OBJ <- paste(Path.out,paste("CrownePlaza_BuildingElec_Utility2009.Rdata",sep=""),sep="/")			# OUTPUT 
FL.Utility2009.CSV <- paste(Path.out,paste("CrownePlaza_BuildingElec_Utility2009.CSV",sep=""),sep="/")				# OUTPUT 
FL.allMySQL.CSV    <- paste(Path.out,paste("CrownePlaza_BuildingElec_Utility2009_allMySQL.csv",sep=""),sep="/")			# OUTPUT MySQL queries
FL.PNG             <- paste(Path.out,paste("CrownePlaza_BuildingElec_Utility2009.png",sep=""),sep="/")				# OUTPUT 
if (file.exists(FL.Utility2009.OBJ)){print(paste(FL.Utility2009.OBJ," exist. Delete it!"));file.remove(FL.Utility2009.OBJ)}	# remove existing OUTPUT files
if (file.exists(FL.Utility2009.CSV)){print(paste(FL.Utility2009.CSV," exist. Delete it!"));file.remove(FL.Utility2009.CSV)}	# remove existing OUTPUT files
if (file.exists(FL.allMySQL.CSV)){print(paste(FL.allMySQL.CSV," exist. Delete it!"));file.remove(FL.allMySQL.CSV)}		# remove existing OUTPUT files
if (file.exists(FL.PNG)){print(paste(FL.PNG," exist. Delete it!"));file.remove(FL.PNG)}						# remove existing OUTPUT files

cat(paste("\n********************************************************************************************",
            "*             All MySQL queries generated by [new35b_CrownePlaza_BuildingElectricity_Utility2009_retrieve.R]           *",
            "********************************************************************************************",sep="\n"),file=FL.allMySQL.CSV, append=TRUE)
            

	
# -------------------------------------------------------------------------------------------------
#    input data files from the MySQL database directly
# -------------------------------------------------------------------------------------------------
#
# prepare MySQL connection
#
myDatabase <- "NAC_read_only"
myUser     <- "read_only"
myPassword <- "password"
ch <- odbcConnect(myDatabase,uid=myUser,pwd=myPassword)

# 
# check the tables in the database
#
myTables <- sqlTables(ch)

#
# get the rental information
#
myQuery  <- paste("SELECT *",
	          "FROM   grrentalstatus",
	          "WHERE  buildingID=100",
		   sep=" ");
myRental <- sqlQuery(ch,myQuery,errors=TRUE)


# ********************************
# hourly utility data
# ********************************
	# -----------------------------------------------------------------------------------------
	# construct a query for [hourly] aggregated data
	# the utility data consists of 0s and they should nmot be read out Nov 24, 2010
	# -----------------------------------------------------------------------------------------
	myQuery  <- paste("SELECT TimeStamp(concat(year(meterData.DateTime), '-',month(meterData.DateTime), '-',day(meterData.DateTime),' ',hour(meterData.DateTime), ':00:00')) as DateTime,",
                                   "meterData.meterID,",
                                   "meters.meterName,",
                                   "sum(meterData.Value)                     as value_sum,",
                                   "avg(meterData.Value) * meters.scaling    as value_avg,",	# hourly average need to multiply the scaling factor get the hourly sum
                                   "meters.scaling                           as scaling,",
                                   "meters.units                             as units,",
                                   "meters.meterLocation                     as loc,",
				   "year(meterData.DateTime)                 as year,", 
				   "month(meterData.DateTime)                as month,", 
				   "day(meterData.DateTime)                  as day,", 
				   "hour(meterData.DateTime)                 as hour,",                                   
                                   "dayOfWeek(meterData.DateTime)            as dayOfWeek",
                          "FROM     meterData,meters",
			  "WHERE    meters.meterID = meterdata.meterID",
			  "AND      meters.buildingID = meterdata.buildingID", 
                        "  AND      meterdata.datetime between '2009-1-1 0:0:0' and '2010-12-30 23:59:59'",	# added a time range on Nov 24, 2010			  
			  "AND      meterData.BuildingID = 100",
			  "AND      meters.meterID = 1",
			  "AND      meterdata.value > 0",			
			  "AND      meterData.Error = 0",
		  	  "GROUP BY year(meterData.DateTime), month(meterData.DateTime), day(meterData.DateTime),hour(meterdata.datetime)",
		           sep=" ")

	# 
	# save myQuery to a text file for reference
	#
	cat(myQuery,file=FL.allMySQL.CSV,append=TRUE)


		           
	#
	# execute the query (NOTE: using hourly averged data as "elec.wholeBldg"
	#
	myData.utility.hourly  <- sqlQuery(ch,myQuery,errors=TRUE)
  names(myData.utility.hourly) <- c("DateTime","meter.ID","meter.name","value.old","elec.wholeBldg","meter.scale","meter.unit","meter.loc","year","month","day","hour","day.week")
	no.records <- dim(myData.utility.hourly)[1]	
	
	cat(paste("\n meter ID1 has ",no.records," hourly records!\n",sep=""))
	cat(paste("\n meter ID1 has ",no.records," hourly records!\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)

# ********************************
# daily utility data
# ********************************
	# -----------------------------------------------------------------------------------------
	# construct a query for [daily] aggregated data
	# the utility data consists of 0s and they should nmot be read out Nov 24, 2010
	# -----------------------------------------------------------------------------------------
	myQuery  <- paste("SELECT TimeStamp(concat(year(meterData.DateTime), '-',month(meterData.DateTime), '-',day(meterData.DateTime),' ', '00:00:00')) as DateTime,",
                                   "meterData.meterID,",
                                   "meters.meterName,",
                                   "sum(meterData.Value)                       as value_sum,",
                                   "avg(meterData.Value) * meters.scaling * 24 as value_avg,",# hourly average need to multiply the scaling factor and then 24 to get the daily sum
                                   "meters.scaling                             as scaling,",
                                   "meters.units                               as units,",
                                   "meters.meterLocation                       as loc,",
				   "year(meterData.DateTime)                   as year,", 
				   "month(meterData.DateTime)                  as month,", 
				   "day(meterData.DateTime)                    as day,",                                    
                                   "dayOfWeek(meterData.DateTime)              as dayOfWeek",
                          "FROM     meterData,meters",
			  "WHERE    meters.meterID = meterdata.meterID",
			  "AND      meters.buildingID = meterdata.buildingID", 
                        "  AND      meterdata.datetime between '2009-1-1 0:0:0' and '2010-12-30 23:59:59'",	# added a time range on Nov 24, 2010			  			  
			  "AND      meterData.BuildingID = 100",
			  "AND      meters.meterID = 1",
			  "AND      meterdata.value > 0",			  
			  "AND      meterData.Error = 0",
		  	  "GROUP BY year(meterData.DateTime), month(meterData.DateTime), day(meterData.DateTime)",
		           sep=" ")

	# 
	# save myQuery to a text file for reference
	#
	cat(myQuery,file=FL.allMySQL.CSV,append=TRUE)
		           
	#
	# execute the query: output the avgSum as "elec.wholeBldg" instead of the original sum to avoid less than 48 half-hour points situation (Nov 24, 2010)
	#
	myData.utility.daily  <- sqlQuery(ch,myQuery,errors=TRUE)
  names(myData.utility.daily) <- c("DateTime","meter.ID","meter.name","value.old","elec.wholeBldg","meter.scale","meter.unit","meter.loc","year","month","day","day.week")
	no.records <- dim(myData.utility.daily)[1]	
	
	cat(paste("\n meter ID1 has ",no.records," daily records!\n",sep=""))
	cat(paste("\n meter ID1 has ",no.records," daily records!\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)
	

#
# add a chron data time object for [daily] data
#
chron.daily <- chron(dates  = paste(myData.utility.daily[,"month"],
                                 myData.utility.daily[,"day"],
                                 myData.utility.daily[,"year"],sep="/"), 
                     times  = rep("00:00:00",length(year)), 
                     format = c('m/d/y','h:m:s'))
myData.utility.daily <- cbind(myData.utility.daily,chron.daily=chron.daily)

#
# add a chron data time object for [hourly]y data
#
chron.hourly <- chron(dates  = paste(myData.utility.hourly[,"month"],
                                     myData.utility.hourly[,"day"],
                                     myData.utility.hourly[,"year"],sep="/"), 
                      times  = rep("00:00:00",length(year)), 
                      format = c('m/d/y','h:m:s'))
myData.utility.hourly <- cbind(myData.utility.hourly,chron.hourly=chron.hourly)



# ********************************
# hourly outdoor air temp data
# ********************************
	#
	# add a retrival of the outdoor air temperature
	#


	# -----------------------------------------------------------------------------------------
	# construct a query for [hourly] average, minimum and maximum temperature
	# -----------------------------------------------------------------------------------------
	myQuery  <- paste("SELECT  meterData_avg.DateTime,",
				  "meterData_avg.meterID,",
				  "meterData_avg.meterName,",
		            paste("meterData_avg.value_avg                         as 'value_avg',",sep=""),
		            paste("meterData_avg.value_min                         as 'value_min',",sep=""),
		            paste("meterData_avg.value_max                         as 'value_max',",sep=""),
				  "meterData_avg.scaling                           as scaling,",
				  "meterData_avg.units                             as unit,",
				  "meterData_avg.loc                               as loc,",
				  "year(meterData_avg.DateTime)                    as year,", 
				  "month(meterData_avg.DateTime)                   as month,", 
				  "day(meterData_avg.DateTime)                     as day,", 
				  "hour(meterData_avg.DateTime)                    as hour,",                                   
                                  "dayOfWeek(meterData_avg.DateTime)               as dayOfWeek",				  

	                  "FROM   (SELECT TimeStamp(concat(year(meterData.DateTime), '-',month(meterData.DateTime), '-',day(meterData.DateTime),' ',hour(meterData.DateTime), ':00:00')) as DateTime,",
                                         "meterData.meterID,",
                                         "meters_tmp.meterName,",
                                         "avg(meterData.Value)     as value_avg,",
                                         "min(meterData.Value)     as value_min,",
                                         "max(meterData.Value)     as value_max,",
                                         "meters_tmp.scaling       as scaling,",
                                         "meters_tmp.units         as units,",
                                         "meters_tmp.meterLocation as loc",
                                  "FROM   meterData,",
				  paste("(SELECT meterName,scaling,units,meterLocation FROM meters WHERE meterID = 2) AS meters_tmp",sep=""),

				   "WHERE meterData.Error = 0",
                                   "AND   meterdata.datetime between '2009-10-1 0:0:0' and '2010-9-30 23:59:59'",	# added a time range on Nov 24, 2010			  				  
				   "AND   meterData.BuildingID = 100",
			     paste("AND   meterData.MeterID = 2",sep=""),
				   "GROUP BY year(meterData.DateTime), month(meterData.DateTime), day(meterData.DateTime), hour(meterData.DateTime)) as meterData_avg",
		           sep=" ")
		           
	#
	# execute the query
	#
	myData.Tout.hourly  <- sqlQuery(ch,myQuery,errors=TRUE)
  names(myData.Tout.hourly) <- c("DateTime","meter.ID","meter.name","outdoor.airT.avg","outdoor.airT.min","outdoor.airT.max","meter.scale","meter.unit","meter.loc","year","month","day","hour","day.week")
	no.records <- dim(myData.Tout.hourly)[1]	
	
	# 
	# save myQuery to a text file for reference
	#
	cat(paste("\n meter ID2 has ",no.records," hourly records!\n",sep=""))
	cat(paste("\n meter ID2 has ",no.records," hourly records!\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)
	cat(myQuery,file=FL.allMySQL.CSV,append=TRUE)

# ********************************
# daily outdoor air temp data
# ********************************
	# -----------------------------------------------------------------------------------------
	# construct a query for [daily] average, min and max Temperature
	# -----------------------------------------------------------------------------------------
	myQuery  <- paste("SELECT  meterData_avg.DateTime,",
				  "meterData_avg.meterID,",
				  "meterData_avg.meterName,",
		            paste("meterData_avg.value_avg                         as 'value_avg',",sep=""),
		            paste("meterData_avg.value_min                         as 'value_min',",sep=""),
		            paste("meterData_avg.value_max                         as 'value_max',",sep=""),
				  "meterData_avg.scaling                           as scaling,",
				  "meterData_avg.units                             as unit,",
				  "meterData_avg.loc                               as loc,",
				  "year(meterData_avg.DateTime)                    as year,", 
				  "month(meterData_avg.DateTime)                   as month,", 
				  "day(meterData_avg.DateTime)                     as day,",                                 
                                  "dayOfWeek(meterData_avg.DateTime)               as dayOfWeek",				  

	                  "FROM   (SELECT TimeStamp(concat(year(meterData.DateTime), '-',month(meterData.DateTime), '-',day(meterData.DateTime),' ', '00:00:00')) as DateTime,",
                                         "meterData.meterID,",
                                         "meters_tmp.meterName,",
                                         "avg(meterData.Value)     as value_avg,",
                                         "min(meterData.Value)     as value_min,",
                                         "max(meterData.Value)     as value_max,",
                                         "meters_tmp.scaling       as scaling,",
                                         "meters_tmp.units         as units,",
                                         "meters_tmp.meterLocation as loc",
                                  "FROM   meterData,",
				  paste("(SELECT meterName,scaling,units,meterLocation FROM meters WHERE meterID = 2) AS meters_tmp",sep=""),

				   "WHERE meterData.Error = 0",
				   "AND   meterData.BuildingID = 100",
                                   "AND   meterdata.datetime between '2009-10-1 0:0:0' and '2010-9-30 23:59:59'",	# added a time range on Nov 24, 2010			  				  				   
			     paste("AND   meterData.MeterID = 2",sep=""),
				   "GROUP BY year(meterData.DateTime), month(meterData.DateTime), day(meterData.DateTime)) as meterData_avg",
		           sep=" ")
		           
	#
	# execute the query
	#
	myData.Tout.daily  <- sqlQuery(ch,myQuery,errors=TRUE)
  names(myData.Tout.daily) <- c("DateTime","meter.ID","meter.name","outdoor.airT.avg","outdoor.airT.min","outdoor.airT.max","meter.scale","meter.unit","meter.loc","year","month","day","day.week")
	no.records <- dim(myData.Tout.daily)[1]	
	
	# 
	# save myQuery to a text file for reference
	#
	cat(paste("\n meter ID2 has ",no.records," daily records!\n",sep=""))
	cat(paste("\n meter ID2 has ",no.records," daily records!\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)
	cat(myQuery,file=FL.allMySQL.CSV,append=TRUE)
	
	
	
#
# add a chron data time object for [daily] data
#
chron.daily <- chron(dates  = paste(myData.Tout.daily[,"month"],
                                    myData.Tout.daily[,"day"],
                                    myData.Tout.daily[,"year"],sep="/"), 
                     times  = rep("00:00:00",length(year)), 
                     format = c('m/d/y','h:m:s'))
myData.Tout.daily <- cbind(myData.Tout.daily,chron.daily=chron.daily)

#
# add a chron data time object for [hourly]y data
#
chron.hourly <- chron(dates  = paste(myData.Tout.hourly[,"month"],
                                     myData.Tout.hourly[,"day"],
                                     myData.Tout.hourly[,"year"],sep="/"), 
                      times  = rep("00:00:00",length(year)), 
                      format = c('m/d/y','h:m:s'))
myData.Tout.hourly <- cbind(myData.Tout.hourly,chron.hourly=chron.hourly)



# *************************************************************************************************
# add a "dayType" field: 2*6 (Mon*Fri) into 0 (Weekday)
#                        7   (Sat)     into 1 (Saturday)
#                        1   (Sun)     into 2 (Sunday)
# *************************************************************************************************
tmp <- rep(1,dim(myData.utility.daily)[1])					# monday - Friday: 	weekday
tmp[(myData.utility.daily[,"day.week"] == 7)] <- 0				# saturday:		weekend
tmp[(myData.utility.daily[,"day.week"] == 1)] <- 0				# sunday:		weekend
myData.utility.daily <- cbind(myData.utility.daily,day.type = tmp)

tmp <- rep(1,dim(myData.utility.hourly)[1])					# monday - Friday: 	weekday
tmp[(myData.utility.hourly[,"day.week"] == 7)] <- 0				# saturday:		weekend
tmp[(myData.utility.hourly[,"day.week"] == 1)] <- 0				# sunday:		weekend
myData.utility.hourly <- cbind(myData.utility.hourly,day.type = tmp)

tmp <- rep(1,dim(myData.Tout.daily)[1])						# monday - Friday: 	weekday
tmp[(myData.Tout.daily[,"day.week"] == 7)] <- 0					# saturday:		weekend
tmp[(myData.Tout.daily[,"day.week"] == 1)] <- 0					# sunday:		weekend
myData.Tout.daily <- cbind(myData.Tout.daily,day.type = tmp)


tmp <- rep(1,dim(myData.Tout.hourly)[1])					# monday - Friday: 	weekday
tmp[(myData.Tout.hourly[,"day.week"] == 7)] <- 0				# saturday:		weekend
tmp[(myData.Tout.hourly[,"day.week"] == 1)] <- 0				# sunday:		weekend
myData.Tout.hourly <- cbind(myData.Tout.hourly,day.type = tmp)


cat(paste("\nA \"dataType\" field is added!\n",sep=""))
cat(paste("\nA \"dataType\" field is added!\n",sep=""),file=FL.LOG,append=TRUE)




# *************************************************************************************************
# for weekly plot, we plot the sequence from Monday to Sunday
# we need to convert the current day.week code to a new day.week.code
# current mon-saturday 2 - 7	new code monday - saturday 0 - 5
#         sunday       1	         sunday            6
# *************************************************************************************************
day.week.new <- myData.utility.daily[,"day.week"] - 2
day.week.new[day.week.new<0] <- day.week.new[day.week.new<0] + 7	# turn sunday to 6
myData.utility.daily <- data.frame(myData.utility.daily,day.week.new = day.week.new)

day.week.new <- myData.utility.hourly[,"day.week"] - 2
day.week.new[day.week.new<0] <- day.week.new[day.week.new<0] + 7	# turn sunday to 6
myData.utility.hourly <- data.frame(myData.utility.hourly,day.week.new = day.week.new)

day.week.new <- myData.Tout.daily[,"day.week"] - 2
day.week.new[day.week.new<0] <- day.week.new[day.week.new<0] + 7	# turn sunday to 6
myData.Tout.daily <- data.frame(myData.Tout.daily,day.week.new = day.week.new)

day.week.new <- myData.Tout.hourly[,"day.week"] - 2
day.week.new[day.week.new<0] <- day.week.new[day.week.new<0] + 7	# turn sunday to 6
myData.Tout.hourly <- data.frame(myData.Tout.hourly,day.week.new = day.week.new)


# *************************************************************************************************
# convert day.week and day.type into factors
# *************************************************************************************************		
myData.utility.daily[,"day.week"]  <- factor(myData.utility.daily[,"day.week"],levels=week.label,   labels=week.names)		# convert 1-7 to Sun,Mon,...,Sat
myData.utility.daily[,"day.type"]  <- factor(myData.utility.daily[,"day.type"],levels=dayType.label,labels=dayType.names)	# convert 1 & 7 to "wkend and 2-6 to "wkday"

myData.utility.hourly[,"day.week"] <- factor(myData.utility.hourly[,"day.week"],levels=week.label,   labels=week.names)		# convert 1-7 to Sun,Mon,...,Sat
myData.utility.hourly[,"day.type"] <- factor(myData.utility.hourly[,"day.type"],levels=dayType.label,labels=dayType.names)	# convert 1 & 7 to "wkend and 2-6 to "wkday"

myData.Tout.daily[,"day.week"]     <- factor(myData.Tout.daily[,"day.week"],levels=week.label,   labels=week.names)		# convert 1-7 to Sun,Mon,...,Sat
myData.Tout.daily[,"day.type"]     <- factor(myData.Tout.daily[,"day.type"],levels=dayType.label,labels=dayType.names)		# convert 1 & 7 to "wkend and 2-6 to "wkday"

myData.Tout.hourly[,"day.week"]    <- factor(myData.Tout.hourly[,"day.week"],levels=week.label,   labels=week.names)		# convert 1-7 to Sun,Mon,...,Sat
myData.Tout.hourly[,"day.type"]    <- factor(myData.Tout.hourly[,"day.type"],levels=dayType.label,labels=dayType.names)		# convert 1 & 7 to "wkend and 2-6 to "wkday"

cat(paste("\nconverted [day.type] and [day.week] into factors!\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("\nconverted [day.type] and [day.week] into factors!\n",sep=""))	





# ********************************
# hotel occupancy data
# ********************************
	# -----------------------------------------------------------------------------------------
	# construct a query for hotel occupancy percentage
	# -----------------------------------------------------------------------------------------
	myQuery  <- paste("SELECT * FROM meterdata WHERE buildingID = 100 AND meterID = 903 AND ERROR = 0",sep=" ")
		           
	#
	# execute the query
	#
	meter903  <- sqlQuery(ch,myQuery,errors=TRUE)
  names(meter903) <- c("Building.ID","meter.ID","DateTime","Value","Error")
	no.records <- dim(meter903)[1]	
	
	# 
	# save myQuery to a text file for reference
	#
	cat(paste("\n meter ID903 has ",no.records," daily records!\n",sep=""))
	cat(paste("\n meter ID903 has ",no.records," daily records!\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)
	cat(myQuery,file=FL.allMySQL.CSV,append=TRUE)



	#
	# add a chron data time object for [daily] data
	#
	YMD     <- unlist(strsplit(as.character(meter903[,"DateTime"]),"-"))
	year    <- as.numeric(YMD[seq(from=1,to=length(YMD),by=3)])
	month   <- as.numeric(YMD[seq(from=2,to=length(YMD),by=3)])
	day     <- as.numeric(YMD[seq(from=3,to=length(YMD),by=3)])
	chron.daily <- chron(dates=paste(month,day,year,sep="/"), times="00:00:00", format=c('m/d/y','h:m:s'))
	meter903 <- cbind(meter903,
			      year  = year,
			      month = month,
			      day   = day,
			      chron.daily=chron.daily)




# ********************************
# monitored GR occupancy data
# ********************************
	# -----------------------------------------------------------------------------------------
	# construct a query for hotel occupancy percentage
	# -----------------------------------------------------------------------------------------
	myQuery  <- paste("SELECT * FROM meterdata WHERE buildingID = 100 AND meterID = 904 AND ERROR = 0",sep=" ")
		           
	#
	# execute the query
	#
	meter904  <- sqlQuery(ch,myQuery,errors=TRUE)
  names(meter904) <- c("Building.ID","meter.ID","DateTime","Value","Error")
	no.records <- dim(meter904)[1]	
	
	# 
	# save myQuery to a text file for reference
	#
	cat(paste("\n meter ID904 has ",no.records," daily records!\n",sep=""))
	cat(paste("\n meter ID904 has ",no.records," daily records!\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)
	cat(myQuery,file=FL.allMySQL.CSV,append=TRUE)


	#
	# add a chron data time object for [daily] data
	#
	#
	YMD     <- unlist(strsplit(as.character(meter904[,"DateTime"]),"-"))
	year    <- as.numeric(YMD[seq(from=1,to=length(YMD),by=3)])
	month   <- as.numeric(YMD[seq(from=2,to=length(YMD),by=3)])
	day     <- as.numeric(YMD[seq(from=3,to=length(YMD),by=3)])
	chron.daily <- chron(dates=paste(month,day,year,sep="/"), times="00:00:00", format=c('m/d/y','h:m:s'))
	meter904 <- cbind(meter904,
			      year  = year,
			      month = month,
			      day   = day,
			      chron.daily=chron.daily)




# ****************************************************************
# output whole building electricity data 
# ****************************************************************
cat("daily elecBldg,",file = FL.Utility2009.CSV,  append = TRUE)
write.table(myData.utility.daily,file = FL.Utility2009.CSV,sep = ",", col.names = TRUE, row.names = TRUE, append = TRUE)	
cat(paste("daily whole building electricity averaged data are outputted to a csv file!\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("daily whole building electricity averaged data are outputted to a csv file!\n",sep=""))


cat("\nhourly elecBldg,",file = FL.Utility2009.CSV,  append = TRUE)
write.table(myData.utility.hourly,file = FL.Utility2009.CSV,sep = ",", col.names = TRUE, row.names = TRUE, append = TRUE)	
cat(paste("hourly whole building electricity averaged data are outputted to a csv file!\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("hourly whole building electricity averaged data are outputted to a csv file!\n",sep=""))


# ****************************************************************
# output outdoor air Temperature data 
# ****************************************************************
cat("daily outdoor air T,",file = FL.Utility2009.CSV,  append = TRUE)
write.table(myData.Tout.daily,file = FL.Utility2009.CSV,sep = ",", col.names = TRUE, row.names = TRUE, append = TRUE)	
cat(paste("daily outdoor air temperature averaged data are outputted to a csv file!\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("daily outdoor air temperature averaged data are outputted to a csv file!\n",sep=""))


cat("\nhourly outdoor air T,",file = FL.Utility2009.CSV,  append = TRUE)
write.table(myData.Tout.hourly,file = FL.Utility2009.CSV,sep = ",", col.names = TRUE, row.names = TRUE, append = TRUE)	
cat(paste("hourly outdoor air temperature averaged data are outputted to a csv file!\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("hourly outdoor air temperature averaged data are outputted to a csv file!\n",sep=""))




#
# save the data
#
save(myData.utility.daily,myData.utility.hourly,myData.Tout.daily,myData.Tout.hourly,meter903,meter904,file=FL.Utility2009.OBJ)
cat(paste("saved the hourly and daily aggregated data into a R object file!\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("saved the hourly and daily aggregated data into a R object file!\n",sep=""))



#
# prepare plot of Figure 1
#
myData.jan2009 <- myData.utility.hourly[myData.utility.hourly[,"year"]==2009 & myData.utility.hourly[,"month"]==1,]
myData.may2009 <- myData.utility.hourly[myData.utility.hourly[,"year"]==2009 & myData.utility.hourly[,"month"]==5,]
myData.aug2009 <- myData.utility.hourly[myData.utility.hourly[,"year"]==2009 & myData.utility.hourly[,"month"]==8,]
myData.oct2009 <- myData.utility.hourly[myData.utility.hourly[,"year"]==2009 & myData.utility.hourly[,"month"]==10,]


#
# loadprofile of Jan 2009
#
	# *********************************************************************************
	list.4agg <- list(myData.jan2009[,"day.week"],myData.jan2009[,"hour"])
	name.4agg <- c("day.week","hour")
	Jan2009day.week  <- aggregate(myData.jan2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(Jan2009day.week) <- c(name.4agg,"elec.wholeBldg")

	# *********************************************************************************
	list.4agg <- list(myData.jan2009[,"day.type"],myData.jan2009[,"hour"])
	name.4agg <- c("day.type","hour")
	Jan2009day.type  <- aggregate(myData.jan2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(Jan2009day.type) <- c(name.4agg,"elec.wholeBldg")

	# *********************************************************************************
	list.4agg <- list(myData.jan2009[,"hour"])
	name.4agg <- c("hour")
	Jan2009avg  <- aggregate(myData.jan2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(Jan2009avg) <- c(name.4agg,"elec.wholeBldg")


#
# loadprofile of may 2009
#
	# *********************************************************************************
	list.4agg <- list(myData.may2009[,"day.week"],myData.may2009[,"hour"])
	name.4agg <- c("day.week","hour")
	May2009day.week  <- aggregate(myData.may2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(May2009day.week) <- c(name.4agg,"elec.wholeBldg")

	# *********************************************************************************
	list.4agg <- list(myData.may2009[,"day.type"],myData.may2009[,"hour"])
	name.4agg <- c("day.type","hour")
	May2009day.type  <- aggregate(myData.may2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(May2009day.type) <- c(name.4agg,"elec.wholeBldg")

	# *********************************************************************************
	list.4agg <- list(myData.may2009[,"hour"])
	name.4agg <- c("hour")
	May2009avg  <- aggregate(myData.may2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(May2009avg) <- c(name.4agg,"elec.wholeBldg")


#
# loadprofile of aug 2009
#
	# *********************************************************************************
	list.4agg <- list(myData.aug2009[,"day.week"],myData.aug2009[,"hour"])
	name.4agg <- c("day.week","hour")
	Aug2009day.week  <- aggregate(myData.aug2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(Aug2009day.week) <- c(name.4agg,"elec.wholeBldg")

	# *********************************************************************************
	list.4agg <- list(myData.aug2009[,"day.type"],myData.aug2009[,"hour"])
	name.4agg <- c("day.type","hour")
	Aug2009day.type  <- aggregate(myData.aug2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(Aug2009day.type) <- c(name.4agg,"elec.wholeBldg")

	# *********************************************************************************
	list.4agg <- list(myData.aug2009[,"hour"])
	name.4agg <- c("hour")
	Aug2009avg  <- aggregate(myData.aug2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(Aug2009avg) <- c(name.4agg,"elec.wholeBldg")



#
# loadprofile of oct 2009
#
	# *********************************************************************************
	list.4agg <- list(myData.oct2009[,"day.week"],myData.oct2009[,"hour"])
	name.4agg <- c("day.week","hour")
	Oct2009day.week  <- aggregate(myData.oct2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(Oct2009day.week) <- c(name.4agg,"elec.wholeBldg")

	# *********************************************************************************
	list.4agg <- list(myData.oct2009[,"day.type"],myData.oct2009[,"hour"])
	name.4agg <- c("day.type","hour")
	Oct2009day.type  <- aggregate(myData.oct2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(Oct2009day.type) <- c(name.4agg,"elec.wholeBldg")

	# *********************************************************************************
	list.4agg <- list(myData.oct2009[,"hour"])
	name.4agg <- c("hour")
	Oct2009avg  <- aggregate(myData.oct2009[,"elec.wholeBldg",drop=FALSE],list.4agg,FUN = mean,na.rm=TRUE)
  names(Oct2009avg) <- c(name.4agg,"elec.wholeBldg")




# split the page into 4 blocks
png(file = FL.PNG,width=960,height=960,unit="px",bg="transparent",pointsize=12)
	par(mfrow=c(2,2),mar=c(4,5,3,3)+0.3,pty="m")




	y.limit <- c(0,900)
	x.limit <- c(0,23)
	#
	# January 2009
	#
	plot(Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Mon","hour"],
	     Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Mon","elec.wholeBldg"],
	     xaxt = "n",
	     col = "red",
	     pch = 16,cex=1.5,
	     type = "b", lty = 1,lwd = 0.25,
	     cex.main = 2.5,cex.sub=1.5,cex.axis = 1.5, cex.lab = 2.0,
	     ylim = y.limit,
	     xlim = x.limit,
	     xlab = "hour",
	     ylab = "kW",
	     main = "January")     
	axis(1,at=seq(0,23),labels=seq(0,23),las=1,cex.lab=2.0,cex.axis=1.5)
	abline(h=c(100,200,300,400,500,600,700,800),col="grey",lty=2,lwd=0.5)
	abline(v=seq(0,23),col="grey",lty=2,lwd=0.5)


	lines(Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Tue",  "hour"],Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Tue",  "elec.wholeBldg"],col = "blue",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Wed",  "hour"],Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Wed",  "elec.wholeBldg"],col = "green",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Thu",  "hour"],Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Thu",  "elec.wholeBldg"],col = "cyan",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Fri",  "hour"],Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Fri",  "elec.wholeBldg"],col = "magenta",pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Sat",  "hour"],Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Sat",  "elec.wholeBldg"],col = "purple", pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Sun",  "hour"],Jan2009day.week[as.character(Jan2009day.week[,"day.week"]) == "Sun",  "elec.wholeBldg"],col = "brown",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Jan2009day.type[as.character(Jan2009day.type[,"day.type"]) == "wkday","hour"],Jan2009day.type[as.character(Jan2009day.type[,"day.type"]) == "wkday","elec.wholeBldg"],col = "violet", pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Jan2009day.type[as.character(Jan2009day.type[,"day.type"]) == "wkend","hour"],Jan2009day.type[as.character(Jan2009day.type[,"day.type"]) == "wkend","elec.wholeBldg"],col = "gold",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Jan2009avg[,"hour"],Jan2009avg[,"elec.wholeBldg"],col = "black",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)


	legend(min(x.limit), 500,legend=c("Monday","Tueday","Wednesday","Thursday","Friday","Saturday","Sunday","Weekday","Weekend","Average"),
				    col=c("red","blue","green","cyan","magenta","purple","brown","violet","gold","black"),
				    pch=c(16,16,16,16,16,16,16,16,16,16),
				    text.col = c("red","blue","green","cyan","magenta","purple","brown","violet","gold","black"))   

	#
	# May 2009
	#
	plot(May2009day.week[as.character(May2009day.week[,"day.week"]) == "Mon","hour"],
	     May2009day.week[as.character(May2009day.week[,"day.week"]) == "Mon","elec.wholeBldg"],
	     xaxt = "n",
	     col = "red",
	     pch = 16,cex=1.5,
	     type = "b", lty = 1,lwd = 0.25,
	     cex.main = 2.5,cex.sub=1.5,cex.axis = 1.5, cex.lab = 2.0,
	     ylim = y.limit,
	     xlim = x.limit,
	     xlab = "hour",
	     ylab = "kW",
	     main = "May")     
	axis(1,at=seq(0,23),labels=seq(0,23),las=1,cex.lab=2.0,cex.axis=1.5)
	abline(h=c(100,200,300,400,500,600,700,800),col="grey",lty=2,lwd=0.5)
	abline(v=seq(0,23),col="grey",lty=2,lwd=0.5)



	lines(May2009day.week[as.character(May2009day.week[,"day.week"]) == "Tue",  "hour"],May2009day.week[as.character(May2009day.week[,"day.week"]) == "Tue",  "elec.wholeBldg"],col = "blue",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(May2009day.week[as.character(May2009day.week[,"day.week"]) == "Wed",  "hour"],May2009day.week[as.character(May2009day.week[,"day.week"]) == "Wed",  "elec.wholeBldg"],col = "green",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(May2009day.week[as.character(May2009day.week[,"day.week"]) == "Thu",  "hour"],May2009day.week[as.character(May2009day.week[,"day.week"]) == "Thu",  "elec.wholeBldg"],col = "cyan",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(May2009day.week[as.character(May2009day.week[,"day.week"]) == "Fri",  "hour"],May2009day.week[as.character(May2009day.week[,"day.week"]) == "Fri",  "elec.wholeBldg"],col = "magenta",pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(May2009day.week[as.character(May2009day.week[,"day.week"]) == "Sat",  "hour"],May2009day.week[as.character(May2009day.week[,"day.week"]) == "Sat",  "elec.wholeBldg"],col = "purple", pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(May2009day.week[as.character(May2009day.week[,"day.week"]) == "Sun",  "hour"],May2009day.week[as.character(May2009day.week[,"day.week"]) == "Sun",  "elec.wholeBldg"],col = "brown",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(May2009day.type[as.character(May2009day.type[,"day.type"]) == "wkday","hour"],May2009day.type[as.character(May2009day.type[,"day.type"]) == "wkday","elec.wholeBldg"],col = "violet", pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(May2009day.type[as.character(May2009day.type[,"day.type"]) == "wkend","hour"],May2009day.type[as.character(May2009day.type[,"day.type"]) == "wkend","elec.wholeBldg"],col = "gold",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(May2009avg[,"hour"],May2009avg[,"elec.wholeBldg"],col = "black",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)


	#
	# August 2009
	#
	plot(Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Mon","hour"],
	     Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Mon","elec.wholeBldg"],
	     xaxt = "n",
	     col = "red",
	     pch = 16,cex=1.5,
	     type = "b", lty = 1,lwd = 0.25,
	     cex.main = 2.5,cex.sub=1.5,cex.axis = 1.5, cex.lab = 2.0,
	     ylim = y.limit,
	     xlim = x.limit,
	     xlab = "hour",
	     ylab = "kW",
	     main = "August")     
	axis(1,at=seq(0,23),labels=seq(0,23),las=1,cex.lab=2.0,cex.axis=1.5)
	abline(h=c(100,200,300,400,500,600,700,800),col="grey",lty=2,lwd=0.5)
	abline(v=seq(0,23),col="grey",lty=2,lwd=0.5)


	lines(Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Tue",  "hour"],Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Tue",  "elec.wholeBldg"],col = "blue",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Wed",  "hour"],Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Wed",  "elec.wholeBldg"],col = "green",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Thu",  "hour"],Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Thu",  "elec.wholeBldg"],col = "cyan",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Fri",  "hour"],Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Fri",  "elec.wholeBldg"],col = "magenta",pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Sat",  "hour"],Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Sat",  "elec.wholeBldg"],col = "purple", pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Sun",  "hour"],Aug2009day.week[as.character(Aug2009day.week[,"day.week"]) == "Sun",  "elec.wholeBldg"],col = "brown",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Aug2009day.type[as.character(Aug2009day.type[,"day.type"]) == "wkday","hour"],Aug2009day.type[as.character(Aug2009day.type[,"day.type"]) == "wkday","elec.wholeBldg"],col = "violet", pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Aug2009day.type[as.character(Aug2009day.type[,"day.type"]) == "wkend","hour"],Aug2009day.type[as.character(Aug2009day.type[,"day.type"]) == "wkend","elec.wholeBldg"],col = "gold",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Aug2009avg[,"hour"],Aug2009avg[,"elec.wholeBldg"],col = "black",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)



	#
	# October 2009
	#
	plot(Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Mon","hour"],
	     Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Mon","elec.wholeBldg"],
	     xaxt = "n",
	     col = "red",
	     pch = 16,cex=1.5,
	     type = "b", lty = 1,lwd = 0.25,
	     cex.main = 2.5,cex.sub=1.5,cex.axis = 1.5, cex.lab = 2.0,
	     ylim = y.limit,
	     xlim = x.limit,
	     xlab = "hour",
	     ylab = "kW",
	     main = "October")     
	axis(1,at=seq(0,23),labels=seq(0,23),las=1,cex.lab=2.0,cex.axis=1.5)
	abline(h=c(100,200,300,400,500,600,700,800),col="grey",lty=2,lwd=0.5)
	abline(v=seq(0,23),col="grey",lty=2,lwd=0.5)


	lines(Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Tue",  "hour"],Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Tue",  "elec.wholeBldg"],col = "blue",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Wed",  "hour"],Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Wed",  "elec.wholeBldg"],col = "green",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Thu",  "hour"],Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Thu",  "elec.wholeBldg"],col = "cyan",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Fri",  "hour"],Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Fri",  "elec.wholeBldg"],col = "magenta",pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Sat",  "hour"],Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Sat",  "elec.wholeBldg"],col = "purple", pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Sun",  "hour"],Oct2009day.week[as.character(Oct2009day.week[,"day.week"]) == "Sun",  "elec.wholeBldg"],col = "brown",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Oct2009day.type[as.character(Oct2009day.type[,"day.type"]) == "wkday","hour"],Oct2009day.type[as.character(Oct2009day.type[,"day.type"]) == "wkday","elec.wholeBldg"],col = "violet", pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Oct2009day.type[as.character(Oct2009day.type[,"day.type"]) == "wkend","hour"],Oct2009day.type[as.character(Oct2009day.type[,"day.type"]) == "wkend","elec.wholeBldg"],col = "gold",   pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
	lines(Oct2009avg[,"hour"],Oct2009avg[,"elec.wholeBldg"],col = "black",  pch = 16,cex=1.5,type = "b", lty = 1,lwd = 0.25)
dev.off()






# -------------------------------------------------------------------------------------------------
# close the database
# -------------------------------------------------------------------------------------------------
odbcClose(ch)
cat(paste("the connection to database ",myDatabase," is closed!\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("the connection to database ",myDatabase," is closed!\n",sep=""))

# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\nnew35b_CrownePlaza_BuildingElectricity_Utility2009_retrieve_revised_0225_2011.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\nnew35b_CrownePlaza_BuildingElectricity_Utility2009_retrieve_revised_0225_2011.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [new35b_CrownePlaza_BuildingElectricity_Utility2009_retrieve_revised_0225_2011.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [new35b_CrownePlaza_BuildingElectricity_Utility2009_retrieve_revised_0225_2011.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)


