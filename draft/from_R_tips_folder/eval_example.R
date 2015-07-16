# -------------------------------------------------------------------------------------------------
# example 1
# -------------------------------------------------------------------------------------------------
#
command.string_save <- "save("
for (guest.room in guest.rooms)
{
	command.string_save <- paste(command.string_save,
	                             paste(guest.room,".data",sep=""),",",
	                             paste(guest.room,".loadprofile",sep=""),",",
	                             sep="")	
}       # end of guest room loop              


command.string_save <- paste(command.string_save,"file=FL.GR_Data.OBJ)",sep="")
EVAL(parse(text=command.string_save))
cat(command.string_save,file=FL.GR_Query.CSV,append=TRUE)
	cat("\n",file=FL.GR_Query.CSV,append=TRUE)
	
	
# -------------------------------------------------------------------------------------------------
# example 2
# -------------------------------------------------------------------------------------------------
#
command.string_save <- "save("

# loopping through categories
idx.meter <- 1
for (meter in meters)
{
	myData.tmp <- paste("myData.",meter.name,sep="")
	command.string <- paste(myData.tmp," <- myData",sep="")
	EVAL(parse(text=command.string))
	cat(paste("\n",command.string,"\n",sep=""),file=FL.LOG,append=TRUE)

	# put the data frame in the save command string	
	command.string_save <- paste(command.string_save,myData.tmp,",",sep="")
}
command.string_save <- paste(command.string_save,"myData.merged,myData.merged.daily,file=FL.DATA.OBJ)",sep="")
EVAL(parse(text=command.string_save))
cat("\n",file=FL.LOG,append=TRUE)
cat(command.string,file=FL.LOG,append=TRUE)	
	
# -------------------------------------------------------------------------------------------------
# example 3
# -------------------------------------------------------------------------------------------------
#
# loopping through the PTHP meters and retrieve hourly aggregated data from the database
#
meter.no <- 0
for (meter.ID.this in myMeter.PTHP[,"meter.ID"])
{
       meter.dataframe.this <- paste("meter.ID",meter.ID.this,sep="")							# assign a data frame for the meter with non-zero records
	if (no.records > 0)
	{
		meter.no <- meter.no + 1
		meter.dataframes[meter.no,"meter.ID"]   <- meter.dataframe.this		# assign all available data frames into an array
		meter.dataframes[meter.no,"meter.name"] <- meter.name.this		# assign all available data frames into an array
		command.string  <- paste(meter.dataframe.this," <- myData",sep="")	# assign the retrived data to this data frame
		EVAL(parse(text=command.string))
		cat(command.string,file=FL.allMySQL.CSV,append=TRUE)
	}
}

command.string  <- paste("merged.tmp <- ",meter.dataframes[1,"meter.ID"],"[,c(\"DateTime\",\"value.new\")]",sep="")	# assign the 1st data frame to [merged.tmp] with only "DateTime" and "value.new" retained.
EVAL(parse(text=command.string))
names(merged.tmp) <- c("DateTime",meter.dataframes[1,"meter.ID"])

for (meter.idx in seq(from=2,to=dim(meter.dataframes)[1]))
{
	if (meter.dataframes[meter.idx,"meter.ID"] != "")
	{
		cat(paste("align and merge meter",meter.idx,":[",meter.dataframes[meter.idx,"meter.ID"],"](",meter.dataframes[meter.idx,"meter.name"],")\n",sep=""))
		cat(paste("align and merge meter",meter.idx,":[",meter.dataframes[meter.idx,"meter.ID"],"](",meter.dataframes[meter.idx,"meter.name"],")\n",sep=""),file=FL.LOG,append=TRUE)
		command.string  <- paste("B <- ",meter.dataframes[meter.idx,"meter.ID"],"[,c(\"DateTime\",\"value.new\")]",sep="")	# assign each of the rest data frame to B with only "DateTime" and "value.new" retained.
		EVAL(parse(text=command.string))
		names(B) <- c("DateTime",meter.dataframes[meter.idx,"meter.ID"])
	
		merged.tmp <- merge(merged.tmp,B,by.x=c("DateTime"),by.y=c("DateTime"),all.x=TRUE,all.y=TRUE)
	}
}
merged.hourly.data <- merged.tmp
cat(paste("finishing merging all meters!\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("finishing merging all meters!\n",sep=""))


# -------------------------------------------------------------------------------------------------
# example 4
# -------------------------------------------------------------------------------------------------
          meter.dataframes  <- data.frame(meter.ID   = rep(NaN,dim(myMeter.elec)[1]),
                                          meter.name = rep(NaN,dim(myMeter.elec)[1]))
                                          
meter.no <- 0
for (meter.ID.this in myMeter.elec[,"meter.ID"])
{
        meter.dataframe.this <- paste("meter.ID",meter.ID.this,sep="")							# assign a data frame for the meter with non-zero records
	if (no.records > 0)
	{
		meter.no <- meter.no + 1
		meter.dataframes[meter.no,"meter.ID"]   <- meter.dataframe.this		# assign all available data frames into an array
		meter.dataframes[meter.no,"meter.name"] <- meter.name.this		# assign all available data frames into an array
		
		# *********************************************************************************
		# Use the materID as the data frame name to store the data in [myData]
		command.string  <- paste(meter.dataframe.this," <- myData",sep="")	# assign the retrived data to this data frame
		EVAL(parse(text=command.string))
		cat(command.string,file=FL.allMySQL.CSV,append=TRUE)
	}
}
command.string  <- paste("A <- ",meter.dataframes[1,"meter.ID"],"[,c(\"DateTime\",\"value.new\")]",sep="")	# assign the 1st data frame to A with only "DateTime" and "value.new" retained.
EVAL(parse(text=command.string))
names(A) <- c("DateTime",meter.dataframes[1,"meter.ID"])

for (meter.idx in seq(from=2,to=dim(meter.dataframes)[1]))
{
	if (meter.dataframes[meter.idx,"meter.ID"] != "")
	{
		cat(paste("align and merge meter",meter.idx,":[",meter.dataframes[meter.idx,"meter.ID"],"](",meter.dataframes[meter.idx,"meter.name"],")\n",sep=""))
		cat(paste("align and merge meter",meter.idx,":[",meter.dataframes[meter.idx,"meter.ID"],"](",meter.dataframes[meter.idx,"meter.name"],")\n",sep=""),file=FL.LOG,append=TRUE)
		command.string  <- paste("B <- ",meter.dataframes[meter.idx,"meter.ID"],"[,c(\"DateTime\",\"value.new\")]",sep="")	# assign each of the rest data frame to B with only "DateTime" and "value.new" retained.
		EVAL(parse(text=command.string))
		names(B) <- c("DateTime",meter.dataframes[meter.idx,"meter.ID"])
	
		merged.tmp <- merge(A,B,by.x=c("DateTime"),by.y=c("DateTime"),all.x=TRUE,all.y=TRUE)
		
		A <- merged.tmp
	}
}
merged.data <- merged.tmp
cat(paste("finishing merging all meters!\n",sep=""),file=FL.LOG,append=TRUE)
cat(paste("finishing merging all meters!\n",sep=""))



# -------------------------------------------------------------------------------------------------
# example 5
# -------------------------------------------------------------------------------------------------
command.string_save <- "save(myRental,myPTHP.info,"
no <- 0
for (guestRoom in guestRooms)
{
	command.string <- paste(paste("myData.GR",guestRoom,sep="")," <- myData",sep="")
	EVAL(parse(text=command.string))


	command.string_save <- paste(command.string_save,paste("myData.GR",guestRoom,sep=""),",",sep="")
}
command.string_save <- paste(command.string_save,"myData.allPTHP,file=FL.PTHPdata)",sep="")
EVAL(parse(text=command.string_save))
cat(command.string,file=FL.allMySQL.CSV,append=TRUE)
	
# -------------------------------------------------------------------------------------------------
# example 6
# -------------------------------------------------------------------------------------------------
for (guestRoom in guestRooms[seq(from=18,to=32)])
{
	for (enduse in enduses)
	{
		myData <- cbind(myData,chron.date=tmp)

		command.string<- paste(paste("myData.",enduse.name,sep="")," <- myData",sep="")	
		EVAL(parse(text=command.string))
	}
	# -------------------------------------------------------------------------------------------------
	# merge
	# -------------------------------------------------------------------------------------------------
	command.string <- paste("tmp1 <- ",paste("myData.",array.4merge[1],sep=""),sep="")
	EVAL(parse(text=command.string))
	cat(paste("\n",command.string,"\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)

	for (enduse in array.4merge[seq(from=2,to=length(array.4merge))])
	{
		command.string <- paste("tmp2 <- ",paste(paste("myData.",enduse,sep=""),"[,c(\"chron.date\",\"",enduse,"\")]",sep=""),sep="")
		EVAL(parse(text=command.string))
		cat(paste("\n",command.string,"\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)

		command.string <- paste("tmp1 <- merge(tmp1,tmp2,by.x=c(\"chron.date\"),by.y=c(\"chron.date\"),all.x=TRUE,all.y=TRUE)",sep="")
		EVAL(parse(text=command.string))
		cat(paste("\n",command.string,"\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)
	}


	#
	# put the merged data to a data object [myData.GR[room.name]]
	#
	command.string<- paste(paste("myData.GR",guestRoom,sep="")," <- tmp1",sep="")	
	EVAL(parse(text=command.string))
	cat(paste("\n",command.string,"\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)

	#
	# save the merged data into a R object
	#
	command.string<- paste("save(",paste("myData.GR",guestRoom,sep=""),",file=FL.panelData.OBJ)",sep="")
	EVAL(parse(text=command.string))
	cat(paste("\n",command.string,"\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)

	#
	# write merged data into a CSV file
	#
	command.string <- paste("cat(\",\",file=FL.panelData.CSV,append=TRUE)",sep="")
	EVAL(parse(text=command.string))
	cat(paste("\n",command.string,"\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)
	
	command.string<- paste("write.table(",paste("myData.GR",guestRoom,sep=""),",file=FL.panelData.CSV,sep=\",\",col.names = TRUE, row.names = TRUE, append = TRUE)",sep="")
	EVAL(parse(text=command.string))
	cat(paste("\n",command.string,"\n",sep=""),file=FL.allMySQL.CSV,append=TRUE)
}
	

# -------------------------------------------------------------------------------------------------
# example 7
# -------------------------------------------------------------------------------------------------

idx.EPlus <- paste("EPLus-",run.string,"-",climate.city,"-",facade,"-",floor.lab,sep="")
idx.RAD   <- paste("RAD-",run.string,"-",climate.city,"-",facade,"-",floor.lab,sep="")

# put the two new column in the data frame
command.string <- 
paste("all.data <- cbind(all.data,",
			 "\"",idx.EPlus,"\" = myData.EPlus[,\"",col.EPlus,"\"],",
			 "\"",idx.RAD,  "\" = myData.RAD[,\"",col.RAD,"\"])",
sep="")

EVAL(parse(text=command.string))	