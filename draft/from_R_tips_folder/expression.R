#
# 34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R 
# 
# Plot and fit the PTHP Power and T data retrieved by "33_CrownePlaza_PTHP_Power_vs_T_Retrieve_daily.R"
#
#
# Created  on January 13, 2010
#
# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "/phome/nap/CrownePlaza/Data_uploaded_till_Dec2009/CrownePlaza_allData/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2010_NAP_DataAnalysis/CrownePlaza_allData/0_scripts"
}
setwd(Path.Current)

# -------------------------------------------------------------------------
# 	load stuff defined in the "0_CrownePlaza_FunctionR"
# -------------------------------------------------------------------------------------------------
source(paste(Path.Current,"0_CrownePlaza_Function.R",sep="/"))



#
# x limits
#
xlim4plot    <- c(chron(dates= "9/3/2009", times="0:0:0",format=c('m/d/y','h:m:s')),
                  chron(dates="12/11/2009",times="0:0:0",format=c('m/d/y','h:m:s')))



# -------------------------------------------------------------------------------------------------
#	setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.in   <- "../33_CrownePlaza_PTHP_Power_vs_T_Retrieve_daily"		# INPUT  
Path.out  <- "../34_CrownePlaza_PTHP_Power_vs_T_Plot_daily"		# OUTPUT
Path.log  <- "../0_log"							# OUTPUT log  directory
if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# ------------------------------------------------------------------------------------------------- 
# 	create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
FL.LOG <- paste(Path.log,"34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.log",sep="/")	# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file for data processing script [34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n******************a*********************************************************",
            "*                      [34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R]      *",
            "****************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# 	files
# ------------------------------------------------------------------------------------------------- 
FL.PTHPdata <- paste(Path.in,paste("CrownePlaza_PTHPdata.Rdata",sep=""),sep="/")			# INPUT PTHP data in R object
if (!file.exists(FL.PTHPdata)){stop(paste(FL.PTHPdata," does not exist. Find out why!"))}		# Check the existence of the INPUT data file          

FL.RENT.PDF     <- paste(Path.out,"PTHP_RENT.pdf",sep="/")			# OUTPUT PDF file for rental info (note: rental is on daily basis and PTHP retrieved is on hourly basis, manually checked the merging and it is allright)
FL.PTHP.PDF     <- paste(Path.out,"PTHP_Power_vs_T_Plot_hourly.pdf",sep="/")	# OUTPUT PDF file for PTHP vs Temperature plots.
FL.RENTAL 	<- paste(Path.out,"rental.csv",sep="/")				# OUTPUT CSV file for verifying merging rental status information
FL.PTHP   	<- paste(Path.out,"PTHP.csv",sep="/")				# OUTPUT CSV file for verifying merging rental status information
if (file.exists(FL.RENT.PDF)){print(paste(FL.RENT.PDF,"exist.Delete it!"));file.remove(FL.RENT.PDF)}
if (file.exists(FL.PTHP.PDF)){print(paste(FL.PTHP.PDF,"exist.Delete it!"));file.remove(FL.PTHP.PDF)}
if (file.exists(FL.RENTAL)){print(paste(FL.RENTAL,"exist.Delete it!"));file.remove(FL.RENTAL)}
if (file.exists(FL.PTHP)){print(paste(FL.PTHP,"exist.Delete it!"));file.remove(FL.PTHP)}


# list of guest room number            
guestRooms   <- c(1001,1007,1010,1011,1024,1025,1027,1028,1200,1201,1207,1224,1227,1228,908,923,927,928,200,201,205,222,227,228,400,401,407,424,427,428,900,901)

# ------------------------------------------------------------------------------------------------- 
# load the PTHP data
# ------------------------------------------------------------------------------------------------- 
load(FL.PTHPdata)
names(myData.allPTHP) <- c("date.time","Day","meterID.returnT","meterID.current","avgT.outdoor","avgT.return","avg.current","avg.power","room.name","room.corner","room.orient","PTHP.vintage","PTHP.thermostat")
# note: any change in the fields of the retrieved data, need a change on the names here
# note: the [Day] field is used for merging the RENTAL STATUS information with the PTHP data


# 
# derive a revised rental status field: the day immediately after a rental day should be regarded as a rental day as well
# 
myRental.revised <- myRental[,"RentalStatus"]	# intialize the revised rental status with current rental info
for (idx in seq(from =2, to = dim(myRental)[1]))
{
	if (myRental[idx,  "RentalStatus"] == 0 &					# current day not rented
	    myRental[idx-1,"RentalStatus"] == 1 &					# previous day is rented
	    myRental[idx,  "MeterLocation"] == myRental[idx,"MeterLocation"] &		# current day and previous data are for the same guest room
	    as.numeric(myRental[idx,  "DateTime"] - myRental[idx-1,"DateTime"]) == 1)	# current day and previous day are consecutive
	    {
	    	myRental.revised[idx] <- 1						# change the rental status from "not rented" to "rented" if the immediate previous day is rented.
	    }
}
myRental <- cbind(myRental,RentalStatus.revised = myRental.revised)			# revised rental status of the guest rooms

#
# also add a gap of days between two consecutive data for verifying the correctness of the reevised rental status
#
tmp <- rep(0,dim(myRental)[1])
tmp[seq(from=2,to=length(tmp))] <- as.numeric(myRental[seq(from=2,to=length(tmp)),"DateTime"] - myRental[seq(from=1,to=length(tmp)-1),"DateTime"])
myRental <- cbind(myRental,gap.days = tmp)


#
# add a field for the difference between the Outdoor Air Temperature and the Return Air Temperature
#
myData.allPTHP <- cbind(myData.allPTHP,deltaT = myData.allPTHP[,"avgT.outdoor"] - myData.allPTHP[,"avgT.return"])

#
# add a ID field: which combines the "Corner", "Orientation", "Thermostat" and "PTHP Vintage"
#
myData.allPTHP <- cbind(myData.allPTHP,PTHP.ID = myPTHP.info[as.character(myData.allPTHP[,"room.name"]),"PTHP.ID"])


#
# y.limit: the range of PTHP power for plotting purpose
#
y.limit <- c(trunc(floor(min(myData.allPTHP[,"avg.power"],   na.rm=TRUE)/10))*10,		# ***************** round to the 10th place of the maximum solar radiation *****************
           trunc(ceiling(max(myData.allPTHP[,"avg.power"],   na.rm=TRUE)/10))*10)		# ***************** round to the 10th place of the maximum solar radiation ***************** )




# -------------------------------------------------------------------------------------------------
# merge the rental status information in [myRental] to the PTHP data frame [myData.allPTHP]
# -------------------------------------------------------------------------------------------------
myData.allPTHP <- cbind(myData.allPTHP,rental = rep(NaN,dim(myData.allPTHP)[1]),rental.revised = rep(NaN,dim(myData.allPTHP)[1]))			# initialize the rental information field
for (guestRoom in guestRooms)
{
	A   <- myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),]						# subset of PTHP data for current guest room
	B   <- myRental[as.character(myRental[,"MeterLocation"]) == paste("GR",guestRoom,sep=""),]							# subset of rental info of current guest room
	idx <- match(A[,"Day"],B[,"DateTime"])														# the index in the Rental subset corresponding to the PTHP subset
	myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"rental"]         <- B[idx,"RentalStatus"]		# merged         rental to PTHP data
	myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"rental.revised"] <- B[idx,"RentalStatus.revised"]	# merged revised rental to PTHP data
}
	


		
# -------------------------------------------------------------------------------------------------
# A. Power vs T: plot the PTHP Power (W) against the Outdoor Air Temperature (F)
# -------------------------------------------------------------------------------------------------
cat(paste("\nA. (\"average power\": ) Plot average PTHP power against outdoor air temperature for each guest room!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nA. (\"average power\": ) Plot average PTHP power against outdoor air temperature for each guest room!\n"))
pdf(file = FL.PTHP.PDF,paper="a4r", width=0, height=0)

plot.obj <- xyplot(avg.power ~ avgT.outdoor | as.factor(PTHP.ID), 
                   data = myData.allPTHP,
                   
                   xlab =  expression("Outdoor Temperature F"^{o}),
                   ylab = "PTHP Power (W)",
                   main = expression(paste("PTHP Power (W) vs Outdoor Temperature F"^{o}," (solid line: LOESS of degree=1)",sep="")),
		   lty  = 1,		   
		   pch  = ".",
		   cex  = 1.5,
                   between = list(x=0.5,y=0.5),
                   ylim = y.limit,
                   layout = c(4,3),
                   as.table = TRUE,
                   panel = function(...){
                         panel.grid(h=-1,v=-1)
                         panel.xyplot(...,col="red")
                         panel.loess(...,col="blue",degree=1,family = "gaussian", span=0.3)})
		   
plot(plot.obj)   

# -------------------------------------------------------------------------------------------------
# B. Power vs DeltaT:    plot the PTHP Power (W) against the Difference between Outdoor Air Temperature (F) and the return Air Temperature 
# -------------------------------------------------------------------------------------------------
cat(paste("\nB. (\"average power\": ) Plot average PTHP power against deltaT for each guest room!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nB. (\"average power\": ) Plot average PTHP power against deltaT for each guest room!\n"))

plot.obj <- xyplot(avg.power ~ deltaT | as.factor(PTHP.ID), 
                   data = myData.allPTHP,
                   xlab = expression(paste(Delta(T)," F"^{o},sep="")),
                   ylab = "PTHP Power (W)",
                   main = expression(paste("PTHP Power (W) vs ",Delta(T)," F"^{o},"      (solid line: LOESS of degree=1)",sep="")),
		   lty  = 1,		   
		   pch  = ".",
		   cex  = 1.5,
                   between = list(x=0.5,y=0.5),
                   ylim = y.limit,
                   layout = c(4,3),
                   as.table = TRUE,
                            panel.grid(h=-1,v=-1)
                panel = function(...){
                         panel.xyplot(...,col="grey")
                         panel.loess(...,col="red",degree=1,family = "gaussian", span=0.3)})
		   
plot(plot.obj) 


# -------------------------------------------------------------------------------------------------
# expand [myData.allPTHP] with an loess estimate: degree 1, window = 0.3
# -------------------------------------------------------------------------------------------------
myData.allPTHP <- cbind(myData.allPTHP,     T.loess = rep(NaN,dim(myData.allPTHP)[1]))	# initialize the loess fit field
myData.allPTHP <- cbind(myData.allPTHP,deltaT.loess = rep(NaN,dim(myData.allPTHP)[1]))	# initialize the loess fit field
# -------------------------------------------------------------------------------------------------
# C: Loess Fitted of the PTHP Power (W) against the Outdoor Air Temperature (F)
# -------------------------------------------------------------------------------------------------
for (guestRoom in guestRooms)
{
	cat(paste("\nC. Loess Fitted of Power against T",paste("GR",guestRoom,sep=""),"!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nC. Loess Fitted of Power against T",paste("GR",guestRoom,sep=""),"!\n"))
	idx <- myData.allPTHP[,"room.name"] == paste("GR",guestRoom,sep="")

	# a loess fitting
	smooth.loess.30 <- loess(avg.power ~ avgT.outdoor,data = myData.allPTHP[idx,],span = 0.30, family = "gaussian", degree = 1)
	fit.loess.30    <- predict(smooth.loess.30)
	
	#
	# use linear estimate of window size = 0.3 for loess estimates
	#
	myData.allPTHP[idx,"T.loess"] <- fit.loess.30
}
cat(paste("\nC. added a LOESS fit of [Power vs T] field !\n"),file=FL.LOG,append=TRUE)
cat(paste("\nC. added a LOESS fit of [Power vs T] field !\n"))


# -------------------------------------------------------------------------------------------------
# D: Loess Fitted of DeltaT of the PTHP Power (W) against the Difference between Outdoor Air Temperature (F) and the return Air Temperature 
# -------------------------------------------------------------------------------------------------
for (guestRoom in guestRooms)
{
	cat(paste("\nD. Loess Fitted of Power against deltaT: ",paste("GR",guestRoom,sep=""),"!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nD. Loess Fitted of Power against deltaT: ",paste("GR",guestRoom,sep=""),"!\n"))

	idx <- myData.allPTHP[,"room.name"] == paste("GR",guestRoom,sep="")

	# a loess fitting
	smooth.loess.30 <- loess(avg.power ~ deltaT,data = myData.allPTHP[idx,],span = 0.30, family = "gaussian", degree = 1)
	fit.loess.30    <- predict(smooth.loess.30)

	#
	# use linear estimate of window size = 0.3 for loess estimates
	#
	myData.allPTHP[idx,"deltaT.loess"] <- fit.loess.30
}
cat(paste("\nD. added a LOESS fit of [Power vs delta T] field !\n"),file=FL.LOG,append=TRUE)
cat(paste("\nD. added a LOESS fit of [Power vs delta T] field !\n"))


# -------------------------------------------------------------------------------------------------
# E: plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)
# -------------------------------------------------------------------------------------------------
cat(paste("\nE. plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nE. plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)!\n"))


x.limit <- c(trunc(floor(min(myData.allPTHP[,"avgT.outdoor"],   na.rm=TRUE)/10))*10,		# ***************** round to the 10th place of the maximum solar radiation *****************
           trunc(ceiling(max(myData.allPTHP[,"avgT.outdoor"],   na.rm=TRUE)/10))*10)		# ***************** round to the 10th place of the maximum solar radiation ***************** )

# sort the data according to T before plotting
myData.allPTHP <- myData.allPTHP[order(myData.allPTHP[,"room.name"],myData.allPTHP[,"avgT.outdoor"]),]


# split the page into 4 blocks
par(mfrow=c(2,2),mar=c(4,5,3,3)+0.3,pty="m")



# E1. Power vs T: allData
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","T.loess"],		# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],	# auto determine the color of the line depending on "New" or "Old" PTHP vintage
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (all data)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
     	lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"avgT.outdoor"],
	      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data")   

# E2. Power vs T: allData
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","T.loess"],		# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],	# auto determine the color of the line depending on "New" or "Old" PTHP vintage
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (all data)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
     	lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"avgT.outdoor"],
	      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 


# E3. Power vs T: rented data
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 1),"avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 1),"T.loess"],	# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (Rented)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
	lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 1),"avgT.outdoor"],
	      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 1),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
    }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Rented (original)")   
				  
# E4. Power vs T: non rented data
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 0),"avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 0),"T.loess"],	# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (Not Rented)",sep="")))

     # add one line for each room     
     for (guestRoom in guestRooms)
     {
	lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 0),"avgT.outdoor"],
	      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 0),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Not Rented (original)")   


# -------------------------------------------------------------------------------------------------
# EE: plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)
# -------------------------------------------------------------------------------------------------
cat(paste("\nEE. plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nEE. plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)!\n"))


# split the page into 4 blocks
par(mfrow=c(2,2),mar=c(4,5,3,3)+0.3,pty="m")


# EE1. Power vs T: allData
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","T.loess"],		# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],	# auto determine the color of the line depending on "New" or "Old" PTHP vintage
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (all data)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
     	lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"avgT.outdoor"],
	      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data")   

# EE2. Power vs T: allData
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","T.loess"],		# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],	# auto determine the color of the line depending on "New" or "Old" PTHP vintage
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (all data)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
     	lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"avgT.outdoor"],
	      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 


# EE3. Power vs T: rented data
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 1),"avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 1),"T.loess"],	# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (Rented)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
	lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 1),"avgT.outdoor"],
	      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 1),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
    }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Rented (revised)")   
				  
# EE4. Power vs T: non rented data
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 0),"avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 0),"T.loess"],	# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (Not Rented)",sep="")))

     # add one line for each room     
     for (guestRoom in guestRooms)
     {
	lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 0),"avgT.outdoor"],
	      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 0),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Not Rented (revised)")   



				  
# -------------------------------------------------------------------------------------------------
# F: [Loess fitted Power] vs [T]: one page with 4 blocks, one for "S", one for "N", one for "C" and one for New"
# -------------------------------------------------------------------------------------------------
cat(paste("\nF. Plot [Loess Fitted of Power] against [T] for [New] and [Old] PTHP units!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nF. Plot [Loess Fitted of Power] against [T] for [New] and [Old] PTHP units!\n"))

# split the page into 4 blocks
par(mfrow=c(2,2),mar=c(4,5,3,3)+0.3,pty="m")

# 1. "South" for block 1
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.orient"]) == "S",]		# only room facing "South"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms facing "South"

plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"avgT.outdoor"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"T.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o},sep="")))
     
     # add one line for each of the rest rooms
     for (guestRoom in rooms)	# loopping through the rest of the room in this subset
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"avgT.outdoor"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"T.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="South")   


# 2. "North" for block 2
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.orient"]) == "N",]		# only room facing "North"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms facing "North"


plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"avgT.outdoor"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"T.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o},sep="")))

     # add one line for each of the rest rooms     
     for (guestRoom in rooms)	# loopping through the rest of the room in this subset
     {
  	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"avgT.outdoor"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"T.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="North")   


# 3. "Corner" for block 3
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.corner"]) == "C",]		# only room at "Corner"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms at "Corner"


plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"avgT.outdoor"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"T.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o},sep="")))

     # add one line for each of the rest rooms          
     for (guestRoom in rooms)
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"avgT.outdoor"],	# pick one room to start the plot loess fitted power vs T
              data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"T.loess"],	# pick one room to start the plot loess fitted power vs T
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
    }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Corner")   


# 4. "Thermostat" yes/no for New PTHP
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"PTHP.vintage"]) == "New",]		# ALL "New" PTHP units
rooms <- unique(as.character(data.subset[,"room.name"]))					# THE ROOMS WITH "nEW" pthp UNITS

plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"avgT.outdoor"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"T.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.thermostat"])=="T1",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o},sep="")))

     # add one line for each of the rest rooms               
     for (guestRoom in rooms)
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"avgT.outdoor"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"T.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.thermostat"])=="T1",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("Yes","No"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Thermostat")   

				  
				  
     

# -------------------------------------------------------------------------------------------------
# G: plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)
# -------------------------------------------------------------------------------------------------
cat(paste("\nG. plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nG. plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)!\n"))

x.limit <- c(trunc(floor(min(myData.allPTHP[,"deltaT"],   na.rm=TRUE)/10))*10,		# ***************** round to the 10th place of the maximum solar radiation *****************
           trunc(ceiling(max(myData.allPTHP[,"deltaT"],   na.rm=TRUE)/10))*10)		# ***************** round to the 10th place of the maximum solar radiation ***************** )

# sort the data according to deltaT before plotting
myData.allPTHP <- myData.allPTHP[order(myData.allPTHP[,"PTHP.ID"],myData.allPTHP[,"deltaT"]),]


# G1. Power vs deltaT: plot all data
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT"],
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (All Data)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT"],
		      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 

# G2. Power vs deltaT: plot all data
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT"],
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (All Data)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT"],
		      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 

# G3. Power vs deltaT: rented
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 1),"deltaT"],
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 1),"deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (Rented)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 1),"deltaT"],
		      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 1),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Rented (original)")  

# G4. Power vs deltaT: not rented
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 0),"deltaT"],
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 0),"deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (Not Rented)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 0),"deltaT"],
		      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 0),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Not Rented (original)")  



# -------------------------------------------------------------------------------------------------
# GG: plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)
# -------------------------------------------------------------------------------------------------
cat(paste("\nG. plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nG. plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)!\n"))

x.limit <- c(trunc(floor(min(myData.allPTHP[,"deltaT"],   na.rm=TRUE)/10))*10,		# ***************** round to the 10th place of the maximum solar radiation *****************
           trunc(ceiling(max(myData.allPTHP[,"deltaT"],   na.rm=TRUE)/10))*10)		# ***************** round to the 10th place of the maximum solar radiation ***************** )

# sort the data according to deltaT before plotting
myData.allPTHP <- myData.allPTHP[order(myData.allPTHP[,"PTHP.ID"],myData.allPTHP[,"deltaT"]),]


# GG1. Power vs deltaT: plot all data
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT"],
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (All Data)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT"],
		      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 

# GG2. Power vs deltaT: plot all data
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT"],
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (All Data)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT"],
		      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 

# GG3. Power vs deltaT: rented
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 1),"deltaT"],
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 1),"deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (Rented)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 1),"deltaT"],
		      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 1),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Rented (revised)")  

# GG4. Power vs deltaT: not rented
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 0),"deltaT"],
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 0),"deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (Not Rented)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 0),"deltaT"],
		      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 0),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Not Rented (revised)")  



# -------------------------------------------------------------------------------------------------
# H: [Loess fitted Power] vs [deltaT]: one page with 4 blocks, one for "S", one for "N", one for "C" and one for New"
# -------------------------------------------------------------------------------------------------
cat(paste("\nH. Plot [Loess Fitted of Power] against [deltaT] for [New] and [Old] PTHP units!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nH. Plot [Loess Fitted of Power] against [deltaT] for [New] and [Old] PTHP units!\n"))

# split the page into 4 blocks
par(mfrow=c(2,2),mar=c(4,5,3,3)+0.3,pty="m")

# 1. "South" for block 1
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.orient"]) == "S",]		# only room facing "South"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms facing "South"

plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o},sep="")))
     
     # add one line for each of the rest rooms
     for (guestRoom in rooms)	# loopping through the rest of the room in this subset
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="South")   


# 2. "North" for block 2
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.orient"]) == "N",]		# only room facing "North"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms facing "North"


plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o},sep="")))

     # add one line for each of the rest rooms     
     for (guestRoom in rooms)	# loopping through the rest of the room in this subset
     {
  	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="North")   


# 3. "Corner" for block 3
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.corner"]) == "C",]		# only room at "Corner"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms at "Corner"


plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o},sep="")))

     # add one line for each of the rest rooms          
     for (guestRoom in rooms)
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT"],	# pick one room to start the plot loess fitted power vs T
              data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT.loess"],	# pick one room to start the plot loess fitted power vs T
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
    }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Corner")   


# 4. "Thermostat" yes/no for New PTHP
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"PTHP.vintage"]) == "New",]		# ALL "New" PTHP units
rooms <- unique(as.character(data.subset[,"room.name"]))					# THE ROOMS WITH "nEW" pthp UNITS

plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.thermostat"])=="T1",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o},sep="")))

     # add one line for each of the rest rooms               
     for (guestRoom in rooms)
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.thermostat"])=="T1",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("Yes","No"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Thermostat")   

				  
	
dev.off()

# -------------------------------------------------------------------------------------------------
# I: verify the rental status information are all right
# -------------------------------------------------------------------------------------------------
pdf(file = FL.RENT.PDF,paper="a4r", width=0, height=0)


y.limit <- c(0,1)		# ***************** round to the 10th place of the maximum solar radiation ***************** )
x.limit <- c(min(myData.allPTHP[,"date.time"]),max(myData.allPTHP[,"date.time"]))		# ***************** round to the 10th place of the maximum solar radiation ***************** )
for (guestRoom in guestRooms)
{
	cat(paste("\nG. rental status: ",paste("GR",guestRoom,sep=""),"!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nG. rental status: ",paste("GR",guestRoom,sep=""),"!\n"))

	idx.PTHP <- as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")
	PTHP.subset <- myData.allPTHP[idx.PTHP,]
	PTHP.subset <- PTHP.subset[order(PTHP.subset[,"date.time"]),]

	idx.RENT <- as.character(myRental[,"MeterLocation"]) == paste("GR",guestRoom,sep="")
	RENT.subset <- myRental[idx.RENT,]	
	RENT.subset <- RENT.subset[order(RENT.subset[,"DateTime"]),]

	plot(PTHP.subset[,"date.time"],
	     PTHP.subset[,"rental"],
	     col = col.array[1],
	     type = "p",
	     lty = 1,
	     cex = 1,
	     lwd = 0.5,
	     ylim = y.limit,
	     xlab = "Date",
	     ylab = "rental status",
	     main = "Rental Status Verification")

	lines(RENT.subset[,"DateTime"],
	      RENT.subset[,"RentalStatus"],
		      col = col.array[2],
		      type = "h",
		      lty = 1,
		      pch = 16,
		      cex = 0.5,
		      lwd = 0.5)
 
	legend(min(xlim4plot), max(y.limit)*.80,legend=c("New","Old"),
				  col=c("red","blue"),
				  lty=c(1,1),
				  text.col = c("red","blue"))	
}


dev.off()

#
# output the processed data
#
cat(",",file = FL.PTHP,  append = TRUE)
cat(",",file = FL.RENTAL,append = TRUE)
write.table(myData.allPTHP,file = FL.PTHP,  sep = ",", col.names = TRUE, row.names = TRUE, append = TRUE)	
write.table(myRental,      file = FL.RENTAL,sep = ",", col.names = TRUE, row.names = TRUE, append = TRUE)	




# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)












#
# 34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R 
# 
# Plot and fit the PTHP Power and T data retrieved by "33_CrownePlaza_PTHP_Power_vs_T_Retrieve_daily.R"
#
#
# Created  on January 13, 2010
#
# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "/phome/nap/CrownePlaza/Data_uploaded_till_Dec2009/CrownePlaza_allData/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2010_NAP_DataAnalysis/CrownePlaza_allData/0_scripts"
}
setwd(Path.Current)

# -------------------------------------------------------------------------
# 	load stuff defined in the "0_CrownePlaza_FunctionR"
# -------------------------------------------------------------------------------------------------
source(paste(Path.Current,"0_CrownePlaza_Function.R",sep="/"))



#
# x limits
#
xlim4plot    <- c(chron(dates= "9/3/2009", times="0:0:0",format=c('m/d/y','h:m:s')),
                  chron(dates="12/11/2009",times="0:0:0",format=c('m/d/y','h:m:s')))



# -------------------------------------------------------------------------------------------------
#	setup output and log directory
# -------------------------------------------------------------------------------------------------
Path.in   <- "../33_CrownePlaza_PTHP_Power_vs_T_Retrieve_daily"		# INPUT  
Path.out  <- "../34_CrownePlaza_PTHP_Power_vs_T_Plot_daily"		# OUTPUT
Path.log  <- "../0_log"							# OUTPUT log  directory
if (!file.exists(Path.out)){print(paste("NOT existing:",Path.out));dir.create(Path.out,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}

# ------------------------------------------------------------------------------------------------- 
# 	create a LOG file and a TIME Recording file
# ------------------------------------------------------------------------------------------------- 
FL.LOG <- paste(Path.log,"34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.log",sep="/")	# OUTPUT Log file
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}

cat(paste("Log file for data processing script [34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n******************a*********************************************************",
            "*                      [34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R]      *",
            "****************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)

# ------------------------------------------------------------------------------------------------- 
# 	files
# ------------------------------------------------------------------------------------------------- 
FL.PTHPdata <- paste(Path.in,paste("CrownePlaza_PTHPdata.Rdata",sep=""),sep="/")			# INPUT PTHP data in R object
if (!file.exists(FL.PTHPdata)){stop(paste(FL.PTHPdata," does not exist. Find out why!"))}		# Check the existence of the INPUT data file          

FL.RENT.PDF     <- paste(Path.out,"PTHP_RENT.pdf",sep="/")			# OUTPUT PDF file for rental info (note: rental is on daily basis and PTHP retrieved is on hourly basis, manually checked the merging and it is allright)
FL.PTHP.PDF     <- paste(Path.out,"PTHP_Power_vs_T_Plot_hourly.pdf",sep="/")	# OUTPUT PDF file for PTHP vs Temperature plots.
FL.RENTAL 	<- paste(Path.out,"rental.csv",sep="/")				# OUTPUT CSV file for verifying merging rental status information
FL.PTHP   	<- paste(Path.out,"PTHP.csv",sep="/")				# OUTPUT CSV file for verifying merging rental status information
if (file.exists(FL.RENT.PDF)){print(paste(FL.RENT.PDF,"exist.Delete it!"));file.remove(FL.RENT.PDF)}
if (file.exists(FL.PTHP.PDF)){print(paste(FL.PTHP.PDF,"exist.Delete it!"));file.remove(FL.PTHP.PDF)}
if (file.exists(FL.RENTAL)){print(paste(FL.RENTAL,"exist.Delete it!"));file.remove(FL.RENTAL)}
if (file.exists(FL.PTHP)){print(paste(FL.PTHP,"exist.Delete it!"));file.remove(FL.PTHP)}


# list of guest room number            
guestRooms   <- c(1001,1007,1010,1011,1024,1025,1027,1028,1200,1201,1207,1224,1227,1228,908,923,927,928,200,201,205,222,227,228,400,401,407,424,427,428,900,901)

# ------------------------------------------------------------------------------------------------- 
# load the PTHP data
# ------------------------------------------------------------------------------------------------- 
load(FL.PTHPdata)
names(myData.allPTHP) <- c("date.time","Day","meterID.returnT","meterID.current","avgT.outdoor","avgT.return","avg.current","avg.power","room.name","room.corner","room.orient","PTHP.vintage","PTHP.thermostat")
# note: any change in the fields of the retrieved data, need a change on the names here
# note: the [Day] field is used for merging the RENTAL STATUS information with the PTHP data


# 
# derive a revised rental status field: the day immediately after a rental day should be regarded as a rental day as well
# 
myRental.revised <- myRental[,"RentalStatus"]	# intialize the revised rental status with current rental info
for (idx in seq(from =2, to = dim(myRental)[1]))
{
	if (myRental[idx,  "RentalStatus"] == 0 &					# current day not rented
	    myRental[idx-1,"RentalStatus"] == 1 &					# previous day is rented
	    myRental[idx,  "MeterLocation"] == myRental[idx,"MeterLocation"] &		# current day and previous data are for the same guest room
	    as.numeric(myRental[idx,  "DateTime"] - myRental[idx-1,"DateTime"]) == 1)	# current day and previous day are consecutive
	    {
	    	myRental.revised[idx] <- 1						# change the rental status from "not rented" to "rented" if the immediate previous day is rented.
	    }
}
myRental <- cbind(myRental,RentalStatus.revised = myRental.revised)			# revised rental status of the guest rooms

#
# also add a gap of days between two consecutive data for verifying the correctness of the reevised rental status
#
tmp <- rep(0,dim(myRental)[1])
tmp[seq(from=2,to=length(tmp))] <- as.numeric(myRental[seq(from=2,to=length(tmp)),"DateTime"] - myRental[seq(from=1,to=length(tmp)-1),"DateTime"])
myRental <- cbind(myRental,gap.days = tmp)


#
# add a field for the difference between the Outdoor Air Temperature and the Return Air Temperature
#
myData.allPTHP <- cbind(myData.allPTHP,deltaT = myData.allPTHP[,"avgT.outdoor"] - myData.allPTHP[,"avgT.return"])

#
# add a ID field: which combines the "Corner", "Orientation", "Thermostat" and "PTHP Vintage"
#
myData.allPTHP <- cbind(myData.allPTHP,PTHP.ID = myPTHP.info[as.character(myData.allPTHP[,"room.name"]),"PTHP.ID"])


#
# y.limit: the range of PTHP power for plotting purpose
#
y.limit <- c(trunc(floor(min(myData.allPTHP[,"avg.power"],   na.rm=TRUE)/10))*10,		# ***************** round to the 10th place of the maximum solar radiation *****************
           trunc(ceiling(max(myData.allPTHP[,"avg.power"],   na.rm=TRUE)/10))*10)		# ***************** round to the 10th place of the maximum solar radiation ***************** )




# -------------------------------------------------------------------------------------------------
# merge the rental status information in [myRental] to the PTHP data frame [myData.allPTHP]
# -------------------------------------------------------------------------------------------------
myData.allPTHP <- cbind(myData.allPTHP,rental = rep(NaN,dim(myData.allPTHP)[1]),rental.revised = rep(NaN,dim(myData.allPTHP)[1]))			# initialize the rental information field
for (guestRoom in guestRooms)
{
	A   <- myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),]						# subset of PTHP data for current guest room
	B   <- myRental[as.character(myRental[,"MeterLocation"]) == paste("GR",guestRoom,sep=""),]							# subset of rental info of current guest room
	idx <- match(A[,"Day"],B[,"DateTime"])														# the index in the Rental subset corresponding to the PTHP subset
	myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"rental"]         <- B[idx,"RentalStatus"]		# merged         rental to PTHP data
	myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"rental.revised"] <- B[idx,"RentalStatus.revised"]	# merged revised rental to PTHP data
}
	


		
# -------------------------------------------------------------------------------------------------
# A. Power vs T: plot the PTHP Power (W) against the Outdoor Air Temperature (F)
# -------------------------------------------------------------------------------------------------
cat(paste("\nA. (\"average power\": ) Plot average PTHP power against outdoor air temperature for each guest room!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nA. (\"average power\": ) Plot average PTHP power against outdoor air temperature for each guest room!\n"))
pdf(file = FL.PTHP.PDF,paper="a4r", width=0, height=0)

plot.obj <- xyplot(avg.power ~ avgT.outdoor | as.factor(PTHP.ID), 
                   data = myData.allPTHP,
                   
                   xlab =  expression("Outdoor Temperature F"^{o}),
                   ylab = "PTHP Power (W)",
                   main = expression(paste("PTHP Power (W) vs Outdoor Temperature F"^{o}," (solid line: LOESS of degree=1)",sep="")),
		   lty  = 1,		   
		   pch  = ".",
		   cex  = 1.5,
                   between = list(x=0.5,y=0.5),
                   ylim = y.limit,
                   layout = c(4,3),
                   as.table = TRUE,
                   panel = function(...){
                         panel.grid(h=-1,v=-1)
                         panel.xyplot(...,col="red")
                         panel.loess(...,col="blue",degree=1,family = "gaussian", span=0.3)})
		   
plot(plot.obj)   

# -------------------------------------------------------------------------------------------------
# B. Power vs DeltaT:    plot the PTHP Power (W) against the Difference between Outdoor Air Temperature (F) and the return Air Temperature 
# -------------------------------------------------------------------------------------------------
cat(paste("\nB. (\"average power\": ) Plot average PTHP power against deltaT for each guest room!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nB. (\"average power\": ) Plot average PTHP power against deltaT for each guest room!\n"))

plot.obj <- xyplot(avg.power ~ deltaT | as.factor(PTHP.ID), 
                   data = myData.allPTHP,
                   xlab = expression(paste(Delta(T)," F"^{o},sep="")),
                   ylab = "PTHP Power (W)",
                   main = expression(paste("PTHP Power (W) vs ",Delta(T)," F"^{o},"      (solid line: LOESS of degree=1)",sep="")),
		   lty  = 1,		   
		   pch  = ".",
		   cex  = 1.5,
                   between = list(x=0.5,y=0.5),
                   ylim = y.limit,
                   layout = c(4,3),
                   as.table = TRUE,
                   panel = function(...){
                         panel.grid(h=-1,v=-1)
                         panel.xyplot(...,col="grey")
                         panel.loess(...,col="red",degree=1,family = "gaussian", span=0.3)})
		   
plot(plot.obj) 


# -------------------------------------------------------------------------------------------------
# expand [myData.allPTHP] with an loess estimate: degree 1, window = 0.3
# -------------------------------------------------------------------------------------------------
myData.allPTHP <- cbind(myData.allPTHP,     T.loess = rep(NaN,dim(myData.allPTHP)[1]))	# initialize the loess fit field
myData.allPTHP <- cbind(myData.allPTHP,deltaT.loess = rep(NaN,dim(myData.allPTHP)[1]))	# initialize the loess fit field
# -------------------------------------------------------------------------------------------------
# C: Loess Fitted of the PTHP Power (W) against the Outdoor Air Temperature (F)
# -------------------------------------------------------------------------------------------------
for (guestRoom in guestRooms)
{
	cat(paste("\nC. Loess Fitted of Power against T",paste("GR",guestRoom,sep=""),"!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nC. Loess Fitted of Power against T",paste("GR",guestRoom,sep=""),"!\n"))
	idx <- myData.allPTHP[,"room.name"] == paste("GR",guestRoom,sep="")

	# a loess fitting
	smooth.loess.30 <- loess(avg.power ~ avgT.outdoor,data = myData.allPTHP[idx,],span = 0.30, family = "gaussian", degree = 1)
	fit.loess.30    <- predict(smooth.loess.30)
	
	#
	# use linear estimate of window size = 0.3 for loess estimates
	#
	myData.allPTHP[idx,"T.loess"] <- fit.loess.30
}
cat(paste("\nC. added a LOESS fit of [Power vs T] field !\n"),file=FL.LOG,append=TRUE)
cat(paste("\nC. added a LOESS fit of [Power vs T] field !\n"))


# -------------------------------------------------------------------------------------------------
# D: Loess Fitted of DeltaT of the PTHP Power (W) against the Difference between Outdoor Air Temperature (F) and the return Air Temperature 
# -------------------------------------------------------------------------------------------------
for (guestRoom in guestRooms)
{
	cat(paste("\nD. Loess Fitted of Power against deltaT: ",paste("GR",guestRoom,sep=""),"!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nD. Loess Fitted of Power against deltaT: ",paste("GR",guestRoom,sep=""),"!\n"))

	idx <- myData.allPTHP[,"room.name"] == paste("GR",guestRoom,sep="")

	# a loess fitting
	smooth.loess.30 <- loess(avg.power ~ deltaT,data = myData.allPTHP[idx,],span = 0.30, family = "gaussian", degree = 1)
	fit.loess.30    <- predict(smooth.loess.30)

	#
	# use linear estimate of window size = 0.3 for loess estimates
	#
	myData.allPTHP[idx,"deltaT.loess"] <- fit.loess.30
}
cat(paste("\nD. added a LOESS fit of [Power vs delta T] field !\n"),file=FL.LOG,append=TRUE)
cat(paste("\nD. added a LOESS fit of [Power vs delta T] field !\n"))


# -------------------------------------------------------------------------------------------------
# E: plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)
# -------------------------------------------------------------------------------------------------
cat(paste("\nE. plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nE. plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)!\n"))


x.limit <- c(trunc(floor(min(myData.allPTHP[,"avgT.outdoor"],   na.rm=TRUE)/10))*10,		# ***************** round to the 10th place of the maximum solar radiation *****************
           trunc(ceiling(max(myData.allPTHP[,"avgT.outdoor"],   na.rm=TRUE)/10))*10)		# ***************** round to the 10th place of the maximum solar radiation ***************** )

# sort the data according to T before plotting
myData.allPTHP <- myData.allPTHP[order(myData.allPTHP[,"room.name"],myData.allPTHP[,"avgT.outdoor"]),]


# split the page into 4 blocks
par(mfrow=c(2,2),mar=c(4,5,3,3)+0.3,pty="m")



# E1. Power vs T: allData
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","T.loess"],		# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],	# auto determine the color of the line depending on "New" or "Old" PTHP vintage
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (all data)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
     	lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"avgT.outdoor"],
	      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data")   

# E2. Power vs T: allData
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","T.loess"],		# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],	# auto determine the color of the line depending on "New" or "Old" PTHP vintage
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (all data)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
     	lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"avgT.outdoor"],
	      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 


# E3. Power vs T: rented data
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 1),"avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 1),"T.loess"],	# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (Rented)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
	lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 1),"avgT.outdoor"],
	      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 1),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
    }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Rented (original)")   
				  
# E4. Power vs T: non rented data
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 0),"avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 0),"T.loess"],	# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (Not Rented)",sep="")))

     # add one line for each room     
     for (guestRoom in guestRooms)
     {
	lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 0),"avgT.outdoor"],
	      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 0),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Not Rented (original)")   


# -------------------------------------------------------------------------------------------------
# EE: plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)
# -------------------------------------------------------------------------------------------------
cat(paste("\nEE. plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nEE. plotting Loess fitted [Power] vs [T]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)!\n"))


# split the page into 4 blocks
par(mfrow=c(2,2),mar=c(4,5,3,3)+0.3,pty="m")


# EE1. Power vs T: allData
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","T.loess"],		# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],	# auto determine the color of the line depending on "New" or "Old" PTHP vintage
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (all data)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
     	lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"avgT.outdoor"],
	      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data")   

# EE2. Power vs T: allData
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","T.loess"],		# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],	# auto determine the color of the line depending on "New" or "Old" PTHP vintage
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (all data)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
     	lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"avgT.outdoor"],
	      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 


# EE3. Power vs T: rented data
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 1),"avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 1),"T.loess"],	# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (Rented)",sep="")))
     
     # add one line for each room
     for (guestRoom in guestRooms)
     {
	lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 1),"avgT.outdoor"],
	      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 1),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
    }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Rented (revised)")   
				  
# EE4. Power vs T: non rented data
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 0),"avgT.outdoor"],	# pick anyroom for plotting the first line does not have to be "GR200"
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 0),"T.loess"],	# pick anyroom for plotting the first line does not have to be "GR200"
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o}," (Not Rented)",sep="")))

     # add one line for each room     
     for (guestRoom in guestRooms)
     {
	lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 0),"avgT.outdoor"],
	      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 0),"T.loess"],
	      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Not Rented (revised)")   



				  
# -------------------------------------------------------------------------------------------------
# F: [Loess fitted Power] vs [T]: one page with 4 blocks, one for "S", one for "N", one for "C" and one for New"
# -------------------------------------------------------------------------------------------------
cat(paste("\nF. Plot [Loess Fitted of Power] against [T] for [New] and [Old] PTHP units!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nF. Plot [Loess Fitted of Power] against [T] for [New] and [Old] PTHP units!\n"))

# split the page into 4 blocks
par(mfrow=c(2,2),mar=c(4,5,3,3)+0.3,pty="m")

# 1. "South" for block 1
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.orient"]) == "S",]		# only room facing "South"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms facing "South"

plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"avgT.outdoor"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"T.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o},sep="")))
     
     # add one line for each of the rest rooms
     for (guestRoom in rooms)	# loopping through the rest of the room in this subset
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"avgT.outdoor"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"T.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="South")   


# 2. "North" for block 2
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.orient"]) == "N",]		# only room facing "North"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms facing "North"


plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"avgT.outdoor"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"T.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o},sep="")))

     # add one line for each of the rest rooms     
     for (guestRoom in rooms)	# loopping through the rest of the room in this subset
     {
  	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"avgT.outdoor"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"T.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="North")   


# 3. "Corner" for block 3
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.corner"]) == "C",]		# only room at "Corner"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms at "Corner"


plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"avgT.outdoor"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"T.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o},sep="")))

     # add one line for each of the rest rooms          
     for (guestRoom in rooms)
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"avgT.outdoor"],	# pick one room to start the plot loess fitted power vs T
              data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"T.loess"],	# pick one room to start the plot loess fitted power vs T
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
    }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Corner")   


# 4. "Thermostat" yes/no for New PTHP
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"PTHP.vintage"]) == "New",]		# ALL "New" PTHP units
rooms <- unique(as.character(data.subset[,"room.name"]))					# THE ROOMS WITH "nEW" pthp UNITS

plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"avgT.outdoor"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"T.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.thermostat"])=="T1",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression("Outdoor Temperature F"^{o}),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs T(outdoor) F"^{o},sep="")))

     # add one line for each of the rest rooms               
     for (guestRoom in rooms)
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"avgT.outdoor"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"T.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.thermostat"])=="T1",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("Yes","No"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Thermostat")   

				  
				  
     

# -------------------------------------------------------------------------------------------------
# G: plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)
# -------------------------------------------------------------------------------------------------
cat(paste("\nG. plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nG. plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (ORIGINAL rental data)!\n"))

x.limit <- c(trunc(floor(min(myData.allPTHP[,"deltaT"],   na.rm=TRUE)/10))*10,		# ***************** round to the 10th place of the maximum solar radiation *****************
           trunc(ceiling(max(myData.allPTHP[,"deltaT"],   na.rm=TRUE)/10))*10)		# ***************** round to the 10th place of the maximum solar radiation ***************** )

# sort the data according to deltaT before plotting
myData.allPTHP <- myData.allPTHP[order(myData.allPTHP[,"PTHP.ID"],myData.allPTHP[,"deltaT"]),]


# G1. Power vs deltaT: plot all data
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT"],
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (All Data)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT"],
		      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 

# G2. Power vs deltaT: plot all data
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT"],
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (All Data)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT"],
		      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 

# G3. Power vs deltaT: rented
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 1),"deltaT"],
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 1),"deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (Rented)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 1),"deltaT"],
		      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 1),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Rented (original)")  

# G4. Power vs deltaT: not rented
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 0),"deltaT"],
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental"] == 0),"deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (Not Rented)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 0),"deltaT"],
		      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental"] == 0),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Not Rented (original)")  



# -------------------------------------------------------------------------------------------------
# GG: plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)
# -------------------------------------------------------------------------------------------------
cat(paste("\nG. plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nG. plotting Loess fitted [Power] vs [deltaT]: block 1: all, block 2: all, block 3: rented, block 4: not rented (REVISED rental data)!\n"))

x.limit <- c(trunc(floor(min(myData.allPTHP[,"deltaT"],   na.rm=TRUE)/10))*10,		# ***************** round to the 10th place of the maximum solar radiation *****************
           trunc(ceiling(max(myData.allPTHP[,"deltaT"],   na.rm=TRUE)/10))*10)		# ***************** round to the 10th place of the maximum solar radiation ***************** )

# sort the data according to deltaT before plotting
myData.allPTHP <- myData.allPTHP[order(myData.allPTHP[,"PTHP.ID"],myData.allPTHP[,"deltaT"]),]


# GG1. Power vs deltaT: plot all data
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT"],
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (All Data)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT"],
		      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 

# GG2. Power vs deltaT: plot all data
plot(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT"],
     myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (All Data)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT"],
		      myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="all Data") 

# GG3. Power vs deltaT: rented
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 1),"deltaT"],
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 1),"deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (Rented)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 1),"deltaT"],
		      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 1),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Rented (revised)")  

# GG4. Power vs deltaT: not rented
plot(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 0),"deltaT"],
     myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == "GR200") & (myData.allPTHP[,"rental.revised"] == 0),"deltaT.loess"],
     col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == "GR200","PTHP.vintage"])=="New",1,2)][1],
     type = "l",lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o}," (Not Rented)",sep="")))
     
	for (guestRoom in guestRooms)
	{
		lines(myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 0),"deltaT"],
		      myData.allPTHP[(as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")) & (myData.allPTHP[,"rental.revised"] == 0),"deltaT.loess"],
		      col = col.array[ifelse(as.character(myData.allPTHP[as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep=""),"PTHP.vintage"])=="New",1,2)][1],
		      type = "l",lty = 1,lwd = 0.25)
	}
 
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Not Rented (revised)")  



# -------------------------------------------------------------------------------------------------
# H: [Loess fitted Power] vs [deltaT]: one page with 4 blocks, one for "S", one for "N", one for "C" and one for New"
# -------------------------------------------------------------------------------------------------
cat(paste("\nH. Plot [Loess Fitted of Power] against [deltaT] for [New] and [Old] PTHP units!\n"),file=FL.LOG,append=TRUE)
cat(paste("\nH. Plot [Loess Fitted of Power] against [deltaT] for [New] and [Old] PTHP units!\n"))

# split the page into 4 blocks
par(mfrow=c(2,2),mar=c(4,5,3,3)+0.3,pty="m")

# 1. "South" for block 1
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.orient"]) == "S",]		# only room facing "South"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms facing "South"

plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o},sep="")))
     
     # add one line for each of the rest rooms
     for (guestRoom in rooms)	# loopping through the rest of the room in this subset
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="South")   


# 2. "North" for block 2
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.orient"]) == "N",]		# only room facing "North"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms facing "North"


plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o},sep="")))

     # add one line for each of the rest rooms     
     for (guestRoom in rooms)	# loopping through the rest of the room in this subset
     {
  	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="North")   


# 3. "Corner" for block 3
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"room.corner"]) == "C",]		# only room at "Corner"
rooms <- unique(as.character(data.subset[,"room.name"]))					# the rooms at "Corner"


plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.vintage"])=="New",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o},sep="")))

     # add one line for each of the rest rooms          
     for (guestRoom in rooms)
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT"],	# pick one room to start the plot loess fitted power vs T
              data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT.loess"],	# pick one room to start the plot loess fitted power vs T
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.vintage"])=="New",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
    }
legend(min(x.limit), max(y.limit),legend=c("New","Old"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Corner")   


# 4. "Thermostat" yes/no for New PTHP
data.subset <- myData.allPTHP[as.character(myData.allPTHP[,"PTHP.vintage"]) == "New",]		# ALL "New" PTHP units
rooms <- unique(as.character(data.subset[,"room.name"]))					# THE ROOMS WITH "nEW" pthp UNITS

plot(data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT"],		# pick one room to start the plot loess fitted power vs T
     data.subset[(as.character(data.subset[,"room.name"]) == rooms[1]),"deltaT.loess"],		# pick one room to start the plot loess fitted power vs T
     col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == rooms[1],"PTHP.thermostat"])=="T1",1,2)][1],
     type = "l", lty = 1,lwd = 0.25,ylim = y.limit,xlim = x.limit,
     xlab = expression(paste(Delta(T)," F"^{o},sep="")),
     ylab = "Loess Fitted (degree=1) PTHP Power (W)",
     main = expression(paste("Loess Fitted PTHP Power (W) vs ",Delta(T)," F"^{o},sep="")))

     # add one line for each of the rest rooms               
     for (guestRoom in rooms)
     {
	lines(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT"],
	      data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"deltaT.loess"],
	      col = col.array[ifelse(as.character(data.subset[as.character(data.subset[,"room.name"]) == guestRoom,"PTHP.thermostat"])=="T1",1,2)][1],
	      type = "l",lty = 1,lwd = 0.25)
     }
legend(min(x.limit), max(y.limit),legend=c("Yes","No"),col=c("red","blue"),lty=c(1,1),text.col = c("red","blue"),title="Thermostat")   

				  
	
dev.off()

# -------------------------------------------------------------------------------------------------
# I: verify the rental status information are all right
# -------------------------------------------------------------------------------------------------
pdf(file = FL.RENT.PDF,paper="a4r", width=0, height=0)


y.limit <- c(0,1)		# ***************** round to the 10th place of the maximum solar radiation ***************** )
x.limit <- c(min(myData.allPTHP[,"date.time"]),max(myData.allPTHP[,"date.time"]))		# ***************** round to the 10th place of the maximum solar radiation ***************** )
for (guestRoom in guestRooms)
{
	cat(paste("\nG. rental status: ",paste("GR",guestRoom,sep=""),"!\n"),file=FL.LOG,append=TRUE)
	cat(paste("\nG. rental status: ",paste("GR",guestRoom,sep=""),"!\n"))

	idx.PTHP <- as.character(myData.allPTHP[,"room.name"]) == paste("GR",guestRoom,sep="")
	PTHP.subset <- myData.allPTHP[idx.PTHP,]
	PTHP.subset <- PTHP.subset[order(PTHP.subset[,"date.time"]),]

	idx.RENT <- as.character(myRental[,"MeterLocation"]) == paste("GR",guestRoom,sep="")
	RENT.subset <- myRental[idx.RENT,]	
	RENT.subset <- RENT.subset[order(RENT.subset[,"DateTime"]),]

	plot(PTHP.subset[,"date.time"],
	     PTHP.subset[,"rental"],
	     col = col.array[1],
	     type = "p",
	     lty = 1,
	     cex = 1,
	     lwd = 0.5,
	     ylim = y.limit,
	     xlab = "Date",
	     ylab = "rental status",
	     main = "Rental Status Verification")

	lines(RENT.subset[,"DateTime"],
	      RENT.subset[,"RentalStatus"],
		      col = col.array[2],
		      type = "h",
		      lty = 1,
		      pch = 16,
		      cex = 0.5,
		      lwd = 0.5)
 
	legend(min(xlim4plot), max(y.limit)*.80,legend=c("New","Old"),
				  col=c("red","blue"),
				  lty=c(1,1),
				  text.col = c("red","blue"))	
}


dev.off()

#
# output the processed data
#
cat(",",file = FL.PTHP,  append = TRUE)
cat(",",file = FL.RENTAL,append = TRUE)
write.table(myData.allPTHP,file = FL.PTHP,  sep = ",", col.names = TRUE, row.names = TRUE, append = TRUE)	
write.table(myRental,      file = FL.RENTAL,sep = ",", col.names = TRUE, row.names = TRUE, append = TRUE)	




# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [34_CrownePlaza_PTHP_Power_vs_T_Plot_daily.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

