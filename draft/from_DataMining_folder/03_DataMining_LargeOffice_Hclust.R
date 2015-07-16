#
# 03_DataMining_LargeOffice_Hclust.R 
# 
# Feb 27, 2015: re-visit the scripts
#
# Feb 3, 2015: load the data for each CZ and each EEMs
#
# Large Office:
# Simulated at Miami for all of the individual EEMs
#
#

# eliminate all stuff
rm(list = ls(all = TRUE))

# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

library("RSNNS")
library("chron")
library("reshape2")
library("lattice")
library("ggplot2")
library("mclust")
library(fpc)

col.array <- c("red","blue","green","magenta","cyan","purple","brown","black")

# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

this.method <- "Hclust"
# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Project <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining"
	Path.Sim     <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining"
	Path.Script  <- "/phome/comstd/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/0_scripts"
}else{
	Path.Project <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining"
	Path.Sim     <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining"
	Path.Script  <- "X:/CtrlBenefit/simulation_for_DataMining/OfficeLarge/sim_selected_4_DataMining/prepare.data.for.DataMining/0_scripts"
}
setwd(Path.Script)

Path.IN  <- paste(Path.Sim,"02_Plotting_LargeOffice",         sep="/")
Path.OUT <- paste(Path.Sim,"03_DataMining_LargeOffice_Hclust",sep="/")
if (!file.exists(Path.IN)){print(paste(Path.IN," does not exist. Check why!",sep=""));die}
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}

FL.LOG <- paste(Path.OUT,"03_DataMining_LargeOffice_Hclust.log",sep="/")
if (file.exists(FL.LOG)){print(paste(FL.LOG," exist. Delete it!"));file.remove(FL.LOG)}		

# 1. load multiplot function
source("multipleplot.R")
cat(paste("1. insert a [multipleplot] function for ggplot2.\n",sep=""))
cat(paste("1. insert a [multipleplot] function for ggplot2.\n",sep=""),file=FL.LOG,append=TRUE)	

# 2. CZ arrays
CZ.arrays <- c("Miami","Houston","Phoenix","Atlanta","LosAngeles","LasVegas","SanFrancisco","Baltimore","Albuquerque","Seattle","Chicago","Denver","Minneapolis","Helena","Duluth","Fairbanks")
cat(paste("2. specify inptu/out file/folder.\n",sep=""))
cat(paste("2. specify inptu/out file/folder.\n",sep=""),file=FL.LOG,append=TRUE)	



for (this.CZ in c("SanFrancisco","Albuquerque"))	#for (this.CZ in CZ.arrays)
{	
	# 3. subfolder for each CZ	
	Path.CZ.IN  <- paste(Path.IN, this.CZ,sep="/")
	Path.CZ.OUT <- paste(Path.OUT,this.CZ,sep="/")
	if (!file.exists(Path.CZ.IN)) {print(paste(Path.CZ.IN," does not exist. Check why!",sep=""));die}
	if (!file.exists(Path.CZ.OUT)){print(paste("NOT existing:",Path.CZ.OUT));dir.create(Path.CZ.OUT,showWarnings=TRUE,recursive=TRUE)}
	cat(paste("3. [",this.CZ,"]: Output folder has been set.\n",sep=""))
	cat(paste("3. [",this.CZ,"]: Output folder has been set.\n",sep=""),file=FL.LOG,append=TRUE)	


	# -----------------------------------------------------------------------------------------
	# 4. extract all Rdata file name in this CZ folder
	# -----------------------------------------------------------------------------------------
	EEMs.arrays <- grep("\\.Rdata",list.files(Path.CZ.IN),value=TRUE)				# all EEM objects
	cat(paste("4. [",this.CZ,"]: extract all R object files into [EEMs.arrays].\n",sep=""))
	cat(paste("4. [",this.CZ,"]: extract all R object files into [EEMs.arrays].\n",sep=""),file=FL.LOG,append=TRUE)	
	
	# 5. delete "GatheringData_OfficeLarge.Rdata)
	EEMs.arrays <- grep("GatheringData",EEMs.arrays,value=TRUE,invert=TRUE)
	cat(paste("5. [",this.CZ,"]: delete [atheringData].\n",sep=""))
	cat(paste("5. [",this.CZ,"]: delete [atheringData].\n",sep=""),file=FL.LOG,append=TRUE)	
	
	# 6. stripping EEM names
	EEMs.name   <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\1_\\2_\\3",EEMs.arrays)
	EEMs.fuel   <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\1",EEMs.arrays)
	EEMs.num    <- as.numeric(sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\2",EEMs.arrays))
	EEMs.label  <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\3",EEMs.arrays)
	EEMs.saving <- sub("(.*)_(.*)_(.*)_(.*)_(.*)_(.*)","\\4",EEMs.arrays)
	cat(paste("6. [",this.CZ,"]: get EEM related information.\n",sep=""))
	cat(paste("6. [",this.CZ,"]: get EEM related information.\n",sep=""),file=FL.LOG,append=TRUE)	
	
	# 7. prepare a data frame consists of the information of all EEMs
	EEMs.df <- data.frame(EEMs.names  = EEMs.name,
	                      EEMs.fuel   = EEMs.fuel,
	                      EEMs.num    = EEMs.num,
	                      EEMs.label  = EEMs.label,
	                      EEMs.saving = EEMs.saving)
	cat(paste("7. [",this.CZ,"]: put all EEMs related into [EEMs.df].\n",sep=""))
	cat(paste("7. [",this.CZ,"]: put all EEMs related into [EEMs.df].\n",sep=""),file=FL.LOG,append=TRUE)	


	# -----------------------------------------------------------------------------------------
	# -----------------------------------------------------------------------------------------
	# -----------------------------------------------------------------------------------------
	# loopping through the EEMs                     
	# -----------------------------------------------------------------------------------------
	# -----------------------------------------------------------------------------------------
	# -----------------------------------------------------------------------------------------
	for (this.idx in seq(1,length(EEMs.name)))
	{
		this.EEM.name   = EEMs.name[this.idx]
		this.EEM.fuel   = EEMs.fuel[this.idx]
		this.EEM.num    = EEMs.num[this.idx]
		this.EEM.label  = EEMs.label[this.idx]
		this.EEM.saving = EEMs.saving[this.idx]
		this.EEM.Rdata  = EEMs.arrays[this.idx]
		
		# 
		# 11. current EEM being processed.
		#
		this.EEM.pdf    = sub("\\.Rdata",".pdf",this.EEM.Rdata)
		this.EEM.clsLAB = sub("\\.Rdata","_cluster_Label.csv",this.EEM.Rdata)
		this.EEM.clsDAT = sub("\\.Rdata","_cluster_Data.csv", this.EEM.Rdata)
		cat(paste("\n\n11. [",this.CZ,"]-[",this.EEM.name,"]: processing......................\n",sep=""))
		cat(paste("\n\n11. [",this.CZ,"]-[",this.EEM.name,"]: processing......................\n",sep=""),file=FL.LOG,append=TRUE)	

		#
		# 12. define a character string for the plot title
		#
		thisLearn.string <- paste(this.method,sep="")
		cat(paste("12. [",this.CZ,"]-[",this.EEM.name,"]: character string for plot title.\n",sep=""))
		cat(paste("12. [",this.CZ,"]-[",this.EEM.name,"]: character string for plot title.\n",sep=""),file=FL.LOG,append=TRUE)	

		# use different learn rate for electricity and naturaal gas
		if (length(grep("Elec",this.EEM.name))>0)
		{
			thisInit   <- 0.85
			thisLearn  <- 0.98
			thisUpdate <- 0.98		                                                
		}else if (length(grep("Gas",this.EEM.name))>0)
		{
			thisInit   <- 0.85
			thisLearn  <- 0.98
			thisUpdate <- 0.98
		}else{
			thisInit   <- 0.85
			thisLearn  <- 0.98
			thisUpdate <- 0.98
		}
		thisLearn.string <- paste("INI=",thisInit," LR=",thisLearn," Update=",thisUpdate,sep="")
		cat(paste("12B. [",this.CZ,"]-[",this.EEM.name,"]: parameters.\n",sep=""))
		cat(paste("12B. [",this.CZ,"]-[",this.EEM.name,"]: parameters.\n",sep=""),file=FL.LOG,append=TRUE)	


		#
		# 13. generate files for each EEMs
		#
		FL.IN.OBJ     <- paste(Path.CZ.IN, this.EEM.Rdata,sep="/")
		FL.OUT.OBJ    <- paste(Path.CZ.OUT,this.EEM.Rdata,sep="/")
		FL.OUT.PDF    <- paste(Path.CZ.OUT,this.EEM.pdf,  sep="/")
		FL.OUT.CLSLAB <- paste(Path.CZ.OUT,this.EEM.clsLAB,sep="/")
		FL.OUT.CLSDAT <- paste(Path.CZ.OUT,this.EEM.clsDAT,sep="/")
		if (!(file.exists(FL.IN.OBJ)))  {print(paste(FL.IN.OBJ," does exist. Check Why!"));die}
		if  (file.exists(FL.OUT.OBJ))   {print(paste(FL.OUT.OBJ," exist. Delete it!"));file.remove(FL.OUT.OBJ)}		
		if  (file.exists(FL.OUT.PDF))   {print(paste(FL.OUT.PDF," exist. Delete it!"));file.remove(FL.OUT.PDF)}		
		if  (file.exists(FL.OUT.CLSLAB)){print(paste(FL.OUT.CLSLAB," exist. Delete it!"));file.remove(FL.OUT.CLSLAB)}			
		if  (file.exists(FL.OUT.CLSDAT)){print(paste(FL.OUT.CLSDAT," exist. Delete it!"));file.remove(FL.OUT.CLSDAT)}	
		cat(paste("13. [",this.CZ,"]-[",this.EEM.name,"]: prepare files for output.\n",sep=""))
		cat(paste("13. [",this.CZ,"]-[",this.EEM.name,"]: prepare files for output.\n",sep=""),file=FL.LOG,append=TRUE)	



		# ---------------------------------------------------------------------------------
		# 14. load the data which contains 
		#					[myData.base]
		#					[myData.advn]
		#					[myData.merged.long]
		#					[myData.4.weeklyLong]
		#					[myData.4.dailyLong]
		# ---------------------------------------------------------------------------------
		load(FL.IN.OBJ)
		# myData.4.dailyLong[,"month.lab"]  <- myData.4.dailyLong[,"month"]
		# myData.4.dailyLong[,"month.lab"]  <- factor(myData.4.dailyLong[,"month.lab"],levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)
		# myData.4.weeklyLong[,"month.lab"] <- myData.4.weeklyLong[,"month"]
		# myData.4.weeklyLong[,"month.lab"] <- factor(myData.4.weeklyLong[,"month.lab"],levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)
		cat(paste("14. [",this.CZ,"]-[",this.EEM.name,"]: load data from [",FL.IN.OBJ,"].\n",sep=""))
		cat(paste("14. [",this.CZ,"]-[",this.EEM.name,"]: load data from [",FL.IN.OBJ,"].\n",sep=""),file=FL.LOG,append=TRUE)
		
		
		
		#
		# 15. output: immediately save out
		#
		save(myData.base,myData.advn,myData.merged.long,myData.4.weeklyLong,myData.4.dailyLong,file=FL.OUT.OBJ)
		cat(paste("15. [",this.CZ,"]-[",this.EEM.name,"]: save into [",FL.OUT.OBJ,"].\n",sep=""))
		cat(paste("15. [",this.CZ,"]-[",this.EEM.name,"]: save into [",FL.OUT.OBJ,"].\n",sep=""),file=FL.LOG,append=TRUE)

		# ----------------------------------------------------------------------------------------
		# 16. open the pdf file
		# ----------------------------------------------------------------------------------------
		pdf(file = FL.OUT.PDF,paper="special", width=17, height=11,bg = "transparent")
		cat(paste("16. [",this.CZ,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""))
		cat(paste("16. [",this.CZ,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""),file=FL.LOG,append=TRUE)	

		# --------------------------------------------------------------
		# 17. manually specify a scaling factor in order to use ggplot2 to plot T as well.
		# --------------------------------------------------------------			
		scaling.factor <- (max(myData.4.weeklyLong[,c("EnergyGJ")]) / max(myData.4.weeklyLong[,c("T.dryBulb")])) * 2
		cat(paste("17. [",this.CZ,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""))
		cat(paste("17. [",this.CZ,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""),file=FL.LOG,append=TRUE)	

		# 
		# the following code may not be used at all
		#
		myTmp1 <- myData.4.weeklyLong[,c("Date.Time","T.dryBulb","EEM")]			
		myTmp1[,"T.dryBulb"] <- myTmp1[,"T.dryBulb"] * scaling.factor
		myTmp1[,"ID"] <- "T.dryBulb (scaled)"
		names(myTmp1) <- sub("T.dryBulb","value",names(myTmp1))


		myTmp2 <- myData.4.weeklyLong[,c("Date.Time","EnergyGJ","EEM")]
		myTmp2[,"ID"] <- "EnergyGJ"
		names(myTmp2) <- sub("EnergyGJ","value",names(myTmp2))

		myTmp3 <- rbind(myTmp1,myTmp2)
		myTmp3[,"Variable"] <- paste(myTmp3[,"EEM"],myTmp3[,"ID"],sep="_")




		# ----------------------------------------------------------------------------------------
		# PLOTTING RAW DATA ..................
		# ----------------------------------------------------------------------------------------


		# --------------------------------------------------------------
		# 18. plot weekly plot in each month
		# --------------------------------------------------------------
		p.weekly1 <- qplot(data=myData.4.weeklyLong,x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM,facets=month.lab~week.idx.in.month,geom="line") 
		p.weekly1 <- p.weekly1 + theme(legend.position="top") 
		p.weekly1 <- p.weekly1 + labs(title=paste(paste("Raw Data: ",paste(this.EEM.fuel,this.EEM.num,this.EEM.label,sep="_"),sep="")," (",this.EEM.saving,") Weekly Profile of Weeks in Each Month (",this.CZ,")",sep=""))	
		plot(p.weekly1)
		cat(paste("18. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly profile for each month.\n",sep=""))
		cat(paste("18. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly profile for each month.\n",sep=""),file=FL.LOG,append=TRUE)	


		# --------------------------------------------------------------
		# 19. plot weekly plot in conseccutive weeks
		# --------------------------------------------------------------
		p.weekly2 <- qplot(data=myData.4.weeklyLong,x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM,facets=~week.idx,geom="line") 
		p.weekly2 <- p.weekly2 + theme(legend.position="top") 
		p.weekly2 <- p.weekly2 + labs(title=paste(paste("Raw Data: ",paste(this.EEM.fuel,this.EEM.num,this.EEM.label,sep="_"),sep="")," (",this.EEM.saving,") Weekly Profile of All Weeks in the Year (",this.CZ,")",sep=""))		
		plot(p.weekly2)
		cat(paste("19. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly plot in conseccutive weeks.\n",sep=""))
		cat(paste("19. [",this.CZ,"]-[",this.EEM.name,"]: plot weekly plot in conseccutive weeks.\n",sep=""),file=FL.LOG,append=TRUE)	


		# --------------------------------------------------------------
		# 20. plot daily plot in each month
		# --------------------------------------------------------------
		p.daily1 <- qplot(data=myData.4.dailyLong,x=hour,y=EnergyGJ,group=EEM,color=EEM,facets=month.lab~day,geom="line")  
		p.daily1 <- p.daily1 + theme(legend.position="top") 
		p.daily1 <- p.daily1 + labs(title=paste(paste("Raw Data: ",paste(this.EEM.fuel,this.EEM.num,this.EEM.label,sep="_"),sep="")," (",this.EEM.saving,") Daily Profile of All Days in Each Month (",this.CZ,")",sep=""))	
		plot(p.daily1)
		cat(paste("20. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month\n",sep=""))
		cat(paste("20. [",this.CZ,"]-[",this.EEM.name,"]: plot daily plot in each month.\n",sep=""),file=FL.LOG,append=TRUE)	



		# ----------------------------------------------------------------------------------------
		# DATA MINING ..................
		# ----------------------------------------------------------------------------------------

 		# ---------------------------------------------------------------------------------
 		# ---------------------------------------------------------------------------------
 		# ---------------------------------------------------------------------------------
 		# Data Mining: all days together
 		# ---------------------------------------------------------------------------------
 		# ---------------------------------------------------------------------------------
 		# ---------------------------------------------------------------------------------
 		# 21. to avoid worngly taking existing data frame,
 		df_name <- "class.days";	if (exists(df_name) && is.data.frame(get(df_name))){rm("class.days")}
 		df_name <- "myData.Work.long";	if (exists(df_name) && is.data.frame(get(df_name))){rm("myData.Work.long")}
 		df_name <- "myData.Work.wide";	if (exists(df_name) && is.data.frame(get(df_name))){rm("myData.Work.wide")}
		cat(paste("21. [",this.CZ,"]-[",this.EEM.name,"]: to avoid worngly taking existing data frame, delete them first.\n",sep=""))
		cat(paste("21. [",this.CZ,"]-[",this.EEM.name,"]: to avoid worngly taking existing data frame, delete them first..\n",sep=""),file=FL.LOG,append=TRUE)	
 
 		
 		for (this.subset in c("artificial1","artificial2","artificial3","July","July-August","WeekDay","All Data","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Holiday"))
 		{
 		
			# -----------------------------------------------------------------
			# 22.[myData.Work.long]: all days in the two years
			#                        only keep the "base" and the current "EEM"
			#                        only keep "EnergyGJ","EEM","hour","date" fields
			#                        distinguish the "base" and "EEM" using "date.new" which put "base" in another year for displaying
			# -----------------------------------------------------------------
			if (this.subset == "artificial1")
			{
				myData.Work.long <- subset(myData.4.dailyLong,subset = month %in% c(7),            select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))
				
				# artificially assign 1 to 8:00 am to 17:00 pm to 2010 and 2 to 2009
				myData.Work.long[,"EnergyGJ"] <-  0
				myData.Work.long[myData.Work.long[,"hour"] %in% c(8,9,10,11,12,13,14,15,16,17),"EnergyGJ"] <- 1 
				myData.Work.long[((sub("(.*)_(.*)_(.*)","\\3",myData.Work.long[,"EEM"]) %in% "base") & (myData.Work.long[,"hour"] %in% c(8,9,10,11,12,13,14,15,16,17))),"EnergyGJ"] <- 2  
				
				
				myData.Work.long[,"EnergyGJ"] <- myData.Work.long[,"EnergyGJ"] + rnorm(dim(myData.Work.long)[1],0,0.025)
				
				no.cls <- 10
				cluster.range <- c(1:5)
				
			}else if (this.subset == "artificial2")
			{
				myData.Work.long <- subset(myData.4.dailyLong,subset = month %in% c(7),            select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))
				
				# artificially assign 1 to 8:00 am to 17:00 pm to 2010 and 2 to 2009
				myData.Work.long[,"EnergyGJ"] <-  0
				myData.Work.long[myData.Work.long[,"hour"] %in% c(8,9,10,11,12,13,14,15,16,17),"EnergyGJ"] <- 1
				myData.Work.long[((sub("(.*)_(.*)_(.*)","\\3",myData.Work.long[,"EEM"]) %in% "base") & (myData.Work.long[,"hour"] %in% c(8,9,10,11,12,13,14,15,16,17,18))),"EnergyGJ"] <- 1  
				
				myData.Work.long[,"EnergyGJ"] <- myData.Work.long[,"EnergyGJ"] + rnorm(dim(myData.Work.long)[1],0,0.025)
				
				no.cls <- 20
				cluster.range <- c(1:5)
				
			}else if (this.subset == "artificial3")
			{
				myData.Work.long <- subset(myData.4.dailyLong,subset = month %in% c(7),            select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))
				
				# artificially assign 1 to 8:00 am to 17:00 pm to 2010 and 2 to 2009
				myData.Work.long[,"EnergyGJ"] <-  0
				myData.Work.long[myData.Work.long[,"hour"] %in% c(8,9,10,11,12,13,14,15,16,17),"EnergyGJ"] <- 1 
				myData.Work.long[((sub("(.*)_(.*)_(.*)","\\3",myData.Work.long[,"EEM"]) %in% "base") & (myData.Work.long[,"hour"] %in% c(8,9,10,11,12,13,14,15,16,17,18))),"EnergyGJ"] <- 2 
				
				myData.Work.long[,"EnergyGJ"] <- myData.Work.long[,"EnergyGJ"] + rnorm(dim(myData.Work.long)[1],0,0.025)
				
				no.cls <- 20
				cluster.range <- c(1:5)
				
			}else if (this.subset == "July")
			{
				myData.Work.long <- subset(myData.4.dailyLong,subset = month %in% c(7),            select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))	
				no.cls <- 20
				cluster.range <- c(5:15)
			}else if (this.subset == "July-August")
			{
				myData.Work.long <- subset(myData.4.dailyLong,subset = month %in% c(7,8),          select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))
				no.cls <- 20
				cluster.range <- c(5:15)
			}else if (this.subset == "WeekDay")
			{
				myData.Work.long <- subset(myData.4.dailyLong,subset=(day.type.lab == "Weekday"),select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))			
				no.cls <- 20
				cluster.range <- c(11:20)

			}else if (this.subset %in% c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Holiday"))
			{
				myData.Work.long <- subset(myData.4.dailyLong,subset=(day.week.lab == this.subset),select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))			
				if (this.subset == "Holiday")
				{
					no.cls <- 5
				}
				cluster.range <- c(5:15)
			}else if (this.subset == "All Data")
			{
				myData.Work.long <- subset(myData.4.dailyLong,                                     select = c("T.dryBulb","EnergyGJ","EEM","hour","date","day.week.lab","day.type.lab","week.idx"))	
				no.cls <- 20
				cluster.range <- c(11:20)
			}
			cat(paste("22. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: [myData.Work.long] is a subset of [myData.4.dailyLong] used for data mining.\n",sep=""))
			cat(paste("22. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: [myData.Work.long] is a subset of [myData.4.dailyLong] used for data mining.\n",sep=""),file=FL.LOG,append=TRUE)	

			#
			# 23. purposely assign year to 2009 for the baseline data and to 2010 for the EEM data
			#
			myData.Work.long[grep("_base",myData.Work.long[,"EEM"]),"date"] <- sub("2010","2009",myData.Work.long[grep("_base",myData.Work.long[,"EEM"]),"date"])
			cat(paste("23. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: assign 2009 to baseline year and 2010 to EEM year.\n",sep=""))
			cat(paste("23. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: assign 2009 to baseline year and 2010 to EEM year.\n",sep=""),file=FL.LOG,append=TRUE)	


			#
			# 24. use the artificial 2009 and 2010 year label to generate a new [date.new] field
			# 		
			myData.Work.long[,"date.new"] <- sub("2009","09",sub("2010","10",myData.Work.long[,"date"]))
			cat(paste("24. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: the year is a two digits so turn 2009 to 09 and 2010 to 10 for [date.new].\n",sep=""))
			cat(paste("24. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: the year is a two digits so turn 2009 to 09 and 2010 to 10 for [date.new].\n",sep=""),file=FL.LOG,append=TRUE)	


			# seems like the following are not used.  turn the long format to wide format
			var.intact   <- c("date","hour")		# fields will not be changed
			var.expand   <- c("EEM")			# fields will be used to expand
			var.EnergyGJ <- c("EnergyGJ.norm")		# fields of the measurement
			var.TdryBulb <- c("T.dryBulb.norm")		# fields of the measurement

			# -----------------------------------------------------------------
			# 25. normalize the energy consumption data in [myData.Work.long]
			# -----------------------------------------------------------------
			myData.Work.long[,"EnergyGJ.norm"]  <- (myData.Work.long[,"EnergyGJ"]  - min(myData.Work.long[,"EnergyGJ"]))  / (max(myData.Work.long[,"EnergyGJ"])  - min(myData.Work.long[,"EnergyGJ"]))
			myData.Work.long[,"T.dryBulb.norm"] <- (myData.Work.long[,"T.dryBulb"] - min(myData.Work.long[,"T.dryBulb"])) / (max(myData.Work.long[,"T.dryBulb"]) - min(myData.Work.long[,"T.dryBulb"]))
			cat(paste("25. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: normalize the data in [myData.Work.long].\n",sep=""))
			cat(paste("25. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: normalize the data in [myData.Work.long]..\n",sep=""),file=FL.LOG,append=TRUE)	

			# -----------------------------------------------------------------
			# 26. [myData.Work.wide] for data mining the daily profiles
			# -----------------------------------------------------------------
			myData.Work.wide.EnergyGJ <- dcast(myData.Work.long,date.new + EEM ~ hour,value.var = var.EnergyGJ)
			myData.Work.wide.TdryBulb <- dcast(myData.Work.long,date.new + EEM ~ hour,value.var = var.TdryBulb)
			cat(paste("26. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: turn the long format to a wide format having 24 hour a day.\n",sep=""))
			cat(paste("26. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: turn the long format to a wide format having 24 hour a day.\n",sep=""),file=FL.LOG,append=TRUE)	


			# 27. use [date.new] as row names
			# myData.Work.wide[grep("_base",myData.Work.wide[,"EEM"]),"date"] <- sub("2010","2009",myData.Work.wide[grep("_base",myData.Work.wide[,"EEM"]),"date"])
			# myData.Work.wide[,"date"] <- sub("2009","09",sub("2010","10",myData.Work.wide[,"date"]))
			row.names(myData.Work.wide.EnergyGJ) <- as.Date(myData.Work.wide.EnergyGJ[,"date.new"],"%m/%d/%y")
			row.names(myData.Work.wide.TdryBulb) <- as.Date(myData.Work.wide.TdryBulb[,"date.new"],"%m/%d/%y")
			cat(paste("27. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use date.new as row names.\n",sep=""))
			cat(paste("27. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use date.new as row names.\n",sep=""),file=FL.LOG,append=TRUE)	

			# ---------------------------------------------------------------------------------
			# 28. use "GJ.h" for fields name in [myData.Work.wide.EnergyGJ]
			# ---------------------------------------------------------------------------------
			field1 <- c("date.new","EEM")
			field2 <- names(myData.Work.wide.EnergyGJ)[!(names(myData.Work.wide.EnergyGJ) %in% field1)]
			myData.GJ.part1  <- myData.Work.wide.EnergyGJ[,field1]
			myData.GJ.part2  <- myData.Work.wide.EnergyGJ[,field2]
			myData.Work.wide.EnergyGJ  <- cbind(myData.GJ.part1,myData.GJ.part2)
		  names(myData.Work.wide.EnergyGJ) <- c(field1,paste("GJ.h",field2,sep=""))
			cat(paste("28. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: a re-organization on the GJ data.\n",sep=""))
			cat(paste("28. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: a re-organization.\n",sep=""),file=FL.LOG,append=TRUE)	

			# ---------------------------------------------------------------------------------
			# 29. use "T.h" for fields name in [myData.Work.wide.TdryBulb]
			# ---------------------------------------------------------------------------------
			field1 <- c("date.new","EEM")
			field2 <- names(myData.Work.wide.TdryBulb)[!(names(myData.Work.wide.TdryBulb) %in% field1)]
			myData.T.part1  <- myData.Work.wide.TdryBulb[,field1]
			myData.T.part2  <- myData.Work.wide.TdryBulb[,field2]
			myData.Work.wide.TdryBulb  <- cbind(myData.T.part1,myData.T.part2)
		  names(myData.Work.wide.TdryBulb) <- c(field1,paste("T.h",field2,sep=""))
			cat(paste("29. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: a re-organization.\n",sep=""))
			cat(paste("29. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: a re-organization.\n",sep=""),file=FL.LOG,append=TRUE)	
		  

			# ---------------------------------------------------------------------------------
			# 30. decide if we want to merge GJ and T of the 24 hours OR
			#                       just use GJ data
			# ---------------------------------------------------------------------------------
			# myData.Work.wide <- merge(myData.Work.wide.EnergyGJ,myData.Work.wide.TdryBulb)
			  myData.Work.wide <- myData.Work.wide.EnergyGJ
			cat(paste("30. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use the date as row name for the data frame.\n",sep=""))
			cat(paste("30. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use the date as row name for the data frame.\n",sep=""),file=FL.LOG,append=TRUE)	

			# -----------------------------------------------------------------
			# 31. [myData.Work.long] and [myData.Work.wide] are ready
			# -----------------------------------------------------------------
			myData.Work.long[,"date.new"] <- as.Date(myData.Work.long[,"date.new"],"%m/%d/%y")
			myData.Work.wide[,"date.new"] <- as.Date(myData.Work.wide[,"date.new"],"%m/%d/%y")
			cat(paste("31. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: have the [date.new] field in [myData.Work.long] and [myData.Work.wide].\n",sep=""))
			cat(paste("31. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: have the [date.new] field in [myData.Work.long] and [myData.Work.wide].\n",sep=""),file=FL.LOG,append=TRUE)	


			# 32. create a mapping table between [date.new] and [day.type.lab] and [day.weel.lab]
			myData.dayMapping  <- myData.Work.long[myData.Work.long[,"hour"] == 0,c("EEM","date","day.week.lab","day.type.lab","week.idx","date.new")]		
	      row.names(myData.dayMapping) <- myData.dayMapping[,"date.new"]
			cat(paste("32. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use the date as row name for the data frame.\n",sep=""))
			cat(paste("32. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: use the date as row name for the data frame.\n",sep=""),file=FL.LOG,append=TRUE)	


			# -----------------------------------------------------------------
			# 33. [myData.Work] is the one used for data mining, consist of only variables 
			# -----------------------------------------------------------------			
			myDate      <- myData.Work.wide[,c(1,2)]
			myData.Work <- myData.Work.wide[,-c(1,2)]
			names(myData.Work) <- paste("hour",names(myData.Work),sep="")
			cat(paste("33. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: only keep the data for Data Mining.\n",sep=""))
			cat(paste("33. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: only keep the data for Data Mining.\n",sep=""),file=FL.LOG,append=TRUE)	

			### #
			### # nget min and max
			### #
			### min.arrays <- matrix(rep(apply(myData.Work,1,min),each=dim(myData.Work)[2]),nrow=dim(myData.Work)[1],byrow=TRUE)
			### max.arrays <- matrix(rep(apply(myData.Work,1,max),each=dim(myData.Work)[2]),nrow=dim(myData.Work)[1],byrow=TRUE)
			###     names(min.arrays) <-     names(myData.Work)
			###     names(max.arrays) <-     names(myData.Work)		
			### row.names(min.arrays) <- row.names(myData.Work)
			### row.names(max.arrays) <- row.names(myData.Work)
			### cat(paste("33B. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: calculate min and max arrays.\n",sep=""))
			### cat(paste("33B. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: calculate min and max arrays.\n",sep=""),file=FL.LOG,append=TRUE)	
			### 
			### #
			### # normalize
			### #
			### myData.Work.norm <- (myData.Work - min.arrays) / (max.arrays - min.arrays)
			### cat(paste("33C. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: normalize.\n",sep=""))
			### cat(paste("33C. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: normalize.\n",sep=""),file=FL.LOG,append=TRUE)	
			
			myData.Work.norm <- myData.Work
			cat(paste("33D. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: does not normalize.\n",sep=""))
			cat(paste("33D. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: does not normalize.\n",sep=""),file=FL.LOG,append=TRUE)	


			# -----------------------------------------------------------------
			# 41. Hclust 
			# -----------------------------------------------------------------
			no.object   <- dim(myData.Work)[1]
			no.variable <- dim(myData.Work)[2]
			if (no.object >= no.variable)
			{ 
				# calculate distance
				dist.matrix <- dist(myData.Work)			# The euclidean distance
				
				# agglomeration method 
				myModel.single   <- hclust(dist.matrix,method = "single")
				thisLearn.string <- paste("Hierarchical Cluster: single")				
				plot(myModel.single,main = paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))

				myModel.complete <- hclust(dist.matrix,method = "complete")
				thisLearn.string <- paste("Hierarchical Cluster: complete")				
				plot(myModel.complete,main = paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep="")
				
				par(mfrow=c(1,2))				
				pamk.best <- pamk(myData.Work.norm)			# Partitioning around medoids with estimation of number of clusters
				no.cls.best1 <- pamk.best$nc
				cat("number of clusters estimated by optimum average silhouette width:",no.cls.best1, "\n")
				plot(pam(myData.Work, no.cls.best1),main = paste(this.subset,": fpc package",sep=""))

				asw <- numeric(no.cls)
				for (k in 2:no.cls)
				{
				    asw[[k]] <- pam(myData.Work, k) $ silinfo $ avg.width
				}
				no.cls.best2 <- which.max(asw)
				cat("silhouette-optimal number of clusters:", no.cls.best2, "\n")

				no.cls.best <- no.cls.best2


				
				myModel  <-  pam(myData.Work, no.cls.best,metric = "euclidean",medoids = NULL)
				cls.Work <-  (myModel)$clustering


				cat(paste("34. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: Hclust modeling.\n",sep=""))
				cat(paste("34. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: Hclust modeling.\n",sep=""),file=FL.LOG,append=TRUE)	

				cat(paste("35. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: Hclust predicting.\n",sep=""))
				cat(paste("35. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: Hclust predicting.\n",sep=""),file=FL.LOG,append=TRUE)	

				myResults.Work <- cbind(myDate,myData.Work.norm)
				cat(paste("36. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: combine clustering results with data.\n",sep=""))
				cat(paste("36. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: combine clustering results with data.\n",sep=""),file=FL.LOG,append=TRUE)	

				# assign cluster label
				myResults.Work[,"cls.Work"] <- myModel$clustering
				cat(paste("37. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: combine clustering results with data.\n",sep=""))
				cat(paste("37. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: combine clustering results with data.\n",sep=""),file=FL.LOG,append=TRUE)	

				#
				# no.cls.best <- max(myResults.Work[,"cls.Work"])
				thisLearn.string <- paste(this.method,": ",no.cls.best," clusters",sep="")
				cat(paste("41. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: add [cls.Work].\n",sep=""))
				cat(paste("41. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: add [cls.Work].\n",sep=""),file=FL.LOG,append=TRUE)	


				# ---------------------------------------------------------------------------------
				# 42. create [class.days] data frame to record the date, type of the day,EEM and class label
				# ---------------------------------------------------------------------------------
				class.days <- myResults.Work[,c("date.new","cls.Work")]
				row.names(class.days) <- class.days[,"date.new"]

				# add "day.type" and "day.week" into [days.class]
				class.days[,"day.type.lab"] <- NA
				class.days[,"day.week.lab"] <- NA
				class.days[,"EEM"]          <- NA

				class.days[,"day.type.lab"] <- myData.dayMapping[as.character(class.days[,"date.new"]),"day.type.lab"]
				class.days[,"day.week.lab"] <- myData.dayMapping[as.character(class.days[,"date.new"]),"day.week.lab"]
				class.days[,"EEM"]          <- myData.dayMapping[as.character(class.days[,"date.new"]),"EEM"]
				cls.levels                  <- paste("cls",sort(unique(class.days[,"cls.Work"])),sep="-")
				class.days[,"cluster"]      <- factor(paste("cls",class.days[,"cls.Work"],sep="-"),levels = cls.levels,labels = cls.levels,ordered = TRUE)
				cat(paste("clustering results on [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"],",sep=""),file=FL.OUT.CLSLAB,append=TRUE)
				write.table(class.days,file=FL.OUT.CLSLAB,sep=",",row.names=TRUE,col.names=TRUE,appen=TRUE)
				cat(paste("42. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: associate dates and class label.\n",sep=""))
				cat(paste("42. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: associate dates and class label.\n",sep=""),file=FL.LOG,append=TRUE)	


				# ---------------------------------------------------------------------------------
				# 43. add a class label to [myData.Work.wide] and [myData.Work.long]
				# ---------------------------------------------------------------------------------
				myData.Work.long[,"cls.Work"] <- -999
				myData.Work.long[,"cls.Work"] <- class.days[as.character(myData.Work.long[,"date.new"]),"cls.Work"]

				myData.Work.wide[,"cls.Work"] <- -999
				myData.Work.wide[,"cls.Work"] <- class.days[as.character(myData.Work.wide[,"date.new"]),"cls.Work"]	

				cls.levels <- paste("cls",sort(unique(myData.Work.long[,"cls.Work"])),sep="-")
				myData.Work.long[,"cluster"]  <- factor(paste("cls",myData.Work.long[,"cls.Work"],sep="-"),levels = cls.levels,labels = cls.levels,ordered = TRUE)
				myData.Work.wide[,"cluster"]  <- factor(paste("cls",myData.Work.wide[,"cls.Work"],sep="-"),levels = cls.levels,labels = cls.levels,ordered = TRUE)

				cat(paste("\n\nclustering results on [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"],",sep=""),file=FL.OUT.CLSDAT,append=TRUE)
				write.table(myData.Work.long,file=FL.OUT.CLSDAT,sep=",",row.names=TRUE,col.names=TRUE,appen=TRUE)				

				cat(paste("43. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: add [cls.Work] to [myData.Work.wide] and [myData.Work.long].\n",sep=""))
				cat(paste("43. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: add [cls.Work] to [myData.Work.wide] and [myData.Work.long].\n",sep=""),file=FL.LOG,append=TRUE)	


				# -----------------------------------------------------------------
				# 50. PLOTTING: profile of the classes
				# -----------------------------------------------------------------

				# ---------------------------------------------------------------------------------------
				# 51. plot1: (p.cluster): plot the profile of the clusters
				# ---------------------------------------------------------------------------------------
				if(length(grep("artificial",this.subset)))
				{
					p.cluster <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date.new,color=EEM,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}else{
					p.cluster.all <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date.new,color=cluster,shape=EEM,geom=c("line","point"))  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
					p.cluster <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date.new,color=EEM,facets=~cluster,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}
			      # p.cluster <- p.cluster + facet_grid(~cluster,scales="free_y")
				cat(paste("51. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters.\n",sep=""))
				cat(paste("51. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters.\n",sep=""),file=FL.LOG,append=TRUE)	

				# ---------------------------------------------------------------------------------------
				# 52. plot2: (p.cluster.dayType): [Cluster] vs [day.type.lab] vs [base|EEM]
				# ---------------------------------------------------------------------------------------
				# plot cluster results of all dates in both "base" and "EEM" in terms of [day.type.lab]
				if(length(grep("artificial",this.subset)))
				{				
					p.cluster.dayType <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date.new,color=EEM,facets=~day.type.lab,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}else{
					p.cluster.dayType <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date.new,color=EEM,facets=day.type.lab~cluster,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}
			      # p.cluster.dayType <- p.cluster.dayType + facet_grid(day.type.lab~cluster,scales="free_y")
				cat(paste("52. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters in terms of day type.\n",sep=""))
				cat(paste("52. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters in terms of day type.\n",sep=""),file=FL.LOG,append=TRUE)

				# ---------------------------------------------------------------------------------------
				# 53. plot 3: (p.cluster.dayinWeek): [Cluster] vs [day.week.lab] vs [base|EEM]
				# ---------------------------------------------------------------------------------------
				# plot cluster results of all dates in both "base" and "EEM" in terms of [day.week.lab]
				if(length(grep("artificial",this.subset)))
				{				
					p.cluster.dayinWeek <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date.new,color=EEM,facets=~day.week.lab,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}else{
					p.cluster.dayinWeek <- qplot(data=myData.Work.long,x=hour,y=EnergyGJ,group=date.new,color=EEM,facets=day.week.lab~cluster,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				}
			      # p.cluster.dayinWeek <- p.cluster.dayinWeek + facet_grid(day.week.lab~cluster,scales="free_y")
				cat(paste("53. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters in terms of day in the week.\n",sep=""))
				cat(paste("53. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot daily profile of all clusters in terms of day in the week.\n",sep=""),file=FL.LOG,append=TRUE)

				# ---------------------------------------------------------------------------------------
				# 54. plot 4: (p.cluster.dates): [Cluster] vs [Date] vs [base|EEM]
				# ---------------------------------------------------------------------------------------
				year.separator <- as.Date("01/01/10","%m/%d/%y")
				p.cluster.dates <- qplot(data=class.days,x=date.new,y=cluster,color=cluster,alpha=I(0.5),shape=EEM,geom="point")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [",this.subset,"] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
				p.cluster.dates <- p.cluster.dates + geom_vline(aes(xintercept = as.numeric(year.separator)),linetype=14,colour="black")
				cat(paste("54. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot object distribution in the clusters along the time axis.\n",sep=""))
				cat(paste("54. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: plot object distribution in the clusters along the time axis.\n",sep=""),file=FL.LOG,append=TRUE)

				# ---------------------------------------------------------------------------------------
				# 55. actual plotting
				# ---------------------------------------------------------------------------------------
				# print(p.cluster,       position=c(0, 0.5, 1, 1.0), more=TRUE)	# only work for lattice plots not for ggplot2 plots		
				# print(p.cluster.dates, position=c(0, 0.0, 1, 0.5))		# only work for lattice plots not for ggplot2 plots
				if(length(grep("artificial",this.subset,invert = TRUE)))	# for all non-artificial subset, plot this (all clusters and both baseline and EEM together
				{
					plot(p.cluster.all)
				}
				
				multiplot(p.cluster,           p.cluster.dates, cols=2)
								
				if(length(grep("July",this.subset)) | length(grep("July-August",this.subset)) | length(grep("All Data",this.subset)))
				{	
					multiplot(p.cluster.dayType,   p.cluster.dates, cols=2)	# data from complete month or year can be plotted in terms of day in the week or day type
					multiplot(p.cluster.dayinWeek, p.cluster.dates, cols=2)	# data from complete month or year can be plotted in terms of day in the week or day type
				}else if (length(grep("WeekDay",this.subset)))
				{

					multiplot(p.cluster.dayinWeek, p.cluster.dates, cols=2)	# data from a day type will only be plotted in terms of day in the week
				}
				cat(paste("55. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: actually generate the plots.\n",sep=""))
				cat(paste("55. [",this.CZ,"]-[",this.EEM.name,"]-[",this.subset,"]: actually generate the plots.\n",sep=""),file=FL.LOG,append=TRUE)
			}	# end of decision if number of objects is larger than the number of variables
		}		# end of subset of current EEM


		# ---------------------------------------------------------------------------------
		# ---------------------------------------------------------------------------------
		# ---------------------------------------------------------------------------------
		# 60. IV. Data Mining: weekly
		# ---------------------------------------------------------------------------------
		# ---------------------------------------------------------------------------------
		# ---------------------------------------------------------------------------------

		# to avoid worngly taking existing data frame,
		df_name <- "class.weeks";	if (exists(df_name) && is.data.frame(get(df_name))){rm("class.weeks")}
		df_name <- "myData.Work.long";	if (exists(df_name) && is.data.frame(get(df_name))){rm("myData.Work.long")}
		df_name <- "myData.Work.wide";	if (exists(df_name) && is.data.frame(get(df_name))){rm("myData.Work.wide")}

		# -----------------------------------------------------------------
		# 61. [myData.Work.long]: subsetting from [myData.4.dailyLong]
		#                         only keep the "base" and the current "EEM"
		#                         only keep "EnergyGJ","EEM","hour.in.week","week.idx" fields
		#                         distinguish the "base" and "EEM" using "week.idx.new" which put "base" week 1 to 52 and EEM in week 53 to 104
		# -----------------------------------------------------------------
		# delete the data from the last day which starts a incomplete new week
		myData.Work.long <- subset(myData.4.weeklyLong,subset=(week.idx != 53),select = c("EnergyGJ","EEM","hour.in.week","week.idx"))			
		cat(paste("61. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: processing data.\n",sep=""))
		cat(paste("61. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: processing data.\n",sep=""),file=FL.LOG,append=TRUE)	

		# initialize the "week.idx.new" field
		myData.Work.long[,"week.idx.new"] <- myData.Work.long[,"week.idx"] 

		# the row index of the lines which is EEM not baseline
		idx.EEM <- seq(1:dim(myData.Work.long)[1])[-(grep("_base",myData.Work.long[,"EEM"]))]

		# 62. keep the week idx of baseline as 1 to 52 and change the week idx of EEM to 53 to 104
		myData.Work.long[idx.EEM,"week.idx.new"] <- myData.Work.long[idx.EEM,"week.idx"] + 52
		cat(paste("62. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: the week idx is 1 to 52 for baseline and 53 to 104 for EEM.\n",sep=""))
		cat(paste("62. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: the week idx is 1 to 52 for baseline and 53 to 104 for EEM.\n",sep=""),file=FL.LOG,append=TRUE)	

		# turn the long format to wide format
		var.intact <- c("week.idx.new","hour.in.week")		# fields will not be changed
		var.expand <- c("EEM")					# fields will be used to expand
		var.measur <- c("EnergyGJ")				# fields of the measurement

		# -----------------------------------------------------------------
		# 63. [myData.Work.wide]
		# -----------------------------------------------------------------			
		myData.Work.wide <- dcast(myData.Work.long,week.idx.new + EEM ~ hour.in.week,value.var = var.measur)
		cat(paste("63. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: turn the long format to a wide format having 24 hour a day.\n",sep=""))
		cat(paste("63. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: turn the long format to a wide format having 24 hour a day.\n",sep=""),file=FL.LOG,append=TRUE)	


		# -----------------------------------------------------------------
		# 64. [myData.Work]
		# -----------------------------------------------------------------			
		myDate      <- myData.Work.wide[,c(1,2)]		# the first two fields are date and EEM label related
		myData.Work <- myData.Work.wide[,-c(1,2)]		# the following fields are hour in a week
		names(myData.Work) <- paste("hour",names(myData.Work),sep="")
		cat(paste("64. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: only keep the data for Data Mining.\n",sep=""))
		cat(paste("64. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: only keep the data for Data Mining.\n",sep=""),file=FL.LOG,append=TRUE)	


		####
		#### get min and max
		####
		#### min.arrays <- matrix(rep(apply(myData.Work,1,min),each=dim(myData.Work)[2]),nrow=dim(myData.Work)[1],byrow=TRUE)
		#### max.arrays <- matrix(rep(apply(myData.Work,1,max),each=dim(myData.Work)[2]),nrow=dim(myData.Work)[1],byrow=TRUE)
		####     names(min.arrays) <-     names(myData.Work)
		####     names(max.arrays) <-     names(myData.Work)		
		#### row.names(min.arrays) <- row.names(myData.Work)
		#### row.names(max.arrays) <- row.names(myData.Work)
		#### 
		####
		#### normalize
		####
		#### myData.Work.norm <- (myData.Work - min.arrays) / (max.arrays - min.arrays)
		
		
		myData.Work.norm <- myData.Work
		cat(paste("64D. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: does not normalize.\n",sep=""))
		cat(paste("64D. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: does not normalize.\n",sep=""),file=FL.LOG,append=TRUE)	



		#
		# 70. Hclust 
		#
		no.object   <- dim(myData.Work)[1]
		no.variable <- dim(myData.Work)[2]
		if (no.object >= no.variable)
		{		
		
			par(mfrow=c(1,2))				
			pamk.best <- pamk(myData.Work.norm)		# Partitioning around medoids with estimation of number of clusters
			no.cls.best1 <- pamk.best$nc
			cat("number of clusters estimated by optimum average silhouette width:",no.cls.best1, "\n")
			plot(pam(myData.Work, no.cls.best1),main = paste(this.subset,": fpc package",sep=""))

			asw <- numeric(no.cls)
			for (k in 2:no.cls)
			{
			    asw[[k]] <- pam(myData.Work, k) $ silinfo $ avg.width
			}
			no.cls.best2 <- which.max(asw)
			cat("silhouette-optimal number of clusters:", no.cls.best2, "\n")

			no.cls.best <- no.cls.best2



			myModel  <-  pam(myData.Work, no.cls.best,metric = "euclidean",medoids = NULL)
			cls.Work <-  (myModel)$clustering



			myResults.Work <- cbind(myDate,myData.Work.norm)
			cat(paste("36. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: combine clustering results with data.\n",sep=""))
			cat(paste("36. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: combine clustering results with data.\n",sep=""),file=FL.LOG,append=TRUE)	

			# assign cluster label
			myResults.Work[,"cls.Work"] <- myModel$clustering
			cat(paste("37. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: combine clustering results with data.\n",sep=""))
			cat(paste("37. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: combine clustering results with data.\n",sep=""),file=FL.LOG,append=TRUE)	

			#
			# no.cls.best <- max(myResults.Work[,"cls.Work"])
			thisLearn.string <- paste(this.method,": ",no.cls.best," clusters",sep="")
			cat(paste("71. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: add [cls.Work].\n",sep=""))
			cat(paste("71. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: add [cls.Work].\n",sep=""),file=FL.LOG,append=TRUE)	


			# assign class label back to [thisID.hourly.24.long]
			class.weeks <- myResults.Work[,c("week.idx.new","cls.Work","EEM")]
			cls.levels  <- paste("cls",sort(unique(class.weeks[,"cls.Work"])),sep="-")
			class.weeks[,"cluster"] <- factor(paste("cls",class.weeks[,"cls.Work"],sep="-"),levels = cls.levels,labels = cls.levels,ordered = TRUE)
			row.names(class.weeks) <- class.weeks[,"week.idx.new"]
			cat(paste("\n\nclustering results on [",this.CZ,"]-[",this.EEM.name,"]-[weekly],",sep=""),file=FL.OUT.CLSLAB,append=TRUE)
			write.table(class.weeks,file=FL.OUT.CLSLAB,sep=",",row.names=TRUE,col.names=TRUE,appen=TRUE)
			cat(paste("72. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: associate dates and class label.\n",sep=""))
			cat(paste("72. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: associate dates and class label.\n",sep=""),file=FL.LOG,append=TRUE)	


			# add a class label to [myData.Work.wide] and [myData.Work.long]
			myData.Work.long[,"cls.Work"] <- -999
			myData.Work.long[,"cls.Work"] <- class.weeks[as.character(myData.Work.long[,"week.idx.new"]),"cls.Work"]

			myData.Work.wide[,"cls.Work"] <- -999
			myData.Work.wide[,"cls.Work"] <- class.weeks[as.character(myData.Work.wide[,"week.idx.new"]),"cls.Work"]

			cls.levels <- paste("cls",sort(unique(myData.Work.long[,"cls.Work"])),sep="-")
			myData.Work.long[,"cluster"]  <- factor(paste("cls",myData.Work.long[,"cls.Work"],sep="-"),levels = cls.levels,labels = cls.levels,ordered = TRUE)
			myData.Work.wide[,"cluster"]  <- factor(paste("cls",myData.Work.wide[,"cls.Work"],sep="-"),levels = cls.levels,labels = cls.levels,ordered = TRUE)

			cat(paste("\n\nclustering results on [",this.CZ,"]-[",this.EEM.name,"]-[weekly],",sep=""),file=FL.OUT.CLSDAT,append=TRUE)
			write.table(myData.Work.long,file=FL.OUT.CLSDAT,sep=",",row.names=TRUE,col.names=TRUE,appen=TRUE)						

			cat(paste("73. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: add [cls.Work] to [myData.Work.wide] and [myData.Work.long].\n",sep=""))
			cat(paste("73. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: add [cls.Work] to [myData.Work.wide] and [myData.Work.long].\n",sep=""),file=FL.LOG,append=TRUE)	

			# -----------------------------------------------------------------
			# PLOTTING: profile of the classes
			# -----------------------------------------------------------------

			# ---------------------------------------------------------------------------------------
			# plot1: (p.cluster): plot the profile of the clusters
			# ---------------------------------------------------------------------------------------
			p.cluster <- qplot(data=myData.Work.long,x=hour.in.week,y=EnergyGJ,group=week.idx.new,color=EEM,facets=~cluster,geom="line")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [weekly] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
		      # p.cluster <- p.cluster + facet_grid(~cluster,scales="free_y")
			cat(paste("74. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: plot daily profile of all clusters.\n",sep=""))
			cat(paste("74. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: plot daily profile of all clusters.\n",sep=""),file=FL.LOG,append=TRUE)	


			# ---------------------------------------------------------------------------------------
			# plot 2: (p.cluster.dates): [Cluster] vs [Date] vs [base|EEM]
			# ---------------------------------------------------------------------------------------
			year.separator <- 52.5
			p.cluster.dates <- qplot(data=class.weeks,x=week.idx.new,y=cluster,color=cluster,alpha=I(0.5),shape=EEM,geom="point")  + theme(legend.position="top") + labs(title=paste("[",thisLearn.string,"] Clustering on subset [weekly] with ",no.object," objects\n[",this.CZ,"]-[",this.EEM.name,"]",sep=""))	
			p.cluster.dates <- p.cluster.dates + geom_vline(aes(xintercept = as.numeric(year.separator)),linetype=14,colour="black")
			cat(paste("75. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: plot object distribution in the clusters along the time axis.\n",sep=""))
			cat(paste("75. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: plot object distribution in the clusters along the time axis.\n",sep=""),file=FL.LOG,append=TRUE)

			# ---------------------------------------------------------------------------------------
			# actual plotting
			# ---------------------------------------------------------------------------------------
			# print(p.cluster,       position=c(0, 0.5, 1, 1.0), more=TRUE)	# only work for lattice plots not for ggplot2 plots		
			# print(p.cluster.dates, position=c(0, 0.0, 1, 0.5))		# only work for lattice plots not for ggplot2 plots
			multiplot(p.cluster,p.cluster.dates, cols=2)
			cat(paste("76. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: actually generate the plots.\n",sep=""))
			cat(paste("76. [",this.CZ,"]-[",this.EEM.name,"]-[weekly]: actually generate the plots.\n",sep=""),file=FL.LOG,append=TRUE)	

		}	
		dev.off()
	}			# end of current EEM
	cat(paste("80. [",this.CZ,"]: finished the processing of all EEMs.\n\n\n",sep=""))
	cat(paste("80. [",this.CZ,"]: finished the processing of all EEMs.\n\n\n",sep=""),file=FL.LOG,append=TRUE)	
}				# end of current CZ

cat(paste("\n\n90. Completed the processing the data of all CZs.\n",sep=""))
cat(paste("\n\n90. Completed the processing the data of all CZs.\n",sep=""),file=FL.LOG,append=TRUE)	


# -------------------------------------------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n03_DataMining_LargeOffice_PAM.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n03_DataMining_LargeOffice_PAM.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)


cat(paste("\nProcessing time for [03_DataMining_LargeOffice_Hclust.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [03_DataMining_LargeOffice_Hclust.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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
