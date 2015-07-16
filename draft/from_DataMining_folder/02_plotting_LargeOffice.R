#
# 02_plotting_LargeOffice.R 
# 
# Feb 3, 2015: re-generate this data for Data Mining purpose
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

library("ggplot2")
library("chron")
library("reshape2")
library("lattice")

# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number



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

Path.IN  <- paste(Path.Sim,"01_GatheringData_LargeOffice",sep="/")
Path.OUT <- paste(Path.Sim,"02_plotting_LargeOffice",     sep="/")
if (!file.exists(Path.IN)){print(paste(Path.IN," does not exist. Check why!",sep=""));die}
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}

FL.LOG <- paste(Path.OUT,"02_plotting_LargeOffice.log",sep="/")
if (file.exists(FL.LOG)){print(paste(FL.LOG," exist. Delete it!"));file.remove(FL.LOG)}		



# CZ arrays
CZ.arrays <- c("Miami","Houston","Phoenix","Atlanta","LosAngeles","LasVegas","SanFrancisco","Baltimore","Albuquerque","Seattle","Chicago","Denver","Minneapolis","Helena","Duluth","Fairbanks")



for (this.CZ in CZ.arrays)
{
	# different EEMs show maximum saving in different CZ
	# limit to the top two EEMs for each CZ but always keep base (0) and OptimalStart (33)
	if (this.CZ == "Miami")
	{
		EEMs.kept <- c(0,4,16,33,8,27)
	}else if (this.CZ == "Houston"){
		EEMs.kept <- c(0,4,16,33)
	}else if (this.CZ == "Phoenix"){
		EEMs.kept <- c(0,4,16,33,8,17)	
	}else if (this.CZ == "Atlanta"){
		EEMs.kept <- c(0,4,16,33)
	}else if (this.CZ == "LosAngeles"){
		EEMs.kept <- c(0,4,16,33)
	}else if (this.CZ == "LasVegas"){
		EEMs.kept <- c(0,4,16,33)		
	}else if (this.CZ == "SanFrancisco"){
		EEMs.kept <- c(0,4,16,33)	
	}else if (this.CZ == "Baltimore"){
		EEMs.kept <- c(0,4,16,33)	
	}else if (this.CZ == "Albuquerque"){
		EEMs.kept <- c(0,4,16,33,8)	
	}else if (this.CZ == "Seattle"){
		EEMs.kept <- c(0,4,16,33)	
	}else if (this.CZ == "Chicago"){
		EEMs.kept <- c(0,4,16,33)	
	}else if (this.CZ == "Denver"){
		EEMs.kept <- c(0,4,16,33)	
	}else if (this.CZ == "Minneapolis"){
		EEMs.kept <- c(0,4,16,33)	
	}else if (this.CZ == "Helena"){
		EEMs.kept <- c(0,4,16,33)	
	}else if (this.CZ == "Duluth"){
		EEMs.kept <- c(0,4,16,33)	
	}else if (this.CZ == "Fairbanks"){
		EEMs.kept <- c(0,4,16,33,44)	
	}
	
	
	
	
	Path.CZ.IN  <- paste(Path.IN, this.CZ,sep="/")
	Path.CZ.OUT <- paste(Path.OUT,this.CZ,sep="/")
	if (!file.exists(Path.CZ.IN)) {print(paste(Path.CZ.IN," does not exist. Check why!",sep=""));die}
	if (!file.exists(Path.CZ.OUT)){print(paste("NOT existing:",Path.CZ.OUT));dir.create(Path.CZ.OUT,showWarnings=TRUE,recursive=TRUE)}
	cat(paste("3. [",this.CZ,"]: Output folder has been set.\n",sep=""))
	cat(paste("3. [",this.CZ,"]: Output folder has been set.\n",sep=""),file=FL.LOG,append=TRUE)	



	# define output files
	FL.LargeOffice.LOG <- paste(Path.CZ.OUT,paste("02_plotting_LargeOffice.log",  sep=""),sep="/")
	FL.LargeOffice.CSV <- paste(Path.CZ.OUT,paste("02_plotting_LargeOffice.csv",  sep=""),sep="/")
	if (file.exists(FL.LargeOffice.LOG)){print(paste(FL.LargeOffice.LOG," exist. Delete it!"));file.remove(FL.LargeOffice.LOG)}		
	if (file.exists(FL.LargeOffice.CSV)){print(paste(FL.LargeOffice.CSV," exist. Delete it!"));file.remove(FL.LargeOffice.CSV)}		



	# define input data file from simulation
	FL.LargeOffice_IN.OBJ  <- paste(Path.CZ.IN, paste("GatheringData_LargeOffice.Rdata",sep=""),sep="/")
	FL.LargeOffice_CZ.OBJ  <- paste(Path.CZ.OUT,paste("GatheringData_LargeOffice.Rdata",sep=""),sep="/")
	if(!file.exists(FL.LargeOffice_IN.OBJ)){print(paste(FL.LargeOffice_IN.OBJ," does not exist. Check why!",sep=""));die}		
	if (file.exists(FL.LargeOffice_CZ.OBJ)){print(paste(FL.LargeOffice_CZ.OBJ," exist. Delete it!"));file.remove(FL.LargeOffice_CZ.OBJ)}	
	cat(paste("1. [",this.CZ,"]: simulated data.\n",sep=""))
	cat(paste("1. [",this.CZ,"]: simulated data.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	

	# -----------------------------------------------------------------------------------------
	# load the simulated data of all EEM for Large Office: [myData.wide],[myData.Elec.wide],[myData.gas.wide],[myData.long],[myData.Elec.long],[myData.Gas.long]
	# -----------------------------------------------------------------------------------------
	load(FL.LargeOffice_IN.OBJ)
	cat(paste("2A. [",this.CZ,"]: simulated data of Large Office has been loaded.\n",sep=""))
	cat(paste("2A. [",this.CZ,"]: simulated data of Large Office has been loaded.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	


	# check the energy and energy saving of the EEMs (E+ output is in Joule and the data passing to this script is in GJ)
	var.elec    <- grep("Elec",names(myData.Elec.wide),value=TRUE)
	var.gas     <- grep("Gas",names(myData.Gas.wide),value=TRUE)
	sum.elec    <- apply(myData.Elec.wide[,var.elec],2,sum)
	sum.gas     <- apply(myData.Gas.wide[,var.gas],2,sum)
	names(sum.elec) <- sub("Elec_","",names(sum.elec))
	names(sum.gas)  <- sub("Gas_", "",names(sum.gas))

	df.sum.energy <- data.frame(cbind(sum.elec,sum.gas))
	df.sum.energy[,"total"]   <- apply(df.sum.energy,1,sum)

	# these are in GJ units
	names(df.sum.energy) <- paste(names(df.sum.energy),"(GJ)",sep="")


	df.sum.energy[,"elec.delta(GJ)"]    <- df.sum.energy["00_base","sum.elec(GJ)"] - df.sum.energy[,"sum.elec(GJ)"]
	df.sum.energy[,"gas.delta(GJ)"]     <- df.sum.energy["00_base","sum.gas(GJ)"]  - df.sum.energy[,"sum.gas(GJ)"]
	df.sum.energy[,"total.delta(GJ)"]   <- df.sum.energy["00_base","total(GJ)"]    - df.sum.energy[,"total(GJ)"]
	df.sum.energy[,"elec.save(%)"]  <- 100 * (df.sum.energy[,"elec.delta(GJ)"]  / df.sum.energy["00_base","sum.elec(GJ)"])
	df.sum.energy[,"gas.save(%)"]   <- 100 * (df.sum.energy[,"gas.delta(GJ)"]   / df.sum.energy["00_base","sum.gas(GJ)"])
	df.sum.energy[,"total.save(%)"] <- 100 * (df.sum.energy[,"total.delta(GJ)"] / df.sum.energy["00_base","total(GJ)"])


	# convert elec to kWh and gas to kbtu/h
	kbtu_per_GJ = 947.81712;	# the factor used in the "proc_table_csv.pl" script
	kwh_per_GJ = 277.77778;		# the factor used in the "proc_table_csv.pl" script
	df.sum.energy[,"sum.elec(kwh)"] <-  df.sum.energy[,"sum.elec(GJ)"] * kwh_per_GJ
	df.sum.energy[,"sum.gas(kbtu)"] <-  df.sum.energy[,"sum.gas(GJ)"]  * kbtu_per_GJ 
	cat(paste("energy and energy saving of the EEMs,",sep=""),file=FL.LargeOffice.CSV,append=TRUE)
	write.table(df.sum.energy,file=FL.LargeOffice.CSV,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
	cat(paste("2B. [",this.CZ,"]: the energy consumption and energy saving of each EEMs are calculated.\n",sep=""))
	cat(paste("2B. [",this.CZ,"]: the energy consumption and energy saving of each EEMs are calculated.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	

	# -----------------------------------------------------------------------------------------
	# loopping through fuel types
	# Note: [myData.Elec.long] and [myData.Gas.long] are used.
	# -----------------------------------------------------------------------------------------
	for (this.fuel in c("elec","gas"))	# 
	{
		if (this.fuel == "elec")
		{
			myData.Work <- myData.Elec.long[(myData.Elec.long[,"EEM.idx"] %in% EEMs.kept),]
			this.fuel.string <- "Electricity"
		}else if (this.fuel == "gas")
		{
			myData.Work <- myData.Gas.long[(myData.Gas.long[,"EEM.idx"] %in% EEMs.kept),]
			this.fuel.string <- "NaturalGas"
		}

		# T.dryBuld and T.WetBuld could be factors (do not know why)
		if(is.factor(myData.Work[,"T.dryBulb"])){myData.Work[,"T.dryBulb"] <- as.numeric(as.character(myData.Work[,"T.dryBulb"]))}
		if(is.factor(myData.Work[,"T.WetBulb"])){myData.Work[,"T.WetBulb"] <- as.numeric(as.character(myData.Work[,"T.WetBulb"]))}



		# add a date.time field to [myData.Work]
		myData.Work[,"Date.Time"] <- as.POSIXct(strptime(paste(paste("2010",myData.Work[,"month"],myData.Work[,"day"],sep="-"),myData.Work[,"time"],sep=" "),"%Y-%m-%d %H:%M:%S"))
		cat(paste("3. [",this.CZ,"]-[",this.fuel,"]: only working on [myData.Work] and a [Date.Time] filed is added.\n",sep=""))
		cat(paste("3. [",this.CZ,"]-[",this.fuel,"]: only working on [myData.Work] and a [Date.Time] filed is added.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	


		# change the filed name from "value" to "EnergyGJ" and from "variable" to "EEMs"
		names(myData.Work) <- sub("variable","EEM",sub("value","EnergyGJ",names(myData.Work)))
		cat(paste("4. [",this.CZ,"]-[",this.fuel,"]: rename [value] to [EnergyGJ] and from [variable] to [EEM].\n",sep=""))
		cat(paste("4. [",this.CZ,"]-[",this.fuel,"]: rename [value] to [EnergyGJ] and from [variable] to [EEM].\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	

		# assign the first week idx to each month
		week.idx.1st <- tapply(myData.Work[,"week.idx"],myData.Work[,"month"],min)
		myData.Work[,"week.idx.1st.in.month"] <- week.idx.1st[myData.Work[,"month"]]

		# sort the weeklyg data and add "wwek.idx.in.month" and "day.idx.in.month" fields
		myData.Work <- myData.Work[order(myData.Work[,"EEM.name"],myData.Work[,"week.idx"],myData.Work[,"hour.in.week"]),]
		myData.Work[,"week.idx.in.month"] <- myData.Work[,"week.idx"] - myData.Work[,"week.idx.1st.in.month"] + 1			# take a while to figure out this
		myData.Work[,"day.idx.in.month"]  <-         myData.Work[,"day"] 
		cat(paste("5. [",this.CZ,"]-[",this.fuel,"]: add [week.idx.in.month] and [day.idx.in.month] fields.\n",sep=""))
		cat(paste("5. [",this.CZ,"]-[",this.fuel,"]: add [week.idx.in.month] and [day.idx.in.month] fields.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	

		if (this.fuel == "elec")
		{
			this.base.name <- "Elec_00_base"
		}else if (this.fuel == "gas")
		{
			this.base.name <- "Gas_00_base"
		}
		EEM.arrays <- as.character(unique(myData.Work[,"EEM"])[!(unique(myData.Work[,"EEM"]) %in% this.base.name)])
		cat(paste("6. [",this.CZ,"]-[",this.fuel,"]: put all EEMs of Large Office in [EEM.arrays].\n",sep=""))
		cat(paste("6. [",this.CZ,"]-[",this.fuel,"]: put all EEMs of Large Office in [EEM.arrays].\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	

		# loopping through EEMs
		for (this.idx in seq(from=1,to=length(EEM.arrays),by=1))
		# for (this.idx in c(55))
		{


			# this EEMs
			this.EEM.name <- EEM.arrays[this.idx]
			this.EEM.fuel <- sub("(.*)_(.*)_(.*)","\\1",this.EEM.name)
			this.EEM.num  <- sub("(.*)_(.*)_(.*)","\\2",this.EEM.name)
			this.EEM.lab  <- sub("(.*)_(.*)_(.*)","\\3",this.EEM.name)

			# the saving % of this EEM
			this.EEM.elec.save  <- round(df.sum.energy[paste(this.EEM.num,this.EEM.lab,sep="_"),"elec.save(%)"], digits=1)
			this.EEM.gas.save   <- round(df.sum.energy[paste(this.EEM.num,this.EEM.lab,sep="_"),"gas.save(%)"],  digits=1)
			this.EEM.total.save <- round(df.sum.energy[paste(this.EEM.num,this.EEM.lab,sep="_"),"total.save(%)"],digits=1)
			if (this.fuel == "elec")
			{
				this.saving.4.plot <- paste(this.fuel,":",this.EEM.elec.save,"% & total:",this.EEM.total.save,"%",sep="")	# saving in plot title consisting both "total" and "fuel" savings
				this.saving.4.file <- this.EEM.elec.save  									# saving in file name containing ONLY the saving of particular fuel
			}else if (this.fuel=="gas")
			{
				this.saving.4.plot <- paste(this.fuel,":",this.EEM.gas.save,"% & total:",this.EEM.total.save,"%",sep="")	# saving in plot title consisting both "total" and "fuel" savings
				this.saving.4.file <- this.EEM.gas.save 									# saving in file name containing ONLY the saving of particular fuel
			}
			cat(paste("\n\n7. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: processing......................\n",sep=""))
			cat(paste("\n\n7. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: processing......................\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	


			# generate files for each EEMs
			FL.LargeOffice.OBJ <- paste(Path.CZ.OUT,paste(this.EEM.name,paste("_saving",this.saving.4.file,sep=""),"_Plotting_LargeOffice.Rdata",sep=""),sep="/")
			FL.LargeOffice.PDF <- paste(Path.CZ.OUT,paste(this.EEM.name,paste("_saving",this.saving.4.file,sep=""),"_Plotting_LargeOffice.pdf",  sep=""),sep="/")

			if (file.exists(FL.LargeOffice.OBJ)){print(paste(FL.LargeOffice.OBJ," exist. Delete it!"));file.remove(FL.LargeOffice.OBJ)}		
			if (file.exists(FL.LargeOffice.PDF)){print(paste(FL.LargeOffice.PDF," exist. Delete it!"));file.remove(FL.LargeOffice.PDF)}		
			cat(paste("8. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: prepare files for output.\n",sep=""))
			cat(paste("8. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: prepare files for output.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	


			pdf(file = FL.LargeOffice.PDF,paper="special", width=17, height=11,bg = "transparent")
			cat(paste("9. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""))
			cat(paste("9. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	


			myData.base <- subset(myData.Work,EEM == this.base.name)
			myData.advn <- subset(myData.Work,EEM == this.EEM.name)
			cat(paste("10. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: get [myData.base] and [myData.advn].\n",sep=""))
			cat(paste("10. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: get [myData.base] and [myData.advn].\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	

			# ----------------------------------------------------------------------------------
			# merged in long format
			# ----------------------------------------------------------------------------------
			myData.merged.long <- rbind(myData.base,myData.advn)
			myData.merged.long[,"date.idx"] <- as.Date(myData.merged.long[,"date"],"%m/%d/%y")
			cat(paste("11. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: merge [myData.base] and [myData.advn] to [myData.merged.long].\n",sep=""))
			cat(paste("11. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: merge [myData.base] and [myData.advn] to [myData.merged.long].\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	




			# sort accoring to hour in the week and then week
			myData.4.weeklyLong <- myData.merged.long[order(myData.merged.long[,"week.idx"],myData.merged.long[,"hour.in.week"]),]
			myData.4.dailyLong  <- myData.merged.long[order(myData.merged.long[,"date.idx"],myData.merged.long[,"hour"]),]
			cat(paste("12. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: prepare [myData.4.weeklyLong] and [myData.4.dailyLong] for plotting.\n",sep=""))
			cat(paste("12. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: prepare [myData.4.weeklyLong] and [myData.4.dailyLong] for plotting.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	

			# add "month.lab"
			myData.4.dailyLong[,"month.lab"]  <- myData.4.dailyLong[,"month"]
			myData.4.dailyLong[,"month.lab"]  <- factor(myData.4.dailyLong[,"month.lab"],levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)
			myData.4.weeklyLong[,"month.lab"] <- myData.4.weeklyLong[,"month"]
			myData.4.weeklyLong[,"month.lab"] <- factor(myData.4.weeklyLong[,"month.lab"],levels = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)
			cat(paste("12A. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: added [month.lab].\n",sep=""))
			cat(paste("12A. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: added [month.lab].\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	


			
			# ----------------------------------------------------------------------------------------
			# PLOTTING...................
			# ----------------------------------------------------------------------------------------
			# 
			# for (this.month in seq(1,12))
			# {
			# 	myData.this.month <- subset(myData.4.weeklyLong,month==this.month)
			# 	pp.weekly <- ggplot(data=myData.this.month,aes(x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM)) + geom_line() + facet_wrap(month~week.idx,ncol=6)
			# 	
			# 	p.weekly  <- qplot(data=myData.this.month,x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM,facets=~week.idx,geom="line",ncol=5)
			# 	pp.weekly <- ggplot(data=yData.this.month,aes(x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM)) + geom_line() + facet_wrap(month~week.idx,ncol=5)
			# 	
			# 	plot(pp.weekly)
			# }
			# 
			# 
			# --------------------------------------------------------------
			# 0. a yearly time series with T.dryBulb
			# --------------------------------------------------------------			
			scaling.factor <- (max(myData.4.weeklyLong[,c("EnergyGJ")]) / max(myData.4.weeklyLong[,c("T.dryBulb")])) * 2
			cat(paste("12B. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""))
			cat(paste("12B. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: open pdf file for plotting.\n",sep=""),file=FL.LOG,append=TRUE)	
			
			myTmp1 <- myData.4.weeklyLong[,c("Date.Time","T.dryBulb","EEM")]			
			myTmp1[,"T.dryBulb"] <- myTmp1[,"T.dryBulb"] * scaling.factor
			myTmp1[,"ID"] <- "T.dryBulb (scaled)"
 			names(myTmp1) <- sub("T.dryBulb","value",names(myTmp1))

			
			myTmp2 <- myData.4.weeklyLong[,c("Date.Time","EnergyGJ","EEM")]
			myTmp2[,"ID"] <- "EnergyGJ"
			names(myTmp2) <- sub("EnergyGJ","value",names(myTmp2))
			
			myTmp3 <- rbind(myTmp1,myTmp2)
			myTmp3[,"Variable"] <- paste(myTmp3[,"EEM"],myTmp3[,"ID"],sep="_")

			myTmp4 <- myTmp3[!(seq(1:dim(myTmp3)[1]) %in% (grep("_base_T\\.dryBulb (scaled)",myTmp3[,"Variable"]))),]


			p.yearly <- qplot(data=myTmp4,x=Date.Time,y=value,group=Variable,color=Variable,geom="line") + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,")  Yearly Profile with T.dryBulb (",this.CZ,")",sep=""))
			plot(p.yearly)
			cat(paste("12C. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot yearly time series.\n",sep=""))
			cat(paste("12C. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot yearly time series.\n",sep=""),file=FL.LOG,append=TRUE)	
			
			


			
			# --------------------------------------------------------------
			# 1A. plot1?. plot weekly plot in each month
			# --------------------------------------------------------------
			p.weekly1 <- qplot(data=myData.4.weeklyLong,x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM,facets=month.lab~week.idx.in.month,geom="line") + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,")  Weekly Profile of Weeks in Each Month (",this.CZ,")",sep=""))	
			plot(p.weekly1)
			cat(paste("12D. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot weekly profile for months.\n",sep=""))
			cat(paste("12D. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot weekly profile for months.\n",sep=""),file=FL.LOG,append=TRUE)	

			###### # plot2?  the ggplot version
			###### pp.weekly1 <- ggplot(data=myData.4.weeklyLong,aes(x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM)) + geom_line() + facet_wrap(month~week.idx.in.month) + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,")",sep=""))	
			###### cplot(pp.weekly1)

			# --------------------------------------------------------------
			# 1B. plot3: OK. plot weekly plot in conseccutive weeks
			# --------------------------------------------------------------
			p.weekly2 <- qplot(data=myData.4.weeklyLong,x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM,facets=~week.idx,geom="line") + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,") Weekly Profile of All Weeks in the Year (",this.CZ,")",sep=""))		
			plot(p.weekly2)
			cat(paste("12E. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot weekly profile in conseccutive weeks.\n",sep=""))
			cat(paste("12E. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot weekly profile in conseccutive weeks.\n",sep=""),file=FL.LOG,append=TRUE)	

			###### plot4 OK
			###### pp.weekly3 <- ggplot(data=myData.4.weeklyLong,aes(x=hour.in.week,y=EnergyGJ,group=EEM,color=EEM)) + geom_line() + facet_wrap(~week.idx) + theme(legend.title=element_text(colour="black",size=12,face="bold"),legend.position="top",title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,")",sep=""))
			###### plot(pp.weekly3)

			# --------------------------------------------------------------
			# 1C. plot3: OK. plot weekly plot superimposed
			# --------------------------------------------------------------
			p.weekly3 <- qplot(data=myData.4.weeklyLong,x=hour.in.week,y=EnergyGJ,group=week.idx,color=EEM,geom="line") + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,") Overlaid Weekly Profile of All Weeks in the Year (",this.CZ,")",sep=""))			
			plot(p.weekly3)
			cat(paste("12E. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot weekly profile superimposed.\n",sep=""))
			cat(paste("12E. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot weekly profile superimposeds.\n",sep=""),file=FL.LOG,append=TRUE)	


			# --------------------------------------------------------------
			# 1D. plot3: OK. plot weekly plot in superimposed but separated between "baseline" and "EEM"
			# --------------------------------------------------------------
			p.weekly4 <- qplot(data=myData.4.weeklyLong,x=hour.in.week,y=EnergyGJ,group=week.idx,color=EEM,geom="line",facets=~EEM) + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,") Overlaid Weekly Profile of All Weeks in the Year (",this.CZ,")",sep=""))
			plot(p.weekly4)
			cat(paste("12E. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot weekly profile superimposed but separated between [baseline] and [EEM].\n",sep=""))
			cat(paste("12E. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot weekly profile superimposed but separated between [baseline] and [EEM].\n",sep=""),file=FL.LOG,append=TRUE)	

			# --------------------------------------------------------------
			# 2A. plot5. OK. plot daily plot in each month
			# --------------------------------------------------------------
			p.daily1 <- qplot(data=myData.4.dailyLong,x=hour,y=EnergyGJ,group=EEM,color=EEM,facets=month.lab~day,geom="line")  + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,") Daily Profile of All Days in Each Month (",this.CZ,")",sep=""))	
			plot(p.daily1)
			cat(paste("12E. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot daily profile for each month.\n",sep=""))
			cat(paste("12E. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot daily profile for each month.\n",sep=""),file=FL.LOG,append=TRUE)	

			###### # plot6??. the ggplot version
			###### pp.daily1 <- ggplot(data=myData.4.dailyLong,aes(x=hour,y=EnergyGJ,group=EEM,color=EEM)) + geom_line() + facet_wrap(month~day) + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,")",sep=""))	
			###### plot(pp.daily1)
			###### cat(paste("13. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: made some ggplots\n",sep=""))
			###### cat(paste("13. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: made some ggplots.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	



			##### --------------------------------------------------------------
			##### 3A. plot5. OK. plot daily plot in each type of days (weekday, saturday and sunday)
			##### --------------------------------------------------------------
			##### p.daily.day.type <- qplot(data=myData.4.dailyLong,x=hour,y=EnergyGJ,group=day.in.year,color=EEM,facets=~day.type.lab,geom="line")  + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,") Overlaid Daily Profiles of Day Types (",this.CZ,")",sep=""))	
			##### plot(p.daily.day.type)


			# --------------------------------------------------------------
			# 3B. plot5. OK. plot daily plot in each type of days (weekday, saturday and sunday)
			# --------------------------------------------------------------
			p.daily.day.type <- qplot(data=myData.4.dailyLong,x=hour,y=EnergyGJ,group=day.in.year,color=EEM,facets=EEM~day.type.lab,geom="line")  + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,") Overlaid Daily Profiles of Day Types (",this.CZ,")",sep=""))
			plot(p.daily.day.type)
			cat(paste("12F. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot daily profile superimposed for each day type.\n",sep=""))
			cat(paste("12F. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot daily profile superimposed for each day type.\n",sep=""),file=FL.LOG,append=TRUE)	
			

			##### --------------------------------------------------------------
			##### 4A. plot5. OK. plot daily plot in each type of days (Monday, .., Sunday)
			##### --------------------------------------------------------------
			##### p.daily.day.lab <- qplot(data=myData.4.dailyLong,x=hour,y=EnergyGJ,group=day.in.year,color=EEM,facets=~day.week.lab,geom="line")  + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,") Overlaid Daily Profiles of Each Day of the Week (",this.CZ,")",sep=""))	
			##### plot(p.daily.day.lab)

			# --------------------------------------------------------------
			# 4B. plot5. OK. plot daily plot in each type of days (Monday, .., Sunday)
			# --------------------------------------------------------------
			p.daily.day.lab <- qplot(data=myData.4.dailyLong,x=hour,y=EnergyGJ,group=day.in.year,color=EEM,facets=EEM~day.week.lab,geom="line")  + theme(legend.position="top") + labs(title=paste(paste(this.EEM.fuel,this.EEM.num,this.EEM.lab,sep="_")," saving(",this.saving.4.plot,") Overlaid Daily Profiles of Each Day of the Week (",this.CZ,")",sep=""))
			plot(p.daily.day.lab)
			cat(paste("12G. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot daily profile superimposed for each day of the week.\n",sep=""))
			cat(paste("12G. [",this.CZ,"]-[",this.fuel,"]-[",this.EEM.name,"]: plot daily profile superimposed for each day of the week.\n",sep=""),file=FL.LOG,append=TRUE)	

			###### # field common in both "base" and "advn"
			###### var.common <- names(myData.base)[!(names(myData.base) %in% c("EEM","EnergyGJ","fuel","EEM.idx","EEM.name"))]
			###### 
			###### # rename the "EEM" filde with the name of the EEMs
			###### names(myData.base) <- sub("EnergyGJ",this.base.name,names(myData.base))
			###### names(myData.advn) <- sub("EnergyGJ",this.EEM.name, names(myData.advn))
			###### 
			###### 
			###### # ----------------------------------------------------------------------------------
			###### # merge base and advn in wide format
			###### # ----------------------------------------------------------------------------------
			###### myData.merged.wide <- merge(myData.base[,c(var.common,this.base.name)],myData.advn[,c(var.common,this.EEM.name)],by=var.common)
			###### 
			###### # add "date.idx" for sorting according to dates
			###### myData.merged.wide[,"date.idx"] <- as.Date(myData.merged.wide[,"date"],"%m/%d/%y")
			###### 
			###### # sort accoring to hour in the week and then week
			###### myData.4.weeklyWide <- myData.merged.wide[order(myData.merged.wide[,"week.idx"],myData.merged.wide[,"hour.in.week"]),]
			###### myData.4.dailyWide  <- myData.merged.wide[order(myData.merged.wide[,"date.idx"],myData.merged.wide[,"hour"]),]

			###### # ----------------------------------------------------------------------------------
			###### # plot daily
			###### # ----------------------------------------------------------------------------------
			###### plot.daily <- xyplot(EnergyGJ ~ hour | day.idx.in.month + month, data = myData.4.dailyLong, group=EEM,
			###### 		     main = paste("daily plot"),
			###### 		     xlab = "hour in the day",
			###### 		     ylab = "Electricity Consumption in GJ",
			###### 		     col = c("red","blue"),
			###### 		     type= c("l","l"),
			###### 		     layout = c(6,5),
			###### 		     as.table = TRUE,
			###### 		     lty = c(1,1))
			###### plot(plot.daily)	                     
			###### 
			###### 
			###### 
			###### # plot weekly
			###### 
			###### 
			###### plot.weekly <- xyplot(EnergyGJ ~ hour.in.week | week.idx.in.month + month, data = myData.4.weeklyLong, group=EEM,
			###### 		     main = paste("weekly plot"),
			###### 		     xlab = "hour in the week",
			###### 		     ylab = "Electricity Consumption in GJ",
			###### 		     col = c("red","blue"),
			###### 		     type= c("l","l"),
			###### 		     layout = c(2,3),
			###### 		     as.table = TRUE,
			###### 		     lty = c(1,1))
			###### plot(plot.weekly)	                     
			###### cat(paste("14. [",this.fuel,"]-[",this.EEM.name,"]: made more lattice plots\n",sep=""))
			###### cat(paste("14. [",this.fuel,"]-[",this.EEM.name,"]: made more lattice plots.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	
			###### 
			dev.off()
			
			#
			# output
			#
			save(myData.base,myData.advn,myData.merged.long,myData.4.weeklyLong,myData.4.dailyLong,file=FL.LargeOffice.OBJ)
			cat(paste("15. [",this.CZ,"]: save into [",FL.LargeOffice.OBJ,"].\n",sep=""))
			cat(paste("15. [",this.CZ,"]: save into [",FL.LargeOffice.OBJ,"].\n",sep=""),file=FL.LOG,append=TRUE)
			
		}
		cat(paste("16. [",this.CZ,"]-[",this.fuel,"]: Completed the processing\n",sep=""))
		cat(paste("16. [",this.CZ,"]-[",this.fuel,"]: Completed the processing.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	
	}

	cat(paste("\n\n17. [",this.CZ,"]: Completed the processing\n",sep=""))
	cat(paste("\n\n17. [",this.CZ,"]: Completed the processing.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	

	#
	# output
	#
	save(myData.wide,myData.Elec.wide,myData.Gas.wide,
	     myData.long,myData.Elec.long,myData.Gas.long,file=FL.LargeOffice_CZ.OBJ)
	cat(paste("18. [",this.CZ,"]: save into [",FL.LargeOffice_CZ.OBJ,"].\n",sep=""))
	cat(paste("18. [",this.CZ,"]: save into [",FL.LargeOffice_CZ.OBJ,"].\n",sep=""),file=FL.LOG,append=TRUE)



}
cat(paste("\n\n19. Completed the processing the data of all CZs.\n",sep=""))
cat(paste("\n\n19. Completed the processing the data of all CZs.\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)	


# -------------------------------------------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n02_plotting_LargeOffice.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n02_plotting_LargeOffice.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LargeOffice.LOG,append=TRUE)


cat(paste("\nProcessing time for [02_plotting_LargeOffice.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [02_plotting_LargeOffice.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LargeOffice.LOG,append=TRUE)

#
# put run related information into the log file
#
cat(paste("This run was conducted in ",.Platform$OS.type,"\n",sep=""));
cat(paste("This run was conducted in ",.Platform$OS.type,"\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE);




# get the version of R used for this computation and the latest version released
current.Rversion <- R.Version()$version.string
tmp = readLines("http://cran.r-project.org/sources.html")
rls = tmp[grep("latest release", tmp) + 1L]			# the version number is in the next line of 'The latest release'
latest.Rversion  <- gsub("(.*R-|\\.tar\\.gz.*)", "", rls)	# "The latest release: R-2.13.0.tar.gz"
if (latest.Rversion != current.Rversion)
{
	cat(paste("\n\nyou may want to upgrade R from the version you are using [",current.Rversion,"] to the latest version of [",latest.Rversion,"]\n",sep=""));
	cat(paste("\n\nyou may want to upgrade R from the version you are using [",current.Rversion,"] to the latest version of [",latest.Rversion,"]\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE);
}else{
	cat(paste("\n\nThe R version you are using is the latest version released so far!\n",sep=""))
	cat(paste("\n\nThe R version you are using is the latest version released so far!\n",sep=""),file=FL.LargeOffice.LOG,append=TRUE)
}



# get the version information of the attached libraries
cat(paste("\n\nThe information of the packages you used for this calculation:\n"))
cat(paste("\n\nThe information of the packages you used for this calculation:\n"),file=FL.LargeOffice.LOG,append=TRUE)
tmp <- sessionInfo()
pkg.loaded <- tmp$otherPkgs
no.pkg.loaded <- length(pkg.loaded)
for (i in seq(1,no.pkg.loaded))
{
	cat(paste(pkg.loaded[[i]]$Package,":",pkg.loaded[[i]]$Version," ",pkg.loaded[[i]]$Date,"\n",sep=" "))
	cat(paste(pkg.loaded[[i]]$Package,":",pkg.loaded[[i]]$Version," ",pkg.loaded[[i]]$Date,"\n",sep=" "),file=FL.LargeOffice.LOG,append=TRUE)
}
