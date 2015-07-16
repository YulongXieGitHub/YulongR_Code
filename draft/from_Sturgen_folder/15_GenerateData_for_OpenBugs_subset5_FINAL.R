#
# 15_GenerateData_for_OpenBugs_subset5_Final.R
#
# This script is used to generate model and data for openBugs for a given variable set.
#
#
# -------------------------------------------------------------------------------------------------
# June 8, 2013: This is to check the subset data created by "12_Prep_Subset_Data.R" mainly on checking the re-grouping of lithology and particle size categorical variables.
#
# June 7, 2013: Chris M provided the re-group rules for the lithology and part size categorical variables for each subset of the data.
#
# June 4, 2013
# Variables to be retained for the analysis vary in terms of subsets.
# We are going to do the following five subsets
# 1. Lower - Gillnet	  (GN14S, GN41S, GN18S, GN81S)  
# 2. Lower - Trotline	  (TLC1S, TLC2S)
# 3. Lower - active gears (OT16S and TNS and TN25S)
# 4. Upper - active gears (OT16S and TNS and TN25S)
# 5. Upper - Trotline	  (TLC1S, TLC2S)
# -------------------------------------------------------------------------------------------------
# Chris Vernon prepared a revised data sheet on May 31,   2013 after having deleted some segment 7 data.
# Chris Vernon prepared a revised data sheet on May 27,   2013 with (1) relative discharge    and (2) new field names
#
# Data difference between May 27, 2013 and May 14, 2013

# The data has been pre-processed in excel in the following way before saved as the csv format to be used by this script.
# (1) replace all 1,380,446 NULL  with empty field
# (2) replace all       142 NaN   with empty field
# (3) replace all         0 -9999 with empty field
# (4) replace 8     "#DIV/0!"     with empty field in the CPUE field
# (5) replace 8 "DIV/0!" for "CPUE" and 1 "DIV/0!" for "CPUA" and the same number for "CPUE2" and "CPUA2".
#
# eliminate all stuff
rm(list = ls(all = TRUE))

options(scipen=999)
# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}


# today's month, day and year in the format of "Thu Jun 16 08:48:36 2011", 5 fields separated by space
today.month  <- strsplit(date(),"\\s+",perl=TRUE)[[1]][2]
today.day    <- strsplit(date(),"\\s+",perl=TRUE)[[1]][3]
today.year   <- strsplit(date(),"\\s+",perl=TRUE)[[1]][5]
today.hour   <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][1]
today.minute <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][2]
today.second <- strsplit(strsplit(date(),"\\s+",perl=TRUE)[[1]][4],":",perl=TRUE)[[1]][3]

#
# The same subset of data can be treated with either pscl, OpenBUGS, WinBUGS or JAGS.  So need to set up a method string
#
method.string <- "OpenBUGS"

#
# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_scripts"
}else{
	Path.Current <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_scripts"
}
setwd(Path.Current)

FL.package  <- paste(Path.Current,"package_loading.R",sep="/")
source(FL.package)

# Data Folder
Path.IN  <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/12_Prep_Subset_Data"
Path.log <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.OUT <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/15_GenerateData_for_OpenBugs_Final"
if (!file.exists(Path.IN)) {stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}

# data file
FL.OBJ.IN <- paste(Path.IN, "12_Prep_Subset_Data.RData",       sep="/")
FL.LOG    <- paste(Path.log,"15_GenerateData_for_OpenBugs.log",sep="/")	
if (!file.exists(FL.OBJ.IN)){stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.LOG))   {print(paste(FL.LOG,      "exist.Delete it!")); file.remove(FL.LOG)}


# ******************************************************************
# Loop of Subsets
# ******************************************************************
subsets.section <- c("Upper","Upper","Lower","Lower","Lower","Upper",   "Lower",   "All")
subsets.gear    <- c("OT_TN","TLC",  "OT_TN","TLC",  "GN",   "Logistic","Logistic","All")
for (idx.subset in c(5))
{
	# -------------------------------------------------------------------------------------------------
	# 1. name subset and save them in the binary R data file
	# -------------------------------------------------------------------------------------------------
	subset.section <- subsets.section[idx.subset]
	subset.gear    <- subsets.gear[idx.subset]


	#
	# prepare INPUT DATA FILE
	#
	Path.IN.subset  <- paste(Path.IN, paste(paste("subset",idx.subset,sep=""),subset.section,subset.gear,sep="_"),sep="/")
	Path.OUT.subset <- paste(Path.OUT,paste(paste("subset",idx.subset,sep=""),subset.section,subset.gear,method.string,sep="_"),sep="/")
	if (!file.exists(Path.OUT.subset)){print(paste("NOT existing:",Path.OUT.subset));dir.create(Path.OUT.subset,showWarnings=TRUE,recursive=TRUE)}
	if (!file.exists(Path.IN.subset)) {print(paste("NOT existing:",Path.IN.subset," Check Why!\n",sep=""));die}
	
	#
	# load the data
	#
	load(FL.OBJ.IN)
	cat(paste("0. [",FL.OBJ.IN,"] has been loaded!\n",sep=""))
	cat(paste("0. [",FL.OBJ.IN,"] has been loaded!\n",sep=""),file=FL.LOG,append=TRUE)
	

	cat(paste("\n\n************************************ [",subset.gear,"] and [",subset.section,"] ","************************************","\n",sep=""))
	cat(paste("\n\n************************************ [",subset.gear,"] and [",subset.section,"] ","************************************","\n",sep=""),file=FL.LOG,append=TRUE)
	
	FL.OUT.Obj   <<- paste(Path.OUT.subset, paste("RData_Subset_",idx.subset,"_",subset.section,"_",subset.gear,"_",method.string,".RData",sep=""),sep="/")
	FL.OUT.Data  <<- paste(Path.OUT.subset, paste("Data_Subset_", idx.subset,"_",subset.section,"_",subset.gear,"_",method.string,".csv",  sep=""),sep="/")
	FL.OUT.Model <<- paste(Path.OUT.subset, paste("Model_Subset_",idx.subset,"_",subset.section,"_",subset.gear,"_",method.string,".csv",  sep=""),sep="/")
	if  (file.exists(FL.OUT.Obj))  {print(paste(FL.OUT.Obj,  "exist.Delete it!"));file.remove(FL.OUT.Obj)}
	if  (file.exists(FL.OUT.Data)) {print(paste(FL.OUT.Data, "exist.Delete it!"));file.remove(FL.OUT.Data)}
	if  (file.exists(FL.OUT.Model)){print(paste(FL.OUT.Model,"exist.Delete it!"));file.remove(FL.OUT.Model)}


	# subset 1: upper OT and TN (active gear) [myData.Upper_Active]
	if (idx.subset == 1)
	{		
		myData.Sub      <- myData.Upper_Active    
		col.title.4.sub <- col.title.Upper_Active  
		col.class.4.sub <- col.class.Upper_Active  	

		Y.var  <- "Pal_cnt"
		var.rm.fix <- c("Gear","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","MicroClass","WaterVel","Fish_cnt","CPUA","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUA.calc")
		var.rm     <- c("Gear","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","MicroClass","WaterVel","Fish_cnt","CPUA","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUA.calc")

		
		
	# subset 2: upper TLC [myData.Upper_TLC]
	}else if(idx.subset == 2){
		myData.Sub      <- myData.Upper_TLC    
		col.title.4.sub <- col.title.Upper_TLC  
		col.class.4.sub <- col.class.Upper_TLC 	
		
		Y.var  <- "Pal_cnt"
		var.rm.fix <- c("Gear","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","MicroClass","WaterVel","Fish_cnt","CPUE","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUE.calc")
		var.rm     <- c("Gear","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","MicroClass","WaterVel","Fish_cnt","CPUE","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUE.calc")
		
		
	# subset 3: lower OT & TN	[myData.Lower_Active]
	}else if(idx.subset == 3){
		myData.Sub      <- myData.Lower_Active    
		col.title.4.sub <- col.title.Lower_Active  
		col.class.4.sub <- col.class.Lower_Active 	
		
		Y.var  <- "Pal_cnt"
		var.rm.fix <- c("Gear","D_dist_dn","D_dist_near","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","WaterVel","Fish_cnt","CPUA","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUA.calc")
		var.rm     <- c("Gear","D_dist_dn","D_dist_near","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","WaterVel","Fish_cnt","CPUA","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUA.calc")
		
		
	# subset 4: lower TLC [myData.Lower_TLC]
	}else if(idx.subset == 4){
		myData.Sub      <- myData.Lower_TLC    
		col.title.4.sub <- col.title.Lower_TLC   
		col.class.4.sub <- col.class.Lower_TLC  	
		
		Y.var  <- "Pal_cnt"
		var.rm.fix <- c("Gear","D_dist_dn","D_dist_near","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","WaterVel","Fish_cnt","CPUE","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUE.calc","D_dist_up")	
		var.rm     <- c("Gear","D_dist_dn","D_dist_near","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","WaterVel","Fish_cnt","CPUE","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUE.calc","D_dist_up")
		
	# subset 5: lower GN [myData.Lower_GN]
	}else if(idx.subset == 5){
		myData.Sub      <- myData.Lower_GN    
		col.title.4.sub <- col.title.Lower_GN   
		col.class.4.sub <- col.class.Lower_GN 	
		
		Y.var  <- "Pal_cnt"
		var.rm.fix <- c("Gear","Season","D_dist_dn","D_dist_near","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","WaterVel","Fish_cnt","CPUE","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUE.calc")
		var.rm     <- c("Gear","Season","D_dist_dn","D_dist_near","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","WaterVel","Fish_cnt","CPUE","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUE.calc")
		
		
	# subset 6: Logistic Upper [myData.Upper_Logistic]
	}else if(idx.subset == 6){
		myData.Sub      <- myData.Upper_Logistic    
		col.title.4.sub <- col.title.Upper_Logistic   
		col.class.4.sub <- col.class.Upper_Logistic 
		
		Y.var  <- "binary"
		var.rm.fix <- c("Gear","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","MicroClass","WaterVel","Fish_cnt","CPUE","CPUA","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUA.calc","CPUE.calc")
		var.rm     <- c("Gear","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","MicroClass","WaterVel","Fish_cnt","CPUE","CPUA","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUA.calc","CPUE.calc")
		
		
	# subset 7: Logistic Lower [myData.Lower_Logistic]
	}else if(idx.subset == 7){
		myData.Sub      <- myData.Lower_Logistic    
		col.title.4.sub <- col.title.Lower_Logistic   
		col.class.4.sub <- col.class.Lower_Logistic 	
		
		Y.var  <- "binary"
		var.rm.fix <- c("Gear","D_dist_dn","D_dist_near","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","WaterVel","Fish_cnt","CPUE","CPUA","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUA.calc","CPUE.calc")
		var.rm     <- c("Gear","D_dist_dn","D_dist_near","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","WaterVel","Fish_cnt","CPUE","CPUA","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUA.calc","CPUE.calc")
		
		
	# subset 8: All
	}else if(idx.subset == 8){
		myData.Sub      <- myData.All    
		col.title.4.sub <- col.title.All   
		col.class.4.sub <- col.class.All 
		
		var.rm.fix <- c("Gear","D_dist_dn","D_dist_near","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","WaterVel","Fish_cnt","CPUE","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUE.calc")
		var.rm     <- c("Gear","D_dist_dn","D_dist_near","MA","Lith_1","Lith_2","Lith_desc","NFHAP","Tax_part_sz","Turb","Meso","WaterVel","Fish_cnt","CPUE","Stk_12_dist","Stk_24_dist","Stk_36_dist","Stk_60_dist","Stk_120_dist","gear.type1","gear.type2","CPUE.calc")
		
	}	
	
	
	# -----------------------------------------------------------------------------------------
	# July 10, 2013
	# remove NA rows before stepwise to ensure all variables starting with the same data
	# based on the initial assessment of the variables to be kept (not the actual variables we could be able to keep which depends on the computational singurity)
	# July 10, 2013
	# -----------------------------------------------------------------------------------------
	# naCol.2.rm: the number of missing of each variable
	# naRow.2.rm: the number of missing of each observation.  
	idxCol.2.subset <- match(var.rm.fix,names(myData.Sub))	
	naCol.2.rm <- apply(myData.Sub[,-idxCol.2.subset],2,function(x){sum(is.na(x))})
	naRow.2.rm <- apply(myData.Sub[,-idxCol.2.subset],1,function(x){sum(is.na(x))})
	if (sum(naCol.2.rm) != sum(naRow.2.rm)){cat("Something is not right!\n");die}
	idx.NA <- seq(1,dim(myData.Sub)[1])[naRow.2.rm>0]
	

	
	if(length(idx.NA)>0)
	{
		myData.Sub <- myData.Sub[-idx.NA,]
	}
	
	#
	# reduced data set for each subset of data
	idxCol.2.rm <- match(var.rm,names(myData.Sub))
	if(is.na(any(idxCol.2.rm))){cat(paste("Check the col names of [myDataSub] for Subset_",subset.section,"_",subset.gear,sep=""));die}
	myData.Reduced <- myData.Sub[,-idxCol.2.rm]

	
	idx.2.rm <- match(var.rm,col.title.4.sub)
	if(is.na(any(idx.2.rm))){cat(paste("Check the col names of [col.title.4.sub] for Subset_",subset.section,"_",subset.gear,sep=""));die}	
	col.title.reduced <- col.title.4.sub[-idx.2.rm]
	col.class.reduced <- col.class.4.sub[-idx.2.rm]
		
	#	
	X.var.cat    <- grep("binary",col.title.reduced[col.class.reduced == "factor"],value=TRUE,invert=TRUE)
	X.var.num    <- grep("Pal_cnt",grep("SUPPORT.calc",col.title.reduced[col.class.reduced != "factor"],value=TRUE,invert=TRUE),value=TRUE,invert=TRUE)	
	X.var.offset <- "SUPPORT.calc"
	
	#
	myData.X.num    <- myData.Reduced[,X.var.num,   drop=FALSE]
	myData.X.cat    <- myData.Reduced[,X.var.cat,   drop=FALSE] 
	myData.X.offset <- myData.Reduced[,X.var.offset,drop=FALSE]
	myData.Y        <- myData.Reduced[,Y.var,       drop=FALSE] 
	
	# make sure the categorical are factors
	for (i in names(myData.X.cat))
	{
		if (!(is.factor(myData.X.cat[,i])))
		{
			myData.X.cat[,i] <- as.factor(myData.X.cat[,i])
		}
	}

	# standardized the numerical variables
	var.mean <- as.data.frame(matrix(rep(apply(myData.X.num,2,mean,na.rm=TRUE),dim(myData.X.num)[1]),nrow=dim(myData.X.num)[1],byrow=TRUE))
	var.sd   <- as.data.frame(matrix(rep(apply(myData.X.num,2,sd,na.rm=TRUE),  dim(myData.X.num)[1]),nrow=dim(myData.X.num)[1],byrow=TRUE))
	myData.X.num.std <- (myData.X.num - var.mean)/var.sd
	cat(paste("The data has been prepared for this subset!\n",sep=""))
	cat(paste("The data has been prepared for this subset!\n",sep=""),file=FL.LOG,append=TRUE)
	

	
	if(idx.subset == 5)	# PSCL Lower TLC
	{
		# [Subset 4 Lower TLC]: define an empty array to hold model summary
		model.count <- 0
		OpenBugs.summary <- c()
		
		# compile a working data set: [myData.Work] consists of 4 pieces, i.e, [myData.Y],
		#                                                                      [myData.X.num.std],
		#                                                                      [myData.X.cat],		which need to be converted into N-1 dummy variables
		#                                                                      [myData.offset],		which will be under the array name [SUPPORT.calc] as well
		myData.Work <- cbind(myData.Y,myData.X.num.std,myData.X.cat,myData.X.offset)

		# remove rows with NA
		naRow.2.rm <- apply(myData.Work,1,function(x){sum(is.na(x))})
		idx.NA <- seq(1,dim(myData.Work)[1])[naRow.2.rm>0]
		if(length(idx.NA)>0)
		{
			myData.Work <- myData.Work[-idx.NA,]
		}

		idx.lines <- dim(myData.Work)[1] + 2
		SUPPORT.calc <- myData.Work[,"SUPPORT.calc"]
		
		cat(paste("2. The data has been prepared for this subset further!\n",sep=""))
		cat(paste("2. The data has been prepared for this subset further!\n",sep=""),file=FL.LOG,append=TRUE)
	
		
		# ---------------------------------------------------------------------------------
		# evaulate/manipulate on the [list.var] specified below
		# ---------------------------------------------------------------------------------
		for (model4test in seq(from=1,to=1,by=1))
		{
			if (model4test == 1)
			{
				list.var <- c("Wet_wid","new_meso","new_lith","Rel_dsch_WY","SY","Temp","new_NFHAP","Rel_dsch_AY","Depth_Mean")	
				B <- c(-5.599,-0.705,0.898,-0.827,-0.718,0.311,1.184,0.253,-0.322,-0.839,0.226,0.917,1.639,0.636,0.185) 
				A <- c(-12.455,-2.439,0.749,-1.461,-5.133,1.325,3.029,-0.550,-2.865,-10.045,-0.199,9.757,11.018,3.991,0.759) 
			}
			
			# will use the variable name in [list_var] as the folder name
			# folder.name <- paste(list.var,collapse="_")
			
			
			# define variable specific strings	
			string.var   <- paste("model4test_var",model4test,sep="_")
			Path.OUT.var <- paste(Path.OUT.subset,string.var,sep="/")
			if (!file.exists(Path.OUT.var)){print(paste("NOT existing:",Path.OUT.var));dir.create(Path.OUT.var,showWarnings=TRUE,recursive=TRUE)}

			
			FL.ODC     <- paste(Path.OUT.var,paste("model_for_openBUGS.odc",sep=""),sep="/")
			FL.MODEL   <- paste(Path.OUT.var,paste("model.txt",sep=""),sep="/")
			FL.DATA    <- paste(Path.OUT.var,paste("data.txt", sep=""),sep="/")
			FL.INIT    <- paste(Path.OUT.var,paste("init.txt", sep=""),sep="/")
			FL.PAR     <- paste(Path.OUT.var,paste("par.txt",  sep=""),sep="/")
			FL.MCMC    <- paste(Path.OUT.var,paste("MCMC.txt", sep=""),sep="/")
			if  (file.exists(FL.ODC))  {print(paste(FL.ODC,"exist.Delete it!"));  file.remove(FL.ODC)}
			if  (file.exists(FL.MODEL)){print(paste(FL.MODEL,"exist.Delete it!"));file.remove(FL.MODEL)}
			if  (file.exists(FL.INIT)) {print(paste(FL.INIT,"exist.Delete it!")); file.remove(FL.INIT)}
			if  (file.exists(FL.DATA)) {print(paste(FL.DATA,"exist.Delete it!")); file.remove(FL.DATA)}
			if  (file.exists(FL.PAR))  {print(paste(FL.PAR, "exist.Delete it!")); file.remove(FL.PAR)}
			if  (file.exists(FL.MCMC)) {print(paste(FL.MCMC,"exist.Delete it!")); file.remove(FL.MCMC)}
			cat(paste("3. local file names specified for model4test ",model4test,"!\n",sep=""))
			cat(paste("3. local file names specified for model4test ",model4test,"!\n",sep=""),file=FL.LOG,append=TRUE)


			# ------------------------------------------------------------------------------------------------
			# check if all the variables specified in [list.var] exist in [myData.Work]
			# ------------------------------------------------------------------------------------------------ 
			match.in.X.var.cat <- match(list.var,X.var.cat)[!is.na(match(list.var,X.var.cat))]	# the indeces in [myData.Work] of those categorical variable names in [list.var].  Could have "NA" 
			match.in.X.var.num <- match(list.var,X.var.num)[!is.na(match(list.var,X.var.num))]	# the indeces in [myData.Work] of those numeric     variable names in [list.var].  Could have "NA" 
			
			# get the categorical variables in the [list.var] and put into [XX.var.cat]
			if (all(!(is.na(match.in.X.var.cat))))
			{
				XX.var.cat <- X.var.cat[match.in.X.var.cat]
			}else{
				cat(paste("One or more categorical variables in [list.var] does not present in the [myData.Work].  Check it!\n",sep=""))
				cat(paste("One or more categorical variables in [list.var] does not present in the [myData.Work].  Check it!\n",sep=""),file=FL.LOG,append=TRUE)
				die;
			}
			
			# get the numeric variables in the [list.var] and put into [XX.var.num]
			if (all(!(is.na(match.in.X.var.num))))
			{
				XX.var.num <- X.var.num[match.in.X.var.num]
			}else{
				cat(paste("One or more numeric variables in [list.var] does not present in the [myData.Work].  Check it!\n",sep=""))
				cat(paste("One or more numeric variables in [list.var] does not present in the [myData.Work].  Check it!\n",sep=""),file=FL.LOG,append=TRUE)
				die;
			}			
			
			# the total number of matched categoical and numeric variables should be equal to the total number of variable in [list.var].  Otherwise, some variables are not found in [myData.Work]
			if (length(list.var) != sum(length(XX.var.cat),length(XX.var.num)))
			{
				cat(paste("the number of variables in the list [",length(list.var),"] is not the same as the sum of the numbers of categorical and  numeric variables [", sum(length(XX.var.cat),length(XX.var.num)),"], Check why!\n",sep=""))
				cat(paste("the number of variables in the list [",length(list.var),"] is not the same as the sum of the numbers of categorical and  numeric variables [", sum(length(XX.var.cat),length(XX.var.num)),"], Check why!\n",sep=""),file=FL.LOG,append=TRUE)
			}

			count.var <- 0
			
			for (this.var in list.var)
			{
				count.var <- count.var + 1
				if(is.factor(myData.Work[,this.var]))
				{
					this.cat.levels   <- levels(myData.Work[,this.var])
					this.cat.no.lvl   <- length(this.cat.levels)
					this.cat.no.obs   <- dim(myData.Work)[1]
					this.cat.var.kept <- paste(this.var,"_",levels(myData.Work[,this.var])[-1],sep="")	# turn the categorical variable into N-1 dummy variable after excluding the baseline level.

					# define an empty array for the N-1 extended categorical variables
				      myData.X.this.cat  <- data.frame(matrix(rep(0,(this.cat.no.lvl-1)*this.cat.no.obs),ncol=(this.cat.no.lvl-1)))
				names(myData.X.this.cat) <- this.cat.var.kept

					# assign 1 for each dummy variable when the level presents
					for (idx.level in seq(from=2,to=length(this.cat.levels)))
					{
						# initialize with 0
						new.var  <- paste(this.var,"_",levels(myData.Work[,this.var])[idx.level],sep="")
						command.string <- paste("myData.X.this.cat[,\"",new.var,"\"] <- rep(0,dim(myData.Work)[1])",sep="")
						eval(parse(text=command.string))

						# adjust to 1 if the level presents
						command.string <- paste("myData.X.this.cat[myData.Work[,this.var] == this.cat.levels[idx.level],\"",new.var,"\"] <- 1",sep="")
						eval(parse(text=command.string))		
					}

					# put the converted categorical variable back to a data frame
					if (count.var == 1)
					{
						myData.X   <- myData.X.this.cat					# data: accumulate the converted dummy variables derived for each of the categorical variable
						X.var.kept <- this.cat.var.kept					# name: accumulate the converted dummy variables derived for each of the categorical variable
					}else{
						myData.X   <- cbind(myData.X,myData.X.this.cat)			# data: accumulate the converted dummy variables derived for each of the categorical variable
						X.var.kept <- c(X.var.kept,this.cat.var.kept)			# name: accumulate the converted dummy variables derived for each of the categorical variable
					}				
				}else{
					myData.X.this.num  <- myData.Work[,this.var,drop=FALSE]
					
					if (count.var == 1)
					{
						myData.X   <- myData.X.this.num					# data: accumulate the converted dummy variables derived for each of the categorical variable
						X.var.kept <- this.var						# name: accumulate the converted dummy variables derived for each of the categorical variable
					}else{
						myData.X   <- cbind(myData.X,myData.X.this.num)			# data: accumulate the converted dummy variables derived for each of the categorical variable
						X.var.kept <- c(X.var.kept,this.var)				# name: accumulate the converted dummy variables derived for each of the categorical variable
					}				
				}
			}

	
			#
			# July 17, 2013: 
			# The conversion of categorical variables to dummy variables may introduce un-desired character in the column names.
			#         need to remove " ", "or", "-" from the col names which will cause problems in OpenBUGS
			#
			names(myData.X) <- sub("\\.","_",sub("-","",sub("\\s+","",sub(" or ","_",names(myData.X)))))
			X.var.kept      <- sub("\\.","_",sub("-","",sub("\\s+","",sub(" or ","_",X.var.kept))))
			
			
			
			# ------------------------------------------------------------------------------
			# the data after conversion for this [list.var]
			# ------------------------------------------------------------------------------
			no.obs         <- dim(myData.X)[1]
			no.var         <- length(X.var.kept)
			myData.Tmp     <- cbind(myData.Y,myData.X,SUPPORT.calc)
		  names(myData.Tmp)    <- c("Pal_cnt",names(myData.X),"support")
			myData.support <- SUPPORT.calc
			cat(paste("4c. data ready for Bugs!\n",sep=""))
			cat(paste("4c. data ready for Bugs!\n",sep=""),file=FL.LOG,append=TRUE)										


				# ------------------------------------------------------------------------------
				# MODEL
				# ------------------------------------------------------------------------------				
				# create the model
				# part 1:
				part1= 
"model{
	for (i in 1:n)					
	{
		Pal_cnt[i] ~ dpois(mu[i])		
		mu[i] <- lambda[i,T[i]] * support[i]	\n"

							
				
				# part 2: lambda part
				part2.lambda <- "\n\n
		# Log-linear model [Poisson means] 
		lambda[i,1] <- 0						
		log(lambda[i,2]) <- b0                      + \n"	
				idx.count <- 0
				for (idx.var in X.var.kept)
				{
					idx.count <- idx.count + 1
					if (idx.count == no.var)
					{
						part2.lambda <- paste(part2.lambda,"\t\tb[",idx.count,"]*",idx.var,"[i]\n",   sep="")
					}else{
						part2.lambda <- paste(part2.lambda,"\t\tb[",idx.count,"]*",idx.var,"[i]                      + \n",sep="")
					}
				}
				
				# part3: logistic part
				part3.logit <- "\n\n
		# Logistic regression for the [zero-inflation probability]
		logit(P[i,1]) <-    a0                      + 	\n"	
				idx.count <- 0
				for (idx.var in X.var.kept)
				{
					idx.count <- idx.count + 1
					if(idx.count == no.var)
					{
						part3.logit <- paste(part3.logit,"\t\ta[",idx.count,"]*",idx.var,"[i]\n",   sep="")
					}else{
						part3.logit <- paste(part3.logit,"\t\ta[",idx.count,"]*",idx.var,"[i]                      + \n",sep="")
					}
				}
				
				# part 4: the rest
				part4 <- paste("
				
		P[i,2] <- 1-P[i,1]				
		T[i] ~ dcat(P[i,1:2])		
	}		

	# normal distributions are assumed for all the parameters beta and gamma
	# Flat Priors on parameters
	a0 ~ dnorm(0,1.0E-6)
	b0 ~ dnorm(0,1.0E-6)

	for (j in 1:",no.var,"){a[j]  ~ dnorm(0,1.0E-6)}
	for (j in 1:",no.var,"){b[j]  ~ dnorm(0,1.0E-6)}

}",sep="")	
					
			modelstring = paste(part1,part2.lambda,part3.logit,part4)

			# write the model out
			writeLines(modelstring,con=FL.MODEL)
			writeLines(modelstring,con=FL.ODC)				
			cat(paste("5. Model has been set up for [",list.var,"]!\n"))
			cat(paste("5. Model has been set up for [",list.var,"]!\n"),file=FL.LOG,append=TRUE)


			# ------------------------------------------------------------------------------------------------
			# INIT
			# ------------------------------------------------------------------------------------------------
			cat(paste(    "INITS\nlist(a0=",A[1],",\nb0=",B[1],",\na=c(",paste(A[2:length(A)],collapse=","),"),\nb=c(",paste(B[2:length(B)],collapse=","),"))\n",sep=""),file=FL.INIT,append=TRUE)
			cat(paste("\n\nINITS\nlist(a0=",A[1],",\nb0=",B[1],",\na=c(",paste(A[2:length(A)],collapse=","),"),\nb=c(",paste(B[2:length(B)],collapse=","),"))\n",sep=""),file=FL.ODC, append=TRUE)
			cat(paste("6. Inits have been prepared and loaded into Bugs for [",list.var,"]!\n"))
			cat(paste("6. Inits have been prepared and loaded into Bugs for [",list.var,"]!\n"),file=FL.LOG,append=TRUE)



			# ------------------------------------------------------------------------------
			# DATA
			# ------------------------------------------------------------------------------
			cat(paste(    "DATA\nlist(n=",no.obs,",\n",sep=""),    file=FL.DATA,append=TRUE)
			cat(paste("\n\nDATA\nlist(n=",no.obs,",\n",sep=""),file=FL.ODC, append=TRUE)
			count.var <- 0
			for (var.name in names(myData.Tmp))
			{
				count.var <- count.var + 1
				if (count.var < dim(myData.Tmp)[2])
				{
					cat(paste(var.name," = c(",paste(myData.Tmp[,var.name],collapse=","),"),\n",sep=""),file=FL.DATA,append=TRUE)
					cat(paste(var.name," = c(",paste(myData.Tmp[,var.name],collapse=","),"),\n",sep=""),file=FL.ODC, append=TRUE)
				}else{
					cat(paste(var.name," = c(",paste(myData.Tmp[,var.name],collapse=","),"))\n",sep=""),file=FL.DATA,append=TRUE)
					cat(paste(var.name," = c(",paste(myData.Tmp[,var.name],collapse=","),"))\n",sep=""),file=FL.ODC, append=TRUE)
				}
			}
			cat(paste("7. Data has been prepared and loaded into Bugs for [",list.var,"]!\n"))
			cat(paste("7. Data has been prepared and loaded into Bugs for [",list.var,"]!\n"),file=FL.LOG,append=TRUE)
		}	# End of model to be evaluated by OpenBUGS 			
	} 		# End of Conditional Loop of the existence of the candidate variable
}			# End of Loop 1 through the list of candidate variables



# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n15_GenerateData_for_OpenBugs.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n15_GenerateData_for_OpenBugs.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [15_GenerateData_for_OpenBugs.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [15_GenerateData_for_OpenBugs.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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


