#
# 13_Chk_Regroup_in_Subsets.R
#
# This is revised on "9_Chk_Regroup_in_Subsets.R"
#
# purpose: to check the re-grouping of lithology and particle size variables
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
# Kyle prepared the relative discharge on June 21, 2013
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

# Data Folder
Path.IN  <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/12_Prep_Subset_Data"
Path.log <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.OUT <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/13_Chk_Regroup_in_Subsets"
if (!file.exists(Path.IN)) {stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}

# data file
FL.OBJ.IN <- paste(Path.IN, "12_Prep_Subset_Data.RData", sep="/")
FL.LOG    <- paste(Path.log,"13_Chk_Regroup_in_Subsets.log",sep="/")	
if (!file.exists(FL.OBJ.IN)){stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.LOG))   {print(paste(FL.LOG,      "exist.Delete it!")); file.remove(FL.LOG)}


FL.ReGroup.SUM <- paste(Path.OUT, paste("ReGrouping_Summary.csv",sep=""),sep="/")
if  (file.exists(FL.ReGroup.SUM)){print(paste(FL.ReGroup.SUM,"exist.Delete it!")); file.remove(FL.ReGroup.SUM)}


#
# load the data
#
load(FL.OBJ.IN)
cat(paste("0. [",FL.OBJ.IN,"] has been loaded!\n",sep=""))
cat(paste("0. [",FL.OBJ.IN,"] has been loaded!\n",sep=""),file=FL.LOG,append=TRUE)

# ******************************************************************
# Loop of Subsets
# ******************************************************************
subsets.section <- c("Upper","Upper","Lower","Lower","Lower","Upper",   "Lower",   "All")
subsets.gear    <- c("OT_TN","TLC",  "OT_TN","TLC",  "GN",   "Logistic","Logistic","All")
for (idx.subset in c(1,2,3,4,5,6,7,8))
{
	# -------------------------------------------------------------------------------------------------
	# 1. name subset and save them in the binary R data file
	# -------------------------------------------------------------------------------------------------
	subset.section <- subsets.section[idx.subset]
	subset.gear    <- subsets.gear[idx.subset]

	#
	# prepare folder for subset of data
	#
	Path.OUT.subset <- paste(Path.OUT,paste(paste("subset",idx.subset,sep=""),subset.section,subset.gear,sep="_"),sep="/")
	if (!file.exists(Path.OUT.subset))    {print(paste("NOT existing:",Path.OUT.subset));dir.create(Path.OUT.subset,showWarnings=TRUE,recursive=TRUE)}


	
	cat(paste("\n\n************************************ [",subset.gear,"] and [",subset.section,"] ","************************************","\n",sep=""))
	cat(paste("\n\n************************************ [",subset.gear,"] and [",subset.section,"] ","************************************","\n",sep=""),file=FL.LOG,append=TRUE)
	
	FL.OUT.CSV <- paste(Path.OUT.subset, paste("Subset_",subset.section,"_",subset.gear,".csv",sep=""),sep="/")
	if  (file.exists(FL.OUT.CSV)){print(paste(FL.OUT.CSV,"exist.Delete it!")); file.remove(FL.OUT.CSV)}





	# subset 1: upper OT and TN (active gear) [myData.Upper_Active]
	if (idx.subset == 1)
	{		
		myData.Sub      <- myData.Upper_Active    
		col.title.4.sub <- col.title.Upper_Active  
		col.class.4.sub <- col.class.Upper_Active  	
		
		old.Lith <- "Lith_1"
		
	# subset 2: upper TLC [myData.Upper_TLC]
	}else if(idx.subset == 2){
		myData.Sub      <- myData.Upper_TLC    
		col.title.4.sub <- col.title.Upper_TLC  
		col.class.4.sub <- col.class.Upper_TLC 	
		
		old.Lith <- "Lith_1"
		
	# subset 3: lower OT & TN	[myData.Lower_Active]
	}else if(idx.subset == 3){
		myData.Sub      <- myData.Lower_Active    
		col.title.4.sub <- col.title.Lower_Active  
		col.class.4.sub <- col.class.Lower_Active 			
		
		old.Lith <- "Lith_desc"
		
	# subset 4: lower TLC [myData.Lower_TLC]
	}else if(idx.subset == 4){
		myData.Sub      <- myData.Lower_TLC    
		col.title.4.sub <- col.title.Lower_TLC   
		col.class.4.sub <- col.class.Lower_TLC  		
		
		old.Lith <- "Lith_desc"
		
	# subset 5: lower GN [myData.Lower_GN]
	}else if(idx.subset == 5){
		myData.Sub      <- myData.Lower_GN    
		col.title.4.sub <- col.title.Lower_GN   
		col.class.4.sub <- col.class.Lower_GN 		
		
		old.Lith <- "Lith_desc"
		
	# subset 6: Logistic Upper [myData.Upper_Logistic]
	}else if(idx.subset == 6){
		myData.Sub      <- myData.Upper_Logistic    
		col.title.4.sub <- col.title.Upper_Logistic   
		col.class.4.sub <- col.class.Upper_Logistic 		
		
		old.Lith <- "Lith_1"
		
	# subset 6: Logistic Lower [myData.Lower_Logistic]
	}else if(idx.subset == 7){
		myData.Sub      <- myData.Lower_Logistic    
		col.title.4.sub <- col.title.Lower_Logistic   
		col.class.4.sub <- col.class.Lower_Logistic 		
		
		old.Lith <- "Lith_desc"
		
	# subset 7: All
	}else if(idx.subset == 8){
		myData.Sub      <- myData.All    
		col.title.4.sub <- col.title.All   
		col.class.4.sub <- col.class.All 		
		
		old.Lith <- "Lith_1"
	}	
	
	cat(paste("[",subset.section,"] and [",subset.gear,"],",sep=""),file=FL.OUT.CSV,append=TRUE)
	write.table(myData.Sub,file=FL.OUT.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	
	cat(paste("\t1. Subset data of [",subset.section,"] and [",subset.gear,"] has been assigned and re-written in csv file!\n",sep=""))
	cat(paste("\t1. Subset data of [",subset.section,"] and [",subset.gear,"] has been assigned and re-written in csv file!\n",sep=""),file=FL.LOG,append=TRUE)
	
	# 2. check the re-grouping of lithology and particle size variables
	new.Lith <- "new_lith"
	
	command.string <- paste("df.chk.sum1 <- as.data.frame(cast(myData.Sub,",old.Lith," ~ ",new.Lith,",fun=length,margins = c(\"grand_row\",\"grand_col\"),value=\"SY\"))",sep="")
	eval(parse(text=command.string))
	
	cat(paste("\n[",old.Lith,"] vs [",new.Lith,"] for [",subset.section,"] and [",subset.gear,"],",sep=""),file=FL.ReGroup.SUM,append=TRUE)
	write.table(df.chk.sum1,file=FL.ReGroup.SUM,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	
	cat(paste("\t2. The correspondence of re-grouping of lithology for subset data of [",subset.section,"] and [",subset.gear,"] has been checked!\n",sep=""))
	cat(paste("\t2. The correspondence of re-grouping of lithology for subset data of [",subset.section,"] and [",subset.gear,"] has been checked!\n",sep=""),file=FL.LOG,append=TRUE)
	
	# 3. check particle size re-grouping
	df.chk.sum2 <- as.data.frame(cast(myData.Sub,Tax_part_sz ~ new_part,fun=length,margins = c("grand_row","grand_col"),value="SY"))
	
	cat(paste("\n[Tax_part_sz] vs [new_part] for [",subset.section,"] and [",subset.gear,"],",sep=""),file=FL.ReGroup.SUM,append=TRUE)
	write.table(df.chk.sum2,file=FL.ReGroup.SUM,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	
	cat(paste("\t3. The correspondence of re-grouping of lithology for subset data of [",subset.section,"] and [",subset.gear,"] has been checked!\n",sep=""))
	cat(paste("\t3. The correspondence of re-grouping of lithology for subset data of [",subset.section,"] and [",subset.gear,"] has been checked!\n",sep=""),file=FL.LOG,append=TRUE)	


	# 4. check Meso re-grouping
	df.chk.sum3 <- as.data.frame(cast(myData.Sub,Meso ~ new_meso,fun=length,margins = c("grand_row","grand_col"),value="SY"))
	
	cat(paste("\n[Meso] vs [new_meso] for [",subset.section,"] and [",subset.gear,"],",sep=""),file=FL.ReGroup.SUM,append=TRUE)
	write.table(df.chk.sum3,file=FL.ReGroup.SUM,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	
	
	df.chk.sum4 <- cast(myData.Sub,Meso ~ new_meso | binary,fun=length)
	for (idx in names(df.chk.sum4))
	{
		cat(paste("\n[Meso] vs [new_meso] | [binary] in [",idx,"],"),file=FL.ReGroup.SUM,append=TRUE)
		write.table(data.frame(df.chk.sum4[idx]),file=FL.ReGroup.SUM,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)			
	}
	cat(paste("\t4. The correspondence of re-grouping of Meso for subset data of [",subset.section,"] and [",subset.gear,"] has been checked!\n",sep=""))
	cat(paste("\t4. The correspondence of re-grouping of Meso for subset data of [",subset.section,"] and [",subset.gear,"] has been checked!\n",sep=""),file=FL.LOG,append=TRUE)	


	# 5. check NFHAP re-grouping
	df.chk.sum5 <- as.data.frame(cast(myData.Sub,NFHAP ~ new_NFHAP,fun=length,margins = c("grand_row","grand_col"),value="SY"))
	
	cat(paste("\n[NFHAP] vs [new_NFHAP] for [",subset.section,"] and [",subset.gear,"],",sep=""),file=FL.ReGroup.SUM,append=TRUE)
	write.table(df.chk.sum5,file=FL.ReGroup.SUM,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	
	
	df.chk.sum5 <- cast(myData.Sub,NFHAP ~ new_NFHAP | binary,fun=length)
	for (idx in names(df.chk.sum5))
	{
		cat(paste("\n[NFHAP] vs [new_NFHAP] | [binary] in [",idx,"],"),file=FL.ReGroup.SUM,append=TRUE)
		write.table(data.frame(df.chk.sum4[idx]),file=FL.ReGroup.SUM,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)			
	}
	cat(paste("\t5. The correspondence of re-grouping of NFHAP for subset data of [",subset.section,"] and [",subset.gear,"] has been checked!\n",sep=""))
	cat(paste("\t5. The correspondence of re-grouping of NFHAP for subset data of [",subset.section,"] and [",subset.gear,"] has been checked!\n",sep=""),file=FL.LOG,append=TRUE)	
}


# 6. did not see entries of "limestone" and "gravel" of [Lith_1] in the upper river subsets (lower river use [Lith_desc] not [Lith_1] so there should be no "gravel" appearing in the lower river subset.: check it
df.chk.sum <- as.data.frame(cast(myData.Sub,Lith_1 ~ gear.type1,fun=length))
cat(paste("\n[Lith_1] vs [gear.type1] of all data\n"),file=FL.ReGroup.SUM,append=TRUE)
write.table(df.chk.sum,file=FL.ReGroup.SUM,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			


df.chk.sum <- cast(myData.Sub,Lith_1 ~ gear.type1 | MA,fun=length)
for (idx in names(df.chk.sum))
{
	cat(paste("\n[Lith_1] vs [gear.type1] in [",idx,"],"),file=FL.ReGroup.SUM,append=TRUE)
	write.table(data.frame(df.chk.sum[idx]),file=FL.ReGroup.SUM,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)			
}


df.chk.sum <- as.data.frame(cast(myData.Sub,Lith_1 ~ Lith_desc,fun=length))
cat(paste("\n[Lith_1] vs [Lith_desc] of all data\n"),file=FL.ReGroup.SUM,append=TRUE)
write.table(df.chk.sum,file=FL.ReGroup.SUM,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)

df.chk.sum <- cast(myData.Sub,Lith_1 ~ Lith_desc | MA,fun=length)
for (idx in names(df.chk.sum))
{
	cat(paste("\n[Lith_1] vs [Lith_desc] in [",idx,"],"),file=FL.ReGroup.SUM,append=TRUE)
	write.table(data.frame(df.chk.sum[idx]),file=FL.ReGroup.SUM,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)			
}

cat(paste("\t6. The correspondence of re-grouping of lithology for subset data of [",subset.section,"] and [",subset.gear,"] has been checked!\n",sep=""))
cat(paste("\t6. The correspondence of re-grouping of lithology for subset data of [",subset.section,"] and [",subset.gear,"] has been checked!\n",sep=""),file=FL.LOG,append=TRUE)	

# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n13_Chk_Regroup_in_Subsets.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n13_Chk_Regroup_in_Subsets.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [13_Chk_Regroup_in_Subsets.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [13_Chk_Regroup_in_Subsets.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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






