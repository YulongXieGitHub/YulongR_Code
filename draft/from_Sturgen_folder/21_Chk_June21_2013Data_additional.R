#
# 21_Chk_June21_2013Data_additional.R 
# (different from 21_Chk_June21Data.R on here only calc on categorical variable "SY")
# (11_Chk_June21_2013Data_categoricalSum.csv is one we need)
#
# August 15, 2013
# Create this on August 15, 2013 based on the 11 series of R scripts.
# The purpose is to remove the NA data of those variables we are going to use eventually in front.
# This will make sure the data statistics / plots are consistsent with the modeling results 
#
# # Used to get the zero distribution of the data in the river streams.
#
#
# June 24, 2013
#
# This is a revision of "6_Chk_May31_2013Data.R".
#
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
Path.Data.IN <- "C:/YuLong_Projects/FY2012_MORiver/_data_received/sturg_spatial/data/Habitat_Metrics_Output"
Path.log     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/21_Chk_June21_2013Data_additional"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}

# data file
FL.Data.IN   <- paste(Path.Data.IN,"MoRiver_Variables_2013May31_revisedOn_2013June21_YLX.csv",sep="/")
FL.LOG       <- paste(Path.log,"21_Chk_June21_2013Data.log",                                  sep="/")	
FL.Data.OUT  <- paste(Path.Out,"MoRiver_Variables_2013May31_revisedOn_2013June21_YLX_app.csv",sep="/")
FL.SUM.cat   <- paste(Path.Out,"21_Chk_June21_2013Data_categoricalSum.csv",                   sep="/")
FL.SUM.num   <- paste(Path.Out,"21_Chk_June21_2013Data_numericSum.csv",                       sep="/")
FL.Summary   <- paste(Path.Out,"21_Chk_June21_2013Data_summary.csv",                          sep="/")
FL.missing   <- paste(Path.Out,"21_Chk_June21_2013Data_missing.csv",                          sep="/")
if (!file.exists(FL.Data.IN))  {stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.LOG))      {print(paste(FL.LOG,      "exist.Delete it!")); file.remove(FL.LOG)}
if  (file.exists(FL.Data.OUT)) {print(paste(FL.Data.OUT, "exist.Delete it!")); file.remove(FL.Data.OUT)}
if  (file.exists(FL.SUM.cat))  {print(paste(FL.SUM.cat,  "exist.Delete it!")); file.remove(FL.SUM.cat)}
if  (file.exists(FL.SUM.num))  {print(paste(FL.SUM.num,  "exist.Delete it!")); file.remove(FL.SUM.num)}
if  (file.exists(FL.Summary))  {print(paste(FL.Summary,  "exist.Delete it!")); file.remove(FL.Summary)}
if  (file.exists(FL.missing))  {print(paste(FL.missing,  "exist.Delete it!")); file.remove(FL.missing)}


library("lattice")
library("reshape")



# read April 24, 2013 data 
row.titles  <- c("ID",     "SY",     "FieldOffice","Project","UniqueID","Gear",  "Season","Bend",   "BendRN", "BendRiverMile","Near_NHD2rm_FID","Near_NHD2RM_dist_m","NHD2_RM","X1960BendID","RM1960_RM","UpDamDist","UpDamNm","DnDamDist","DnDamNm",                                 "Ch_W_Full","Ch_W_NoIsl","Braiding","UpTrib",   "UpTribDist","DnTrib",   "DTribDist",                            "Reach",  "Mean_z", "Max_z",  "Min_z",  "Grade10RM","GradeBend","Rocktype1","Rocktype2","Lithology","NFHAP_Scr","MedFlow",  "MeanFlow",  "taxorder","taxpartsiz", "comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Segment","nPage",  "TotalPages","SetDate",  "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turbidity","Conductivity","DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "DecimalTimeDifference","StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3",              "Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","VelocityBot2","VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Total_Fish_Count","Pallid_Only_Count","CPUE",   "CPU_Area","Alt_Pallid_Only_Count","Alt_CPUE","Alt_CPU_Area","S_U10_D2_Near","S_U10_D2_Cumu","S_U20_D4_Near","S_U20_D4_Cumu","S_U30_D6_Near","S_U30_D6_Cumu","S_U50_D10_Near","S_U50_D10_Cumu","S_U100_D20_Near","S_U100_D20_Cumu",                            "gear.type1","gear.type2","macro.type")
colClasses  <- c("integer","integer","factor",     "integer","integer", "factor","factor","integer","factor", "numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",  "numeric",  "factor", "numeric",  "factor",                                  "numeric",  "numeric",   "integer", "factor",   "numeric",   "factor",   "numeric",                              "factor", "numeric","numeric","numeric","numeric",  "numeric",  "factor",   "factor",   "factor",   "factor",   "numeric",  "numeric",   "factor",  "factor",     "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "integer","integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric",             "numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",                                    "factor",    "factor",    "factor")
used.4.plot <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     TRUE,    TRUE,    TRUE,     FALSE,   TRUE,            FALSE,            FALSE,               FALSE,    FALSE,        TRUE,       TRUE,       FALSE,    FALSE,      FALSE,                                     TRUE,       TRUE,        TRUE,      FALSE,      TRUE,        FALSE,      TRUE,                                   TRUE,     TRUE,     TRUE,     TRUE,     TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,        TRUE,      TRUE,         TRUE,       TRUE,         TRUE,         TRUE,         TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     FALSE,      FALSE,         FALSE,    TRUE,      FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    TRUE,    FALSE,   TRUE,    TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          TRUE,     TRUE,     TRUE,                  FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,                                         TRUE,        TRUE,        TRUE)
used.4.anal <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     FALSE,   TRUE,    FALSE,    FALSE,    FALSE,          FALSE,            FALSE,               FALSE,    FALSE,        FALSE,      TRUE,       FALSE,    FALSE,      FALSE,                                     TRUE,       FALSE,       TRUE,      FALSE,      TRUE,        FALSE,      TRUE,                                   TRUE,     FALSE,    FALSE,    FALSE,    TRUE,       FALSE,      TRUE,       FALSE,      FALSE,      TRUE,       FALSE,      FALSE,       FALSE,     TRUE,         TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     TRUE,       FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    FALSE,   FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,                 FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,                                         TRUE,        TRUE,        TRUE)

row.titles  <- c("Dep_ID", "SY",     "FO",         "Proj",   "Un_ID",   "Gear",  "Season","Bend",   "Bend_RN","Bend_RM",      "Near_NHD2rm_FID","Near_NHD2RM_dist_m","GIS_RM", "Bend_ID_60", "Bend_RM_60","D_dist_up","D_name_up","D_dist_dn","D_name_dn","D_name_near","D_dist_near","Chan_wid", "Wet_wid",   "Braid"   ,"T_name_up","T_dist_up", "T_name_dn","T_dist_dn","T_name_near","T_dist_near","MA",     "Mean_z", "Max_z",  "Min_z",  "Grade_10_RM",          "Lith_1",   "Lith_2",   "Lith_desc","NFHAP",    "Disch_med","Disch_mean","Tax_ord", "Tax_part_sz","Comp_per", "Sand_per",   "Clay_per",   "Frag_per",   "VF_wid",  "Seg",    "nPage",  "TotalPages","Set_date", "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turb",     "Cond",        "DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "Set_dur",              "StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3", "Depth_Mean","Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","Btm_vel",     "VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Fish_cnt",        "Pal_cnt",          "CPUE",   "CPUA",    "Alt_Pal_Cnt",          "CPUE_2",  "CPUA_2",      "Stk_12_dist",  "Stk_12_cnt",   "Stk_24_dist",  "Stk_24_cnt",   "Stk_36_dist",  "Stk_36_cnt",   "Stk_60_dist",   "Stk_60_cnt",    "Stk_120_dist",   "Stk_120_cnt",    "Rel_dsch_AY","Rel_dsch_WY","gear.type1","gear.type2","macro.type")
colClasses  <- c("integer","integer","factor",     "integer","integer", "factor","factor","integer","factor" ,"numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",   "numeric",  "factor",   "numeric",  "factor",   "factor",     "numeric",    "numeric",  "numeric",   "integer", "factor",   "numeric",   "factor",   "numeric",  "factor"     ,"numeric",    "factor", "numeric","numeric","numeric","numeric",              "factor",   "factor",   "factor",   "factor",   "numeric",  "numeric",   "factor",  "factor",     "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "integer","integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric","numeric",   "numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",        "numeric",    "numeric",    "factor",    "factor",    "factor")
used.4.plot <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     TRUE,    TRUE,    TRUE,     FALSE,    TRUE,           FALSE,            FALSE,               FALSE,    FALSE,        TRUE,        TRUE,       FALSE,      TRUE,       FALSE,      TRUE,         TRUE,         TRUE,       TRUE,        TRUE,      FALSE,      TRUE,        FALSE,      TRUE,       TRUE,         TRUE,         TRUE,     TRUE,     TRUE,     TRUE,     TRUE,                   TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,        TRUE,      TRUE,         TRUE,       TRUE,         TRUE,         TRUE,         TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     FALSE,      FALSE,         FALSE,    TRUE,      FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    TRUE,    FALSE,   TRUE,    TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          TRUE,     TRUE,     TRUE,     TRUE,        FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,         TRUE,         TRUE,        TRUE,        TRUE)
used.4.anal <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     FALSE,   TRUE,    FALSE,    FALSE,    FALSE,          FALSE,            FALSE,               FALSE,    FALSE,        FALSE,       TRUE,       FALSE,      TRUE,       FALSE,      TRUE,         TRUE,         TRUE,       FALSE,       TRUE,      FALSE,      TRUE,        FALSE,      TRUE,       TRUE,         TRUE,         TRUE,     FALSE,    FALSE,    FALSE,    TRUE,                   TRUE,       FALSE,      FALSE,      TRUE,       FALSE,      FALSE,       FALSE,     TRUE,         TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     TRUE,       FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    FALSE,   FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,    TRUE,        FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,         TRUE,         TRUE,        TRUE,        TRUE)


# the raw data including all variables
      myData.raw  <- read.csv(file=FL.Data.IN,sep=",",header=TRUE,stringsAsFactors=TRUE)
names(myData.raw) <- row.titles

#
# remove data from segment 5 and 6
#
myData.raw <- myData.raw[(myData.raw[,"Seg"] != 5) & (myData.raw[,"Seg"] != 6),]

# **** added a [binary] field **** 
myData.raw <- cbind(myData.raw,binary = rep("no",dim(myData.raw)[1]),stringsAsFactors=FALSE)
myData.raw[myData.raw[,"Pal_cnt"] > 0,"binary"] <- "Yes"

#
# **** it is realized that the CPUE field contains values for active gear net like OT and TN, re-set them to NA.  (see the formula used in Chris Vernon's spread sheet). *** 
#
myData.raw[myData.raw[,"Gear"] == "OT16S" | myData.raw[,"Gear"] == "TNS" | myData.raw[,"Gear"] == "TN25S",c("CPUE","CPUE_2")] <- NA


# *********************************************************************************************************************
# The following Section is added to take care of the suggestion in Eric's May 21, 2013 message onthe CPUE/CPUA checking
# FL_Total_cnt_vs_Pal_cnt.sum <- paste(Path.Out,"21_Chk_June21_2013Data_Gear_CPUE_CPUA.csv",sep="/")	
#
# Total Fish Catch vs Pallid Fish Catch
#
FL_Total_cnt_vs_Pal_cnt.pdf <- paste(Path.Out,"21_Chk_June21_2013Data_Tot_Cnt_vs_Pal_Cnt.pdf",sep="/")	
if  (file.exists(FL_Total_cnt_vs_Pal_cnt.pdf)){print(paste(FL_Total_cnt_vs_Pal_cnt.pdf,"exist.Delete it!")); file.remove(FL_Total_cnt_vs_Pal_cnt.pdf)}
pdf(file = FL_Total_cnt_vs_Pal_cnt.pdf,paper="a4r",width=0,height=0)	

par(mfrow = c(3,3))
boxplot(Fish_cnt~binary,data=myData.raw,                             notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Pallid Catch",ylab="Total Fish Catch",main = "\nAll data")
boxplot(Fish_cnt~binary,data=myData.raw[myData.raw[,"MA"]=="Lower",],notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Pallid Catch",ylab="Total Fish Catch",main = "Total Fish Catch vs Pallid Catch\nUpper")
boxplot(Fish_cnt~binary,data=myData.raw[myData.raw[,"MA"]=="Upper",],notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Pallid Catch",ylab="Total Fish Catch",main = "\nLower")

plot(Fish_cnt~Pal_cnt,data=myData.raw,                             type="p",pch=16,col="red",xlab="Pallid Catch",ylab="Total Fish Catch",main = "\nAll data")
plot(Fish_cnt~Pal_cnt,data=myData.raw[myData.raw[,"MA"]=="Lower",],type="p",pch=16,col="red",xlab="Pallid Catch",ylab="Total Fish Catch",main = "Total Fish Catch vs Pallid Catch\nUpper")
plot(Fish_cnt~Pal_cnt,data=myData.raw[myData.raw[,"MA"]=="Upper",],type="p",pch=16,col="red",xlab="Pallid Catch",ylab="Total Fish Catch",main = "\nLower")

plot(Fish_cnt~Pal_cnt,data=myData.raw[myData.raw[,"binary"]=="Yes",],                             type="p",pch=16,col="blue",xlab="Pallid Catch",ylab="Total Fish Catch",main = "\nAll data")
plot(Fish_cnt~Pal_cnt,data=myData.raw[myData.raw[,"binary"]=="Yes" & myData.raw[,"MA"]=="Lower",],type="p",pch=16,col="blue",xlab="Pallid Catch",ylab="Total Fish Catch",main = "Total Fish Catch vs Pallid Catch\nUpper")
plot(Fish_cnt~Pal_cnt,data=myData.raw[myData.raw[,"binary"]=="Yes" & myData.raw[,"MA"]=="Upper",],type="p",pch=16,col="blue",xlab="Pallid Catch",ylab="Total Fish Catch",main = "\nLower")
dev.off()
# *********************************************************************************************************************







# *********************************************************************************************************************
# The following Section is added to take care of the suggestion in Eric's May 21, 2013 message onthe CPUE/CPUA checking
# Results are in two files.
# FL_CPUE_CPUA.sum <- paste(Path.Out,"21_Chk_June21_2013Data_Gear_CPUE_CPUA.csv",sep="/")	
# FL_CPUE_CPUA.pdf <- paste(Path.Out,"21_Chk_June21_2013Data_Gear_CPUE_CPUA.pdf",sep="/")	
# *********************************************************************************************************************
#
# variable specific check: From: Oldenburg, Eric W; Sent: Tuesday, May 21, 2013 12:33 PM.  
# Suggest to compare the CPUE and CPUA between gear types 
#                                          CPUE for gill net (i.e., GN14-GN41 and GN18-GN81) and 
#                                          CPUE for trotline (i.e., TLC1S and TLS2S) and
#                                          CPUA for trammel net (i.e., TNS & TN25S) and Otte Trawl
#
#
# NOTE: CPUE: should ignore data for [OT16S] and [TN25S/TNS] in the original database
# formula for CPUE and CPUA calculation:
# CPUE:
# -----------------------------------------------------------------------------
# =IF(OR(S685="GN14S",S685="GN41S"),(EB685/CZ685)*24,
#  IF(OR(S685="GN18S",S685="GN81S"),(EB685/(CZ685*2))*24,
#  IF(OR(S685="OT16S",S685="TN25S",S685="TNS"),(EB685/CB685)*100,
#  IF(OR(S685="TLC1S",S685="TLC2S"),(((EB685/CB685)*20)/CZ685)*24))))
# -----------------------------------------------------------------------------
# GN14S and GN41S:	CPUE =  (n caught/ hours fished)   *24				caught: [Pallid_Only_Count], 	time duration: [Set-dur]		
# GN18S and GN81S:	CPUE =  (n caught/(hours fished*2))*24
# OT16S, TN25S, TNS:	CPUE =  (n caught/(distance drifted*gear width)*100		caught: [Pallid_Only_Count], 	drift distance: [Distance].					Do not see gear width????
# TLC1S, TLC2S:		CPUE = ((n caught/20 hooks*20)/hours fished)*24			caught: [Pallid_Only_Count], 	number of hooks: [Distance].	time duration: [Set-dur]	
#
# CPUA
# -----------------------------------------------------------------------------
# =IF(OR(S685="TN25S",S685="TNS"),(EB685/(CB685*(CONVERT(125,"ft","m"))))*100,	
#  IF(S685="OT16S",               (EB685/(CB685*(CONVERT(16,"ft","m"))))*100,"NULL"))
# -----------------------------------------------------------------------------
# TN25S and TNS:	CPUA = (n caught/(distance drifted * 125 (ft))*100		caught: [Pallid_Only_Count], 	drift distance: [Distance].					gear width: 125 ft
# OT16S:		CPUA = (n caught/(distance drifted * 16  (ft))*100		caught: [Pallid_Only_Count], 	drift distance: [Distance].					gear width: 16 ft

# -------------------------------------------------------------------------------------------------
# re-calculate the CPUE and CPUA for verification
# -------------------------------------------------------------------------------------------------
# GN14S and GN41S:	CPUE =  (n caught/ hours fished)   *24
# GN18S and GN81S:	CPUE =  (n caught/(hours fished*2))*24
# OT16S, TN25S, TNS:	CPUA =  (n caught/(distance drifted*gear width)*100		Do not see gear width????
# TLC1S, TLC2S:		CPUE = ((n caught/20 hooks*20)/hours fished)*24
# **** added a [CPUE.calc] and [CPUA.calc] field **** 
myData.raw[,"CPUE.calc"] <- rep(NA,dim(myData.raw)[1])
myData.raw[,"CPUA.calc"] <- rep(NA,dim(myData.raw)[1])
# CPUE
index.GN14S_41S <- myData.raw["Gear"] == "GN14S" | myData.raw["Gear"] == "GN41S"
myData.raw[index.GN14S_41S,"CPUE.calc"] <- (myData.raw[index.GN14S_41S,"Pal_cnt"] / myData.raw[index.GN14S_41S,"Set_dur"]) * 24

index.GN18S_81S <- myData.raw["Gear"] == "GN18S" | myData.raw["Gear"] == "GN81S"
myData.raw[index.GN18S_81S,"CPUE.calc"] <- (myData.raw[index.GN18S_81S,"Pal_cnt"] / myData.raw[index.GN18S_81S,"Set_dur"]) * 12

index.TLC <- myData.raw["Gear"] == "TLC1S" | myData.raw["Gear"] == "TLC2S"
myData.raw[index.TLC,"CPUE.calc"] <- (20 * (myData.raw[index.TLC,"Pal_cnt"] / myData.raw[index.TLC,"Distance"]) / myData.raw[index.TLC,"Set_dur"]) * 24

# CPUA
index.OT <- myData.raw["Gear"] == "OT16S"
myData.raw[index.OT,"CPUA.calc"] <- (100 * (myData.raw[index.OT,"Pal_cnt"] / (16*0.3048 * myData.raw[index.OT,"Distance"])))

index.TN <- myData.raw["Gear"] == "TNS" | myData.raw["Gear"] == "TN25S"
myData.raw[index.TN,"CPUA.calc"] <- (100 * (myData.raw[index.TN,"Pal_cnt"] / (125*0.3048 * myData.raw[index.TN,"Distance"])))

#
# output the appended data for checking the CPUA/CPUE fields against what already in the database
#
# output the number of data/missing data
# write.table(myData.raw,file=FL.Data.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
# cat(paste("Appended Input Data Has Been Outputted!\n",sep=""))
#cat(paste("Appended Input Data Has Been Outputted!\n",sep=""),file=FL.LOG,append=TRUE)
# 




# -------------------------------------------------------------------------------------------------
# 0. Check the gear CPUE/CPUA calculation related variables
# -------------------------------------------------------------------------------------------------
# open pdf file for outputting plots
FL_CPUE_CPUA.pdf <- paste(Path.Out,"21_Chk_June21_2013Data_Gear_CPUE_CPUA.pdf",sep="/")	
if  (file.exists(FL_CPUE_CPUA.pdf)){print(paste(FL_CPUE_CPUA.pdf,"exist.Delete it!")); file.remove(FL_CPUE_CPUA.pdf)}
# pdf(file = FL_CPUE_CPUA.pdf,paper="a4r",width=0,height=0)	


# NOTE: CPUE: ignore data for [OT16S] and [TN25S/TNS]
# formula for CPUE and CPUA calculation:
# CPUE:
# -----------------------------------------------------------------------------
# =IF(OR(S685="GN14S",S685="GN41S"),(EB685/CZ685)*24,
#  IF(OR(S685="GN18S",S685="GN81S"),(EB685/(CZ685*2))*24,
#  IF(OR(S685="OT16S",S685="TN25S",S685="TNS"),(EB685/CB685)*100,
#  IF(OR(S685="TLC1S",S685="TLC2S"),(((EB685/CB685)*20)/CZ685)*24))))
# -----------------------------------------------------------------------------
# GN14S and GN41S:	CPUE =  (n caught/ hours fished)   *24				caught: [Pallid_Only_Count], 	time duration: [Set-dur]		
# GN18S and GN81S:	CPUE =  (n caught/(hours fished*2))*24
# OT16S, TN25S, TNS:	CPUE =  (n caught/(distance drifted*gear width)*100		caught: [Pallid_Only_Count], 	drift distance: [Distance].					Do not see gear width????
# TLC1S, TLC2S:		CPUE = ((n caught/20 hooks*20)/hours fished)*24			caught: [Pallid_Only_Count], 	number of hooks: [Distance].	time duration: [Set-dur]	
#
# CPUA
# -----------------------------------------------------------------------------
# =IF(OR(S685="TN25S",S685="TNS"),(EB685/(CB685*(CONVERT(125,"ft","m"))))*100,	
#  IF(S685="OT16S",               (EB685/(CB685*(CONVERT(16,"ft","m"))))*100,"NULL"))
# -----------------------------------------------------------------------------
# TN25S and TNS:	CPUA = (n caught/(distance drifted * 125 (ft))*100		caught: [Pallid_Only_Count], 	drift distance: [Distance].					gear width: 125 ft
# OT16S:		CPUA = (n caught/(distance drifted * 16  (ft))*100		caught: [Pallid_Only_Count], 	drift distance: [Distance].					gear width: 16 ft

# variables for the CPUE calculation
myData.T <- myData.raw[myData.raw[,"Gear"]=="TLC1S",];plot1  <- histogram(myData.T[,"Distance"],xlab="Number of Hooks",ylab="Count",main="TLC1S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="TLC2S",];plot2  <- histogram(myData.T[,"Distance"],xlab="Number of Hooks",ylab="Count",main="TLC2S",nint=100)
 
myData.T <- myData.raw[myData.raw[,"Gear"]=="TLC1S",];plot3  <- histogram(myData.T[,"Set_dur"], xlab="Hours", ylab="Count",main="TLC1S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="TLC2S",];plot4  <- histogram(myData.T[,"Set_dur"], xlab="Hours", ylab="Count",main="TLC2S",nint=100)
 
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN14S",];plot5  <- histogram(myData.T[,"Set_dur"], xlab="Hours", ylab="Count",main="GN14S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN41S",];plot6  <- histogram(myData.T[,"Set_dur"], xlab="Hours", ylab="Count",main="GN41S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN18S",];plot7  <- histogram(myData.T[,"Set_dur"], xlab="Hours", ylab="Count",main="GN18S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN81S",];plot8  <- histogram(myData.T[,"Set_dur"], xlab="Hours", ylab="Count",main="GN81S",nint=100)


# variables for the CPUA calculation
myData.T <- myData.raw[myData.raw[,"Gear"]=="OT16S",];plot9  <- histogram(myData.T[,"Distance"],xlab="Distance Drifted",ylab="Count",main="OT16S",nint=100)

myData.T <- myData.raw[myData.raw[,"Gear"]=="TNS",];  plot10 <- histogram(myData.T[,"Distance"],xlab="Distance Drifted",ylab="Count",main="TNS",  nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="TN25S",];plot11 <- histogram(myData.T[,"Distance"],xlab="Distance Drifted",ylab="Count",main="TN25S",nint=100)

# CPUE
myData.T <- myData.raw[myData.raw[,"Gear"]=="TLC1S",];plot001 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="TLC1S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="TLC2S",];plot002 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="TLC2S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN14S",];plot003 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="GN14S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN41S",];plot004 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="GN41S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN18S",];plot005 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="GN18S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN81S",];plot006 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="GN81S",nint=100)

# CPUA
myData.T <- myData.raw[myData.raw[,"Gear"]=="OT16S",];plot007 <- histogram(myData.T[,"CPUA"],xlab="CPUA",ylab="Count",main="OT16S",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="TNS",];  plot008 <- histogram(myData.T[,"CPUA"],xlab="CPUA",ylab="Count",main="TNS",  nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="TN25S",];plot009 <- histogram(myData.T[,"CPUA"],xlab="CPUA",ylab="Count",main="TN25S",nint=100)

# CPUE excluded 0 values
myData.T <- myData.raw[myData.raw[,"Gear"]=="TLC1S" & myData.raw[,"Pal_cnt"]>0,];plot011 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="TLC1S (no 0)",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="TLC2S" & myData.raw[,"Pal_cnt"]>0,];plot012 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="TLC2S (no 0)",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN14S" & myData.raw[,"Pal_cnt"]>0,];plot013 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="GN14S (no 0)",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN41S" & myData.raw[,"Pal_cnt"]>0,];plot014 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="GN41S (no 0)",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN18S" & myData.raw[,"Pal_cnt"]>0,];plot015 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="GN18S (no 0)",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="GN81S" & myData.raw[,"Pal_cnt"]>0,];plot016 <- histogram(myData.T[,"CPUE"], xlab="CPUE", ylab="Count",main="GN81S (no 0)",nint=100)

# CPUA excluded 0 values
myData.T <- myData.raw[myData.raw[,"Gear"]=="OT16S" & myData.raw[,"Pal_cnt"]>0,];plot017 <- histogram(myData.T[,"CPUA"],xlab="CPUA",ylab="Count",main="OT16S (no 0)",nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="TNS"   & myData.raw[,"Pal_cnt"]>0,];plot018 <- histogram(myData.T[,"CPUA"],xlab="CPUA",ylab="Count",main="TNS (no 0)",  nint=100)
myData.T <- myData.raw[myData.raw[,"Gear"]=="TN25S" & myData.raw[,"Pal_cnt"]>0,];plot019 <- histogram(myData.T[,"CPUA"],xlab="CPUA",ylab="Count",main="TN25S (no 0)",nint=100)




# plot: TLC1S and TLC2S
# plot(plot1,split=c(1,1,2,2))
# plot(plot2,split=c(2,1,2,2),newpage=FALSE)	
# plot(plot3,split=c(1,2,2,2),newpage=FALSE)	
# plot(plot4,split=c(2,2,2,2),newpage=FALSE)
# 
# # plot: GN14S, GN41S, GN18S, GN81S
# # plot(plot5,split=c(1,1,2,2))
# # plot(plot6,split=c(2,1,2,2),newpage=FALSE)	
# plot(plot7,split=c(1,2,2,2),newpage=FALSE)	
# plot(plot8,split=c(2,2,2,2),newpage=FALSE)
# 
# # plot: OT16S, TNS, TN25S
# # plot(plot9, split=c(1,1,1,2))
# # plot(plot10,split=c(1,2,2,2),newpage=FALSE)	
# plot(plot11,split=c(2,2,2,2),newpage=FALSE)	
# 
# plot: TLC1S and TLC2S
# plot(plot1,  split=c(1,1,3,2))
# plot(plot2,  split=c(1,2,3,2),newpage=FALSE)	
# plot(plot001,split=c(2,1,3,2),newpage=FALSE)	
# plot(plot002,split=c(2,2,3,2),newpage=FALSE)
# plot(plot011,split=c(3,1,3,2),newpage=FALSE)	
# plot(plot012,split=c(3,2,3,2),newpage=FALSE)
# 
# # plot: GN14S, GN41S, GN18S, GN81S
# plot(plot003,split=c(1,1,4,2))
# plot(plot004,split=c(2,1,4,2),newpage=FALSE)	
# plot(plot005,split=c(3,1,4,2),newpage=FALSE)	
# plot(plot006,split=c(4,1,4,2),newpage=FALSE)
# plot(plot013,split=c(1,2,4,2),newpage=FALSE)
# plot(plot014,split=c(2,2,4,2),newpage=FALSE)	
# plot(plot015,split=c(3,2,4,2),newpage=FALSE)	
# plot(plot016,split=c(4,2,4,2),newpage=FALSE)
# 
# 
# # plot: OT16S, TNS, TN25S
# # plot(plot007,split=c(1,1,3,2))
# plot(plot008,split=c(2,1,3,2),newpage=FALSE)	
# plot(plot009,split=c(3,1,3,2),newpage=FALSE)	
# plot(plot017,split=c(1,2,3,2),newpage=FALSE)
# plot(plot018,split=c(2,2,3,2),newpage=FALSE)	
# plot(plot019,split=c(3,2,3,2),newpage=FALSE)	
# 
# # -------------------------------------------------------------------------------------------------
# compare CPUE for gill net: between GN14S/41S against GN18S/81S
# -------------------------------------------------------------------------------------------------
# par(mfcol = c(2,3))
# # all data
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="GN",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"GN.type"] <- rep("GN14S_41S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "GN18S" |  myData.T[,"Gear"] == "GN81S"),"GN.type"] <- "GN18S_81S"
# 	boxplot(CPUE  ~ GN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Gill Net Type",ylab="CPUE",main = "(all data) Gill net types: included 0 count")
# }
# 
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="GN" & myData.raw[,"Pal_cnt"] > 0,]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"GN.type"] <- rep("GN14S_41S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "GN18S" |  myData.T[,"Gear"] == "GN81S"),"GN.type"] <- "GN18S_81S"
# 	boxplot(CPUE  ~ GN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Gill Net Type",ylab="CPUE",main = "(all data) Gill net types: excluded 0 count")
# }
# 
# # upper stream
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="GN" & myData.raw[,"MA"]=="Upper",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"GN.type"] <- rep("GN14S_41S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "GN18S" |  myData.T[,"Gear"] == "GN81S"),"GN.type"] <- "GN18S_81S"
# 	boxplot(CPUE  ~ GN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Gill Net Type",ylab="CPUE",main = "(Upper Stream) Gill net types: included 0 count")
# }
# 
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="GN" & myData.raw[,"MA"]=="Upper" & myData.raw[,"Pal_cnt"] > 0,]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"GN.type"] <- rep("GN14S_41S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "GN18S" |  myData.T[,"Gear"] == "GN81S"),"GN.type"] <- "GN18S_81S"
# 	boxplot(CPUE  ~ GN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Gill Net Type",ylab="CPUE",main = "(Upper Stream) Gill net types: excluded 0 count")
# }
# 
# # lower stream
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="GN" & myData.raw[,"MA"]=="Lower",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"GN.type"] <- rep("GN14S_41S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "GN18S" |  myData.T[,"Gear"] == "GN81S"),"GN.type"] <- "GN18S_81S"
# 	boxplot(CPUE  ~ GN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Gill Net Type",ylab="CPUE",main = "(Lower Stream) Gill net types: included 0 count")
# }
# 
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="GN" & myData.raw[,"MA"]=="Lower" & myData.raw[,"Pal_cnt"] > 0,]
# if (dim(myData.T)[1] > 0)
# {
# 	
# 	myData.T[,"GN.type"] <- rep("GN14S_41S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "GN18S" |  myData.T[,"Gear"] == "GN81S"),"GN.type"] <- "GN18S_81S"
# 	boxplot(CPUE  ~ GN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Gill Net Type",ylab="CPUE",main = "(Lower Stream) Gill net types: excluded 0 count")
# }
# 
# 
# # -------------------------------------------------------------------------------------------------
# # compare CPUE for trotline: between TLC1S and TLC2S
# # -------------------------------------------------------------------------------------------------
# par(mfcol = c(2,3))
# # all data
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TLC",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TLC.type"] <- rep("TLC1S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TLC2S"),"TLC.type"] <- "TLC2S"
# 	boxplot(CPUE  ~ TLC.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trotline Type",ylab="CPUE",main = "(All Data) Trotline type: included 0 count")
# }
# 
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TLC" & myData.raw[,"Pal_cnt"] > 0,]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TLC.type"] <- rep("TLC1S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TLC2S"),"TLC.type"] <- "TLC2S"
# 	boxplot(CPUE  ~ TLC.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trotline Type",ylab="CPUE",main = "(All Data) Trotline type: excluded 0 count")
# }
# 
# # upper stream data
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TLC" & myData.raw[,"MA"]=="Upper",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TLC.type"] <- rep("TLC1S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TLC2S"),"TLC.type"] <- "TLC2S"
# 	boxplot(CPUE  ~ TLC.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trotline Type",ylab="CPUE",main = "(Upper Stream) Trotline type: included 0 count")
# }
# 
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TLC"  & myData.raw[,"MA"]=="Upper" & myData.raw[,"Pal_cnt"] > 0,]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TLC.type"] <- rep("TLC1S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TLC2S"),"TLC.type"] <- "TLC2S"
# 	boxplot(CPUE  ~ TLC.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trotline Type",ylab="CPUE",main = "(Upper Stream) Trotline type: excluded 0 count")
# }
# 
# # lower stream data
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TLC" & myData.raw[,"MA"]=="Lower",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TLC.type"] <- rep("TLC1S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TLC2S"),"TLC.type"] <- "TLC2S"
# 	boxplot(CPUE  ~ TLC.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trotline Type",ylab="CPUE",main = "(Lower Stream) Trotline type: included 0 count")
# }
# 
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TLC" & myData.raw[,"MA"]=="Lower" & myData.raw[,"Pal_cnt"] > 0,]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TLC.type"] <- rep("TLC1S",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TLC2S"),"TLC.type"] <- "TLC2S"
# 	boxplot(CPUE  ~ TLC.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trotline Type",ylab="CPUE",main = "(Lower Stream) Trotline type: excluded 0 count")
# }
# 
# 
# # -------------------------------------------------------------------------------------------------
# # compare CPUA for trammel net: between TNS and TN25S
# # -------------------------------------------------------------------------------------------------
# par(mfcol = c(2,3))
# # all data
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TN",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TN.type"] <- rep("TNS",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TN25S"),"TN.type"] <- "TN25S"
# 	boxplot(CPUA  ~ TN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trammel Net Type",ylab="CPUA",main = "(All Data) Trammel Net type: included 0 count")
# }
# 
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TN" & myData.raw[,"Pal_cnt"] > 0,]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TN.type"] <- rep("TNS",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TN25S"),"TN.type"] <- "TN25S"
# 	boxplot(CPUA  ~ TN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trammel Net Type",ylab="CPUA",main = "(All Data) Trammel Net type: excluded 0 count")
# }
# 
# # upper stream data
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TN" & myData.raw[,"MA"]=="Upper",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TN.type"] <- rep("TNS",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TN25S"),"TN.type"] <- "TN25S"
# 	boxplot(CPUA  ~ TN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trammel Net Type",ylab="CPUA",main = "(Upper Stream) Trammel Net type: included 0 count")
# }
# 
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TN"  & myData.raw[,"MA"]=="Upper" & myData.raw[,"Pal_cnt"] > 0,]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TN.type"] <- rep("TNS",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TN25S"),"TN.type"] <- "TN25S"
# 	boxplot(CPUA  ~ TN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trammel Net Type",ylab="CPUA",main = "(Upper Stream) Trammel Net type: excluded 0 count")
# }
# 
# # lower stream data
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TN" & myData.raw[,"MA"]=="Lower",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TN.type"] <- rep("TNS",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TN25S"),"TN.type"] <- "TN25S"
# 	boxplot(CPUA  ~ TN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trammel Net Type",ylab="CPUA",main = "(Lower Stream) Trammel Net type: included 0 count")
# }
# 
# myData.T <- myData.raw[myData.raw[,"gear.type1"]=="TN" & myData.raw[,"MA"]=="Lower" & myData.raw[,"Pal_cnt"] > 0,]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"TN.type"] <- rep("TNS",dim(myData.T)[1])
# 	myData.T[(myData.T[,"Gear"] == "TN25S"),"TN.type"] <- "TN25S"
# 	boxplot(CPUA  ~ TN.type,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue"),xlab="Trammel Net Type",ylab="CPUA",main = "(Lower Stream) Trammel Net type: excluded 0 count")
# }
# 
# 
# 
# # -------------------------------------------------------------------------------------------------
# # compare all gear types: 
# # -------------------------------------------------------------------------------------------------
# par(mfcol = c(2,3))
# # All Data
# myData.T <- myData.raw
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"CPUE_CPUA"] <- myData.T[,"CPUE"]
# 	myData.T[myData.T[,"gear.type2"] == "active","CPUE_CPUA"] <- myData.T[myData.T[,"gear.type2"] == "active","CPUA"] 
# 	boxplot(CPUE_CPUA  ~ Gear,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","pink","brown"),xlab="Type",ylab="CPUE/CPUA",main = "(all data) all types: included 0 count")
# }
# 
# # non-zero All Data 
# myData.T <- myData.raw[myData.raw[,"Pal_cnt"] > 0,]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"CPUE_CPUA"] <- myData.T[,"CPUE"]
# 	myData.T[myData.T[,"gear.type2"] == "active","CPUE_CPUA"] <- myData.T[myData.T[,"gear.type2"] == "active","CPUA"] 
# 	boxplot(CPUE_CPUA  ~ Gear,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","pink","brown"),xlab="Type",ylab="CPUE/CPUA",main = "(all data) all types: excluded 0 count")
# }
# 
# # Upper Stream Data
# myData.T <- myData.raw[myData.raw[,"MA"]=="Upper",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"CPUE_CPUA"] <- myData.T[,"CPUE"]
# 	myData.T[myData.T[,"gear.type2"] == "active","CPUE_CPUA"] <- myData.T[myData.T[,"gear.type2"] == "active","CPUA"] 
# 	boxplot(CPUE_CPUA  ~ Gear,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","pink","brown"),xlab="Type",ylab="CPUE/CPUA",main = "(Upper Stream data) all types: included 0 count")
# }
# 
# # non-zero Upper Stream 
# myData.T <- myData.raw[myData.raw[,"Pal_cnt"] > 0 & myData.raw[,"MA"]=="Upper",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"CPUE_CPUA"] <- myData.T[,"CPUE"]
# 	myData.T[myData.T[,"gear.type2"] == "active","CPUE_CPUA"] <- myData.T[myData.T[,"gear.type2"] == "active","CPUA"] 
# 	boxplot(CPUE_CPUA  ~ Gear,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","pink","brown"),xlab="Type",ylab="CPUE/CPUA",main = "(Upper Stream data) all types: excluded 0 count")
# }
# 
# # Lower Stream
# myData.T <- myData.raw[myData.raw[,"MA"]=="Lower",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"CPUE_CPUA"] <- myData.T[,"CPUE"]
# 	myData.T[myData.T[,"gear.type2"] == "active","CPUE_CPUA"] <- myData.T[myData.T[,"gear.type2"] == "active","CPUA"] 
# 	boxplot(CPUE_CPUA  ~ Gear,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","pink","brown"),xlab="Type",ylab="CPUE/CPUA",main = "(Lower Stream data) all types: included 0 count")
# }
# 
# # non-zero Lower Stream Data
# myData.T <- myData.raw[myData.raw[,"Pal_cnt"] > 0 & myData.raw[,"MA"]=="Lower",]
# if (dim(myData.T)[1] > 0)
# {
# 	myData.T[,"CPUE_CPUA"] <- myData.T[,"CPUE"]
# 	myData.T[myData.T[,"gear.type2"] == "active","CPUE_CPUA"] <- myData.T[myData.T[,"gear.type2"] == "active","CPUA"] 
# 	boxplot(CPUE_CPUA  ~ Gear,data=myData.T,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","pink","brown"),xlab="Type",ylab="CPUE/CPUA",main = "(Lower Stream data) all types: excluded 0 count")
# }
# 
# 
# 
# ---------------------------
# CPUE/CPUA Summary
# --------------------------- 
#
# get summary of all CPUE/CPUA data
#
myData.T1 <- myData.raw
myData.T1[,"CPUE_CPUA"] <- myData.T1[,"CPUE"]
myData.T1[myData.T1[,"gear.type2"] == "active","CPUE_CPUA"] <- myData.T1[myData.T1[,"gear.type2"] == "active","CPUA"] 

mySummary.All <- data.frame(sum.no  = tapply(myData.T1[,"CPUE_CPUA"],myData.T1[,"Gear"],function(x){length(x)}),			
		   	    sum.NA  = tapply(myData.T1[,"CPUE_CPUA"],myData.T1[,"Gear"],function(x){sum(is.na(x))}),
			    sum.min = tapply(myData.T1[,"CPUE_CPUA"],myData.T1[,"Gear"],min,na.rm=TRUE),
	                    sum.med = tapply(myData.T1[,"CPUE_CPUA"],myData.T1[,"Gear"],function(x){quantile(x,0.5,na.rm=TRUE)}),
			    sum.max = tapply(myData.T1[,"CPUE_CPUA"],myData.T1[,"Gear"],max,na.rm=TRUE),
			    sum.sd  = tapply(myData.T1[,"CPUE_CPUA"],myData.T1[,"Gear"],sd,na.rm=TRUE))

#
# get summary of Upper Stream CPUE/CPUA data
#
myData.T2 <- myData.T1[myData.T1[,"MA"] == "Upper",]
mySummary.Upper <- data.frame(sum.no  = tapply(myData.T2[myData.T2[,"MA"] == "Upper","CPUE_CPUA"],myData.T2[,"Gear"],function(x){length(x)}),			
			      sum.NA  = tapply(myData.T2[myData.T2[,"MA"] == "Upper","CPUE_CPUA"],myData.T2[,"Gear"],function(x){sum(is.na(x))}),
			      sum.min = tapply(myData.T2[myData.T2[,"MA"] == "Upper","CPUE_CPUA"],myData.T2[,"Gear"],min,na.rm=TRUE),
	                      sum.med = tapply(myData.T2[myData.T2[,"MA"] == "Upper","CPUE_CPUA"],myData.T2[,"Gear"],function(x){quantile(x,0.5,na.rm=TRUE)}),
			      sum.max = tapply(myData.T2[myData.T2[,"MA"] == "Upper","CPUE_CPUA"],myData.T2[,"Gear"],max,na.rm=TRUE),
			      sum.sd  = tapply(myData.T2[myData.T2[,"MA"] == "Upper","CPUE_CPUA"],myData.T2[,"Gear"],sd,na.rm=TRUE))
			
#
# get summary of Lower Stream CPUE/CPUA data
#	
myData.T3 <-  myData.T1[myData.T1[,"MA"] == "Lower",]
mySummary.Lower <- data.frame(sum.no  = tapply(myData.T3[myData.T3[,"MA"] == "Lower","CPUE_CPUA"],myData.T3[,"Gear"],function(x){length(x)}),			
			      sum.NA  = tapply(myData.T3[myData.T3[,"MA"] == "Lower","CPUE_CPUA"],myData.T3[,"Gear"],function(x){sum(is.na(x))}),
			      sum.min = tapply(myData.T3[myData.T3[,"MA"] == "Lower","CPUE_CPUA"],myData.T3[,"Gear"],min,na.rm=TRUE),
	                      sum.med = tapply(myData.T3[myData.T3[,"MA"] == "Lower","CPUE_CPUA"],myData.T3[,"Gear"],function(x){quantile(x,0.5,na.rm=TRUE)}),
			      sum.max = tapply(myData.T3[myData.T3[,"MA"] == "Lower","CPUE_CPUA"],myData.T3[,"Gear"],max,na.rm=TRUE),
			      sum.sd  = tapply(myData.T3[myData.T3[,"MA"] == "Lower","CPUE_CPUA"],myData.T3[,"Gear"],sd,na.rm=TRUE))



myData.T4 <-  myData.T1[myData.T1[,"Pal_cnt"] > 0,]
mySummary.All.no0 <- data.frame(sum.no  = tapply(myData.T4[,"CPUE_CPUA"],myData.T4[,"Gear"],function(x){length(x)}),			
		   	        sum.NA  = tapply(myData.T4[,"CPUE_CPUA"],myData.T4[,"Gear"],function(x){sum(is.na(x))}),
			        sum.min = tapply(myData.T4[,"CPUE_CPUA"],myData.T4[,"Gear"],min,na.rm=TRUE),
	                        sum.med = tapply(myData.T4[,"CPUE_CPUA"],myData.T4[,"Gear"],function(x){quantile(x,0.5,na.rm=TRUE)}),
			        sum.max = tapply(myData.T4[,"CPUE_CPUA"],myData.T4[,"Gear"],max,na.rm=TRUE),
			        sum.sd  = tapply(myData.T4[,"CPUE_CPUA"],myData.T4[,"Gear"],sd,na.rm=TRUE))

#
# get summary of Upper Stream CPUE/CPUA data
#
myData.T5 <- myData.T1[myData.T1[,"Pal_cnt"] > 0 & myData.T1[,"MA"] == "Upper",]			
mySummary.Upper.no0 <- data.frame(sum.no  = tapply(myData.T5[myData.T5[,"MA"] == "Upper","CPUE_CPUA"],myData.T5[,"Gear"],function(x){length(x)}),			
			          sum.NA  = tapply(myData.T5[myData.T5[,"MA"] == "Upper","CPUE_CPUA"],myData.T5[,"Gear"],function(x){sum(is.na(x))}),
			          sum.min = tapply(myData.T5[myData.T5[,"MA"] == "Upper","CPUE_CPUA"],myData.T5[,"Gear"],min,na.rm=TRUE),
	                          sum.med = tapply(myData.T5[myData.T5[,"MA"] == "Upper","CPUE_CPUA"],myData.T5[,"Gear"],function(x){quantile(x,0.5,na.rm=TRUE)}),
			          sum.max = tapply(myData.T5[myData.T5[,"MA"] == "Upper","CPUE_CPUA"],myData.T5[,"Gear"],max,na.rm=TRUE),
			          sum.sd  = tapply(myData.T5[myData.T5[,"MA"] == "Upper","CPUE_CPUA"],myData.T5[,"Gear"],sd,na.rm=TRUE))
			
#
# get summary of Lower Stream CPUE/CPUA data
#	
myData.T6 <- myData.T1[myData.T1[,"Pal_cnt"] > 0 & myData.T1[,"MA"] == "Lower",]
mySummary.Lower.no0 <- data.frame(sum.no  = tapply(myData.T6[myData.T6[,"MA"] == "Lower","CPUE_CPUA"],myData.T6[,"Gear"],function(x){length(x)}),			
			          sum.NA  = tapply(myData.T6[myData.T6[,"MA"] == "Lower","CPUE_CPUA"],myData.T6[,"Gear"],function(x){sum(is.na(x))}),
			          sum.min = tapply(myData.T6[myData.T6[,"MA"] == "Lower","CPUE_CPUA"],myData.T6[,"Gear"],min,na.rm=TRUE),
	                          sum.med = tapply(myData.T6[myData.T6[,"MA"] == "Lower","CPUE_CPUA"],myData.T6[,"Gear"],function(x){quantile(x,0.5,na.rm=TRUE)}),
			          sum.max = tapply(myData.T6[myData.T6[,"MA"] == "Lower","CPUE_CPUA"],myData.T6[,"Gear"],max,na.rm=TRUE),
			          sum.sd  = tapply(myData.T6[myData.T6[,"MA"] == "Lower","CPUE_CPUA"],myData.T6[,"Gear"],sd,na.rm=TRUE))
			
#			
# write the CPUE/CPUA summary out
#
# FL_CPUE_CPUA.sum <- paste(Path.Out,"21_Chk_June21_2013Data_Gear_CPUE_CPUA.csv",sep="/")	
# if  (file.exists(FL_CPUE_CPUA.sum)){print(paste(FL_CPUE_CPUA.sum,"exist.Delete it!")); file.remove(FL_CPUE_CPUA.sum)}
# 
# cat(paste("All Data (including 0 count data),",sep=""),file=FL_CPUE_CPUA.sum,append=TRUE)
# write.table(mySummary.All,file=FL_CPUE_CPUA.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
# 
# cat(paste("\nUpstream Data (including 0 count data),",sep=""),file=FL_CPUE_CPUA.sum,append=TRUE)
# write.table(mySummary.Upper,file=FL_CPUE_CPUA.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
# 
# cat(paste("\nLower Stream Data (including 0 count data),",sep=""),file=FL_CPUE_CPUA.sum,append=TRUE)
# write.table(mySummary.Lower,file=FL_CPUE_CPUA.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
# 
# cat(paste("\nAll Data (excluding 0 count data),",sep=""),file=FL_CPUE_CPUA.sum,append=TRUE)
# write.table(mySummary.All.no0,file=FL_CPUE_CPUA.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
# 
# cat(paste("\nUpstream Data (excluding 0 count data),",sep=""),file=FL_CPUE_CPUA.sum,append=TRUE)
# write.table(mySummary.Upper.no0,file=FL_CPUE_CPUA.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
# 
# cat(paste("\nLower Stream Data (excluding 0 count data),",sep=""),file=FL_CPUE_CPUA.sum,append=TRUE)
# write.table(mySummary.Lower.no0,file=FL_CPUE_CPUA.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
# cat(paste("CPUE/CPUA summary has been outputted!\n",sep=""))
# cat(paste("CPUE/CPUA summary has been outputted!\n",sep=""),file=FL.LOG,append=TRUE)
# 
# # also output the distribution of gear type against the bend
# myGearDist <- cast(as.data.frame(table(myData.raw[,c("Gear","Bend")])),Gear~Bend,value="Freq")
# cat(paste("\nGear Distribution (all data),",sep=""),file=FL_CPUE_CPUA.sum,append=TRUE)
# write.table(myGearDist,file=FL_CPUE_CPUA.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
# 
# # River Stream Wise
# myGearDist <- cast(as.data.frame(table(myData.raw[,c("Gear","Bend","MA")])),Gear~Bend~MA,value="Freq")
# cat(paste("\nGear Distribution (Stream-Wise),",sep=""),file=FL_CPUE_CPUA.sum,append=TRUE)
# write.table(myGearDist,file=FL_CPUE_CPUA.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
# 
# # River Stream Wise-Pallid-Catch-Wise
# myGearDist <- cast(as.data.frame(table(myData.raw[,c("Gear","Bend","MA","binary")])),Gear~Bend~MA~binary,value="Freq")
# cat(paste("\nGear Distribution (Stream-Wise & Pallid Catch-Wise),",sep=""),file=FL_CPUE_CPUA.sum,append=TRUE)
# write.table(myGearDist,file=FL_CPUE_CPUA.sum,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
# 
# myData.raw.freq <- as.data.frame(table(myData.raw[,c("binary","Bend","Gear")]))
# barchart(myData.raw.freq[,"Bend"] ~ myData.raw.freq[,"Freq"] | myData.raw.freq[,"Gear"], data = myData.raw.freq,groups = myData.raw.freq[,"binary"],layout = c(9,1),stack = TRUE,auto.key = list(points = FALSE, rectangles = TRUE, space = "top"),ylab = list("Bend",cex=1),xlab=list("Count",cex=1),horizontal=TRUE,scales="free",between = list(x=0.5,y=0.5),main="Distribution of Gear Employed in Bends") 
# dev.off()
# # *********************************************************************************************************************
# # The Above Section is added to take care of the suggestion in Eric's May 21, 2013 message onthe CPUE/CPUA checking
# # *********************************************************************************************************************
# 
# # -------------------------------------------------------------------------------------------------
# # I. Check the 10 recently added variables: distance to the stocking stations
# -------------------------------------------------------------------------------------------------
# open pdf file for outputting plots
FL.stock.PDF <- paste(Path.Out,"21_Chk_June21_2013Data_stock_var.pdf",sep="/")	
if  (file.exists(FL.stock.PDF)){print(paste(FL.stock.PDF,"exist.Delete it!")); file.remove(FL.stock.PDF)}
# pdf(file = FL.stock.PDF,paper="a4r",width=0,height=0)	



var.new      <-             grep("Stk_",names(myData.raw),value=TRUE)
var.new.near <- grep("dist",grep("Stk_",names(myData.raw),value=TRUE),value=TRUE)
var.new.cumu <- grep("cnt", grep("Stk_",names(myData.raw),value=TRUE),value=TRUE)


# # 1a. histogram of near distance
# plot.near1 <- histogram(myData.raw[,"Stk_12_dist"], nint=100,col="red")
# plot.near2 <- histogram(myData.raw[,"Stk_24_dist"], nint=100,col="red")
# plot.near3 <- histogram(myData.raw[,"Stk_36_dist"], nint=100,col="red")
# plot.near4 <- histogram(myData.raw[,"Stk_60_dist"], nint=100,col="red")
# plot.near5 <- histogram(myData.raw[,"Stk_120_dist"],nint=100,col="red")
# 
# # 1b. histogram of cumu distance
# plot.cumu1 <- histogram(myData.raw[,"Stk_12_cnt"], nint=100,col="blue")
# plot.cumu2 <- histogram(myData.raw[,"Stk_24_cnt"], nint=100,col="blue")
# plot.cumu3 <- histogram(myData.raw[,"Stk_36_cnt"], nint=100,col="blue")
# plot.cumu4 <- histogram(myData.raw[,"Stk_60_cnt"], nint=100,col="blue")
# plot.cumu5 <- histogram(myData.raw[,"Stk_120_cnt"],nint=100,col="blue")
# 
# plot(plot.near1,split=c(1,1,5,2))
# plot(plot.near2,split=c(2,1,5,2),newpage=FALSE)	
# plot(plot.near3,split=c(3,1,5,2),newpage=FALSE)	
# plot(plot.near4,split=c(4,1,5,2),newpage=FALSE)	
# plot(plot.near5,split=c(5,1,5,2),newpage=FALSE)	
# 
# plot(plot.cumu1,split=c(1,2,5,2),newpage=FALSE)	
# plot(plot.cumu2,split=c(2,2,5,2),newpage=FALSE)	
# plot(plot.cumu3,split=c(3,2,5,2),newpage=FALSE)	
# plot(plot.cumu4,split=c(4,2,5,2),newpage=FALSE)	
# plot(plot.cumu5,split=c(5,2,5,2),newpage=FALSE)	
# 
#
# # 1. histogram of the distance variables
# par(mfrow = c(5,2))
# hist(myData.raw[,"Stk_12_dist"], xlab="",ylab = paste("Stk_12_dist", sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
# hist(myData.raw[,"Stk_12_cnt"],  xlab="",ylab = paste("Stk_12_cnt",  sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
#  
# hist(myData.raw[,"Stk_24_dist"], xlab="",ylab = paste("Stk_24_dist", sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
# hist(myData.raw[,"Stk_24_cnt"],  xlab="",ylab = paste("Stk_24_cnt",  sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
# 
# hist(myData.raw[,"Stk_36_dist"], xlab="",ylab = paste("Stk_36_dist", sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
# hist(myData.raw[,"Stk_36_cnt"],  xlab="",ylab = paste("Stk_36_cnt",  sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
# 
# hist(myData.raw[,"Stk_60_dist"], xlab="",ylab = paste("Stk_60_dist", sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
# hist(myData.raw[,"Stk_60_cnt"],  xlab="",ylab = paste("Stk_60_cnt",  sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
# 
# hist(myData.raw[,"Stk_120_dist"],xlab="",ylab = paste("Stk_120_dist",sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
# hist(myData.raw[,"Stk_120_cnt"], xlab="",ylab = paste("Stk_120_cnt", sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
# 
# 
# 
# 
# # 2a. scatter plot among different radiu of near distance
# par(mfrow = c(2,1))
#   plot(myData.raw[,"Stk_120_dist"],myData.raw[,"Stk_60_dist"], col="black",main="black: U50, cyan:U30, magenta: U20, green: U10");abline(a=0,b=1,col="red")
# points(myData.raw[,"Stk_120_dist"],myData.raw[,"Stk_36_dist"], col="cyan")
# points(myData.raw[,"Stk_120_dist"],myData.raw[,"Stk_24_dist"], col="magenta")
# points(myData.raw[,"Stk_120_dist"],myData.raw[,"Stk_12_dist"], col="green")
# 
# # 2b. scatter plot among different radiu of cumudistance
#   plot(myData.raw[,"Stk_120_cnt"],myData.raw[,"Stk_60_cnt"], col="black",main="black: U50, cyan:U30, magenta: U20, green: U10");abline(a=0,b=1,col="red")
# points(myData.raw[,"Stk_120_cnt"],myData.raw[,"Stk_36_cnt"], col="cyan")
# points(myData.raw[,"Stk_120_cnt"],myData.raw[,"Stk_24_cnt"], col="magenta")
# points(myData.raw[,"Stk_120_cnt"],myData.raw[,"Stk_12_cnt"], col="green")
# 
# # 3. scatter plots between near and cumu distance
# par(mfrow = c(2,3))
#   plot(myData.raw[,"Stk_12_dist"], myData.raw[,"Stk_12_cnt"], xlab = "near",ylab="cumu",main = "U10_D2")
#   plot(myData.raw[,"Stk_24_dist"], myData.raw[,"Stk_24_cnt"], xlab = "near",ylab="cumu",main = "U20_D4")
#   plot(myData.raw[,"Stk_36_dist"], myData.raw[,"Stk_36_cnt"], xlab = "near",ylab="cumu",main = "U30_D6")
#   plot(myData.raw[,"Stk_60_dist"], myData.raw[,"Stk_60_cnt"], xlab = "near",ylab="cumu",main = "U50_D10")
#   plot(myData.raw[,"Stk_120_dist"],myData.raw[,"Stk_120_cnt"],xlab = "near",ylab="cumu",main = "U100_D20")
#   
#   
# # 4. scatter plot between stocking station distance and the sturgeon catch number
# par(mfrow = c(2,3))
# boxplot(Stk_12_dist  ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="Stk_12_dist")
# boxplot(Stk_24_dist  ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="Stk_24_dist")
# boxplot(Stk_36_dist  ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="Stk_36_dist")
# boxplot(Stk_60_dist  ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="Stk_60_dist")
# boxplot(Stk_120_dist ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="Stk_120_dist")
# 
# par(mfrow = c(2,3))
# boxplot(Stk_12_cnt  ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="Stk_12_cnt")
# boxplot(Stk_24_cnt  ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="Stk_24_cnt")
# boxplot(Stk_36_cnt  ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="Stk_36_cnt")
# boxplot(Stk_60_cnt  ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="Stk_60_cnt")
# boxplot(Stk_120_cnt ~ Pal_cnt,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="Stk_120_cnt")
# 
# 
# 
# 
# # # check the binary variable of the stocking station distance against the sturgeon counting
# # # turn the NULL in the stocking variables into one category and the non-zero distance into another category
# # myData.raw[,"stocking"] <- rep("yes",dim(myData.raw)[1])
# # myData.raw[!(is.na(myData.raw[,"Stk_120_dist"])),"stocking"] <- "no"
# # # -------------------------------------------------------------------------------------------------
# 
# dev.off();
# 
# 
# 
# -------------------------------------------------------------------------------------------------
# II. Check the newly assigned micro habitat variable
# -------------------------------------------------------------------------------------------------
# open pdf file for outputting plots
# FL.microHabitat <- paste(Path.Out,"21_Chk_June21_2013Data_microHabitat.pdf",sep="/")	
# if  (file.exists(FL.microHabitat)){print(paste(FL.microHabitat,"exist.Delete it!")); file.remove(FL.microHabitat)}
# # pdf(file = FL.microHabitat,paper="a4r",width=0,height=0)	
# 
# 
# 
# 
# myData.Lower.micro <- myData.raw[myData.raw[,"MA"]=="Lower",c("macro.type","gear.type1","gear.type2","MicroClass","Pal_cnt")]
# myData.Lower.micro <- cbind(myData.Lower.micro,binary = rep("no",dim(myData.Lower.micro)[1]),stringsAsFactors=FALSE)
# myData.Lower.micro[myData.Lower.micro[,"Pal_cnt"] > 0,"binary"] <- "Yes"
# 
# # get the frequency
# table(myData.Lower.micro[,c("binary", "MicroClass")])
# table(myData.Lower.micro[,c("Pal_cnt","MicroClass")])
# 
# dev.off()
# 
# # -------------------------------------------------------------------------------------------------
# # III. Check the correlation of the three percentage to avoid closure
# # -------------------------------------------------------------------------------------------------
# FL.sandClay <- paste(Path.Out,"21_Chk_June21_2013Data_sandClay.pdf",sep="/")	
# pdf(file = FL.sandClay,paper="a4r",width=0,height=0)	
# 
# 
# if  (file.exists(FL.sandClay)){print(paste(FL.sandClay,"exist.Delete it!")); file.remove(FL.sandClay)}
# 
# plot.obj1 <- xyplot(Sand_per~Clay_per,data=myData.raw,type="p",pch=16,col="red",panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
# plot.obj2 <- xyplot(Sand_per~Clay_per | MA,data=myData.raw,type="p",pch=16,col="red",panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")},as.table=TRUE)
# 
# plot(plot.obj1)
# plot(plot.obj2)
# 
# dev.off()
# 
# 
# 
# 
# 
# # since the missing entries (-9999, NaN, NULL) in the excel spread sheet has been set to empty, we need to reset the empty entries of the factor variable to NA in order to count the missing numbers
# for (idx in seq(from=1,to=dim(myData.raw)[2]))
# {
# 	if (is.factor(myData.raw[,idx]))
# 	{
# 		myData.raw[myData.raw[,idx]=="",idx] <- NA
# 	}
# }
# cat(paste("empty entries in the factor variables have been re-assigned to [NA]\n",sep=""))



# -------------------------------------------------------------------------------------------------
# 1a. check the total data point
# -------------------------------------------------------------------------------------------------
df.data    <- NULL
number.allData <-  apply(myData.raw[,1,drop=FALSE],2,length)
number.MA      <- tapply(myData.raw[,1],list(myData.raw[,"MA"]), length)
number.Seg     <- tapply(myData.raw[,1],list(myData.raw[,"Seg"]),length)

names(number.Seg)     <- paste("Seg",names(number.Seg),sep="")
names(number.allData) <- "all"
names(number.MA)      <- sub("L","l",names(number.MA))
names(number.MA)      <- sub("U","u",names(number.MA))

df.data <- data.frame(t(c(number.allData,number.MA,number.Seg)))
row.names(df.data) <- "no.data"
cat(paste("total number of data has been counted\n",sep=""))
cat(paste("total number of data has been counted\n",sep=""),file=FL.LOG,append=TRUE)

                         
# -------------------------------------------------------------------------------------------------
# 1b. check and re-assign missing data
# -------------------------------------------------------------------------------------------------
df.missing <- NULL
number.missing  <- function(x){sum(is.na(x))}
missing.allData <- apply(myData.raw,2,number.missing)
missing.Upper   <- apply(myData.raw[myData.raw[,"MA"]=="Upper",],2,number.missing)
missing.Lower   <- apply(myData.raw[myData.raw[,"MA"]=="Lower",],2,number.missing)


df.missing <- data.frame(all = missing.allData,                         
                         lower = missing.Lower,
                         upper = missing.Upper)

# missing value in each Seg                         
for (idx.Seg in sort(unique(myData.raw[,"Seg"])))
{
	missing.data   <- apply(myData.raw[myData.raw[,"Seg"]==idx.Seg,],2,number.missing)
	command.string <- paste("df.missing <- cbind(df.missing,",paste("Seg",idx.Seg,sep="")," = missing.data)",sep="")
	eval(parse(text=command.string))
}	
cat(paste("missing data has been counted\n",sep=""))

# output the number of data/missing data
cat(paste("no. of  data,",sep=""),file=FL.missing,append=TRUE)
# write.table(df.data,file=FL.missing,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Number of data have been accounted\n",sep=""))
cat(paste("Number of data have been accounted\n",sep=""),file=FL.LOG,append=TRUE)


# output the number of data/missing data
cat(paste("no. of missing data,",sep=""),file=FL.missing,append=TRUE)
# write.table(df.missing,file=FL.missing,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Missing values have been accounted\n",sep=""))
cat(paste("Missing values have been accounted\n",sep=""),file=FL.LOG,append=TRUE)


# 
# check the missing data in the CPUA field  in term of passive and active gear type
#
myData.tmp0 <- myData.raw[,c("gear.type2","CPUA")]
missing.tmp0 <- tapply(myData.tmp0[,"CPUA"],list(myData.tmp0[,"gear.type2"]),number.missing)





# -------------------------------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------------------------------
myData <- myData.raw[,c(used.4.plot,TRUE)]
cat(paste("Only some of the variables are retained for plotting!\n",sep=""))
cat(paste("Only some of the variables are retained for plotting!\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 2a. check each individual variable: [21_Chk_June21_2013Data_indVar.pdf] and [21_Chk_June21_2013Data_num.sum]
# -------------------------------------------------------------------------------------------------
# open pdf file for outputting plots
FL.indVar.PDF <- paste(Path.Out,"21_Chk_June21_2013Data_indVar.pdf",sep="/")	
if  (file.exists(FL.indVar.PDF)){print(paste(FL.indVar.PDF,"exist.Delete it!")); file.remove(FL.indVar.PDF)}
pdf(file = FL.indVar.PDF,         paper="a4r",width=0,height=0)	


mySummary <- NULL
idx <- 1
for (var in grep("[^binary]",names(myData),value=TRUE,perl=TRUE))	# filed other than "binary" in [myData]
{
	cat("---------------------------- ",var," ----------------------------\n")
	
	# Categorical Varibles
	if (is.factor(myData[,var]) | var == "SY")
	{	
		no.NA       <- sum(is.na(myData[,var]))
		no.NA.upper <- sum(is.na(myData[myData[,"MA"] == "Upper",var]))
		no.NA.lower <- sum(is.na(myData[myData[,"MA"] == "Lower",var]))
		
		myData.tmp <- myData[!(is.na(myData[,var])),var]
		myData.tmp.upper <- myData[!(is.na(myData[,var])) & myData[,"MA"] == "Upper",var]
		myData.tmp.lower <- myData[!(is.na(myData[,var])) & myData[,"MA"] == "Lower",var]
		
		# plot.all <- barplot(table(myData.tmp),xlab=var,ylab="Frequency",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",sep=""))
		# layout(matrix(c(1,2),nrow=2,ncol=1),height = c(1,1))	
		# layout(rbind(c(1,1),c(0,2)),respect=rbind(FALSE,TRUE))		             
	             
	             
		plot.all    <- barchart(myData.tmp,      ylab=var,xlab="Count",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",sep=""))
		plot.upper  <- barchart(myData.tmp.upper,ylab=var,xlab="Count",main=paste("Distribution of ",var,"(with ",no.NA.upper," missing values)\nUpper",sep=""))
		plot.lower  <- barchart(myData.tmp.lower,ylab=var,xlab="Count",main=paste("Distribution of ",var,"(with ",no.NA.lower," missing values)\nLower",sep=""))
		
		# group the variable in term of "binary" and "MA"
		myData.freq <- as.data.frame(table(myData[,c("binary",var,"MA")]))
		
		# the entire river
		myData.TBD <- myData.freq
		command.string <- paste("Tmp1 <- cast(myData.TBD,",var,"~binary,fun=sum,margins=c(\"grand_row\",\"grand_col\"))",sep="")
		eval(parse(text=command.string))
		Tmp1 <- cbind(Tmp1,zero=Tmp1[,2]/Tmp1[,4])
		
		if (var != "MA")
		{
			# river upper stream
			myData.TBD <- myData.freq[myData.freq[,"MA"]=="Upper",]
			command.string <- paste("Tmp2 <- cast(myData.TBD,",var,"~binary,fun=sum,margins=c(\"grand_row\",\"grand_col\"))",sep="")
			eval(parse(text=command.string))
			Tmp2 <- cbind(Tmp2,zero=Tmp2[,2]/Tmp2[,4])
			
			# river lower stream
			myData.TBD <- myData.freq[myData.freq[,"MA"]=="Lower",]
			command.string <- paste("Tmp3 <- cast(myData.TBD,",var,"~binary,fun=sum,margins=c(\"grand_row\",\"grand_col\"))",sep="")
			eval(parse(text=command.string))
			Tmp3 <- cbind(Tmp3,zero=Tmp3[,2]/Tmp3[,4])
		
			# output
			cat(paste(var,",\n",sep=""),file=FL.SUM.cat,append=TRUE)
			write.table(cbind(Tmp1,Tmp2,Tmp3),file=FL.SUM.cat,sep=",",col.names=TRUE,row.names=FALSE,append=TRUE)
			cat(paste("\n\n\n",sep=""),file=FL.SUM.cat,append=TRUE)
		}else{
			# output
			cat(paste(var,",\n",sep=""),file=FL.SUM.cat,append=TRUE)
			write.table(cbind(Tmp1),file=FL.SUM.cat,sep=",",col.names=TRUE,row.names=FALSE,append=TRUE)
			cat(paste("\n\n\n",sep=""),file=FL.SUM.cat,append=TRUE)
		}


		# # output to the summary fileni
		# cat(paste("(All) ",var,",\n",sep=""),file=FL.SUM.cat,append=TRUE)
		# write.table(Tmp1,file=FL.SUM.cat,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
		# 
		# cat(paste("(Upper) ",var,",\n",sep=""),file=FL.SUM.cat,append=TRUE)
		# write.table(Tmp2,file=FL.SUM.cat,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
		# 
		# cat(paste("(Lower) ",var,",\n",sep=""),file=FL.SUM.cat,append=TRUE)
		# write.table(Tmp3,file=FL.SUM.cat,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)		
		
		plot.binary <- barchart(myData.freq[,var] ~ myData.freq[,"Freq"] | myData.freq[,"MA"], data = myData.freq,groups = myData.freq[,"binary"],layout = c(2,1),stack = TRUE,auto.key = list(points = FALSE, rectangles = TRUE, space = "top"),ylab = var,horizontal=TRUE,scales="free",between = list(x=2.5,y=0.5)) 
		
		# Defaults to FALSE if x is a factor or shingle, TRUE otherwise.		
		plot(plot.all,   split=c(1,1,1,3))
		plot(plot.lower, split=c(1,2,2,3),newpage=FALSE)
		plot(plot.upper, split=c(2,2,2,3),newpage=FALSE)
		plot(plot.binary,split=c(1,3,1,3),newpage=FALSE)
		
		# output to the summary fileni
		# cat(paste(var,",\n",sep=""),file=FL.SUM.cat,append=TRUE)
		# write.table(as.data.frame(table(myData[,var])),file=FL.SUM.cat,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	
	# continuous variables		
	}
	
	# make a table for missing data
	idx <- idx + 1
}
cat(paste("each retained individual variable has been checked!\n",sep=""))
cat(paste("each retained individual variable has been checked!\n",sep=""),file=FL.LOG,append=TRUE)





# #
# # to have consistent format for the summary (i.e., have Pallid Count 0 to 11 as the column header,
# # 1.
#       summary.count  <- aggregate(myData[,"Pal_cnt"],list(myData[,"Pal_cnt"]),length)
# names(summary.count) <- c("Count","value")
# cat(paste("Count distribution\n"),file=FL.Summary,append=TRUE)
# write.table(t(summary.count),file=FL.Summary,sep=",",row.names=TRUE,col.names=FALSE,append=TRUE)	# write [sum.GearType2.Macro.Count] out
# 
# 
# # 2. 
#       summary.year.count  <- aggregate(myData[,"Pal_cnt"],list(myData[,"SY"],myData[,"Pal_cnt"]),length)
# names(summary.year.count) <- c("Year","Count","value")
# tmp <- cast(summary.year.count,Year~Count,fun=sum,margins=c("grand_row","grand_col"))
# write.table(as.data.frame(tmp),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
# 
# 3. 
#       summary.year.gearType1.count  <- aggregate(myData[,"Pal_cnt"],list(myData[,"SY"],myData[,"gear.type1"],myData[,"Pal_cnt"]),length)
# names(summary.year.gearType1.count) <- c("Year","gear.type1","Count","value")
# tmp <- cast(summary.year.gearType1.count,Year~Count | gear.type1,fun=sum,rm.na=FALSE)
# write.table(as.data.frame(tmp),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
# 
# no.rows <- length(unique(myData[,"SY"]))
# no.cols <- length(unique(myData[,"Pal_cnt"]))
#  data.default <- data.frame(matrix(rep(0,no.rows * no.cols),nrow=no.rows))
#      names(data.default) <- sort(unique(myData[,"Pal_cnt"]))
#  row.names(data.default) <- sort(unique(myData[,"SY"]))
#  data.default <- cbind(Year = sort(unique(myData[,"SY"])),data.default)
 
 
 
 


# get summary of the data



# 1   objectID					should ignore.  not sure what it is.  It is unique.
# 2   SY					should keep.	Sample year (2006-2011)
# 3   FieldOffice				should ignore.  One of the 7 field office (CF, MO, NE, SD, GP, MR, MT)
# 4   Project					should ignore.  always take 1.  one of the four (01: PSPAP, 02: Habitat Assessment, 03: Chute Study-Mitigation), 04: Spring Rise Evaluation)
# 5   UniqueIdentifier				should ignore.  Four-digits (1-9999) (Required). Restarts to 1 at the beginning of every sampling year. NOTE: it is very critical that the UID’s are never the same for each Seg.
# 6   Gear					should keep.	five digit code of gear type (9 values: GN14S, GN18S, GN41S, GN48S, OT16S, TLC1S, TLC2S, TNS, TN25S) where "S" stands for "standard", "W" for "Wild Gear" and "E" for "Evaluation".  Should turn into 4 gear types, GN, OT, TLC, TN)
# 7   Season					should keep.	either ST: Sturgeon or FC: Fish Community seasons
# 8   Bend					should keep.	bend index from 1 to 87
# 9   BendRN					should ignore.	R: random selected bend and N: non-randomly selected bend.  Here we only retain the randomly selected bend.
# 10  BendRiverMile				Is it useful?	Record the upper river mile identifying the bend being sampled in the Missouri River or the river mile of the tributary being sampled. All tributaries being sampled have Seg numbers.
# 11  Near_NHD2rm_FID				should ignore.  the near GIS feature to the plotted PAP catches
# 12  Near_NHD2RM_dist_m			should ignore.  the near GIS feature distance to the plotted PAP catches
# 13  NHD2_RM					should ignore.  NHD2_RM is the new river mile delineation that was created from the NHDPlus data to replace/fix the old 1960 Missouri River Miles data
# 14  1960BendID		399   -9999	GIS fields.	
# 15  RM1960_RM					GIS fields.
# 16  UpDamDist					GIS fields.
# 17  UpDamNm					GIS fields.
# 18  DnDamDist			23542 -9999	GIS fields.
# 19  DnDamNm			23542 NaN.	GIS fields.
# 20  Ch_W_Full					GIS fields.
# 21  Ch_W_NoIsl				GIS fields.
# 22  Braiding					GIS fields.
# 23  UpTrib			38    NaN.	GIS fields.
# 24  UpTribDist		38    -9999.	GIS fields.
# 25  DnTrib			104   NaN	GIS fields.
# 26  DTribDist			104   -9999	GIS fields.
# 27  Reach					GIS fields.
# 28  Mean_z					GIS fields.
# 29  Max_z					GIS fields.
# 30  Min_z					GIS fields.
# 31  Grade10RM					GIS fields.
# 32  GradeBend					GIS fields.
# 33  Rocktype1					GIS fields.
# 34  Rocktype2					GIS fields.
# 35  Lithology					GIS fields.
# 36  NFHAP_Scr					GIS fields.
# 37  MedFlow					GIS fields.
# 38  MeanFlow					GIS fields.
# 39  taxorder					GIS fields.
# 40  taxpartsiz				GIS fields.
# 41  comppct_r					GIS fields.
# 42  sandtotal_r				GIS fields.
# 43  claytotal_r				GIS fields.
# 44  frag3to10_r				GIS fields.
# 45  VF_width			12    -9999.	GIS fields.
# 46  Segment					segment 2-14.  See page 172 of PAP_Database_SOP_V15_June2010.pdf for name of the segment
# 47  nPage					should ignore.  which page of the total pages for a particular gear deployment.  Is it the page number in the log book?
# 48  TotalPages				should ignore.  the total number of pages used for a particular gear deployment.  Is it the page number in the log book?
# 49  SetDate					should ignore.  not sure what it is.  Always has a "00:00.0" value. 
# 50  Replicate			35033 NULL.	should ignore.  not sure what it is.  Always has a "NULL" value. 
# 51  ReplicateRN		35033 NULL.	should ignore.  not sure what it is.  Always has a "NULL" value.  
# 52  SubSample					should ignore.  we have value from 1 to 20:	Each gear deployment is a sub-sample. Sub-sample numbering will be consistent with the design of the Project 1 where gear deployment is guided by the habitats available within the bend. Sub-samples will be numbered 1-X for each gear by Macrohabitat/Mesohabitat combination. For example, if you were sampling in bend and made 5 trammel net drifts in the ISB and 3 in the CHXO, you would number your sub-samples as 1 through 5 for those drifts in the ISB and 1-3 for those drifted in the CHXO to achieve the minimum of 8 sub-samples for that gear.
# 53  SubSamplePass				should ignore.  We always have 1 which mean the first sub-sample in an area.  Did we deliberately delete the additional samples whenever a pallid sturgeon was caught because whenever a pallid sturgeon is collected with an active gear, it is a requirement to do two additional samples in this exact location even if the minimum distance for the gear is not achieved?????
# 54  SubSampleN		35033 NULL.	should ignore.  always "NULL".  This might be because all additional pas need to be recorded as "N": non-random and they are not included in this data compilation.
# 55  Biologist			35033 NULL.	should ignore.  not sure what it is. Always "NULL"
# 56  Recorder					should ignore.  The initials of the individual that recorded the data. The first initial of the individual’s first, middle, and last name (If no middle name, use "X" for middle initial.).
# 57  CheckBy			4     NULL.	should ignore.  The initials of the individual that checks the data sheet prior to submitting this for data entry. This must be somebody other than the recorder! The first initial of the individual’s first, middle, and last name (If no middle name, use “X” for middle initial.). The individual that is checking the data sheets should clarify in the comments section all situations where the protocols were not followed. For example, for a large collection of shovelnose sturgeon that weight data was not collected, this should be explained in the comments section referencing the unique ID and the circumstances (shovelnose sturgeon weight data was not collected…..too many fish, scale malfunction, etc.).
# 58  Temp			66    NULL.	Three digit temperature. The recorder is required to record to the nearest degree in Celsius, but has the option of record up to a single decimal place. Temperature is recorded on the day the gear is set.
# 59  Turbidity			21458 NULL.	Four digits Nephelometric Turbidity (NTU).  Record to the nearest Nephelometric Turbidity Unit (NTU). Recorded following gear retrieval.
# 60  Conductivity		35029 NULL.	Four-digits Conductivity in "microhos/cm3).  Record to the nearest microhos/cm3 . Recorded following gear retrieval.
# 61  DO			35022 NULL.	Dissolved Oxygen in mg/L: Three-digits (Not required for Project 1).  Record to one decimal place (units are parts per million (ppm) and milligrams/liter (mg/L). Recorded following gear retrieval.
# 62  Distance			6428  NULL.	Length of sample for trawling, drifting trammel, seining).  0-661 with "NULL".  Three-digits (Required for trawling, drifting trammel nets, seining when using the rectangular method and mini-fyke netting).  Record in meters, indicating length of sample. Mini-fyke netting distance is measured perpendicular to the bank line to the midline of the first cab. This measurement will be recorded in centimeters for mini-fyke nets. When retrieving the trotline, the number of hooks that fished is recorded in the DISTANCE box on the front of the data sheet.
# 63  Width			35031 NULL.	The width of the seine hauls.  Two values: 7.5 and 15.9 with "NULL".  Three-digits (Only required for Seining).  Record width in tenths of a meter for the width of the seine hauls (not the length of the seine).
# 64  NetRiverMile		12948 NULL.	0.1 to 1759.4 miles.  Record the river mile location identifying where the sample was collected in the mainstem Missouri or the river mile of the tributary if sampling a tributary. Should be recorded to the nearest .1 of a mile based on the quality of the maps available.
# 65  StructureNumber		25559 NULL.	9 digits code.  Character string with "NULL". Record the Corps dikes or other structures number (based on 1890 river mileage). An extra box was added after the decimal point for the number structures involving additional letters or numbers.
# 66  USGS			24714 NULL.	Eight digit code with "NULL".  USGS Gauge Station Code - The nearest USGS gauge to the sampling location should be used. If sampling near a tributary, use the nearest gauging station that represents the discharge at the location you are sampling. For example, if sampling a bend above a major trib, there's a possibility that the "nearest" gauge will have tributary influence thus a higher discharge and stage. The gauge should be read in conjunction with the time that the net was set for passive gears
# 67  RiverStage		28089 NULL.	three digits. 2.2 to 19.4 with "NULL".  Daily gauge height in feet
# 68  Discharge			25347 NULL.	six digit.  daily discharge in cubic feet per second (CFS)
# 69  U1			34477 NULL.	Utility box 1: was being used in conjunction with the PIT Tagging/Scute Marking effort in the Fort Peck Reach.
# 70  U2			31748 NULL.	Utility box 2: initial hook number for trotline
# 71  U3			34742 NULL.	Utility box 3: Push Trawl/Minifyke proximity evaluation
# 72  U4			34535 NULL.	Utility box 4: was being used in conjunction with the PIT Tagging/Scute Marking effort in the Fort Peck Reach.
# 73  U5			34573 NULL.	Utility box 5: was being used in conjunction with the PIT Tagging/Scute Marking effort in the Fort Peck Reach.
# 74  U6			35017 NULL.	Utility box 6: If a net does not fish properly, but catches fish, MNCF will be recorded in this utility box and the actual fish species codes will be recorded on the back of the standard data sheet.
# 75  U7			34767 NULL.	Utility box 7: For targeted pallid sturgeon broodstock collection efforts, “BS” (Broodstock) will be recorded for all gear deployments
# 76  Macro					four digits code: 14 options
# 77  MacroSW			35033 NULL.	should ignore.  NULL
# 78  Meso					four digits code: 6 options
# 79  MesoSW			35033 NULL.	should ignore.  NULL
# 80  Micro			16232 NULL.	six digits code: (required for segment 8-14). 
# 81  StartTime			600   NULL.	should ignore.  Four-digits (Required)
# 82  StopTime			17764 NULL.	should ignore.  Four-digits (Required for passive gears only)
# 83  DecimalTimeDifference	278   NULL.	not sure what it is.
# 84  StartLatitude				should ignore.  
# 85  StopLatitude		8966  NULL.	should ignore.  
# 86  StartLongitude				should ignore.  
# 87  StopLongitude		8970  NULL.	should ignore.  
# 88  Depth1			28    NULL.	Why we have three depth variables
# 89  Depth2			41    NULL.	Why we have three depth variables
# 90  Depth3			62    NULL.	Why we have three depth variables
# 91  Velocity02or06_1		35033 NULL.	Why we have 9 velocityh variables
# 92  Velocity02or06_2		23245 NULL.	Why we have 9 velocityh variables
# 93  Velocity02or06_3		35031 NULL.	Why we have 9 velocityh variables
# 94  Velocity08_1		35033 NULL.	Why we have 9 velocityh variables
# 95  Velocity08_2		23572 NULL.	Why we have 9 velocityh variables
# 96  Velocity08_3		35033 NULL.	Why we have 9 velocityh variables
# 97  VelocityBot1		35032 NULL.	Why we have 9 velocityh variables
# 98  VelocityBot2		23269 NULL.	Why we have 9 velocityh variables
# 99  VelocityBot3		35032 NULL.	Why we have 9 velocityh variables
# 100 WaterVel			3755  NULL.	Visual estimation of water velocity.  Single digit code 9from 0 to 5 ([0]: no reliable visual est, [1]: Eddy, [2]: 0.0-0.3 m/s, [3]: 0.3-0.6 m/s, [4]: 0.6-0.9m/s, [5]: => 0.9 m/s).
# 101 HabitatRN			21343 NULL.	Habitat R: random and N: non-random.
# 102 Cobble			31690 NULL.	Substrate: [0]: absent, [1]: incidental, [2]: dominant, [3]: ubiquitous
# 103 silt			33195 NULL.	Substrate: sum of [silt], [sand] and [gravel} equal to 100%.
# 104 Sand			31645 NULL.	Substrate: sum of [silt], [sand] and [gravel} equal to 100%.
# 105 Gravel			32842 NULL.	Substrate: sum of [silt], [sand] and [gravel} equal to 100%.
# 106 Organic			32489 NULL.	Substrate: [0]: absent, [1]: incidental, [2]: dominant, [3]: ubiquitous
# 107 QC			35020 NULL.	X or NULL
# 108 Comments			29412 NULL.	should ignore.  
# 109 MappingBox		35033 NULL.	should ignore.  Always "NULL".
# 110 Total_Fish_Count				0 to 1843
# 111 Pallid_Only_Count				0 to 11
# 112 CPUE			56   NULL, 8 #DIV/0!. 	see formula
# 113 CPU_Area			8985 NULL, 1 #DIV/0!.	see formula
# 114 Alt_Pallid_Only_Count			should ignore.  alternative way: use 0.01 to represent count of 0
# 115 Alt_CPUE			56   NULL, 8 #DIV/0!. 	should ignore.  alternative way: use 0.01 to represent count of 0
# 116 Alt_CPU_Area		8985 NULL, 1 #DIV/0!.	should ignore.  alternative way: use 0.01 to represent count of 0	


# An example of "recast" which is a combined steps of "melt" and "cast"

# original data frame is "french_fries"
# str(french_fries)
# ----------------------------------------------------------------------------
# 'data.frame':   696 obs. of  9 variables:
#  $ time     : Factor w/ 10 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ treatment: Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
#  $ subject  : Factor w/ 12 levels "3","10","15",..: 1 1 2 2 3 3 4 4 5 5 ...
#  $ rep      : num  1 2 1 2 1 2 1 2 1 2 ...
#  $ potato   : num  2.9 14 11 9.9 1.2 8.8 9 8.2 7 13 ...
#  $ buttery  : num  0 0 6.4 5.9 0.1 3 2.6 4.4 3.2 0 ...
#  $ grassy   : num  0 0 0 2.9 0 3.6 0.4 0.3 0 3.1 ...
#  $ rancid   : num  0 1.1 0 2.2 1.1 1.5 0.1 1.4 4.9 4.3 ...
#  $ painty   : num  5.5 0 0 0 5.1 2.3 0.2 4 3.2 10.3 ...
# ----------------------------------------------------------------------------
# where "time", "treatment", "subject", "rep" are id vars
#       "potato","buttery", "grassy", "rancid", and "painty" are measure vars
# 
# First: "melt", need to specify id.vars and measure.vars.  By default, all factor will be treated as id.vars and all non-id vars will be treated as measure vars.
# Usage:  melt.data.frame(data, id.vars, measure.vars, variable_name = "variable", na.rm = !preserve.na, preserve.na = TRUE, ...)
#
# A <- melt(french_fries,id.var = c("time", "treatment", "subject", "rep"),variable_name="measurement")
# str(A)
# ----------------------------------------------------------------------------
# 'data.frame':   3480 obs. of  6 variables:
#  $ time        : Factor w/ 10 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ treatment   : Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
#  $ subject     : Factor w/ 12 levels "3","10","15",..: 1 1 2 2 3 3 4 4 5 5 ...
#  $ rep         : num  1 2 1 2 1 2 1 2 1 2 ...
#  $ measurement : Factor w/ 5 levels "potato","buttery",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ value       : num  2.9 14 11 9.9 1.2 8.8 9 8.2 7 13 ...
# ----------------------------------------------------------------------------
#
# Then: "cast":  Cast a molten data frame into the reshaped or aggregated form you want
# Usage: cast(data, formula = ... ~ variable, fun.aggregate=NULL, ..., margins=FALSE, subset=TRUE, df=FALSE, fill=NULL, add.missing=FALSE, value = guess_value(data))
#  The cast formula has the following format: x_variable + x_2 ~ y_variable + y_2 ~ z_variable ~ ... 
#  The order of the variables makes a difference. The first varies slowest, and the last fastest. 
#  There are a couple of special variables: "..." represents all other variables not used in the formula and "." represents no variable, so you can do formula = var1 ~ ..
#
#
#
# B <- cast(A,time~measurement,fun=length)
# str(B)
# ----------------------------------------------------------------------------
# List of 6
#  $ time   : Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10
#  $ potato : int [1:10] 72 72 72 72 72 72 72 72 60 60
#  $ buttery: int [1:10] 72 72 72 72 72 72 72 72 60 60
#  $ grassy : int [1:10] 72 72 72 72 72 72 72 72 60 60
#  $ rancid : int [1:10] 72 72 72 72 72 72 72 72 60 60
#  $ painty : int [1:10] 72 72 72 72 72 72 72 72 60 60
#  - attr(*, "row.names")= int [1:10] 1 2 3 4 5 6 7 8 9 10
#  - attr(*, "idvars")= chr "time"
#  - attr(*, "rdimnames")=List of 2
#   ..$ :'data.frame':    10 obs. of  1 variable:
#   .. ..$ time: Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10
#   ..$ :'data.frame':    5 obs. of  1 variable:
#   .. ..$ measurement: Factor w/ 5 levels "potato","buttery",..: 1 2 3 4 5
# ----------------------------------------------------------------------------
#  B
#    time potato buttery grassy rancid painty
# 1     1     72      72     72     72     72
# 2     2     72      72     72     72     72
# 3     3     72      72     72     72     72
# 4     4     72      72     72     72     72
# 5     5     72      72     72     72     72
# 6     6     72      72     72     72     72
# 7     7     72      72     72     72     72
# 8     8     72      72     72     72     72
# 9     9     60      60     60     60     60
# 10   10     60      60     60     60     60
# ----------------------------------------------------------------------------
#
# To use "recast", we achieve the same thing in one step
#  C <- recast(french_fries,time~variable,fun=length,id.var = c("time", "treatment", "subject", "rep"))
#  C
#    time potato buttery grassy rancid painty
# 1     1     72      72     72     72     72
# 2     2     72      72     72     72     72
# 3     3     72      72     72     72     72
# 4     4     72      72     72     72     72
# 5     5     72      72     72     72     72
# 6     6     72      72     72     72     72
# 7     7     72      72     72     72     72
# 8     8     72      72     72     72     72
# 9     9     60      60     60     60     60
# 10   10     60      60     60     60     60
# ----------------------------------------------------------------------------