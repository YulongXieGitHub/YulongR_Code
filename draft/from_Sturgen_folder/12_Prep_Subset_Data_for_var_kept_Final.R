#
# 12_Prep_Subset_Data_for_var_kept_Final.R
#
# October 2, 2013
#
# This is a revision of "12_Prep_Subset_Data_for_var_kept.R" which is then based on "8_Prep_Subset_Data.R".
#
# Removethe scatter plot for numeric variable.
#
# 
# purpose: to split the data into different subsets for analysis.  The categories of the lithology and particle size variables vary from subset to subset.
#
# plot(plot.all,   split=c(1,1,2,2))
#                        c(idx of column, idx of row, number of column, number of rows)
# -------------------------------------------------------------------------------------------------
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
Path.Data.IN <- "C:/YuLong_Projects/FY2012_MORiver/_data_received/sturg_spatial/data/Habitat_Metrics_Output"
Path.log     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/12_Prep_Subset_Data_for_var_kept_Final"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}

# data file
FL.Data.IN   <- paste(Path.Data.IN,"MoRiver_Variables_2013May31_revisedOn_2013June21_YLX.csv",sep="/")
FL.LOG       <- paste(Path.log,"12_Prep_Subset_Data_for_var_kept_Final.log",                sep="/")	
FL.Data.OBJ  <- paste(Path.Out,"12_Prep_Subset_Data_for_var_kept_Final.Rdata",              sep="/")

if (!file.exists(FL.Data.IN))  {stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.LOG))      {print(paste(FL.LOG,      "exist.Delete it!")); file.remove(FL.LOG)}
if  (file.exists(FL.Data.OBJ)) {print(paste(FL.Data.OBJ, "exist.Delete it!")); file.remove(FL.Data.OBJ)}


library("lattice")
library("reshape")



# read April 24, 2013 data 
row.titles  <- c("ID",     "SY",     "FieldOffice","Project","UniqueID","Gear",  "Season","Bend",   "BendRN", "BendRiverMile","Near_NHD2rm_FID","Near_NHD2RM_dist_m","NHD2_RM","X1960BendID","RM1960_RM","UpDamDist","UpDamNm","DnDamDist","DnDamNm",                                 "Ch_W_Full","Ch_W_NoIsl","Braiding","UpTrib",   "UpTribDist","DnTrib",   "DTribDist",                            "Reach",  "Mean_z", "Max_z",  "Min_z",  "Grade10RM","GradeBend","Rocktype1","Rocktype2","Lithology","NFHAP_Scr","MedFlow",  "MeanFlow",  "taxorder","taxpartsiz", "comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Segment","nPage",  "TotalPages","SetDate",  "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turbidity","Conductivity","DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "DecimalTimeDifference","StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3",              "Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","VelocityBot2","VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Total_Fish_Count","Pallid_Only_Count","CPUE",   "CPU_Area","Alt_Pallid_Only_Count","Alt_CPUE","Alt_CPU_Area","S_U10_D2_Near","S_U10_D2_Cumu","S_U20_D4_Near","S_U20_D4_Cumu","S_U30_D6_Near","S_U30_D6_Cumu","S_U50_D10_Near","S_U50_D10_Cumu","S_U100_D20_Near","S_U100_D20_Cumu",                            "gear.type1","gear.type2","macro.type")
colClasses  <- c("integer","factor", "factor",     "integer","integer", "factor","factor","integer","factor", "numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",  "numeric",  "factor", "numeric",  "factor",                                  "numeric",  "numeric",   "integer", "factor",   "numeric",   "factor",   "numeric",                              "factor", "numeric","numeric","numeric","numeric",  "numeric",  "factor",   "factor",   "factor",   "factor",   "numeric",  "numeric",   "factor",  "factor",     "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "factor", "integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric",             "numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",                                    "factor",    "factor",    "factor")
used.4.anal <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     FALSE,   TRUE,    FALSE,    FALSE,    FALSE,          FALSE,            FALSE,               FALSE,    FALSE,        FALSE,      TRUE,       FALSE,    FALSE,      FALSE,                                     TRUE,       FALSE,       TRUE,      FALSE,      TRUE,        FALSE,      TRUE,                                   TRUE,     FALSE,    FALSE,    FALSE,    TRUE,       FALSE,      TRUE,       FALSE,      FALSE,      TRUE,       FALSE,      FALSE,       FALSE,     TRUE,         TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     TRUE,       FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    FALSE,   FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,                 FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,                                         TRUE,        TRUE,        TRUE)

row.titles  <- c("Dep_ID", "SY",     "FO",         "Proj",   "Un_ID",   "Gear",  "Season","Bend",   "Bend_RN","Bend_RM",      "Near_NHD2rm_FID","Near_NHD2RM_dist_m","GIS_RM", "Bend_ID_60", "Bend_RM_60","D_dist_up","D_name_up","D_dist_dn","D_name_dn","D_name_near","D_dist_near","Chan_wid", "Wet_wid",   "Braid"   ,"T_name_up","T_dist_up", "T_name_dn","T_dist_dn","T_name_near","T_dist_near","MA",     "Mean_z", "Max_z",  "Min_z",  "Grade_10_RM",          "Lith_1",   "Lith_2",   "Lith_desc","NFHAP",    "Disch_med","Disch_mean","Tax_ord", "Tax_part_sz","Comp_per", "Sand_per",   "Clay_per",   "Frag_per",   "VF_wid",  "Seg",    "nPage",  "TotalPages","Set_date", "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turb",     "Cond",        "DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "Set_dur",              "StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3", "Depth_Mean","Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","Btm_vel",     "VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Fish_cnt",        "Pal_cnt",          "CPUE",   "CPUA",    "Alt_Pal_Cnt",          "CPUE_2",  "CPUA_2",      "Stk_12_dist",  "Stk_12_cnt",   "Stk_24_dist",  "Stk_24_cnt",   "Stk_36_dist",  "Stk_36_cnt",   "Stk_60_dist",   "Stk_60_cnt",    "Stk_120_dist",   "Stk_120_cnt",    "Rel_dsch_AY","Rel_dsch_WY","gear.type1","gear.type2","macro.type")
colClasses  <- c("integer","factor", "factor",     "integer","integer", "factor","factor","integer","factor" ,"numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",   "numeric",  "factor",   "numeric",  "factor",   "factor",     "numeric",    "numeric",  "numeric",   "integer", "factor",   "numeric",   "factor",   "numeric",  "factor"     ,"numeric",    "factor", "numeric","numeric","numeric","numeric",              "factor",   "factor",   "factor",   "factor",   "numeric",  "numeric",   "factor",  "factor",     "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "factor", "integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric","numeric",   "numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",        "numeric",    "numeric",    "factor",    "factor",    "factor")
used.4.anal <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     TRUE,    TRUE,    FALSE,    FALSE,    FALSE,          FALSE,            FALSE,               TRUE,     FALSE,        FALSE,       TRUE,       FALSE,      TRUE,       FALSE,      FALSE,        TRUE,         TRUE,       TRUE,        TRUE,      FALSE,      TRUE,        FALSE,      TRUE,       FALSE,        TRUE,         TRUE,     FALSE,    FALSE,    FALSE,    TRUE,                   TRUE,       TRUE,       TRUE,       TRUE,       FALSE,      FALSE,       FALSE,     TRUE,         TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     TRUE,       FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,   FALSE,    TRUE,    FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,    TRUE,        FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,         TRUE,         TRUE,        TRUE,        TRUE)


# the raw data including all variables
      myData.raw  <- read.csv(file=FL.Data.IN,sep=",",header=TRUE,stringsAsFactors=TRUE)
names(myData.raw) <- row.titles

# ******************************************************************
# Data set manipulations
# ******************************************************************
# remove data from segment 5 and 6
myData.raw <- myData.raw[(myData.raw[,"Seg"] != 5) & (myData.raw[,"Seg"] != 6),]

# ------------------------------------------------------------------------------------------------
# NOTE: the two variables [colClasses] and [used.4.anal] have to go with [myData.raw] together.
# **** added a [binary] field ****
# ------------------------------------------------------------------------------------------------
myData.raw <- cbind(myData.raw,binary = rep("no",dim(myData.raw)[1]),stringsAsFactors=FALSE)
myData.raw[myData.raw[,"Pal_cnt"] > 0,"binary"] <- "yes"

# updated due to the addition of "binary"
row.titles  <- c("Dep_ID", "SY",     "FO",         "Proj",   "Un_ID",   "Gear",  "Season","Bend",   "Bend_RN","Bend_RM",      "Near_NHD2rm_FID","Near_NHD2RM_dist_m","GIS_RM", "Bend_ID_60", "Bend_RM_60","D_dist_up","D_name_up","D_dist_dn","D_name_dn","D_name_near","D_dist_near","Chan_wid", "Wet_wid",   "Braid"   ,"T_name_up","T_dist_up", "T_name_dn","T_dist_dn","T_name_near","T_dist_near","MA",     "Mean_z", "Max_z",  "Min_z",  "Grade_10_RM",          "Lith_1",   "Lith_2",   "Lith_desc","NFHAP",    "Disch_med","Disch_mean","Tax_ord", "Tax_part_sz","Comp_per", "Sand_per",   "Clay_per",   "Frag_per",   "VF_wid",  "Seg",    "nPage",  "TotalPages","Set_date", "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turb",     "Cond",        "DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "Set_dur",              "StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3", "Depth_Mean","Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","Btm_vel",     "VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Fish_cnt",        "Pal_cnt",          "CPUE",   "CPUA",    "Alt_Pal_Cnt",          "CPUE_2",  "CPUA_2",      "Stk_12_dist",  "Stk_12_cnt",   "Stk_24_dist",  "Stk_24_cnt",   "Stk_36_dist",  "Stk_36_cnt",   "Stk_60_dist",   "Stk_60_cnt",    "Stk_120_dist",   "Stk_120_cnt",    "Rel_dsch_AY","Rel_dsch_WY","gear.type1","gear.type2","macro.type","binary")
colClasses  <- c("integer","factor", "factor",     "integer","integer", "factor","factor","integer","factor" ,"numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",   "numeric",  "factor",   "numeric",  "factor",   "factor",     "numeric",    "numeric",  "numeric",   "integer", "factor",   "numeric",   "factor",   "numeric",  "factor"     ,"numeric",    "factor", "numeric","numeric","numeric","numeric",              "factor",   "factor",   "factor",   "factor",   "numeric",  "numeric",   "factor",  "factor",     "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "factor", "integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric","numeric",   "numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",        "numeric",    "numeric",    "factor",    "factor",    "factor",    "factor")
used.4.anal <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     TRUE,    TRUE,    FALSE,    FALSE,    FALSE,          FALSE,            FALSE,               TRUE,     FALSE,        FALSE,       TRUE,       FALSE,      TRUE,       FALSE,      FALSE,        TRUE,         TRUE,       TRUE,        TRUE,      FALSE,      TRUE,        FALSE,      TRUE,       FALSE,        TRUE,         TRUE,     FALSE,    FALSE,    FALSE,    TRUE,                   TRUE,       TRUE,       TRUE,       TRUE,       FALSE,      FALSE,       FALSE,     TRUE,         TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     TRUE,       FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,   FALSE,    TRUE,    FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,    TRUE,        FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,         TRUE,         TRUE,        TRUE,        TRUE,        TRUE)

# **** it is realized that the CPUE field contains values for active gear net like OT and TN, re-set them to NA.  (see the formula used in Chris Vernon's spread sheet). *** 
myData.raw[myData.raw[,"Gear"] == "OT16S" | myData.raw[,"Gear"] == "TNS" | myData.raw[,"Gear"] == "TN25S",c("CPUE","CPUE_2")] <- NA

# ------------------------------------------------------------------------------------------------
# NOTE: the two variables [colClasses] and [used.4.anal] have to go with [myData.raw] together.
# ***** added 3 variables: calculate CPUE/CPUA as well as the Area and Unit for CPUE and CPUA calculation
# ------------------------------------------------------------------------------------------------
myData.raw[,"SUPPORT.calc"] <- rep(NA,dim(myData.raw)[1])
myData.raw[,"CPUE.calc"]    <- rep(NA,dim(myData.raw)[1])
myData.raw[,"CPUA.calc"]    <- rep(NA,dim(myData.raw)[1])

# CPUE
index.GN14S_41S <- myData.raw["Gear"] == "GN14S" | myData.raw["Gear"] == "GN41S"
myData.raw[index.GN14S_41S,"SUPPORT.calc"] <- myData.raw[index.GN14S_41S,"Set_dur"] / 24
myData.raw[index.GN14S_41S,"CPUE.calc"]    <- myData.raw[index.GN14S_41S,"Pal_cnt"] / myData.raw[index.GN14S_41S,"SUPPORT.calc"]

index.GN18S_81S <- myData.raw["Gear"] == "GN18S" | myData.raw["Gear"] == "GN81S"
myData.raw[index.GN18S_81S,"SUPPORT.calc"] <- myData.raw[index.GN18S_81S,"Set_dur"] / 12
myData.raw[index.GN18S_81S,"CPUE.calc"]    <- myData.raw[index.GN18S_81S,"Pal_cnt"] / myData.raw[index.GN18S_81S,"SUPPORT.calc"]

index.TLC <- myData.raw["Gear"] == "TLC1S" | myData.raw["Gear"] == "TLC2S"
myData.raw[index.TLC,"SUPPORT.calc"] <- (myData.raw[index.TLC,"Distance"] * myData.raw[index.TLC,"Set_dur"]) / (20 * 24)
myData.raw[index.TLC,"CPUE.calc"]    <-  myData.raw[index.TLC,"Pal_cnt"] / myData.raw[index.TLC,"SUPPORT.calc"] 

# CPUA
index.OT <- myData.raw["Gear"] == "OT16S"
myData.raw[index.OT,"SUPPORT.calc"] <- (16*0.3048 * myData.raw[index.OT,"Distance"]) / 100
myData.raw[index.OT,"CPUA.calc"]    <- myData.raw[index.OT,"Pal_cnt"] / myData.raw[index.OT,"SUPPORT.calc"]

index.TN <- myData.raw["Gear"] == "TNS" | myData.raw["Gear"] == "TN25S"
myData.raw[index.TN,"SUPPORT.calc"] <- (125*0.3048 * myData.raw[index.TN,"Distance"]) / 100
myData.raw[index.TN,"CPUA.calc"]    <-  myData.raw[index.TN,"Pal_cnt"] / myData.raw[index.TN,"SUPPORT.calc"]

# updated due to the addition of "binary"
row.titles  <- c("Dep_ID", "SY",     "FO",         "Proj",   "Un_ID",   "Gear",  "Season","Bend",   "Bend_RN","Bend_RM",      "Near_NHD2rm_FID","Near_NHD2RM_dist_m","GIS_RM", "Bend_ID_60", "Bend_RM_60","D_dist_up","D_name_up","D_dist_dn","D_name_dn","D_name_near","D_dist_near","Chan_wid", "Wet_wid",   "Braid"   ,"T_name_up","T_dist_up", "T_name_dn","T_dist_dn","T_name_near","T_dist_near","MA",     "Mean_z", "Max_z",  "Min_z",  "Grade_10_RM",          "Lith_1",   "Lith_2",   "Lith_desc","NFHAP",    "Disch_med","Disch_mean","Tax_ord", "Tax_part_sz","Comp_per", "Sand_per",   "Clay_per",   "Frag_per",   "VF_wid",  "Seg",    "nPage",  "TotalPages","Set_date", "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turb",     "Cond",        "DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "Set_dur",              "StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3", "Depth_Mean","Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","Btm_vel",     "VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Fish_cnt",        "Pal_cnt",          "CPUE",   "CPUA",    "Alt_Pal_Cnt",          "CPUE_2",  "CPUA_2",      "Stk_12_dist",  "Stk_12_cnt",   "Stk_24_dist",  "Stk_24_cnt",   "Stk_36_dist",  "Stk_36_cnt",   "Stk_60_dist",   "Stk_60_cnt",    "Stk_120_dist",   "Stk_120_cnt",    "Rel_dsch_AY","Rel_dsch_WY","gear.type1","gear.type2","macro.type","binary","SUPPORT.calc","CPUE.calc","CPUA.calc")
colClasses  <- c("integer","factor", "factor",     "integer","integer", "factor","factor","integer","factor" ,"numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",   "numeric",  "factor",   "numeric",  "factor",   "factor",     "numeric",    "numeric",  "numeric",   "integer", "factor",   "numeric",   "factor",   "numeric",  "factor"     ,"numeric",    "factor", "numeric","numeric","numeric","numeric",              "factor",   "factor",   "factor",   "factor",   "numeric",  "numeric",   "factor",  "factor",     "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "factor", "integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric","numeric",   "numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",        "numeric",    "numeric",    "factor",    "factor",    "factor",    "factor","numeric",     "numeric",  "numeric")
used.4.anal <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     TRUE,    TRUE,    FALSE,    FALSE,    FALSE,          FALSE,            FALSE,               TRUE,     FALSE,        FALSE,       TRUE,       FALSE,      TRUE,       FALSE,      FALSE,        TRUE,         TRUE,       TRUE,        TRUE,      FALSE,      TRUE,        FALSE,      TRUE,       FALSE,        TRUE,         TRUE,     FALSE,    FALSE,    FALSE,    TRUE,                   TRUE,       TRUE,       TRUE,       TRUE,       FALSE,      FALSE,       FALSE,     TRUE,         TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     TRUE,       FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,   FALSE,    TRUE,    FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,    TRUE,        FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,         TRUE,         TRUE,        TRUE,        TRUE,        TRUE,    TRUE,          TRUE,       TRUE)


# ------------------------------------------------------------------------------------------------
# NOTE: the two variables [colClasses] and [used.4.anal] have to go with [myData.raw] together.
# ***** added 2 variables: a reduced numberof categories for "Meso" and "NFHAP" 
# ------------------------------------------------------------------------------------------------
# (I) new_Meso: see Eric's email on June 5, 2013 11:20 AM.   (for all data without distinction among subsets).
# [new_Meso]: "CHNB"                         --> "CHNB"
#             "DTWT"                         --> "DTWT"
#             "TLWG", "POOL", "ITIP", "BARS" --> "Other"
# (II) new_NFHAP: .   (for all data without distinction among subsets).
#                 "Low"                                    --> "Low"
#                 "Moderate"                               --> "Moderate"
#                 "High", "Very High"                      --> "High"
#                 "Not Scored / Unavailable at this scale" --> NA
myData.raw[,"new_meso"]  <- rep(NA,dim(myData.raw)[1])
myData.raw[,"new_NFHAP"] <- rep(NA,dim(myData.raw)[1])

# new_Meso: see Eric's email on June 5, 2013 11:20 AM.   (for all data without distinction among subsets).  
# Eric believe Other has higher CPUE: Other 264 yes/3264= 8.0% and CHNB: 1215 yes / 28896 = 4.2% 
# Since DTWT only has 96 points and none is associated with Pallid catch, we treated it as "no data", i.e., NA.
myData.raw[myData.raw[,"Meso"] == "CHNB","new_meso"]  <- "CHNB"
myData.raw[myData.raw[,"Meso"] == "DTWT","new_meso"]  <- NA
myData.raw[myData.raw[,"Meso"] == "TLWG","new_meso"]  <- "OTHER"
myData.raw[myData.raw[,"Meso"] == "POOL","new_meso"]  <- "OTHER"
myData.raw[myData.raw[,"Meso"] == "ITIP","new_meso"]  <- "OTHER"
myData.raw[myData.raw[,"Meso"] == "BARS","new_meso"]  <- "OTHER"

# Not Scored / Unavailable at this scale" treated as "NA" and combined "High" with "Very High".
myData.raw[myData.raw[,"NFHAP"] == "Low","new_NFHAP"]                                        <- "Low"
myData.raw[myData.raw[,"NFHAP"] == "Moderate","new_NFHAP"]                                   <- "Moderate"
myData.raw[myData.raw[,"NFHAP"] == "High" | myData.raw[,"NFHAP"] == "Very High","new_NFHAP"] <- "High"



# updated due to the addition of "binary"
row.titles  <- c("Dep_ID", "SY",     "FO",         "Proj",   "Un_ID",   "Gear",  "Season","Bend",   "Bend_RN","Bend_RM",      "Near_NHD2rm_FID","Near_NHD2RM_dist_m","GIS_RM", "Bend_ID_60", "Bend_RM_60","D_dist_up","D_name_up","D_dist_dn","D_name_dn","D_name_near","D_dist_near","Chan_wid", "Wet_wid",   "Braid"   ,"T_name_up","T_dist_up", "T_name_dn","T_dist_dn","T_name_near","T_dist_near","MA",     "Mean_z", "Max_z",  "Min_z",  "Grade_10_RM",          "Lith_1",   "Lith_2",   "Lith_desc","NFHAP",    "Disch_med","Disch_mean","Tax_ord", "Tax_part_sz","Comp_per", "Sand_per",   "Clay_per",   "Frag_per",   "VF_wid",  "Seg",    "nPage",  "TotalPages","Set_date", "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turb",     "Cond",        "DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "Set_dur",              "StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3", "Depth_Mean","Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","Btm_vel",     "VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Fish_cnt",        "Pal_cnt",          "CPUE",   "CPUA",    "Alt_Pal_Cnt",          "CPUE_2",  "CPUA_2",      "Stk_12_dist",  "Stk_12_cnt",   "Stk_24_dist",  "Stk_24_cnt",   "Stk_36_dist",  "Stk_36_cnt",   "Stk_60_dist",   "Stk_60_cnt",    "Stk_120_dist",   "Stk_120_cnt",    "Rel_dsch_AY","Rel_dsch_WY","gear.type1","gear.type2","macro.type","binary","SUPPORT.calc","CPUE.calc","CPUA.calc","new_meso","new_NFHAP")
colClasses  <- c("integer","factor", "factor",     "integer","integer", "factor","factor","integer","factor" ,"numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",   "numeric",  "factor",   "numeric",  "factor",   "factor",     "numeric",    "numeric",  "numeric",   "integer", "factor",   "numeric",   "factor",   "numeric",  "factor"     ,"numeric",    "factor", "numeric","numeric","numeric","numeric",              "factor",   "factor",   "factor",   "factor",   "numeric",  "numeric",   "factor",  "factor",     "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "factor", "integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric","numeric",   "numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",        "numeric",    "numeric",    "factor",    "factor",    "factor",    "factor","numeric",     "numeric",  "numeric",  "factor",  "factor")
used.4.anal <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     TRUE,    TRUE,    FALSE,    FALSE,    FALSE,          FALSE,            FALSE,               TRUE,     FALSE,        FALSE,       TRUE,       FALSE,      TRUE,       FALSE,      FALSE,        TRUE,         TRUE,       TRUE,        TRUE,      FALSE,      TRUE,        FALSE,      TRUE,       FALSE,        TRUE,         TRUE,     FALSE,    FALSE,    FALSE,    TRUE,                   TRUE,       TRUE,       TRUE,       TRUE,       FALSE,      FALSE,       FALSE,     TRUE,         TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     TRUE,       FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,   FALSE,    TRUE,    FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,    TRUE,        FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,         TRUE,         TRUE,        TRUE,        TRUE,        TRUE,    TRUE,          TRUE,       TRUE,       TRUE,      TRUE)

# ------------------------------------------------------------------------------------------------
# use [myData.4.anal] to store the maximum possible used variables: the maximum number of variable to be kept for the analysis
# ------------------------------------------------------------------------------------------------
myData.4.anal    <- myData.raw[,used.4.anal]	    # keep the maximum number of variables for the analysis
col.title.4.anal <- row.titles[used.4.anal]	    # keep the maximum number of variables for the analysis
col.class.4.anal <- colClasses[used.4.anal]	    # keep the maximum number of variables for the analysis

# October 2, 2013: use more descriptive name in plots
label.fields <- c("SY",         "Gear","Season",       "GIS_RM",              "D_dist_up",                        "D_dist_dn",                          "D_dist_near",             "Chan_wid",                   "Wet_wid",                  "Braid"   ,"T_dist_up",                               "T_dist_dn",                                "T_dist_near",                   "MA",     "Grade_10_RM",                       "Lith_1",   "Lith_2",   "Lith_desc","NFHAP",    "Tax_part_sz","Comp_per", "Sand_per",   "Clay_per",   "VF_wid",  "Seg",      "Temp",                      "Turb",     "Meso",  "MicroClass","Depth_Mean",                       "WaterVel","Fish_cnt",        "Pal_cnt",          "CPUE",   "CPUA",    "Stk_12_dist",  "Stk_12_cnt",                            "Stk_24_dist",  "Stk_24_cnt",                           "Stk_36_dist",  "Stk_36_cnt",                         "Stk_60_dist",   "Stk_60_cnt",                              "Stk_120_dist",   "Stk_120_cnt",                              "Rel_dsch_AY",                       "Rel_dsch_WY",                          "gear.type1","gear.type2","macro.type",       "binary","SUPPORT.calc","CPUE.calc","CPUA.calc","new_meso",        "new_NFHAP")
label.names  <- c("Sample Year","Gear","Sample Season","GIS River Mile (RM)", "Distance to Nearest Upstream Dam (RM)","Distance to Nearest Downstream Dam (RM)","Distance to Nearest Dam (RM)","Width of Full Channel (ft)", "Width of Wetted Aaea (ft)","Braid"   ,"Distance to Nearest Upstream Tributary (RM)", "Distance to Nearest Downstream Tributary (RM)","Distance to Nearest Tributary (RM)","MA",     "Grade Within 10 River Miles (m/km)","Lith_1",   "Lith_2",   "Lith_desc","NFHAP",    "Tax_part_sz","Comp_per", "Sand_per",   "Clay_per",   "VF_wid",  "Segement", "Temp When Gear Was Set (C)","Turb",     "Meso",  "MicroClass","Mean Depth During Deployment (ft)","WaterVel","Fish_cnt",        "Pal_cnt",          "CPUE",   "CPUA",    "Stk_12_dist",  "Stock Released within 10up/2down RM",   "Stk_24_dist",  "Stock Released within 20up/4down RM",  "Stk_36_dist",  "Stock Released within 30up/6down RM","Stk_60_dist",   "Stock Released within 50up/10down RM",    "Stk_120_dist",   "Stock Released within 100up/20down RM",    "Relative Discharge Among All Years","Relative Discharge Within Sample Year","Gear Type", "gear.type2","Macrohabitat Type","binary","SUPPORT.calc","CPUE.calc","CPUA.calc","Mesohabitat Type","NFHAP (Cumulative habitat condition index)")
names(label.names) <- label.fields


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
	Path.Out.subset <- paste(Path.Out,paste(paste("subset",idx.subset,sep=""),subset.section,subset.gear,sep="_"),sep="/")
	if (!file.exists(Path.Out.subset))    {print(paste("NOT existing:",Path.Out.subset));dir.create(Path.Out.subset,showWarnings=TRUE,recursive=TRUE)}


	
	
	cat(paste("\n\n************************************ [",subset.gear,"] and [",subset.section,"] ","************************************","\n",sep=""))
	cat(paste("\n\n************************************ [",subset.gear,"] and [",subset.section,"] ","************************************","\n",sep=""),file=FL.LOG,append=TRUE)
	
	# NOTE: the two variables [col.title.4.anal] and [col.class.4.anal] have to go with [myData.Sub] together.
	# subset 1: upper OT and TN (active gear)
	if (idx.subset == 1)
	{
		myData.Sub <- myData.4.anal[myData.4.anal[,"gear.type2"] == "active" & myData.4.anal[,"MA"] == subset.section,]	# keep those variables in [used.4.anal] plus "binary","Support","CPUE.calc","CPUA.calc" added above
		
		# remove "CPUE" field for active gear
		myData.Sub <- myData.Sub[,grep("[^CPUE.calc]",grep("[^CPUE]",names(myData.Sub),value=TRUE),value=TRUE)]
		
		# updates [col.title.4.anal] and [col.class.4.anal]
		idx.2.rm <- match(c("CPUE","CPUE.calc"),col.title.4.anal)
		col.title.4.sub  <- col.title.4.anal[-idx.2.rm]
		col.class.4.sub  <- col.class.4.anal[-idx.2.rm]
		
		new_y <- "CPUA.calc"
		
	# subset 2: upper TLC
	}else if(idx.subset == 2){
		myData.Sub <- myData.4.anal[myData.4.anal[,"gear.type1"] == "TLC"    & myData.4.anal[,"MA"] == subset.section,]
		
		# remove "CPUA" field for passive gear
		myData.Sub <- myData.Sub[,grep("[^CPUA.calc]",grep("[^CPUA]",names(myData.Sub),value=TRUE),value=TRUE)]
		
		# updates [col.title.4.anal] and [col.class.4.anal]
		idx.2.rm <- match(c("CPUA","CPUA.calc"),col.title.4.anal)
		col.title.4.sub  <- col.title.4.anal[-idx.2.rm]
		col.class.4.sub  <- col.class.4.anal[-idx.2.rm]
		
		new_y <- "CPUE.calc"
		
	# subset 3: lower OT & TN
	}else if(idx.subset == 3){
		myData.Sub <- myData.4.anal[myData.4.anal[,"gear.type2"] == "active" & myData.4.anal[,"MA"] == subset.section,]
		
		# remove "CPUE" field for active gear
		myData.Sub <- myData.Sub[,grep("[^CPUE.calc]",grep("[^CPUE]",names(myData.Sub),value=TRUE),value=TRUE)]
		
		# updates [col.title.4.anal] and [col.class.4.anal]
		idx.2.rm <- match(c("CPUE","CPUE.calc"),col.title.4.anal)
		col.title.4.sub  <- col.title.4.anal[-idx.2.rm]
		col.class.4.sub  <- col.class.4.anal[-idx.2.rm]
		
		new_y <- "CPUA.calc"
		
	# subset 4: lower TLC
	}else if(idx.subset == 4){
		myData.Sub <- myData.4.anal[myData.4.anal[,"gear.type1"] == "TLC"    & myData.4.anal[,"MA"] == subset.section,]
		
		# remove "CPUA" field for passive gear
		myData.Sub <- myData.Sub[,grep("[^CPUA.calc]",grep("[^CPUA]",names(myData.Sub),value=TRUE),value=TRUE)]
		
		# updates [col.title.4.anal] and [col.class.4.anal]
		idx.2.rm <- match(c("CPUA","CPUA.calc"),col.title.4.anal)
		col.title.4.sub  <- col.title.4.anal[-idx.2.rm]
		col.class.4.sub  <- col.class.4.anal[-idx.2.rm]
		
		new_y <- "CPUE.calc"
		
	# subset 5: lower GN
	}else if(idx.subset == 5){
		myData.Sub <- myData.4.anal[myData.4.anal[,"gear.type1"] == "GN"     & myData.4.anal[,"MA"] == subset.section,]
		
		# remove "CPUA" field for passive gear
		myData.Sub <- myData.Sub[,grep("[^CPUA.calc]",grep("[^CPUA]",names(myData.Sub),value=TRUE),value=TRUE)]
		
		# updates [col.title.4.anal] and [col.class.4.anal]
		idx.2.rm <- match(c("CPUA","CPUA.calc"),col.title.4.anal)
		col.title.4.sub  <- col.title.4.anal[-idx.2.rm]
		col.class.4.sub  <- col.class.4.anal[-idx.2.rm]
		
		new_y <- "CPUE.calc"
		
	# subset 6: Logistic upper
	}else if(idx.subset == 6){
		myData.Sub <- myData.4.anal[myData.4.anal[,"MA"] == subset.section,]
		
		col.title.4.sub  <- col.title.4.anal
		col.class.4.sub  <- col.class.4.anal
		
		new_y <- NA
		
	# subset 7: Logistic lower
	}else if(idx.subset == 7){
		myData.Sub <- myData.4.anal[myData.4.anal[,"MA"] == subset.section,]
		
		col.title.4.sub  <- col.title.4.anal
		col.class.4.sub  <- col.class.4.anal
		
		new_y <- NA
		
	# subset 8: All	
	}else if(idx.subset == 8){
		myData.Sub <- myData.4.anal
		
		col.title.4.sub  <- col.title.4.anal
		col.class.4.sub  <- col.class.4.anal		
		
		new_y <- NA		
	}	
	
	cat(paste("\t0. Subset data has been extracted for [",subset.section,"] and [",subset.gear,"]!\n",sep=""))
	cat(paste("\t0. Subset data has been extracted for [",subset.section,"] and [",subset.gear,"]!\n",sep=""),file=FL.LOG,append=TRUE)

	# -----------------------------------------------------------------------------------------------------------------------------------------------------------
	# re-grouping the categorical variables:
	# -----------------------------------------------------------------------------------------------------------------------------------------------------------
	# (I) new_lith, (II) new_part
	# (I) and (II): new_Lith and new_part see Chris Murray email on 
	# [Lith_1] for upper river "clay or mud", "silt"             --> "clay or mud"
	#                          "gravel", "sand", "allvium"       --> "allvium"
	#                          "shale", "sandstone", "limestone" --> "shale"
	#
	# [Tax_part_sz] in Upper River: "fine", "clayey", "clayey over loamy", "fine-silty", "coarse-silty" and "coarse-silty over clayey" --> "clayey"
	#                               "loamy", "fine-loamy", and "sandy"                                                                 --> "fine-loamy"
	#	
	# [Lith_desc] for lower river: keep use the [Unconsolidated] and [Sedimentary]
	#
	# [Tax_part_sz] in Lower River: "clayey over loamy", "clayey", "fine", and "fine-silty"                                            --> "fine"
	#                               "coarse-silty", "coarse-silty over clayey", "fine-loamy", and "loamy"                              --> "coarse"
	# -----------------------------------------------------------------------------------------------------------------------------------------------------------
	# subset 1: upper OT and TN (active gear) [myData.Upper_Active]
	if (idx.subset == 1)
	{
		myData.Sub[,"new_lith"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Lith_1"] == "clay or mud" | myData.Sub[,"Lith_1"] == "silt" ,"new_lith"]                                                    <- "clay or mud"
		myData.Sub[myData.Sub[,"Lith_1"] == "gravel"      | myData.Sub[,"Lith_1"] == "sand"              | myData.Sub[,"Lith_1"] == "alluvium", "new_lith"] <- "allvium"
		myData.Sub[myData.Sub[,"Lith_1"] == "shale"       | myData.Sub[,"Lith_1"] == "sandstone"         | myData.Sub[,"Lith_1"] == "limestone","new_lith"] <- "shale"
		if(any(myData.Sub[,"new_lith"] == "error")){cat(paste("Check the lithology re-grouping!\n",sep=""));die}

		myData.Sub[,"new_part"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "fine"  | myData.Sub[,"Tax_part_sz"] == "clayey"     | myData.Sub[,"Tax_part_sz"] == "clayey over loamy" | myData.Sub[,"Tax_part_sz"] == "fine-silty" | myData.Sub[,"Tax_part_sz"] == "coarse-silty" | myData.Sub[,"Tax_part_sz"] == "coarse-silty over clayey" ,"new_part"] <- "clayey"
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "loamy" | myData.Sub[,"Tax_part_sz"] == "fine-loamy" | myData.Sub[,"Tax_part_sz"] == "sandy",                                                                                                                                                                    "new_part"] <- "fine-loamy"
		if(any(myData.Sub[,"new_part"] == "error")){cat(paste("Check the particle size re-grouping!\n",sep=""));die}	
	# subset 2: upper TLC [myData.Upper_TLC]
	}else if(idx.subset == 2){
		myData.Sub[,"new_lith"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Lith_1"] == "clay or mud" | myData.Sub[,"Lith_1"] == "silt" ,                                                   "new_lith"] <- "clay or mud"
		myData.Sub[myData.Sub[,"Lith_1"] == "gravel"      | myData.Sub[,"Lith_1"] == "sand"              | myData.Sub[,"Lith_1"] == "alluvium", "new_lith"] <- "allvium"
		myData.Sub[myData.Sub[,"Lith_1"] == "shale"       | myData.Sub[,"Lith_1"] == "sandstone"         | myData.Sub[,"Lith_1"] == "limestone","new_lith"] <- "shale"
		if(any(myData.Sub[,"new_lith"] == "error")){cat(paste("Check the lithology re-grouping!\n",sep=""));die}

		myData.Sub[,"new_part"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "fine"  | myData.Sub[,"Tax_part_sz"] == "clayey"     | myData.Sub[,"Tax_part_sz"] == "clayey over loamy" | myData.Sub[,"Tax_part_sz"] == "fine-silty" | myData.Sub[,"Tax_part_sz"] == "coarse-silty" | myData.Sub[,"Tax_part_sz"] == "coarse-silty over clayey" ,"new_part"] <- "clayey"
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "loamy" | myData.Sub[,"Tax_part_sz"] == "fine-loamy" | myData.Sub[,"Tax_part_sz"] == "sandy",                                                                                                                                                                    "new_part"] <- "fine-loamy"
		if(any(myData.Sub[,"new_part"] == "error")){cat(paste("Check the particle size re-grouping!\n",sep=""));die}
	# subset 3: lower OT & TN	[myData.Lower_Active]
	}else if(idx.subset == 3){
		myData.Sub[,"new_lith"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Lith_desc"] == "Unconsolidated", "new_lith"] <- "Unconsolidated"
		myData.Sub[myData.Sub[,"Lith_desc"] == "Sedimentary",    "new_lith"] <- "Sedimentary"
		if(any(myData.Sub[,"new_lith"] == "error")){cat(paste("Check the lithology re-grouping!\n",sep=""));die}

		myData.Sub[,"new_part"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "clayey over loamy" | myData.Sub[,"Tax_part_sz"] == "fine"  | myData.Sub[,"Tax_part_sz"] == "fine-silty",                                                           "new_part"] <- "fine"
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "coarse-silty"      | myData.Sub[,"Tax_part_sz"] == "coarse-silty over clayey" | myData.Sub[,"Tax_part_sz"] == "loamy" | myData.Sub[,"Tax_part_sz"] == "fine-loamy","new_part"] <- "coarse"
		if(any(myData.Sub[,"new_part"] == "error")){cat(paste("Check the particle size re-grouping!\n",sep=""));die}
	# subset 4: lower TLC [myData.Lower_TLC]
	}else if(idx.subset == 4){
		myData.Sub[,"new_lith"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Lith_desc"] == "Unconsolidated", "new_lith"] <- "Unconsolidated"
		myData.Sub[myData.Sub[,"Lith_desc"] == "Sedimentary",    "new_lith"] <- "Sedimentary"
		if(any(myData.Sub[,"new_lith"] == "error")){cat(paste("Check the lithology re-grouping!\n",sep=""));die}

		myData.Sub[,"new_part"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "clayey over loamy" | myData.Sub[,"Tax_part_sz"] == "clayey" | myData.Sub[,"Tax_part_sz"] == "fine"  | myData.Sub[,"Tax_part_sz"] == "fine-silty",                  "new_part"] <- "fine"
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "coarse-silty"      | myData.Sub[,"Tax_part_sz"] == "coarse-silty over clayey" | myData.Sub[,"Tax_part_sz"] == "loamy" | myData.Sub[,"Tax_part_sz"] == "fine-loamy","new_part"] <- "coarse"
		if(any(myData.Sub[,"new_part"] == "error")){cat(paste("Check the particle size re-grouping!\n",sep=""));die}
	# subset 5: lower GN [myData.Lower_GN]
	}else if(idx.subset == 5){
		myData.Sub[,"new_lith"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Lith_desc"] == "Unconsolidated", "new_lith"] <- "Unconsolidated"
		myData.Sub[myData.Sub[,"Lith_desc"] == "Sedimentary",    "new_lith"] <- "Sedimentary"
		if(any(myData.Sub[,"new_lith"] == "error")){cat(paste("Check the lithology re-grouping!\n",sep=""));die}

		myData.Sub[,"new_part"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "clayey over loamy" | myData.Sub[,"Tax_part_sz"] == "clayey" | myData.Sub[,"Tax_part_sz"] == "fine"  | myData.Sub[,"Tax_part_sz"] == "fine-silty",                   "new_part"] <- "fine"
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "coarse-silty"      | myData.Sub[,"Tax_part_sz"] == "coarse-silty over clayey"  | myData.Sub[,"Tax_part_sz"] == "loamy" | myData.Sub[,"Tax_part_sz"] == "fine-loamy","new_part"] <- "coarse"
		if(any(myData.Sub[,"new_part"] == "error")){cat(paste("Check the particle size re-grouping!\n",sep=""));die}
	# subset 6: Logistic Upper [myData.Upper_Logistic]
	}else if(idx.subset == 6){
		myData.Sub[,"new_lith"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Lith_1"] == "clay or mud" | myData.Sub[,"Lith_1"] == "silt" ,                                                   "new_lith"] <- "clay or mud"
		myData.Sub[myData.Sub[,"Lith_1"] == "gravel"      | myData.Sub[,"Lith_1"] == "sand"              | myData.Sub[,"Lith_1"] == "alluvium", "new_lith"] <- "allvium"
		myData.Sub[myData.Sub[,"Lith_1"] == "shale"       | myData.Sub[,"Lith_1"] == "sandstone"         | myData.Sub[,"Lith_1"] == "limestone","new_lith"] <- "shale"
		if(any(myData.Sub[,"new_lith"] == "error")){cat(paste("Check the lithology re-grouping!\n",sep=""));die}

		myData.Sub[,"new_part"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "fine"  | myData.Sub[,"Tax_part_sz"] == "clayey"     | myData.Sub[,"Tax_part_sz"] == "clayey over loamy" | myData.Sub[,"Tax_part_sz"] == "fine-silty" | myData.Sub[,"Tax_part_sz"] == "coarse-silty" | myData.Sub[,"Tax_part_sz"] == "coarse-silty over clayey" ,"new_part"] <- "clayey"
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "loamy" | myData.Sub[,"Tax_part_sz"] == "fine-loamy" | myData.Sub[,"Tax_part_sz"] == "sandy",                                                                                                                                                                    "new_part"] <- "fine-loamy"
		if(any(myData.Sub[,"new_part"] == "error")){cat(paste("Check the particle size re-grouping!\n",sep=""));die}
	# subset 6: Logistic Lower [myData.Lower_Logistic]
	}else if(idx.subset == 7){
		myData.Sub[,"new_lith"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Lith_desc"] == "Unconsolidated", "new_lith"] <- "Unconsolidated"
		myData.Sub[myData.Sub[,"Lith_desc"] == "Sedimentary",    "new_lith"] <- "Sedimentary"
		if(any(myData.Sub[,"new_lith"] == "error")){cat(paste("Check the lithology re-grouping!\n",sep=""));die}

		myData.Sub[,"new_part"] <- rep("error",dim(myData.Sub)[1])
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "clayey over loamy" | myData.Sub[,"Tax_part_sz"] == "clayey" | myData.Sub[,"Tax_part_sz"] == "fine",                                                                                                         "new_part"] <- "fine"
		myData.Sub[myData.Sub[,"Tax_part_sz"] == "fine-silty" | myData.Sub[,"Tax_part_sz"] == "coarse-silty"  | myData.Sub[,"Tax_part_sz"] == "coarse-silty over clayey" | myData.Sub[,"Tax_part_sz"] == "loamy" | myData.Sub[,"Tax_part_sz"] == "fine-loamy","new_part"] <- "coarse"
		if(any(myData.Sub[,"new_part"] == "error")){cat(paste("Check the particle size re-grouping!\n",sep=""));die}
	# subset 7: All
	}else if(idx.subset == 8){
		myData.Sub[,"new_lith"] <- myData.Sub[,"Lith_1"]
		myData.Sub[,"new_part"] <- myData.Sub[,"Tax_part_sz"]
	}	
	
	# updated due to the addition of "new_lith" and "new_part"
	col.title.4.sub  <- c(col.title.4.sub,"new_lith","new_part")
	col.class.4.sub  <- c(col.class.4.sub,"factor",  "factor")		
	cat(paste("\t1a. Lithology and Particle Size variables for [",subset.section,"] and [",subset.gear,"] have been re-grouped!\n",sep=""))
	cat(paste("\t1a. Lithology and Particle Size variables for [",subset.section,"] and [",subset.gear,"] have been re-grouped!\n",sep=""),file=FL.LOG,append=TRUE)
	

	
	
	#
	# make sure factors are factors
	#
	for (idx.tmp in col.title.4.sub)
	{
		
		cond1 <- col.class.4.sub[grep(paste("^",idx.tmp,"$",sep=""),col.title.4.sub)]=="factor"	# current "idx.tmp" is supposed to be a factro as defined in "col.class.4.sub".  Note: need toapply "^" and "$" to restrict the whole string not the partial string to be matched because some column titles may contain common parts. e.g., "CPUA" in both "CPUA" and "CPUA.calc" fields.
		cond2 <- !(is.factor(myData.Sub[,idx.tmp]))					# current "idx.tmp" column in [myData.Sub]" is not a factor
		cat(paste(idx.tmp,cond1,length(cond1),cond2,length(cond2),"\n",sep="\t"))
		if (cond1 & cond2)
		{
			myData.Sub[,idx.tmp] <- as.factor(myData.Sub[,idx.tmp])
			cat(paste("\tdone with",idx.tmp,"\n",sep=""))
		}
	}
	cat(paste("\t1b. Made sure all factors are factors in [myData.Sub] for [",subset.section,"] and [",subset.gear,"] have been re-grouped!\n",sep=""))
	cat(paste("\t1b. Made sure all factors are factors in [myData.Sub] for [",subset.section,"] and [",subset.gear,"] have been re-grouped!\n",sep=""),file=FL.LOG,append=TRUE)




	#
	# record the data for each of the subsets and use [myData] as the working data file
	#
	#
	# re-grouping the categorical variables
	#
	# 
	# subset 1: upper OT and TN (active gear) [myData.Upper_Active]
	if (idx.subset == 1)
	{		
		myData.Upper_Active    <- myData.Sub
		col.title.Upper_Active <- col.title.4.sub  
		col.class.Upper_Active <- col.class.4.sub  			
	# subset 2: upper TLC [myData.Upper_TLC]
	}else if(idx.subset == 2){
		myData.Upper_TLC    <- myData.Sub	
		col.title.Upper_TLC <- col.title.4.sub  
		col.class.Upper_TLC <- col.class.4.sub 		
	# subset 3: lower OT & TN	[myData.Lower_Active]
	}else if(idx.subset == 3){
		myData.Lower_Active    <- myData.Sub
		col.title.Lower_Active <- col.title.4.sub  
		col.class.Lower_Active <- col.class.4.sub 			
	# subset 4: lower TLC [myData.Lower_TLC]
	}else if(idx.subset == 4){
		myData.Lower_TLC    <- myData.Sub
		col.title.Lower_TLC <- col.title.4.sub  
		col.class.Lower_TLC <- col.class.4.sub 		
	# subset 5: lower GN [myData.Lower_GN]
	}else if(idx.subset == 5){
		myData.Lower_GN    <- myData.Sub
		col.title.Lower_GN <- col.title.4.sub  
		col.class.Lower_GN <- col.class.4.sub		
	# subset 6: Logistic Upper [myData.Upper_Logistic]
	}else if(idx.subset == 6){
		myData.Upper_Logistic    <- myData.Sub	
		col.title.Upper_Logistic <- col.title.4.sub  
		col.class.Upper_Logistic <- col.class.4.sub		
	# subset 6: Logistic Lower [myData.Lower_Logistic]
	}else if(idx.subset == 7){
		myData.Lower_Logistic    <- myData.Sub
		col.title.Lower_Logistic <- col.title.4.sub  
		col.class.Lower_Logistic <- col.class.4.sub		
	# subset 7: All
	}else if(idx.subset == 8){
		myData.All    <- myData.Sub
		col.title.All <- col.title.4.sub  
		col.class.All <- col.class.4.sub		
	}			
	cat(paste("\t1c. Subset data and their field titles/classes of [",subset.section,"] and [",subset.gear,"] have been stored!\n",sep=""))
	cat(paste("\t1c. Subset data and their field titles/classes of [",subset.section,"] and [",subset.gear,"] have been stored!\n",sep=""),file=FL.LOG,append=TRUE)
	


	# ##########################################################################################################################################
	# ##########################################################################################################################################
	# ##########################################################################################################################################
	# ##########################################################################################################################################
	# August 27, 2013: for each subset set, only keep those variables we are going use. And delete any rows with missing data in any variables
	#                  find out the row index of the rows with missing data in any of the variables in the list
	# ##########################################################################################################################################
	var.to.use <- c(c("Pal_cnt","binary"),
	                c("SY","Season","Seg","macro.type","new_meso","new_NFHAP","gear.type1"),
	                c("GIS_RM","D_dist_up","D_dist_dn","D_dist_near","T_dist_up","T_dist_dn","T_dist_near","Stk_12_cnt","Stk_24_cnt","Stk_36_cnt","Stk_60_cnt","Stk_120_cnt","Rel_dsch_AY","Rel_dsch_WY","Depth_Mean","VF_wid","Wet_wid","Chan_wid","Temp","Grade_10_RM","Braid"))
	
	if (!(is.na(new_y)))
	{
		var.to.use <- c(var.to.use,"SUPPORT.calc")	# August 27, 2013: do not include "new_y" on purpose		
	}
	
	# these are the avriables originally kept in the model selection but am going to exclude
	var.to.use <- c(var.to.use,"Comp_per","Sand_per","Clay_per","new_lith","new_part","MicroClass") 
	
	if (idx.subset >=6)
	{
		var.to.use <- c(var.to.use,"gear.type1") 
	}


	# #
	# # (1) remove "CPUA.calc" from passive gear types and "CPUE.calc" from active gear types
	# # (2) remove "MiscoClass" from upper river section
	# if (idx.subset == 1)
	# {
	# }else{
	# }
	
	# remove rows when the list of variable above has missing values (those variable are variable to be used in the modeling).
	# naCol.2.rm: the number of missing of each variable
	# naRow.2.rm: the number of missing of each observation.  )	
	naCol.2.rm      <- apply(myData.Sub[,var.to.use],2,function(x){sum(is.na(x))})
	naRow.2.rm      <- apply(myData.Sub[,var.to.use],1,function(x){sum(is.na(x))})
	if (sum(naCol.2.rm) != sum(naRow.2.rm)){cat("Something is not right!\n");die}
	idx.NA <- seq(1,dim(myData.Sub)[1])[naRow.2.rm>0]
	
	cat(paste("Dimension of [myData.Sub] before NA removal is ",dim(myData.Sub)[1]," by ",dim(myData.Sub)[2],"!\t",sep=""))
	
	if(length(idx.NA)>0)
	{
		myData.Sub <- myData.Sub[-idx.NA,]
	}
	cat(paste("and after NA removal is ",dim(myData.Sub)[1]," by ",dim(myData.Sub)[2],"!\n",sep=""))
	
	

	
	
	
	if (idx.subset == 1)
	{		
		myData.Upper_Active.for.plot      <- myData.Sub	
		var.Upper_Active.for.plot         <- var.to.use
	# subset 2: upper TLC [myData.Upper_TLC]
	}else if(idx.subset == 2){
		myData.Upper_TLC.for.plot         <- myData.Sub	
		var.Upper_TLC.for.plot            <- var.to.use
	# subset 3: lower OT & TN	[myData.Lower_Active]
	}else if(idx.subset == 3){
		myData.Lower_Active.for.plot      <- myData.Sub
		var.Lower_Active.for.plot         <- var.to.use
	# subset 4: lower TLC [myData.Lower_TLC]
	}else if(idx.subset == 4){
		myData.Lower_TLC.for.plot         <- myData.Sub	
		var.Lower_TLC.for.plot            <- var.to.use
	# subset 5: lower GN [myData.Lower_GN]
	}else if(idx.subset == 5){
		myData.Lower_GN.for.plot          <- myData.Sub	
		var.Lower_GN.for.plot             <- var.to.use
	# subset 6: Logistic Upper [myData.Upper_Logistic]
	}else if(idx.subset == 6){
		myData.Upper_Logistic.for.plot    <- myData.Sub
		var.Upper_Logistic.for.plot       <- var.to.use
	# subset 6: Logistic Lower [myData.Lower_Logistic]
	}else if(idx.subset == 7){
		myData.Lower_Logistic.for.plot    <- myData.Sub	
		var.Lower_Logistic.for.plot       <- var.to.use
	# subset 7: All
	}else if(idx.subset == 8){
		myData.All.for.plot               <- myData.Sub
		var.All.for.plot                  <- var.to.use
		
	}	
	# ##########################################################################################################################################
	# ##########################################################################################################################################
	# ##########################################################################################################################################
	# ##########################################################################################################################################

	cat(paste("(",subset.gear,")_River(",subset.section,"):variable list:\n",sep=""))
	cat(paste(var.to.use,collapse=",",sep=""))
	
	
	# -------------------------------------------------------------------------------------------------
	# 2. check each individual variable: 
	# -------------------------------------------------------------------------------------------------	
	# open pdf file for outputting plots
	# FL.Plots.PDF     <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")_plot.pdf",sep=""),sep="/")	
	FL.CPUE_CPUA.PDF <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")_CPUE_CPUA_plot.pdf",sep=""),sep="/")
	FL.Data.Sum      <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")_data.csv",sep=""),sep="/")	
	# if  (file.exists(FL.Plots.PDF))    {print(paste(FL.Plots.PDF,"exist.Delete it!"));    file.remove(FL.Plots.PDF)}
	if  (file.exists(FL.CPUE_CPUA.PDF)){print(paste(FL.CPUE_CPUA.PDF,"exist.Delete it!"));file.remove(FL.CPUE_CPUA.PDF)}
	if  (file.exists(FL.Data.Sum))     {print(paste(FL.Data.Sum, "exist.Delete it!"));    file.remove(FL.Data.Sum)}
	
	
	# open the pdf file
	# pdf(file = FL.Plots.PDF,paper="a4r",width=0,height=0)	


	mySummary <- NULL
	idx <- 1
	
	# October 2, 2013: add output of JPG/PNG plot
	#            plotting starts here
	#            to use more meaningful character string for the plots
	label.fields <- c("SY",         "Gear","Season",       "GIS_RM",              "D_dist_up",                            "D_dist_dn",                              "D_dist_near",                 "Chan_wid",                   "Wet_wid",                  "Braid"   ,"T_dist_up",                                   "T_dist_dn",                                    "T_dist_near",                       "MA",     "Grade_10_RM",                       "Lith_1",   "Lith_2",   "Lith_desc","NFHAP",    "Tax_part_sz","Comp_per", "Sand_per",   "Clay_per",   "VF_wid",                     "Seg",      "Temp",                             "Turb",     "Meso",  "MicroClass","Depth_Mean",                       "WaterVel","Fish_cnt",        "Pal_cnt",          "CPUE",   "CPUA",    "Stk_12_dist",  "Stk_12_cnt",                            "Stk_24_dist",  "Stk_24_cnt",                           "Stk_36_dist",  "Stk_36_cnt",                         "Stk_60_dist",   "Stk_60_cnt",                              "Stk_120_dist",   "Stk_120_cnt",                              "Rel_dsch_AY",                       "Rel_dsch_WY",                          "gear.type1","gear.type2","macro.type",       "binary","SUPPORT.calc","CPUE.calc","CPUA.calc","new_meso",        "new_NFHAP",                                 "new_lith", "new_part")
	label.names  <- c("Sample Year","Gear","Sample Season","GIS River Mile (RM)", "Distance to Nearest Upstream Dam (RM)","Distance to Nearest Downstream Dam (RM)","Distance to Nearest Dam (RM)","Width of Full Channel (M)",  "Width of Wetted Aaea (M)", "Braid"   ,"Distance to Nearest Upstream Tributary (RM)", "Distance to Nearest Downstream Tributary (RM)","Distance to Nearest Tributary (RM)","MA",     "Grade Within 10 River Miles (m/km)","Lith_1",   "Lith_2",   "Lith_desc","NFHAP",    "Tax_part_sz","Comp_per", "Sand%",      "Clay%",      "Width of Valley Floor (M)",  "Segement", "Temperature When Gear Was Set (C)","Turb",     "Meso",  "MicroClass","Mean Depth During Deployment (ft)","WaterVel","Fish_cnt",        "Pal_cnt",          "CPUE",   "CPUA",    "Stk_12_dist",  "Stock Released within 10up/2down RM",   "Stk_24_dist",  "Stock Released within 20up/4down RM",  "Stk_36_dist",  "Stock Released within 30up/6down RM","Stk_60_dist",   "Stock Released within 50up/10down RM",    "Stk_120_dist",   "Stock Released within 100up/20down RM",    "Relative Discharge Among All Years","Relative Discharge Within Sample Year","Gear Type", "gear.type2","Macrohabitat Type","binary","SUPPORT.calc","CPUE.calc","CPUA.calc","Mesohabitat Type","NFHAP (Cumulative habitat condition index)","lithology","particle size")
	names(label.names) <- label.fields

	idx.plot <- 0
	for (var in grep("[^Pal_cnt]",grep("[^binary]",names(myData.Sub),value=TRUE,perl=TRUE),value=TRUE,perl=TRUE))	# field other than "binary" and "Pal_cnt" in [myData.Sub]
	{
		cat("---------------------------- ",var," ----------------------------\n")
		var.name.4plot <- label.names[var]


		# Categorical Varibles
		if (is.factor(myData.Sub[,var]))
		{	
			no.NA       <- sum(is.na(myData.Sub[,var]))
			if (idx.subset >= 8)
			{
				no.NA.upper <- sum(is.na(myData.Sub[myData.Sub[,"MA"] == "Upper",var]))
				no.NA.lower <- sum(is.na(myData.Sub[myData.Sub[,"MA"] == "Lower",var]))
			}

			myData.tmp <- myData.Sub[!(is.na(myData.Sub[,var])),var]
			if (idx.subset >= 8)
			{		  
				myData.tmp.upper <- myData.Sub[!(is.na(myData.Sub[,var])) & myData.Sub[,"MA"] == "Upper",var]
				myData.tmp.lower <- myData.Sub[!(is.na(myData.Sub[,var])) & myData.Sub[,"MA"] == "Lower",var]
			}


			plot.all <- barchart(myData.tmp,ylab=var.name.4plot,xlab="Count",main=paste("Distribution of ",var.name.4plot,"(with ",no.NA,      " missing values)",sep=""))
			if (idx.subset >= 8)
			{		
				plot.upper  <- barchart(myData.tmp.upper,ylab=var.name.4plot,xlab="Count",main=paste("Distribution of ",var.name.4plot,"(with ",no.NA.upper," missing values)\nUpper",sep=""))
				plot.lower  <- barchart(myData.tmp.lower,ylab=var.name.4plot,xlab="Count",main=paste("Distribution of ",var.name.4plot,"(with ",no.NA.lower," missing values)\nLower",sep=""))
			}

			# group the variable in term of "binary" and "MA"
			myData.Sub.freq <- as.data.frame(table(myData.Sub[,c("binary",var)]))
			plot.binary <- barchart(myData.Sub.freq[,var] ~ myData.Sub.freq[,"Freq"], data = myData.Sub.freq,groups = myData.Sub.freq[,"binary"],layout = c(1,1),stack = TRUE,auto.key = list(points = FALSE, rectangles = TRUE, space = "top"),xlab = "Freq",ylab = var.name.4plot,horizontal=TRUE,scales="free",between = list(x=0.5,y=0.5)) 

			if (idx.subset >= 8)
			{	
				# October 2, 2013: add output of JPG/PNG plot
				#                  split this plot into two.
				idx.plot <- idx.plot + 1
				
				FL.Plot.PNG <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")",paste("_Plot",idx.plot,"A.png",sep=""),sep=""),sep="/")	
				png(file=FL.Plot.PNG,width = 600, height = 400, units = "px", pointsize = 12,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo", "cairo-png"))			

					plot(plot.all,   split=c(1,1,2,2))
					plot(plot.lower, split=c(1,2,2,2),newpage=FALSE)
					plot(plot.upper, split=c(2,2,2,2),newpage=FALSE)
				dev.off()
				
				# October 2, 2013: add output of JPG/PNG plot
				#                  split this plot into two.
				FL.Plot.PNG <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")",paste("_Plot",idx.plot,"B.png",sep=""),sep=""),sep="/")	
				png(file=FL.Plot.PNG,width = 600, height = 400, units = "px", pointsize = 12,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo", "cairo-png"))			
					plot(plot.binary,split=c(1,1,1,1))
				dev.off()
			}else{
				# October 2, 2013: add output of JPG/PNG plot
				#                  split this plot into two.
				idx.plot <- idx.plot + 1
				
				FL.Plot.PNG <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")",paste("_Plot",idx.plot,"A.png",sep=""),sep=""),sep="/")	
				png(file=FL.Plot.PNG,width = 600, height = 400, units = "px", pointsize = 12,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo", "cairo-png"))						
					plot(plot.all,   split=c(1,1,1,1))
				dev.off()
				
				
				# October 2, 2013: add output of JPG/PNG plot
				#                  split this plot into two.
				FL.Plot.PNG <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")",paste("_Plot",idx.plot,"B.png",sep=""),sep=""),sep="/")	
				png(file=FL.Plot.PNG,width = 600, height = 400, units = "px", pointsize = 12,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo", "cairo-png"))							
					plot(plot.binary,split=c(1,1,1,1),newpage=FALSE)
				dev.off()
			}
			
			
			# output to the summary fileni
			cat(paste(var,",\n",sep=""),file=FL.Data.Sum,append=TRUE)
			write.table(as.data.frame(table(myData.Sub[,var])),file=FL.Data.Sum,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)	
			
			
			# August 13, 2013: add a new plot of the scatter plot between either CPUA or CPUE against the numerical variable
			if(!(is.na(new_y)))
			{
				myData.tmp_new1 <- myData.Sub[!(is.na(myData.Sub[,var])),c(var,new_y)]
				command.string  <- paste("plot_new1 <- bwplot(",new_y,"   ~ ",var,",data=myData.tmp_new1,      notch=TRUE,outline=FALSE,range=0,main=\"",paste(subset.section,subset.gear,sep="-")," (all data)\",  col=c(\"red\",\"blue\",\"cyan\",\"magenta\",\"green\",\"brown\",\"yellow\",\"pink\",\"purple\"),xlab=\"",var.name.4plot,"\",ylab=\"",new_y,"\")",sep="")
				eval(parse(text=command.string))
				
				myData.tmp_new2 <- myData.Sub[!(is.na(myData.Sub[,var])) & myData.Sub[,"binary"] == "yes" ,c(var,new_y)]
				command.string  <- paste("plot_new2 <- bwplot(",new_y,"   ~ ",var,",data=myData.tmp_new2,      notch=TRUE,outline=FALSE,range=0,main=\"",paste(subset.section,subset.gear,sep="-")," (presence)\",  col=c(\"red\",\"blue\",\"cyan\",\"magenta\",\"green\",\"brown\",\"yellow\",\"pink\",\"purple\"),xlab=\"",var.name.4plot,"\",ylab=\"",new_y,"\")",sep="")
				eval(parse(text=command.string))	


				# October 2, 2013: add output of JPG/PNG plot
				idx.plot <- idx.plot + 1
				FL.Plot.PNG <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")",paste("_Plot",idx.plot,".png",sep=""),sep=""),sep="/")	
				png(file=FL.Plot.PNG,width = 600, height = 400, units = "px", pointsize = 12,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo", "cairo-png"))
					plot(plot_new1,split=c(1,1,2,1))
					plot(plot_new2,split=c(2,1,2,1),newpage=FALSE)				
				dev.off()
				# myData.tmp_new3 <- myData.Sub[!(is.na(myData.Sub[,var])) & myData.Sub[,"binary"] == "no" ,c(var,new_y)]"",paste(subset.section,subset.gear,sep="-"),"\",  col=c(\"red\",\"blue\",\"cyan\",\"magenta\",\"green\",\"brown\",\"yellow\",\"pink\",\"purple\"),xlab=\"",var,"\",ylab=\"",new_y,"\")",sep="")
				# eval(parse(text=command.string))				
			}
			
			
			
		# continuous variables		
		}else{
			no.NA <- sum(is.na(myData.Sub[,var]))
			if (idx.subset >= 8)
			{		  
				no.NA.upper <- sum(is.na(myData.Sub[myData.Sub[,"MA"] == "Upper",var]))
				no.NA.lower <- sum(is.na(myData.Sub[myData.Sub[,"MA"] == "Lower",var]))
			}

			myData.tmp <- myData.Sub[!(is.na(myData.Sub[,var])),c(var,"MA","binary","Pal_cnt")]
			if (idx.subset >= 8)
			{		
				myData.tmp.upper <- myData.Sub[!(is.na(myData.Sub[,var])) & myData.Sub[,"MA"] == "Upper",c(var,"binary","Pal_cnt")]
				myData.tmp.lower <- myData.Sub[!(is.na(myData.Sub[,var])) & myData.Sub[,"MA"] == "Lower",c(var,"binary","Pal_cnt")]
			}
			cat(paste("got the subset data for current variable [",var,"]\n",sep=""))
			cat(paste("got the subset data for current variable [",var,"]\n",sep=""),file=FL.LOG,append=TRUE)


			plot.all   <- histogram(myData.tmp[,var],freq=FALSE,nint=100, xlab=var.name.4plot,ylab="Count",type="count",main=paste("Distribution of ",var.name.4plot,"(with ",no.NA,      " missing values)",       sep=""))
			if (idx.subset >= 8)
			{		
				plot.upper <- histogram(myData.tmp.upper[,var],freq=FALSE,nint=100, xlab=var.name.4plot,ylab="Count",type="count",main=paste("Distribution of ",var.name.4plot,"(with ",no.NA.upper," missing values)\nUpper",sep=""))
				plot.lower <- histogram(myData.tmp.lower[,var],freq=FALSE,nint=100, xlab=var.name.4plot,ylab="Count",type="count",main=paste("Distribution of ",var.name.4plot,"(with ",no.NA.lower," missing values)\nLower",sep=""))
			}

			# October 2, 2013: add output of JPG/PNG plot
			idx.plot <- idx.plot + 1
			FL.Plot.PNG <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")",paste("_Plot",idx.plot,".png",sep=""),sep=""),sep="/")	
			png(file=FL.Plot.PNG,width = 600, height = 400, units = "px", pointsize = 12,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo", "cairo-png"))
				if (idx.subset >= 8)
				{		
					plot(plot.all,  split=c(1,1,2,2))
					plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
					plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)		
				}else{
					plot(plot.all,  split=c(1,1,1,1))
				}
			dev.off()
			cat(paste("plotted histogram for current variable [",var,"]\n",sep=""))
			cat(paste("plotted histogram for current variable [",var,"]\n",sep=""),file=FL.LOG,append=TRUE)
			



			# August 13, 2013: add a new plot of the scatter plot between either CPUA or CPUE against the numerical variable
			if(!(is.na(new_y)))
			{
				myData.tmp_new4 <- myData.Sub[!(is.na(myData.Sub[,var])),c(var,new_y)]
				command.string <- paste("plot_new4 <- xyplot(myData.tmp_new4[,\"",new_y,"\"] ~ myData.tmp_new4[,\"",var,"\"],data=myData.tmp_new4,main=\"",paste(subset.section,subset.gear,sep="-")," (all data)\",xlab=\"",var.name.4plot,"\",ylab=\"",new_y,"\",pch=16,cex=0.5,col=\"red\")",sep="")
				eval(parse(text=command.string))
				
				
				myData.tmp_new5 <- myData.Sub[!(is.na(myData.Sub[,var])) & myData.Sub[,"binary"] == "yes" ,c(var,new_y)]
				command.string <- paste("plot_new5 <- xyplot(myData.tmp_new5[,\"",new_y,"\"] ~ myData.tmp_new5[,\"",var,"\"],data=myData.tmp_new5,main=\"",paste(subset.section,subset.gear,sep="-")," (presence)\",xlab=\"",var.name.4plot,"\",ylab=\"",new_y,"\",pch=16,cex=0.5,col=\"blue\")",sep="")
				eval(parse(text=command.string))
				
			}
			
			
			# a box plot of the continuous variable against Pallid Count/Presence/Absence
			if (var != "Pal_cnt")
			{
				# October 2, 2013: add output of JPG/PNG plot
				idx.plot <- idx.plot + 1
				FL.Plot.PNG <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")",paste("_Plot",idx.plot,".png",sep=""),sep=""),sep="/")	
				png(file=FL.Plot.PNG,width = 600, height = 400, units = "px", pointsize = 12,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo", "cairo-png"))
			
				if (idx.subset >= 8)
				{
					par(mfrow = c(2,3))
				}else{
					if (!(is.na(new_y)))
					{
						# par(mfrow = c(1,3))	# Oct 2, 2013: commented out to not plot the scatter plot for numeric variables
						  par(mfrow = c(1,2))	# Oct 2, 2013: commented out to not plot the scatter plot for numeric variables
					}else{
						par(mfrow = c(1,2))
					}
				}

					command.string <- paste("boxplot(",var,"   ~ Pal_cnt,data=myData.tmp,      notch=TRUE,outline=FALSE,range=0,main=\"all\",  col=c(\"red\",\"blue\",\"cyan\",\"magenta\",\"green\",\"brown\",\"yellow\",\"pink\",\"purple\"),xlab=\"Pallid Count\",ylab=\"",var.name.4plot,"\")",sep="")
					eval(parse(text=command.string))

				if (idx.subset >= 8)
				{
					command.string <- paste("boxplot(",var,"   ~ Pal_cnt,data=myData.tmp.upper,notch=TRUE,outline=FALSE,range=0,main=\"upper\",col=c(\"red\",\"blue\",\"cyan\",\"magenta\",\"green\",\"brown\",\"yellow\",\"pink\",\"purple\"),xlab=\"Pallid Count\",ylab=\"",var.name.4plot,"\")",sep="")
					eval(parse(text=command.string))			
					command.string <- paste("boxplot(",var,"   ~ Pal_cnt,data=myData.tmp.lower,notch=TRUE,outline=FALSE,range=0,main=\"lower\",col=c(\"red\",\"blue\",\"cyan\",\"magenta\",\"green\",\"brown\",\"yellow\",\"pink\",\"purple\"),xlab=\"Pallid Count\",ylab=\"",var.name.4plot,"\")",sep="")
					eval(parse(text=command.string))		
				}


					command.string <- paste("boxplot(",var,"   ~ binary,data=myData.tmp,       notch=TRUE,outline=FALSE,range=0,main=\"all\",  col=c(\"red\",\"cyan\"),xlab=\"Pallid Presence/Absence\",ylab=\"",var.name.4plot,"\")",sep="")
					eval(parse(text=command.string))		

				if (idx.subset >= 8)
				{
					command.string <- paste("boxplot(",var,"   ~ binary,data=myData.tmp.upper, notch=TRUE,outline=FALSE,range=0,main=\"upper\",col=c(\"red\",\"cyan\"),xlab=\"Pallid Presence/Absence\",ylab=\"",var.name.4plot,"\")",sep="")
					eval(parse(text=command.string))		
					command.string <- paste("boxplot(",var,"   ~ binary,data=myData.tmp.lower, notch=TRUE,outline=FALSE,range=0,main=\"lower\",col=c(\"red\",\"cyan\"),xlab=\"Pallid Presence/Absence\",ylab=\"",var.name.4plot,"\")",sep="")
					eval(parse(text=command.string))
				}
				
				# Oct 2, 2013: commented out to not plot the scatter plot for numeric variables
				# # August 13, 2013: add a new plot of the scatter plot between either CPUA or CPUE against the numerical variable
				# if(!(is.na(new_y)))
				# {
				# 	command.string <- paste("plot(myData.tmp_new5[,\"",var,"\"],myData.tmp_new5[,\"",new_y,"\"],xlab=\"",var,"\",ylab=\"",new_y,"\",col=\"red\",pch=16)",sep="")
				# 	eval(parse(text=command.string))			
				# }
				
				dev.off()

			}
			cat(paste("plotted boxplot for current variable [",var,"]\n",sep=""))
			cat(paste("plotted boxplot for current variable [",var,"]\n",sep=""),file=FL.LOG,append=TRUE)
			

			# August 13, 2013: add a new plot of the scatter plot between either CPUA or CPUE against the numerical variable
			if(!(is.na(new_y)))
			{
				# October 2, 2013: add output of JPG/PNG plot
				idx.plot <- idx.plot + 1
				FL.Plot.PNG <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")",paste("_Plot",idx.plot,".png",sep=""),sep=""),sep="/")	
				png(file=FL.Plot.PNG,width = 600, height = 400, units = "px", pointsize = 12,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo", "cairo-png"))			
					plot(plot_new4,split=c(1,1,2,1))
					plot(plot_new5,split=c(2,1,2,1),newpage=FALSE)					
				dev.off()
			}


			#
			# get summary of the data
			#
			if (idx.subset >= 8)
			{
				mySummary <- cbind(
					     data.frame(min.all = apply(myData.Sub[,var,drop=FALSE],2,min,na.rm=TRUE),
							med.all = apply(myData.Sub[,var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
							max.all = apply(myData.Sub[,var,drop=FALSE],2,max,na.rm=TRUE),
							sd.all  = apply(myData.Sub[,var,drop=FALSE],2,sd,na.rm=TRUE),
							no.all  = apply(myData.Sub[,var,drop=FALSE],2,function(x){length(x)}),
							NA.all  = apply(myData.Sub[,var,drop=FALSE],2,function(x){sum(is.na(x))})),
					     data.frame(min.upper = apply(myData.Sub[myData.Sub[,"MA"] == "Upper",var,drop=FALSE],2,min,na.rm=TRUE),
							med.upper = apply(myData.Sub[myData.Sub[,"MA"] == "Upper",var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
							max.upper = apply(myData.Sub[myData.Sub[,"MA"] == "Upper",var,drop=FALSE],2,max,na.rm=TRUE),
							sd.upper  = apply(myData.Sub[myData.Sub[,"MA"] == "Upper",var,drop=FALSE],2,sd,na.rm=TRUE),
							no.upper  = apply(myData.Sub[myData.Sub[,"MA"] == "Upper",var,drop=FALSE],2,function(x){length(x)}),
							NA.upper  = apply(myData.Sub[myData.Sub[,"MA"] == "Upper",var,drop=FALSE],2,function(x){sum(is.na(x))})),
					     data.frame(min.lower = apply(myData.Sub[myData.Sub[,"MA"] == "Lower",var,drop=FALSE],2,min,na.rm=TRUE),
							med.lower = apply(myData.Sub[myData.Sub[,"MA"] == "Lower",var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
							max.lower = apply(myData.Sub[myData.Sub[,"MA"] == "Lower",var,drop=FALSE],2,max,na.rm=TRUE),
							sd.lower  = apply(myData.Sub[myData.Sub[,"MA"] == "Lower",var,drop=FALSE],2,sd,na.rm=TRUE),
							no.lower  = apply(myData.Sub[myData.Sub[,"MA"] == "Lower",var,drop=FALSE],2,function(x){length(x)}),
							NA.lower  = apply(myData.Sub[myData.Sub[,"MA"] == "Lower",var,drop=FALSE],2,function(x){sum(is.na(x))})))   		
			}else{
				mySummary <- cbind(
					     data.frame(min.all = apply(myData.Sub[,var,drop=FALSE],2,min,na.rm=TRUE),
							med.all = apply(myData.Sub[,var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
							max.all = apply(myData.Sub[,var,drop=FALSE],2,max,na.rm=TRUE),
							sd.all  = apply(myData.Sub[,var,drop=FALSE],2,sd,na.rm=TRUE),
							no.all  = apply(myData.Sub[,var,drop=FALSE],2,function(x){length(x)}),
							NA.all  = apply(myData.Sub[,var,drop=FALSE],2,function(x){sum(is.na(x))})))                        
			}					
			cat(paste("computed summary for current variable [",var,"]\n",sep=""))
			cat(paste("computed summary for current variable [",var,"]\n",sep=""),file=FL.LOG,append=TRUE)


			if (idx == 1)
			{
				cat(",",file=FL.Data.Sum,append=TRUE)
				write.table(mySummary,file=FL.Data.Sum,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
			}else{
				write.table(mySummary,file=FL.Data.Sum,sep=",",col.names=FALSE,row.names=TRUE,append=TRUE)
			}
			cat(paste("wrote summary for current variable [",var,"]\n",sep=""))
			cat(paste("wrote summary for current variable [",var,"]\n",sep=""),file=FL.LOG,append=TRUE)

		}

		# make a table for missing data
		idx <- idx + 1
	}
	# dev.off()
	cat(paste("\t2. Each retained individual variable has been checked for [",subset.section,"] and [",subset.gear,"]!\n",sep=""))
	cat(paste("\t2. Each retained individual variable has been checked for [",subset.section,"] and [",subset.gear,"]!\n",sep=""),file=FL.LOG,append=TRUE)





	# -------------------------------------------------------------------------------------------------
	# 3. count summary: 
	# -------------------------------------------------------------------------------------------------	
	FL.Count.Summary.csv <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")_Count_Summary.csv",sep=""),sep="/")
	if  (file.exists(FL.Count.Summary.csv)){print(paste(FL.Count.Summary.csv,"exist.Delete it!")); file.remove(FL.Count.Summary.csv)}
	
	### row.retained <- c(row.titles[used.4.anal])
	### 
	### # note: CPUE not included for active gear and CPUA not included for passive gear
	### if (idx.subset == 1 | idx.subset == 3)
	### {
	### 	row.retained <- grep("[^CPUE]",row.retained,value=TRUE)
        ### 
	### }else if (idx.subset == 2 | idx.subset == 4 | idx.subset == 5){
	### 	row.retained <- grep("[^CPUA]",row.retained,value=TRUE)
	### }
	
	myData.tmp <- melt(myData.Sub,id.var=grep("[^Pal_cnt]",names(myData.Sub),value=TRUE),measure.var="Pal_cnt")

	sum.SY.Count        <- as.data.frame(cast(myData.tmp,SY          ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across sample year   [SY]
	sum.Season.Count    <- as.data.frame(cast(myData.tmp,Season      ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Season        [Season]
	sum.Lith_1.Count    <- as.data.frame(cast(myData.tmp,Lith_1      ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Lith_1        [Lith_1]
	sum.Lith_2.Count    <- as.data.frame(cast(myData.tmp,Lith_2      ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Lith_2        [Lith_2]
	sum.Lith_desc.Count <- as.data.frame(cast(myData.tmp,Lith_desc   ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Lith_desc     [Lith_desc]
	sum.Tax_part.Count  <- as.data.frame(cast(myData.tmp,Tax_part_sz ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Tax_part      [Tax_part]
	sum.Macro.Count     <- as.data.frame(cast(myData.tmp,macro.type  ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Macro         [macro.type]
	sum.Micro.Count     <- as.data.frame(cast(myData.tmp,MicroClass  ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across MicroClass    [MicroClass]
	sum.Seg.Count       <- as.data.frame(cast(myData.tmp,Seg         ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Seg           [Seg]
	sum.Gear.Count      <- as.data.frame(cast(myData.tmp,Gear        ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Gear          [Gear]

	sum.Meso.Count      <- as.data.frame(cast(myData.tmp,new_meso    ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Meso          [Meso]
	sum.NFHAP.Count     <- as.data.frame(cast(myData.tmp,new_NFHAP   ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across NFHAP         [new_NFHAP]
	sum.part.Count      <- as.data.frame(cast(myData.tmp,new_part    ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Meso          [new_part]
	sum.lith.Count      <- as.data.frame(cast(myData.tmp,new_lith    ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Meso          [new_lith]


	if (idx.subset >= 6)
	{
		sum.MA.Count        <- as.data.frame(cast(myData.tmp,MA          ~ value,fun=length,margins = c("grand_row","grand_col")))		# count distribution across MA            [MA]
		sum.Gear1.Count     <- as.data.frame(cast(myData.tmp,gear.type1  ~ value,fun=length,margins = c("grand_row","grand_col")))		# count distribution across Gear.type1    [gear.type1]
		sum.Gear2.Count     <- as.data.frame(cast(myData.tmp,gear.type2  ~ value,fun=length,margins = c("grand_row","grand_col")))		# count distribution across Gear.type2    [gear.type2]	
	}


	#
	# single variable distribution
	#
	cat("Count distribution in Year\n",             file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.SY.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)								# write [sum.SY.Count] out

	cat("\n\nCount distribution in Season\n",       file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.Season.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Season.Count] out

	cat("\n\nCount distribution in Lith_1\n",       file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.Lith_1.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Lith_1.Count] out

	cat("\n\nCount distribution in Lith_2\n",       file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.Lith_2.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Lith_2.Count] out

	cat("\n\nCount distribution in Lith_desc\n",    file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.Lith_desc.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Lith_desc.Count] out

	cat("\n\nCount distribution in Macro\n",        file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.Macro.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Macro.Count] out

	cat("\n\nCount distribution in Micro\n",        file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.Micro.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Micro.Count] out

	cat("\n\nCount distribution in Seg\n",          file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.Seg.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)								# write [sum.Seg.Count] out

	cat("\n\nCount distribution in Gear\n",         file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.Gear.Count, file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Gear.Count] out


	cat("\n\nCount distribution in Meso\n",         file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.Meso.Count, file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Meso.Count] out

	cat("\n\nCount distribution in NFHAP\n",         file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.NFHAP.Count, file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.NFHAP.Count] out

	cat("\n\nCount distribution in part\n",         file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.part.Count, file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.part.Count] out

	cat("\n\nCount distribution in part\n",         file=FL.Count.Summary.csv,append=TRUE)
	write.table(sum.lith.Count, file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.lith.Count] out

	if (idx.subset >= 6)
	{
		cat("\n\nCount distribution in MA\n",           file=FL.Count.Summary.csv,append=TRUE)
		write.table(sum.MA.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.MA.Count] out

		cat("\n\nCount distribution in Gear Type 1\n"  ,file=FL.Count.Summary.csv,append=TRUE)
		write.table(sum.Gear1.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Gear1.Count] out
	
		cat("\n\nCount distribution in Gear Type 2\n",  file=FL.Count.Summary.csv,append=TRUE)
		write.table(sum.Gear2.Count,file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Gear2.Count] out
	}

	#
	# two variable distribution
	#
	if (idx.subset >= 6)
	{	
		# ........ [Year] vs [Gear] ......................
		cat("\n\n\tCount distribution in [Year] and [Gear]\n")
		cat("\n\nCount distribution in [Year] and [Gear]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.SY.Gear1.Count <- cast(myData.tmp,SY ~ value | Gear,fun=length)											# count distribution across sample year   [SY] and [Gear]
		for (idx in names(sum.SY.Gear1.Count))
		{
			cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.SY.Gear1.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.SY.Gear1.Count] out
		}
	
		# ........ [Year] vs [Gear.Type1] ......................
		cat("\n\n\tCount distribution in [Year] and [Gear.Type1]\n")
		cat("\n\nCount distribution in [Year] and [Gear.Type1]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.SY.Gear1.Count <- cast(myData.tmp,SY ~ value | gear.type1,fun=length)										# count distribution across sample year   [SY] and [gear.type1]
		for (idx in names(sum.SY.Gear1.Count))
		{
			cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.SY.Gear1.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.SY.Gear1.Count] out
		}

		# ........ [Year] vs [Gear.Type2] ......................
		cat("\n\n\tCount distribution in [Year] and [Gear.Type2]\n")
		cat("\n\nCount distribution in [Year] and [Gear.Type2]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.SY.Gear2.Count <- cast(myData.tmp,SY ~ value | gear.type2,fun=length)										# count distribution across sample year   [SY] and [gear.type2]
		for (idx in names(sum.SY.Gear2.Count))
		{
			cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.SY.Gear2.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.SY.Gear2.Count] out
		}

		# ........ [MA] vs [Gear] ......................
		cat("\n\n\tCount distribution in [MA] and [Gear]\n")
		cat("\n\nCount distribution in [MA] and [Gear]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.MA.Gear1.Count <- cast(myData.tmp,MA ~ value | Gear,fun=length)											# count distribution across MA [MA] and [Gear]
		for (idx in names(sum.MA.Gear1.Count))
		{
			cat(paste("Count distribution in [MA] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.Gear1.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.MA.Gear1.Count] out
		}



		# ........ [MA] vs [Gear.Type1] ......................
		cat("\n\n\tCount distribution in [MA] and [Gear.Type1]\n")
		cat("\n\nCount distribution in [MA] and [Gear.Type1]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.MA.Gear1.Count <- cast(myData.tmp,MA ~ value | gear.type1,fun=length)										# count distribution across MA [MA] and [gear.type1]
		for (idx in names(sum.MA.Gear1.Count))
		{
			cat(paste("Count distribution in [MA] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.Gear1.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.MA.Gear1.Count] out
		}

		# ........ [MA] vs [Gear.Type2] ......................
		cat("\n\n\tCount distribution in [MA] and [Gear.Type2]\n")
		cat("\n\nCount distribution in [MA] and [Gear.Type2]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.MA.Gear2.Count <- cast(myData.tmp,MA ~ value | gear.type2,fun=length)										# count distribution across MA [MA] and [gear.type2]
		for (idx in names(sum.SY.Gear2.Count))
		{
			cat(paste("Count distribution in [MA] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.Gear2.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.MA.Gear2.Count] out
		}

		# ........ [MA] vs [macro.type] ......................
		cat("\n\n\tCount distribution in [MA] and [macro.type]\n")
		cat("\n\nCount distribution in [MA] and [macro.type]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.MA.macro.type.Count <- cast(myData.tmp,MA ~ value | macro.type,fun=length)										# count distribution across MA [MA] and macro.type [macro.type]
		for (idx in names(sum.MA.macro.type.Count))
		{
			cat(paste("Count distribution in [MA] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.macro.type.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.MA.macro.type.Count] out
		}

		# ........ [Gear] vs [macro.type] ......................
		cat("\n\n\tCount distribution in [Gear] and [macro.type]\n")
		cat("\n\nCount distribution in [Gear] and [macro.type]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.GearType1.macro.type.Count <- cast(myData.tmp,Gear ~ value | macro.type,fun=length)									# count distribution across Gear Type1 [Gear] and macro.type [macro.type]
		for (idx in names(sum.GearType1.macro.type.Count))
		{
			cat(paste("Count distribution in [Gear] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.macro.type.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.macro.type.Count] out
		}



		# ........ [Gear.Type1] vs [macro.type] ......................
		cat("\n\n\tCount distribution in [Gear.type1] and [macro.type]\n")
		cat("\n\nCount distribution in [Gear.type1] and [macro.type]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.GearType1.macro.type.Count <- cast(myData.tmp,gear.type1 ~ value | macro.type,fun=length)								# count distribution across Gear Type1 [gear.type1] and macro.type [macro.type]
		for (idx in names(sum.GearType1.macro.type.Count))
		{
			cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.macro.type.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.macro.type.Count] out
		}

		# ........ [Gear.Type2] vs [macro.type] ......................
		cat("\n\n\tCount distribution in [Gear.type2] and [macro.type]\n")
		cat("\n\nCount distribution in [Gear.type2] and [macro.type]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.GearType2.macro.type.Count <- cast(myData.tmp,gear.type1 ~ value | macro.type,fun=length)								# count distribution across Gear Type1 [gear.type2] and macro.type [macro.type]
		for (idx in names(sum.GearType2.macro.type.Count))
		{
			cat(paste("Count distribution in [Gear.type2] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType2.macro.type.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.macro.type.Count] out
		}

		# ........ [MA] vs [macro.type] ......................
		cat("\n\n\tCount distribution in [MA] and [macro.type]\n")
		cat("\n\nCount distribution in [MA] and [macro.type]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.MA.macro.type.Count <- cast(myData.tmp,MA ~ value | macro.type,fun=length)										# count distribution across MA [MA] and macro.type [macro.type]
		for (idx in names(sum.MA.macro.type.Count))
		{
			cat(paste("Count distribution in [MA] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.macro.type.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.MA.macro.type.Count] out
		}


		# ........ [MA] vs [MicroClass] ......................
		cat("\n\n\tCount distribution in [MA] and [MicroClass]\n")
		cat("\n\nCount distribution in [MA] and [MicroClass]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.MA.MicroClass.Count <- cast(myData.tmp,MA ~ value | MicroClass,fun=length)										# count distribution across MA [MA] and MicroClass [MicroClass]
		for (idx in names(sum.MA.MicroClass.Count))
		{
			cat(paste("Count distribution in [MA] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.MicroClass.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.MA.MicroClass.Count] out
		}

		# ........ [Gear] vs [MicroClass] ......................
		cat("\n\n\tCount distribution in [Gear] and [MicroClass]\n")
		cat("\n\nCount distribution in [Gear] and [MicroClass]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.GearType1.MicroClass.Count <- cast(myData.tmp,Gear ~ value | MicroClass,fun=length)									# count distribution across Gear Type1 [Gear] and MicroClass [MicroClass]
		for (idx in names(sum.GearType1.MicroClass.Count))
		{
			cat(paste("Count distribution in [Gear] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.MicroClass.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.MicroClass.Count] out
		}



		# ........ [Gear.Type1] vs [MicroClass] ......................
		cat("\n\n\tCount distribution in [Gear.type1] and [MicroClass]\n")
		cat("\n\nCount distribution in [Gear.type1] and [MicroClass]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.GearType1.MicroClass.Count <- cast(myData.tmp,gear.type1 ~ value | MicroClass,fun=length)								# count distribution across Gear Type1 [gear.type1] and MicroClass [MicroClass]
		for (idx in names(sum.GearType1.MicroClass.Count))
		{
			cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.MicroClass.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.MicroClass.Count] out
		}

		# ........ [Gear.Type2] vs [MicroClass] ......................
		cat("\n\n\tCount distribution in [Gear.type2] and [MicroClass]\n")
		cat("\n\nCount distribution in [Gear.type2] and [MicroClass]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.GearType2.MicroClass.Count <- cast(myData.tmp,gear.type1 ~ value | MicroClass,fun=length)								# count distribution across Gear Type1 [gear.type2] and MicroClass [MicroClass]
		for (idx in names(sum.GearType2.MicroClass.Count))
		{
			cat(paste("Count distribution in [Gear.type2] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType2.MicroClass.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.MicroClass.Count] out
		}

		# ........ [Gear] vs [Seg] ......................
		cat("\n\n\tCount distribution in [Gear] and [Seg]\n")
		cat("\n\nCount distribution in [Gear] and [Seg]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.GearType1.Seg.Count <- cast(myData.tmp,Gear ~ value | Seg,fun=length)										# count distribution across Gear Type1 [Gear] and Seg [Seg]
		for (idx in names(sum.GearType1.Seg.Count))
		{
			cat(paste("Count distribution in [Gear] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.Seg.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.GearType1.Seg.Count] out
		}


	
		# ........ [Gear.Type1] vs [Seg] ......................
		cat("\n\n\tCount distribution in [Gear.type1] and [Seg]\n")
		cat("\n\nCount distribution in [Gear.type1] and [Seg]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.GearType1.Seg.Count <- cast(myData.tmp,gear.type1 ~ value | Seg,fun=length)										# count distribution across Gear Type1 [gear.type1] and Seg [Seg]
		for (idx in names(sum.GearType1.Seg.Count))
		{
			cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.Seg.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.GearType1.Seg.Count] out
		}

		# ........ [Gear.Type2] vs [Seg] ......................
		cat("\n\n\tCount distribution in [Gear.Type2] and [Seg]\n")
		cat("\n\nCount distribution in [Gear.Type2] and [Seg]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.GearType2.Seg.Count <- cast(myData.tmp,gear.type2 ~ value | Seg,fun=length)										# count distribution across Gear Type1 [gear.type2] and Seg [Seg]
		for (idx in names(sum.GearType2.Seg.Count))
		{
			cat(paste("Count distribution in [Gear.Type2] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType2.Seg.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.GearType2.Seg.Count] out
		}


		# ........ [MA] vs [Lith_1] ......................
		cat("\n\n\tCount distribution in [MA] and [Lith_1]\n")
		cat("\n\nCount distribution in [MA] and [Lith_1]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.MA.Lith_1.Count <- cast(myData.tmp,MA ~ value | Lith_1,fun=length)											# count distribution across MA [MA] and Lith_1 [Lith_1]
		for (idx in names(sum.MA.Lith_1.Count))
		{
			cat(paste("Count distribution in [MA] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.Lith_1.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.MA.Lith_1.Count] out
		}


		# ........ [MA] vs [Tax_part_sz] ......................
		cat("\n\n\tCount distribution in [MA] and [Tax_part_sz]\n")
		cat("\n\nCount distribution in [MA] and [Tax_part_sz]\n",file=FL.Count.Summary.csv,append=TRUE)
		sum.MA.Tax_part_sz.Count <- cast(myData.tmp,MA ~ value | Tax_part_sz,fun=length)									# count distribution across MA [MA] and Tax_part_sz [Tax_part_sz]
		for (idx in names(sum.MA.Tax_part_sz.Count))
		{
			cat(paste("Count distribution in [MA] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.Tax_part_sz.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.MA.Tax_part_sz.Count] out
		}
	}


	# ........ [Year] vs [Seg] ......................
	cat("\n\n\tCount distribution in [Year] and [Seg]\n")
	cat("\n\nCount distribution in [Year] and [Seg]\n",file=FL.Count.Summary.csv,append=TRUE)
	sum.Year.Seg.Count <- cast(myData.tmp,SY ~ value | Seg,fun=length)												# count distribution across Year [Year] and Seg [Seg]
	for (idx in names(sum.Year.Seg.Count))
	{
		cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
		write.table(data.frame(sum.Year.Seg.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)				# write [sum.Year.Seg.Count] out
	}
	
	# ........ [Year] vs [macro.type] ......................
	cat("\n\n\tCount distribution in [Year] and [macro.type]\n")
	cat("\n\nCount distribution in [Year] and [macro.type]\n",file=FL.Count.Summary.csv,append=TRUE)
	sum.Year.macro.type.Count <- cast(myData.tmp,SY ~ value | macro.type,fun=length)										# count distribution across Year [Year] and macro.type [macro.type]
	for (idx in names(sum.Year.macro.type.Count))
	{
		cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
		write.table(data.frame(sum.Year.macro.type.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.Year.macro.type.Count] out
	}
	
	# ........ [Seg] vs [macro.type] ......................
	cat("\n\n\tCount distribution in [Seg] and [macro.type]\n")
	cat("\n\nCount distribution in [Seg] and [macro.type]\n",file=FL.Count.Summary.csv,append=TRUE)
	sum.Seg.macro.type.Count <- cast(myData.tmp,Seg ~ value | macro.type,fun=length)										# count distribution across Seg [Seg] and macro.type [macro.type]
	for (idx in names(sum.Seg.macro.type.Count))
	{
		cat(paste("Count distribution in [Seg] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
		write.table(data.frame(sum.Seg.macro.type.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.Seg.macro.type.Count] out
	}


	# ........ [MicroClass] vs [macro.type] ......................
	cat("\n\n\tCount distribution in [MicroClass] and [macro.type]\n")
	cat("\n\nCount distribution in [MicroClass] and [macro.type]\n",file=FL.Count.Summary.csv,append=TRUE)
	sum.MicroClass.macro.type.Count <- cast(myData.tmp,MicroClass ~ value | macro.type,fun=length)									# count distribution across MicroClass [MicroClass] and macro.type [macro.type]
	for (idx in names(sum.MicroClass.macro.type.Count))
	{
		cat(paste("Count distribution in [MicroClass] and [",idx,"]\n"),file=FL.Count.Summary.csv,append=TRUE)
		write.table(data.frame(sum.MicroClass.macro.type.Count[idx]),file=FL.Count.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.MicroClass.macro.type.Count] out
	}
	
	cat(paste("\t3. Count summary has been done for some single variables and some two variable combinations for [",subset.section,"] and [",subset.gear,"]!\n",sep=""))
	cat(paste("\t3. Count summary has been done for some single variables and some two variable combinations for [",subset.section,"] and [",subset.gear,"]!\n",sep=""),file=FL.LOG,append=TRUE)







	# -------------------------------------------------------------------------------------------------
	# 4. Presen/Absence summary:
	# -------------------------------------------------------------------------------------------------	
	FL.Binary.Summary.csv <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")_Binary_Summary.csv",sep=""),sep="/")
	if  (file.exists(FL.Binary.Summary.csv)){print(paste(FL.Binary.Summary.csv,"exist.Delete it!")); file.remove(FL.Binary.Summary.csv)}
	
	myData.tmp <- melt(myData.Sub,id.var=grep("[^binary]",names(myData.Sub),value=TRUE),measure.var="binary")

	sum.SY.Count        <- as.data.frame(cast(myData.tmp,SY          ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across sample year   [SY]
	sum.Season.Count    <- as.data.frame(cast(myData.tmp,Season      ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across Season        [Season]
	sum.Lith_1.Count    <- as.data.frame(cast(myData.tmp,Lith_1      ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across Lith_1        [Lith_1]
	sum.Lith_2.Count    <- as.data.frame(cast(myData.tmp,Lith_2      ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across Lith_2        [Lith_2]
	sum.Lith_desc.Count <- as.data.frame(cast(myData.tmp,Lith_desc   ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across Lith_desc     [Lith_desc]
	sum.Tax_part.Count  <- as.data.frame(cast(myData.tmp,Tax_part_sz ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across Tax_part      [Tax_part]
	sum.macro.type.Count<- as.data.frame(cast(myData.tmp,macro.type  ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across macro.type    [macro.type]
	sum.Micro.Count     <- as.data.frame(cast(myData.tmp,MicroClass  ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across MicroClass    [MicroClass]
	sum.Seg.Count       <- as.data.frame(cast(myData.tmp,Seg         ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across Seg           [Seg]
	sum.Gear.Count      <- as.data.frame(cast(myData.tmp,Gear        ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across Gear          [Gear]

	sum.Meso.Count      <- as.data.frame(cast(myData.tmp,new_meso    ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across Meso          [new_meso]
	sum.NFHAP.Count     <- as.data.frame(cast(myData.tmp,new_NFHAP   ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across NFHAP         [new_NFHAP]
	sum.part.Count      <- as.data.frame(cast(myData.tmp,new_part    ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across Meso          [new_part]
	sum.lith.Count      <- as.data.frame(cast(myData.tmp,new_lith    ~ value,fun=length,margins = c("grand_row","grand_col")))					# count distribution across Meso          [new_lith]

	if (idx.subset >= 6)
	{
		sum.MA.Count        <- as.data.frame(cast(myData.tmp,MA          ~ value,fun=length,margins = c("grand_row","grand_col")))				# count distribution across MA            [MA]
		sum.Gear1.Count     <- as.data.frame(cast(myData.tmp,gear.type1  ~ value,fun=length,margins = c("grand_row","grand_col")))				# count distribution across Gear.type1    [gear.type1]
		sum.Gear2.Count     <- as.data.frame(cast(myData.tmp,gear.type2  ~ value,fun=length,margins = c("grand_row","grand_col")))				# count distribution across Gear.type2    [gear.type2]	
	}


	#
	# single variable distribution
	#
	cat("Presence/Absence distribution in Year\n",             file=FL.Binary.Summary.csv,append=TRUE)
	write.table(sum.SY.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)								# write [sum.SY.Count] out

	cat("\n\n\tPresence/Absence distribution in Season\n",       file=FL.Binary.Summary.csv,append=TRUE)
	write.table(sum.Season.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Season.Count] out

	cat("\n\nPresence/Absence distribution in Lith_1\n",       file=FL.Binary.Summary.csv,append=TRUE)
	write.table(sum.Lith_1.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Lith_1.Count] out

	cat("\n\n\tPresence/Absence distribution in Lith_2\n",       file=FL.Binary.Summary.csv,append=TRUE)
	write.table(sum.Lith_2.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Lith_2.Count] out

	cat("\n\nPresence/Absence distribution in Lith_desc\n",    file=FL.Binary.Summary.csv,append=TRUE)
	write.table(sum.Lith_desc.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Lith_desc.Count] out

	cat("\n\n\tPresence/Absence distribution in macro.type\n",        file=FL.Binary.Summary.csv,append=TRUE)
	write.table(sum.macro.type.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.macro.type.Count] out

	cat("\n\nPresence/Absence distribution in Micro\n",        file=FL.Binary.Summary.csv,append=TRUE)
	write.table(sum.Micro.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Micro.Count] out

	cat("\n\n\tPresence/Absence distribution in Seg\n",          file=FL.Binary.Summary.csv,append=TRUE)
	write.table(sum.Seg.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Seg.Count] out

	cat("\n\nPresence/Absence distribution in Gear\n",         file=FL.Binary.Summary.csv,append=TRUE)
	write.table(sum.Gear.Count, file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.Gear.Count] out

	if (idx.subset >= 8)
	{
		cat("\n\nPresence/Absence distribution in MA\n",           file=FL.Binary.Summary.csv,append=TRUE)
		write.table(sum.MA.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)							# write [sum.MA.Count] out

		cat("\n\nPresence/Absence distribution in Gear Type 1\n"  ,file=FL.Binary.Summary.csv,append=TRUE)
		write.table(sum.Gear1.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Gear1.Count] out
	
		cat("\n\nPresence/Absence distribution in Gear Type 2\n",  file=FL.Binary.Summary.csv,append=TRUE)
		write.table(sum.Gear2.Count,file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Gear2.Count] out
	}

	#
	# two variable distribution
	#
	if (idx.subset >= 6)
	{	
		# ........ [Year] vs [Gear] ......................
		cat("\n\n\tPresence/Absence distribution in [Year] and [Gear]\n")
		cat("\n\nPresence/Absence distribution in [Year] and [Gear]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.SY.Gear1.Count <- cast(myData.tmp,SY ~ value | Gear,fun=length)											# count distribution across sample year   [SY] and [Gear]
		for (idx in names(sum.SY.Gear1.Count))
		{
			cat(paste("Presence/Absence distribution in [Year] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.SY.Gear1.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.SY.Gear1.Count] out
		}
	
	
		# ........ [Year] vs [Gear.Type1] ......................
		cat("\n\n\tPresence/Absence distribution in [Year] and [Gear.Type1]\n")
		cat("\n\nPresence/Absence distribution in [Year] and [Gear.Type1]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.SY.Gear1.Count <- cast(myData.tmp,SY ~ value | gear.type1,fun=length)										# count distribution across sample year   [SY] and [gear.type1]
		for (idx in names(sum.SY.Gear1.Count))
		{
			cat(paste("Presence/Absence distribution in [Year] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.SY.Gear1.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.SY.Gear1.Count] out
		}

		# ........ [Year] vs [Gear.Type2] ......................
		cat("\n\n\tPresence/Absence distribution in [Year] and [Gear.Type2]\n")
		cat("\n\nPresence/Absence distribution in [Year] and [Gear.Type2]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.SY.Gear2.Count <- cast(myData.tmp,SY ~ value | gear.type2,fun=length)										# count distribution across sample year   [SY] and [gear.type2]
		for (idx in names(sum.SY.Gear2.Count))
		{
			cat(paste("Presence/Absence distribution in [Year] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.SY.Gear2.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.SY.Gear2.Count] out
		}

		# ........ [MA] vs [Gear] ......................
		cat("\n\n\tPresence/Absence distribution in [MA] and [Gear]\n")
		cat("\n\nPresence/Absence distribution in [MA] and [Gear]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.MA.Gear1.Count <- cast(myData.tmp,MA ~ value | Gear,fun=length)											# count distribution across MA [MA] and [Gear]
		for (idx in names(sum.MA.Gear1.Count))
		{
			cat(paste("Presence/Absence distribution in [MA] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.Gear1.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.MA.Gear1.Count] out
		}


		# ........ [MA] vs [Gear.Type1] ......................
		cat("\n\n\tPresence/Absence distribution in [MA] and [Gear.Type1]\n")
		cat("\n\nPresence/Absence distribution in [MA] and [Gear.Type1]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.MA.Gear1.Count <- cast(myData.tmp,MA ~ value | gear.type1,fun=length)										# count distribution across MA [MA] and [gear.type1]
		for (idx in names(sum.MA.Gear1.Count))
		{
			cat(paste("Presence/Absence distribution in [MA] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.Gear1.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.MA.Gear1.Count] out
		}

		# ........ [MA] vs [Gear.Type2] ......................
		cat("\n\n\tPresence/Absence distribution in [MA] and [Gear.Type2]\n")
		cat("\n\nPresence/Absence distribution in [MA] and [Gear.Type2]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.MA.Gear2.Count <- cast(myData.tmp,MA ~ value | gear.type2,fun=length)										# count distribution across MA [MA] and [gear.type2]
		for (idx in names(sum.SY.Gear2.Count))
		{
			cat(paste("Presence/Absence distribution in [MA] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.Gear2.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.MA.Gear2.Count] out
		}

		# ........ [MA] vs [macro.type] ......................
		cat("\n\n\tPresence/Absence distribution in [MA] and [macro.type]\n")
		cat("\n\nPresence/Absence distribution in [MA] and [macro.type]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.MA.macro.type.Count <- cast(myData.tmp,MA ~ value | macro.type,fun=length)										# count distribution across MA [MA] and macro.type [macro.type]
		for (idx in names(sum.MA.macro.type.Count))
		{
			cat(paste("Presence/Absence distribution in [MA] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.macro.type.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.MA.macro.type.Count] out
		}

		# ........ [Gear] vs [macro.type] ......................
		cat("\n\n\tPresence/Absence distribution in [Gear] and [macro.type]\n")
		cat("\n\nPresence/Absence distribution in [Gear] and [macro.type]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.GearType1.macro.type.Count <- cast(myData.tmp,Gear ~ value | macro.type,fun=length)									# count distribution across Gear Type1 [Gear] and macro.type [macro.type]
		for (idx in names(sum.GearType1.macro.type.Count))
		{
			cat(paste("Presence/Absence distribution in [Gear] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.macro.type.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.macro.type.Count] out
		}



		# ........ [Gear.Type1] vs [macro.type] ......................
		cat("\n\n\tPresence/Absence distribution in [Gear.type1] and [macro.type]\n")
		cat("\n\nPresence/Absence distribution in [Gear.type1] and [macro.type]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.GearType1.macro.type.Count <- cast(myData.tmp,gear.type1 ~ value | macro.type,fun=length)								# count distribution across Gear Type1 [gear.type1] and macro.type [macro.type]
		for (idx in names(sum.GearType1.macro.type.Count))
		{
			cat(paste("Presence/Absence distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.macro.type.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.macro.type.Count] out
		}

		# ........ [Gear.Type2] vs [macro.type] ......................
		cat("\n\n\tPresence/Absence distribution in [Gear.type2] and [macro.type]\n")
		cat("\n\nPresence/Absence distribution in [Gear.type2] and [macro.type]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.GearType2.macro.type.Count <- cast(myData.tmp,gear.type1 ~ value | macro.type,fun=length)								# count distribution across Gear Type1 [gear.type2] and macro.type [macro.type]
		for (idx in names(sum.GearType2.macro.type.Count))
		{
			cat(paste("Presence/Absence distribution in [Gear.type2] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType2.macro.type.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.macro.type.Count] out
		}

		# ........ [MA] vs [macro.type] ......................
		cat("\n\n\tPresence/Absence distribution in [MA] and [macro.type]\n")
		cat("\n\nPresence/Absence distribution in [MA] and [macro.type]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.MA.macro.type.Count <- cast(myData.tmp,MA ~ value | macro.type,fun=length)										# count distribution across MA [MA] and macro.type [macro.type]
		for (idx in names(sum.MA.macro.type.Count))
		{
			cat(paste("Presence/Absence distribution in [MA] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.macro.type.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.MA.macro.type.Count] out
		}


		# ........ [MA] vs [MicroClass] ......................
		cat("\n\n\tPresence/Absence distribution in [MA] and [MicroClass]\n")
		cat("\n\nPresence/Absence distribution in [MA] and [MicroClass]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.MA.MicroClass.Count <- cast(myData.tmp,MA ~ value | MicroClass,fun=length)										# count distribution across MA [MA] and MicroClass [MicroClass]
		for (idx in names(sum.MA.MicroClass.Count))
		{
			cat(paste("Presence/Absence distribution in [MA] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.MicroClass.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.MA.MicroClass.Count] out
		}

		# ........ [Gear] vs [MicroClass] ......................
		cat("\n\n\tPresence/Absence distribution in [Gear] and [MicroClass]\n")
		cat("\n\nPresence/Absence distribution in [Gear] and [MicroClass]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.GearType1.MicroClass.Count <- cast(myData.tmp,Gear ~ value | MicroClass,fun=length)									# count distribution across Gear Type1 [Gear] and MicroClass [MicroClass]
		for (idx in names(sum.GearType1.MicroClass.Count))
		{
			cat(paste("Presence/Absence distribution in [Gear] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.MicroClass.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.MicroClass.Count] out
		}


		# ........ [Gear.Type1] vs [MicroClass] ......................
		cat("\n\n\tPresence/Absence distribution in [Gear.type1] and [MicroClass]\n")
		cat("\n\nPresence/Absence distribution in [Gear.type1] and [MicroClass]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.GearType1.MicroClass.Count <- cast(myData.tmp,gear.type1 ~ value | MicroClass,fun=length)								# count distribution across Gear Type1 [gear.type1] and MicroClass [MicroClass]
		for (idx in names(sum.GearType1.MicroClass.Count))
		{
			cat(paste("Presence/Absence distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.MicroClass.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.MicroClass.Count] out
		}

		# ........ [Gear.Type2] vs [MicroClass] ......................
		cat("\n\n\tPresence/Absence distribution in [Gear.type2] and [MicroClass]\n")
		cat("\n\nPresence/Absence distribution in [Gear.type2] and [MicroClass]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.GearType2.MicroClass.Count <- cast(myData.tmp,gear.type1 ~ value | MicroClass,fun=length)								# count distribution across Gear Type1 [gear.type2] and MicroClass [MicroClass]
		for (idx in names(sum.GearType2.MicroClass.Count))
		{
			cat(paste("Presence/Absence distribution in [Gear.type2] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType2.MicroClass.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.MicroClass.Count] out
		}

		# ........ [Gear] vs [Seg] ......................
		cat("\n\n\tPresence/Absence distribution in [Gear] and [Seg]\n")
		cat("\n\nPresence/Absence distribution in [Gear] and [Seg]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.GearType1.Seg.Count <- cast(myData.tmp,Gear ~ value | Seg,fun=length)										# count distribution across Gear Type1 [Gear] and Seg [Seg]
		for (idx in names(sum.GearType1.Seg.Count))
		{
			cat(paste("Presence/Absence distribution in [Gear] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.Seg.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.GearType1.Seg.Count] out
		}

	
		# ........ [Gear.Type1] vs [Seg] ......................
		cat("\n\n\tPresence/Absence distribution in [Gear.type1] and [Seg]\n")
		cat("\n\nPresence/Absence distribution in [Gear.type1] and [Seg]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.GearType1.Seg.Count <- cast(myData.tmp,gear.type1 ~ value | Seg,fun=length)										# count distribution across Gear Type1 [gear.type1] and Seg [Seg]
		for (idx in names(sum.GearType1.Seg.Count))
		{
			cat(paste("Presence/Absence distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType1.Seg.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.GearType1.Seg.Count] out
		}

		# ........ [Gear.Type2] vs [Seg] ......................
		cat("\n\n\tPresence/Absence distribution in [Gear.Type2] and [Seg]\n")
		cat("\n\nPresence/Absence distribution in [Gear.Type2] and [Seg]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.GearType2.Seg.Count <- cast(myData.tmp,gear.type2 ~ value | Seg,fun=length)										# count distribution across Gear Type1 [gear.type2] and Seg [Seg]
		for (idx in names(sum.GearType2.Seg.Count))
		{
			cat(paste("Presence/Absence distribution in [Gear.Type2] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.GearType2.Seg.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.GearType2.Seg.Count] out
		}


		# ........ [MA] vs [Lith_1] ......................
		cat("\n\n\tPresence/Absence distribution in [MA] and [Lith_1]\n")
		cat("\n\nPresence/Absence distribution in [MA] and [Lith_1]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.MA.Lith_1.Count <- cast(myData.tmp,MA ~ value | Lith_1,fun=length)											# count distribution across MA [MA] and Lith_1 [Lith_1]
		for (idx in names(sum.MA.Lith_1.Count))
		{
			cat(paste("Presence/Absence distribution in [MA] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.Lith_1.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.MA.Lith_1.Count] out
		}


		# ........ [MA] vs [Tax_part_sz] ......................
		cat("\n\n\tPresence/Absence distribution in [MA] and [Tax_part_sz]\n")
		cat("\n\nPresence/Absence distribution in [MA] and [Tax_part_sz]\n",file=FL.Binary.Summary.csv,append=TRUE)
		sum.MA.Tax_part_sz.Count <- cast(myData.tmp,MA ~ value | Tax_part_sz,fun=length)									# count distribution across MA [MA] and Tax_part_sz [Tax_part_sz]
		for (idx in names(sum.MA.Tax_part_sz.Count))
		{
			cat(paste("Presence/Absence distribution in [MA] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
			write.table(data.frame(sum.MA.Tax_part_sz.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.MA.Tax_part_sz.Count] out
		}
	}


	# ........ [Year] vs [Seg] ......................
	cat("\n\n\tPresence/Absence distribution in [Year] and [Seg]\n")
	cat("\n\nPresence/Absence distribution in [Year] and [Seg]\n",file=FL.Binary.Summary.csv,append=TRUE)
	sum.Year.Seg.Count <- cast(myData.tmp,SY ~ value | Seg,fun=length)												# count distribution across Year [Year] and Seg [Seg]
	for (idx in names(sum.Year.Seg.Count))
	{
		cat(paste("Presence/Absence distribution in [Year] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
		write.table(data.frame(sum.Year.Seg.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)				# write [sum.Year.Seg.Count] out
	}
	
	# ........ [Year] vs [macro.type] ......................
	cat("\n\n\tPresence/Absence distribution in [Year] and [macro.type]\n")
	cat("\n\nPresence/Absence distribution in [Year] and [macro.type]\n",file=FL.Binary.Summary.csv,append=TRUE)
	sum.Year.macro.type.Count <- cast(myData.tmp,SY ~ value | macro.type,fun=length)										# count distribution across Year [Year] and macro.type [macro.type]
	for (idx in names(sum.Year.macro.type.Count))
	{
		cat(paste("Presence/Absence distribution in [Year] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
		write.table(data.frame(sum.Year.macro.type.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.Year.macro.type.Count] out
	}
	
	
	# ........ [Seg] vs [macro.type] ......................
	cat("\n\n\tPresence/Absence distribution in [Seg] and [macro.type]\n")
	cat("\n\nPresence/Absence distribution in [Seg] and [macro.type]\n",file=FL.Binary.Summary.csv,append=TRUE)
	sum.Seg.macro.type.Count <- cast(myData.tmp,Seg ~ value | macro.type,fun=length)										# count distribution across Seg [Seg] and macro.type [macro.type]
	for (idx in names(sum.Seg.macro.type.Count))
	{
		cat(paste("Presence/Absence distribution in [Seg] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
		write.table(data.frame(sum.Seg.macro.type.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)			# write [sum.Seg.macro.type.Count] out
	}


	# ........ [MicroClass] vs [macro.type] ......................
	cat("\n\n\tPresence/Absence distribution in [MicroClass] and [macro.type]\n")
	cat("\n\nPresence/Absence distribution in [MicroClass] and [macro.type]\n",file=FL.Binary.Summary.csv,append=TRUE)
	sum.MicroClass.macro.type.Count <- cast(myData.tmp,MicroClass ~ value | macro.type,fun=length)									# count distribution across MicroClass [MicroClass] and macro.type [macro.type]
	for (idx in names(sum.MicroClass.macro.type.Count))
	{
		cat(paste("Presence/Absence distribution in [MicroClass] and [",idx,"]\n"),file=FL.Binary.Summary.csv,append=TRUE)
		write.table(data.frame(sum.MicroClass.macro.type.Count[idx]),file=FL.Binary.Summary.csv,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.MicroClass.macro.type.Count] out
	}
	
	
	#
	# one more table for number of Presence/Absence in terms of "Gear", "Segment" combination
	# The fifference between the following two:
	# (a) cast(myData.Sub,Gear~Seg|binary,fun=length,value="Meso")	                                                    Value field can be any fields other than the three already used.
	# (b) cast(as.data.frame(table(myData.Sub[,c("Gear","Seg","binary")])),Gear ~ Seg | binary,FUN=length,value="Freq")
	#     (a) will only keep the combination with data and (b) will keep all combinations by assigning 0.
	#
	sum.count.presence_absence <- cast(as.data.frame(table(myData.Sub[,c("Gear","Seg","binary")])),Gear ~ Seg | binary,FUN=length,value="Freq")	
	for (idx in names(sum.count.presence_absence))
	{
		if (idx == "yes")							# because binary has "yes" and "no" values
		{
			count.yes <- data.frame(sum.count.presence_absence[idx])	# this will add "yes." in the column names
			names(count.yes) <- sub("yes.","",names(count.yes))
		}else if (idx == "no"){
			count.no  <- data.frame(sum.count.presence_absence[idx])	# this will add "no."  in the column names
			names(count.no) <- sub("no.","",names(count.no))
		}
	}
	count.all        <- count.no
	count.all[,-1]   <- count.no[,-1] + count.yes[,-1]
	count.ratio      <- count.all
	count.ratio[,-1] <- round(100*(count.yes[,-1] / count.all[,-1]),digits=2)
	count.out        <- cbind(count.no,count.yes,count.all,count.ratio)
	cat(paste("Absence|Presence|Total|Ratio distribution in [Gear] and [Segment],"),file=FL.Binary.Summary.csv,append=TRUE)
	write.table(count.out,file=FL.Binary.Summary.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)		
	cat(paste("\t4. Presence/Absence summary has been done for some single variables and some two variable combinations for [",subset.section,"] and [",subset.gear,"]!\n",sep=""))
	cat(paste("\t4. Presence/Absence summary has been done for some single variables and some two variable combinations for [",subset.section,"] and [",subset.gear,"]!\n",sep=""),file=FL.LOG,append=TRUE)
	
	#
	# 5. check the relationship between count and CPUE/CPUA interm of each single gear type variant in each segment
	#
	FL.Count_CPUE_CPUA.PDF <- paste(Path.Out.subset,paste("12_indVar_Gear(",subset.gear,")_River(",subset.section,")_CPUE_CPUA_Count.pdf",sep=""),sep="/")	
	if  (file.exists(FL.Count_CPUE_CPUA.PDF)){print(paste(FL.Count_CPUE_CPUA.PDF,"exist.Delete it!")); file.remove(FL.Count_CPUE_CPUA.PDF)}

	# open the pdf file
	pdf(file = FL.Count_CPUE_CPUA.PDF,paper="a4r",width=0,height=0)	
	if (idx.subset == 1 | idx.subset == 3)
	{
		# all segments
		plot.obj1 <- xyplot(CPUA ~ Pal_cnt   | Gear,data=myData.Sub,type="p",xlab="Count",    ylab="CPUA",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot.obj2 <- xyplot(CPUA ~ CPUA.calc | Gear,data=myData.Sub,type="p",xlab="CPUA.calc",ylab="CPUA",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot(plot.obj1,split=c(1,1,1,2))
		plot(plot.obj2,split=c(1,2,1,2),newpage=FALSE)
		
		# segment-wise
		plot.obj3 <- xyplot(CPUA ~ Pal_cnt   | Gear + Seg,data=myData.Sub,between = list(x=0.5,y=0.5),type="p",xlab="Count",    ylab="CPUA",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot.obj4 <- xyplot(CPUA ~ CPUA.calc | Gear + Seg,data=myData.Sub,between = list(x=0.5,y=0.5),type="p",xlab="CPUA.calc",ylab="CPUA",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot(plot.obj3)
		plot(plot.obj4)		
		
	}else if(idx.subset == 2 | idx.subset == 4 | idx.subset == 5){
		plot.obj1 <- xyplot(CPUE ~ Pal_cnt   | Gear,data=myData.Sub,type="p",xlab="Count",    ylab="CPUE",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot.obj2 <- xyplot(CPUE ~ CPUE.calc | Gear,data=myData.Sub,type="p",xlab="CPUE.calc",ylab="CPUE",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot(plot.obj1,split=c(1,1,1,2))
		plot(plot.obj2,split=c(1,2,1,2),newpage=FALSE)	
		
		# segment-wise
		plot.obj3 <- xyplot(CPUE ~ Pal_cnt   | Gear + Seg,data=myData.Sub,between = list(x=0.5,y=0.5),type="p",xlab="Count",    ylab="CPUE",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot.obj4 <- xyplot(CPUE ~ CPUE.calc | Gear + Seg,data=myData.Sub,between = list(x=0.5,y=0.5),type="p",xlab="CPUE.calc",ylab="CPUE",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot(plot.obj3)
		plot(plot.obj4)	
	}else if(idx.subset == 6 | idx.subset == 7){
		# all segments
		plot.obj1 <- xyplot(CPUA ~ Pal_cnt   | Gear,data=myData.Sub,type="p",xlab="Count",    ylab="CPUA",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot.obj2 <- xyplot(CPUE ~ Pal_cnt   | Gear,data=myData.Sub,type="p",xlab="Count",    ylab="CPUE",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})

		plot.obj3 <- xyplot(CPUA ~ CPUA.calc | Gear,data=myData.Sub,type="p",xlab="CPUA.calc",ylab="CPUA",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot.obj4 <- xyplot(CPUE ~ CPUE.calc | Gear,data=myData.Sub,type="p",xlab="CPUE.calc",ylab="CPUE",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})

		plot(plot.obj1,split=c(1,1,2,2))
		plot(plot.obj2,split=c(1,2,2,2),newpage=FALSE)
		plot(plot.obj3,split=c(2,1,2,2),newpage=FALSE)
		plot(plot.obj4,split=c(2,2,2,2),newpage=FALSE)	
		
		# segment-wise
		plot.obj5 <- xyplot(CPUA ~ Pal_cnt   | Gear + Seg,data=myData.Sub,type="p",xlab="Count",    ylab="CPUA",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot.obj6 <- xyplot(CPUE ~ Pal_cnt   | Gear + Seg,data=myData.Sub,type="p",xlab="Count",    ylab="CPUE",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})

		plot.obj7 <- xyplot(CPUA ~ CPUA.calc | Gear + Seg,data=myData.Sub,type="p",xlab="CPUA.calc",ylab="CPUA",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot.obj8 <- xyplot(CPUE ~ CPUE.calc | Gear + Seg,data=myData.Sub,type="p",xlab="CPUE.calc",ylab="CPUE",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot(plot.obj5)
		plot(plot.obj6)	
		plot(plot.obj7)
		plot(plot.obj8)			
		
	}else if(idx.subset == 8){
		plot.obj1 <- xyplot(CPUA ~ Pal_cnt   | Gear,data=myData.Sub,type="p",xlab="Count",    ylab="CPUA",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot.obj2 <- xyplot(CPUE ~ Pal_cnt   | Gear,data=myData.Sub,type="p",xlab="Count",    ylab="CPUE",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})

		plot.obj3 <- xyplot(CPUA ~ CPUA.calc | Gear,data=myData.Sub,type="p",xlab="CPUA.calc",ylab="CPUA",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})
		plot.obj4 <- xyplot(CPUE ~ CPUE.calc | Gear,data=myData.Sub,type="p",xlab="CPUE.calc",ylab="CPUE",main=paste(subset.section,subset.gear,sep="_"),strip=strip.custom(strip.levels=TRUE,strip.names=FALSE),panel=function(...){panel.xyplot(...);panel.abline(a=0,b=1)})

		plot(plot.obj1,split=c(1,1,2,2))
		plot(plot.obj2,split=c(1,2,2,2),newpage=FALSE)
		plot(plot.obj3,split=c(2,1,2,2),newpage=FALSE)
		plot(plot.obj4,split=c(2,2,2,2),newpage=FALSE)		
	}	
	
	dev.off()
	cat(paste("\t5. Checked the CPUE|CPUA/Count relationship for [",subset.section,"] and [",subset.gear,"]!\n",sep=""))
	cat(paste("\t5. Checked the CPUE|CPUA/Count relationship for [",subset.section,"] and [",subset.gear,"]!\n",sep=""),file=FL.LOG,append=TRUE)
}


#
# save the subset of data
#
save(myData.Upper_Active,myData.Upper_TLC,myData.Lower_Active,myData.Lower_TLC,myData.Lower_GN,myData.Upper_Logistic,myData.Lower_Logistic,myData.All,col.title.Upper_Active,col.title.Upper_TLC,col.title.Lower_Active,col.title.Lower_TLC,col.title.Lower_GN,col.title.Upper_Logistic,col.title.Lower_Logistic,col.title.All,col.class.Upper_Active,col.class.Upper_TLC,col.class.Lower_Active,col.class.Lower_TLC,col.class.Lower_GN,col.class.Upper_Logistic,col.class.Lower_Logistic,col.class.All,file=FL.Data.OBJ)	
cat(paste("\t6. Subset data of all subsets have been saved!\n",sep=""))
cat(paste("\t6. Subset data of all subsets have been saved!\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n12_Prep_Subset_Data_for_var_kept_Final.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n12_Prep_Subset_Data_for_var_kept_Final.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [12_Prep_Subset_Data_for_var_kept_Final.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [12_Prep_Subset_Data_for_var_kept_Final.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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






