#
# 4_Chk_May14_2013Data.R
#
# Chris Vernon prepared a revised data sheet on April 24, 2013 with (1) revised micro habitat and (2) several distance to stock location vaiables.
# The data has been pre-processed in excel in the following way before saved as the csv format to be used by this script.
# (1) replace all 1,421,420 NULL  with empty field
# (2) replace all    23,684 NaN   with empty field
# (3) replace all    24,095 -9999 with empty field
# (4) replace 8     "#DIV/0!"     with empty field in the CPUE field
#
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
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/4_Chk_May14_2013Data"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}

# data file
FL.Data.IN   <- paste(Path.Data.IN,"MoRiver_Variables_2013May14_YLX.csv",sep="/")
FL.LOG       <- paste(Path.log,"4_Chk_May14_2013Data.log",sep="/")	
FL.SUM.cat   <- paste(Path.Out,"4_Chk_May14_2013Data_categoricalSum.csv",sep="/")
FL.SUM.num   <- paste(Path.Out,"4_Chk_May14_2013Data_numericSum.csv",sep="/")
FL.Summary   <- paste(Path.Out,"4_Chk_May14_2013Data_summary.csv",sep="/")
FL.missing   <- paste(Path.Out,"4_Chk_May14_2013Data_missing.csv",sep="/")
if (!file.exists(FL.Data.IN))  {stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.LOG))      {print(paste(FL.LOG,      "exist.Delete it!")); file.remove(FL.LOG)}
if  (file.exists(FL.SUM.cat))  {print(paste(FL.SUM.cat,  "exist.Delete it!")); file.remove(FL.SUM.cat)}
if  (file.exists(FL.SUM.num))  {print(paste(FL.SUM.num,  "exist.Delete it!")); file.remove(FL.SUM.num)}
if  (file.exists(FL.Summary))  {print(paste(FL.Summary,  "exist.Delete it!")); file.remove(FL.Summary)}
if  (file.exists(FL.missing))  {print(paste(FL.missing,  "exist.Delete it!")); file.remove(FL.missing)}


library("lattice")
library("reshape")



# read April 24, 2013 data 
row.titles  <- c("ID",     "SY",     "FieldOffice","Project","UniqueID","Gear",  "Season","Bend",   "BendRN","BendRiverMile","Near_NHD2rm_FID","Near_NHD2RM_dist_m","NHD2_RM","X1960BendID","RM1960_RM","UpDamDist","UpDamNm","DnDamDist","DnDamNm","Ch_W_Full","Ch_W_NoIsl","Braiding","UpTrib","UpTribDist","DnTrib","DTribDist","Reach", "Mean_z", "Max_z",  "Min_z",  "Grade10RM","GradeBend","Rocktype1","Rocktype2","Lithology","NFHAP_Scr","MedFlow","MeanFlow","taxorder","taxpartsiz","comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Segment","nPage",  "TotalPages","SetDate",  "Replicate","ReplicateRN","SubSample","SubSamplePass","SubSampleN","Biologist","Recorder", "CheckBy",  "Temp",   "Turbidity","Conductivity","DO",     "Distance","Width",  "NetRiverMile","StructureNumber","USGS",  "RiverStage","Discharge","U1",       "U2",       "U3",       "U4",       "U5",       "U6",       "U7",       "Macro", "MacroSW","Meso",  "MesoSW","Micro", "MicroClass","StartTime","StopTime", "DecimalTimeDifference","StartLatitude","StopLatitude","StartLongitude","StopLongitude","Depth1", "Depth2", "Depth3", "Velocity02or06_1","Velocity02or06_2","Velocity02or06_3","Velocity08_1","Velocity08_2","Velocity08_3","VelocityBot1","VelocityBot2","VelocityBot3","WaterVel","HabitatRN","Cobble", "silt",   "Sand",   "Gravel", "Organic","QC",       "Comments", "MappingBox","Total_Fish_Count","Pallid_Only_Count","CPUE",   "CPU_Area","Alt_Pallid_Only_Count","Alt_CPUE","Alt_CPU_Area","S_U10_D2_Near","S_U10_D2_Cumu","S_U20_D4_Near","S_U20_D4_Cumu","S_U30_D6_Near","S_U30_D6_Cumu","S_U50_D10_Near","S_U50_D10_Cumu","S_U100_D20_Near","S_U100_D20_Cumu","gear.type1","gear.type2","macro.type")
colClasses  <- c("integer","integer","factor",     "integer","integer", "factor","factor","integer","factor","numeric",      "integer",        "numeric",           "numeric","integer",    "numeric",  "numeric",  "factor", "numeric",  "factor", "numeric",  "numeric",   "integer", "factor","numeric",   "factor","numeric",  "factor","numeric","numeric","numeric","numeric",  "numeric",  "factor",   "factor",   "factor",   "factor",   "numeric","numeric", "factor",  "factor",    "numeric",  "numeric",    "numeric",    "numeric",    "numeric", "integer","integer","integer",   "factor",   "logic",    "logic",      "integer",  "integer",      "character", "character","character","character","numeric","numeric",  "numeric",     "numeric","numeric", "numeric","numeric",     "factor",         "factor","numeric",   "numeric",  "character","character","character","character","character","character","character","factor","factor", "factor","factor","factor","factor",    "character","character","numeric",              "numeric",      "numeric",     "numeric",       "numeric",      "numeric","numeric","numeric","numeric",         "numeric",         "numeric",         "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric",     "numeric", "character","numeric","numeric","numeric","numeric","numeric","character","character","character", "integer",         "inetger",          "numeric","numeric", "integer",              "integer", "numeric",     "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",      "numeric",       "numeric",       "numeric",        "numeric",        "factor",    "factor",    "factor")
used.4.plot <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     TRUE,    TRUE,    TRUE,     FALSE,   TRUE,           FALSE,            FALSE,               FALSE,    FALSE,        TRUE,       TRUE,       FALSE,    FALSE,      FALSE,    TRUE,       TRUE,        TRUE,      FALSE,   TRUE,        FALSE,   TRUE,       TRUE,    TRUE,     TRUE,     TRUE,     TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       TRUE,     TRUE,      TRUE,      TRUE,        TRUE,       TRUE,         TRUE,         TRUE,         TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     FALSE,      FALSE,         FALSE,    TRUE,      FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    TRUE,    FALSE,   TRUE,    TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          TRUE,     TRUE,     TRUE,     FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,        TRUE,        TRUE)
used.4.anal <- c( FALSE,    TRUE,     FALSE,        FALSE,    FALSE,     FALSE,   TRUE,    FALSE,    FALSE,   FALSE,          FALSE,            FALSE,               FALSE,    FALSE,        FALSE,      TRUE,       FALSE,    FALSE,      FALSE,    TRUE,       FALSE,       TRUE,      FALSE,   TRUE,        FALSE,   TRUE,       TRUE,    FALSE,    FALSE,    FALSE,    TRUE,       FALSE,      TRUE,       FALSE,      FALSE,      TRUE,       FALSE,    FALSE,     FALSE,     TRUE,        TRUE,       TRUE,         TRUE,         FALSE,        TRUE,      TRUE,     FALSE,    FALSE,       FALSE,      FALSE,      FALSE,        FALSE,      FALSE,          FALSE,       FALSE,      FALSE,      FALSE,      TRUE,     TRUE,       FALSE,         FALSE,    FALSE,     FALSE,    FALSE,         FALSE,            FALSE,   FALSE,       FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,    FALSE,    FALSE,   FALSE,   FALSE,   TRUE,        FALSE,      FALSE,      FALSE,                  FALSE,          FALSE,         FALSE,           FALSE,          FALSE,    FALSE,    FALSE,    FALSE,             FALSE,             FALSE,             FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         FALSE,         TRUE,      FALSE,      FALSE,    FALSE,    FALSE,    FALSE,    FALSE,    FALSE,      FALSE,      FALSE,       TRUE,              TRUE,               TRUE,     TRUE,      FALSE,                  FALSE,     FALSE,         TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,           TRUE,            TRUE,            TRUE,             TRUE,             TRUE,        TRUE,        TRUE)


# the raw data including all variables
      myData.raw  <- read.csv(file=FL.Data.IN,sep=",",header=TRUE,stringsAsFactors=TRUE)
names(myData.raw) <- row.titles

myData.raw <- cbind(myData.raw,binary = rep("no",dim(myData.raw)[1]),stringsAsFactors=FALSE)
myData.raw[myData.raw[,"Pallid_Only_Count"] > 0,"binary"] <- "Yes"

#
# add a mean depth field
#
myData.raw[,"Depth.mean"] <- apply(myData.raw[,c("Depth1","Depth2","Depth3")],1,mean,na.rm=TRUE)
row.titles  <- c(row.titles,"Depth.mean")
colClasses  <- c(colClasses, "numeric")
used.4.plot <- c(used.4.plot,TRUE)
used.4.anal <- c(used.4.anal,TRUE)


#
# variable specific check
#
# histogram(~Pallid_Only_Count | SY,data=myData.raw,strip=strip.custom(style=4),plot.points=FALSE,ref=TRUE,type="count",nint=100,layout=c(3,2),aspect="fill",auto.key="FALSE",col="red",border="red",scale=list(x=list(limits=range(-1,13),at=seq(0,12))),as.table=TRUE,key=key.string)



# -------------------------------------------------------------------------------------------------
# I. Check the 10 recently added variables: distance to the stocking stations
# -------------------------------------------------------------------------------------------------
# open pdf file for outputting plots
FL.stock.PDF <- paste(Path.Out,"4_Chk_May14_2013Data_stock_var.pdf",sep="/")	
if  (file.exists(FL.stock.PDF)){print(paste(FL.stock.PDF,"exist.Delete it!")); file.remove(FL.stock.PDF)}
pdf(file = FL.stock.PDF,paper="a4r",width=0,height=0)	



var.new      <-             grep("S_U",names(myData.raw),value=TRUE)
var.new.near <- grep("Near",grep("S_U",names(myData.raw),value=TRUE),value=TRUE)
var.new.cumu <- grep("Cumu",grep("S_U",names(myData.raw),value=TRUE),value=TRUE)


# # 1a. histogram of near distance
# plot.near1 <- histogram(myData.raw[,"S_U10_D2_Near"],  nint=100,col="red")
# plot.near2 <- histogram(myData.raw[,"S_U20_D4_Near"],  nint=100,col="red")
# plot.near3 <- histogram(myData.raw[,"S_U30_D6_Near"],  nint=100,col="red")
# plot.near4 <- histogram(myData.raw[,"S_U50_D10_Near"], nint=100,col="red")
# plot.near5 <- histogram(myData.raw[,"S_U100_D20_Near"],nint=100,col="red")
# 
# # 1b. histogram of cumu distance
# plot.cumu1 <- histogram(myData.raw[,"S_U10_D2_Cumu"],  nint=100,col="blue")
# plot.cumu2 <- histogram(myData.raw[,"S_U20_D4_Cumu"],  nint=100,col="blue")
# plot.cumu3 <- histogram(myData.raw[,"S_U30_D6_Cumu"],  nint=100,col="blue")
# plot.cumu4 <- histogram(myData.raw[,"S_U50_D10_Cumu"], nint=100,col="blue")
# plot.cumu5 <- histogram(myData.raw[,"S_U100_D20_Cumu"],nint=100,col="blue")
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
# 1. histogram of the distance variables
par(mfrow = c(5,2))
hist(myData.raw[,"S_U10_D2_Near"],  xlab="",ylab = paste("S_U10_D2_Near",  sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
hist(myData.raw[,"S_U10_D2_Cumu"],  xlab="",ylab = paste("S_U10_D2_Cumu",  sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	

hist(myData.raw[,"S_U20_D4_Near"],  xlab="",ylab = paste("S_U20_D4_Near",  sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
hist(myData.raw[,"S_U20_D4_Cumu"],  xlab="",ylab = paste("S_U20_D4_Cumu",  sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	

hist(myData.raw[,"S_U30_D6_Near"],  xlab="",ylab = paste("S_U30_D6_Near",  sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
hist(myData.raw[,"S_U30_D6_Cumu"],  xlab="",ylab = paste("S_U30_D6_Cumu",  sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	

hist(myData.raw[,"S_U50_D10_Near"], xlab="",ylab = paste("S_U50_D10_Near", sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
hist(myData.raw[,"S_U50_D10_Cumu"], xlab="",ylab = paste("S_U50_D10_Cumu", sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	

hist(myData.raw[,"S_U100_D20_Near"],xlab="",ylab = paste("S_U100_D20_Near",sep=""),main="",col="red", border="red", between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	
hist(myData.raw[,"S_U100_D20_Cumu"],xlab="",ylab = paste("S_U100_D20_Cumu",sep=""),main="",col="blue",border="blue",between = list(x=0.5,y=0.5),as.table = TRUE,scales = list(x= "free",y="free"),nclass = 100,table=TRUE,freq=FALSE)	




# 2a. scatter plot among different radiu of near distance
par(mfrow = c(2,1))
  plot(myData.raw[,"S_U100_D20_Near"],myData.raw[,"S_U50_D10_Near"],col="black",main="black: U50, cyan:U30, magenta: U20, green: U10");abline(a=0,b=1,col="red")
points(myData.raw[,"S_U100_D20_Near"],myData.raw[,"S_U30_D6_Near"], col="cyan")
points(myData.raw[,"S_U100_D20_Near"],myData.raw[,"S_U20_D4_Near"], col="magenta")
points(myData.raw[,"S_U100_D20_Near"],myData.raw[,"S_U10_D2_Near"], col="green")

# 2b. scatter plot among different radiu of cumudistance
  plot(myData.raw[,"S_U100_D20_Cumu"],myData.raw[,"S_U50_D10_Cumu"],col="black",main="black: U50, cyan:U30, magenta: U20, green: U10");abline(a=0,b=1,col="red")
points(myData.raw[,"S_U100_D20_Cumu"],myData.raw[,"S_U30_D6_Cumu"], col="cyan")
points(myData.raw[,"S_U100_D20_Cumu"],myData.raw[,"S_U20_D4_Cumu"], col="magenta")
points(myData.raw[,"S_U100_D20_Cumu"],myData.raw[,"S_U10_D2_Cumu"], col="green")

# 3. scatter plots between near and cumu distance
par(mfrow = c(2,3))
  plot(myData.raw[,"S_U10_D2_Near"],  myData.raw[,"S_U10_D2_Cumu"],  xlab = "near",ylab="cumu",main = "U10_D2")
  plot(myData.raw[,"S_U20_D4_Near"],  myData.raw[,"S_U20_D4_Cumu"],  xlab = "near",ylab="cumu",main = "U20_D4")
  plot(myData.raw[,"S_U30_D6_Near"],  myData.raw[,"S_U30_D6_Cumu"],  xlab = "near",ylab="cumu",main = "U30_D6")
  plot(myData.raw[,"S_U50_D10_Near"], myData.raw[,"S_U50_D10_Cumu"], xlab = "near",ylab="cumu",main = "U50_D10")
  plot(myData.raw[,"S_U100_D20_Near"],myData.raw[,"S_U100_D20_Cumu"],xlab = "near",ylab="cumu",main = "U100_D20")
  
  
# 4. scatter plot between stocking station distance and the sturgeon catch number
par(mfrow = c(2,3))
boxplot(S_U10_D2_Near   ~ Pallid_Only_Count,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U10_D2_Near  ")
boxplot(S_U20_D4_Near   ~ Pallid_Only_Count,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U20_D4_Near  ")
boxplot(S_U30_D6_Near   ~ Pallid_Only_Count,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U30_D6_Near  ")
boxplot(S_U50_D10_Near  ~ Pallid_Only_Count,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U50_D10_Near ")
boxplot(S_U100_D20_Near ~ Pallid_Only_Count,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U100_D20_Near")

par(mfrow = c(2,3))
boxplot(S_U10_D2_Cumu   ~ Pallid_Only_Count,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U10_D2_Cumu  ")
boxplot(S_U20_D4_Cumu   ~ Pallid_Only_Count,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U20_D4_Cumu  ")
boxplot(S_U30_D6_Cumu   ~ Pallid_Only_Count,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U30_D6_Cumu  ")
boxplot(S_U50_D10_Cumu  ~ Pallid_Only_Count,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U50_D10_Cumu ")
boxplot(S_U100_D20_Cumu ~ Pallid_Only_Count,data=myData.raw,notch=TRUE,outline=FALSE,range=0,col=c("red","blue","cyan","magenta","green","brown","yellow","pink","purple"),xlab="Pallid Count",ylab="S_U100_D20_Cumu")




# check the binary variable of the stocking station distance against the sturgeon counting
# turn the NULL in the stocking variables into one category and the non-zero distance into another category
myData.raw[,"stocking"] <- rep("yes",dim(myData.raw)[1])
myData.raw[!(is.na(myData.raw[,"S_U100_D20_Near"])),"stocking"] <- "no"
# -------------------------------------------------------------------------------------------------

dev.off();



# -------------------------------------------------------------------------------------------------
# II. Check the newly assigned micro habitat variable
# -------------------------------------------------------------------------------------------------
# open pdf file for outputting plots
FL.microHabitat <- paste(Path.Out,"4_Chk_May14_2013Data_microHabitat.pdf",sep="/")	
if  (file.exists(FL.microHabitat)){print(paste(FL.microHabitat,"exist.Delete it!")); file.remove(FL.microHabitat)}
pdf(file = FL.microHabitat,paper="a4r",width=0,height=0)	




myData.Lower.micro <- myData.raw[myData.raw[,"Reach"]=="Lower",c("macro.type","gear.type1","gear.type2","MicroClass","Pallid_Only_Count")]
myData.Lower.micro <- cbind(myData.Lower.micro,binary = rep("no",dim(myData.Lower.micro)[1]),stringsAsFactors=FALSE)
myData.Lower.micro[myData.Lower.micro[,"Pallid_Only_Count"] > 0,"binary"] <- "Yes"

# get the frequency
table(myData.Lower.micro[,c("binary",           "MicroClass")])
table(myData.Lower.micro[,c("Pallid_Only_Count","MicroClass")])

dev.off()

# -------------------------------------------------------------------------------------------------
# III. Check the correlation of the three percentage to avoid closure
# -------------------------------------------------------------------------------------------------
FL.sandClay <- paste(Path.Out,"4_Chk_May14_2013Data_sandClay.pdf",sep="/")	
pdf(file = FL.sandClay,paper="a4r",width=0,height=0)	


if  (file.exists(FL.sandClay)){print(paste(FL.sandClay,"exist.Delete it!")); file.remove(FL.sandClay)}

plot.obj1 <- xyplot(sandtotal_r~claytotal_r,data=myData.raw,type="p",pch=16,col="red",panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
plot.obj2 <- xyplot(sandtotal_r~claytotal_r | Reach,data=myData.raw,type="p",pch=16,col="red",panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")},as.table=TRUE)

plot(plot.obj1)
plot(plot.obj2)

dev.off()





# since the missing entries (-9999, NaN, NULL) in the excel spread sheet has been set to empty, we need to reset the empty entries of the factor variable to NA in order to count the missing numbers
for (idx in seq(from=1,to=dim(myData.raw)[2]))
{
	if (is.factor(myData.raw[,idx]))
	{
		myData.raw[myData.raw[,idx]=="",idx] <- NA
	}
}
cat(paste("empty entries in the factor variables have been re-assigned to [NA]\n",sep=""))



# -------------------------------------------------------------------------------------------------
# 1a. check the total data point
# -------------------------------------------------------------------------------------------------
df.data    <- NULL
number.allData <- apply(myData.raw[,1,drop=FALSE],2,length)
number.Reach   <- tapply(myData.raw[,1],list(myData.raw[,"Reach"]),  length)
number.Segment <- tapply(myData.raw[,1],list(myData.raw[,"Segment"]),length)

names(number.Segment) <- paste("segment",names(number.Segment),sep="")
names(number.allData) <- "all"
names(number.Reach)   <- sub("L","l",names(number.Reach))
names(number.Reach)   <- sub("U","u",names(number.Reach))

df.data <- data.frame(t(c(number.allData,number.Reach,number.Segment)))
row.names(df.data) <- "no.data"
cat(paste("total number of data has been counted\n",sep=""))
cat(paste("total number of data has been counted\n",sep=""),file=FL.LOG,append=TRUE)

                         
# -------------------------------------------------------------------------------------------------
# 1b. check and re-assign missing data
# -------------------------------------------------------------------------------------------------
df.missing <- NULL
number.missing  <- function(x){sum(is.na(x))}
missing.allData <- apply(myData.raw,2,number.missing)
missing.Upper   <- apply(myData.raw[myData.raw[,"Reach"]=="Upper",],2,number.missing)
missing.Lower   <- apply(myData.raw[myData.raw[,"Reach"]=="Lower",],2,number.missing)


df.missing <- data.frame(all = missing.allData,                         
                         lower = missing.Lower,
                         upper = missing.Upper)

# missing value in each segment                         
for (idx.segment in sort(unique(myData.raw[,"Segment"])))
{
	missing.data <- apply(myData.raw[myData.raw[,"Segment"]==idx.segment,],2,number.missing)
	command.string <- paste("df.missing <- cbind(df.missing,",paste("segment",idx.segment,sep="")," = missing.data)",sep="")
	eval(parse(text=command.string))
}	
cat(paste("missing data has been counted\n",sep=""))

# output the number of data/missing data
cat(paste("no. of  data,",sep=""),file=FL.missing,append=TRUE)
write.table(df.data,file=FL.missing,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Number of data have been accounted\n",sep=""))
cat(paste("Number of data have been accounted\n",sep=""),file=FL.LOG,append=TRUE)


# output the number of data/missing data
cat(paste("no. of missing data,",sep=""),file=FL.missing,append=TRUE)
write.table(df.missing,file=FL.missing,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Missing values have been accounted\n",sep=""))
cat(paste("Missing values have been accounted\n",sep=""),file=FL.LOG,append=TRUE)


# 
# check the missing data in the CPU_Area field  in term of passive and active gear type
#
myData.tmp0 <- myData.raw[,c("gear.type2","CPU_Area")]
missing.tmp0 <- tapply(myData.tmp0[,"CPU_Area"],list(myData.tmp0[,"gear.type2"]),number.missing)





# -------------------------------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------------------------------
myData <- myData.raw[,c(used.4.plot,TRUE)]
cat(paste("Only some of the variables are retained for plotting!\n",sep=""))
cat(paste("Only some of the variables are retained for plotting!\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 2a. check each individual variable: [4_Chk_May14_2013Data_indVar.pdf] and [4_Chk_May14_2013Data_num.sum]
# -------------------------------------------------------------------------------------------------
# open pdf file for outputting plots
FL.indVar.PDF <- paste(Path.Out,"4_Chk_May14_2013Data_indVar.pdf",sep="/")	
if  (file.exists(FL.indVar.PDF)){print(paste(FL.indVar.PDF,"exist.Delete it!")); file.remove(FL.indVar.PDF)}
pdf(file = FL.indVar.PDF,         paper="a4r",width=0,height=0)	


mySummary <- NULL
idx <- 1
for (var in grep("[^binary]",names(myData),value=TRUE,perl=TRUE))	# filed other than "binary" in [myData]
{
	cat("---------------------------- ",var," ----------------------------\n")
	
	# Categorical Varible
	if (is.factor(myData[,var]))
	{	
		no.NA <- sum(is.na(myData[,var]))
		no.NA.upper <- sum(is.na(myData[myData[,"Reach"] == "Upper",var]))
		no.NA.lower <- sum(is.na(myData[myData[,"Reach"] == "Lower",var]))
		
		myData.tmp <- myData[!(is.na(myData[,var])),var]
		myData.tmp.upper <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Upper",var]
		myData.tmp.lower <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Lower",var]
		
		# plot.all <- barplot(table(myData.tmp),xlab=var,ylab="Frequency",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",sep=""))
		# layout(matrix(c(1,2),nrow=2,ncol=1),height = c(1,1))	
		# layout(rbind(c(1,1),c(0,2)),respect=rbind(FALSE,TRUE))		             
	             
	             
		plot.all    <- barchart(myData.tmp,      ylab=var,xlab="Count",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",sep=""))
		plot.upper  <- barchart(myData.tmp.upper,ylab=var,xlab="Count",main=paste("Distribution of ",var,"(with ",no.NA.upper," missing values)\nUpper",sep=""))
		plot.lower  <- barchart(myData.tmp.lower,ylab=var,xlab="Count",main=paste("Distribution of ",var,"(with ",no.NA.lower," missing values)\nLower",sep=""))
		
		# group the variable in term of "binary" and "Reach"
		myData.freq <- as.data.frame(table(myData[,c("binary",var,"Reach")]))
		plot.binary <- barchart(myData.freq[,var] ~ myData.freq[,"Freq"] | myData.freq[,"Reach"], data = myData.freq,groups = myData.freq[,"binary"],layout = c(2,1),stack = TRUE,auto.key = list(points = FALSE, rectangles = TRUE, space = "top"),ylab = var,horizontal=TRUE,scales="free",between = list(x=2.5,y=0.5)) 
		
		# Defaults to FALSE if x is a factor or shingle, TRUE otherwise.		
		plot(plot.all,   split=c(1,1,1,3))
		plot(plot.lower, split=c(1,2,2,3),newpage=FALSE)
		plot(plot.upper, split=c(2,2,2,3),newpage=FALSE)
		plot(plot.binary,split=c(1,3,1,3),newpage=FALSE)
		
		# output to the summary file
		cat(paste(var,",\n",sep=""),file=FL.SUM.cat,append=TRUE)
		write.table(as.data.frame(table(myData[,var])),file=FL.SUM.cat,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)		
	}else{
		no.NA <- sum(is.na(myData[,var]))
		no.NA.upper <- sum(is.na(myData[myData[,"Reach"] == "Upper",var]))
		no.NA.lower <- sum(is.na(myData[myData[,"Reach"] == "Lower",var]))
		
		myData.tmp       <- myData[!(is.na(myData[,var])),                              c(var,"binary","Pallid_Only_Count")]
		myData.tmp.upper <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Upper",c(var,"binary","Pallid_Only_Count")]
		myData.tmp.lower <- myData[!(is.na(myData[,var])) & myData[,"Reach"] == "Lower",c(var,"binary","Pallid_Only_Count")]

		
		# hist(myData.tmp,freq=FALSE,nclass=100,xlab=var,ylab="Density",main=paste("Distribution of ",var,"(with ",no.NA," missing values)",sep=""))
		plot.all   <- histogram(myData.tmp[,var],      freq=FALSE,nint=100, xlab=var,ylab="Count",type="count",main=paste("Distribution of ",var,"(with ",no.NA,      " missing values)",       sep=""))
		plot.upper <- histogram(myData.tmp.upper[,var],freq=FALSE,nint=100, xlab=var,ylab="Count",type="count",main=paste("Distribution of ",var,"(with ",no.NA.upper," missing values)\nUpper",sep=""))
		plot.lower <- histogram(myData.tmp.lower[,var],freq=FALSE,nint=100, xlab=var,ylab="Count",type="count",main=paste("Distribution of ",var,"(with ",no.NA.lower," missing values)\nLower",sep=""))
		
		plot(plot.all,  split=c(1,1,1,2))
		plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
		plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)		
		
		# a box plot of the continuous variable against Pallid Count/Presence/Absence
		if (var != "Pallid_Only_Count")
		{
			par(mfrow = c(2,3))
			command.string <- paste("boxplot(",var,"   ~ Pallid_Only_Count,data=myData.tmp,      notch=TRUE,outline=FALSE,range=0,main=\"all\",  col=c(\"red\",\"blue\",\"cyan\",\"magenta\",\"green\",\"brown\",\"yellow\",\"pink\",\"purple\"),xlab=\"Pallid Count\",ylab=\"",var,"\")",sep="")
			eval(parse(text=command.string))
			command.string <- paste("boxplot(",var,"   ~ Pallid_Only_Count,data=myData.tmp.upper,notch=TRUE,outline=FALSE,range=0,main=\"upper\",col=c(\"red\",\"blue\",\"cyan\",\"magenta\",\"green\",\"brown\",\"yellow\",\"pink\",\"purple\"),xlab=\"Pallid Count\",ylab=\"",var,"\")",sep="")
			eval(parse(text=command.string))
			command.string <- paste("boxplot(",var,"   ~ Pallid_Only_Count,data=myData.tmp.lower,notch=TRUE,outline=FALSE,range=0,main=\"lower\",col=c(\"red\",\"blue\",\"cyan\",\"magenta\",\"green\",\"brown\",\"yellow\",\"pink\",\"purple\"),xlab=\"Pallid Count\",ylab=\"",var,"\")",sep="")
			eval(parse(text=command.string))		
			command.string <- paste("boxplot(",var,"   ~ binary,data=myData.tmp,      notch=TRUE,outline=FALSE,range=0,main=\"all\",  col=c(\"red\",\"cyan\"),xlab=\"Pallid Presence/Absence\",ylab=\"",var,"\")",sep="")
			eval(parse(text=command.string))		
			command.string <- paste("boxplot(",var,"   ~ binary,data=myData.tmp.upper,notch=TRUE,outline=FALSE,range=0,main=\"upper\",col=c(\"red\",\"cyan\"),xlab=\"Pallid Presence/Absence\",ylab=\"",var,"\")",sep="")
			eval(parse(text=command.string))		
			command.string <- paste("boxplot(",var,"   ~ binary,data=myData.tmp.lower,notch=TRUE,outline=FALSE,range=0,main=\"lower\",col=c(\"red\",\"cyan\"),xlab=\"Pallid Presence/Absence\",ylab=\"",var,"\")",sep="")
			eval(parse(text=command.string))
		}
		
		
		
		#
		# get summary of the data
		mySummary <- cbind(
			     data.frame(min.all = apply(myData[,var,drop=FALSE],2,min,na.rm=TRUE),
					med.all = apply(myData[,var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
					max.all = apply(myData[,var,drop=FALSE],2,max,na.rm=TRUE),
					sd.all  = apply(myData[,var,drop=FALSE],2,sd,na.rm=TRUE),
					no.all  = apply(myData[,var,drop=FALSE],2,function(x){length(x)}),
					NA.all  = apply(myData[,var,drop=FALSE],2,function(x){sum(is.na(x))})),
			     data.frame(min.upper = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,min,na.rm=TRUE),
					med.upper = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
					max.upper = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,max,na.rm=TRUE),
					sd.upper  = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,sd,na.rm=TRUE),
					no.upper  = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,function(x){length(x)}),
					NA.upper  = apply(myData[myData[,"Reach"] == "Upper",var,drop=FALSE],2,function(x){sum(is.na(x))})),
			     data.frame(min.lower = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,min,na.rm=TRUE),
					med.lower = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,function(x){quantile(x,0.5,na.rm=TRUE)}),
					max.lower = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,max,na.rm=TRUE),
					sd.lower  = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,sd,na.rm=TRUE),
					no.lower  = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,function(x){length(x)}),
					NA.lower  = apply(myData[myData[,"Reach"] == "Lower",var,drop=FALSE],2,function(x){sum(is.na(x))})))                        
		
		if (idx == 1)
		{
			cat(",",file=FL.SUM.num,append=TRUE)
			write.table(mySummary,file=FL.SUM.num,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
		}else{
			write.table(mySummary,file=FL.SUM.num,sep=",",col.names=FALSE,row.names=TRUE,append=TRUE)
		}
		
	}
	
	# make a table for missing data
	idx <- idx + 1
}
cat(paste("each retained individual variable has been checked!\n",sep=""))
cat(paste("each retained individual variable has been checked!\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 2b. check the correlation between small group of variables: [4_Chk_May14_2013Data_indVar.pdf]
# -------------------------------------------------------------------------------------------------
	# 1A. Ch_W_Full and CH_W_Nolsl
	myData.subset <- myData[,c("Ch_W_Full","Ch_W_NoIsl","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("Ch_W_Full","Ch_W_NoIsl")],use="pairwise.complete.obs")[1,2]
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Ch_W_Full","Ch_W_NoIsl")],use="pairwise.complete.obs")[1,2]
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Ch_W_Full","Ch_W_NoIsl")],use="pairwise.complete.obs")[1,2]

	plot.all   <- xyplot(myData.subset[,"Ch_W_Full"]                                   ~ myData.subset[,"Ch_W_NoIsl"],                                   type = "p", pch=16,col="black",xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Correlation between the two Channel Width variables (corcoef=",round(cor.cef.all,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Ch_W_Full"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","Ch_W_NoIsl"], type = "p", pch=16,col="red",  xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Upper Stream (corcoef=",round(cor.cef.upper,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Ch_W_Full"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","Ch_W_NoIsl"], type = "p", pch=16,col="red",  xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Lower Stream (corcoef=",round(cor.cef.lower,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot(plot.all,  split=c(1,1,1,2))
	plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
	plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)
	


	# 1B. Ch_W_Full and CH_W_Nolsl and VF_width
	myData.subset    <- myData.raw[,c("Ch_W_Full","Ch_W_NoIsl","VF_width","Reach")]
	cor.cef.all      <- cor(myData.subset[,c("Ch_W_Full","Ch_W_NoIsl","VF_width")],use="pairwise.complete.obs")
	cor.cef.upper    <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Ch_W_Full","Ch_W_NoIsl","VF_width")],use="pairwise.complete.obs")
	cor.cef.lower    <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Ch_W_Full","Ch_W_NoIsl","VF_width")],use="pairwise.complete.obs")
	cor.cef.all.12   <-  cor.cef.all[1,2]
	cor.cef.all.13   <-  cor.cef.all[1,3]
	cor.cef.all.23   <-  cor.cef.all[2,3]
	cor.cef.upper.12 <-  cor.cef.upper[1,2]
	cor.cef.upper.13 <-  cor.cef.upper[1,3]
	cor.cef.upper.23 <-  cor.cef.upper[2,3]
	cor.cef.lower.12 <-  cor.cef.lower[1,2]
	cor.cef.lower.13 <-  cor.cef.lower[1,3]
	cor.cef.lower.23 <-  cor.cef.lower[2,3]	
	

	plot.all.12   <- xyplot(myData.subset[,"Ch_W_Full"]                                   ~ myData.subset[,"Ch_W_NoIsl"],                                   type = "p", pch=16,col="black",xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Correlation among [2 channel width and VF width (corcoef=",round(cor.cef.all.12,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper.12 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Ch_W_Full"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","Ch_W_NoIsl"], type = "p", pch=16,col="red",  xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Upper Stream (corcoef=",round(cor.cef.upper.12,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower.12 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Ch_W_Full"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","Ch_W_NoIsl"], type = "p", pch=16,col="blue", xlab="Ch_W_NoIsl",ylab="Ch_W_Full",main=paste("Lower Stream (corcoef=",round(cor.cef.lower.12,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot.all.13   <- xyplot(myData.subset[,"Ch_W_Full"]                                   ~ myData.subset[,"VF_width"],                                   type = "p", pch=16,col="black",xlab="VF_width",ylab="Ch_W_Full",main=paste("Correlation among [2 channel width and VF width (corcoef=",round(cor.cef.all.13,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper.13 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Ch_W_Full"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","VF_width"], type = "p", pch=16,col="red",  xlab="VF_width",ylab="Ch_W_Full",main=paste("Upper Stream (corcoef=",round(cor.cef.upper.13,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower.13 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Ch_W_Full"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","VF_width"], type = "p", pch=16,col="blue", xlab="VF_width",ylab="Ch_W_Full",main=paste("Lower Stream (corcoef=",round(cor.cef.lower.13,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot.all.23   <- xyplot(myData.subset[,"Ch_W_NoIsl"]                                   ~ myData.subset[,"VF_width"],                                   type = "p", pch=16,col="black",xlab="VF_width",ylab="Ch_W_NoIsl",main=paste("Correlation among [2 channel width and VF width (corcoef=",round(cor.cef.all.23,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper.23 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Ch_W_NoIsl"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","VF_width"], type = "p", pch=16,col="red",  xlab="VF_width",ylab="Ch_W_NoIsl",main=paste("Upper Stream (corcoef=",round(cor.cef.upper.23,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower.23 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Ch_W_NoIsl"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","VF_width"], type = "p", pch=16,col="blue", xlab="VF_width",ylab="Ch_W_NoIsl",main=paste("Lower Stream (corcoef=",round(cor.cef.lower.23,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot(plot.all.12,  split=c(1,1,3,3))
	plot(plot.upper.12,split=c(2,1,3,3),newpage=FALSE)
	plot(plot.lower.12,split=c(3,1,3,3),newpage=FALSE)
	
	plot(plot.all.13,  split=c(1,2,3,3),newpage=FALSE)
	plot(plot.upper.13,split=c(2,2,3,3),newpage=FALSE)
	plot(plot.lower.13,split=c(3,2,3,3),newpage=FALSE)
	
	plot(plot.all.23,  split=c(1,3,3,3),newpage=FALSE)
	plot(plot.upper.23,split=c(2,3,3,3),newpage=FALSE)
	plot(plot.lower.23,split=c(3,3,3,3),newpage=FALSE)	
	
	# 1C. Temp and CH_W_Nolsl and Conductivity
	myData.subset    <- myData.raw[,c("Temp","Turbidity","Conductivity","Reach")]
	cor.cef.all      <- cor(myData.subset[,c("Temp","Turbidity","Conductivity")],use="pairwise.complete.obs")
	cor.cef.upper    <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Temp","Turbidity","Conductivity")],use="pairwise.complete.obs")
	cor.cef.lower    <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Temp","Turbidity","Conductivity")],use="pairwise.complete.obs")
	cor.cef.all.12   <-  cor.cef.all[1,2]
	cor.cef.all.13   <-  cor.cef.all[1,3]
	cor.cef.all.23   <-  cor.cef.all[2,3]
	cor.cef.upper.12 <-  cor.cef.upper[1,2]
	cor.cef.upper.13 <-  cor.cef.upper[1,3]
	cor.cef.upper.23 <-  cor.cef.upper[2,3]
	cor.cef.lower.12 <-  cor.cef.lower[1,2]
	cor.cef.lower.13 <-  cor.cef.lower[1,3]
	cor.cef.lower.23 <-  cor.cef.lower[2,3]	
	

	plot.all.12   <- xyplot(myData.subset[,"Temp"]                                   ~ myData.subset[,"Turbidity"],                                   type = "p", pch=16,col="black",xlab="Turbidity",ylab="Temp",main=paste("Correlation among [temp, [Turb] & [Cond] (corcoef=",round(cor.cef.all.12,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper.12 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Temp"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","Turbidity"], type = "p", pch=16,col="red",  xlab="Turbidity",ylab="Temp",main=paste("Upper Stream (corcoef=",round(cor.cef.upper.12,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower.12 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Temp"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","Turbidity"], type = "p", pch=16,col="blue", xlab="Turbidity",ylab="Temp",main=paste("Lower Stream (corcoef=",round(cor.cef.lower.12,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot.all.13   <- xyplot(myData.subset[,"Temp"]                                   ~ myData.subset[,"Conductivity"],                                   type = "p", pch=16,col="black",xlab="Conductivity",ylab="Temp",main=paste("Correlation among [temp, [Turb] & [Cond] (corcoef=",round(cor.cef.all.13,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper.13 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Temp"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","Conductivity"], type = "p", pch=16,col="red",  xlab="Conductivity",ylab="Temp",main=paste("Upper Stream (corcoef=",round(cor.cef.upper.13,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower.13 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Temp"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","Conductivity"], type = "p", pch=16,col="blue", xlab="Conductivity",ylab="Temp",main=paste("Lower Stream (corcoef=",round(cor.cef.lower.13,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot.all.23   <- xyplot(myData.subset[,"Turbidity"]                                   ~ myData.subset[,"Conductivity"],                                   type = "p", pch=16,col="black",xlab="Conductivity",ylab="Turbidity",main=paste("Correlation among [temp, [Turb] & [Cond] (corcoef=",round(cor.cef.all.23,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper.23 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Turbidity"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","Conductivity"], type = "p", pch=16,col="red",  xlab="Conductivity",ylab="Turbidity",main=paste("Upper Stream (corcoef=",round(cor.cef.upper.23,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower.23 <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Turbidity"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","Conductivity"], type = "p", pch=16,col="blue", xlab="Conductivity",ylab="Turbidity",main=paste("Lower Stream (corcoef=",round(cor.cef.lower.23,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot(plot.all.12,  split=c(1,1,3,3))
	plot(plot.upper.12,split=c(2,1,3,3),newpage=FALSE)
	plot(plot.lower.12,split=c(3,1,3,3),newpage=FALSE)
	
	plot(plot.all.13,  split=c(1,2,3,3),newpage=FALSE)
	plot(plot.upper.13,split=c(2,2,3,3),newpage=FALSE)
	plot(plot.lower.13,split=c(3,2,3,3),newpage=FALSE)
	
	plot(plot.all.23,  split=c(1,3,3,3),newpage=FALSE)
	plot(plot.upper.23,split=c(2,3,3,3),newpage=FALSE)
	plot(plot.lower.23,split=c(3,3,3,3),newpage=FALSE)	
	

	# 2. Grad10RM and GradBend
	myData.subset <- myData[,c("Grade10RM","GradeBend","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("Grade10RM","GradeBend")],use="pairwise.complete.obs")[1,2]
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Grade10RM","GradeBend")],use="pairwise.complete.obs")[1,2]
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Grade10RM","GradeBend")],use="pairwise.complete.obs")[1,2]

	plot.all   <- xyplot(myData.subset[,"Grade10RM"]                                   ~ myData.subset[,"GradeBend"],                                   type = "p", pch=16,col="black",xlab="GradeBend",ylab="Grade10RM",main=paste("Correlation between the two River Garde Variables (corcoef=",round(cor.cef.all,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","Grade10RM"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","GradeBend"], type = "p", pch=16,col="red",  xlab="GradeBend",ylab="Grade10RM",main=paste("Upper Stream (corcoef=",round(cor.cef.upper,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","Grade10RM"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","GradeBend"], type = "p", pch=16,col="red",  xlab="GradeBend",ylab="Grade10RM",main=paste("Lower Stream (corcoef=",round(cor.cef.lower,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot(plot.all,  split=c(1,1,1,2))
	plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
	plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)

	# 3. "MedFlow","MeanFlow",
	myData.subset <- myData[,c("MedFlow","MeanFlow","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("MedFlow","MeanFlow")],use="pairwise.complete.obs")[1,2]
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("MedFlow","MeanFlow")],use="pairwise.complete.obs")[1,2]
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("MedFlow","MeanFlow")],use="pairwise.complete.obs")[1,2]

	plot.all   <- xyplot(myData.subset[,"MedFlow"]                                   ~ myData.subset[,"MeanFlow"],                                   type = "p", pch=16,col="black",xlab="MeanFlow",ylab="MedFlow",main=paste("Correlation between the two River Flows (corcoef=",round(cor.cef.all,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.upper <- xyplot(myData.subset[myData.subset[,"Reach"] == "Upper","MedFlow"] ~ myData.subset[myData.subset[,"Reach"] == "Upper","MeanFlow"], type = "p", pch=16,col="red",  xlab="MeanFlow",ylab="MedFlow",main=paste("Upper Stream (corcoef=",round(cor.cef.upper,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})
	plot.lower <- xyplot(myData.subset[myData.subset[,"Reach"] == "Lower","MedFlow"] ~ myData.subset[myData.subset[,"Reach"] == "Lower","MeanFlow"], type = "p", pch=16,col="red",  xlab="MeanFlow",ylab="MedFlow",main=paste("Lower Stream (corcoef=",round(cor.cef.lower,digits=2),")",sep=""),panel = function(x,y,...){panel.xyplot(x,y,...); panel.abline(a=0,b=1,col="black")})

	plot(plot.all,  split=c(1,1,1,2))
	plot(plot.upper,split=c(1,2,2,2),newpage=FALSE)
	plot(plot.lower,split=c(2,2,2,2),newpage=FALSE)


	# 4. Mean_z, min_z, max_z
	myData.subset <- myData[,c("Mean_z","Min_z","Max_z","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("Mean_z","Min_z","Max_z")],use="pairwise.complete.obs")
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Mean_z","Min_z","Max_z")],use="pairwise.complete.obs")
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Mean_z","Min_z","Max_z")],use="pairwise.complete.obs")

	cor.string.all   <- paste("correlation among elevation vars are ",round(cor.cef.all[1,2],digits=2),  " and ",round(cor.cef.all[1,3],digits=2),  " and ",round(cor.cef.all[2,3],digits=2),  " (all data)",sep="")
	cor.string.upper <- paste("correlation among elevation vars are ",round(cor.cef.upper[1,2],digits=2)," and ",round(cor.cef.upper[1,3],digits=2)," and ",round(cor.cef.upper[2,3],digits=2)," (Upper Stream)",sep="")
	cor.string.lower <- paste("correlation among elevation vars are ",round(cor.cef.lower[1,2],digits=2)," and ",round(cor.cef.lower[1,3],digits=2)," and ",round(cor.cef.lower[2,3],digits=2)," (Lower Stream)", sep="")

	par(mfrow = c(3,1))
	plot(myData.subset[,c("Mean_z")],col=c("red"),type="l",lty=1,ylab="mean/min/max elevation (all data)",main = cor.string.all)
	lines(myData.subset[,c("Min_z")],col=c("blue"),type="l",lty=1)
	lines(myData.subset[,c("Max_z")],col=c("green"),type="l",lty=1)

	# for upper stream
	plot(myData.subset[myData.subset[,"Reach"] == "Upper",c("Mean_z")],col=c("red"),type="l",lty=1,ylab="mean/min/max elevation (Upper Stream)",main = cor.string.upper)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("Min_z")],col=c("blue"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("Max_z")],col=c("green"),type="l",lty=1)

	# for lower stream
	plot(myData.subset[myData.subset[,"Reach"] == "Lower",c("Mean_z")],col=c("magenta"),type="l",lty=1,ylab="mean/min/max elevation (Lower Stream)",main = cor.string.lower)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("Min_z")],col=c("cyan"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("Max_z")],col=c("light green"),type="l",lty=1)


	# 5. Depth1, Depth2, Depth3
	myData.subset <- myData[,c("Depth1","Depth2","Depth3","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("Depth1","Depth2","Depth3")],use="pairwise.complete.obs")
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("Depth1","Depth2","Depth3")],use="pairwise.complete.obs")
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("Depth1","Depth2","Depth3")],use="pairwise.complete.obs")

	cor.string.all   <- paste("correlation among three Depths vars are ",round(cor.cef.all[1,2],digits=2),  " and ",round(cor.cef.all[1,3],digits=2),  " and ",round(cor.cef.all[2,3],digits=2),  " (all data)",sep="")
	cor.string.upper <- paste("correlation among three Depths vars are ",round(cor.cef.upper[1,2],digits=2)," and ",round(cor.cef.upper[1,3],digits=2)," and ",round(cor.cef.upper[2,3],digits=2)," (Upper Stream)",sep="")
	cor.string.lower <- paste("correlation among three Depths vars are ",round(cor.cef.lower[1,2],digits=2)," and ",round(cor.cef.lower[1,3],digits=2)," and ",round(cor.cef.lower[2,3],digits=2)," (Lower Stream)", sep="")



	par(mfrow = c(3,1))
	 plot(myData.subset[,c("Depth1")],col=c("red"),type="l",lty=1,ylab="three Depths (all data)",main = cor.string.all)
	lines(myData.subset[,c("Depth2")],col=c("blue"),type="l",lty=1)
	lines(myData.subset[,c("Depth3")],col=c("green"),type="l",lty=1)

	# for upper stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Upper",c("Depth1")],col=c("red"),type="l",lty=1,ylab="three Depths (Upper Stream)",main = cor.string.upper)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("Depth2")],col=c("blue"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("Depth3")],col=c("green"),type="l",lty=1)

	# for lower stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Lower",c("Depth1")],col=c("magenta"),type="l",lty=1,ylab="three Depths (Lower Stream)",main = cor.string.lower)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("Depth2")],col=c("cyan"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("Depth3")],col=c("light green"),type="l",lty=1)


	# 6a. May 6, 2013: check the correlation of the newly added distance to the stock stations
	myData.subset <- myData[,c("S_U10_D2_Near","S_U20_D4_Near","S_U30_D6_Near","S_U50_D10_Near","S_U100_D20_Near","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("S_U10_D2_Near","S_U20_D4_Near","S_U30_D6_Near","S_U50_D10_Near","S_U100_D20_Near")],use="pairwise.complete.obs")
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U10_D2_Near","S_U20_D4_Near","S_U30_D6_Near","S_U50_D10_Near","S_U100_D20_Near")],use="pairwise.complete.obs")
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U10_D2_Near","S_U20_D4_Near","S_U30_D6_Near","S_U50_D10_Near","S_U100_D20_Near")],use="pairwise.complete.obs")

	cor.string.all   <- paste("correlation among three stock station distnace vars are ",round(cor.cef.all[1,2],digits=2),  " and ",round(cor.cef.all[1,3],digits=2),  " and ",round(cor.cef.all[2,3],digits=2),  " (all data)",sep="")
	cor.string.upper <- paste("correlation among three stock station distnace vars are ",round(cor.cef.upper[1,2],digits=2)," and ",round(cor.cef.upper[1,3],digits=2)," and ",round(cor.cef.upper[2,3],digits=2)," (Upper Stream)",sep="")
	cor.string.lower <- paste("correlation among three stock station distnace vars are ",round(cor.cef.lower[1,2],digits=2)," and ",round(cor.cef.lower[1,3],digits=2)," and ",round(cor.cef.lower[2,3],digits=2)," (Lower Stream)", sep="")


	par(mfrow = c(3,1))
	 plot(myData.subset[,c("S_U10_D2_Near")],  col=c("red"),    type="l",lty=1,ylab="five near stock dists (all data)",main = cor.string.all)
	lines(myData.subset[,c("S_U20_D4_Near")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[,c("S_U30_D6_Near")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[,c("S_U50_D10_Near")], col=c("magenta"),type="l",lty=1)
	lines(myData.subset[,c("S_U100_D20_Near")],col=c("cyan"),   type="l",lty=1)

	# for upper stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U10_D2_Near")],  col=c("red"),    type="l",lty=1,ylab="five near stock dists (Upper Stream)",main = cor.string.upper)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U20_D4_Near")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U30_D6_Near")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U50_D10_Near")], col=c("magenta"),type="l",lty=1)	
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U100_D20_Near")],col=c("cyan"),   type="l",lty=1)	

	# for lower stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U10_D2_Near")],  col=c("red"),    type="l",lty=1,ylab="five near stock dists (Lower Stream)",main = cor.string.lower)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U20_D4_Near")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U30_D6_Near")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U50_D10_Near")], col=c("magenta"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U100_D20_Near")],col=c("cyan"),   type="l",lty=1)


	
	# 6b. May 6, 2013: check the correlation of the newly added distance to the stock stations and cumulative realease of fishes
	myData.subset <- myData[,c("S_U10_D2_Cumu","S_U20_D4_Cumu","S_U30_D6_Cumu","S_U50_D10_Cumu","S_U100_D20_Cumu","Reach")]
	cor.cef.all   <- cor(myData.subset[,c("S_U10_D2_Cumu","S_U20_D4_Cumu","S_U30_D6_Cumu","S_U50_D10_Cumu","S_U100_D20_Cumu")],use="pairwise.complete.obs")
	cor.cef.upper <- cor(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U10_D2_Cumu","S_U20_D4_Cumu","S_U30_D6_Cumu","S_U50_D10_Cumu","S_U100_D20_Cumu")],use="pairwise.complete.obs")
	cor.cef.lower <- cor(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U10_D2_Cumu","S_U20_D4_Cumu","S_U30_D6_Cumu","S_U50_D10_Cumu","S_U100_D20_Cumu")],use="pairwise.complete.obs")

	cor.string.all   <- paste("correlation among three stock station distnace vars are ",round(cor.cef.all[1,2],digits=2),  " and ",round(cor.cef.all[1,3],digits=2),  " and ",round(cor.cef.all[2,3],digits=2),  " (all data)",sep="")
	cor.string.upper <- paste("correlation among three stock station distnace vars are ",round(cor.cef.upper[1,2],digits=2)," and ",round(cor.cef.upper[1,3],digits=2)," and ",round(cor.cef.upper[2,3],digits=2)," (Upper Stream)",sep="")
	cor.string.lower <- paste("correlation among three stock station distnace vars are ",round(cor.cef.lower[1,2],digits=2)," and ",round(cor.cef.lower[1,3],digits=2)," and ",round(cor.cef.lower[2,3],digits=2)," (Lower Stream)", sep="")
	
	par(mfrow = c(3,1))
	 plot(myData.subset[,c("S_U10_D2_Cumu")],  col=c("red"),    type="l",lty=1,ylab="five cum stock dists (all data)",main = cor.string.all)
	lines(myData.subset[,c("S_U20_D4_Cumu")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[,c("S_U30_D6_Cumu")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[,c("S_U50_D10_Cumu")], col=c("magenta"),type="l",lty=1)
	lines(myData.subset[,c("S_U100_D20_Cumu")],col=c("cyan"),   type="l",lty=1)

	# for upper stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U10_D2_Cumu")],  col=c("red"),    type="l",lty=1,ylab="five cum stock dists (Upper Stream)",main = cor.string.upper)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U20_D4_Cumu")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U30_D6_Cumu")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U50_D10_Cumu")], col=c("magenta"),type="l",lty=1)	
	lines(myData.subset[myData.subset[,"Reach"] == "Upper",c("S_U100_D20_Cumu")],col=c("cyan"),   type="l",lty=1)	

	# for lower stream
	 plot(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U10_D2_Cumu")],  col=c("red"),    type="l",lty=1,ylab="five cum stock dists (Lower Stream)",main = cor.string.lower)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U20_D4_Cumu")],  col=c("blue"),   type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U30_D6_Cumu")],  col=c("green"),  type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U50_D10_Cumu")], col=c("magenta"),type="l",lty=1)
	lines(myData.subset[myData.subset[,"Reach"] == "Lower",c("S_U100_D20_Cumu")],col=c("cyan"),   type="l",lty=1)


		



	
	

dev.off()	# close (FL.indVar.PDF)
cat(paste("some groups of variables have been analyzed!\n",sep=""))
cat(paste("some groups of variables have been analyzed!\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 3. check correlation between numeric covariates: [4_Chk_May14_2013Data_Correlation.pdf]
# -------------------------------------------------------------------------------------------------
FL.Correlation.PDF  <- paste(Path.Out,"4_Chk_May14_2013Data_Correlation.pdf",sep="/")	
if  (file.exists(FL.Correlation.PDF)) {print(paste(FL.Correlation.PDF, "exist.Delete it!")); file.remove(FL.Correlation.PDF)}
pdf(file = FL.Correlation.PDF,         paper="a4r",width=0,height=0)	
# y.vars <- c("RM1960_RM","UpDamDist","Ch_W_Full","UpTribDist","DTribDist","Mean_z", "Grade10RM","MedFlow","comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Temp",   "Distance","Depth1", "Depth2", "Depth3","S_U10_D2_Near","S_U10_D2_Cumu","S_U20_D4_Near","S_U20_D4_Cumu","S_U30_D6_Near","S_U30_D6_Cumu","S_U50_D10_Near","S_U50_D10_Cumu","S_U100_D20_Near","S_U100_D20_Cumu")
  y.vars <- c("RM1960_RM","UpDamDist","Ch_W_Full","UpTribDist","DTribDist","Mean_z", "Grade10RM","MedFlow","comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Temp",   "Distance","Depth1", "Depth2", "Depth3")
x.var  <- c("BendRiverMile")
	# 1. scatter plot against BendRiverMiles
	for (y.var in y.vars)
	{
		myData.work <- myData.raw[,c(x.var,y.var,"Reach")]
		command.string <- paste("plot.obj1 <- xyplot(",y.var," ~ ",x.var,", data = myData.work,xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(All) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
		eval(parse(text=command.string))

		command.string <- paste("plot.obj2 <- xyplot(",y.var," ~ ",x.var,", data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(Upper) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
		eval(parse(text=command.string))			

		command.string <- paste("plot.obj3 <- xyplot(",y.var," ~ ",x.var,", data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(Lower) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
		eval(parse(text=command.string))

		plot(plot.obj1,split=c(1,1,2,2))
		plot(plot.obj2,split=c(2,1,2,2),newpage=FALSE)	
		plot(plot.obj3,split=c(1,2,2,2),newpage=FALSE)	
	}
	
	# 2. scatter plot of each other
	for (i in seq(from=1,to=(length(y.vars)-1)))
	{
		for (j in seq(from = (i+1),to = length(y.vars)))
		{
			x.var <- y.vars[i]
			y.var <- y.vars[j]
			
			myData.work <- myData.raw[,c(x.var,y.var,"Reach")]
			command.string <- paste("plot.obj1 <- xyplot(",y.var," ~ ",x.var,", data = myData.work,xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(All) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
			eval(parse(text=command.string))

			command.string <- paste("plot.obj2 <- xyplot(",y.var," ~ ",x.var,", data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(Upper) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
			eval(parse(text=command.string))			

			command.string <- paste("plot.obj3 <- xyplot(",y.var," ~ ",x.var,", data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",x.var,"\",ylab=\"",y.var,"\",main=paste(\"(Lower) \",\"",y.var,"\",\" vs \",\"",x.var,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
			eval(parse(text=command.string))

			plot(plot.obj1,split=c(1,1,2,2))
			plot(plot.obj2,split=c(2,1,2,2),newpage=FALSE)	
			plot(plot.obj3,split=c(1,2,2,2),newpage=FALSE)	
		}
	}	
	
dev.off()
cat(paste("correlation between covariates!\n",sep=""))
cat(paste("correlation between covariates!\n",sep=""),file=FL.LOG,append=TRUE)


# -------------------------------------------------------------------------------------------------
# 4. count summary: FL.Summary == [4_Chk_May14_2013Data_summary.csv]
# -------------------------------------------------------------------------------------------------
myData.sub <- melt(myData,id.var=c("SY","Reach","Season","Bend","Macro","Meso","Segment","Gear","gear.type1","gear.type2","macro.type","Rocktype1","taxpartsiz","MicroClass"),measure.var="Pallid_Only_Count")

sum.SY.Count      <- as.data.frame(cast(myData.sub,SY         ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across sample year   [SY]
sum.Reach.Count   <- as.data.frame(cast(myData.sub,Reach      ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Reach         [Reach]
sum.Season.Count  <- as.data.frame(cast(myData.sub,Season     ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Season        [Season]
sum.Macro1.Count  <- as.data.frame(cast(myData.sub,Macro      ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Macro         [Macro]
sum.Macro2.Count  <- as.data.frame(cast(myData.sub,macro.type ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Macro         [macro.type]
sum.Segment.Count <- as.data.frame(cast(myData.sub,Segment    ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Segment       [Segment]
sum.Gear.Count    <- as.data.frame(cast(myData.sub,Gear       ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Gear          [Gear]
sum.Gear1.Count   <- as.data.frame(cast(myData.sub,gear.type1 ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Gear.type1    [gear.type1]
sum.Gear2.Count   <- as.data.frame(cast(myData.sub,gear.type2 ~ value,fun=length,margins = c("grand_row","grand_col")))			# count distribution across Gear.type2    [gear.type2]




#
# single variable distribution
#
cat("Count distribution in Year\n",             file=FL.Summary,append=TRUE)
write.table(sum.SY.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.SY.Count] out

cat("\n\nCount distribution in Reach\n",        file=FL.Summary,append=TRUE)
write.table(sum.Reach.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Reach.Count] out

cat("\n\nCount distribution in Season\n",       file=FL.Summary,append=TRUE)
write.table(sum.Season.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)					# write [sum.Season.Count] out

cat("\n\nCount distribution in Macro1\n",       file=FL.Summary,append=TRUE)
write.table(sum.Macro1.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)					# write [sum.Macro1.Count] out

cat("\n\nCount distribution in Macro2\n",       file=FL.Summary,append=TRUE)
write.table(sum.Macro2.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)					# write [sum.Macro2.Count] out

cat("\n\nCount distribution in Segment\n",      file=FL.Summary,append=TRUE)
write.table(sum.Segment.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)					# write [sum.Segment.Count] out

cat("\n\nCount distribution in Gear\n",         file=FL.Summary,append=TRUE)
write.table(sum.Gear.Count, file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Gear.Count] out

cat("\n\nCount distribution in Gear Type 1\n"  ,file=FL.Summary,append=TRUE)
write.table(sum.Gear1.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Gear1.Count] out

cat("\n\nCount distribution in Gear Type 2\n",  file=FL.Summary,append=TRUE)
write.table(sum.Gear2.Count,file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)						# write [sum.Gear2.Count] out


#
# two variable distribution
# ........ [Year] vs [Gear.Type1] ......................
cat("\n\nCount distribution in [Year] and [Gear.Type1]\n")
cat("\n\nCount distribution in [Year] and [Gear.Type1]\n",file=FL.Summary,append=TRUE)
sum.SY.Gear1.Count <- cast(myData.sub,SY ~ value | gear.type1,fun=length)								# count distribution across sample year   [SY] and [gear.type1]
for (idx in names(sum.SY.Gear1.Count))
{
	cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.SY.Gear1.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.SY.Gear1.Count] out
}

# ........ [Year] vs [Gear.Type2] ......................
cat("\n\nCount distribution in [Year] and [Gear.Type2]\n")
cat("\n\nCount distribution in [Year] and [Gear.Type2]\n",file=FL.Summary,append=TRUE)
sum.SY.Gear2.Count <- cast(myData.sub,SY ~ value | gear.type2,fun=length)								# count distribution across sample year   [SY] and [gear.type2]
for (idx in names(sum.SY.Gear2.Count))
{
	cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.SY.Gear2.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.SY.Gear2.Count] out
}

# ........ [Reach] vs [Gear.Type1] ......................
cat("\n\nCount distribution in [Reach] and [Gear.Type1]\n")
cat("\n\nCount distribution in [Reach] and [Gear.Type1]\n",file=FL.Summary,append=TRUE)
sum.Reach.Gear1.Count <- cast(myData.sub,Reach ~ value | gear.type1,fun=length)								# count distribution across Reach [Reach] and [gear.type1]
for (idx in names(sum.Reach.Gear1.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.Gear1.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.Gear1.Count] out
}

# ........ [Reach] vs [Gear.Type2] ......................
cat("\n\nCount distribution in [Reach] and [Gear.Type2]\n")
cat("\n\nCount distribution in [Reach] and [Gear.Type2]\n",file=FL.Summary,append=TRUE)
sum.Reach.Gear2.Count <- cast(myData.sub,Reach ~ value | gear.type2,fun=length)								# count distribution across Reach [Reach] and [gear.type2]
for (idx in names(sum.SY.Gear2.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.Gear2.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.Gear2.Count] out
}

# ........ [Macro2] vs [Meso] ......................
cat("\n\nCount distribution in [Macro2] and [Meso]\n")
cat("\n\nCount distribution in [Macro2] and [Meso]\n",file=FL.Summary,append=TRUE)
sum.Macro2.Meso.Count <- cast(myData.sub,macro.type ~ value | Meso,fun=length)								# count distribution across Macro2 [macro.type] and Meso [Meso]
for (idx in names(sum.Macro2.Meso.Count))
{
	cat(paste("Count distribution in [macro.type] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Macro2.Meso.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Macro2.Meso.Count] out
}

# ........ [Reach] vs [Macro] ......................
cat("\n\nCount distribution in [Reach] and [Macro]\n")
cat("\n\nCount distribution in [Reach] and [Macro]\n",file=FL.Summary,append=TRUE)
sum.Reach.Macro.Count <- cast(myData.sub,Reach ~ value | Macro,fun=length)								# count distribution across Reach [Reach] and Macro [Macro]
for (idx in names(sum.Reach.Macro.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.Macro.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.Macro.Count] out
}

# ........ [Gear.Type1] vs [Macro] ......................
cat("\n\nCount distribution in [Gear.type1] and [Macro]\n")
cat("\n\nCount distribution in [Gear.type1] and [Macro]\n",file=FL.Summary,append=TRUE)
sum.GearType1.Macro.Count <- cast(myData.sub,gear.type1 ~ value | Macro,fun=length)							# count distribution across Gear Type1 [gear.type1] and Macro [Macro]
for (idx in names(sum.GearType1.Macro.Count))
{
	cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType1.Macro.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.Macro.Count] out
}

# ........ [Gear.Type2] vs [Macro] ......................
cat("\n\nCount distribution in [Gear.type2] and [Macro]\n")
cat("\n\nCount distribution in [Gear.type2] and [Macro]\n",file=FL.Summary,append=TRUE)
sum.GearType2.Macro.Count <- cast(myData.sub,gear.type1 ~ value | Macro,fun=length)							# count distribution across Gear Type1 [gear.type2] and Macro [Macro]
for (idx in names(sum.GearType2.Macro.Count))
{
	cat(paste("Count distribution in [Gear.type2] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType2.Macro.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.Macro.Count] out
}




# ........ [Reach] vs [macro.type] ......................
cat("\n\nCount distribution in [Reach] and [macro.type]\n")
cat("\n\nCount distribution in [Reach] and [macro.type]\n",file=FL.Summary,append=TRUE)
sum.Reach.macro.type.Count <- cast(myData.sub,Reach ~ value | macro.type,fun=length)								# count distribution across Reach [Reach] and macro.type [macro.type]
for (idx in names(sum.Reach.macro.type.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.macro.type.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.macro.type.Count] out
}

# ........ [Gear.Type1] vs [macro.type] ......................
cat("\n\nCount distribution in [Gear.type1] and [macro.type]\n")
cat("\n\nCount distribution in [Gear.type1] and [macro.type]\n",file=FL.Summary,append=TRUE)
sum.GearType1.macro.type.Count <- cast(myData.sub,gear.type1 ~ value | macro.type,fun=length)							# count distribution across Gear Type1 [gear.type1] and macro.type [macro.type]
for (idx in names(sum.GearType1.macro.type.Count))
{
	cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType1.macro.type.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.macro.type.Count] out
}

# ........ [Gear.Type2] vs [macro.type] ......................
cat("\n\nCount distribution in [Gear.type2] and [macro.type]\n")
cat("\n\nCount distribution in [Gear.type2] and [macro.type]\n",file=FL.Summary,append=TRUE)
sum.GearType2.macro.type.Count <- cast(myData.sub,gear.type1 ~ value | macro.type,fun=length)							# count distribution across Gear Type1 [gear.type2] and macro.type [macro.type]
for (idx in names(sum.GearType2.macro.type.Count))
{
	cat(paste("Count distribution in [Gear.type2] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType2.macro.type.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.macro.type.Count] out
}





# ........ [Reach] vs [MicroClass] ......................
cat("\n\nCount distribution in [Reach] and [MicroClass]\n")
cat("\n\nCount distribution in [Reach] and [MicroClass]\n",file=FL.Summary,append=TRUE)
sum.Reach.MicroClass.Count <- cast(myData.sub,Reach ~ value | MicroClass,fun=length)								# count distribution across Reach [Reach] and MicroClass [MicroClass]
for (idx in names(sum.Reach.MicroClass.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.MicroClass.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.MicroClass.Count] out
}

# ........ [Gear.Type1] vs [MicroClass] ......................
cat("\n\nCount distribution in [Gear.type1] and [MicroClass]\n")
cat("\n\nCount distribution in [Gear.type1] and [MicroClass]\n",file=FL.Summary,append=TRUE)
sum.GearType1.MicroClass.Count <- cast(myData.sub,gear.type1 ~ value | MicroClass,fun=length)							# count distribution across Gear Type1 [gear.type1] and MicroClass [MicroClass]
for (idx in names(sum.GearType1.MicroClass.Count))
{
	cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType1.MicroClass.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.MicroClass.Count] out
}

# ........ [Gear.Type2] vs [MicroClass] ......................
cat("\n\nCount distribution in [Gear.type2] and [MicroClass]\n")
cat("\n\nCount distribution in [Gear.type2] and [MicroClass]\n",file=FL.Summary,append=TRUE)
sum.GearType2.MicroClass.Count <- cast(myData.sub,gear.type1 ~ value | MicroClass,fun=length)							# count distribution across Gear Type1 [gear.type2] and MicroClass [MicroClass]
for (idx in names(sum.GearType2.MicroClass.Count))
{
	cat(paste("Count distribution in [Gear.type2] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType2.MicroClass.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.MicroClass.Count] out
}








# ........ [Gear.Type1] vs [Segment] ......................
cat("\n\nCount distribution in [Gear.type1] and [Segment]\n")
cat("\n\nCount distribution in [Gear.type1] and [Segment]\n",file=FL.Summary,append=TRUE)
sum.GearType1.Segment.Count <- cast(myData.sub,gear.type1 ~ value | Segment,fun=length)							# count distribution across Gear Type1 [gear.type1] and Segment [Segment]
for (idx in names(sum.GearType1.Segment.Count))
{
	cat(paste("Count distribution in [Gear.type1] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType1.Segment.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType1.Segment.Count] out
}

# ........ [Gear.Type2] vs [Segment] ......................
cat("\n\nCount distribution in [Gear.Type2] and [Segment]\n")
cat("\n\nCount distribution in [Gear.Type2] and [Segment]\n",file=FL.Summary,append=TRUE)
sum.GearType2.Segment.Count <- cast(myData.sub,gear.type2 ~ value | Segment,fun=length)							# count distribution across Gear Type1 [gear.type2] and Segment [Segment]
for (idx in names(sum.GearType2.Segment.Count))
{
	cat(paste("Count distribution in [Gear.Type2] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.GearType2.Segment.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)	# write [sum.GearType2.Segment.Count] out
}

# ........ [Year] vs [Segment] ......................
cat("\n\nCount distribution in [Year] and [Segment]\n")
cat("\n\nCount distribution in [Year] and [Segment]\n",file=FL.Summary,append=TRUE)
sum.Year.Segment.Count <- cast(myData.sub,SY ~ value | Segment,fun=length)								# count distribution across Year [Year] and Segment [Segment]
for (idx in names(sum.Year.Segment.Count))
{
	cat(paste("Count distribution in [Year] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Year.Segment.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Year.Segment.Count] out
}
cat(paste("\n\nCount summary has been done for some single variables and some two variable combinations!\n",sep=""))
cat(paste("\n\nCount summary has been done for some single variables and some two variable combinations!\n",sep=""),file=FL.LOG,append=TRUE)


# ........ [Reach] vs [Rocktype1] ......................
cat("\n\nCount distribution in [Reach] and [Rocktype1]\n")
cat("\n\nCount distribution in [Reach] and [Rocktype1]\n",file=FL.Summary,append=TRUE)
sum.Reach.Rocktype1.Count <- cast(myData.sub,Reach ~ value | Rocktype1,fun=length)								# count distribution across Reach [Reach] and Rocktype1 [Rocktype1]
for (idx in names(sum.Reach.Rocktype1.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.Rocktype1.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.Rocktype1.Count] out
}


# ........ [Reach] vs [taxpartsiz] ......................
cat("\n\nCount distribution in [Reach] and [taxpartsiz]\n")
cat("\n\nCount distribution in [Reach] and [taxpartsiz]\n",file=FL.Summary,append=TRUE)
sum.Reach.taxpartsiz.Count <- cast(myData.sub,Reach ~ value | taxpartsiz,fun=length)								# count distribution across Reach [Reach] and taxpartsiz [taxpartsiz]
for (idx in names(sum.Reach.taxpartsiz.Count))
{
	cat(paste("Count distribution in [Reach] and [",idx,"]\n"),file=FL.Summary,append=TRUE)
	write.table(data.frame(sum.Reach.taxpartsiz.Count[idx]),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)		# write [sum.Reach.taxpartsiz.Count] out
}




# -------------------------------------------------------------------------------------------------
# 5. add a presence/absence variable for Pallid Sturgeon Count
# -------------------------------------------------------------------------------------------------
myData.sup <- cbind(myData,binary = rep("no",dim(myData)[1]),stringsAsFactors=FALSE)
myData.sup[myData.sup[,"Pallid_Only_Count"] > 0,"binary"] <- "Yes"

y.vars.num <- "Pallid_Only_Count"
# x.vars.num <- c("UpDamDist","Ch_W_Full","Ch_W_NoIsl","Braiding","UpTribDist","DTribDist","Mean_z", "Max_z",  "Min_z",  "Grade10RM","GradeBend","MedFlow","MeanFlow","comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Segment","Temp",   "Distance","Depth1", "Depth2", "Depth3", "WaterVel","Depth.mean","S_U20_D4_Near","S_U20_D4_Cumu","S_U30_D6_Near","S_U30_D6_Cumu","S_U50_D10_Near","S_U50_D10_Cumu","S_U100_D20_Near","S_U100_D20_Cumu")
  x.vars.num <- c("UpDamDist","Ch_W_Full","Ch_W_NoIsl","Braiding","UpTribDist","DTribDist","Mean_z", "Max_z",  "Min_z",  "Grade10RM","GradeBend","MedFlow","MeanFlow","comppct_r","sandtotal_r","claytotal_r","frag3to10_r","VF_width","Segment","Temp",   "Distance","Depth1", "Depth2", "Depth3", "WaterVel","Depth.mean")

y.vars.cat <- "binary"
x.vars.cat <- c("Gear",  "Season","Bend",  "Reach", "Rocktype1","Rocktype2","Lithology","NFHAP_Scr","taxorder","taxpartsiz","Segment","gear.type1","gear.type2","macro.type","MicroClass","SY")

myData.work <- myData.sup[,c(x.vars.num,x.vars.cat,y.vars.num,y.vars.cat)]
cat(paste("\n\nAdded a pbinary variable for Pallid Sturgeon (presence/absence)!\n",sep=""))
cat(paste("\n\nAdded a pbinary variable for Pallid Sturgeon (presence/absence)!\n",sep=""),file=FL.LOG,append=TRUE)





# -------------------------------------------------------------------------------------------------
# 6. check correlation of continusous variables vs the [count] of Pallid Sturgeon: [4_Chk_May14_2013Data_Count_numVar.pdf]
# -------------------------------------------------------------------------------------------------
FL.Count_num.PDF  <- paste(Path.Out,"4_Chk_May14_2013Data_Count_numVar.pdf",sep="/")	
if  (file.exists(FL.Count_num.PDF)) {print(paste(FL.Count_num.PDF, "exist.Delete it!")); file.remove(FL.Count_num.PDF)}
pdf(file = FL.Count_num.PDF,         paper="a4r",width=0,height=0)	

cat(paste("check correlation of count with some variables!\n",sep=""))
cat(paste("\nCheck the distribution of continuous variables in terms of the count of Pallid Sturgeon!\n",sep=""),file=FL.Summary,append=TRUE)
cat(paste("\nCheck the distribution of continuous variables in terms of the count of Pallid Sturgeon!\n",sep=""),file=FL.LOG,    append=TRUE)
for (var.num in x.vars.num)
{
	# 1. plot vs river stream
	# all data
	command.string <- paste("plot.obj1 <- xyplot(",var.num," ~ ",y.vars.num,", data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
	eval(parse(text=command.string))

	command.string <- paste("plot.obj2 <- bwplot(",var.num," ~ as.factor(",y.vars.num,"), data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),col=\"red\")",sep="")
	eval(parse(text=command.string))

	# upper stream
	command.string <- paste("plot.obj3 <- xyplot(",var.num," ~ ",y.vars.num,", data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(Upper) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
	eval(parse(text=command.string))

	command.string <- paste("plot.obj4 <- bwplot(",var.num," ~ as.factor(",y.vars.num,"), data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(Upper) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),col=\"blue\")",sep="")
	eval(parse(text=command.string))
	
	# lower stream
	command.string <- paste("plot.obj5 <- xyplot(",var.num," ~ ",y.vars.num,", data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(Lower) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
	eval(parse(text=command.string))

	command.string <- paste("plot.obj6 <- bwplot(",var.num," ~ as.factor(",y.vars.num,"), data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(Lower) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),col=\"green\")",sep="")
	eval(parse(text=command.string))
	
	plot(plot.obj1,split=c(1,1,2,3))
	plot(plot.obj2,split=c(2,1,2,3),newpage=FALSE)	
	plot(plot.obj3,split=c(1,2,2,3),newpage=FALSE)	
	plot(plot.obj4,split=c(2,2,2,3),newpage=FALSE)	
	plot(plot.obj5,split=c(1,3,2,3),newpage=FALSE)	
	plot(plot.obj6,split=c(2,3,2,3),newpage=FALSE)	
	
	# 2a. Segment-wise: scatter plots
	command.string <- paste("plot.obj <- xyplot(",var.num," ~ ",y.vars.num," | Segment, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)

	# 2b. Segment-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.num,") | Segment, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),col=\"red\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
	
	# 3a. Macro-wise: scatter plots
	command.string <- paste("plot.obj <- xyplot(",var.num," ~ ",y.vars.num," | macro.type, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)

	# 3b. Macro-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.num,") | macro.type, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
	
	# 4a. Micro-wise: scatter plots
	command.string <- paste("plot.obj <- xyplot(",var.num," ~ ",y.vars.num," | MicroClass, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)

	# 4b. Micro-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.num,") | MicroClass, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
	
	# 5a. Gear Type1-wise: scatter plots
	command.string <- paste("plot.obj <- xyplot(",var.num," ~ ",y.vars.num," | gear.type1, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)

	# 5b. Gear Type1-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.num,") | gear.type1, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
	
	# 6a. Gear type2-wise: scatter plots
	command.string <- paste("plot.obj <- xyplot(",var.num," ~ ",y.vars.num," | gear.type2, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)

	# 6b. Gear type2-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.num,") | gear.type2, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
		
	
	# 7a. Sample-Year-wise: scatter plots
	command.string <- paste("plot.obj <- xyplot(",var.num," ~ ",y.vars.num," | SY, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)

	# 7b. Sample-Year-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.num,") | SY, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.num,"\",main=paste(\"(All) \",\"",var.num,"\",\" vs \",\"",y.vars.num,"\",sep=\"\"),col=\"blue\")",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
	
	
		
}
dev.off()
cat(paste("\n\nchecked correlation of continusous variables vs the [count] of Pallid Sturgeon!\n",sep=""))
cat(paste("\n\nchecked correlation of continusous variables vs the [count] of Pallid Sturgeon!\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 7. check the correlation of discrete variables vs the [count] of Pallid Sturgeon: FL.Summary == [4_Chk_May14_2013Data_summary.csv]
# -------------------------------------------------------------------------------------------------
# FL.Count_cat.PDF  <- paste(Path.Out,"4_Chk_May14_2013Data_Count_catVar.pdf",sep="/")	
# if  (file.exists(FL.Count_cat.PDF)) {print(paste(FL.Count_cat.PDF, "exist.Delete it!")); file.remove(FL.Count_cat.PDF)}
# pdf(file = FL.Count_cat.PDF,         paper="a4r",width=0,height=0)	
# 
cat(paste("Check the distribution of variables in terms of Presence/Absence of Pallid Sturgeon!\n",sep=""),file=FL.Summary,append=TRUE)
cat(paste("Check the distribution of variables in terms of Presence/Absence of Pallid Sturgeon!\n",sep=""),file=FL.LOG,    append=TRUE)
for (var.cat in x.vars.cat)
{
# 	# all data
# 	command.string <- paste("plot.obj1 <- xyplot(",y.vars.num," ~ ",var.cat,", data = myData.work,xlab=\"",var.cat,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj2 <- bwplot(",var.cat," ~ as.factor(",y.vars.num,"), data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	# upper stream
# 	command.string <- paste("plot.obj3 <- xyplot(",y.vars.num," ~ ",var.cat,", data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",var.cat,"\",ylab=\"",y.vars.num,"\",main=paste(\"(Upper) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj4 <- bwplot(",var.cat," ~ as.factor(",y.vars.num,"), data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",y.vars.num,"\",ylab=\"",var.cat,"\",main=paste(\"(Upper) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	
# 	# lower stream
# 	command.string <- paste("plot.obj5 <- xyplot(",y.vars.num," ~ ",var.cat,", data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",var.cat,"\",ylab=\"",y.vars.num,"\",main=paste(\"(Lower) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj6 <- bwplot(",var.cat," ~ as.factor(",y.vars.num,"), data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",y.vars.num,"\",ylab=\"",var.cat,"\",main=paste(\"(Lower) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"green\")",sep="")
# 	eval(parse(text=command.string))
# 	
# 	plot(plot.obj1,split=c(1,1,2,3))
# 	plot(plot.obj2,split=c(2,1,2,3),newpage=FALSE)	
# 	plot(plot.obj3,split=c(1,2,2,3),newpage=FALSE)	
# 	plot(plot.obj4,split=c(2,2,2,3),newpage=FALSE)	
# 	plot(plot.obj5,split=c(1,3,2,3),newpage=FALSE)	
# 	plot(plot.obj6,split=c(2,3,2,3),newpage=FALSE)	
# 	
# 	# Segment-wise: scatter plots
# 	command.string <- paste("plot.obj <- xyplot(",y.vars.num," ~ ",var.cat," | Segment, data = myData.work,xlab=\"",var.cat,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 
# 	# Segment-wise: boxplot plots
# 	command.string <- paste("plot.obj <- bwplot(",var.cat," ~ as.factor(",y.vars.num,") | Segment, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 	
# 	# Macro-wise: scatter plots
# 	command.string <- paste("plot.obj <- xyplot(",y.vars.num," ~ ",var.cat," | macro.type, data = myData.work,xlab=\"",var.cat,"\",ylab=\"",y.vars.num,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 
# 	# Macro-wise: boxplot plots
# 	command.string <- paste("plot.obj <- bwplot(",var.cat," ~ as.factor(",y.vars.num,") | macro.type, data = myData.work,xlab=\"",y.vars.num,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.num,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 	
 	#
 	# make cross table
 	#
 	command.string <- paste("myData.tmp <- myData.work[,c(\"",var.cat,"\",\"",y.vars.cat,"\")]",sep="")
 	eval(parse(text=command.string))
 
 	tmp.table <- data.frame(table(myData.tmp))
 	command.string <- paste("tmp.df <- cast(tmp.table,",var.cat," ~ ",y.vars.cat,")",sep="")
 	eval(parse(text=command.string))
 
 	# write the frequency out
 	cat(paste(var.cat," vs ",y.vars.cat,",",sep=""),file=FL.Summary,append=TRUE)
 	write.table(tmp.df,file=FL.Summary,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
 	cat("\n\n",file=FL.Summary,append=TRUE)
 }
# dev.off()
 cat(paste("\n\nchecked correlation of discrete variables vs the [count] of Pallid Sturgeon!\n",sep=""))
 cat(paste("\n\nchecked correlation of discrete variables vs the [count] of Pallid Sturgeon!\n",sep=""),file=FL.LOG,append=TRUE)
 



# -------------------------------------------------------------------------------------------------
# 8. check correlation of continusous variables vs the [count] of Pallid Sturgeon: 4_Chk_May14_2013Data_Binary_numVar.pdf; 	FL.Summary == [4_Chk_May14_2013Data_summary.csv]
# -------------------------------------------------------------------------------------------------
FL.binary_num.PDF  <- paste(Path.Out,"4_Chk_May14_2013Data_Binary_numVar.pdf",sep="/")	
if  (file.exists(FL.binary_num.PDF)) {print(paste(FL.binary_num.PDF, "exist.Delete it!")); file.remove(FL.binary_num.PDF)}
pdf(file = FL.binary_num.PDF,         paper="a4r",width=0,height=0)	

cat(paste("check correlation of count with some variables!\n",sep=""))
cat(paste("\nCheck the distribution of continuous variables in terms of the count of Pallid Sturgeon!\n",sep=""),file=FL.Summary,append=TRUE)
cat(paste("\nCheck the distribution of continuous variables in terms of the count of Pallid Sturgeon!\n",sep=""),file=FL.LOG,    append=TRUE)
for (var.num in x.vars.num)
{
	# 1. 
	# all data
	command.string <- paste("plot.obj1 <- bwplot(",var.num," ~ as.factor(",y.vars.cat,"), data = myData.work,xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(All: River Stream)) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"red\",scale=list(y=\"same\"))",sep="")
	eval(parse(text=command.string))

	# upper stream
	command.string <- paste("plot.obj2 <- bwplot(",var.num," ~ as.factor(",y.vars.cat,"), data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(Upper: River Stream) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"blue\",scale=list(y=\"same\"))",sep="")
	eval(parse(text=command.string))
	
	# lower stream
	command.string <- paste("plot.obj3 <- bwplot(",var.num," ~ as.factor(",y.vars.cat,"), data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(Lower: River Stream) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"green\",scale=list(y=\"same\"))",sep="")
	eval(parse(text=command.string))
	
	plot(plot.obj1,split=c(1,1,3,1))
	plot(plot.obj2,split=c(2,1,3,1),newpage=FALSE)	
	plot(plot.obj3,split=c(3,1,3,1),newpage=FALSE)	
	
	# 2. Segment-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.cat,") | Segment, data = myData.work,xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(All:segment) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"red\",scale=list(y=\"same\"))",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
	
	# 3. Macro-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.cat,") | macro.type, data = myData.work,xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(All: Macro Habitat) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"blue\",scale=list(y=\"same\"))",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)

	# 4. MicroClass-wise: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.cat,") | MicroClass, data = myData.work,xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(All: Micro Habitat) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"blue\",scale=list(y=\"same\"))",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
		
	# 5. Gear Type1: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.cat,") | gear.type1, data = myData.work,xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(All: Gear Tyep1) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"blue\",scale=list(y=\"same\"))",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)
	
	# 6. Gear Type2: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.cat,") | gear.type2, data = myData.work,xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(All: Gear Tyep2) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"blue\",scale=list(y=\"same\"))",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)	
	
	# 7. Sample Year: boxplot plots
	command.string <- paste("plot.obj <- bwplot(",var.num," ~ as.factor(",y.vars.cat,") | SY, data = myData.work,xlab=\"",y.vars.cat,"\",ylab=\"",var.num,"\",main=paste(\"(All: Sample Year) \",\"",y.vars.cat,"\",\" vs \",\"",var.num,"\",sep=\"\"),col=\"blue\",scale=list(y=\"same\"))",sep="")
	eval(parse(text=command.string))
	plot(plot.obj)		
	
	
}
dev.off()
cat(paste("\n\nchecked correlation of continusous variables vs the [count] of Pallid Sturgeon!\n",sep=""))
cat(paste("\n\nchecked correlation of continusous variables vs the [count] of Pallid Sturgeon!\n",sep=""),file=FL.LOG,append=TRUE)



# -------------------------------------------------------------------------------------------------
# 9. check the correlation of discrete variables vs the [count] of Pallid Sturgeon
# -------------------------------------------------------------------------------------------------
# FL.binary_cat.PDF  <- paste(Path.Out,"4_Chk_May14_2013Data_Binary_catVar.pdf",sep="/")	
# if  (file.exists(FL.binary_cat.PDF)) {print(paste(FL.binary_cat.PDF, "exist.Delete it!")); file.remove(FL.binary_cat.PDF)}
# pdf(file = FL.binary_cat.PDF,         paper="a4r",width=0,height=0)	
# 
FL.binary_cat.csv <- paste(Path.Out,"4_Chk_May14_2013Data_Binary_catVar.csv",sep="/")
if  (file.exists(FL.binary_cat.csv)) {print(paste(FL.binary_cat.csv, "exist.Delete it!")); file.remove(FL.binary_cat.csv)}

 cat(paste("Check the distribution of variables in terms of Presence/Absence of Pallid Sturgeon!\n",sep=""),file=FL.binary_cat.csv,append=TRUE)
 cat(paste("Check the distribution of variables in terms of Presence/Absence of Pallid Sturgeon!\n",sep=""),file=FL.LOG,    append=TRUE)
 for (var.cat in x.vars.cat)
 {
# 	# all data
# 	command.string <- paste("plot.obj1 <- xyplot(",y.vars.cat," ~ ",var.cat,", data = myData.work,xlab=\"",var.cat,"\",ylab=\"",y.vars.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj2 <- bwplot(",var.cat," ~ as.factor(",y.vars.cat,"), data = myData.work,xlab=\"",y.vars.cat,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	# upper stream
# 	command.string <- paste("plot.obj3 <- xyplot(",y.vars.cat," ~ ",var.cat,", data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",var.cat,"\",ylab=\"",y.vars.cat,"\",main=paste(\"(Upper) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj4 <- bwplot(",var.cat," ~ as.factor(",y.vars.cat,"), data = myData.work[myData.work[,\"Reach\"]==\"Upper\",],xlab=\"",y.vars.cat,"\",ylab=\"",var.cat,"\",main=paste(\"(Upper) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	
# 	# lower stream
# 	command.string <- paste("plot.obj5 <- xyplot(",y.vars.cat," ~ ",var.cat,", data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",var.cat,"\",ylab=\"",y.vars.cat,"\",main=paste(\"(Lower) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"green\")",sep="")
# 	eval(parse(text=command.string))
# 
# 	command.string <- paste("plot.obj6 <- bwplot(",var.cat," ~ as.factor(",y.vars.cat,"), data = myData.work[myData.work[,\"Reach\"]==\"Lower\",],xlab=\"",y.vars.cat,"\",ylab=\"",var.cat,"\",main=paste(\"(Lower) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"green\")",sep="")
# 	eval(parse(text=command.string))
# 	
# 	plot(plot.obj1,split=c(1,1,2,3))
# 	plot(plot.obj2,split=c(2,1,2,3),newpage=FALSE)	
# 	plot(plot.obj3,split=c(1,2,2,3),newpage=FALSE)	
# 	plot(plot.obj4,split=c(2,2,2,3),newpage=FALSE)	
# 	plot(plot.obj5,split=c(1,3,2,3),newpage=FALSE)	
# 	plot(plot.obj6,split=c(2,3,2,3),newpage=FALSE)	
# 	
# 	# Segment-wise: scatter plots
# 	command.string <- paste("plot.obj <- xyplot(",y.vars.cat," ~ ",var.cat," | Segment, data = myData.work,xlab=\"",var.cat,"\",ylab=\"",y.vars.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 
# 	# Segment-wise: boxplot plots
# 	command.string <- paste("plot.obj <- bwplot(",var.cat," ~ as.factor(",y.vars.cat,") | Segment, data = myData.work,xlab=\"",y.vars.cat,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"red\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 	
# 	# Macro-wise: scatter plots
# 	command.string <- paste("plot.obj <- xyplot(",y.vars.cat," ~ ",var.cat," | macro.type, data = myData.work,xlab=\"",var.cat,"\",ylab=\"",y.vars.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),type=\"p\",pch=16,col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 
# 	# Macro-wise: boxplot plots
# 	command.string <- paste("plot.obj <- bwplot(",var.cat," ~ as.factor(",y.vars.cat,") | macro.type, data = myData.work,xlab=\"",y.vars.cat,"\",ylab=\"",var.cat,"\",main=paste(\"(All) \",\"",y.vars.cat,"\",\" vs \",\"",var.cat,"\",sep=\"\"),col=\"blue\")",sep="")
# 	eval(parse(text=command.string))
# 	plot(plot.obj)
# 	
 	#
 	# make cross table
 	#
 	command.string <- paste("myData.tmp <- myData.work[,c(\"",var.cat,"\",\"",y.vars.cat,"\")]",sep="")
 	eval(parse(text=command.string))
 
 	tmp.table <- data.frame(table(myData.tmp))
 	command.string <- paste("tmp.df <- cast(tmp.table,",var.cat," ~ ",y.vars.cat,")",sep="")
 	eval(parse(text=command.string))
 
 	# write the frequency out
 	cat(paste(var.cat," vs ",y.vars.cat,",",sep=""),file=FL.binary_cat.csv,append=TRUE)
 	write.table(tmp.df,file=FL.binary_cat.csv,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
 	cat("\n\n",file=FL.binary_cat.csv,append=TRUE)
 }
# dev.off()
 cat(paste("\n\nchecked correlation of discrete variables vs the [count] of Pallid Sturgeon!\n",sep=""))
 cat(paste("\n\nchecked correlation of discrete variables vs the [count] of Pallid Sturgeon!\n",sep=""),file=FL.LOG,append=TRUE)



# #
# # to have consistent format for the summary (i.e., have Pallid Count 0 to 11 as the column header,
# # 1.
#       summary.count  <- aggregate(myData[,"Pallid_Only_Count"],list(myData[,"Pallid_Only_Count"]),length)
# names(summary.count) <- c("Count","value")
# cat(paste("Count distribution\n"),file=FL.Summary,append=TRUE)
# write.table(t(summary.count),file=FL.Summary,sep=",",row.names=TRUE,col.names=FALSE,append=TRUE)	# write [sum.GearType2.Macro.Count] out
# 
# 
# # 2. 
#       summary.year.count  <- aggregate(myData[,"Pallid_Only_Count"],list(myData[,"SY"],myData[,"Pallid_Only_Count"]),length)
# names(summary.year.count) <- c("Year","Count","value")
# tmp <- cast(summary.year.count,Year~Count,fun=sum,margins=c("grand_row","grand_col"))
# write.table(as.data.frame(tmp),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
# 
# 3. 
#       summary.year.gearType1.count  <- aggregate(myData[,"Pallid_Only_Count"],list(myData[,"SY"],myData[,"gear.type1"],myData[,"Pallid_Only_Count"]),length)
# names(summary.year.gearType1.count) <- c("Year","gear.type1","Count","value")
# tmp <- cast(summary.year.gearType1.count,Year~Count | gear.type1,fun=sum,rm.na=FALSE)
# write.table(as.data.frame(tmp),file=FL.Summary,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
# 
# no.rows <- length(unique(myData[,"SY"]))
# no.cols <- length(unique(myData[,"Pallid_Only_Count"]))
#  data.default <- data.frame(matrix(rep(0,no.rows * no.cols),nrow=no.rows))
#      names(data.default) <- sort(unique(myData[,"Pallid_Only_Count"]))
#  row.names(data.default) <- sort(unique(myData[,"SY"]))
#  data.default <- cbind(Year = sort(unique(myData[,"SY"])),data.default)
 
 
 
 


# get summary of the data



# 1   objectID					should ignore.  not sure what it is.  It is unique.
# 2   SY					should keep.	Sample year (2006-2011)
# 3   FieldOffice				should ignore.  One of the 7 field office (CF, MO, NE, SD, GP, MR, MT)
# 4   Project					should ignore.  always take 1.  one of the four (01: PSPAP, 02: Habitat Assessment, 03: Chute Study-Mitigation), 04: Spring Rise Evaluation)
# 5   UniqueIdentifier				should ignore.  Four-digits (1-9999) (Required). Restarts to 1 at the beginning of every sampling year. NOTE: it is very critical that the UID’s are never the same for each Segment.
# 6   Gear					should keep.	five digit code of gear type (9 values: GN14S, GN18S, GN41S, GN48S, OT16S, TLC1S, TLC2S, TNS, TN25S) where "S" stands for "standard", "W" for "Wild Gear" and "E" for "Evaluation".  Should turn into 4 gear types, GN, OT, TLC, TN)
# 7   Season					should keep.	either ST: Sturgeon or FC: Fish Community seasons
# 8   Bend					should keep.	bend index from 1 to 87
# 9   BendRN					should ignore.	R: random selected bend and N: non-randomly selected bend.  Here we only retain the randomly selected bend.
# 10  BendRiverMile				Is it useful?	Record the upper river mile identifying the bend being sampled in the Missouri River or the river mile of the tributary being sampled. All tributaries being sampled have segment numbers.
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