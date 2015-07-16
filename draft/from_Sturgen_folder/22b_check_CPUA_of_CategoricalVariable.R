#
# 22b_check_CPUA_of_CategoricalVariable.R
#
#
# August 15, 2013
# Create this on August 15, 2013 based on the 11 series of R scripts.
# The purpose is to remove the NA data of those variables we are going to use eventually in front.
# This will make sure the data statistics / plots are consistsent with the modeling results 
#
# August 14, 2013
#

FL.OBJ <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/22_Prep_Subset_Data/22_Prep_Subset_Data.Rdata"
load(FL.OBJ)

subsets.section <- c("Upper","Upper","Lower","Lower","Lower","Upper",   "Lower",   "All")
subsets.gear    <- c("OT_TN","TLC",  "OT_TN","TLC",  "GN",   "Logistic","Logistic","All")


var.cat <- c("SY","Season","macro.type","new_meso","new_part","new_lith","new_NFHAP","Seg")


# Data Folder
Path.IN  <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/22_Prep_Subset_Data"
Path.log <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.OUT <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/22b_check_CPUA_of_CategoricalVariable"
if (!file.exists(Path.IN)) {stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log)){print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.OUT)){print(paste("NOT existing:",Path.OUT));dir.create(Path.OUT,showWarnings=TRUE,recursive=TRUE)}

# data file
FL.OBJ.IN <- paste(Path.IN, "22_Prep_Subset_Data.RData", sep="/")
FL.LOG    <- paste(Path.log,"22b_check_CPUA_of_CategoricalVariable.log",sep="/")	
if (!file.exists(FL.OBJ.IN)){stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.LOG))   {print(paste(FL.LOG,      "exist.Delete it!")); file.remove(FL.LOG)}


subsets.section <- c("Upper","Upper","Lower","Lower","Lower","Upper",   "Lower",   "All")
subsets.gear    <- c("OT_TN","TLC",  "OT_TN","TLC",  "GN",   "Logistic","Logistic","All")

for (idx.subset in c(1,2,3,4,5))
{

	subset.section <- subsets.section[idx.subset]
	subset.gear    <- subsets.gear[idx.subset]

	#
	Path.OUT.subset <- paste(Path.OUT,paste(paste("subset",idx.subset,sep=""),subset.section,subset.gear,sep="_"),sep="/")
	if (!file.exists(Path.OUT.subset))    {print(paste("NOT existing:",Path.OUT.subset));dir.create(Path.OUT.subset,showWarnings=TRUE,recursive=TRUE)}


	FL.OUT.CSV <- paste(Path.OUT.subset, paste(paste("subset",idx.subset,sep=""),subset.section,"_",subset.gear,".csv",sep=""),sep="/")
	FL.OUT.PDF <- paste(Path.OUT.subset, paste(paste("subset",idx.subset,sep=""),subset.section,"_",subset.gear,".pdf",sep=""),sep="/")
	if  (file.exists(FL.OUT.CSV)){print(paste(FL.OUT.CSV,"exist.Delete it!")); file.remove(FL.OUT.CSV)}
	if  (file.exists(FL.OUT.PDF)){print(paste(FL.OUT.PDF,"exist.Delete it!")); file.remove(FL.OUT.PDF)}


	# subset 1: upper OT and TN (active gear) [myData.Upper_Active]
	if      (idx.subset == 1)
	{		
		myData.Sub      <- myData.Upper_Active  
		new.y           <- "CPUA.calc"
	# subset 2: upper TLC [myData.Upper_TLC]
	}else if(idx.subset == 2){
		myData.Sub      <- myData.Upper_TLC    
		new.y           <- "CPUE.calc"
	# subset 3: lower OT & TN	[myData.Lower_Active]
	}else if(idx.subset == 3){
		myData.Sub      <- myData.Lower_Active    
		new.y           <- "CPUA.calc"
	# subset 4: lower TLC [myData.Lower_TLC]
	}else if(idx.subset == 4){
		myData.Sub      <- myData.Lower_TLC    
		new.y           <- "CPUE.calc"
	# subset 5: lower GN [myData.Lower_GN]
	}else if(idx.subset == 5){
		myData.Sub      <- myData.Lower_GN    
		new.y           <- "CPUE.calc"
	}

	# get the reduced data set
	myData.Work <- myData.Sub[,c("binary","Pal_cnt",new.y,var.cat)]
		

	# -------------------
	# SY
	# -------------------
	A <- myData.Work
	AA <- A[,c("binary","Pal_cnt",new.y,"SY")]
	tapply(AA[,new.y],AA[,"SY"],mean,na.rm=TRUE)
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	B1 <- tapply(AA[,"binary"],AA[,"SY"],length)
	B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"SY"],sum)
	SY_0_rate <- B2/B1
	#     2006      2007      2008      2009      2010      2011 
	# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

	SY_all_mean <- tapply(AA[,new.y],AA[,"SY"],mean,na.rm=TRUE)
	#       2006       2007       2008       2009       2010       2011 
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	SY_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"SY"],mean,na.rm=TRUE)
	#     2006      2007      2008      2009      2010      2011 
	# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
	SY_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"SY"],median,na.rm=TRUE)
	#      2006      2007      2008      2009      2010      2011 
	# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 

	# -------------------
	# new_meso
	# -------------------
	A <- myData.Work
	AA <- A[,c("binary","Pal_cnt",new.y,"new_meso")]
	tapply(AA[,new.y],AA[,"new_meso"],mean,na.rm=TRUE)
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	B1 <- tapply(AA[,"binary"],AA[,"new_meso"],length)
	B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"new_meso"],sum)
	new_meso_0_rate <- B2/B1
	#     2006      2007      2008      2009      2010      2011 
	# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

	new_meso_all_mean <- tapply(AA[,new.y],AA[,"new_meso"],mean,na.rm=TRUE)
	#       2006       2007       2008       2009       2010       2011 
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	new_meso_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"new_meso"],mean,na.rm=TRUE)
	#     2006      2007      2008      2009      2010      2011 
	# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
	new_meso_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"new_meso"],median,na.rm=TRUE)
	#      2006      2007      2008      2009      2010      2011 
	# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 



	# -------------------
	# new_lith
	# -------------------
	A <- myData.Work
	AA <- A[,c("binary","Pal_cnt",new.y,"new_lith")]
	tapply(AA[,new.y],AA[,"new_lith"],mean,na.rm=TRUE)
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	B1 <- tapply(AA[,"binary"],AA[,"new_lith"],length)
	B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"new_lith"],sum)
	new_lith_0_rate <- B2/B1
	#     2006      2007      2008      2009      2010      2011 
	# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

	new_lith_all_mean <- tapply(AA[,new.y],AA[,"new_lith"],mean,na.rm=TRUE)
	#       2006       2007       2008       2009       2010       2011 
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	new_lith_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"new_lith"],mean,na.rm=TRUE)
	#     2006      2007      2008      2009      2010      2011 
	# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
	new_lith_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"new_lith"],median,na.rm=TRUE)
	#      2006      2007      2008      2009      2010      2011 
	# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 


	# -------------------
	# new_part
	# -------------------
	A <- myData.Work
	AA <- A[,c("binary","Pal_cnt",new.y,"new_part")]
	tapply(AA[,new.y],AA[,"new_part"],mean,na.rm=TRUE)
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	B1 <- tapply(AA[,"binary"],AA[,"new_part"],length)
	B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"new_part"],sum)
	new_part_0_rate <- B2/B1
	#     2006      2007      2008      2009      2010      2011 
	# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

	new_part_all_mean <- tapply(AA[,new.y],AA[,"new_part"],mean,na.rm=TRUE)
	#       2006       2007       2008       2009       2010       2011 
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	new_part_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"new_part"],mean,na.rm=TRUE)
	#     2006      2007      2008      2009      2010      2011 
	# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
	new_part_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"new_part"],median,na.rm=TRUE)
	#      2006      2007      2008      2009      2010      2011 
	# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 


	# -------------------
	# new_NFHAP
	# -------------------
	A <- myData.Work
	AA <- A[,c("binary","Pal_cnt",new.y,"new_NFHAP")]
	tapply(AA[,new.y],AA[,"new_NFHAP"],mean,na.rm=TRUE)
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	B1 <- tapply(AA[,"binary"],AA[,"new_NFHAP"],length)
	B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"new_NFHAP"],sum)
	new_NFHAP_0_rate <- B2/B1
	#     2006      2007      2008      2009      2010      2011 
	# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

	new_NFHAP_all_mean <- tapply(AA[,new.y],AA[,"new_NFHAP"],mean,na.rm=TRUE)
	#       2006       2007       2008       2009       2010       2011 
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	new_NFHAP_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"new_NFHAP"],mean,na.rm=TRUE)
	#     2006      2007      2008      2009      2010      2011 
	# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
	new_NFHAP_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"new_NFHAP"],median,na.rm=TRUE)
	#      2006      2007      2008      2009      2010      2011 
	# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 


	# -------------------
	# Season
	# -------------------
	A <- myData.Work
	AA <- A[,c("binary","Pal_cnt",new.y,"Season")]
	tapply(AA[,new.y],AA[,"Season"],mean,na.rm=TRUE)
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	B1 <- tapply(AA[,"binary"],AA[,"Season"],length)
	B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"Season"],sum)
	Season_0_rate <- B2/B1
	#     2006      2007      2008      2009      2010      2011 
	# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

	Season_all_mean <- tapply(AA[,new.y],AA[,"Season"],mean,na.rm=TRUE)
	#       2006       2007       2008       2009       2010       2011 
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	Season_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"Season"],mean,na.rm=TRUE)
	#     2006      2007      2008      2009      2010      2011 
	# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
	Season_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"Season"],median,na.rm=TRUE)
	#      2006      2007      2008      2009      2010      2011 
	# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 


	# -------------------
	# macro.type
	# -------------------
	A <- myData.Work
	AA <- A[,c("binary","Pal_cnt",new.y,"macro.type")]
	tapply(AA[,new.y],AA[,"macro.type"],mean,na.rm=TRUE)
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	B1 <- tapply(AA[,"binary"],AA[,"macro.type"],length)
	B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"macro.type"],sum)
	macro.type_0_rate <- B2/B1
	#     2006      2007      2008      2009      2010      2011 
	# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

	macro.type_all_mean <- tapply(AA[,new.y],AA[,"macro.type"],mean,na.rm=TRUE)
	#       2006       2007       2008       2009       2010       2011 
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	macro.type_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"macro.type"],mean,na.rm=TRUE)
	#     2006      2007      2008      2009      2010      2011 
	# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
	macro.type_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"macro.type"],median,na.rm=TRUE)
	#      2006      2007      2008      2009      2010      2011 
	# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 


	# -------------------
	# Seg
	# -------------------
	A <- myData.Work
	AA <- A[,c("binary","Pal_cnt",new.y,"Seg")]
	tapply(AA[,new.y],AA[,"Seg"],mean,na.rm=TRUE)
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	B1 <- tapply(AA[,"binary"],AA[,"Seg"],length)
	B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"Seg"],sum)
	Seg_0_rate <- B2/B1
	#     2006      2007      2008      2009      2010      2011 
	# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

	Seg_all_mean <- tapply(AA[,new.y],AA[,"Seg"],mean,na.rm=TRUE)
	#       2006       2007       2008       2009       2010       2011 
	# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

	Seg_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"Seg"],mean,na.rm=TRUE)
	#     2006      2007      2008      2009      2010      2011 
	# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
	Seg_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,new.y],AA[AA[,"Pal_cnt"]>0,"Seg"],median,na.rm=TRUE)
	#      2006      2007      2008      2009      2010      2011 
	# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 
	
	
	
	SY.out        <- data.frame(cbind(zero.rate = SY_0_rate,mean.CPUA=SY_all_mean,mean_CPUA_presence=SY_presence_mean,median_CPUA_presence=SY_presence_median))
	new_NFHAP.out <- data.frame(cbind(zero.rate = new_NFHAP_0_rate,mean.CPUA=new_NFHAP_all_mean,mean_CPUA_presence=new_NFHAP_presence_mean,median_CPUA_presence=new_NFHAP_presence_median))
	new_meso.out  <- data.frame(cbind(zero.rate = new_meso_0_rate,mean.CPUA=new_meso_all_mean,mean_CPUA_presence=new_meso_presence_mean,median_CPUA_presence=new_meso_presence_median))
	new_lith.out  <- data.frame(cbind(zero.rate = new_lith_0_rate,mean.CPUA=new_lith_all_mean,mean_CPUA_presence=new_lith_presence_mean,median_CPUA_presence=new_lith_presence_median))
	new_part.out  <- data.frame(cbind(zero.rate = new_part_0_rate,mean.CPUA=new_part_all_mean,mean_CPUA_presence=new_part_presence_mean,median_CPUA_presence=new_part_presence_median))
	Season.out    <- data.frame(cbind(zero.rate = Season_0_rate,mean.CPUA=Season_all_mean,mean_CPUA_presence=Season_presence_mean,median_CPUA_presence=Season_presence_median))
	macro.type.out<- data.frame(cbind(zero.rate = macro.type_0_rate,mean.CPUA=macro.type_all_mean,mean_CPUA_presence=macro.type_presence_mean,median_CPUA_presence=macro.type_presence_median))
	Seg.out       <- data.frame(cbind(zero.rate = Seg_0_rate,mean.CPUA=Seg_all_mean,mean_CPUA_presence=Seg_presence_mean,median_CPUA_presence=Seg_presence_median))


	#
	if (new.y == "CPUE.calc")
	{
		names(SY.out)        <- sub("CPUA","CPUE",names(SY.out))
		names(Season.out)    <- sub("CPUA","CPUE",names(Season.out))
		names(new_part.out)  <- sub("CPUA","CPUE",names(new_part.out))
		names(new_lith.out)  <- sub("CPUA","CPUE",names(new_lith.out))
		names(new_meso.out)  <- sub("CPUA","CPUE",names(new_meso.out))
		names(new_NFHAP.out) <- sub("CPUA","CPUE",names(new_NFHAP.out))
		names(macro.type.out)<- sub("CPUA","CPUE",names(macro.type.out))
		names(Seg.out)       <- sub("CPUA","CPUE",names(Seg.out))
	}
	
	
	# -------------------------------
	# OUTPUT
	# -------------------------------
	cat(paste(dim(myData.Sub)[1],sep=""),file=FL.OUT.CSV,append=TRUE)
	cat("SY: zero rate,",file=FL.OUT.CSV,append=TRUE)
	write.table(SY.out,       file=FL.OUT.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
	cat("\n\n",file=FL.OUT.CSV,append=TRUE)

	cat("new_meso: zero rate,",file=FL.OUT.CSV,append=TRUE)
	write.table(new_meso.out,       file=FL.OUT.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
	cat("\n\n",file=FL.OUT.CSV,append=TRUE)

	cat("new_lith: zero rate,",file=FL.OUT.CSV,append=TRUE)
	write.table(new_lith.out,       file=FL.OUT.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
	cat("\n\n",file=FL.OUT.CSV,append=TRUE)

	cat("new_part: zero rate,",file=FL.OUT.CSV,append=TRUE)
	write.table(new_part.out,       file=FL.OUT.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
	cat("\n\n",file=FL.OUT.CSV,append=TRUE)

	cat("Season: zero rate,",file=FL.OUT.CSV,append=TRUE)
	write.table(Season.out,       file=FL.OUT.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
	cat("\n\n",file=FL.OUT.CSV,append=TRUE)

	cat("NFHAP: zero rate,",file=FL.OUT.CSV,append=TRUE)
	write.table(new_NFHAP.out,       file=FL.OUT.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
	cat("\n\n",file=FL.OUT.CSV,append=TRUE)

	cat("macro.type: zero rate,",file=FL.OUT.CSV,append=TRUE)
	write.table(macro.type.out,       file=FL.OUT.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
	cat("\n\n",file=FL.OUT.CSV,append=TRUE)	
	
	cat("Seg: zero rate,",file=FL.OUT.CSV,append=TRUE)
	write.table(Seg.out,       file=FL.OUT.CSV,sep=",",col.names=TRUE,row.names=TRUE,append=TRUE)
	cat("\n\n",file=FL.OUT.CSV,append=TRUE)		
	
	
	# plot the CPUA/CPUE aganist the count
	pdf(file = FL.OUT.PDF,paper="a4r",width=0,height=0)	
		plot(myData.Sub[,new.y] ~ myData.Sub[,"Pal_cnt"],data=myData.Sub,xlab="Pallid Count",ylab=new.y,type="p",pch=16,col="red")
	dev.off()
	
}












