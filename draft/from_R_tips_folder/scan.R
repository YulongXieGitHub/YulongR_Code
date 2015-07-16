#
# Compare_mutzCalc_IECC_ASHRAE.R 
# -------------------------------------------------------------------------------------------------
#
# July 13, 2012
# Since the way we initialized the 3 IECC standards, the revised mutz calculation script (IECC and ASHRAE calculated differently) cannot be directly verified except IECC 2006.
# This script is to indirectly verifying the results through comparing the mutz calculated values stored in "primary_" files in
# 1st folder                  "\MUTZ\_p.addendumCK\calculation_revised_4_both_IECC_and_ASHRAE\" folder and
# 2nd folder                  "\MUTZ\_p.addendumCK\calculation_revised_4_IECC\" folder
# For the same case started with "ASHRAE_", the Vot in the 2nd folder should be no smaller than those in the 1st folder 
#                                           the MDP in the 2nd folder should be no larger  than those in the 1st folder 
#
#
rm(list = ls(all = TRUE))
start_time <- date();
# format(Sys.time(), "%a %b %d %X %Y %Z")
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number


# close all devices which have been opened
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

# -------------------------------------------------------------------------------------------------
# parameter for plotting
# -------------------------------------------------------------------------------------------------
lty.array  <- c(rep(1,10),rep(3,10),rep(5,10),rep(1,500))		# 
lty.array  <- c(rep(1,500))
pch.array  <- c(rep(c(1,2,5,6,16,17,18),500))
cex.array  <- c(rep(2,500))
lwd.array  <- c(rep(2,10),rep(2,10),rep(2,10),rep(2,500))
col.array  <- c("red","blue","green","magenta","cyan","purple","azure","black","brown","aliceblue","beige","bisque","chocolate","darkgrey",
                 "burlywood","aquamarine","deepskyblue","gold","ivory","maroon","orange","orchid","pink","tomato","rosybrown","violet","plum","yellow","salmon","springgreen")
col.array  <- rep(c("red","blue","green","magenta","cyan","black","pink","violet","grey","brown"),500)

# -------------------------------------------------------------------------------------------------
# 	change to the script directory
# ------------------------------------------------------------------------------------------------- 
if(.Platform$OS.type == "unix") 
{
	Path.Current <- "../00_scripts";
}else{
	Path.Current <- "../00_scripts";
}

setwd(Path.Current)

# define the target minimum system ventilation effectiveness "Evt"
Evt <- 0.6

# define data paths
Path.4IECC <- "../calculation_revised_4_IECC"
Path.4Both <- "../calculation_revised_4_both_IECC_and_ASHRAE"
Path.OUT   <- "../Compare_mutzCalc_IECC_ASHRAE"
Path.LOG   <- "../00_log"

FL.SUM.MDP <- paste(Path.OUT,"Summary_MDP.csv",sep="/")
FL.SUM.Vot <- paste(Path.OUT,"Summary_Vot.csv",sep="/")
FL.LOG     <- paste(Path.LOG,"Compare_mutzCalc_IECC_ASHRAE.log",sep="/")								# output file

if (!file.exists(Path.4IECC)){stop(paste("Why ",Path.4IECC," does not exist. Check it!",sep=""))}
if (!file.exists(Path.4Both)){stop(paste("Why ",Path.4Both," does not exist. Check it!",sep=""))}
if (!file.exists(Path.OUT)){print(paste(Path.OUT," does not exist. Create it!"));dir.create(Path.OUT)}
if (!file.exists(Path.LOG)){print(paste(Path.LOG," does not exist. Create it!"));dir.create(Path.LOG)}
if (file.exists(FL.SUM.MDP)){print(paste(FL.SUM.MDP,"exist.Delete it!"));file.remove(FL.SUM.MDP)}	
if (file.exists(FL.SUM.Vot)){print(paste(FL.SUM.Vot,"exist.Delete it!"));file.remove(FL.SUM.Vot)}	
if (file.exists(FL.LOG)){print(paste(FL.LOG,"exist.Delete it!"));file.remove(FL.LOG)}	


# prototypes
prototypes <- c("Hospital","HotelLarge","OfficeLarge","OfficeMedium","OutPatientHealthCare","SchoolPrimary","SchoolSecondary")

# OA scenarios
OA.scenarios <- c("ASHRAE901_STD2004_OA1999","ASHRAE901_STD2004_OA2004","ASHRAE901_STD2007_OA2004","ASHRAE901_STD2010_OA2004","ASHRAE901_STD2013_OA2004");

# 
Paths <- c("calculation_revised_4_IECC","calculation_revised_4_both_IECC_and_ASHRAE")
# load libraries
library("lattice")
#library("splus2R")	# for convertin case in a string


# write to output file
cat(paste("Log file for data processing script [Compare_mutzCalc_IECC_ASHRAE.R]!\n",sep=""),file=FL.LOG, append=TRUE)
cat(paste("\n***************************************************************************",
            "*                      [Compare_mutzCalc_IECC_ASHRAE.R]      *",
            "***************************************************************************",sep="\n"),file=FL.LOG, append=TRUE)



# -------------------------------------------------------------------------------------------------
# loop through prototypes
# -------------------------------------------------------------------------------------------------
no.sum <- 0
MDP_string <- list("character","character","character","character","character","numeric","character","numeric")
Vot_string <- list("character","character","character","character",            "numeric","character")

for (prototype in prototypes)
{
	
	no.sum <- no.sum + 1
	
	if       (prototype == "Hospital")
	{
		L0_MDP <- 4
		L1_MDP <- 394
		L0_Vot <- 398
		L1_Vot <- 431
	}else if (prototype == "HotelLarge")
	{
		L0_MDP <- 4
		L1_MDP <- 224
		L0_Vot <- 228
		L1_Vot <- 244	
	}else if (prototype == "OfficeLarge")
	{
		L0_MDP <- 4
		L1_MDP <- 258
		L0_Vot <- 262
		L1_Vot <- 312	
	}else if (prototype == "OfficeMedium")
	{
		L0_MDP <- 4
		L1_MDP <- 258
		L0_Vot <- 262
		L1_Vot <- 312	
	}else if (prototype == "OutPatientHealthCare")
	{
		L0_MDP <- 4
		L1_MDP <- 1074
		L0_Vot <- 1078
		L1_Vot <- 1094	
	}else if (prototype == "SchoolPrimary")
	{
		L0_MDP <- 4
		L1_MDP <- 377
		L0_Vot <- 381
		L1_Vot <- 448	
	}else if (prototype == "SchoolSecondary")
	{
		L0_MDP <- 4
		L1_MDP <- 700
		L0_Vot <- 704
		L1_Vot <- 771	
	}
	


	# -----------------------------------------------------------------------------------------
	# loop of OA scenarios
	# -----------------------------------------------------------------------------------------
	
	for (OA.scenario in OA.scenarios)
	{
		
		for (Path in Paths)
		{
			if (Path == "calculation_revised_4_IECC")
			{
				Path.IN <- Path.4IECC
			}else if (Path == "calculation_revised_4_both_IECC_and_ASHRAE")
			{
				Path.IN <- Path.4Both
			}
			
			

			
			# define the primary file name
			FL.primary <- paste(Path.IN,paste("primary_",prototype,"_",OA.scenario,".csv",sep=""),sep="/")
			if (!file.exists(FL.primary)){stop(paste("Why ",FL.primary," does not exist. Check it!",sep=""))}			# output file


			for (variable in c("MDP","Vot"))
			{
				cat(paste(prototype,"-",OA.scenario,"-",variable,"\n",sep=""))
				
				
				if (variable == "MDP")
				{
					L0 <- L0_MDP-1
					L1 <- L1_MDP-L0_MDP + 1
					var.string <- MDP_string

					A <- scan(file = FL.primary, what = var.string,sep = ",",skip = L0, nlines = L1, na.strings = "NA")


					D <- data.frame(OA.scenario = as.character(unlist(A[1])),
							climate     = as.character(unlist(A[2])),
							protptype   = as.character(unlist(A[3])),
							sys         = as.character(unlist(A[4])),
							zone        = as.character(unlist(A[5])),
							MDPa        = as.numeric(unlist(A[6])),
							climate.zone= as.character(unlist(A[7])),
							MDPa.revised= as.numeric(unlist(A[8])))
							
							if (Path == "calculation_revised_4_IECC")
							{
								myData.MDP.IECC <- D
								names(myData.MDP.IECC) <- paste("IECC_",names(myData.MDP.IECC),sep="")
							}else if (Path == "calculation_revised_4_both_IECC_and_ASHRAE")
							{
								myData.MDP.Both <- D
								names(myData.MDP.Both) <- paste("Both_",names(myData.MDP.Both),sep="")
							}							
					
					
				}else if(variable == "Vot")
				{
					L0 <- L0_Vot-1
					L1 <- L1_Vot-L0_Vot + 1	
					var.string <- Vot_string
					
					A <- scan(file = FL.primary, what = var.string,sep = ",",skip = L0, nlines = L1, na.strings = "NA")


					D <- data.frame(OA.scenario = as.character(unlist(A[1])),
							climate     = as.character(unlist(A[2])),
							protptype   = as.character(unlist(A[3])),
							sys         = as.character(unlist(A[4])),
							Vot         = as.numeric(unlist(A[5])),
							climate.zone= as.character(unlist(A[6])))
							
							if (Path == "calculation_revised_4_IECC")
							{
								myData.Vot.IECC <- D
								names(myData.Vot.IECC) <- paste("IECC_",names(myData.Vot.IECC),sep="")
							}else if (Path == "calculation_revised_4_both_IECC_and_ASHRAE")
							{
								myData.Vot.Both <- D
								names(myData.Vot.Both) <- paste("Both_",names(myData.Vot.Both),sep="")
							}
											
					
				}
			}
		}
		
		# combind the two batch and 
		myData.MDP <- cbind(myData.MDP.IECC,myData.MDP.Both)
		myData.Vot <- cbind(myData.Vot.IECC,myData.Vot.Both)
		
		# quick check
		if (!(setequal(myData.MDP.IECC[,c("IECC_OA.scenario","IECC_climate","IECC_protptype","IECC_sys","IECC_zone","IECC_climate.zone")],
		               myData.MDP.Both[,c("Both_OA.scenario","Both_climate","Both_protptype","Both_sys","Both_zone","Both_climate.zone")])))
		{
			cat("something wrong in MDP\n")
			die;
		}

		if (!(setequal(myData.Vot.IECC[,c("IECC_OA.scenario","IECC_climate","IECC_protptype","IECC_sys","IECC_climate.zone")],
		               myData.Vot.Both[,c("Both_OA.scenario","Both_climate","Both_protptype","Both_sys","Both_climate.zone")])))
		{
			cat("something wrong in Vot\n")
			die;
		}
		
		# compare
		MDP.OK <- rep("OK",dim(myData.MDP)[1])
		Vot.OK <- rep("OK",dim(myData.Vot)[1])
		
		# look for violation cases
		MDP.OK[myData.MDP.IECC[,"IECC_MDPa.revised"] > myData.MDP.Both[,"Both_MDPa.revised"]] <- "Bad"
		Vot.OK[myData.Vot.IECC[,"IECC_Vot"]          < myData.Vot.Both[,"Both_Vot"]]          <- "Bad"
		
		# combined
		myData.MDP <- cbind(myData.MDP,MDP.OK)
		myData.Vot <- cbind(myData.Vot,Vot.OK)
		
		
		# write to the summary file
		if (no.sum == 1)
		{
			write.table(myData.MDP,file = FL.SUM.MDP,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
			write.table(myData.Vot,file = FL.SUM.Vot,sep=",",row.names=FALSE,col.names=TRUE,append=TRUE)
		}else
		{
			write.table(myData.MDP,file = FL.SUM.MDP,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE)
			write.table(myData.Vot,file = FL.SUM.Vot,sep=",",row.names=FALSE,col.names=FALSE,append=TRUE)		
		}			
	}
}
				
	
		    
# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\nCompare_mutzCalc_IECC_ASHRAE.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\nCompare_mutzCalc_IECC_ASHRAE.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [Compare_mutzCalc_IECC_ASHRAE.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [Compare_mutzCalc_IECC_ASHRAE.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)
