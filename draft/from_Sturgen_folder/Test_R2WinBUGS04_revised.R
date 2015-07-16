#
# Test_R2WinBUGS04.R
#
# The package version used for this paper : 
# R 2.7.0
# MASS 7.2-42
# pscl: 0.95
# sandwich 2.1-0
# car 1.2-8
# lmtest 0.9-21
# March 7, 2013: note: sandwich does not exist for 64 bit machine, so this script hasd to run on a X86 machine.

#
# Test_R2WinBUGS04.R
#
#
# eliminate all stuff
rm(list = ls(all = TRUE))


Path.WinBUGS <- "C:/WinBUGS14"
source("openGraphSaveGraph.R")


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

# -------------------------------------------------------------------------------------------------
# Data Folder and files
# -------------------------------------------------------------------------------------------------
Path.Data.IN <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/Test_Brugs01"
Path.log     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/Test_R2WinBUGS04_Revised"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}


FL.Data.IN  <- paste(Path.Data.IN,"TLC_data.Rdata",sep="/")
FL.Data.OUT <- paste(Path.Out,"Test_R2WinBUGS04_data.csv",sep="/")
FL.RESL.OUT <- paste(Path.Out,"Test_R2WinBUGS04_results.csv",sep="/")
FL.MODEL    <- paste(Path.Out,"Test_R2WinBUGS04_model.Rdata",sep="/")

FL.PostDist.CSV <- paste(Path.Out,"Test_R2WinBUGS04_PostDist.csv",sep="/")
FL.PostDist.OBJ <- paste(Path.Out,"Test_R2WinBUGS04_PostDist.Rdata",sep="/")


FL.LOG      <- paste(Path.log,"Test_R2WinBUGS04.log",sep="/")	
FL.PDF      <- paste(Path.Out,"Test_R2WinBUGS04.pdf",sep="/")	
FL.SUM.cat  <- paste(Path.Out,"Test_R2WinBUGS04_cat.sum",sep="/")
FL.SUM.num  <- paste(Path.Out,"Test_R2WinBUGS04_num.sum",sep="/")

FL.package  <- paste(Path.Current,"package_loading.R",sep="/")


if (!file.exists(FL.Data.IN))  {stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.Data.OUT)) {print(paste(FL.Data.OUT,"exist.Delete it!")); file.remove(FL.Data.OUT)}
if  (file.exists(FL.RESL.OUT)) {print(paste(FL.RESL.OUT,"exist.Delete it!")); file.remove(FL.RESL.OUT)}
if  (file.exists(FL.MODEL))    {print(paste(FL.MODEL,"exist.Delete it!")); file.remove(FL.MODEL)}

if  (file.exists(FL.PostDist.CSV))    {print(paste(FL.PostDist.CSV,"exist.Delete it!")); file.remove(FL.PostDist.CSV)}
if  (file.exists(FL.PostDist.OBJ))    {print(paste(FL.PostDist.OBJ,"exist.Delete it!")); file.remove(FL.PostDist.OBJ)}


if  (file.exists(FL.LOG))      {print(paste(FL.LOG,     "exist.Delete it!")); file.remove(FL.LOG)}
if  (file.exists(FL.PDF))      {print(paste(FL.PDF,     "exist.Delete it!")); file.remove(FL.PDF)}
if  (file.exists(FL.SUM.cat))  {print(paste(FL.SUM.cat, "exist.Delete it!")); file.remove(FL.SUM.cat)}
if  (file.exists(FL.SUM.num))  {print(paste(FL.SUM.num, "exist.Delete it!")); file.remove(FL.SUM.num)}

# open pdf file for outputting plots
pdf(file = FL.PDF,         paper="a4r",width=0,height=0)	

# -------------------------------------------------------------------------------------------------
# Loading packages
# -------------------------------------------------------------------------------------------------
argument <- "glm"
source(FL.package)

# -------------------------------------------------------------------------------------------------
# load data (pscl example data processed by "0_classical_GLM_revised.R")
# -------------------------------------------------------------------------------------------------
load(FL.Data.IN)

# -------------------------------------------------------------------------------------------------
# output the other format used by WinBUGS or BUGS: [Test_R2WinBUGS04_data.csv]
# -------------------------------------------------------------------------------------------------
cat("DATA(LIST)\n",FL.Data.OUT,append=TRUE)
cat("list(n=",dim(myData.reduced)[1],",\n",file=FL.Data.OUT,append=TRUE)
idx <- 0
for (col.name in names(myData.reduced))
{
	idx <- idx + 1
	if (idx == dim(myData.reduced)[2])
	{
		A <- paste(col.name,paste(" = c(",paste(myData.reduced[,col.name],collapse=","),")",sep=""),")\n",sep="")
	}else{
		A <- paste(col.name,paste(" = c(",paste(myData.reduced[,col.name],collapse=","),")",sep=""),",\n",sep="")
	}
	cat(A,file=FL.Data.OUT,append=TRUE)
}
cat("data presented in WinBUGS/BUGS format has been outputted!\n")
cat("data presented in WinBUGS/BUGS format has been outputted!\n",file=FL.LOG,append=TRUE)


library(BRugs)         



#------------------------------------------------------------------------------
# Specify zero inflated Poisson Model (without health)
#------------------------------------------------------------------------------
modelstring = "
	model{
		for (i in 1:n)					
		{
			Pal_cnt[i] ~ dpois(mu[i])		
			mu[i] <- lambda[i,T[i]]	

			# Log-linear model [Poisson means] 
			lambda[i,1] <- 0						
			log(lambda[i,2]) <- b0                      + 			
					    b[1]*D_dist_up[i]       +			
					    b[2]*Chan_wid[i]        + 			
					    b[3]*T_dist_up[i]       + 			
					    b[4]*Grade_10_RM[i]     + 			
					    b[5]*VF_wid[i]     	    +
					    b[6]*Temp[i]               


			# Logistic regression for the [zero-inflation probability]
			logit(P[i,1]) <-    a0                      + 			
					    a[1]*D_dist_up[i]       +			
					    a[2]*Chan_wid[i]        + 			
					    a[3]*T_dist_up[i]       + 			
					    a[4]*Grade_10_RM[i]     + 			
					    a[5]*VF_wid[i]    	    +
					    a[6]*Temp[i]                					    

			P[i,2] <- 1-P[i,1]				
			T[i] ~ dcat(P[i,1:2])		
		}		

		# normal distributions are assumed for all the parameters beta and gamma
		# Flat Priors on parameters
		a0 ~ dnorm(0,1.0E-4)
		b0 ~ dnorm(0,1.0E-4)

		for (j in 1:6){a[j]  ~ dnorm(0,1.0E-4)}
		for (j in 1:6){b[j]  ~ dnorm(0,1.0E-4)}

	}

" # close quote for modelstring
writeLines(modelstring,con="model.txt")
# modelCheck( "model.txt" )
cat(paste("1. Model has been set up!\n"))
cat(paste("1. Model has been set up!\n"),file=FL.LOG,append=TRUE)



#------------------------------------------------------------------------------
# THE DATA.  Data supplies not through external files but through the [datalist]
#------------------------------------------------------------------------------
# Get the data into BUGS:
dataList = list(
		   Pal_cnt        = as.vector(myData.reduced[1:600,"Pal_cnt"]),
		   D_dist_up      = as.vector(myData.reduced[1:600,"D_dist_up"]),
		   Chan_wid       = as.vector(myData.reduced[1:600,"Chan_wid"]),
		   T_dist_up      = as.vector(myData.reduced[1:600,"T_dist_up"]),
		   Grade_10_RM    = as.vector(myData.reduced[1:600,"Grade_10_RM"]),
		   VF_wid         = as.vector(myData.reduced[1:600,"VF_wid"]),		   
		   Temp           = as.vector(myData.reduced[1:600,"Temp"]),	   

		   n = 600
		   # n = dim(myData.reduced)[1] 
)
# modelData( bugsData( datalist ) )
# cat(paste("2. Data has been prepared and loaded!\n"))
# cat(paste("2. Data has been prepared and loaded!\n"),file=FL.LOG,append=TRUE)

#------------------------------------------------------------------------------
# INTIALIZE THE CHAINS. (Initialization of the model is a challenge)
#------------------------------------------------------------------------------
initsList <-  	function(){list(	
 		a0 = -0.06, 							#
 		b0 =  1.19,							#					#
 		a  = c(-6.5712,  -0.4831,  0.4302,  -0.4708, -0.3367,  0.7129),			#
 		b  = c(-0.744121,-0.237194,0.001871,-0.004010,0.094706,0.054689)				#
 	)}

parameters    <- c("a0","b0","a","b")
adaptSteps    <- 100
burnInSteps   <- 200
nChains       <- 3
numSavedSteps <- 3000
thinSteps     <- 1
nPerChain     <- ceiling((numSavedSteps * thinSteps)/nChains)


# Create, initialize, and adpat the model
cat( "Create, initialize, and adpat the model...\n" )

 out.OpenBugs <- bugs(data=dataList , inits=initsList, parameters, "model.txt", n.chains=nChains , n.iter=nPerChain, n.burnin=burnInSteps, debug = FALSE,program="OpenBUGS")
#  out.winBugs  <- bugs(data=dataList , inits=initsList, parameters, "model.txt", n.chains=nChains , n.iter=nPerChain, n.burnin=burnInSteps, debug = FALSE,program="WinBUGS", bugs.directory=Path.WinBUGS)

OpenBugs.sample  <- out.OpenBugs$sims.array
OpenBugs.list    <- out.OpenBugs$sims.list
OpenBugs.summary <- out.OpenBugs$summary
OpenBugs.pD      <- out.OpenBugs$pD
OpenBugs.DIC     <- out.OpenBugs$DIC

# WinBugs.sample  <- out.WinBugs$sims.array
# WinBugs.list    <- out.WinBugs$sims.list
# WinBugs.summary <- out.WinBugs$summary
# WinBugs.pD      <- out.WinBugs$pD
# WinBugs.DIC     <- out.WinBugs$DIC


cat(paste("model1\n"),file=FL.RESL.OUT,append=TRUE)
write.table(as.data.frame(OpenBugs.summary),file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("model1,pD,",OpenBugs.pD,"\tDIC,",OpenBugs.DIC,"\n"),file=FL.RESL.OUT,append=TRUE)


#
# plot the sample trace
#
no.par <- dim(OpenBugs.sample)[3]
for (idx in seq(1:no.par))
{
	
	FL.pdf <- paste(Path.Out,paste("July11_2013_OpenBugs_",idx,".pdf",sep=""),sep="/")
	pdf(file = FL.pdf,paper="a4r", width=0, height=0)
	
	array.this <- OpenBugs.sample[,,idx]
	# list.this  <- OpenBugs.list[[idx]]
	
	# diagnostics plot, marginal posterior distributions of Ntot and p, traceplots
	par(mfrow=c(1,1))
	# hist(list.this,nclass=50,freq=F,main=paste(paste("pD=",OpenBugs.pD," | DIC =",OpenBugs.DIC,sep=""),round(median(as.numeric(list.this)),0)),cex.main=.8,xlab="Ntot",cex.axis=.9)
	# lines(density(list.this,adjust=1),lty=1,lwd=2,col="red")
	matplot(array.this,type="l",main=idx,bty="l",cex.main=.8,ylab=idx) # population.mean trace
	
	
	dev.off()
}



dev.off()

# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\nTest_R2WinBUGS04.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\nTest_R2WinBUGS04.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [Test_R2WinBUGS04.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [Test_R2WinBUGS04.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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

