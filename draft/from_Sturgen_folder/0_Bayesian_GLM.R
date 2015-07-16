#
# 0_Bayesian_GLM.R
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
# 0_Bayesian_GLM.R
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

# -------------------------------------------------------------------------------------------------
# Data Folder and files
# -------------------------------------------------------------------------------------------------
Path.Data.IN <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_classical_GLM"
Path.log     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_Bayesian_GLM"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}


FL.Data.IN  <- paste(Path.Data.IN,"dt.Rdata",sep="/")
FL.Data.OUT <- paste(Path.Out,"0_Bayesian_GLM_data.csv",sep="/")
FL.RESL.OUT <- paste(Path.Out,"0_Bayesian_GLM_results.csv",sep="/")
FL.MODEL    <- paste(Path.Out,"0_Bayesian_GLM_model.Rdata",sep="/")

FL.PostDist.CSV <- paste(Path.Out,"0_Bayesian_GLM_PostDist.csv",sep="/")
FL.PostDist.OBJ <- paste(Path.Out,"0_Bayesian_GLM_PostDist.Rdata",sep="/")


FL.LOG      <- paste(Path.log,"0_Bayesian_GLM.log",sep="/")	
FL.PDF      <- paste(Path.Out,"0_Bayesian_GLM.pdf",sep="/")	
FL.SUM.cat  <- paste(Path.Out,"0_Bayesian_GLM_cat.sum",sep="/")
FL.SUM.num  <- paste(Path.Out,"0_Bayesian_GLM_num.sum",sep="/")

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
# load data (pscl example data processed by "0_classical_GLM.R")
# -------------------------------------------------------------------------------------------------
load(FL.Data.IN)

# -------------------------------------------------------------------------------------------------
# output the other format used by WinBUGS or BUGS
# -------------------------------------------------------------------------------------------------
cat("DATA(LIST)\n",FL.Data.OUT,append=TRUE)
cat("list(n=",dim(dt2)[1],",\n",file=FL.Data.OUT,append=TRUE)
idx <- 0
for (col.name in names(dt2))
{
	idx <- idx + 1
	if (idx == dim(dt2)[2])
	{
		A <- paste(col.name,paste(" = c(",paste(dt2[,col.name],collapse=","),")",sep=""),")\n",sep="")
	}else{
		A <- paste(col.name,paste(" = c(",paste(dt2[,col.name],collapse=","),")",sep=""),",\n",sep="")
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
			ofp[i] ~ dpois(mu[i])		
			mu[i] <- lambda[i,T[i]]	

			# Log-linear model [Poisson means] 
			lambda[i,1] <- 0						
			log(lambda[i,2]) <- b0                      + 			
					    b[1]*hosp[i]            +			
					    b[2]*numchron[i]        + 			
					    b[3]*gendermale[i]      + 			
					    b[4]*school[i]          + 			
					    b[5]*privinsyes[i]     			


			# Logistic regression for the [zero-inflation probability]
			logit(P[i,1]) <-    a0                      + 			
					    a[1]*hosp[i]            +			
					    a[2]*numchron[i]        + 			
					    a[3]*gendermale[i]      + 			
					    a[4]*school[i]          + 			
					    a[5]*privinsyes[i]    	

			P[i,2] <- 1-P[i,1]				
			T[i] ~ dcat(P[i,1:2])		
		}		

		# normal distributions are assumed for all the parameters beta and gamma
		# Flat Priors on parameters
		a0 ~ dnorm(0,1.0E-6)
		b0 ~ dnorm(0,1.0E-6)

		for (j in 1:5){a[j]  ~ dnorm(0,1.0E-6)}
		for (j in 1:5){b[j]  ~ dnorm(0,1.0E-6)}

	}

" # close quote for modelstring
writeLines(modelstring,con="model.txt")
modelCheck( "model.txt" )
cat(paste("Model has been set up!\n"))
cat(paste("Model has been set up!\n"),file=FL.LOG,append=TRUE)



#------------------------------------------------------------------------------
# THE DATA.
#------------------------------------------------------------------------------
# Get the data into BUGS:
datalist = list(
		   ofp             = as.vector(dt2[,"ofp"]),
		   hosp            = as.vector(dt2[,"hosp"]),
		   numchron        = as.vector(dt2[,"numchron"]),
		   gendermale      = as.vector(dt2[,"gendermale"]),
		   school          = as.vector(dt2[,"school"]),
		   privinsyes      = as.vector(dt2[,"privinsyes"]),		   

		   n = dim(dt2)[1] 
)
modelData( bugsData( datalist ) )
cat(paste("Data has been prepared and loaded!\n"))
cat(paste("Data has been prepared and loaded!\n"),file=FL.LOG,append=TRUE)




predictedName = "ofp"
predictorNames = c( "hops","numchron","gendermale","school","privinsyes" )
nPredictors <- length(predictorNames)

#------------------------------------------------------------------------------
# Compile the model
#------------------------------------------------------------------------------
nChains = 3
modelCompile( numChains = nChains )
cat(paste("Model has been compiled!\n"))


#------------------------------------------------------------------------------
# INTIALIZE THE CHAINS.
#------------------------------------------------------------------------------
genInitList1 <- function(nPred=nPredictors) {
	list(
		a0 = -0.06, 
		b0 =  1.19,
		a  = c(-0.82,-1.24, 0.65,-0.08,-1.15),
		b  = c( 0.21, 0.13,-0.08, 0.02, 0.13)
	)
}
genInitList2 <- function(nPred=nPredictors) {
	list(
		a0 =  0.06, 
		b0 =  0.89,
		a  = c(-0.52,-1.00, 0.45, 0.08,-0.15),
		b  = c( 0.11,-0.13, 0.08, 1.02, 1.13)
	)
}
genInitList3 <- function(nPred=nPredictors) {
	list(
		a0 = -0.00, 
		b0 =  0.5,
		a  = c( 0.82, 1.24,-0.65, 0.08, 1.15),
		b  = c(-0.21,-0.13, 0.08,-0.02,-0.13)
	)
}



#
# this step often leads to unexpected exit of the R session: modelGenInits() # often won't work for diffuse prior
#
for ( chainIdx in 1 : nChains ) 
{
	if (chainIdx == 1)
	{
     		modelInits( bugsInits( genInitList1))	# What has not been initialized?
     		modelGenInits()				# intialize other un-initialized parameters
     	}else if(chainIdx == 2)
     	{
     		modelInits( bugsInits( genInitList2))	# What has not been initialized?
		modelGenInits()				# intialize other un-initialized parameters
	}else if(chainIdx == 3)
     	{
     		modelInits( bugsInits( genInitList3))	# What has not been initialized?
		modelGenInits()				# intialize other un-initialized parameters
	}     	
}
cat(paste("Data has been initialized!\n"))
cat(paste("Data has been initialized!\n"),file=FL.LOG,append=TRUE)



#------------------------------------------------------------------------------
# BURNING IN
#------------------------------------------------------------------------------
BurnInSteps = 10000
modelUpdate( BurnInSteps )
cat(paste("MCMC has been burning in!\n"))
cat(paste("MCMC has been burning in!\n"),file=FL.LOG,append=TRUE)


#------------------------------------------------------------------------------
# Set up parameter for monitoring and start the actual MCMC
#------------------------------------------------------------------------------
samplesSet( c("a0","b0","a","b") )
nPerChain = 50000
thinStep = 1
modelUpdate( nPerChain , thin=thinStep )
cat(paste("MCMC has been updated!\n"))
cat(paste("MCMC has been updated!\n"),file=FL.LOG,append=TRUE)


#------------------------------------------------------------------------------
# Post-processing the MCMC results
#------------------------------------------------------------------------------
fname <- "0_Bayesian_GLM"
source("plotChains_revised.R")		# loading the two function in John K Kruschke's book.  This is to plot the trace of the chain
source("plotPost.R")			# loading the two function in John K Kruschke's book.  This is to plot the posterior distribution of the parameters

#
# pltting the traces of the chains
#
checkConvergence = TRUE
if ( checkConvergence ) {
	a0Sum = plotChains( "a0" , saveplots=T , filenameroot=fname,path.pdf=Path.Out )
	dev.copy2pdf(file=paste(Path.Out,paste(fname,"_ZeroInflation_a0.pdf",sep=""),sep="/"))

	b0Sum = plotChains( "b0" , saveplots=T , filenameroot=fname,path.pdf=Path.Out )
	dev.copy2pdf(file=paste(Path.Out,paste(fname,"_Poisson_b0.pdf",sep=""),sep="/"))

	aSum  = plotChains( "a"  , saveplots=T , filenameroot=fname,path.pdf=Path.Out )
	dev.copy2pdf(file=paste(Path.Out,paste(fname,"_ZeroInflation_a.pdf",sep=""),sep="/"))
	
	bSum  = plotChains( "b"  , saveplots=T , filenameroot=fname,path.pdf=Path.Out )
	dev.copy2pdf(file=paste(Path.Out,paste(fname,"_Poisson_b.pdf",sep=""),sep="/"))	
}
cat(paste("MCMC chain traces have been plotted!\n"))
cat(paste("MCMC chain traces have been plotted!\n"),file=FL.LOG,append=TRUE)


###          mean      sd MC_error val2.5pc   median val97.5pc start sample
### a[1] -0.02858 0.13610 0.010670 -0.28090 -0.03395   0.25490  1001  10000
### a[2] -0.31640 0.09363 0.002450 -0.50200 -0.31470  -0.13720  1001  10000
### a[3] -0.54770 0.04376 0.001694 -0.63510 -0.54770  -0.46070  1001  10000
### a[4]  0.41050 0.08774 0.002717  0.24530  0.40550   0.59070  1001  10000
### a[5] -0.05762 0.01195 0.000938 -0.08115 -0.05756  -0.03479  1001  10000
### a[6] -0.74600 0.09932 0.005027 -0.94060 -0.74620  -0.55470  1001  10000
###          mean       sd  MC_error val2.5pc   median val97.5pc start sample
### b[1]  1.43800 0.022670 0.0017520  1.39300  1.43900   1.48000  1001  10000
### b[2]  0.17490 0.005854 0.0004439  0.16430  0.17490   0.18630  1001  10000
### b[3]  0.12910 0.004485 0.0003246  0.12020  0.12930   0.13710  1001  10000
### b[4] -0.06247 0.012230 0.0010870 -0.08454 -0.06388  -0.03793  1001  10000
### b[5]  0.01461 0.001808 0.0001307  0.01102  0.01461   0.01824  1001  10000
### b[6]  0.06029 0.017410 0.0014190  0.02962  0.05864   0.09721  1001  10000

#
# Extract the posterior sample from BUGS:
#
a0Sample   <- matrix( samplesSample( "a0" ))
b0Sample   <- matrix( samplesSample( "b0" ))
aSample  <- matrix( 0 , nrow=nChains*nPerChain,ncol=nPredictors) 	# how the thin affect this if thiin is not 1
bSample  <- matrix( 0 , nrow=nChains*nPerChain,ncol=nPredictors) 
for (idx in 1:nPredictors)
{
	nodeName.a <- paste("a[",idx,"]",sep="")
	nodeName.b <- paste("b[",idx,"]",sep="")
	
	aSample[,idx] = samplesSample(nodeName.a) # BRugs gets sample from BUGS
	bSample[,idx] = samplesSample(nodeName.b) # BRugs gets sample from BUGS
}
save( a0Sample, b0Sample, aSample,bSample, file=FL.MODEL)
cat(paste("MCMC parameters of posterior distrbution have been extracted and saved!\n"))
cat(paste("MCMC parameters of posterior distrbution have been extracted and saved!\n"),file=FL.LOG,append=TRUE)


#
# Scatter plots of parameter values, pairwise:
#
if ( nPredictors <= nPredictors ) { # don't display if too many predictors
    windows()
    thinIdx = seq(1,dim(aSample)[1])
    

    pairs(cbind(a0Sample[thinIdx,],aSample[thinIdx,]),labels=c("intercept",predictorNames),main=paste("Zero Inflation Parameters",sep=""))
    dev.copy2pdf(file=paste(Path.Out,paste(fname,"_Pairs_ZeroInflation.pdf",sep=""),sep="/"))
    
    pairs(cbind(b0Sample[thinIdx,],bSample[thinIdx,]),labels=c("intercept",predictorNames),main=paste("Poisson Parameters",sep=""))      
    dev.copy2pdf(file=paste(Path.Out,paste(fname,"_Pairs_Poisson.pdf",sep=""),sep="/"))
}
cat(paste("MCMC parameters of posterior distrbution have been plotted!\n"))
cat(paste("MCMC parameters of posterior distrbution have been plotted!\n"),file=FL.LOG,append=TRUE)

#
# Show correlation matrix:
#
cat("\nCorrlations of posterior sigma, b0, and bs:\n")
      a.corcoef  <- data.frame(cor(cbind(a0Sample,aSample)))
names(a.corcoef) <- c("intercept",predictorNames)
      b.corcoef  <- data.frame(cor(cbind(b0Sample,bSample)))
names(b.corcoef) <- c("intercept",predictorNames)
cat(paste("MCMC parameters coorelation have been plotted!\n"))
cat(paste("MCMC parameters coorelation have been plotted!\n"),file=FL.LOG,append=TRUE)



#
# Display the posterior distributions
#
par( mar=c(4,3,2.5,0) , mgp=c(2,0.7,0) )
layout(matrix(1:6,nrow=2,ncol=3))

# parameters for zero inflation
plotPost(a0Sample,main=paste("a0:",sep=""),cex.main=1.67 , cex.lab=1.33 )
for ( sIdx in 1:nPredictors ) {
	plotPost(aSample[,sIdx],main=paste("Zero Inflation a:",predictorNames[sIdx],sep=""),cex.main=1.67 , cex.lab=1.33 )
}
dev.copy2pdf(file=paste(Path.Out,paste(fname,"_PostHist_ZeroInflation.pdf",sep=""),sep="/"))

# parameter for Poisson distribution
plotPost(b0Sample,main=paste("b0:",sep=""),cex.main=1.67 , cex.lab=1.33 )
for ( sIdx in 1:nPredictors ) {
	plotPost(bSample[,sIdx],main=paste("Poisson Dist. b:",predictorNames[sIdx],sep=""),cex.main=1.67 , cex.lab=1.33 )
}
dev.copy2pdf(file=paste(Path.Out,paste(fname,"_PostHist_Poisson.pdf",sep=""),sep="/"))
cat(paste("MCMC parameters posterior distribution have been plotted!\n"))
cat(paste("MCMC parameters posterior distribution have been plotted!\n"),file=FL.LOG,append=TRUE)

#
# fitting calculation for each observations in the data set
# [PostPred.Poisson], [PostPred.Lambda], [PostPred.p0Poisson], [PostPred.ZeroInfl], [PostPred.p0Infl] are [no.data] by [no.postSample] matrix
# [yHDIlim.Poisson],  [yHDIlim.Lambda],  [yHDIlim.p0Poisson],  [yHDIlim.ZeroInfl],  [yHDIlim.p0Infl]  are [no.data] by [2] matrix
#
no.thin <- 50
thinIdx <- seq(1,dim(aSample)[1],by=no.thin)
no.data       <- dim(dt2)[1]
no.postSample <- dim(a0Sample)[1] / no.thin
xPostPred = cbind(
		   as.vector(dt2[,"hosp"]),
		   as.vector(dt2[,"numchron"]),
		   as.vector(dt2[,"gendermale"]),
		   as.vector(dt2[,"school"]),
		   as.vector(dt2[,"privinsyes"])
		   )

PostPred.Poisson   <- matrix( 0 , nrow=no.data, ncol=no.postSample)	# linear predictor of Poisson process
PostPred.Lambda    <- matrix( 0 , nrow=no.data, ncol=no.postSample)	# expected count of Poisson process
PostPred.p0Poisson <- matrix( 0 , nrow=no.data, ncol=no.postSample)	# probability 0 count of Poisson process

PostPred.ZeroInfl  <- matrix( 0 , nrow=no.data, ncol=no.postSample)	# linear predictor of zero Inflation process
PostPred.p0Infl    <- matrix( 0 , nrow=no.data, ncol=no.postSample)	# probability of zero inflation

yHDIlim.Poisson    <- matrix( 0 , nrow=no.data, ncol=2 )		# HDI limits of posterior: linear predictor of Poisson process
yHDIlim.Lambda     <- matrix( 0 , nrow=no.data, ncol=2 )		# HDI limits of posterior: expected count of Poisson process
yHDIlim.p0Poisson  <- matrix( 0 , nrow=no.data, ncol=2 )		# HDI limits of posterior: probability 0 count of Poisson process

yHDIlim.ZeroInfl   <- matrix( 0 , nrow=no.data, ncol=2 )		# HDI limits of posterior: linear predictor of zero Inflation process
yHDIlim.p0Infl     <- matrix( 0 , nrow=no.data, ncol=2 )		# HDI limits of posterior: probability of zero inflation
				  
# Posterior Calculation:
for (idx.samp in 1:no.postSample) 
{
	# the linear predctors of Poisson and ZeroInflation
    	PostPred.Poisson[,idx.samp]  <- b0Sample[idx.samp,1] + xPostPred %*% bSample[idx.samp,]
    	PostPred.ZeroInfl[,idx.samp] <- a0Sample[idx.samp,1] + xPostPred %*% aSample[idx.samp,]
}
    	
# The Poisson Lambda and p0 of ZeroInflation
PostPred.Lambda <- exp(PostPred.Poisson)		# link function is: log(lambda) = eta and mean function is: lambda = exp(eta)
PostPred.p0Infl <- 1 / (1 + exp(-PostPred.ZeroInfl))	# link function is: logit(p) = eta, i.e., p/(1-p) = eta and mean function is: p = 1/1+exp(-eta)
    	
# the probability to have 0 count from Poisson distribution with a lambda like this
PostPred.p0Poisson <- dpois(0,PostPred.Lambda)
cat(paste("A variety of quantities have been calculated for each observation based on the posterior distribution!\n"))
cat(paste("A variety of quantities have been calculated for each observation based on the posterior distribution!\n"),file=FL.LOG,append=TRUE)

#
# calculate the HDI
#
source("HDIofMCMC.R")
for ( xIdx in 1:no.data ) 
{
    	yHDIlim.Poisson[xIdx,]   <- HDIofMCMC(PostPred.Poisson[xIdx,])
    	yHDIlim.Lambda[xIdx,]    <- HDIofMCMC(PostPred.Lambda[xIdx,])
    	yHDIlim.p0Poisson[xIdx,] <- HDIofMCMC(PostPred.p0Poisson[xIdx,])
    	
    	yHDIlim.ZeroInfl[xIdx,]  <- HDIofMCMC(PostPred.ZeroInfl[xIdx,])
    	yHDIlim.p0Infl[xIdx,]    <- HDIofMCMC(PostPred.p0Infl[xIdx,])    	
}
cat(paste("as well as their HDI!\n"))
cat(paste("as well as their HDI!\n"),file=FL.LOG,append=TRUE)

#
# posterior mean of each quantities for each observation
#
PostMean.Poisson   <- apply(PostPred.Poisson,1,  mean,na.rm=TRUE)
PostMean.Lambda    <- apply(PostPred.Lambda,1,   mean,na.rm=TRUE)
PostMean.p0Poisson <- apply(PostPred.p0Poisson,1,mean,na.rm=TRUE)
PostMean.ZeroInfl  <- apply(PostPred.ZeroInfl,1, mean,na.rm=TRUE)
PostMean.p0Infl    <- apply(PostPred.p0Infl,1,   mean,na.rm=TRUE)

PostSD.Poisson   <- apply(PostPred.Poisson,1,  sd,na.rm=TRUE)
PostSD.Lambda    <- apply(PostPred.Lambda,1,   sd,na.rm=TRUE)
PostSD.p0Poisson <- apply(PostPred.p0Poisson,1,sd,na.rm=TRUE)
PostSD.ZeroInfl  <- apply(PostPred.ZeroInfl,1, sd,na.rm=TRUE)
PostSD.p0Infl    <- apply(PostPred.p0Infl,1,   sd,na.rm=TRUE)


#
# summary of posterior distribution
#
table.summary <- cbind(observed.count   = dt2[,"ofp"],

                       Poisson.HDI025   = yHDIlim.Poisson[,1],  
                       Poisson.HDI975   = yHDIlim.Poisson[,2],  
                       Poisson.PostMean = PostMean.Poisson,  
                       Poisson.PostSD   = PostSD.Poisson,
                       
                       Lambda.HDI025   = yHDIlim.Lambda[,1],  
                       Lambda.HDI975   = yHDIlim.Lambda[,2],  
                       Lambda.PostMean = PostMean.Lambda,  
                       Lambda.PostSD   = PostSD.Lambda,
                       
                       p0Poisson.HDI025   = yHDIlim.p0Poisson[,1],  
                       p0Poisson.HDI975   = yHDIlim.p0Poisson[,2],  
                       p0Poisson.PostMean = PostMean.p0Poisson,  
                       p0Poisson.PostSD   = PostSD.p0Poisson,
                       
                       ZeroInfl.HDI025   = yHDIlim.ZeroInfl[,1],  
                       ZeroInfl.HDI975   = yHDIlim.ZeroInfl[,2],  
                       ZeroInfl.PostMean = PostMean.ZeroInfl,  
                       ZeroInfl.PostSD   = PostSD.ZeroInfl,
                       
                       p0Infl.HDI025   = yHDIlim.p0Infl[,1],  
                       p0Infl.HDI975   = yHDIlim.p0Infl[,2],  
                       p0Infl.PostMean = PostMean.p0Infl,  
                       p0Infl.PostSD   = PostSD.p0Infl)

par(mfrow = c(2,2))
	hist(table.summary[,"observed.count"], nclass=100,xlab="observed.count", ylab="frequency",main="distribution of observed count")
	hist(table.summary[,"Lambda.PostMean"],nclass=100,xlab="PostMean.Lambda",ylab="frequency",main="distribution of expected count")
	plot(table.summary[,"observed.count"],table.summary[,"Lambda.PostMean"],,type="p",pch=16,cex=0.5,xlab="observed count",ylab="expected",main="model fitting")                    
 	abline(a=0,b=1,col="red")                       

dev.copy2pdf(file=paste(Path.Out,paste(fname,"_summary_posterior.pdf",sep=""),sep="/"))

#
# store the posterior distribution results
#
save(table.summary,PostPred.Poisson,PostPred.Lambda,PostPred.p0Poisson,PostPred.ZeroInfl,PostPred.p0Infl,yHDIlim.Poisson,yHDIlim.Lambda,yHDIlim.p0Poisson,yHDIlim.ZeroInfl,yHDIlim.p0Infl,file = FL.PostDist.OBJ)
cat(paste("fitting the data,",sep=""),file=FL.PostDist.CSV,append=TRUE)
write.table(table.summary,sep=",",row.names=FALSE,col.names=TRUE,file=FL.PostDist.CSV,append=TRUE)
cat(paste("Processing of Posterior Distribution are finished!\n"))
cat(paste("Processing of Posterior Distribution are finished!\n"),file=FL.LOG,append=TRUE)


dev.off()

# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\n0_classical_GLM.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\n0_classical_GLM.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [0_classical_GLM.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [0_classical_GLM.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)

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

