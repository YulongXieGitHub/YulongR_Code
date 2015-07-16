# The package version used for this paper : 
# R 2.7.0
# MASS 7.2-42
# pscl: 0.95
# sandwich 2.1-0
# car 1.2-8
# lmtest 0.9-21
# March 7, 2013: note: sandwich does not exist for 64 bit machine, so this script hasd to run on a X86 machine.

#
# Page246_Table7_5_Brugs.R
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
# Data Folder and files
# -------------------------------------------------------------------------------------------------
Path.Data.IN <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_classical_GLM"
Path.log     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"			
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/Page246_Table7_5_Brugs"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}


FL.Data.IN  <- paste(Path.Data.IN,"dt.Rdata",sep="/")
FL.Data.OUT <- paste(Path.Out,"Page246_Table7_5_Brugs_Data.csv",sep="/")
FL.RESL.OUT <- paste(Path.Out,"Page246_Table7_5_Brugs_results.csv",sep="/")
FL.MODEL    <- paste(Path.Out,"Page246_Table7_5_Brugs_model.Rdata",sep="/")

FL.PostDist.CSV <- paste(Path.Out,"Page246_Table7_5_Brugs_PostDist.csv",sep="/")
FL.PostDist.OBJ <- paste(Path.Out,"Page246_Table7_5_Brugs_PostDist.Rdata",sep="/")


FL.LOG      <- paste(Path.log,"Page246_Table7_5_Brugs.log",sep="/")	
FL.PDF      <- paste(Path.Out,"Page246_Table7_5_Brugs.pdf",sep="/")	
FL.SUM.cat  <- paste(Path.Out,"Page246_Table7_5_Brugs_cat.sum",sep="/")
FL.SUM.num  <- paste(Path.Out,"Page246_Table7_5_Brugs_num.sum",sep="/")

Path.Current <- getwd()
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


library(BRugs)         # Kruschke, J. K. (2010). Doing Bayesian data analysis:
                       # A Tutorial with R and BUGS. Academic Press / Elsevier.
#------------------------------------------------------------------------------
# THE MODEL.

modelstring = "
	model{
	    # Poisson model likelihood
	    for (i in 1:n){ 
		damage[i] ~ dpois( lambda[i] )
		log(lambda[i]) <- beta[1] + beta[2] * type[i] + beta[3] * bombload[i] + beta[4] * airexp[i]
	    }
			#
			# prior
			#
			for (j in 1:4){
				beta[j]~dnorm( 0.0, 0.001 )
				B[j] <- exp( beta[j] )
			}
			
			#
			# profiles
			#
			
			# values for bombload
			profiles[1,1] <- ranked( bombload[], 1 ) # minimum of bombload
			profiles[2,1] <- mean(bombload[])        # mean of bombload  
			profiles[3,1] <- 0.5*( ranked( bombload[], 15 )+ranked( bombload[], 16 )) #median
			profiles[4,1] <- ranked( bombload[], 30 )  #max
	   
	   		# values for airexp
			profiles[1,2] <- ranked( airexp[], 30 ) #max experience
			profiles[2,2] <- mean(airexp[])         #mean  
			profiles[3,2] <- 0.5*( ranked( airexp[], 15 )+ranked( airexp[], 16 )) #median
			profiles[4,2] <- ranked( airexp[], 1 )  #min experience

			for (k in 1:4){
					a4.profile[k] <- exp( beta[1] + beta[3]*profiles[k,1] + beta[4]*profiles[k,2] )
					a6.profile[k] <- a4.profile[k]*B[2] 
					# this is equivalent to setting exp( beta[1] + beta[3]*profile[k,1] + beta[4]*profile[k,2] )
			}


	}


" # close quote for modelstring
writeLines(modelstring,con="model.txt")
modelCheck( "model.txt" )
cat(paste("Model has been set up!\n"))
cat(paste("Model has been set up!\n"),file=FL.LOG,append=TRUE)




#------------------------------------------------------------------------------
# THE DATA.
damage   <- c(0, 1, 0, 0, 0, 0, 1, 0, 0, 2, 1, 1, 1, 1, 2, 3, 1, 1, 1, 2, 0, 1, 1, 2, 5, 1, 1, 5, 5, 7)
type     <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
bombload <- c(4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 7, 7, 7, 10, 10, 10, 12, 12, 12, 8, 8, 8, 14, 14, 14)
airexp   <- c(91.5, 84, 76.5, 69, 61.5, 80, 72.5, 65, 57.5, 50, 103, 95.5, 88, 80.5, 73, 116.1, 100.6, 85, 69.4, 53.9, 112.3, 96.7, 81.1, 65.6, 50, 120, 104.4, 88.9, 73.7, 57.8)

# Get the data into BUGS:
datalist = list(
		   damage          = damage,
		   type            = type,
		   bombload        = bombload,
		   airexp          = airexp,

		   n = length(damage)
)
modelData( bugsData( datalist ) )
cat(paste("Data has been prepared and loaded!\n"))
cat(paste("Data has been prepared and loaded!\n"),file=FL.LOG,append=TRUE)

predictedName = "damage"
predictorNames = c( "intercept","type","bombload","airexp" )
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
fm_pois <- glm(damage ~ type+bombload+airexp, family = poisson)
df.coef  <- data.frame(summary(fm_pois)$coefficients)[,1]


genInitList1 <- function(nPred=nPredictors) {
	list(
		beta = as.vector(df.coef)
	)
}
genInitList2 <- function(nPred=nPredictors) {
	list(
		beta = c(0,0,0,0)
	)
}
genInitList3 <- function(nPred=nPredictors) {
	list(
		beta  = c(-0.21,-0.13, 0.08,-0.02)
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
samplesSet( c( "beta","B","profiles[,1]","profiles[,2]","a4.profile","a6.profile") )
nPerChain = 50000
thinStep = 40
modelUpdate( nPerChain , thin=thinStep )
cat(paste("MCMC has been updated!\n"))
cat(paste("MCMC has been updated!\n"),file=FL.LOG,append=TRUE)


#------------------------------------------------------------------------------
# Post-processing the MCMC results
#------------------------------------------------------------------------------
fname <- "Page246_Table7_5_Brugs"
source("plotChains_revised.R")		# loading the two function in John K Kruschke's book.  This is to plot the trace of the chain
source("plotPost.R")			# loading the two function in John K Kruschke's book.  This is to plot the posterior distribution of the parameters

#
# pltting the traces of the chains
#
checkConvergence = TRUE
if ( checkConvergence ) {
	betaSum  = plotChains( "beta"  , saveplots=T , filenameroot=fname,path.pdf=Path.Out )
	dev.copy2pdf(file=paste(Path.Out,paste(fname,"_beta.pdf",sep=""),sep="/"))
	
	BSum  = plotChains( "B"  , saveplots=T , filenameroot=fname,path.pdf=Path.Out )
	dev.copy2pdf(file=paste(Path.Out,paste(fname,"_B.pdf",sep=""),sep="/"))	
	
	A4ProfileSum  = plotChains( "a4.profile"  , saveplots=T , filenameroot=fname,path.pdf=Path.Out )
	dev.copy2pdf(file=paste(Path.Out,paste(fname,"_A4Profile.pdf",sep=""),sep="/"))

	A6ProfileSum  = plotChains( "a6.Profile"  , saveplots=T , filenameroot=fname,path.pdf=Path.Out )
	dev.copy2pdf(file=paste(Path.Out,paste(fname,"_A6Profile.pdf",sep=""),sep="/"))
	
}
cat(paste("MCMC chain traces have been plotted!\n"))
cat(paste("MCMC chain traces have been plotted!\n"),file=FL.LOG,append=TRUE)

#
# Extract the posterior sample from BUGS:
#
betaSample  <- matrix( 0 , nrow=nChains*nPerChain,ncol=nPredictors) 	# how the thin affect this if thiin is not 1
BSample  <- matrix( 0 , nrow=nChains*nPerChain,ncol=nPredictors) 
profiles1Sample = matrix( 0 , ncol=nPredictors , nrow=nChains*nPerChain )
profiles2Sample = matrix( 0 , ncol=nPredictors , nrow=nChains*nPerChain )
a4profileSample = matrix( 0 , ncol=nPredictors , nrow=nChains*nPerChain )
a6profileSample = matrix( 0 , ncol=nPredictors , nrow=nChains*nPerChain )
for (idx in 1:nPredictors)
{
	    nodeName = paste( "beta[" , idx , "]" , sep="" )
	    betaSample[,idx] = samplesSample( nodeName )

	    nodeName = paste( "B[" , idx , "]" , sep="" )
	    BSample[,idx] = samplesSample( nodeName )

	    # nodeName = paste( "profiles[" , idx , ",1]" , sep="" )
	    # profiles1Sample[,idx] = samplesSample( nodeName )

	    # nodeName = paste( "profiles[" , idx , ",2]" , sep="" )
	    # profiles2Sample[,idx] = samplesSample( nodeName )
	    
	    nodeName = paste( "a4.profile[" , idx , "]" , sep="" )
	    a4profileSample[,idx] = samplesSample( nodeName )	    
	    
	    nodeName = paste( "a6.profile[" , idx , "]" , sep="" )
	    a6profileSample[,idx] = samplesSample( nodeName )
}
save( betaSample,BSample, profiles1Sample, profiles2Sample, a4profileSample,a6profileSample,file=FL.MODEL)
cat(paste("MCMC parameters of posterior distrbution have been extracted and saved!\n"))
cat(paste("MCMC parameters of posterior distrbution have been extracted and saved!\n"),file=FL.LOG,append=TRUE)

#
# Scatter plots of parameter values, pairwise:
#
if ( nPredictors <= nPredictors ) { # don't display if too many predictors
    windows()
    thinIdx = seq(1,dim(betaSample)[1])
    

    pairs(cbind(betaSample[thinIdx,]),labels=c(predictorNames),main=paste("beta",sep=""))
    dev.copy2pdf(file=paste(Path.Out,paste(fname,"_beta.pdf",sep=""),sep="/"))
    
    pairs(cbind(BSample[thinIdx,]),labels=c(predictorNames),main=paste("B",sep=""))
    dev.copy2pdf(file=paste(Path.Out,paste(fname,"_B.pdf",sep=""),sep="/"))
    
    pairs(cbind(a4profileSample[thinIdx,],a6profileSample[thinIdx,]),labels=c("a4.orofile","a6.profile"),main=paste("profiles",sep=""))
    dev.copy2pdf(file=paste(Path.Out,paste(fname,"_profiles.pdf",sep=""),sep="/"))    
}
cat(paste("MCMC parameters of posterior distrbution have been plotted!\n"))
cat(paste("MCMC parameters of posterior distrbution have been plotted!\n"),file=FL.LOG,append=TRUE)

#
# Show correlation matrix:
#
cat("\nCorrlations of posterior beta, B, and a4.profile, a6.profile:\n")
     beta.corcoef  <- data.frame(cor(cbind(betaSample)))
names(beta.corcoef) <- c(predictorNames)
      B.corcoef  <- data.frame(cor(cbind(BSample)))
names(B.corcoef) <- c(predictorNames)
cat(paste("MCMC parameters coorelation have been plotted!\n"))
cat(paste("MCMC parameters coorelation have been plotted!\n"),file=FL.LOG,append=TRUE)




#
# Display the posterior distributions
#
par( mar=c(4,3,2.5,0) , mgp=c(2,0.7,0) )
layout(matrix(1:10,nrow=2,ncol=5))

# parameters: beta
plotPost(a4profileSample,main=paste("a4profileSample:",sep=""),cex.main=1.67 , cex.lab=1.33 )
for ( sIdx in 1:nPredictors ) {
	plotPost(betaSample[,sIdx],main=paste("beta:",predictorNames[sIdx],sep=""),cex.main=1.67 , cex.lab=1.33 )
}
dev.copy2pdf(file=paste(Path.Out,paste(fname,"_PostHist_ZeroInflation.pdf",sep=""),sep="/"))

# parameter for Poisson distribution
plotPost(a6profileSample,main=paste("a6profileSample:",sep=""),cex.main=1.67 , cex.lab=1.33 )
for ( sIdx in 1:nPredictors ) {
	plotPost(BSample[,sIdx],main=paste("B:",predictorNames[sIdx],sep=""),cex.main=1.67 , cex.lab=1.33 )
}
dev.copy2pdf(file=paste(Path.Out,paste(fname,"_PostHist_Poisson.pdf",sep=""),sep="/"))
cat(paste("MCMC parameters posterior distribution have been plotted!\n"))
cat(paste("MCMC parameters posterior distribution have been plotted!\n"),file=FL.LOG,append=TRUE)

#
# fitting calculation for each observations in the data set
# [PostPred.Poisson], [PostPred.Lambda], [PostPred.p0Poisson], [PostPred.ZeroInfl], [PostPred.p0Infl] are [no.data] by [no.postSample] matrix
# [yHDIlim.Poisson],  [yHDIlim.Lambda],  [yHDIlim.p0Poisson],  [yHDIlim.ZeroInfl],  [yHDIlim.p0Infl]  are [no.data] by [2] matrix
#
 beta[1] + beta[2] * type[i] + beta[3] * bombload[i] + beta[4] * airexp[i]
 
 
no.thin <- 1
thinIdx <- seq(1,dim(betaSample)[1],by=no.thin)
no.data       <- length(type)
no.postSample <- dim(betaSample)[1] / no.thin
xPostPred = cbind(
		   rep(1,length(type)),
		   as.vector(type),
		   as.vector(bombload),
		   as.vector(airexp)
		   )

PostPred.Poisson   <- matrix( 0 , nrow=no.data, ncol=no.postSample)	# linear predictor of Poisson process
PostPred.Lambda    <- matrix( 0 , nrow=no.data, ncol=no.postSample)	# expected count of Poisson process
PostPred.p0Poisson <- matrix( 0 , nrow=no.data, ncol=no.postSample)	# probability 0 count of Poisson process


yHDIlim.Poisson    <- matrix( 0 , nrow=no.data, ncol=2 )		# HDI limits of posterior: linear predictor of Poisson process
yHDIlim.Lambda     <- matrix( 0 , nrow=no.data, ncol=2 )		# HDI limits of posterior: expected count of Poisson process
yHDIlim.p0Poisson  <- matrix( 0 , nrow=no.data, ncol=2 )		# HDI limits of posterior: probability 0 count of Poisson process

				  
# Posterior Calculation:
for (idx.samp in 1:no.postSample) 
{
	# the linear predctors of Poisson and ZeroInflation
    	PostPred.Poisson[,idx.samp]  <- xPostPred %*% betaSample[idx.samp,]
}
    	
# The Poisson Lambda and p0 of ZeroInflation
PostPred.Lambda <- exp(PostPred.Poisson)		# link function is: log(lambda) = eta and mean function is: lambda = exp(eta)
    	
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
}
cat(paste("as well as their HDI!\n"))
cat(paste("as well as their HDI!\n"),file=FL.LOG,append=TRUE)

#
# posterior mean of each quantities for each observation
#
PostMean.Poisson   <- apply(PostPred.Poisson,1,  mean,na.rm=TRUE)
PostMean.Lambda    <- apply(PostPred.Lambda,1,   mean,na.rm=TRUE)
PostMean.p0Poisson <- apply(PostPred.p0Poisson,1,mean,na.rm=TRUE)

PostSD.Poisson   <- apply(PostPred.Poisson,1,  sd,na.rm=TRUE)
PostSD.Lambda    <- apply(PostPred.Lambda,1,   sd,na.rm=TRUE)
PostSD.p0Poisson <- apply(PostPred.p0Poisson,1,sd,na.rm=TRUE)


#
# summary of posterior distribution
#
table.summary <- cbind(observed.count   = damage,

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
                       p0Poisson.PostSD   = PostSD.p0Poisson
                       
)

par(mfrow = c(2,2))
	hist(table.summary[,"observed.count"], nclass=100,xlab="observed.count", ylab="frequency",main="distribution of observed count")
	hist(table.summary[,"Lambda.PostMean"],nclass=100,xlab="PostMean.Lambda",ylab="frequency",main="distribution of expected count")
	plot(table.summary[,"observed.count"],table.summary[,"Lambda.PostMean"],,type="p",pch=16,cex=0.5,xlab="observed count",ylab="expected",main="model fitting")                    
 	abline(a=0,b=1,col="red")                       

dev.copy2pdf(file=paste(Path.Out,paste(fname,"_summary_posterior.pdf",sep=""),sep="/"))

#
# store the posterior distribution results
#
save(table.summary,PostPred.Poisson,PostPred.Lambda,PostPred.p0Poisson,yHDIlim.Poisson,yHDIlim.Lambda,yHDIlim.p0Poisson,file = FL.PostDist.OBJ)
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


 if ( NO <= 5 ) { # Only make this figure if there are not too many coins
  windows(3.2*3,2.5*(1+NO))
 layout( matrix( 1:(3*(NO+1)) , nrow=(NO+1) , byrow=T ) )
 par(mar=c(2.95,2.95,1.0,0),mgp=c(1.35,0.35,0),oma=c( 0.1, 0.1, 0.1, 0.1))
 nPtsToPlot = 500
 plotIdx = floor(seq(1,length(betaSample[,1]),length=nPtsToPlot))
 kPltLim = signif( quantile( betaSample[,2] , p=c(.01,.99) ) , 4 )
 plot( betaSample[plotIdx,1] , betaSample[plotIdx,2] , type="p" , ylim=kPltLim ,
       xlim=c(0,1) , xlab=expression(beta{1}) , ylab=expression(beta{2}) , cex.lab=1.5 )
plotPost( betaSample[,1] , xlab="beta1" , xlim=c(0,1) , main="" , breaks=20 )
plotPost( betaSample[,2] , xlab="beta2" , main="" , breaks=20 , HDItextPlace=.6 )
for ( coinIdx in 1:NO ) {
    plotPost( betaSample[coinIdx,] , xlab=paste("beta",coinIdx,sep="") ,
               xlim=c(0,1) , main="" , breaks=20 , HDItextPlace=.3 )
     plot( betaSample[coinIdx,plotIdx] , betaSample[plotIdx] , type="p" ,
           xlim=c(0,1) , ylim=c(0,1) , cex.lab=1.5 ,
           xlab=bquote(theta[.(coinIdx)]) , ylab=expression(mu) )
     plot( thetaSample[coinIdx,plotIdx] , kappaSample[plotIdx] , type="p" ,
           xlim=c(0,1) , ylim=kPltLim , cex.lab=1.5 ,
           xlab=bquote(theta[.(coinIdx)]) , ylab=expression(kappa) )
}
dev.copy2eps(file=paste("page246_Table7_5",paste(z,collapse=""),".eps",sep=""))
# 
# 
#------------------------------------------------------------------------------
# EXAMINE THE RESULTS
# 

# 
# 
# 
# #
# # --------------------------------------------------------------
#
#
# 
# # Extract chain values:
# zb0Samp = matrix( samplesSample( "b0" ) )
# zbSamp = NULL
# for ( j in 1:nPredictors ) {
#    zbSamp = cbind( zbSamp , samplesSample( paste("b[",j,"]",sep="") ) )
# }
# zTauSamp = matrix( samplesSample( "tau" ) )
# zSigmaSamp = 1 / sqrt( zTauSamp ) # Convert precision to SD
# chainLength = length(zTauSamp)
# 
# # Convert to original scale:
# bSamp = zbSamp * matrix( sd(y)/apply(x,2,sd) , byrow=TRUE ,
#                      ncol=nPredictors , nrow=NROW(zbSamp) )
# b0Samp = ( zb0Samp * sd(y)
#           + mean(y)
#           - rowSums( zbSamp
#           * matrix( sd(y)/apply(x,2,sd) , byrow=TRUE ,
#                     ncol=nPredictors , nrow=NROW(zbSamp) )
#           * matrix( apply(x,2,mean) , byrow=TRUE ,
#                     ncol=nPredictors , nrow=NROW(zbSamp) ) ) )
# sigmaSamp = zSigmaSamp * sd(y)
# 
# # Save MCMC sample:
# save( b0Samp , bSamp , sigmaSamp , 
#       file="Page246_Table_7_5Guber1999.Rdata" )
# 
# # Scatter plots of parameter values, pairwise:
# if ( nPredictors <= 6 ) { # don't display if too many predictors
#     windows()
#     thinIdx = round(seq(1,length(zb0Samp),length=200))
#     pairs( cbind( zSigmaSamp[thinIdx] , zb0Samp[thinIdx] , zbSamp[thinIdx,] )  ,
#       labels=c("Sigma zy","zIntercept",paste("zSlope",predictorNames,sep="")))
#     windows()
#     thinIdx = seq(1,length(b0Samp),length=700)
#     pairs( cbind( sigmaSamp[thinIdx] , b0Samp[thinIdx] , bSamp[thinIdx,] ) ,
#       labels=c( "Sigma y" , "Intercept", paste("Slope",predictorNames,sep="")))
#     dev.copy2eps(file=paste(fname,"PostPairs.eps",sep=""))
# }
# # Show correlation matrix on console:
# cat("\nCorrlations of posterior sigma, b0, and bs:\n")
# show( cor( cbind( sigmaSamp , b0Samp , bSamp ) ) )
# 
# # Display the posterior:
# nPlotPerRow = 5
# nPlotRow = ceiling((2+nPredictors)/nPlotPerRow)
# nPlotCol = ceiling((2+nPredictors)/nPlotRow)
# windows(3.5*nPlotCol,2.25*nPlotRow)
# layout( matrix(1:(nPlotRow*nPlotCol),nrow=nPlotRow,ncol=nPlotCol,byrow=T) )
# par( mar=c(4,3,2.5,0) , mgp=c(2,0.7,0) )
# histInfo = plotPost( sigmaSamp , xlab="Sigma Value" , compVal=NULL ,
#                      breaks=30 , main=bquote(sigma[y]) ,
#                      cex.main=1.67 , cex.lab=1.33 )
# histInfo = plotPost( b0Samp , xlab="Intercept Value" , compVal=NULL ,
#                      breaks=30 , main=bquote(.(predictedName) *" at "* x==0) ,
#                      cex.main=1.67 , cex.lab=1.33 )
# for ( sIdx in 1:nPredictors ) {
# histInfo = plotPost( bSamp[,sIdx] , xlab="Slope Value" , compVal=0.0 ,
#                      breaks=30 ,
#                      main=bquote( Delta * .(predictedName) /
#                                   Delta * .(predictorNames[sIdx]) ) ,
#                      cex.main=1.67 , cex.lab=1.33 )
# }
# dev.copy2eps(file=paste(fname,"PostHist.eps",sep=""))
# 
# # Posterior prediction:
# # Specify x values for which predicted y's are needed.
# # xPostPred is a matrix such that ncol=nPredictors and nrow=nPostPredPts.
# xPostPred = rbind(
#     apply(x,2,mean)-3*apply(x,2,sd) , # mean of data x minus thrice SD of data x
#     apply(x,2,mean)                 , # mean of data x
#     apply(x,2,mean)+3*apply(x,2,sd)   # mean of data x plus thrice SD of data x
# )
# # Define matrix for recording posterior predicted y values for each xPostPred.
# # One row per xPostPred value, with each row holding random predicted y values.
# postSampSize = chainLength
# yPostPred = matrix( 0 , nrow=NROW(xPostPred) , ncol=postSampSize )
# # Define matrix for recording HDI limits of posterior predicted y values:
# yHDIlim = matrix( 0 , nrow=NROW(xPostPred) , ncol=2 )
# # Generate posterior predicted y values.
# This gets only one y value, at each x, for each step in the chain.
# for ( chainIdx in 1:chainLength ) {
#     yPostPred[,chainIdx] = rnorm( NROW(xPostPred) ,
#                            mean = b0Samp[chainIdx]
#                                   + xPostPred %*% cbind(bSamp[chainIdx,]) ,
#                            sd = rep( sigmaSamp[chainIdx] , NROW(xPostPred) ) )
# }
# source("HDIofMCMC.R")
# for ( xIdx in 1:NROW(xPostPred) ) {
#     yHDIlim[xIdx,] = HDIofMCMC( yPostPred[xIdx,] )
# }
# cat( "\nPosterior predicted y for selected x:\n" )
# show( cbind( xPostPred , yPostPredMean=rowMeans(yPostPred) , yHDIlim ) )
# 
# #------------------------------------------------------------------------------
# 
# 