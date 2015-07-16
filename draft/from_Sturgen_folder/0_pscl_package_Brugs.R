#
# 0_pscl_package_Brugs_Brugs.R
#
# March 8, 2013: use Brugs and pscl example data to specify a BUGS model for zero inflated poisson model
#
graphics.off()
rm(list=ls(all=TRUE))
fname = "0_pscl_package_Brugs_Brugs"

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
Path.Data.IN <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_pscl_data"
Path.log     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_pscl_package_Brugs"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}


FL.Data.IN  <- paste(Path.Data.IN,"DebTrivedi.rda",sep="/")
FL.Data.OUT <- paste(Path.Out,"dt.csv",sep="/")
FL.RESL.OUT <- paste(Path.Out,"0_pscl_data_results.csv",sep="/")
FL.MODEL    <- paste(Path.Out,"0_pscl_package_Brugs.Rdata",sep="/")	
FL.Rdata    <- paste(Path.Out,"0_pscl_data.Rdata",sep="/")	

FL.LOG      <- paste(Path.log,"0_pscl_package_Brugs.log",sep="/")	
FL.PDF      <- paste(Path.Out,"0_pscl_package_Brugs.pdf",sep="/")	
FL.SUM.cat  <- paste(Path.Out,"0_pscl_package_Brugs_cat.sum",sep="/")
FL.SUM.num  <- paste(Path.Out,"0_pscl_package_Brugs_num.sum",sep="/")

FL.package  <- paste(Path.Current,"package_loading.R",sep="/")


if (!file.exists(FL.Data.IN))  {stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.LOG))      {print(paste(FL.LOG,    "exist.Delete it!")); file.remove(FL.LOG)}
if  (file.exists(FL.PDF))      {print(paste(FL.PDF,    "exist.Delete it!")); file.remove(FL.PDF)}
if  (file.exists(FL.SUM.cat))  {print(paste(FL.SUM.cat,"exist.Delete it!")); file.remove(FL.SUM.cat)}
if  (file.exists(FL.SUM.num))  {print(paste(FL.SUM.num,"exist.Delete it!")); file.remove(FL.SUM.num)}

                       
clog <- function(x) log(x + 0.5)

cfac <- function(x, breaks = NULL) 
{
	if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
	x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
	levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
	c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
	sep = "")
	return(x)
}


##### # load data (used in pscl package)
##### load(FL.Data.IN)
##### dt <- DebTrivedi[,c(1,6:8,13,15,18)]
##### hist(dt$ofp, breaks = 0:90 -0.5)
##### plot(table(dt$ofp))
##### 
##### plot(clog(ofp) ~ cfac(numchron), data = dt)
##### 
##### plot(clog(ofp) ~ health, data = dt, varwidth = TRUE)
##### plot(clog(ofp) ~ cfac(numchron), data = dt)
##### plot(clog(ofp) ~ privins, data = dt, varwidth = TRUE)
##### plot(clog(ofp) ~ cfac(hosp, c(0:2, 8)), data = dt)
##### plot(clog(ofp) ~ gender, data = dt, varwidth = TRUE)
##### plot(cfac(ofp, c(0:2, 4, 6, 10, 100)) ~ school, data = dt, breaks = 9)
##### 
##### 
##### # convert factor to integer
##### dt.converted <- as.data.frame(matrix(rep(0,dim(dt)[1]*dim(dt)[2]), ncol=dim(dt)[2]))
##### 
##### # prepare data
##### names(dt.converted) <- names(dt)
##### idx <- 1
##### for (i in 1:dim(dt)[2])
##### {
##### 	if (is.factor(dt[,i]))
##### 	{
##### 		if       (names(dt.converted)[i] == "health")
##### 		{			
##### 			dt.converted[dt[,i] == "poor",i] <- 1
##### 			dt.converted[dt[,i] == "excellent",i] <- 2
##### 		}else if (names(dt.converted)[i] == "gender") 
##### 		{
##### 			dt.converted[dt[,i] == "male",i] <- 1	
##### 		}else if (names(dt)[i] == "privins")	
##### 		{
##### 			dt.converted[dt[,i] == "yes",i] <- 1	
##### 		}else{
##### 			dt.converted[,i] <- dt[,i]
##### 		}
##### 	}else{
##### 		if (idx == 1)
##### 		{
##### 			di.converted <- dt[,i]
##### 		}else{
##### 			cbind(dt.converted,dt[,i])
##### 		}
##### 	}
##### 	
##### }
##### cat("data has been converted\n")
##### 
##### # write the data out
##### write.table(dt.converted,file=FL.Data.OUT,sep="\t",row.names=FALSE,col.names=TRUE)
##### 
##### 
##### dt2 <- dt[,c("ofp","hosp","numchron","school")]
##### dt2 <- cbind(dt2,healthpoor = rep(0,dim(dt)[1]),healthexcellent = rep(0,dim(dt)[1]),gendermale=rep(0,dim(dt)[1]),privinsyes=rep(0,dim(dt)[1]))
##### dt2[dt[,"health"]  == "poor",     "healthpoor"]      <- 1
##### dt2[dt[,"health"]  == "excellent","healthexcellent"] <- 1
##### dt2[dt[,"gender"]  == "male",     "gendermale"]      <- 1
##### dt2[dt[,"privins"] == "yes",      "privinsyes"]      <- 1
##### cat("data has been reformated\n")
##### # write the data out
##### write.table(dt2,FL.Data.OUT,sep="\t",row.names=FALSE,col.names=TRUE)
##### save(dt2,file=FL.Rdata)
load(FL.Rdata)


library(BRugs)         

#------------------------------------------------------------------------------
# THE MODEL.

modelstring = "
	model{
		for (i in 1:n)					
		{
			ofp[i] ~ dpois(mu[i])		
			mu[i] <- lambda[i,T[i]]	

			# Log-linear model Poisson means
			lambda[i,1] <- 0						
			log(lambda[i,2]) <- b0                      + 			
					    b[1]*hosp[i]            +			
					    b[2]*numchron[i]        + 			
					    b[3]*gendermale[i]      + 			
					    b[4]*school[i]          + 			
					    b[5]*privinsyes[i]     			


			# Logistic regression for the zero-inflation probability
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



#------------------------------------------------------------------------------
# THE DATA.

# Get the data into BUGS:
datalist = list(
		   ofp             = as.vector(dt2[,"ofp"]),
		   hosp            = as.vector(dt2[,"hosp"]),
		   numchron        = as.vector(dt2[,"numchron"]),
		   school          = as.vector(dt2[,"school"]),
		   privinsyes      = as.vector(dt2[,"privinsyes"]),
		   gendermale      = as.vector(dt2[,"gendermale"]),

		   n = dim(dt2)[1] 
)
modelData( bugsData( datalist ) )



nPredictors <- 5
cat(paste("Data has been prepared!\n"))

   predictedName = "ofp"
   predictorNames = c( "hops","numchron","school","privinsyes","gendermale" )

#------------------------------------------------------------------------------
# INTIALIZE THE CHAINS.

nChains = 1
modelCompile( numChains = nChains )
cat(paste("Model has been compiled!\n"))

genInitList <- function(nPred=nPredictors) {
    list(
    	a0 = -0.06, 
    	b0 = 1.19,
        a=c(-0.82,-1.24,0.65,-0.08,-1.15),
        b=c(0.21,0.13,-0.08,0.02,0.13)
        
        # P = matrix( 0.5 , nrow=dim(dt2)[1] , ncol=2 )
    )
}

#
# modelGenInits() # often won't work for diffuse prior
for ( chainIdx in 1 : nChains ) {
     modelInits( bugsInits( genInitList ) )	# 	What has not been initialized?
}

modelGenInits()					# terminate the program unepxectedly.


cat(paste("Model has been initialized!\n"))

#------------------------------------------------------------------------------
# RUN THE CHAINS

# burn in
BurnInSteps = 1000
modelUpdate( BurnInSteps )
# actual samples
samplesSet( c("a0","b0","a","b") )
nPerChain = ceiling(1000/nChains)
thinStep = 1
modelUpdate( nPerChain , thin=thinStep )
cat(paste("Chain has been run!\n"))


#------------------------------------------------------------------------------
# EXAMINE THE RESULTS

source("plotChains.R")
source("plotPost.R")

checkConvergence = TRUE
if ( checkConvergence ) {
	a0Sum = plotChains( "a0" , saveplots=F , filenameroot=fname )
	b0Sum = plotChains( "b0" , saveplots=F , filenameroot=fname )
	aSum  = plotChains( "a"  , saveplots=F , filenameroot=fname )
	bSum  = plotChains( "b"  , saveplots=F , filenameroot=fname )
}


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


# Extract the posterior sample from BUGS:
a0Sample = matrix( samplesSample( "a0" ) )
b0Sample = matrix( samplesSample( "b0" ) )
aSample = matrix( 0 , nrow=nPredictors , ncol=nChains*nPerChain ) 
bSample = matrix( 0 , nrow=nPredictors , ncol=nChains*nPerChain ) 
for (idx in 1:nPredictors)
{
	nodeName.a <- paste("a[",idx,"]",sep="")
	nodeName.b <- paste("b[",idx,"]",sep="")
	
	aSample[idx,] = samplesSample(nodeName.a) # BRugs gets sample from BUGS
	bSample[idx,] = samplesSample(nodeName.b) # BRugs gets sample from BUGS
}

# Save MCMC sample:
save( aSample , bSample , file=FL.MODEL)


# Scatter plots of parameter values, pairwise:
if ( nPredictors <= nPredictors ) { # don't display if too many predictors
    windows()
    thinIdx = seq(1,dim(aSample)[1])
    
    pairs( cbind( aSample[thinIdx,],bSample[thinIdx,])  ,
      labels=c(paste("aSample",predictorNames,sep=""),paste("bSample",predictorNames,sep="")))
      
    dev.copy2pdf(file=paste(fname,"PostPairs.pdf",sep=""))
}
# Show correlation matrix on console:
cat("\nCorrlations of posterior sigma, b0, and bs:\n")
show( cor( cbind( aSample , bSample ) ) )



# Display the posterior:
# nPlotPerRow = 6
# nPlotRow = ceiling((2+nPredictors)/nPlotPerRow)
# nPlotCol = ceiling((2+nPredictors)/nPlotRow)
# windows(3.5*nPlotCol,2.25*nPlotRow)
# layout( matrix(1:(nPlotRow*nPlotCol),nrow=nPlotRow,ncol=nPlotCol,byrow=T) )
par( mar=c(4,3,2.5,0) , mgp=c(2,0.7,0) )
layout(matrix(1:12,nrow=2,ncol=6))
	plotPost(a0Sample,main=paste("a0:",sep=""),cex.main=1.67 , cex.lab=1.33 )
for ( sIdx in 1:nPredictors ) {
	plotPost(aSample[sIdx,],main=paste("a:",predictorNames[sIdx],sep=""),cex.main=1.67 , cex.lab=1.33 )
}

	plotPost(b0Sample,main=paste("b0:",sep=""),cex.main=1.67 , cex.lab=1.33 )
for ( sIdx in 1:nPredictors ) {
	plotPost(bSample[sIdx,],main=paste("b:",predictorNames[sIdx],sep=""),cex.main=1.67 , cex.lab=1.33 )
}



dev.copy2pdf(file=paste(fname,"PostHist.pdf",sep=""))


die


# Posterior prediction:
# Specify x values for which predicted y's are needed.
# xPostPred is a matrix such that ncol=nPredictors and nrow=nPostPredPts.
xPostPred = rbind(
    apply(x,2,mean)-3*apply(x,2,sd) , # mean of data x minus thrice SD of data x
    apply(x,2,mean)                 , # mean of data x
    apply(x,2,mean)+3*apply(x,2,sd)   # mean of data x plus thrice SD of data x
)
# Define matrix for recording posterior predicted y values for each xPostPred.
# One row per xPostPred value, with each row holding random predicted y values.
postSampSize = chainLength
yPostPred = matrix( 0 , nrow=NROW(xPostPred) , ncol=postSampSize )
# Define matrix for recording HDI limits of posterior predicted y values:
yHDIlim = matrix( 0 , nrow=NROW(xPostPred) , ncol=2 )
# Generate posterior predicted y values.
# This gets only one y value, at each x, for each step in the chain.
for ( chainIdx in 1:chainLength ) {
    yPostPred[,chainIdx] = rnorm( NROW(xPostPred) ,
                           mean = b0Samp[chainIdx]
                                  + xPostPred %*% cbind(bSample[chainIdx,]) ,
                           sd = rep( sigmaSamp[chainIdx] , NROW(xPostPred) ) )
}
source("HDIofMCMC.R")
for ( xIdx in 1:NROW(xPostPred) ) {
    yHDIlim[xIdx,] = HDIofMCMC( yPostPred[xIdx,] )
}
cat( "\nPosterior predicted y for selected x:\n" )
show( cbind( xPostPred , yPostPredMean=rowMeans(yPostPred) , yHDIlim ) )




die;


# Extract chain values:
zb0Samp = matrix( samplesSample( "b0" ) )
zbSamp = NULL
for ( j in 1:nPredictors ) {
   zbSamp = cbind( zbSamp , samplesSample( paste("b[",j,"]",sep="") ) )
}
zTauSamp = matrix( samplesSample( "tau" ) )
zSigmaSamp = 1 / sqrt( zTauSamp ) # Convert precision to SD
chainLength = length(zTauSamp)

# Convert to original scale:
bSample = zbSamp * matrix( sd(y)/apply(x,2,sd) , byrow=TRUE ,
                     ncol=nPredictors , nrow=NROW(zbSamp) )
b0Samp = ( zb0Samp * sd(y)
          + mean(y)
          - rowSums( zbSamp
          * matrix( sd(y)/apply(x,2,sd) , byrow=TRUE ,
                    ncol=nPredictors , nrow=NROW(zbSamp) )
          * matrix( apply(x,2,mean) , byrow=TRUE ,
                    ncol=nPredictors , nrow=NROW(zbSamp) ) ) )
sigmaSamp = zSigmaSamp * sd(y)

# Save MCMC sample:
save( b0Samp , bSample , sigmaSamp , 
      file="0_pscl_package_Brugs_BrugsGuber1999.Rdata" )

# Scatter plots of parameter values, pairwise:
if ( nPredictors <= 6 ) { # don't display if too many predictors
    windows()
    thinIdx = round(seq(1,length(zb0Samp),length=200))
    pairs( cbind( zSigmaSamp[thinIdx] , zb0Samp[thinIdx] , zbSamp[thinIdx,] )  ,
      labels=c("Sigma zy","zIntercept",paste("zSlope",predictorNames,sep="")))
    windows()
    thinIdx = seq(1,length(b0Samp),length=700)
    pairs( cbind( sigmaSamp[thinIdx] , b0Samp[thinIdx] , bSample[thinIdx,] ) ,
      labels=c( "Sigma y" , "Intercept", paste("Slope",predictorNames,sep="")))
    dev.copy2eps(file=paste(fname,"PostPairs.eps",sep=""))
}
# Show correlation matrix on console:
cat("\nCorrlations of posterior sigma, b0, and bs:\n")
show( cor( cbind( sigmaSamp , b0Samp , bSample ) ) )

# Display the posterior:
nPlotPerRow = 5
nPlotRow = ceiling((2+nPredictors)/nPlotPerRow)
nPlotCol = ceiling((2+nPredictors)/nPlotRow)
windows(3.5*nPlotCol,2.25*nPlotRow)
layout( matrix(1:(nPlotRow*nPlotCol),nrow=nPlotRow,ncol=nPlotCol,byrow=T) )
par( mar=c(4,3,2.5,0) , mgp=c(2,0.7,0) )
histInfo = plotPost( sigmaSamp , xlab="Sigma Value" , compVal=NULL ,
                     breaks=30 , main=bquote(sigma[y]) ,
                     cex.main=1.67 , cex.lab=1.33 )
histInfo = plotPost( b0Samp , xlab="Intercept Value" , compVal=NULL ,
                     breaks=30 , main=bquote(.(predictedName) *" at "* x==0) ,
                     cex.main=1.67 , cex.lab=1.33 )
for ( sIdx in 1:nPredictors ) {
histInfo = plotPost( bSample[,sIdx] , xlab="Slope Value" , compVal=0.0 ,
                     breaks=30 ,
                     main=bquote( Delta * .(predictedName) /
                                  Delta * .(predictorNames[sIdx]) ) ,
                     cex.main=1.67 , cex.lab=1.33 )
}
dev.copy2eps(file=paste(fname,"PostHist.eps",sep=""))

# Posterior prediction:
# Specify x values for which predicted y's are needed.
# xPostPred is a matrix such that ncol=nPredictors and nrow=nPostPredPts.
xPostPred = rbind(
    apply(x,2,mean)-3*apply(x,2,sd) , # mean of data x minus thrice SD of data x
    apply(x,2,mean)                 , # mean of data x
    apply(x,2,mean)+3*apply(x,2,sd)   # mean of data x plus thrice SD of data x
)
# Define matrix for recording posterior predicted y values for each xPostPred.
# One row per xPostPred value, with each row holding random predicted y values.
postSampSize = chainLength
yPostPred = matrix( 0 , nrow=NROW(xPostPred) , ncol=postSampSize )
# Define matrix for recording HDI limits of posterior predicted y values:
yHDIlim = matrix( 0 , nrow=NROW(xPostPred) , ncol=2 )
# Generate posterior predicted y values.
# This gets only one y value, at each x, for each step in the chain.
for ( chainIdx in 1:chainLength ) {
    yPostPred[,chainIdx] = rnorm( NROW(xPostPred) ,
                           mean = b0Samp[chainIdx]
                                  + xPostPred %*% cbind(bSample[chainIdx,]) ,
                           sd = rep( sigmaSamp[chainIdx] , NROW(xPostPred) ) )
}
source("HDIofMCMC.R")
for ( xIdx in 1:NROW(xPostPred) ) {
    yHDIlim[xIdx,] = HDIofMCMC( yPostPred[xIdx,] )
}
cat( "\nPosterior predicted y for selected x:\n" )
show( cbind( xPostPred , yPostPredMean=rowMeans(yPostPred) , yHDIlim ) )

#------------------------------------------------------------------------------