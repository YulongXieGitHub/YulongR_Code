#
# 0_classical_GLM.R
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
# 0_classical_GLM.R
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
Path.Data.IN <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_pscl_data"
Path.log     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_log"												
Path.Out     <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/0_classical_GLM"
if (!file.exists(Path.Data.IN)){stop(paste(" INPUT data folder does NOT exist!\n",sep=""))}
if (!file.exists(Path.log))    {print(paste("NOT existing:",Path.log));dir.create(Path.log,showWarnings=TRUE,recursive=TRUE)}
if (!file.exists(Path.Out))    {print(paste("NOT existing:",Path.Out));dir.create(Path.Out,showWarnings=TRUE,recursive=TRUE)}


FL.Data.IN  <- paste(Path.Data.IN,"DebTrivedi.rda",sep="/")
FL.Data.OUT <- paste(Path.Out,"dt.csv",sep="/")
FL.Data.OBJ <- paste(Path.Out,"dt.Rdata",sep="/")
FL.RESL.OUT <- paste(Path.Out,"0_pscl_data_results.csv",sep="/")

FL.LOG      <- paste(Path.log,"0_classical_GLM.log",sep="/")	
FL.PDF      <- paste(Path.Out,"0_classical_GLM.pdf",sep="/")	
FL.SUM.cat  <- paste(Path.Out,"0_pscl_package_cat.sum",sep="/")
FL.SUM.num  <- paste(Path.Out,"0_pscl_package_num.sum",sep="/")

FL.package  <- paste(Path.Current,"package_loading.R",sep="/")


if (!file.exists(FL.Data.IN))  {stop(paste(" INPUT data file   does NOT exist!\n",sep=""))}
if  (file.exists(FL.Data.OUT)) {print(paste(FL.Data.OUT,"exist.Delete it!")); file.remove(FL.Data.OUT)}
if  (file.exists(FL.Data.OBJ)) {print(paste(FL.Data.OBJ,"exist.Delete it!")); file.remove(FL.Data.OBJ)}
if  (file.exists(FL.RESL.OUT)) {print(paste(FL.RESL.OUT,"exist.Delete it!")); file.remove(FL.RESL.OUT)}

if  (file.exists(FL.LOG))      {print(paste(FL.LOG,     "exist.Delete it!")); file.remove(FL.LOG)}
if  (file.exists(FL.PDF))      {print(paste(FL.PDF,     "exist.Delete it!")); file.remove(FL.PDF)}
if  (file.exists(FL.SUM.cat))  {print(paste(FL.SUM.cat, "exist.Delete it!")); file.remove(FL.SUM.cat)}
if  (file.exists(FL.SUM.num))  {print(paste(FL.SUM.num, "exist.Delete it!")); file.remove(FL.SUM.num)}

# open pdf file for outputting plots
pdf(file = FL.PDF,         paper="a4r",width=0,height=0)	



# -------------------------------------------------------------------------------------------------
# load two functions which is needed
# -------------------------------------------------------------------------------------------------
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

# -------------------------------------------------------------------------------------------------
# Loading packages
# -------------------------------------------------------------------------------------------------
argument <- "glm"
source(FL.package)

# -------------------------------------------------------------------------------------------------
# load data (used in pscl package)
# -------------------------------------------------------------------------------------------------
load(FL.Data.IN)
dt <- DebTrivedi[,c(1,6:8,13,15,18)]
hist(dt$ofp, breaks = 0:90 -0.5)
plot(table(dt$ofp))

plot(clog(ofp) ~ cfac(numchron), data = dt)

plot(clog(ofp) ~ health, data = dt, varwidth = TRUE)
plot(clog(ofp) ~ cfac(numchron), data = dt)
plot(clog(ofp) ~ privins, data = dt, varwidth = TRUE)
plot(clog(ofp) ~ cfac(hosp, c(0:2, 8)), data = dt)
plot(clog(ofp) ~ gender, data = dt, varwidth = TRUE)
plot(cfac(ofp, c(0:2, 4, 6, 10, 100)) ~ school, data = dt, breaks = 9)


# convert factor to integer
dt.converted <- as.data.frame(matrix(rep(0,dim(dt)[1]*dim(dt)[2]), ncol=dim(dt)[2]))

# prepare data by converting the character string categories into integer for factor levels
names(dt.converted) <- names(dt)
idx <- 1
for (i in 1:dim(dt)[2])
{
	if (is.factor(dt[,i]))
	{
		if       (names(dt.converted)[i] == "health")
		{			
			dt.converted[dt[,i] == "poor",i] <- 1
			dt.converted[dt[,i] == "excellent",i] <- 2
		}else if (names(dt.converted)[i] == "gender") 
		{
			dt.converted[dt[,i] == "male",i] <- 1	
		}else if (names(dt)[i] == "privins")	
		{
			dt.converted[dt[,i] == "yes",i] <- 1	
		}else{
			dt.converted[,i] <- dt[,i]
		}
	}else{
		if (idx == 1)
		{
			di.converted <- dt[,i]
		}else{
			cbind(dt.converted,dt[,i])
		}
	}
	
}
write.table(dt.converted,file=FL.Data.OUT,sep="\t",row.names=FALSE,col.names=TRUE)
cat("character string categories have been converted into integers and the converted data has been outputted!\n")
cat("character string categories have been converted into integers and the converted data has been outputted!\n",file=FL.LOG,append=TRUE)

# -------------------------------------------------------------------------------------------------
# dummy code categorical variables in to 0/1 coding
# -------------------------------------------------------------------------------------------------
dt2 <- dt[,c("ofp","hosp","numchron","school")]
dt2 <- cbind(dt2,healthpoor = rep(0,dim(dt)[1]),healthexcellent = rep(0,dim(dt)[1]),gendermale=rep(0,dim(dt)[1]),privinsyes=rep(0,dim(dt)[1]))
dt2[dt[,"health"]  == "poor",     "healthpoor"]      <- 1
dt2[dt[,"health"]  == "excellent","healthexcellent"] <- 1
dt2[dt[,"gender"]  == "male",     "gendermale"]      <- 1
dt2[dt[,"privins"] == "yes",      "privinsyes"]      <- 1
write.table(dt2,FL.Data.OUT,sep="\t",row.names=FALSE,col.names=TRUE)
cat("data reformated by replacing the categorical variables with dummy variable and the reformated data has been outputted!\n")
cat("data reformated by replacing the categorical variables with dummy variable and the reformated data has been outputted!\n",file=FL.LOG,append=TRUE)




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





# *************************************************************************************************
# 1. Poisson Model
# *************************************************************************************************
cat(paste("Poisson Model\n",sep=""))
cat(paste("Poisson Model\n",sep=""),file=FL.LOG,append=TRUE)
fm_pois <- glm(ofp ~ ., data = dt, family = poisson)

# estimated lambda
est.pred   <- predict(fm_pois)		# predict is the linear predictor, eta.  In Poisson the linear predictor is linked with a log function: log(mu) = eta, so the lambda in Poisson distribution is: lambda = mu = exp(eta)
est.mu     <- exp(est.pred)		# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda <- fm_pois$fitted		# lambda is mu which is the expected value: lamnda estimated using the fitted values from modeling, which should be exact the same as "est.mu"
est.zero.p <- dpois(0,est.lambda)	# the lambda defines the Posisson distribution and we can estimate the probability of zeros of each observation
est.pois   <- cbind(ofp        = dt[,"ofp"],
                    eta        = est.pred,
                    mu         = est.mu,
                    lambda     = est.lambda,
                    est.zero.p = est.zero.p)
                    
estimation.Poisson <- rbind(sum1 = apply(est.pois,2,sum),
                            sum0 = apply(est.pois,2,function(x){sum(x==0)}),
                            est.pois)
                    

par(mfrow = c(2,2))
 hist(estimation.Poisson[3:4408,"ofp"],nclass=100,xlab="ofp",ylab="frequency",main="[fm_pois]: distribution of observed count")
 hist(estimation.Poisson[3:4408,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_pois]: distribution of expected count")

 plot(estimation.Poisson[3:4408,"ofp"],estimation.Poisson[3:4408,"mu"],type="p",pch=16,cex=0.5,xlab="ofp count",ylab="expected",main="[fm_Poisson]: model fitting")
 abline(a=0,b=1,col="red")
                    

summary(fm_pois)	# note the Pr is from Wald test which might be too optimistic due to a misspecificaTION OF THE LIKELIHOOD.
      df.coef  <- data.frame(summary(fm_pois)$coefficients)
names(df.coef) <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(aic = summary(fm_pois)$aic,deviance.null = summary(fm_pois)$null.deviance,df.null = summary(fm_pois)$df.null,deviance.residual = summary(fm_pois)$deviance,df.residual=summary(fm_pois)$df.residual)

cat(paste("\nPoisson Model\n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Poisson Model,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Poisson Model,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

# TO RE-COMPUTE THE wALD TEST USING SANDWICH STANDARD ERROS
tmp <- coeftest(fm_pois, vcov = sandwich)

      df.wald  <- data.frame(tmp[1:8,])
names(df.wald) <- c("Estimate","StdError","zValue","Pr(>|z|)")

cat(paste("Poisson Model(re-calc Wald test),",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.wald,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Poisson Model is finished! ------------------------\n")

# -----------------------------------------------------------------
# Call:
# glm(formula = ofp ~ ., family = poisson, data = dt)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -8.4055  -1.9962  -0.6737   0.7049  16.3620  
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      1.028874   0.023785  43.258   <2e-16 ***		# 1. should be clear what is this Wald test being computed
# hosp             0.164797   0.005997  27.478   <2e-16 ***
# healthpoor       0.248307   0.017845  13.915   <2e-16 ***
# healthexcellent -0.361993   0.030304 -11.945   <2e-16 ***
# numchron         0.146639   0.004580  32.020   <2e-16 ***
# gendermale      -0.112320   0.012945  -8.677   <2e-16 ***
# school           0.026143   0.001843  14.182   <2e-16 ***
# privinsyes       0.201687   0.016860  11.963   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
#     Null deviance: 26943  on 4405  degrees of freedom			# 1. should be able to hand calculate the deviance
# Residual deviance: 23168  on 4398  degrees of freedom
# AIC: 35959
# 
# Number of Fisher Scoring iterations: 5
# -----------------------------------------------------------------




# -----------------------------------------------------------------
# z test of coefficients:
# 
#                  Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)      1.028874   0.064530 15.9442 < 2.2e-16 ***		# 1. should be clear what is this revised Wald test being computed
# hosp             0.164797   0.021945  7.5095 5.935e-14 ***
# healthpoor       0.248307   0.054022  4.5964 4.298e-06 ***
# healthexcellent -0.361993   0.077449 -4.6740 2.954e-06 ***
# numchron         0.146639   0.012908 11.3605 < 2.2e-16 ***
# gendermale      -0.112320   0.035343 -3.1780  0.001483 ** 
# school           0.026143   0.005084  5.1422 2.715e-07 ***
# privinsyes       0.201687   0.043128  4.6765 2.919e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
# -----------------------------------------------------------------


# *************************************************************************************************
# 2. Quasi-Poisson Model
# *************************************************************************************************
rm(df.coef,df.sum)
fm_qpois <- glm(ofp ~ ., data = dt, family = quasipoisson)

# estimated lambda
est.pred    <- predict(fm_qpois)	# predict is the linear predictor, eta.  In Poisson the linear predictor is linked with a log function: log(mu) = eta, so the lambda in Poisson distribution is: lambda = mu = exp(eta)
est.mu      <- exp(est.pred)		# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda  <- fm_qpois$fitted		# lambda is mu which is the expected value: lamnda estimated using the fitted values from modeling, which should be exact the same as "est.mu"
est.zero.p  <- dpois(0,est.lambda)	# the lambda defines the Posisson distribution and we can estimate the probability of zeros of each observation
est.qpois   <- cbind(ofp        = dt[,"ofp"],
                     eta        = est.pred,
                     mu         = est.mu,
                     lambda     = est.lambda,
                     est.zero.p = est.zero.p)
                    
estimation.QuasiPoisson <- rbind(sum1 = apply(est.qpois,2,sum),
                                 sum0 = apply(est.qpois,2,function(x){sum(x==0)}),
                                 est.qpois)
                    


par(mfrow = c(2,2))
 hist(estimation.QuasiPoisson[3:4408,"ofp"],nclass=100,xlab="ofp",ylab="frequency",main="[fm_qpois]: distribution of observed count")
 hist(estimation.QuasiPoisson[3:4408,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_qpois]: distribution of expected count")

 plot(estimation.QuasiPoisson[3:4408,"ofp"],estimation.QuasiPoisson[3:4408,"mu"],type="p",pch=16,cex=0.5,xlab="ofp count",ylab="expected",main="[fm_QuasiPoisson]: model fitting")
 abline(a=0,b=1,col="red")
 
                    
                    
summary(fm_qpois)							# Coefficients are exact the same but se are different so the Pr


coeftest(fm_qpois, vcov = sandwich)					# why "coeftest(fm_qpois, vcov = sandwich)" provide exact the same results as "coeftest(fm_pois, vcov = sandwich)"  


      df.coef  <- data.frame(summary(fm_qpois)$coefficients)
names(df.coef) <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(aic = summary(fm_qpois)$aic,deviance.null = summary(fm_qpois)$null.deviance,df.null = summary(fm_qpois)$df.null,deviance.residual = summary(fm_qpois)$deviance,df.residual=summary(fm_qpois)$df.residual)

cat(paste("\nQuasi-Poisson\n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Quasi-Poisson,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Quasi-Poisson,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

tmp <- coeftest(fm_qpois, vcov = sandwich)
      df.wald  <- data.frame(tmp[1:8,])
names(df.wald) <- c("Estimate","StdError","zValue","Pr(>|z|)")

cat(paste("Quasi-Poisson(re-calc Wald test),",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.wald,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Quasi-Poisson Model is finished! ------------------------\n")



# Call:
# glm(formula = ofp ~ ., family = quasipoisson, data = dt)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -8.4055  -1.9962  -0.6737   0.7049  16.3620  
# 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      1.028874   0.061594  16.704  < 2e-16 ***
# hosp             0.164797   0.015531  10.611  < 2e-16 ***
# healthpoor       0.248307   0.046211   5.373 8.13e-08 ***
# healthexcellent -0.361993   0.078476  -4.613 4.09e-06 ***
# numchron         0.146639   0.011860  12.364  < 2e-16 ***
# gendermale      -0.112320   0.033523  -3.351 0.000813 ***
# school           0.026143   0.004774   5.477 4.58e-08 ***
# privinsyes       0.201687   0.043661   4.619 3.96e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for quasipoisson family taken to be 6.706254)
# 
#     Null deviance: 26943  on 4405  degrees of freedom
# Residual deviance: 23168  on 4398  degrees of freedom
# AIC: NA
# 
# Number of Fisher Scoring iterations: 5
# 
# > coeftest(fm_qpois, vcov = sandwich)# why "coeftest(fm_qpois, vcov = sandwich)" provide exact the same results as "coeftest(fm_pois, vcov = sandwich)"  
# 
# z test of coefficients:
# 
#                  Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)      1.028874   0.064530 15.9442 < 2.2e-16 ***
# hosp             0.164797   0.021945  7.5095 5.935e-14 ***
# healthpoor       0.248307   0.054022  4.5964 4.298e-06 ***
# healthexcellent -0.361993   0.077449 -4.6740 2.954e-06 ***
# numchron         0.146639   0.012908 11.3605 < 2.2e-16 ***
# gendermale      -0.112320   0.035343 -3.1780  0.001483 ** 
# school           0.026143   0.005084  5.1422 2.715e-07 ***
# privinsyes       0.201687   0.043128  4.6765 2.919e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# 

# *************************************************************************************************
# 3. Negative Binomial Model
# dnbinom(x, size, prob, mu, log = FALSE)
# x:	vector of (non-negative integer) quantiles.
# q:	vector of quantiles.
# p:	vector of probabilities.
# n:	number of observations. If length(n) > 1, the length is taken to be the number required.
# size:	target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution). Must be strictly positive, need not be integer.
# prob: probability of success in each trial. 0 < prob <= 1.
# mu:	alternative parametrization via mean: see ‘Details’.
# *************************************************************************************************
rm(df.coef,df.sum)
fm_nbin <- glm.nb(ofp ~ ., data = dt)

# estimated lambda
est.pred    <- predict(fm_nbin)					# predict is the linear predictor, eta.  In Poisson the linear predictor is linked with a log function: log(mu) = eta, so the lambda in Poisson distribution is: lambda = mu = exp(eta)
est.mu      <- exp(est.pred)					# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda  <- fm_nbin$fitted					# lambda is mu which is the expected value: lamnda estimated using the fitted values from modeling, which should be exact the same as "est.mu"
est.zero.p  <- dnbinom(0,mu=est.lambda,size=fm_nbin$theta)	# the lambda defines the Posisson distribution and we can estimate the probability of zeros of each observation
est.nbin    <- cbind(ofp        = dt[,"ofp"],
                     eta        = est.pred,
                     mu         = est.mu,
                     lambda     = est.lambda,
                     est.zero.p = est.zero.p)
                    
estimation.nbin <- rbind(sum1 = apply(est.nbin,2,sum),
                         sum0 = apply(est.nbin,2,function(x){sum(x==0)}),
                         est.nbin)
                    
                    

par(mfrow = c(2,2))
 hist(estimation.nbin[3:4408,"ofp"],nclass=100,xlab="ofp",ylab="frequency",main="[fm_nbin]: distribution of observed count")
 hist(estimation.nbin[3:4408,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_nbin]: distribution of expected count")

 plot(estimation.nbin[3:4408,"ofp"],estimation.nbin[3:4408,"mu"],type="p",pch=16,cex=0.5,xlab="ofp count",ylab="expected",main="[fm_nbin]: model fitting")
 abline(a=0,b=1,col="red")
 
                    
                    
                    
                    
                    
summary(fm_nbin)
coeftest(fm_nbin, vcov = sandwich)					# so the sandwich adjustment should not be used in nb and qpois models??????


      df.coef  <- data.frame(summary(fm_nbin)$coefficients)
names(df.coef) <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(aic = summary(fm_nbin)$aic,deviance.null = summary(fm_nbin)$null.deviance,df.null = summary(fm_nbin)$df.null,deviance.residual = summary(fm_nbin)$deviance,df.residual=summary(fm_nbin)$df.residual)

cat(paste("\nNegative Binomial Model\n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Negative Binomial Model,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Negative Binomial Model,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

tmp <- coeftest(fm_nbin, vcov = sandwich)
      df.wald  <- data.frame(tmp[1:8,])
names(df.wald) <- c("Estimate","StdError","zValue","Pr(>|z|)")

cat(paste("Negative Binomial Model(re-calc Wald test),",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.wald,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Negative Binomial Model is finished! ------------------------\n")



# Call:
# glm.nb(formula = ofp ~ ., data = dt, init.theta = 1.206603534, 
#     link = log)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -3.0469  -0.9955  -0.2948   0.2961   5.8185  
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      0.929257   0.054591  17.022  < 2e-16 ***
# hosp             0.217772   0.020176  10.793  < 2e-16 ***
# healthpoor       0.305013   0.048511   6.288 3.23e-10 ***
# healthexcellent -0.341807   0.060924  -5.610 2.02e-08 ***
# numchron         0.174916   0.012092  14.466  < 2e-16 ***
# gendermale      -0.126488   0.031216  -4.052 5.08e-05 ***
# school           0.026815   0.004394   6.103 1.04e-09 ***
# privinsyes       0.224402   0.039464   5.686 1.30e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for Negative Binomial(1.2066) family taken to be 1)
# 
#     Null deviance: 5743.7  on 4405  degrees of freedom
# Residual deviance: 5044.5  on 4398  degrees of freedom
# AIC: 24359
# 
# Number of Fisher Scoring iterations: 1
# 
# 
#               Theta:  1.2066 
#           Std. Err.:  0.0336 
# 
#  2 x log-likelihood:  -24341.1070 
# > coeftest(fm_nbin, vcov = sandwich)# so the sandwich adjustment should not be used in nb and qpois models??????
# 
# z test of coefficients:
# 
#                   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)      0.9292566  0.0654395 14.2002 < 2.2e-16 ***
# hosp             0.2177722  0.0192186 11.3313 < 2.2e-16 ***
# healthpoor       0.3050130  0.0542329  5.6241 1.864e-08 ***
# healthexcellent -0.3418066  0.0820047 -4.1681 3.071e-05 ***
# numchron         0.1749155  0.0127451 13.7242 < 2.2e-16 ***
# gendermale      -0.1264881  0.0354814 -3.5649  0.000364 ***
# school           0.0268151  0.0051228  5.2345 1.655e-07 ***
# privinsyes       0.2244019  0.0441435  5.0835 3.706e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# 
# 
# *************************************************************************************************
# 4. Hurdle regression Model
# *************************************************************************************************
rm(df.coef,df.sum)
fm_hurdle0 <- hurdle(ofp ~ ., data = dt, dist = "negbin")
summary(fm_hurdle0)


      df.coef.count  <- data.frame(summary(fm_hurdle0)$coefficients$count)
      df.coef.zero   <- data.frame(summary(fm_hurdle0)$coefficients$zero)
names(df.coef.count) <- c("Estimate","StdError","zValue","Pr(>|z|)")
names(df.coef.zero)  <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(df.null = fm_hurdle0$df.null,df.residual=fm_hurdle0$df.residual,theta = fm_hurdle0$theta,loglik = fm_hurdle0$loglik)

cat(paste("\nHurdle regression Model \n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Hurdle regression Model (count) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.count,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Hurdle regression Model (zero) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.zero, file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Hurdle regression Model ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Hurdle regression Model regression Model is finished! ------------------------\n")





# Call:
# hurdle(formula = ofp ~ ., data = dt, dist = "negbin")
# 
# Pearson residuals:
#     Min      1Q  Median      3Q     Max 
# -1.1718 -0.7080 -0.2737  0.3196 18.0092 
# 
# Count model coefficients (truncated negbin with log link):
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      1.197699   0.058973  20.309  < 2e-16 ***
# hosp             0.211898   0.021396   9.904  < 2e-16 ***
# healthpoor       0.315958   0.048056   6.575 4.87e-11 ***
# healthexcellent -0.331861   0.066093  -5.021 5.14e-07 ***
# numchron         0.126421   0.012452  10.152  < 2e-16 ***
# gendermale      -0.068317   0.032416  -2.108   0.0351 *  
# school           0.020693   0.004535   4.563 5.04e-06 ***
# privinsyes       0.100172   0.042619   2.350   0.0188 *  
# Log(theta)       0.333255   0.042754   7.795 6.46e-15 ***
# Zero hurdle model coefficients (binomial with logit link):
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      0.043147   0.139852   0.309 0.757688    
# hosp             0.312449   0.091437   3.417 0.000633 ***
# healthpoor      -0.008716   0.161024  -0.054 0.956833    
# healthexcellent -0.289570   0.142682  -2.029 0.042409 *  
# numchron         0.535213   0.045378  11.794  < 2e-16 ***
# gendermale      -0.415658   0.087608  -4.745 2.09e-06 ***
# school           0.058541   0.011989   4.883 1.05e-06 ***
# privinsyes       0.747120   0.100880   7.406 1.30e-13 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Theta: count = 1.3955
# Number of iterations in BFGS optimization: 16 
# Log-likelihood: -1.209e+04 on 17 Df



# calculate the likelihood of the Poisson and Negative Binomial Model to compare with the likelihhod of the hurdle model

# *************************************************************************************************
# 5. Hurdle regression Model by removing health since the one including health show insignificant coeeficients
# *************************************************************************************************
rm(df.coef.count,df.coef.zero,df.sum)
fm_hurdle <- hurdle(ofp ~ . | hosp + numchron + privins + school + gender, data = dt, dist = "negbin")	# 
summary(fm_hurdle)
waldtest(fm_hurdle0, fm_hurdle)
lrtest(fm_hurdle0, fm_hurdle)			# likelihood ratio test for the two models (should the two models are nest models???


      df.coef.count  <- data.frame(summary(fm_hurdle)$coefficients$count)
      df.coef.zero   <- data.frame(summary(fm_hurdle)$coefficients$zero)
names(df.coef.count) <- c("Estimate","StdError","zValue","Pr(>|z|)")
names(df.coef.zero)  <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(df.null = fm_hurdle$df.null,df.residual=fm_hurdle$df.residual,theta = fm_hurdle$theta,loglik = fm_hurdle$loglik)

cat(paste("\nHurdle regression Model Without Health \n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Hurdle regression Model Without Health (count) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.count,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Hurdle regression Model Without Health (zero) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.zero, file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Hurdle regression Model Without Health ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Hurdle regression Model Without Health regression Model is finished! ------------------------\n")




# Call:
# hurdle(formula = ofp ~ . | hosp + numchron + privins + school + gender, data = dt, dist = "negbin")
# 
# Pearson residuals:
#     Min      1Q  Median      3Q     Max 
# -1.1719 -0.7089 -0.2733  0.3229 17.9185 
# 
# Count model coefficients (truncated negbin with log link):
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      1.197699   0.058973  20.309  < 2e-16 ***
# hosp             0.211898   0.021396   9.904  < 2e-16 ***
# healthpoor       0.315958   0.048056   6.575 4.87e-11 ***
# healthexcellent -0.331861   0.066093  -5.021 5.14e-07 ***
# numchron         0.126421   0.012452  10.152  < 2e-16 ***
# gendermale      -0.068317   0.032416  -2.108   0.0351 *  
# school           0.020693   0.004535   4.563 5.04e-06 ***
# privinsyes       0.100172   0.042619   2.350   0.0188 *  
# Log(theta)       0.333255   0.042754   7.795 6.46e-15 ***
# Zero hurdle model coefficients (binomial with logit link):
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.01594    0.13776   0.116 0.907881    
# hosp         0.31843    0.09107   3.496 0.000471 ***
# numchron     0.54783    0.04358  12.571  < 2e-16 ***
# privinsyes   0.74572    0.10031   7.434 1.05e-13 ***
# school       0.05707    0.01193   4.785 1.71e-06 ***
# gendermale  -0.41915    0.08751  -4.790 1.67e-06 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Theta: count = 1.3955
# Number of iterations in BFGS optimization: 16 
# Log-likelihood: -1.209e+04 on 15 Df
# > waldtest(fm_hurdle0, fm_hurdle)
# Error in nobs.default(x, ...) : no 'nobs' method is available
# Error in nobs.default(x, ...) : no 'nobs' method is available
# Error in nobs.default(x, ...) : no 'nobs' method is available
# Wald test
# 
# Model 1: ofp ~ .
# Model 2: ofp ~ . | hosp + numchron + privins + school + gender
#   Res.Df Df  Chisq Pr(>Chisq)
# 1   4389                     
# 2   4391 -2 4.1213     0.1274
# > 
# > 
# > lrtest(fm_hurdle0, fm_hurdle)
# Error in nobs.default(x, ...) : no 'nobs' method is available
# Error in nobs.default(x, ...) : no 'nobs' method is available
# Likelihood ratio test
# 
# Model 1: ofp ~ .
# Model 2: ofp ~ . | hosp + numchron + privins + school + gender
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  17 -12088                     
# 2  15 -12090 -2 3.9875     0.1362				       The test does not show significant difference between the two models, so we should use the model with the removal of health???!!!
# 

# *************************************************************************************************
# 6. Zero-inflated negative binomial regression
#
# How to get the two estimates for each observations, i.e., the expected count and the inflated zero probability?
#
# The predict function in zeroinfl is different from the glm. (Both the fitted and predict methods can compute fitted responses in zeroinfl)
# In GLM, "predict" gives the values of the linear predictor "eta" and "fitted" gives the expected values "mu" which is "eta" after applied the mean function on the linear predictore, in Poisson, mu = exp(eta)
# In zeroinfl, "predict" gives the same thing of "fitted"
#              also, "predict" without argument gives the estimated expected values (counts) Default is "response"
#                    "predict" has 4 types, which are "response","prob","count","zero"
#                              "response": the fitted count
#                              "prob": probability to have count from 0 to the maximum observed count in the data
#                              "count":  the predicted mean from the count component (without zero inflation) 
#                              "zero":   and the predicted probability for the zero component
#                   
#                    "predict" with argument "type="prob"" gives a matrix of probability with a dimensionof N by K where N is the number of observations and K is the maximum count observed in the data.
# *************************************************************************************************
rm(df.coef.count,df.coef.zero,df.sum)
fm_zinb0 <- zeroinfl(ofp ~ ., data = dt, dist = "negbin")

# estimated lambda
est.mu          <- predict(fm_zinb0)				# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda      <- fm_zinb0$fitted				# the expected count from the model
est.zero.p      <- predict(fm_zinb0, type = "prob")[,1]		# the probability to have zero count
est.zero.infl   <- predict(fm_zinb0, type = "zero")		# the probability to have inflated zeros
est.count       <- predict(fm_zinb0, type = "count")		# the expected count without the zero inflation

est.zinb0   	<- cbind(ofp           = dt[,"ofp"],
                         mu            = est.mu,
                         lambda        = est.lambda,
                         est.zero.p    = est.zero.p,
                         est.zero.infl = est.zero.infl,
                         est.count     = est.count)
                    
estimation.zinb0 <- rbind(sum1 = apply(est.zinb0,2,sum),
                          sum0 = apply(est.zinb0,2,function(x){sum(x==0)}),
                          est.zinb0)
                    
par(mfrow = c(2,2))
 hist(estimation.zinb0[3:4408,"ofp"],nclass=100,xlab="ofp",ylab="frequency",main="[fm_zinb0]: distribution of observed count")
 hist(estimation.zinb0[3:4408,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_zinb0]: distribution of expected count")
                    
 hist(estimation.zinb0[3:4408,"est.zero.infl"],nclass=100,xlab="p.0infl",ylab="frequency",main="[fm_zinb0]:distribution of probability of inflated zero")
 plot(estimation.zinb0[3:4408,"ofp"],estimation.zinb0[3:4408,"mu"],type="p",pch=16,cex=0.5,xlab="ofp count",ylab="expected",main="[fm_zinb0]:model fitting (Zero-Inflated)")
 abline(a=0,b=1,col="red")
                    

                    
summary(fm_zinb0)



      df.coef.count  <- data.frame(summary(fm_zinb0)$coefficients$count)
      df.coef.zero   <- data.frame(summary(fm_zinb0)$coefficients$zero)
names(df.coef.count) <- c("Estimate","StdError","zValue","Pr(>|z|)")
names(df.coef.zero)  <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(df.null = fm_zinb0$df.null,df.residual=fm_zinb0$df.residual,theta = fm_zinb0$theta,loglik = fm_zinb0$loglik)

cat(paste("\nZero-inflated negative binomial \n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Zero-inflated negative binomial (count) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.count,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Zero-inflated negative binomial (zero) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.zero, file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Zero-inflated negative binomial ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Zero-inflated negative binomial regression Model is finished! ------------------------\n")







# Call:
# zeroinfl(formula = ofp ~ ., data = dt, dist = "negbin")
# 
# 
# Zero-inflation model coefficients (binomial with logit link):
#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -0.06354    0.27668  -0.230  0.81837    
# hosp            -0.81760    0.43875  -1.863  0.06240 .  
# healthpoor       0.10178    0.44071   0.231  0.81735    
# healthexcellent  0.10488    0.30965   0.339  0.73484    
# numchron        -1.24630    0.17918  -6.956 3.51e-12 ***
# gendermale       0.64937    0.20046   3.239  0.00120 ** 
# school          -0.08481    0.02676  -3.169  0.00153 ** 
# privinsyes      -1.15808    0.22436  -5.162 2.45e-07 ***

# Pearson residuals:
#     Min      1Q  Median      3Q     Max 
# -1.1966 -0.7097 -0.2784  0.3256 17.7661 
# 
# Count model coefficients (negbin with log link):
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      1.193466   0.056737  21.035  < 2e-16 ***
# hosp             0.201214   0.020392   9.867  < 2e-16 ***
# healthpoor       0.287190   0.045940   6.251 4.07e-10 ***
# healthexcellent -0.313540   0.062977  -4.979 6.40e-07 ***
# numchron         0.128955   0.011938  10.802  < 2e-16 ***
# gendermale      -0.080093   0.031035  -2.581  0.00986 ** 
# school           0.021338   0.004368   4.886 1.03e-06 ***
# privinsyes       0.126815   0.041687   3.042  0.00235 ** 
# Log(theta)       0.394731   0.035145  11.231  < 2e-16 ***


# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Theta = 1.484 
# Number of iterations in BFGS optimization: 31 
# Log-likelihood: -1.209e+04 on 17 Df
# 
# 
# 
# 
# 
# 
# *************************************************************************************************
# 7. Zero-inated regression without health
# *************************************************************************************************
rm(df.coef.count,df.coef.zero,df.sum)
fm_zinb <- zeroinfl(ofp ~ hosp + numchron + privins + school + gender | hosp + numchron + privins + school + gender,data = dt, dist = "negbin")

# estimated lambda
est.mu          <- predict(fm_zinb)				# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda      <- fm_zinb$fitted				# the expected count from the model
est.zero.p      <- predict(fm_zinb, type = "prob")[,1]		# the probability to have zero count
est.zero.infl   <- predict(fm_zinb, type = "zero")		# the probability to have inflated zeros
est.count       <- predict(fm_zinb, type = "count")		# the expected count without the zero inflation

est.zinb   	<- cbind(ofp           = dt[,"ofp"],
                         mu            = est.mu,
                         lambda        = est.lambda,
                         est.zero.p    = est.zero.p,
                         est.zero.infl = est.zero.infl,
                         est.count     = est.count)
                    
estimation.zinb <- rbind(sum1 = apply(est.zinb,2,sum),
                         sum0 = apply(est.zinb,2,function(x){sum(x==0)}),
                         est.zinb)
                    
             
                    
par(mfrow = c(2,2))
 hist(estimation.zinb[3:4408,"ofp"],nclass=100,xlab="ofp",ylab="frequency",main="[fm_zinb]: distribution of observed count")
 hist(estimation.zinb[3:4408,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_zinb]: distribution of expected count")
                    
 hist(estimation.zinb[3:4408,"est.zero.infl"],nclass=100,xlab="p.0infl",ylab="frequency",main="[fm_zinb]:distribution of probability of inflated zero")
 plot(estimation.zinb[3:4408,"ofp"],estimation.zinb[3:4408,"mu"],type="p",pch=16,cex=0.5,xlab="ofp count",ylab="expected",main="[fm_zinb]:model fitting (Zero-Inflated)")
 abline(a=0,b=1,col="red")
                                        
                    
                    
summary(fm_zinb)


      df.coef.count  <- data.frame(summary(fm_zinb)$coefficients$count)
      df.coef.zero   <- data.frame(summary(fm_zinb)$coefficients$zero)
names(df.coef.count) <- c("Estimate","StdError","zValue","Pr(>|z|)")
names(df.coef.zero)  <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(df.null = fm_zinb$df.null,df.residual=fm_zinb$df.residual,theta = fm_zinb$theta,loglik = fm_zinb$loglik)

cat(paste("\nZero-inflated negative binomial without Health \n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Zero-inflated negative binomial without Health (count) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.count,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Zero-inflated negative binomial without Health (zero) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.zero, file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Zero-inflated negative binomial without Health ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Zero-inflated negative binomial regression Model without Health is finished! ------------------------\n")





# Call:
# zeroinfl(formula = ofp ~ . | hosp + numchron + privins + school + gender, data = dt, dist = "negbin")
# 
# Pearson residuals:
#     Min      1Q  Median      3Q     Max 
# -1.1963 -0.7105 -0.2779  0.3253 17.8452 
# 
# Count model coefficients (negbin with log link):
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      1.193716   0.056661  21.068  < 2e-16 ***
# hosp             0.201477   0.020360   9.896  < 2e-16 ***
# healthpoor       0.285133   0.045093   6.323 2.56e-10 ***
# healthexcellent -0.319339   0.060405  -5.287 1.25e-07 ***
# numchron         0.128999   0.011931  10.813  < 2e-16 ***
# gendermale      -0.080277   0.031024  -2.588  0.00967 ** 
# school           0.021423   0.004358   4.916 8.82e-07 ***
# privinsyes       0.125865   0.041588   3.026  0.00247 ** 
# Log(theta)       0.394144   0.035035  11.250  < 2e-16 ***
# 
# Zero-inflation model coefficients (binomial with logit link):
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.04684    0.26855  -0.174  0.86154    
# hosp        -0.80046    0.42081  -1.902  0.05715 .  
# numchron    -1.24790    0.17831  -6.999 2.59e-12 ***
# privinsyes  -1.17558    0.22012  -5.341 9.26e-08 ***
# school      -0.08378    0.02625  -3.191  0.00142 ** 
# gendermale   0.64766    0.20011   3.236  0.00121 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Theta = 1.4831 
# Number of iterations in BFGS optimization: 28 
# Log-likelihood: -1.209e+04 on 15 Df
# 
# > lrtest(fm_zinb0, fm_zinb)
# Error in nobs.default(x, ...) : no 'nobs' method is available
# Error in nobs.default(x, ...) : no 'nobs' method is available
# Likelihood ratio test
# 
# Model 1: ofp ~ .
# Model 2: ofp ~ . | hosp + numchron + privins + school + gender
#   #Df LogLik Df  Chisq Pr(>Chisq)
# 1  17 -12091                     
# 2  15 -12091 -2 0.1525     0.9266
# > waldtest(fm_zinb0, fm_zinb)
# Error in nobs.default(x, ...) : no 'nobs' method is available
# Error in nobs.default(x, ...) : no 'nobs' method is available
# Error in nobs.default(x, ...) : no 'nobs' method is available
# Wald test
# 
# Model 1: ofp ~ .
# Model 2: ofp ~ . | hosp + numchron + privins + school + gender
#   Res.Df Df  Chisq Pr(>Chisq)
# 1   4389                     
# 2   4391 -2 0.1584     0.9238
# 
# **************************************************************************************
# This is very different to what was printed on page 18 of Journal Statistical Softwares 27(8), 2008.

# *************************************************************************************************
# 8. Zero-inflated Poisson regression
# *************************************************************************************************
rm(df.coef.count,df.coef.zero,df.sum)
fm_zip0 <- zeroinfl(ofp ~ ., data = dt, dist = "poisson")

# estimated lambda
est.mu          <- predict(fm_zip0)				# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda      <- fm_zip0$fitted				# the expected count from the model
est.zero.p      <- predict(fm_zip0, type = "prob")[,1]		# the probability to have zero count
est.zero.infl   <- predict(fm_zip0, type = "zero")		# the probability to have inflated zeros
est.count       <- predict(fm_zip0, type = "count")		# the expected count without the zero inflation

est.zip0   	<- cbind(ofp           = dt[,"ofp"],
                         mu            = est.mu,
                         lambda        = est.lambda,
                         est.zero.p    = est.zero.p,
                         est.zero.infl = est.zero.infl,
                         est.count     = est.count)
                    
estimation.zip0 <- rbind(sum1 = apply(est.zip0,2,sum),
                         sum0 = apply(est.zip0,2,function(x){sum(x==0)}),
                         est.zip0)

par(mfrow = c(2,2))
 hist(estimation.zip0[3:4408,"ofp"],nclass=100,xlab="ofp",ylab="frequency",main="[fm_zinp0]: distribution of observed count")
 hist(estimation.zip0[3:4408,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_zinp0]: distribution of expected count")
                    
 hist(estimation.zip0[3:4408,"est.zero.infl"],nclass=100,xlab="p.0infl",ylab="frequency",main="[fm_zinp0]:distribution of probability of inflated zero")
 plot(estimation.zip0[3:4408,"ofp"],estimation.zip0[3:4408,"mu"],type="p",pch=16,cex=0.5,xlab="ofp count",ylab="expected",main="[fm_zinp0]:model fitting (Zero-Inflated)")
 abline(a=0,b=1,col="red")
                                                   
                    
                    
summary(fm_zip0)


      df.coef.count  <- data.frame(summary(fm_zip0)$coefficients$count)
      df.coef.zero   <- data.frame(summary(fm_zip0)$coefficients$zero)
names(df.coef.count) <- c("Estimate","StdError","zValue","Pr(>|z|)")
names(df.coef.zero)  <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(df.null = fm_zip0$df.null,df.residual=fm_zip0$df.residual,loglik = fm_zip0$loglik)

cat(paste("\nZero-inflated Poisson \n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Zero-inflated Poisson (count) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.count,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Zero-inflated Poisson (zero) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.zero, file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Zero-inflated Poisson ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Zero-inflated Poisson regression Model is finished! ------------------------\n")





# 
# Call:
# zeroinfl(formula = ofp ~ ., data = dt, dist = "poisson")
# 
# Pearson residuals:
#     Min      1Q  Median      3Q     Max 
# -5.4092 -1.1579 -0.4769  0.5435 25.0380 
# 
# Count model coefficients (poisson with log link):
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      1.405812   0.024175  58.152  < 2e-16 ***
# hosp             0.159011   0.006060  26.239  < 2e-16 ***
# healthpoor       0.253454   0.017705  14.315  < 2e-16 ***
# healthexcellent -0.304134   0.031151  -9.763  < 2e-16 ***
# numchron         0.101836   0.004721  21.571  < 2e-16 ***
# gendermale      -0.062332   0.013054  -4.775 1.80e-06 ***
# school           0.019144   0.001873  10.221  < 2e-16 ***
# privinsyes       0.080557   0.017145   4.699 2.62e-06 ***
# 
# Zero-inflation model coefficients (binomial with logit link):
#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -0.08102    0.14233  -0.569 0.569219    
# hosp            -0.30330    0.09158  -3.312 0.000927 ***
# healthpoor       0.02166    0.16170   0.134 0.893431    
# healthexcellent  0.23786    0.14990   1.587 0.112550    
# numchron        -0.53117    0.04601 -11.545  < 2e-16 ***
# gendermale       0.41527    0.08919   4.656 3.22e-06 ***
# school          -0.05677    0.01223  -4.640 3.49e-06 ***
# privinsyes      -0.75294    0.10257  -7.341 2.12e-13 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Number of iterations in BFGS optimization: 24 
# Log-likelihood: -1.613e+04 on 16 Df
# 
# 
# 
# *************************************************************************************************
# 9. Zero-inflated Poisson regression without health
# *************************************************************************************************
rm(df.coef.count,df.coef.zero,df.sum)
fm_zip <- zeroinfl(ofp ~ hosp + numchron + privins + school + gender | hosp + numchron + privins + school + gender,data = dt, dist = "poisson")

# estimated lambda
est.mu          <- predict(fm_zip)				# mu is lambda which is the expected value: log(mu) = eta = linear combination
est.lambda      <- fm_zip$fitted				# the expected count from the model
est.zero.p      <- predict(fm_zip, type = "prob")[,1]		# the probability to have zero count
est.zero.infl   <- predict(fm_zip, type = "zero")		# the probability to have inflated zeros
est.count       <- predict(fm_zip, type = "count")		# the expected count without the zero inflation

est.zip   	<- cbind(ofp           = dt[,"ofp"],
                         mu            = est.mu,
                         lambda        = est.lambda,
                         est.zero.p    = est.zero.p,
                         est.zero.infl = est.zero.infl,
                         est.count     = est.count)
                    
estimation.zip <- rbind(sum1 = apply(est.zip,2,sum),
                          sum0 = apply(est.zip,2,function(x){sum(x==0)}),
                          est.zip)


par(mfrow = c(2,2))
 hist(estimation.zip[3:4408,"ofp"],nclass=100,xlab="ofp",ylab="frequency",main="[fm_zinp]: distribution of observed count")
 hist(estimation.zip[3:4408,"mu"], nclass=100,xlab="mu", ylab="frequency",main="[fm_zinp]: distribution of expected count")
                    
 hist(estimation.zip[3:4408,"est.zero.infl"],nclass=100,xlab="p.0infl",ylab="frequency",main="[fm_zinp]:distribution of probability of inflated zero")
 plot(estimation.zip[3:4408,"ofp"],estimation.zip[3:4408,"mu"],type="p",pch=16,cex=0.5,xlab="ofp count",ylab="expected",main="[fm_zinp]:model fitting (Zero-Inflated)")
 abline(a=0,b=1,col="red")
                                                  
                    
                    
summary(fm_zip)



      df.coef.count  <- data.frame(summary(fm_zip)$coefficients$count)
      df.coef.zero   <- data.frame(summary(fm_zip)$coefficients$zero)
names(df.coef.count) <- c("Estimate","StdError","zValue","Pr(>|z|)")
names(df.coef.zero)  <- c("Estimate","StdError","zValue","Pr(>|z|)")

        df.sum <- data.frame(df.null = fm_zip$df.null,df.residual=fm_zip$df.residual,loglik = fm_zip$loglik)

cat(paste("\nZero-inflated Poisson Without Health \n",sep=""),file=FL.RESL.OUT,append=TRUE)

cat(paste("Zero-inflated Poisson Without Health (count) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.count,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat(paste("Zero-inflated Poisson Without Health (zero) ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.coef.zero, file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)

cat(paste("Zero-inflated Poisson Without Health ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(df.sum,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Zero-inflated Poisson Without Health regression Model is finished! ------------------------\n")



# 
# Call:
# zeroinfl(formula = ofp ~ . | hosp + numchron + privins + school + gender, data = dt, dist = "poisson")
# 
# Pearson residuals:
#     Min      1Q  Median      3Q     Max 
# -5.4168 -1.1640 -0.4700  0.5461 25.4710 
# 
# Count model coefficients (poisson with log link):
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      1.405600   0.024179  58.134  < 2e-16 ***
# hosp             0.159014   0.006060  26.240  < 2e-16 ***
# healthpoor       0.253416   0.017706  14.313  < 2e-16 ***
# healthexcellent -0.307366   0.031265  -9.831  < 2e-16 ***
# numchron         0.101846   0.004721  21.573  < 2e-16 ***
# gendermale      -0.062352   0.013056  -4.776 1.79e-06 ***
# school           0.019169   0.001873  10.232  < 2e-16 ***
# privinsyes       0.080533   0.017147   4.697 2.65e-06 ***
# 
# Zero-inflation model coefficients (binomial with logit link):
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.05937    0.14040  -0.423 0.672392    
# hosp        -0.30669    0.09121  -3.363 0.000772 ***
# numchron    -0.53972    0.04419 -12.212  < 2e-16 ***
# privinsyes  -0.75373    0.10211  -7.381 1.57e-13 ***
# school      -0.05560    0.01218  -4.564 5.02e-06 ***
# gendermale   0.41806    0.08920   4.687 2.77e-06 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Number of iterations in BFGS optimization: 21 
# Log-likelihood: -1.614e+04 on 14 Df
# 



# put all models in a list
# In summary, the models are not too dierent with respect to their tted mean functions. The
fm <- list("ML-Pois" = fm_pois, "Quasi-Pois" = fm_qpois, "NB" = fm_nbin,"Hurdle-NB" = fm_hurdle, "ZINB" = fm_zinb,"ZIP" = fm_zip)
sapply(fm, function(x) coef(x)[1:8])

#                ML-Pois  Quasi-Pois          NB   Hurdle-NB        ZINB
# (Intercept)      1.02887420  1.02887420  0.92925658  1.19769892  1.19371555
# hosp             0.16479739  0.16479739  0.21777223  0.21189820  0.20147683
# healthpoor       0.24830697  0.24830697  0.30501303  0.31595757  0.28513277
# healthexcellent -0.36199320 -0.36199320 -0.34180660 -0.33186113 -0.31933918
# numchron         0.14663928  0.14663928  0.17491552  0.12642059  0.12899916
# gendermale      -0.11231992 -0.11231992 -0.12648813 -0.06831702 -0.08027732
# school           0.02614299  0.02614299  0.02681508  0.02069321  0.02142327
# privinsyes       0.20168688  0.20168688  0.22440187  0.10017164  0.12586475

# the associated estimated standard errors are very similar as well. The only exception are the model-based standard errors for the Poisson model, when treated
# as a fully specied model, which is obviously not appropriate for this data set.
std.error <- data.frame(cbind("ML-Pois" = sqrt(diag(vcov(fm_pois))),"Adj-Pois" = sqrt(diag(sandwich(fm_pois))),	# add pois and sandwich adjusted poisson
                                    sapply(fm[-1], function(x) sqrt(diag(vcov(x)))[1:8])))			# remove the pois SE which is the first one in the list.  Also note that we are extracting the first 8 coefficients


cat(paste("\nStandard Error of Various Models: ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(std.error,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Standard Error of Models! ------------------------\n")



# > std.error
#                     ML-Pois    Adj-Pois  Quasi-Pois          NB  Hurdle-NB        ZINB         ZIP
# (Intercept)     0.023784601 0.064529808 0.061593641 0.054591271 0.05897349 0.056660841 0.024178655
# hosp            0.005997367 0.021945186 0.015531043 0.020176492 0.02139606 0.020359727 0.006060009
# healthpoor      0.017844531 0.054021990 0.046210977 0.048510797 0.04805566 0.045092639 0.017705506
# healthexcellent 0.030303905 0.077448586 0.078476316 0.060923623 0.06609306 0.060404889 0.031264705
# numchron        0.004579677 0.012907865 0.011859732 0.012091749 0.01245231 0.011930513 0.004721027
# gendermale      0.012945146 0.035343487 0.033523316 0.031215523 0.03241561 0.031024027 0.013056404
# school          0.001843329 0.005084002 0.004773565 0.004393971 0.00453483 0.004357569 0.001873485
# privinsyes      0.016859826 0.043128006 0.043660942 0.039463744 0.04261858 0.041587616 0.017147025










                                
# The dierences become obvious if not only the mean but the full likelihood is considered: 
# note: The quasi-Poisson model and the sandwich-adjusted Poisson model are not associated with a fitted likelihood.
tmp <- data.frame(rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),					# useful usage of sapply
      Df     = sapply(fm, function(x) attr(logLik(x), "df")),
      AIC    = sapply(fm, function(x) round(AIC(x), digits = 0)),
      BIC    = sapply(fm, function(x) round(BIC(x), digits = 0))))
      
cat(paste("\nOther Diagnosis of Models: ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(tmp,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)
cat("------------------------ Other Diagnosis of Models! ------------------------\n")
      
#        ML-Pois Quasi-Pois     NB Hurdle-NB   ZINB    ZIP
# logLik  -17972         NA -12171    -12090 -12091 -16135
# Df           8          8      9        15     15     14
# AIC      35959         NA  24359     24210  24211  32298
# BIC      36010         NA  24417        NA     NA     NA
# 
# 




# notice that:
logLik(fm_pois)			# to get the model's log likelihood					# useful usage to get the logLik from a model
attr(logLik(fm_pois),"df")	# to get the df of the model




# Additionally, it is of interest how the zero counts are captured by the various models.
# Therefore, the observed zero counts are compared to the expected number of zero counts for
# the likelihood-based models:
# fitted(model) gives the fitted mean function such as lambda in the poisson distribution.  For each data point, one of such lambda will be estimated from the model
# dpois: density function, taking values between the valid range of the function say -infinity o infinity of the normal distribution
# ppois: cumulative distribution function, taking values between the valid range of the function say -infinity o infinity of the normal distribution
# qpois: quantile of a distribution, taking values between 0 and 1
number.zero  <- round(c("Obs" 		= sum(dt$ofp < 1),
			"ML-Pois" 	= sum(dpois(0, fitted(fm_pois))),				# the probability to get 0 with the lambda estimated from the model
			"NB" 		= sum(dnbinom(0, mu = fitted(fm_nbin), size = fm_nbin$theta)), 	# the probability to get 0 with the lambda and theta estimated from the model
			"NB-Hurdle" 	= sum(predict(fm_hurdle, type = "prob")[,1]),			# the probability to get 0 predicted 
			"ZINB" 		= sum(predict(fm_zinb, type = "prob")[,1]),			# the probability to get 0 predicted 
			"ZIP" 		= sum(predict(fm_zip, type = "prob")[,1])))			# the probability to get 0 predicted 
# 			
# number.zero
#      Obs   ML-Pois        NB NB-Hurdle      ZINB       ZIP 
#      683        47       608       683       709       682 INB 
	
# By construction, the expected number of zero counts in the hurdle model matches the observed number.

# predicted count
count.predict <- cbind("Obs" 		= dt$ofp,
			"ML-Pois" 	= predict(fm_hurdle,type = "prob")[,1],		# the probability to get 0 with the lambda estimated from the model
			"NB" 		= predict(fm_hurdle,type = "prob")[,1], 	# the probability to get 0 with the lambda and theta estimated from the model
			"NB-Hurdle" 	= predict(fm_hurdle,type = "prob")[,1],		# the probability to get 0 predicted 
			"ZINB" 		= predict(fm_zinb,  type = "prob")[,1],		# the probability to get 0 predicted 
			"ZIP" 		= predict(fm_zip,   type = "prob")[,1])		# the probability to get 0 predicted 


# summarize the coefficients for the count components
coef.count     <- rbind("ML-Pois" 	= fm_pois$coefficients,				# the probability to get 0 with the lambda estimated from the model
		  	"NB" 		= fm_nbin$coefficients, 			# the probability to get 0 with the lambda and theta estimated from the model
			"NB-Hurdle" 	= fm_hurdle$coefficients$count,			# the probability to get 0 predicted 
			"ZINB" 		= fm_zinb$coefficients$count,			# the probability to get 0 predicted 
			"ZIP" 		= fm_zip$coefficients$count)			# the probability to get 0 predicted 

#           (Intercept)      hosp healthpoor healthexcellent  numchron  gendermale     school privinsyes
# ML-Pois     1.0288742 0.1647974  0.2483070      -0.3619932 0.1466393 -0.11231992 0.02614299  0.2016869
# NB          0.9292566 0.2177722  0.3050130      -0.3418066 0.1749155 -0.12648813 0.02681508  0.2244019
# NB-Hurdle   1.1976989 0.2118982  0.3159576      -0.3318611 0.1264206 -0.06831702 0.02069321  0.1001716
# ZINB        1.1937156 0.2014768  0.2851328      -0.3193392 0.1289992 -0.08027732 0.02142327  0.1258647
# ZIP         1.4056000 0.1590135  0.2534164      -0.3073657 0.1018459 -0.06235219 0.01916943 0.08053267

# summarize the coefficients for the count components
coef.zero     <- rbind(	"NB-Hurdle" 	= fm_hurdle$coefficients$zero,			# the probability to get 0 predicted 
			"ZINB" 		= fm_zinb$coefficients$zero,			# the probability to get 0 predicted 
			"ZIP" 		= fm_zip$coefficients$zero)			# the probability to get 0 predicted 


#           (Intercept)       hosp   numchron privinsyes      school gendermale
# NB-Hurdle  0.01594017  0.3184345  0.5478325   0.745720  0.05707282 -0.4191478
# ZINB      -0.04683873 -0.8004650 -1.2478971  -1.175584 -0.08377747  0.6476602
# ZIP       -0.05936931 -0.3066871 -0.5397165 -0.7537324 -0.05559510  0.4180648
#  
# 

#
# the sum of the zero probability
#
p.zero <- data.frame(data.0  = sum(dt[,"ofp"]==0),
                     Poisson = estimation.Poisson[1,"est.zero.p"],
                     NBIN    = estimation.nbin[1,"est.zero.p"],
                     ZINB    = estimation.zinb[1,"est.zero.p"],
                     ZIP     = estimation.zip[1,"est.zero.p"])


cat(paste("\nZeros Estimated in prob: ,",sep=""),file=FL.RESL.OUT,append=TRUE)
write.table(p.zero,file=FL.RESL.OUT,sep=",",row.names=TRUE,col.names=TRUE,append=TRUE)


#
# save the data in R data format
#
save(dt2,dt,estimation.nbin,estimation.Poisson,estimation.QuasiPoisson,estimation.zinb,estimation.zinb0,estimation.zip,estimation.zip0,file=FL.Data.OBJ )



# How to get the esitmated values???????
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


