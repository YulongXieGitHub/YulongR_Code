# Week1,Week2,Week3,Week4,Week5,Week6,Week7,Week8
# Sun,11,10,12,15,8,24,14,11
# Mon,23,28,27,22,23,18,28,20
# Tues,18,21,27,23,27,28,25,19
# Wed,51,45,32,19,15,37,34,32
# Thurs,31,26,15,17,23,19,21,22
# Fri,21,18,13,20,17,20,13,11
# Sat,6,12,10,14,7,10,17,5
# 
# 
w      <- read.table("jim-albert-bayes-and-r.csv",row.names=1,header=TRUE,sep=",")
day    <- factor(rep(1:7,8),labels=row.names(w))
week   <- factor(rep(1:8,each=7),labels=dimnames(w)[[2]])
count  <- stack(w)$values

# keep "day" and "week" as categoryical variables
myData1<- data.frame(cbind(count,I(day),I(week)))	# use I() to preserve the factor or string when putting in the data frame
model1  <- glm(count~day+week,data = myData1,family=poisson)

# keep "day" and "week" as numerical variable
myData2<- data.frame(cbind(count,day,week))		
model2  <- glm(count~day+week,data = myData2,family=poisson)

# define a function
loglinearpost=function (beta, d)
{
	X=model.matrix(~day+week,data=d)
	beta=as.vector(beta)
	sum(dpois(d$count,lambda=exp(X%*%beta),log=TRUE))
}

# MCMC
library(LearnBayes)
d=list(day=day, week=week, count=count)
fit=laplace(loglinearpost,c(2.7,rep(0,13)),d)
proposal=list(var=fit$var,scale=0.7)
start=fit$mode
my.mcmc.fit=rwmetrop(loglinearpost,proposal,start,10000,d)

# Brugs
library(arm)
library(BRugs)

model
{
	for (i in 1:n)
	{ 
		count[i] ~ dpois(lam[i])
		log(lam[i]) <- beta[1] + X[i,2]*beta[2] + X[i,3]*beta[3] + X[i,4]*beta[4] + X[i,5]*beta[5] + X[i,6]*beta[6] + X[i,7]*beta[7] + X[i,8]*beta[8] + X[i,9]*beta[9] + X[i,10]*beta[10] + X[i,11]*beta[11] + X[i,12]*beta[12] + X[i,13]*beta[13] + X[i,14]*beta[14]
	}
	for (j in 1:14)
	{ 
		beta[j] ~ dnorm(0, 0.001)}
	} 	
	n=56
	web.data = list("n","count","X")
	web.parameters = c("beta")
	web.inits = function(){
	list(beta=fit$coef)
}
web.fit = bugs(web.data, web.inits, web.parameters,"webhits.bug", n.chains=1, n.iter=10000, debug=TRUE,n.burnin=1000, program = "openbugs")

summary(mcmc(web.fit$sims.matrix))
