getwd()
setwd()

RSiteSearch()


# install packages
update.packages()
install.packages(c("name1","name2",...,"name"),repo="http://cran.stat.ucla.edu",dep=TRUE)
install.packages(c("adapt","dse1","fSeries","fracdiff","foreign","graphics","Hmisc","lattice","lmtest","MASS","Matrix","MCMCpack","MNP","nlme","nls","nnet","quantreg","R.matlab","sandwich","sem",",survival","ts","tseries","xtable","zoo"),repo="http://cran.stat.ucla.edu",dep=TRUE)

# merge dataframes
OUT<-merge(b,E,by.x="var1",by.y="var2")

# edit dataframe
data.entry(nameofdataframe)
fix()
edit()

# multiple plots
split.screen()
layout()
par()  	[mfrow: two entries for number of rows and columns]
	[mar: 4 entries for margins at bottom, left top and right, default is c(5,4,4,2)+0.1]
par(mfrow=c(2,2),max=c(3,4,2,2)+.1)

	
# change settings	
op<-par(no.readonly=TRUE)	# save current setting
par(op)				# return current setting after plotting

# saving plots in png, jpg, eps, pdf and xfig format
png()
pdf()
jpg()
postscript()

in order for later editing, try [xaxt="n",yaxt="n",xlab="",ylab=""]

# adding Grrek Letters and Math Symbols to Plots  
# Note 1: use [substitue]  
# Note 2: to get equal sign in expression, one needs to use double equal signs
# Note 3: to mix test and symbols, use [paste] inside [substitue]
plot(x,y,main=substitute(y==Psi*z-sum(beta^gamma)),type="l")
text(3,40,substitute(Delta[K]==1))
text(0.6,20,substitute(Delta[K]==epsilon))

plot(density(x),main=substitute(paste("t-stat of ",beta[0])))

# statistics of Normal (mean=-inf:inf, sd=(0,inf), x=-inf:inf)
par(mfrow=c(2,2),mar=c(3,4,2,2)+.1)
x<-seq(from=-1000,by=1,to=1000)
curve(rnorm(x,mean=0,sd=1),main="Normal Random Numbers")
curve(dnorm(x,mean=0,sd=1),main="Normal PDF",xlim=c(-4,4))
curve(pnorm(x,mean=0,sd=1),main="Normal CDF",ylim=c(0,1),xlim=c(-4,4))
x<-seq(from=.0001,by=.01,to=0.9999)
curve(qnorm(x,mean=0,sd=1),main="number corrsponds to Normal CDF",xlim=c(0,1),ylim=c(-4,4))

# statistics of gamma (shape>0,scale>0,x=[0,infinity))
par(mfrow=c(2,2),mar=c(3,4,2,2)+.1)
x<-seq(from=-1000,by=1,to=1000)
curve(rgamma(x,shape=2,scale=2),main="gamma Random Numbers")
curve(dgamma(x,shape=2,scale=2),main="gamma PDF",xlim=c(0,20))
curve(pgamma(x,shape=2,scale=2),main="gamma CDF",ylim=c(0,1),xlim=c(0,20))
x<-seq(from=.0001,by=.01,to=0.9999)
curve(qgamma(x,shape=2,scale=2),main="number corrsponds to gamma CDF",xlim=c(0,1),ylim=c(0,20))


# statistics of beta (shape1>0,shape2>0,x=[0,1])
par(mfrow=c(2,2),mar=c(3,4,2,2)+.1)
x<-seq(from=-1000,by=1,to=1000)
curve(rbeta(x,shape1=2,shape2=2,ncp=0),main="beta Random Numbers")
curve(dbeta(x,shape1=2,shape2=2,ncp=0),main="beta PDF",xlim=c(0,1))
curve(pbeta(x,shape1=2,shape2=2,ncp=0),main="beta CDF",ylim=c(0,1),xlim=c(0,1))
x<-seq(from=.0001,by=.01,to=0.9999)
curve(qbeta(x,shape1=2,shape2=2,ncp=0),main="number corrsponds to beta CDF",xlim=c(0,1),ylim=c(0,1))

# statistics of pois (k={0,1,2,discrete),lambda=0:infinity)
par(mfrow=c(2,2),mar=c(3,4,2,2)+.1)
x<-seq(from=0,by=1,to=1000)
curve(rpois(x,lambda=10),main="pois Random Numbers")
curve(dpois(x,lambda=10),main="pois PDF",xlim=c(0,20))
curve(ppois(x,lambda=10),main="pois CDF",ylim=c(0,1),xlim=c(0,20))
x<-seq(from=.0001,by=.01,to=0.9999)
curve(qpois(x,shape1=2,shape2=2,ncp=0),main="number corrsponds to pois CDF",xlim=c(0,20),ylim=c(0,1))

# =============================== use function form ======================================================
# statistics of Normal (mean=-inf:inf, sd=(0,inf), x=-inf:inf)
par(mfrow=c(2,2),mar=c(3,4,2,2)+.1)
x<-seq(from=-4,by=.01,to=4)
q<-x
p<-seq(from=0.001,by=0.001,to=0.999)
n<-1000
plot(rnorm(n,mean=0,sd=1),main="Normal Random Numbers:N(0,1)",type="l")
plot(x,dnorm(x,mean=0,sd=1),main="Normal PDF:N(0,1)",xlim=c(-4,4),type="l")
plot(q,pnorm(q,mean=0,sd=1),main="Normal CDF:N(0,1)",ylim=c(0,1),xlim=c(-4,4),type="l")
plot(p,qnorm(p,mean=0,sd=1),main="number corrsponds to Normal CDF:N(0,1)",xlim=c(0,1),ylim=c(-4,4),type="l")

# statistics of gamma (shape>0,scale>0,x=[0,infinity))
par(mfrow=c(2,2),mar=c(3,4,2,2)+.1)
x<-seq(from=0,by=.01,to=20)	# continuous
q<-x
p<-seq(from=0.001,by=0.001,to=0.999)
n<-1000
plot(rgamma(n,shape=2,scale=2),main="gamma Random Numbers: G(2,2)",type="l")
plot(x,dgamma(x,shape=2,scale=2),main="gamma PDF: G(2,2)",xlim=c(0,20),type="l")
plot(q,pgamma(q,shape=2,scale=2),main="gamma CDF: G(2,2)",ylim=c(0,1),xlim=c(0,20),type="l")
plot(p,qgamma(p,shape=2,scale=2),main="number corrsponds to gamma CDF: G(2,2)",xlim=c(0,1),ylim=c(0,20),type="l")



# statistics of beta (shape1>0,shape2>0,ncp=0,,x=[0,1])
par(mfrow=c(2,2),mar=c(3,4,2,2)+.1)
x<-seq(from=0,by=.001,to=1)	# continuous
q<-x
p<-seq(from=0.001,by=0.001,to=0.999)
n<-1000
plot(rbeta(n,shape1=2,shape2=2,ncp=0),main="beta Random Numbers: B(2,2)",type="l")
plot(x,dbeta(x,shape1=2,shape2=2,ncp=0),main="beta PDF: B(2,2)",xlim=c(0,1),type="l")
plot(q,pbeta(q,shape1=2,shape2=2,ncp=0),main="beta CDF: B(2,2)",ylim=c(0,1),xlim=c(0,1),type="l")
plot(p,qbeta(p,shape1=2,shape2=2,ncp=0),main="number corrsponds to beta CDF: B(2,2)",xlim=c(0,1),ylim=c(0,1),type="l")


# statistics of Poisson (k={0,1,2,discrete),lambda=0:infinity)
par(mfrow=c(2,2),mar=c(3,4,2,2)+.1)
x<-seq(from=0,by=1,to=20)	# discrete
q<-x
p<-seq(from=0.001,by=0.001,to=0.999)
n<-1000
plot(rpois(n,lambda=10),main="Poisson Random Numbers: P(10)",type="l")
plot(x,dpois(x,lambda=10),main="Poisson PDF: P(10)",xlim=c(0,20),type="l")
plot(q,ppois(q,lambda=10),main="Poisson CDF: P(10)",ylim=c(0,1),xlim=c(0,20),type="l")
plot(p,qpois(p,lambda=10),main="number corrsponds to pois CDF: P(10)",xlim=c(0,1),ylim=c(0,20),type="l")


# statistics ofChi Sqaure (k=[0:infinity),df=0:infinity)
par(mfrow=c(2,2),mar=c(3,4,2,2)+.1)
x<-seq(from=0,by=.001,to=20)	# continuous
q<-x
p<-seq(from=0.001,by=0.001,to=0.999)
n<-1000
plot(rchisq(n,df=5,ncp=0),main="chisquare Random Numbers: X(5)",type="l")
plot(x,dchisq(x,df=5,ncp=0),main="Chisquare PDF: X(5)",xlim=c(0,20),type="l")
plot(q,pchisq(q,df=5,ncp=0),main="Chisquare CDF: X(5)",ylim=c(0,1),xlim=c(0,20),type="l")
plot(p,qchisq(p,df=5,ncp=0),main="number corrsponds to chisq CDF: X(5)",xlim=c(0,1),ylim=c(0,20),type="l")


# statistics of binomial (n>=0 integer, pron=0-1)
par(mfrow=c(2,2),mar=c(3,4,2,2)+.1)
x<-seq(from=0,by=1,to=30)	# discrete
q<-x
p<-seq(from=0.001,by=0.001,to=0.999)
n<-1000
plot(rbinom(n,size=20,prob=0.7),main="binomial Random Numbers: B(20,0.7)",type="l")
plot(x,dbinom(x,size=20,prob=0.7),main="binomial PDF: B(20,0.7)",xlim=c(0,30),type="l")
plot(q,pbinom(q,size=20,prob=0.7),main="binomial CDF: B(20,0.7)",ylim=c(0,1),xlim=c(0,30),type="l")
plot(p,qbinom(p,size=20,prob=0.7),main="number corrsponds to binom CDF: B(20,0.7)",xlim=c(0,1),ylim=c(0,30),type="l")


# P values
1-pf(3.6,4,43)=0.01284459
Reject null hypothesis if the p-value is less than the alpha value
if alpha = 1%, fail to reject
if alpha = 5%, reject
Note:  if doing two tailed test the p value should be multiplied by two.

# the one and two-tailed t-test  of a t statistics of 2.8 with 21 dof are:
1-pt(2.8,21) = 0.005364828 for one tailed test
2*(1-pt(2.8,21)) = 0.01072966 for two tailed t test

# TIME FUNCTION
system.time()
proc.time()

# options
options(digits=10) # change the number of significant digits from the default 7 to 10 for output


# initialize
A<-array(0,dim=c(500,1))
x<-rnorm(25,mean=2,sd=1)
for (i in 1:500)
{
	y<-rnorm(25,mean=(3*x+2),sd=1)
	beta<-lm(y~x)
	A[i]<-beta$coef[2]
}
Abar<-mean(A)
varA<-var(A)

# The Haar Wavelet
haar<-function(x)
{
	Y<-X*0
	for(i in 1:nrow(y))
	{
		if(x[i]<0 && x[i]>-1)
		{	
			y[i]=-1/sqrt(2)
		}
		else if (x[i]>0 && x[i]<1)
		{
			y[i]=1/sqrt(2)
		}
	}
	y	
}

# MLE
mloglik <- function(beta,Y,L,K)
{
	n<-length(Y)
	sum((log(Y)-beta[1]-beta[2]*log(L)-beta[3]*log(K))^2)/(2*beta[4]^2)+n/2*log(2*pi)+n*log(beta[4])
}
mlem<-nlm(mloglik,c(1,0.75,0.25,0.03),Y=Y,L=L,K=K)


# retain non-missing element
y <- x[!is.na(x)]
y <- x[!is.na(x) & x>0]	# x both non-na and >0
x[is.na(x)] <- 0	# replacing missing values with 0
x[x<0] <- -x[x<0]	# absolute values

#
unclass

