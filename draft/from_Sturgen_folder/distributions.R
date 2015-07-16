rm(list = ls(all = TRUE))


# setup start date and time
start_time <- date();
Start.time <- Sys.time()
set.seed(12345, kind = NULL)	# set seed of random number

# close all devices which are currently open
device.list <- dev.list()
if (length(device.list) != 0){for (device.this in device.list){dev.off(device.this)}}

# -----------------------------------------------------------------------------------------------
# Beta Distribution
# -----------------------------------------------------------------------------------------------
a <- c(0.001,0.5,1,2,5,10)
b <- c(0.001,0.5,1,2,5,10)
no.a <- length(a)
no.b <- no.a


par(mfrow = c(no.a,no.b),mar=c(2,2,2,2),mgp=c(1.0,0.5,0))
for (idx.a in a)
{
	for (idx.b in b)
	{
		theta <- seq(from=0.001,to=0.999,by=0.001)
		p_theta <- dbeta(theta,idx.a,idx.b)
		
		plot(theta,p_theta,type="l",col="red",xlab=bquote(theta),ylab=bquote(p(theta)))
	}
}


# -----------------------------------------------------------------------------------------------
# Gamma Distribution
# -----------------------------------------------------------------------------------------------
shape <- c(0.001,1,5,10,50)
scale <- c(0.001,1,5,10,50)
no.shape <- length(shape)
no.scale <- no.shape


par(mfrow = c(no.shape,no.scale),mar=c(2,2,2,2),mgp=c(1.0,0.5,0))
for (idx.shape in shape)
{
	for (idx.scale in scale)
	{
		x <- seq(from=0,to=20,by=0.01)
		p_x <- dgamma(x,idx.shape,idx.scale)
		
		plot(x,p_x,type="l",col="blue",xlab=bquote(x),ylab=bquote(p(x)))
	}
}
