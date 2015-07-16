for (site in sites)
{
	# -------------------------------------------------------------------------------------------
	# Ozone data of current site
	# -------------------------------------------------------------------------------------------
	lab.O3 <- paste("O3",site,sep=".")
	inc.O3 <- paste("inc",lab.O3,sep=".")

	ymax <- max(data.1hr.runAvg[,lab.O3],na.rm=TRUE)*1.1
	ymax <- trunc(ceiling(ymax/10))*10
	
	#
	# plot the raw and one-hour running average of the O3 at the three sites
	#
	key.string <- list(title="day",space="right",columns=1,text=list(levels(as.factor(data.1hr.runAvg[,"day"]))),lines = list(lty=lty.array),col=col.array)
	
	plot.obj <- xyplot(data.1hr.runAvg[,lab.O3]~data.1hr.runAvg[,"hourInDay"],data=data.1hr.runAvg,
		       groups = day,
		       xlab = "CST (Hour)",ylab=expression("O"[{3}] (ppbv)),main=paste(site),
		       type = rep("l",30),
        	       lty  = lty.array,
        	       lwd  = lwd.array,
        	       pch  = pch.array,
		       col  = col.array,
		       cex  = cex.array,
		       ylim = c(0,ymax),
		       xlim = c(0,24),
		       xat  = c(0,2,4,6,8,10,12,14,16,18,20,24),
		       key  = key.string,
		 
		   panel = function(x,y,...) {
			   panel.xyplot(x, y,...)
			   panel.abline(h=c(120),lty=c(2),col=c("black"))}	# mark O3 levels (120 ppb) 

		       )
	plot(plot.obj)		      
}

