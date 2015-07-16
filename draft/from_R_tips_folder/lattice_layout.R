# ---------------------------------------------------------------------------------------------------
# time series of pre-installation
# ---------------------------------------------------------------------------------------------------
FL.PDF <- paste(Path.out,paste("timeseries_pre.pdf",sep=""),sep="/")	# OUTPUT Time Series 
TI.PDF <- paste("timeseries_pre.pdf",sep="")
pdf(file = FL.PDF,title=TI.PDF,paper="a4r", width=0, height=0)
plot.obj <- xyplot(value ~ time.chron | as.factor(vari),data=data.pre.long,
		   xlab="date/time",ylab="",
		   type="l",lty=1,cex=0.5,col="red",
		   main=paste("pre-installation:"," (",str.title,")",sep=""),
		   scales=list(x=list(limits=xlim4plot_pre,at = xat4plot_pre),y="free"),
		   layout=c(0,12),
		   as.table=TRUE,
		   
		   panel = function(x,y,...) {
			   panel.xyplot(x, y,type="l",lty=1,cex=0.5,col="red")
			   panel.abline(h=0,lty=2,col="black")}
		   )		 
plot(plot.obj)  
dev.off()
