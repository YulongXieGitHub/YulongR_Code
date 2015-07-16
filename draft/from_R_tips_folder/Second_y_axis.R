
# 
# 1. load libraries
# 
library(lattice)
library(chron)	
library(gplots)

		par(mar=c(5, 4, 4, 5)) 
		layout(rbind(c(1,1,1),
			     c(0,2,0)),
			     respect=rbind(FALSE,TRUE))	

		plot(data.merged.wide[,"CST.chron"],data.merged.wide[,name1],
		     type="l",lty=1,lwd=0.15,col="red",col.lab="red",
		     xlab="date/time (CST)",
		     ylab=paste("Conc(ppbv) ",name1,sep=""),
		     main=paste("(",site,")  Corr coef of ",name1," and ",chem2,"(",name2,") is ",cor.pearson,"|",cor.spearman,sep=""))
		par(new=TRUE)
		plot(data.merged.wide[,"CST.chron"],data.merged.wide[,name2],
		     type="l",lty=1,lwd=0.15,col="blue",col.lab="blue",
		     axes=FALSE,bty="n",
		     xlab="",ylab="")
		axis(side=4,at=pretty(range(data.merged.wide[,name2],na.rm=TRUE)))
		mtext(col="blue",paste("Conc(ppbv) ",chem2,"(",name2,")",sep=""),side=4,line=3)
		
		plot(data.merged.wide[,name1],data.merged.wide[,name2],
		     type="p",pch=".",col="red",cex=4,
		     pty="s",
		     xlab=name1,ylab=paste(chem2,"(",name2,")",sep=""),
		     main=paste("(",site,")  scatter plot of ",name1," vs ",chem2,"(",name2,")",sep=""))
		
