	par(mar=c(5, 4, 4, 5)) 
	layout(rbind(c(1,1,1),
	             c(0,2,0)),
	             respect=rbind(FALSE,TRUE))	
	plot(data.merged.wide[,"Date.chron"],as.numeric(data.merged.wide[,name2]),
	     type="l",lty=1,lwd=0.15,col="red",col.lab="red",
	     xlab="date/time(CST)",
	     ylab=paste("Conc(ppbv) PTRMS ",name2,sep=""),
	     main=paste("(DeerPark)  Corr coef of ",name1," and ",chem2,"(",name2,") is ",cor.pearson,"|",cor.spearman,sep=""))
	par(new=TRUE)
	plot(data.merged.wide[,"Date.chron"],as.numeric(data.merged.wide[,name1]),
	     type="l",lty=1,lwd=0.15,col="blue",col.lab="blue",
	     axes=FALSE,bty="n",
	     xlab="",ylab="")
	axis(side=4,at=pretty(range(as.numeric(data.merged.wide[,name1]),na.rm=TRUE)))
	mtext(col="blue",paste("Conc(ppbv) autoGC",name1,sep=""),side=4,line=2)

	plot(as.numeric(data.merged.wide[,name1]),as.numeric(data.merged.wide[,name2]),
	     type="p",pch=".",col="black",cex=3,
	     pty="s",
	     ylab=paste(chem2,"(",name2,") PTRMS",sep=""),xlab=paste(name1," autoGC",sep=""),
	     main=paste("(DeerPark)  scatter plot of ",name1," vs ",chem2,"(",name2,")",sep=""))
