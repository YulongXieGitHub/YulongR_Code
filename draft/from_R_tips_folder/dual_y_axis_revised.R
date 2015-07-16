myData.RTU1 <- myData.raw.this[myData.raw.this[,"meter.name"] == "RTU Feed B14",var.list]
myData.RTU2 <- myData.raw.this[myData.raw.this[,"meter.name"] == "RTU Feed B16",var.list]
myData.RTU3 <- myData.raw.this[myData.raw.this[,"meter.name"] == "RTU Feed B18",var.list]
myData.Tout <- myData.raw.this[myData.raw.this[,"meter.name"] == "Outdoor Air Temperature",var.list]
RTU.lim <- c(0,max(max(max(myData.RTU1[,"value.raw"])),max(max(myData.RTU2[,"value.raw"])),max(max(myData.RTU3[,"value.raw"]))))


plot(myData.RTU1[,"hour.day"],myData.RTU1[,"value.raw"],
     type = "l",lty=1,lwd=1,col="red",col.lab="black",
     xlab = "hour of the day",
     ylim = RTU.lim,
     xlim = c(0,24),
     ylab = "A",
     main = paste("RTU (",this.date,")",sep=""))
lines(myData.RTU2[,"hour.day"],myData.RTU2[,"value.raw"],col="blue", lty=1,lwd=1)
lines(myData.RTU3[,"hour.day"],myData.RTU3[,"value.raw"],col="green",lty=1,lwd=1)
abline(v=seq(0,24),lty=2,col="grey")

par(new=TRUE)
plot(myData.Tout[,"hour.day"],myData.Tout[,"value.raw"],
     type="l",lty=1,lwd=1,col="black",col.lab="black",
     axes=FALSE,bty="n",
     xlab="",ylab="")
axis(side=4,at=pretty(range(myData.Tout[,"value.raw"],na.rm=TRUE)))
mtext(col="black",paste("Outdoor Air Temperature (C)",sep=""),side=4,line=3)
