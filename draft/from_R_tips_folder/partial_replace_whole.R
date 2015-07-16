#
# use partial to represent the whole
#
myData.Wide.U[,"date"]       <- sub("\\(","",sub("(.*)\\s+(.*)","\\1",myData.Wide.U[,"date-time"]))
myData.Wide.U[,"time"]       <- sub("\\)","",sub("(.*)\\s+(.*)","\\2",myData.Wide.U[,"date-time"]))