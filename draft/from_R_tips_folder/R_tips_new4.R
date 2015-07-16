# 
# Merge the columns of two data frames by rows based on common rows
#
# Spe 24, 2008
 A <- cbind(idx=seq(from=1,to=10),CO=rnorm(10),NOx=rnorm(10))
 B <- cbind(idx=seq(from=3,to=12),             NOx=rnorm(10),NOy=rnorm(10))


merge(A,B,by.x="idx",by.y="idx",all=TRUE)

#
# merge the two data frame by common columns
#
 A <- cbind(idx=seq(from=1,to=10),CO=rnorm(10),NOx=rnorm(10),NOy=rnorm(10))
 B <- cbind(idx=seq(from=3,to=12),             NOx=rnorm(10),NOy=rnorm(10),O3=rnorm(10))


merge(A,B,by.x=c("idx","NOx","NOy"),by.y=c("idx","NOx","NOy"),all.x=TRUE,all.y=TRUE)
