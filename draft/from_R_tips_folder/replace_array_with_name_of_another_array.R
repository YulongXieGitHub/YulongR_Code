# If we have a long vector which consists of values which have been defined in another array, use "match" to find the correspondency

# e.g., we have 
      NetWWR              <- c("N29","N48","N58","N72")	# the ne      tWWR of the 4 windows
names(NetWWR)             <- c(  20,   33,   40,   50)	# the apparent WWR of the 4 windows

# now we have a long list with the NetWWR values 
piece.rad.netww <- c(rep(NetWWR,10))
piece.rad.netww <- piece.rad.netww[sample(seq(1,length(piece.rad.netww)),replace=FALSE)]

# we want to find out the apparent WWR of this long list
index <- match(piece.rad.netww,NetWWR)	# retrun a vector of the positions of the "piece.rad.ww" element in the "NetWWR" vector
piece.wwr <- names(NetWWR)[index]	# get the correspond WWR of the netWWR used in Radiance

# confirm the correctness
setequal(names(piece.rad.netww),piece.wwr)