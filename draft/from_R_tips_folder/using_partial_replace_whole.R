
# use partial to replace the whole
#
# a <- "ASHRAE30pct_ApartmentHighRise_STD2004_El_Paso.dd.idf"
# sub("(.*)_ApartmentHighRise(.*)",          "\\1",a,perl=TRUE)

standards.array <- c("IECCplus.state_STD2012" "IECC.state_STD2009" )
standard.year   <- sub("(.*[^\\d+])(\\d+)(.*)","\\2",standards.array,perl=TRUE)	# the first part is everything without digits, the second parts is a digit string and the 3rd part is everything after that digit string
# we will get "2012" and "2009"

		    
# -------------------------------------------------------------------------------------------------
# time used for completing this script
# -------------------------------------------------------------------------------------------------
End.time  <- Sys.time()
Diff.time <- End.time - Start.time
Diff.time

cat(paste("\nCompare_idf_from_old_new_sizing.R is finished successfully at ",End.time,"!\n",sep=" "))
cat(paste("\nCompare_idf_from_old_new_sizing.R is finished successfully at ",End.time,"!\n",sep=" "),file=FL.LOG,append=TRUE)

cat(paste("\nProcessing time for [Compare_idf_from_old_new_sizing.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "))
cat(paste("\nProcessing time for [Compare_idf_from_old_new_sizing.R] is ",as.numeric(Diff.time, units="mins")," minutes\n",sep=" "),file=FL.LOG,append=TRUE)




# ---------------------
 myTmp2 <- c("EMS.DR_Demand....RunPeriod.","EMS.DR_Pre_Demand....RunPeriod.","EMS.DR_Post_Demand....RunPeriod.")
sub(".*\\.(\\D+_Demand)(.*)","\\1",names(myTmp2))
