


FL.OBJ <- "C:/YuLong_Projects/FY2012_MORiver/DataAnalysis/12_Prep_Subset_Data/12_Prep_Subset_Data.Rdata"
load(FL.OBJ)


# -------------------
# SY
# -------------------
A <- myData.Lower_GN
AA <- A[,c("binary","Pal_cnt","CPUE.calc","SY")]
tapply(AA[,"CPUE.calc"],AA[,"SY"],mean,na.rm=TRUE)
# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

B1 <- tapply(AA[,"binary"],AA[,"SY"],length)
B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"SY"],sum)
SY_0_rate <- B2/B1
#     2006      2007      2008      2009      2010      2011 
# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

SY_all_mean <- tapply(AA[,"CPUE.calc"],AA[,"SY"],mean,na.rm=TRUE)
#       2006       2007       2008       2009       2010       2011 
# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

SY_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,"CPUE.calc"],AA[AA[,"Pal_cnt"]>0,"SY"],mean,na.rm=TRUE)
#     2006      2007      2008      2009      2010      2011 
# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
SY_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,"CPUE.calc"],AA[AA[,"Pal_cnt"]>0,"SY"],median,na.rm=TRUE)
#      2006      2007      2008      2009      2010      2011 
# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 

# -------------------
# new_meso
# -------------------
A <- myData.Lower_GN
AA <- A[,c("binary","Pal_cnt","CPUE.calc","new_meso")]
tapply(AA[,"CPUE.calc"],AA[,"new_meso"],mean,na.rm=TRUE)
# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

B1 <- tapply(AA[,"binary"],AA[,"new_meso"],length)
B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"new_meso"],sum)
new_meso_0_rate <- B2/B1
#     2006      2007      2008      2009      2010      2011 
# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

new_meso_all_mean <- tapply(AA[,"CPUE.calc"],AA[,"new_meso"],mean,na.rm=TRUE)
#       2006       2007       2008       2009       2010       2011 
# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

new_meso_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,"CPUE.calc"],AA[AA[,"Pal_cnt"]>0,"new_meso"],mean,na.rm=TRUE)
#     2006      2007      2008      2009      2010      2011 
# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
new_meso_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,"CPUE.calc"],AA[AA[,"Pal_cnt"]>0,"new_meso"],median,na.rm=TRUE)
#      2006      2007      2008      2009      2010      2011 
# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 



# -------------------
# new_lith
# -------------------
A <- myData.Lower_GN
AA <- A[,c("binary","Pal_cnt","CPUE.calc","new_lith")]
tapply(AA[,"CPUE.calc"],AA[,"new_lith"],mean,na.rm=TRUE)
# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

B1 <- tapply(AA[,"binary"],AA[,"new_lith"],length)
B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"new_lith"],sum)
new_lith_0_rate <- B2/B1
#     2006      2007      2008      2009      2010      2011 
# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

new_lith_all_mean <- tapply(AA[,"CPUE.calc"],AA[,"new_lith"],mean,na.rm=TRUE)
#       2006       2007       2008       2009       2010       2011 
# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

new_lith_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,"CPUE.calc"],AA[AA[,"Pal_cnt"]>0,"new_lith"],mean,na.rm=TRUE)
#     2006      2007      2008      2009      2010      2011 
# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
new_lith_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,"CPUE.calc"],AA[AA[,"Pal_cnt"]>0,"new_lith"],median,na.rm=TRUE)
#      2006      2007      2008      2009      2010      2011 
# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 



# -------------------
# new_NFHAP
# -------------------
A <- myData.Lower_GN
AA <- A[,c("binary","Pal_cnt","CPUE.calc","new_NFHAP")]
tapply(AA[,"CPUE.calc"],AA[,"new_NFHAP"],mean,na.rm=TRUE)
# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

B1 <- tapply(AA[,"binary"],AA[,"new_NFHAP"],length)
B2 <- tapply(as.numeric(AA[AA[,"Pal_cnt"]==0,"binary"]),AA[AA[,"Pal_cnt"]==0,"new_NFHAP"],sum)
new_NFHAP_0_rate <- B2/B1
#     2006      2007      2008      2009      2010      2011 
# 0.9677419 0.9679359 0.9181495 0.9448732 0.9403553 0.9243176 

new_NFHAP_all_mean <- tapply(AA[,"CPUE.calc"],AA[,"new_NFHAP"],mean,na.rm=TRUE)
#       2006       2007       2008       2009       2010       2011 
# 0.02202209 0.02147676 0.05571050 0.04402713 0.04111331 0.05542646 

new_NFHAP_presence_mean <- tapply(AA[AA[,"Pal_cnt"]>0,"CPUE.calc"],AA[AA[,"Pal_cnt"]>0,"new_NFHAP"],mean,na.rm=TRUE)
#     2006      2007      2008      2009      2010      2011 
# 0.6826848 0.6698065 0.6806369 0.7986521 0.6893040 0.7323562 
new_NFHAP_presence_median <- tapply(AA[AA[,"Pal_cnt"]>0,"CPUE.calc"],AA[AA[,"Pal_cnt"]>0,"new_NFHAP"],median,na.rm=TRUE)
#      2006      2007      2008      2009      2010      2011 
# 0.6009014 0.6088305 0.6147541 0.6110005 0.6113092 0.5925926 
