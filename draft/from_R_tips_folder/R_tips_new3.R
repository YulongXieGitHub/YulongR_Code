
FL.OBJ <- "../2b_prepData4PMF/Bayland/Data_Bayland_PMF4.Rdata"
load(FL.OBJ)
DAT <- DAT.PMF
ERR <- ERR.PMF
QA     <- ERR/DAT
QA.BL  <- QA
ERR.BL <- ERR
DAT.BL <- DAT


FL.OBJ <- "../2b_prepData4PMF/DeerPark/Data_DeerPark_PMF4.Rdata"
load(FL.OBJ)
DAT <- DAT.PMF
ERR <- ERR.PMF
QA     <- ERR/DAT
QA.DP  <- QA
ERR.DP <- ERR
DAT.DP <- DAT



QA   <- ERR/DAT
ncol <- dim(QA)[2]
nrow <- dim(QA)[1]

# find the maximum
index   <- which(QA==max(QA))
idx.col <- ceiling(index / nrow)
idx.row <- index - (idx.col-1)*nrow

MAX <- c(QA[idx.row,idx.col],DAT[idx.row,idx.col],ERR[idx.row,idx.col])


# find the minimum
index   <- which(QA==min(QA))
idx.col <- ceiling(index / nrow)
idx.row <- index - (idx.col-1)*nrow

MIN <- c(QA[idx.row,idx.col],DAT[idx.row,idx.col],ERR[idx.row,idx.col])