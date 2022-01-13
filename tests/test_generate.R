# This is a test script for RGENERATE::generate function
# 
# Author: Emanuele Cordano
###############################################################################
rm(list=ls())

## TESTING R CODE: 
library(testthat)
context("Verfiy RGENERATE::generate example output")

library(RGENERATE)


write_test_outcomes=FALSE
##test_outcomes=!write_test_outcomes

seed = 122
set.seed(seed)
NSTEP <- 1000
x <- rnorm(NSTEP)
y <- x+rnorm(NSTEP)
z <- c(rnorm(1),y[-1]+rnorm(NSTEP-1))
df <- data.frame(x=x,y=y,z=z)
var <- VAR(df,type="none")
gg <- generate(var,n=20) 
if (write_test_outcomes)  saveRDS(gg,file="/home/ecor/local/rpackages/rendena100/RGENERATE/inst/outcomes/gg.rds")
ggo <- readRDS(system.file("outcomes/gg.rds",package="RGENERATE")) 

##ggo <- data.frame(x=1:10,y=0,z=0)
##
test_that(desc="Testing generate.varest",code=expect_equal(gg,ggo, tolerance = .002, scale = 1))
##

##stop("QUI")
cov <- cov(gg)
set.seed(seed)
ggg <- generate(FUN=rnorm,n=NSTEP,cov=cov)
if (write_test_outcomes)  saveRDS(ggg,file="/home/ecor/local/rpackages/rendena100/RGENERATE/inst/outcomes/ggg.rds")
gggo <- readRDS(system.file("outcomes/ggg.rds",package="RGENERATE")) 

test_that(desc="Testing generate.default",code=expect_equal(ggg,gggo, tolerance = .002, scale = 1))

##test_that(desc="Testing generate.varest",code=expect_equal(test,test0, tolerance = .002, scale = 1))
##stop("QUI")

library(RMAWGEN)
####
exogen <- as.data.frame(x+5)
set.seed(seed)
gpcavar <- getVARmodel(data=df,suffix=NULL,p=3,n_GPCA_iteration=5,
                       n_GPCA_iteration_residuals=5,exogen=exogen)
gpcavar <- readRDS(system.file("outcomes/gpcavar.rds",package="RGENERATE")) ## gpcavar may differ in different machines! (This must be investigated!)
gpcagg <- generate(gpcavar,n=20,exogen=exogen) 

####
if (write_test_outcomes)  saveRDS(gpcavar,file="/home/ecor/local/rpackages/rendena100/RGENERATE/inst/outcomes/gpcavar.rds")
gpcavaro <- readRDS(system.file("outcomes/gpcavar.rds",package="RGENERATE")) 
test_that(desc="Testing getVARMODEL output (gpcavar)",code=expect_equal(gpcavar,gpcavaro, tolerance = .002, scale = 1))

if (write_test_outcomes)  saveRDS(gpcagg,file="/home/ecor/local/rpackages/rendena100/RGENERATE/inst/outcomes/gpcagg.rds")
gpcaggo <- readRDS(system.file("outcomes/gpcagg.rds",package="RGENERATE")) 

test_that(desc="Testing generate.GPCAvarest2",code=expect_equal(gpcagg,gpcaggo, tolerance = .002, scale = 1))

####
## Generate an auto-regrassive time-series with a generic matrix 

A <- diag(c(1,-1,1))
set.seed(seed)
mgg <- generate(A,n=100)

if (write_test_outcomes)  saveRDS(mgg,file="/home/ecor/local/rpackages/rendena100/RGENERATE/inst/outcomes/mgg.rds")
mggo <- readRDS(system.file("outcomes/mgg.rds",package="RGENERATE"))

test_that(desc="Testing generate.matrix",code=expect_equal(mggo,mgg, tolerance = .002, scale = 1))


### Gap Filling Examples
# 
# #### Gap filling with GPCAvarest (2 columns with NAs)
# dfobs <- df
# dfobs[20:30,] <- NA 
# n <- nrow(df)
# set.seed(seed)
# dffill <- generate(gpcavar,n=n,exogen=exogen,gap.filling=dfobs,names=names(dfobs)) 
# 
# qqplot(dfobs$y,dffill$y)
# abline(0,1)
# 
# if (write_test_outcomes)  saveRDS(dffill,file="/home/ecor/local/rpackages/rendena100/RGENERATE/inst/outcomes/dffill.rds")
# dffillo <- readRDS(system.file("outcomes/dffill.rds",package="RGENERATE"))
# 
# test_that(desc="Testing gap filling with generate.GPCAvarest (2 columns with NAs) ",code=expect_equal(dffillo,dffill, tolerance = .002, scale = 1))
# 

#### Gap filling with matrix 

mgg_n <- mgg
mgg_n[20:30,2] <- NA 
set.seed(seed)
mgg_nfill <- generate(A,gap.filling=mgg_n)


print(mgg_n[1:31,])
print(mgg_nfill[1:31,])

if (write_test_outcomes)  saveRDS(mgg_nfill,file="/home/ecor/local/rpackages/rendena100/RGENERATE/inst/outcomes/mgg_nfill.rds")
mgg_nfillo <- readRDS(system.file("outcomes/mgg_nfill.rds",package="RGENERATE"))

test_that(desc="Testing gap filling with generate.matrix (autoregression)",code=expect_equal(mgg_nfillo,mgg_nfill, tolerance = .002, scale = 1))




# #### Gap filling with GPCAvarest (1 column with NAs)
# dfobs2 <- df
# dfobs2[20:30,2] <- NA
# n <- nrow(df)
# set.seed(seed)
# dffill2 <- generate(gpcavar,n=n,exogen=exogen,gap.filling=dfobs2,names=names(dfobs2)) 
# 
# qqplot(dfobs2$y,dffill2$y)
# abline(0,1)
# 
# if (write_test_outcomes)  saveRDS(dffill2,file="/home/ecor/local/rpackages/rendena100/RGENERATE/inst/outcomes/dffill2.rds")
# dffill2o <- readRDS(system.file("outcomes/dffill2.rds",package="RGENERATE"))
# 
# test_that(desc="Testing gap filling with generate.GPCAvarest (1 column  with NAs)",code=expect_equal(dffill2o,dffill2, tolerance = .002, scale = 1))
# 



### generation with 'generetion.matrix' 
### and matrix 'x' is a covariance matrix 

covariance <- array(0.5,c(3,3))

diag(covariance) <- 1

set.seed(seed)
ngns <- 1000
gg1 <- generate(FUN=rnorm,n=ngns,cov=covariance)
set.seed(seed)
gg2 <- generate(covariance,type="covariance",n=ngns)

if (write_test_outcomes)  saveRDS(gg1,file="/home/ecor/local/rpackages/rendena100/RGENERATE/inst/outcomes/gg1.rds")
gg1o <- readRDS(system.file("outcomes/gg1.rds",package="RGENERATE"))

test_that(desc="Testing generate.matrix (autoregression) (1)",code=expect_equal(gg1o,gg1, tolerance = .002, scale = 1))
test_that(desc="Testing generate.matrix (autoregression) (2)",code=expect_equal(gg2,gg1, tolerance = .002, scale = 1))


## generate with a list of covariance matrix 
ndim <- 5
dim <- c(ndim,ndim)
CS1 <- array(0.3,dim)
CS2 <- array(0.5,dim)
CS3 <- array(0.7,dim)
CS4 <- array(0.1,dim)

diag(CS1) <- 1
diag(CS2) <- 1
diag(CS3) <- 1
diag(CS4) <- 1

list <- list(CS1=CS1,CS2=CS2,CS3=CS3,CS4=CS4)

series <- rep(1:4,times=4,each=100)
series <- sprintf("CS%d",series)
names_A <- sprintf("A%d",1:ndim)
set.seed(seed)
ggs <- generate(list,factor.series=series,FUN=rnorm,type="covariance",names=names_A)


####


#####
ggs_CS1 <- ggs[series=="CS1",]
cov(ggs_CS1)

ggs_CS3 <- ggs[series=="CS3",] 
cov(ggs_CS3)

if (write_test_outcomes)  saveRDS(ggs,file="/home/ecor/local/rpackages/rendena100/RGENERATE/inst/outcomes/ggs.rds")
ggso <- readRDS(system.file("outcomes/ggs.rds",package="RGENERATE"))

test_that(desc="Testing generate.list (covariance)",code=expect_equal(ggso,ggs, tolerance = .002, scale = 1))

