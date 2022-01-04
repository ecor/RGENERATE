## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----package,echo=TRUE--------------------------------------------------------

rm(list=ls())

library(RMAWGEN)
library(RGENERATE)
library(magrittr)


## ----dataset,echo=TRUE--------------------------------------------------------
data(trentino)
str(TEMPERATURE_MAX)
str(TEMPERATURE_MIN)


## ----refperiod,echo=TRUE------------------------------------------------------
year_min <- 1978
year_max <- 2007
origin <- sprintf("%04d-1-1",year_min) 
origin_input <- origin
origin_output <- origin

####

station <- c("T0090","T0083")




## ----settings,echo=TRUE-------------------------------------------------------
sample <- "monthly"

###
n_GPCA_iteration <- 15 
n_GPCA_iteration_residuals <- 15
p <- 2

###
param <- setComprehensiveTemperatureGeneratorParameters(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,sample=sample)
std_tm=param[['stdTm']] ## Standard deviation for "mean" 
param$data_original

###



## ----model,echo=TRUE----------------------------------------------------------

exogen <- NULL ## GAUSSIANIZATION!!!


###
model <- getVARmodel(param$data_for_var,suffix=c("_T1","_T2"),sep="",p=p,exogen=exogen,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,n_GPCA_iteration=n_GPCA_iteration) 
###


## ----generation,echo=TRUE-----------------------------------------------------


gen1 <- generate(model,n=nrow(param$data_for_var),names=names(param$data_for_var)) 

gen1 <- gen1 %>% normalizeGaussian_severalstations(data=param$data_original,inverse=TRUE,type=3,sample=sample,origin_x=origin_output,origin_data=origin_input)
##res_multigen <- normalizeGaussian_severalstations(x=res_multigen0,data=original_data,inverse=TRUE,type=type,sample=sample,origin_x=origin_x,origin_data=origin_data,extremes=extremes)
###

## ----plots,echo=TRUE----------------------------------------------------------
###
for (i in 1:ncol(gen1)) {

  print(ks.test(param$data_original[,i],gen1[,i]))
}
##getVARmodel <-
##  function (data,suffix=c("_Tx","_Tn"),sep="",p=1,type="none",season=NULL,exogen=NULL,lag.max=NULL,ic="AIC",activateVARselect=FALSE,na.rm=TRUE,
##            n_GPCA_iteration=0,n_GPCA_iteration_residuals=n_GPCA_iteration,extremes=TRUE) { 

#####
#####











