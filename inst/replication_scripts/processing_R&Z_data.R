# This script processes the data obtained from the website of Valerie A. Ramey, relating to the paper Ramey and Zubairy (2018)
rm(list=ls())
start_time<-Sys.time()
library(HDLPrepro) #1.0.0

# in case the following packages are not installed, run:
#install.packages(c("readxl", "dplyr"))
library(readxl) #1.4.3
library(dplyr) #1.1.2

#use this command set the directory in which the plots will be saved
#setwd("your/path/here")

# We obtained the data from the webpage of Valerie A. Ramey: https://econweb.ucsd.edu/~vramey/research.html#govt.
# Specifically, from the link "Data and Programs" under the section "Government Spending Multipliers in Good Times and in Bad: Evidence from U.S. Historical Data"
# We ran the provided STATA code to define the variables as they do in their paper, then saved them into a new .xls file, and we include this file as part of the package
RZ<-readxl::read_xls(system.file("extdata", "processed_data.xls", package="HDLPrepro", mustWork = TRUE)) #This will throw some warnings about dates in the wrong format. We don't use that variable so don't bother fixing it
RZdf<-data.frame(quarter=RZ$quarter, newsy=RZ$newsy, g=RZ$g, y=RZ$y, taxy=RZ$taxy,
                 lag_slack=dplyr::lag(RZ$slack), lag_zlb=dplyr::lag(RZ$zlb_dummy), lag_recession=dplyr::lag(RZ$recession))
dc<-na.omit(RZdf)
saveRDS(dc, "dc.RData")
# noting the time ---------------------------------------------------------
end_time <- Sys.time()
write(paste0("start: ",start_time,", end: ", end_time,", difference: ", end_time-start_time), file="runtime_processing_R&Z_data.txt")