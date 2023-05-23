library(HDLPrepro)
library(readxl)
library(dplyr)
# We obtained the data from the webpage of Valerie A. Ramey: https://econweb.ucsd.edu/~vramey/research.html#govt.
# Specifically, from the link "Data and Programs" under the section "Government Spending Multipliers in Good Times and in Bad: Evidence from U.S. Historical Data"
# We ran the provided STATA code to define the variables as they do in their paper, then saved them into a new .xls file, and we include this file as part of the package
RZ<-readxl::read_xls(system.file("extdata", "processed data.xls", package="HDLPrepro", mustWork = TRUE)) #This will throw some warnings about dates in the wrong format. We don't use that variable so don't bother fixing it
RZdf<-data.frame(quarter=RZ$quarter, newsy=RZ$newsy, g=RZ$g, y=RZ$y, taxy=RZ$taxy,
                 lag_slack=dplyr::lag(RZ$slack), lag_zlb=dplyr::lag(RZ$zlb_dummy), lag_recession=dplyr::lag(RZ$recession))
dc<-na.omit(RZdf)
