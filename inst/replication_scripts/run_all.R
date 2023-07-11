#set a local folder where all outputs of all scripts will be stored
setwd("your/path/here")

total_start_time<- Sys.time()
write(paste0("start: ",total_start_time), file="total_start_time.txt")

library(HDLPrepro)
source(file=paste0(system.file("replication_scripts", package="HDLPrepro", mustWork = TRUE),"/running_simulation3_1.R"), local=TRUE)
source(file=paste0(system.file("replication_scripts", package="HDLPrepro", mustWork = TRUE),"/plotting_simulation3_1.R"), local=TRUE)
source(file=paste0(system.file("replication_scripts", package="HDLPrepro", mustWork = TRUE),"/processing_FREDMD.R"), local=TRUE)
source(file=paste0(system.file("replication_scripts", package="HDLPrepro", mustWork = TRUE),"/running_simulation3_2.R"), local=TRUE)
source(file=paste0(system.file("replication_scripts", package="HDLPrepro", mustWork = TRUE),"/plotting_simulation3_2.R"), local=TRUE)
source(file=paste0(system.file("replication_scripts", package="HDLPrepro", mustWork = TRUE),"/application4_1.R"), local=TRUE)
source(file=paste0(system.file("replication_scripts", package="HDLPrepro", mustWork = TRUE),"/processing_R&Z_data.R"), local=TRUE)
source(file=paste0(system.file("replication_scripts", package="HDLPrepro", mustWork = TRUE),"/application4_2.R"), local=TRUE)

total_end_time <- Sys.time()
write(paste0("end: ",total_end_time), file="total_end_time.txt")