#you can run the following to install the exact versions of packages we used
#remotes::install_version("desla", version = "0.3.0", repos = "https://cloud.r-project.org")
#remotes::install_version("dplyr", version = "1.1.2", repos = "https://cloud.r-project.org")
#remotes::install_version("ggpattern", version = "1.0.1", repos = "https://cloud.r-project.org")
#remotes::install_version("ggplot2", version = "3.4.2", repos = "https://cloud.r-project.org")
#remotes::install_version("ggpubr", version = "0.6.0", repos = "https://cloud.r-project.org")
#remotes::install_version("readxl", version = "1.4.3", repos = "https://cloud.r-project.org")
#remotes::install_version("reshape2", version = "1.4.4", repos = "https://cloud.r-project.org")
#remotes::install_version("xtable", version = "1.8-4", repos = "https://cloud.r-project.org")
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