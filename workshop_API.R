library(celltracktech)
library(duckdb)
start <- Sys.time()

####SETTINGS#####
myproject <- "Meadows V2" #this is your project name on your CTT account
outpath <- "~/Documents/workshop" #where your downloaded files are to go 
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "/home/jess/Documents/workshop/meadows.duckdb", read_only = FALSE)
################
get_my_data(my_token, outpath, con, myproject=myproject, begin=as.Date("2023-07-31"), end=as.Date("2023-10-31"), filetypes=c("raw", "node_health"))
update_db(con, outpath, myproject)
DBI::dbDisconnect(con)

time_elapse <- Sys.time() - start
print(time_elapse)