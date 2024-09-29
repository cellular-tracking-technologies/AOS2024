library(celltracktech)
library(duckdb)
start <- Sys.time()

####SETTINGS#####
myproject <- "Meadows V2" #this is your project name on your CTT account
outpath <- "~/Documents/workshop/test_data" #where your downloaded files are to go 
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "/home/jess/Documents/workshop/test_data/meadows_test.duckdb", read_only = FALSE)
################
get_my_data(my_token, outpath, con, myproject=myproject, begin=as.Date("2023-08-01"), end=as.Date("2023-08-03"), filetypes=c("raw", "node_health"))
update_db(con, outpath, myproject)
DBI::dbDisconnect(con)
time_elapse <- Sys.time() - start
print(time_elapse)