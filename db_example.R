library(duckdb)
library(dplyr)

tagid = c("072A6633","2D4B782D")

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "/home/jess/Documents/workshop/full_data/meadows.duckdb", read_only = TRUE)

testdata <- tbl(con, "raw") |> 
  filter(time >= as.Date("2023-09-02") & time <= as.Date("2023-09-03")) |>
  filter(tag_id %in% tagid) |>
  collect()

start_buff = as.Date("2023-09-01", tz="UTC") 
end_buff = as.Date("2023-09-03", tz="UTC")

nodehealth <- tbl(con, "node_health") |>
  filter(time >= start_buff  & time <= end_buff) |>
  collect()

DBI::dbDisconnect(con)