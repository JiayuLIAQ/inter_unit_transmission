source("R/functions.R")

path_mdb <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE,
                                         pattern = "\\.mdb$"))

# map(path_mdb$path, read_mdb_and_write_xlsx)

dt_innova <- map(path_mdb$path, read_mdb) %>% rbindlist

dt_innova[, datetime := as_datetime(tim) + hours(8)]

dt_innova %>% setnames(old = "meas_0", new = "sf6_conc")

dt_innova[,c("datetime","sf6_conc","table_name","path")] %>% fwrite("clean_data/innova/innova_raw.csv")




#
path <- path_mdb$path[2]
# process_mdb_and_write_xlsx(path)

dt_raw <- read_mdb(path)

dt_raw[, datetime := as_datetime(tim) + hours(8)]

dt_raw %>% setnames(old = "meas_0", new = "sf6_conc")

dt_raw %>% ggplot(aes(datetime, sf6_conc, color = table_name) )+ geom_point()

# path  <-  "data/Henderson_210423/Innova_210423.mdb"


channel.sadc <- odbcConnectAccess(path)

# conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=data/Henderson_210423/Innova_210423.mdb")



basename(path)


list_of_datasets <- list(table_names[1] = test[table_name == table_names[1]], 
                         table_names[2] = test[table_name == table_names[2]])

list_of_datasets <- list("airrec01" = test[table_name == table_names[1]], 
                         "airrec02" = test[table_name == table_names[2]])

openxlsx::write.xlsx(list_of_datasets, file = "writeXLSX2.xlsx")


write.xlsx(dataframe1, file="filename.xlsx", sheetName="sheet1", row.names=FALSE)
write.xlsx(dataframe2, file="filename.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)

data_[, datetime := as_datetime(tim) + hours(8)]




channel.sadc <- odbcConnectAccess(conn)

conn <- odbcConnectAccess2007(path.expand("~/Database.accdb"))