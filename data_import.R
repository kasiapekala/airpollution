library(readxl)
library(dplyr)
library(tidyr)

import_data <- function(files_list) {
  date_downloaded <- download_all_files(files_list)
  unzip_all_files(files_list)
  return(date_downloaded)
}

download_all_files <- function(list_of_files) {
  apply(list_of_files,1,get_zip_file)
  dateDownloaded <- date()
  return(dateDownloaded)
}

get_zip_file <- function(i) {
  get_url <- paste("http://powietrze.gios.gov.pl/pjp/archives/downloadFile/", i[2], sep="")
  to_dest <- paste("Data", "/zip/", i[1], ".zip", sep="")
  download.file(get_url,destfile = to_dest, mode='wb', cacheOK=FALSE)
  print("get zip file, status:")
  print(i)
}

unzip_all_files <- function(list_of_files) {
  apply(list_of_files,1,unzip_file)
}

unzip_file <- function(i){
  unzip(paste("Data", "/zip/", i[1], ".zip", sep=""),exdir="Data\\raw")
  print("unzip_file, status:")
  print(i)
}

get_data_in_long_form <- function(file_name) {
  fn <- paste("Data/raw/",file_name,sep="")
  df <- read_excel(fn)
  if (colnames(df)[1] != "Kod stacji") {
    colnames(df) <- df[1,]
    df <- df[-c(1:5),]
  } else {
    df <- df[-c(1:2),]
  }
  colnames(df)[1] <- "Timestamp"
  n <- ncol(df)
  df <- gather(df,"Stacja","Pomiar",2:n)
  return(df)
}

get_pm25 <- function() {
  Timestamp <- as.list(c())
  Stacja <- as.list(c())
  Pomiar <- as.list(c())
  df_result <- data.frame(Timestamp,Stacja,Pomiar)
  list_of_files <- list.files(path = "data/raw",pattern = "[0-9]{4}.?PM2.?5.?1")
  df_long <- lapply(list_of_files,get_data_in_long_form)
  df_result <- bind_rows(df_result,df_long)
  return(df_result)
}

get_dict <- function() {
  get_url <- "http://powietrze.gios.gov.pl/pjp/archives/downloadFile/102"
  to_dest <- "Data/slownik.xlsx"
  download.file(get_url,destfile = to_dest, mode='wb', cacheOK=FALSE)
  slownik <- readxl::read_excel("Data/slownik.xlsx")
  colnames(slownik)[c(2:5)] <- c("WOJEWODZTWO","KOD_STARY","KOD_NOWY","NAZWA_STACJI")
  return(slownik)
}

get_metadata <- function() {
  get_url <- "http://powietrze.gios.gov.pl/pjp/archives/downloadFile/265"
  to_dest <- "Data/metadane.xlsx"
  download.file(get_url,destfile = to_dest, mode='wb', cacheOK=FALSE)
  metadane <- readxl::read_excel("Data/metadane.xlsx")
  colnames(metadane)[c(15,16)] <- c("Lat","Lon")
  return(metadane)
}

