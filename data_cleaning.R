library(lubridate)
library(xts)

fill_missing_dict_data <- function(slownik,metadane) {
  tmp <- slownik[is.na(slownik$ADRES),]
  tmp$ADRES <- metadane$Ulica[match(tmp$KOD_NOWY, metadane$`Kod stacji`)]
  slownik[is.na(slownik$ADRES),"ADRES"] <- tmp$ADRES
  
  tmp <- slownik[is.na(slownik$MIEJSCOWOSC),]
  
  colnames(metadane)[13] <- "Miejscowosc"
  
  tmp$MIEJSCOWOSC <- metadane$Miejscowosc[match(tmp$KOD_NOWY, metadane$`Kod stacji`)]
  slownik[is.na(slownik$MIEJSCOWOSC),"MIEJSCOWOSC"] <- tmp$MIEJSCOWOSC
  
  tmp <- slownik[is.na(slownik$NAZWA_STACJI),]
  tmp$NAZWA_STACJI <- metadane$`Nazwa stacji`[match(tmp$KOD_NOWY, metadane$`Kod stacji`)]
  slownik[is.na(slownik$NAZWA_STACJI),"NAZWA_STACJI"] <- tmp$NAZWA_STACJI
  
  return(slownik)
}

fill_missing_dict_data_full <- function(slownik,metadane,missing_codes) {
  missing_data <- filter(metadane,`Kod stacji` %in% missing_codes)
  missing_data <- missing_data[,c(1,12,2,2,4,13,14,6,7,8,9,10,15,16)]
  colnames(missing_data) <- colnames(slownik[,c(1:14)])
  missing_data <- mutate(missing_data,
                         LOKALIZACJA = paste(MIEJSCOWOSC,ADRES,sep=", "))
  slownik <- rbind(slownik,missing_data)
  return(slownik)
}


check_for_missing_values <- function(df) {
  # returns FALSE if any timestamp/name data is missing
  res = TRUE
  if (any(is.na(df$Timestamp))) {
    res = FALSE
    cat("Data cleaning warning: \nSome timestamp data is missing.")
  }
  if (any(is.na(df$Stacja))) {
    res = FALSE
    cat("Data cleaning warning: \nsome station name data is missing.")
  }
  return(res)
}

clean_station_names <- function(df,slownik) {
  df$Stacja <- plyr::mapvalues(df$Stacja,slownik$KOD_STARY,
                               slownik$KOD_NOWY,warn_missing = F)
  return(df)
}

transform_time_col <- function(df) {
  df$Timestamp <- as.POSIXct(as.numeric(df$Timestamp)*3600*24-3600,origin = "1899-12-30")
  return(df)
}

transform_data_col <- function(df) {
  df$Stacja <- as.factor(df$Stacja)
  df$Pomiar <- gsub(",",".",df$Pomiar)
  df$Pomiar <- as.numeric(df$Pomiar)
  df$Pomiar <- round(df$Pomiar,digits = 0)
  return(df)
}

round_tmstamp_column <- function(df) {
  df$Timestamp <- round_date(df$Timestamp, unit = "5 mins")
  return(df)
}

timestamp_analysis <- function(df) {
  #print(table(length(df_cleaned$Timestamp)))
  dft <- as.data.frame(table(unique(df$Timestamp)))
  vect_min <- substr(dft$Var1, 15, 19)
  print(unique(vect_min))
}

remove_err <- function(df) {
  print(filter(df_cleaned,Pomiar<0))
  neg_id <- which(df_cleaned$Pomiar<0)
  df_cleaned$Pomiar[neg_id] <- NA
}

add_dict <- function(df,st_dict) {
  df$Stacja <- as.character(df$Stacja)
  df_joined <- left_join(df, st_dict, by = c("Stacja"="KOD_NOWY"))
  df_obs <- select(df_joined, -"KOD_STARY")
  return(df_obs)
}

clean_new_columns <- function(slownik){
  slownik$`Data uruchomienia` <- as.Date(slownik$`Data uruchomienia`)
  slownik$`Data zamkniecia` <- as.Date(slownik$`Data zamkniecia`)
  slownik$Status <- as.factor(slownik$Status)
  slownik$`Typ stacji` <- as.factor(slownik$`Typ stacji`)
  slownik$`Typ obszaru` <- as.factor(slownik$`Typ obszaru`)
  return(slownik)
}

compare_stations_no_end_date_vs_status <- function(metadane) {
  vec_station_missing_end_date <- 
    metadane[is.na(metadane$`Data zamkniecia`),]$`Kod stacji`
  vec_station_active <- filter(metadane,Status == "aktywny")$`Kod stacji`
  result_number <- length(setdiff(vec_station_missing_end_date,vec_station_active))
  return(ifelse(result_number==0,"Ten sam zbior stacji","Rozne zbiory stacji"))
}

check_for_dup_in_imported_slownik <- function(slownik) {
  a1 <- any(duplicated(slownik$NR))
  a2 <- any(duplicated(slownik$KOD_STARY))
  a3 <- any(duplicated(slownik$KOD_NOWY))
  a4 <- any(duplicated(slownik$NAZWA_STACJI))
  return(ifelse(isTRUE(a1|a2|a3|a4),"Wystepuja duplikaty","Brak duplikatow"))
}