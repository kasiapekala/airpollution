################ Sourcing and import ###########################

#source files
source("data_import.R")
source("data_cleaning.R")
source("teryt.r")

#load library
library(stringdist)

# import and create data frame for measurments
# fl1 <- c("2009","2010","2011","2012","2013","2014","2015","2016")
# fl2 <- c("230","231","232","233","234","235","236","242")
# files_list <- data.frame(fl1,fl2)
# import_data(files_list) #data imports
df <- get_pm25()

#import and create data frames
slownik <- get_dict() #dictionary import
metadane <- get_metadata() #metadata import

################ Imported data frames info #####################################
dim(df) # dimensions of a data frame
dim(slownik)
dim(metadane)

format(object.size(df),units = "Mb") #size of a data frame
format(object.size(slownik),units = "Mb") #size of a data frame
format(object.size(metadane),units = "Mb") #size of a data frame


################ Cleaning dictionary ########################################### 

glimpse(slownik)

#check for duplicated values
check_for_dup_in_imported_slownik(slownik)

#update missing values
sapply(slownik, function(x) sum(is.na(x))) #how many missing values
kody_brakujace <- slownik[!complete.cases(slownik),"KOD_NOWY"]
length(setdiff(kody_brakujace$KOD_NOWY,metadane$`Kod stacji`)) #are missing codes present in metadane (yes = 0)
slownik <- fill_missing_dict_data(slownik,metadane) #updating dictionary
sapply(slownik, function(x) sum(is.na(x))) #how many missing values

# get only selected columns from metadane
glimpse(metadane)
metadane_full <- metadane
metadane <- metadane[,c(2,6,7,8,9,10,15:16)]
colnames(metadane)[3] <- "Data zamkniecia"

#preparing for join 
tmp <- setdiff(slownik$KOD_NOWY,metadane$`Kod stacji`)
isTRUE(length(setdiff(tmp,unique(df$Stacja))) == length(tmp)) #TRUE = missing codes are absent from df
sapply(metadane, function(x) sum(is.na(x))) #how many missing values
any(duplicated(metadane$`Kod stacji`))
compare_stations_no_end_date_vs_status(metadane) #no end date => (status==active)?

#joining metdane and slownik
dim_slownik <- dim(slownik)
slownik <- left_join(slownik,metadane,by = c("KOD_NOWY"="Kod stacji"))
isTRUE(dim(slownik)[1]==dim_slownik[1]) #is row number correct after join
isTRUE(dim(slownik)[2]==dim(metadane)[2]+dim_slownik[2]-1) #is col number correct after join
slownik <- clean_new_columns(slownik)

#slownik - checking for missing val
sapply(slownik, function(x) sum(is.na(x))) #how many missing values
#View(slownik[!complete.cases(slownik),])

#checking values in new columns
unique(filter(slownik,!(KOD_NOWY %in% tmp))[,10])
unique(filter(slownik,!(KOD_NOWY %in% tmp))[,11])
unique(filter(slownik,!(KOD_NOWY %in% tmp))[,12])
summary(unique(filter(slownik,!(KOD_NOWY %in% tmp))[,8]))
summary(unique(filter(slownik,!(KOD_NOWY %in% tmp))[,9]))

#checking for duplicated stations
slownik_tmp <- slownik %>%
  mutate(crd = paste(Lon,Lat))
slownik_tmp <- filter(slownik_tmp,!is.na(Lat))
any(duplicated(slownik_tmp$crd))
vec <- duplicated(slownik_tmp$crd) | duplicated(slownik_tmp$crd, fromLast=TRUE)
res <- slownik_tmp[vec,]
#View(res)

#removing duplicated stations
setdiff(c(res$KOD_STARY,res$KOD_NOWY),unique(df$Stacja)) #which codes are absent from df
slownik <- filter(slownik,NR != 170)

#checking lat and lon
extreme_north = 54.835778
extreme_south = 49.0025
extreme_west = 14.123609
extreme_east = 24.145521

isTRUE(dim(filter(slownik,Lat>extreme_north | Lat<extreme_south))[1]==0) #TRUE = lat ok
isTRUE(dim(filter(slownik,Lon>extreme_east | Lon<extreme_west))[1]==0) #TRUE = lon ok

#checking Levenshtein distance
dstc_mtrx <- stringdistmatrix(slownik$KOD_NOWY,useNames="strings",method="lv")
dstc_mtrx <- as.data.frame(as.matrix(dstc_mtrx))
dstc_res <- which(dstc_mtrx>0 & dstc_mtrx<4, arr.ind=TRUE)
#View(filter(slownik,KOD_NOWY %in% rownames(dstc_res)))

rm(metadane,tmp,kody_brakujace,dim_slownik,res,slownik_tmp,vec,dstc_res,dstc_mtrx)
rm(extreme_east,extreme_north,extreme_south,extreme_west)
################ Dictionary vs TERYT ########################################### 

#comparing with TERYT data
wojewodztwa_list <- filter(TERC,NAZWA_DOD=="województwo")[,5]
wojewodztwa_list <- as.character(unique(wojewodztwa_list))
check_teryt_wojewodztwa(unique(slownik$WOJEWODZTWO))
check_teryt_places(unique(slownik$MIEJSCOWOSC))

#analyzing places not found in TERYT
missing_places <-list_diff_teryt_places(unique(slownik$MIEJSCOWOSC))
print(paste("Liczba niezgodnych miejscowości wynosi",length(missing_places)))
missing_places_df <- filter(slownik,toupper(MIEJSCOWOSC) %in% missing_places)
missing_places_df<- mutate(missing_places_df,
                           crd=paste(missing_places_df$Lat,
                                     missing_places_df$Lon,sep=","))
rm(missing_places_df,missing_places)

#correcting places
slownik$MIEJSCOWOSC[slownik$MIEJSCOWOSC == "Czerniawa"] <- "Czerniawa-Zdrój"
slownik$MIEJSCOWOSC[slownik$NR == 51] <- "Zwierzyniec"
slownik$ADRES[slownik$NR == 51] <- "ul. Biały słup 16"
slownik$MIEJSCOWOSC[slownik$MIEJSCOWOSC == "Osiedle Nowiny"] <- "Nowiny"
slownik$MIEJSCOWOSC[slownik$NR == 202] <- "Nowa Słupia"
slownik$ADRES[slownik$NR == 202] <- "Święty Krzyż 1"

missing_places <-list_diff_teryt_places(unique(slownik$MIEJSCOWOSC))
print(paste("Liczba niezgodnych miejscowości wynosi",length(missing_places)))

#creating new column
#View(slownik$NAZWA_STACJI)
slownik <- slownik %>%
   mutate(LOKALIZACJA = paste(MIEJSCOWOSC,ADRES,sep=", "))
any(duplicated(slownik$LOKALIZACJA)) #FALSE = no dup

rm(TERC, places_categories, places_list, wojewodztwa_list, missing_places)

colnames(slownik) <- toupper(colnames(slownik))

glimpse(slownik)

################ Cleaning main dataframe #######################################

# general info about dataframe
glimpse(df)
sapply(df, function(x) sum(is.na(x))) #how many missing values

# remove rows with NA's
na_no <- sum(is.na(df$Pomiar))
print(paste(na_no,scales::percent(na_no/nrow(df))))
df <- df[!is.na(df$Pomiar),]
rm(na_no)

# transform timestamp column
df <- transform_time_col(df)

# update dictionary (from metadane_full)
kody_brakujace <- setdiff(unique(df$Stacja),c(slownik$KOD_STARY,slownik$KOD_NOWY))
length(setdiff(kody_brakujace,metadane_full$`Kod stacji`))
slownik <- fill_missing_dict_data_full(slownik,metadane_full,kody_brakujace)
length(setdiff(unique(df$Stacja),c(slownik$KOD_STARY,slownik$KOD_NOWY)))
rm(metadane_full,kody_brakujace)

# cleaning main dataframe - station codes and column types
df <- clean_station_names(df,slownik) #mapping to KOD_NOWY
df <- transform_data_col(df) #change column types

# removing negative measurements
summary(df$Pomiar)
sum(df$Pomiar<0)
df <- df[!df$Pomiar<0,]
summary(df$Pomiar)

# checking for full hours in timestamps
timestamp_analysis(df)
df <- round_tmstamp_column(df)
timestamp_analysis(df)

# checking for timestamp column completness
start_point <- min(df$Timestamp)
end_point <- max(df$Timestamp)
tml <- seq(from=start_point,
           to=end_point,
           by="hour")  
length(setdiff(tml,df[,1]))
length(setdiff(df[,1],tml))
rm(start_point,end_point,tml)

slownik <- filter(slownik, KOD_NOWY %in% unique(df$Stacja))

################ Save data frames to rds files #######################################

saveRDS(df, file = "df.rds")
saveRDS(slownik, file = "slownik.rds")