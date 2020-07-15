TERC <- read.csv("Data/TERC.csv", encoding="UTF-8", sep=";")

check_teryt_wojewodztwa <- function(to_be_checked)
{
  result_number <- length(setdiff(toupper(to_be_checked),
                                  toupper(wojewodztwa_list)))
  return(ifelse(result_number==0,"Passed","Failed"))
}

places_categories <- 
  c("gmina miejska",
    "gmina miejska, miasto stołeczne",
    "gmina miejsko-wiejska",
    "gmina wiejska",
    "miasto",
    "miasto na prawach powiatu",
    "miasto stołeczne, na prawach powiatu",
    "obszar wiejski")

places_list <- as.character(unique(filter(TERC,NAZWA_DOD  %in% places_categories)[,5]))

check_teryt_places <- function(to_be_checked)
{
  result_number <- length(setdiff(toupper(to_be_checked),
                                  toupper(places_list)))
  return(ifelse(result_number==0,"Passed","Failed"))
}

list_diff_teryt_places <- function(to_be_checked)
{
  result_list <- setdiff(toupper(to_be_checked),
                                  toupper(places_list))
  return(result_list)
}

# SIMC <- read.csv("Data/SIMC.csv", encoding="UTF-8", sep=";")
# SIMC2 <- select(SIMC,NAZWA,SYM)
# 
# ULIC <- read.csv("Data/ULIC.csv", encoding="UTF-8", sep=";")
# ULIC2 <- select(ULIC,SYM,CECHA,NAZWA_1,NAZWA_2)
# setdiff(ULIC2$SYM,SIMC2$SYM)
# dim(ULIC2)[1]
# dim(ULIC2)[2]
# ulice_join <- left_join(ULIC2,SIMC2,by = c("SYM"="SYM"))
# ulice <- ulice_join %>%
#   mutate(nazwa_ulicy = paste(CECHA,NAZWA_1,NAZWA_2),
#          nazwa_ulicy2 = paste(CECHA,NAZWA_1)) %>%
#   select(NAZWA,nazwa_ulicy,nazwa_ulicy2)
# colnames(ulice)[1] <- "miejscowosc"
# 
# slownik2 <- mutate(slownik,
#                    ADRES2 = trimws(gsub('([0-9]+.*$)', '', gsub('([0-9]+$)', '', ADRES)), 
#                                    which = c("right")))
# setdiff(toupper(slownik2$ADRES2),
#         toupper(c(ulice$nazwa_ulicy,ulice$nazwa_ulicy2)))
