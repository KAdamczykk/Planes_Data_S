# Projekt 2 - PDU 2023
# Czesc wykonana przez Krzysztofa Adamczyka

# Pytania:
# 1) Rozklad opoznien dla 25 najwiekszych lotnisk (w tym szczeglowe dane dla 3 najwiekszych)
# 2) Samoloty z ktorej dekady sa najbardziej podatne na opoznienia pogodowe

# Dane jak i wszystkie funcje uruchamiamy  po kolei, nigdy wszystko razem

# zaczytanie danych z lat 2003 - 2008
options(stringsAsFactors = FALSE)
library("data.table")
Y2003 <- read.csv("D:/Studia/Semestr 2/R/Data Science Projekt/Years/2003.csv.bz2") 
Y2004 <- read.csv("D:/Studia/Semestr 2/R/Data Science Projekt/Years/2004.csv.bz2")
Y2005 <- read.csv("D:/Studia/Semestr 2/R/Data Science Projekt/Years/2005.csv.bz2")
Y2006 <- read.csv("D:/Studia/Semestr 2/R/Data Science Projekt/Years/2006.csv.bz2")
Y2007 <- read.csv("D:/Studia/Semestr 2/R/Data Science Projekt/Years/2007.csv.bz2")
Y2008 <- read.csv("D:/Studia/Semestr 2/R/Data Science Projekt/Years/2008.csv.bz2")

# funkcja obetnijKolumny obcina nam kolumny w data.table tylko do tych ktore beda uzywane w projekcie
obetnijKolumny <- function(Rok){
  Rok <- setDT(Rok)
  Rok <- Rok[, .(Origin, DepDelay, WeatherDelay, NASDelay, CarrierDelay, SecurityDelay, TailNum)]
  return(Rok)
}
Y2008 <- obetnijKolumny(Y2008)

# laczymy wszytskie lata w 1 duza ramke
MergujRamki <- function(Rok1, Rok2){
  Rok1 <- setDT(Rok1)
  Rok2<- setDT(Rok2)
  merguj <- rbind(Rok1, Rok2)
}

Y78 <- MergujRamki(Y2007, Y2008) # robimy tak dla kazdej ramki
remove(Y2007, Y2008) # usuwamy zeby zwolnic pamiec


# Rozklad opoznien dla 25 najwiekszych lotnisk

# funkcja zwraca ramke danych z podzialem na rozne typy opoznien (Weather, NAS, Carrier, Security, Overall), i ilosc lotow dla danego origin
CoMaWiekszyWplyw <- function(Rok){
  Rok <- setDT(Rok)
  IloscLotow <- Rok[, .(FlightCount = .N), by = Origin]
  CalkowityDelay <- Rok[,  .(TotalDelayPerOrigin = sum(DepDelay[DepDelay > 0], na.rm = TRUE)), by = Origin]
  TotalWeatherDelay <- Rok[, .(TotalWeatherDelay = sum(WeatherDelay, na.rm = TRUE)), by = Origin]
  TotalNASDelay <- Rok[, .(TotalNASDelay = sum(NASDelay, na.rm = TRUE)), by = Origin]
  TotalCarrierDelay <- Rok[, .(TotalCarrierDelay = sum(CarrierDelay, na.rm = TRUE)), by = Origin]
  TotalSecurityDelay <- Rok[, .(TotalSecurityDelay = sum(SecurityDelay, na.rm = TRUE)), by = Origin]
  one <- CalkowityDelay[TotalWeatherDelay, on=.(Origin = Origin)]
  sec <- one[TotalNASDelay, on=.(Origin = Origin)]
  lastt <- sec[TotalCarrierDelay, on= .(Origin = Origin)]
  nex <- lastt[TotalSecurityDelay, on = .(Origin = Origin)]
  notleast <- nex[IloscLotow, on = .(Origin = Origin)]
  ret <- head(setorder(notleast, cols= -"FlightCount"), 25)
  ret <- ret[,.(Origin, TotalWeatherDelay, TotalNASDelay, TotalCarrierDelay, TotalSecurityDelay, TotalDelayPerOrigin, FlightCount)]
  ret <- ret[, TotalWeatherDelay:= round(TotalWeatherDelay / 60, digits = 0)]
  ret <- ret[, TotalNASDelay:= round(TotalNASDelay / 60, digits = 0)]
  ret <- ret[, TotalCarrierDelay:= round(TotalCarrierDelay / 60, digits = 0)]
  ret <- ret[, TotalSecurityDelay:= round(TotalSecurityDelay / 60, digits = 0)]
  ret <- ret[, TotalDelayPerOrigin:= round(TotalDelayPerOrigin / 60, digits = 0)]
 
}
y <- CoMaWiekszyWplyw(Y38)


# funkcja oblicza procent poszczegolnych rodzajow opoznien w stosunku do calkowitego opoznienia
ObliczProcent <- function(Rok){
  Rok <- setDT(Rok)
  Rok <- Rok[, TotalWeatherDelay := round((TotalWeatherDelay / TotalDelayPerOrigin)*100, digits = 2)]
  Rok <- Rok[, TotalNASDelay := round((TotalNASDelay / TotalDelayPerOrigin)*100, digits = 2)]
  Rok <- Rok[, TotalCarrierDelay := round((TotalCarrierDelay / TotalDelayPerOrigin)*100, digits = 2)]
  Rok <- Rok[, TotalSecurityDelay := round((TotalSecurityDelay / TotalDelayPerOrigin) * 100, digits = 2)]
  Rok <- Rok[, .(Origin, TotalWeatherDelay, TotalNASDelay, TotalCarrierDelay, TotalSecurityDelay)]
}
z <- ObliczProcent(y)

# to samo co powyzej ale bez origin
ObliczProcentW <- function(Rok){
  Rok <- setDT(Rok)
  Rok <- Rok[, TotalWeatherDelay := round((TotalWeatherDelay / TotalDelayPerOrigin)*100, digits = 2)]
  Rok <- Rok[, TotalNASDelay := round((TotalNASDelay / TotalDelayPerOrigin)*100, digits = 2)]
  Rok <- Rok[, TotalCarrierDelay := round((TotalCarrierDelay / TotalDelayPerOrigin)*100, digits = 2)]
  Rok <- Rok[, TotalSecurityDelay := round((TotalSecurityDelay / TotalDelayPerOrigin) * 100, digits = 2)]
  Rok <- Rok[, .(TotalWeatherDelay, TotalNASDelay, TotalCarrierDelay, TotalSecurityDelay)]
}

# funkcja liczy sume poszczegolnych opoznien dla 25 najwiekszych lotnisk
LaczneOpoznienieDlaTop25 <- function(Rok){
  all <- colSums(Rok[, -1])
  kol <- names(all)
  df <- data.frame(as.list(all))
  names(df) <- kol
  df
}
w <- LaczneOpoznienieDlaTop25(y)
procentW <- ObliczProcentW(w)

# funkcja przeksztalca ramke danych z 1 wierszem na wektor
RozlaczWektor <- function(ramka, NrWiersza){
  namy <- colnames(ramka)
  wart <- unlist(ramka[NrWiersza,])
  names(wart) <- namy
  wart
}
procentwww <- RozlaczWektor(procentW, 1)

# funkcja tworzy wykresy dla poszczegolnych lotnisk
TworzWykresDlaLotniska <- function(Rok, NrWiersza){
  wiersz <- RozlaczWektor(Rok, NrWiersza)
  wart <- as.numeric(wiersz[-1])
  kolorki <- c("blue", "green4", "orange2", "pink4")
  names(wart) <- c("Weather", "NAS", "Carrier", "Security")
  wykres <- barplot(wart, las = 1, ylim = c(0,50), cex.names = 1, cex.axis = 1, ylab = "% of delays", col = kolorki)
  abline(h = seq(0, 50, by=5), lty = 2, col = 'gray21')
  text(x = wykres, y = wart, labels = wart, pos = 3)
  title(paste("% ditribution of delays for", wiersz[1], "airport"))
  return(wykres)
}
wykres <- TworzWykresDlaLotniska(z,3)
print(wykres)

# tworzenie wykresu laczacego top 25 lotnisk i rozklad
names(procentwww) <- c("Weather", "NAS", "Carrier", "Security")
kolorki <- c("blue", "green4", "orange2", "pink4")
w <- barplot(procentwww, las = 1, ylim = c(0,50), ylab = "% of delays", cex.names = 1, cex.axis = 1, col = kolorki)
abline(h = seq(0, 50, by=5), lty = 2, col = 'gray21')
text(x = w, y = procentwww, labels = procentwww, pos = 3)
title(paste("% distribution of delays for top 25 largest airports"))
print(w)


# Samoloty z ktorej dekady sa najbardziej podatne na opoznienia pogodowe

# funckja obcina ramke z latami do potrzebnych nam kolumn
planes <- read.csv("D:/Studia/Semestr 2/R/Data Science Projekt/Years/planes.csv")
obcinanie1 <- function(Rok){
  Rok <- setDT(Rok)
  ret <- Rok[, .(TailNum, WeatherDelay)]
  ret
}

# funkcja obcinajaca ramke z samolotami do potrzebnych kolumn
obcinanie2 <- function(ROk){
  Rok <- setDT(ROk)
  ret <- Rok[, .(Tailnum, Year)]
}

planes <- obcinanie2(planes)
Y38v2 <- obcinanie1(Y38)

# funkcja laczy ramki z latami i samolotami
laczramki <- function(Rok, planes) {
  Rok <- setDT(Rok)
  planes <- setDT(planes)
  mer <- Rok[planes, on=.(TailNum = Tailnum)]
  mer
}
Y38v2 <- laczramki(Y38v2, planes)

# funkcja ktora sprawdza jaki jest zakres lat produkcji samolotow
wyznaczMinMaxRok <- function(frame){
  frame <- frame[, Year := as.numeric(Year)]
  frame <- frame[Year != 0]
  najm <- min(frame$Year, na.rm = TRUE)
  najw <- max(frame$Year, na.rm = TRUE)
  c(najm, najw)
}
minmax <- wyznaczMinMaxRok(Y38v2)

# funckja zwraca ilosc lotow dla samolotow powstalych w roznych dekadach
iloscLotowPerDekadaSamolotu <- function(Rok){
  Rok <- setDT(Rok)
  Rok <- Rok[Year > 1949]
  Rok <- Rok[Year < 2010]
  dt <- Rok[, group := cut(Year, breaks = seq(1950, 2010, by= 10), labels = FALSE)]
  dt <- Rok[, .(FlightNumber = .N), by = group]
  dt <- setorder(dt, cols = "group")
  napisy <- c("50", "60", "70", "80", "90", "00")
  wartosci <- unlist(c(dt[1,2], dt[2,2], dt[3,2], dt[4,2], dt[5,2], dt[6,2]))
  names(wartosci) <- napisy
  wartosci
}
rozkladwiekusamolotow <- iloscLotowPerDekadaSamolotu(Y38v2)

kolorki <- c("orange2", "red3", "pink2", "blue", "darkgreen", "yellow3")

# tworzymy wykres z iloscia lotow dla samolotow stworzonych w roznych dekadach
wykres <- barplot(rozkladwiekusamolotow, las = 0, ylim = c(0,13000000), cex.axis = 1, cex.names = 1, ylab = "Number of flights", xlab = "Decades", col = kolorki)
text(x = wykres, y = rozkladwiekusamolotow, labels = rozkladwiekusamolotow, pos = 3)
abline(h = seq(0,12000000, by = 1000000), lty = 2, col ='gray41')
title("Flights distribution depending on the \n year of construction of aircraft")
print(wykres)

# funckja zwraca calkowite opoznienie pogogdowe dla samolotow z roznych dekad w minutach
weatherDelaydlaLat <- function(frame){
  Rok <- setDT(frame)
  Rok <- Rok[Year > 1949]
  Rok <- Rok[Year < 2009]
  dt <- Rok[, group := cut(Year, breaks = seq(1950, 2010, by= 10), labels = FALSE)]
  dt <- Rok[, .(TotalWeatherDelay = sum(WeatherDelay, na.rm = TRUE)), by = group]
  dt <- setorder(dt, cols = "group")
  napisy <- c("50", "60", "70", "80", "90", "00")
  wartosci <- unlist(c(dt[1,2], dt[2,2], dt[3,2], dt[4,2], dt[5,2], dt[6,2]))
  names(wartosci) <- napisy
  wartosci
}
weatherDelay <- weatherDelaydlaLat(Y38v2)

# wykres do funckji powyzej
wykres1 <- barplot(weatherDelay, las = 0, ylim = c(0,8200000), cex.axis = 1, cex.names = 1, xlab = "Decades", ylab = "Minutes", col = kolorki)
text(x = wykres1, y = weatherDelay, labels = weatherDelay, pos = 3)
abline(h = seq(0,8200000, by = 500000), lty = 2, col ='gray41')
title("Total weather delay in minutes depending on year of construction of aircraft")
print(wykres1)

# wykres dla sredniego opoznienia pogodowego dla poszczegolnych lat na pojedynczy lot (w minutach)
nowy <- round(weatherDelay / rozkladwiekusamolotow, digits = 4)
wykres2 <- barplot(nowy, las = 0, ylim = c(0,1.5), cex.axis = 1, cex.names = 1, xlab = "Decades", ylab = "Average number of minutes", col = kolorki)
text(x = wykres2, y = nowy, labels = nowy, pos = 3)
abline(h = seq(0,1.5, by = 0.25), lty = 2, col ='gray41')
title("Average number of minutes of weather delay \n depending on year of construction of aircraft")
print(wykres2)