library(data.table)
library(dplyr)
library(readr)
# pobranie i przygotowanie danych
USA_Crimes <- read_csv("USA_Crimerates_1979-2018.csv")
USA_Crimes_State <- split(USA_Crimes, USA_Crimes$state)
# Maksymalne/Minimalne zbrodnie z użyciem przemocy, maksymalne/minimalne zbrodnie przeciw mieniu

maxVcrimes <- USA_Crimes %>% filter(USA_Crimes$violent_crime == max(USA_Crimes$violent_crime))
maxPcrimes <- USA_Crimes %>% filter(USA_Crimes$property_crime == max(USA_Crimes$property_crime))
minVcrimes <- USA_Crimes %>% filter(USA_Crimes$violent_crime == min(USA_Crimes$violent_crime))
minPcrimes <- USA_Crimes %>% filter(USA_Crimes$property_crime == min(USA_Crimes$property_crime))

# Wyświetlenie znalezionych danych
#max
maxPcrimes
maxVcrimes
#min
minPcrimes
minVcrimes

# obliczenie wskaźnika zbrodni na tysiąć mieszkańców

violentcrimes_per_1k=(USA_Crimes$violent_crime/USA_Crimes$population)*1000
propertycrimes_per_1k=(USA_Crimes$property_crime/USA_Crimes$population)*1000

# stworzenie nowej tabeli z aktualnymi danymi
updated_data=cbind(USA_Crimes,violentcrimes_per_1k,propertycrimes_per_1k)

# min i max ilośc violentcrimes i property_crime na tysiąc mieszkańców

maxVcrimes_per1k <- updated_data %>% filter(updated_data$violentcrimes_per_1k == max(updated_data$violentcrimes_per_1k))
maxPcrimes_per1k <- updated_data %>% filter(updated_data$propertycrimes_per_1k == max(updated_data$propertycrimes_per_1k))

minVcrimes_per1k <- updated_data %>% filter(updated_data$violentcrimes_per_1k == min(updated_data$violentcrimes_per_1k))
minPcrimes_per1k <- updated_data %>% filter(updated_data$propertycrimes_per_1k == min(updated_data$propertycrimes_per_1k))

minVcrimes_per1k
maxVcrimes_per1k

minPcrimes_per1k
maxPcrimes_per1k

#
#Srednie przestepstw
# avg crimes per state
stateVCrimes_mean <- USA_Crimes %>% group_by(state) %>% summarize(AvgVCrime = mean(violent_crime))
statePCrimes_mean <- USA_Crimes %>% group_by(state) %>% summarize(AvgPCrime = mean(property_crime))
# avg crimes per year
yearlyVCrimes_mean <- USA_Crimes %>% group_by(year) %>% summarize(YearVCrime = mean(violent_crime))
yearlyPCrimes_mean <- USA_Crimes %>% group_by(year) %>% summarize(YearPCrime = mean(property_crime))

# merge dataframes into one
Avg_states <- merge(stateVCrimes_mean,statePCrimes_mean)
Avg_year <- merge(yearlyVCrimes_mean,yearlyPCrimes_mean)
row.names(Avg_states) <- states

#

# plots preparation
paint <- c("cyan","aliceblue","antiquewhite","antiquewhite1","antiquewhite2",
           "antiquewhite3","antiquewhite4","aquamarine","aquamarine1","aquamarine2",   
           "aquamarine3","aquamarine4","azure","azure1","azure2",
           "azure3","azure4","beige","bisque","bisque1",
           "bisque2","bisque3","bisque4","black","blanchedalmond",
           "blue","blue1","blue2","blue3","blue4" ,       
           "blueviolet","brown","brown1","brown2","brown3" ,       
           "brown4","burlywood","burlywood1","burlywood2","burlywood3" ,   
           "burlywood4","cadetblue","cadetblue1","cadetblue2","cadetblue3" ,   
           "cadetblue4","chartreuse","chartreuse1","chartreuse2","chartreuse3",   
           "chartreuse4")

states <- vector("character")
for(i in 1:51){
  states <- c(states,USA_Crimes_State[[i]]$state[1])
}
nam1 <- states[1:16]
nam2 <- states[17:34]
nam3 <- states[35:51]

updated_data_bplot <- split(updated_data, updated_data$state)




#Wykres violent_crime part1
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$violent_crime,type = 'l',col = paint[1]
     ,main = "violent_crime part1",xlab = "Year",ylab = "Violent crimes",ylim = c(0,maxVcrimes$violent_crime))
for (i in 2:16) {lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$violent_crime,col = paint[i])
}
legend("topright", legend = nam1,col = paint[1:16], lty=1:2, cex=0.8)


#Wykres violent_crime part2
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$violent_crime,type = 'l',col = paint[1]
     ,main = "violent_crime part2",xlab = "Year",ylab = "Violent crimes",ylim = c(0,250000))
for (i in 17:34) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$violent_crime,col = paint[i])
}
legend("topright", legend = nam2,col = paint[17:34], lty=1:2, cex=0.8)


#Wykres violent_crime part3
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$violent_crime,type = 'l',col = paint[1]
     ,main = "violent_crime part3",xlab = "Year",ylab = "Violent crimes",ylim = c(0,150000))
for (i in 35:51) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$violent_crime,col = paint[i])
}
legend("topright", legend = nam3,col = paint[35:51], lty=1:2, cex=0.8)



#Wykres property_crime part1
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$property_crime,type = 'l',col = paint[1]
     ,main = "property_crime part1",xlab = "Year",ylab = "Property crimes",ylim = c(0,maxPcrimes$property_crime))
for (i in 2:16) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$property_crime,col = paint[i])
}
legend("topright", legend = nam1,col = paint[1:16], lty=1:2, cex=0.8)


#Wykres property_crime part2
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$violent_crime,type = 'l',col = paint[1]
     ,main = "property_crime part2",xlab = "Year",ylab = "Property crimes",ylim = c(0,maxPcrimes$property_crime))
for (i in 17:34) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$property_crime,col = paint[i])
}
legend("topright", legend = nam2,col = paint[17:34], lty=1:2, cex=0.8)


#Wykres property_crime part3
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$violent_crime,type = 'l',col = paint[1]
     ,main = "property_crime part3",xlab = "Year",ylab = "Property crimes",ylim = c(0,maxPcrimes$property_crime))
for (i in 35:51) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$property_crime,col = paint[i])
}
legend("topright", legend = nam3,col = paint[35:51], lty=1:2, cex=0.8)
#



#wykres pudełkowy przestepstw z użyciem przemocy na 1k mieszkancow
dev.new()
boxplot(updated_data_bplot[[1]]$violentcrimes_per_1k,updated_data_bplot[[2]]$violentcrimes_per_1k,updated_data_bplot[[3]]$violentcrimes_per_1k,
        updated_data_bplot[[4]]$violentcrimes_per_1k,updated_data_bplot[[5]]$violentcrimes_per_1k,updated_data_bplot[[6]]$violentcrimes_per_1k,
        updated_data_bplot[[7]]$violentcrimes_per_1k,updated_data_bplot[[8]]$violentcrimes_per_1k,updated_data_bplot[[9]]$violentcrimes_per_1k,
        updated_data_bplot[[10]]$violentcrimes_per_1k,updated_data_bplot[[11]]$violentcrimes_per_1k,updated_data_bplot[[12]]$violentcrimes_per_1k,
        updated_data_bplot[[13]]$violentcrimes_per_1k,
        main = "Violent crimes per 1k citizens cz1",names = states[1:13],col = paint[1:13])

dev.new()
boxplot(updated_data_bplot[[14]]$violentcrimes_per_1k,updated_data_bplot[[15]]$violentcrimes_per_1k,updated_data_bplot[[16]]$violentcrimes_per_1k,
        updated_data_bplot[[17]]$violentcrimes_per_1k,updated_data_bplot[[18]]$violentcrimes_per_1k,updated_data_bplot[[19]]$violentcrimes_per_1k,
        updated_data_bplot[[20]]$violentcrimes_per_1k,updated_data_bplot[[21]]$violentcrimes_per_1k,updated_data_bplot[[22]]$violentcrimes_per_1k,
        updated_data_bplot[[23]]$violentcrimes_per_1k,
       
        main = "Violent crimes per 1k citizens cz2",names = states[14:23],col = paint[14:24])

dev.new()
boxplot(updated_data_bplot[[24]]$violentcrimes_per_1k,updated_data_bplot[[25]]$violentcrimes_per_1k,updated_data_bplot[[26]]$violentcrimes_per_1k,updated_data_bplot[[27]]$violentcrimes_per_1k,updated_data_bplot[[28]]$violentcrimes_per_1k,updated_data_bplot[[29]]$violentcrimes_per_1k,
        updated_data_bplot[[30]]$violentcrimes_per_1k,updated_data_bplot[[31]]$violentcrimes_per_1k,updated_data_bplot[[32]]$violentcrimes_per_1k,
        updated_data_bplot[[33]]$violentcrimes_per_1k,
        main = "Violent crimes per 1k citizens cz3",names = states[24:33],col = paint[24:35])

dev.new()
boxplot(updated_data_bplot[[34]]$violentcrimes_per_1k,updated_data_bplot[[35]]$violentcrimes_per_1k,updated_data_bplot[[36]]$violentcrimes_per_1k,
        updated_data_bplot[[37]]$violentcrimes_per_1k,updated_data_bplot[[38]]$violentcrimes_per_1k,updated_data_bplot[[39]]$violentcrimes_per_1k,
        updated_data_bplot[[40]]$violentcrimes_per_1k,updated_data_bplot[[41]]$violentcrimes_per_1k,updated_data_bplot[[42]]$violentcrimes_per_1k,
        main = "Violent crimes per 1k citizens cz4",names = states[34:42],col = paint[34:42])

dev.new()
boxplot(updated_data_bplot[[43]]$violentcrimes_per_1k,updated_data_bplot[[44]]$violentcrimes_per_1k,updated_data_bplot[[45]]$violentcrimes_per_1k,
        updated_data_bplot[[46]]$violentcrimes_per_1k,updated_data_bplot[[47]]$violentcrimes_per_1k,updated_data_bplot[[48]]$violentcrimes_per_1k,
        updated_data_bplot[[49]]$violentcrimes_per_1k,updated_data_bplot[[50]]$violentcrimes_per_1k,updated_data_bplot[[51]]$violentcrimes_per_1k,
        main = "Violent crimes per 1k citizens cz5",names = states[43:51],col = paint[43:51])


#wykres pudełkowy przestępstw mienia na 1k mieszkańców
dev.new()
boxplot(updated_data_bplot[[1]]$propertycrimes_per_1k,updated_data_bplot[[2]]$propertycrimes_per_1k,updated_data_bplot[[3]]$propertycrimes_per_1k,
        updated_data_bplot[[4]]$propertycrimes_per_1k,updated_data_bplot[[5]]$propertycrimes_per_1k,updated_data_bplot[[6]]$propertycrimes_per_1k,
        updated_data_bplot[[7]]$propertycrimes_per_1k,updated_data_bplot[[8]]$propertycrimes_per_1k,updated_data_bplot[[9]]$propertycrimes_per_1k,
        updated_data_bplot[[10]]$propertycrimes_per_1k,updated_data_bplot[[11]]$propertycrimes_per_1k,updated_data_bplot[[12]]$propertycrimes_per_1k,
        updated_data_bplot[[13]]$propertycrimes_per_1k,
        main = "Property crimes per 1k citizens cz1",names = states[1:13],col = paint[1:13])

dev.new()
boxplot(updated_data_bplot[[14]]$propertycrimes_per_1k,updated_data_bplot[[15]]$propertycrimes_per_1k,updated_data_bplot[[16]]$propertycrimes_per_1k,
        updated_data_bplot[[17]]$propertycrimes_per_1k,updated_data_bplot[[18]]$propertycrimes_per_1k,updated_data_bplot[[19]]$propertycrimes_per_1k,
        updated_data_bplot[[20]]$propertycrimes_per_1k,updated_data_bplot[[21]]$propertycrimes_per_1k,updated_data_bplot[[22]]$propertycrimes_per_1k,
        updated_data_bplot[[23]]$propertycrimes_per_1k,
        
        main = "Property crimes per 1k citizens cz2",names = states[14:23],col = paint[14:24])

dev.new()
boxplot(updated_data_bplot[[24]]$propertycrimes_per_1k,updated_data_bplot[[25]]$propertycrimes_per_1k,updated_data_bplot[[26]]$propertycrimes_per_1k,updated_data_bplot[[27]]$propertycrimes_per_1k,updated_data_bplot[[28]]$propertycrimes_per_1k,updated_data_bplot[[29]]$propertycrimes_per_1k,
        updated_data_bplot[[30]]$propertycrimes_per_1k,updated_data_bplot[[31]]$propertycrimes_per_1k,updated_data_bplot[[32]]$propertycrimes_per_1k,
        updated_data_bplot[[33]]$propertycrimes_per_1k,
        main = "Property crimes per 1k citizens cz3",names = states[24:33],col = paint[24:35])

dev.new()
boxplot(updated_data_bplot[[34]]$propertycrimes_per_1k,updated_data_bplot[[35]]$propertycrimes_per_1k,updated_data_bplot[[36]]$propertycrimes_per_1k,
        updated_data_bplot[[37]]$propertycrimes_per_1k,updated_data_bplot[[38]]$propertycrimes_per_1k,updated_data_bplot[[39]]$propertycrimes_per_1k,
        updated_data_bplot[[40]]$propertycrimes_per_1k,updated_data_bplot[[41]]$propertycrimes_per_1k,updated_data_bplot[[42]]$propertycrimes_per_1k,
        main = "Property crimes per 1k citizens cz4",names = states[34:42],col = paint[34:42])

dev.new()
boxplot(updated_data_bplot[[43]]$propertycrimes_per_1k,updated_data_bplot[[44]]$propertycrimes_per_1k,updated_data_bplot[[45]]$propertycrimes_per_1k,
        updated_data_bplot[[46]]$propertycrimes_per_1k,updated_data_bplot[[47]]$propertycrimes_per_1k,updated_data_bplot[[48]]$propertycrimes_per_1k,
        updated_data_bplot[[49]]$propertycrimes_per_1k,updated_data_bplot[[50]]$propertycrimes_per_1k,updated_data_bplot[[51]]$propertycrimes_per_1k,
        main = "Property crimes per 1k citizens cz5",names = states[43:51],col = paint[43:51])

#Obliczanie czestosci skumulowanej
#
for(i in 1:51){
  USA_Crimes_State[[i]]$skumulowana_Vcrimes <- cumsum(USA_Crimes_State[[i]]$violent_crime)
  USA_Crimes_State[[i]]$skumulowana_Pcrimes <- cumsum(USA_Crimes_State[[i]]$property_crime)
}
#Wykres czestosci skumulowanej Violent crimes cz1
#
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$skumulowana_Vcrimes,type = 'l',col = paint[1]
     ,main = "Czestosc skumulowana Violent crimes cz1",xlab = "Years",ylab = "Skumulowana Violent crimes",ylim = c(0,10000000))
for (i in 2:16) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$skumulowana_Vcrimes,col = paint[i])
}
legend("topright",legend = nam1,col = paint[1:16], lty=1:2, cex=0.8)




#Wykres czestosci skumulowanej Violent crimes cz2
#
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$skumulowana_Vcrimes,type = 'l',col = paint[1]
     ,main = "Czestosc skumulowana Violent crimes cz2",xlab = "Years",ylab = "Skumulowana Violent crimes",ylim = c(0,6000000))
for (i in 17:34) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$skumulowana_Vcrimes,col = paint[i])
}
legend("topright",legend = nam2,col = paint[17:34], lty=1:2, cex=0.8)




#Wykres czestosci skumulowanej Violent crimes cz3
#
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$skumulowana_Vcrimes,type = 'l',col = paint[1]
     ,main = "Czestosc skumulowana Violent crimes cz3",xlab = "Years",ylab = "Skumulowana Violent crimes",ylim = c(0,5000000))
for (i in 35:51) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$skumulowana_Vcrimes,col = paint[i])
}
legend("topright",legend = nam3,col = paint[35:51], lty=1:2, cex=0.8)



#Wykres czestosci skumulowanej property crimes cz1
#
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$skumulowana_Pcrimes,type = 'l',col = paint[1]
     ,main = "Czestosc skumulowana property crimes cz1",xlab = "Years",ylab = "Skumulowana peoperty crimes",ylim = c(0,60000000))
for (i in 2:16) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$skumulowana_Pcrimes,col = paint[i])
}
legend("topright",legend = nam1,col = paint[1:16], lty=1:2, cex=0.8)




#Wykres czestosci skumulowanej property crimes cz2
#
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$skumulowana_Pcrimes,type = 'l',col = paint[1]
     ,main = "Czestosc skumulowana property crimes cz2",xlab = "Years",ylab = "Skumulowana property crimes",ylim = c(0,25000000))
for (i in 17:34) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$skumulowana_Pcrimes,col = paint[i])
}
legend("topright",legend = nam2,col = paint[17:34], lty=1:2, cex=0.8)




#Wykres czestosci skumulowanej property crimes cz3
#
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$skumulowana_Pcrimes,type = 'l',col = paint[1]
     ,main = "Czestosc skumulowana property crimes cz3",xlab = "Years",ylab = "Skumulowana property crimes",ylim = c(0,40000000))
for (i in 35:51) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$skumulowana_Pcrimes,col = paint[i])
}
legend("topright",legend = nam3,col = paint[35:51], lty=1:2, cex=0.8)


#Obliczanie tempa zmian 
# tempo zmian
#
for(i in 1:51){
  tempvalues <- USA_Crimes_State[[i]]
  tempvalues$zmiany_Vcrimes <- tempvalues %>% mutate(rate = 100 * (violent_crime - lag(violent_crime))/lag(violent_crime)) 
  tempvalues$zmiany_Vcrimes <- as.numeric(tempvalues$zmiany_Vcrimes$rate)
  tempvalues$zmiany_Pcrimes <- tempvalues %>% mutate(rate = 100 * (property_crime - lag(property_crime))/lag(property_crime))
  tempvalues$zmiany_Pcrimes <- as.numeric(tempvalues$zmiany_Pcrimes$rate)
  USA_Crimes_State[[i]] <- tempvalues
  remove(tempvalues)
}
#
#Najwieksze zmiany
#
max_zmiany_Vcrimes_val <- 0
max_zmiany_Pcrimes_val <- 0
for(i in 1:51){
  if(max_zmiany_Vcrimes_val < max(USA_Crimes_State[[i]]$zmiany_Vcrimes,na.rm = TRUE)){
    max_zmiany_Vcrimes <- USA_Crimes_State[[i]] %>% 
      filter(USA_Crimes_State[[i]]$zmiany_Vcrimes == max(USA_Crimes_State[[i]]$zmiany_Vcrimes,na.rm = TRUE))
    max_zmiany_Vcrimes_val <- max_zmiany_Vcrimes$zmiany_Vcrimes
  }
  if(max_zmiany_Pcrimes_val < max(USA_Crimes_State[[i]]$zmiany_Pcrimes,na.rm = TRUE)){
    max_zmiany_Pcrimes <- USA_Crimes_State[[i]] %>% 
      filter(USA_Crimes_State[[i]]$zmiany_Pcrimes == max(USA_Crimes_State[[i]]$zmiany_Pcrimes,na.rm = TRUE))
    max_zmiany_Pcrimes_val <- max_zmiany_Pcrimes$zmiany_Pcrimes
  }
}
remove(max_zmiany_Vcrimes_val)
remove(max_zmiany_Pcrimes_val)
#



#wykresy tempa zmian
#
#wykres zmian violentCrimes cz1
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$zmiany_Vcrimes,type = 'l',col = paint[1]
     ,main = "Zmiany ViolentCrimes cz1",xlab = "Year",ylab = "Zmiany",ylim = c(-50,50))
for (i in 2:16) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$zmiany_Vcrimes,col = paint[i])
}
#wykres zmian violentCrimes cz2
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$zmiany_Vcrimes,type = 'l',col = paint[1]
     ,main = "Zmiany ViolentCrimes cz2",xlab = "Year",ylab = "Zmiany",ylim = c(-50,100))
for (i in 17:34) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$zmiany_Vcrimes,col = paint[i])
}
#wykres zmian violentCrimes cz3
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$zmiany_Vcrimes,type = 'l',col = paint[1]
     ,main = "Zmiany ViolentCrimes cz3",xlab = "Year",ylab = "Zmiany",ylim = c(-50,100))
for (i in 35:51) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$zmiany_Vcrimes,col = paint[i])
}



#wykres zmian propertyCrimes cz1
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$zmiany_Pcrimes,type = 'l',col = paint[1]
     ,main = "Zmiany PropertyCrimes cz1",xlab = "Year",ylab = "Zmiany",ylim = c(-50,50))
for (i in 2:16) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$zmiany_Pcrimes,col = paint[i])
}
#wykres zmian propertyCrimes cz2
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$zmiany_Pcrimes,type = 'l',col = paint[1]
     ,main = "Zmiany PropertyCrimes cz2",xlab = "Year",ylab = "Zmiany",ylim = c(-50,100))
for (i in 17:34) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$zmiany_Pcrimes,col = paint[i])
}
#wykres zmian propertyCrimes cz3
dev.new()
plot(USA_Crimes_State[[1]]$year,USA_Crimes_State[[1]]$zmiany_Pcrimes,type = 'l',col = paint[1]
     ,main = "Zmiany PropertyCrimes cz3",xlab = "Year",ylab = "Zmiany",ylim = c(-50,100))
for (i in 35:51) {
  lines(USA_Crimes_State[[i]]$year,USA_Crimes_State[[i]]$zmiany_Pcrimes,col = paint[i])
}

#
# Exploracja danych
#
#
#przygotowanie danych (violent_crime ~ property_crime)
#
USA_CrimesEXP <- data.table(USA_Crimes$violent_crime,USA_Crimes$property_crime)
colnames(USA_CrimesEXP) <- c("violent_crimes","property_crimes")

#Wykres violent crimes do property crimes
dev.new()
scatter.smooth(x=USA_Crimes$violent_crime,y=USA_Crimes$property_crime,main="Violent crime ~ Property crime",xlab = "Violent crime",ylab = "Property Crime")

#Korelacja liniowa pearsona
#
pearson_kor <- cor.test(USA_Crimes$violent_crime,USA_Crimes$property_crime,method = "pearson")
pearson_kor
#
#Model regresji liniowej
#
USACrimesLinMod <- lm(violent_crimes ~ property_crimes,data = USA_CrimesEXP)
summary(USACrimesLinMod)
#
#Wspolczynnik determinacji i zbieznosci
#
wsDeterminacji <- summary(USACrimesLinMod)$r.squared
wsZbieznosci <- (1-wsDeterminacji)
#
#dzielenie na klastry
#
klastry <- kmeans(USA_CrimesEXP,3)
klastry
#
#Dendrogram
#
dd <- dist(scale(Avg_states[2:3]),method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
hdc <- as.dendrogram(hc)
dev.new()
plot(hdc,ylab = "Wysokosc",main = "Dendrogram Srednich")

