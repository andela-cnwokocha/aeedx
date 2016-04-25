library(DescTools)
library(readr)
library(lubridate)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(dplyr)
library(ggvis)

## Unit 1: Part One
crime <- read_csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/mvtWeek1.csv"))
str(crime)
Desc(crime)
head(crime)
max(crime$ID)
min(crime$Beat)
table(crime$Arrest)
table(crime$LocationDescription)
length(crime$LocationDescription[crime$LocationDescription %in% "ALLEY"])
crime$Date[1]
crime$newDate <- as.Date(strptime(crime$Date, "%m/%d/%y %H:%M"))
head(crime$newDate)
str(crime$newDate)
length(unique(crime$newDate))
table(year(crime$newDate)) 
periods <- table(months(crime$newDate), weekdays(crime$newDate))
periods
todanger_palette <- colorRampPalette(c("green", "yellow", "red"))(n = 299)
heatmap.2(periods)
format(crime$newDate[which(crime$newDate == median(crime$newDate))][1], "%B %d, %Y")
table(crime$Domestic, months(crime$newDate))
which.min(table(months(crime$newDate)))
which.max(table(weekdays(crime$newDate)))
which.max(table(crime$Arrest, months(crime$newDate))[2, ])
crime_hist <- ggplot(data = crime, aes(newDate, ..count..))
crime_hist +
  geom_histogram(binwidth = 60, colour = "white")
head(crime)
crime_boxplot <- ggplot(crime, aes(Arrest, newDate))
crime_boxplot +
  geom_boxplot() # first half
with(crime, boxplot(newDate ~ Arrest)) 
crime %>%
  select(Arrest, Year) %>%
  filter(Year == 2001) %>%
  table %>%
  prop.table

crime %>%
  select(Arrest, Year) %>%
  filter(Year == 2012) %>%
  table %>%
  prop.table

top5_locations = c(names(sort(table(crime$LocationDescription), decreasing = T)[1:6])[-3])
top5s <- subset(crime, LocationDescription %in% top5_locations, select = c(LocationDescription, Arrest))
table(top5s) / rowSums(table(top5s))

crime %>%
  mutate(days = weekdays(newDate)) %>%
  filter(LocationDescription == "GAS STATION") %>%
  select(days) %>%
  table()

crime %>%
  mutate(days = weekdays(newDate)) %>%
  filter(LocationDescription == top5_locations[5]) %>%
  select(days) %>%
  table()


## Part Two
ibm_stock <- read.csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/IBMStock.csv"))
ge_stock <- read.csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/GEStock.csv"))
procGamble_stock <- read.csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/ProcterGambleStock.csv"))
cocacola_stock <- read.csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/CocaColaStock.csv"))
boeing_stock <- read.csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/BoeingStock.csv"))

cocacola_stock$yearVals = year(cocacola_stock$NewDate)
procGamble_stock$yearVals = year(cocacola_stock$NewDate)
ibm_stock$yearVals = year(ibm_stock$NewDate)
boeing_stock$yearVals = year(boeing_stock$NewDate)
ge_stock$yearVals = year(ge_stock$NewDate)

cocacola_stock$NewDate <- as.Date(cocacola_stock$Date, "%m/%d/%y")
procGamble_stock$NewDate <- as.Date(procGamble_stock$Date, "%m/%d/%y")
ge_stock$NewDate <- as.Date(ge_stock$Date, "%m/%d/%y")
ibm_stock$NewDate <- as.Date(ibm_stock$Date, "%m/%d/%y")
boeing_stock$NewDate <- as.Date(boeing_stock$Date, "%m/%d/%y")

all_data <- list(ibm_stock, ge_stock, procGamble_stock, cocacola_stock, boeing_stock)
names(all_data) <- c("IBM", "General Electric", "Proctor and Gamble", "Coca Cola", "Boeing")
names(all_data[5])

changeToDate <- function(vectorOfDate) {
  dateVals <- as.Date(vectorOfDate[,1], "%m/%d/%y")
  return(dateVals)
}

lapply(lapply(all_data, changeToDate), year)
lapply(all_data, function(company) {
  return(mean(company[,2]))
})
min(ge_stock$StockPrice)
max(cocacola_stock$StockPrice)
median(boeing_stock$StockPrice)
sd(procGamble_stock$StockPrice)

cc_plot <- ggplot(cocacola_stock, aes(NewDate, StockPrice,  label = year(NewDate)))
cc_plot +
  geom_line() + 
  geom_point() +
  geom_text(aes(label=ifelse(year(NewDate) == 1972, year(NewDate), " ")),hjust=0, vjust=0)

cocacola_stock %>%
  ggvis(~NewDate, ~StockPrice, stroke = ~factor(yearVals), 
        shape = ~factor(weekdays(NewDate))) %>%
  layer_lines() %>%
  layer_points()

cocacola_stock %>%
  ggvis(~NewDate, ~StockPrice, stroke := ~factor(yearVals)) %>%
  layer_lines()

summary(cocacola_stock$StockPrice)
summary(procGamble_stock$StockPrice)
which(cocacola_stock$NewDate == "2000-03-01")
which(procGamble_stock$NewDate == "2000-03-01")
year1983 = subset(cocacola_stock, yearVals == 1983)
startYear = format(year1983$NewDate[1], "%B %d, %Y")
endYear = format(year1983$NewDate[nrow(year1983)], "%B %d, %Y")

cocacola_proctor_plot <- ggplot(mapping = aes(NewDate, StockPrice))
cocacola_proctor_plot + 
  geom_line(data = cocacola_stock,  aes( colour = "Cocacola")) +
  geom_line(data = procGamble_stock, aes( colour = "Proctor Gamble"), position = "jitter") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2000-03-01"))), colour = "blue",
             linetype = 4) +
  geom_text(mapping=aes(x=as.Date("2000-03-01"), y=0, label="March 3, 2000"),
            size=4, angle=90, vjust=-0.4, hjust=0) +
  geom_vline(aes(xintercept = as.numeric(as.Date(year1983$NewDate[1]))),
             colour = "blue", linetype = 4) +
  geom_text(mapping=aes(x=as.Date(year1983$NewDate[1]), y=0, label=startYear),
            size=4, angle=90, vjust=-0.4, hjust=0) +
  geom_vline(aes(xintercept = as.numeric(as.Date(year1983$NewDate[nrow(year1983)]))),
             colour = "blue", linetype = 4) +
  geom_text(mapping=aes(x=as.Date(year1983$NewDate[nrow(year1983)]), y=0, label=endYear),
            size=4, angle=90, vjust=1, hjust=0) +
  theme_bw()

## Plotting line graph of all companies stock. Given the organisation of the data, I'm deciding to use the data of one of the company
year_1995To2005 = subset(cocacola_stock, yearVals %in% c(1995:2005))
startYear = format(year_1995To2005$NewDate[1], "%B %d, %Y")
endYear = format(year_1995To2005$NewDate[nrow(year_1995To2005)], "%B %d, %Y")

for (i in 1:length(all_data)) {
 all_data[[i]] = subset(all_data[[i]], yearVals %in% c(1995:2005))
}
names(all_data) <- c("IBM", "General Electric", "Proctor and Gamble", "Coca Cola", "Boeing") -> companies

all_companies_plot <- ggplot(mapping = aes(NewDate, StockPrice))
all_companies_plot +
  geom_line(data = all_data[[1]],  aes( colour = companies[1])) +
  geom_line(data = all_data[[2]], aes( colour = companies[2])) +
  geom_line(data = all_data[[3]], aes(colour = companies[3])) +
  geom_line(data = all_data[[4]], aes( colour = companies[4])) +
  geom_line(data = all_data[[5]], aes( colour = companies[5])) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2000-03-01"))),
             colour = "blue", linetype = 4) +
  geom_text(mapping=aes(x=as.Date("2000-03-01"), y=0, label="March 01, 2000"),
            size=4, angle=90, vjust=-0.4, hjust=0) +
  geom_vline(aes(xintercept = as.numeric(as.Date("1997-09-01"))),
             colour = "black", linetype = 4) +
  geom_text(mapping=aes(x=as.Date("1997-09-01"), y=0, label="September 01, 1997"),
            size=4, angle=90, vjust=-0.4, hjust=0) +
  geom_vline(aes(xintercept = as.numeric(as.Date("1997-11-01"))),
             colour = "black", linetype = 4) +
  geom_text(mapping=aes(x=as.Date("1997-11-01"), y=0, label="November 01, 1997"),
            size=4, angle=90, vjust=1, hjust=0) +
  guides(colour=guide_legend(title="Companies")) +
  theme_bw()
  
# Instantiate all_data again to be the full dataset (and names), not the 1997 - 2005 subset 
companies_stockMean <- sapply(all_data, function(x) {
  val = tapply(x$StockPrice, months(x$NewDate), mean)
  append(val, mean(x$StockPrice))
})
ibm = companies_stockMean[,1] 
which(ibm > ibm[length(ibm)])
companies_stockMean[, c("General Electric", "Coca Cola")]

## Part Three
cpsdata <- read_csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/CPSData.csv"))
head(cpsdata)
str(cpsdata)
which.max(table(cpsdata$Industry))
which.min(table(cpsdata$State))
which.max(table(cpsdata$State))
props <- prop.table(table(cpsdata$Citizenship))
sum(props[1], props[2])
table(cpsdata$Hispanic, cpsdata$Race)[2,] 
sapply(cpsdata, function(x) {
  table(is.na(x))
})
prop.table(table(cpsdata$Region, is.na(cpsdata$Married)),2)
prop.table(table(cpsdata$Sex, is.na(cpsdata$Married)),2)
tapply(cpsdata$Age, is.na(cpsdata$Married), length) / nrow(cpsdata)
heatmap(prop.table(table(cpsdata$Age, is.na(cpsdata$Married)), 2))
prop.table(table(cpsdata$Citizenship, is.na(cpsdata$Married)), 2)
which.max(prop.table(table(cpsdata$State, is.na(cpsdata$MetroAreaCode))[,2]))
which.max(prop.table(table(cpsdata$Region, is.na(cpsdata$MetroAreaCode)))[,2])
nonmetro <- tapply(is.na(cpsdata$MetroAreaCode), cpsdata$State, mean)
nonmetro[which(nonmetro > .29 & nonmetro <= .3)]
sort(nonmetro, decreasing = T)[3] ## since there're just two states with all citizens non-metro
cpsdata
metrocodes <- read_csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/MetroAreaCodes.csv"))
countrycodes <- read_csv(url("https://d37djvu3ytnwxt.cloudfront.net/asset-v1:MITx+15.071x_3+1T2016+type@asset+block/CountryCodes.csv"))
nrow(metrocodes)
nrow(countrycodes)
cpsdata <- merge(cpsdata, metrocodes, by.x = "MetroAreaCode", by.y = "Code", all.x = TRUE)
dim(cpsdata)
colnames(cpsdata)
table(is.na(cpsdata$MetroArea))
head(sort(table(cpsdata$MetroArea), decreasing = T), 20)
sort(tapply(cpsdata$Hispanic, cpsdata$MetroArea, mean), decreasing = T)[1]
table(cpsdata$MetroArea, cpsdata$Hispanic)
asians <- (table(cpsdata$MetroArea, cpsdata$Race == "Asian"))
length(which(asians[,2]/rowSums(asians) > .2))
which.min(tapply(cpsdata$Education == "No high school diploma", cpsdata$MetroArea, mean, na.rm = T))
cpsdata <- merge(cpsdata, countrycodes, by.x = "CountryOfBirthCode", by.y = "Code", all.x = TRUE)
table(cpsdata$Country)
table(is.na(cpsdata$Country))
sort(table(cpsdata$Country), decreasing = T)
nyj <- subset(cpsdata, MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA")
prop.table(table(nyj$Country != "United States"))
countryMetroPopulation <- function(country) {
  cpsdata %>%
    filter(Country == country) %>%
    select(MetroArea) %>%
    table() %>%
    sort(decreasing = T) %>%
    head(1)
}
countryMetroPopulation("India")
countryMetroPopulation("Brazil")
countryMetroPopulation("Somalia")
