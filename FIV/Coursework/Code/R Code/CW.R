library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(gridExtra)
library(tidyverse)
library(Stack)

SEP <- ","

R_DATA_GENERAL <<- "/Users/44785/Documents/R/CW/"

#----------------------------------#
# Load Data
#----------------------------------#

message("loading data...")

# full development indicator data for all 6 countries
fullIndicators <- read.table(paste(R_DATA_GENERAL, "world-development-indicators-SEA-1960-2019.csv", sep = ""), quote = "\"", header = T, comment.char = "", sep = SEP)

#----------------------------------#
# Manipulate Data
#----------------------------------#

message("filtering data...")

#separate the data into their respective countries
chinaIndicators <- subset(fullIndicators, CountryCode == "CHN")
indonesiaIndicators <- subset(fullIndicators, CountryCode == "IDN")
japanIndicators <- subset(fullIndicators, CountryCode == "JPN")
koreaIndicators <- subset(fullIndicators, CountryCode == "KOR")
philippinesIndicators <- subset(fullIndicators, CountryCode == "PHL")
thailandIndicators <- subset (fullIndicators, CountryCode == "THA")

#function to subset a singular series, taking the name as a parameter
subsetSingular <- function(countryIndicators, series, yearSelection){
  countryVar <- as.data.frame(t(as.matrix(select(filter(countryIndicators, SeriesName == series), all_of(yearSelection)))))
  countryVar <- tibble::rownames_to_column(countryVar, "Year")
  names(countryVar)[2] <- "Var"
  countryVar$Year <- substring(countryVar$Year, 2)
  countryVar$Var <- as.numeric(as.character(countryVar$Var))
  countryVar$Year <- as.numeric(as.character(countryVar$Year))
  countryVar <- subset(countryVar, !is.na(Var))
  return(countryVar)
}

#subset the GDPs for each country
chinaGDP <- subsetSingular(chinaIndicators, "GDP (constant 2010 US$)", 5:63)
indonesiaGDP <- subsetSingular(indonesiaIndicators, "GDP (constant 2010 US$)", 5:63)
japanGDP <- subsetSingular(japanIndicators, "GDP (constant 2010 US$)", 5:63)
koreaGDP <- subsetSingular(koreaIndicators, "GDP (constant 2010 US$)", 5:63)
philippinesGDP <- subsetSingular(philippinesIndicators, "GDP (constant 2010 US$)", 5:63)
thailandGDP <- subsetSingular(thailandIndicators, "GDP (constant 2010 US$)", 5:63)

#subset the life expectancies for each country
chinaLE <- subsetSingular(chinaIndicators, "Life expectancy at birth, total (years)", 5:62)
indonesiaLE <- subsetSingular(indonesiaIndicators, "Life expectancy at birth, total (years)", 5:62)
japanLE <- subsetSingular(japanIndicators, "Life expectancy at birth, total (years)", 5:62)
koreaLE <- subsetSingular(koreaIndicators, "Life expectancy at birth, total (years)", 5:62)
philippinesLE <- subsetSingular(philippinesIndicators, "Life expectancy at birth, total (years)", 5:62)
thailandLE <- subsetSingular(thailandIndicators, "Life expectancy at birth, total (years)", 5:62)

#subset the pupils in secondary education for each country
chinaSE <- suppressWarnings(subsetSingular(chinaIndicators, "Secondary education, pupils", 5:63))
indonesiaSE <- suppressWarnings(subsetSingular(indonesiaIndicators, "Secondary education, pupils", 5:63))
japanSE <- suppressWarnings(subsetSingular(japanIndicators, "Secondary education, pupils", 5:63))
koreaSE <- suppressWarnings(subsetSingular(koreaIndicators, "Secondary education, pupils", 5:63))
philippinesSE <- suppressWarnings(subsetSingular(philippinesIndicators, "Secondary education, pupils", 5:63))
thailandSE <- suppressWarnings(subsetSingular(thailandIndicators, "Secondary education, pupils", 5:63))

#subset the total greenhouse gas emissions for each country
chinaGGE <- suppressWarnings(subsetSingular(chinaIndicators, "Total greenhouse gas emissions (kt of CO2 equivalent)", 5:63))
indonesiaGGE <- suppressWarnings(subsetSingular(indonesiaIndicators, "Total greenhouse gas emissions (kt of CO2 equivalent)", 5:63))
japanGGE <- suppressWarnings(subsetSingular(japanIndicators, "Total greenhouse gas emissions (kt of CO2 equivalent)", 5:63))
koreaGGE <- suppressWarnings(subsetSingular(koreaIndicators, "Total greenhouse gas emissions (kt of CO2 equivalent)", 5:63))
philippinesGGE <- suppressWarnings(subsetSingular(philippinesIndicators, "Total greenhouse gas emissions (kt of CO2 equivalent)", 5:63))
thailandGGE <- suppressWarnings(subsetSingular(thailandIndicators, "Total greenhouse gas emissions (kt of CO2 equivalent)", 5:63))

#----------------------------------#
# Basic Analysis and Visualization
#----------------------------------#

message("RQ1: Which countries underwent rapid economic growth in the later half of the 20th Century?")

p11 <- ggplot(data=chinaGDP, aes(x=Year, y=Var)) + geom_line() + labs(title = "China", y="GDP (constant 2010 US$)") + theme(plot.title = element_text(hjust = 0.5))
p12 <- ggplot(data=indonesiaGDP, aes(x=Year, y=Var)) + geom_line() + labs(title = "Indonesia", y="GDP (constant 2010 US$)") + theme(plot.title = element_text(hjust = 0.5))
p13 <- ggplot(data=japanGDP, aes(x=Year, y=Var)) + geom_line() + labs(title = "Japan", y="GDP (constant 2010 US$)") + theme(plot.title = element_text(hjust = 0.5))
p14 <- ggplot(data=koreaGDP, aes(x=Year, y=Var)) + geom_line() + labs(title = "South Korea", y="GDP (constant 2010 US$)") + theme(plot.title = element_text(hjust = 0.5))
p15 <- ggplot(data=philippinesGDP, aes(x=Year, y=Var)) + geom_line() + labs(title = "Philippines", y="GDP (constant 2010 US$)") + theme(plot.title = element_text(hjust = 0.5))
p16 <- ggplot(data=thailandGDP, aes(x=Year, y=Var)) + geom_line() + labs(title = "Thailand", y="GDP (constant 2010 US$)") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p11, p12, p13, p14, p15, p16, ncol=3, nrow=2)


message("RQ2: Did life expectancy increase throughout this period?")

#merge GDP and life expectancy into one data frame

chinaLE <- merge(chinaGDP, chinaLE, by="Year")
indonesiaLE <- merge(indonesiaGDP, indonesiaLE, by="Year")
japanLE <- merge(japanGDP, japanLE, by="Year")
koreaLE <- merge(koreaGDP, koreaLE, by="Year")
philippinesLE <- merge(philippinesGDP, philippinesLE, by="Year")
thailandLE <- merge(thailandGDP, thailandLE, by="Year")

p21 <- ggplot(data=chinaLE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method="gam", formula = y ~ s(x, bs = "cs")) + labs(title = "China", x="GDP (constant 2010 US$)", y="Life Expectancy at birth, total (years)") + theme(plot.title = element_text(hjust = 0.5))
p22 <- ggplot(data=indonesiaLE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method="gam", formula = y ~ s(x, bs = "cs")) + labs(title = "Indonesia", x="GDP (constant 2010 US$)", y="Life Expectancy at birth, total (years)") + theme(plot.title = element_text(hjust = 0.5))
p23 <- ggplot(data=japanLE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method="gam", formula = y ~ s(x, bs = "cs")) + labs(title = "Japan", x="GDP (constant 2010 US$)", y="Life Expectancy at birth, total (years)") + theme(plot.title = element_text(hjust = 0.5))
p24 <- ggplot(data=koreaLE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method="gam", formula = y ~ s(x, bs = "cs")) + labs(title = "South Korea", x="GDP (constant 2010 US$)", y="Life Expectancy at birth, total (years)") + theme(plot.title = element_text(hjust = 0.5))
p25 <- ggplot(data=philippinesLE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method="gam", formula = y ~ s(x, bs = "cs")) + labs(title = "Philippines", x="GDP (constant 2010 US$)", y="Life Expectancy at birth, total (years)") + theme(plot.title = element_text(hjust = 0.5))
p26 <- ggplot(data=thailandLE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method="gam", formula = y ~ s(x, bs = "cs")) + labs(title = "Thailand", x="GDP (constant 2010 US$)", y="Life Expectancy at birth, total (years)") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p21, p22, p23, p24, p25, p26, ncol=3, nrow=2)

message("RQ3: Did education improve during this period?")

#merge GDP and pupils in secondary education into one data frame
chinaSE <- merge(chinaGDP, chinaSE, by="Year")
indonesiaSE <- merge(indonesiaGDP, indonesiaSE, by="Year")
japanSE <- merge(japanGDP, japanSE, by="Year")
koreaSE <- merge(koreaGDP, koreaSE, by="Year")
philippinesSE <- merge(philippinesGDP, philippinesSE, by="Year")
thailandSE <- merge(thailandGDP, thailandSE, by="Year")

p31 <- ggplot(data=chinaSE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "China", x="GDP (constant 2010 US$)", y="Secondary education, pupils") + theme(plot.title = element_text(hjust = 0.5))
p32 <- ggplot(data=indonesiaSE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "Indonesia", x="GDP (constant 2010 US$)", y="Secondary education, pupils") + theme(plot.title = element_text(hjust = 0.5))
#Japan omitted for lack of data
p34 <- ggplot(data=koreaSE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "South Korea", x="GDP (constant 2010 US$)", y="Secondary education, pupils") + theme(plot.title = element_text(hjust = 0.5))
p35 <- ggplot(data=philippinesSE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "Philippines", x="GDP (constant 2010 US$)", y="Secondary education, pupils") + theme(plot.title = element_text(hjust = 0.5))
p36 <- ggplot(data=thailandSE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "Thailand", x="GDP (constant 2010 US$)", y="Secondary education, pupils") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p31, p32, p34, p35, p36, ncol=2, nrow=3)

message("RQ4: Did pollution increase during this period?")

#merge GDP and total greenhouse gas emissions
chinaGGE <- merge(chinaGDP, chinaGGE, by="Year")
indonesiaGGE <- merge(indonesiaGDP, indonesiaGGE, by="Year")
japanGGE <- merge(japanGDP, japanGGE, by="Year")
koreaGGE <- merge(koreaGDP, koreaGGE, by="Year")
philippinesGGE <- merge(philippinesGDP, philippinesGGE, by="Year")
thailandGGE <- merge(thailandGDP, thailandGGE, by="Year")

p41 <- ggplot(data=chinaGGE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "China", x="GDP (constant 2010 US$)", y="Total greenhouse gas emissions (kt of CO2 equivalent)") + theme(plot.title = element_text(hjust = 0.5))
p42 <- ggplot(data=indonesiaGGE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "Indonesia", x="GDP (constant 2010 US$)", y="Total greenhouse gas emissions (kt of CO2 equivalent)") + theme(plot.title = element_text(hjust = 0.5))
p43 <- ggplot(data=japanGGE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "Japan", x="GDP (constant 2010 US$)", y="Total greenhouse gas emissions (kt of CO2 equivalent)") + theme(plot.title = element_text(hjust = 0.5))
p44 <- ggplot(data=koreaGGE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "South Korea", x="GDP (constant 2010 US$)", y="Total greenhouse gas emissions (kt of CO2 equivalent)") + theme(plot.title = element_text(hjust = 0.5))
p45 <- ggplot(data=philippinesGGE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "Philippines", x="GDP (constant 2010 US$)", y="Total greenhouse gas emissions (kt of CO2 equivalent)") + theme(plot.title = element_text(hjust = 0.5))
p46 <- ggplot(data=thailandGGE, aes(x=Var.x, y=Var.y)) + geom_point() + geom_smooth(method = "loess", formula = y ~ x) + labs(title = "Thailand", x="GDP (constant 2010 US$)", y="Total greenhouse gas emissions (kt of CO2 equivalent)") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p41, p42, p43, p44, p45, p46, ncol=3, nrow=2)

#----------------------------------------------#
# Exploratory Questions and New Visualizations
#----------------------------------------------#

message("FQ1: How does the economic growth of each country compare?")

# subset the GDP growth % for each country
gdpGrowth <- subsetSingular(fullIndicators, "GDP growth (annual %)", 6:63)

# rename each column correctly
names(gdpGrowth)[2] <- "China"
names(gdpGrowth)[3] <- "Japan"
names(gdpGrowth)[4] <- "South Korea"
names(gdpGrowth)[5] <- "Thailand"
names(gdpGrowth)[6] <- "Indonesia"
names(gdpGrowth)[7] <- "Philippines"

# transform the columns to numeric data
gdpGrowth$Indonesia <- as.numeric(as.character(gdpGrowth$Indonesia))
gdpGrowth$Japan <- as.numeric(as.character(gdpGrowth$Japan))
gdpGrowth$`South Korea` <- as.numeric(as.character(gdpGrowth$`South Korea`))
gdpGrowth$Philippines <- as.numeric(as.character(gdpGrowth$Philippines))
gdpGrowth$Thailand <- as.numeric(as.character(gdpGrowth$Thailand))

#remove the year column and sum the others
gdpGrowth <- select(gdpGrowth, -c("Year"))
gdpGrowth <- as.data.frame(colSums(gdpGrowth))
gdpGrowth <- tibble::rownames_to_column(gdpGrowth, "Country")
names(gdpGrowth)[2] <- "sum"

ggplot(data=gdpGrowth, aes(x=Country, y=sum)) + geom_bar(stat="identity") + labs(y="Sum of GDP growth (annual %)")

message("FQ2: Why do some countries have higher life expectancies than others?")

#subset each series
healthData1 <- select(filter(fullIndicators, SeriesName == "Hospital beds (per 1,000 people)"), 1, 3, 50:55)
healthData2 <- select(filter(fullIndicators, SeriesName == "Nurses and midwives (per 1,000 people)"), 1, 3, 50:55)
healthData3 <- select(filter(fullIndicators, SeriesName == "Physicians (per 1,000 people)"), 1, 3, 50:55)

#stack the data together
healthData <- Stack(healthData1, healthData2)
healthData <- Stack(healthData, healthData3)
names(healthData)[1] <- "Country"

#transform the columns to numeric values
healthData$X2005 <- as.numeric(as.character(healthData$X2005))
healthData$X2006 <- as.numeric(as.character(healthData$X2006))
healthData$X2007 <- as.numeric(as.character(healthData$X2007))
healthData$X2008 <- as.numeric(as.character(healthData$X2008))
healthData$X2009 <- as.numeric(as.character(healthData$X2009))
healthData$X2010 <- as.numeric(as.character(healthData$X2010))

#take the mean values (excluding N/A0 and remove the columns for each year)
healthData$Mean <- rowMeans(healthData[,3:8], na.rm=TRUE)
healthData <- select(healthData, -c(3:8))

ggplot(data=healthData, aes(fill=SeriesName, x=Country, y=Mean)) + geom_bar(position="dodge", stat="identity") + labs(y="Mean (2005-2010) per 1,000 people")

message("FQ3: In China and South Korea, was the drop in secondary education pupils a result of a lower population in the secondary education age range?")

#subset for each country and merge the two
educationChina <- subsetSingular(chinaIndicators, "Population ages 0-14, total", 5:63)
educationKorea <- subsetSingular(koreaIndicators, "Population ages 0-14, total", 5:63)
educationAgeRange <- merge(educationChina, educationKorea, by="Year")
names(educationAgeRange)[2] <- "China"
names(educationAgeRange)[3] <- "Korea"

p71 <- ggplot(data=educationAgeRange, aes(x=Year)) + geom_line(aes(y=China)) + labs(title="China", y="Population ages 0-14, total") + theme(plot.title = element_text(hjust = 0.5))
p72 <- ggplot(data=educationAgeRange, aes(x=Year)) + geom_line(aes(y=Korea)) + labs(title="South Korea", y="Population ages 0-14, total") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p71, p72, ncol=2)

message("FQ4: Which sectors were the biggest contributors to CO2 emissions in China?")

#data to be used in the interactive visualization
pollutionChina1 <- filter(chinaIndicators, SeriesName == "CO2 emissions from electricity and heat production, total (% of total fuel combustion)")
pollutionChina2 <- filter(chinaIndicators, SeriesName == "CO2 emissions from manufacturing industries and construction (% of total fuel combustion)")
pollutionChina3 <- filter(chinaIndicators, SeriesName == "CO2 emissions from other sectors, excluding residential buildings and commercial and public services (% of total fuel combustion)")
pollutionChina4 <- filter(chinaIndicators, SeriesName == "CO2 emissions from residential buildings and commercial and public services (% of total fuel combustion)")
pollutionChina5 <- filter(chinaIndicators, SeriesName == "CO2 emissions from transport (% of total fuel combustion)")

pollutionChina <- Stack(pollutionChina1, pollutionChina2)
pollutionChina <- Stack(pollutionChina, pollutionChina3)
pollutionChina <- Stack(pollutionChina, pollutionChina4)
pollutionChina <- Stack(pollutionChina, pollutionChina5)

pollutionChina <- as.data.frame(t(as.matrix(select(select(pollutionChina, -c(1:15)), -c(45:49)))))
pollutionChina <- tibble::rownames_to_column(pollutionChina, "Year")
pollutionChina$Year <- substring(pollutionChina$Year, 2)
names(pollutionChina)[2] <- "Electricity & Heat"
names(pollutionChina)[3] <- "Manufacturing Industries & Construction"
names(pollutionChina)[4] <- "Other sectors"
names(pollutionChina)[5] <- "Residential Buildings and Commercial and Public Services"
names(pollutionChina)[6] <- "Transport"

write.csv(pollutionChina,"C:/Users/44785/Documents/R/CW/chinaPollution.csv", row.names = FALSE)


