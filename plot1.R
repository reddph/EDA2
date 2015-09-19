# Question: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.

## Since the NEI CSV file being loaded is quite large, it is useful to ensure that it is loaded once 
## as csv but saved as rda (native R format) file which is more compact for loading. The code is designed 
## to detect the presence of the rda file in the current working directory for subsequent loading. If the 
## rda file is missing, logic requires a check if the zip file exists in the current working 
## directory. Downloading the zip file is initiated only if the zip file is missing. 

wd <- getwd()

if(!grepl("/EDA2",wd))
{
    if(!file.exists(file.path("./EDA2")))
    {
        dir.create("./EDA2")    
    }
    
    setwd("./EDA2/")
}

if(!file.exists(file.path("./NEI.rda")))
{
    if(!file.exists(file.path("./exdata_Fdata_FNEI_data.zip")))
    {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      destfile = "./exdata_Fdata_FNEI_data.zip")
    }
    
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")      
    
    save(NEI,file="./NEI.rda")
    save(SCC,file="./SCC.rda")
} else {
    load(file="./NEI.rda")
    load(file="./SCC.rda")
}

## load packages for easy date processing (lubridate) and filtering and summarization (dplyr)

library(lubridate)
library(dplyr)
library(png)

yearlyPM25 <- summarize(group_by(NEI,year),TotalEmissions=sum(Emissions))

png(filename="plot1.png", width=480, height=480, units="px")
with(yearlyPM25, plot(year, log10(TotalEmissions), main="Yearly PM2.5 Emissions from All U.S Sources",ylab="log10(Total PM2.5 Emissions)", type='p', xlab="year"))
model <- lm(log10(TotalEmissions) ~ year, yearlyPM25)
abline(model, lwd=2, col="Purple")

dev.off()

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# Create a new folder for the analysis and set the working directory to it. 
wd <- getwd()

if(!grepl("/EDA2",wd))
{
    if(!file.exists(file.path("./EDA2")))
    {
        dir.create("./EDA2")    
    }
    
    setwd("./EDA2/")
}

if(!file.exists(file.path("./NEI.rda")))
{
    if(!file.exists(file.path("./exdata_Fdata_FNEI_data.zip")))
    {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      destfile = "./exdata_Fdata_FNEI_data.zip")
    }
    
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")      
    
    save(NEI,file="./NEI.rda")
    save(SCC,file="./SCC.rda")
} else {
    load(file="./NEI.rda")
    load(file="./SCC.rda")
}

## load packages for easy date processing (lubridate) and filtering and summarization (dplyr)

library(lubridate)
library(dplyr)
library(png)

NEI_Baltimore <- filter(NEI, fips=="24510")
yearlyBaltPM25 <- summarize(group_by(NEI_Baltimore,year),TotalEmissions=sum(Emissions))

png(filename="plot2.png", width=480, height=480, units="px")
with(yearlyBaltPM25, plot(years, log10(TotalEmissions), main="Yearly Baltimore PM2.5 Emissions from All Sources",ylab="log10(Total PM2.5 Emissions)", type='p', xlab="year"))
model <- lm(log10(TotalEmissions) ~ year, yearlyBaltPM25)
abline(model, lwd=2, col="Blue")

dev.off()
# 
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a 
# plot answer this question.

wd <- getwd()

if(!grepl("/EDA2",wd))
{
    if(!file.exists(file.path("./EDA2")))
    {
        dir.create("./EDA2")    
    }
    
    setwd("./EDA2/")
}

if(!file.exists(file.path("./NEI.rda")))
{
    if(!file.exists(file.path("./exdata_Fdata_FNEI_data.zip")))
    {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      destfile = "./exdata_Fdata_FNEI_data.zip")
    }
    
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")      
    
    save(NEI,file="./NEI.rda")
    save(SCC,file="./SCC.rda")
} else {
    load(file="./NEI.rda")
    load(file="./SCC.rda")
}

library(lubridate)
library(dplyr)
library(png)

yearlyByTypeBaltPM25 <- summarize(group_by(NEI_Baltimore,year,type),TotalEmissions=sum(Emissions))

library(ggplot2)

png(filename="plot3.png", width=480, height=480, units="px")

g <- ggplot(yearlyByTypeBaltPM25, aes(year, log10(TotalEmissions),color=type))
g + geom_line(lwd=1) +
labs(title ="Yearly Baltimore PM2.5 Emissions by Source") + labs(x = "years", y = "log10(TotalEmissions)") 

dev.off()

# 
# Question: Across the United States, how have emissions from coal combustion-related sources changed 
# from 1999-2008?
# 

## Since the NEI CSV file being loaded is quite large, it is useful to ensure that it is loaded once 
## as csv but saved as rda (native R format) file which is more compact for loading. The code is designed 
## to detect the presence of the rda file in the current working directory for subsequent loading. If the 
## rda file is missing, logic requires a check if the zip file exists in the current working 
## directory. Downloading the zip file is initiated only if the zip file is missing.

wd <- getwd()

if(!grepl("/EDA2",wd))
{
    if(!file.exists(file.path("./EDA2")))
    {
        dir.create("./EDA2")    
    }
    
    setwd("./EDA2/")
}

if(!file.exists(file.path("./NEI.rda")))
{
    if(!file.exists(file.path("./exdata_Fdata_FNEI_data.zip")))
    {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      destfile = "./exdata_Fdata_FNEI_data.zip")
    }
    
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")      
    
    save(NEI,file="./NEI.rda")
    save(SCC,file="./SCC.rda")
} else {
    load(file="./NEI.rda")
    load(file="./SCC.rda")
}

## load packages for easy date processing (lubridate) and filtering and summarization (dplyr)

library(lubridate)
library(dplyr)
library(png)

# Add a column for EI.Sector related to Fuel Combustion
SCC2 <- mutate(SCC,Coal.Comb.Bool=grepl("Coal",EI.Sector))

# Fuel Combustion Source data set
FCS <- filter(SCC2,Coal.Comb.Bool==TRUE)

# Filter NEI by sources in fuel combustion source set
NEI_CoalComb <- filter(NEI,SCC %in% FCS$SCC)

yearlyPM25_CCS <- summarize(group_by(NEI_CoalComb,year),TotalEmissions=sum(Emissions))

png(filename="plot4.png", width=480, height=480, units="px")
with(yearlyPM25_CCS, plot(year, log10(TotalEmissions), main="Yearly PM2.5 Emissions from Coal Combustion Sources",ylab="log10(Coal Combustion PM2.5 Emissions)", type='p', xlab="year"))
model <- lm(log10(TotalEmissions) ~ year, yearlyPM25_CCS)
abline(model, lwd=2, col="Red")

dev.off()

# Question: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

## Since the NEI CSV file being loaded is quite large, it is useful to ensure that it is loaded once 
## as csv but saved as rda (native R format) file which is more compact for loading. The code is designed 
## to detect the presence of the rda file in the current working directory for subsequent loading. If the 
## rda file is missing, logic requires a check if the zip file exists in the current working 
## directory. Downloading the zip file is initiated only if the zip file is missing.

wd <- getwd()

if(!grepl("/EDA2",wd))
{
    if(!file.exists(file.path("./EDA2")))
    {
        dir.create("./EDA2")    
    }
    
    setwd("./EDA2/")
}

if(!file.exists(file.path("./NEI.rda")))
{
    if(!file.exists(file.path("./exdata_Fdata_FNEI_data.zip")))
    {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      destfile = "./exdata_Fdata_FNEI_data.zip")
    }
    
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")      
    
    save(NEI,file="./NEI.rda")
    save(SCC,file="./SCC.rda")
} else {
    load(file="./NEI.rda")
    load(file="./SCC.rda")
}

library(lubridate)
library(dplyr)
library(png)

NEI_Baltimore <- filter(NEI, fips=="24510")

# testing for the right filter
# SCC2 <- filter(SCC,SCC %in% unique(NEI_Baltimore$SCC))
# SCC.Level.Two.Baltimore <- summarize(group_by(SCC2,SCC.Level.Two),SRCCount=n())
# SCC.Level.Two.Baltimore[grep("Vehicle",SCC.Level.Two.Baltimore$SCC.Level.Two),]

# Add a column for EI.Sector related to Fuel Combustion
SCC2 <- mutate(SCC,MotorVehicles.Bool=grepl("Vehicle",SCC.Level.Two))

# Data set for Motor Vehicle sources in Baltimore
MotVehBalt <- filter(SCC2,MotorVehicles.Bool==TRUE)

# Filter NEI by sources in motor vehicle baltimore set
NEI_MotVehBalt <- filter(NEI_Baltimore,SCC %in% MotVehBalt$SCC)

# Let us designate MVB as name for motor vehicles in Baltimore set
yearlyPM25_MVB <- summarize(group_by(NEI_MotVehBalt,year),TotalEmissions=sum(Emissions))

png(filename="plot5.png", width=480, height=480, units="px")
with(yearlyPM25_MVB, plot(year, log10(TotalEmissions), main="Yearly PM2.5 Emissions from Motor Vehicle Sources in Baltimore",ylab="log10(Aggregate PM2.5 Emissions)", type='p', xlab="year"))
model <- lm(log10(TotalEmissions) ~ year, yearlyPM25_MVB)
abline(model, lwd=2, col="Red")

dev.off()


# Question: Compare emissions from motor vehicle sources in Baltimore City (fips == "24510") with emissions from motor 
# vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater
# changes over time in motor vehicle emissions?
# 

## Since the NEI CSV file being loaded is quite large, it is useful to ensure that it is loaded once 
## as csv but saved as rda (native R format) file which is more compact for loading. The code is designed 
## to detect the presence of the rda file in the current working directory for subsequent loading. If the 
## rda file is missing, logic requires a check if the zip file exists in the current working 
## directory. Downloading the zip file is initiated only if the zip file is missing.

wd <- getwd()

if(!grepl("/EDA2",wd))
{
    if(!file.exists(file.path("./EDA2")))
    {
        dir.create("./EDA2")    
    }
    
    setwd("./EDA2/")
}

if(!file.exists(file.path("./NEI.rda")))
{
    if(!file.exists(file.path("./exdata_Fdata_FNEI_data.zip")))
    {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      destfile = "./exdata_Fdata_FNEI_data.zip")
    }
    
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")      
    
    save(NEI,file="./NEI.rda")
    save(SCC,file="./SCC.rda")
} else {
    load(file="./NEI.rda")
    load(file="./SCC.rda")
}

library(lubridate)
library(dplyr)
library(png)

NEI_Baltimore_LA <- filter(NEI, fips %in% c("24510","06037"))

# Add a column for EI.Sector related to Fuel Combustion
SCC2 <- mutate(SCC,MotorVehicles.Bool=grepl("Vehicle",SCC.Level.Two))

# Data set for Motor Vehicle sources and LA
MotVehBaltLA <- filter(SCC2,MotorVehicles.Bool==TRUE)

# A simple function to map fips to city name for ggplot city labels
cityFromFips <- function(x) {
    city <- NA
    if(x=="24510") {
        city <- "Baltimore" 
    }
    else { 
        if (x=="06037") {
            city <- "LA"
        }
    }
    city
}

# Filter NEI by sources in motor vehicle Baltimore and LA data set
NEI_MotVehBaltLA <- filter(NEI_Baltimore_LA,SCC %in% MotVehBaltLA$SCC)

cityName <- unlist(lapply(NEI_MotVehBaltLA$fips,cityFromFips))

NEI_MotVehBaltLA <- mutate(NEI_MotVehBaltLA, city=cityName)
# Let us designate MVB name for motor vehicles in Baltimore and LA set
yearlyPM25_MVBLA <- summarize(group_by(NEI_MotVehBaltLA,year,city),TotalEmissions=sum(Emissions))

library(ggplot2)

png(filename="plot6.png", width=480, height=480, units="px")

g <- ggplot(yearlyPM25_MVBLA, aes(year, log10(TotalEmissions),color=city))
g + geom_line(lwd=1) +
    labs(title ="Yearly Baltimore and LA PM2.5 Motor Vehicular Emissions") + labs(x = "years", y = "log10(Total Motor Vehicular Emissions)") 

dev.off()








