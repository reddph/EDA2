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