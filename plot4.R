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
