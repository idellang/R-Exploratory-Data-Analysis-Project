#load libraries
library(tidyverse)
library(janitor)
library(data.table)
library(lubridate)
library(stringr)

#load data
NEI <- as_tibble(readRDS("summarySCC_PM25.rds")) 
SCC <- as_tibble(readRDS("Source_Classification_Code.rds"))

#clean names
all_data = clean_names(all_data)
SCC = clean_names(SCC)
NEI = clean_names(NEI)

#join data
all_data = NEI %>%
      left_join(SCC, by = 'SCC')


#compute yearly total
baltimore_total_yearly = NEI %>%
      filter(fips == '24510') %>%
      group_by(year) %>%
      summarise(total_emission = sum(emissions, na.rm = TRUE))

#initialize plot
png(filename = 'plot2.png')

#plot
with(baltimore_total_yearly, barplot(total_emission, names = year, ylab = 'total PM2.5 emissions', main = 'Total Annual Emission from 1999 to 2008 in Baltimore City'))

dev.off()