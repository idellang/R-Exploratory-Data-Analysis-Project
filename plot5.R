#load libraries
library(tidyverse)
library(janitor)
library(data.table)
library(stringr)

#load data
NEI <- as_tibble(readRDS("summarySCC_PM25.rds")) 
SCC <- as_tibble(readRDS("Source_Classification_Code.rds"))

#join data
all_data = NEI %>%
      left_join(SCC, by = 'SCC')

#clean names
all_data = clean_names(all_data)
SCC = clean_names(SCC)
NEI = clean_names(NEI)




#get baltimore vehicles
baltimore_vehicles = all_data %>%
      gather(key = 'scc_level', value = 'value', scc_level_one, scc_level_two, scc_level_three, scc_level_four) %>%
      filter(grepl('vehicle',value, ignore.case = TRUE)) %>%
      filter(fips == '24510')

#Plot the data
graph5 = baltimore_vehicles %>%
      mutate(year = factor(year)) %>%
      group_by(year) %>%
      summarise(total_emission = sum(emissions, na.rm = TRUE)) %>%
      ggplot(aes(year, total_emission))+
      geom_col(aes(fill = year))+
      labs(title = 'Yearly Total PM2.5 Emission from Vehicles in Baltimore City', 
           x = '',
           y = 'total PM2.5 emissions')+
      theme(legend.position = 'none')

#initialize plot
png(filename = 'plot5.png')

#plot
graph5

dev.off()