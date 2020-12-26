#load libraries
library(tidyverse)
library(janitor)
library(data.table)
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


#get graph3
graph3 = NEI %>%
      filter(fips == '24510') %>%
      group_by(year, type) %>%
      summarise(total_emission = sum(emissions, na.rm = TRUE)) %>%
      mutate(type = factor(type),
             year = factor(year)) %>%
      ggplot(aes(year, total_emission)) +
      geom_col(aes(fill = type))+
      facet_grid(.~ type, scales = 'free', space = 'free')+
      labs(title = 'Yearly total PM 2.5 Emission in Baltimore City per Source', x ='', y = 'total PM2.5 emissions') +
      theme(legend.position = 'none')

#initialize plot
png(filename = 'plot3.png')

#plot
graph3

dev.off()