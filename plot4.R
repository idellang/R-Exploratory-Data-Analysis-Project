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




#get coal and combustion
coal_combustion = all_data %>%
      gather(key = 'scc_level', value = 'value', scc_level_one, scc_level_two, scc_level_three, scc_level_four) %>%
      filter(str_detect(value, 'Coal') | str_detect(value, 'Combustion'))

#Plot the data
graph4 = coal_combustion %>%
      mutate(year = factor(year)) %>%
      group_by(year) %>%
      summarise(total_emission = sum(emissions, na.rm = TRUE)) %>%
      ggplot(aes(year, total_emission))+
      geom_col(aes(fill = year)) +
      labs(title = 'Total PM2.5 Emissions from Coal or Combustion Sources', x = '', y = 'total PM2.5 emissions')+
      theme(legend.position = 'none')

#initialize plot
png(filename = 'plot4.png')

#plot
graph4

dev.off()