library(tidyverse)

rm(list=ls())

data <- read_csv('~/Desktop/9810000202_databaseLoadingData.csv') 

pop_2016<- data %>% 
  filter(`Population and dwelling counts (13)` == 'Population, 2016') %>% 
  transmute ('county' = GEO,
             'population' = `Population and dwelling counts (13)`,
             'total' = VALUE)

pop_2021 <- data %>% 
  filter(`Population and dwelling counts (13)` == 'Population, 2021') %>% 
  transmute ('county' = GEO,
             'population' = `Population and dwelling counts (13)`,
             'total' = VALUE)

pop <- pop_2016 %>% 
  transmute(county = county,
            pop_2016 = pop_2016$total,
            pop_2021 = pop_2021$total,
            dif = pop_2021-pop_2016,
            pct_change = round((pop_2021/pop_2016)-1,4),
            change = ifelse(dif < 0, 'decrease', 'increase')) %>% 
  filter(pct_change != 'NaN')

pop_prov <- pop %>% 
  filter(county == 'Newfoundland and Labrador'
         | county == 'Prince Edward Island'
         | county == 'Nova Scotia'
         | county == 'New Brunswick'
         | county == 'Quebec'
         | county == 'Ontario'
         | county == 'Manitoba'
         | county == 'Saskatchewan'
         | county == 'Alberta'
         | county == 'British Columbia'
         | county == 'Nunavut'
         | county == 'Northwestern Territories'
         | county == 'Yukon') %>% 
  arrange(desc(pct_change))

pop_county <- pop %>% 
  filter(county != 'Canada'
         & county != 'Newfoundland and Labrador'
         & county != 'Prince Edward Island'
         & county != 'Nova Scotia'
         & county != 'New Brunswick'
         & county != 'Quebec'
         & county != 'Ontario'
         & county != 'Manitoba'
         & county != 'Saskatchewan'
         & county != 'Alberta'
         & county != 'British Columbia'
         & county != 'Nunavut'
         & county != 'Northwestern Territories'
         & county != 'Yukon')

write_csv(pop, '~/Desktop/pop_change.csv')
