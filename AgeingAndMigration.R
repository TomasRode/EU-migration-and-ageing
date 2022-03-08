################  IMPORT LIBRARIES #########################
library(readr)
library(dplyr)
library(readxl)
library(countrycode)
library(tidyr)
library(ggplot2)
library(plotly)

################ IMPORT DATA ###############################
EU28_codes <- c('BE', 'BG', 'CZ', 'DK', 'DE', 'EE', 'IE', 'EL', 'ES', 'FR', 'HR', 'IT', 'CY', 
                'LV', 'LT', 'LU', 'HU', 'MT', 'NL', 'AT', 'PL', 'PT', 'RO', 'SI', 'SK', 'FI', 'SE', 'UK')
EFTA_codes <- c('IS', 'LI', 'NO', 'CH')

migration_flows <- read.csv2("data/Migration_flows.csv", sep = ",") %>% filter(agedef == "REACH") %>% select("citizen", "age","sex","geo", "TIME_PERIOD", "OBS_VALUE")
#migration_flows$EU_EFTA <- (migration_flows$citizen %in% EU28_codes) | (migration_flows$citizen %in% EFTA_codes)
migration_flows$OBS_VALUE <- as.numeric(migration_flows$OBS_VALUE)

LFP <- read_excel("data/LFP.xlsx") %>% rename(Country = ...1) %>% head(-1) %>% select(-"...16") %>% gather(Year, Participation, colnames(.)[-1])
LFP$Country <- countrycode(LFP$Country, "country.name","iso2c")
LFP$Year <- as.numeric(LFP$Year)
LFP$Participation <- as.numeric(LFP$Participation)

Tertiary <- read_excel("data/Data_Tertiary.xlsx") %>% rename(Country = ...1) %>% head(-1) %>% select(-"...16") %>% gather(Year, Tertiary, colnames(.)[-1])
Tertiary$Country <- countrycode(Tertiary$Country, "country.name","iso2c")
Tertiary$Year <- as.numeric(Tertiary$Year)
Tertiary$Tertiary <- as.numeric(Tertiary$Tertiary)

Population <- read_excel("data/Population.xlsx") %>% rename(Country = ...1) %>% head(-1) %>% select(-"...8") %>% gather(Year, Population, colnames(.)[-1])
Population$Country <- countrycode(Population$Country, "country.name","iso2c")
Population$Year <- as.numeric(Population$Year)
Population$Population <- as.numeric(gsub(",", "", Population$Population))

Age <- read_excel("data/Age_1564.xlsx") %>% rename(Country = ...1) %>% head(-1) %>% select(-"...16") %>% gather(Year, Age, colnames(.)[-1])
Age$Country <- countrycode(Age$Country, "country.name","iso2c")
Age$Year <- as.numeric(Age$Year)
Age$Age <- as.numeric(Age$Age)



### SPLIT PRE/POST 2013 BECAUSE OF CHANGE IN DATA GATHERING

migration_EU_pre2013 <- migration_flows %>% filter(citizen %in% EU28_codes, geo %in% EU28_codes & TIME_PERIOD < 2013) %>% 
  group_by(age, sex, geo, TIME_PERIOD) %>% summarise(OBS_VALUE = sum(OBS_VALUE)) %>%
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareEUImmigrants = OBS_VALUE / Population) %>% 
  select(-"OBS_VALUE")

migration_EFTA_pre2013 <- migration_flows %>% filter(citizen %in% EFTA_codes, geo %in% EU28_codes & TIME_PERIOD < 2013) %>% 
  group_by(age, sex, geo, TIME_PERIOD) %>% summarise(OBS_VALUE = sum(OBS_VALUE)) %>%
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareEFTAImmigrants = OBS_VALUE / Population)  %>% 
  select(-"OBS_VALUE")

migration_EU_post2013 <- migration_flows %>% filter(citizen == "EU28_FOR" & TIME_PERIOD >= 2013) %>%
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareEUImmigrants = OBS_VALUE / Population) %>% 
  select(-"citizen", -"OBS_VALUE")

migration_EFTA_post2013 <- migration_flows %>% filter(citizen == "EFTA_FOR" & TIME_PERIOD >= 2013) %>%
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareEFTAImmigrants = OBS_VALUE / Population)  %>% 
  select(-"citizen", -"OBS_VALUE")

migration_TOTAL  <- migration_flows %>% filter(citizen == "TOTAL", geo %in% EU28_codes) %>%
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareTOTALImmigrants = OBS_VALUE / Population)  %>% 
  select(-"citizen", -"OBS_VALUE")

min_TOTAL <- migration_TOTAL %>% group_by(geo) %>% summarise(Minimum = min(TIME_PERIOD))

migration_EU <-rbind(migration_EU_pre2013, migration_EU_post2013)
migration_EFTA <- rbind(migration_EFTA_pre2013, migration_EFTA_post2013)

migration_data <- left_join(migration_EU, migration_EFTA, by=c("age", "sex", "geo", "TIME_PERIOD")) %>% left_join(migration_TOTAL)


#### ALTERNATIVE MIGRATION DATA

migration_EU_alter <- migration_flows %>% filter(citizen %in% EU28_codes, geo %in% EU28_codes) %>% 
  group_by(age, sex, geo, TIME_PERIOD) %>% summarise(OBS_VALUE = sum(OBS_VALUE)) %>%
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareEUImmigrants = OBS_VALUE / Population) %>% 
  select(-"OBS_VALUE")

migration_EFTA_alter <- migration_flows %>% filter(citizen %in% EFTA_codes, geo %in% EU28_codes) %>% 
  group_by(age, sex, geo, TIME_PERIOD) %>% summarise(OBS_VALUE = sum(OBS_VALUE)) %>%
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareEFTAImmigrants = OBS_VALUE / Population)  %>% 
  select(-"OBS_VALUE")

migration_data_alter <- left_join(migration_EU_alter, migration_EFTA_alter, by=c("age", "sex", "geo", "TIME_PERIOD")) %>% 
  left_join(migration_TOTAL) %>% select(-"Population.x", -"Population.y", -"Population")

## Data visualization

#Age graph
Age_graph <- ggplot(Age, aes(x=Year, y=Age, color=Country)) + geom_line() + 
  labs(title = "Share of pupulation aged 15 to 64 by EU countries") +
  ylab("Share")
ggplotly(Age_graph)

#Interesting selection -- LU, PL, DE, SI

#Labor Force Participation
LFP_graph <- ggplot(LFP, aes(x=Year, y=Participation, color=Country)) + geom_line() + 
  labs(title = "Labor force participation by EU countries") +
  ylab("Participation share")
ggplotly(LFP_graph)


# Migration intra-EU/EFTA
migration_EU_EFTA <- migration_data_alter %>% mutate(EU_EFTA_Share = ShareEUImmigrants + ShareEFTAImmigrants)

migration_EU_EFTA_graph <- ggplot(migration_EU_EFTA %>% filter(age == "TOTAL" & sex == "T"), aes(x=TIME_PERIOD, y=EU_EFTA_Share, color=geo)) + geom_line()
ggplotly(migration_EU_EFTA_graph)

# Migration total
migration_TOTAL_graph <- ggplot(migration_TOTAL %>% filter(age == "TOTAL" & sex == "T"), aes(x=TIME_PERIOD, y=ShareTOTALImmigrants, color=geo)) + geom_line()
ggplotly(migration_TOTAL_graph)

### DATA for first regression
data_reg_1 <- migration_data_alter %>% left_join(LFP, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% 
  filter(age == "TOTAL", sex=="T")  %>% left_join(Age, by = c("geo" = "Country", "TIME_PERIOD" = "Year"))

reg1 <- lm(Participation ~ Age + ShareTOTALImmigrants - 1, data_reg_1)
summary(reg1)

data_reg_2 <- migration_data_alter %>% left_join(LFP, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% 
  filter(age == "TOTAL", sex=="T")  %>% left_join(Age, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% 
  mutate(EU_EFTA_Share = ShareEUImmigrants + ShareEFTAImmigrants) %>% mutate(Non_EU_EFTA_Share = ShareTOTALImmigrants - EU_EFTA_Share)

reg2 <- lm(Participation ~ Age + EU_EFTA_Share + Non_EU_EFTA_Share - 1, data_reg_2)
summary(reg2)

reg3 <- lm(Participation ~ Age + Non_EU_EFTA_Share - 1, data_reg_2)
summary(reg3)


