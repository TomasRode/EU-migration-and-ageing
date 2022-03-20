################  IMPORT LIBRARIES #########################
library(readr)
library(dplyr)
library(readxl)
library(countrycode)
library(ggplot2)
library(ggpubr)
library(plotly)
library(plm)
library(tidyr)
library(conflicted)
library(scales)
library(stargazer)
library(broom)
library(kableExtra)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

################ IMPORT DATA ###############################
EU28_codes <- c('BE', 'BG', 'CZ', 'DK', 'DE', 'EE', 'IE', 'EL', 'ES', 'FR', 'HR', 'IT', 'CY', 
                'LV', 'LT', 'LU', 'HU', 'MT', 'NL', 'AT', 'PL', 'PT', 'RO', 'SI', 'SK', 'FI', 'SE', 'UK')
EFTA_codes <- c('IS', 'LI', 'NO', 'CH')

imigration_flows <- read.csv2("data/Imigration_flows.csv", sep = ",") %>% filter(agedef == "REACH") %>% 
  select("citizen", "age","sex","geo", "TIME_PERIOD", "OBS_VALUE") %>%
  rename("Imigration" = "OBS_VALUE")
imigration_flows$Imigration <- as.numeric(imigration_flows$Imigration)


emigration_flows <- read.csv2(file = 'data/Emigration_flows.csv', sep = ',', header = TRUE) %>% filter(agedef == "REACH") %>% 
  select("citizen", "age","sex","geo", "TIME_PERIOD", "OBS_VALUE") %>%
  rename("Emigration" = "OBS_VALUE")
emigration_flows$Emigration <- as.numeric(emigration_flows$Emigration)

migration_flows <- inner_join(imigration_flows, emigration_flows, by= c("citizen", "age", "sex", "TIME_PERIOD", "geo")) %>%
  mutate(NetMigration = Imigration - Emigration)

#migration_flows$EU_EFTA <- (migration_flows$citizen %in% EU28_codes) | (migration_flows$citizen %in% EFTA_codes)


LFP <- read_excel("data/LFP.xlsx") %>% rename(Country = ...1) %>% head(-1) %>% select(-"...16") %>% gather(Year, Participation, colnames(.)[-1])
LFP$Country <- countrycode(LFP$Country, "country.name","iso2c")
LFP$Year <- as.numeric(LFP$Year)
LFP$Participation <- as.numeric(LFP$Participation)

LFP <-  LFP %>% group_by(Country) %>% mutate(Participation_diff = Participation - dplyr::lag(Participation)) %>% ungroup()

Tertiary <- read_excel("data/Data_Tertiary.xlsx") %>% rename(Country = ...1) %>% head(-1) %>% select(-"...16") %>% gather(Year, Tertiary, colnames(.)[-1])
Tertiary$Country <- countrycode(Tertiary$Country, "country.name","iso2c")
Tertiary$Year <- as.numeric(Tertiary$Year)
Tertiary$Tertiary <- as.numeric(Tertiary$Tertiary)

Tertiary <- Tertiary %>% group_by(Country) %>% mutate(Tertiary_diff = Tertiary - lag(Tertiary)) %>% ungroup()

Population <- read_excel("data/Population.xlsx") %>% rename(Country = ...1) %>% head(-1) %>% select(-"...8") %>% gather(Year, Population, colnames(.)[-1])
Population$Country <- countrycode(Population$Country, "country.name","iso2c")
Population$Year <- as.numeric(Population$Year)
Population$Population <- as.numeric(gsub(",", "", Population$Population))

Age <- read_excel("data/Age_1564.xlsx") %>% rename(Country = ...1) %>% head(-1) %>% select(-"...16") %>% gather(Year, Age, colnames(.)[-1])
Age$Country <- countrycode(Age$Country, "country.name","iso2c")
Age$Year <- as.numeric(Age$Year)
Age$Age <- as.numeric(Age$Age)

Age <- Age %>% group_by(Country) %>% mutate(Age_diff = Age - lag(Age)) %>% ungroup()

Employment <- read_excel("data/Employment.xlsx") %>% rename(Country = ...1) %>% head(-1) %>% select(-"...16") %>% gather(Year, Employment, colnames(.)[-1])
Employment$Country <- countrycode(Employment$Country, "country.name","iso2c")
Employment$Year <- as.numeric(Employment$Year)
Employment$Employment <- as.numeric(Employment$Employment)

Employment <- Employment %>% group_by(Country) %>% mutate(Employment_diff = Employment - lag(Employment)) %>% ungroup()

Growth <- read_excel("data/GDP_growth_rate.xlsx") %>% rename(Country = ...1) %>% head(-1) %>% select(-"...16") %>% gather(Year, Growth, colnames(.)[-1])
Growth$Country <- countrycode(Growth$Country, "country.name","iso2c")
Growth$Year <- as.numeric(Growth$Year)
Growth$Growth <- as.numeric(Growth$Growth)

OldWork <- read.csv2("data/OldWork.csv", sep = ",") %>% select("geo", "TIME_PERIOD", "OBS_VALUE") %>% 
  rename(EmploymentOld = OBS_VALUE)
OldWork$EmploymentOld <- as.numeric(OldWork$EmploymentOld)

OldWork <- OldWork %>% group_by(geo) %>% mutate(EmploymentOld_diff = EmploymentOld - lag(EmploymentOld)) %>% ungroup()


Employment_migration <-read.csv2(file = 'data/Employment_citizenship.csv', sep = ',', header = TRUE) %>%
  select(citizen, geo, TIME_PERIOD, OBS_VALUE) %>% filter(TIME_PERIOD == 2019)
Employment_migration$OBS_VALUE <- as.numeric(Employment_migration$OBS_VALUE)
Employment_migration$citizen[Employment_migration$citizen == "EU27_2020_FOR"] <- "EU"
Employment_migration$citizen[Employment_migration$citizen == "NAT"] <- "Native"
Employment_migration$citizen[Employment_migration$citizen == "NEU27_2020_FOR"] <- "Non-EU"

rm(imigration_flows, emigration_flows)

#### ALTERNATIVE MIGRATION DATA

migration_TOTAL  <- migration_flows %>% filter(citizen == "TOTAL", geo %in% EU28_codes) %>% 
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareTOTALMigrants = NetMigration / Population) %>%
  select(-"Population", -"citizen")

migration_EU <- migration_flows %>% 
  filter(citizen %in% EU28_codes, geo %in% EU28_codes) %>% 
  group_by(age, sex, geo, TIME_PERIOD) %>% 
  summarise(NetMigrationEU = sum(NetMigration), ImigrationEU = sum(Imigration), EmigrationEU = sum(Emigration)) %>%
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareEUMigrants = NetMigrationEU / Population) %>%
  select(-"Population")

migration_EFTA <- migration_flows %>% 
  filter(citizen %in% EFTA_codes, geo %in% EU28_codes) %>% 
  group_by(age, sex, geo, TIME_PERIOD) %>% 
  summarise(NetMigrationEFTA = sum(NetMigration), ImigrationEFTA = sum(Imigration), EmigrationEFTA = sum(Emigration)) %>%
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareEFTAMigrants = NetMigrationEFTA / Population) %>%
  select(-"Population")

migration_EU_alter <- migration_flows %>% filter(citizen == "EU28_FOR") %>%
   left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareEUImmigrants = NetMigration / Population) %>% 
   select(-"citizen", -"Population") %>% rename(NetMigrationEU = NetMigration, ImigrationEU = Imigration, EmigrationEU = Emigration)

migration_EFTA_alter <- migration_flows %>% filter(citizen == "EFTA_FOR") %>%
  left_join(Population, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% mutate(ShareEFTAImmigrants = NetMigration / Population)  %>% 
  select(-"citizen", -"Population") %>% rename(NetMigrationEFTA = NetMigration, ImigrationEFTA = Imigration, EmigrationEFTA = Emigration)


#differences in both approaches
delta_EU <- anti_join(migration_EU_alter, migration_EU, by=c("age", "sex", "geo", "TIME_PERIOD"))


migration_data <- inner_join(migration_EU, migration_EFTA, by=c("age", "sex", "geo", "TIME_PERIOD")) %>% 
  inner_join(migration_TOTAL)

## Data visualization

selection <- c("SE", "FR", "LU", "DE", "SI", "IT", "EE")

#Age graph
Age_graph <- ggplot(Age %>% filter(Country %in% selection), aes(x=Year, y=Age/100, color=Country)) + 
  geom_line() + geom_point() + 
  labs(title = "Share of pupulation aged 15 to 64 by EU countries") +
  ylab("Share") + 
  scale_y_continuous(labels = percent) + scale_fill_brewer(palette="Set2")
Age_graph

#Interesting selection -- SK, FR, LU, DE, SI

#Labor Force Participation
LFP_graph <- ggplot(LFP %>% filter(Country %in% selection), aes(x=Year, y=Participation/100, color=Country)) + 
  geom_line() + geom_point() +
  labs(title = "Labor force participation by EU countries") +
  ylab("Share of 15+ population") + 
  scale_y_continuous(labels = percent) + scale_fill_brewer(palette="Set2")
LFP_graph

#Employment graph
Emp_graph <- ggplot(Employment %>% filter(Country %in% selection), aes(x=Year, y=Employment/100, color=Country)) + 
  geom_line() + geom_point() +
  labs(title = "Employment by EU countries") +
  ylab("Share of 15+ population") + 
  scale_y_continuous(labels = percent) + scale_fill_brewer(palette="Set2")
Emp_graph

#Tertiary graph
Tert_graph <- ggplot(Tertiary %>% filter(Country %in% selection), aes(x=Year, y=Tertiary/100, color=Country)) + 
  geom_line() + geom_point() +
  labs(title = "Tertiary education enrollment by EU countries") +
  ylab("Share in total population") + 
  scale_y_continuous(labels = percent) + scale_fill_brewer(palette="Set2")
Tert_graph


# Migration intra-EU/EFTA
migration_EU_EFTA <- migration_data %>% mutate(EU_EFTA_Share = ShareEUMigrants + ShareEFTAMigrants)

#migration_EU_EFTA_graph <- ggplot(migration_EU_EFTA %>% filter(age == "TOTAL" & sex == "T" & geo %in% selection), aes(x=TIME_PERIOD, y=EU_EFTA_Share, color=geo)) + 
#  geom_line()
#ggplotly(migration_EU_EFTA_graph)

# Migration total
migration_TOTAL_graph <- ggplot(migration_TOTAL %>% filter(age == "TOTAL" & sex == "T" & geo %in% selection), aes(x=TIME_PERIOD, y=ShareTOTALMigrants, color=geo)) + 
  geom_line() + geom_point() + 
  labs(title = "Share of total net migration in population by country", colour = "Country") +
  ylab("Share") + xlab("Year") + 
  scale_y_continuous(labels = percent) + scale_fill_brewer(palette="Set2")
migration_TOTAL_graph

#Old Age work

OldWork_graph <- ggplot(OldWork %>% filter(geo %in% selection, TIME_PERIOD >= 2009), aes(x=TIME_PERIOD, y=EmploymentOld/100, color=geo)) + 
  geom_line() + geom_point() + 
  labs(title = "Employment in population aged from 55 to 64", color="Country") +
  ylab("Share in aged 55 to 64") +
  xlab("Year") + 
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette="Set2")
OldWork_graph

# IS OLD WORK CONNECTED TO HIGHER EDUCATION -- maybe dont have to include this

education_connection <- c("AT", "BE", "BG", "DE", "DK", "EE", "ES", "FR", "IE", "LV", "MT", "NL")

OldWorkEdu <- OldWork %>% left_join(Tertiary, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% filter(TIME_PERIOD >= 2009) 

OldWorkEdu_connection <- ggplot(OldWorkEdu %>% filter(geo %in% education_connection), 
                                aes(x=Tertiary/100, y=EmploymentOld/100, color=geo)) +
  geom_path() + geom_point() +
  labs(title="Strong connection", colour="Country") +
  ylab("Share of employed in population aged 55 to 64") + 
  xlab("Tertiary education enrollment") + 
  scale_y_continuous(labels = percent) + 
  theme(legend.title= element_blank()) + 
  scale_x_continuous(labels = percent) + scale_fill_brewer(palette="Set2")

OldWorkEdu_none <- ggplot(OldWorkEdu %>% filter(!(geo %in% education_connection), geo %in% EU28_codes, geo != "EL"), 
                                aes(x=Tertiary/100, y=EmploymentOld/100, color=geo)) +
  geom_path() + geom_point() +
  labs(title="Weak connection", colour="Country") +
  ylab("Share of employed in population aged 55 to 64") + 
  xlab("Tertiary education enrollment") + 
  scale_y_continuous(labels = percent) + 
  theme(legend.title= element_blank()) + 
  scale_x_continuous(labels = percent) + scale_fill_brewer(palette="Set2")

OldWorkEdu_graph <- ggarrange(OldWorkEdu_connection, OldWorkEdu_none + rremove("ylab"), 
                              ncol = 2, nrow = 1,
                              legend = "bottom")

annotate_figure(OldWorkEdu_graph, top = text_grob("Employment at older age and tertiary education", 
                                               color = "black", face = "bold", size = 14))

## AGE structure migrants

mig1564 <- migration_data %>% filter(age == "Y15-64", sex=="T") %>% ungroup() %>% select("geo", "TIME_PERIOD", "Imigration") %>%
  rename(Nr1564 = Imigration) %>%
  left_join(migration_TOTAL %>% filter(age == "TOTAL", sex=="T") %>% select("geo", "TIME_PERIOD", "Imigration"), by=c("geo", "TIME_PERIOD")) %>%
  mutate(Share1564 = Nr1564 * 100 / Imigration) %>%
  filter(Nr1564 > 0, Imigration > 0, TIME_PERIOD == 2019) %>%
  left_join(Age, by=c("geo" = "Country", "TIME_PERIOD" = "Year")) %>%
  select("geo", "TIME_PERIOD", "Share1564", "Age") %>%
  rename(Immigrants = Share1564, Whole = Age) %>%
  pivot_longer(cols = c("Immigrants", "Whole"), names_to = "Category", values_to = "WorkingAge")

Age_diff_graph <- ggplot(mig1564, aes(x=reorder(geo, WorkingAge), y=WorkingAge/100, fill=Category)) + 
  geom_col(position = "dodge") +
  labs(title="Working age shares in immigrant and native populations in 2019",
       fill="Population category") +
  ylab("Share") + 
  xlab("Country") + scale_fill_brewer(palette="Set2") +
  scale_y_continuous(labels=percent)
Age_diff_graph

#why slovakia negative? 
#Very small number of migrants -- when we take a subset (age group - even smaller). In 2014 only 24 people in right age group.

## EMPLOYMENT OF MIGRANTS

Employment_migration_graph <- ggplot(Employment_migration %>% filter(geo %in% selection, TIME_PERIOD == 2019), 
                                     aes(x = geo, y = OBS_VALUE / 100, fill = citizen)) +
  geom_col( position = "dodge") +
  labs(title="Employment by citizenship in 2019", x="Country", y="Share") +
  scale_fill_brewer(palette="Set2", name="Citizenship")

Employment_migration_graph

## GENDER structure of imigrants

migM <- migration_data %>% filter(age == "TOTAL", sex=="M") %>% ungroup() %>% select("geo", "TIME_PERIOD", "Imigration") %>%
  rename(Male = Imigration) %>%
  left_join(migration_TOTAL %>% filter(age == "TOTAL", sex=="T") %>% select("geo", "TIME_PERIOD", "Imigration"), by=c("geo", "TIME_PERIOD")) %>%
  filter(Male > 0, Imigration > 0) %>%
  mutate(Male_share = Male / Imigration) %>% select("geo", "TIME_PERIOD", "Male_share") %>%
  filter(TIME_PERIOD > 2015) %>%
  group_by(geo) %>% summarise(Mean_Male_Share = mean(Male_share))

Sex_graph <- ggplot(migM, aes(x=reorder(geo, Mean_Male_Share / (1 - Mean_Male_Share)), y=Mean_Male_Share / (1 - Mean_Male_Share))) + 
  geom_col(aes(fill=geo %in% selection)) +
  labs(title="Ratio between men and women immigrants", x="Country", y="Ratio") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 1, color="white", linetype="dashed") +
  scale_y_continuous(labels=percent) +
  scale_color_brewer(palette="Set2")
Sex_graph

## Employment of immigrants

################################################################
library(car)

### DATA for first regression
data_reg_1 <- migration_data %>% full_join(LFP, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% 
  filter(age == "TOTAL", sex=="T")  %>% full_join(Age, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>%
  full_join(Growth, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>%
  full_join(Tertiary, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>%
  full_join(Employment, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% ungroup() %>%
  select(-"age", -"sex") %>% left_join(OldWork)

reg1 <- plm(Employment_diff ~ ShareTOTALMigrants + Growth + Tertiary + Age - 1, data_reg_1, model = "random")
summary(reg1)
stargazer(reg1, type = "html", 
          title="Regression of Employment differences on Migration, Age and Tertiary Education enrollment", 
          out = "reg_tables/reg1.html", 
          covariate.labels = c("Net Migrant Share in Population", 
                               "GDP Growth Rate",
                               "Enrollment in Tertiary Education",
                               "Share of Population Aged 15-64"),
          dep.var.labels = c("Employment yearly differences"),
          style = "qje")

### EXPLANATION FOR NEGATIVE AGE FACTOR

not_available_oldeage <- c("DE", "PL")
OldE_and_Age <- ggplot(data_reg_1 %>% filter(geo %in% selection), 
                       aes(x=Age/100, y=EmploymentOld/100, group=geo, color=geo)) + 
  geom_path() + geom_point() +
  labs(title = "Employment at older age and share of working aged population",
       y="Share of employed in aged 55-64",
       x="Share of aged 15-64 in population",
       color="Country") +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=percent)
OldE_and_Age


### ARE EU MIGRANTS DIFFERENT FROM NON EU?

data_reg_2 <- migration_data %>% left_join(LFP, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% 
  filter(age == "TOTAL", sex=="T")  %>% left_join(Age, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% 
  mutate(EU_EFTA_Share = ShareEUMigrants + ShareEFTAMigrants) %>% mutate(Non_EU_EFTA_Share = ShareTOTALMigrants - EU_EFTA_Share) %>%
  inner_join(Growth, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>%
  inner_join(Employment, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% ungroup() %>%
  select(-"age", -"sex") %>%
  inner_join(Tertiary, by = c("geo" = "Country", "TIME_PERIOD" = "Year"))

reg2 <- plm(Employment_diff ~ EU_EFTA_Share + Non_EU_EFTA_Share + Growth + Tertiary + Age -1, data_reg_2, model = "random")
summary(reg2)
stargazer(reg2, type = "html",  title="EU and EFTA vs. Other Citizenships Regression", out = "reg_tables/reg2.html", 
          covariate.labels = c("EU or EFTA Net Migrant Share in Population",
                               "Non-EU and Non-EFTA Net Migrant Share in Population",
                               "GDP Growth Rate",
                               "Enrollment in Tertiary Education",
                               "Share of Population Aged 15-64"),
          dep.var.labels = c("Employment yearly differences"),
          style = "qje")


test_EU <- linearHypothesis(reg2, c("EU_EFTA_Share=Non_EU_EFTA_Share"), test = "F")
rownames(test_EU) <- c("", "Value of Statistic")
stargazer(test_EU[2, 3:4], type = "html", 
          out = "reg_tables/test_EU.html", summary = F,
          style = "qje")

### ARE WOMEN DIFFERENT THAN MEN?


data_reg_3 <-migration_data %>% filter(age == "TOTAL") %>% ungroup() %>% select("sex", "geo", "TIME_PERIOD", "ShareTOTALMigrants") %>%
  pivot_wider(names_from = c(sex), values_from = c(ShareTOTALMigrants), names_prefix = "ShareMigrants") %>% 
  left_join(LFP, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% 
  left_join(Age, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>% 
  inner_join(Growth, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>%
  inner_join(Employment, by = c("geo" = "Country", "TIME_PERIOD" = "Year")) %>%
  inner_join(Tertiary, by = c("geo" = "Country", "TIME_PERIOD" = "Year"))

reg3 <- plm(Employment_diff ~ ShareMigrantsM + ShareMigrantsF + Growth + Tertiary + Age - 1, data_reg_3, model = "random")
summary(reg3)
stargazer(reg3, type = "html",  title="Male vs. Female Regression", out = "reg_tables/reg3.html", 
          covariate.labels = c("Male Net Migrant Share in Population",
                               "Female Net Migrant Share in Population",
                               "GDP Growth Rate",
                               "Enrollment in Tertiary Education",
                               "Share of Population Aged 15-64"),
          dep.var.labels = c("Employment yearly differences"),
          style = "qje")


test_sex <- linearHypothesis(reg3, c("ShareMigrantsM=ShareMigrantsF"), test = "F")
rownames(test_sex) <- c("", "Value of Statistic")
stargazer(test_sex[2, 3:4], type = "html", 
          out = "reg_tables/test_sex.html", summary = F,
          style = "qje")

### EFFECT OF MIGRANTS IN 2015

data_reg_4 <- data_reg_1 %>% mutate(Y2015 = (TIME_PERIOD %in% c(2015, 2016)))
data_reg_4$Y2015 <- as.numeric(data_reg_4$Y2015)
data_reg_4$Mcrisis <- data_reg_4$ShareTOTALMigrants * data_reg_4$Y2015
data_reg_4$Mrest <- data_reg_4$ShareTOTALMigrants * (1 - data_reg_4$Y2015)

reg4 <- plm(Employment_diff ~ Mcrisis + Mrest + Growth + Tertiary + Age -1, data_reg_4, model = "random")
summary(reg4)
stargazer(reg4, type = "html",  title="Non-Crisis vs. 2015 and 2016 Crisis Regression", out = "reg_tables/reg4.html", 
          covariate.labels = c("Net Migrant Share in Population in 2015 and 2016",
                               "Net Migrant Share in Population in other years",
                               "GDP Growth Rate",
                               "Enrollment in Tertiary Education",
                               "Share of Population Aged 15-64"),
          dep.var.labels = c("Employment yearly differences"),
          style = "qje")


test_2015 <- linearHypothesis(reg4, c("Mcrisis=Mrest"), test = "F")
rownames(test_2015) <- c("", "Value of Statistic")
stargazer(test_2015[2, 3:4], type = "html", 
          out = "reg_tables/test_2015.html", summary = F,
          style = "qje")

######################################### LFP

reg1_LFP <- plm(Participation_diff ~ ShareTOTALMigrants + Growth + Tertiary + Age - 1, data_reg_1, model = "random")
summary(reg1_LFP)

stargazer(reg1_LFP, type = "html",  title="Non-Model for Labor Force Participation", out = "reg_tables/reg_LFP.html", 
          covariate.labels = c("Net Migrant Share in Population",
                               "GDP Growth Rate",
                               "Enrollment in Tertiary Education",
                               "Share of Population Aged 15-64"),
          dep.var.labels = c("Participation yearly differences"),
          style = "qje")

reg2_LFP <- plm(Participation ~ Age + EmploymentOld + ShareTOTALMigrants - 1, data_reg_1, model = "random")
summary(reg2_LFP)

#NONE OF OUR MODELS WORK FOR LFP -- no answer for this



