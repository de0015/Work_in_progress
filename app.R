library(shiny)
library(shinythemes)
library(lubridate)
library(dplyr)
library(ggplot2)
library(magrittr)
library(rvest)
library(scales)
library(DT)
library(RCurl)
library(formattable)
library(viridis)
library(tidyverse)


## If you post this to github... please remove line below
#rsconnect::setAccountInfo(name='de0105', token='84DE64071296560C73C4B4F8D042A7B1', secret='gDAOFxHUfDxccymL6EHBneo9h0KpuxldahV1wv5k')


yesterday <- format(Sys.Date()-1,"%m-%d-%Y.csv")

date_eval<- format(Sys.Date()-1,"%Y-%m-%d") %>%
    as.Date()

percent_change<- format(Sys.Date()-2,"%m-%d-%Y.csv") 

percent_change_date <- format(Sys.Date()-2,"%Y-%m-%d") %>%
    as.Date()

### This was added on 8/16 to extend the prediction
percent_change_3_days <- format(Sys.Date()-3,"%m-%d-%Y.csv")
percent_change_4_days <- format(Sys.Date()-4,"%m-%d-%Y.csv")
percent_change_5_days <- format(Sys.Date()-5,"%m-%d-%Y.csv")
three_days_url <-paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", percent_change_3_days)
World_3_days_data <- getURL(three_days_url)
World_3_days_data <- read.csv(text = World_3_days_data) 

four_days_url <-paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", percent_change_4_days)
World_4_days_data <- getURL(four_days_url)
World_4_days_data <- read.csv(text = World_4_days_data) 

five_days_url <-paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", percent_change_5_days)
World_5_days_data <- getURL(five_days_url)
World_5_days_data <- read.csv(text = World_5_days_data) 


## Vaccine Data

## List of countries with vaccine and the type

Vaccines <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv")
Vaccines_2 <- read.csv(text=Vaccines) %>% select(1,3,4,5)

names(Vaccines_2) <- c("Location", "Vaccine Name", "Last Report", "Source")

## Global Vaccine (Our World in Data)

global_vaccine <-getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
global_vaccine_2 <- read.csv(text=global_vaccine)
#global_vaccine_2$date <- as.Date(global_vaccine_2$date)
test <- global_vaccine_2 %>% group_by(location) %>% filter(date == max(date)) 
global_vaccinated <- test %>% select(location,date,people_fully_vaccinated_per_hundred) 
global_test <- global_vaccine_2 %>% filter(!is.na(people_fully_vaccinated_per_hundred) )%>% 
  group_by(location) %>%
  slice(which.max(as.Date(date))) %>% select(location,date,people_fully_vaccinated_per_hundred) %>%
  mutate(people_fully_vaccinated_per_hundred/100) 

names(global_test) <- c("Country", "Last Report Date", "People Fully Vaccinated Per 100", "Percentage of Population Vaccinated")

global_test$`Percentage of Population Vaccinated` <- percent(global_test$`Percentage of Population Vaccinated`, digits = 0, format = "f")

### Import the CDC Vaccination Data

US_Vaccine <-getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv")
cdc_vaccine_2 <- read.csv(text=US_Vaccine)
test_cdc <- cdc_vaccine_2 %>% group_by(location) %>% filter(date == max(date)) %>% select(location,date,people_fully_vaccinated_per_hundred)  %>%
  mutate(people_fully_vaccinated_per_hundred/100) 
names(test_cdc) <- c("State", "Date", "People Fully Vaccinated Per 100", "Percentage of Population Vaccinated")

test_cdc$`Percentage of Population Vaccinated` <- percent(test_cdc$`Percentage of Population Vaccinated`, digits = 0, format = "f")
## John Hopkins State Data Below

state_compare_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",percent_change) 

state_recent_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",yesterday) 

US_State_2_days <- getURL(state_compare_url)

US_State_2_days <- read.csv(text = US_State_2_days) 

US_STATE_NEW <- getURL(state_recent_url)

US_STATE_NEW <- read.csv(text = US_STATE_NEW)

state_3_days_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",percent_change_3_days)
US_State_3_days <- getURL(state_3_days_url)
US_State_3_days <- read.csv(text = US_State_3_days) 
state_4_days_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",percent_change_4_days)
US_State_4_days <- getURL(state_4_days_url)
US_State_4_days <- read.csv(text = US_State_4_days) 
state_5_days_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",percent_change_5_days)
US_State_5_days <- getURL(state_5_days_url)
US_State_5_days <- read.csv(text = US_State_5_days) 

######


data_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",yesterday) 

percent_change_data <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",percent_change) 

two_days_ago_date <- getURL(percent_change_data)

compare_date_jh <- read.csv(text = two_days_ago_date)

yesterday_jh <- getURL(data_url)

yesterday_jh_data <- read.csv(text = yesterday_jh)

table_date <- format(Sys.Date()-2,"%Y-%m-%d") %>%
    as.Date()

#### Aggregate The Country Data

agg_compare <- aggregate(compare_date_jh$Confirmed, by=list(Category=compare_date_jh$Country_Region), FUN=sum) 
agg_compare_death <- aggregate(compare_date_jh$Deaths, by=list(Category=compare_date_jh$Country_Region), FUN=sum) 
agg_compare_recovery <- aggregate(compare_date_jh$Recovered, by=list(Category=compare_date_jh$Country_Region), FUN=sum) 

agg_country <- aggregate(yesterday_jh_data$Confirmed, by=list(Category=yesterday_jh_data$Country_Region), FUN=sum) 

agg_death <- aggregate(yesterday_jh_data$Deaths, by=list(Category=yesterday_jh_data$Country_Region), FUN=sum)

names(agg_compare) <-c("Country","Confirmed Cases")
names(agg_compare_death) <-c("Country","Deaths")
names(agg_compare_recovery) <-c("Country","Recoveries")

comparison_data <- merge(agg_compare,agg_compare_death, by = c("Country"))
comparison_data <- merge(comparison_data, agg_compare_recovery, by = c("Country"))

### Aggregate the 3,4,5 days for Country and State... This blows....

Agg_compare_3_day_cases <- aggregate(World_3_days_data$Confirmed, by=list(Category=World_3_days_data$Country_Region), FUN=sum)
Agg_compare_3_day_death <- aggregate(World_3_days_data$Deaths, by=list(Category=World_3_days_data$Country_Region), FUN=sum)
Agg_compare_3_day_recovered <- aggregate(World_3_days_data$Recovered, by=list(Category=World_3_days_data$Country_Region), FUN=sum)
names(Agg_compare_3_day_cases) <-c("Country","Confirmed Cases")
names(Agg_compare_3_day_death) <-c("Country","Deaths")
names(Agg_compare_3_day_recovered) <-c("Country","Recoveries")
three_days_merged <- merge(Agg_compare_3_day_cases, Agg_compare_3_day_death, by = c("Country"))


Agg_compare_4_day_cases <- aggregate(World_4_days_data$Confirmed, by=list(Category=World_4_days_data$Country_Region), FUN=sum)
Agg_compare_4_day_death <- aggregate(World_4_days_data$Deaths, by=list(Category=World_4_days_data$Country_Region), FUN=sum)
Agg_compare_4_day_recovered <- aggregate(World_4_days_data$Recovered, by=list(Category=World_4_days_data$Country_Region), FUN=sum)
names(Agg_compare_4_day_cases) <-c("Country","Confirmed Cases")
names(Agg_compare_4_day_death) <-c("Country","Deaths")
names(Agg_compare_4_day_recovered) <-c("Country","Recoveries")
four_days_merged <- merge(Agg_compare_4_day_cases, Agg_compare_4_day_death, by = c("Country"))

Agg_compare_5_day_cases <- aggregate(World_5_days_data$Confirmed, by=list(Category=World_5_days_data$Country_Region), FUN=sum)
Agg_compare_5_day_death <- aggregate(World_5_days_data$Deaths, by=list(Category=World_5_days_data$Country_Region), FUN=sum)
Agg_compare_5_day_recovered <- aggregate(World_5_days_data$Recovered, by=list(Category=World_5_days_data$Country_Region), FUN=sum)
names(Agg_compare_5_day_cases) <-c("Country","Confirmed Cases")
names(Agg_compare_5_day_death) <-c("Country","Deaths")
names(Agg_compare_5_day_recovered) <-c("Country","Recoveries")
five_days_merged <- merge(Agg_compare_5_day_cases, Agg_compare_5_day_death, by = c("Country"))

### Lets do the states next...said noone...


agg_state_confirmed_3_days <- aggregate(US_State_3_days$Confirmed, by=list(Category=US_State_3_days$Province_State), FUN=sum)
agg_state_deaths_3_days <- aggregate(US_State_3_days$Deaths, by=list(Category=US_State_3_days$Province_State), FUN=sum)
agg_state_recovery_3_days <- aggregate(US_State_3_days$Recovered, by=list(Category=US_State_3_days$Province_State), FUN=sum)
names(agg_state_confirmed_3_days) <-c("Country","Confirmed Cases")
names(agg_state_deaths_3_days) <-c("Country","Deaths")
names(agg_state_recovery_3_days) <-c("Country","Recoveries")
compare_data_state_3_days <- merge(agg_state_confirmed_3_days, agg_state_deaths_3_days, by = "Country")

agg_state_confirmed_4_days <- aggregate(US_State_4_days$Confirmed, by=list(Category=US_State_4_days$Province_State), FUN=sum)
agg_state_deaths_4_days <- aggregate(US_State_4_days$Deaths, by=list(Category=US_State_4_days$Province_State), FUN=sum)
agg_state_recovery_4_days <- aggregate(US_State_4_days$Recovered, by=list(Category=US_State_4_days$Province_State), FUN=sum)
names(agg_state_confirmed_4_days) <-c("Country","Confirmed Cases")
names(agg_state_deaths_4_days) <-c("Country","Deaths")
names(agg_state_recovery_4_days) <-c("Country","Recoveries")
compare_data_state_4_days <- merge(agg_state_confirmed_4_days, agg_state_deaths_4_days, by = "Country")

agg_state_confirmed_5_days <- aggregate(US_State_5_days$Confirmed, by=list(Category=US_State_5_days$Province_State), FUN=sum)
agg_state_deaths_5_days <- aggregate(US_State_5_days$Deaths, by=list(Category=US_State_5_days$Province_State), FUN=sum)
agg_state_recovery_5_days <- aggregate(US_State_5_days$Recovered, by=list(Category=US_State_5_days$Province_State), FUN=sum)
names(agg_state_confirmed_5_days) <-c("Country","Confirmed Cases")
names(agg_state_deaths_5_days) <-c("Country","Deaths")
names(agg_state_recovery_5_days) <-c("Country","Recoveries")
compare_data_state_5_days <- merge(agg_state_confirmed_5_days, agg_state_deaths_5_days, by = "Country")

### The code below is for the merge of the final dataset

agg_recovery <- aggregate(yesterday_jh_data$Recovered, by=list(Category=yesterday_jh_data$Country_Region), FUN=sum)
names(agg_country) <-c("Country","Confirmed Cases")
names(agg_death) <-c("Country","Deaths")
names(agg_recovery) <-c("Country","Recoveries")
total_country <- merge(agg_country,agg_death, by=c("Country"))

total_country <- merge(total_country, agg_recovery, by=c("Country"))

total_country$`Confirmed Cases` <- comma(total_country$`Confirmed Cases`, digits = 0)
total_country$Deaths <- comma(total_country$Deaths, digits = 0)

total_country$Recoveries <- comma(total_country$Recoveries, digits = 0)

agg_state_confirmed_yesterday <- aggregate(compare_date_jh$Confirmed, by=list(Category=compare_date_jh$Province_State), FUN=sum)
agg_state_deaths_yesterday <- aggregate(compare_date_jh$Deaths, by=list(Category=compare_date_jh$Province_State), FUN=sum)
agg_state_recovery_yesterday <- aggregate(compare_date_jh$Recovered, by=list(Category=compare_date_jh$Province_State), FUN=sum)

compare_data_state <- merge(agg_state_confirmed_yesterday,agg_state_deaths_yesterday, by = "Category")
compare_data_state <- merge(compare_data_state, agg_state_recovery_yesterday, by= "Category")

names(compare_data_state) <-c("State","Confirmed Cases","Deaths","Recoveries")

agg_state_confirmed_today <- aggregate(yesterday_jh_data$Confirmed, by=list(Category=yesterday_jh_data$Province_State), FUN=sum)
agg_state_deaths_today <- aggregate(yesterday_jh_data$Deaths, by=list(Category=yesterday_jh_data$Province_State), FUN=sum)
agg_state_recovery_today <- aggregate(yesterday_jh_data$Recovered, by=list(Category=yesterday_jh_data$Province_State), FUN=sum)

compare_data_state_today <- merge(agg_state_confirmed_today,agg_state_deaths_today, by = "Category")
compare_data_state_today <- merge(compare_data_state_today, agg_state_recovery_today, by= "Category")

names(compare_data_state_today) <-c("State","Confirmed Cases","Deaths","Recoveries")

state_filter <- c("Vermont","New York","Pennsylvania","New Jersey","West Virginia","North Carolina","Texas","Puerto Rico","Texas","California","Illinois","District of Columbia", "Florida", "Michigan")

state_filter_2<- c("Vermont","New York State","Pennsylvania","New Jersey","West Virginia","North Carolina","Texas","Puerto Rico","Texas","California","Illinois","District of Columbia", "Florida", "United States", "Michigan")

choices <-c("Albania",
            "Armenia",
            "Algeria",
            "Australia",
            "Austria",
            "Azerbaijan",
            "Bahrain",
            "Belarus",
            "Belgium",
            "Bosnia and Herzegovina",
            "Brazil",
            "Bulgaria",
            "Canada",
            "China",
            "Costa Rica",
            "Cote d'Ivoire",
            "Croatia",
            "Cyprus",
            "Czechia",
            "Denmark",
            "Dominican Republic",
            "Egypt",
            "Estonia",
            "Finland",
            "France",
            "Georgia",
            "Germany",
            "Greece",
            "Guatemala",
            "Honduras",
            "Hong Kong",
            "Hungary",
            "India",
            "Indonesia",
            "Iran",
            "Iraq",
            "Ireland",
            "Italy",
            "Japan",
            "Jordan",
            "Kazakhstan",
            "Kenya",
            "Kuwait",
            "Kyrgyzstan",
            "Latvia",
            "Lebanon",
            "Lithuania",
            "Luxembourg",
            "Macedonia",
            "Malaysia",
            "Mexico",
            "Morocco",
            "Netherlands",
            "New Zealand",
            "Nigeria",
            "Norway",
            "Oman",
            "Panama",
            "Peru",
            "Philippines",
            "Poland",
            "Portugal",
            "Qatar",
            "Romania",
            "Russia",
            "Saudi Arabia",
            "Serbia",
            "Singapore",
            "Slovakia",
            "Slovenia",
            "South Africa",
            "Korea, South",
            "Spain",
            "Sweden",
            "Switzerland",
            "Taiwan*",
            "Thailand",
            "Tunisia",
            "Turkey",
            "Uganda",
            "Ukraine",
            "United Arab Emirates",
            "United Kingdom",
            "US",
            "Vietnam",
            "Zambia")

### Use these below to calculate
#df <- mydata[ -c(1,3:4) ]

us_state_today <- US_STATE_NEW %>% filter(Province_State %in% state_filter)
us_state_yesterday <- US_State_2_days %>% filter(Province_State %in% state_filter)
us_state_3_days_filtered <- compare_data_state_3_days %>% filter(Country %in% state_filter)
names(us_state_3_days_filtered) <- c("Province_State", "Confirmed" , "Deaths")
us_state_4_days_filtered <- compare_data_state_4_days %>% filter(Country %in% state_filter)
names(us_state_4_days_filtered) <- c("Province_State", "Confirmed" , "Deaths")
us_state_5_days_filtered <- compare_data_state_5_days %>% filter(Country %in% state_filter)
names(us_state_5_days_filtered) <- c("Province_State", "Confirmed" , "Deaths")
### Lets do 3 day calculator

US_three_day_calculation <- merge(us_state_today, us_state_3_days_filtered, by = "Province_State")
US_three_day_calculation <- US_three_day_calculation[-c(2,3,4,5,8:18)]
US_four_day_calculation <- merge(us_state_today, us_state_4_days_filtered, by = "Province_State")
US_four_day_calculation <- US_four_day_calculation[-c(2,3,4,5,8:18)]
US_five_day_calculation <- merge(us_state_today, us_state_5_days_filtered, by = "Province_State")
US_five_day_calculation <- US_five_day_calculation[-c(2,3,4,5,8:18)]

### Now to calculate the 3/4/5 day US State one
US_three_day_calculation$`Confirmed Case Increase` <- US_three_day_calculation$Confirmed.x - US_three_day_calculation$Confirmed.y
US_three_day_calculation$`Confirmed Death Increase` <- US_three_day_calculation$Deaths.x - US_three_day_calculation$Deaths.y
names(US_three_day_calculation) <- c("Province_State", "Confirmed Cases", "Confirmed Case Increase", "Confirmed Deaths", "Confirmed Death Increase")

US_four_day_calculation$`Confirmed Case Increase` <- US_four_day_calculation$Confirmed.x - US_four_day_calculation$Confirmed.y
US_four_day_calculation$`Confirmed Death Increase` <- US_four_day_calculation$Deaths.x - US_four_day_calculation$Deaths.y

US_five_day_calculation$`Confirmed Case Increase` <- US_five_day_calculation$Confirmed.x - US_five_day_calculation$Confirmed.y
US_five_day_calculation$`Confirmed Death Increase` <- US_five_day_calculation$Deaths.x - US_five_day_calculation$Deaths.y
### The States are filtered above

us_state_calc <- merge(us_state_today,us_state_yesterday, by = "Province_State")

us_state_calc$`Confirmed Case Increase` <-  us_state_calc$Confirmed.x - us_state_calc$Confirmed.y 
us_state_calc$`Confirmed Death Increase` <- us_state_calc$Deaths.x - us_state_calc$Deaths.y 
us_state_calc$`Confirmed Recovery Increase` <- us_state_calc$Recovered.x - us_state_calc$Recovered.y

us_state_calc <- us_state_calc[ ,c(1,6,36,7,37)]

names(us_state_calc) <-c("State","Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase")

us_state_calc <- arrange(us_state_calc)

#### Calculate 3,4,5 day differences

#US_three_day_calculation$case_increase <- US_three_day_calculation$Confirmed.x - US_three_day_calculation$Confirmed.y
#US_three_day_calculation$death_increase <- US_three_day_calculation$Deaths.x - US_three_day_calculation$Deaths.y

#US_three_day_calculation <- US_three_day_calculation[ ,c(1,6,21,7,22)]
#names(US_three_day_calculation) <-c("State","Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase")

#US_four_day_calculation$case_increase <- four_day_calculation$Confirmed.x - four_day_calculation$Confirmed.y
#US_four_day_calculation$death_increase <- four_day_calculation$Deaths.x - four_day_calculation$Deaths.y

#US_four_day_calculation <- four_day_calculation[ ,c(1,6,21,7,22)]
#names(four_day_calculation) <-c("State","Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase")


#five_day_calculation$case_increase <- five_day_calculation$Confirmed.x - five_day_calculation$Confirmed.y
#five_day_calculation$death_increase <- five_day_calculation$Deaths.x - five_day_calculation$Deaths.y

#five_day_calculation <- five_day_calculation[ ,c(1,6,21,7,22)]
#names(five_day_calculation) <-c("State","Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase")


### Changed the cdc state filter to alphabetize

cdc_state_filtered <- filter(test_cdc, State %in% state_filter_2) 
cdc_state_filtered <-  cdc_state_filtered[order(cdc_state_filtered$State),] 

world_vaccine_filtered <- filter(global_test, Country %in% choices) %>%
  arrange(desc(`People Fully Vaccinated Per 100`))


###

total_country <- arrange(total_country,desc(total_country$Country))

selected_states_current <- total_country %>% subset(Country %in% choices)


selected_states_compare <- comparison_data %>% subset(Country %in% choices)

names(selected_states_compare) <-c("Country","Confirmed Cases","Deaths","Recoveries")
selected_states_compare$`Confirmed Cases` <- comma(selected_states_compare$`Confirmed Cases`, digits = 0)


compar <- merge(selected_states_current,selected_states_compare, by = "Country") 

compar$`Confirmed Case Increase` <-  (compar$`Confirmed Cases.x` - compar$`Confirmed Cases.y`) 
compar$`Confirmed Death Increase` <- compar$Deaths.x - compar$Deaths.y 
compar$`Confirmed Recovery Increase` <- compar$Recoveries.x - compar$Recoveries.y

#df <- mydata[ -c(1,3:4) ]
### Below sets the columns for the final table

compar <- compar[,c(1,2,8,3,9,4,10)] 

merge_test <- merge(compar, world_vaccine_filtered, all.x = TRUE)

merge_test <- merge_test[,c(1,2,3,4,5,10)] 

merge_test <- arrange(merge_test,desc(compar$`Confirmed Cases`))

names(merge_test) <- c("Country","Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase","Percentage Fully Vaccinated")

### The lines below are to fix John's Hopkins Data move
US_State_2_days$Recovered <- replace_na(US_State_2_days$Recovered,0)
usa_2_days_cases <- aggregate(US_State_2_days$Confirmed, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_2_days_deaths <- aggregate(US_State_2_days$Deaths, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_2_days_recovery <- aggregate(US_State_2_days$Recovered, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_2_day_merge_1 <- merge(usa_2_days_cases,usa_2_days_deaths, by = "Category")
usa_2_day_merge_2 <- merge(usa_2_day_merge_1, usa_2_days_recovery, by = "Category")
### 
 US_STATE_NEW$Recovered <- replace_na(US_STATE_NEW$Recovered,0)
usa_yesterday_cases <- aggregate(US_STATE_NEW$Confirmed, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_yesterday_deaths <- aggregate(US_STATE_NEW$Deaths, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_yesterday_recovery <- aggregate(US_STATE_NEW$Recovered, by=list(Category=US_State_2_days$Country_Region), FUN=sum)
usa_yesterday_merge_1 <- merge(usa_yesterday_cases,usa_yesterday_deaths,  by = "Category")
usa_yesterday_merge_2 <- merge(usa_yesterday_merge_1, usa_yesterday_recovery, by = "Category")
###

### Merge the USA DF's
USA_Totals <- merge(usa_yesterday_merge_2, usa_2_day_merge_2, by = "Category")
names(USA_Totals) <-c("Country", "Yesterday Cases", "Yesterday Deaths", "Yesterday Recoveries", "2 day cases", "2 day deaths", "2 day recoveries")
USA_Totals$CaseIncrease <- USA_Totals$`Yesterday Cases`- USA_Totals$`2 day cases`
USA_Totals$DeathIncrease <- USA_Totals$`Yesterday Deaths` - USA_Totals$`2 day deaths`
USA_Totals$RecoveryIncrease <- USA_Totals$`Yesterday Recoveries` - USA_Totals$`2 day recoveries`
USA_Totals <- USA_Totals[,c(1,2,8,3,9,4,10)]

names(USA_Totals) <- c("Country", "Confirmed Cases", "Case Increase", "Deaths", "Death Increase", "Recoveries", "Recoveries Increase")
names(compar) <-c("Country","Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase","Total Recoveries", "Daily Recoveries Increase")

compar <- arrange(compar,desc(compar$`Confirmed Cases`))
compar <- compar[-c(6,7)]
us_state_calc <- us_state_calc[-c(6,7)]
USA_Totals <- USA_Totals[-c(6,7)]


# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"),
    fluidPage(
    theme = shinytheme("cosmo")
  #  titlePanel(""),
  #  sidebarLayout(
      # sidebarPanel(
           # checkboxGroupInput("dataset", "State",
            #            choices = choices, selected = "North Carolina")
        ),
        mainPanel(
          mainPanel(
            tabsetPanel(
              id = 'dataset',
              tabPanel("Country", DT::dataTableOutput("table")),
              tabPanel("State", DT::dataTableOutput("mytable2")),
              tabPanel("US Calculation", DT::dataTableOutput("mytable3")),
              tabPanel("US Vaccination Filtered", DT::dataTableOutput("mytable4")),
              tabPanel("US Vaccination Filtered 3 day", DT::dataTableOutput("mytable7")),
             # tabPanel("World Vaccination Filtered", DT::dataTableOutput("mytable5")),
              tabPanel("World Vaccination Filtered", DT::dataTableOutput("mytable6"))
            )
          )
        )
  )
          
        
    server <- shinyServer(
        
        function(input,output){
            
        #    datasetInput <- reactive({
         #       NY_times_state %>% filter(state == input$dataset)
         #   })
            
            # plot time series
           
            output$table <- DT::renderDataTable({
                compar %>%
                    datatable(extensions = 'Buttons',
                                           options = list(
                                               pageLength = 100,
                                               scrollX=TRUE,
                                               width = '700px',
                                               dom = 'T<"clear">lBfrtip'
                                           )
                ) %>%
                    formatCurrency(2:4, currency = "", mark = ",") %>%
                    formatRound(columns=c("Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase"), digits=0)
           ## If the countries disappear, remove Country from the format round above... The damn thing tries to round a string...
            })
            
            output$mytable2 <- DT::renderDataTable({
              us_state_calc %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip')) %>%
                formatCurrency(2:4, currency = "", mark = ",") %>%
               formatRound(columns=c("Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase"), digits=0)
            })    
            output$mytable7 <- DT::renderDataTable({
              US_three_day_calculation %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip')) %>%
                formatCurrency(2:4, currency = "", mark = ",") %>%
                formatRound(columns=c("State","Confirmed Cases","Confirmed Cases Increase","Total Deaths","Daily Deaths Increase"), digits=0)
            })
            
            output$mytable3 <- DT::renderDataTable({
              USA_Totals %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip')) %>%
                formatCurrency(2:4, currency = "", mark = ",") %>%
                formatRound(columns=c("Confirmed Cases", "Case Increase", "Deaths", "Death Increase"), digits=0)
            })
            
            output$mytable4 <- DT::renderDataTable({
              cdc_state_filtered %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip'))%>%
                formatPercentage(4, mark = ",", digits = 2)
              #  formatRound(columns=c("State", "Date", "People Fully Vaccinated Per 100"), digits=0)
            })   
            
            output$mytable5 <- DT::renderDataTable({
              world_vaccine_filtered %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip'))%>%
                formatPercentage(4, mark = ",", digits = 2)
              #  formatRound(columns=c("Country", "Last Report Date", "People Fully Vaccinated Per 100"), digits=0)
            })   
            
            output$mytable6 <- DT::renderDataTable({
              merge_test %>%
                datatable(extensions = 'Buttons',
                          options = list(
                            pageLength = 100,
                            scrollX=TRUE,
                            width = '700px',
                            dom = 'T<"clear">lBfrtip')) %>%
                formatCurrency(2:4, currency = "", mark = ",", digits = 0) %>%
                formatPercentage(6, mark = ",", digits = 2)
            })   
        })
    
    shiny::shinyApp(ui = ui, server = server)
    