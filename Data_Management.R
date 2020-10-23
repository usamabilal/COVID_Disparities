rm(list=ls())
library(tidyverse)
library(data.table)
library(tidycensus)
library(readxl)
library(rgdal)
library(lubridate)
library(sf)
library(foreign)
library(zoo)
library(spdep)
library(rmapshaper)
options(scipen=999)
select<-dplyr::select

# download and load geo data
# download SHPs once
# counties  shp
# download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_500k.zip",
#               destfile ="Data/cb_2018_us_county_500k.zip" )
# unzip("Data/cb_2018_us_county_500k.zip", exdir = "Data/countyshp/")
# # states shp
# download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip",
#               destfile ="Data/cb_2018_us_state_500k.zip" )
# unzip("Data/cb_2018_us_state_500k.zip", exdir = "Data/stateshp/")
# # places for PA
# download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_42_place_500k.zip",
#               destfile ="Data/cb_2018_42_place_500k.zip" )
# unzip("Data/cb_2018_42_place_500k.zip", exdir = "Data/PHL/place_pa/")
# # places for NY
# download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_36_place_500k.zip",
#               destfile ="Data/cb_2018_36_place_500k.zip" )
# unzip("Data/cb_2018_36_place_500k.zip", exdir = "Data/NYC/place_ny/")
# CBG
# download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_36_bg_500k.zip",
#               destfile ="Data/cb_2018_36_bg_500k.zip" )
# unzip("Data/cb_2018_36_bg_500k.zip", exdir = "Data/NYC/bg_ny/")
# download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_42_bg_500k.zip",
#               destfile ="Data/cb_2018_42_bg_500k.zip" )
# unzip("Data/cb_2018_42_bg_500k.zip", exdir = "Data/PHL/bg_pa/")
# download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_17_bg_500k.zip",
#               destfile ="Data/cb_2018_17_bg_500k.zip" )
# unzip("Data/cb_2018_17_bg_500k.zip", exdir = "Data/Chicago/bg_il/")



# load generic shapefiles
# states
shp_states = readOGR('Data/stateshp/cb_2018_us_state_500k.shp')
shp_states$STATEFP<-as.numeric(as.character(shp_states$STATEFP))
shp_states<-shp_states[grepl("Delaware|New Jersey|Pennsylvania|New York|Connecticut|Indiana|Illinois", shp_states$NAME),]
shp_states<-ms_simplify(shp_states)
# place, to set boundaries
# PA
shp_place_pa = readOGR('Data/PHL/place_pa/', 'cb_2018_42_place_500k')
shp_place_pa<-shp_place_pa[shp_place_pa$NAME=="Philadelphia",]
bbox_pa<-st_bbox(shp_place_pa)
# IL
shp_place_il = readOGR('Data/Chicago/place_il/', 'cb_2018_17_place_500k')
shp_place_il<-shp_place_il[shp_place_il$NAME=="Chicago",]
bbox_il<-st_bbox(shp_place_il)
# NY
shp_place_ny = readOGR('Data/NYC/place_ny/', 'cb_2018_36_place_500k')
shp_place_ny<-shp_place_ny[shp_place_ny$NAME=="New York",]
bbox_ny<-st_bbox(shp_place_ny)
# zcta (all)
shp_zip<-readOGR('Data/zipcodeshp/', 'cb_2018_us_zcta510_500k')
shp_zip$GEOID<-as.numeric(as.character(shp_zip$ZCTA5CE10))
shp_zip<-shp_zip %>% st_as_sf
# get modified ZCTA from NYC
shp_zip_mod<-readOGR('Data/NYC/Geography-resources/', 'MODZCTA_2010')
shp_zip_mod$GEOID<-as.numeric(as.character(shp_zip_mod$MODZCTA))
shp_zip_mod<-shp_zip_mod %>% st_as_sf %>% 
  st_transform(crs=st_crs(shp_place_ny))




# load area and transform to miles
area<-read.dbf("Data/zipcodeshp/cb_2018_us_zcta510_500k.dbf") %>% 
  mutate(GEOID=as.numeric(as.character(GEOID10)),
         area=ALAND10/1000000/2.59) %>% 
  select(GEOID, area)

# obtain a census api key https://api.census.gov/data/key_signup.html
# census_api_key(key="xxx", install = T)
# use this dataset to check variable names
vars<-load_variables(year=2018, "acs5")
# get data at the zipcode level and process
zcta_data_raw<-get_acs(geography = "zcta",
                      variables=c(
                        # age
                        paste0("B01001_", sprintf("%03d", 1:49)),
                        # median household income
                        "B19013_001",
                        # education
                        "B15003_022","B15003_023","B15003_024","B15003_025",
                        "B15003_001",
                        # citizenship
                        "B05001_001", "B05001_006",
                        # foreign born
                        "B06001_001", "B06001_049",
                        # race/ethnicity
                        "B03002_001", 
                        "B03002_003", 
                        "B03002_012", 
                        "B03002_004",
                        # limited english stuff
                        "C16002_001","C16002_004", "C16002_007", 
                        "C16002_010", "C16002_013",
                        # health insurance (uninsured)
                        "B27010_001","B27010_017", "B27010_033", "B27010_050", "B27010_066",
                        # healthcare workers
                        "C24010_001","C24010_016","C24010_017", 
                        "C24010_018","C24010_020",
                        "C24010_052","C24010_053",
                        "C24010_054","C24010_056",
                        # food service, personal care, and service ocupations
                        "C24010_024","C24010_060","C24010_026","C24010_062",
                        # overcrowding denominator
                        "B25014_001", 
                        #overcrowding 1 and more
                        "B25014_005", "B25014_011",
                        #overcrowding 1.5more
                        "B25014_006", "B25014_012",
                        #overcrowding 2more
                        "B25014_007", "B25014_013",
                        # public transit excluding taxicab
                        "B08006_001","B08006_008"
                      ),
                      year=2018) %>% 
  select(GEOID, variable, estimate) %>% 
  spread(variable, estimate) %>% 
  mutate(GEOID=as.numeric(GEOID))
# do some data management to create indicators
zcta_data<-zcta_data_raw %>% 
  rowwise() %>% 
  mutate(mhi=B19013_001,
         pct_age2044=sum(c(B01001_008,B01001_009,B01001_010,B01001_011,B01001_012,
                           B01001_013,B01001_014,B01001_032,B01001_033,B01001_034,
                           B01001_035,B01001_036,B01001_037,B01001_038))/B01001_001,
         pct_age4564=sum(c(B01001_015,B01001_016,B01001_017,B01001_018,B01001_019,
                           B01001_039,B01001_040,B01001_041,B01001_042,B01001_043))/B01001_001,
         pct_age6574=sum(c(B01001_020,B01001_021,B01001_022,B01001_044,B01001_045,
                           B01001_046))/B01001_001,
         pct_age7584=sum(c(B01001_023,B01001_024,B01001_047,B01001_048))/B01001_001,
         pct_age85plus=sum(c(B01001_025,B01001_049))/B01001_001,
         pct_hisp=B03002_012/B03002_001,
         pct_black=B03002_004/B03002_001,
         pct_nhwhite=B03002_003/B03002_001,
         pct_noncitizen=B05001_006/B05001_001,
         pct_foreignborn=B06001_049/B06001_001,
         total_pop=B03002_001,
         total_hisp=B03002_012,
         total_black=B03002_004,
         limited_engl=sum(c(C16002_004, C16002_007, C16002_010, C16002_013))/C16002_001,
         no_healthins=sum(c(B27010_017, B27010_033, B27010_050, B27010_066))/B27010_001,
         pct_healthcareworkers=sum(c(C24010_016,C24010_017,C24010_018,C24010_020,C24010_052,C24010_053,C24010_054,C24010_056))/C24010_001,
         pct_service=sum(c(C24010_024, C24010_026, C24010_060, C24010_062))/C24010_001,
         pct_overcrowded2=sum(c(B25014_007, B25014_013))/B25014_001,
         pct_overcrowded15=sum(c(B25014_007, B25014_006,
                                 B25014_012, B25014_013))/B25014_001,
         pct_overcrowded1=sum(c(B25014_007, B25014_006,B25014_005,
                                B25014_011,B25014_012, B25014_013))/B25014_001,
         pct_transit=B08006_008/B08006_001,
         pct_college=sum(c(B15003_022,B15003_023,B15003_024,B15003_025))/B15003_001,
         GEOID=as.numeric(GEOID)) %>% 
  select(GEOID, mhi, pct_hisp, pct_black, pct_nhwhite,
         pct_age2044, pct_age4564, pct_age6574, pct_age7584, pct_age85plus,
         total_pop, total_hisp, total_black,
         limited_engl, no_healthins, pct_college,
         pct_healthcareworkers,pct_service,pct_transit,
         pct_overcrowded2, pct_overcrowded15, pct_overcrowded1,
         pct_noncitizen, pct_foreignborn) %>% 
  left_join(area) %>% 
  mutate(density=total_pop/area)
# for NYC: get data at the "modified ZCTA"-level
cw<-fread("DAta/NYC/Geography-resources/ZCTA-to-MODZCTA.csv")
modified_zcta<-zcta_data_raw %>% 
  right_join(cw %>% rename(GEOID=ZCTA)) %>% 
  # some of them are 0 pop
  filter(B03002_001>0)
summary(modified_zcta)
# just 3 missing values in all variables in places with >0pop
modified_zcta %>% filter(is.na(B19013_001))
# 3 ZCTAs with 148, 43 and 198 people
# will have minor impact on estimates, we'll take a mean of income weighted by population, removing missings
modified_zcta_data<-
  full_join(modified_zcta %>% 
              group_by(MODZCTA) %>% 
              # mean of MHI weighted by pop
              summarise(B19013_001=weighted.mean(B19013_001, w=B03002_001, na.rm=T)),
            modified_zcta %>% 
              group_by(MODZCTA) %>% 
              # sum of all others (they are all counts)
              summarise_all(sum) %>% 
              select(-B19013_001, -GEOID)) %>% 
  rename(GEOID=MODZCTA) %>% 
  rowwise() %>% 
  # do some data management to create indicators
  mutate(mhi=B19013_001,
         pct_age2044=sum(c(B01001_008,B01001_009,B01001_010,B01001_011,B01001_012,
                           B01001_013,B01001_014,B01001_032,B01001_033,B01001_034,
                           B01001_035,B01001_036,B01001_037,B01001_038))/B01001_001,
         pct_age4564=sum(c(B01001_015,B01001_016,B01001_017,B01001_018,B01001_019,
                           B01001_039,B01001_040,B01001_041,B01001_042,B01001_043))/B01001_001,
         pct_age6574=sum(c(B01001_020,B01001_021,B01001_022,B01001_044,B01001_045,
                           B01001_046))/B01001_001,
         pct_age7584=sum(c(B01001_023,B01001_024,B01001_047,B01001_048))/B01001_001,
         pct_age85plus=sum(c(B01001_025,B01001_049))/B01001_001,
         pct_hisp=B03002_012/B03002_001,
         pct_black=B03002_004/B03002_001,
         pct_nhwhite=B03002_003/B03002_001,
         pct_noncitizen=B05001_006/B05001_001,
         pct_foreignborn=B06001_049/B06001_001,
         total_pop=B03002_001,
         total_hisp=B03002_012,
         total_black=B03002_004,
         limited_engl=sum(c(C16002_004, C16002_007, C16002_010, C16002_013))/C16002_001,
         no_healthins=sum(c(B27010_017, B27010_033, B27010_050, B27010_066))/B27010_001,
         pct_healthcareworkers=sum(c(C24010_016,C24010_017,C24010_018,C24010_020,C24010_052,C24010_053,C24010_054,C24010_056))/C24010_001,
         pct_service=sum(c(C24010_024, C24010_026, C24010_060, C24010_062))/C24010_001,
         pct_overcrowded2=sum(c(B25014_007, B25014_013))/B25014_001,
         pct_overcrowded15=sum(c(B25014_007, B25014_006,
                                 B25014_012, B25014_013))/B25014_001,
         pct_overcrowded1=sum(c(B25014_007, B25014_006,B25014_005,
                                B25014_011,B25014_012, B25014_013))/B25014_001,
         pct_transit=B08006_008/B08006_001,
         pct_college=sum(c(B15003_022,B15003_023,B15003_024,B15003_025))/B15003_001,
         GEOID=as.numeric(GEOID)) %>% 
  select(GEOID, mhi, pct_hisp, pct_black, pct_nhwhite,
         pct_age2044, pct_age4564, pct_age6574, pct_age7584, pct_age85plus,
         total_pop, total_hisp, total_black,
         limited_engl, no_healthins, pct_college,
         pct_healthcareworkers,pct_service,pct_transit,
         pct_overcrowded2, pct_overcrowded15, pct_overcrowded1,
         pct_noncitizen, pct_foreignborn) %>% 
  left_join(area) %>% 
  mutate(density=total_pop/area)


# Get NYC data over time
# tests by zcta (tests and positives)
# list of commits first day of the month
commits1<-c("9e26adc2c475d3378d7579e48e936f8a807b254b",
            "097cbd70aa00eb635b17b177bc4546b2fce21895")
dates1<-c("05/01/2020",
          "04/01/2020")
history<-paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",commits1,"/tests-by-zcta.csv")
nyc1<-map2_dfr(history, dates1, function(file, date){
  t<-fread(file)
  t$date<-date
  t
}) %>% rename(GEOID=MODZCTA) %>% 
  filter(!is.na(GEOID), GEOID!=99999) %>% 
  rename(positives=Positive,
         all=Total) %>% 
  select(GEOID, positives, all, date) 

# data by mod-zcta (includes deaths, etc.)
# list of commits first day of the month
commits2<-c("697dcb6c9bd3531b3838a733bfb26aeb97538224", 
            "c1469896916cb2c53e0e1faf3fa571d700602187",
            "a7315f625e7ef4eb15ed326d8ef66a703fccb613",
            "41f823d90887251735ad4e6f4814548fc9526606",
            "62444c1cbfda69c28ed468d14be3f332ea35eea7")
dates2<-c("10/01/2020",
          "09/01/2020",
          "08/01/2020",
          "07/01/2020",
          "06/01/2020")
history<-paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",commits2,"/data-by-modzcta.csv")
nyc2<-map2_dfr(history, dates2, function(file, date){
  t<-fread(file)
  t$date<-date
  t
}) %>% rename(GEOID=MODIFIED_ZCTA) %>% 
  filter(!is.na(GEOID), GEOID!=99999) %>% 
  # data now has positives and % positive, so calculate total tests
  mutate(all=round(COVID_CASE_COUNT/(PERCENT_POSITIVE/100))) %>% 
  rename(positives=COVID_CASE_COUNT,
         deaths=COVID_DEATH_COUNT,
         total_pop=POP_DENOMINATOR, 
         all2=TOTAL_COVID_TESTS) %>% 
  select(GEOID, positives, all, deaths, date, total_pop) 
# keeping NYC improved population estimations
nyc_pop<-nyc2 %>% filter(date==max(date)) %>% select(GEOID, total_pop)

nyc<-bind_rows(nyc1, nyc2 %>% select(-total_pop))
# make sure all zip codes are included every date
# if not, set their number of tests to 0
template<-expand.grid(GEOID=unique(nyc$GEOID),
                      date=unique(nyc$date), stringsAsFactors = F)
nyc<-full_join(nyc, template) %>% 
  mutate(positives=replace_na(positives, 0),
         all=replace_na(all, 0)) %>% 
  mutate(date=as.Date(date, "%m/%d/%Y")) %>% 
  arrange(GEOID, date)

table(nyc$date)

# check
# check
ggplot(nyc, aes(x=date, y=positives)) + geom_line(aes(group=GEOID))
ggplot(nyc, aes(x=date, y=all)) + geom_line(aes(group=GEOID))
ggplot(nyc, aes(x=date, y=deaths)) + geom_line(aes(group=GEOID))
nyc %>% group_by(GEOID) %>% 
  mutate(positives_new=positives-lag(positives),
         all_new=all-lag(all),
         deaths_new=deaths-lag(deaths)) %>% 
  filter(positives_new<0) %>% 
  arrange(positives_new) %>% 
  View
nyc %>% group_by(GEOID) %>% 
  mutate(positives_new=positives-lag(positives),
         all_new=all-lag(all),
         deaths_new=deaths-lag(deaths)) %>% 
  filter(all_new<0) %>% 
  arrange(all_new) %>% 
  View
nyc %>% group_by(GEOID) %>% 
  mutate(positives_new=positives-lag(positives),
         all_new=all-lag(all),
         deaths_new=deaths-lag(deaths)) %>% 
  filter(deaths_new<0) %>% 
  arrange(deaths_new) %>% 
  View
# NYC reports in their github some corrections to zipcode level data
# Specifically, 2 zip codes have corrections to testing and case data in August/september, 
## resulting in negative new tests/cases
## for cases both are very minor changes
## for tests both are large changes (-1k and -1.4k)
# A few more zip codes have minor changes to the total number of deaths (mostly +-1)
# for cases and deaths: set to 0 (this assumes the later value is correct, which is plausible)
# for tests: setting them to 0 will cause an issue in the positivity model (denominator of 0).
## instead: impute the value of the "corrected" month t to the average of t-1 and t+1
## specifically: impute the value of August in c(10018,11229) for their average of July and September
extract<-nyc %>% filter(GEOID%in%c(10018,11229), 
                        date%in%mdy("09-01-2020", "07-01-2020")) %>% 
  group_by(GEOID) %>% 
  summarise(all=round(mean(all))) %>% 
  mutate(date=mdy("08-01-2020"))
extract<-nyc %>% filter(GEOID%in%c(10018,11229), 
                        date%in%mdy("08-01-2020")) %>% 
  select(-all) %>% full_join(extract)

nyc<-bind_rows(nyc %>% filter(!(GEOID%in%c(10018,11229)& 
                                date%in%mdy("08-01-2020"))), extract) %>% arrange(GEOID, date)



# get philadelphia data over time
# first, data obtained by april 24th with all cases up to that date
# exclude April 24th (incomplete data)
phl1<-fread("Data/PHL/Tableau_file_04252020.csv") %>% 
  # renaming zipcode identifier to GEOID
  rename(GEOID=ZIP1,
         test=Result,
         date=`Result Date`) %>% 
  mutate(n=1,
         date=mdy(date)) %>% 
  group_by(GEOID, date, test) %>% 
  summarise(n=sum(n)) %>% 
  filter(test!="") %>% 
  spread(test, n)
table(phl1$date)

# fill in the gaps
template<-expand.grid(GEOID=unique(phl1$GEOID),
                      date=seq(min(phl1$date, na.rm=T), max(phl1$date, na.rm=T), by=1))
phl1<-phl1 %>% full_join(template) %>% 
  mutate(negative=replace_na(NEG, 0),
         positive=replace_na(POS, 0),
         total_tests=negative+positive) %>% 
  filter(!is.na(date)) %>% 
  arrange(GEOID, date) %>% 
  group_by(GEOID) %>% 
  mutate(cumulative_tests=cumsum(total_tests),
         cumulative_positives=cumsum(positive)) %>% 
  rename(positives=cumulative_positives,
         all=cumulative_tests) %>% 
  select(GEOID, date, positives, all) %>% 
  filter(date>=ymd("2020-02-29"),
         date<ymd("2020-04-24")) %>% 
  filter(date%in%c(ymd("2020-03-01"), ymd("2020-04-01")))
  
# then files downloaded almost daily from open data philly
history<-list.files("Data/PHL/", pattern="covid_cases_by_zip")
phl<-map_dfr(history, function(file){
  print(file)
  t<-fread(paste0("Data/PHL/", file))
  date<-as_date(t %>% pull(etl_timestamp) %>% unique)
  if (date=="2020-10-01"){
    t<-t %>% 
      rowwise() %>% 
      mutate(all=sum(POS, NEG, na.rm=T),
             POS=replace_na(POS, 0),
             GEOID=as.numeric(zip_code),
             date=date) %>% 
      rename(positives=POS) %>% 
      select(GEOID, date, positives, all) %>% 
      filter(!is.na(GEOID))
  } else {
    t<-t %>% select(covid_status, zip_code, count) %>% 
      spread(covid_status, count) %>% 
      rowwise() %>% 
      mutate(all=sum(POS, NEG),
             GEOID=as.numeric(zip_code),
             date=date) %>% 
      rename(positives=POS) %>% 
      select(GEOID, date, positives, all) %>% 
      filter(!is.na(GEOID))
  }
  
  t
})
# take zip code list from last date 
phl_zcta<-phl %>% filter(date==max(date)) %>% pull(GEOID) %>% unique
phl<-bind_rows(phl, phl1)
# make sure all zip codes are included every date
# if not, set their number of tests to 0
template<-expand.grid(GEOID=unique(phl$GEOID),
                      date=unique(phl$date), stringsAsFactors = F)
phl<-full_join(phl, template) %>% 
  mutate(positives=replace_na(positives, 0),
         all=replace_na(all, 0)) %>% 
  filter(GEOID%in%phl_zcta)
# make sure no duplicate dates
phl<-phl %>% group_by(GEOID) %>% 
  filter(!duplicated(date)) %>% 
  arrange(GEOID, date)

# get deaths in PHL zipcodes
history<-list.files("Data/PHL/", pattern="covid_deaths_by_zip")
phl_deaths<-map_dfr(history, function(file){
  t<-fread(paste0("Data/PHL/", file))
  date<-as_date(t %>% pull(etl_timestamp) %>% unique)
  t<-t %>% select(covid_outcome, zip_code, count) %>% 
    spread(covid_outcome, count) %>% 
    rowwise() %>% 
    mutate(deaths=DIED,
           GEOID=as.numeric(zip_code),
           date=date) %>% 
    select(GEOID, date, deaths) %>% 
    filter(!is.na(GEOID))
  t
})
# make sure all zip codes are included every date
# if not, set their number of deaths to 0
# note: taking the list of zip codes from phl_Zcta above
template<-expand.grid(GEOID=phl_zcta,
                      date=unique(phl_deaths$date), stringsAsFactors = F)
phl_deaths<-full_join(phl_deaths, template) %>% 
  mutate(deaths=replace_na(deaths, 0)) %>% 
  filter(GEOID%in%phl_zcta)
# make sure no duplicate dates
phl_deaths<-phl_deaths %>% group_by(GEOID) %>% 
  filter(!duplicated(date))

# merge
phl<-full_join(phl, phl_deaths)
table(phl$date)


ggplot(phl, aes(x=date, y=positives)) + geom_line(aes(group=GEOID))
ggplot(phl, aes(x=date, y=all)) + geom_line(aes(group=GEOID))
ggplot(phl, aes(x=date, y=deaths)) + geom_line(aes(group=GEOID))
phl %>% group_by(GEOID) %>% 
  mutate(positives_new=positives-lag(positives),
         all_new=all-lag(all),
         deaths_new=deaths-lag(deaths)) %>% 
  filter(positives_new<0, positives!=0) %>% 
  arrange(positives_new) %>% 
  View
# no issue
phl %>% group_by(GEOID) %>% 
  mutate(positives_new=positives-lag(positives),
         all_new=all-lag(all),
         deaths_new=deaths-lag(deaths)) %>% 
  filter(all_new<0, all!=0) %>% 
  arrange(all_new) %>% 
  View
# no issue
phl %>% group_by(GEOID) %>% 
  mutate(positives_new=positives-lag(positives),
         all_new=all-lag(all),
         deaths_new=deaths-lag(deaths)) %>% 
  filter(deaths_new<0, deaths!=0) %>% 
  arrange(deaths_new) %>% 
  View


# Chicago data (from CDPH)
chi<-fread("Data/Chicago/COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code_100720.csv") %>% 
  mutate(GEOID=as.numeric(`ZIP Code`),
         date=mdy(`Week End`)) %>% 
  filter(!is.na(GEOID)) %>% 
  rename(positives=`Cases - Cumulative`,
         all=`Tests - Cumulative`,
         deaths=`Deaths - Cumulative`) %>% 
  select(GEOID, date, positives, all, deaths) %>% 
  mutate(positives=replace_na(positives, 0)) %>% 
  arrange(GEOID, date)
# check
ggplot(chi, aes(x=date, y=positives)) + geom_line(aes(group=GEOID))
ggplot(chi, aes(x=date, y=all)) + geom_line(aes(group=GEOID))
ggplot(chi, aes(x=date, y=deaths)) + geom_line(aes(group=GEOID))
chi %>% group_by(GEOID) %>% 
  mutate(positives_new=positives-lag(positives),
         all_new=all-lag(all),
         deaths_new=deaths-lag(deaths)) %>% 
  filter(positives_new<0) %>% 
  arrange(positives_new) %>% 
  View
chi %>% group_by(GEOID) %>% 
  mutate(positives_new=positives-lag(positives),
         all_new=all-lag(all),
         deaths_new=deaths-lag(deaths)) %>% 
  filter(all_new<0) %>% 
  arrange(all_new) %>% 
  View
chi %>% group_by(GEOID) %>% 
  mutate(positives_new=positives-lag(positives),
         all_new=all-lag(all),
         deaths_new=deaths-lag(deaths)) %>% 
  filter(deaths_new<0) %>% 
  arrange(deaths_new) %>% 
  View

final_zcta_data<-bind_rows( zcta_data %>% 
                              filter(substr(sprintf("%05d", GEOID), 1, 2)%in%c(19, 60)),
                            modified_zcta_data %>% 
                             select(-total_pop) %>% 
                             left_join(nyc_pop))
# put all of if together
all<-bind_rows(phl %>% 
            mutate(city="Philadelphia"),
            nyc %>% 
            mutate(city="New York City"),
            chi %>% 
            mutate(city="Chicago"))%>%
  left_join(final_zcta_data) %>% 
  # remove those without (or very little) population/ACS data and remove industrial zipcodes (e.g.: philly airport)
  filter(!is.na(mhi)) %>% 
  filter(total_pop>100)

last_available_date<-all %>% 
  # compute the 3 key outcomes
  mutate(pct_pos=positives/all,
         pos_pc=positives/total_pop*1000,
         tests_pc=all/total_pop*1000, 
         deaths_pc=deaths/total_pop*1000) %>% 
  group_by(city) %>% 
  filter(date==max(date))


# get monthly  testing, positivity, incidence and deaths
# For March: first date in april=new things in March (1-2 cases per city in January/February)
# For the rest, for month t: first available date in month T+1 - first availalbe date in month t
changes<-all %>%
  mutate(date=as_date(date)) %>% 
  ungroup() %>% 
  mutate(month=month(date)) %>% 
  arrange(GEOID, (date)) %>% 
  group_by(GEOID, month) %>% 
  filter(!duplicated(GEOID), month>=4)

march_nodeath<-changes %>% filter(month==4) %>% 
  mutate(positives_new=positives,
         all_new=all) %>% 
  select(GEOID, city, month, matches("new")) 
rest_nodeath<-changes %>%
  arrange(GEOID, month) %>% 
  group_by(GEOID) %>% 
  mutate(positives_new=positives-lag(positives),
         all_new=all-lag(all)) %>% 
  select(GEOID, city, month, matches("new")) %>% 
  filter(month>4)
changes_nodeath<-bind_rows(march_nodeath, rest_nodeath) %>% 
  mutate(month=month-1)
summary(changes_nodeath)
# for deaths in chicago it's the same as above
march_death_chi<-changes %>% filter(city=="Chicago", month==4) %>% 
  mutate(deaths_new=deaths) %>% 
  select(GEOID, city, month, matches("new")) 
rest_death_chi<-changes %>%
  filter(city=="Chicago") %>% 
  arrange(GEOID, month) %>% 
  group_by(GEOID) %>% 
  mutate(deaths_new=deaths-lag(deaths)) %>% 
  select(GEOID, city, month, matches("new")) %>% 
  filter(month>4)
# in NYC and Philadelphia, may is first month, so take june 1st as cumulative data through May, and then changes in June and July
may_death_nochi<-changes %>% filter(city!="Chicago", month==6) %>% 
  mutate(deaths_new=deaths) %>% 
  select(GEOID, city, month, matches("new")) 
rest_death_nochi<-changes %>%
  filter(city!="Chicago") %>% 
  arrange(GEOID, month) %>% 
  group_by(GEOID) %>% 
  mutate(deaths_new=deaths-lag(deaths)) %>% 
  select(GEOID, city, month, matches("new")) %>% 
  filter(month>6)
changes_death<-bind_rows(march_death_chi, rest_death_chi,
                         may_death_nochi, rest_death_nochi) %>% 
  mutate(month=month-1)
summary(changes_death)
changes<-full_join(changes_nodeath, changes_death) %>% 
  # a few corrections to the data were made in new days, some of them result in negative new deaths, censoring at 0
  mutate(deaths_new=ifelse(deaths_new<0, 0, deaths_new),
         positives_new=ifelse(positives_new<0, 0, positives_new),
         all_new=ifelse(all_new<0, 0, all_new)) %>% 
  left_join(final_zcta_data ) %>% 
  mutate(pct_pos_new=positives_new/all_new,
         pos_pc_new=positives_new/total_pop*1000,
         tests_pc_new=all_new/total_pop*1000, 
         deaths_pc_new=deaths_new/total_pop*1000)
summary(changes)
table(changes$month, changes$city)

# get SVI
# first, get ZCTA to CT crosswalk
cw<-fread("Data/zcta_tract_rel_10.txt") %>% 
  filter(ZCTA5%in%last_available_date$GEOID) %>% 
  rename(FIPS=GEOID,
         GEOID=ZCTA5) %>% 
  select(FIPS, GEOID, ZPOPPCT) %>% 
  mutate(ZPOPPCT=ZPOPPCT/100)
# get SVI by CT, convert -999 to NA, join with crosswalk, and aggregate to ZCTA
# one for each state (as recommended by CDC)
svi1<-fread("Data/svi/Illinois.csv") %>% 
  mutate(RPL_THEMES=ifelse(RPL_THEMES==-999, NA, RPL_THEMES),
         RPL_THEME1=ifelse(RPL_THEME1==-999, NA, RPL_THEME1),
         RPL_THEME2=ifelse(RPL_THEME2==-999, NA, RPL_THEME2),
         RPL_THEME3=ifelse(RPL_THEME3==-999, NA, RPL_THEME3),
         RPL_THEME4=ifelse(RPL_THEME4==-999, NA, RPL_THEME4)) %>% 
  left_join(cw) %>% 
  filter(!is.na(GEOID)) %>% 
  group_by(GEOID) %>% 
  summarise(svi=weighted.mean(RPL_THEMES, w=ZPOPPCT, na.rm = T),
            svi1=weighted.mean(RPL_THEME1, w=ZPOPPCT, na.rm = T),
            svi2=weighted.mean(RPL_THEME2, w=ZPOPPCT, na.rm = T),
            svi3=weighted.mean(RPL_THEME3, w=ZPOPPCT, na.rm = T),
            svi4=weighted.mean(RPL_THEME4, w=ZPOPPCT, na.rm = T)) %>% 
  mutate(svi=as.numeric(scale(svi)),
         svi1=as.numeric(scale(svi1)),
         svi2=as.numeric(scale(svi2)),
         svi3=as.numeric(scale(svi3)),
         svi4=as.numeric(scale(svi4)))
svi2<-fread("Data/svi/NewYork.csv") %>% 
  mutate(RPL_THEMES=ifelse(RPL_THEMES==-999, NA, RPL_THEMES),
         RPL_THEME1=ifelse(RPL_THEME1==-999, NA, RPL_THEME1),
         RPL_THEME2=ifelse(RPL_THEME2==-999, NA, RPL_THEME2),
         RPL_THEME3=ifelse(RPL_THEME3==-999, NA, RPL_THEME3),
         RPL_THEME4=ifelse(RPL_THEME4==-999, NA, RPL_THEME4)) %>% 
  left_join(cw) %>% 
  filter(!is.na(GEOID)) %>% 
  group_by(GEOID) %>% 
  summarise(svi=weighted.mean(RPL_THEMES, w=ZPOPPCT, na.rm = T),
            svi1=weighted.mean(RPL_THEME1, w=ZPOPPCT, na.rm = T),
            svi2=weighted.mean(RPL_THEME2, w=ZPOPPCT, na.rm = T),
            svi3=weighted.mean(RPL_THEME3, w=ZPOPPCT, na.rm = T),
            svi4=weighted.mean(RPL_THEME4, w=ZPOPPCT, na.rm = T)) %>% 
  mutate(svi=as.numeric(scale(svi)),
         svi1=as.numeric(scale(svi1)),
         svi2=as.numeric(scale(svi2)),
         svi3=as.numeric(scale(svi3)),
         svi4=as.numeric(scale(svi4)))
svi3<-fread("Data/svi/Pennsylvania.csv") %>% 
  mutate(RPL_THEMES=ifelse(RPL_THEMES==-999, NA, RPL_THEMES),
         RPL_THEME1=ifelse(RPL_THEME1==-999, NA, RPL_THEME1),
         RPL_THEME2=ifelse(RPL_THEME2==-999, NA, RPL_THEME2),
         RPL_THEME3=ifelse(RPL_THEME3==-999, NA, RPL_THEME3),
         RPL_THEME4=ifelse(RPL_THEME4==-999, NA, RPL_THEME4)) %>% 
  left_join(cw) %>% 
  filter(!is.na(GEOID)) %>% 
  group_by(GEOID) %>% 
  summarise(svi=weighted.mean(RPL_THEMES, w=ZPOPPCT, na.rm = T),
            svi1=weighted.mean(RPL_THEME1, w=ZPOPPCT, na.rm = T),
            svi2=weighted.mean(RPL_THEME2, w=ZPOPPCT, na.rm = T),
            svi3=weighted.mean(RPL_THEME3, w=ZPOPPCT, na.rm = T),
            svi4=weighted.mean(RPL_THEME4, w=ZPOPPCT, na.rm = T)) %>% 
  mutate(svi=as.numeric(scale(svi)),
         svi1=as.numeric(scale(svi1)),
         svi2=as.numeric(scale(svi2)),
         svi3=as.numeric(scale(svi3)),
         svi4=as.numeric(scale(svi4)))
svi<-bind_rows(svi1, svi2, svi3)
last_available_date<-full_join(last_available_date, svi)
changes<-full_join(changes, svi)


# get neighbors for each city
neighbors_id<-all %>%
  group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_by(city) %>% 
  group_keys()
neighbors<-all %>% 
  group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_by(city) %>% 
  group_map(~{
    if (.y$city=="New York City"){
      # for NYC, there are a few things to fix:
      shp_clusters<-merge(shp_zip_mod, .x, by="GEOID", all.y=T, all.x=F) 
      nbmat<-poly2nb(shp_clusters)
      # fix roosvelt island, 0 neighbors, but is connected across the river.
      nbmat[[which(shp_clusters$GEOID==10044)]]<-which(shp_clusters$GEOID%in%c(11106, 11101))
      nbmat[[which(shp_clusters$GEOID==11106)]]<-c(nbmat[[which(shp_clusters$GEOID==11106)]], which(shp_clusters$GEOID%in%c(10044)))
      nbmat[[which(shp_clusters$GEOID==11101)]]<-c(nbmat[[which(shp_clusters$GEOID==11101)]], which(shp_clusters$GEOID%in%c(10044)))
      # also fix NB separated because of odd MODZCTA shape: 11101 and 11222
      nbmat[[which(shp_clusters$GEOID==11101)]]<-c(nbmat[[which(shp_clusters$GEOID==11101)]], which(shp_clusters$GEOID%in%c(11222)))
      nbmat[[which(shp_clusters$GEOID==11222)]]<-c(nbmat[[which(shp_clusters$GEOID==11222)]], which(shp_clusters$GEOID%in%c(11101)))
    } else {
      shp_clusters<-merge(shp_zip, .x, by="GEOID", all.y=T, all.x=F) 
      nbmat<-poly2nb(shp_clusters %>% filter(!duplicated(GEOID)))
    }
    nbmat
  })
# save neighbors as adjacency matrices for INLA
nb2INLA(file="Data/nb_inla_chi.adj", nb=neighbors[[1]])
nb2INLA(file="Data/nb_inla_nyc.adj", nb=neighbors[[2]])
nb2INLA(file="Data/nb_inla_phl.adj", nb=neighbors[[3]])


# last: get city-level tests, cases and deaths
phl_city<-fread("Data/PHL/covid_cases_by_date_101920.csv") %>% 
  mutate(date=as_date(collection_date)) %>% 
  select(date, test_result, count) %>% 
  spread(test_result, count) %>% 
  mutate(positive=replace_na(positive, 0),
         negative=replace_na(negative, 0),
         positives=positive,
         all=positive+negative,
         city="Philadelphia") %>% 
  select(city, date, positives)
phl_city2<-fread("Data/PHL/covid_deaths_by_date_101920.csv") %>% 
  mutate(date=as_date(clinical_date_of_death),
         deaths=count) %>% 
  select(date, deaths) %>% 
  arrange(date) %>% 
  filter(!is.na(date))
#manually adding deaths from March from the PDPH dashboard https://www.phila.gov/programs/coronavirus-disease-2019-covid-19/testing-and-data/
manual<-data.frame(date=as.Date(c("03-22-2020","03-23-2020","03-25-2020",
                          "03-26-2020","03-27-2020","03-28-2020",
                          "03-29-2020","03-30-2020","03-31-2020"),
                          format="%m-%d-%Y"),
                   deaths=c(1, 2, 1, 
                            1, 5, 2,
                            2, 5, 5))
phl_city2<-bind_rows(phl_city2, manual)

phl_city<-full_join(phl_city, phl_city2)
  
chi_city<-fread("Data/Chicago/COVID-19_Daily_Cases__Deaths__and_Hospitalizations_101920.csv") %>% 
  mutate(date=as.Date(Date, format="%m/%d/%y")) %>% 
  mutate(positives=`Cases - Total`,
         deaths=`Deaths - Total`,
         city="Chicago") %>% 
  select(city, date, positives, deaths) %>% 
  arrange(date) %>% filter(!is.na(date))
# new york
nyc_city<-fread("Data/NYC/case-hosp-death_101920.csv") %>% 
  mutate(date=as.Date(DATE_OF_INTEREST, format="%m/%d/%Y"),
         positives=CASE_COUNT,
         deaths=DEATH_COUNT, city="New York City") %>% 
  select(city, date, positives, deaths) %>% 
  arrange(date)


city_level<-bind_rows(phl_city, chi_city, nyc_city)
# fill in gaps with 0s
dates<-seq(as.Date("01-01-2020", format="%m-%d-%Y"), max(city_level$date), by=1)
template<-expand.grid(date=dates,
                      city=unique(city_level$city))
city_level<-full_join(city_level, template) %>% 
  mutate(positives=replace_na(positives, 0),
         deaths=replace_na(deaths, 0)) %>% 
  arrange(city, date) %>% 
  # smooth over 7 days 
  group_by(city) %>% 
  mutate(positives=rollmean(positives, k=7, align="center", na.pad=T),
         deaths=rollmean(deaths, k=7, align="center", na.pad=T)) %>% 
  # delete first/last 7 observations [median delay PDPH uses for delayed reporting]
  arrange(city, desc(date)) %>% 
  group_by(city) %>% 
  slice(-(1:7)) %>% 
  #merge with city level pop
  full_join(all %>% filter(date==max(date)) %>% group_by(city) %>% 
              summarise(total_pop=sum(total_pop))) %>% 
  mutate(positives_pc=positives/total_pop*100000,
         deaths_pc=deaths/total_pop*100000)


save(last_available_date, changes,
     city_level,loadings, 
     neighbors_id,neighbors,
     shp_zip, shp_zip_mod,
     bbox_il, bbox_pa, bbox_ny,
     file="Data/clean_data.rdata")
