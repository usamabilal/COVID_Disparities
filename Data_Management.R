rm(list=ls())
library(tidyverse)
library(data.table)
library(tidycensus)
library(readxl)
library(broom)
library(gridExtra)
library(foreign)
library(lubridate)
library(scales)
library(grid)
library(rgdal)
library(rgeos)
library(sf)
options(scipen=999)
# only need to run these two commands once
# download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_zcta510_500k.zip",
#               destfile ="Data/cb_2018_us_zcta510_500k.zip" )
# unzip("Data/cb_2018_us_zcta510_500k.zip", exdir = "Data/zipcodeshp/")
area<-read.dbf("Data/zipcodeshp/cb_2018_us_zcta510_500k.dbf") %>% 
  mutate(GEOID=as.numeric(as.character(GEOID10)),
         area=ALAND10/1000000/2.59) %>% 
  select(GEOID, area)

# obtain a census api key https://api.census.gov/data/key_signup.html
# census_api_key(key="xxx", install = T)
# use this dataset to check variable names
vars<-load_variables(year=2018, "acs5")
# example:
vars %>% filter(grepl("foreign", concept, ignore.case=T))
# healthcare workers:
vars %>% filter(grepl("C24010", name), grepl("health", label, ignore.case=T)) %>% pull(name)
# get data at the zipcode level and process
zipcode_data<-get_acs(geography = "zcta",
                      variables=c(
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
  rowwise() %>% 
  # do some data management to create indicators
  mutate(mhi=B19013_001,
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
         total_pop, total_hisp, total_black,
         limited_engl, no_healthins, pct_college,
         pct_healthcareworkers,pct_service,pct_transit,
         pct_overcrowded2, pct_overcrowded15, pct_overcrowded1,
         pct_noncitizen, pct_foreignborn) %>% 
  left_join(area) %>% 
  mutate(density=total_pop/area)

# Get NYC data over time: this is just a saved html of the github page for the zcta file history
# we get the commit ids
file<-read_file("Data/History for tests-by-zcta.csv - nychealth_coronavirus-data.html")
file2<-read_file("Data/History for tests-by-zcta.csv - nychealth_coronavirus-data_older.html")

list<-gregexpr("repo:250296192:commit:", file)[[1]]
commits<-map_chr(list, function(init){
  substr(file, init+22, init+22+39)  
})
list2<-gregexpr("repo:250296192:commit:", file2)[[1]]
commits2<-map_chr(list2, function(init){
  substr(file2, init+22, init+22+39)  
})
commits<-c(commits, commits2)

list<-gregexpr("Commits on ", file)[[1]]
dates<-map(list, function(init){
  mdy(paste0(gsub("\\,", "", substr(file, init+11, init+11+6)  ), ", ", 2020))
}) %>% do.call(what = c)
list2<-gregexpr("Commits on ", file2)[[1]]
dates2<-map(list2, function(init){
  mdy(paste0(gsub("\\,", "", substr(file2, init+11, init+11+6)  ), ", ", 2020))
}) %>% do.call(what = c)
dates<-c(dates, dates2)

# and then import them
history<-paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",commits,"/tests-by-zcta.csv")
# temporarily remove third item (may 11)
history<-history[-3]
nyc<-map2_dfr(history, dates, function(file, date){
  t<-fread(file)
  t$date<-date
  t
}) %>% rename(GEOID=MODZCTA) %>% 
  filter(!is.na(GEOID), GEOID!=99999) %>% 
  rename(positives=Positive,
         all=Total) %>% 
  select(GEOID, positives, all, date)
# apparently April 26th was somehow erroneous (big jump in cases, and was fixed on 27)
nyc<-nyc %>% filter(date!=as_date("2020-04-26"))

# get philadelphia data over time
# first, data obtained by april 24th with all cases up to that date
# exclude April 24th (incomplete data)
phl1<-fread("Data/PHL/Tableau_file_04252020.csv") %>% 
  # renaming zipcode identifier to GEOID
  rename(GEOID=ZIP1,
         test=Result,
         date=`Result Date`) %>% 
  mutate(n=1) %>% 
  group_by(GEOID, date, test) %>% 
  summarise(n=sum(n)) %>% 
  filter(test!="") %>% 
  spread(test, n)
table(mdy(phl1$date))
# Number of zipcodes is stable from March 20th to April 24th

# fill in the gaps
template<-expand.grid(GEOID=unique(phl1$GEOID),
                      date=unique(phl1$date))
phl1<-phl1 %>% full_join(template) %>% 
  mutate(NEG=replace_na(NEG, 0),
         POS=replace_na(POS, 0)) %>% 
  rename(negative=NEG, positive=POS) %>% 
  rowwise() %>% 
  mutate(total_tests=sum(c(negative+positive))) %>% 
  arrange(GEOID, date) %>% 
  mutate(date=mdy(date)) %>% 
  filter(!is.na(date)) %>% 
  filter(date>=ymd("2020-03-20"),
         date<ymd("2020-04-24")) %>% 
  arrange(GEOID, date) %>% 
  group_by(GEOID) %>% 
  mutate(cumulative_tests=cumsum(total_tests),
         cumulative_positives=cumsum(positive)) %>% 
  select(GEOID, date, cumulative_tests, cumulative_positives) %>% 
  rename(positives=cumulative_positives,
         all=cumulative_tests)
# then files downloaded almost daily from open data philly
history<-list.files("Data/PHL/", pattern="covid_cases_by_zip")
phl<-map_dfr(history, function(file){
  t<-fread(paste0("Data/PHL/", file))
  date<-as_date(t %>% pull(etl_timestamp) %>% unique)
  t<-t %>% select(covid_status, zip_code, count) %>% 
    spread(covid_status, count) %>% 
    rowwise() %>% 
    mutate(all=sum(POS, NEG),
           GEOID=as.numeric(zip_code),
           date=date) %>% 
    rename(positives=POS) %>% 
    select(GEOID, date, positives, all) %>% 
    filter(!is.na(GEOID))
  t
})

phl<-bind_rows(phl, phl1)

# get chicago at two time points
# get place dataset for Illinois
# download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_17_place_500k.zip",
#               destfile ="Data/cb_2018_17_place_500k.zip" )
# unzip("Data/cb_2018_17_place_500k.zip", exdir = "Data/Chicago/place_il/")
place_il<-readOGR('Data/Chicago/place_il/', 'cb_2018_17_place_500k') %>% st_as_sf
zcta = readOGR('Data/zipcodeshp/', 'cb_2018_us_zcta510_500k') %>% st_as_sf
chicago_zctas<-place_il %>% filter(grepl("^Chicago$", NAME, ignore.case=T)) %>% 
  st_intersection(zcta) %>% 
  mutate(ZCTA5CE10=as.numeric(as.character(ZCTA5CE10))) %>% 
  pull(ZCTA5CE10)
chicago1<-fread("Data/Chicago/Illinois_zipcode_7_May.csv") %>% 
  rename(GEOID=Zip, positives=`Positive Cases`, date=Date,
         total=Tested) %>% 
  select(GEOID, positives, total, date)  %>% 
  mutate(GEOID=as.numeric(GEOID),
         date=mdy(date),
         positives=as.numeric(positives),
         all=as.numeric(total),
         loc="Illinois") %>% 
  filter(!is.na(GEOID)) %>% 
  filter(GEOID%in%chicago_zctas) 
chicago2<-fread("Data/Chicago/Illinois_tests_zip_13_may.csv") %>% 
  rename(GEOID=Zip, positives=`Positive Cases`, date=Date,
         total=Tested) %>% 
  select(GEOID, positives, total, date)  %>% 
  mutate(GEOID=as.numeric(GEOID),
         date=mdy(date),
         positives=as.numeric(positives),
         all=as.numeric(total),
         loc="Illinois") %>% 
  filter(!is.na(GEOID)) %>% 
  filter(GEOID%in%chicago_zctas)
chicago<-bind_rows(chicago1, chicago2) %>% select(GEOID, positives, all, date)


all<-bind_rows(phl %>% mutate(city="Philadelphia"), 
               nyc %>% mutate(city="New York City"),
               chicago %>% mutate(city="Chicago")) %>% 
  left_join(zipcode_data) %>% 
  # remove those without population/ACS data and remove industrial zipcodes (e.g.: philly airport)
  filter(!is.na(mhi)) %>% 
  filter(total_pop>100) %>% 
  # compute the 3 key outcomes
  mutate(pct_pos=positives/all,
         pos_pc=positives/total_pop*1000,
         tests_pc=all/total_pop*1000)

# PCA to get first component. City-stratified
pca<-all %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_modify(~{
    pca<-prcomp(.x %>% mutate(logmhi=log(mhi)) %>% 
      select(pct_nhwhite, logmhi, 
             pct_college, no_healthins,pct_service, 
             pct_overcrowded1), scale=T)
    data.frame(GEOID=.x$GEOID, pc1=pca$x[,1])
  })
loadings<-all %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_modify(~{
    pca<-prcomp(.x %>% mutate(logmhi=log(mhi)) %>% 
                  select(pct_nhwhite, logmhi, 
                         pct_college, no_healthins, pct_service, 
                         pct_overcrowded1), scale=T)
    data.frame(var=names(pca$rotation[,1]),loading=pca$rotation[,1]) %>% 
      mutate_at(-1,format, digits=2, nsmall=2)
  })

all<-all %>% full_join(pca)
save(all, loadings, file="Data/clean_data.rdata")
