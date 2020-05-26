rm(list=ls())
library(tidyverse)
library(data.table)
library(tidycensus)
library(jsonlite)
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

# load generic shapefiles
# states
shp_states = readOGR('Data/stateshp/', 'cb_2018_us_state_500k')
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


# Get NYC data over time: this is just a saved html of the github page for the zcta file history
# 3 files: 2 for the old tests by zcta, 1 for the new data by zcta
# we get the commit ids
file<-read_file("Data/NYC/History for tests-by-zcta.csv - nychealth_coronavirus-data_part1.html")
file2<-read_file("Data/NYC/History for tests-by-zcta.csv - nychealth_coronavirus-data_part2.html")
file3<-read_file("Data/NYC/History for data-by-modzcta.csv - nychealth_coronavirus-data.html")
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
# 7th item(may 11) has some issues for downloading
history<-history[-7]
nyc1<-map2_dfr(history, dates, function(file, date){
  t<-fread(file)
  t$date<-date
  t
}) %>% rename(GEOID=MODZCTA) %>% 
  filter(!is.na(GEOID), GEOID!=99999) %>% 
  rename(positives=Positive,
         all=Total) %>% 
  select(GEOID, positives, all, date) 
# also get the new data
list3<-gregexpr("repo:250296192:commit:", file3)[[1]]
commits3<-map_chr(list3, function(init){
  substr(file3, init+22, init+22+39)  
})
list3<-gregexpr("Commits on ", file3)[[1]]
dates3<-map(list3, function(init){
  mdy(paste0(gsub("\\,", "", substr(file3, init+11, init+11+6)  ), ", ", 2020))
}) %>% do.call(what = c)
history<-paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",commits3,"/data-by-modzcta.csv")
nyc2<-map2_dfr(history, dates3, function(file, date){
  t<-fread(file)
  t$date<-date
  t
}) %>% rename(GEOID=MODIFIED_ZCTA) %>% 
  filter(!is.na(GEOID), GEOID!=99999) %>% 
  # data now has positives and % positive, so calculate total tests
  mutate(all=round(COVID_CASE_COUNT/(PERCENT_POSITIVE/100))) %>% 
  rename(positives=COVID_CASE_COUNT,
         total_pop=POP_DENOMINATOR) %>% 
  select(GEOID, positives, all, date, total_pop) 
nyc_pop<-nyc2 %>% filter(date==max(date)) %>% select(GEOID, total_pop)

# April 26th was somehow erroneous  according to github notes
# (big jump in cases, and was fixed on 27)
nyc<-bind_rows(nyc1, nyc2 %>% select(-total_pop))
nyc<-nyc %>% filter(date!=as_date("2020-04-26"))
# there also seems to be an error on 4-10, where zip code 11697 has two observations, one of them duplicated from the previous day
# removing it manually
nyc<-nyc %>% filter(!(date==as_date("2020-04-10")&GEOID==11697&positives==52))

# make sure all zip codes are included every date
# if not, set their number of tests to 0
template<-expand.grid(GEOID=unique(nyc$GEOID),
                      date=unique(nyc$date), stringsAsFactors = F)
nyc<-full_join(nyc, template) %>% 
  mutate(positives=replace_na(positives, 0),
         all=replace_na(all, 0))


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
# keeping a total_pop as provided by PDPH
phl_pop<-fread("Data/PHL/Tableau_file_04252020.csv") %>% 
  rename(GEOID=ZIP1, total_pop=`Zip Pop`) %>% 
  filter(!duplicated(GEOID)) %>% 
  select(GEOID, total_pop)


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
  filter(date>=ymd("2020-03-21"),
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
# take zip code list from May 18th 
phl_zcta<-phl %>% filter(date=="2020-05-18") %>% pull(GEOID) %>% unique
phl<-bind_rows(phl, phl1)
# make sure all zip codes are included every date
# if not, set their number of tests to 0
template<-expand.grid(GEOID=unique(phl$GEOID),
                      date=unique(phl$date), stringsAsFactors = F)
phl<-full_join(phl, template) %>% 
  mutate(positives=replace_na(positives, 0),
         all=replace_na(all, 0)) %>% 
  filter(GEOID%in%phl_zcta)


# get chicago at two time points
# get place dataset for Illinois
# download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_17_place_500k.zip",
#               destfile ="Data/cb_2018_17_place_500k.zip" )
# unzip("Data/cb_2018_17_place_500k.zip", exdir = "Data/Chicago/place_il/")
place_il<-readOGR('Data/Chicago/place_il/', 'cb_2018_17_place_500k') %>% st_as_sf
chicago_zctas2<-place_il %>% filter(grepl("^Chicago$", NAME, ignore.case=T)) %>% 
  st_intersection(shp_zip %>% st_as_sf) %>% 
  mutate(ZCTA5CE10=as.numeric(as.character(ZCTA5CE10))) %>% 
  pull(ZCTA5CE10)
# get chicago pop
chi_pop<-fread("Data/Chicago/COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code_052220.csv") %>% 
  mutate(GEOID=as.numeric(`ZIP Code`),
         date=mdy(`Week End`)) %>% 
  filter(date==max(date)) %>% 
  rename(total_pop=Population) %>% 
  filter(!is.na(GEOID)) %>% 
  select(GEOID, total_pop)
# this also represents chicago ZCTAs, the one for which the city reports data
# instead of the intersecting ones
chicago_zctas<-chi_pop %>% pull(GEOID) %>% unique


# Chicago data, downloaded from the Chicago Reporter
files<-list.files("Data/Chicago/Chicago_Reporter/", pattern="json")
#file<-files[[1]]
chicago<-map_dfr(files, function(file){
  #print(file)
  temp<-fromJSON(paste0("Data/Chicago/Chicago_Reporter/",file))
  date<-temp[[1]]
  date<-as_date(paste0(date[[1]], "-", date[[2]], "-", date[[3]]))
  temp<-temp[[2]]
  temp<-temp[,grepl("zip|confirmed|total_tested", colnames(temp))]
  temp$date<-date
  temp
}) %>%
  rename(GEOID=zip,
         positives=confirmed_cases,
         all=total_tested) %>% 
  mutate(GEOID=as.numeric(GEOID)) %>% 
  # remove the first few dates where there was no total testing data
  filter(!is.na(all),
         GEOID%in%chicago_zctas) %>% 
  # there seems to be an issue with the first date with total testing (April 18th) and total testing, so excluding that date and including only from April 18th
  filter(date>"2020-04-18")
# make sure all zip codes are included every date
# if not, set their number of tests to 0
template<-expand.grid(GEOID=unique(chicago$GEOID),
                      date=unique(chicago$date), stringsAsFactors = F)
chicago<-full_join(chicago, template) %>% 
  mutate(positives=replace_na(positives, 0),
         all=replace_na(all, 0))
# correcting an issue with 60603 (small zipcode), that has 3 missing dates
# 6/49 cases/tests on 5/12 -> missing for 3 days -> 6/53 cases/tests on 5/16
# 2 options: exclude all together, impute a value
# for now ,imputing the mean of the last date before missing and first date after missing
chicago[chicago$date>="2020-05-13"&
          chicago$date<="2020-05-15"&
          chicago$GEOID==60603,c("positives", "all")]<-c(6,6,6,51,51,51)


all<-bind_rows(phl %>% 
            mutate(city="Philadelphia") %>% 
            left_join(zcta_data) %>% 
              rename(total_pop_census=total_pop) %>% 
              left_join(phl_pop),
          nyc %>% 
            mutate(city="New York City") %>% 
            left_join(modified_zcta_data)%>% 
            rename(total_pop_census=total_pop) %>% 
            left_join(nyc_pop),
          chicago %>% 
            mutate(city="Chicago") %>% 
            left_join(zcta_data)%>% 
            rename(total_pop_census=total_pop) %>% 
            left_join(chi_pop)) %>% 
  # remove those without (or very little) population/ACS data and remove industrial zipcodes (e.g.: philly airport)
  filter(!is.na(mhi)) %>% 
  filter(total_pop_census>100) %>% 
  # compute the 3 key outcomes
  mutate(pct_pos=positives/all,
         pos_pc=positives/total_pop*1000,
         tests_pc=all/total_pop*1000)
table(all$date, all$city)


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

all<-all %>% full_join(pca) %>% ungroup()
head(all)
summary(all)


save(all, loadings, zcta_data, modified_zcta_data,
     shp_zip, shp_zip_mod,
     bbox_il, bbox_pa, bbox_ny,
     file="Data/clean_data.rdata")
