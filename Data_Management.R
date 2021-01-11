# Spatial Inequities in COVID-19 Testing, Positivity, Incidence and Mortality in 3 US Cities: a Longitudinal Ecological Study
# Bilal, Barber, Tabb & Diez-Roux
# Contact: ub45@drexel.edu
# Repository: https://github.com/usamabilal/COVID_Disparities
# Code files:
## Data_Management.R: contains all data management
## Exploratory_Analysis.R: contains all exploratory analysis
## Models.R: contains the modeling part of the paper using INLA
## Tables_Figures.R: contains the creation of final tables and visualizations
## Helper_Functions.R: contains package list, options, and a couple useful functions

# Data_management file (A).
# Index: ---- 
## A.1: Setup (packages, data, help variables)
## A.2: Download and load geospatial files
## A.3: Download and arrange census data
## A.4: Download and arrange COVID-19 data
### A.4.1: NYC data
### A.4.2: Philadelphia data
### A.4.3: Chicago data
## A.5: Merge all data together
## A.6: Get neighbors / adjacency matrix data for each city

rm(list=ls())
# A.1: Setup (packages, data, help variables) ----
source("Helper_Functions.R")

# A.2: Download and load geospatial files ---- 
# download SHPs once (uncomment if need to download)
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

# load shapefiles, select needed ones, and simplify shapefiles
# states
shp_states = readOGR('Data/stateshp/cb_2018_us_state_500k.shp')
shp_states$STATEFP<-as.numeric(as.character(shp_states$STATEFP))
shp_states<-shp_states[grepl("Delaware|New Jersey|Pennsylvania|New York|Connecticut|Indiana|Illinois", shp_states$NAME),]
shp_states<-ms_simplify(shp_states)
# place, to set boundaries for future maps (bbox)
# PA
shp_place_pa = readOGR('Data/placesshp/place_pa/', 'cb_2018_42_place_500k')
shp_place_pa<-shp_place_pa[shp_place_pa$NAME=="Philadelphia",]
bbox_pa<-st_bbox(shp_place_pa)
# IL
shp_place_il = readOGR('Data/placesshp/place_il/', 'cb_2018_17_place_500k')
shp_place_il<-shp_place_il[shp_place_il$NAME=="Chicago",]
bbox_il<-st_bbox(shp_place_il)
# NY
shp_place_ny = readOGR('Data/placesshp/place_ny/', 'cb_2018_36_place_500k')
shp_place_ny<-shp_place_ny[shp_place_ny$NAME=="New York",]
bbox_ny<-st_bbox(shp_place_ny)
# zcta (all states)
shp_zip<-readOGR('Data/zipcodeshp/', 'cb_2018_us_zcta510_500k')
shp_zip$GEOID<-as.numeric(as.character(shp_zip$ZCTA5CE10))
shp_zip<-shp_zip %>% st_as_sf
# get modified ZCTA from NYC
shp_zip_mod<-readOGR('Data/NYC/Geography-resources/', 'MODZCTA_2010')
shp_zip_mod$GEOID<-as.numeric(as.character(shp_zip_mod$MODZCTA))
shp_zip_mod<-shp_zip_mod %>% st_as_sf %>% 
  st_transform(crs=st_crs(shp_place_ny))



# A.3: Download and arrange census data ----
# NOTE: we use the tidycensus package, which requires a census API
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
  mutate(pct_age65plus=pct_age6574+pct_age7584+pct_age85plus)

# for NYC: get data at the "modified ZCTA"-level, 3 key steps: 
## bring in a crosswalk (obtained from NYC DOH Github)
## merge crosswalk with raw ZCTA-level data, and reaggregate to modified ZCTA
## compute indicators
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
  mutate(pct_age65plus=pct_age6574+pct_age7584+pct_age85plus)
  

# Arrange SVI data too:
# first, get ZCTA to CT crosswalk
cw_zcta<-fread("Data/zcta_tract_rel_10.txt") %>% 
  #filter(ZCTA5%in%last_available_date$GEOID) %>% 
  rename(FIPS=GEOID,
         GEOID=ZCTA5) %>% 
  select(FIPS, GEOID, ZPOPPCT) %>% 
  mutate(ZPOPPCT=ZPOPPCT/100)
# get SVI by CT, convert -999 to NA, join with crosswalk, and aggregate to ZCTA
# one for each state (as recommended by CDC when using single state data)
svi1<-fread("Data/svi/Illinois.csv") %>% 
  mutate(RPL_THEMES=ifelse(RPL_THEMES==-999, NA, RPL_THEMES),
         RPL_THEME1=ifelse(RPL_THEME1==-999, NA, RPL_THEME1),
         RPL_THEME2=ifelse(RPL_THEME2==-999, NA, RPL_THEME2),
         RPL_THEME3=ifelse(RPL_THEME3==-999, NA, RPL_THEME3),
         RPL_THEME4=ifelse(RPL_THEME4==-999, NA, RPL_THEME4)) %>% 
  left_join(cw_zcta) %>% 
  filter(!is.na(GEOID)) %>% 
  group_by(GEOID) %>% 
  summarise(svi=weighted.mean(RPL_THEMES, w=ZPOPPCT, na.rm = T),
            svi1=weighted.mean(RPL_THEME1, w=ZPOPPCT, na.rm = T),
            svi2=weighted.mean(RPL_THEME2, w=ZPOPPCT, na.rm = T),
            svi3=weighted.mean(RPL_THEME3, w=ZPOPPCT, na.rm = T),
            svi4=weighted.mean(RPL_THEME4, w=ZPOPPCT, na.rm = T)) 
# NYC needs two crosswalks: CT to ZCTA to MODZCTA
svi2<-fread("Data/svi/NewYork.csv") %>% 
  mutate(RPL_THEMES=ifelse(RPL_THEMES==-999, NA, RPL_THEMES),
         RPL_THEME1=ifelse(RPL_THEME1==-999, NA, RPL_THEME1),
         RPL_THEME2=ifelse(RPL_THEME2==-999, NA, RPL_THEME2),
         RPL_THEME3=ifelse(RPL_THEME3==-999, NA, RPL_THEME3),
         RPL_THEME4=ifelse(RPL_THEME4==-999, NA, RPL_THEME4)) %>% 
  left_join(cw_zcta) %>% 
  left_join(cw %>% rename(GEOID=ZCTA)) %>% 
  filter(!is.na(MODZCTA)) %>% 
  group_by(MODZCTA) %>% 
  summarise(svi=weighted.mean(RPL_THEMES, w=ZPOPPCT, na.rm = T),
            svi1=weighted.mean(RPL_THEME1, w=ZPOPPCT, na.rm = T),
            svi2=weighted.mean(RPL_THEME2, w=ZPOPPCT, na.rm = T),
            svi3=weighted.mean(RPL_THEME3, w=ZPOPPCT, na.rm = T),
            svi4=weighted.mean(RPL_THEME4, w=ZPOPPCT, na.rm = T)) %>% 
  rename(GEOID=MODZCTA)
svi3<-fread("Data/svi/Pennsylvania.csv") %>% 
  mutate(RPL_THEMES=ifelse(RPL_THEMES==-999, NA, RPL_THEMES),
         RPL_THEME1=ifelse(RPL_THEME1==-999, NA, RPL_THEME1),
         RPL_THEME2=ifelse(RPL_THEME2==-999, NA, RPL_THEME2),
         RPL_THEME3=ifelse(RPL_THEME3==-999, NA, RPL_THEME3),
         RPL_THEME4=ifelse(RPL_THEME4==-999, NA, RPL_THEME4)) %>% 
  left_join(cw_zcta) %>% 
  filter(!is.na(GEOID)) %>% 
  group_by(GEOID) %>% 
  summarise(svi=weighted.mean(RPL_THEMES, w=ZPOPPCT, na.rm = T),
            svi1=weighted.mean(RPL_THEME1, w=ZPOPPCT, na.rm = T),
            svi2=weighted.mean(RPL_THEME2, w=ZPOPPCT, na.rm = T),
            svi3=weighted.mean(RPL_THEME3, w=ZPOPPCT, na.rm = T),
            svi4=weighted.mean(RPL_THEME4, w=ZPOPPCT, na.rm = T)) 
svi<-bind_rows(svi1, svi2, svi3)


# A.4: Download and arrange COVID-19 data ---- 

# A.4.1: NYC data ----
# Obtained from the October 1st snapshot of the NYC DOH Github repository
nyc<-fread("Data/NYC/data-by-modzcta_100120.csv") %>% 
  rename(GEOID=MODIFIED_ZCTA) %>% 
  filter(!is.na(GEOID), GEOID!=99999) %>% 
  # data now has positives and % positive, so calculate total tests
  rename(positives=COVID_CASE_COUNT,
         deaths=COVID_DEATH_COUNT,
         total_pop=POP_DENOMINATOR,
         all=TOTAL_COVID_TESTS) %>% 
  select(GEOID, positives, all, deaths, total_pop) %>% 
  mutate(date=ymd("2020-10-01"))
# keeping NYC improved population estimations
nyc_pop<-nyc %>% filter(date==max(date)) %>% select(GEOID, total_pop)
nyc<-nyc %>% select(-total_pop) %>% 
  arrange(GEOID, date)
summary(nyc);head(nyc)

# A.4.2: Philadelphia data ----
# Obtained from the October 1st snapshot of the PDPH OpenDataPhilly repository
# then files downloaded almost daily from open data philly
phl<-fread("Data/PHL/covid_cases_by_zip_100120.csv") %>% 
    rowwise() %>% 
      mutate(all=sum(POS, NEG, na.rm=T),
             POS=replace_na(POS, 0),
             GEOID=as.numeric(zip_code),
             date=as_date(ymd_hms(etl_timestamp))) %>% 
      rename(positives=POS) %>% 
      select(GEOID, date, positives, all) %>% 
      filter(!is.na(GEOID))
# get deaths in PHL zipcodes
phl_deaths<-fread("Data/PHL/covid_deaths_by_zip_100120.csv") %>% 
  mutate(date=as_date(ymd_hms(etl_timestamp))) %>% 
  select(covid_outcome, date, zip_code, count) %>% 
    spread(covid_outcome, count) %>% 
    rowwise() %>% 
    mutate(deaths=DIED,
           GEOID=as.numeric(zip_code)) %>% 
    select(GEOID, date, deaths) %>% 
    filter(!is.na(GEOID))
# merge, replace NAs (no deaths) by 0
phl<-full_join(phl, phl_deaths) %>% 
  mutate(deaths=replace_na(deaths, 0))
head(phl);summary(phl);table(phl$date)

# A.4.3: Chicago data ----
# Obtained from the October 7th snapshot of the CDPH Chicago Data Portal repository
chi<-fread("Data/Chicago/COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code_100720.csv") %>% 
  mutate(GEOID=as.numeric(`ZIP Code`),
         date=mdy(`Week End`)) %>% 
  filter(!is.na(GEOID)) %>% 
  rename(positives=`Cases - Cumulative`,
         all=`Tests - Cumulative`,
         deaths=`Deaths - Cumulative`) %>% 
  select(GEOID, date, positives, all, deaths) %>% 
  mutate(positives=replace_na(positives, 0)) %>% 
  arrange(GEOID, date) %>% 
  filter(date==max(date))
head(chi);summary(chi);table(chi$date)

# A.5: Putting all data together ----
# First, ZCTA data
final_zcta_data<-bind_rows( zcta_data %>% 
                              filter(substr(sprintf("%05d", GEOID), 1, 2)%in%c(19, 60)),
                            modified_zcta_data %>% 
                             select(-total_pop) %>% 
                             left_join(nyc_pop)) %>% 
  left_join(svi)
# COVID-Data
all<-bind_rows(phl %>% 
            mutate(city="Philadelphia"),
            nyc %>% 
            mutate(city="New York City"),
            chi %>% 
            mutate(city="Chicago"))%>%
  left_join(final_zcta_data) %>% 
  # remove those without (or very little) population/ACS data and remove industrial zipcodes (e.g.: philly airport)
  filter(!is.na(mhi)) %>% 
  filter(total_pop>100) %>% 
  # re-scale SVI by city 
  group_by(city) %>% 
  mutate(svi=scale(svi),
         svi1=scale(svi1),
         svi2=scale(svi2),
         svi3=scale(svi3),
         svi4=scale(svi4))

last_available_date<-all %>% 
  # compute the 3 key outcomes
  mutate(pct_pos=positives/all,
         pos_pc=positives/total_pop*1000,
         tests_pc=all/total_pop*1000, 
         deaths_pc=deaths/total_pop*1000) %>% 
  group_by(city) %>% 
  filter(date==max(date)) %>% 
  arrange(city, date, GEOID)

head(last_available_date);summary(last_available_date);table(last_available_date$city);table(last_available_date$date)


# A.6: Get neighbors / adjacency matrix data for each city ----
## NOTE: there are 2 changes done to NYC neighbor files
### NYC: Roosvelt Island has no neighbors in the shapefile, but is connected to its neighboring zip codes across the river
### NYC: There are two zip codes (11101 and 11222) which have an odd shape and do not show up as connected but should be
neighbors_id<-last_available_date %>%
  group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_by(city) %>% 
  group_keys()
neighbors<-last_available_date %>% 
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

last_available_date %>% View

save(last_available_date,
     neighbors_id,neighbors,
     shp_zip, shp_zip_mod,svi,
     bbox_il, bbox_pa, bbox_ny,
     file="Data/clean_data.rdata")
