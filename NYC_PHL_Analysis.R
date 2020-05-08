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
options(scipen=999)
# obtain a census api key https://api.census.gov/data/key_signup.html
# census_api_key(key="xxx", install = T)
# use this dataset to check variable names
vars<-load_variables(year=2018, "acs5")
# example:
vars %>% filter(grepl("foreign", concept, ignore.case=T))
# healthcare workers:
vars %>% filter(grepl("C24010", name), grepl("health", label, ignore.case=T)) %>% pull(name)
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
         pct_noncitizen, pct_foreignborn)

# only need to run these two commands once
download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_zcta510_500k.zip",
              destfile ="Data/cb_2018_us_zcta510_500k.zip" )
unzip("Data/cb_2018_us_zcta510_500k.zip", exdir = "Data/zipcodeshp/")

area<-read.dbf("Data/zipcodeshp/cb_2018_us_zcta510_500k.dbf") %>% 
  mutate(GEOID=as.numeric(as.character(GEOID10)),
         area=ALAND10/1000000/2.59) %>% 
  select(GEOID, area)

# Get NYC data over time
ea<-read_file("Data/History for tests-by-zcta.csv - nychealth_coronavirus-data.html")
list<-gregexpr("repo:250296192:commit:", ea)[[1]]
commits<-map_chr(list, function(init){
  substr(ea, init+22, init+22+39)  
})
list<-gregexpr("Commits on ", ea)[[1]]
fechas<-map_chr(list, function(init){
  substr(ea, init+11, init+11+6)  
})
fechas<-gsub("\\,", "", fechas)
fechas<-paste0(fechas, ",", 2020)
fechas<-mdy(fechas)
fechas
history<-paste0("https://raw.githubusercontent.com/nychealth/coronavirus-data/",commits,"/tests-by-zcta.csv")
nyc<-map2_dfr(history, fechas, function(file, fecha){
  t<-fread(file)
  t$date<-fecha
  t
}) %>% rename(GEOID=MODZCTA) %>% 
  filter(!is.na(GEOID), GEOID!=99999) %>% 
  left_join(area) %>% 
  left_join(zipcode_data) %>% 
  rename(positives=Positive,
         all=Total) %>% 
  mutate(density=total_pop/area,
         pct_pos=positives/all,
         pos_pc=positives/total_pop*1000,
         tests_pc=all/total_pop*1000)
nyc_last<-nyc %>% filter(date==max(date))

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
  filter(date>=ymd("2020-03-04"),
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

phl<-bind_rows(phl, phl1) %>% 
  left_join(area) %>% 
  left_join(zipcode_data) %>% 
  # remove those without population/ACS data
  filter(!is.na(mhi)) %>% 
  mutate(density=total_pop/area,
         pct_pos=positives/all,
         pos_pc=positives/total_pop*1000,
         tests_pc=all/total_pop*1000)

phl_last<-phl %>% ungroup() %>% filter(date==max(date))


# PCA to get first component
nyc_pca<-prcomp(nyc_last %>% mutate(logmhi=log(mhi)) %>% select(pct_nhwhite, logmhi, 
                                                            pct_college, no_healthins, 
                                                            limited_engl, pct_service, 
                                                            pct_overcrowded1), scale=T)
nyc_pca<-data.frame(GEOID=nyc_last$GEOID, pc1=nyc_pca$x[,1])
nyc_last<-nyc_last %>% 
  left_join(nyc_pca)
nyc_full<-nyc %>% left_join(nyc_pca)

phl_pca<-prcomp(phl_last %>% mutate(logmhi=log(mhi)) %>% select(pct_nhwhite, logmhi, 
                                                                pct_college, no_healthins, 
                                                                limited_engl, pct_service, 
                                                                pct_overcrowded1), scale=T)
phl_pca<-data.frame(GEOID=phl_last$GEOID, pc1=phl_pca$x[,1])
phl_last<-phl_last %>% 
  left_join(phl_pca)
phl_full<-phl %>% left_join(phl_pca)

all<-bind_rows(phl_full %>% mutate(city="Philadelphia"), 
               nyc_full %>% mutate(city="New York City")) %>% 
  filter(date>=as_date("2020-04-01"))


cors<-all %>% group_by(city, date) %>% 
  group_modify(~{
    data.frame(tests_pc=cor(.x$pc1, log(.x$tests_pc), use="complete.obs"),
               pct_pos=cor(.x$pc1, .x$pct_pos, use="complete.obs"),
               pos_pc=cor(.x$pc1, log(.x$pos_pc), use="complete.obs"))
  }) %>% 
  gather(type, cor, -city, -date) %>% 
  mutate(type2=ifelse(type=="pct_pos", "% Positive tests",
                      ifelse(type=="pos_pc", "Positives per 1,000",
                             "Tests per 1,000"))) %>% ungroup()
city_label<-c("Philadelphia", "New York City")
names(city_label)<-c("Philadelphia", "New York City")
corsplot<-ggplot(cors, aes(x=date, y=cor, group=type)) +
  geom_hline(yintercept = 0, lty=2)+
  geom_line()+
  geom_point(pch=21, fill="gray", color="black", size=3)+
  geom_text(data=cors %>% filter(date==max(date)), aes(label=type2),
            position=position_nudge(x = 0, y=0.1), hjust=0.5)+
  scale_y_continuous(limits=c(-1, 1),
                     breaks=seq(-1, 1, by=0.25))+
  #scale_x_discrete(labels=c("April 1st", "April 7th", "April 14th"))+
  scale_x_date(breaks="3 days")+
  labs(x="Date", y="Pearson Correlation Coefficient with Deprivation Index",
       title="Correlation of Zip Code Deprivation and COVID-19 Testing by City, Date and Outcome",
       caption="Source: NYCDOH, PDPH, and ACS")+
  facet_wrap(~city, labeller = labeller(city=city_label)) +
  theme_bw() +
  theme(axis.text.y=element_text(color="black", size=14),
        axis.text.x=element_text(color="black", size=12, angle=90, hjust=1, vjust=.5),
        axis.title=element_text(color="black", size=14, face="bold"),
        strip.text=element_text(color="black", face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave("Results/Testing_By_CityDate.pdf",corsplot, width=12, height=6)

# add a panel with cases per city
casesjhu<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
cases<-bind_rows(casesjhu %>% 
                   filter(grepl("Bronx|Queens|(New York)|Kings|Richmond", Admin2),
                          grepl("New York", Combined_Key)) %>% 
                   summarise_if(is.numeric, sum) %>% 
                   mutate(n=1) %>% 
                   select(-(1:5)) %>% 
                   gather(date, cases, -n) %>% 
                   mutate(city="New York City",
                          date=mdy(date)) %>% 
                   select(-n),
                 casesjhu %>% 
                   filter(grepl("Philadelphia", Admin2),
                          grepl("Penn", Combined_Key)) %>% 
                   summarise_if(is.numeric, sum) %>% 
                   mutate(n=1) %>% 
                   select(-(1:5)) %>% 
                   gather(date, cases, -n) %>% 
                   mutate(city="Philadelphia",
                          date=mdy(date)) %>% 
                   select(-n)) %>% 
  filter(date>=ymd("2020-03-01"))
colors<-brewer_pal(type="qual", palette=2)(2)
casesplot<-ggplot(cases, aes(x=date, y=cases, group=city))+
  annotate("rect", ymin=-Inf, ymax=+Inf,
           xmin=as_date("2020-04-01"), xmax=as_date("2020-05-01"),
           alpha=0.3, fill="blue")+
  annotate("text", x=as_date("2020-04-06"), y=150000,
           label="New York City", color=colors[1])+
  annotate("text", x=as_date("2020-04-13"), y=20000,
           label="Philadelphia", color=colors[2])+
  geom_line(aes(color=city),size=1.3) +
  scale_y_continuous(expand=c(0,0))+
  #scale_y_log10()+
  #annotation_logticks(sides="l")+
  scale_x_date(breaks="1 week")+
  scale_color_manual(values=colors)+
  labs(x="Date", y="Confirmed Cases",
       title="Total Number of Confirmed Cases in Philadelphia and New York City",
       caption="Source: NYCDOH, PDPH, and ACS")+
  guides(color=F)+
  theme_bw() +
  theme(axis.text.y=element_text(color="black", size=14),
        axis.text.x=element_text(color="black", size=12, angle=90, hjust=1, vjust=.5),
        axis.title=element_text(color="black", size=14, face="bold"),
        strip.text=element_text(color="black", face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

pall<-arrangeGrob(grobs=list(corsplot, casesplot), ncol=2)
ggsave("Results/Testing_By_CityDate_Cases.pdf", pall, width=20, height=10)

# get incidence ine ach city and plot icnidence vs corrs
# MAPITA

smoother<-stat_smooth(method="lm", se=F)
#.x<-all %>% ungroup() %>% filter(city=="Philadelphia", date==max(date))
#.y<-data.frame(city="1PHL", date="0407", stringsAsFactors = F)
plots<-all %>% group_by(city, date) %>% 
  group_map(~{
    p1<-ggplot(.x, aes(x=pc1, y=tests_pc)) +
      smoother+
      # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
      #          alpha=0.3, color="gray")+
      # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
      #          alpha=0.3, color="gray")+
      geom_point()+
      # scale_x_log10(limits=c(15000, 120000),
      #               breaks=c(10000, 20000, 30000, 40000,
      #                        50000, 70000, 100000))+
      #scale_y_continuous(limits=c(0, NA))+
      #scale_y_log10(limits=c(min(all$tests_pc[all$tests_pc>0]), max(all$tests_pc[all$tests_pc>0])))+
      scale_y_log10()+
      scale_x_continuous(limits=c(NA, NA))+
      annotation_logticks(sides="l")+
      theme_bw() +
      labs(x="Deprivation Index (SD)",
           y="Tests per 1,000 pop",
           title="")+
      theme(axis.text=element_text(color="black"))
    p2<-ggplot(.x, aes(x=pc1, y=pct_pos)) +
      smoother+
      # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
      #          alpha=0.3, color="gray")+
      # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
      #          alpha=0.3, color="gray")+
      geom_point()+
      # scale_x_log10(limits=c(15000, 120000),
      #               breaks=c(10000, 20000, 30000, 40000,
      #                        50000, 70000, 100000))+
      scale_y_continuous(limits=c(0, NA), labels=scales::percent_format(accuracy = 1))+
      scale_x_continuous(limits=c(NA, NA))+
      #annotation_logticks(sides="b")+
      theme_bw() +
      labs(x="Deprivation Index (SD)",
           y="% Positive tests",
           title="")+
      theme(axis.text=element_text(color="black"))
    p3<-ggplot(.x, aes(x=pc1, y=pos_pc)) +
      smoother+
      # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
      #          alpha=0.3, color="gray")+
      # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
      #          alpha=0.3, color="gray")+
      geom_point()+
      # scale_x_log10(limits=c(15000, 120000),
      #               breaks=c(10000, 20000, 30000, 40000,
      #                        50000, 70000, 100000))+
      #scale_y_log10(limits=c(min(all$pos_pc[all$pos_pc>0]), max(all$pos_pc[all$pos_pc>0])))+
      scale_y_log10()+
      scale_x_continuous(limits=c(NA, NA))+
      annotation_logticks(sides="l")+
      theme_bw() +
      labs(x="Deprivation Index (SD)",
           y="Confirmed cases per 1,000 pop",
           title="")+
      theme(axis.text=element_text(color="black"))
    title1<-.y$date
    title2<-.y$city
    title<-paste0("COVID-19 Testing in Zip Codes of ", title2, " by ", title1)
    arrangeGrob(grobs=list(p1, p2, p3), ncol=3,
                top=textGrob(title, gp=gpar(fontsize=20,face="bold",font=8)))
  }, keep=T) 
ids<-all %>% group_by(city, date) %>% 
  group_modify(~data.frame(n=NA)) %>% 
  ungroup() %>%
  mutate(n=row_number()) %>%  
  arrange(city, desc(date)) %>% 
  filter(!duplicated(city)) %>% 
  pull(n)
pall<-arrangeGrob(grobs=list(plots[[ids[[1]]]], plots[[ids[[2]]]]))
ggsave("Results/Testing_by_CityDep.pdf", pall, width=20, height=10)
plot(pall)
# loadings
nyc_pca<-prcomp(nyc_last %>% mutate(logmhi=log(mhi)) %>% select(pct_nhwhite, logmhi, 
                                                                pct_college, no_healthins, 
                                                                limited_engl, pct_service, 
                                                                pct_overcrowded1), scale=T)
phl_pca<-prcomp(phl_last %>% mutate(logmhi=log(mhi)) %>% select(pct_nhwhite, logmhi, 
                                                                pct_college, no_healthins, 
                                                                limited_engl, pct_service, 
                                                                pct_overcrowded1), scale=T)
loadings<-data.frame(var=names(nyc_pca$rotation[,1]),
                     NYC=nyc_pca$rotation[,1], 
                     PHL=phl_pca$rotation[,1]) %>% 
  mutate_at(-1,format, digits=2, nsmall=2)
write.csv(loadings, file="results/loadings.csv")


# plotting specific variables for the last available date in NYC
smoother<-stat_smooth(method="lm")
both<-all %>% filter(city=="New York City") %>% 
  ungroup() %>% 
  filter(date==max(date))
# MHI plot
p1<-ggplot(both, aes(x=mhi, y=pct_pos)) +
  smoother+
  geom_point()+
  scale_x_log10(breaks=c(10000, 20000, 30000,
                         50000, 70000, 100000, 200000))+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="Median Household Income\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))
# Completed college
p1b<-ggplot(both, aes(x=pct_college, y=pct_pos)) +
  smoother+
  geom_point()+
  # scale_x_log10(breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000, 200000))+
  # annotation_logticks(sides="b")+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  theme_bw() +
  labs(x="% Completed College or above\n(2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))
# % Black
p2<-ggplot(both, aes(x=pct_black, y=pct_pos)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(.2, .650), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% non-Hispanic Black\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))
# % non-Hispanic White
p2b<-ggplot(both, aes(x=pct_nhwhite, y=pct_pos)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% non-Hispanic White\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))
p2c<-ggplot(both, aes(x=pct_transit, y=pct_pos)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Transit Users\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))
# % Hispanic and tests
p3<-ggplot(both, aes(x=pct_hisp, y=pct_pos)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Hispanic\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))

# Limited english proficiency
p4<-ggplot(both, aes(x=limited_engl, y=pct_pos)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% households with Limited English Profiency\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))

# health insurance
p5<-ggplot(both, aes(x=no_healthins, y=pct_pos)) +
  smoother+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  #annotation_logticks(sides="b")+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  theme_bw() +
  labs(x="% with no Health Insurance\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))
# overcrowding
p6<-ggplot(both, aes(x=pct_overcrowded2, y=pct_pos)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>2 ppl/room)\n (2014-2018)",
       y="% Positive tests",
       title="")
p7<-ggplot(both, aes(x=pct_overcrowded15, y=pct_pos)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>1.5 ppl/room)\n (2014-2018)",
       y="% Positive tests",
       title="")
p8<-ggplot(both, aes(x=pct_overcrowded1, y=pct_pos)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>1 ppl/room)\n (2014-2018)",
       y="% Positive tests",
       title="")
# service workers
p9<-ggplot(both, aes(x=pct_service, y=pct_pos)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(.2, .65), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Service Workers [food prep/serving, personal care, \nand service occupations] (2014-2018)",
       y="% Positive tests",
       title="")
pall<-(arrangeGrob(grobs=list(p1, p1b, p2b, p5, p8, p9), ncol=3, 
                   top=textGrob("% Positive Tests per Zip Code in NYC", gp=gpar(fontsize=20,face="bold",font=8)),
                   bottom=textGrob("Source: NYC DOH (github) and 5-year 2014-2018 ACS", gp=gpar(fontsize=10,font=8))))
ggsave("Results/NYC_Vars_Pct_pos.pdf", pall, width=16, height=12/(20/15))



# MHI plot
p1<-ggplot(both, aes(x=mhi, y=tests_pc)) +
  smoother+
  geom_point()+
  scale_x_log10(breaks=c(10000, 20000, 30000,
                         50000, 70000, 100000, 200000))+
  scale_y_log10()+
  annotation_logticks(sides="bl")+
  theme_bw() +
  labs(x="Median Household Income\n (2014-2018)",
       y="Tests per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))
# Completed college
p1b<-ggplot(both, aes(x=pct_college, y=tests_pc)) +
  smoother+
  geom_point()+
  # scale_x_log10(breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000, 200000))+
  # annotation_logticks(sides="b")+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  scale_y_log10()+
  annotation_logticks(sides="l")+
  theme_bw() +
  labs(x="% Completed College or above\n(2014-2018)",
       y="Tests per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))
# % Black
p2<-ggplot(both, aes(x=pct_black, y=tests_pc)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+
  annotation_logticks(sides="l")+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% non-Hispanic Black\n (2014-2018)",
       y="Tests per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))
# % non-Hispanic White
p2b<-ggplot(both, aes(x=pct_nhwhite, y=tests_pc)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% non-Hispanic White\n (2014-2018)",
       y="Tests per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))

# % Hispanic and tests
p3<-ggplot(both, aes(x=pct_hisp, y=tests_pc)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Hispanic\n (2014-2018)",
       y="Tests per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))

# Limited english proficiency
p4<-ggplot(both, aes(x=limited_engl, y=tests_pc)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% households with Limited English Profiency\n (2014-2018)",
       y="Tests per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))

# health insurance
p5<-ggplot(both, aes(x=no_healthins, y=tests_pc)) +
  smoother+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  #annotation_logticks(sides="b")+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  theme_bw() +
  labs(x="% with no Health Insurance\n (2014-2018)",
       y="Tests per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))
# overcrowding
p6<-ggplot(both, aes(x=pct_overcrowded2, y=tests_pc)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>2 ppl/room)\n (2014-2018)",
       y="Tests per 1,000",
       title="")
p7<-ggplot(both, aes(x=pct_overcrowded15, y=tests_pc)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>1.5 ppl/room)\n (2014-2018)",
       y="Tests per 1,000",
       title="")
p8<-ggplot(both, aes(x=pct_overcrowded1, y=tests_pc)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>1 ppl/room)\n (2014-2018)",
       y="Tests per 1,000",
       title="")
# service workers
p9<-ggplot(both, aes(x=pct_service, y=tests_pc)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Service Workers [food prep/serving, personal care, \nand service occupations] (2014-2018)",
       y="Tests per 1,000",
       title="")
pall<-(arrangeGrob(grobs=list(p1, p1b, p2b, p5, p8, p9), ncol=3, 
                   top=textGrob("Total Tests per Zip Code in NYC", gp=gpar(fontsize=20,face="bold",font=8)),
                   bottom=textGrob("Source: NYHealth (github) and 5-year 2014-2018 ACS", gp=gpar(fontsize=10,font=8))))
ggsave("Results/NYC_Vars_tests_pc.pdf", pall, width=16, height=12/(20/15))


# MHI plot
p1<-ggplot(both, aes(x=mhi, y=pos_pc*1000)) +
  smoother+
  geom_point()+
  scale_x_log10(breaks=c(10000, 20000, 30000,
                         50000, 70000, 100000, 200000))+
  #scale_y_continuous(limits=c(0, NA))+
  scale_y_log10()+
  annotation_logticks(sides="bl")+
  theme_bw() +
  labs(x="Median Household Income\n (2014-2018)",
       y="Confirmed cases per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))
# Completed college
p1b<-ggplot(both, aes(x=pct_college, y=pos_pc*1000)) +
  smoother+
  geom_point()+
  # scale_x_log10(breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000, 200000))+
  # annotation_logticks(sides="b")+
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  theme_bw() +
  labs(x="% Completed College or above\n(2014-2018)",
       y="Confirmed cases per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))
# % Black
p2<-ggplot(both, aes(x=pct_black, y=pos_pc*1000)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% non-Hispanic Black\n (2014-2018)",
       y="Confirmed cases per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))
# % non-Hispanic White
p2b<-ggplot(both, aes(x=pct_nhwhite, y=pos_pc*1000)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% non-Hispanic White\n (2014-2018)",
       y="Confirmed cases per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))

# % Hispanic and tests
p3<-ggplot(both, aes(x=pct_hisp, y=pos_pc*1000)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Hispanic\n (2014-2018)",
       y="Confirmed cases per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))

# Limited english proficiency
p4<-ggplot(both, aes(x=limited_engl, y=pos_pc*1000)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% households with Limited English Profiency\n (2014-2018)",
       y="Confirmed cases per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))

# health insurance
p5<-ggplot(both, aes(x=no_healthins, y=pos_pc*1000)) +
  smoother+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  #annotation_logticks(sides="b")+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  theme_bw() +
  labs(x="% with no Health Insurance\n (2014-2018)",
       y="Confirmed cases per 1,000",
       title="")+
  theme(axis.text=element_text(color="black"))
# overcrowding
p6<-ggplot(both, aes(x=pct_overcrowded2, y=pos_pc*1000)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>2 ppl/room)\n (2014-2018)",
       y="Confirmed cases per 1,000",
       title="")
p7<-ggplot(both, aes(x=pct_overcrowded15, y=pos_pc*1000)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>1.5 ppl/room)\n (2014-2018)",
       y="Confirmed cases per 1,000",
       title="")
p8<-ggplot(both, aes(x=pct_overcrowded1, y=pos_pc*1000)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>1 ppl/room)\n (2014-2018)",
       y="Confirmed cases per 1,000",
       title="")
# service workers
p9<-ggplot(both, aes(x=pct_service, y=pos_pc*1000)) +
  smoother+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_log10()+   annotation_logticks(sides="l")+   
  scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Service Workers [food prep/serving, personal care, \nand service occupations] (2014-2018)",
       y="Confirmed cases per 1,000",
       title="")
pall<-(arrangeGrob(grobs=list(p1, p1b, p2b, p5, p8, p9), ncol=3, 
                   top=textGrob("Confirmed Cases per Zip Code in NYC", gp=gpar(fontsize=20,face="bold",font=8)),
                   bottom=textGrob("Source: NYHealth (github) and 5-year 2014-2018 ACS", gp=gpar(fontsize=10,font=8))))
ggsave("results/NYC_Vars_Pos_pc.pdf", pall, width=16, height=12/(20/15))
