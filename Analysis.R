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
load("data/clean_data.rdata")

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
city_label<-c("Philadelphia", "New York City", "Chicago")
names(city_label)<-c("Philadelphia", "New York City", "Chicago")
corsplot<-ggplot(cors, aes(x=date, y=cor, group=type)) +
  geom_hline(yintercept = 0, lty=2)+
  geom_line()+
  geom_point(aes(pch=type), fill="gray", color="black", size=2)+
  scale_y_continuous(limits=c(-1, 1),
                     breaks=seq(-1, 1, by=0.25))+
  scale_x_date(breaks="1 week")+
  scale_shape_manual(values=c(21, 22, 23),name="",
                     labels=c("% Positives", "Cumulative Incidence per 1,000", "Tests per 1,000"))+
  guides(shape=guide_legend(override.aes=list(size=5)))+
  labs(x="Date", y="Pearson Correlation Coefficient\nwith Deprivation Index",
       #title="Correlation of Zip Code Deprivation and COVID-19 Testing by City, Date and Outcome",
       caption="Source: NYCDOH, IDPH, PDPH, and ACS")+
  facet_wrap(~city, labeller = labeller(city=city_label)) +
  theme_bw() +
  theme(legend.position = "bottom",
    axis.text.y=element_text(color="black", size=14),
        axis.text.x=element_text(color="black", size=12, angle=90, hjust=1, vjust=.5),
        axis.title=element_text(color="black", size=14, face="bold"),
    legend.text=element_text(color="black", size=14),
    strip.text=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave("Results/Figure3.pdf",corsplot, width=12, height=6)

# add a panel with cases per city
casesjhu<-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
# using cook county
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
smoother<-stat_smooth(method="lm", se=F)
#.x<-all %>% ungroup() %>% filter(city=="Philadelphia", date==max(date));.y<-data.frame(city="1PHL", date="0407", stringsAsFactors = F)
plots<-all %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
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
    title1<-unique(.x$date)
    title2<-.y$city
    title<-paste0("COVID-19 Testing and Outcomes in Zip Codes of ", title2, " by ", title1)
    arrangeGrob(grobs=list(p1, p2, p3), ncol=3,
                top=textGrob(title, gp=gpar(fontsize=20,face="bold",font=8)))
  }, keep=T) 

pall<-arrangeGrob(grobs=plots, ncol=1)
ggsave("Results/Testing_by_CityDep.pdf", pall, width=20, height=10*3/2)
plot(pall)


# plotting specific variables for the last available date each place
# get incidence ine ach city and plot icnidence vs corrs
smoother<-stat_smooth(method="lm", se=F)
#.x<-all %>% ungroup() %>% filter(city=="Philadelphia", date==max(date));.y<-data.frame(city="1PHL", date="0407", stringsAsFactors = F)
plots_pct_pos<-all %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    title1<-unique(.x$date)
    title2<-.y$city
    title<-paste0("COVID-19 % Positivity in Zip Codes of ", title2, " by ", title1)
    smoother<-stat_smooth(method="lm")
    p1<-ggplot(.x, aes(x=mhi, y=pct_pos)) +
      smoother+
      geom_point()+
      scale_x_log10(breaks=c(10000, 20000, 30000,
                             50000, 70000, 100000, 200000))+
      scale_y_continuous(limits=c(0, NA), labels=scales::percent_format(accuracy = 1))+
      annotation_logticks(sides="b")+
      theme_bw() +
      labs(x="Median Household Income\n (2014-2018)",
           y="% Positive tests",
           title="")+
      theme(axis.text=element_text(color="black"))
    # Completed college
    p2<-ggplot(.x, aes(x=pct_college, y=pct_pos)) +
      smoother+
      geom_point()+
      # scale_x_log10(breaks=c(10000, 20000, 30000, 40000,
      #                        50000, 70000, 100000, 200000))+
      # annotation_logticks(sides="b")+
      scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
      scale_y_continuous(limits=c(0, NA), labels=scales::percent_format(accuracy = 1))+
      theme_bw() +
      labs(x="% Completed College or above\n(2014-2018)",
           y="% Positive tests",
           title="")+
      theme(axis.text=element_text(color="black"))
    # % non-Hispanic White
    p3<-ggplot(.x, aes(x=pct_nhwhite, y=pct_pos)) +
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
      scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
      #annotation_logticks(sides="b")+
      theme_bw() +
      labs(x="% non-Hispanic White\n (2014-2018)",
           y="% Positive tests",
           title="")+
      theme(axis.text=element_text(color="black"))
    # health insurance
    p4<-ggplot(.x, aes(x=no_healthins, y=pct_pos)) +
      smoother+
      geom_point()+
      # scale_x_log10(limits=c(15000, 120000),
      #               breaks=c(10000, 20000, 30000, 40000,
      #                        50000, 70000, 100000))+
      #annotation_logticks(sides="b")+
      scale_y_continuous(limits=c(0, NA), labels=scales::percent_format(accuracy = 1))+
      scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
      theme_bw() +
      labs(x="% with no Health Insurance\n (2014-2018)",
           y="% Positive tests",
           title="")+
      theme(axis.text=element_text(color="black"))
    # overcrowding
    p5<-ggplot(.x, aes(x=pct_overcrowded1, y=pct_pos)) +
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
      scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
      #annotation_logticks(sides="b")+
      theme_bw() +
      labs(x="% Overcrowding (>2 ppl/room)\n (2014-2018)",
           y="% Positive tests",
           title="")
    # service workers
    p6<-ggplot(.x, aes(x=pct_service, y=pct_pos)) +
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
      scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
      #annotation_logticks(sides="b")+
      theme_bw() +
      labs(x="% Service Workers [food prep/serving, personal care, \nand service occupations] (2014-2018)",
           y="% Positive tests",
           title="")
    pall<-(arrangeGrob(grobs=list(p1, p2, p3, p4, p5, p6), ncol=3, 
                       top=textGrob(title, gp=gpar(fontsize=20,face="bold",font=8)),
                       bottom=textGrob("Source: NYC DOH (github) and 5-year 2014-2018 ACS", gp=gpar(fontsize=10,font=8))))
    
  }, keep=T) 

ggsave("Results/Chicago_Vars_Pct_pos.pdf", plots_pct_pos[[1]], width=16, height=12/(20/15))
ggsave("Results/NYC_Vars_Pct_pos.pdf", plots_pct_pos[[2]], width=16, height=12/(20/15))
ggsave("Results/PHL_Vars_Pct_pos.pdf", plots_pct_pos[[3]], width=16, height=12/(20/15))

plots_pos_pc<-all %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    title1<-unique(.x$date)
    title2<-.y$city
    title<-paste0("COVID-19 Cumulative Incidence in Zip Codes of ", title2, " by ", title1)
    smoother<-stat_smooth(method="lm")
    ylab<-"Confirmed cases per 1,000 people"
    p1<-ggplot(.x, aes(x=mhi, y=pos_pc)) +
      smoother+
      geom_point()+
      scale_x_log10(breaks=c(10000, 20000, 30000,
                             50000, 70000, 100000, 200000))+
      scale_y_log10()+
      annotation_logticks(sides="bl")+
      theme_bw() +
      labs(x="Median Household Income\n (2014-2018)",
           y=ylab,
           title="")+
      theme(axis.text=element_text(color="black"))
    # Completed college
    p2<-ggplot(.x, aes(x=pct_college, y=pos_pc)) +
      smoother+
      geom_point()+
      scale_y_log10()+
      annotation_logticks(sides="l")+
      scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
      theme_bw() +
      labs(x="% Completed College or above\n(2014-2018)",
           y=ylab,
           title="")+
      theme(axis.text=element_text(color="black"))
    # % non-Hispanic White
    p3<-ggplot(.x, aes(x=pct_nhwhite, y=pos_pc)) +
      smoother+
      geom_point()+
      scale_y_log10()+
      annotation_logticks(sides="l")+
      scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
      theme_bw() +
      labs(x="% non-Hispanic White\n (2014-2018)",
           y=ylab,
           title="")+
      theme(axis.text=element_text(color="black"))
    # health insurance
    p4<-ggplot(.x, aes(x=no_healthins, y=pos_pc)) +
      smoother+
      geom_point()+
      scale_y_log10()+
      annotation_logticks(sides="l")+
      scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
      theme_bw() +
      labs(x="% with no Health Insurance\n (2014-2018)",
           y=ylab,
           title="")+
      theme(axis.text=element_text(color="black"))
    # overcrowding
    p5<-ggplot(.x, aes(x=pct_overcrowded1, y=pos_pc)) +
      smoother+
      geom_point()+
      scale_y_log10()+
      annotation_logticks(sides="l")+
      scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
      theme_bw() +
      labs(x="% Overcrowding (>2 ppl/room)\n (2014-2018)",
           y=ylab,
           title="")
    # service workers
    p6<-ggplot(.x, aes(x=pct_service, y=pos_pc)) +
      smoother+
      geom_point()+
      scale_y_log10()+
      annotation_logticks(sides="l")+
      scale_x_continuous(limits=c(NA, NA), labels=scales::percent_format(accuracy = 1))+
      theme_bw() +
      labs(x="% Service Workers [food prep/serving, personal care, \nand service occupations] (2014-2018)",
           y=ylab,
           title="")
    pall<-(arrangeGrob(grobs=list(p1, p2, p3, p4, p5, p6), ncol=3, 
                       top=textGrob(title, gp=gpar(fontsize=20,face="bold",font=8)),
                       bottom=textGrob("Source: NYC DOH (github) and 5-year 2014-2018 ACS", gp=gpar(fontsize=10,font=8))))
    
  }, keep=T) 

ggsave("Results/Chicago_Vars_pos_pc.pdf", plots_pos_pc[[1]], width=16, height=12/(20/15))
ggsave("Results/NYC_Vars_pos_pc.pdf", plots_pos_pc[[2]], width=16, height=12/(20/15))
ggsave("Results/PHL_Vars_pos_pc.pdf", plots_pos_pc[[3]], width=16, height=12/(20/15))
  