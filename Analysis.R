rm(list=ls())
get_legend<-function(plot){
  grobs<-ggplotGrob(plot)$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]  
  return(legend)
}
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
library(rmapshaper)
library(spdep)

options(scipen=999)
load("data/clean_data.rdata")

table(all$date, all$city)
# last available date is May 18th for the three cities

# figure 3
#.x<-all %>% filter(city=="Philadelphia", date==as_date("2020-03-21"))
rateratios<-all %>% group_by(city, date) %>% 
  group_modify(~{
    .x$pc1<-as.numeric(scale(.x$pc1, scale=T, center=T))
    m_tests_pc<-glm(all~pc1+offset(log(total_pop)), data=.x, family=poisson)
    m_pct_pos<-glm(cbind(positives, all-positives)~pc1, data=.x, family=binomial(link="log"))
    m_pos_pc<-glm(positives~pc1+offset(log(total_pop)), data=.x, family=poisson)
    map2_dfr(list(m_tests_pc, m_pct_pos, m_pos_pc), 
             c("tests_pc", "pct_pos", "pos_pc"),
             function(model, id){
               model %>% tidy %>% 
                 filter(term=="pc1") %>% 
                 select(estimate, std.error)%>% 
                 mutate(type=id)
             }) 
  }) %>% 
  mutate(type2=ifelse(type=="pct_pos", "Positivity rate",
                      ifelse(type=="pos_pc", "Cumulative incidence per 1,000",
                             "Tests per 1,000")),
         lci=exp(estimate-1.96*std.error),
         uci=exp(estimate+1.96*std.error),
         rr=exp(estimate)) %>% 
  ungroup() 
# for abstract
rateratios %>% 
  filter(city=="Philadelphia",
         date%in%c(as_date("2020-04-01"), as_date("2020-05-18")),
         type=="tests_pc")

city_label<-c("Philadelphia", "New York City", "Chicago")
names(city_label)<-c("Philadelphia", "New York City", "Chicago")
ggplot(rateratios, aes(x=date, y=rr, group=type)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(aes(ymin=lci, ymax=uci, fill=type2))+
  geom_line()+
  scale_y_continuous(trans=log_trans(), breaks=c(0.66, 0.8, 1, 1.25, 1.5))+
  scale_x_date(breaks="1 week")+
  scale_fill_discrete(name="")+
  guides(shape=guide_legend(override.aes=list(size=5)))+
  labs(x="Date", y="Rate Ratio (95% CI) per 1 SD increase in Deprivation Index",
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
ggsave("Results/Figure2.pdf", width=12, height=6)


# cases per city
cases_city<-all %>% 
  group_by(city, date) %>% 
  summarise(positives=sum(positives),
            total_pop=sum(total_pop)) %>% 
  mutate(city_ci=positives/total_pop*100000)

cases_city %>% filter(city=="New York City", date==as_date("2020-04-01")) %>% select(-positives, -total_pop)
cases_city %>% filter(city=="Philadelphia", date==as_date("2020-03-21")) %>% select(-positives, -total_pop)
cases_city %>% filter(city=="Philadelphia", date==as_date("2020-04-01")) %>% select(-positives, -total_pop)
cases_city %>% filter(date==as_date("2020-05-18")) %>% select(-positives, -total_pop)

cases_city<-cases_city %>% inner_join(rateratios) %>% 
  mutate(type=factor(type, levels=c("tests_pc", "pct_pos", "pos_pc")))

type_label<-cases_city %>% ungroup() %>% filter(!duplicated(type)) %>% pull(type2)
names(type_label)<-cases_city %>%  ungroup() %>% filter(!duplicated(type)) %>% pull(type)
ggplot(cases_city, aes(x=city_ci, y=rr, group=paste0(type, city))) +
  geom_hline(yintercept = 1, lty=2)+
  #geom_line(aes(color=city)) +
  #geom_point(aes(pch=type, fill=city), color="black", size=2)+
  geom_ribbon(aes(ymin=lci, ymax=uci, fill=city))+
  geom_line()+
  scale_x_log10()+
  scale_y_continuous(trans=log_trans(), breaks=c(0.66, 0.8, 1, 1.25, 1.5))+
  annotation_logticks(sides="b")+
  scale_fill_brewer(name="", type="qual", palette=2)+
  scale_shape_manual(values=c(21, 22, 23),name="",
                     labels=c("Positivity rate", "Cumulative incidence per 1,000", "Tests per 1,000"))+
  scale_linetype_manual(values=c(21, 22, 23),name="",
                        labels=c("Positivity rate", "Cumulative incidence per 1,000", "Tests per 1,000"))+
  guides(shape=guide_legend(override.aes=list(size=5)),
         color=guide_legend(override.aes=list(size=5)))+
  labs(x="Cumulative incidence per 100,000", 
       y="Rate Ratio (95% CI) per 1 SD increase in Deprivation Index",
       #title="Correlation of Zip Code Deprivation and COVID-19 Testing by City, Date and Outcome",
       caption="Source: NYCDOH, IDPH, PDPH, and ACS")+
  facet_wrap(~type, labeller = labeller(type=type_label)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.y=element_text(color="black", size=14),
        axis.text.x=element_text(color="black", size=12),
        axis.title=element_text(color="black", size=14, face="bold"),
        legend.text=element_text(color="black", size=14),
        strip.text=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave("Results/AppendixFigure5.pdf", width=10, height=7.5)


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
           y="Positivity rate",
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
           y="Positive tests per 1,000 pop",
           title="")+
      theme(axis.text=element_text(color="black"))
    title1<-unique(.x$date)
    title2<-.y$city
    title<-paste0("COVID-19 Testing and Outcomes in Zip Codes of ", title2, " by ", title1)
    arrangeGrob(grobs=list(p1, p2, p3), ncol=3,
                top=textGrob(title2, gp=gpar(fontsize=20,face="bold",font=8)))
  }, keep=T) 

pall<-arrangeGrob(grobs=plots, ncol=1)
ggsave("Results/AppendixFigure1.pdf", pall, width=12, height=6*3/2)
plot(pall)


# plotting specific variables for the last available date each place
vars<-c("mhi", "pct_nhwhite", 
        "pct_college", "no_healthins", 
        "pct_service", "pct_overcrowded1", "pc1")
last_date<-all %>% filter(date==as_date("2020-05-18")) %>% 
  select(GEOID, city, pct_pos, tests_pc, pos_pc, all_of(vars)) %>% 
  gather(var, value, -GEOID, -city, -pct_pos,-tests_pc, -pos_pc) %>% 
  mutate(var=factor(var, levels=vars))
names(vars)<-c("Log(Median Household Income)", "% Non-Hispanic White",
               "% College", "% Uninsured",
               "% Service Workers", "% Overcrowding", "Deprivation Index")
#.x<-last_date %>% filter(var=="mhi");.y<-data.frame(var="mhi", stringsAsFactors = F)
part1<-last_date %>% group_by(var) %>% 
  group_map(~{
    var_name<-names(vars)[vars%in%.y$var]
    plot<-ggplot(.x, aes(x=value, y=tests_pc)) +
      stat_smooth(method=lm, aes(color=city), se=F)+
      geom_point(aes(fill=city, shape=city), color="black")+
      scale_x_continuous(limits=c(NA, NA))+
      scale_y_log10()+
      scale_shape_manual(values=c(21, 22, 23))+
      guides(fill=F, color=F, shape=F)+
      labs(x="",
           y="",
           title=var_name)+
      theme_void()+
      theme(plot.title=element_text(size=14, color="black", hjust=.5),
            axis.title=element_text(size=14, color="black", angle=90))
    if (.y$var=="mhi"){
      plot<-plot+
        scale_x_log10(breaks=c(10000, 20000, 30000,
                               50000, 70000, 100000, 200000))+
        labs(y="Tests per 1,000")
    }
    plot
  })
part2<-last_date %>% group_by(var) %>% 
  group_map(~{
    var_name<-names(vars)[vars%in%.y$var]
    plot<-ggplot(.x, aes(x=value, y=pct_pos)) +
      stat_smooth(method=lm, aes(color=city), se=F)+
      geom_point(aes(fill=city, shape=city), color="black")+
      scale_x_continuous(limits=c(NA, NA))+
      scale_y_continuous(limits=c(0, NA), labels=scales::percent_format(accuracy = 1))+
      scale_shape_manual(values=c(21, 22, 23))+
      guides(fill=F, color=F, shape=F)+
      labs(x="",
           y="",
           title="")+
      theme_void()+
      theme(plot.title=element_text(size=14, color="black", hjust=.5),
            axis.title=element_text(size=14, color="black", angle=90))
    if (.y$var=="mhi"){
      plot<-plot+
        scale_x_log10(breaks=c(10000, 20000, 30000,
                               50000, 70000, 100000, 200000))+
        labs(y="Positivity rate")
    }
    plot
  })
part3<-last_date %>% group_by(var) %>% 
  group_map(~{
    var_name<-names(vars)[vars%in%.y$var]
    plot<-ggplot(.x, aes(x=value, y=pos_pc)) +
      stat_smooth(method=lm, aes(color=city), se=F)+
      geom_point(aes(fill=city, shape=city), color="black")+
      scale_x_continuous(limits=c(NA, NA))+
      scale_y_log10()+
      scale_shape_manual(values=c(21, 22, 23))+
      guides(fill=F, color=F, shape=F)+
      labs(x=var_name,
           y="",
           title="")+
      theme_void()+
      theme(plot.title=element_text(size=14, color="black", hjust=.5),
            axis.title.y=element_text(size=14, color="black", angle=90),
            axis.title.x=element_text(size=14, color="black"))
    if (.y$var=="mhi"){
      plot<-plot+
        scale_x_log10(breaks=c(10000, 20000, 30000,
                               50000, 70000, 100000, 200000))+
        labs(y="Positives per 1,000")
    }
    plot
  })
pall<-arrangeGrob(grobs=list(arrangeGrob(grobs=part1, nrow=1),
                             arrangeGrob(grobs=part2, nrow=1),
                             arrangeGrob(grobs=part3, nrow=1)), ncol=1)
legend<-ggplot(last_date, aes(x=value, y=pos_pc)) +
  stat_smooth(method=lm, aes(color=city), se=F)+
  geom_point(aes(fill=city, shape=city), color="black")+
  scale_shape_manual(values=c(21, 22, 23), name="")+
  scale_fill_discrete(name="")+
  scale_color_discrete(name="")+
  guides(fill=F, shape=F, color=guide_legend(override.aes=list(size=5)))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.text = element_text(size=16, color="black"),
        plot.title=element_text(size=16, color="black", hjust=.5),
        axis.title.y=element_text(size=16, color="black", angle=90),
        axis.title.x=element_text(size=16, color="black"))
legend<-get_legend(legend)
pall<-arrangeGrob(grobs=list(pall, legend), ncol=1,heights=c(10, 1))
ggsave("Results/Figure1.pdf", pall, width=20, height=12.5)

# table 1
vars<-c("mhi", "pct_nhwhite", 
        "pct_college", "no_healthins", 
        "pct_service", "pct_overcrowded1", "pc1")
last_date<-all %>% filter(date==as_date("2020-05-18")) %>% 
  select(GEOID, city, positives, all, total_pop, all_of(vars)) %>% 
  gather(var, value, -GEOID, -city, -positives,-all, -total_pop)
#.x<-last_date %>% filter(city=="Philadelphia", var=="pct_college")
table1<-last_date %>% 
  mutate(var=factor(var, levels=c(vars))) %>%
  mutate(value=ifelse(var=="mhi", log(value), value)) %>% 
  group_by(city, var) %>% 
  group_modify(~{
    .x$value<-as.numeric(scale(.x$value, scale=T, center=T))
    m_tests_pc<-glm(all~value+offset(log(total_pop)), data=.x, family="poisson")
    m_pct_pos<-glm(positives~value+offset(log(all)), data=.x, family="poisson")
    m_pos_pc<-glm(positives~value+offset(log(total_pop)), data=.x, family="poisson")
    map2_dfr(list(m_tests_pc, m_pct_pos, m_pos_pc), 
             c("tests_pc", "pct_pos", "pos_pc"),
             function(model, id){
               model %>% tidy %>% 
                 filter(term=="value") %>% 
                 select(estimate, std.error)%>% 
                 mutate(type=id)
             }) %>% 
      mutate(lci=exp(estimate-1.96*std.error),
             uci=exp(estimate+1.96*std.error),
             rr=exp(estimate),
             coef=paste0(format(rr, digits=2, nsmall=2),
                         "(",
                         format(lci, digits=2, nsmall=2),
                         ";",
                         format(uci, digits=2, nsmall=2),
                         ")")) %>% 
      select(type, coef) %>% 
      spread(type, coef)
  }) %>% 
  select(city, var, tests_pc, pct_pos, pos_pc)
fwrite(table1, "results/table1.csv")

smoother<-stat_smooth(method="lm", se=F)
#.x<-all %>% ungroup() %>% filter(city=="Philadelphia", date==max(date));.y<-data.frame(city="1PHL", date="0407", stringsAsFactors = F)
plots_pct_pos<-all %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    title1<-unique(.x$date)
    title2<-.y$city
    title<-paste0("COVID-19 Positivity Rate in Zip Codes of ", title2, " by ", title1)
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
           y="Positivity rate",
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
           y="Positivity rate",
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
           y="Positivity rate",
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
           y="Positivity rate",
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
           y="Positivity rate",
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
           y="Positivity rate",
           title="")
    pall<-(arrangeGrob(grobs=list(p1, p2, p3, p4, p5, p6), ncol=3, 
                       top=textGrob(title, gp=gpar(fontsize=20,face="bold",font=8)),
                       bottom=textGrob("Source: NYC DOH (github) and 5-year 2014-2018 ACS", gp=gpar(fontsize=10,font=8))))
    
  }, keep=T) 

#ggsave("Results/Chicago_Vars_Pct_pos.pdf", plots_pct_pos[[1]], width=16, height=12/(20/15))
#ggsave("Results/NYC_Vars_Pct_pos.pdf", plots_pct_pos[[2]], width=16, height=12/(20/15))
#ggsave("Results/PHL_Vars_Pct_pos.pdf", plots_pct_pos[[3]], width=16, height=12/(20/15))

plots_pos_pc<-all %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    title1<-unique(.x$date)
    title2<-.y$city
    title<-paste0("COVID-19 Positives per 1,000 in Zip Codes of ", title2, " by ", title1)
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

ggsave("Results/AppendixFigure2.pdf", plots_pos_pc[[1]], width=16, height=12/(20/15))
ggsave("Results/AppendixFigure3.pdf", plots_pos_pc[[2]], width=16, height=12/(20/15))
ggsave("Results/AppendixFigure4.pdf", plots_pos_pc[[3]], width=16, height=12/(20/15))


# MAP
# first get data by may 18th
vars<-c("mhi", "pct_nhwhite", 
        "pct_college", "no_healthins", 
        "pct_service", "pct_overcrowded1", "pc1")
last_date<-all %>% filter(date==as_date("2020-05-18")) %>% 
  select(GEOID, city, tests_pc,pct_pos, pos_pc, all_of(vars)) %>% 
  gather(var, value, -GEOID, -city, -tests_pc,-pct_pos, -pos_pc)
# first generic shapefiles
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
# modified zcta for NYC
city_var="New York City"

maps<-map(c("Chicago", "New York City", "Philadelphia"), function(city_var){
  data<-last_date %>% 
    filter(city==city_var, var=="pc1") %>% 
    select(GEOID, pct_pos, tests_pc, pos_pc, value) %>% 
    rename(pc1=value)
  if (city_var=="Philadelphia"){
    ## add missing zip code (navy yard)
    data<-data %>% bind_rows(data.frame(GEOID=19112))  
    bbox<-bbox_pa
    shp_with_data<-data %>% left_join(shp_zip)
  } else if (city_var=="Chicago") {
    bbox<-bbox_il
    shp_with_data<-data %>% left_join(shp_zip)
  } else {
    bbox<-bbox_ny
    shp_with_data<-data %>% left_join(shp_zip_mod)
  }
  
  m1<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=pos_pc))+
    scale_fill_binned(low="white", high="red", 
                      name="Positives per 1,000")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(size=F, alpha=F)+
    labs(title="Positives per capita") +
    theme_void()+
    theme(plot.title = element_text(size=20, face="bold", hjust=.5),
          panel.background = element_rect(fill = "white", color=NA),
          legend.position="bottom")
  m2<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=pc1))+
    scale_fill_binned(low="white", high="red", 
                      name="Deprivation Index")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(size=F, alpha=F)+
    labs(title="Deprivation") +
    theme_void()+
    theme(plot.title = element_text(size=20, face="bold", hjust=.5),
          panel.background = element_rect(fill = "white", color=NA),
          legend.position="bottom")
  
  full_map<-arrangeGrob(grobs=list(m1, m2), ncol=1,
                        top=textGrob(city_var, gp=gpar(fontsize=20,face="bold",font=8)))
  full_map
})
pall<-arrangeGrob(grobs=maps, ncol=3)
#ggsave("results/Figure3.pdf", pall, width=15, height=10)

shp_zip_point<-st_centroid(shp_zip)
shp_zip_mod_point<-st_centroid(shp_zip_mod)
# city_var<-"New York City"
maps_v2<-map(c("Chicago", "New York City", "Philadelphia"), function(city_var){
  # re-scale pc1
  data_pc1<-last_date %>% 
    filter(var=="pc1") %>% 
    select(GEOID, value) %>% 
    rename(pc1=value) %>% 
    ungroup() %>% 
    mutate(pc1=as.numeric(scale(pc1, scale=T, center=T)))
  data<-last_date %>% 
    filter(city==city_var, var=="pc1") %>% 
    select(GEOID, pct_pos, tests_pc, pos_pc, value) %>% 
    rename(pc1=value) %>% 
    left_join(data_pc1)
  if (city_var=="Philadelphia"){
    ## add missing zip code (navy yard)
    data<-data %>% bind_rows(data.frame(GEOID=19112))  
    bbox<-bbox_pa
  } else if (city_var=="Chicago") {
    bbox<-bbox_il
  } else {
    bbox<-bbox_ny
  }
  if (city_var=="New York City"){
    shp_with_data<-data %>% left_join(shp_zip_mod)
    shp_with_data2<-data %>% 
      left_join(shp_zip_mod_point) %>% 
      mutate(pos_pc_q5=cut(pos_pc, 
                           quantile(pos_pc, probs = seq(0, 1, by=.2), na.rm = T), 
                           include.lowest = T),
             pct_pos_q5=cut(pct_pos, 
                            quantile(pct_pos, probs = seq(0, 1, by=.2), na.rm = T), 
                            include.lowest = T),
             tests_pc_q5=cut(tests_pc, 
                             quantile(tests_pc, probs = seq(0, 1, by=.2), na.rm = T), 
                             include.lowest = T))
  } else {
    shp_with_data<-data %>% left_join(shp_zip)
    shp_with_data2<-data %>% 
      left_join(shp_zip_point) %>% 
      mutate(pos_pc_q5=cut(pos_pc, 
                           quantile(pos_pc, probs = seq(0, 1, by=.2), na.rm = T), 
                           include.lowest = T),
             pct_pos_q5=cut(pct_pos, 
                            quantile(pct_pos, probs = seq(0, 1, by=.2), na.rm = T), 
                            include.lowest = T),
             tests_pc_q5=cut(tests_pc, 
                             quantile(tests_pc, probs = seq(0, 1, by=.2), na.rm = T), 
                             include.lowest = T))
  }
  
  m1<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=pc1))+
    geom_sf(data=shp_with_data2, pch=21, color="black", fill=NA,
            aes(geometry=geometry, size=pos_pc_q5))+
    scale_size_manual(values=2^(0:4))+
    scale_fill_binned(name="Deprivation", type="gradient",
                      low="white", high="red")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, fill=F)+
    labs(title=ifelse(city_var=="Chicago", "Positives per Capita", "")) +
    theme_void()+
    theme(plot.title = element_text(size=20, face="bold", hjust=.5),
          panel.background = element_rect(fill = "white", color=NA),
          legend.position="bottom")
  m2<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=pc1))+
    geom_sf(data=shp_with_data2, pch=21, color="black", fill=NA,
            aes(geometry=geometry, size=pct_pos_q5))+
    scale_size_manual(values=2^(0:4))+
    scale_fill_binned(name="Deprivation", type="gradient",
                      low="white", high="red")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, fill=F)+
    labs(title=ifelse(city_var=="Chicago", "Positivity rate", "")) +
    theme_void()+
    theme(plot.title = element_text(size=20, face="bold", hjust=.5),
          panel.background = element_rect(fill = "white", color=NA),
          legend.position="bottom")
  m3<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=pc1))+
    geom_sf(data=shp_with_data2, pch=21, color="black", fill=NA,
            aes(geometry=geometry, size=tests_pc_q5))+
    scale_size_manual(values=2^(0:4))+
    scale_fill_binned(name="Deprivation", type="gradient",
                      low="white", high="red")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, fill=F)+
    labs(title=ifelse(city_var=="Chicago", "Tests per capita", "")) +
    theme_void()+
    theme(plot.title = element_text(size=20, face="bold", hjust=.5),
          panel.background = element_rect(fill = "white", color=NA),
          legend.position="bottom")
  arrangeGrob(grobs=list(m3, m2, m1), ncol=3,
              top=textGrob(city_var, gp=gpar(fontsize=20,face="bold",font=8)))
})
ylims<-c(abs(diff(bbox_il[c("ymax", "ymin")])), 
         abs(diff(bbox_ny[c("ymax", "ymin")])), 
         abs(diff(bbox_pa[c("ymax", "ymin")])))
pall<-arrangeGrob(grobs=maps_v2, ncol=1, heights=ylims/sum(ylims)*20)
ggsave("results/Figure3.pdf", pall, width=20, height=15)

# Clusters of the 4 variables
last_date<-all %>% filter(date==as_date("2020-05-18")) %>% 
  select(GEOID, city, tests_pc,pct_pos, pos_pc, all_of(vars)) %>% 
  gather(var, value, -GEOID, -city, -tests_pc,-pct_pos, -pos_pc) %>% 
  filter(var=="pc1") %>% 
  select(city, GEOID, pct_pos, tests_pc, pos_pc, value) %>% 
  rename(pc1=value)

cluster_maps<-map(c("pos_pc", "pct_pos", "tests_pc", "pc1"), function(outcome_var){
  maps_temp<-map(c("Chicago", "New York City", "Philadelphia"), function(city_var){
    data<-last_date %>% 
      filter(city==city_var) %>% 
      rename(outcome=contains(outcome_var))
    if (city_var=="Philadelphia"){
      ## add missing zip code (navy yard)
      missing_zip<-merge(shp_zip, data %>% bind_rows(data.frame(GEOID=19112)) %>% 
                           filter(GEOID==19112), by="GEOID", all.y=T, all.x=F) %>%
        st_as_sf %>% mutate(g=NA, g_p_adj=NA, gclust=NA)
      bbox<-bbox_pa
    } else if (city_var=="Chicago") {
      bbox<-bbox_il
    } else {
      bbox<-bbox_ny
    }
    if (city_var=="New York City"){
      shp_clusters<-merge(shp_zip_mod, data, by="GEOID", all.y=T, all.x=F)
    } else {
      shp_clusters<-merge(shp_zip, data, by="GEOID", all.y=T, all.x=F) 
    }
    if (outcome_var%in%c("pos_pc", "tests_pc")){
      shp_clusters<-shp_clusters %>% 
        mutate(outcome=log(outcome))
    }
    outcome_labels<-case_when(
      outcome_var=="pos_pc" ~ c("Low incidence", "High incidence"),
      outcome_var=="pct_pos" ~ c("Low positivity", "High positivity"),
      outcome_var=="tests_pc" ~ c("Low testing", "High testing"),
      outcome_var=="pc1" ~ c("Low deprivation", "High deprivation")
    )
    
    map_nbq <-poly2nb(shp_clusters) # Creates list of neighbors to each CSA
    map_nbq_w <-nb2listw(include.self(map_nbq), zero.policy = T) # Creates list of neighbors and weights. Weight = 1/number of neighbors.
    shp_clusters<-shp_clusters %>% 
      mutate(g=localG(outcome, map_nbq_w),
             g_p_adj=p.adjustSP(pnorm(2*(abs(g)), lower.tail=FALSE),map_nbq, "bonferroni"),
             g=ifelse(g_p_adj<0.05, g, NA),
             gclust=ifelse(g<0, outcome_labels[[1]], outcome_labels[[2]])) %>% 
      st_as_sf
    if (city_var=="Philadelphia") shp_clusters<-rbind(shp_clusters, missing_zip)
    ggplot()+
      geom_sf(data=shp_clusters %>% filter(!is.na(gclust)), size=0,color=NA,
              aes(geometry=geometry, fill=as.factor(gclust)))+
      geom_sf(data=shp_clusters, size=.1,fill=NA,
              aes(geometry=geometry))+
      # geom_sf(data=shp_clusters, size=0,color=NA,
      #         aes(geometry=geometry, fill=g))+
      scale_fill_brewer(type="qual", palette=2, name="Cluster")+
      # scale_fill_binned(type="viridis",
      #                   name="Deprivation Index")+
      coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
               ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
      guides(size=F, alpha=F)+
      labs(title=city_var) +
      theme_void()+
      theme(plot.title = element_text(size=20, face="bold", hjust=.5),
            panel.background = element_rect(fill = "white", color=NA),
            legend.position="bottom",
            legend.text=element_text(color="black", size=20),
            legend.title=element_text(face="bold",color="black", size=20))
  })
  legend<-get_legend(maps_temp[[1]])
  maps_temp<-map(maps_temp, function(xx) xx+guides(fill=F))
  pall<-arrangeGrob(grobs=maps_temp, ncol=3)
  pall<-arrangeGrob(grobs=list(pall, legend), nrow=2, heights=c(10, 1))
  pall
})
ggsave("results/Figure5.pdf", cluster_maps[[1]], width=20, height=7.5)
ggsave("results/AppendixFigure6.pdf", cluster_maps[[2]], width=20, height=7.5)
ggsave("results/AppendixFigure7.pdf", cluster_maps[[3]], width=20, height=7.5)
ggsave("results/AppendixFigure8.pdf", cluster_maps[[4]], width=20, height=7.5)


# save loadings
apptable1<-loadings %>% spread(city, loading) %>% 
  mutate(var=factor(var, levels=c("logmhi", "pct_nhwhite", 
                                  "pct_college", "no_healthins",
                                  "pct_service", "pct_overcrowded1"))) %>% 
  arrange(var)
fwrite(apptable1, file="Results/AppendixTable1.csv")



