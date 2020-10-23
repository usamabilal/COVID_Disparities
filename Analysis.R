rm(list=ls())
# function to get legends out of ggplot grobs
get_legend<-function(plot){
  grobs<-ggplotGrob(plot)$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]  
  return(legend)
}
# function to get default ggplot discrete colors https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n, alpha=1) {
  hues = seq(15, 375, length = n + 1)
  t<-hcl(h = hues, l = 65, c = 100)[1:n]
  adjustcolor(t, alpha)
}
# basic packages + plotting
library(tidyverse)
library(data.table)
library(gridExtra)
library(grid)
library(lubridate)
library(scales)
library(broom)
# packages with useful functions for model checking
library(MASS)
library(performance)
# mapping and spatial analysis
library(INLA)
library(spdep)
library(sf)
select<-dplyr::select
gather<-tidyr::gather
options(scipen=999)
load("data/clean_data.rdata")

# list of variables we'll be looking at later on:
vars_outcome<-c("tests_pc", "pct_pos", "pos_pc", "deaths_pc")
vars_exposure<-c("svi", "svi1", "svi2", "svi3", "svi4")
xlabs_outcome<-c("Testing", "Positivity", "Incidence", "Mortality")
names(xlabs_outcome)<-vars_outcome
xlabs_svi<-c("Social Vulnerability Index",
             "Socioeconomic Status",
             "Household composition & disability",
             "Minority status & language",
             "Housing type & transportation")
names(xlabs_svi)<-vars_exposure
xlabs<-c(xlabs_outcome, xlabs_svi)

# total numbers for the first results paragraph
last_available_date %>% 
  group_by(city) %>% 
  summarise(all=sum(all),
            positives=sum(positives),
            deaths=sum(deaths))

# figure 1: Show city-level epidemic curves
colors<-gg_color_hue(3)
f1a<-ggplot(city_level, aes(x=date, y=positives_pc)) +
  annotate("rect", 
           xmin=mdy("03-01-2020"),
           xmax=mdy("10-03-2020"),
           ymin=30, ymax=40, fill=colors[1], alpha=0.2)+
  annotate("rect", 
           xmin=mdy("04-01-2020"),
           xmax=mdy("10-01-2020"),
           ymin=40, ymax=50, fill=colors[2], alpha=0.2)+
  annotate("rect", 
           xmin=mdy("03-01-2020"),
           xmax=mdy("10-01-2020"),
           ymin=20, ymax=30, fill=colors[3], alpha=0.2)+
  geom_line(aes(color=city)) +
  scale_x_date(breaks="1 month", date_labels = "%b",
               limits=c(mdy("01-01-2020"),mdy("10-03-2020")))+
  scale_color_discrete(name="")+
  labs(x="", y="Confirmed cases per 100,000 (rolling 7-day mean)",
       title="New Cases")+
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_text(color="black", size=14),
        axis.text.x=element_text(color="black", size=12),
        axis.title=element_text(color="black", size=14, face="bold"),
        legend.text=element_text(color="black", size=14),
        strip.text=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        plot.title=element_text(color="black", size=16, face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
f1b<-ggplot(city_level, aes(x=date, y=deaths_pc)) +
  annotate("rect", 
           xmin=mdy("03-01-2020"),
           xmax=mdy("10-03-2020"),
           ymin=4, ymax=5, fill=colors[1], alpha=0.2)+
  annotate("rect", 
           xmin=mdy("05-18-2020"),
           xmax=mdy("10-01-2020"),
           ymin=5, ymax=6, fill=colors[2], alpha=0.2)+
  annotate("rect", 
           xmin=mdy("05-22-2020"),
           xmax=mdy("10-01-2020"),
           ymin=3, ymax=4, fill=colors[3], alpha=0.2)+
  geom_line(aes(color=city)) +
  scale_x_date(breaks="1 month", date_labels = "%b",
               limits=c(mdy("01-01-2020"),mdy("10-03-2020")))+
  scale_color_discrete(name="")+
  labs(x="", y="Deaths per 100,000 (rolling 7-day mean)",
       title="New Deaths")+
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_text(color="black", size=14),
        axis.text.x=element_text(color="black", size=12),
        axis.title=element_text(color="black", size=14, face="bold"),
        legend.text=element_text(color="black", size=14),
        strip.text=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        plot.title=element_text(color="black", size=16, face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
legend<-get_legend(f1a)
f1a<-f1a+guides(color=F)
f1b<-f1b+guides(color=F)
figure1<-arrangeGrob(grobs=list(f1a, f1b), ncol=1)
figure1<-arrangeGrob(grobs=list(figure1, legend), ncol=1, heights =c(20, 1))
grid.draw(figure1)
ggsave("results/Figure1.pdf",figure1,  width=7.5, height=12.5)

# exploratory figures and tables
# choropleth maps of outcomes and exposures
# city_var<-"New York City"
maps_outcome<-map(c("Chicago", "New York City", "Philadelphia"), function(city_var){
  data<-last_available_date %>% 
    filter(city==city_var) %>% 
    select(GEOID, pct_pos, tests_pc, pos_pc, deaths_pc) 
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
    shp_with_data<-data %>% 
      left_join(shp_zip_mod) %>% 
      ungroup() 
  } else {
    shp_with_data<-data %>% 
      left_join(shp_zip) %>% 
      ungroup()
  }
  
  m1<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=pos_pc))+
    scale_fill_binned(name="Cases\nper 1,000", type="gradient",
                      show.limits=T,n.breaks=5,labels=round,
                      low="white", high="red", trans="log10")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, color=guide_bins(override.aes = list(color="black")))+
    #labs(title=ifelse(city_var=="Chicago", "Incidence", "")) +
    labs(title="Incidence") +
    theme_void()+
    theme(plot.title = element_text(size=20, face="bold", hjust=.5),
          panel.background = element_rect(fill = "white", color=NA),
          legend.position="bottom")
  m2<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=pct_pos*100))+
    scale_fill_binned(name="Positivity (%)", type="gradient",
                      show.limits=T,n.breaks=5,labels=round,
                      low="white", high="red")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, color=guide_bins(override.aes = list(color="black")))+
    #labs(title=ifelse(city_var=="Chicago", "Positivity", "")) +
    labs(title="Positivity") +
    theme_void()+
    theme(plot.title = element_text(size=20, face="bold", hjust=.5),
          panel.background = element_rect(fill = "white", color=NA),
          legend.position="bottom")
  m3<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=tests_pc/10))+
    scale_fill_binned(name="Tests\nper 10,000", type="gradient",
                      show.limits=T,n.breaks=5,labels=round,
                      low="white", high="red", trans="log10")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, color=guide_bins(override.aes = list(color="black")))+
    #labs(title=ifelse(city_var=="Chicago", "Testing", "")) +
    labs(title="Testing") +
    theme_void()+
    theme(plot.title = element_text(size=20, face="bold", hjust=.5),
          panel.background = element_rect(fill = "white", color=NA),
          legend.position="bottom")
  m4<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=deaths_pc))+
    scale_fill_binned(name="Deaths\nper 1,000", type="gradient",
                      show.limits=T,n.breaks=5,labels=round,
                      low="white", high="red", trans="log10")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, color=guide_bins(override.aes = list(color="black")))+
    #labs(title=ifelse(city_var=="Chicago", "Mortality", "")) +
    labs(title="Mortality") +
    theme_void()+
    theme(plot.title = element_text(size=20, face="bold", hjust=.5),
          panel.background = element_rect(fill = "white", color=NA),
          legend.position="bottom")
  arrangeGrob(grobs=list(m3, m2, m1, m4), ncol=4,
              top=textGrob(city_var, gp=gpar(fontsize=26,face="bold",font=8)))
})
ylims<-c(abs(diff(bbox_il[c("ymax", "ymin")])), 
         abs(diff(bbox_ny[c("ymax", "ymin")])), 
         abs(diff(bbox_pa[c("ymax", "ymin")])))
pall<-arrangeGrob(grobs=maps_outcome, ncol=1, heights=ylims/sum(ylims)*20)
ggsave("results/AppendixFigure1.pdf", pall, width=25, height=15)
plot(pall)
# exposures
maps_exposures<-map(c("Chicago", "New York City", "Philadelphia"), function(city_var){
  data<-last_available_date %>% 
    filter(city==city_var) %>% 
    select(GEOID, svi, svi1, svi2, svi3, svi4) 
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
    shp_with_data<-data %>% 
      left_join(shp_zip_mod) %>% 
      ungroup() 
  } else {
    shp_with_data<-data %>% 
      left_join(shp_zip) %>% 
      ungroup()
  }
  map<-map(0:4, function(i){
    variable<-ifelse(i==0, "svi", paste0("svi", i))
    ggplot()+
      geom_sf(data=shp_with_data, size=.1,
              aes_string(geometry="geometry", fill=variable))+
      scale_fill_binned(name="", type="gradient",
                        show.limits=T,n.breaks=5,labels=round,
                        low="white", high="red")+
      coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
               ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
      guides(alpha=F, size=F, color=guide_bins(override.aes = list(color="black")))+
      #labs(title=ifelse(city_var=="Chicago", xlabs_svi[i+1], "")) +
      labs(title=xlabs_svi[i+1]) +
      theme_void()+
      theme(plot.title = element_text(size=20, face="bold", hjust=.5),
            panel.background = element_rect(fill = "white", color=NA),
            legend.position="bottom")
  })
  arrangeGrob(grobs=map, ncol=5,
              top=textGrob(city_var, gp=gpar(fontsize=26,face="bold",font=8)))
})
ylims<-c(abs(diff(bbox_il[c("ymax", "ymin")])), 
         abs(diff(bbox_ny[c("ymax", "ymin")])), 
         abs(diff(bbox_pa[c("ymax", "ymin")])))
pall<-arrangeGrob(grobs=maps_exposures, ncol=1, heights=ylims/sum(ylims)*20)
ggsave("results/AppendixFigure2.pdf", pall, width=25, height=15)
plot(pall)

# Global Moran's I of the 4 outcomes
globalmoran<-last_available_date %>% 
  select(city, GEOID, pct_pos, tests_pc, pos_pc, deaths_pc) %>% 
  gather(variable, value, -city, -GEOID) %>% 
  group_by(city, variable) %>% 
  group_modify(~{
    # get neighbors
    id<-which(neighbors_id$city==.y$city)
    nb_mat<-neighbors[[id]]
    temp<-moran.test(x = .x %>% pull(value), listw = nb2listw(nb_mat, style = "B"))
    data.frame(statistic=temp$estimate[1], pval=temp$p.value)
  }) %>% 
  mutate(variable=factor(variable, levels=c(vars_outcome, vars_exposure),
                         labels=xlabs),
         out=paste0(round(statistic, digits=3), " (", ifelse(pval<0.001, "<0.001",round(pval, digits=3)), ")")) %>% 
  select(city, variable, out) %>% 
  arrange(city, variable) %>% 
  spread(city, out)
globalmoran
fwrite(globalmoran, file="results/AppendixTable3.csv")

# local Moran's I of the 4 outcomes and 5 exposures
localmoran<-last_available_date %>% 
  select(city, GEOID, pct_pos, tests_pc, pos_pc, deaths_pc,svi, svi1, svi2, svi3, svi4) %>% 
  gather(variable, value, -city, -GEOID) %>% 
  group_by(city, variable) %>% 
  group_modify(~{
    # get neighbors
    print(paste0(.y$city, "; ", .y$variable))
    id<-which(neighbors_id$city==.y$city)
    nb_mat<-neighbors[[id]]
    temp<-localmoran(x = .x %>% pull(value), 
                     listw = nb2listw(nb_mat, style = "B"))
    # standardize value in the ZCTA, and get lagged values
    .x<-.x %>% 
      mutate(value=as.numeric(scale(value, center=T, scale=T)),
             lagged=lag.listw(x=nb2listw(nb_mat, style = "B"), 
                              var=value),
             pval=temp %>% as.data.frame %>% pull(`Pr(z > 0)`),
             moran_cluster=case_when(
               value>0 & lagged>0 ~ "high-high",
               value<=0 & lagged<=0 ~ "low-low",
               value>0 & lagged<=0 ~ "high-low",
               value<=0 & lagged>0 ~ "low-high",
             ),
             moran_cluster=ifelse(pval<0.05, moran_cluster, NA))
  }) %>% 
  mutate(variable=factor(variable, levels=c(vars_outcome, vars_exposure)),
         moran_cluster=factor(moran_cluster, levels=c("high-high", "high-low",
                                                      "low-high",
                                                      "low-low", NA)))

maps_moran<-localmoran %>% group_by(city) %>% 
  group_map(~{
    #.x<-localmoran %>% filter(city=="Chicago");.y<-data.frame(city="Chicago", variable="deaths_pc")
    city_var<-.y$city
    maps<-.x %>% group_by(variable) %>% 
      group_map(~{
        #.x<-.x %>% filter(variable=="deaths_pc")
        if (city_var=="Philadelphia"){
          ## add missing zip code (navy yard)
          .x<-.x %>% bind_rows(data.frame(GEOID=19112))  
          bbox<-bbox_pa
        } else if (city_var=="Chicago") {
          bbox<-bbox_il
        } else {
          bbox<-bbox_ny
        }
        if (city_var=="New York City"){
          shp_clusters<-merge(shp_zip_mod, .x, by="GEOID", all.y=T, all.x=F)
        } else {
          shp_clusters<-merge(shp_zip, .x, by="GEOID", all.y=T, all.x=F) 
        }
        #title=ifelse(city_var=="Chicago",xlabs[which(names(xlabs)%in%.y$variable)],"")
        title<-xlabs[which(names(xlabs)%in%.y$variable)]
        moran_colors<-c("high-high"="red",
                        "high-low"="tomato1",
                        "low-high"="skyblue1",
                        "low-low"="blue")
        ggplot()+
          geom_sf(data=shp_clusters %>% filter(!is.na(moran_cluster)), size=0,color=NA,
                  aes(geometry=geometry, fill=(moran_cluster)))+
          geom_sf(data=shp_clusters, size=.1,fill=NA,
                  aes(geometry=geometry))+
          scale_fill_manual(values=moran_colors, name="")+
          coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
                   ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
          guides(size=F, alpha=F)+
          labs(title=title) +
          theme_void()+
          theme(plot.title = element_text(size=20, face="bold", hjust=.5),
                panel.background = element_rect(fill = "white", color=NA),
                legend.position="bottom",
                legend.text=element_text(color="black", size=20),
                legend.title=element_text(face="bold",color="black", size=20))
      })
    legend<-get_legend(maps[[1]])
    maps<-map(maps, function(xx) xx+guides(fill=F))
    moran_outcome<-arrangeGrob(grobs=maps[1:4], ncol=4,
                           top=textGrob(city_var, gp=gpar(fontsize=26,face="bold",font=8)))
    moran_exp<-arrangeGrob(grobs=maps[5:9], ncol=5,
                               top=textGrob(city_var, gp=gpar(fontsize=26,face="bold",font=8)))
    list(legend=legend, moran_exp=moran_exp, moran_outcome=moran_outcome)
  })
ylims<-c(abs(diff(bbox_il[c("ymax", "ymin")])), 
         abs(diff(bbox_ny[c("ymax", "ymin")])), 
         abs(diff(bbox_pa[c("ymax", "ymin")])))
moran_exp<-map(maps_moran, function(xx) xx$moran_exp)
moran_outcome<-map(maps_moran, function(xx) xx$moran_outcome)
legend<-maps_moran[[2]]$legend
pall<-arrangeGrob(grobs=moran_exp, ncol=1, heights=ylims/sum(ylims)*20)
pall<-arrangeGrob(grobs=list(pall, legend), ncol=1, heights=c(30, 1))
ggsave("results/AppendixFigure3.pdf", pall, width=25, height=15)
pall<-arrangeGrob(grobs=moran_outcome, ncol=1, heights=ylims/sum(ylims)*20)
pall<-arrangeGrob(grobs=list(pall, legend), ncol=1, heights=c(30, 1))
ggsave("results/Figure2.pdf", pall, width=25, height=15)


# SVI vs 4 outcomes
smoother<-stat_smooth(method="loess", se=F)
#.x<-last_available_date %>% ungroup() %>% filter(city=="Philadelphia");.y<-data.frame(city="1PHL", date="0407", stringsAsFactors = F)
plots<-last_available_date %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    p1<-ggplot(.x, aes(x=svi, y=tests_pc)) +
      geom_point()+
      smoother+
      scale_y_log10()+
      scale_x_continuous(limits=c(NA, NA))+
      annotation_logticks(sides="l")+
      theme_bw() +
      labs(x="Social Vulnerability Index (SD)",
           y="Tests per 1,000",
           title="Testing")+
      theme(axis.text=element_text(color="black"))
    p2<-ggplot(.x, aes(x=svi, y=pct_pos)) +
      geom_point()+
      smoother+
      scale_y_continuous(limits=c(0, NA), labels=scales::percent_format(accuracy = 1))+
      scale_x_continuous(limits=c(NA, NA))+
      #annotation_logticks(sides="b")+
      theme_bw() +
      labs(x="Social Vulnerability Index (SD)",
           y="Positivity ratio",
           title="Positivity")+
      theme(axis.text=element_text(color="black"))
    p3<-ggplot(.x, aes(x=svi, y=pos_pc)) +
      geom_point()+
      smoother+
      scale_y_log10()+
      scale_x_continuous(limits=c(NA, NA))+
      annotation_logticks(sides="l")+
      theme_bw() +
      labs(x="Social Vulnerability Index (SD)",
           y="Incidence per 1,000",
           title="Incidence")+
      theme(axis.text=element_text(color="black"))
    p4<-ggplot(.x, aes(x=svi, y=deaths_pc)) +
      geom_point()+
      smoother+
      scale_y_log10()+
      scale_x_continuous(limits=c(NA, NA))+
      annotation_logticks(sides="l")+
      theme_bw() +
      labs(x="Social Vulnerability Index (SD)",
           y="Deaths per 1,000",
           title="Mortality")+
      theme(axis.text=element_text(color="black"))
    title1<-unique(.x$date)
    title2<-.y$city
    title<-paste0("COVID-19 Testing and Outcomes in Zip Codes of ", title2, " by ", title1)
    arrangeGrob(grobs=list(p1, p2, p3, p4), ncol=4,
                top=textGrob(title2, gp=gpar(fontsize=20,face="bold",font=8)))
  }, .keep=T) 

pall<-arrangeGrob(grobs=plots, ncol=1)
ggsave("Results/Figure3.pdf", pall, width=16, height=6*3/2)
plot(pall)
# 4 SVI domains vs 4 outcomes [appendix]
smoother<-stat_smooth(method="loess", se=F)
plots_tests_pc<-last_available_date %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    title<-.y$city
    ylab<-"Tests per 1,000 pop."
    temp<-map(1:4, function(i){
      variable<-paste0("svi", i)
      ggplot(.x, aes_string(x=variable, y="tests_pc")) +
        smoother+
        geom_point()+
        scale_x_continuous()+
        scale_y_log10()+
        annotation_logticks(sides="l")+
        theme_bw() +
        labs(x=xlabs[which(names(xlabs)%in%variable)],
             y=ylab,
             title="")+
        theme(axis.text=element_text(color="black"),
              axis.title=element_text(color="black", size=16, face="bold"))
    })
    pall<-(arrangeGrob(grobs=temp, ncol=4, 
                       top=textGrob(title, gp=gpar(fontsize=20,face="bold",font=8))))
    
  }, keep=T) 
plots_tests_pc<-arrangeGrob(grobs=plots_tests_pc, ncol=1)
ggsave("Results/AppendixFigure5.pdf",plots_tests_pc, width=20, height=10)
plots_pct_pos<-last_available_date %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    title<-.y$city
    ylab<-"Positivity ratio"
    temp<-map(1:4, function(i){
      variable<-paste0("svi", i)
      ggplot(.x, aes_string(x=variable, y="pct_pos")) +
        smoother+
        geom_point()+
        scale_x_continuous()+
        scale_y_continuous(limits=c(0, NA), labels=scales::percent_format(accuracy = 1))+
        #annotation_logticks(sides="b")+
        theme_bw() +
        labs(x=xlabs[which(names(xlabs)%in%variable)],
             y=ylab,
             title="")+
        theme(axis.text=element_text(color="black"),
              axis.title=element_text(color="black", size=16, face="bold"))
    })
    pall<-(arrangeGrob(grobs=temp, ncol=4, 
                       top=textGrob(title, gp=gpar(fontsize=20,face="bold",font=8))))
    
  }, keep=T) 
plots_pct_pos<-arrangeGrob(grobs=plots_pct_pos, ncol=1)
ggsave("Results/AppendixFigure6.pdf",plots_pct_pos, width=20, height=10)
plots_pos_pc<-last_available_date %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    title<-.y$city
    ylab<-"Cases per 1,000 pop."
    temp<-map(1:4, function(i){
      variable<-paste0("svi", i)
      ggplot(.x, aes_string(x=variable, y="pos_pc")) +
        smoother+
        geom_point()+
        scale_x_continuous()+
        scale_y_log10()+
        annotation_logticks(sides="l")+
        theme_bw() +
        labs(x=xlabs[which(names(xlabs)%in%variable)],
             y=ylab,
             title="")+
        theme(axis.text=element_text(color="black"),
              axis.title=element_text(color="black", size=16, face="bold"))
    })
    pall<-(arrangeGrob(grobs=temp, ncol=4, 
                       top=textGrob(title, gp=gpar(fontsize=20,face="bold",font=8))))
    
  }, keep=T) 
plots_pos_pc<-arrangeGrob(grobs=plots_pos_pc, ncol=1)
ggsave("Results/AppendixFigure7.pdf",plots_pos_pc, width=20, height=10)
plots_deaths_pc<-last_available_date %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    title<-.y$city
    ylab<-"Deaths per 1,000 pop."
    temp<-map(1:4, function(i){
      variable<-paste0("svi", i)
      ggplot(.x, aes_string(x=variable, y="deaths_pc")) +
        smoother+
        geom_point()+
        scale_x_continuous()+
        scale_y_log10()+
        annotation_logticks(sides="l")+
        theme_bw() +
        labs(x=xlabs[which(names(xlabs)%in%variable)],
             y=ylab,
             title="")+
        theme(axis.text=element_text(color="black"),
              axis.title=element_text(color="black", size=16, face="bold"))
    })
    pall<-(arrangeGrob(grobs=temp, ncol=4, 
                       top=textGrob(title, gp=gpar(fontsize=20,face="bold",font=8))))
    
  }, keep=T) 
plots_deaths_pc<-arrangeGrob(grobs=plots_deaths_pc, ncol=1)
ggsave("Results/AppendixFigure8.pdf",plots_deaths_pc, width=20, height=10)


# Modeling
last_date<-last_available_date %>% 
  rowwise() %>% 
  mutate(pct_age65plus=pct_age6574+pct_age7584+pct_age85plus) %>% 
  select(GEOID, city, positives, all, deaths, total_pop, all_of(vars_exposure), pct_age65plus) %>%
  gather(var, value, -GEOID, -city, -positives,-all,-deaths, -total_pop, -pct_age65plus) %>%
  mutate(var=factor(var, levels=c(vars_exposure))) %>%
  mutate(value=ifelse(var=="mhi", log(value), value)) %>%
  group_by(city, var) %>%
  group_split(.keep=T)
last_date_ids<-last_available_date %>% 
  rowwise() %>% 
  mutate(pct_age65plus=pct_age6574+pct_age7584+pct_age85plus) %>% 
  select(GEOID, city, positives, all, deaths, total_pop, all_of(vars_exposure), pct_age65plus) %>%
  gather(var, value, -GEOID, -city, -positives,-all,-deaths, -total_pop, -pct_age65plus) %>%
  mutate(var=factor(var, levels=c(vars_exposure))) %>%
  mutate(value=ifelse(var=="mhi", log(value), value)) %>%
  group_by(city, var) %>%
  group_keys()

# first, create table with means and variances for the three outcomes by city
var_distribution_table<-last_available_date %>% 
  select(city, GEOID, positives, all, deaths) %>% 
  gather(outcome, value, -city, -GEOID) %>% 
  group_by(city, outcome) %>% 
  summarise(mean=mean(value),
            variance=var(value)) %>% 
  mutate(outcome=factor(outcome, levels=c("all", "positives", "deaths"),
                        labels=c("Tests", "Cases", "Deaths"))) %>% 
  arrange(city ,outcome)
var_distribution_table
fwrite(var_distribution_table, file="Results/AppendixTable1.csv")

# second, test Poisson model [non-spatial] and check for overdispersion
overdispersion_table<-map_dfr(last_date, function(temp){
  city_var<-unique(temp$city)
  var<-unique(temp$var)
  print(paste0(city_var, "; ", var))
  temp$value<-as.numeric(scale(temp$value, center=T, scale=T))
  m_tests_pc<-glm(formula=all~value+offset(log(total_pop)),family="poisson",
                  data=temp)
  m_pct_pos<-glm(formula=positives~value+offset(log(all)),family="poisson",
                 data=temp)
  m_pos_pc<-glm(formula=positives~value+offset(log(total_pop)),family="poisson",
                data=temp)
  m_deaths_pc<-glm(formula=deaths~value+offset(log(total_pop)),family="poisson",
                   data=temp)
  
  d1<-check_overdispersion(m_tests_pc)
  d2<-check_overdispersion(m_pct_pos)
  d3<-check_overdispersion(m_pos_pc)
  d4<-check_overdispersion(m_deaths_pc)
  data.frame(city=city_var, var=var, tests_pc=d1$p_value, pct_pos=d2$p_value,
             pos_pc=d3$p_value, deaths_pc=d4$p_value)
}) %>% 
  mutate(tests_pc=ifelse(tests_pc<0.001, "<0.001", round(tests_pc, digits=3)),
         pct_pos=ifelse(pct_pos<0.001, "<0.001", round(pct_pos, digits=3)),
         pos_pc=ifelse(pos_pc<0.001, "<0.001", round(pos_pc, digits=3)),
         deaths_pc=ifelse(deaths_pc<0.001, "<0.001", round(deaths_pc, digits=3))) %>% 
  mutate(var=factor(var, levels=c("svi", "svi1", "svi2", "svi3", "svi4"),
                    labels=xlabs_svi))
overdispersion_table
fwrite(overdispersion_table, file="Results/AppendixTable2.csv")
  
# is there evidence for spatial autocorrelation conditional on SVI?
#temp<-last_date[[1]]
naive_models<-map(last_date, function(temp){
  city_var<-unique(temp$city)
  var<-unique(temp$var)
  print(paste0(city_var, "; ", var))
  id<-which(neighbors_id$city==city_var)
  nb_mat<-neighbors[[id]]
  nb_matrix<-nb2mat(nb_mat,style = "B")
  temp$value<-as.numeric(scale(temp$value, center=T, scale=T))
  m_tests_pc<-glm.nb(formula=all~value+pct_age65plus+offset(log(total_pop)),
                     data=temp)
  m_pct_pos<-glm.nb(formula=positives~value+pct_age65plus+offset(log(all)),
                    data=temp)
  m_pos_pc<-glm.nb(formula=positives~value+pct_age65plus+offset(log(total_pop)),
                   data=temp)
  m_deaths_pc<-glm.nb(formula=deaths~value+pct_age65plus+offset(log(total_pop)),
                      data=temp)
  
  moran<-list(moran.test(residuals(m_tests_pc, type="pearson"),
                         listw=nb2listw(nb_mat, style="B", zero.policy = T)),
              moran.test(residuals(m_pct_pos, type="pearson"),
                         listw=nb2listw(nb_mat, style="B", zero.policy = T)),
              moran.test(residuals(m_pos_pc, type="pearson"),
                         listw=nb2listw(nb_mat, style="B", zero.policy = T)),
              moran.test(residuals(m_deaths_pc, type="pearson"),
                         listw=nb2listw(nb_mat, style="B", zero.policy = T)))
  correlo<-list(sp.correlogram(nb_mat, residuals(m_tests_pc, type="pearson"), order = 5, zero.policy = T),
                sp.correlogram(nb_mat, residuals(m_pct_pos, type="pearson"), order = 5, zero.policy = T),
                sp.correlogram(nb_mat, residuals(m_pos_pc, type="pearson"), order = 5, zero.policy = T),
                sp.correlogram(nb_mat, residuals(m_deaths_pc, type="pearson"), order = 5, zero.policy = T))
  list(moran=moran, correlo=correlo, city=city_var, variable=var)
})
# moran on NB model with SVI
moran_model<-map_dfr(naive_models, function(model){
  moran<-model$moran
  moran<-map_chr(moran, function(temp){
    ifelse(temp$p.value<0.001, "<0.001", round(temp$p.value, digits=3))
  })
  data.frame(city=model$city, var=model$variable,
             tests_pc=moran[[1]], pct_pos=moran[[2]], pos_pc=moran[[3]], deaths_pc=moran[[4]])
})
moran_model
fwrite(moran_model, file="results/AppendixTable4.csv")


correlos<-map_dfr(naive_models, function(model){
  correlo<-model$correlo
  correlo<-map2_dfr(correlo, c("tests_pc", "pct_pos", "pos_pc", "deaths_pc"), function(temp, outcome){
    res<-as.numeric(temp$res)
    lags<-as.numeric(names(temp$res))
    data.frame(type=outcome, res=res, lag=lags, city=model$city, var=model$variable)
  })
  correlo
}) %>% 
  mutate(type2=case_when(
    type=="pct_pos" ~ "Positivity",
    type=="deaths_pc" ~ "Mortality",
    type=="pos_pc" ~ "Incidence",
    type=="tests_pc" ~ "Testing"),
    type2=factor(type2, levels=c("Testing", "Positivity", "Incidence" ,"Mortality")))

ggplot(correlos %>% filter(var=="svi"), aes(x=lag, y=res)) +
  geom_linerange(aes(ymin=0, ymax=res))+
  geom_hline(yintercept = 0, lty=1)+
  facet_grid(type2~city) + 
  labs(x="Lags", y="Spatial autocorrelation")+
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_text(color="black", size=10),
        axis.text.x=element_text(color="black", size=10),
        axis.title=element_text(color="black", size=14, face="bold"),
        legend.text=element_text(color="black", size=14),
        strip.text=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = "Results/AppendixFigure4.pdf", width=10, height=12)


# we have overdispersion -> use negative binomial
# we have spatial autocorrelation -> use spatial model
# negative binomial spatial CAR model -> INLA
# modeling time

# data for spatio temporal models
bycity<-changes %>% 
  mutate(pct_age65plus=pct_age6574+pct_age7584+pct_age85plus) %>% 
  filter(month<=9) %>% 
  ungroup() %>% 
  group_by(city) %>% 
  group_split(.keep = T)


run_models<-F
if (run_models){
  #temp<-last_date[[1]]
  table1_models<-map(last_date, function(temp){
    city_var<-unique(temp$city)
    var<-unique(temp$var)
    print(paste0(city_var, "; ", var))
    inla_adj<-case_when(
      city_var=="Chicago" ~ "Data/nb_inla_chi.adj",
      city_var=="New York City" ~ "Data/nb_inla_nyc.adj",
      city_var=="Philadelphia" ~ "Data/nb_inla_phl.adj"
    )
    temp$value<-as.numeric(scale(temp$value, center=T, scale=T))
    temp$spatial_unit<-as.numeric(as.factor(temp$GEOID))
    m_tests_pc <-inla(formula=all~value+pct_age65plus+offset(log(total_pop))+
                        f(spatial_unit, model = "bym", 
                          graph = inla_adj,
                          hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                     prec.spatial=list(prior="loggamma", param=c(1, .5)))),
                      family = "nbinomial", data = temp, 
                      control.fixed = control.fixed(mean=0, prec=0.001),
                      control.predictor = list(compute = TRUE), 
                      control.compute = list(config = TRUE, dic=T, cpo=T))
    
    m_pct_pos <-inla(formula=positives~value+pct_age65plus+offset(log(all))+ 
                       f(spatial_unit, model = "bym", 
                         graph = inla_adj,
                         hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                    prec.spatial=list(prior="loggamma", param=c(1, .5)))),
                     family = "nbinomial", data = temp, 
                     control.fixed = control.fixed(mean=0, prec=0.001),
                     control.predictor = list(compute = TRUE), 
                     control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc <-inla(formula=positives~value+pct_age65plus+offset(log(total_pop))+ 
                      f(spatial_unit, model = "bym", 
                        graph = inla_adj,
                        hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                   prec.spatial=list(prior="loggamma", param=c(1, .5)))),
                    family = "nbinomial", data = temp, 
                    control.fixed = control.fixed(mean=0, prec=0.001),
                    control.predictor = list(compute = TRUE), 
                    control.compute = list(config = TRUE, dic=T, cpo=T))
    m_deaths_pc <-inla(formula=deaths~value+pct_age65plus+offset(log(total_pop))+ 
                         f(spatial_unit, model = "bym", 
                           graph = inla_adj,
                           hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                      prec.spatial=list(prior="loggamma", param=c(1, .5)))),
                       family = "nbinomial", data = temp, 
                       control.fixed = control.fixed(mean=0, prec=0.001),
                       control.predictor = list(compute = TRUE), 
                       control.compute = list(config = TRUE, dic=T, cpo=T))
    models<-list(m_tests_pc=m_tests_pc, m_pct_pos=m_pct_pos, m_pos_pc=m_pos_pc, m_deaths_pc=m_deaths_pc)
    models
  })
  # re-fit with default priors for comparison below
  table1_models_defaultpriors<-map(last_date, function(temp){
    city_var<-unique(temp$city)
    var<-unique(temp$var)
    print(paste0(city_var, "; ", var))
    inla_adj<-case_when(
      city_var=="Chicago" ~ "Data/nb_inla_chi.adj",
      city_var=="New York City" ~ "Data/nb_inla_nyc.adj",
      city_var=="Philadelphia" ~ "Data/nb_inla_phl.adj"
    )
    temp$value<-as.numeric(scale(temp$value, center=T, scale=T))
    temp$spatial_unit<-as.numeric(as.factor(temp$GEOID))
    m_tests_pc <-inla(formula=all~value+pct_age65plus+offset(log(total_pop))+
                        f(spatial_unit, model = "bym", 
                          graph = inla_adj),
                      family = "nbinomial", data = temp, 
                      control.predictor = list(compute = TRUE), 
                      control.compute = list(config = TRUE, dic=T, cpo=T))
    
    m_pct_pos <-inla(formula=positives~value+pct_age65plus+offset(log(all))+ 
                       f(spatial_unit, model = "bym", 
                         graph = inla_adj),
                     family = "nbinomial", data = temp, 
                     control.predictor = list(compute = TRUE), 
                     control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc <-inla(formula=positives~value+pct_age65plus+offset(log(total_pop))+ 
                      f(spatial_unit, model = "bym", 
                        graph = inla_adj),
                    family = "nbinomial", data = temp, 
                    control.predictor = list(compute = TRUE), 
                    control.compute = list(config = TRUE, dic=T, cpo=T))
    m_deaths_pc <-inla(formula=deaths~value+pct_age65plus+offset(log(total_pop))+ 
                         f(spatial_unit, model = "bym", 
                           graph = inla_adj),
                       family = "nbinomial", data = temp, 
                       control.predictor = list(compute = TRUE), 
                       control.compute = list(config = TRUE, dic=T, cpo=T))
    models<-list(m_tests_pc=m_tests_pc, m_pct_pos=m_pct_pos, m_pos_pc=m_pos_pc, m_deaths_pc=m_deaths_pc)
    models
  })
  
  # spatio-temporal models
  # first, explore the best time parametrization
  # 7 options: dummys for month; linear; quadratic; 4 potential non-parametric specifications
  #temp<-bycity[[1]]
  results_monthly_all_types<-map(bycity, function(temp){
    city_var<-unique(temp$city)
    print(city_var)
    inla_adj<-case_when(
      city_var=="Chicago" ~ "Data/nb_inla_chi.adj",
      city_var=="New York City" ~ "Data/nb_inla_nyc.adj",
      city_var=="Philadelphia" ~ "Data/nb_inla_phl.adj"
    )
    # re-scaling SVI 
    temp$svi<-as.numeric(scale(temp$svi, scale=T, center=T))
    # linear time and quadratic
    temp$month_lin<-temp$month-3
    temp$month_quad<-temp$month_lin^2
    # preparing unique variables for INLA (starting from 1 onwards)
    temp$month2<-temp$month-2
    temp$month3<-temp$month2
    temp$month4<-temp$month2
    temp$spatial_unit<-as.numeric(as.factor(temp$GEOID))
    temp$spatial_unit2<-temp$spatial_unit
    temp$spatial_time_id<-1:nrow(temp)
    
    m_tests_pc_param_dummy <-inla(formula=all_new~as.factor(month)+
                                    f(spatial_unit, model = "bym", 
                                      graph = inla_adj,
                                      hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                                 prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                    f(spatial_unit2, month2, model = "iid",
                                      hyper=list(prec = list(param = c(0.001, 0.001)))),
                                  control.fixed = control.fixed(mean=0, prec=0.001),
                                  family = "nbinomial", data = temp, 
                                  E=total_pop,
                                  control.predictor = list(compute = TRUE), 
                                  control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_param_linear <-inla(formula=all_new~month_lin+
                                     f(spatial_unit, model = "bym", 
                                       graph = inla_adj,
                                       hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                                  prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                     f(spatial_unit2, month2, model = "iid",
                                       hyper=list(prec = list(param = c(0.001, 0.001)))),
                                   control.fixed = control.fixed(mean=0, prec=0.001),
                                   family = "nbinomial", data = temp, 
                                   E=total_pop,
                                   control.predictor = list(compute = TRUE), 
                                   control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_param_quad <-inla(formula=all_new~month_lin+svi*month_quad+
                                   f(spatial_unit, model = "bym", 
                                     graph = inla_adj,
                                     hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                                prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                   f(spatial_unit2, month2, model = "iid",
                                     hyper=list(prec = list(param = c(0.001, 0.001)))),
                                 control.fixed = control.fixed(mean=0, prec=0.001),
                                 family = "nbinomial", data = temp, 
                                 E=total_pop,
                                 control.predictor = list(compute = TRUE), 
                                 control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_noparam1 <-inla(formula=all_new~1+
                                 f(spatial_unit, model = "bym", 
                                   graph = inla_adj,
                                   hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                              prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                 f(month2, model = "rw2",
                                   hyper=list(prec = list(param = c(0.001, 0.001))))+
                                 f(month3, model = "iid",
                                   hyper=list(prec = list(param = c(0.001, 0.001))))+
                                 f(spatial_time_id, model = "iid",
                                   hyper=list(prec = list(param = c(0.001, 0.001)))),
                               control.fixed = control.fixed(mean=0, prec=0.001),
                               family = "nbinomial", data = temp, 
                               E=total_pop,
                               control.predictor = list(compute = TRUE), 
                               control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_noparam2 <-inla(formula=all_new~1+
                                 f(spatial_unit, model = "bym", 
                                   graph = inla_adj,
                                   hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                              prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                 f(month2, model = "rw2",
                                   hyper=list(prec = list(param = c(0.001, 0.001))))+
                                 f(month3, model = "iid",
                                   hyper=list(prec = list(param = c(0.001, 0.001))))+
                                 f(spatial_unit2, model = "iid",
                                   group=month4, control.group=list(model="rw2"),
                                   hyper=list(prec = list(param = c(0.001, 0.001)))),
                               control.fixed = control.fixed(mean=0, prec=0.001),
                               family = "nbinomial", data = temp, 
                               E=total_pop,
                               control.predictor = list(compute = TRUE), 
                               control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_noparam3 <-inla(formula=all_new~1+
                                 f(spatial_unit, model = "bym", 
                                   graph = inla_adj,
                                   hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                              prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                 f(month2, model = "rw2",
                                   hyper=list(prec = list(param = c(0.001, 0.001))))+
                                 f(month3, model = "iid",
                                   hyper=list(prec = list(param = c(0.001, 0.001))))+
                                 f(month4, model = "iid",
                                   group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                                   hyper=list(prec = list(param = c(0.001, 0.001)))),
                               control.fixed = control.fixed(mean=0, prec=0.001),
                               family = "nbinomial", data = temp, 
                               E=total_pop,
                               control.predictor = list(compute = TRUE), 
                               control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_noparam4 <-inla(formula=all_new~1+
                                 f(spatial_unit, model = "bym", 
                                   graph = inla_adj,
                                   hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                              prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                 f(month2, model = "rw2",
                                   hyper=list(prec = list(param = c(0.001, 0.001))))+
                                 f(month3, model = "iid",
                                   hyper=list(prec = list(param = c(0.001, 0.001))))+
                                 f(spatial_unit2, model = "besag",graph=inla_adj,
                                   group=month4, control.group=list(model="rw2"),
                                   hyper=list(prec = list(param = c(0.001, 0.001)))),
                               control.fixed = control.fixed(mean=0, prec=0.001),
                               family = "nbinomial", data = temp, 
                               E=total_pop,
                               control.predictor = list(compute = TRUE), 
                               control.compute = list(config = TRUE, dic=T, cpo=T))
    tests_pc_model_list<-list(param_dummy=m_tests_pc_param_dummy,
                              param_linear=m_tests_pc_param_linear,
                              param_quad=m_tests_pc_param_quad,
                              no_param1=m_tests_pc_noparam1,
                              no_param2=m_tests_pc_noparam2,
                              no_param3=m_tests_pc_noparam3,
                              no_param4=m_tests_pc_noparam4)
    
    m_pct_pos_param_dummy <-inla(formula=positives_new~as.factor(month)+
                                   f(spatial_unit, model = "bym", 
                                     graph = inla_adj,
                                     hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                                prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                   f(spatial_unit2, month2, model = "iid",
                                     hyper=list(prec = list(param = c(0.001, 0.001)))),
                                 control.fixed = control.fixed(mean=0, prec=0.001),
                                 family = "nbinomial", data = temp, 
                                 E=all_new,
                                 control.predictor = list(compute = TRUE), 
                                 control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_param_linear <-inla(formula=positives_new~month_lin+
                                    f(spatial_unit, model = "bym", 
                                      graph = inla_adj,
                                      hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                                 prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                    f(spatial_unit2, month2, model = "iid",
                                      hyper=list(prec = list(param = c(0.001, 0.001)))),
                                  control.fixed = control.fixed(mean=0, prec=0.001),
                                  family = "nbinomial", data = temp, 
                                  E=all_new,
                                  control.predictor = list(compute = TRUE), 
                                  control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_param_quad <-inla(formula=positives_new~month_lin+month_quad+
                                  f(spatial_unit, model = "bym", 
                                    graph = inla_adj,
                                    hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                               prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                  f(spatial_unit2, month2, model = "iid",
                                    hyper=list(prec = list(param = c(0.001, 0.001)))),
                                control.fixed = control.fixed(mean=0, prec=0.001),
                                family = "nbinomial", data = temp, 
                                E=all_new,
                                control.predictor = list(compute = TRUE), 
                                control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_noparam1 <-inla(formula=positives_new~1+
                                f(spatial_unit, model = "bym", 
                                  graph = inla_adj,
                                  hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                             prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                f(month2, model = "rw2",
                                  hyper=list(prec = list(param = c(0.001, 0.001))))+
                                f(month3, model = "iid",
                                  hyper=list(prec = list(param = c(0.001, 0.001))))+
                                f(spatial_time_id, model = "iid",
                                  hyper=list(prec = list(param = c(0.001, 0.001)))),
                              control.fixed = control.fixed(mean=0, prec=0.001),
                              family = "nbinomial", data = temp, 
                              E=all_new,
                              control.predictor = list(compute = TRUE), 
                              control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_noparam2 <-inla(formula=positives_new~1+
                                f(spatial_unit, model = "bym", 
                                  graph = inla_adj,
                                  hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                             prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                f(month2, model = "rw2",
                                  hyper=list(prec = list(param = c(0.001, 0.001))))+
                                f(month3, model = "iid",
                                  hyper=list(prec = list(param = c(0.001, 0.001))))+
                                f(spatial_unit2, model = "iid",
                                  group=month4, control.group=list(model="rw2"),
                                  hyper=list(prec = list(param = c(0.001, 0.001)))),
                              control.fixed = control.fixed(mean=0, prec=0.001),
                              family = "nbinomial", data = temp, 
                              E=all_new,
                              control.predictor = list(compute = TRUE), 
                              control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_noparam3 <-inla(formula=positives_new~1+
                                f(spatial_unit, model = "bym", 
                                  graph = inla_adj,
                                  hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                             prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                f(month2, model = "rw2",
                                  hyper=list(prec = list(param = c(0.001, 0.001))))+
                                f(month3, model = "iid",
                                  hyper=list(prec = list(param = c(0.001, 0.001))))+
                                f(month4, model = "iid",
                                  group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                                  hyper=list(prec = list(param = c(0.001, 0.001)))),
                              control.fixed = control.fixed(mean=0, prec=0.001),
                              family = "nbinomial", data = temp, 
                              E=all_new,
                              control.predictor = list(compute = TRUE), 
                              control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_noparam4 <-inla(formula=positives_new~1+
                                f(spatial_unit, model = "bym", 
                                  graph = inla_adj,
                                  hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                             prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                f(month2, model = "rw2",
                                  hyper=list(prec = list(param = c(0.001, 0.001))))+
                                f(month3, model = "iid",
                                  hyper=list(prec = list(param = c(0.001, 0.001))))+
                                f(spatial_unit2, model = "besag",graph=inla_adj,
                                  group=month4, control.group=list(model="rw2"),
                                  hyper=list(prec = list(param = c(0.001, 0.001)))),
                              control.fixed = control.fixed(mean=0, prec=0.001),
                              family = "nbinomial", data = temp, 
                              E=all_new,
                              control.predictor = list(compute = TRUE), 
                              control.compute = list(config = TRUE, dic=T, cpo=T))
    pct_pos_model_list<-list(param_dummy=m_pct_pos_param_dummy,
                             param_linear=m_pct_pos_param_linear,
                             param_quad=m_pct_pos_param_quad,
                             no_param1=m_pct_pos_noparam1,
                             no_param2=m_pct_pos_noparam2,
                             no_param3=m_pct_pos_noparam3,
                             no_param4=m_pct_pos_noparam4)
    
    m_pos_pc_param_dummy <-inla(formula=positives_new~as.factor(month)+
                                  f(spatial_unit, model = "bym", 
                                    graph = inla_adj,
                                    hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                               prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                  f(spatial_unit2, month2, model = "iid",
                                    hyper=list(prec = list(param = c(0.001, 0.001)))),
                                control.fixed = control.fixed(mean=0, prec=0.001),
                                family = "nbinomial", data = temp, 
                                E=total_pop,
                                control.predictor = list(compute = TRUE), 
                                control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_param_linear <-inla(formula=positives_new~month_lin+
                                   f(spatial_unit, model = "bym", 
                                     graph = inla_adj,
                                     hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                                prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                   f(spatial_unit2, month2, model = "iid",
                                     hyper=list(prec = list(param = c(0.001, 0.001)))),
                                 control.fixed = control.fixed(mean=0, prec=0.001),
                                 family = "nbinomial", data = temp, 
                                 E=total_pop,
                                 control.predictor = list(compute = TRUE), 
                                 control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_param_quad <-inla(formula=positives_new~month_lin+month_quad+
                                 f(spatial_unit, model = "bym", 
                                   graph = inla_adj,
                                   hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                              prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                                 f(spatial_unit2, month2, model = "iid",
                                   hyper=list(prec = list(param = c(0.001, 0.001)))),
                               control.fixed = control.fixed(mean=0, prec=0.001),
                               family = "nbinomial", data = temp, 
                               E=total_pop,
                               control.predictor = list(compute = TRUE), 
                               control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_noparam1 <-inla(formula=positives_new~1+
                               f(spatial_unit, model = "bym", 
                                 graph = inla_adj,
                                 hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                            prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                               f(month2, model = "rw2",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(month3, model = "iid",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(spatial_time_id, model = "iid",
                                 hyper=list(prec = list(param = c(0.001, 0.001)))),
                             control.fixed = control.fixed(mean=0, prec=0.001),
                             family = "nbinomial", data = temp, 
                             E=total_pop,
                             control.predictor = list(compute = TRUE), 
                             control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_noparam2 <-inla(formula=positives_new~1+
                               f(spatial_unit, model = "bym", 
                                 graph = inla_adj,
                                 hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                            prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                               f(month2, model = "rw2",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(month3, model = "iid",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(spatial_unit2, model = "iid",
                                 group=month4, control.group=list(model="rw2"),
                                 hyper=list(prec = list(param = c(0.001, 0.001)))),
                             control.fixed = control.fixed(mean=0, prec=0.001),
                             family = "nbinomial", data = temp, 
                             E=total_pop,
                             control.predictor = list(compute = TRUE), 
                             control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_noparam3 <-inla(formula=positives_new~1+
                               f(spatial_unit, model = "bym", 
                                 graph = inla_adj,
                                 hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                            prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                               f(month2, model = "rw2",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(month3, model = "iid",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(month4, model = "iid",
                                 group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                                 hyper=list(prec = list(param = c(0.001, 0.001)))),
                             control.fixed = control.fixed(mean=0, prec=0.001),
                             family = "nbinomial", data = temp, 
                             E=total_pop,
                             control.predictor = list(compute = TRUE), 
                             control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_noparam4 <-inla(formula=positives_new~1+
                               f(spatial_unit, model = "bym", 
                                 graph = inla_adj,
                                 hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                            prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                               f(month2, model = "rw2",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(month3, model = "iid",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(spatial_unit2, model = "besag",graph=inla_adj,
                                 group=month4, control.group=list(model="rw2"),
                                 hyper=list(prec = list(param = c(0.001, 0.001)))),
                             control.fixed = control.fixed(mean=0, prec=0.001),
                             family = "nbinomial", data = temp, 
                             E=total_pop,
                             control.predictor = list(compute = TRUE), 
                             control.compute = list(config = TRUE, dic=T, cpo=T))
    pos_pc_model_list<-list(param_dummy=m_pos_pc_param_dummy,
                            param_linear=m_pos_pc_param_linear,
                            param_quad=m_pos_pc_param_quad,
                            no_param1=m_pos_pc_noparam1,
                            no_param2=m_pos_pc_noparam2,
                            no_param3=m_pos_pc_noparam3,
                            no_param4=m_pos_pc_noparam4)
    
    all_models<-list(tests_pc=tests_pc_model_list,
                     pct_pos=pct_pos_model_list,
                     pos_pc=pos_pc_model_list,
                     city=city_var)
    all_models
  })
  # get DICs
  dics<-map_dfr(results_monthly_all_types, function(model_list_city){
    city<-model_list_city$city
    model_list_city<-model_list_city[-length(model_list_city)]
    map2_dfr(model_list_city, c("tests_pc", "pct_pos", "pos_pc"),function(model_list, type){
      map_dfr(model_list, function(model){
        data.frame(dic=model$dic$dic)
      }) %>% 
        mutate(model=0:6,
               type=type,
               city=city)
    })
  }) %>% 
    mutate(model=factor(model, levels=0:6,
                        labels=c("Parametric", "Parametric Linear", 
                                 "Parametric Quadratic",
                                 "Non-Parametric I",
                                 "Non-Parametric II",
                                 "Non-Parametric III",
                                 "Non-Parametric IV")),
           type=factor(type, levels=c("tests_pc", "pct_pos", "pos_pc"),
                       labels=c("Testing", "Positivity", "Incidence")))
  
  ggplot(dics, aes(x=model, y=dic)) +
    geom_point(pch=21, color="black", fill="gray", size=3)+
    facet_wrap(~city+type, scales="free_y") +
    labs(x="", y="DIC")+
    theme_bw()+
    theme(legend.position = "bottom",
          axis.text.y=element_text(color="black", size=14),
          axis.text.x=element_text(color="black", size=12, 
                                   angle=45, vjust=1, hjust=1),
          axis.title=element_text(color="black", size=14, face="bold"),
          legend.text=element_text(color="black", size=14),
          strip.text=element_text(color="black", size=16, face="bold"),
          strip.background = element_blank(),
          plot.title=element_text(color="black", size=16, face="bold"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
  ggsave("results/appendixFigureDIC_TimeParametriation.pdf", width=15, height=10)
  
  # Non-Parametric Type III looks like the best fit in 8 of the 9 city/outcome combinations, and second best for the last one.
  # We'll do that modeling of time, and do a sensitivity analysis for NYC-Positivity modeling the trend with Non-Parametric Type I
  # Now lets test four different parametrizations of the change of the effect of SVI
  results_monthly_np3_times<-map(bycity, function(temp){
    city_var<-unique(temp$city)
    print(city_var)
    inla_adj<-case_when(
      city_var=="Chicago" ~ "Data/nb_inla_chi.adj",
      city_var=="New York City" ~ "Data/nb_inla_nyc.adj",
      city_var=="Philadelphia" ~ "Data/nb_inla_phl.adj"
    )
    # re-scaling SVI 
    temp$svi<-as.numeric(scale(temp$svi, scale=T, center=T))
    # linear time and quadratic
    temp$month_lin<-temp$month-3
    temp$month_quad<-temp$month_lin^2
    temp$month_spline<-ifelse(temp$month_lin<=2, 0, temp$month_lin-2)
    # preparing unique variables for INLA (starting from 1 onwards)
    temp$month2<-temp$month-2
    temp$month3<-temp$month2
    temp$month4<-temp$month2
    temp$spatial_unit<-as.numeric(as.factor(temp$GEOID))
    temp$spatial_unit2<-temp$spatial_unit
    temp$spatial_time_id<-1:nrow(temp)
    
    m_tests_pc_dummy <-inla(formula=all_new~svi+svi:as.factor(month)+pct_age65plus+
                              f(spatial_unit, model = "bym", 
                                graph = inla_adj,
                                hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                           prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                              f(month2, model = "rw2",
                                hyper=list(prec = list(param = c(0.001, 0.001))))+
                              f(month3, model = "iid",
                                hyper=list(prec = list(param = c(0.001, 0.001))))+
                              f(month4, model = "iid",
                                group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                                hyper=list(prec = list(param = c(0.001, 0.001)))),
                            control.fixed = control.fixed(mean=0, prec=0.001),
                            lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                       `svi:as.factor(month)4`=c(0, 1, 0, 0, 0, 0, 0),
                                                       `svi:as.factor(month)5`=c(0, 0, 1, 0, 0, 0, 0),
                                                       `svi:as.factor(month)6`=c(0, 0, 0, 1, 0, 0, 0),
                                                       `svi:as.factor(month)7`=c(0, 0, 0, 0, 1, 0, 0),
                                                       `svi:as.factor(month)8`=c(0, 0, 0, 0, 0, 1, 0),
                                                       `svi:as.factor(month)9`=c(0, 0, 0, 0, 0, 0, 1)),
                            family = "nbinomial", data = temp, 
                            E=total_pop,
                            control.predictor = list(compute = TRUE), 
                            control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_linear <-inla(formula=all_new~svi+svi:month_lin+pct_age65plus+
                               f(spatial_unit, model = "bym", 
                                 graph = inla_adj,
                                 hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                            prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                               f(month2, model = "rw2",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(month3, model = "iid",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(month4, model = "iid",
                                 group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                                 hyper=list(prec = list(param = c(0.001, 0.001)))),
                             control.fixed = control.fixed(mean=0, prec=0.001),
                             lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                        `svi:month_lin`=c(0:6)),
                             family = "nbinomial", data = temp, 
                             E=total_pop,
                             control.predictor = list(compute = TRUE), 
                             control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_quad <-inla(formula=all_new~svi+svi:month_lin+svi:month_quad+pct_age65plus+
                             f(spatial_unit, model = "bym", 
                               graph = inla_adj,
                               hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                          prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                             f(month2, model = "rw2",
                               hyper=list(prec = list(param = c(0.001, 0.001))))+
                             f(month3, model = "iid",
                               hyper=list(prec = list(param = c(0.001, 0.001))))+
                             f(month4, model = "iid",
                               group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                               hyper=list(prec = list(param = c(0.001, 0.001)))),
                           control.fixed = control.fixed(mean=0, prec=0.001),
                           lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                      `svi:month_lin`=c(0:6),
                                                      `svi:month_quad`=c(0:6)^2),
                           family = "nbinomial", data = temp, 
                           E=total_pop,
                           control.predictor = list(compute = TRUE), 
                           control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_spline <-inla(formula=all_new~svi+svi:month_lin+svi:month_spline+pct_age65plus+
                               f(spatial_unit, model = "bym", 
                                 graph = inla_adj,
                                 hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                            prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                               f(month2, model = "rw2",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(month3, model = "iid",
                                 hyper=list(prec = list(param = c(0.001, 0.001))))+
                               f(month4, model = "iid",
                                 group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                                 hyper=list(prec = list(param = c(0.001, 0.001)))),
                             control.fixed = control.fixed(mean=0, prec=0.001),
                             lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                        `svi:month_lin`=c(0:6),
                                                        `svi:month_spline`=c(0, 0, 0, 1, 2, 3, 4)),
                             family = "nbinomial", data = temp, 
                             E=total_pop,
                             control.predictor = list(compute = TRUE), 
                             control.compute = list(config = TRUE, dic=T, cpo=T))
    tests_pc_model_list<-list(dummy=m_tests_pc_dummy,
                              linear=m_tests_pc_linear,
                              quad=m_tests_pc_quad,
                              spline=m_tests_pc_spline)
    
    
    m_pct_pos_dummy <-inla(formula=positives_new~svi+svi:as.factor(month)+pct_age65plus+
                             f(spatial_unit, model = "bym", 
                               graph = inla_adj,
                               hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                          prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                             f(month2, model = "rw2",
                               hyper=list(prec = list(param = c(0.001, 0.001))))+
                             f(month3, model = "iid",
                               hyper=list(prec = list(param = c(0.001, 0.001))))+
                             f(month4, model = "iid",
                               group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                               hyper=list(prec = list(param = c(0.001, 0.001)))),
                           control.fixed = control.fixed(mean=0, prec=0.001),
                           lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                      `svi:as.factor(month)4`=c(0, 1, 0, 0, 0, 0, 0),
                                                      `svi:as.factor(month)5`=c(0, 0, 1, 0, 0, 0, 0),
                                                      `svi:as.factor(month)6`=c(0, 0, 0, 1, 0, 0, 0),
                                                      `svi:as.factor(month)7`=c(0, 0, 0, 0, 1, 0, 0),
                                                      `svi:as.factor(month)8`=c(0, 0, 0, 0, 0, 1, 0),
                                                      `svi:as.factor(month)9`=c(0, 0, 0, 0, 0, 0, 1)),
                           family = "nbinomial", data = temp, 
                           E=all_new,
                           control.predictor = list(compute = TRUE), 
                           control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_linear <-inla(formula=positives_new~svi+svi:month_lin+pct_age65plus+
                              f(spatial_unit, model = "bym", 
                                graph = inla_adj,
                                hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                           prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                              f(month2, model = "rw2",
                                hyper=list(prec = list(param = c(0.001, 0.001))))+
                              f(month3, model = "iid",
                                hyper=list(prec = list(param = c(0.001, 0.001))))+
                              f(month4, model = "iid",
                                group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                                hyper=list(prec = list(param = c(0.001, 0.001)))),
                            control.fixed = control.fixed(mean=0, prec=0.001),
                            lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                       `svi:month_lin`=c(0:6)),
                            family = "nbinomial", data = temp, 
                            E=all_new,
                            control.predictor = list(compute = TRUE), 
                            control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_quad <-inla(formula=positives_new~svi+svi:month_lin+svi:month_quad+pct_age65plus+
                            f(spatial_unit, model = "bym", 
                              graph = inla_adj,
                              hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                         prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                            f(month2, model = "rw2",
                              hyper=list(prec = list(param = c(0.001, 0.001))))+
                            f(month3, model = "iid",
                              hyper=list(prec = list(param = c(0.001, 0.001))))+
                            f(month4, model = "iid",
                              group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                              hyper=list(prec = list(param = c(0.001, 0.001)))),
                          control.fixed = control.fixed(mean=0, prec=0.001),
                          lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                     `svi:month_lin`=c(0:6),
                                                     `svi:month_quad`=c(0:6)^2),
                          family = "nbinomial", data = temp, 
                          E=all_new,
                          control.predictor = list(compute = TRUE), 
                          control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_spline <-inla(formula=positives_new~svi+svi:month_lin+svi:month_spline+pct_age65plus+
                              f(spatial_unit, model = "bym", 
                                graph = inla_adj,
                                hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                           prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                              f(month2, model = "rw2",
                                hyper=list(prec = list(param = c(0.001, 0.001))))+
                              f(month3, model = "iid",
                                hyper=list(prec = list(param = c(0.001, 0.001))))+
                              f(month4, model = "iid",
                                group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                                hyper=list(prec = list(param = c(0.001, 0.001)))),
                            control.fixed = control.fixed(mean=0, prec=0.001),
                            lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                       `svi:month_lin`=c(0:6),
                                                       `svi:month_spline`=c(0, 0, 0, 1, 2, 3, 4)),
                            family = "nbinomial", data = temp, 
                            E=all_new,
                            control.predictor = list(compute = TRUE), 
                            control.compute = list(config = TRUE, dic=T, cpo=T))
    pct_pos_model_list<-list(dummy=m_pct_pos_dummy,
                             linear=m_pct_pos_linear,
                             quad=m_pct_pos_quad,
                             spline=m_pct_pos_spline)
    
    m_pos_pc_dummy <-inla(formula=positives_new~svi+svi:as.factor(month)+pct_age65plus+
                            f(spatial_unit, model = "bym", 
                              graph = inla_adj,
                              hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                         prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                            f(month2, model = "rw2",
                              hyper=list(prec = list(param = c(0.001, 0.001))))+
                            f(month3, model = "iid",
                              hyper=list(prec = list(param = c(0.001, 0.001))))+
                            f(month4, model = "iid",
                              group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                              hyper=list(prec = list(param = c(0.001, 0.001)))),
                          control.fixed = control.fixed(mean=0, prec=0.001),
                          lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                     `svi:as.factor(month)4`=c(0, 1, 0, 0, 0, 0, 0),
                                                     `svi:as.factor(month)5`=c(0, 0, 1, 0, 0, 0, 0),
                                                     `svi:as.factor(month)6`=c(0, 0, 0, 1, 0, 0, 0),
                                                     `svi:as.factor(month)7`=c(0, 0, 0, 0, 1, 0, 0),
                                                     `svi:as.factor(month)8`=c(0, 0, 0, 0, 0, 1, 0),
                                                     `svi:as.factor(month)9`=c(0, 0, 0, 0, 0, 0, 1)),
                          family = "nbinomial", data = temp, 
                          E=total_pop,
                          control.predictor = list(compute = TRUE), 
                          control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_linear <-inla(formula=positives_new~svi+svi:month_lin+pct_age65plus+
                             f(spatial_unit, model = "bym", 
                               graph = inla_adj,
                               hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                          prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                             f(month2, model = "rw2",
                               hyper=list(prec = list(param = c(0.001, 0.001))))+
                             f(month3, model = "iid",
                               hyper=list(prec = list(param = c(0.001, 0.001))))+
                             f(month4, model = "iid",
                               group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                               hyper=list(prec = list(param = c(0.001, 0.001)))),
                           control.fixed = control.fixed(mean=0, prec=0.001),
                           lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                      `svi:month_lin`=c(0:6)),
                           family = "nbinomial", data = temp, 
                           E=total_pop,
                           control.predictor = list(compute = TRUE), 
                           control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_quad <-inla(formula=positives_new~svi+svi:month_lin+svi:month_quad+pct_age65plus+
                           f(spatial_unit, model = "bym", 
                             graph = inla_adj,
                             hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                        prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                           f(month2, model = "rw2",
                             hyper=list(prec = list(param = c(0.001, 0.001))))+
                           f(month3, model = "iid",
                             hyper=list(prec = list(param = c(0.001, 0.001))))+
                           f(month4, model = "iid",
                             group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                             hyper=list(prec = list(param = c(0.001, 0.001)))),
                         control.fixed = control.fixed(mean=0, prec=0.001),
                         lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                    `svi:month_lin`=c(0:6),
                                                    `svi:month_quad`=c(0:6)^2),
                         family = "nbinomial", data = temp, 
                         E=total_pop,
                         control.predictor = list(compute = TRUE), 
                         control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_spline <-inla(formula=positives_new~svi+svi:month_lin+svi:month_spline+pct_age65plus+
                             f(spatial_unit, model = "bym", 
                               graph = inla_adj,
                               hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                          prec.spatial=list(prior="loggamma", param=c(1, .5))))+
                             f(month2, model = "rw2",
                               hyper=list(prec = list(param = c(0.001, 0.001))))+
                             f(month3, model = "iid",
                               hyper=list(prec = list(param = c(0.001, 0.001))))+
                             f(month4, model = "iid",
                               group=spatial_unit2, control.group=list(model="besag", graph=inla_adj),
                               hyper=list(prec = list(param = c(0.001, 0.001)))),
                           control.fixed = control.fixed(mean=0, prec=0.001),
                           lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                      `svi:month_lin`=c(0:6),
                                                      `svi:month_spline`=c(0, 0, 0, 1, 2, 3, 4)),
                           family = "nbinomial", data = temp, 
                           E=total_pop,
                           control.predictor = list(compute = TRUE), 
                           control.compute = list(config = TRUE, dic=T, cpo=T))
    pos_pc_model_list<-list(dummy=m_pos_pc_dummy,
                            linear=m_pos_pc_linear,
                            quad=m_pos_pc_quad,
                            spline=m_pos_pc_spline)
    
    all_models<-list(tests_pc=tests_pc_model_list,
                     pct_pos=pct_pos_model_list,
                     pos_pc=pos_pc_model_list,
                     city=city_var)
    all_models
  })
  # repeat models with default priors for comparison below
  results_monthly_np3_times_defaultpriors<-map(bycity, function(temp){
    city_var<-unique(temp$city)
    print(city_var)
    inla_adj<-case_when(
      city_var=="Chicago" ~ "Data/nb_inla_chi.adj",
      city_var=="New York City" ~ "Data/nb_inla_nyc.adj",
      city_var=="Philadelphia" ~ "Data/nb_inla_phl.adj"
    )
    # re-scaling SVI 
    temp$svi<-as.numeric(scale(temp$svi, scale=T, center=T))
    # linear time and quadratic
    temp$month_lin<-temp$month-3
    temp$month_quad<-temp$month_lin^2
    temp$month_spline<-ifelse(temp$month_lin<=2, 0, temp$month_lin-2)
    # preparing unique variables for INLA (starting from 1 onwards)
    temp$month2<-temp$month-2
    temp$month3<-temp$month2
    temp$month4<-temp$month2
    temp$spatial_unit<-as.numeric(as.factor(temp$GEOID))
    temp$spatial_unit2<-temp$spatial_unit
    temp$spatial_time_id<-1:nrow(temp)
    
    m_tests_pc_dummy <-inla(formula=all_new~svi+svi:as.factor(month)+pct_age65plus+
                              f(spatial_unit, model = "bym", 
                                graph = inla_adj)+
                              f(month2, model = "rw2")+
                              f(month3, model = "iid")+
                              f(month4, model = "iid",
                                group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                            lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                       `svi:as.factor(month)4`=c(0, 1, 0, 0, 0, 0, 0),
                                                       `svi:as.factor(month)5`=c(0, 0, 1, 0, 0, 0, 0),
                                                       `svi:as.factor(month)6`=c(0, 0, 0, 1, 0, 0, 0),
                                                       `svi:as.factor(month)7`=c(0, 0, 0, 0, 1, 0, 0),
                                                       `svi:as.factor(month)8`=c(0, 0, 0, 0, 0, 1, 0),
                                                       `svi:as.factor(month)9`=c(0, 0, 0, 0, 0, 0, 1)),
                            family = "nbinomial", data = temp, 
                            E=total_pop,
                            control.predictor = list(compute = TRUE), 
                            control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_linear <-inla(formula=all_new~svi+svi:month_lin+pct_age65plus+
                               f(spatial_unit, model = "bym", 
                                 graph = inla_adj)+
                               f(month2, model = "rw2")+
                               f(month3, model = "iid")+
                               f(month4, model = "iid",
                                 group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                             lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                        `svi:month_lin`=c(0:6)),
                             family = "nbinomial", data = temp, 
                             E=total_pop,
                             control.predictor = list(compute = TRUE), 
                             control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_quad <-inla(formula=all_new~svi+svi:month_lin+svi:month_quad+pct_age65plus+
                             f(spatial_unit, model = "bym", 
                               graph = inla_adj)+
                             f(month2, model = "rw2")+
                             f(month3, model = "iid")+
                             f(month4, model = "iid",
                               group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                           lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                      `svi:month_lin`=c(0:6),
                                                      `svi:month_quad`=c(0:6)^2),
                           family = "nbinomial", data = temp, 
                           E=total_pop,
                           control.predictor = list(compute = TRUE), 
                           control.compute = list(config = TRUE, dic=T, cpo=T))
    m_tests_pc_spline <-inla(formula=all_new~svi+svi:month_lin+svi:month_spline+pct_age65plus+
                               f(spatial_unit, model = "bym", 
                                 graph = inla_adj)+
                               f(month2, model = "rw2")+
                               f(month3, model = "iid")+
                               f(month4, model = "iid",
                                 group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                             lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                        `svi:month_lin`=c(0:6),
                                                        `svi:month_spline`=c(0, 0, 0, 1, 2, 3, 4)),
                             family = "nbinomial", data = temp, 
                             E=total_pop,
                             control.predictor = list(compute = TRUE), 
                             control.compute = list(config = TRUE, dic=T, cpo=T))
    tests_pc_model_list<-list(dummy=m_tests_pc_dummy,
                              linear=m_tests_pc_linear,
                              quad=m_tests_pc_quad,
                              spline=m_tests_pc_spline)
    
    m_pct_pos_dummy <-inla(formula=positives_new~svi+svi:as.factor(month)+pct_age65plus+
                             f(spatial_unit, model = "bym", 
                               graph = inla_adj)+
                             f(month2, model = "rw2")+
                             f(month3, model = "iid")+
                             f(month4, model = "iid",
                               group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                           lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                      `svi:as.factor(month)4`=c(0, 1, 0, 0, 0, 0, 0),
                                                      `svi:as.factor(month)5`=c(0, 0, 1, 0, 0, 0, 0),
                                                      `svi:as.factor(month)6`=c(0, 0, 0, 1, 0, 0, 0),
                                                      `svi:as.factor(month)7`=c(0, 0, 0, 0, 1, 0, 0),
                                                      `svi:as.factor(month)8`=c(0, 0, 0, 0, 0, 1, 0),
                                                      `svi:as.factor(month)9`=c(0, 0, 0, 0, 0, 0, 1)),
                           family = "nbinomial", data = temp, 
                           E=all_new,
                           control.predictor = list(compute = TRUE), 
                           control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_linear <-inla(formula=positives_new~svi+svi:month_lin+pct_age65plus+
                              f(spatial_unit, model = "bym", 
                                graph = inla_adj)+
                              f(month2, model = "rw2")+
                              f(month3, model = "iid")+
                              f(month4, model = "iid",
                                group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                            lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                       `svi:month_lin`=c(0:6)),
                            family = "nbinomial", data = temp, 
                            E=all_new,
                            control.predictor = list(compute = TRUE), 
                            control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_quad <-inla(formula=positives_new~svi+svi:month_lin+svi:month_quad+pct_age65plus+
                            f(spatial_unit, model = "bym", 
                              graph = inla_adj)+
                            f(month2, model = "rw2")+
                            f(month3, model = "iid")+
                            f(month4, model = "iid",
                              group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                          lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                     `svi:month_lin`=c(0:6),
                                                     `svi:month_quad`=c(0:6)^2),
                          family = "nbinomial", data = temp, 
                          E=all_new,
                          control.predictor = list(compute = TRUE), 
                          control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pct_pos_spline <-inla(formula=positives_new~svi+svi:month_lin+svi:month_spline+pct_age65plus+
                              f(spatial_unit, model = "bym", 
                                graph = inla_adj)+
                              f(month2, model = "rw2")+
                              f(month3, model = "iid")+
                              f(month4, model = "iid",
                                group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                            lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                       `svi:month_lin`=c(0:6),
                                                       `svi:month_spline`=c(0, 0, 0, 1, 2, 3, 4)),
                            family = "nbinomial", data = temp, 
                            E=all_new,
                            control.predictor = list(compute = TRUE), 
                            control.compute = list(config = TRUE, dic=T, cpo=T))
    pct_pos_model_list<-list(dummy=m_pct_pos_dummy,
                             linear=m_pct_pos_linear,
                             quad=m_pct_pos_quad,
                             spline=m_pct_pos_spline)
    
    m_pos_pc_dummy <-inla(formula=positives_new~svi+svi:as.factor(month)+pct_age65plus+
                            f(spatial_unit, model = "bym", 
                              graph = inla_adj)+
                            f(month2, model = "rw2")+
                            f(month3, model = "iid")+
                            f(month4, model = "iid",
                              group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                          lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                     `svi:as.factor(month)4`=c(0, 1, 0, 0, 0, 0, 0),
                                                     `svi:as.factor(month)5`=c(0, 0, 1, 0, 0, 0, 0),
                                                     `svi:as.factor(month)6`=c(0, 0, 0, 1, 0, 0, 0),
                                                     `svi:as.factor(month)7`=c(0, 0, 0, 0, 1, 0, 0),
                                                     `svi:as.factor(month)8`=c(0, 0, 0, 0, 0, 1, 0),
                                                     `svi:as.factor(month)9`=c(0, 0, 0, 0, 0, 0, 1)),
                          family = "nbinomial", data = temp, 
                          E=total_pop,
                          control.predictor = list(compute = TRUE), 
                          control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_linear <-inla(formula=positives_new~svi+svi:month_lin+pct_age65plus+
                             f(spatial_unit, model = "bym", 
                               graph = inla_adj)+
                             f(month2, model = "rw2")+
                             f(month3, model = "iid")+
                             f(month4, model = "iid",
                               group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                           lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                      `svi:month_lin`=c(0:6)),
                           family = "nbinomial", data = temp, 
                           E=total_pop,
                           control.predictor = list(compute = TRUE), 
                           control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_quad <-inla(formula=positives_new~svi+svi:month_lin+svi:month_quad+pct_age65plus+
                           f(spatial_unit, model = "bym", 
                             graph = inla_adj)+
                           f(month2, model = "rw2")+
                           f(month3, model = "iid")+
                           f(month4, model = "iid",
                             group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                         lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                    `svi:month_lin`=c(0:6),
                                                    `svi:month_quad`=c(0:6)^2),
                         family = "nbinomial", data = temp, 
                         E=total_pop,
                         control.predictor = list(compute = TRUE), 
                         control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc_spline <-inla(formula=positives_new~svi+svi:month_lin+svi:month_spline+pct_age65plus+
                             f(spatial_unit, model = "bym", 
                               graph = inla_adj)+
                             f(month2, model = "rw2")+
                             f(month3, model = "iid")+
                             f(month4, model = "iid",
                               group=spatial_unit2, control.group=list(model="besag", graph=inla_adj)),
                           lincomb=inla.make.lincombs(svi=c(1, 1, 1, 1, 1, 1, 1),
                                                      `svi:month_lin`=c(0:6),
                                                      `svi:month_spline`=c(0, 0, 0, 1, 2, 3, 4)),
                           family = "nbinomial", data = temp, 
                           E=total_pop,
                           control.predictor = list(compute = TRUE), 
                           control.compute = list(config = TRUE, dic=T, cpo=T))
    pos_pc_model_list<-list(dummy=m_pos_pc_dummy,
                            linear=m_pos_pc_linear,
                            quad=m_pos_pc_quad,
                            spline=m_pos_pc_spline)
    
    all_models<-list(tests_pc=tests_pc_model_list,
                     pct_pos=pct_pos_model_list,
                     pos_pc=pos_pc_model_list,
                     city=city_var)
    all_models
  })
  
  
  # save results
  save(table1_models,table1_models_defaultpriors,last_date_ids,
       results_monthly_all_types, results_monthly_np3_times,results_monthly_np3_times_defaultpriors,
       file="Results/Model_results.rdata")
} else {
  load("Results/model_results.rdata")  
}


# produce Table 1
table1<-map2_dfr(table1_models, 1:nrow(last_date_ids),function(model_list, id){
  id<-last_date_ids %>% ungroup() %>% slice(id)
  map2_dfr(model_list[1:4],
           c("tests_pc", "pct_pos", "pos_pc", "deaths_pc"),
           function(model, id){
             fixed<-model$summary.fixed
             fixed<-fixed[rownames(fixed)=="value",]
             data.frame(rr=exp(fixed$mean),
                        lci=exp(fixed$mean-1.96*fixed$sd),
                        uci=exp(fixed$mean+1.96*fixed$sd),
                        type=id)
           }) %>%
    mutate(coef=paste0(format(rr, digits=2, nsmall=2),
                       "(",
                       format(lci, digits=2, nsmall=2),
                       ";",
                       format(uci, digits=2, nsmall=2),
                       ")")) %>%
    select(type, coef) %>%
    spread(type, coef) %>%
    mutate(city=id$city, var=id$var)
})
table1<-table1 %>%
  select(city, var, tests_pc, pct_pos, pos_pc, deaths_pc)
table1 %>% View
fwrite(table1, "results/table1.csv")

table1_rrs_newpriors<-map2_dfr(table1_models, 1:nrow(last_date_ids),function(model_list, id){
  id<-last_date_ids %>% ungroup() %>% slice(id)
  map2_dfr(model_list[1:4],
           c("tests_pc", "pct_pos", "pos_pc", "deaths_pc"),
           function(model, id){
             fixed<-model$summary.fixed
             fixed<-fixed[rownames(fixed)=="value",]
             data.frame(rr_new=exp(fixed$mean),
                        lci_new=exp(fixed$mean-1.96*fixed$sd),
                        uci_new=exp(fixed$mean+1.96*fixed$sd),
                        type=id)
           }) %>%
    mutate(city=id$city, var=id$var)
})
table1_rrs_defaultpriors<-map2_dfr(table1_models, 1:nrow(last_date_ids),function(model_list, id){
  id<-last_date_ids %>% ungroup() %>% slice(id)
  map2_dfr(model_list[1:4],
           c("tests_pc", "pct_pos", "pos_pc", "deaths_pc"),
           function(model, id){
             fixed<-model$summary.fixed
             fixed<-fixed[rownames(fixed)=="value",]
             data.frame(rr_default=exp(fixed$mean),
                        lci_default=exp(fixed$mean-1.96*fixed$sd),
                        uci_default=exp(fixed$mean+1.96*fixed$sd),
                        type=id)
           }) %>%
    mutate(city=id$city, var=id$var)
})

# compare new vs default priors for table 1 results
comparison<-full_join(table1_rrs_newpriors, table1_rrs_defaultpriors) %>% 
  mutate(type2=case_when(
    type=="pct_pos" ~ "Positivity",
    type=="deaths_pc" ~ "Mortality",
    type=="pos_pc" ~ "Incidence",
    type=="tests_pc" ~ "Testing"),
    type2=factor(type2, levels=c("Testing", "Positivity", "Incidence" ,"Mortality")))
ggplot(comparison, aes(x=rr_default, y=rr_new, color=as.factor(var))) +
  geom_abline(intercept = 0, slope=1, lty=1)+
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = 1, lty=2)+
  geom_errorbar(aes(ymin=lci_new, ymax=uci_new))+
  geom_errorbarh(aes(xmin=lci_default, xmax=uci_default))+
  scale_color_discrete(name="Variable")+
  scale_x_continuous(trans="log", limits=c(0.7, 2.1), breaks=c(0.5, 0.75, 1, 1.5, 2))+
  scale_y_continuous(trans="log", limits=c(0.7, 2.1), breaks=c(0.5, 0.75, 1, 1.5, 2))+
  geom_point()+
  facet_grid(type2~city) +
  labs(x="RR (95% CrI) with default priors",
       y="RR (95% CrI) with alternative priors",
       title="Comparison of fixed effects using default and alternative priors",
       subtitle="Spatial Model (Table 1)")+
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_text(color="black", size=10),
        axis.text.x=element_text(color="black", size=10),
        axis.title=element_text(color="black", size=14, face="bold"),
        legend.text=element_text(color="black", size=14),
        legend.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=16, face="bold"),
        plot.subtitle=element_text(color="black", size=14, face="bold"),
        strip.text=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave("Results/AppendixFigurePriors_Spatial.pdf", width=10, height=7.5)



# figure 4
# first, extract monthly RRs
rateratios<-map_dfr(results_monthly_np3_times, function(model_list_city){
  city<-model_list_city$city
  model_list_city<-model_list_city[-length(model_list_city)]
  map2_dfr(model_list_city, c("tests_pc", "pct_pos", "pos_pc"),function(model_list, type){
    map2_dfr(model_list,c("Dummy", "Linear", "Quadratic", "Spline"), function(model, int_type){
      temp<-model$summary.lincomb.derived %>% as.data.frame
      temp$date<-3:9
      temp %>% 
        mutate(lci=exp(mean-1.96*sd),
               uci=exp(mean+1.96*sd),
               rr=exp(mean),
               model=int_type) %>% 
        select(date, rr, lci, uci, model)
    }) %>% 
      mutate(type=type,
             city=city)
  })
}) %>% 
  mutate(type2=case_when(
    type=="pct_pos" ~ "Positivity",
    type=="pos_pc" ~ "Incidence",
    type=="tests_pc" ~ "Testing"),
    type2=factor(type2, levels=c("Testing", "Positivity", "Incidence")))
# extract also for default priors
rateratios_default<-map_dfr(results_monthly_np3_times_defaultpriors, function(model_list_city){
  city<-model_list_city$city
  model_list_city<-model_list_city[-length(model_list_city)]
  map2_dfr(model_list_city, c("tests_pc", "pct_pos", "pos_pc"),function(model_list, type){
    map2_dfr(model_list,c("Dummy", "Linear", "Quadratic", "Spline"), function(model, int_type){
      temp<-model$summary.lincomb.derived %>% as.data.frame
      temp$date<-3:9
      temp %>% 
        mutate(lci=exp(mean-1.96*sd),
               uci=exp(mean+1.96*sd),
               rr=exp(mean),
               model=int_type) %>% 
        select(date, rr, lci, uci, model)
    }) %>% 
      mutate(type=type,
             city=city)
  })
}) %>% 
  mutate(type2=case_when(
    type=="pct_pos" ~ "Positivity",
    type=="pos_pc" ~ "Incidence",
    type=="tests_pc" ~ "Testing"),
    type2=factor(type2, levels=c("Testing", "Positivity", "Incidence")))
# results for appendix showing all parametrizations
comparison_f4<-rateratios %>% group_by(city) %>% 
  group_map(~{
    max_yaxis<-pmax(max(.x$uci), 1/min(.x$lci))
    ggplot(.x, aes(x=date, y=rr, group=type)) +
      geom_hline(yintercept = 1, lty=2)+
      geom_ribbon(aes(ymin=lci, ymax=uci, fill=type2),
                  alpha=0.3)+
      geom_line()+
      # geom_linerange(aes(ymin=lci, ymax=uci))+
      geom_point(pch=21, aes(fill=type2), color="black")+
      scale_y_continuous(trans=log_trans(),
                         breaks=c(0.5, 0.66, 1, 1.5, 2),
                         limits=c(1/max_yaxis, max_yaxis))+
      scale_x_continuous(breaks=3:9, labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))+
      scale_fill_discrete(name="")+
      guides(shape=guide_legend(override.aes=list(size=5)),
             fill=guide_legend(override.aes=list(alpha=1)))+
      labs(x="Month", y="Rate Ratio (95% CrI) per 1 SD increase in SVI",
           title=.y$city)+
      #facet_wrap(~city, labeller = labeller(city=city_label)) +
      guides(fill=F)+
      facet_grid(type2~model) +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.text.y=element_text(color="black", size=10),
            axis.text.x=element_text(color="black", size=10),
            axis.title=element_text(color="black", size=14, face="bold"),
            legend.text=element_text(color="black", size=14),
            strip.text=element_text(color="black", size=16, face="bold"),
            plot.title=element_text(color="black", size=16, face="bold"),
            strip.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank())
  })
ggsave(comparison_f4[[1]],file= "Results/AppendixFigure9.pdf", width=9, height=6)
ggsave(comparison_f4[[2]],file= "Results/AppendixFigure10.pdf", width=9, height=6)
ggsave(comparison_f4[[3]],file= "Results/AppendixFigure11.pdf", width=9, height=6)

# get DICs for the 4 different parametrizations
dics_np3<-map_dfr(results_monthly_np3_times, function(model_list_city){
  city<-model_list_city$city
  model_list_city<-model_list_city[-length(model_list_city)]
  map2_dfr(model_list_city, c("tests_pc", "pct_pos", "pos_pc"),function(model_list, type){
    map_dfr(model_list, function(model){
      data.frame(dic=model$dic$dic)
    }) %>% 
      mutate(model=1:4,
             type=type,
             city=city)
  })
}) %>% 
  mutate(model=factor(model, levels=1:4,
                      labels=c("Dummy", "Linear","Quadratic", "Spline")),
         type=factor(type, levels=c("tests_pc", "pct_pos", "pos_pc"),
                     labels=c("Testing", "Positivity", "Incidence")))

ggplot(dics_np3, aes(x=model, y=dic)) +
  geom_point(pch=21, color="black", fill="gray", size=3)+
  facet_wrap(~city+type, scales="free_y") +
  labs(x="", y="DIC")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.y=element_text(color="black", size=14),
        axis.text.x=element_text(color="black", size=12, 
                                 angle=45, vjust=1, hjust=1),
        axis.title=element_text(color="black", size=14, face="bold"),
        legend.text=element_text(color="black", size=14),
        strip.text=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        plot.title=element_text(color="black", size=16, face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave("results/appendixFigureDIC_Time.pdf", width=15, height=10)

# create select model based on min DIC by city/outcome
min_dic<-dics_np3 %>% group_by(city, type) %>% 
  filter(dic==min(dic)) %>% 
  rename(type2=type)
rateratios<-rateratios %>% right_join(min_dic)

city_label<-c("Philadelphia", "New York City", "Chicago")
names(city_label)<-c("Philadelphia", "New York City", "Chicago")
#max on either side
max_yaxis<-pmax(max(rateratios$uci), 1/min(rateratios$lci))
ggplot(rateratios, aes(x=date, y=rr, group=type)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(aes(ymin=lci, ymax=uci, fill=type2),
              alpha=0.3)+
  geom_line()+
  # geom_linerange(aes(ymin=lci, ymax=uci))+
  geom_point(pch=21, aes(fill=type2), color="black")+
  scale_y_continuous(trans=log_trans(),
                     breaks=c(0.5, 0.66, 1, 1.5, 2),
                     limits=c(1/max_yaxis, max_yaxis))+
  scale_x_continuous(breaks=3:9, labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))+
  scale_fill_discrete(name="")+
  guides(shape=guide_legend(override.aes=list(size=5)),
         fill=guide_legend(override.aes=list(alpha=1)))+
  labs(x="Month", y="Rate Ratio (95% CrI) per 1 SD increase in SVI")+
  #facet_wrap(~city, labeller = labeller(city=city_label)) +
  guides(fill=F)+
  facet_grid(type2~city, labeller = labeller(city=city_label)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_text(color="black", size=10),
        axis.text.x=element_text(color="black", size=10),
        axis.title=element_text(color="black", size=14, face="bold"),
        legend.text=element_text(color="black", size=14),
        strip.text=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave("Results/Figure4.pdf", width=9, height=6)

# compare NYC results for Positivity


# compare results of main models with new vs old priors
comparison<-full_join(rateratios %>% 
                        rename(rr_new=rr,lci_new=lci, uci_new=uci), 
                      rateratios_default %>% 
                        right_join(min_dic) %>% 
                        rename(rr_default=rr,lci_default=lci, uci_default=uci)) %>% 
  mutate(type2=case_when(
    type=="pct_pos" ~ "Positivity",
    type=="pos_pc" ~ "Incidence",
    type=="tests_pc" ~ "Testing"),
    type2=factor(type2, levels=c("Testing", "Positivity", "Incidence")))
ggplot(comparison, aes(x=rr_default, y=rr_new, color=as.factor(date))) +
  geom_abline(intercept = 0, slope=1, lty=1)+
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = 1, lty=2)+
  geom_errorbar(aes(ymin=lci_new, ymax=uci_new))+
  geom_errorbarh(aes(xmin=lci_default, xmax=uci_default))+
  scale_color_discrete(name="Month")+
  scale_x_continuous(trans="log", limits=c(0.6, 1/0.6), breaks=c(0.5, 0.75, 1, 1.5, 2))+
  scale_y_continuous(trans="log", limits=c(0.6, 1/0.6), breaks=c(0.5, 0.75, 1, 1.5, 2))+
  geom_point()+
  facet_grid(type2~city) +
  labs(x="RR (95% CrI) with default priors",
       y="RR (95% CrI) with alternative priors",
       title="Comparison of fixed effects using default and alternative priors",
       subtitle="Spatio-temporal Model (Figure 4)")+
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_text(color="black", size=10),
        axis.text.x=element_text(color="black", size=10),
        axis.title=element_text(color="black", size=14, face="bold"),
        legend.text=element_text(color="black", size=14),
        legend.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=16, face="bold"),
        strip.text=element_text(color="black", size=16, face="bold"),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave("Results/AppendixFigurePriors_Time.pdf", width=10, height=7.5)


# get KLDs for spatial models and the selected spatio-temporal models
klds1<-map_dfr(table1_models, function(model_list){
  bind_rows(model_list$m_tests_pc$summary.fixed$kld %>% as_tibble %>% mutate(outcome="tests_pc"),
            model_list$m_pct_pos$summary.fixed$kld %>% as_tibble %>% mutate(outcome="pct_pos"),
            model_list$m_pos_pc$summary.fixed$kld %>% as_tibble %>% mutate(outcome="pos_pc"),
            model_list$m_deaths_pc$summary.fixed$kld %>% as_tibble %>% mutate(outcome="deaths_pc")) %>% 
    mutate(city=model_list$city,
           model="spatial")
})
klds2<-map_dfr(results_monthly_np3_times, function(model_list_city){
  city<-model_list_city$city
  model_list_city<-model_list_city[-length(model_list_city)]
  map2_dfr(model_list_city, c("Testing", "Positivity", "Incidence"), function(model_list, type){
    map2_dfr(model_list, c("Dummy", "Linear", "Quadratic", "Spline"), function(model, time){
      model$summary.fixed$kld %>% as_tibble %>% mutate(type2=type,
                                                       model=time)
    })
  }) %>% mutate(city=city)
}) %>% right_join(min_dic)
klds<-bind_rows(klds1, klds2)
summary(klds$value)
log10(max(klds$value))
