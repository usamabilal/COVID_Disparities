# Spatial Inequities in COVID-19 Testing, Positivity, Incidence and Mortality in 3 US Cities: a Longitudinal Ecological Study
# Bilal, Barber, Tabb & Diez-Roux
# Contact: ub45@drexel.edu
# Repository: https://github.com/usamabilal/COVID_Disparities
# Code files:
## Data_Management.R: contains all data management
## Exploratory_Analysis.R: contains all exploratory analysis
## Models.R: contains the modeling part of the paper using INLA
## Tables_Figures.R: contains the creation of final tables and visualizations

# Tables and Figures file (D). 
# Index: ---- 
## D.1: Setup (packages, data, help variables)
## D.2: Choropleth map
## D.3: Clusters map
## D.4: Putting maps together (Figures 1-3)
### D.4.1: Chicago (Figure 1)
### D.4.2: NYC (Figure 2)
### D.4.3: Philadelphia (Figure 3)
## D.5: Figure 3 (Scatter plot)
## D.6: Table 1 (modeling results)
## D.7: Moran's plot for Appendix

# D.1: Setup ---- 
rm(list=ls())
# load functions and packages
source("Helper_Functions.R")
# load data
load("data/clean_data.rdata")
# load local moran results
load("Results/Localmoran_Results.rdata")
# load INLA model results
load("Results/Model_results.rdata")
# list of variables we'll be looking at later on:
vars_outcome<-c("tests_pc", "pct_pos", "pos_pc", "deaths_pc")
vars_exposure<-c("svi", "svi1", "svi2", "svi3", "svi4")
# labels for figures (mostly x axis titles)
xlabs_outcome<-c("Testing", "Positivity", "Incidence", "Mortality")
names(xlabs_outcome)<-vars_outcome
xlabs_svi<-c("Social Vulnerability",
             "Socioeconomic Status",
             "Household composition & disability",
             "Minority status & language",
             "Housing type & transportation")
names(xlabs_svi)<-vars_exposure
xlabs<-c(xlabs_outcome, xlabs_svi)
# saving smoother for scatterplots latter (loess or lm)
smoother<-stat_smooth(method="loess", se=F, color="black")


# D.2: Choropleth map ---- 
# specific theme for these maps
theme_map_1<-theme_void()+theme(plot.title = element_text(size=20, face="bold", hjust=.5),
                   panel.background = element_rect(fill = "white", color=NA),
                   legend.position="bottom",
                   legend.title=element_text(size=14),
                   legend.text=element_text(size=12))
choropleth<-map(c("Chicago", "New York City", "Philadelphia"), function(city_var){
  # restrict data to each city
  data<-last_available_date %>% 
    filter(city==city_var) %>% 
    select(GEOID, pct_pos, tests_pc, pos_pc, deaths_pc, svi) 
  # get the map extent for each city (saved as bbox_pa, bbox_il, or bbox_ny)
  if (city_var=="Philadelphia"){
    ## add missing zip code (Navy Yard of Philadelphia)
    data<-data %>% bind_rows(data.frame(GEOID=19112))  
    bbox<-bbox_pa
  } else if (city_var=="Chicago") {
    bbox<-bbox_il
  } else {
    bbox<-bbox_ny
  }
  # for NYC, join data with the modified ZCTA shapefile. for the rest use the regular census file
  if (city_var=="New York City"){
    shp_with_data<-merge(shp_zip_mod, data, by="GEOID", all.y=T, all.x=F) 
  } else {
    shp_with_data<-merge(shp_zip, data, by="GEOID", all.y=T, all.x=F) 
  }
  # map the four outcomes
  m1<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=pos_pc))+
    geom_sf(data=shp_with_data, size=.1,fill=NA, color="black",
            aes(geometry=geometry))+
    scale_fill_binned(name="Cases\nper 1k", type="gradient",
                      show.limits=T,n.breaks=5,labels=round,
                      low="white", high="black", trans="log10")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, color=guide_bins(override.aes = list(color="black")))+
    #labs(title=ifelse(city_var=="Chicago", "Incidence", "")) +
    labs(title="Incidence") +
    theme_map_1
  m2<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=pct_pos*100))+
    geom_sf(data=shp_with_data, size=.1,fill=NA, color="black",
            aes(geometry=geometry))+
    scale_fill_binned(name="Positivity (%)", type="gradient",
                      show.limits=T,n.breaks=5,labels=round,
                      low="white", high="black")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, color=guide_bins(override.aes = list(color="black")))+
    #labs(title=ifelse(city_var=="Chicago", "Positivity", "")) +
    labs(title="Positivity") +
    theme_map_1
  m3<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=tests_pc/10))+
    geom_sf(data=shp_with_data, size=.1,fill=NA, color="black",
            aes(geometry=geometry))+
    scale_fill_binned(name="Tests\nper 10k", type="gradient",
                      show.limits=T,n.breaks=5,labels=round,
                      low="white", high="black", trans="log10")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, color=guide_bins(override.aes = list(color="black")))+
    #labs(title=ifelse(city_var=="Chicago", "Testing", "")) +
    labs(title="Testing") +
    theme_map_1
  m4<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=deaths_pc))+
    geom_sf(data=shp_with_data, size=.1,fill=NA, color="black",
            aes(geometry=geometry))+
    scale_fill_binned(name="Deaths\nper 1k", type="gradient",
                      show.limits=T,n.breaks=5,labels=function(xx) round(xx, digits=1),
                      low="white", high="black", trans="log10")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, color=guide_bins(override.aes = list(color="black")))+
    #labs(title=ifelse(city_var=="Chicago", "Mortality", "")) +
    labs(title="Mortality") +
    theme_map_1
  m5<-ggplot()+
    geom_sf(data=shp_with_data, size=.1,
            aes(geometry=geometry, fill=svi))+
    geom_sf(data=shp_with_data, size=.1,fill=NA, color="black",
            aes(geometry=geometry))+
    scale_fill_binned(name="SVI\n(SD)", type="gradient",
                      show.limits=T,n.breaks=5,labels=function(xx) round(xx, digits=1),
                      low="white", high="black")+
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    guides(alpha=F, size=F, color=guide_bins(override.aes = list(color="black")))+
    #labs(title=ifelse(city_var=="Chicago", "Mortality", "")) +
    labs(title="Social Vulnerability") +
    theme_map_1
  list(m3, m2, m1, m4, m5)
})


# D.3: Clusters ----
# specific theme for these maps
theme_map_2<-theme_void()+theme(plot.title = element_text(size=20, face="bold", hjust=.5),
                                panel.background = element_rect(fill = "white", color=NA),
                                legend.position="bottom",
                                legend.title=element_text(size=20),
                                legend.text=element_text(size=20))
# extract moran's clusters for the outcome varaibles
clusters<-localmoran %>% 
  filter(grepl("tests|pct|pos|deaths|(svi$)", variable)) %>% 
  group_by(city) %>% 
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
        
        title<-xlabs[which(names(xlabs)%in%.y$variable)]
        moran_colors<-c("high-high"="black",
                        "high-low"="tomato1",
                        "low-high"="skyblue1",
                        "low-low"="gray")
        
        ggplot()+
          geom_sf(data=shp_clusters %>% filter(!is.na(moran_cluster)), size=0,color=NA,
                  aes(geometry=geometry, fill=(moran_cluster)))+
          geom_sf(data=shp_clusters, size=.1,fill=NA,color="black",
                  aes(geometry=geometry))+
          scale_fill_manual(values=moran_colors, name="")+
          coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
                   ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
          guides(size=F, alpha=F, fill = guide_legend(override.aes = list(alpha=0))) +
          #labs(title=title) +
          labs(title="")+
          theme_map_2 + theme(legend.text=element_blank())
      })
    maps
  })

# create a custom legend to add below
temp<-data.frame(cluster=factor(0:1, levels=0:1, labels=c("High-High", "Low-Low")), id=0:1)
legend<-ggplot(temp, aes(x=id, y=id))+
  geom_point(aes(fill=cluster), pch=22, size=4, color=NA) +
  scale_fill_manual(values=c("black", "gray"), name="") +
  guides(fill=guide_legend(override.aes = (list(size=20))))+
  theme_map_2
legend<-get_legend(legend)

# D.4: Putting things together for Figures 1-3 ----
{
  # D.4.1: Chicago (Figure 1) ----
  legends1<-map(choropleth[[1]], function(xx) get_legend(xx))
  map1<-map(choropleth[[1]], function(xx) xx+guides(fill=F))
  map2<-map(clusters[[1]], function(xx) xx+guides(fill=F))
  map1<-arrangeGrob(grobs=(map1), ncol=5)
  legends1<-arrangeGrob(grobs=(legends1), ncol=5)
  map2<-arrangeGrob(grobs=(map2), ncol=5)
  pall<-arrangeGrob(grobs=list(map1, legends1, map2, legend), ncol=1, heights=c(5, 0.5, 5, 0.5))
  ggsave("results/Figure1.pdf", pall, width=25, height=15)
  # D.4.2: NYC (Figure 2) ----
  legends1<-map(choropleth[[2]], function(xx) get_legend(xx))
  map1<-map(choropleth[[2]], function(xx) xx+guides(fill=F))
  map2<-map(clusters[[2]], function(xx) xx+guides(fill=F))
  map1<-arrangeGrob(grobs=(map1), ncol=5)
  legends1<-arrangeGrob(grobs=(legends1), ncol=5)
  map2<-arrangeGrob(grobs=(map2), ncol=5)
  pall<-arrangeGrob(grobs=list(map1, legends1, map2, legend), ncol=1, heights=c(5, 0.5, 5, 0.5))
  ggsave("results/Figure2.pdf", pall, width=25, height=12.5)
  # D.4.3: Philadelphia (Figure 3) ----
  legends1<-map(choropleth[[3]], function(xx) get_legend(xx))
  map1<-map(choropleth[[3]], function(xx) xx+guides(fill=F))
  map2<-map(clusters[[3]], function(xx) xx+guides(fill=F))
  map1<-arrangeGrob(grobs=(map1), ncol=5)
  legends1<-arrangeGrob(grobs=(legends1), ncol=5)
  map2<-arrangeGrob(grobs=(map2), ncol=5)
  pall<-arrangeGrob(grobs=list(map1, legends1, map2, legend), ncol=1, heights=c(5, 0.5, 5, 0.5))
  ggsave("results/Figure3.pdf", pall, width=35, height=15)
}


# D.5: Figure 3 (Scatter plot) ----
figure4<-last_available_date %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    p1<-ggplot(.x, aes(x=svi, y=tests_pc/10)) +
      geom_point(pch=21, color="black", fill="gray")+
      smoother+
      scale_y_log10()+
      scale_x_continuous(limits=c(NA, NA))+
      annotation_logticks(sides="l")+
      theme_bw() +
      labs(x="Social Vulnerability Index (SD)",
           y="Tests per 10,000",
           title="Testing")+
      theme(axis.text=element_text(color="black"))
    p2<-ggplot(.x, aes(x=svi, y=pct_pos)) +
      geom_point(pch=21, color="black", fill="gray")+
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
      geom_point(pch=21, color="black", fill="gray")+
      smoother+
      scale_y_log10()+
      scale_x_continuous(limits=c(NA, NA))+
      annotation_logticks(sides="l")+
      theme_bw() +
      labs(x="Social Vulnerability Index (SD)",
           y="Cases per 1,000",
           title="Confirmed cases")+
      theme(axis.text=element_text(color="black"))
    p4<-ggplot(.x, aes(x=svi, y=deaths_pc)) +
      geom_point(pch=21, color="black", fill="gray")+
      smoother+
      scale_y_log10()+
      scale_x_continuous(limits=c(NA, NA))+
      annotation_logticks(sides="l")+
      theme_bw() +
      labs(x="Social Vulnerability Index (SD)",
           y="Deaths per 1,000",
           title="Mortality")+
      theme(axis.text=element_text(color="black"))
    title1<-.y$city
    title2<-nrow(.x)
    title<-paste0(title1, " (n=", title2, ")")
    arrangeGrob(grobs=list(p1, p2, p3, p4), ncol=4,
                top=textGrob(title, gp=gpar(fontsize=20,face="bold",font=8)))
  }, .keep=T) 

pall<-arrangeGrob(grobs=figure4, ncol=1)
ggsave("Results/Figure4.pdf", pall, width=16, height=6*3/2)


# D.6: Table 1 (modeling results) ----
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


# D.7: Moran's plot and table for Appendix ----
moranplot<-localmoran %>%
  filter(grepl("tests|pct|pos|deaths", variable)) %>% 
  group_by(city) %>% 
  group_map(~{
    #.x<-localmoran %>% filter(grepl("tests|pct|pos|deaths", variable), city=="New York City");.y<-data.frame(city="New York City")
    ggplot(.x, aes(x=value, y=lagged)) +
      geom_hline(yintercept = 0, lty=2)+
      geom_vline(xintercept = 0, lty=2)+
      geom_point(aes(size=pval_cat, fill=pval_cat), pch=21, color="black") +
      stat_smooth(method="lm", lty=1, size=1, se=F)+
      #geom_text(aes(label=GEOID2))+
      scale_size_manual(values=c(5:1), name="p-value: ") +
      scale_fill_brewer(palette="Set1", name="p-value: ")+
      facet_wrap(~type2, ncol = 4, scales="free")+
      theme_bw() +
      labs(x="Values for each ZCTA",
           y="Lagged values (surrounding ZCTAs)",
           title=.y$city)+
      theme(axis.text=element_text(color="black", size=12),
            axis.title=element_text(face="bold", color="black", size=12),
            plot.title=element_text(face="bold", color="black", size=14),
            strip.text =element_text(face="bold", color="black", size=12),
            strip.background = element_blank(),
            legend.position = "bottom",
            legend.text=element_text(size=12),
            legend.title=element_text(size=12, face="bold"))
  })
legend<-get_legend(moranplot[[1]])
moranplot<-map(moranplot, function(xx) xx+guides(fill=F, size=F))
pall<-arrangeGrob(grobs=list(arrangeGrob(grobs=moranplot, ncol=1), legend), ncol=1, heights=c(10, .2))
ggsave("Results/Supplemental_Results/Appendix_Moran_Local_Figure.pdf", 
       pall, width=20, height=15)

# create the table with moran's results
table_moran<-localmoran %>% 
  ungroup() %>% 
  group_by(city) %>% 
  group_modify(~{
    #.x<-localmoran %>% filter(city=="Chicago") %>% ungroup
    temp<-.x %>% filter(grepl("tests_pc|pct_pos|pos_pc|deaths_pc", variable)) %>% 
      group_by(variable) %>% 
      group_map(~{
        #.x<-.x %>% filter(variable=="tests_pc");.y<-data.frame(variable="tests_pc")
        var<-.y$variable
        .x %>% select(GEOID, value, lagged, moranI, pval,moran_cluster) %>% 
          mutate(value=round(value, digits=3),
                 lagged=round(lagged, digits=3),
                 moranI=round(moranI, digits=3),
                 pval=ifelse(pval<0.001, "<0.001", as.character(round(pval, digits=3))),
                 moran_cluster=gsub("high", "H", moran_cluster),
                 moran_cluster=gsub("low", "L", moran_cluster)) %>% 
          rename_at(-1, ~paste0(., "_",var ))
      }) %>% 
      reduce(full_join) %>% 
      arrange(GEOID)
  })
fwrite(table_moran, file="Moran_Table_AllZCTAs.csv")

# displaying a table of all clusters by zip code for each city
table_moran %>% filter(city=="Chicago") %>% 
  select(GEOID, contains("cluster")) %>% 
  View
table_moran %>% filter(city=="New York City") %>% 
  select(GEOID, contains("cluster")) %>% 
  View
table_moran %>% filter(city=="Philadelphia") %>% 
  select(GEOID, contains("cluster")) %>% 
  View
