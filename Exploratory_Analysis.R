# Spatial Inequities in COVID-19 Testing, Positivity, Incidence and Mortality in 3 US Cities: a Longitudinal Ecological Study
# Bilal, Barber, Tabb & Diez-Roux
# Contact: ub45@drexel.edu
# Repository: https://github.com/usamabilal/COVID_Disparities
# Code files:
## Data_Management.R: contains all data management
## Exploratory_Analysis.R: contains all exploratory analysis
## Models.R: contains the modeling part of the paper using INLA
## Tables_Figures.R: contains the creation of final tables and visualizations

# Exploratory Analysis file (B). 
# Index: ---- 
## B.1: Setup (packages, data, help variables)
## B.2: Total numbers
## B.3: Scatterplots of exposures vs outcomes
## B.4: Checking for which model to use (poisson or NB): overdispersion tests
## B.5: Maps
## B.6: Measures of global autocorrelation
## B.7: Checking for the presence of residual spatial autocorrelation conditional on SVI


# B.1: Setup ---- 
rm(list=ls())
# load functions and packages
source("Helper_Functions.R")
# load data
load("data/clean_data.rdata")
# list of variables we'll be looking at later on:
vars_outcome<-c("tests_pc", "pct_pos", "pos_pc", "deaths_pc")
vars_exposure<-c("svi", "svi1", "svi2", "svi3", "svi4")
# labels for figures (mostly x axis titles)
xlabs_outcome<-c("Testing", "Positivity", "Incidence", "Mortality")
names(xlabs_outcome)<-vars_outcome
xlabs_svi<-c("Social Vulnerability Index",
             "Socioeconomic Status",
             "Household composition & disability",
             "Minority status & language",
             "Housing type & transportation")
names(xlabs_svi)<-vars_exposure
xlabs<-c(xlabs_outcome, xlabs_svi)
# saving smoother for scatterplots latter (loess or lm)
smoother<-stat_smooth(method="loess", se=F)

# B.2: Total numbers ---- 
# get total numbers for the first results paragraph
last_available_date %>% 
  group_by(city) %>% 
  summarise(all=sum(all),
            positives=sum(positives),
            deaths=sum(deaths))

# B.3: Scatterplots of exposures vs outcomes ---- 
# First: main SVI vs 4 outcomes. Loop over city
plots<-last_available_date %>% group_by(city) %>% 
  filter(date==max(date)) %>% 
  group_map(~{
    # one plot per outcome
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
plot(pall)

# Then explore the 4 SVI domains vs 4 outcomes (one outcome per plot)
# first: plot testing rate
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
ggsave("Results/Supplemental_Results/AppendixFigure5.pdf",plots_tests_pc, width=20, height=10)
# positivity
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
ggsave("Results/Supplemental_Results/AppendixFigure6.pdf",plots_pct_pos, width=20, height=10)
# incidence
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
ggsave("Results/Supplemental_Results/AppendixFigure7.pdf",plots_pos_pc, width=20, height=10)
# mortality
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
ggsave("Results/Supplemental_Results/AppendixFigure8.pdf",plots_deaths_pc, width=20, height=10)


# B.4: Checking for which model to use (poisson or NB): overdispersion tests ----
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
fwrite(var_distribution_table, file="Results/Supplemental_Results/AppendixTable1.csv")
# second, test Poisson model [non-spatial] and check for overdispersion using the check_overdispersion function from the performance package
# create a list of dataset (one dataset per city and exposure)
last_date<-last_available_date %>% 
  rowwise() %>% 
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
# check for overdispersion in each dataset (city-exposure)
overdispersion_table<-map_dfr(last_date, function(temp){
  city_var<-unique(temp$city)
  var<-unique(temp$var)
  print(paste0(city_var, "; ", var))
  temp$value<-as.numeric(scale(temp$value, center=T, scale=T))
  # fit a non-spatial poisson model for each outcome
  m_tests_pc<-glm(formula=all~value+offset(log(total_pop)),family="poisson",
                  data=temp)
  m_pct_pos<-glm(formula=positives~value+offset(log(all)),family="poisson",
                 data=temp)
  m_pos_pc<-glm(formula=positives~value+offset(log(total_pop)),family="poisson",
                data=temp)
  m_deaths_pc<-glm(formula=deaths~value+offset(log(total_pop)),family="poisson",
                   data=temp)
  # using the overdispersion test from Gelman & Hill
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
fwrite(overdispersion_table, file="Results/Supplemental_Results/AppendixTable2.csv")


# B.5: Maps (exploratory; see main maps in Tables_Figures.R file) ---- 
# Exploratory Choropleth maps of outcomes and exposures. paper versions to be found in Tables_Figures file
# loop over cities
maps_outcome<-map(c("Chicago", "New York City", "Philadelphia"), function(city_var){
  # restrict data to each city
  data<-last_available_date %>% 
    filter(city==city_var) %>% 
    select(GEOID, pct_pos, tests_pc, pos_pc, deaths_pc) 
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
    shp_with_data<-data %>% 
      left_join(shp_zip_mod) %>% 
      ungroup() 
  } else {
    shp_with_data<-data %>% 
      left_join(shp_zip) %>% 
      ungroup()
  }
  # map the four outcomes
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
ggsave("results/Supplemental_Results/AppendixFigure1.pdf", pall, width=25, height=15)
# maps of exposures
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
ggsave("results/Supplemental_Results/AppendixFigure2.pdf", pall, width=25, height=15)

# B.6: Measures of global autocorrelation ----
# Global Moran's I of the 4 outcomes (using the moran.mc function from spdep)
# loop over every city-outcome combination
globalmoran<-last_available_date %>% 
  select(city, GEOID, pct_pos, tests_pc, pos_pc, deaths_pc) %>% 
  gather(variable, value, -city, -GEOID) %>% 
  group_by(city, variable) %>% 
  group_modify(~{
    # get neighbors
    id<-which(neighbors_id$city==.y$city)
    nb_mat<-neighbors[[id]]
    # use the moran.mc function. first, x=value to be explored
    temp<-moran.mc(x = .x %>% pull(value), 
                   # listw= list of neighbors, obtained from the nb adjacency matrix by using the nb2listw spdep function
                   listw = nb2listw(nb_mat, style = "B"), 
                   # last, number of permutations for the permutation-based moran test
                   nsim=9999)
    # extract moran statistic and p value
    data.frame(statistic=temp$statistic[1], pval=temp$p.value)
  }) %>% 
  mutate(variable=factor(variable, levels=c(vars_outcome, vars_exposure),
                         labels=xlabs),
         out=paste0(round(statistic, digits=3), " (", ifelse(pval<0.001, "<0.001",round(pval, digits=3)), ")")) %>% 
  select(city, variable, out) %>% 
  arrange(city, variable) %>% 
  spread(city, out)
globalmoran
fwrite(globalmoran, file="results/Supplemental_Results/AppendixTable3.csv")


# B.7: Checking for the presence of residual spatial autocorrelation conditional on SVI ----
# for this, we use the moran.test function from the spdep package
# loop over each dataset (one city-exposure combination per dataset in last_date)
naive_models<-map(last_date, function(temp){
  city_var<-unique(temp$city)
  var<-unique(temp$var)
  print(paste0(city_var, "; ", var))
  # get neighbors from the list of neighbors
  id<-which(neighbors_id$city==city_var)
  nb_mat<-neighbors[[id]]
  # convert to a binary matrix (0-1, 1=neighbor)
  nb_matrix<-nb2mat(nb_mat,style = "B")
  # make sure SVI is standardized for each city
  temp$value<-as.numeric(scale(temp$value, center=T, scale=T))
  # fit a negative binomial model
  m_tests_pc<-glm.nb(formula=all~value+pct_age65plus+offset(log(total_pop)),
                     data=temp)
  m_pct_pos<-glm.nb(formula=positives~value+pct_age65plus+offset(log(all)),
                    data=temp)
  m_pos_pc<-glm.nb(formula=positives~value+pct_age65plus+offset(log(total_pop)),
                   data=temp)
  m_deaths_pc<-glm.nb(formula=deaths~value+pct_age65plus+offset(log(total_pop)),
                      data=temp)
  # run a global moran's I test (moran.mc function from spdep package) on the residuals of each model
  moran<-list(moran.mc(residuals(m_tests_pc, type="pearson"),
                         listw=nb2listw(nb_mat, style="B", zero.policy = T),
                       nsim=9999),
              moran.mc(residuals(m_pct_pos, type="pearson"),
                         listw=nb2listw(nb_mat, style="B", zero.policy = T),
                       nsim=9999),
              moran.mc(residuals(m_pos_pc, type="pearson"),
                         listw=nb2listw(nb_mat, style="B", zero.policy = T),
                       nsim=9999),
              moran.mc(residuals(m_deaths_pc, type="pearson"),
                         listw=nb2listw(nb_mat, style="B", zero.policy = T),
                       nsim=9999))
  # also save the correlogram of the residuals, using the sp.correlogram function
  correlo<-list(sp.correlogram(nb_mat, residuals(m_tests_pc, type="pearson"), order = 5, zero.policy = T),
                sp.correlogram(nb_mat, residuals(m_pct_pos, type="pearson"), order = 5, zero.policy = T),
                sp.correlogram(nb_mat, residuals(m_pos_pc, type="pearson"), order = 5, zero.policy = T),
                sp.correlogram(nb_mat, residuals(m_deaths_pc, type="pearson"), order = 5, zero.policy = T))
  # save all results
  list(moran=moran, correlo=correlo, city=city_var, variable=var)
})
# Extract Moran's I from the list of results above (naive_models)
# loop over each city-exposure combination
moran_model<-map_dfr(naive_models, function(model){
  # extract the list of four moran's I (one per outcome)
  moran<-model$moran
  # get Moran values
  moran_stat<-map_chr(moran, function(temp) format(temp$statistic, digits=3, nsmall=3))
  # get p-values
  moran_p<-map_chr(moran, function(temp){
    ifelse(temp$p.value<0.001, "<0.001", format(temp$p.value, digits=2, nsmall=2))
  })
  # put both together
  moran=paste0(moran_stat, " (", moran_p, ")")
  # output the p-value of each
  data.frame(city=model$city, var=model$variable,
             tests_pc=moran[[1]], pct_pos=moran[[2]], pos_pc=moran[[3]], deaths_pc=moran[[4]])
})
moran_model
fwrite(moran_model, file="results/Supplemental_Results/AppendixTable4.csv")
# Extract the correlograms for each city-exposure combination
correlos<-map_dfr(naive_models, function(model){
  correlo<-model$correlo
  # loop over the four outcomes, extract the correlations (y axis) and lags (x ais)
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
# plot correlograms for SVI
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
ggsave(filename = "Results/Supplemental_Results/AppendixFigure4.pdf", width=10, height=12)
