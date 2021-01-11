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

# Models file (C). 
# Index: ---- 
## C.1: Setup (packages, data, help variables)
## C.2: local Moran's I -> FIGURE 2
## C.3: INLA models
### C.3.1: Re-arranging data
### C.3.2: Cross-sectional models
### C.3.2.1: New priors -> TABLE 1
#### C.3.2.1.1: Models for testing rates
#### C.3.2.1.2: Models for positivity
#### C.3.2.1.3: Models for incidence
#### C.3.2.1.4: Models for mortality
### C.3.2.2: Default priors
## C.4: Model diagnostics
### C.4.1: Convergence
### C.4.2: Evaluating role of new vs default priors

# C.1: Setup ---- 
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

# C.2: Local Moran's I ---- 
# estimate local moran's I for each city, for each exposure (svi, svi1...) and outcome (tests, etc.)
# we use the localmoran function from spdep
.x<-last_available_date %>% 
  select(city, GEOID, pct_pos, tests_pc, pos_pc, deaths_pc,svi, svi1, svi2, svi3, svi4) %>% 
  gather(variable, value, -city, -GEOID) %>% 
  filter(city=="Philadelphia", variable=="pct_pos");.y<-data.frame(city="Philadelphia", variable="pct_pos")
localmoran<-last_available_date %>% 
  select(city, GEOID, pct_pos, tests_pc, pos_pc, deaths_pc,svi, svi1, svi2, svi3, svi4) %>% 
  gather(variable, value, -city, -GEOID) %>% 
  group_by(city, variable) %>% 
  group_modify(~{
    # get neighbors from neighbors list
    print(paste0(.y$city, "; ", .y$variable))
    .x<-.x %>% arrange(GEOID)
    id<-which(neighbors_id$city==.y$city)
    nb_mat<-neighbors[[id]]
    # calculate local moran's I
    temp<-localmoran(x = .x %>% pull(value), 
                     listw = nb2listw(nb_mat, style = "B"))
    # assign cluster status to each ZCTA. we need:
    ## is the value high or low?
    ### standardize value -> if negative (low-); if positive (high-)
    ## are its neighbors high or low?
    ### calculate lagged values (values of neighbors) -> if negative (-low), if positive (-high)
    ## create the four potential combinations (low-low; low-high; high-high, high-low)
    ## and only assign cluster status if p<0.05
    .x<-.x %>% 
      mutate(value=as.numeric(scale(value, center=T, scale=T)),
             lagged=lag.listw(x=nb2listw(nb_mat, style = "B"), 
                              var=value),
             pval=temp %>% as.data.frame %>% pull(`Pr(z > 0)`),
             moranI=temp %>% as.data.frame %>% pull(`Ii`),
             moran_exp=temp %>% as.data.frame %>% pull(`E.Ii`),
             moran_var=temp %>% as.data.frame %>% pull(`Var.Ii`),
             moran_z=temp %>% as.data.frame %>% pull(`Z.Ii`),
             moran_cluster=case_when(
               value>0 & lagged>0 ~ "high-high",
               value<=0 & lagged<=0 ~ "low-low",
               value>0 & lagged<=0 ~ "high-low",
               value<=0 & lagged>0 ~ "low-high",
             ),
             moran_cluster=ifelse(pval<0.05, moran_cluster, NA))
  }) %>% 
  # factorize variables and cluster labels
  mutate(variable=factor(variable, levels=c(vars_outcome, vars_exposure)),
         moran_cluster=factor(moran_cluster, levels=c("high-high", "high-low",
                                                      "low-high",
                                                      "low-low", NA)),
         GEOID2=ifelse(is.na(moran_cluster), "", GEOID)) %>% 
  mutate(pval_cat=case_when(pval<0.001 ~ "<0.001",
                            pval<0.01 ~ "<0.01",
                            pval<0.05 ~ "<0.05",
                            pval<0.1 ~ "<0.1",
                            T ~ ">=0.1"),
         pval_cat=factor(pval_cat, levels=c("<0.001", "<0.01", "<0.05", "<0.1", ">=0.1")),
         type2=case_when(
           variable=="pct_pos" ~ "Positivity",
           variable=="deaths_pc" ~ "Mortality",
           variable=="pos_pc" ~ "Incidence",
           variable=="tests_pc" ~ "Testing"),
           type2=factor(type2, levels=c("Testing", "Positivity", "Incidence" ,"Mortality")))

save(localmoran, file="Results/Localmoran_Results.rdata")

# C.3: Modeling ---- 
## First, conclusions from exploratory analysis
### we have overdispersion -> use negative binomial
### we have spatial autocorrelation -> use spatial model
### We need a negative binomial spatial CAR model -> INLA

# C.3.1: Re-arranging data ---- 
# rearranging data for cross-sectional models
# last_date is a list of datasets, with one dataset per city-exposure combination
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

# flag to run the models [T] (or not, replace for F)
run_models<-T
if (run_models){
  # C.3.2: Cross-sectional models (feeds into table 1 later on) ---- 
  # C.3.2.1: New priors
  # loop over each last_date dataset (15 city-exposure combinations)
  table1_models<-map(last_date, function(temp){
    city_var<-unique(temp$city)
    var<-unique(temp$var)
    print(paste0(city_var, "; ", var))
    # get INLA adjacency matrix (see Data_Management.R file)
    inla_adj<-case_when(
      city_var=="Chicago" ~ "Data/nb_inla_chi.adj",
      city_var=="New York City" ~ "Data/nb_inla_nyc.adj",
      city_var=="Philadelphia" ~ "Data/nb_inla_phl.adj"
    )
    # standardize exposure
    temp$value<-as.numeric(scale(temp$value, center=T, scale=T))
    # INLA only accepts numeric IDs for the ZCTAs
    temp$spatial_unit<-as.numeric(as.factor(temp$GEOID))
    # C.3.2.1.1: model for testing rates ----
                  # formula: all tests ~ exposure + age 
    m_tests_pc <-inla(formula=all~value+pct_age65plus+
                        # spatial part: spatial_unit defines the ZCTA
                        # we use a BYM model [Besag-York-Mollie]
                        f(spatial_unit, model = "bym", 
                          # neighbors using the adjacecy matrix shown above
                          graph = inla_adj,
                          # priors of random effects, as defined in the manuscript
                          # note that BYM models have two random effects (a unstructured one and a spatial one)
                          hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                     prec.spatial=list(prior="loggamma", param=c(1, .5)))),
                      # model is negative binomial
                      family = "nbinomial", data = temp, 
                      # offset
                      E=total_pop,
                      # prior for all fixed effects
                      control.fixed = control.fixed(mean=0, prec=0.001),
                      # INLA options [compute DIC/CPO as fit statistics, compute expected values]
                      control.predictor = list(compute = TRUE), 
                      control.compute = list(config = TRUE, dic=T, cpo=T))
    # C.3.2.1.2: model for positivity ----
    # NOTE: follows the same structure as above, with two exceptions:
    # outcome is number of positive tests, offset is number of total tests
    m_pct_pos <-inla(formula=positives~value+pct_age65plus+
                       f(spatial_unit, model = "bym", 
                         graph = inla_adj,
                         hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                    prec.spatial=list(prior="loggamma", param=c(1, .5)))),
                     family = "nbinomial", data = temp, 
                     E=all,
                     control.fixed = control.fixed(mean=0, prec=0.001),
                     control.predictor = list(compute = TRUE), 
                     control.compute = list(config = TRUE, dic=T, cpo=T))
    # C.3.2.1.3: model for incidence ----
    # NOTE: follows the same structure as above, with two exceptions:
    # outcome is number of positive tests, offset is total pop
    m_pos_pc <-inla(formula=positives~value+pct_age65plus+
                      f(spatial_unit, model = "bym", 
                        graph = inla_adj,
                        hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                   prec.spatial=list(prior="loggamma", param=c(1, .5)))),
                    family = "nbinomial", data = temp, 
                    E=total_pop,
                    control.fixed = control.fixed(mean=0, prec=0.001),
                    control.predictor = list(compute = TRUE), 
                    control.compute = list(config = TRUE, dic=T, cpo=T))
    # C.3.2.1.4: model for mortality ----
    # NOTE: follows the same structure as above, with two exceptions:
    # outcome is number of deaths, offset is total pop
    m_deaths_pc <-inla(formula=deaths~value+pct_age65plus+
                         f(spatial_unit, model = "bym", 
                           graph = inla_adj,
                           hyper=list(prec.unstruct=list(prior="loggamma", param=c(1, .5)),
                                      prec.spatial=list(prior="loggamma", param=c(1, .5)))),
                       family = "nbinomial", data = temp, 
                       E=total_pop,
                       control.fixed = control.fixed(mean=0, prec=0.001),
                       control.predictor = list(compute = TRUE), 
                       control.compute = list(config = TRUE, dic=T, cpo=T))
    # stores the four models in a list (models)
    models<-list(m_tests_pc=m_tests_pc, m_pct_pos=m_pct_pos, m_pos_pc=m_pos_pc, m_deaths_pc=m_deaths_pc)
    models
  })
  
  # C.3.2.2: Cross-sectional models with default priors ---- 
  # Note: same structure as above (see annotations above), with two exceptions
  ## the specified list of priors for the random effects of the BYM model is now gone (so it goes to the default)
  ## the specified list of priors for the fixed effects is now gone (so it goes to the default)
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
    m_tests_pc <-inla(formula=all~value+pct_age65plus+
                        f(spatial_unit, model = "bym", 
                          graph = inla_adj),
                      family = "nbinomial", data = temp, 
                      E=total_pop,
                      control.predictor = list(compute = TRUE), 
                      control.compute = list(config = TRUE, dic=T, cpo=T))
    
    m_pct_pos <-inla(formula=positives~value+pct_age65plus+
                       f(spatial_unit, model = "bym", 
                         graph = inla_adj),
                     family = "nbinomial", data = temp, 
                     E=all,
                     control.predictor = list(compute = TRUE), 
                     control.compute = list(config = TRUE, dic=T, cpo=T))
    m_pos_pc <-inla(formula=positives~value+pct_age65plus+
                      f(spatial_unit, model = "bym", 
                        graph = inla_adj),
                    family = "nbinomial", data = temp, 
                    E=total_pop,
                    control.predictor = list(compute = TRUE), 
                    control.compute = list(config = TRUE, dic=T, cpo=T))
    m_deaths_pc <-inla(formula=deaths~value+pct_age65plus+
                         f(spatial_unit, model = "bym", 
                           graph = inla_adj),
                       family = "nbinomial", data = temp, 
                       E=total_pop,
                       control.predictor = list(compute = TRUE), 
                       control.compute = list(config = TRUE, dic=T, cpo=T))
    models<-list(m_tests_pc=m_tests_pc, m_pct_pos=m_pct_pos, m_pos_pc=m_pos_pc, m_deaths_pc=m_deaths_pc)
    models
  })
  
  # save all results
  save(table1_models,table1_models_defaultpriors,last_date_ids,
       file="Results/Model_results.rdata")
} else {
  load("Results/Model_results.rdata")
}

# C.4: Model Diagnostics ---
# C.4.1: Convergence: get KLDs for spatial models  ----
# loop over all models, extract KLD, assign ids, and summarize
klds<-map_dfr(table1_models, function(model_list){
  bind_rows(model_list$m_tests_pc$summary.fixed$kld %>% as_tibble %>% mutate(outcome="tests_pc"),
            model_list$m_pct_pos$summary.fixed$kld %>% as_tibble %>% mutate(outcome="pct_pos"),
            model_list$m_pos_pc$summary.fixed$kld %>% as_tibble %>% mutate(outcome="pos_pc"),
            model_list$m_deaths_pc$summary.fixed$kld %>% as_tibble %>% mutate(outcome="deaths_pc")) %>% 
    mutate(city=model_list$city,
           model="spatial")
})
summary(klds$value)
log10(max(klds$value))
log10(min(klds$value))
# All < 10^(-5) [good, should be close to 0 for indication of convervgence]
ggplot(klds, aes(x=value)) +
  geom_histogram(bins=30, fill="gray", color="black") +
  scale_x_log10(breaks=10^-(10:5),
                labels=paste0("10^-",(10:5)))+
  scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
  annotation_logticks(sides="b")+
  labs(x="KLD", y="Count",
       title="Kullback-Leibler divergence for all 60 models")+
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
ggsave("Results/Supplemental_Results/Appendix_KLD_Figure.pdf", width=7.5, height=5)

# C.4.2: Comparing results using different priors
# extract coefs and 95% CIs for both the new and default priors
table1_rrs_newpriors<-map2_dfr(table1_models, 1:nrow(last_date_ids),function(model_list, id){
  print(id)
  id<-last_date_ids %>% ungroup() %>% slice(id)
  map2_dfr(model_list[1:4],
           c("tests_pc", "pct_pos", "pos_pc", "deaths_pc"),
           function(model, id){
             print(id)
             fixed<-model$summary.fixed
             fixed<-fixed[rownames(fixed)=="value",]
             data.frame(rr_new=exp(fixed$mean),
                        lci_new=exp(fixed$mean-1.96*fixed$sd),
                        uci_new=exp(fixed$mean+1.96*fixed$sd),
                        type=id)
           }) %>%
    mutate(city=id$city, var=id$var)
})
table1_rrs_defaultpriors<-map2_dfr(table1_models_defaultpriors, 1:nrow(last_date_ids),function(model_list, id){
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
# compare new vs default priors 
comparison<-full_join(table1_rrs_newpriors, table1_rrs_defaultpriors) %>% 
  mutate(type2=case_when(
    type=="pct_pos" ~ "Positivity",
    type=="deaths_pc" ~ "Mortality",
    type=="pos_pc" ~ "Incidence",
    type=="tests_pc" ~ "Testing"),
    type2=factor(type2, levels=c("Testing", "Positivity", "Incidence" ,"Mortality")))
ggplot(comparison, aes(x=rr_default, y=rr_new)) +
  geom_abline(intercept = 0, slope=1, lty=1)+
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = 1, lty=2)+
  geom_errorbar(aes(ymin=lci_new, ymax=uci_new, color=as.factor(var)))+
  geom_errorbarh(aes(xmin=lci_default, xmax=uci_default, color=as.factor(var)))+
  geom_point(pch=21, color="black",aes(fill=as.factor(var)))+
  scale_color_discrete(name="Variable")+
  scale_fill_discrete(name="Variable")+
  coord_fixed(ratio=1)+
  scale_x_continuous(trans="log", limits=c(0.7, 2.1), breaks=c(0.5, 0.75, 1, 1.5, 2))+
  scale_y_continuous(trans="log", limits=c(0.7, 2.1), breaks=c(0.5, 0.75, 1, 1.5, 2))+
  facet_grid(city~type2) +
  labs(x="RR (95% CrI) with default priors",
       y="RR (95% CrI) with alternative priors",
       title="Comparison of fixed effects using default and alternative priors")+
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y=element_text(color="black", size=10),
        axis.text.x=element_text(color="black", size=10),
        axis.title=element_text(color="black", size=14, face="bold"),
        legend.text=element_text(color="black", size=14),
        legend.title=element_text(color="black", size=14, face="bold"),
        plot.title=element_text(color="black", size=14, face="bold"),
        plot.subtitle=element_text(color="black", size=14, face="bold"),
        strip.text=element_text(color="black", size=14, face="bold"),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave("Results/Supplemental_Results/AppendixFigurePriors_Spatial.pdf", width=9, height=7.5)
# barely any changes at all





