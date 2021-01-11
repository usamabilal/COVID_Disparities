# Spatial Inequities in COVID-19 Testing, Positivity, Incidence and Mortality in 3 US Cities: a Longitudinal Ecological Study
*Authors*: Bilal, Barber, Tabb & Diez-Roux

*Contact*: ub45@drexel.edu

*Code files:*

- Data_Management.R: contains all data management
- Exploratory_Analysis.R: contains all exploratory analysis
- Models.R: contains the modeling part of the paper using INLA
- Tables_Figures.R: contains the creation of final tables and visualizations
- Helper_Functions.R: contains package list, options, and a couple useful functions

*Other files:*

- Moran_Table_AllZCTAs.csv: contains local Moran's I results for all ZCTAs/outcomes in each city.
- Data/Clean_rdata.rdata: contains all clean and final data
- Data/SVI: contains the SVI for the three states
- Data/zcta_tract_rel_10.txt: relationship file (CT <-> ZCTA)
- Data/nb_inla_chi.adj and _nyc.adj and _phl.adj: adjacency files for each city.

We used data from three large US cities to characterize spatial and social inequities in testing, positivity, confirmed cases, and mortality.

Preprint here: https://www.medrxiv.org/content/10.1101/2020.05.01.20087833v4