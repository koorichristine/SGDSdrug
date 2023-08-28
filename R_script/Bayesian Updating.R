# Set the working directory
setwd("C:/Users/chris/Desktop/SGDS/Stan_Input_Datasets")

# Load the packages with library()
library("tidyverse")
library("sf")
library("tmap")
library("spdep")
library("ggpubr")
library("INLA")

library("rstan")
library("geostan")
library("SpatialEpi")
library("tidybayes")
library("zoo")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load shapefiles for England lads and regions shp
England_Region_shp <- read_sf("C:/Users/chris/Desktop/SGDS/Data/England_shp/England Regions Shapefile.shp")
England_LA_2011_shp<- read_sf("C:/Users/chris/Desktop/SGDS/Data/England_shp/England Local Authority 2011 Shapefile.shp")
England_LA_2014_shp<- read_sf("C:/Users/chris/Desktop/SGDS/Data/England_shp/England Local Authority 2014 Shapefile.shp")
England_LA_2015_shp<- read_sf("C:/Users/chris/Desktop/SGDS/Data/England_shp/England Local Authority 2015 Shapefile.shp")
England_LA_2019_shp<- read_sf("C:/Users/chris/Desktop/SGDS/Data/England_shp/England Local Authority 2019 Shapefile.shp")
England_LA_2020_shp<- read_sf("C:/Users/chris/Desktop/SGDS/Data/England_shp/England Local Authority 2020 Shapefile.shp")
England_LA_2021_shp<- read_sf("C:/Users/chris/Desktop/SGDS/Data/England_shp/England Local Authority 2021 Shapefile.shp")


# Import analysis dataset
Drug_2011 = read.csv("2011_data.csv")
Drug_2012 = read.csv("2012_data.csv")
Drug_2013 = read.csv("2013_data.csv")
Drug_2014 = read.csv("2014_data.csv")
Drug_2015 = read.csv("2015_data.csv")
Drug_2016 = read.csv("2016_data.csv")
Drug_2017 = read.csv("2017_data.csv")
Drug_2018 = read.csv("2018_data.csv")
Drug_2019 = read.csv("2019_data.csv")
Drug_2020 = read.csv("2020_data.csv")
Drug_2021 = read.csv("2021_data.csv")

# calculate expected numbers
# Convert 'population' column to numeric
Drug_2011$Population <- as.numeric(Drug_2011$Population)
Drug_2012$Population <- as.numeric(Drug_2012$Population)
Drug_2012$Population <- na.locf(Drug_2012$Population, na.rm = FALSE)

Drug_2013$Population <- as.numeric(Drug_2013$Population)
Drug_2013$Population <- na.locf(Drug_2013$Population, na.rm = FALSE)

Drug_2014$Population <- as.numeric(Drug_2014$Population)
Drug_2014$Population <- na.locf(Drug_2014$Population, na.rm = FALSE)

Drug_2015$Population <- as.numeric(Drug_2015$Population)
Drug_2015$Population <- na.locf(Drug_2015$Population, na.rm = FALSE)

Drug_2016$Population <- as.numeric(Drug_2016$Population)
Drug_2016$Population <- na.locf(Drug_2016$Population, na.rm = FALSE)

Drug_2017$Population <- as.numeric(Drug_2017$Population)
Drug_2017$Population <- na.locf(Drug_2017$Population, na.rm = FALSE)

Drug_2018$Population <- as.numeric(Drug_2018$Population)
Drug_2018$Population <- na.locf(Drug_2018$Population, na.rm = FALSE)

Drug_2019$Population <- as.numeric(Drug_2019$Population)
Drug_2019$Population <- na.locf(Drug_2019$Population, na.rm = FALSE)

Drug_2020$Population <- as.numeric(Drug_2020$Population)
Drug_2020$Population <- na.locf(Drug_2020$Population, na.rm = FALSE)

Drug_2021$Population <- as.numeric(Drug_2021$Population)

Drug_2011$Expected <- round(expected(population = Drug_2011$Population, cases = Drug_2011$Drug_Mortality, n.strata = 1), 0)
Drug_2012$Expected <- round(expected(population = Drug_2012$Population, cases = Drug_2012$Drug_Mortality, n.strata = 1), 0)
Drug_2013$Expected <- round(expected(population = Drug_2013$Population, cases = Drug_2013$Drug_Mortality, n.strata = 1), 0)
Drug_2014$Expected <- round(expected(population = Drug_2014$Population, cases = Drug_2014$Drug_Mortality, n.strata = 1), 0)
Drug_2015$Expected <- round(expected(population = Drug_2015$Population, cases = Drug_2015$Drug_Mortality, n.strata = 1), 0)
Drug_2016$Expected <- round(expected(population = Drug_2016$Population, cases = Drug_2016$Drug_Mortality, n.strata = 1), 0)
Drug_2017$Expected <- round(expected(population = Drug_2017$Population, cases = Drug_2017$Drug_Mortality, n.strata = 1), 0)
Drug_2018$Expected <- round(expected(population = Drug_2018$Population, cases = Drug_2018$Drug_Mortality, n.strata = 1), 0)
Drug_2019$Expected <- round(expected(population = Drug_2019$Population, cases = Drug_2019$Drug_Mortality, n.strata = 1), 0)
Drug_2020$Expected <- round(expected(population = Drug_2020$Population, cases = Drug_2020$Drug_Mortality, n.strata = 1), 0)
Drug_2021$Expected <- round(expected(population = Drug_2021$Population, cases = Drug_2021$Drug_Mortality, n.strata = 1), 0)


# Merge the attribute table to the shp
# 2011-2013: There were no changes in local authority districts during this period.
# 2014: The City of Bradford (E08000032) was changed slightly, with a small part of it moving to Lancashire (E10000017).
# 2015-2018: There were no changes in local authority districts during this period.
# 2019: A significant local government reorganisation took place in Dorset. The existing nine councils (a county council and eight district councils) were replaced with two new unitary authorities: Dorset (E06000059) and Bournemouth, Christchurch, and Poole (E06000058).
# 2020: Another major change occurred in Northamptonshire. The county's eight local authorities were replaced with two new unitary authorities: North Northamptonshire (E07000221) and West Northamptonshire (E07000220).
# 2021: Buckinghamshire (E06000060) became a unitary authority, merging the county council with the four district councils in the area.
spatial_data_2011 <- merge(England_LA_2011_shp, Drug_2011, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)
spatial_data_2012 <- merge(England_LA_2011_shp, Drug_2012, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)
spatial_data_2013 <- merge(England_LA_2011_shp, Drug_2013, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)
spatial_data_2014 <- merge(England_LA_2014_shp, Drug_2014, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)
spatial_data_2015 <- merge(England_LA_2015_shp, Drug_2015, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)
spatial_data_2016 <- merge(England_LA_2015_shp, Drug_2016, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)
spatial_data_2017 <- merge(England_LA_2015_shp, Drug_2017, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)
spatial_data_2018 <- merge(England_LA_2015_shp, Drug_2018, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)
spatial_data_2019 <- merge(England_LA_2019_shp, Drug_2019, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)
spatial_data_2020 <- merge(England_LA_2020_shp, Drug_2020, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)
spatial_data_2021 <- merge(England_LA_2021_shp, Drug_2021, by.x = "LAD21CD", by.y = "Area.Codes", all.x = TRUE)

# Forward filling the nan if any
for (column in colnames(spatial_data_2011)) {
  spatial_data_2011[[column]] <- na.locf(spatial_data_2011[[column]], na.rm=FALSE)
}
for (column in colnames(spatial_data_2012)) {
  spatial_data_2012[[column]] <- na.locf(spatial_data_2012[[column]], na.rm=FALSE)
}
for (column in colnames(spatial_data_2013)) {
  spatial_data_2013[[column]] <- na.locf(spatial_data_2013[[column]], na.rm=FALSE)
}
for (column in colnames(spatial_data_2014)) {
  spatial_data_2014[[column]] <- na.locf(spatial_data_2014[[column]], na.rm=FALSE)
}
for (column in colnames(spatial_data_2015)) {
  spatial_data_2015[[column]] <- na.locf(spatial_data_2015[[column]], na.rm=FALSE)
}
for (column in colnames(spatial_data_2016)) {
  spatial_data_2016[[column]] <- na.locf(spatial_data_2016[[column]], na.rm=FALSE)
}
for (column in colnames(spatial_data_2017)) {
  spatial_data_2017[[column]] <- na.locf(spatial_data_2017[[column]], na.rm=FALSE)
}
for (column in colnames(spatial_data_2018)) {
  spatial_data_2018[[column]] <- na.locf(spatial_data_2018[[column]], na.rm=FALSE)
}
for (column in colnames(spatial_data_2019)) {
  spatial_data_2019[[column]] <- na.locf(spatial_data_2019[[column]], na.rm=FALSE)
}
for (column in colnames(spatial_data_2020)) {
  spatial_data_2020[[column]] <- na.locf(spatial_data_2020[[column]], na.rm=FALSE)
}
for (column in colnames(spatial_data_2021)) {
  spatial_data_2021[[column]] <- na.locf(spatial_data_2021[[column]], na.rm=FALSE)
}


# Check for NA (Repeat this code for each year)
if (any(is.na(spatial_data_2021))) {
  print("Yes, there are NA values.")
  # Count the number of NA values
  na_count <- spatial_data_2021 %>% summarise_all(funs(sum(is.na(.))))
  print(paste("Number of NA values:", sum(na_count)))
} else {
  print("No NA values.")
}


# Coerced into a spatial object
sp.2011 <- as(spatial_data_2011, "Spatial")
sp.2012 <- as(spatial_data_2012, "Spatial")
sp.2013 <- as(spatial_data_2013, "Spatial")
sp.2014 <- as(spatial_data_2014, "Spatial")
sp.2015 <- as(spatial_data_2015, "Spatial")
sp.2016 <- as(spatial_data_2016, "Spatial")
sp.2017 <- as(spatial_data_2017, "Spatial")
sp.2018 <- as(spatial_data_2018, "Spatial")
sp.2019 <- as(spatial_data_2019, "Spatial")
sp.2020 <- as(spatial_data_2020, "Spatial")
sp.2021 <- as(spatial_data_2021, "Spatial")

# Coerced into a matrix object
adjacencyMatrix_2011 <- shape2mat(sp.2011)
# Extract the components for the ICAR model
extractComponents_2011 <- prep_icar_data(adjacencyMatrix_2011)
View(extractComponents_2011)

n <- as.numeric(extractComponents_2011$group_size)
nod1 <- extractComponents_2011$node1
nod2 <- extractComponents_2011$node2
n_edges <- as.numeric(extractComponents_2011$n_edges)

# build the adjacency matrix using INLA library functions
adj.matrix=sparseMatrix(i=nod1,j=nod2,x=1,symmetric=TRUE)
# the ICAR precision matrix (note! This is singular)
Q=Diagonal(n, rowSums(adj.matrix)) - adj.matrix
# add a small jitter to the diagonal for numerical stability (optional but recommended)
Q_pert=Q+Diagonal(n) * max(diag(Q)) * sqrt(.Machine$double.eps)
# Compute the diagonal elements of the covariance matrix subject to the 
# constraint that the entries of the ICAR sum to zero.
# see the inla.qinv function help for further details.
Q_inv=inla.qinv(Q_pert, constr=list(A = matrix(1,1,n),e=0))
#Compute the geometric mean of the variances, which are on the diagonal of Q.inv
scaling_factor=exp(mean(log(diag(Q_inv))))

######################################################################################
# Bayesian Updating Start: 2011
y <- as.integer(spatial_data_2011$Drug_Mortality)
x <- spatial_data_2011 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2011$Expected

stan.dataset=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                  Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2011 = stan("iCAR_Stan_script.stan", data=stan.dataset, iter=60000, chains=6, verbose = FALSE)
proc.time() - ptm

options(scipen = 999)
summary(Bayesian.model, pars=c("alpha", "beta", "sigma", "logit_rho"), probs=c(0.025, 0.975))$summary
summary(Bayesian.model, pars=c("rr_alpha", "rr_beta", "sigma", "logit_rho"), probs=c(0.025, 0.975))$summary

# check rHAT < 1.05
# n_eff > 3000
# check chains for samples that diverged (should not exceed 10% of the total 180000)
d1 <- get_sampler_params(Bayesian.model, inc_warmup = F)[[1]][, 'divergent__'] %>% sum() # chain 1
d2 <- get_sampler_params(Bayesian.model, inc_warmup = F)[[2]][, 'divergent__'] %>% sum() # chain 2
d3 <- get_sampler_params(Bayesian.model, inc_warmup = F)[[3]][, 'divergent__'] %>% sum() # chain 3
d4 <- get_sampler_params(Bayesian.model, inc_warmup = F)[[4]][, 'divergent__'] %>% sum() # chain 4
d5 <- get_sampler_params(Bayesian.model, inc_warmup = F)[[5]][, 'divergent__'] %>% sum() # chain 5
d6 <- get_sampler_params(Bayesian.model, inc_warmup = F)[[6]][, 'divergent__'] %>% sum() # chain 6
(d1+d2+d3+d4+d5+d6)/180000 * 100

# Computes the exceedance probabilities for the intercept & each beta coefficient
threshold.intercept <- function(x){mean(x > 0.00)}
alpha.exc.probs <- Bayesian.model %>% spread_draws(alpha) %>% 
  summarise(alpha=threshold.intercept(alpha)) %>%
  pull(alpha)

threshold.coefficients <- function(x){mean(x > 0.00)}
beta.exc.probs <- Bayesian.model %>% spread_draws(beta[i]) %>% 
  group_by(i) %>% summarise(beta=threshold.coefficients(beta)) %>%
  pull(beta)

# reports exceedance probabilities
alpha.exc.probs
beta.exc.probs

# build a table
names <- c("Intercept", "Racial Segregation Index", "AHAH", "Civic Assets Score", "Connectedness Score", "Engaged Community Score", "Labour Market Composition",
           "Income","Employment","Health","Education","Crime","Wider Barriers to Housing","Indoors Living Environment")

results <- as.data.frame(summary(Bayesian.model, pars = c("rr_alpha", "rr_beta"), probs = c(0.025, 0.975))$summary)
results$variables <- names
row.names(results) <- 1:nrow(results)

View(results)

results <- results[,c(8, 1, 4, 5, 6, 7)]
results$mean <- round(results$mean, 4)
colnames(results)[2] <- "coefficient"
colnames(results)[3] <- "lower95"
colnames(results)[4] <- "upper95"
colnames(results)[5] <- "ess"
colnames(results)[6] <- "rhat"
results$lower95<- round(results$lower95, 4)
results$upper95 <- round(results$upper95, 4)
results$ess <- round(results$ess, 0)
results$rhat <- round(results$rhat, 4)

results$beta_95CrI <- paste(results$coefficient, " (95% CrI: ", results$lower95, " to ", results$upper95, ")", sep = "")
a <- c(alpha.exc.probs, beta.exc.probs)
results$ExceedanceProb <- round(a, 4)
results$Uncertainty <- paste("Prob = ", results$ExceedanceProb, sep = "")

write.csv(results, file = "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/2011 global relative risk coefficients.csv", row.names = FALSE)

# spatial maps of relative risks
head(summary(Bayesian.model, pars=c("rr_mu"), probs=c(0.025, 0.975))$summary)

# extraction key posterior results for the generated quantities 
relativeRisk.results <- as.data.frame(summary(Bayesian.model, pars=c("rr_mu"), probs=c(0.025, 0.975))$summary)
# now cleaning up this table up
# first, insert clean row numbers to new data frame
row.names(relativeRisk.results) <- 1:nrow(relativeRisk.results)
# second, rearrange the columns into order
relativeRisk.results <- relativeRisk.results[, c(1,4,5,7)]
# third, rename the columns appropriately
colnames(relativeRisk.results)[1] <- "rr"
colnames(relativeRisk.results)[2] <- "rrlower"
colnames(relativeRisk.results)[3] <- "rrupper"
colnames(relativeRisk.results)[4] <- "rHAT"
# view clean table 
head(relativeRisk.results)

# extract the exceedence probabilities from the icar_possion_fit object
# compute the probability that an area has a relative risk ratio > 1.0
threshold <- function(x){mean(x > 1.00)}
excProbrr <- Bayesian.model %>% spread_draws(rr_mu[i]) %>% 
  group_by(i) %>% summarise(rr_mu=threshold(rr_mu)) %>%
  pull(rr_mu)

# insert the exceedance values into the spatial data frame
spatial_data_2011$excProb <- excProbrr

# Align the results to the areas in shp
spatial_data_2011$rr <- relativeRisk.results[, "rr"]
spatial_data_2011$rrlower <- relativeRisk.results[, "rrlower"]
spatial_data_2011$rrupper <- relativeRisk.results[, "rrupper"]

# create categories to define if an area has significant increase or decrease in risk, or nothing all 
spatial_data_2011$Significance <- NA
spatial_data_2011$Significance[spatial_data_2011$rrlower<1 & spatial_data_2011$rrupper>1] <- 0    # NOT SIGNIFICANT
spatial_data_2011$Significance[spatial_data_2011$rrlower==1 | spatial_data_2011$rrupper==1] <- 0  # NOT SIGNIFICANT
spatial_data_2011$Significance[spatial_data_2011$rrlower>1 & spatial_data_2011$rrupper>1] <- 1    # SIGNIFICANT INCREASE
spatial_data_2011$Significance[spatial_data_2011$rrlower<1 & spatial_data_2011$rrupper<1] <- -1   # SIGNIFICANT DECREASE

summary(spatial_data_2011$rr)
hist(spatial_data_2011$rr)
table(spatial_data_2011$Significance)

colnames(spatial_data_2011)[21] <- "Risk_2011"
colnames(spatial_data_2011)[24] <- "ExcProb_2011"
colnames(spatial_data_2011)[25] <- "Sign_2011"
spatial_data_2011$Drug_2011 <- paste(round(spatial_data_2011$Risk_2011,2)," (95% CrI:", round(spatial_data_2011$rrlower,2), ",", round(spatial_data_2011$rrupper,2), ")", sep = "")

# Export this spatial data out for ArcGIS Pro visualisation 
st_write(spatial_data_2011, 
         dsn = "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/2011_results/2011_results_shp.shp")

# Define custom label positions
custom_labels <- data.frame(name = c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West"),
                            custom_x = c(415000, 335000, 460000, 475000, 380000, 565000, 530000, 510000, 250000),
                            custom_y = c(590000, 525000, 475000, 325000, 300000, 255000, 180000, 150000, 100000))
# Convert custom_labels to an sf object
custom_labels_sf <- st_as_sf(custom_labels, coords = c("custom_x", "custom_y"), crs = st_crs(spatial_data_2011))

# map of significance regions
# creating the labels
RiskCategorylist <- c(">0.0 to 0.25", "0.26 to 0.50", "0.51 to 0.75", "0.76 to 0.99", "1.00 & <1.01",
                      "1.01 to 1.10", "1.11 to 1.25", "1.26 to 1.50", "1.51 to 1.75", "1.76 to 2.00", "2.01 to 3.00",">3.00")

# scheme ranges from extreme dark blues to light blues to white to light reds to extreme dark reds
RRPalette <- c("#65bafe","#98cffe","#cbe6fe","#dfeffe","white","#fed5d5","#fcbba1","#fc9272","#fb6a4a","#de2d26","#b2182b","#a50f15")

# Assigning the risk categories
spatial_data_2011$RelativeRiskCat <- cut(spatial_data_2011$Risk_2011, 
                                         breaks = c(0, 0.25, 0.50, 0.75, 0.99, 1.01, 1.10, 1.25, 1.50, 1.75, 2.00, 3.00, Inf), 
                                         labels = RiskCategorylist, 
                                         include.lowest = TRUE)

# Map of relative risk
rr_map <- tm_shape(spatial_data_2011) + 
  tm_fill("RelativeRiskCat", style = "cat", title = "Relative Risk", palette = RRPalette, labels = RiskCategorylist) +
  tm_shape(England_Region_shp) + tm_polygons(alpha = 0.10) + 
  tm_shape(custom_labels_sf) + tm_text("name", size = 1.0) +
  tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 1.2, legend.text.size = 1.0) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

tmap_save(rr_map, "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/rr_2011.png", height = 12)

# map of significance regions
sg_map <- tm_shape(spatial_data_2011) + 
  tm_fill("Sign_2011", style = "cat", title = "Significance Categories", 
          palette = c("#67a9cf", "#f7f7f7", "#ef8a62"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
  tm_shape(England_Region_shp) + tm_polygons(alpha = 0.10) +
  tm_shape(custom_labels_sf) + tm_text("name", size = 1.0) +
  tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 1.2, legend.text.size = 1.0) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

tmap_save(sg_map, "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/sign_2011.png", height = 12)

# Define break points and labels
breaks <- seq(0, 1, length.out = 13) 
labels <- c("<0.01", "0.01-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", 
            "0.40-0.49", "0.50-0.59", "0.60-0.69", "0.70-0.79", "0.80-0.89", 
            "0.90-0.99", "1.00")

# Cut data into bins
spatial_data_2011$ProbCat <- cut(spatial_data_2011$ExcProb_2011, breaks = breaks, labels = labels, include.lowest = TRUE)

# Convert to factor for plotting
spatial_data_2011$ProbCat <- as.factor(spatial_data_2011$ProbCat)

# Map of exceedance probabilities
ep_map <-tm_shape(spatial_data_2011) + 
    tm_fill("ProbCat", style = "cat", title = "Probability", palette = "GnBu", labels = labels) +
    tm_shape(England_Region_shp) + tm_polygons(alpha = 0.10) +
    tm_shape(custom_labels_sf) + tm_text("name", size = 1.0, col = "black", bg.color = "white", bg.alpha = 0.5) +
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 1.2, legend.text.size = 1.0) +
    tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

tmap_save(ep_map, "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/excprob_2011.png", height = 12)

# chart the trajectory of risks to see if the remain sustained across all LIRAa surveys
# create a sink for data posterior estimates and build
risk_data <- st_drop_geometry(spatial_data_2011[,c("LAD21CD", "LAD21NM", "Risk_2011", "Sign_2011", "Drug_2011")])
# all results will be sinked into this INITIAL file
write.csv(risk_data, file = "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/2011.txt", row.names = FALSE,  fileEncoding = "UTF-8")

######################################################################################
# REPEAT
#### 2012

y <- as.integer(spatial_data_2012$Drug_Mortality)
x <- spatial_data_2012 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2012$Expected

stan.dataset.2012=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                       Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2011 <- readRDS("Bayesian_model_2011.rds")
Bayesian.model.2012 = stan(fit=Bayesian.model.2011, data=stan.dataset.2012, iter=20000, chains=6, verbose = FALSE)
proc.time() - ptm

saveRDS(Bayesian.model.2012, file = "Bayesian_model_2012.rds")
rm(Bayesian.model.2011)

# RUN: Source file:
source("Source file_2012.R")
rm(Bayesian.model.2012)


######################################################################################
# REPEAT
#### 2013
y <- as.integer(spatial_data_2013$Drug_Mortality)
x <- spatial_data_2013 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2013$Expected

stan.dataset.2013=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                       Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2012 <- readRDS("Bayesian_model_2012.rds")
Bayesian.model.2013 = stan(fit=Bayesian.model.2012, data=stan.dataset.2013, iter=20000, chains=6, verbose = FALSE)
proc.time() - ptm

saveRDS(Bayesian.model.2013, file = "Bayesian_model_2013.rds")
rm(Bayesian.model.2012)

# RUN: Source file:
source("Source file_2013.R")
rm(Bayesian.model.2013)

######################################################################################
# REPEAT
#### 2014
y <- as.integer(spatial_data_2014$Drug_Mortality)
x <- spatial_data_2014 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2014$Expected

stan.dataset.2014=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                       Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2013 <- readRDS("Bayesian_model_2013.rds")
Bayesian.model.2014 = stan(fit=Bayesian.model.2013, data=stan.dataset.2014, iter=60000, chains=6, verbose = FALSE)
proc.time() - ptm

saveRDS(Bayesian.model.2014, file = "Bayesian_model_2014.rds")
rm(Bayesian.model.2013)

# RUN: Source file:
source("Source file_2014.R")
rm(Bayesian.model.2014)

######################################################################################
# REPEAT
#### 2015
y <- as.integer(spatial_data_2015$Drug_Mortality)
x <- spatial_data_2015 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2015$Expected

stan.dataset.2015=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                       Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2014 <- readRDS("Bayesian_model_2014.rds")
Bayesian.model.2015 = stan(fit=Bayesian.model.2014, data=stan.dataset.2015, iter=20000, chains=6, verbose = FALSE)
proc.time() - ptm

saveRDS(Bayesian.model.2015, file = "Bayesian_model_2015.rds")
rm(Bayesian.model.2014)

# RUN: Source file:
source("Source file_2015.R")
rm(Bayesian.model.2015)


######################################################################################
# REPEAT
#### 2016
y <- as.integer(spatial_data_2016$Drug_Mortality)
x <- spatial_data_2016 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2016$Expected

stan.dataset.2016=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                       Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2015 <- readRDS("Bayesian_model_2015.rds")
Bayesian.model.2016 = stan(fit=Bayesian.model.2015, data=stan.dataset.2016, iter=20000, chains=6, verbose = FALSE)
proc.time() - ptm

saveRDS(Bayesian.model.2016, file = "Bayesian_model_2016.rds")
rm(Bayesian.model.2015)

# RUN: Source file:
source("Source file_2016.R")
rm(Bayesian.model.2016)

######################################################################################
# REPEAT
#### 2017
y <- as.integer(spatial_data_2017$Drug_Mortality)
x <- spatial_data_2017 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2017$Expected

stan.dataset.2017=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                       Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2016 <- readRDS("Bayesian_model_2016.rds")
Bayesian.model.2017 = stan(fit=Bayesian.model.2016, data=stan.dataset.2017, iter=20000, chains=6, verbose = FALSE)
proc.time() - ptm

saveRDS(Bayesian.model.2017, file = "Bayesian_model_2017.rds")
rm(Bayesian.model.2016)

# RUN: Source file:
source("Source file_2017.R")
rm(Bayesian.model.2017)

######################################################################################
# REPEAT
#### 2018
y <- as.integer(spatial_data_2018$Drug_Mortality)
x <- spatial_data_2018 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2018$Expected

stan.dataset.2018=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                       Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2017 <- readRDS("Bayesian_model_2017.rds")
Bayesian.model.2018 = stan(fit=Bayesian.model.2017, data=stan.dataset.2018, iter=20000, chains=6, verbose = FALSE)
proc.time() - ptm

saveRDS(Bayesian.model.2018, file = "Bayesian_model_2018.rds")
rm(Bayesian.model.2017)

# RUN: Source file:
source("Source file_2018.R")
rm(Bayesian.model.2018)

######################################################################################
# REPEAT
#### 2019
y <- as.integer(spatial_data_2019$Drug_Mortality)
x <- spatial_data_2019 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2019$Expected

stan.dataset.2019=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                       Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2018 <- readRDS("Bayesian_model_2018.rds")
Bayesian.model.2019 = stan(fit=Bayesian.model.2018, data=stan.dataset.2019, iter=20000, chains=6, verbose = FALSE)
proc.time() - ptm

saveRDS(Bayesian.model.2019, file = "Bayesian_model_2019.rds")
rm(Bayesian.model.2018)

# RUN: Source file:
source("Source file_2019.R")
rm(Bayesian.model.2019)


######################################################################################
# REPEAT
#### 2020
y <- as.integer(spatial_data_2020$Drug_Mortality)
x <- spatial_data_2020 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2020$Expected

stan.dataset.2020=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                       Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2019 <- readRDS("Bayesian_model_2019.rds")
Bayesian.model.2020 = stan(fit=Bayesian.model.2019, data=stan.dataset.2020, iter=20000, chains=6, verbose = FALSE)
proc.time() - ptm

saveRDS(Bayesian.model.2020, file = "Bayesian_model_2020.rds")
rm(Bayesian.model.2019)

# RUN: Source file:
source("Source file_2020.R")
rm(Bayesian.model.2020)


######################################################################################
# REPEAT
#### 2021
y <- as.integer(spatial_data_2021$Drug_Mortality)
x <- spatial_data_2021 %>% 
  st_drop_geometry() %>% 
  select(-LAD21CD, -OBJECTID, -LAD21NM, -Drug_Mortality, -Population, -Expected) %>%
  as.matrix()
e <- spatial_data_2021$Expected

stan.dataset.2021=list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, 
                       Y=y, X=x, K=ncol(x), Offset=e, factor=scaling_factor)

ptm <- proc.time()
# compiles Bayesian model externally in Stan 
Bayesian.model.2020 <- readRDS("Bayesian_model_2020.rds")
Bayesian.model.2021 = stan(fit=Bayesian.model.2020, data=stan.dataset.2021, iter=20000, chains=6, verbose = FALSE)
proc.time() - ptm

saveRDS(Bayesian.model.2021, file = "Bayesian_model_2021.rds")
rm(Bayesian.model.2020)


# RUN: Source file:
source("Source file_2021.R")
rm(Bayesian.model.2021)