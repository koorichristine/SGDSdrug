
# SOURCE FILE: 2014

# remove that annoying scientific notation
options(scipen = 999)
summary(Bayesian.model.2014, pars=c("alpha", "beta", "sigma", "logit_rho"), probs=c(0.025, 0.975))$summary
summary(Bayesian.model.2014, pars=c("rr_alpha", "rr_beta", "sigma", "logit_rho"), probs=c(0.025, 0.975))$summary

# check rHAT < 1.05
# n_eff > 3000
# check chains for samples that diverged (should not exceed 10% of the total 180000)
d1 <- get_sampler_params(Bayesian.model.2014, inc_warmup = F)[[1]][, 'divergent__'] %>% sum() # chain 1
d2 <- get_sampler_params(Bayesian.model.2014, inc_warmup = F)[[2]][, 'divergent__'] %>% sum() # chain 2
d3 <- get_sampler_params(Bayesian.model.2014, inc_warmup = F)[[3]][, 'divergent__'] %>% sum() # chain 3
d4 <- get_sampler_params(Bayesian.model.2014, inc_warmup = F)[[4]][, 'divergent__'] %>% sum() # chain 4
d5 <- get_sampler_params(Bayesian.model.2014, inc_warmup = F)[[5]][, 'divergent__'] %>% sum() # chain 5
d6 <- get_sampler_params(Bayesian.model.2014, inc_warmup = F)[[6]][, 'divergent__'] %>% sum() # chain 6
(d1+d2+d3+d4+d5+d6)/180000 * 100

# This portion of the code computes the exceedance probabilities for the intercept & each beta coefficient
threshold.intercept <- function(x){mean(x > 0.00)}
alpha.exc.probs <- Bayesian.model.2014 %>% spread_draws(alpha) %>% 
  summarise(alpha=threshold.intercept(alpha)) %>%
  pull(alpha)

threshold.coefficients <- function(x){mean(x > 0.00)}
beta.exc.probs <- Bayesian.model.2014 %>% spread_draws(beta[i]) %>% 
  group_by(i) %>% summarise(beta=threshold.coefficients(beta)) %>%
  pull(beta)

# reports exceedance probabilities
alpha.exc.probs
beta.exc.probs

# build a table
names <- c("Intercept", "Racial Segregation Index", "AHAH", "Civic Assets Score", "Connectedness Score", "Engaged Community Score", "Labour Market Composition",
           "Income","Employment","Health","Education","Crime","Wider Barriers to Housing","Indoors Living Environment")

results <- as.data.frame(summary(Bayesian.model.2014, pars = c("rr_alpha", "rr_beta"), probs = c(0.025, 0.975))$summary)
results$variables <- names
row.names(results) <- 1:nrow(results)



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

write.csv(results, file = "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/2014 global relative risk coefficients.csv", row.names = FALSE)

# spatial maps of relative risks
# show first 6 rows only instead of the full 47
head(summary(Bayesian.model.2014, pars=c("rr_mu"), probs=c(0.025, 0.975))$summary)

# extraction key posterior results for the generated quantities 
relativeRisk.results <- as.data.frame(summary(Bayesian.model.2014, pars=c("rr_mu"), probs=c(0.025, 0.975))$summary)
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
excProbrr <- Bayesian.model.2014 %>% spread_draws(rr_mu[i]) %>% 
  group_by(i) %>% summarise(rr_mu=threshold(rr_mu)) %>%
  pull(rr_mu)

# insert the exceedance values into the spatial data frame
spatial_data_2014$excProb <- excProbrr

# Align the results to the areas in shp
spatial_data_2014$rr <- relativeRisk.results[, "rr"]
spatial_data_2014$rrlower <- relativeRisk.results[, "rrlower"]
spatial_data_2014$rrupper <- relativeRisk.results[, "rrupper"]

# create categories to define if an area has significant increase or decrease in risk, or nothing all 
spatial_data_2014$Significance <- NA
spatial_data_2014$Significance[spatial_data_2014$rrlower<1 & spatial_data_2014$rrupper>1] <- 0    # NOT SIGNIFICANT
spatial_data_2014$Significance[spatial_data_2014$rrlower==1 | spatial_data_2014$rrupper==1] <- 0  # NOT SIGNIFICANT
spatial_data_2014$Significance[spatial_data_2014$rrlower>1 & spatial_data_2014$rrupper>1] <- 1    # SIGNIFICANT INCREASE
spatial_data_2014$Significance[spatial_data_2014$rrlower<1 & spatial_data_2014$rrupper<1] <- -1   # SIGNIFICANT DECREASE

summary(spatial_data_2014$rr)
hist(spatial_data_2014$rr)
table(spatial_data_2014$Significance)

colnames(spatial_data_2014)[21] <- "ExcProb_2014"
colnames(spatial_data_2014)[22] <- "Risk_2014"
colnames(spatial_data_2014)[24] <- "rrupper"
colnames(spatial_data_2014)[25] <- "Sign_2014"
spatial_data_2014$Drug_2014 <- paste(round(spatial_data_2014$Risk_2014,2), " (95% CrI:", round(as.numeric(spatial_data_2014$rrlower),2), ",", round(as.numeric(spatial_data_2014$rrupper),2), ")", sep = "")

# Export this spatial data out for ArcGIS Pro visualisation 
st_write(spatial_data_2014, 
         dsn = "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/2014_results/2014_results_shp.shp")

# Define custom label positions
custom_labels <- data.frame(name = c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West"),
                            custom_x = c(415000, 335000, 460000, 475000, 380000, 565000, 530000, 510000, 250000),
                            custom_y = c(590000, 525000, 475000, 325000, 300000, 255000, 180000, 150000, 100000))
# Convert custom_labels to an sf object
custom_labels_sf <- st_as_sf(custom_labels, coords = c("custom_x", "custom_y"), crs = st_crs(spatial_data_2014))

# map of significance regions
# creating the labels
RiskCategorylist <- c(">0.0 to 0.25", "0.26 to 0.50", "0.51 to 0.75", "0.76 to 0.99", "1.00 & <1.01",
                      "1.01 to 1.10", "1.11 to 1.25", "1.26 to 1.50", "1.51 to 1.75", "1.76 to 2.00", "2.01 to 3.00",">3.00")

# scheme ranges from extreme dark blues to light blues to white to light reds to extreme dark reds
RRPalette <- c("#65bafe","#98cffe","#cbe6fe","#dfeffe","white","#fed5d5","#fcbba1","#fc9272","#fb6a4a","#de2d26","#b2182b","#a50f15")

# Assigning the risk categories
spatial_data_2014$RelativeRiskCat <- cut(spatial_data_2014$Risk_2014, 
                                         breaks = c(0, 0.25, 0.50, 0.75, 0.99, 1.01, 1.10, 1.25, 1.50, 1.75, 2.00, 3.00, Inf), 
                                         labels = RiskCategorylist, 
                                         include.lowest = TRUE)

# Map of relative risk
rr_map <- tm_shape(spatial_data_2014) + 
  tm_fill("RelativeRiskCat", style = "cat", title = "Relative Risk", palette = RRPalette, labels = RiskCategorylist) +
  tm_shape(England_Region_shp) + tm_polygons(alpha = 0.10) + 
  tm_shape(custom_labels_sf) + tm_text("name", size = 1.0) +
  tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 1.2, legend.text.size = 1.0) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

tmap_save(rr_map, "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/rr_2014.png", height = 12)

# map of significance regions
sg_map <- tm_shape(spatial_data_2014) + 
  tm_fill("Sign_2014", style = "cat", title = "Significance Categories", 
          palette = c("#67a9cf", "#f7f7f7", "#ef8a62"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
  tm_shape(England_Region_shp) + tm_polygons(alpha = 0.10) +
  tm_shape(custom_labels_sf) + tm_text("name", size = 1.0) +
  tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 1.2, legend.text.size = 1.0) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

tmap_save(sg_map, "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/sign_2014.png", height = 12)

# Define break points and labels
breaks <- seq(0, 1, length.out = 13) 
labels <- c("<0.01", "0.01-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", 
            "0.40-0.49", "0.50-0.59", "0.60-0.69", "0.70-0.79", "0.80-0.89", 
            "0.90-0.99", "1.00")

# Cut data into bins
spatial_data_2014$ProbCat <- cut(spatial_data_2014$ExcProb_2014, breaks = breaks, labels = labels, include.lowest = TRUE)

# Convert to factor for plotting
spatial_data_2014$ProbCat <- as.factor(spatial_data_2014$ProbCat)

# Map of exceedance probabilities
ep_map <-tm_shape(spatial_data_2014) + 
  tm_fill("ProbCat", style = "cat", title = "Probability", palette = "GnBu", labels = labels) +
  tm_shape(England_Region_shp) + tm_polygons(alpha = 0.10) +
  tm_shape(custom_labels_sf) + tm_text("name", size = 1.0, col = "black", bg.color = "white", bg.alpha = 0.5) +
  tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 1.2, legend.text.size = 1.0) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

tmap_save(ep_map, "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/excprob_2014.png", height = 12)


# chart the trajectory of risks to see if the remain sustained across all LIRAa surveys
# create a sink for data posterior estimates and build
risk_data <- st_drop_geometry(spatial_data_2014[,c("LAD21CD", "LAD21NM", "Risk_2014", "Sign_2014", "Drug_2014")])
# all results will be sinked into this INITIAL file
initial_file <- read.csv(file = "C:/Users/chris/Desktop/SGDS/Stan_Output_Results/2013.txt", sep = ",")
append_file <- merge(initial_file, risk_data, by.x=c("LAD21CD", "LAD21NM"), by.y=c("LAD21CD", "LAD21NM"))
# overwrite
write.csv(append_file, file="C:/Users/chris/Desktop/SGDS/Stan_Output_Results/2014.txt", row.names = FALSE, fileEncoding = "UTF-8")