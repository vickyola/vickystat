# Load packages.
library("here")
library("unmarked")
library("AICcmodavg")
library("ggplot2")

# Load data
occ_data <- readRDS("occ_data.RDS")

### To wide format.

# Select a species.
species <- "Eurasian Skylark"

dates <- unique(occ_data$date)
sites <- unique(occ_data$site)

occ_data_wide <- data.frame(matrix(0,length(sites), length(dates)+6))
colnames(occ_data_wide) <- c("site",as.character(dates),
                             c("cntrl__","edg_lng", "edg_ln_", 
                               "prmnn__", "prmn___"))

# Create a data table suitable for occupancy models
# Meaning: 
# sp.detections in a matrix with dims sites*dates
# covariates in a matrix with each variable in a column
for (i in 1:length(sites)){
  print(i)
  s <- sites[i]
  site_data <- occ_data[occ_data$site == s,]
  occ_data_wide[i,c("cntrl__","edg_lng", "edg_ln_", 
                    "prmnn__", "prmn___")] <- site_data[1,c("cntrl__","edg_lng", "edg_ln_", 
                                                         "prmnn__", "prmn___")]
  for (j in 1:length(dates)){
    d <- as.character(dates)[j]
    
    obs_sub <- occ_data[occ_data$species == species &
                          occ_data$site == s &
                          as.character(occ_data$date) == d,]
    
    if (nrow(obs_sub) > 0){
      colnames(occ_data_wide)
      occ_data_wide[i,j+1] <-1
    }
  }
}


# To the unmarked data format.
umf <- unmarkedFrameOccu(y = occ_data_wide[,2:16], 
                         siteCovs = occ_data_wide[,17:21])

# Scale explanatory variables (Substract mean and divide by s.dev)
umf@siteCovs$cntrl__ <- scale(umf@siteCovs$cntrl__)
umf@siteCovs$edg_lng <- scale(umf@siteCovs$edg_lng)
umf@siteCovs$prmnn__ <- scale(umf@siteCovs$prmnn__)

# Fit occupany model
umffm <- occu(formula = ~ 1
                        ~ edg_lng + prmnn__ ,
              data = umf)

# Display model fit
umffm

### Plot the marginal effect of one of the covariates
# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_forest_newdata <- data.frame(prmnn__ = seq(min(umf@siteCovs$prmnn__), 
                                                max(umf@siteCovs$prmnn__), by = 0.1),
                                  edg_lng= mean(umf@siteCovs$edg_lng))


# Model-prediction of occupancy and confidence interval
occu_forest_pred <- modavgPred(list(umffm),
                               parm.type = "psi", # psi = occupancy
                               newdata = occu_forest_newdata)[c("mod.avg.pred",
                                                                "lower.CL",
                                                                "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_forest_pred_df <- data.frame(Predicted = occu_forest_pred$mod.avg.pred,
                                  lower = occu_forest_pred$lower.CL,
                                  upper = occu_forest_pred$upper.CL,
                                  occu_forest_newdata)

# Plot the relationship
occu_forest_pred_plot <- ggplot(occu_forest_pred_df, aes(x = prmnn__, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed") +
  geom_path(size = 1) +
  labs(x = "Grassland proportion (standardized)", y = "Occupancy probability") +
  theme_classic() +
  coord_cartesian(ylim = c(0,1)) +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"))
occu_forest_pred_plot
