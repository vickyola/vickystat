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
farmspecies <- c("Yellowhammer","Eurasian Skylark","Western Yellow Wagtail","Red-backed Shrike","Eurasian Tree Sparrow","Barn Swallow","European Serin","White Stork")

occu_forest_pred_df_list <- list()
occu_forest_pred_df_list_edge <- list()
est <- list()
num_site_occ <-list()
for(species in farmspecies){
  print(species)
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
  
  est[[species]]  <- coef(umffm)
  num_site_occ[[species]]<- ranef(umffm)
# Display model fit
#umffm

### Plot the marginal effect of one of the covariates
# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
  occu_forest_newdata <- data.frame(prmnn__ = seq(min(umf@siteCovs$prmnn__), 
                                                max(umf@siteCovs$prmnn__), by = 0.1),
                                  edg_lng= mean(umf@siteCovs$edg_lng))
  
  occu_forest_newdata_edge <- data.frame(edg_lng= seq(min(umf@siteCovs$edg_lng), 
                                                  max(umf@siteCovs$edg_lng), by = 0.1),
                                    prmnn__= mean(umf@siteCovs$prmnn__))


# Model-prediction of occupancy and confidence interval
  occu_forest_pred <- modavgPred(list(umffm),
                               parm.type = "psi", # psi = occupancy
                               newdata = occu_forest_newdata)[c("mod.avg.pred",
                                                                "lower.CL",
                                                                "upper.CL")]
  occu_forest_pred_edge <- modavgPred(list(umffm),
                                 parm.type = "psi", # psi = occupancy
                                 newdata = occu_forest_newdata_edge)[c("mod.avg.pred",
                                                                  "lower.CL",
                                                                  "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
  occu_forest_pred_df <- data.frame(Predicted = occu_forest_pred$mod.avg.pred,
                                  lower = occu_forest_pred$lower.CL,
                                  upper = occu_forest_pred$upper.CL,
                                  occu_forest_newdata)
  occu_forest_pred_df_edge <- data.frame(Predicted = occu_forest_pred_edge$mod.avg.pred,
                                    lower = occu_forest_pred_edge$lower.CL,
                                    upper = occu_forest_pred_edge$upper.CL,
                                    occu_forest_newdata_edge)
  #occu_forest_pred_df$species = species
 # occu_forest_pred_df_edge$species = species
  #df$species == occu_forest_pred_df
  #df <- merge(df,  occu_forest_pred_df, by = names)
  occu_forest_pred_df_list <- c(occu_forest_pred_df_list, list(occu_forest_pred_df))
  occu_forest_pred_df_list_edge <- c(occu_forest_pred_df_list_edge, list(occu_forest_pred_df_edge))
}


#Scatterplot
r <- as.data.frame(t(as.data.frame(est)))
#r$Yellowhammer[3]
r$'p(Int)'
r$farm <- farmspecies
#$numocc <- num_site_occ #ranef(occu())..don't understand that somehow use to scale? what dataformat?



plot(r$'psi(edg_lng)', r$'psi(prmnn__)',
     xlim=c(min(0),max(r$'psi(edg_lng)')) , ylim=c(-1,max(r$'psi(prmnn__)')), 
     pch=18, 
     cex= 1,#r$numocc, 
     col="#69b3a2",
     xlab="coef edg_lng", ylab="coef prmnn",
     main="Farmland species"
)
text(r$'psi(edg_lng)', r$'psi(prmnn__)',r$farm)


#ranef(occuding) to scale cex 


#prediction plot

names(occu_forest_pred_df_list) <- farmspecies
names(occu_forest_pred_df_list_edge) <- farmspecies
#df <- data.frame(occu_forest_pred_df_list) #funktioniert nicht


# Plot the relationship
plotdata <- occu_forest_pred_df_list_edge$"Western Yellow Wagtail"
plotdata
#not working for all species
occu_forest_pred_plot <- ggplot(data = plotdata, aes(x = edg_lng, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed") +
  geom_path(size = 1) +
  labs(x = "Edge length (standardized)", y = "Occupancy probability") +
  theme_classic() +
  coord_cartesian(ylim = c(0,1)) +
  #facet_wrap(~ names)+
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black")) 
        
occu_forest_pred_plot


#access S4 class (umf) with @


occu_forest_pred_plot <- ggplot(occu_forest_pred_df, aes(x = prmnn__, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed") +
  geom_path(size = 1) +
  labs(x = "Grassland proportion (standardized)", y = "Occupancy probability") +
  theme_classic() +
  coord_cartesian(ylim = c(0,1)) +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"))
occu_forest_pred_plot
