##############
### Set up ###
##############

#Load libraries etc
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working directory
library(terra) #For spatial data analysis
library(tidyterra) #For graphing etc
library(measurements) #For converting units
library(stringr) #For converting units
library(gridExtra) #to help graphing
library(ggplot2) #For graphing
theme_set(theme_bw()) #Because I'm fashionable
library(ggcorrplot) #For generating the correlation plot
library(viridis) #To help graphing
library(ggfortify) #To help graphing

# Load data
df_wide_c <- read.csv("intermediate/df_wide_c.csv")

############################
### Correlation matrices ###
############################

# Create a correlation matrix (rounded to 2 decimal points)
corr <- round(100*cor(df_wide_c[,c(7:ncol(df_wide_c))],use="pairwise.complete.obs"),2)
rownames(corr) <- gsub(".Rating","",rownames(corr))
colnames(corr) <- gsub(".Rating","",colnames(corr))

# Graph the correlation matrix as a correlation plot
g_corrplot <- ggcorrplot(corr,type = "lower",lab = TRUE,
                         lab_col="skyblue") +
  scale_fill_viridis(direction=-1,option="inferno") + 
  labs(title="Correlation between variables (%)") +
  theme(plot.background = element_rect(fill="white"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1))
g_corrplot

# Save correlation plot to file
ggsave("preliminary_analysis/g_corrplot.png",g_corrplot,width=15,height=15)

####################################
### Principal component analysis ###
####################################

# I'll do two PCAs - one on the environmental variables, and one
# on the bands

# Make data frames
df_wide_c$day <- as.factor(df_wide_c$day)
df_wide_c_pca_bands <- df_wide_c[,c(8:19)] #Columns with bands
df_wide_c_pca_env <- df_wide_c[,c(7,20:24)] #Columns with environmental vars

# Generate PCA objects
pca_bands <- prcomp(df_wide_c_pca_bands, scale=T)
pca_env <- prcomp(df_wide_c_pca_env, scale=T)

# Graph
g_pca_bands <- autoplot(pca_bands,
                        data=df_wide_c,
                        colour="day",
                        loadings = T,
                        loadings.label = T)
g_pca_bands

g_pca_env <- autoplot(pca_env,
                        data=df_wide_c,
                        colour="day",
                        loadings = T,
                        loadings.label = T)
g_pca_env

# Save to file
g_pca <- grid.arrange(g_pca_bands,g_pca_env,nrow=2)
ggsave("preliminary_analysis/g_pca.png",g_pca,width=6,height=8)

##########################
### Summary statistics ###
##########################

# View summary statistics for environmental variables
sumstat_env <- data.frame(
                    "min"=sapply(df_wide_c_pca_env, min),
                    "q1"=sapply(df_wide_c_pca_env, quantile, probs=0.25),
                    "mean"=colMeans(df_wide_c_pca_env),
                    "q3"=sapply(df_wide_c_pca_env, quantile, probs=0.75),
                    "max"=sapply(df_wide_c_pca_env, max))
sumstat_env

# View summary statistics for bands
sumstat_bands <- data.frame(
  "min"=sapply(df_wide_c_pca_bands, min),
  "q1"=sapply(df_wide_c_pca_bands, quantile, probs=0.25),
  "mean"=colMeans(df_wide_c_pca_bands),
  "q3"=sapply(df_wide_c_pca_bands, quantile, probs=0.75),
  "max"=sapply(df_wide_c_pca_bands, max))
sumstat_bands

# Save to file
write.csv(sumstat_env,"preliminary_analysis/sumstat_env.csv")
write.csv(sumstat_bands,"preliminary_analysis/sumstat_bands.csv")






