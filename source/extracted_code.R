## body {background-color:#FFFFFF;}

## h1.title {background-color: #74edfe; padding: 0.5em; color: #000078;font-weight: bold; font-size: 35px;}

## .headers h1 {background-color: #74edfe; padding: 0.5em; color: #000078;font-weight: bold; font-size: 25px;}

## .headers h2 {background-color: #74edfe; padding: 0.5em; color: #000078;font-weight: bold; font-size: 20px;}

## .headers h3 {background-color: #74edfe; padding: 0.5em; color: #000078;font-size: 20px;}

## .headers h4 {background-color: #ddebf7; padding: 0.5em; color: #000078;font-size: 20px;}

## p {background-color: #F8F8F8; padding: 0.5em;text-indent: 15px}

## )


## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(repos = list(CRAN="http://cran.rstudio.com/"))
if (!require('cluster')) install.packages('cluster')
if (!require('Stat2Data')) install.packages('Stat2Data')
if (!require('ggpubr')) install.packages('ggpubr')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('formattable')) install.packages('formattable')
if (!require ('kableExtra')) install.packages('kableExtra')
if (!require ('corrplot')) install.packages('corrplot')
if (!require('fpc')) install.packages('fpc')
if (!require('crayon')) install.packages('crayon')
if (!require('dbscan')) install.packages('dbscan')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('opticskxi')) install.packages('opticskxi', force=TRUE)
if (!require('GGally')) install.packages("GGally")
if (!require('lares')) install.packages("lares")
if (!require('caret')) install.packages("caret")
if (!require('rrcov')) install.packages("rrcov")
if (!require('gt')) install.packages("gt")
if (!require('c50')) install.packages('c50',  repos='http://cran.us.r-project.org', dependencies = T)
library(C50)
library(gt)
library(rrcov)
library(caret)
library(lares)
library(GGally)
library(opticskxi)
library(dbscan)
library(fpc)
library(kableExtra)
library(corrplot)
library(formattable)
library(ggpubr)
library(Stat2Data)
library(RColorBrewer)
library(Stat2Data)
library(cluster)
library(ggplot2)
library(plyr)
library(crayon)
library(tidyverse)
library(grid)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------

# read original data Main DF cancer_df
cancer_df <- read.csv('wdbc.data', header = FALSE, col.names = c('id',	'diagnosis',	'radius_mean',	'texture_mean',	'perimeter_mean',	'area_mean',	'smoothness_mean',	'compactness_mean',	'concavity_mean',	'concave points_mean',	'symmetry_mean',	'fractal_dimension_mean',	'radius_se',	'texture_se',	'perimeter_se',	'area_se',	'smoothness_se',	'compactness_se',	'concavity_se',	'concave points_se',	'symmetry_se',	'fractal_dimension_se',	'radius_worst',	'texture_worst',	'perimeter_worst',	'area_worst',	'smoothness_worst',	'compactness_worst',	'concavity_worst',	'concave points_worst',	'symmetry_worst',	'fractal_dimension_worst'))



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------

# Check first ten rows for preview
kable(cancer_df[0:10,],caption="<strong>Dataset Preview</strong>") %>% kable_classic(full_width=F, html_font = "Cambria") %>%kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive","bordered"),position = "left", font_size = 10) %>% row_spec(0,bold = TRUE, align = "c")

# Let's see data structure in botj summary tables and plot
options("lares.palette" =lares_pal("simple"))
df_str(cancer_df, return = "plot")

# Summary Stats for data
summary1 <- data.frame(unclass(summary(cancer_df[,0:10])),check.names = FALSE)
summary2 <- data.frame(unclass(summary(cancer_df[,11:20])),check.names = FALSE)
summary3 <- data.frame(unclass(summary(cancer_df[,21:31])),check.names = FALSE)

kable(summary1, caption = "<strong>Summary 1 Stats</strong>") %>% kable_classic(full_width=F, html_font = "Cambria") %>%kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive","bordered"),position = "left",font_size = 10) %>% row_spec(0,bold = TRUE, align = "c")

kable(summary2, caption = "<strong>Summary 2 Stats</strong>") %>% kable_classic( full_width=F, html_font = "Cambria") %>%kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive","bordered"),position = "left",font_size = 10)  %>% row_spec(0,bold = TRUE, align = "c")

kable(summary3, caption = "<strong>Summary 3 Stats</strong>") %>% kable_classic(full_width=F, html_font = "Cambria") %>%kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive","bordered"),position = "left",font_size = 10) %>% row_spec(0,bold = TRUE, align = "c") 


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------

# Check for NaN values in the dataframe
nan_values <- apply(cancer_df, 2, function(x) any(is.nan(x)))

# Display columns with NaN values
cols_with_nan <- names(cancer_df)[nan_values]
print(paste("Number of NaN values is", length(cols_with_nan)))



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's see the top 10 variables most correlated to diagnosis variable with a correlation over 0.6

cancer_df %>% corr_var(diagnosis, ceiling = 60, top = 10, subtitle = NA, method ='pearson')+ggtitle("Top 10 Variables Correlated with the Diagnosis Variable", subtitle = "")+scale_fill_manual(values=c('#66c2a5','#fc8d62'))+
        theme(legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid"))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
ggpairs(cancer_df,columns = c('diagnosis',	'concave.points_worst',	'perimeter_worst',	'concave.points_mean'), mapping = aes(color=cancer_df$diagnosis))+ scale_fill_manual(values=c('#66c2a5','#fc8d62'))+scale_colour_manual(values=c('#66c2a5','#fc8d62'))+theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid"))

ggpairs(cancer_df,columns = c('diagnosis',	'radius_worst',	'perimeter_mean',	'area_worst'), mapping = aes(color=cancer_df$diagnosis))+ scale_fill_manual(values=c('#66c2a5','#fc8d62'))+scale_colour_manual(values=c('#66c2a5','#fc8d62'))+theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid"))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
options("lares.palette" = c("#66c2a5" = "black", "#fc8d62" = 'white'))
cancer_df %>% distr(diagnosis, concave.points_worst, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
cancer_df %>% distr(diagnosis, perimeter_worst, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
cancer_df %>% distr(diagnosis, concave.points_mean, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
cancer_df %>% distr(diagnosis, radius_worst, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
cancer_df %>% distr(diagnosis, perimeter_mean, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
cancer_df %>% distr(diagnosis, area_worst, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------

# let's scale the dataset
df_scaled<- scale(cancer_df[,3:32])
df_scaled=as.data.frame(df_scaled)


# Convert to long format
df_long <- df_scaled %>%
  pivot_longer(
    everything(), 
    names_to = "variable", 
    values_to = "value"
  )

colourCount = length(unique(df_long$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Var boxplots
ggplot(df_long, aes(x = variable, y = value, fill =variable)) +
  geom_boxplot() +
  labs(title = "Boxplots & Outliers", x = "Variable", y = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position ='none', 
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid"))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
cancer_df$radius_cat <- cut(cancer_df$radius_mean, breaks = 5, labels = c('tiny', 'small', 'normal', 'medium','large'))

cancer_df$area_worst_cat <- cut(cancer_df$area_worst, breaks = 5, labels = c('tiny', 'small', 'normal', 'medium','large'))

cancer_df[0:10, c("radius_mean", "radius_cat","area_worst","area_worst_cat")]     



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
options("lares.palette" =lares_pal("simple"))
df_str(cancer_df, return = "plot")


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
pca_result <- prcomp(df_scaled)

summary(pca_result)

biplot(pca_result)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
# SVD
svd_result <- svd(df_scaled)

singular_values <- svd_result$d

# Square of singular values
singular_values_squared <- singular_values^2

# Total variance
total_variance <- sum(singular_values_squared)

# Proportion of variance explained by each component
explained_variance <- singular_values_squared / total_variance

# Print the explained variance
print(explained_variance)

d <- svd_result$d
u <- svd_result$u
v <- svd_result$v

# First two components
svd_1 <- svd_result$u[,1] * svd_result$d[1]
svd_2 <- svd_result$u[,2] * svd_result$d[2]

# Dataframe for plot
df_svd <- data.frame(svd_1 = svd_1, svd_2 = svd_2)

df_svd$diagnosis <- cancer_df$diagnosis

ggplot(df_svd, aes(x = svd_1, y = svd_2, color = diagnosis)) +
  geom_point() +
  labs(title = "SVD - First Two Singular Components",
       x = "First Singular Component",
       y = "FSecond Singular Component") +
  theme_minimal()


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
# Set the range of number of clusters to consider
library(cluster)
library(factoextra)

set.seed(7)

# Set the desired number of clusters
k <- 5

# Perform PAM clustering
pam_result <- pam(df_scaled, k = k, diss = TRUE)

# Use fviz_nbclust() to determine the optimal number of clusters
nbclust_result <- fviz_nbclust(df_scaled, pam, method = "silhouette")

# Plot the result
print(nbclust_result)



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------

library(cluster)
library(factoextra)
library(ggplot2)
library(ggpubr)

set.seed(7)

# Compute the distance matrix using the Euclidean distance metric
distance_matrix <- dist(df_scaled, method = "euclidean")

# Perform k-medoids clustering
fit2 <- pam(distance_matrix, k = 2)

y_cluster2 <- fit2$clustering

plot15 <- ggplot(cancer_df, aes(x = concave.points_worst, y = perimeter_worst, fill = diagnosis)) +
  geom_point(color = "black", shape = 21, size = 3) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"),
                    labels = c("Benign", "Malignant")) +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid")) +
  ggtitle("Real Classification")

# Add the cluster assignments to the original dataframe
cancer_df1 <- cancer_df
cancer_df1$Cluster <- y_cluster2

# Create a scatter plot with ggplot2
plot16 <- ggplot(cancer_df1, aes(x = concave.points_worst, y = perimeter_worst, fill = factor(-Cluster))) +
  geom_point(color = "black", shape = 21, size = 3) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"),
                    labels = c("Benign", "Malignant")) +
  labs(x = "concave.points_worst", y = "perimeter_worst", color = "Cluster") +
  ggtitle("K-medoids Clustering (k = 2)") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid"))

figure_4 <- ggarrange(plot15 + rremove("xlab"), plot16,
                      ncol = 1, nrow = 2,
                      common.legend = TRUE,
                      align = 'hv',
                      legend = "bottom")

annotate_figure(figure_4, top = text_grob('Comparison: Real Classification vs K-medoids/Euclidean\nVariable: concave.points_worst -- perimeter_worst',
                                          color = "black", 
                                          face = "bold", size = 10))




## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------

# Create label column in the dataframe to compare with the actual diagnosis
for (i in 1:length(cancer_df1$diagnosis)) {
  if (cancer_df1$Cluster[i] == 1) {
    cancer_df1$label[i] <- "M"
  } else {
    cancer_df1$label[i] <- "B"
  }
}

# Using caret package for the evaluation

# Convert the diagnosis and label columns to factors with the same levels
cancer_df1$diagnosis <- factor(cancer_df1$diagnosis, levels = c("B", "M"))
cancer_df1$label <- factor(cancer_df1$label, levels = c("B", "M"))

# Create a confusion matrix object
confusion_obj1 <- confusionMatrix(cancer_df1$label, cancer_df1$diagnosis)

# Print the formatted confusion matrix and performance statistics
cat("Matriz de confusión para el Modelo Kmedoids-Euclidean\n\n")
print(confusion_obj1)

# Save the differente metrics to compare later
kmedoids_model1_precision <- confusion_obj1$byClass["Pos Pred Value"]
kmedoids_model1_accuracy <- confusion_obj1$overall['Accuracy']
kmedoids_model1_sensitivity <- confusion_obj1$byClass["Sensitivity"]
kmedoids_model1_fmeasure <- confusion_obj1$byClass["F1"]



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------

library(cluster)
library(factoextra)
library(ggplot2)
library(ggpubr)

set.seed(7)

# Compute the distance matrix using the Euclidean distance metric
distance_matrix <- dist(df_scaled, method = "manhattan")

# Perform k-medoids clustering
fit2 <- pam(distance_matrix, k = 2)

y_cluster2 <- fit2$clustering

plot15 <- ggplot(cancer_df, aes(x = concave.points_worst, y = perimeter_worst, fill = diagnosis)) +
  geom_point(color = "black", shape = 21, size = 3) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"),
                    labels = c("Benign", "Malignant")) +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid")) +
  ggtitle("Real Classification")

# Add the cluster assignments to the original dataframe
cancer_df2 <- cancer_df
cancer_df2$Cluster <- y_cluster2

# Create a scatter plot with ggplot2
plot16 <- ggplot(cancer_df2, aes(x = concave.points_worst, y = perimeter_worst, fill = factor(-Cluster))) +
  geom_point(color = "black", shape = 21, size = 3) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"),
                    labels = c("Benign", "Malignant")) +
  labs(x = "concave.points_worst", y = "perimeter_worst", color = "Cluster") +
  ggtitle("K-medoids Clustering (k = 2)") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid"))

figure_4 <- ggarrange(plot15 + rremove("xlab"), plot16,
                      ncol = 1, nrow = 2,
                      common.legend = TRUE,
                      align = 'hv',
                      legend = "bottom")

annotate_figure(figure_4, top = text_grob('Comparison: Real Classification vs K-medoids/Mahattan\nVariable: concave.points_worst -- perimeter_worst',
                                          color = "black", 
                                          face = "bold", size = 10))



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------


# Create label column in the dataframe to compare with the actual diagnosis
for (i in 1:length(cancer_df2$diagnosis)) {
  if (cancer_df2$Cluster[i] == 1) {
    cancer_df2$label[i] <- "M"
  } else {
    cancer_df2$label[i] <- "B"
  }
}

# Using caret package for the evaluation

# Convert the diagnosis and label columns to factors with the same levels
cancer_df2$diagnosis <- factor(cancer_df2$diagnosis, levels = c("B", "M"))
cancer_df2$label <- factor(cancer_df2$label, levels = c("B", "M"))

# Create a confusion matrix object
confusion_obj2 <- confusionMatrix(cancer_df2$label, cancer_df2$diagnosis)

# Print the formatted confusion matrix and performance statistics
cat("Matriz de confusión para el Modelo Kmedoids-Manhattan\n\n")
print(confusion_obj2)

# Save the differente metrics to compare later
kmedoids_model2_precision <- confusion_obj2$byClass["Pos Pred Value"]
kmedoids_model2_accuracy <- confusion_obj2$overall['Accuracy']
kmedoids_model2_sensitivity <- confusion_obj2$byClass["Sensitivity"]
kmedoids_model2_fmeasure <- confusion_obj2$byClass["F1"]


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------

# Create a data frame with the evaluation metrics

metrics <- data.frame(
  Model = c("Model1/Euclidean", "Model2/Manhattan"),
  Precision = c(kmedoids_model1_precision, kmedoids_model2_precision),
  Accuracy = c(kmedoids_model1_accuracy, kmedoids_model2_accuracy),
  Sensitivity = c(kmedoids_model1_sensitivity, kmedoids_model2_sensitivity),
  F_measure = c(kmedoids_model1_fmeasure, kmedoids_model2_fmeasure),
  stringsAsFactors = FALSE
)
# Round the numbers to two decimal places in the metrics dataframe
metrics$Precision <- round(metrics$Precision, 2)
metrics$Accuracy <- round(metrics$Accuracy, 2)
metrics$Sensitivity <- round(metrics$Sensitivity, 2)
metrics$F_measure <- round(metrics$F_measure, 2)

# Calculate the percentage difference of each metric based on the existing values in the metrics dataframe
metrics_diff <- data.frame(
  Model = "Difference %",
  Precision = round((metrics$Precision[2] - metrics$Precision[1]) / metrics$Precision[1] * 100, 2),
  Accuracy = round((metrics$Accuracy[2] - metrics$Accuracy[1]) / metrics$Accuracy[1] * 100, 2),
  Sensitivity = round((metrics$Sensitivity[2] - metrics$Sensitivity[1]) / metrics$Sensitivity[1] * 100, 2),
  F_measure = round((metrics$F_measure[2] - metrics$F_measure[1]) / metrics$F_measure[1] * 100, 2),
  stringsAsFactors = FALSE
)

# Append the difference row to the metrics data frame
metrics <- rbind(metrics, metrics_diff)

styled_table <- kable(metrics, caption = "<strong>Comparación del modelo No Supervisado con métricas diferentes</strong>") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive", "bordered"), position = "left", font_size = 10)%>%
  row_spec(0, bold = TRUE, align = "c") %>%
  row_spec(nrow(metrics), bold = TRUE, background = "lightgray")

styled_table



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
library(dbscan)
library(cluster)
library(factoextra)

# Set the range of epsilon values to consider
eps_values <- seq(0.1, 10, by = 0.1)

# Initialize vectors to store evaluation results
silhouette_scores <- vector("numeric", length = length(eps_values))
num_outliers <- vector("numeric", length = length(eps_values))

# Perform DBSCAN for different epsilon values
for (i in 1:length(eps_values)) {
  eps <- eps_values[i]
  
  # Apply DBSCAN
  dbscan_result <- dbscan(df_scaled, eps = eps, minPts = 10)
  
  # Check if valid clusters are present
  if (max(dbscan_result$cluster) > 0) {
    # Calculate the silhouette score
    silhouette_scores[i] <- mean(silhouette(dbscan_result$cluster, dist(df_scaled)))
  } else {
    # If no valid clusters, assign NA to silhouette score
    silhouette_scores[i] <- NA
  }
  
  # Count the number of outliers (noise points)
  num_outliers[i] <- sum(dbscan_result$cluster == 0)
}

# Find the index of the maximum silhouette score
max_silhouette_index <- which.max(silhouette_scores)
max_silhouette_eps <- eps_values[max_silhouette_index]
max_silhouette_score <- silhouette_scores[max_silhouette_index]

# Plot the evaluation results
plot(eps_values, silhouette_scores, type = "l",
     xlab = "Epsilon (eps)", ylab = "Silhouette Score",
     main = "DBSCAN: Silhouette Score vs. Epsilon")
points(max_silhouette_eps, max_silhouette_score, col = "red", pch = 16)
text(max_silhouette_eps, max_silhouette_score, 
     labels = paste("Max Silhouette Score:\nEpsilon =", round(max_silhouette_eps, 2)), 
     pos = 1)

plot(eps_values, num_outliers, type = "l",
     xlab = "Epsilon (eps)", ylab = "Number of Outliers",
     main = "DBSCAN: Number of Outliers vs. Epsilon")




## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
library(cluster)
library(factoextra)
library(ggplot2)
library(ggpubr)

set.seed(7)

# Perform DBSCAN clustering with epsilon = 2.65
dbscan_result <- dbscan(df_scaled, eps = 2.65, minPts = 10)

y_cluster_dbscan <- dbscan_result$cluster

plot17 <- ggplot(cancer_df, aes(x = concave.points_worst, y = perimeter_worst, fill = diagnosis)) +
  geom_point(color = "black", shape = 21, size = 3) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"),
                    labels = c("Benign", "Malignant")) +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid")) +
  ggtitle("Real Classification")

# Add the cluster assignments to the original dataframe
cancer_df_dbscan <- cancer_df
cancer_df_dbscan$Cluster <- -y_cluster_dbscan

# Create a scatter plot with ggplot2
plot18 <- ggplot(cancer_df_dbscan, aes(x = concave.points_worst, y = perimeter_worst, fill = factor(Cluster))) +
  geom_point(color = "black", shape = 21, size = 3) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f"),
                    labels = c("Noise", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
  labs(x = "concave.points_worst", y = "perimeter_worst", color = "Cluster") +
  ggtitle("DBSCAN Clustering (epsilon = 2.5)") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid"))

figure_5 <- ggarrange(plot17 + rremove("xlab"), plot18,
                      ncol = 1, nrow = 2,
                      common.legend = TRUE,
                      align = 'hv',
                      legend = "bottom")

annotate_figure(figure_5, top = text_grob('Comparison: Real Classification vs DBSCAN/Epsilon=2.65\nVariable: concave.points_worst -- perimeter_worst',
                                          color = "black", 
                                          face = "bold", size = 10))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
# Create label column in the dataframe to compare with the actual diagnosis
for (i in 1:length(cancer_df_dbscan$Cluster)) {
  if (cancer_df_dbscan$Cluster[i] == -1) {
    cancer_df_dbscan$label[i] <- "B"
  } else {
    cancer_df_dbscan$label[i] <- "M"
  }
}

# Using caret package for the evaluation

# Convert the diagnosis and label columns to factors with the same levels
cancer_df_dbscan$diagnosis <- factor(cancer_df_dbscan$diagnosis, levels = c("B", "M"))
cancer_df_dbscan$label <- factor(cancer_df_dbscan$label, levels = c("B", "M"))

# Create a confusion matrix object
confusion_obj_dbscan <- confusionMatrix(cancer_df_dbscan$label, cancer_df_dbscan$diagnosis)

# Print the formatted confusion matrix and performance statistics
cat("Confusion Matrix for DBSCAN Clustering (epsilon = 2.65)\n\n")
print(confusion_obj_dbscan)

# Save the different metrics to compare later
dbscan_model_precision <- confusion_obj_dbscan$byClass["Pos Pred Value"]
dbscan_model_accuracy <- confusion_obj_dbscan$overall['Accuracy']
dbscan_model_sensitivity <- confusion_obj_dbscan$byClass["Sensitivity"]
dbscan_model_fmeasure <- confusion_obj_dbscan$byClass["F1"]



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(287)  # For reproducibility

# Remove the first two columns from the dataframe
X1 <- cancer_df %>% select(-c(1, 2))
y1 <- cancer_df$diagnosis

split_prop <- 3
indexes <- sample(1:nrow(cancer_df), size = floor(((split_prop - 1) / split_prop) * nrow(cancer_df)))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(287)

trainX1 <- X1[indexes, ]
trainy1 <- y1[indexes]
testX1 <- X1[-indexes, ]
testy1 <- y1[-indexes]

trainy1 <-  as.factor(trainy1)
model <- C50::C5.0(trainX1, trainy1,rules=TRUE )
summary(model)



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------

model <- C50::C5.0(trainX1, trainy1)
plot(model,gp = gpar(fontsize = 8))



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------

# Calculate performance metrics for the classification tree model
predicted_model <- predict(model, testX1, type = "class")

# Convert the predicted values to character
predicted_model <- as.character(predicted_model)

# Create a factor with the levels and testy1
predicted_model <- factor(predicted_model)
testy1 <- factor(testy1)

# Create a confusion matrix object
confusion_obj_tree <- confusionMatrix(predicted_model, testy1)

# Print the formatted confusion matrix
cat("Confusion Matrix for Classification Tree Model\n\n")
print(confusion_obj_tree)

# Save the different metrics to compare later
tree_model_precision <- confusion_obj_tree$byClass["Pos Pred Value"]
tree_model_accuracy <- confusion_obj_tree$overall["Accuracy"]
tree_model_sensitivity <- confusion_obj_tree$byClass["Sensitivity"]
tree_model_fmeasure <- confusion_obj_tree$byClass["F1"]



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest)

# Set the seed for reproducibility
set.seed(287)

# Split the data into training and testing sets
split_prop <- 3
indexes <- sample(1:nrow(cancer_df), size = floor(((split_prop - 1) / split_prop) * nrow(cancer_df)))

trainX1 <- X1[indexes, ]
trainy1 <- y1[indexes]
testX1 <- X1[-indexes, ]
testy1 <- y1[-indexes]

# Convert the target variable to a factor
trainy1 <- as.factor(trainy1)

# Train the Random Forest model
model_rf <- randomForest(trainX1, trainy1)

# Make predictions on the test set
predicted_rf <- predict(model_rf, testX1)

# Convert the predicted values to character and create factors
predicted_rf <- as.factor(as.character(predicted_rf))
testy1 <- as.factor(as.character(testy1))

# Create a confusion matrix object
confusion_obj_rf <- confusionMatrix(predicted_rf, testy1)

# Print the formatted confusion matrix
cat("Confusion Matrix for Random Forest Model\n")
print(confusion_obj_rf)

# Save the different metrics to compare later
rf_model_precision <- confusion_obj_rf$byClass["Pos Pred Value"]
rf_model_accuracy <- confusion_obj_rf$overall["Accuracy"]
rf_model_sensitivity <- confusion_obj_rf$byClass["Sensitivity"]
rf_model_fmeasure <- confusion_obj_rf$byClass["F1"]



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a data frame with the metrics for each model
metrics <- data.frame(
  Model = c("DBSCAN", "Classification Tree", "K-Medoids Model/Euclidean", "K-Medoids Model/Manhattan", "Random Forest"),
  Precision = c(dbscan_model_precision, tree_model_precision, kmedoids_model1_precision, kmedoids_model2_precision,rf_model_precision),
  Accuracy = c(dbscan_model_accuracy, tree_model_accuracy, kmedoids_model1_accuracy, kmedoids_model2_accuracy,rf_model_accuracy),
  Sensitivity = c(dbscan_model_sensitivity, tree_model_sensitivity, kmedoids_model1_sensitivity, kmedoids_model2_sensitivity,rf_model_sensitivity),
  F_measure = c(dbscan_model_fmeasure, tree_model_fmeasure, kmedoids_model1_fmeasure, kmedoids_model2_fmeasure,rf_model_fmeasure),
  stringsAsFactors = FALSE
)

# Round the numbers to two decimal places in the metrics dataframe
metrics$Precision <- round(metrics$Precision, 2)
metrics$Accuracy <- round(metrics$Accuracy, 2)
metrics$Sensitivity <- round(metrics$Sensitivity, 2)
metrics$F_measure <- round(metrics$F_measure, 2)

# Find the column index of the highest value in each column
highlight_cols <- apply(metrics[-1], 2, function(x) ifelse(x == max(x), "background-color: yellow", ""))

# Create a styled table using the kableExtra package
styled_table <- kable(metrics, caption = "<strong>Comparación de los modelos</strong>") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive", "bordered"), position = "left", font_size = 10) %>%
  row_spec(0, bold = TRUE, align = "c") %>%
  row_spec(nrow(metrics), bold = TRUE, background = "lightgray")

# Apply cell highlighting to each column based on the highest value
for (i in 2:ncol(metrics)) {
  styled_table <- styled_table %>%
    column_spec(i, color = ifelse(highlight_cols[[i - 1]] != "", "black", "inherit"),
                background = highlight_cols[[i - 1]])
}

styled_table


