
<p align="center">
<img src=https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/2dc18629-2010-4b20-9c6b-0daa302c37f1.png width="250px">
</p>

---

# R Data Science BreastCancer-Wisconsin
Data Anlalysis with R of “Breast Cancer Wisconsin (Diagnostic)” Dataset


# Selection and Preparation of a Data Set

## Problem Statement

Breast cancer is one of the most common forms of cancer worldwide and represents a serious public health problem. Every year, millions of new cases are diagnosed, and despite significant advances in diagnosis and treatment, the disease remains a major cause of mortality among women.

This project focuses on the analysis of a dataset from the "Breast Cancer Wisconsin (Diagnostic)" available at the UCI Machine Learning repository (https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29). This dataset includes features calculated from digitized images of fine needle aspiration (FNA) of breast masses. The features describe the properties of the cell nuclei present in the images, which can provide valuable information for the diagnosis of breast cancer.

The main objective of this study is to use data mining and machine learning techniques to develop a predictive model that can accurately classify whether a breast tumor is benign or malignant based on these features. This model could have a range of potential applications, from helping doctors make informed decisions to developing decision support systems in medicine.

In addition to this main objective, we will also explore the data to identify patterns and relationships that can provide a greater understanding of the nature of breast cancer. Specifically, we will perform exploratory data analysis, data preprocessing and cleaning, variable discretization, principal component analysis, and finally, build and evaluate our predictive model.

At the end of this study, we hope not only to have developed an effective predictive model, but also to have gained a deeper insight into the features that influence the diagnosis of breast cancer.

Justification for the Selection of the Data Set {.headers}
The "Breast Cancer Wisconsin (Diagnostic)" dataset is ideal for this project due to its clinical relevance, high quality, and diversity of variables. Breast cancer is one of the leading causes of death in women worldwide, so creating predictive models for its diagnosis has significant social impact.

This dataset, with 569 observations and a variety of cell nucleus features, offers the opportunity to apply various data mining techniques, such as data preprocessing, exploratory analysis, discretization, and dimensionality reduction. Therefore, this dataset is not only aligned with the project's objectives but is also manageable in terms of size and complexity, facilitating the development and evaluation of predictive models.

General Description of the Data {.headers}
The "Breast Cancer Wisconsin (Diagnostic)" dataset contains 569 records, each with 32 attributes. Each record represents a digital image of a fine needle aspiration (FNA) of a breast mass. The features are calculated from a digitized image of an FNA and describe characteristics of the cell nuclei present in the image.

The attributes include a unique identifier for each image, the diagnosis (malignant or benign), and 30 real-valued features that are measurements obtained from the image. These features are the radius, texture, perimeter, area, smoothness, compactness, concavity, concave points, symmetry, and fractal dimension, each with its mean, standard error, and the worst (or largest) value recorded.

In terms of the project's requirements, the dataset meets all criteria: it has over 500 observations, at least 5 numerical variables, 2 categorical, and 1 binary.

To meet the requirement of two categorical variables, we can discretize two of the numerical variables. The variables that can be discretized can be chosen after careful exploration and analysis of the data. An example could be the discretization of the "radius_mean" and "texture_mean" variables into categories such as "low", "medium", and "high" based on their respective tertiles. The discretization process will be carried out during the preprocessing and analysis of the data, using appropriate techniques such as binarization and interval cutting in R.

The reason why each feature appears three times in the dataset is that for each feature, the dataset includes:

➤ The mean value of the feature calculated from the image.

➤The standard error of the feature calculated from the image.

➤ The "worst" or highest value of the feature calculated from the image.

Therefore, for each feature, you have three different measurements: the mean, the standard error, and the worst value. This provides a more complete description of the variability of each feature in the breast mass image.

The "worst" value is calculated by taking the average of the three largest values of that particular feature.

The attributes are as follows:

| Attribute | Description | Data Type |
| :--- | :--- | :---: |
| id | Identification number | Integer |
| diagnosis | Diagnosis (M = malignant, B = benign) | Categorical |
| radius_mean | Mean of distances from center to points on the perimeter | Numerical |
| texture_mean | Standard deviation of gray-scale values | Numerical |
| perimeter_mean | Mean of perimeter | Numerical |
| area_mean | Mean of area | Numerical |
| smoothness_mean | Local variation in radius lengths | Numerical |
| compactness_mean | Perimeter^2 / area - 1.0 | Numerical |
| concavity_mean | Severity of concave portions of the contour | Numerical |
| concave points_mean | Number of concave portions of the contour | Numerical |
| symmetry_mean | Mean of symmetry | Numerical |
| fractal_dimension_mean | "Coastline approximation" - 1 | Numerical |
| radius_se | Standard error for the mean of distances from center to points on the perimeter | Numerical |
| texture_se | Standard error for the standard deviation of gray-scale values | Numerical |
| perimeter_se | Standard error for the mean of perimeter | Numerical |
| area_se | Standard error for the mean of area | Numerical |
| smoothness_se | Standard error for the local variation in radius lengths | Numerical |
| compactness_se | Standard error for the perimeter^2 / area - 1.0 | Numerical |
| concavity_se | Standard error for the severity of concave portions of the contour | Numerical |
| concave points_se | Standard error for the number of concave portions of the contour | Numerical |
| symmetry_se | Standard error for the mean of symmetry | Numerical |
| fractal_dimension_se | Standard error for the "coastline approximation" - 1 | Numerical |
| radius_worst | Worst or largest mean value for the mean of distances from center to points on the perimeter | Numerical |
| texture_worst | Worst or largest mean value for the standard deviation of gray-scale values | Numerical |
| perimeter_worst | Worst or largest mean value for the mean of perimeter | Numerical |
| area_worst | Worst or largest mean value for the mean of area | Numerical |
| smoothness_worst | Worst or largest mean value for the local variation in radius lengths | Numerical |
| compactness_worst | Worst or largest mean value for the perimeter^2 / area - 1.0 | Numerical |
| concavity_worst | Worst or largest mean value for the severity of concave portions of the contour | Numerical |
| concave points_worst | Worst or largest mean value for the number of concave portions of the contour | Numerical |
| symmetry_worst | Worst or largest mean value for the mean of symmetry | Numerical |
| fractal_dimension_worst | Worst or largest mean value for the "coastline approximation" - 1 | Numerical |

<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

## Analytical Objectives

Following the CRISP-DM methodology, the objectives of this analysis are multiple and align with the stages of this standard process for data mining.

First, in the business understanding stage, the objective is to understand the domain of breast cancer and the importance of accurate diagnosis. This includes understanding how features in the images of the cell nuclei are obtained and measured, and how these features might be related to the diagnosis of benignity or malignancy.

In the data understanding stage, the goal is to explore and familiarize oneself with the "Breast Cancer Wisconsin (Diagnostic)" dataset, understanding the distribution, relationship, and quality of the data.

For the data preparation stage, the objective is to clean and transform the data for analysis. This will include discretizing variables to create two new categorical variables, as required for the project.

In the modeling stage, the goal is to build and evaluate various supervised learning models to predict the diagnosis based on the features of the images. This might involve techniques such as logistic regression, decision trees, and support vector machines.

Finally, in the evaluation stage, the objective is to assess the accuracy and relevance of the models, and select the best model for interpretation and presentation of the results.

<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

<strong> Requirements </strong>

```r {r message= FALSE, warning=FALSE}
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
```
<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

## Exploratory Data Analysis

### Data Loading

We load the data from the csv file wdbc.data downloaded from (https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29) and create the dataframe cancer_df

```r {r message= FALSE, warning=FALSE}

# read original data Main DF cancer_df
cancer_df <- read.csv('wdbc.data', header = FALSE, col.names = c('id',	'diagnosis',	'radius_mean',	'texture_mean',	'perimeter_mean',	'area_mean',	'smoothness_mean',	'compactness_mean',	'concavity_mean',	'concave points_mean',	'symmetry_mean',	'fractal_dimension_mean',	'radius_se',	'texture_se',	'perimeter_se',	'area_se',	'smoothness_se',	'compactness_se',	'concavity_se',	'concave points_se',	'symmetry_se',	'fractal_dimension_se',	'radius_worst',	'texture_worst',	'perimeter_worst',	'area_worst',	'smoothness_worst',	'compactness_worst',	'concavity_worst',	'concave points_worst',	'symmetry_worst',	'fractal_dimension_worst'))

```
<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

### Data Summary

In this section, the structure of the dataset is presented both in tables and in graphs.

```r {r message= FALSE, warning=FALSE}

# Check first ten rows for preview
kable(cancer_df[0:10,],caption="<strong>Dataset Preview</strong>") %>% kable_classic(full_width=F, html_font = "Cambria") %>%kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive","bordered"),position = "left", font_size = 10) %>% row_spec(0,bold = TRUE, align = "c")
```
![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/dd0af54b-69c8-4788-9fd8-bcd87e371602)

```r {r message= FALSE, warning=FALSE}
# Let's see data structure in both summary tables and plot
options("lares.palette" =lares_pal("simple"))
df_str(cancer_df, return = "plot")
```

![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/5a230872-5ae4-46ca-a2ee-f623022d8ce1)

```r
# Summary Stats for data
summary1 <- data.frame(unclass(summary(cancer_df[,0:10])),check.names = FALSE)
summary2 <- data.frame(unclass(summary(cancer_df[,11:20])),check.names = FALSE)
summary3 <- data.frame(unclass(summary(cancer_df[,21:31])),check.names = FALSE)

kable(summary1, caption = "<strong>Summary 1 Stats</strong>") %>% kable_classic(full_width=F, html_font = "Cambria") %>%kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive","bordered"),position = "left",font_size = 10) %>% row_spec(0,bold = TRUE, align = "c")
```
![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/c096c3a5-e902-4d08-a784-5f560e1a1ee9)

```r
kable(summary2, caption = "<strong>Summary 2 Stats</strong>") %>% kable_classic( full_width=F, html_font = "Cambria") %>%kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive","bordered"),position = "left",font_size = 10)  %>% row_spec(0,bold = TRUE, align = "c")
```

![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/ac0362a8-f3e4-42e8-9b0e-df839e68482e)

```r
kable(summary3, caption = "<strong>Summary 3 Stats</strong>") %>% kable_classic(full_width=F, html_font = "Cambria") %>%kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive","bordered"),position = "left",font_size = 10) %>% row_spec(0,bold = TRUE, align = "c") 
```

![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/23181322-e08d-4e72-8be9-9603f6f3f9e4)

We can see that there are no missing or invalid values in the dataset; the 'diagnosis' column takes binary values of malignant 'M' or benign 'B'. This will be used in the next stage of the project to validate the different classification or regression models that are relevant to evaluate.

In the discretization section, we will decide which variables to discretize in order to have categorical variables.

<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

### Checking for Invalid Values

Similarly, to ensure we meet the requirement of checking for invalid NaN values:"

```r {r message= FALSE, warning=FALSE}

# Check for NaN values in the dataframe
nan_values <- apply(cancer_df, 2, function(x) any(is.nan(x)))

# Display columns with NaN values
cols_with_nan <- names(cancer_df)[nan_values]
print(paste("Number of NaN values is", length(cols_with_nan)))
```
```
## [1] "Number of NaN values is 0"
```

<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

### Exploration of Variables

In this section, we will see the distribution of the different variables. As this is a dataset with 31 numerical variables, in this section, to facilitate the studies of distribution and correlation, we will initially select those that have significant correlation, although for the PCA (Principal Component Analysis) and SVD (Singular Value Decomposition) studies, the entire dataset will be taken into account.

```r {r message= FALSE, warning=FALSE}
# Let's see the top 10 variables most correlated to diagnosis variable with a correlation over 0.6

cancer_df %>% corr_var(diagnosis, ceiling = 60, top = 10, subtitle = NA, method ='pearson')+ggtitle("Top 10 Variables Correlated with the Diagnosis Variable", subtitle = "")+scale_fill_manual(values=c('#66c2a5','#fc8d62'))+
        theme(legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid"))
```

![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/4eaec7fa-daba-4abe-b21d-5317350f8085)


<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

Let's look at the correlation and bivariate pair distribution of the first six variables:

```r {r message= FALSE, warning=FALSE}
ggpairs(cancer_df,columns = c('diagnosis',	'concave.points_worst',	'perimeter_worst',	'concave.points_mean'), mapping = aes(color=cancer_df$diagnosis))+ scale_fill_manual(values=c('#66c2a5','#fc8d62'))+scale_colour_manual(values=c('#66c2a5','#fc8d62'))+theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid"))
```
![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/8f59f7b4-4024-4364-8309-8ee55c977361)

```r
ggpairs(cancer_df,columns = c('diagnosis',	'radius_worst',	'perimeter_mean',	'area_worst'), mapping = aes(color=cancer_df$diagnosis))+ scale_fill_manual(values=c('#66c2a5','#fc8d62'))+scale_colour_manual(values=c('#66c2a5','#fc8d62'))+theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "#f6f1eb", 
                                        colour = "#f6f1eb", 
                                        size = 0.5, linetype = "solid"))
```
![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/5d8c5112-5378-4026-a883-9bb710626543)

<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

With this, we can get an idea of the most important variables for the correct diagnosis using the data. However, we should compare this with what we obtain from the PCA (Principal Component Analysis) and SVD (Singular Value Decomposition) studies to see if it corresponds.

Another important visualization from the 'lares' package that we can use is the distribution and proportions:

```r {r message= FALSE, warning=FALSE}
options("lares.palette" = c("#66c2a5" = "black", "#fc8d62" = 'white'))
cancer_df %>% distr(diagnosis, concave.points_worst, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
```

![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/2c45d823-0dea-4996-93aa-281698392db6)

```r
cancer_df %>% distr(diagnosis, perimeter_worst, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
```
![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/3edd385c-e664-4412-b307-88c28f8a80ac)

```r
cancer_df %>% distr(diagnosis, concave.points_mean, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
```

![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/4dc62dc3-1547-4bb7-b3e1-a10fa1b31843)

```r
cancer_df %>% distr(diagnosis, radius_worst, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
```

![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/4fb2db67-0257-4bca-8164-36bd404196cc)

```r
cancer_df %>% distr(diagnosis, perimeter_mean, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
```

![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/20142665-f37a-4f88-9717-245b6c877b85)

```r
cancer_df %>% distr(diagnosis, area_worst, breaks = 5)+theme(axis.text.x = element_text(size=10, angle=45))
```

![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/a8358531-9812-4827-ab10-7cfd52f5690e)

In general, it can be observed that some variables classify the diagnosis into the two groups better than others. Also, from this tool, we see how we can discretize certain variables; in this case, we use 5 intervals.

<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

### Identification of Outliers

```r {r message= FALSE, warning=FALSE}

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
```
![image](https://github.com/jlgarciatucci/R-Data-Science-Breast-Cancer-Wisconsin/assets/98712473/15926a52-bac2-42ca-83be-e2cb5e0da992)


<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

## Discretization

To discretize the variables, we will use the 'cut' method. We will take 'radius_mean' and 'area_worst' into 5 categories.

```r {r message= FALSE, warning=FALSE}
cancer_df$radius_cat <- cut(cancer_df$radius_mean, breaks = 5, labels = c('tiny', 'small', 'normal', 'medium','large'))

cancer_df$area_worst_cat <- cut(cancer_df$area_worst, breaks = 5, labels = c('tiny', 'small', 'normal', 'medium','large'))

cancer_df[0:10, c("radius_mean", "radius_cat","area_worst","area_worst_cat")]     

```
<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

Let's now look at the structure of the dataset:

```r {r message= FALSE, warning=FALSE}
options("lares.palette" =lares_pal("simple"))
df_str(cancer_df, return = "plot")
```
<a style="display: block; text-align: right;" href="#top"> ↑ Back to Top </a>

## PCA (Principal Component Analysis)

We will apply PCA (Principal Component Analysis) and evaluate the cumulative proportion and proportion of variance of the components to see which components best describe the data for dimensionality reduction.

```r {r message= FALSE, warning=FALSE}
pca_result <- prcomp(df_scaled)

summary(pca_result)

biplot(pca_result)
```

It can be observed that up to component 15, a cumulative proportion of 98% is already represented, so the dimensionality can be reduced.

<a href="#top" style="display: block; text-align: right;">↑ Back to top</a>

## SVD

In this section, we will apply the SVD (Singular Value Decomposition) method.

```r {r message= FALSE, warning=FALSE}
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
```

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

## Unsupervised Model

For the distance-based unsupervised model, we will use K-Medoids (PAM). As we initially must assume that we do not know the number of groups or clusters to classify our data, we are going to use the silhouette analysis method.

### Silhouette Analysis of K-Medoids Clustering

```r {r message= FALSE, warning=FALSE}
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

```

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

We can conclude that the optimal number of clusters is two. Using the Silhouette analysis method, the point with the highest average silhouette width represents the optimal number of clusters, which is two.

### K-Medoids Model with "Euclidean" Distance Metric

As our first unsupervised model, we will perform classification using K-medoids with the "Euclidean" distance metric, which is the default metric.

$$
\scriptsize \text{Where the distance between two points is calculated:}\\
\scriptsize d((x_1,x_2),(y_1,y_2)) = \scriptsize\sqrt{{(x_2 - x_1)^2 + (y_2 - y_1)^2}}
$$

"Let's visually examine the classification made by the model compared to the actual classification using the two most correlated variables, concave.points_worst and perimeter_worst:


```r {r message= FALSE, warning=FALSE}

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


```

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

### Evaluation of the K-Medoids Model with "Euclidean" Distance Metric

In this section, we will evaluate the K-medoids model using the Euclidean distance metric. For this purpose, using the caret package, we will calculate the confusion matrix and the following evaluation metrics:

$$
\scriptsize \text {Confusion Matrix}\\
\begin{array}{cc|c}
 & \scriptsize \text {Negative Prediction} & \scriptsize \text{Postive Prediction}\\
\hline
\scriptsize \text{Real Value Negative} & \scriptsize TN & \scriptsize FP \\
\scriptsize \text{Real Value Positive} & \scriptsize FN & \scriptsize TP \\
\end{array}
$$


$$
\scriptsize \text{Positive Predicted Value (Precision)} = \frac{{\scriptsize \text{True Positives}}}{{\scriptsize \text{True Positives} + \scriptsize \text{False Positives}}}
$$



$$
\scriptsize \text{Accuracy} = \frac{{\scriptsize \text{Number of Correct Predictions}}}{{\scriptsize \text{Total Number of Predictions}}}
$$


$$
\scriptsize \text{Sensitivity} = \frac{{\scriptsize \text{True Positives}}}{{\scriptsize \text{True Positives} + \scriptsize \text{False Negatives}}}
$$


$$
\scriptsize \text{F-measure} = 2 \times \frac{{\scriptsize \text{Precision} \times \scriptsize \text{Sensitivity}}}{{\scriptsize \text{Precision} + \scriptsize \text{Sensitivity}}}
$$

```r {r message= FALSE, warning=FALSE}

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

```

**K-Medoids Model with "Euclidean" Distance Metric**<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;We can see that the model has:<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Precision: **&nbsp;&nbsp;`r round(kmedoids_model1_precision, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Accuracy: **`r round(kmedoids_model1_accuracy, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Sensitivity: **&nbsp;&nbsp;`r round(kmedoids_model1_sensitivity, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**F-Measure: **&nbsp;&nbsp;`r round(kmedoids_model1_fmeasure, digits=4)`<br>

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

### K-Medoids Model with "Manhattan" Distance Metric

The Manhattan Distance Metric:

$$
\displaystyle \vert u-v\vert = \sum_{i=1}^n \vert u_i-v_i\vert
$$
```r {r message= FALSE, warning=FALSE}

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

```
<a style="display: block; text-align: right;"href="#top"> ↑ Volver al inicio </a>


### Evaluation of the K-Medoids Model with "Manhattan" Distance Metric

```r {r message= FALSE, warning=FALSE}


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
```
<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

### Comparison of Unsupervised Models with Two Different Distance Metrics

```r {r message= FALSE, warning=FALSE}

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

```

It can be observed that both models perform quite well in classifying the expected two clusters. However, comparing the evaluation metrics, it appears that in this case the PAM model performs better with the Manhattan distance metric, which depends greatly on the type of data and how it is grouped.

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

## Classifier Model with DBSCAN OPTICS

In this section, we will use the DBSCAN OPTICS algorithms for the classification of our dataset.

### DBSCAN Model

```r {r message= FALSE, warning=FALSE}
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


```

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

As a first step in determining where the DBSCAN algorithm will achieve the best classification, we need to find the optimal value for epsilon. For this, we create graphs of Silhouette Score vs Epsilon and Outliers vs Epsilon. The goal is to find a balance where the value of Epsilon yields the highest Silhouette Score and a lower number of outliers. Based on the two previous graphs, we will choose the value of 2.65.

```r {r message= FALSE, warning=FALSE}
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
```

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>


```r {r message= FALSE, warning=FALSE}
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

```

**DBSCAN Model with Epsilon 2.65 and MinPts 10**<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;We can see that the model has:<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Precision: **&nbsp;&nbsp;`r round(dbscan_model_precision, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Accuracy: **`r round(dbscan_model_accuracy, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Sensitivity: **&nbsp;&nbsp;`r round(dbscan_model_sensitivity, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**F-Measure: **&nbsp;&nbsp;`r round(dbscan_model_fmeasure, digits=4)`<br>

We can see how the DBSCAN model performs worse than the previous K-medoids (PAM) models due to the nature of the dataset and the complexity of tuning the parameters. For now, we can say that for this binary classification, classifiers like k-means or k-medoids are quite effective as opposed to more complex ones like DBSCAN.

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

## Supervised Model: Decision Tree

### Data Preparation

For the future evaluation of the decision tree, it is necessary to split the dataset into a training set and a test set. The training set is the subset of the original dataset used to build a preliminary model; and the test set, the subset of the original dataset used to evaluate the quality of the model.

It is most appropriate to use a dataset different from the one used to build the tree, that is, different from the training set. There is no fixed proportion; in this case, we will use 2/3 for the training set and 1/3 for the test set.

```r {r message= FALSE, warning=FALSE}
set.seed(287)  # For reproducibility

# Remove the first two columns from the dataframe
X1 <- cancer_df %>% select(-c(1, 2))
y1 <- cancer_df$diagnosis

split_prop <- 3
indexes <- sample(1:nrow(cancer_df), size = floor(((split_prop - 1) / split_prop) * nrow(cancer_df)))
```
### Creation of the Models, Quality Assessment, and Rule Extraction
The decision tree is created using the training data.

```r {r message= FALSE, warning=FALSE}
set.seed(287)

trainX1 <- X1[indexes, ]
trainy1 <- y1[indexes]
testX1 <- X1[-indexes, ]
testy1 <- y1[-indexes]

trainy1 <-  as.factor(trainy1)
model <- C50::C5.0(trainX1, trainy1,rules=TRUE )
summary(model)

```

We can observe that for this model, 6 decision rules are created, and 'Errors' shows the number and percentage of cases misclassified in the training subset. The obtained tree erroneously classifies only 3 out of 379 cases, a misclassification rate of **0.8%**.

Among the rules, we can highlight:

**Rule 1**, which with the parameters indicated below classifies as “B” (Benign) with a validity of **98.6%**

**Rule 1**: (70, lift 1.6)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;	concavity_mean <= 0.06155<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;	compactness_se > 0.01597<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;	->  class B  [0.986]<br>

**Rule 4**, which with the parameters indicated below classifies as “M” (Malignant) with a validity of **99.0%**

**Rule 4**: (70, lift 1.6)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;	concave.points_mean > 0.0539<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;	concave.points_se <= 0.01992<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  concave.points_worst > 0.1423<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;	->  class M  [0.990]

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

### Graphical Representation of the Decision Tree Model {.headers}


```r {r message= FALSE, warning=FALSE}

model <- C50::C5.0(trainX1, trainy1)
plot(model,gp = gpar(fontsize = 8))

```
<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

### Validation of the Decision Tree Model {.headers}

In this section, we will validate the model's metrics as in the other cases, but this time with the dataset reserved for testing.

```r {r message= FALSE, warning=FALSE}

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

```

**Decision Tree Model**<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;We can see that the model has:<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Precision: **&nbsp;&nbsp;`r round(tree_model_precision, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Accuracy: **`r round(tree_model_accuracy, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Sensitivity: **&nbsp;&nbsp;`r round(tree_model_sensitivity, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**F-Measure: **&nbsp;&nbsp;`r round(tree_model_fmeasure, digits=4)`<br>

It is observed that the decision tree model classifies the test set data quite well, with a precision of 91.47%, which is quite similar to that obtained with the training data, making it a very consistent model.

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

## Random Forest

As our second supervised model, we will use a random forest. Similar to the classification tree, we train the model with the same proportion of the dataset and test it with the test set, then generate the confusion matrix.

```r {r message= FALSE, warning=FALSE}
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

```
**Random Forest Model**<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;We can see that the model has:<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Precision: **&nbsp;&nbsp;`r round(rf_model_precision, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Accuracy: **`r round(rf_model_accuracy, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Sensitivity: **&nbsp;&nbsp;`r round(rf_model_sensitivity, digits=4)`<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**F-Measure: **&nbsp;&nbsp;`r round(rf_model_fmeasure, digits=4)`<br>

It is observed that the random forest model classifies the test set data quite well, with a precision of 91.47%, which is quite similar to that obtained with the training data, making it a very consistent model.

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>

## Comparison and Conclusions

First, we will create a table with the metrics of the different models studied:

```r {r message= FALSE, warning=FALSE}
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

```

<br>Comparing the models applied to the classification of the Wisconsin Breast Cancer dataset, we can analyze their performance in terms of precision, accuracy, sensitivity, and F-measure. The models considered are DBSCAN (unsupervised), Decision Tree (supervised), K-Medoids Model with Euclidean distance (unsupervised), K-Medoids Model with Manhattan distance (unsupervised), and Random Forest (supervised).<br><br>

Observing the precision metric, which measures the proportion of correctly predicted positive cases, we can see that the Random Forest **(0.94)** has the highest precision. This indicates that the Random Forest is more effective at correctly identifying malignant cases. However, the Decision Tree **(0.91)** and the K-Medoids Model with Manhattan distance **(0.94)** also demonstrate high precision.<br><br>

Regarding accuracy, which measures the overall correctness of the model's predictions, the Random Forest **(0.94)** and the K-Medoids Model with Manhattan distance **(0.94)** have the highest accuracy. These models make the most accurate predictions overall.<br><br>

In terms of sensitivity, which assesses the model's ability to correctly identify true positive cases, the Random Forest **(0.98)** outperforms the other models. It has the highest sensitivity, indicating it can effectively identify most malignant cases. The Decision Tree **(0.97)** and both K-Medoids models **(0.95)** also show solid sensitivity.<br><br>

F-measure combines precision and sensitivity into a single metric, representing the model's overall effectiveness. Here, the Random Forest **(0.96)** achieves the highest F-measure, indicating its balanced performance in both precision and sensitivity. The Decision Tree **(0.94)** and the K-Medoids Model with Manhattan distance **(0.95)** also demonstrate good F-measure.<br><br>

Considering the limitations of these models, it's important to analyze the characteristics of the dataset and the classification task. The Wisconsin Breast Cancer dataset involves classifying cases as benign or malignant – a binary classification problem.<br><br>

The use of unsupervised models for classification in this context poses several risks. DBSCAN, for instance, relies on density-based clustering and does not consider the actual class labels. It may struggle to correctly classify new instances and is more suitable for anomaly detection rather than explicit binary classification tasks, such as cancer diagnosis.<br><br>

Similarly, K-Medoids models, although based on clustering, may have limitations in effectively discriminating between benign and malignant cases. The choice of distance metric (Euclidean or Manhattan) can impact model performance, but both models may not consider all the information available in the labeled data.<br><br>
In contrast, supervised models like the Decision Tree and Random Forest leverage labeled data to make informed predictions. They consider the relationship between features and class labels, allowing for more accurate and reliable predictions. These models can capture complex patterns and interactions within the dataset, leading to better performance in classification tasks.<br><br>

However, it's important to recognize that all models have their own limitations. They rely on the assumption that the input features are informative and representative of the target variable. Inadequate or irrelevant features can negatively impact model performance. Additionally, overfitting can occur if the models are too complex or if the dataset is small.<br><br>
To mitigate risks, it's important to thoroughly assess models using appropriate evaluation metrics, such as cross-validation, and evaluate their performance on independent test sets. Additionally, feature selection techniques and preprocessing methods can be applied to improve model performance.<br><br>
In conclusion, considering the dataset and classification task provided, the Random Forest model stands out as the best performer overall in terms of precision, accuracy, sensitivity, and F-measure. Supervised models like Random Forest and Decision Tree tend to outperform unsupervised models in this context due to their ability to leverage labeled data and make informed predictions. However, the characteristics of the dataset should be taken into account, and appropriate evaluation techniques applied to choose the most suitable model for classifying different problems.<br><br>

<a href="#top" style="display: block; text-align: right;">↑ Back to the top</a>


