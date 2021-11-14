df_rawdata <- read.csv("C:\\Users\\ND.COM\\Downloads\\Mall_Customers.csv", header = TRUE)
View(df_rawdata)
# For some pre-processing 
colnames(df_rawdata) = c("CustomerID", "Gender", "Age", "Annual_Income", "Spending_Score")
adjusted_data = df_rawdata
summary(adjusted_data)
#  CustomerID        Gender               Age        Annual_Income    Spending_Score 
#Min.   :  1.00   Length:200         Min.   :18.00   Min.   : 15.00   Min.   : 1.00  
#1st Qu.: 50.75   Class :character   1st Qu.:28.75   1st Qu.: 41.50   1st Qu.:34.75  
#Median :100.50   Mode  :character   Median :36.00   Median : 61.50   Median :50.00  
#Mean   :100.50                      Mean   :38.85   Mean   : 60.56   Mean   :50.20  
#3rd Qu.:150.25                      3rd Qu.:49.00   3rd Qu.: 78.00   3rd Qu.:73.00  
#Max.   :200.00                      Max.   :70.00   Max.   :137.00   Max.   :99.00  
is.na(adjusted_data)
#Adding dummy variables to gender
require(tidyr)
require(dplyr)

sorted_df = adjusted_data %>% mutate(value = 1)  %>% spread(Gender, value,  fill = 0 ) 
View(sorted_df)

library(psych)
library(plyr)
multi.hist(mpg) #error, not numeric
multi.hist(mpg[,sapply(mpg, is.numeric)])
multi.hist(sorted_df,nrow = 3, ncol=2,density=TRUE,freq=FALSE,bcol="lightblue",
           dcol= c("red","blue"),dlty=c("solid", "dotted"),
           main=colnames(sorted_df))

# Annual_Income and Age are left skewed.
#Spending score is nearly normal.
#Male and Female just have two peak points. 

#PAM CLUSTERING 
library(cluster)
library(factoextra)
library(tidyverse)  # data manipulation
library(NbClust)    #use zip file to install it 
install.packages("fviz_nbclust")
#We will first need to scale the data to prevent abnormalities in the end product. 
scaled_df = scale(sorted_df)
View(scaled_df)
fviz_nbclust(scaled_df, pam, method ="silhouette")+theme_minimal()
#Since the graph has it's first decrease at k = 2 clusters we can move on to PAM CLustering 
#Note that the k values for the number of clusters is also found by pam method of clustering. 

pamResult <-pam(scaled_df, k = 2)
pamResult

#Medoids:
#ID CustomerID        Age Annual_Income Spending_Score     Female       Male
#[1,] 78 -0.3887408 0.08232511   -0.24976469    -0.08519365 -1.1253282  1.1253282
#[2,] 94 -0.1123029 0.08232511   -0.02132138    -0.39498873  0.8841865 -0.8841865
#Clustering vector:
#  [1] 1 1 2 2 2 2 2 2 1 2 1 2 2 2 1 1 2 1 1 2 1 1 2 1 2 1 2 1 2 2 1 2 1 1 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 1 2 1 2 1 2 1 2 1 1 1 2 2
#[65] 1 1 2 2 1 2 1 2 2 2 1 1 2 1 2 2 1 1 1 2 2 1 2 2 2 2 2 1 1 2 2 1 2 2 1 1 2 2 1 1 1 2 2 1 1 1 1 2 2 1 2 2 2 2 2 2 1 2 2 1 2 2 1 1
#[129] 1 1 1 1 2 2 1 2 2 1 1 2 2 1 2 2 1 1 1 2 2 1 1 1 2 2 2 2 1 2 1 2 2 2 1 2 1 2 1 2 2 1 1 1 1 1 2 2 1 1 1 1 2 2 1 2 2 1 2 1 2 2 2 2
##[193] 1 2 2 2 2 1 1 1
#Objective function:
#  build     swap 
#1.912355 1.912355 

#Available components:
#  [1] "medoids"    "id.med"     "clustering" "objective"  "isolation"  "clusinfo"   "silinfo"    "diss"       "call"      
#[10] "data"      

df_rawdata$cluster = pamResult$cluster
View(df_rawdata)
#To observe the medoids
pamResult$medoids
pamResult$clustering
#Visualizing thw clusters formed. 
fviz_cluster(pamResult, 
             palette =c("#007892","#D9455F"),
             ellipse.type ="euclid",
             repel =TRUE,
             ggtheme =theme_minimal())
