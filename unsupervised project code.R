## data preparation ##

#import data
library(readr)
co2_data <- read_csv("owid-co2-data.csv", na = "NA")
#create data for year = 2011
library(dplyr)
data_2011 <- co2_data %>% filter(year== 2011)
#impute mean into missing values
library(imputeTS)
data_2011 <- na_mean(data_2011)
#create df for country and year
country <- data_2011[,2:3]
#select columns containing per capita data only
library(dplyr)
data_2011 <- data_2011[,4:50] %>% select(contains("capita"))
#combine country and year & data_2011
data_2011 <- cbind(country, data_2011)
#rescale data and round data
data_2011[,3:13] <- round(scale(data_2011[,3:13], center = TRUE),1)
data_2011 <- data_2011 %>% select(-'year')
rownames(data_2011) <- data_2011$country

#check data summary
psych::describe(data_2011)

## modelling part ## 
library(cluster)
library(factoextra) 
#k-means clustering
#choose optimal k using the elbow method 
set.seed(123)
fviz_nbclust(data_2011[,2:12], kmeans, method = "wss")
#plot using k=4 
k4 <- kmeans(data_2011[,2:12], centers = 4, nstart = 25)
str(k4)
k4
#illustration of the clusters
fviz_cluster(k4, data = data_2011[,2:12], labelsize = 6, show.clust.cent = TRUE, repel =TRUE)

#hierarchical clustering 
#hierarchical clustering, k=3
library(proxy)
simil(data_2011[,2:12], method="Gower")
dendro_plot <- hclust(dist(data_2011[,2:12], method="Gower"), method = "complete")
plot(dendro_plot, cex = 0.5, cex = 0.4)
groups <- cutree(dendro_plot, k=4)
groups
rect.hclust(dendro_plot, k=4, border= c("orange","red", "green", "blue"))

#pull country names and respective group
as.data.frame(cbind(groups))
write.csv(as.data.frame(cbind(groups)), file = "country_cluster.csv")
