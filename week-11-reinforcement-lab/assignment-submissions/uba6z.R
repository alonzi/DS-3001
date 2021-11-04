library(tidyverse)
library(plotly)
library(htmltools)
library(devtools)
library(caret)
library(NbClust)

#In this class I learned a few new things about data visualization, and ways of using graphs
#to learn something about data. Using ggplot, I was able to have more of an understanding
#of what the data was about by coloring it based on certain variables. I learned some syntax
#for ggplot as well. I also tried clustering the data and using nbclust to choose the best k
#for kmeans, and that was pretty cool, because even though we did kmeans a few weeks ago, it 
#feels like a loooong time ago. I feel like some of those old skills have been..."reinforced."

#ill show myself out.

# read in data and create dataframe (df1)
df <- read_csv("data-summary.csv")
df1 <- select(df, main_colors, opp_colors, on_play, num_turns, won)

# feature engineering (cora,corc)
df2 <- select(df,"deck_Adeline, Resplendent Cathar":"deck_Wrenn and Seven")
mat = data.matrix(df2)
vec1 <- vector()
vec3 <- vector()
for(i in 1:nrow(mat) ){
  x<-cor( mat[1,] , mat[i,])
  vec1 <- c(vec1,x)
  z<-cor( mat[47,] , mat[i,])
  vec3 <- c(vec3,z)
}

# add new features to dataframe
df1 <- df1 %>% mutate(cora = vec1)
df1 <- df1 %>% mutate(corc = vec3)

# make scatter plot comparing new features
ggplot(df1,aes(x=cora,y=corc))+geom_point()

### Normalization

# Normalization function
normalize = function(x){
  (x - min(x)) / (max(x) - min(x))
}

normalize(df1$corc)
normalize(df1$cora)

normalized_turns = normalize(df1$num_turns)

df1$num_turns = normalize(df1$num_turns)

clust_viz = df1[, c("corc", "cora", "num_turns")]

set.seed(1)
kmeans_obj_viz = kmeans(clust_viz, centers = 2, 
                        algorithm = "Lloyd")

kmeans_obj_viz

clusterDF <- as.factor(kmeans_obj_viz$cluster)

ggplot(df1, aes(x = corc, y = cora, shape = clusterDF)) + 
  geom_point(size = 4) +
  ggtitle("corc vs. cora") +
  xlab("corc") +
  ylab("cora") +
  scale_shape_manual(name = "Cluster", 
                     labels = c("Cluster 1", "Cluster 2"),
                     values = c("1", "2")) +
  theme_light()

ggplot(df1, aes(x = corc, y = cora, color = main_colors,shape = clusterDF)) + 
  geom_point(size = 4) +
  ggtitle("corc vs. cora") +
  xlab("corc") +
  ylab("cora") +
  scale_shape_manual(name = "Cluster", 
                     labels = c("Cluster 1", "Cluster 2"),
                     values = c("1", "2")) +
  theme_light()

#3D Plot

df1$clusters <- (clusterDF)

fig = plot_ly(df1, 
              type = "scatter3d",
              mode="markers",
              symbol = ~clusters,
              x = ~cora, 
              y = ~corc, 
              z = ~num_turns,
              color = ~main_colors,
              text = ~paste('Winner?', won))

fig

# Convert to factors
df1_normalize = na.omit(df1)
df1_normalize[,c(1,2)] = lapply(df1_normalize[,c(1,2)], as.factor)
str(df1_normalize)

# Normalize the factors
df1_cluster = df1_normalize
df1_normalize = df1_normalize[, -c(1,2)]
str(df1_normalize)

df1_normalize = normalize(df1_normalize)
str(df1_normalize)

# Clustering
# Find the number of clusters using NBCluster

str(df1_normalize)

# we want to select the total number of rows, but drop salary later
clust_data_df1 = select_if(df1_normalize,is.numeric)

# dont include main_colors
clust_data_df1 = clust_data_df1[, -c(1)]
view(clust_data_df1)

clust_data_df1 = na.omit(clust_data_df1)

set.seed(1)

kmeans_obj_df1 = kmeans(clust_data_df1, centers = 3, 
                        algorithm = "Lloyd")

kmeans_obj_df1

#Run Nbcluster
(nbclust_obj_df1 = NbClust(data = clust_data_df1, method = "kmeans"))

### Visualization


# Subset the 1st row from Best.nc and convert it 
# to a data frame so ggplot2 can plot it.

freq_k_df1 = nbclust_obj_df1$Best.nc[1,]
freq_k_df1 = data.frame(freq_k_df1)


# Check the maximum number of clusters suggested.
max(freq_k_df1)

# Plot as a histogram.
ggplot(freq_k_df1,
       aes(x = freq_k_df1)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 15, by = 1)) +
  scale_y_continuous(breaks = seq(0, 12, by = 1)) +
  labs(x = "Number of Clusters",
       y = "Number of Votes",
       title = "Cluster Analysis")

#Cluster # 5 got the most votes

### Nbclust

# Now we are going to build a simple decision tree using the clusters as a feature

# reminder this is our model, using 3 clusters 
set.seed(1980)
kmeans_obj_df1 = kmeans(clust_data_df1, centers = 3,
                        algorithm = "Lloyd")

# this is the output of the model. 
kmeans_obj_df1$cluster

df1_normalize$clusters = kmeans_obj_df1$cluster

clusters = as.factor(kmeans_obj_df1$cluster)

View(clusters)

df1_cluster = cbind(clust_data_df1,clusters)

View(df1_cluster)