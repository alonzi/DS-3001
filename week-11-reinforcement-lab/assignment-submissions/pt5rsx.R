library(tidyverse)
library(ggplot)

# The purpose of this code is to cluster the data set from data-frame.csv

# The one coding thing I haven't done before is to do K-means with Hartigan-Wong's algorithm
# After class, I looked up the other algorithms supported by the kmeans function.


df <- read_csv("data-frame.csv")

# make scatter plot comparing new features
ggplot(df,aes(x=cora,y=corc))+geom_point()

df <- df[complete.cases(df),]

# View(clust_data)
cols = c("main_colors", "opp_colors", "on_play", "won")
df = df[ , !(names(df) %in% cols)]

kmeans_obj = kmeans(df, centers = 6, algorithm = "Hartigan-Wong", iter.max = 30)
df["cluster"] = as.factor(kmeans_obj$cluster)
View(df)

ggplot(df,aes(x=cora,y=corc, color=cluster))+geom_point()

