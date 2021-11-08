library(tidyverse)
library(plotly)
library(htmltools)
library(devtools)
library(caret)
library(NbClust)

df <- read_csv("~/Dev/DS3001/3001_handsOn/Week11/data-frame.csv") # importing data frame

# make scatter plot comparing new features
ggplot(df,aes(x=cora,y=corc))+geom_point()

df <- na.omit(df) # remove NAs
str(df)
df1 <- df[c(4, 6, 7)] # subset variables I will be observing
View(df1)

normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
#View(df1)
dfPrep <- df1
#View(dfPrep)
normalize(dfPrep$corc) #normalize corc
normalize(dfPrep$cora) #normalize cora
dfPrep$num_turns <- normalize(dfPrep$num_turns) #normalize num_turns
#dfNor <- normalize(dfPrep)

View(dfPrep)


dfPrep[complete.cases(dfPrep), ] # remove the rows completely where there is NA (this is more effective than na.omit)

dfPrep2 <- dfPrep

dfPrep2$main_colors <- df$main_colors

View(dfPrep2)

dfPrep2[complete.cases(dfPrep2), ]


set.seed(1)
kmeans_obj_df = kmeans(dfPrep, centers = 3,  #clustering with 3 centers 
                       algorithm = "Lloyd")

View(dfPrep2)

kmeans_obj_df

dfPrep$clusters <- kmeans_obj_df$cluster # add cluster into dfPrep
#View(dfPrep)

dfPrep$won <- df$won

ggplot(dfPrep, aes(corc, cora, fill = won)) + geom_boxplot() # plot to see relation between corc and cora by won

#Visualize the output
clusters_df = as.factor(kmeans_obj_df$cluster)

#View(clusters_df)

ggplot(dfPrep, aes(x = corc,  # graphing 2d graph
                  y = cora,
                  color = num_turns,  #<- tell R how to color 
                  #   the data points
                  shape = clusters_df)) + 
  geom_point(size = 6) +
  labs(title = "Corc vs. Cora",
       x = "Corc",
       y = "Cora") +
  scale_shape_manual(name = "Cluster", 
                     labels = c("Cluster 1", "Cluster 2", "Cluster 3"),
                     values = c("1", "2", "3")) +
  theme_light()

ggplot(dfPrep2, aes(x = corc, y = cora, color = main_colors,shape = clusters_df)) + 
  geom_point(size = 4) +
  ggtitle("corc vs. cora") +
  xlab("corc") +
  ylab("cora") +
  scale_shape_manual(name = "Cluster", 
                     labels = c("Cluster 1", "Cluster 2", "Cluster 3"),
                     values = c("1", "2", "3")) +
  theme_light()


dfNor <- cbind(dfPrep, won = df$won) # add won to dfNor for 3d graph

dfNor$clusters <- clusters_df

fig <- plot_ly(dfNor, # 3d graph 
               type = "scatter3d",
               mode="markers",
               symbol = ~clusters,
               x = ~corc, 
               y = ~cora, 
               z = ~num_turns,
               text = ~paste('Won:',won))

fig


# Today, I learned to use various types of a plot to see the difference in the data.  
# I included one of the plots I made (between corc and cora by won). 
# Also, I ran into a problem where I normalized the data but got the graph showing 
# no difference between clusters. I fixed the issue by normalizing variables separately 
# and using complete.cases instead of na.omit. 
# Today, I learned a lot from the class to troubleshoot any errors while preparing 
# data frames to compute the result. One thing I looked up after class is more 
# details on ggplot. Visualization is an essential piece to 
# interpreting the data and outcome. I learned different ways to visualize the data 
# while I was looking up ggplot. Also, I looked up k-mean to see how the clustering is working.

