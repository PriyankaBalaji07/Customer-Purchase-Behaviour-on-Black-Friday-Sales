#Load the required libraries tidyverse and DT
library(tidyverse)
library(DT)
#Read and load the BlackFriday dataset and store it in sales_df variable
sales_df <- read.csv(file.choose(), sep=",", header = TRUE )
head(sales_df)
BlackFridayForClustering <- sales_df %>%
  select(Purchase)
# Utilize map_dbl function to execute multiple models with different k values (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = BlackFridayForClustering, centers = k)
  model$tot.withinss
})

# Generates a data frame that contains both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Draws graphical representation-elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

# Constructs a k-means modelwith 3 cluster centers
model_km3 <- kmeans(BlackFridayForClustering, centers = 3)

# Draws out the vectors acquired by the k-means model
clust_km3 <- model_km3$cluster

# Generates a latest dataframe including the appended cluster assigned
BlackFriday_Clust <- mutate(sales_df, cluster = clust_km3)

# Outlines Clustering
BlackFriday_Clust_Note <- BlackFriday_Clust %>%
  group_by(cluster) %>%
  summarise(min_purchase = min(Purchase),
            max_purchase = max(Purchase),
            avg_purchase = round(mean(Purchase),0))
# Number of people in every cluster
BlackFriday_Clust %>%
  group_by(Gender, cluster) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=Gender, y = n)) +
  geom_col(aes(fill = as.factor(cluster))) +
  theme_linedraw() + 
  theme(legend.box.background = element_rect(colour = "black"),
        legend.background = element_rect(fill = "gainsboro"),
        panel.background = element_rect(fill = "gainsboro", colour = "white", size = 0.5, linetype = "solid"), #theme panel settings
        plot.background = element_rect(fill = "gainsboro"), #theme panel settings
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), # Settings of Theme Panel
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"), #Settings of Theme Panel
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), # Setting of Title
        plot.subtitle = element_text(face = "italic")) + #Setting of Subtitle
  labs(x = 'Gender Category', y = 'Total Purchase (dollars)', title = "Black Friday", #Lable Title and Axes
       subtitle = "Total people in each cluster by gender") + #Label subtitle
  guides(fill=guide_legend(title = "Cluster")) + # Delete Legend color
  scale_y_continuous(labels = scales::comma) #prevent scientific number in x-axis