library(tidyverse)
library(ClusterR)
holder <- read_csv(paste0("example_data/", list.files("example_data/")[2]),
                   skip = 3)

holder %>%
  ggplot(aes(x = Ch1Amplitude, 
             y = Ch2Amplitude)) +
  geom_point()

#km <-
holder %>%
  select(Ch1Amplitude) %>%
  kmeans(., 2) %>% 
  .$cluster %>% 
  cbind(holder %>% 
          select(Ch1Amplitude)) %>%
  clean_some_names(1) %>%
  group_by(x) %>%
  summarise_all(c(max = max, 
                  min = min)) %>% 
  arrange(max) %>%
  mutate(x = c("Negative", "Positive")) %>%
  filter(x == "Positive") %>%
  select(max) %>%
  as.numeric() + 1
  
  km$centers
  
  $tot.withinss
  
km$betweenss / km$totss
  
holder %>%
  select(Ch2Amplitude) %>%
  kmeans(., 2) %>%
  .$withinss

km_ss <-
(holder %>%
  select(Ch2Amplitude) %>%
  kmeans(., 2) %>%
  .$betweenss

/

holder %>%
  select(Ch2Amplitude) %>%
  kmeans(., 2) %>%
  .$totss)
  
  
gmm <-
GMM(holder[2], 
    gaussian_comps = 2)

gmm$centroids %>%
  mean()

gm_predict <-
predict(gm, holder[2])


(
  holder %>%
  select(Ch1Amplitude) %>%
  kmeans(., 2) %>%
  .$totss
  
  /

holder %>%
  select(Ch1Amplitude) %>%
  kmeans(., 2) %>%
  .$betweenss
  )

gmm$centroids %>%
  mean()

gmm$covariance_matrices

  
