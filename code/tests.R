library(tidyverse)

holder <- read_csv(paste0("example_data/", list.files("example_data/")[1]),
                   skip = 3)

holder %>%
  ggplot(aes(x = Ch1Amplitude, 
             y = Ch2Amplitude)) +
  geom_point()

km <-
holder %>%
  select(Ch1Amplitude) %>%
  kmeans(., 3) #%>% 
  .$cluster %>% 
  cbind(holder %>% 
          select(Ch2Amplitude)) %>%
  clean_some_names(1) %>%
  group_by(x) %>%
  summarise_all(c(max = max, 
                  min = min)) %>%
  arrange(max) %>%
  mutate(x = c("Negative", "Positive")) %>%
  filter(x == "Negative") %>%
  select(max) %>%
  as.numeric()
  
  km$betweenss / km$totss
  
  
  
holder %>% nrow()
  
