setwd("C:/Users/Rob/Box Sync/My R Work/BUS211")

library(igraph)
library(tidyverse)

df <- read_csv("C:/Users/Rob/Box Sync/My R Work/BUS211/Data/tinybasket.csv")
df <- select(df, 1:3)
glimpse(df)

df <- df %>% arrange(Visit,Item)

df$k <- 1
df2 <- df %>% 
     inner_join(df, by=c('Visit','k')) %>%
     select(-k)

df3 <- df2 %>%
     group_by(Visit) %>%
     mutate(check = ifelse(Item.x < Item.y, 1,0)) %>%
     filter(check==1) %>%
     select(-check)
