# demo02
#    Some methods of data visualization, as well as exploration, querying and reorganization
#    related to graphs

# first load packages to be used
library(dplyr)   # dplyr helps to reorganize a data table
library(ggplot2) # a graphing package with numerous features 
library(igraph)  # for network diagram
library(hflights) # contains data about flights from Houston Texas

## Explore the data
# convert hflights data frame to a "tibble"
hflights <- tbl_df(hflights)
dim(hflights)  # report dimensions -- rows, columns
glimpse(hflights)

names(hflights)
head(hflights)

# to overcome necessity of typing table name before $VARNAME, we can 
# attach a data frame.  This works well if the script refers to only
# data frame.

attach(hflights) 
summary(ArrDelay)
mean(ArrDelay) #  some functions are "defeated" by NA cells
# To omit NAs from a computation, we can use "rm.na=TRUE"
mean(ArrDelay, na.rm = TRUE)

# More graphing using ggplot -- we'll focus on ggplot as main graphing method
# Build up a graph object in layers
p1 <- ggplot(hflights, aes(ArrDelay))
str(p1)  # pause to look at structure of this new object
p1 <- p1 + geom_histogram(binwidth=15, na.rm=TRUE)
p1 # display the histogram
# Follow several of Yau's rules by adding titles and a comment
p1 <- p1 + ggtitle("Arrival Delays for flights out of Houston", 
          subtitle = "from package 'hflights'") + 
     xlab("Arrival Delay (in minutes)") +
     annotate("text", x = 350, y = 90000, label = "The vast majority of flights arrive on time")
p1
# finally add a caption to credit a source
p1 + labs(caption = "ggplot options found at http://ggplot2.tidyverse.org/reference/index.html")

#########################
# further examinations of flight delays
# by carrier
table(UniqueCarrier)
p2 <- ggplot(hflights, aes(UniqueCarrier, ArrDelay))
p2 + geom_boxplot(na.rm = TRUE) + geom_jitter(width=.1) +coord_flip()

##  Very dense plot -- hard to interpret
# We can AGGREGATE some measures to find patterns more easily
# Create new table of median delays by carrier + Houston airport
# we use some dplyr functions, including the "pipe" operator to chain together several commands

medbycar <- hflights %>% 
  group_by(Origin, UniqueCarrier) %>% 
  summarize(mdelay = median(ArrDelay, na.rm="T"))
View(medbycar)

ggplot(medbycar, aes(x=UniqueCarrier, y=mdelay)) +
  aes(color=Origin, fill=Origin) +
  geom_bar(stat="identity", width=.5, position = "dodge") 
  
#####################
##
#  A network graph showing most common destinations from HOU
# Approach:
#  1.  SELECT just the few columns needed from original dataframe
#  2.  Subset (FILTER) only flights from HOU
#  3.  Two summary statistics for each DEST:  # of flights and median delay
#  4.  Graph will color nodes by median delay, line thickness by # of flights

#  Subset of HOU flights only
hou <- hflights %>%
  filter(Origin == "HOU") %>%
  select(ArrDelay, Origin, Dest)
glimpse(hou)
# Now we want to compute the summaries
edges <- hou %>% 
  group_by(Origin, Dest) %>% 
  summarize(count=n())
edges[1,] <- c("HOU", "HOU", 0)
edges$count <- as.integer(edges$count)

nodes <- hou %>%
  group_by(Dest) %>%
  summarize(mdelay = median(ArrDelay, na.rm=T))
nodes[1,] <- c("HOU",0)
nodes$mdelay <- as.integer(nodes$mdelay)

# igraph wants to identify nodes Edges

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T) 
net
plot(net)  #Ugly!!!

# many options to open up and simplify, but here is one
# first compute edge weights based on frequency
#change arrow size and edge color:
E(net)$arrow.size <- .1
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$count/700

plot(net, vertex.label=V(net)$Dest, 
     vertex.label.font=2, 
     vertex.label.cex=.5)

# Now adjust the vertex sizes
V(net)$size <- (V(net)$mdelay+18)*1.25
plot(net, vertex.label=V(net)$Dest, 
     vertex.label.font=2,
     vertex.label.cex=.5)
