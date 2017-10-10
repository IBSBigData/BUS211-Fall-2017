# demo03 --Creating Network Graphs from Market Basket Data
# Script that produces the analysis in the pdf found on LATTE, entitled
# "Creating Network Graphs from Market Basket Data"

# call packages
library(igraph)
library(tidyverse)
library(dplyr)

# Read in the data; you must change the following line for your installation
sam <- read_csv("C:/Users/Rob/Box Sync/My R Work/BUS211/Data/Sams_trans.csv")
sam <- select(sam, 1:5)
glimpse(sam)

# Preparing data for analysis -- variable types and data reduction
# here we identify top 20 items; other analyses might use more or less
sam$Membership_Nbr <- as.factor(sam$Membership_Nbr)
sam$Item_Nbr <- as.factor(sam$Item_Nbr)
v20 <- sam %>%
     group_by(Item_Nbr) %>%
     summarize(freq = n()) %>%
     arrange(desc(freq)) %>%
     top_n(n=20) 

v20$Item_Nbr <- factor(v20$Item_Nbr, ordered = TRUE)
head(v20)

# subset the data to restrict to most common items

ssam <- tbl_df(sam) %>%  
select(1:5) %>%
inner_join(v20, by="Item_Nbr")

ssam <- ssam %>%
arrange(Visit_Nbr,Item_Nbr)

ssam$Item_Nbr <- factor(ssam$Item_Nbr, ordered = TRUE) # drop unused factor levels
ssam$Visit_Nbr <- factor(ssam$Visit_Nbr, ordered= TRUE)

nodes <- ssam %>%
distinct(Item_Nbr, Primary_Desc, freq) 
head(nodes)

# Perform the equivalent of a cross-join
ssam$k <- 1
ssam2 <- ssam %>% 
     inner_join(ssam, by=c('Visit_Nbr','k')) %>%
     select(-k)

ssam3 <- ssam2 %>%
     group_by(Visit_Nbr) %>%
     mutate(check = ifelse(Item_Nbr.x < Item_Nbr.y, 1,0)) %>%
     filter(check==1) %>%
     select(-check) %>%
     select(-transaction_Date.y) %>%
     select(-Membership_Nbr.y)

head(ssam3)

# Now build the list of edges (arcs) in the network 
edges <- ssam3 %>%
     group_by(Item_Nbr.x, Item_Nbr.y) %>%
     summarize(count=n())
head(edges)


## Creating the graph object

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE) 
hist(degree(net))

#############################################################
# DEMONSTRATE SEVERAL DIFFERENT PLOTTYING OPTIONS AND METHODS
#############################################################

# For a given analysis, you might explore many of these options, 
# but ultimately would choose ONE of them to present your analysis
#
#  This section just illustrates a few of the MANY possibilities

## Make layout reproducible. Different values will produce different layouts,
##  but setting a seed will allow you to reproduce a layout if you like it.
set.seed(1217)  # use any seed we like

# for circular
lc <-layout.circle(net)

#for Frucheterman-Reingold
lfr <- layout.fruchterman.reingold(net, niter=.1)

E(net)$arrow.mode = 0 # no arrows
par=(mar=c(0,1,0,1))  #  set margins around graphs

plot(net, layout = lfr, edge.curved=.1, 
vertex.label=V(net)$Primary_Desc,
vertex.label.font=3,
vertex.label.cex=.3,
vertex.label.distance= 2,
edge.color="brown",
main="Sam's Club Purchase Patterns")

plot(net, layout=lfr, vertex.shape="none",
vertex.label=V(net)$Primary_Desc, 
vertex.label.font=1,
vertex.label.cex=.4,
vertex.label.distance= 2,
main="Sam's Club Purchase Patterns")

plot(net, layout=lc, vertex.shape="none",
vertex.label=V(net)$Primary_Desc, 
vertex.label.font=2,
vertex.label.cex=.4,
main="Sam's Club Purchase Patterns")

# another far more complex approach
minC <- rep(-Inf, vcount(net))
maxC <- rep(Inf, vcount(net))
minC[1] <- maxC[1] <- 0

lfr2 <- layout.fruchterman.reingold(net, minx=minC, maxx=maxC,
miny=minC, maxy=maxC)

plot(net, layout=lfr2, vertex.size=V(net)$freq/20,
vertex.label=V(net)$Primary_Desc, rescale=FALSE,
xlim=range(lfr[,1])*4, ylim=range(lfr[,2])*2, 
vertex.label.font=2,
vertex.label.cex=.4,
vertex.label.color="dark blue",
main="Sam's Club Purchase Patterns")

#  much more complex method with custom layout

layout.by.attr <- function(graph, wc, cluster.strength= 10,layout=layout.auto) {  
g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
E(g)$weight <- 1

attr <- cbind(id=1:vcount(g), val=wc)
g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)

l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
return(l)
}

plot(net, vertex.shape="none", vertex.size=1,     
vertex.label=V(net)$Primary_Desc,
vertex.label.cex=.4,
layout=layout.by.attr(net, wc=5),
main="Sam's Club Purchase Patterns")

