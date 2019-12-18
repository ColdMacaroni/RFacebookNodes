#---------------------      Assignment 2 script, by Barbara       ------------------------------------
# Load require libraries
install.packages("igraph")
library(igraph)


#---------- 1. Import files in R Studio
nodes <- FB_NODES #Both CSV files are saved into objects
links <- FB_LINKS

#---------- Create a igraph
#the directed links between nodes are saved into an object
FBnet <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
FBnet #to show these directed links

#to plot the network
plot.igraph(FBnet, main= "igraph for FBnet")

#---------- 2. Inspect the attributes of the network
E(FBnet)
V(FBnet)

#---------- describing nodes and edges:
FBnetE.df <- as_data_frame(FBnet, what="edges")
FBnetV.df <- as_data_frame(FBnet, what="vertices")

#---------- Examine data frames
head(FBnetE.df)
head(FBnetV.df)


#---------- Plot the network using default settings, describe the main problem
plot(FBnet)


#--------- 3. Add attributes setitngs: colours of nodes (female and male), nodes and arrow sizes
# -------- Plot network with set arrow size for edges (links) and unlabelled vertices (nodes)
#plot(FBnet, edge.arrow.size=.2,vertex.label=NA)


# -------- Set node size based on friend_count
V(FBnet)$size <- V(FBnet)$friend_count/70

#--------- Add blue colour for males and red for females
V(FBnet)$color <- ifelse( V(FBnet)$sex=="female", "red", "blue")

#E(FBnet)$arrow.color="black"
plot(FBnet,vertex.label=NA,edge.arrow.size=0.13, main= "Network of Facebook users",
     edge.curved=0, edge.color="#666666")

colrs <- c("red", "blue")
legend(x=-1.5, y=1.1, c("female","male"), pch=21, col="#777777", pt.bg=colrs, pt.cex=2, 
       cex=.8, bty="n", ncol=1) #To add a legend in this network. https://kateto.net/netscix2016.html


#--------- 4. Plot network for each group using a layout style=layout_with_fr with a suitable 
#node size.

#--------------- Group B ---------------
FBnet_B <- delete_vertices(FBnet, V(FBnet)$group != "B")
V(FBnet_B)$size <- V(FBnet_B)$friend_count/10
l <- layout_with_fr(FBnet_B)
plot(FBnet_B, layout=l, edge.arrow.size= 0.4, vertex.color= "pink", main="Group B (Book Club)", 
     edge.color="#555555")

#to know the number of links in a network
edge_density(FBnet_B, loops=F)

#to know the numer of links a node has with other nodes
degree(FBnet_B)

# Betweenness degree
betweenness(FBnet_B, directed=T, weights=NA)

# Closeness degree
closeness(FBnet_B, mode="all", weights=NA) 

# In-degree. The number of connections in which a particular node is the target. 
degree(FBnet_B, mode="in")

# Out-degree. The number of relationships in which a node is the source.
degree(FBnet_B, mode="out")


#--------------- Group C ---------------
FBnet_C <- delete_vertices(FBnet, V(FBnet)$group != "C")
V(FBnet_C)$size <- V(FBnet_C)$friend_count/13
l <- layout_with_fr(FBnet_C)
plot(FBnet_C, layout=l, edge.arrow.size= 0.4, vertex.color= "yellow", main="Group C (College)", 
     edge.color="#555555")

#to know the number of links in a network
edge_density(FBnet_C, loops=F)

#to know the numer of links a node has with other nodes
degree(FBnet_C)

# Betweenness degree
betweenness(FBnet_C, directed=T, weights=NA)

# Closeness degree
closeness(FBnet_C, mode="all", weights=NA) 

# In-degree. The number of connections in which a particular node is the target. 
degree(FBnet_C, mode="in")

# Out-degree. The number of relationships in which a node is the source.
degree(FBnet_C, mode="out")


#with the simple layout_with_fr function for this particular group did not show a good visualisation
#so, a layout_with_fr function with more specifications was used:

#Group F
FBnet_F <- delete_vertices(FBnet, V(FBnet)$group != "F")
V(FBnet_F)$color <- "lightblue"
V(FBnet_F)$size <- V(FBnet_F)$friend_count/17
E(FBnet_F)$arrow.size=0.3
minC <- rep(-Inf, vcount(FBnet_F))
maxC <- rep(Inf, vcount(FBnet_F))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet_F, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)
l[1,]
plot(FBnet_F, layout=l, rescale=FALSE,main="Group F (Family)",
     xlim=range(l[,1]), edge.color="#555555", ylim=range(l[,2]))

#to know the number of links in a network
edge_density(FBnet_F, loops=F)

#to know the numer of links a node has with other nodes
degree(FBnet_F)

# Betweenness degree
betweenness(FBnet_F, directed=T, weights=NA)

# Closeness degree
closeness(FBnet_F, mode="all", weights=NA) 

# In-degree. The number of connections in which a particular node is the target. 
degree(FBnet_F, mode="in")

# Out-degree. The number of relationships in which a node is the source.
degree(FBnet_F, mode="out")


#Group G
FBnet_G <- delete_vertices(FBnet, V(FBnet)$group != "G")
V(FBnet_G)$size <- V(FBnet_G)$friend_count/15
l <- layout_with_fr(FBnet_G)
plot(FBnet_G, layout=l, edge.arrow.size= 0.4, vertex.color= "coral", main="Group G (Graduate School)", 
     edge.color="#555555")

#to know the number of links in a network
edge_density(FBnet_G, loops=F)

#to know the numer of links a node has with other nodes
degree(FBnet_G)

# Betweenness degree
betweenness(FBnet_G, directed=T, weights=NA)

# Closeness degree
closeness(FBnet_G, mode="all", weights=NA) 

# In-degree. The number of connections in which a particular node is the target. 
degree(FBnet_G, mode="in")

# Out-degree. The number of relationships in which a node is the source.
degree(FBnet_G, mode="out")


#Group H
FBnet_H <- delete_vertices(FBnet, V(FBnet)$group != "H")
V(FBnet_H)$size <- V(FBnet_H)$friend_count/15
l <- layout_with_fr(FBnet_H)
plot(FBnet_H, layout=l, edge.arrow.size= 0.6, vertex.color= "#33FF00", main="Group H (High School)", 
     edge.color="#555555")

#to know the number of links in a network
edge_density(FBnet_H, loops=F)

#to know the numer of links a node has with other nodes
degree(FBnet_H)

# Betweenness degree
betweenness(FBnet_H, directed=T, weights=NA)

# Closeness degree
closeness(FBnet_H, mode="all", weights=NA) 

# In-degree. The number of connections in which a particular node is the target. 
degree(FBnet_H, mode="in")

# Out-degree. The number of relationships in which a node is the source.
degree(FBnet_H, mode="out")


#with the simple layout_with_fr function for this particular group did not show a good visualisation
#so, a layout_with_fr function with more specifications is used:

#Group M
FBnet_M <- delete_vertices(FBnet, V(FBnet)$group != "M")
V(FBnet_M)$color <- "#F5A328"
V(FBnet_M)$size <- V(FBnet_M)$friend_count/10
E(FBnet_M)$arrow.size=0.4
minC <- rep(-Inf, vcount(FBnet_M))
maxC <- rep(Inf, vcount(FBnet_M))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet_M, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)
l[1,]
plot(FBnet_M, layout=l, rescale=FALSE, main="Group M (Music)", xlim=range(l[,1]), edge.color="#555555",
     ylim=range(l[,2]))

#to know the number of links in a network
edge_density(FBnet_M, loops=F)

#to know the numer of links a node has with other nodes
degree(FBnet_M)

# Betweenness degree
betweenness(FBnet_M, directed=T, weights=NA)

# Closeness degree
closeness(FBnet_M, mode="all", weights=NA) 

# In-degree. The number of connections in which a particular node is the target. 
degree(FBnet_M, mode="in")

# Out-degree. The number of relationships in which a node is the source.
degree(FBnet_M, mode="out")


#Group S
FBnet_S <- delete_vertices(FBnet, V(FBnet)$group != "S")
V(FBnet_S)$size <- V(FBnet_S)$friend_count/15
l <- layout_with_fr(FBnet_S)
plot(FBnet_S, layout=l, edge.arrow.size= 0.4, vertex.color= "#F23BBE", main="Group S (Spiel)",
     edge.color="#555555")

#to know the number of links in a network
edge_density(FBnet_S, loops=F)

#to know the numer of links a node has with other nodes
degree(FBnet_S)

# Betweenness degree
betweenness(FBnet_S, directed=T, weights=NA)

# Closeness degree
closeness(FBnet_S, mode="all", weights=NA) 

# In-degree. The number of connections in which a particular node is the target. 
degree(FBnet_S, mode="in")

# Out-degree. The number of relationships in which a node is the source.
degree(FBnet_S, mode="out")


#with the simple layout_with_fr function for this particular group did not show a good visualisation
#so, a layout_with_fr function with more specifications is used:

#Group W
FBnet_W <- delete_vertices(FBnet, V(FBnet)$group != "W")
V(FBnet_W)$color <- "cornsilk"
V(FBnet_W)$size <- V(FBnet_W)$friend_count/9
E(FBnet_W)$arrow.size=0.15
minC <- rep(-Inf, vcount(FBnet_W))
maxC <- rep(Inf, vcount(FBnet_W))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet_W, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)
l[1,]
plot(FBnet_W, layout=l, rescale=FALSE, main="Group W (Work)", xlim=range(l[,1]), edge.color="#555555",
     ylim=range(l[,2]))


#to know the number of links in a network
edge_density(FBnet_W, loops=F)

#to know the numer of links a node has with other nodes
degree(FBnet_W)

# Betweenness degree
betweenness(FBnet_W, directed=T, weights=NA)

# Closeness degree
closeness(FBnet_W, mode="all", weights=NA) 

# In-degree. The number of connections in which a particular node is the target. 
degree(FBnet_W, mode="in")

# Out-degree. The number of relationships in which a node is the source.
degree(FBnet_W, mode="out")


#--------- 5. Re-plot the network containing group F Facebook users for only males, 
#and for only females using the same loyout style. 

dev.off() #to delete the screen for plotting before to plot again
#par(mfrow=c(1,2)) #to locate 2 plot in the same screen

#For only males
FBnet_F_Male <- delete_vertices(FBnet_F, V(FBnet_F)$sex != "male")
V(FBnet_F_Male)$color <- "lightblue"
V(FBnet_F_Male)$size <- V(FBnet_F_Male)$friend_count/17
E(FBnet_F_Male)$arrow.size=0.2
minC <- rep(-Inf, vcount(FBnet_F_Male))
maxC <- rep(Inf, vcount(FBnet_F_Male))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet_F_Male, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)
l[1,]
plot(FBnet_F_Male, layout=l, rescale=FALSE, xlim=range(l[,1]), main="Group F for only males", 
     edge.color="#555555", ylim=range(l[,2]))

#to know the number of links in a network
edge_density(FBnet_F_Male, loops=F)

#to know the numer of links a node has with other nodes
degree(FBnet_F_Male)

# Betweenness degree
betweenness(FBnet_F_Male, directed=T, weights=NA)

# Closeness degree
closeness(FBnet_F_Male, mode="all", weights=NA) 

# In-degree. The number of connections in which a particular node is the target. 
degree(FBnet_F_Male, mode="in")

# Out-degree. The number of relationships in which a node is the source.
degree(FBnet_F_Male, mode="out")

#An analysis  for hubs and authorities was also added to these results.
# Hubs and Authorities
hs <- hub_score(FBnet_F_Male, weights=NA)$vector #function to know the nodes that act as hub
as <- authority_score(FBnet_F_Male, weights=NA)$vector #function to know the nodes that act as authority

dev.off()
par(mfrow=c(1,2))

plot(FBnet_F_Male, vertex.size=hs*35, layout=l, edge.color="#555555", main="Hubs (males)", rescale=FALSE, xlim=range(l[,1]), 
     ylim=range(l[,2]))
plot(FBnet_F_Male, vertex.size=as*35, layout=l, edge.color="#555555", main="Authorities (males)", rescale=FALSE, xlim=range(l[,1]),
     ylim=range(l[,2]))


#For only females
FBnet_F_Fema <- delete_vertices(FBnet_F, V(FBnet_F)$sex != "female")
V(FBnet_F_Fema)$color <- "pink"
V(FBnet_F_Fema)$size <- V(FBnet_F_Fema)$friend_count/17
E(FBnet_F_Fema)$arrow.size=0.2
minC <- rep(-Inf, vcount(FBnet_F_Fema))
maxC <- rep(Inf, vcount(FBnet_F_Fema))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet_F_Fema, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)
l[1,]
plot(FBnet_F_Fema, layout=l, rescale=FALSE, xlim=range(l[,1]), main="Group F for only females", 
     edge.color="#555555", ylim=range(l[,2]))

#to know the number of links in a network
edge_density(FBnet_F_Fema, loops=F)

#to know the numer of links a node has with other nodes
degree(FBnet_F_Fema)

# Betweenness degree
betweenness(FBnet_F_Fema, directed=T, weights=NA)

# Closeness degree
closeness(FBnet_F_Fema, mode="all", weights=NA) 

# In-degree. The number of connections in which a particular node is the target. 
degree(FBnet_F_Fema, mode="in")

# Out-degree. The number of relationships in which a node is the source.
degree(FBnet_F_Fema, mode="out")

#An analysis  for hubs and authorities was also added to these results.
# Hubs and Authorities
hs <- hub_score(FBnet_F_Fema, weights=NA)$vector #function to know the nodes that act as hub
as <- authority_score(FBnet_F_Fema, weights=NA)$vector #function to know the nodes that act as authority

dev.off()
par(mfrow=c(1,2))

plot(FBnet_F_Fema, vertex.size=hs*40, layout=l, edge.color="#555555", main="Hubs (females)", rescale=FALSE, xlim=range(l[,1]), 
     ylim=range(l[,2]))
plot(FBnet_F_Fema, vertex.size=as*40, layout=l, edge.color="#555555", main="Authorities (females)", rescale=FALSE, xlim=range(l[,1]),
     ylim=range(l[,2]))



#--------- 6.	Using the cluster_optimal function, detect the communities in task 5 (using layout_with_fr). 

# Community detection (groups within a subnetwork):

#======== Group F (females and males) with communities

#for males
minC <- rep(-Inf, vcount(FBnet_F_Male))
maxC <- rep(Inf, vcount(FBnet_F_Male))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet_F_Male, minx=minC, maxx=maxC,miny=minC, maxy=maxC)
clpF_m <- cluster_optimal(FBnet_F_Male)

dev.off()
par(mfrow=c(1,2))

plot(FBnet_F_Male, layout=l, rescale=FALSE,main="Group F (males)", xlim=range(l[,1]), edge.color="#555555", 
     ylim=range(l[,2]))

plot(clpF_m, FBnet_F_Male, layout=l, main="Communities in Group F (males)", rescale=FALSE, xlim=range(l[,1]), 
     ylim=range(l[,2]))


#for females
minC <- rep(-Inf, vcount(FBnet_F_Fema))
maxC <- rep(Inf, vcount(FBnet_F_Fema))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet_F_Fema, minx=minC, maxx=maxC,miny=minC, maxy=maxC)
clpF_f <- cluster_optimal(FBnet_F_Fema)

dev.off()
par(mfrow=c(1,2))

plot(FBnet_F_Fema, layout=l, rescale=FALSE,main="Group F (females)", xlim=range(l[,1]), edge.color="#555555", 
     ylim=range(l[,2]))

plot(clpF_f, FBnet_F_Fema, layout=l, main="Communities in Group F (females)", rescale=FALSE, xlim=range(l[,1]), 
     ylim=range(l[,2]))



#--------- 7.	Simplify the original network showing only those nodes with a degree of greater than 10 

# -------- Original network with node size based on friend_count
#V(FBnet)$size <- V(FBnet)$friend_count/13

#--------- Add blue colour for males and red for females
dev.off()
# Original FBnet

V(FBnet)$color <- ifelse(V(FBnet)$sex=="female", "red", "blue") #colours are assigned according to gender
V(FBnet)$size <- V(FBnet)$friend_count/12 #to set an suitable size for nodes
E(FBnet)$arrow.size=0.3 #to set a suitable size for arrows
minC <- rep(-Inf, vcount(FBnet)) #variables used for parameters used in the layaout_with_fr function
maxC <- rep(Inf, vcount(FBnet))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet, minx=minC, maxx=maxC,miny=minC, maxy=maxC)
l[1,]
plot(FBnet, vertex.label=NA, edge.arrow.size=0.13, layout=l, main="Original FBnet",  #A plot is obtained based on the previous paarmeters
     edge.curved=0, rescale=FALSE, edge.color="#555555", xlim=range(l[,1]), ylim=range(l[,2]))


# Simplified FBnet
# The number of incident edges to a vertex which are smaller than 10 are deleted
FBnet.sim <- delete_vertices(FBnet, V(FBnet)[degree(FBnet) < 10]) 
V(FBnet.sim)$color <- ifelse(V(FBnet.sim)$sex=="female", "pink", "lightblue")

V(FBnet.sim)$size <- V(FBnet.sim)$friend_count/18
E(FBnet.sim)$arrow.size=0.19
minC <- rep(-Inf, vcount(FBnet.sim))
maxC <- rep(Inf, vcount(FBnet.sim))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet.sim, minx=minC, maxx=maxC,miny=minC, maxy=maxC)
l[1,]
#vertex Labels are shown
plot(FBnet.sim, layout=l, main="Simplified FBnet",
     edge.curved=0, rescale=FALSE, edge.color="#777777", xlim=range(l[,1]), ylim=range(l[,2])) 

#legend is added
colrs <- c("pink", "lightblue")
legend(x=-1.9, y=4.7, c("female","male"), pch=21, col="#777777", pt.bg=colrs, pt.cex=2, 
       cex=.8, bty="n", ncol=1) 



# Hubs and Authorities
hs.sim <- hub_score(FBnet.sim, weights=NA)$vector
as.sim <- authority_score(FBnet.sim, weights=NA)$vector
l <- layout_with_fr(FBnet.sim)

E(FBnet.sim)$arrow.size=0.07
minC <- rep(-Inf, vcount(FBnet.sim))
maxC <- rep(Inf, vcount(FBnet.sim))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet.sim, minx=minC, maxx=maxC,miny=minC, maxy=maxC)
l[1,]

dev.off()
par(mfrow=c(1,2))

#The node size in each of these plots was 10 times the value of the hub and authority scores. This result is 
#shown in the report. However, it is not a good visualisation, so this value was changed into a more suitable one.

plot(FBnet.sim, vertex.size=hs.sim*32, layout=l, edge.color="#666666", 
    main="Hubs in FBnet", rescale=FALSE, xlim=range(l[,1]), ylim=range(l[,2]))

plot(FBnet.sim, vertex.size=as.sim*32, layout=l, edge.color="#666666",
     main="Authorities in FBnet", rescale=FALSE, xlim=range(l[,1]), ylim=range(l[,2]))

# Observation  -----> using 10 times nodes are too small, so I changed it to get better visualisation


# 8.	For the simplified network calculate the betweeness (ignoring loops) of each node and edge density value.
# Identify the node with the highest betweeness value in this network.

# Betweeness degree
betweenness(FBnet.sim, directed=T, weights=NA)

# Closeness degree
closeness(FBnet.sim, mode="all", weights=NA) 

# Centrality & centralization
# Degree (number of links each node has)
degree(FBnet.sim)

# In-degree ()
degree(FBnet.sim, mode="in")

# Out-degree
degree(FBnet.sim, mode="out")

# Density. The proportion of present edges from all possible ties.
edge_density(FBnet.sim, loops=F)


#  9. Obtention of the whole network with groups separated by colours.

#Import FB_Nodes_v2.CSV file
links <- FB_LINKS #this is the same file used originally for links and saved in the links object
nodes <- FB_Nodes_v2 #nodes attributes are imported from a new CSV file and saved in the nodes object

#nodes and links are used to create a network and stored in a variable called FBnet.v2
FBnet.v2 <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

#A function is executed (http://rpubs.com/niyer/389851) to generate transparent colours
#that allow detecting arrows behind the nodes especially for those bigger.
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
} 

sub_color <- transp(c("pink", "yellow", "lightblue", "blue","#33FF00", "#F5A328",
                      "#F23BBE", "cornsilk"), alpha=0.5)

minC <- rep(-Inf, vcount(FBnet.v2))
maxC <- rep(Inf, vcount(FBnet.v2))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet.v2, minx=minC, maxx=maxC,miny=minC, maxy=maxC)
l[1,] #layout_with_fr is used to spread the network

E(FBnet.v2)$arrow.size=0.11 #arrow size and colour are set
E(FBnet.v2)$arrow.color="#555555"

plot(FBnet.v2, 
     #vertex.label=NA, 
     layout=l,
     vertex.size=V(FBnet.v2)$friend_count/70,
     vertex.label.cex=0.6, 
     vertex.label.color="black",
     main= "Facebook network with coloured groups",
     vertex.color=sub_color[V(FBnet.v2)$grp]) # set the subgroup colors by group


colrs <- c("pink", "yellow", "lightblue", "blue","#33FF00", "#F5A328", "#F23BBE", "cornsilk") #colors for each group
legend(x=-1.8, y=1.1, c("B (Book Club)","C (College)", "F (Family)", "G (Graduate School)", #legend is added
                        "H (High School)", "M (Music)", "S (Spiel)", "W (Work)"), pch=21, 
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


#Dendrogram for FBnet.v2
ceb <- cluster_edge_betweenness(FBnet.v2) #ceb for cluster_edge_betweenness
dendPlot(ceb, mode="hclust") #A dendrogram for FBnet.v2 is plotted (https://kateto.net/netscix2016.html)


#Community detection based on edge betweenness 
E(FBnet.v2)$arrow.size=0.07
minC <- rep(-Inf, vcount(FBnet.v2))
maxC <- rep(Inf, vcount(FBnet.v2))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(FBnet.v2, minx=minC, maxx=maxC,miny=minC, maxy=maxC)
l[1,]

#Communities are detected by colour wrapping
plot(ceb, FBnet.v2, layout=l, rescale=FALSE, main="Community detection for FBnet.v2", xlim=range(l[,1]),
     edge.color="#555555",vertex.size=V(FBnet.v2)$friend_count/13, ylim=range(l[,2]))

#-------------------------------------- end --------------------------------------------

