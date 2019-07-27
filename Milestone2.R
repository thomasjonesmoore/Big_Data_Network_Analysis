setwd("/home/kimbalm2/CSCI424/Project/Final Project")
#install.packages("plyr") # you may need to uncomment this and install on your machine
library(plyr)#used for the join function to join two df without reordering columns
library(igraph)
keep <- c("userid", "itemid", "rating")#data columns to keep in the rating data
movie_drop<- c("IMDbURL", "videoreleasedate", "unknown", "movieid") #data columns to drop in movie data
rating_data <- read.csv("u.data", header = TRUE, sep = "\t")# user  movie rating  time

movie_data <- read.csv("u.item", header = TRUE,sep = "|") #movieid|movietitle|releasedate|
                                                          #videoreleasedate|IMDbURL|unknown|Action|Adventure|Animation|
                                                          #Children's|Comedy|Crime|Documentary|Drama|Fantasy|Film-Noir|Horror|Musical
                                                          #|Mystery|Romance|Sci-Fi|Thriller|War|Western

user_data <- read.csv("u.user", header = TRUE, sep = "|")#user gender and occupation and age

movieTitles <- movie_data$movietitle


new_data<- rating_data # copy of rating data to mess with

orderedMovies <- movieTitles[new_data$itemid]#get the movies in order they appear in the data file

new_data$itemid <- orderedMovies #change the item id numbers into the actual movie names

movies <- new_data$itemid #list of movie names
users <- rating_data$userid #list of user names

#drop the timestamp column but reassign the rating data to the new data frame with movie id being the name
rating_data <- new_data[keep]
#keep all the columns not in the drop vector
movie_data<-movie_data[ ,!(names(movie_data) %in% movie_drop)]

colnames(movie_data)[colnames(movie_data) == "movietitle"]<-"itemid"

#data with all of the movie info and the edges between user and movie
aggregatedf <- join(rating_data,movie_data)

#aggregated_data <- rating_data

#graph the new network -> currently all of the attributes of the graph are edge attributes, which may need fixing
g <- graph_from_data_frame(rating_data)


#aggregated_edges <- E(g)

#This creates a type attribute based on which node it is.
nodes <- V(g)#get all the nodes
#get the user nodes
unodes <- nodes[which( nodes$name %in% users)]
# get the movie nodes
mnodes <- nodes[which(nodes$name %in% movies)]
# set the movie nodes type to movie
g <- set_vertex_attr(g, "type", mnodes, "movie")
# set the employee nodes type to user
g <- set_vertex_attr(g, "type", unodes, "user")
#end of this


#divide sums by degree to get average rating
#but how?
user_edge_sums <- strength(g,vids = unodes, mode="total", weights = E(g)$rating)
user_degree <-degree(g, v = unodes, mode ="out") 

avg_ratings <- user_edge_sums/user_degree

g<- set_vertex_attr(g,"avgRating",unodes,avg_ratings)#add the average rating per user as a node attribute 

#this loop takes a bit
for(user in unodes){
  users_Movies <- subset(aggregatedf, user == aggregatedf$userid)
  genre_sums <- colSums(users_Movies[5:22])
  genre_percentages <- (genre_sums/nrow(users_Movies))*100
  #print(users)
  user_node <- V(g)[which(nodes$name ==user)]
  #now assign these values to node attributes
  g <- set_vertex_attr(g,"percentAction", user_node, genre_percentages[1])
  g <- set_vertex_attr(g,"percentAdventure", user_node, genre_percentages[2])
  g <- set_vertex_attr(g,"percentAnimation", user_node, genre_percentages[3])
  g <- set_vertex_attr(g,"percentChildrens", user_node, genre_percentages[4])
  g <- set_vertex_attr(g,"percentComedy", user_node, genre_percentages[5])
  g <- set_vertex_attr(g,"percentCrime", user_node, genre_percentages[6])
  g <- set_vertex_attr(g,"percentDocumentary", user_node, genre_percentages[7])
  g <- set_vertex_attr(g,"percentDrama", user_node, genre_percentages[8])
  g <- set_vertex_attr(g,"percentFantasy", user_node, genre_percentages[9])
  g <- set_vertex_attr(g,"percentFilmNoir", user_node, genre_percentages[10])
  g <- set_vertex_attr(g,"percentHorror", user_node, genre_percentages[11])
  g <- set_vertex_attr(g,"percentMusical", user_node, genre_percentages[12])
  g <- set_vertex_attr(g,"percentMystery", user_node, genre_percentages[13])
  g <- set_vertex_attr(g,"percentRomance", user_node, genre_percentages[14])
  g <- set_vertex_attr(g,"percentSciFi", user_node, genre_percentages[15])
  g <- set_vertex_attr(g,"percentThriller", user_node, genre_percentages[16])
  g <- set_vertex_attr(g,"percentWar", user_node, genre_percentages[17])
  g <- set_vertex_attr(g,"percentWestern", user_node, genre_percentages[18])
}

#loop for each user, filter movies that a user has subsets.
#subset
#subset(moviedf, movieid %in% vector)#
#df[5:10] run sum on columns <-


#adding node attributes for the movies
g<- set_vertex_attr(g,"Action",mnodes,movie_data$Action)
g<- set_vertex_attr(g,"Adventure",mnodes,movie_data$Adventure)
g<- set_vertex_attr(g,"Animation",mnodes,movie_data$Animation)
g<- set_vertex_attr(g,"Childrens",mnodes,movie_data$Children.s)
g<- set_vertex_attr(g,"Comedy",mnodes,movie_data$Comedy)
g<- set_vertex_attr(g,"Crime",mnodes,movie_data$Crime)
g<- set_vertex_attr(g,"Documentary",mnodes,movie_data$Documentary)
g<- set_vertex_attr(g,"Drama",mnodes,movie_data$Drama)
g<- set_vertex_attr(g,"Fantasy",mnodes,movie_data$Fantasy)
g<- set_vertex_attr(g,"Film.Noir",mnodes,movie_data$Film.Noir)
g<- set_vertex_attr(g,"Horror",mnodes,movie_data$Horror)
g<- set_vertex_attr(g,"Musical",mnodes,movie_data$Musical)
g<- set_vertex_attr(g,"Mystery",mnodes,movie_data$Mystery)
g<- set_vertex_attr(g,"Romance",mnodes,movie_data$Romance)
g<- set_vertex_attr(g,"Sci.Fi",mnodes,movie_data$Sci.Fi)
g<- set_vertex_attr(g,"Thriller",mnodes,movie_data$Thriller)
g<- set_vertex_attr(g,"War",mnodes,movie_data$War)
g<- set_vertex_attr(g,"Western",mnodes,movie_data$Western)
#add the other user attributes
g<- set_vertex_attr(g,"Gender", unodes,user_data$gender)
g<- set_vertex_attr(g,"Age", unodes,user_data$age)
g<- set_vertex_attr(g,"Occupation", unodes,user_data$occupation)

unodes <- nodes[which( nodes$name %in% users)]
# get the movie nodes
mnodes <- nodes[which(nodes$name %in% movies)]

allnodes <- V(g)
#this makes the projections type attribute logical
for (node in allnodes){
  if (vertex_attr(g, "type", index = node) == "movie"){
    g <- set_vertex_attr(g, "type", node, TRUE)
  }
  else{
    g <- set_vertex_attr(g, "type", node, FALSE)
  }
}
allnodes$type <- type.convert(allnodes$type, as.is = TRUE)

#induced sub graph
#g[1:g] for v
#project that graph and check the edges.
#

#high indexes so I get users and movies
small_g <- induced_subgraph(g,vids = V(g)[900:1100])
small_proj <- bipartite_projection(small_g, multiplicity = TRUE, probe1 = NULL)

projection <-
  bipartite.projection(g, multiplicity = TRUE, probe1 = NULL)

user_graph <- projection$proj1# user projection START HERE for descriptive statistics
#movies_graph <- projection$proj2 




#basic plot
#plot(g, vertex.label = NA, vertex.color = adjustcolor("SkyBlue2", alpha.f = .4), 
     #edge.curved=0.2)  
#some other parameters I was messing with: vertex.size=degree(g)/1.2, layout = layout.fruchterman.reingold)

total_degree <- degree(g)
in_degree <- degree(g, mode = "in")
out_degree <- degree(g, mode = "out")
transitive <- transitivity(user_graph, type = "undirected")
mean(out_degree)
mean(in_degree)
mean(total_degree)
reciprocity <- reciprocity(g)

edge_density(user_graph)

graph_without_isolates <- delete.vertices(simplify(g), degree(g) == 0)



