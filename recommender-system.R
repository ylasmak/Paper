library(tidyverse)
library(reshape2)
library(ggplot2)
mydata = read.csv("../input/movieratedataset/ratings.csv")
#norm_df <- read_csv("../input/norm_df.csv")

normalizeRatings <- function (df, rowcol) {
  normalize <- function (x) {
    x - mean(x)
  }
  norm <- apply(df, rowcol, normalize)
  if (rowcol == 1) {
    result <- data.frame(t(norm))
  } else {
    result <- data.frame(norm)
  }
  # result$avg <- apply(df, rowcol, mean, na.rm=TRUE)
  result
}
#if the cosine similartity between two vectors is less than the threshold ,we set the similarity to 0
similarity_threshold = 0.4

#minumun number of similar movie to predict the rating similarity for the unrated movie
item_item_CF = 3

#minimun predicted rate to remoand a movie
m_predectied_rate = 2.9

#preparing data matrix , rows as movies , columns as users , and rate as vlues
tmp =mydata['movieId']
movieVector = unlist(tmp, use.names=FALSE)
moviesList = unique (movieVector)
nbMovies  = length(moviesList)

tmp =mydata['userId']
userVector = unlist(tmp, use.names=FALSE)
userList  = unique(userVector)
nbUsers  = length(userList)

df_matrix_movies_users <- data.frame(matrix( nrow = nbMovies,ncol = nbUsers)) 

rownames(df_matrix_movies_users) <- moviesList
colnames(df_matrix_movies_users) <- userList

df_matrix_movies_users[is.na(df_matrix_movies_users)] <- 0


for(i in 1:nrow(mydata))
{
  row  = mydata[i,]
  
  x  =as.character( row['movieId'][1,1])
  y  =as.character(  row['userId'][1,1])
  df_matrix_movies_users[x,y] <- row['rating'][1,1]
  
  
}

#End preparing data set

number_of_rating_by_user <- unname(apply(df_matrix_movies_users,2,function(x)sum(x!=0)))

h<-hist(number_of_rating_by_user, breaks=10, col="blue", xlab="number of rate per user") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)


number_of_rating_by_movie <- unname(apply(df_matrix_movies_users,1,function(x)sum(x!=0)))

h<-hist(number_of_rating_by_movie, breaks=10, col="red", xlab="number of rating by movie") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit<- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)


#cleaning memory
row = NULL
tmp = NULL 
mydata = NULL

# normalizing the rating dataset
norm_df <- normalizeRatings(df_matrix_movies_users, 1)

#Calcuating cosine similarity betwenn each movies
Matrix <- as.matrix(norm_df)
movies_similarity_matrix <- Matrix / sqrt(rowSums(Matrix * Matrix))
movies_similarity_matrix <- movies_similarity_matrix %*% t(movies_similarity_matrix)

# cleaning memory
Matrix =NULL
norm_df = NULL

#Fltring similarting matrix by the threshold value

df_sim_filter = apply(movies_similarity_matrix,1,function(x)(x > similarity_threshold))

df_sim_filter[df_sim_filter==TRUE] <- 1
df_sim_filter[df_sim_filter==FALSE] <- 0

for(i in 1:nrow(movies_similarity_matrix) )
{
  movies_similarity_matrix[i,i] <-0
}
movies_similarity_matrix <- movies_similarity_matrix * df_sim_filter

#similarty matrix contains onlys values above than the threshold value
df_sim_filter = NULL



movies_similarity_matrix_melt <- melt(movies_similarity_matrix)
movies_similarity_matrix_melt = subset(movies_similarity_matrix_melt, value !=0)


ggplot(data = movies_similarity_matrix_melt, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "white", high = "black", mid = "gray", 
   midpoint = 0, limit = c(0.4,1), space = "Lab", 
   name="Movie similarity\nCorrelation") +
  theme_minimal()+ 
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed() 

#Sart preparing our predication recommendation matrix

#Get the list of rate for the given user userId
df_recomendation <- data.frame(matrix( nrow = nbMovies,ncol = nbUsers)) 
rownames(df_recomendation) <- moviesList
colnames(df_recomendation) <- userList

df_recomendation[is.na(df_recomendation)] <- 0

print("Start loop")
print(Sys.time())

for(u in userList)
{
  u_user = as.character(u) 
  
  u1_movies_list = df_matrix_movies_users[,u_user]
  u1_movies_list2  =u1_movies_list
  u1_movies_list2[u1_movies_list2>0] <-1
  
   rated_movies = moviesList *u1_movies_list2
  
  not_rated_movies = moviesList - rated_movies
  
  not_rated_movies <- not_rated_movies[not_rated_movies!=0]
  not_rated_movies <- as.character( not_rated_movies)
  
  rated_movies = rated_movies[rated_movies!=0]
  rated_movies = as.character(rated_movies)
  

  semliarReatedMovies = data.frame( movies_similarity_matrix[ as.character( not_rated_movies),rated_movies])
  colnames(semliarReatedMovies) <- rated_movies
  semliarReatedMovies$not_null_count = apply(semliarReatedMovies,1,function(a)sum(a != 0))
  semliarReatedMovies = subset(semliarReatedMovies,not_null_count>=item_item_CF)
  
  
  nearest_neighbor <- function (x) {
    
    x = x[1:(length(x)-1)]
    
    movies = as.character( names(sort(x,decreasing = TRUE)[1:item_item_CF]))
    similarity =   as.numeric(unname (sort(x,decreasing = TRUE)[1:item_item_CF]))
    
    c(movies,similarity)
  }
  
  
  if(nrow(semliarReatedMovies) > 0)
  {
    neighbors_p = apply(semliarReatedMovies,1,nearest_neighbor)
    neighbors_p =  t(neighbors_p)
    
    
    v_1 = as.character( c(neighbors_p[,1],neighbors_p[,2],neighbors_p[,3]))
    
    res_p <- df_matrix_movies_users[v_1,u_user,drop=FALSE]
    
    c1 <- neighbors_p[,1]
    c2 <- neighbors_p[,2]
    c3 <-neighbors_p[,3]
    
    c_array = (cbind(res_p[c1,],res_p[c2,],res_p[c3,]))
    neighbors_p = cbind(neighbors_p,c_array)
    neighbors_p  = cbind(neighbors_p,row.names(neighbors_p))
    
    neighbors_t =  cbind(as.numeric(neighbors_p[,4]),as.numeric(neighbors_p[,4]),as.numeric(neighbors_p[,4]))
    
    
    neighbors_v = rowSums (neighbors_t*c_array )/rowSums(neighbors_t)
    
    neighbors_p = cbind(neighbors_p,neighbors_v)
    
    
    neighbors_p <- subset(neighbors_p, neighbors_p[,"neighbors_v"] >m_predectied_rate)
    neighbors_p <-as.character( neighbors_p[,10])
    df_recomendation[neighbors_p,u_user]<-1
    
  }
  
}
print(Sys.time())

number_of_recomendation_by_user <- unname(apply(df_recomendation,2,function(x)sum(x!=0)))

h<-hist(number_of_recomendation_by_user, breaks=10, col="orange", xlab="number of recomendation per user") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="orange", lwd=2)


number_of_recomendation_by_movie <- unname(apply(df_recomendation,1,function(x)sum(x!=0)))

h<-hist(number_of_recomendation_by_movie, breaks=10, col="green", xlab="number of recomendation by movie") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit<- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="green", lwd=2)



write.csv(df_recomendation, file = "../df_recomendation.csv")
