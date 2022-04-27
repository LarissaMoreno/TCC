require(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(recommenderlab)
library(stringr)
library(splitstackshape)
library(wordcloud2)
library(data.table)
#setwd("~/Larissa/TCC")
#load("C:/Users/gabri/Documents/Larissa/TCC/descritiva.RData")
anime=read.csv("animes2.csv")
rating <-read.csv("rating.csv")

#data clean
rating=rating%>%filter(rating!=-1)#remover o grupo de pessoas que assistiram mas nao avaliaram
rating$user_id <- as.factor(rating$user_id)

#criando a matriz usuario X item  esparsa do tipo realRatingMatrix
ratingMatrix <- as(rating, "realRatingMatrix")
ratingMatrix = ratingMatrix[rowCounts(ratingMatrix)>=50 , colCounts(ratingMatrix)>=100]

head(rating)
head(anime[c(1,2,6,7)])

#numero de animes
length(unique(anime$anime_id))

#numero de usuarios
length(unique(rating$user_id))

#numeros de avaliações
dim(rating)[1]


#analisadno a espasiasidade da matriz
vector_ratings <- as.vector(ratingMatrix@data)
table_ratings <- table(vector_ratings)
table_ratings


prop.table(table_ratings)

dim(ratingMatrix@data)

#######################
views_per_movie <- colCounts(ratingMatrix)
views <- as.integer(names(views_per_movie))
user<- rowCounts(ratingMatrix)
user <- as.integer(names(user))

rating=rating%>%filter(user_id %in% user, anime_id %in% views)


#media de avaliações por usuario
media.user.rating=rating%>%
  group_by(user_id)%>%
  summarise(n=n())
summary(media.user.rating$n)

hist(media.user.rating$n,xlim = c(0,900),
     breaks = seq(min(media.user.rating$n), max(media.user.rating$n), length.out = 150),
     main="Distribuição de avaliações por usuário",xlab="Número de avaliações")

#a maioria dos usuarios avaliaram menos de 200 animes


#media de avaliações por anime
media.anime.rating=rating%>%
  group_by(anime_id)%>%
  summarise(n=n())
summary(media.anime.rating$n)

hist(media.anime.rating$n,xlim = c(0,1100),
     breaks = seq(min(media.anime.rating$n), max(media.anime.rating$n), length.out = 600),
     main="Distribuição de avaliações por anime",xlab="Número de animes")
#a maioria dos animes receberao pelo menos 57 avaliações


#Distribuição Valores das avaliações
summary(rating$rating)
rating.dist=rating%>%group_by(rating)%>%summarise(Freq=n())
hist(rating$rating,main="Distribuição das avaliações",xlab="Avaliação",ylab="Frequência",breaks = 10)


#Qual anime é mais avaliados
data=data.frame(anime_id=views,views_per_movie)
data$anime_id=factor(data$anime_id)
data=data%>%inner_join(anime,by="anime_id")
x1=data%>%arrange(desc(data$views_per_movie))%>%head(10)
x1

x1=rating%>%filter(anime_id %in% x1$anime_id)

x1=x1%>%group_by(anime_id)%>%summarise(mean(rating))
median(x1$`mean(rating)`)



#animes vistos
rating1=rating <-read.csv("rating.csv")
rating1$rating[rating1$rating==-1]=1
rating1=rating1%>%filter(anime_id %in% views, user_id %in% user)
rating1=rating1%>%group_by(anime_id)%>%summarise(n=n())
rating1$anime_id=factor(rating1$anime_id)
rating1%>%arrange(desc(n))%>%head(10)%>%inner_join(anime,by="anime_id")%>%select(c(2,3))



#densidade da Média das avaliações por usuários
rating.dist=rating %>% 
  group_by(user_id) %>% 
  summarise(m = mean(rating)) 

plot(density(rating.dist$m),main="Média das avaliações por usuários",xlab="Avaliação Média",ylab="")
hist(rating.dist$m,main="Média das avaliações por usuários",xlab="Avaliação Média",ylab="",breaks = 50)

#Heatmap (nao normalizado) para os usuarios que mais avaliam e para os top animes
#e usuarios que avaliaram pelo menos 150 animes
min_n_movies <- quantile(rowCounts(ratingMatrix), 0.9994)#top animes
min_n_users <- quantile(colCounts(ratingMatrix), 0.996)#top users
min_n_movies

image(ratingMatrix[rowCounts(ratingMatrix) > min_n_movies,
                   colCounts(ratingMatrix) > min_n_users])


x=ratingMatrix[rowCounts(ratingMatrix) > min_n_movies,
               colCounts(ratingMatrix) > min_n_users]

x@data@Dimnames[[2]]
y=anime%>%filter(anime_id %in% x@data@Dimnames[[2]])%>%select(c(1,2))
arrange(y,name)

#barplot tipo de anime
anime1=anime%>%filter(anime_id %in% views)
t=anime1 %>%drop_na()%>% group_by(type) %>% summarise(Count=n())
t$type[t$type=="Music"]="Musical"
t$type[t$type=="Movie"]="Filme"
t$type[t$type=="Special"]="Especial"
t=t%>%arrange(Count)

barplot(height=t$Count, names=t$type,main="Tipos de Anime",ylab = "Frequência",xlab= "Tipo de Programa")


#Generos existentes 
anime1=anime%>%filter(anime_id %in% views)
genre=data.frame(genre=anime1$genre)

type=cSplit_e(as.data.table(genre), "genre", ",", type = "character", fill = 0)
t1=colSums(type[,-1])
t1=as.data.frame(t1)
t1=data.frame(word=rownames(t1),freq=t1$t)


t1$word=str_replace(t1$word,"genre_","")
rownames(t1)=t1$name
wordcloud2(t1,shape = "circle",size=0.5)

t1$prop=t1$freq/sum(t1$freq)
