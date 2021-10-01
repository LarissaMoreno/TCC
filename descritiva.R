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


anime=read.csv("animes2.csv")
rating <-readRDS("rating1.rds")

#data clean
rating=rating%>%filter(rating!=-1)#remover o grupo de pessoas que assistiram mas nao avaliaram
rating$user_id <- as.factor(rating$user_id)

#criando a matriz usuario X item  esparsa do tipo realRatingMatrix
ratingMatrix <- as(rating, "realRatingMatrix")

#e usuarios que avaliaram pelo menos 150 animes
ratingMatrix = ratingMatrix[rowCounts(ratingMatrix)>=150 , colCounts(ratingMatrix)>=150]

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



#Distribuição Valores das avaliações
summary(rating$rating)
rating.dist=rating%>%group_by(rating)%>%summarise(Freq=n())
hist(rating$rating,main="Distribuição das avaliações",xlab="Avaliação",ylab="Frequência",breaks = 10)


#Qual anime é mais visto
anime_views = colCounts(ratingMatrix) # count views for each movie
table_views = data.frame(anime_id = names(anime_views) , views = anime_views) #create dataframe of views
table_views = table_views[order(table_views$view , decreasing = TRUE),] #sort by number of views
table_views$anime_id=as.numeric(table_views$anime_id)

table_views=inner_join(table_views,anime,by="anime_id")
head(table_views[,c(2,3)],10)



#densidade da Média das avaliações por usuários
rating.dist=rating %>% 
  group_by(user_id) %>% 
  summarise(m = mean(rating)) 

plot(density(rating.dist$m),main="Média das avaliações por usuários",xlab="Média",ylab="")

#Heatmap (nao normalizado) para os usuarios que mais avaliam e para os top animes

min_n_movies <- quantile(rowCounts(ratingMatrix), 0.9985)
min_n_users <- quantile(colCounts(ratingMatrix), 0.99)
min_n_movies

image(ratingMatrix[rowCounts(ratingMatrix) > min_n_movies,
                   colCounts(ratingMatrix) > min_n_users])


#barplot tipo de anime
t=anime %>%drop_na()%>% group_by(type) %>% summarise(Count=n())

barplot(height=t$Count, names=t$type,main="Tipos de Anime",ylab = "Frequência",xlab= "Tipo de Programa")


#Generos existentes 
genre=data.frame(genre=anime$genre)

type=cSplit_e(as.data.table(genre), "genre", ",", type = "character", fill = 0)
t1=colSums(type[,-1])
t1=as.data.frame(t1)
t1=data.frame(word=rownames(t1),freq=t1$t)

t1$word=str_replace(t1$word,"genre_","")
rownames(t1)=t1$name
wordcloud2(t1,size=0.8,shape = "circle")



#genero na base de dados rating e proporção dos usuarios
anime$anime_id=factor(anime$anime_id)
anime_genre=rating%>%group_by(anime_id)%>%summarise(n=n())
anime_genre2=anime%>%filter(anime_id %in% anime_genre$anime_id)
anime_genre=anime_genre%>%filter(anime_id %in% anime_genre2$anime_id)
anime_genre$anime_id=factor(anime_genre$anime_id)
anime_genre=inner_join(anime_genre,anime_genre2,by="anime_id")
rm(anime_genre2)
anime_genre <- select(anime_genre, c(1, 2, 4))


type=cSplit_e(as.data.table(anime_genre), "genre", ",", type = "character", fill = 0)

#proporção de usuarios
type2=type
type2=type2[,-c(1,2,3)]
type2=sweep(type2,  1, FUN = `*`,type$n)
type2$n=type$n
c=data.frame(count=colSums(type2))
c$prop=c/c[44,1]
c=colSums(type2)


