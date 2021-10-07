#setwd("~/Larissa/TCC")
#save.image("content-based.RData")
#load("C:/Users/gabri/Documents/Larissa/TCC/content-based.RData")
anime=read.csv("animes2.csv")
genre=data.frame(genre=anime$genre)
library(splitstackshape)
library(data.table)
#dummificando o banco de dados
genre=cSplit_e(as.data.table(genre), "genre", ",", type = "character", fill = 0)
#removendo o genero total
genre=genre[,-1]
library(stringr)
#renomeando as colunas
colnames(genre)=str_replace(colnames(genre),"genre_","")

#aplicando o algoritimo kmeans

##estimando k
library(purrr)
set.seed(30)
tot_withinss <- map_dbl(1:10,  function(k){
 model <- kmeans(x = genre, centers = k)
model$tot.withinss
})

elbow_df <- data.frame(
 k = 1:10,
tot_withinss = tot_withinss
)

# Plot the elbow plot
plot(1:10, elbow_df$tot_withinss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Número de clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow Plot")
abline(v=3,lty=2, lwd=3)


#Acredito que o mehor k é k=3
animecluster= model <- kmeans(x = genre, centers = 3)
anime$cluster=animecluster$cluster

#total de animes por cluster
table(anime$cluster)

#explorando os cluster
genre1=genre
genre1$cluster=anime$cluster
library(plyr)
t1=genre1%>%group_by(cluster)%>% summarise_each(funs(sum))#somando generos por grupo de cluster

t1=t(t1)
t1=t1[-1,]#removendo a linha que contem o numero do cluster
t1=as.data.frame(t1)

#separando cada cluster em um df com os nomes dos generos e as frequencias
cluster1=data.frame(word=rownames(t1),freq=t1$V1)
cluster2=data.frame(word=rownames(t1),freq=t1$V2)
cluster3=data.frame(word=rownames(t1),freq=t1$V3)
rownames(cluster1)=cluster1$word#necessario para fazer a wordcloud
rownames(cluster2)=cluster2$word
rownames(cluster3)=cluster3$word

library(wordcloud2)
wordcloud2(cluster1,size=0.8,shape = "circle")
wordcloud2(cluster2,size=0.8,shape = "circle")
wordcloud2(cluster3,size=0.8,shape = "circle")




#Carregando ratinf dataframe
require(readr)
rating <-read.csv("rating.csv")
library(dplyr)
rating1=rating
rating=rating%>%filter(rating!=-1)#remover o grupo de pessoas que assistiram mas nao avaliaram
rating$user_id <- as.factor(rating$user_id)

#Criando uma função que recomenda items para cada usuario
myfunction=function(user.id,n){
  #subset rating dataframe pelo id selecionado
  x=filter(rating,user_id==user.id)%>%mutate(cluster=0)%>%arrange(anime_id)
  #encontrar o cluster dos animes vistos pelo id selecionado
  x=left_join(x,anime[,c(1,8)],by="anime_id")%>%select(c(-4))%>%rename(cluster=cluster.y)
  #filtrar os animes que já viu mesmo nao tendo recomendado
  x1=filter(rating1,user_id==user.id)%>%mutate(cluster=0)%>%arrange(anime_id)
  
  #calcular a avaliação média de cada cluster da lista de animes "avaliados" do id selecionado
  media.cluster=aggregate(x$rating, by=list(cluster=x$cluster), mean)
  
  #se for maior que 7 (devo mudar isso?) entao o usuario gostou dos animes desse cluster 
  if(max(media.cluster$x)<7){
    media.cluster<-as.vector(0)
    #pegue o numero do clustr que tem a avaliação média mais alta
  } else{
    media.cluster<-as.vector(t(max(subset(media.cluster, x>=7, select=cluster))))
  }
  
  #subset anime dataframe com apenas as colunas anime_id e cluster
  df=anime[,c(1,8)]
  #se a média de avaliação de todos os cluster do usuario for menor que 7
  #então ele nao gostou de nenhum cluster assim recomende animes aleatorios
  if(media.cluster==0){
    recommend<-anime[sample.int(n = dim(anime)[1], size = 100), 1]
  }else{#caso contrario ele seleciona todos os filmes do cluster com maior média 
    recommend<-as.vector(t(subset(df, cluster==media.cluster, select=anime_id)))
  }
  
  recommend<-recommend[-x1$anime_id]#seleciona apenas os animes "não vistos" pelo usuario
  anime.name=filter(anime,anime_id %in% recommend)%>%select(c(2,6))#pegando os nomes dos ids dos animes
  recommend<-data.frame(anime.name)#criando um df com os nomes dos animes os id
  recomend<-recommend%>%arrange(desc(rating))#recomendar aqueles com maior rating
  colnames(recommend)=c("Nome do Anime","Avaliação Geral")
  suggestions = recommend[1:n,]
  return(suggestions)
}
myfunction(1,n=10)



