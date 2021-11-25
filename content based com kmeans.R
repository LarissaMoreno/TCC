#setwd("~/Larissa/TCC")
#save.image("content-based.RData")
#load("C:/Users/gabri/Documents/Larissa/TCC/content-based.RData")
anime=read.csv("animes2.csv")
library(stringr)
anime$name = str_replace_all(anime$name,"&#039;","'")  
anime$name = str_replace_all(anime$name,"&quot;","")
anime$name = str_replace_all(anime$name,".hack//","") 
anime$name = str_replace_all(anime$name,"_","")
anime$name = str_replace_all(anime$name,"&amp;","&")
genre=data.frame(genre=anime$genre)
library(splitstackshape)
library(data.table)
#dummificando o banco de dados
genre=cSplit_e(as.data.table(genre), "genre", ",", type = "character", fill = 0)
#removendo o genero total
genre=genre[,-1]
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
library(dplyr)
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
myfunction=function(user.id,n){ #subset rating dataframe pelo id selecionado
  x=filter(rating,user_id==user.id)%>%mutate(cluster=0)%>%arrange(anime_id)
  #encontrar o cluster dos animes vistos pelo id selecionado
  x=left_join(x,anime[,c(1,3,8)],by="anime_id")%>%select(c(-4))%>%rename(cluster=cluster.y)
  
  #calcular a avaliação medidas de cada cluster da lista de animes "avaliados" do id selecionado
  medidas=x%>%group_by(cluster)%>%summarise(media=mean(rating),n=n(),sd=sd(rating))
  medidas=medidas%>%arrange(desc(media),desc(n),sd)
  cluster.user=medidas[1,1]
  cluster.user=as.vector(unlist(cluster.user))
  #subset anime dataframe com apenas as colunas anime_id e cluster
  df=anime[,c(1,8)]
  #caso contrario ele seleciona todos os filmes do cluster com maior média 
  recommend<-as.vector(t(subset(df, cluster==cluster.user, select=anime_id)))
  #subset df usuario alvo com os animes do cluster escolhido
  df=x%>%filter(cluster==cluster.user)
  #selecionando o genero e anime_id
  genre.user=data.frame(anime_id=df$anime_id,genre=df$genre)
  #tranformando em binario os generos
  genre.user=cSplit_e(as.data.table(genre.user), "genre", ",", type = "character", fill = 0)
  #arrumando nomes das colunas
  colnames(genre.user)=str_replace(colnames(genre.user),"genre_","")
  #matrix somente com os generos binarios
  anime.matrix=genre.user[,3:ncol(genre.user)]
  #vetor com as notas previamnte dadas
  user.rating=df$rating
  #calculando os pesos para decidir o profile do usuario
  weight=anime.matrix*user.rating
  #pesos de cada genero
  weight=colSums(weight)
  #dividindo pela soma
  weight=weight/sum(weight)
  
  #subset dos anime que o usuario ainda não viu
  recommend=anti_join(anime,x,by="anime_id")%>%filter(cluster==cluster.user)%>%select(c(1,2,3))
  #transformando em binario os generos
  recommend.genre=cSplit_e(as.data.table(recommend), "genre", ",", type = "character", fill = 0)
  colnames(recommend.genre)=str_replace(colnames(recommend.genre),"genre_","")
  recommend.genre=recommend.genre%>%select(names(weight))
  #multiplicando pelos pesos
  recommend.genre=t(t(recommend.genre) *weight)
  
  #df com os nomes dos animes candidatos, junto com o peso de cada anime
  recommend=data.frame(anime_id=recommend$anime_id,
                       name=recommend$name,
                       weight=rowSums(recommend.genre))
  #ordenando para recomendar os animes com maior peso
  recommend=recommend%>%arrange(desc(weight))
  colnames(recommend)=c("ID do Anime","Nome do Anime","Peso")
  suggestions = recommend[1:n,2:3]
  suggestions  
  
}
user.id=1
n=10
