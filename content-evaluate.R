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

#Carregando ratinf dataframe
require(readr)
rating <-read.csv("rating.csv")
library(dplyr)
rating=rating%>%filter(rating!=-1)#remover o grupo de pessoas que assistiram mas nao avaliaram
rating$user_id <- as.factor(rating$user_id)


####################################################

evaluation1=function(user.id){
  x=filter(rating,user_id==user.id)%>%mutate(cluster=0)%>%arrange(anime_id)
  #encontrar o cluster dos animes vistos pelo id selecionado
  x=left_join(x,anime[,c(1,3,8)],by="anime_id")%>%select(c(-4))%>%rename(cluster=cluster.y)
  
  #calcular a avaliação medidas de cada cluster da lista de animes "avaliados" do id selecionado
  medidas=x%>%group_by(cluster)%>%summarise(media=mean(rating),n=n(),sd=sd(rating))
  #maior media depois maior numero de animes depois menor desvio padrao da nota
  medidas=medidas%>%arrange(desc(media),desc(n),sd)
  #primeiro cluster é o mais preferivel
  cluster.user=medidas[1,1]
  cluster.user=as.vector(unlist(cluster.user))
  #selecionar apenas os animes que estão no cluster
  x=x%>%filter(cluster==cluster.user)
  #selecionando o genero e anime_id
  genre.user=data.frame(anime_id=x$anime_id,genre=x$genre)
  #tranformando em binario os generos
  genre.user=cSplit_e(as.data.table(genre.user), "genre", ",", type = "character", fill = 0)
  #arrumando nomes das colunas
  colnames(genre.user)=str_replace(colnames(genre.user),"genre_","")
  #matrix somente com os generos binarios
  anime.matrix=genre.user[,3:ncol(genre.user)]
  #vetor com as notas previamnte dadas
  user.rating=x$rating
  #calculando os pesos para decidir o profile do usuario
  weight=anime.matrix*user.rating
  #pesos de cada genero
  weight=colSums(weight)
  #dividindo pela soma
  weight=weight/sum(weight)
  #fazendo o dot product e multiplicando por 10
  recommend.genre=t(t(anime.matrix) *weight)*10
  #somando os dot product
  df=data.frame(anime=x$anime_id,user_id=x$user_id,real.rating=x$rating,predict=rowSums(recommend.genre))
  #ordenando por nota predita
  df=df%>%arrange(desc(predict))

  return(df)
}

#coletando apenas os usuários que avaliaram 150 ou mais animes
teste=rating%>%group_by(user_id)%>%summarise(n=n())%>%filter(n>=150)%>%select(1)
teste=as.numeric(as.vector.factor(teste$user_id))
#aplicando a função a todos os usuarios selecionados
p=do.call(rbind, lapply(teste, evaluation1))
#######################################################################
#medidas de avaliação de desempenho
(rmse=sqrt(mean((p$predict-p$real.rating)^2)))
(mae=mean(abs(p$predict-p$real.rating)))
(mse=mean((p$predict-p$real.rating)^2))

########################################################################

#função para criar a matriz de confusão para o numero de recomendações n iguais a 10,20,...,100
confusion=function(n){
 df= p %>% arrange(desc(user_id)) %>%  group_by(user_id) %>% slice(1:n)%>% 
   mutate(real.rating=ifelse(real.rating>=5,1,0),predict=ifelse(predict>=5,1,0))
 tabela=table(real=df$real.rating,predict=df$predict)
 TP=tabela[1]
 FP=tabela[2]
 FN=tabela[3]
 TN=tabela[4]
 recall=TP/(TP+FN)
 precision=TP/(TP+FP)
 TPR=TP/(TP+FN)
 FPR=FP/(FP+TN)
 data.frame(TP,FP,FN,TN,precision,recall,TPR,FPR,n)
}

n=seq(10,100,10)
#aplicando a funsao da matriz de confusão
confusion.matrix=do.call(rbind, lapply(n, confusion))
confusion.matrix

#gráfico de sensibilidade e 1-especificidade
plot(confusion.matrix$FPR,confusion.matrix$TPR,type="b",xlim=c(0,1),ylim=c(0,1),
     main="Roc Curve", xlab="FPR",ylab="TPR",col="blue")
abline(a=0,b=1,lty=2,col="grey")
legend(x=0.65,y=0.2,legend = "Content-Based",lty = 1, cex=0.8, col = "blue")

#gráfico de precisão-sensibilidade
plot(confusion.matrix$recall,confusion.matrix$precision,type="b",xlim=c(0,1),ylim=c(0,1),
     main="Precisão-Sensibilidade", xlab="recall",ylab="precision",col="blue")
legend(x=0.65,y=0.9,legend = "Content-Based",lty = 1, cex=0.8, col = "blue")



 
 
 
