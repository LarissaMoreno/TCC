#setwd("~/Larissa/TCC")
imgurl=read.csv("animes.csv")
imgurl=distinct(imgurl)

z=imgurl[match(anime$anime_id,imgurl$uid),]

na=which(is.na(z$uid))
na

z[na,c(1,2,4)]=anime[na,1:3]
write.csv(z,file="urlanime.csv")

######################################
#completar no excel os que faltam
##########################################
#importando os novos dados
z=read.csv("urlanime.csv")


############################################3
#filtrar apenas os itens que estão em ratingMatrix para colocar no shiny
rating <-read.csv("rating.csv")
rating=rating%>%filter(rating!=-1)
rating$user_id <- as.factor(rating$user_id)
ratingMatrix <- as(rating, "realRatingMatrix")
ratingMatrix = ratingMatrix[rowCounts(ratingMatrix)>=500 , colCounts(ratingMatrix)>=800]
views_per_movie <- colCounts(ratingMatrix)
views <- as.integer(names(views_per_movie))

#v=z%>%filter(uid %in% views)
#v=v[,-c(1:3)]
#depois fazer um banco de dados dos anime que estao em ratimtrix com as url das imagens
#write.csv(v,file="urlanime1.csv")


#500 e 1000=1462
#500 e 900=1566
#500 e 800=1700
#500 e 700=1848
#500 e 150=3597

############################################################################
v=read.csv("urlanime1.csv")
v=v[,-1]
eval_sets <- evaluationScheme(data = ratingMatrix, method = "split",
                              train = 0.8, given = 4, goodRating =5
) 

IBCF_model <- Recommender(data =getData(eval_sets, "train"), method = "IBCF",param=list(method="cosine",k=30))
UBCF_model <- Recommender(data = getData(eval_sets, "train"), method = "UBCF",param=list(method="pearson",nn=25))
POP_model <- Recommender(data = getData(eval_sets, "train"), method = "POPULAR")



###########################################################
#salvar os modelo criados em rds
set.seed(8)#
m=data.frame(user_id=factor(1),
             anime_id=views,
             rating=sample(c(0:10),1700,T)
)

m$rating=ifelse(m$rating==0,NA,m$rating)

m=as(as(as(m,"realRatingMatrix"),"matrix"),"realRatingMatrix")

getRatingMatrix(m)

head(as(m, "data.frame"))
recom=function(x){
  x=arrange(x,desc(x$rating))
  x$item=as.integer(x$item)
  x=inner_join(x,v,by=c("item"="uid"))%>%select(c(2,3,4))%>%head(10)
}

ibcf=function(x){
  IBCF_prediction <- predict(object = IBCF_model, x, n = 10,type = "ratings")
  x=as(IBCF_prediction, "data.frame")
  x=recom(x)
  head(x,10)
}
ubcf=function(x){
  UBCF_prediction <- predict(object = UBCF_model, x, n = 10,type = "ratings")
  x=as(UBCF_prediction, "data.frame")
  x=recom(x)
  head(x,10)
}
pop=function(x){
  POP_prediction <- predict(object = POP_model, x, n = 10,type = "ratings")
  x=as(POP_prediction, "data.frame")
  x=recom(x)
  head(x,10)
}
x=ibcf(m)
x1=pop(m)
x1


###################################
#v=read.csv("urlanime1.csv")
#v=v[,-1]
#library(stringr)
#v$genre= gsub("[", "", v$genre, fixed = TRUE)
#v$genre= gsub("]", "", v$genre, fixed = TRUE)
#genre=data.frame(genre=v$genre)
#library(splitstackshape)
#library(data.table)
#genre=cSplit_e(as.data.table(genre), "genre", ",", type = "character", fill = 0)
#removendo o genero total
#genre=genre[,-1]
#renomeando as colunas
#colnames(genre)=str_replace(colnames(genre),"genre_","")
#library(purrr)
#set.seed(30)
#tot_withinss <- map_dbl(1:10,  function(k){
 # model <- kmeans(x = genre, centers = k)
#  model$tot.withinss
#})

#elbow_df <- data.frame(
#  k = 1:10,
#  tot_withinss = tot_withinss
#)

#plot(1:10, elbow_df$tot_withinss,
#     type="b", pch = 19, frame = FALSE, 
#     xlab="Número de clusters K",
#     ylab="Total within-clusters sum of squares",
#     main="Elbow Plot")
#abline(v=3,lty=2, lwd=)

#animecluster= model <- kmeans(x = genre, centers = 3)
#v$cluster=animecluster$cluster

#dat=data.frame( anime_id=v$uid,
#                rating=sample(c(0:10),1700,T),
#                cluster=v$cluster)


#rm(elbow_df)
#rm(animecluster)
#rm(model)
#write.csv(v,file="urlanime1.csv")


v=read.csv("urlanime1.csv")
content=function(y){
  x=y%>%filter(rating!=0)
  medidas=x%>%group_by(cluster)%>%summarise(media=mean(rating),n=n(),sd=sd(rating))
  medidas=medidas%>%arrange(desc(media),desc(n),sd)
  cluster.user=medidas[1,1]
  cluster.user=as.vector(unlist(cluster.user))
  df=v[,c(1,4,13)]
  recommend<-as.vector(t(subset(df, cluster==cluster.user, select=uid)))
  df=df%>%filter(cluster==cluster.user)
  genre.user=data.frame(anime_id=df$uid,genre=df$genre)
  genre.user=cSplit_e(as.data.table(genre.user), "genre", ",", type = "character", fill = 0)
  colnames(genre.user)=str_replace(colnames(genre.user),"genre_","")
  anime.matrix=genre.user[,3:ncol(genre.user)]
  user.rating=y%>%filter(cluster==cluster.user)%>%select(2)
  user.rating=unlist(user.rating)
  recommend=anti_join(v,x,by=c("uid"="anime_id"))%>%filter(cluster==cluster.user)%>%select(c(1,2,4))
  recommend.genre=cSplit_e(as.data.table(recommend), "genre", ",", type = "character", fill = 0)
  colnames(recommend.genre)=str_replace(colnames(recommend.genre),"genre_","")
  recommend.genre=recommend.genre%>%select(-c(1:3))
  names.use <- names(anime.matrix)[(names(anime.matrix) %in% names(recommend.genre))]
  names.use
  anime.matrix=genre.user%>%select(names.use)
  
  weight=anime.matrix*user.rating
  weight=colSums(weight)
  weight=weight/sum(weight)
  
  recommend.genre=t(t(recommend.genre) *weight)
  
  recommend=data.frame(anime_id=recommend$uid,
                       name=recommend$title,
                       weight=rowSums(recommend.genre))
  
  recommend=recommend%>%arrange(desc(weight))
  colnames(recommend)=c("ID do Anime","Nome do Anime","Peso")
  suggestions = recommend[1:10,2:3]
  suggestions 
}
#content(dat)