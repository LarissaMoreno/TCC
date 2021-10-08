require(readr)
library(dplyr)
library(stringr)
library(recommenderlab)
#setwd("~/Larissa/TCC")
#save.image("evaluate-sistem.RData")
#load("C:/Users/gabri/Documents/Larissa/TCC/evaluate-sistem.RData")
anime=read.csv("animes2.csv")
rating <-read.csv("rating.csv")
anime$name = str_replace_all(anime$name,"&#039;","'")  
anime$name = str_replace_all(anime$name,"&quot;","")
anime$name = str_replace_all(anime$name,".hack//","") 
anime$name = str_replace_all(anime$name,"_","")
anime$name = str_replace_all(anime$name,"&amp;","&")

#data clean
rating=rating%>%filter(rating!=-1)#remover o grupo de pessoas que assistiram mas nao avaliaram
rating$user_id <- as.factor(rating$user_id)#transformando os user_id em factor

#criando a matriz usuario X item  esparsa do tipo realRatingMatrix
ratingMatrix <- as(rating, "realRatingMatrix")


#filtrando apenas os animes que foram visto pelo menos 150 vezes 
#e usuarios que avaliaram pelo menos 200 animes
ratingMatrix = ratingMatrix[rowCounts(ratingMatrix)>=200 , colCounts(ratingMatrix)>=150]

percentage_training <- 0.8#80% para base de treinammento
min(rowCounts(ratingMatrix))#número mínimo de itens adquiridos por qualquer usuário 


items_to_keep <- 20#normalmente deve ser menor que o numero minimo de itens por usuario
rating_threshold <- 7#notas maiores ou iguais a 7 significa que o usuario gostou
n_eval <- 1#quantas vezes queremos executar a avaliação


#cross-validation
eval_sets <- evaluationScheme(data = ratingMatrix, method = "split",
                              train = percentage_training, given = items_to_keep, goodRating =
                                rating_threshold, k = n_eval) 




#modelo baseado na filtragem colaborativa por item
IBCF_model <- Recommender(data = getData(eval_sets, "train"), method = "IBCF")

IBCF_prediction <- predict(object = IBCF_model, newdata =
                             getData(eval_sets, "known"), n = 10, type = "ratings")


#top 10 recomendações para usuario 129
x=data.frame(as(IBCF_prediction, 'list')[1])
x$anime_id=as.integer(rownames(x))
x=arrange(x,desc(x$X129))%>%head(10)
x=inner_join(x,anime,by="anime_id")%>%select(c(1,3))
colnames(x)=c(paste0("rating de ",colnames(x)[1]),"name")
x


#RMSE MSE MAE
IBCF_accuracy <- calcPredictionAccuracy(
  x = IBCF_prediction, data = getData(eval_sets, "unknown"), byUser =
    F)
head(IBCF_accuracy)



#modelo baseado na filtragem colaborativa por usuario
UBCF_model <- Recommender(data = getData(eval_sets, "train"), method = "UBCF")

UBCF_prediction <- predict(object = UBCF_model, newdata =
                             getData(eval_sets, "known"), n = 10, type = "ratings")

#top 10 recomendações para usuario 129
x1=data.frame(as(UBCF_prediction, 'list')[1])
x1$anime_id=as.integer(rownames(x1))
x1=arrange(x1,desc(x1$X129))%>%head(10)
x1=inner_join(x1,anime,by="anime_id")%>%select(c(1,3))
colnames(x1)=c(paste0("rating de ",colnames(x1)[1]),"name")
x1


#RMSE MSE MAE
UBCF_accuracy <- calcPredictionAccuracy(
  x = UBCF_prediction, data = getData(eval_sets, "unknown"), byUser =
    F)
head(UBCF_accuracy)




#Modelo baseado na popularidade
POP_model <- Recommender(data = getData(eval_sets, "train"), method = "POPULAR")

POP_prediction <- predict(object = POP_model, newdata =
                             getData(eval_sets, "known"), n = 10, type = "ratings")

#top 10 recomendações para usuario 129
x2=data.frame(as(POP_prediction, 'list')[1])
x2$anime_id=as.integer(rownames(x2))
x2=arrange(x2,desc(x2$X129))%>%head(10)
x2=inner_join(x2,anime,by="anime_id")%>%select(c(1,3))
colnames(x2)=c(paste0("rating de ",colnames(x2)[1]),"name")
x2


#RMSE MSE MAE
POP_accuracy <- calcPredictionAccuracy(
  x = POP_prediction, data = getData(eval_sets, "unknown"), byUser =
    F)
head(POP_accuracy)


##Comparando recomendações
x3=cbind(x,x1,x2)
x3=x3[,-c(1,3,5)]
colnames(x3)=c("IBCF","UBCF","POP")
x3


#Matriz de confução 
models_evaluate <- list(
  IBCF = list(name = "IBCF"),
  UBCF=list(name="UBCF"),
  POPULAR = list(name = "POPULAR"))
results <- evaluate(x = eval_sets, method =  models_evaluate, n =seq(10, 100, 10))


results[["IBCF"]]@results[[1]]@cm
results[["UBCF"]]@results[[1]]@cm
results[["POPULAR"]]@results[[1]]@cm



#Não sei pra que que serve ainda
plot(results, annotate = T, legend = "topleft") 
title("ROC Curve")

plot(results, "prec/rec", annotate = T, legend = "bottomright")
title("Pecision-Recall")
