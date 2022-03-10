
library(readr)
library(recommenderlab)
library(dplyr)
library(stringr)
library(splitstackshape)
library(data.table)

rating <-read.csv("rating.csv")
rating=rating%>%filter(rating!=-1)
rating$user_id <- as.factor(rating$user_id)
ratingMatrix <- as(rating, "realRatingMatrix")
ratingMatrix = ratingMatrix[rowCounts(ratingMatrix)>=500 , colCounts(ratingMatrix)>=800]

v=read.csv("urlanime1.csv")
v=v[,-1]
eval_sets <- evaluationScheme(data = ratingMatrix, method = "split",
                              train = 0.8, given = 4, goodRating =5
) 

IBCF_model <- Recommender(data =getData(eval_sets, "train"), method = "IBCF",param=list(method="cosine",k=30))
UBCF_model <- Recommender(data = getData(eval_sets, "train"), method = "UBCF",param=list(method="pearson",nn=25))
POP_model <- Recommender(data = getData(eval_sets, "train"), method = "POPULAR")

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

content=function(y){
    x=y%>%filter(rating!=0)
    medidas=x%>%group_by(cluster)%>%summarise(media=mean(rating),n=n(),sd=sd(rating))
    medidas=medidas%>%arrange(desc(media),desc(n),sd)
    cluster.user=medidas[1,1]
    cluster.user=as.vector(unlist(cluster.user))
    df=teste[,c(1,4,13)]
    recommend<-as.vector(t(subset(df, cluster==cluster.user, select=uid)))
    df=df%>%filter(cluster==cluster.user)
    genre.user=data.frame(anime_id=df$uid,genre=df$genre)
    genre.user=cSplit_e(as.data.table(genre.user), "genre", ",", type = "character", fill = 0)
    colnames(genre.user)=str_replace(colnames(genre.user),"genre_","")
    anime.matrix=genre.user[,3:ncol(genre.user)]
    user.rating=y%>%filter(cluster==cluster.user)%>%select(2)
    user.rating=unlist(user.rating)
    recommend=anti_join(teste,x,by=c("uid"="anime_id"))%>%filter(cluster==cluster.user)%>%select(c(1,2,4))
    recommend.genre=cSplit_e(as.data.table(recommend), "genre", ",", type = "character", fill = 0)
    colnames(recommend.genre)=str_replace(colnames(recommend.genre),"genre_","")
    recommend.genre=recommend.genre%>%select(-c(1:3))
    names.use <- names(anime.matrix)[(names(anime.matrix) %in% names(recommend.genre))]
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
########################################
teste=v
teste=teste%>%arrange(title)
###################################3
library(shiny)
library(shinydashboard)
library(shinyjs)

ui=
  dashboardPage(
    dashboardHeader(title = "Animes Recommender"),
    
    dashboardSidebar(sidebarMenu(width =4,
                                 menuItem("Lista de animes",tabName = "animes"),
                                 menuItem("Recomendações",tabName = "recomendation")
                                 
    )),
    
    dashboardBody(includeCSS("animes.css"),
                  tabItems(          
                    tabItem(tabName = "animes",
                            fluidRow(
                              tags$head(tags$script(HTML(
                                get_replacement_location
                              ))),
                              HTML('<input type="text" id="myInput" onkeyup="myFunction()" placeholder="Search Anime..">
         '),
                              tags$ul(id="myUL",style="background-color:#272729; ",lapply(1:length(teste$uid), function(i) {
                                tags$li(style=" display: inline-block;",
                                        tags$a(href="#",paste0(" ", teste[i,2]),
                                               tags$div(img(src = teste[i,11],width = 150, height = 160, align = "center")),
                                               
                                               numericInput(paste0("select_",as.numeric(teste[i,1])),
                                                            label =HTML("<br/> <br/>  Avaliação"),0, min = 0, max = 10)))
                                
                              }))
                              
                            )),
                    tabItem("recomendation",
                            #table
                            fluidRow(style="overflow-y: hidden;",
                              useShinyjs(),
                                  actionButton("btn", "Gerar Recomendações", class = "btn-warning")
                              ,br(),
                              box(title =h3("Colaborativa Baseado em Item"), tableOutput("table")),
                              box(title = h3("Colaborativa Baseado em Usuário"), tableOutput("table1")),
                              box(title = h3("Baseado em Popularidade"), tableOutput("table2")),
                              box(title = h3("Baseado em Conteúdo"), tableOutput("table3"))
                            )
                            
                            
                    ))
    )
  )


server <- function(input, output, session) {
    
    df1 <- eventReactive(input$btn, { 
        value_list <- reactiveValuesToList(input)
        is=which(str_detect(names(value_list),"select_")==T)
        value_list=value_list[is]
        names=str_replace(names(value_list),"select_",'')
        value_list=unlist(value_list)
        value_list=value_list[order(match(value_list,teste[,1]))]
        df=data.frame(user_id=factor(1),
                      anime_id=names,
                      rating=ifelse(value_list==0,NA,value_list))
        
        df=as(as(as(df,"realRatingMatrix"),"matrix"),"realRatingMatrix")
    })
    
    output$ui1 <- renderUI({
        lapply(1:length(teste$uid), function(i) {
            box(width = 3,
                height = 400, 
                title =h1(paste0(" ", teste[i,2])),
                tags$div(img(src = teste[i,11],width = 150, height = 160, align = "center")),
                
                numericInput(paste0("select_",as.numeric(teste[i,1])),
                             label =HTML("<br/> <br/>  Avaliação"),0, min = 0, max = 10))
            
        })
        
    })
    
    df2= eventReactive(input$btn, { 
        value_list <- reactiveValuesToList(input)
        is=which(str_detect(names(value_list),"select_")==T)
        value_list=value_list[is]
        names=str_replace(names(value_list),"select_",'')
        value_list=unlist(value_list)
        value_list=value_list[order(match(value_list,teste[,1]))]
        
        
        dt=data.frame(anime_id=as.numeric(names),
                      rating=value_list,cluster=teste$cluster)
        
        
    })
    
    output$table3=renderTable({
        class(df2()$anime_id)
        df2()
        content(df2())
    })
    
    output$table=renderTable({
        x=ibcf(df1())
        
    })
    
    output$table1=renderTable({
        
        x=ubcf(df1())
    })
    output$table2=renderTable({
        
        x=pop(df1())
    })
    
}

shinyApp(ui,server)
