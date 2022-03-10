
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
                              HTML('<input type="text" id="myInput" onkeyup="myFunction()" placeholder="Search for names..">
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
shinyApp(ui,server)
