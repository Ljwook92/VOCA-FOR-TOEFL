pacman::p_load("shiny", "dplyr", "data.table")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("토플 3월 스터디"),
    
    navbarPage(
        theme = shinytheme("yeti"), "LC 단어장",
        
    tabPanel("시험지 생성기", icon=icon("chalkboard-teacher"),

        sidebarPanel(
            numericInput("D1", "Start day :", min = 1, max = 30, value = 1, step = 1),
            numericInput("D2", "End day :", min = 1, max = 30, value = 1, step = 1),
            sliderInput("Q", "문제 수", 10, 50, 30),
            tags$hr(),
            actionButton(inputId = "aB",
                         label = "시험지 생성",
                         color = "primary",
                         style = "bordered",
                         # icon = icon("sliders"),
                         block = TRUE),
            ),

        mainPanel(
            tabsetPanel(
                tabPanel("시험지",
                         tableOutput("Question")),
                tabPanel("답안지",
                         tableOutput("Question2"))
                )),
        )
    ))


server <- function(input, output) {
    
    Load.Orig <- reactive({
        data.path <- paste(getwd(), "Desktop/voca.csv", sep="/")
        
        voca <- read.csv("/Users/ljwook_92/Desktop/voca.csv")
        voca
    })
    
    select <- eventReactive(input$aB, {
        
        D_s <- paste("day", input$D1)
        D_e <- paste("day", input$D2)
        
        num <- Load.Orig() %>% filter(Day==D_s | Day==D_e)
        
        random <- sample(1:nrow(num), input$Q, replace=F)
        
        question <- data.frame(Load.Orig()[random,c(2:3)])
        colnames(question) <- c("단어", "뜻")
        
        seq1 <- seq(1,length(random),1)
        Num <- data.frame(seq1)
        colnames(Num) <- c("N")
        
        final <- cbind(Num,question)
        final
    })

    output$Question <- renderTable({
        select()[-3]
    })
    
    output$Question2 <- renderTable({
        select()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
