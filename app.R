(\(x){
  sapply(x, function(x) if(!x %in% installed.packages()){
    install.packages(x, dependencies = T)
  })
})(c("tidyverse", "openxlsx", "plyr", "shiny", "shinyjqui"))
library("tidyverse")
library("openxlsx")
library("shiny")
library("shinyjqui")


ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "@import url('https://fonts.googleapis.com/css2?family=Anton&family=Bebas+Neue&display=swap');
         @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@100&display=swap');
       h1{
       font-family: 'Anton', sans-serif;
       display: inline;
       color: #29282D;
       font-size:100px;
       }
       h2{
       font-family: 'Bebas Neue', cursive;
       color:#E41E75;
       display: inline;
       font-size:40px;
       }
       .footer {
             font-family: font-family: 'Roboto', sans-serif;
             width: 100%;
             background-color: #FFFFFF;
             color: #DBD5D1;
             }
        .button{background-color:#F24949;
        border-color:#F24949;
        color:white}
        .button:hover{background-color:#F00000;
        border-color: #F00000;
                color:white
                }
         .btn-file{
         background-color:#F24949;
        border-color:#F24949;
        color:white}
        .btn-file:hover{
        background-color:#F00000;
        border-color: #F00000;
                color:white
                }
        .progress-bar{
        background-color:#F24949;
        border-color: #F4C3C4
        }"))),
  
  # Application title
  title="Mascot",
  #use_shiny_title(),
  titlePanel(span(h1("Mascot"), img(src="MS.png",
                                    height=100,
                                    style="vertical-align: -20%"))),
  
  
  # Application title
  
  
  sidebarLayout(
    sidebarPanel(
      br(), 
      fileInput(
        "LUT",
        "Upload the lookup table",
        multiple    = FALSE,
        accept      = ".xlsx",
        width       = NULL,
        buttonLabel = "Browse...",
        placeholder = "No file selected",
        capture     = NULL
      ),
      shiny::textInput("folder_path",
                       label= "Files path:",
                       value = ""),

      actionButton("path", 
                   label = "Submit path", 
                   class = "button"),
      br(),
      br(),

      selectInput("charge", "Peptide charge:",
                  choices = c("Yes", "No")),
      br(), 

      orderInput('groups',
                 'Selected groups:',
                 items = NULL,
                 placeholder = 'Drag groups here...',
                 connect = 'all_groups',
                 width = "100%"),
      orderInput('all_groups',
                 'All groups available:',
                 placeholder = 'No groups available',
                 items = NULL,
                 as_source = FALSE,
                 item_class = "default",
                 width = "100%",
                 connect = 'groups'),
      br(),
      actionButton("groups_submit",
                   "Submit groups",
                   class = "button")
      
    ),
    
    mainPanel(
      verbatimTextOutput("groups_")
    )
  ),
  hr(),
  tags$footer("2023, Luiz Felipe Martucci", class = "footer")
)


server <- function(input, output, session) {
  
  
  LUT <- eventReactive(input$path, {
    if(!is.null(input$LUT)){
    input$LUT$datapath
    }
    })
  

  
  path <- reactive(input$folder_path)
  
  data <- reactive({
    withProgress(message = 'Wrangling data', value = 0, {
    
    if(path() != "" & !is.null(LUT())){
      path <- path()
      setwd(path)
      

      setProgress(46, detail = "Preparing data")

      data <- Data_processing_short(LUT())

      updateOrderInput(session,
                               "all_groups",
                               label =" All groups available:",
                               items = unique(data$Comparison))
      return(data)
    }
    })
  })



  groups_sel <- eventReactive(input$groups_submit, {
    if(!is.null(input$groups)){
      return(input$groups)
    }
  })


  output$groups_ <- reactive({
    
    
    if(!is.null(data()) & !is.null(groups_sel())){

      data <- data()
      groups <- groups_sel()
      
      withProgress(message = 'Preparing tables', value = 0, {
        
      if(input$charge == "Yes"){
        Summary_info <- Summary_data_mascot(data, charge= TRUE)
      }else{
        Summary_info <- Summary_data_mascot(data, charge= FALSE)
      }

      quant_path <- file.path(getwd(), "Peptides_quant", fsep = .Platform$file.sep)
      name_tables <- file.path(quant_path, "Results.xlsx", fsep = .Platform$file.sep)
      
      setProgress(46, detail = "Writing data")
     

      workbook_create(Summary_info, name_tables, quant_path, groups)

      return("Work done!")
      })
    }
    })
  
  function(input, output, session) {
    
    if (!interactive()) {
      session$onSessionEnded(function() {
        stopApp()
        q("no")
      })
    } }


  
}

shinyApp(ui = ui, server = server)
