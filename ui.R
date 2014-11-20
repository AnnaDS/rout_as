library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
 
  headerPanel(
    list(tags$head(tags$style("#text1{color: black;  font-size: 20px; font-style: italic; }
                              #text2{color: black;  font-size: 20px; font-style: italic; }
                              #text3{color: black;  font-size: 20px; font-style: italic; }
                              #text4{color: red;  font-size: 40px; font-style: bold; }; }
                              #textD{color: red;  font-size: 40px; font-style: bold; }")),
         
         #HTML('<h1 style="color:red;  font-size: 40px; font-style: bold; align="center" ">  Results Assessment </h1>')
         #,
         HTML('<img src="oracle-toa-logo.png", height="400px", width="400px", style="float:top"/>' )
         ,
         HTML('<h1 style="color:red;  font-size: 40px; font-style: bold; align="center" ">                       </h1>'),
         HTML('<img src="rrr.png", height="200px", width="200px", style="float:bottom"/>' )
        
         ),
    
    img(src="rrr.png", height = 50, width = 50)
    ),
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view
#   fluidRow(
#     column(8,
#            hr(),
#            verbatimTextOutput('comp'),
#            selectInput('in1', 'Options', c(Choose='','comp.names'), selectize=FALSE)
#     ),
#     column(2,actionButton("cmpOK", "Ok"))
#     ),
#   fluidRow(
#       column(4,
#              hr(),
#              verbatimTextOutput('out1'),
#              selectInput('in1', 'Options', c(Choose='', state.name), selectize=FALSE)
#       ),
#       column(2,
#              hr(),
#              verbatimTextOutput('out2'),
#              selectInput('in2', 'Options', state.name, selectize=FALSE)
#       )),
  sidebarLayout(

    sidebarPanel(width = 4,
                 align="left",
                 br(),
                # h1("Select company"),
                uiOutput("choose_dataset"),
              #htmlOutput("choose_dataset"),
#                  selectInput("input_type", "Select company","comp"
#                  ),
                 #uiOutput("choose_dataset"),
                 actionButton("selCmp", "Get data"),
h2(textOutput("input_type_text")),
# br(),
# textOutput("textD"),
fluidRow(
  column(5,
                     hr(),
selectInput("env1", h5("Choose first  environment:"), 
                          choices = c("prod", "test", "train", "stage"),
                          selected = "prod")
),
column(5,
       hr(),
       selectInput("env2", h5("Choose second  environment:"), 
                   choices = c("prod", "test", "train", "stage"),
                   selected = "stage")
)
),
fluidRow(
  column(5,
         hr(),
         selectInput("vers1", h5("Choose first version:"), 
                     choices = c("2.0.7", "2.1.0", "2.1.1"),
                     selected = "2.0.7")
  ),
  column(5,
         hr(),
         selectInput("vers2", h5("Choose second  version:"), 
                     choices = c("2.0.7", "2.1.0", "2.1.1"),
                     selected = "2.0.7")
  )
),

#       selectInput("dataset1", h2("Choose an initial version:"), 
#                   choices = c("2.0.7", "2.1.1"),
#                   selected = "2.0.7"),
#       
#       selectInput("dataset2", h2("Choose a stage version:"), 
#                   choices = c("2.1.1", "2.1.0"),
#                   selected = "2.1.1"),

dateRangeInput("dates", label = h5("Date range for initial version"), start = "2014-07-01", end = "2014-10-14"),
dateRangeInput("dates2", label = h5("Date range for stage version"), start = "2014-08-20", end = "2014-10-14"),
      br(),
      actionButton("goButton", "Go!"),
      br(),
      selectInput("plot.type","Detail analysis of parameters:", 
                  list(Assigned_activities = "Assigned_activities", Overdue_time = "Overdue_time", Travel_time = "Travel_time", SLA_violation = "SLA_violation"
                       , Percent_of_pure_revenue = "Percent_of_pure_revenue", Work_time = "Work_time", Rout_density="Rout_density", Overtime="Overtime", Revenue="Revenue")
      ),
      br(),
      actionButton("goButton1", "Show"),
br(),
br(),
uiOutput("choose_dataset1"),
br(),
actionButton("expert", "Data for routing plan"),
br()
         ),

    
    # Show a summary of the dataset and an HTML table with the 
    # requested number of observations
    mainPanel(
     
      #img(src="oracle-toa-logo.png", height = 400, width = 400),
      textOutput("text4"),
      br(),
     # verbatimTextOutput("textD1"),
     # textOutput("textD1"),
     tabsetPanel(
       tabPanel('',dataTableOutput("textD1"))
     ),
      br(),
      textOutput("text1"),
      br(),
      textOutput("text2"),
      br(),
      textOutput("text3"),
      br(),
     # textOutput("dates"),
      
      tabsetPanel(
        tabPanel('Complex routing assessment',dataTableOutput("fintable"))
        #       tableOutput("view")
      ),
#tabPanel('Complex routing assessment',dataTableOutput("fintable")),
#h4('Complex routing assessment'),
#tableOutput("fintable"),
#       ,
#        
#       
#       
plotOutput("parPlot"),
#plotOutput("parPlot2"),
tabsetPanel(
  tabPanel('',dataTableOutput("mytable1"))
  
#       tableOutput("view")
    ),
h4("Summary of performance time"),
 tableOutput("summary"),

tabsetPanel(
  tabPanel('',dataTableOutput("mytable2"))
  
  #       tableOutput("view")
)

#verbatimTextOutput("summary1"),
#plotOutput("histHT"),
#plotOutput("histMT")

#,
#verbatimTextOutput("summary")#,
# plotOutput("figPlot"),
# plotOutput("distPlot")
  )
)))