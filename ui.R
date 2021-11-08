
if(require("pacman")=="FALSE"){
    install.packages("pacman")
}
pacman::p_load(shiny,shinydashboard,dplyr,shinycssloaders,shinythemes,shinyWidgets,bsplus,htmltools,
               giphyr,readxl,plotly,shinyalert,shinyjs,timevis,vistime,leaflet,lubridate,janitor,
               tidyverse,ggplot2,DT,shinyBS,viridis,cluster,fpc,rpart)

box_height = "31em"
plot_height = "26em"

title1 <- tags$a(tags$img(src="logo1.png", height='35', width='35'), 'DataZymes', 
                 style = "position:absolute; top:7px; z-index:1000000; right:15px; display: block; margin-left: auto; margin-right: auto; color: white;", 
                 href = "https://datazymes.com")

myUI <-  tagList(
    
    useShinyjs(),                 # Use Shinyjs
    useShinydashboard(),          # Use Shinydashboard
    #useShinydashboardPlus(),      # Use ShinydashboardPlus
    includeCSS('www/style.css'),
    tags$head(
        #tags$script(type="text/javascript", src = "code.js"),
        tags$style(
            HTML(".shiny-notification {
    font-weight: 800;
    }"
            )
        )
    ),
    navbarPage(
        windowTitle = "DataZymes",
        id = "appPage",
        title = title1,
            # div("DataZymes",
            #         tags$script(HTML("$('.navbar .container-fluid').append( '<a href=\"https://datazymes.com\"> <img src=\"logo1.png\" align=\"right\" height = \"50px\"> </a>');"
            #         ))),
        theme = shinytheme("cerulean"),
        inverse = TRUE,
        collapsible = TRUE,
        position = "static-top",
        fluid = TRUE,
        tabPanel(title = "Superstore",
                 tabsetPanel(
                     id="inTabset",
                     tabPanel("Overview",
                     fluidRow(
                             width = 12,
                             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                              tags$div("Loading...", id="loadmessage")),
                             bsAlert("alert"),
                             column(
                                 width = 3,
                                 fileInput("file1", label = "File input",placeholder = "Choose  a xls file")
                             )#,
                             ### load data button
                             # column(
                             #     width = 2,style = "padding: 5px 5px 5px 5px;margin: 18px 18px 18px 18px;",
                             # actionButton(inputId = "loadData",
                             #              "Load Data")),
                             # 
                             # bsTooltip("loadData", "Select Maximum of three months"), 
                             # 
                     ),# fluid row end
                    
                     fluidRow(style="padding:0px;display:block;",
                              column(width=12,style="padding:0px;margin-bottom: 5px;",
                                     uiOutput("Data_head"))),
                     
                     fluidRow(style="padding:0px;",
                                     column(width=12,style="padding:0px;margin-bottom: 5px;",
                                               DT::dataTableOutput("contents"),style = "height:480px; overflow-y: scroll;overflow-x: scroll;"
                                            ),
                              actionButton("switch_tab", "Continue Analysis"),style = "margin-top: 25px;"
                               ) # fluid row end
                               
                     ), # closing tabset panel 1
                     tabPanel("Visualizations",
                              fluidRow(
                                  width = 12,
                                  column(
                                      width = 3,
                                      selectInput(
                                          inputId = "column1",
                                          label = "Analyze By:",
                                          choices = c("Customer Name" = "customer_name",
                                                      "Segment" = "segment",
                                                      "City" = "city",
                                                      "State" = "state",
                                                      "Category" = "category",
                                                      "Sub-Category" = "sub_category"
                                                      ),
                                          selected = "Customer Name",
                                          multiple = FALSE)
                                  ),
                                  
                                  column(
                                      width = 3,
                                      selectInput(
                                          inputId =  "dateSelect1", 
                                          label = "Select time period:", 
                                          choices = 2016:2019,
                                          multiple = 5
                                      )),
                                  
                                  column(
                                  width = 3,
                                  radioGroupButtons( 
                                      inputId = "breakd",
                                      label = "Breakdown",
                                      choices = c("Overall", "Yearly"),
                                      status = "primary",
                                      selected = "Overall",
                                      checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                      justified = TRUE
                                  )
                                  #     width = 2,style = "padding: 15px 14px 8px 16px;margin: 7px 7px 5px 12px;",
                                  #     downloadButton("dwddata",
                                  #                    "Download Plot Data", 
                                  #                    style = "width:95%;")
                                   ),
                                  
                                  column(
                                      width = 2,#style = "padding: 5px 5px 5px 5px;margin: 3px 3px 3px 3px;",
                                      uiOutput("top_rangeUI")
                                  )
                              ),
                              
                              fluidRow(style="padding:0px;",
                                       column(width=12,style="padding:0px;margin-bottom: 5px;",
                                                  plotlyOutput("Geo_Volume_Plot"))),
                              fluidRow(style="padding:0px;",
                                       column(width=2,style="padding:0px;margin-bottom: 5px;",
                                                  downloadButton("dwdReceived","Download Plot Data", 
                                                                  style = "width:95%;"))
                                       ),
                              
                              fluidRow(style="padding:0px;",
                                       column(width=12,style="padding:0px;margin-bottom: 5px;",
                                                  style = "padding:3px",
                                                  plotlyOutput("Volume")
                                              )
                                       
                              ),
                              
                              fluidRow(style="padding:0px;",
                                       column(width=2,style="padding:0px;margin-bottom: 5px;",
                                             
                                                  downloadButton("dwdvolume","Download Plot Data", 
                                                                 style = "width:95%;")),
                                       column(width=2,style="padding:0px;margin-bottom: 5px;",
                                                         
                                                  uiOutput("data_button"))),
                              fluidRow(style="padding:0px;",
                                       column(width=2,style="padding:0px;margin-bottom: 5px;",
                                                actionButton("switch_tab1", "Continue Analysis"),style = "margin-top: 25px;"))
                     ),
                     
                     tabPanel("K-means",
                              sidebarLayout(
                                  ## SideBar Start
                                  sidebarPanel(
                                      style = "position:fixed;width:16%;",
                                      width = 2,
                                      
                                      tags$head(tags$style(type="text/css", "

                   #loadmessage {
                   padding: 4px 0px 3px 0px;
                   text-align: center;
                   font-weight: bold;
                   font-size: 100%;
                   color: #000000;
                   border-radius: 4px;
                   background-color: #0fd1db;

                   }

                                        ")),
                                      
                                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                       
                                                       tags$div("Loading...",id="loadmessage")),
                                      
                                      ### SideBar Date Input ----
                                      bsAlert("alert"),
                                      fluidRow(
                                      # pickerInput(
                                      #     inputId = "choose_columns",
                                      #     label = "Cluster By :",
                                      #     choices = c("Sales" = "sales",
                                      #                 "Quantity" = "quantity",
                                      #                 "Discount" = "discount",
                                      #                 "Profit" = "profit"),
                                      #     multiple = TRUE,
                                      #     selected = "sales",
                                      #     inline = FALSE,
                                      #     options = pickerOptions(actionsBox = TRUE, selectedTextFormat = 'values',
                                      #                             tickIcon = "glyphicon-ok",showIcon = TRUE,
                                      #                             showContent = TRUE, liveSearch = TRUE, showTick = TRUE,size = "auto",
                                      #                             deselectAllText = "Deselect All"),
                                      #     choicesOpt = list(
                                      #         style = rep(("color: black; background: lightgrey; font-weight: bold; font-size: 10px;"),4)
                                      #     )
                                          uiOutput("choose_columns")
                                          
                                      ),
                                      fluidRow( uiOutput("choose_cluster")),
                                      fluidRow(
                                          # selectInput("target", "Choose Target Attribute", 
                                          #                  choices  = c("Sales" = "sales",
                                          #                               "Quantity" = "quantity",
                                          #                               "Discount" = "discount",
                                          #                               "Profit" = "profit"))
                                          
                                          uiOutput("choose_target")
                                               )),
                                
                                      
                                  
                             # ),
                              mainPanel(
                                  width = 10,
                                  fluidRow(plotOutput('kmeans_plot')),
                                  fluidRow(uiOutput("Data_head1")),
                                  fluidRow(
                                      tableOutput('agg_table')),
                                  fluidRow(uiOutput("Data_head2")),
                                  fluidRow(
                                      tableOutput("prediction"))
                              )
                              )
                     
        ) # tabPanel
                 ) #tabsetpanel
        ) # tabPanel main
    ) # navBarpage
) # tagList
