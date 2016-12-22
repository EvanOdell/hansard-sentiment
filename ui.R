
library(shiny)
library(shinydashboard)

header<-dashboardHeader(title='Disability Debate in Parliament')##Fix header length



sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Discussion", tabName = "discussion", icon = icon("Discussion")),
    menuItem("Chart of Terms and Phrases", tabName = "chart", icon = icon("Chart of Terms and Phrases")),
    menuItem("Sentiment Analysis", tabName = "senti", icon = icon("Sentiment Analysis"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "discussion",
            
            fluidRow(
              column(width=9,
                     box(width=NULL,
                         includeMarkdown("./assets/summary.Rmd"))
              )
            )
    ),
    tabItem(tabName = "chart",
            
            fluidRow(
              column(width=9,
                     box(width=NULL,
                         h3("Disability Discussion Frequency Chart"),
                         plotOutput('hansardplot')
                     )
              ),
              column(width=3,
                     box(width=NULL,
                         
                         checkboxGroupInput('category_input',
                                            'Words and Phrases',
                                            c('Disabled Person', 
                                              'Disabled Men',
                                              'Disabled Women',          
                                              'Disabled Children',
                                              'Disability Other',
                                              'People With Disability',
                                              'Children With Disability',
                                              'Any With Disability',
                                              'Independent Living',
                                              'Wheelchair',
                                              'Paralympic',
                                              'Spastic',
                                              'Sub-Normal',
                                              'Amputee',
                                              'Retard'),
                                            selected ='Disabled Person'),
                         
                         sliderInput('year', 'Year', 1936, 2016, value = c(1936, 2016), sep='')
                     )
              )
            )
    ),
    tabItem(tabName = "senti",
            
            fluidRow(
              column(width=9,
                     actionButton("toggle", "Switch Chart Type"),
                     box(width = NULL,
                         conditionalPanel(
                           condition = "input.toggle % 3 == 0",
                           h3("Sentiment Line Chart"),
                           plotOutput('sentiplot')
                           ),
                         conditionalPanel(
                           condition = "input.toggle % 3 == 2",
                           h3("Sentiment Box Chart"),
                           plotOutput('sentibox')
                           ),
                         conditionalPanel(
                           condition = "input.toggle % 3 == 1",
                           h3("Sentiment Bar Chart"),
                           plotOutput('sentibar')
                         )
                         
                     )
              ),
              
              column(width=3,
                     box(width = NULL,
                         sliderInput('senti_year', 'Year', 1936, 2016, value = c(1936, 2016), sep='')
                     ),
                     
                     radioButtons(
                       inputId="options",
                       label="Display Options:",
                       choices=list(
                         "All",
                         "Select Debate Type"
                       ),
                       selected="All"),
                     
                     conditionalPanel(
                       condition = "input.options != 'All'",
                       checkboxGroupInput(
                         'debate_type', 
                         'Debate Type to Show',
                         choices=c("All Debate", "Disability"),
                         selected = "Disability"
                       )
                     )
                     
              )
            )
    )
    )
    )

dashboardPage(
  dashboardHeader(title = "Disability in the Hansard"),
  sidebar,
  body,
  skin = "purple"
)

