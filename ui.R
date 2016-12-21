
library(shiny)
library(shinydashboard)

header<-dashboardHeader(title='Disability Debate in Parliament')##Fix header length


## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Chart of Terms and Phrases", tabName = "chart", icon = icon("Chart of Terms and Phrases"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "chart",
            
            fluidRow(
              column(width=9,
                     box(width=NULL,
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
                                              'Retard')),
                         
                         sliderInput('year', 'Year', 1936, 2016, value = c(1936, 2016), sep='')
                     )
              )
            )
    )
    )
    )
  


# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Disability in the Hansard"),
  sidebar,
  body,
  skin = "purple"
)


