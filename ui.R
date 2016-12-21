
library(shiny)
library(shinydashboard)

header<-dashboardHeader(title='Disability Debate in Parliament')##Fix header length

body<-dashboardBody(

  fluidRow(
  column(width=9,
      plotOutput('hansardplot')
  ),
  
  column(width=3,
         box(width=NULL,
  
  checkboxGroupInput('category_input',
                     'Category',
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

  
dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  skin = "purple",
  body
)
