
library(shiny)
library(ggplot2)
library(scales)

shinyServer(function(input, output, session) {
  
  disability_phrase_groups <- readRDS('./data/disability_phrase_groups.rds')
  
  disability_with_sample <- readRDS("./data/disability_with_sample.rds")
  
  average_sentiment <- readRDS("./data/average_sentiment.rds")
  
  base_breaks <- function(n = 10){
    function(x) {
      axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
  }
  
  fmt_dcimals <- function(decimals=0){
    function(x) as.character(round(x,decimals))
  }
  
  getDataSet <- reactive({
  
  all_data <- disability_phrase_groups[disability_phrase_groups$Term == input$category_input
                                      & disability_phrase_groups$Year >= input$year[1]
                                      & disability_phrase_groups$Year <= input$year[2],]
  })
  
  
  sentiDataSet <- reactive({
    
    if(input$options == "All"){
    
    senti_data <- disability_with_sample[disability_with_sample$Year >= input$senti_year[1]
                              & disability_with_sample$Year <= input$senti_year[2],]
    } else {
      
      senti_data <- disability_with_sample[disability_with_sample$Year >= input$senti_year[1]
                                           & disability_with_sample$Year <= input$senti_year[2]
                                           & disability_with_sample$Type == input$debate_type,]
      
    }
    
  })
  
  sentiDataBar <- reactive({
    
    if(input$options == "All"){
    
    senti_data <- average_sentiment[average_sentiment$Year >= input$senti_year[1]
                                  & average_sentiment$Year <= input$senti_year[2],]
    
    } else {
      
      senti_data <- average_sentiment[average_sentiment$Year >= input$senti_year[1]
                                      & average_sentiment$Year <= input$senti_year[2]
                                      & average_sentiment$Type == input$debate_type,]
      
    }
    
  })
  
  line_colours <- c('Disabled Person'	=	'#006109',
                    'Disabled Men'	=	'#8a2093',
                    'Disabled Women'	=	'#00d38d',
                    'Disabled Children'	=	'#ff50a0',
                    'Disability Other'	=	'#5b5700',
                    'People With Disability'	=	'#7f8cff',
                    'Children With Disability'	=	'#fa7422',
                    'Any With Disability'	=	'#134eae',
                    'Independent Living'	=	'#d70039',
                    'Wheelchair'	=	'#97bcff',
                    'Paralympic'	=	'#8a3a1b',
                    'Spastic'	=	'#936996',
                    'Sub-Normal'	=	'#ff9176',
                    'Amputee'	=	'#9b2535',
                    'Retard'	=	'#ff7798')
  
  line_styles <- c('Disabled Person'	=	'solid',
                    'Disabled Men'	=	'solid',
                    'Disabled Women'	=	'solid',
                    'Disabled Children'	=	'solid',
                    'Disability Other'	=	'longdash',
                    'People With Disability'	=	'longdash',
                    'Children With Disability'	=	'longdash',
                    'Any With Disability'	=	'longdash',
                    'Independent Living'	=	'twodash',
                    'Wheelchair'	=	'twodash',
                    'Paralympic'	=	'twodash',
                    'Spastic'	=	'dotdash',
                    'Sub-Normal'	=	'dotdash',
                    'Amputee'	=	'dotdash',
                    'Retard'	=	'dotdash')
  
  senti_line <- c('All Debate' = 'solid',
                  'Disability' = 'solid')
  
  
  output$hansardplot<-renderPlot({
    
    dataSet <- getDataSet()
    
    p3 <- ggplot(dataSet, aes(x=Date, group = Term, col = Term))
    p3 + geom_smooth(aes(y=value, linetype = Term, col=Term), size=1.5, formula=y ~ log(x), se=FALSE) +
      coord_cartesian(xlim = c(as.Date(min(dataSet$Date)), as.Date(max(dataSet$Date)))) + 
      scale_linetype_manual(values=line_styles) +
      scale_color_manual(values=line_colours) + 
      scale_x_date(date_breaks = '5 year',date_labels = '%Y') +
      #theme_bw() +
      scale_y_continuous(trans=log_trans(5),
                         breaks = base_breaks(),
                         name='Average Mentions per Day (Logarithmic Scale)',
                         labels = fmt_dcimals(3)) + 
      theme(axis.text.x = element_text(angle = 30, hjust = 1), 
            legend.position='bottom', legend.background = element_rect()) 
    
  })
  
  output$sentiplot<-renderPlot({
    
    senti_set <- sentiDataSet()
    
    p6 <- ggplot(senti_set, aes(x=Date, group = Type, col = Type))
    
    p6 + geom_smooth(aes(y=value, linetype = Type, col=Type), size=1.5, formula=y ~ log(x)) +
      scale_x_date(date_breaks = '5 year',date_labels = '%Y', name = "Date") + 
      scale_y_continuous(name='Sentiment Score') + 
      scale_linetype_manual(values=senti_line) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), 
            legend.position='bottom', legend.background = element_rect())
    
  })
  
  output$sentibox<-renderPlot({
    
    senti_set <- sentiDataSet()
    
    senti_set$Year <- as.factor(senti_set$Year)
    
    p7 <- ggplot(senti_set, aes(x=Year, y = value))
    
    p7 + geom_boxplot(aes(col = Type)) +
      scale_x_discrete(name="Year", breaks=seq(1935,2020, by=5)) + 
      scale_y_continuous(name='Sentiment Score') + 
      scale_linetype_manual(values=senti_line) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), 
            legend.position='bottom', legend.background = element_rect())
    
  })
  

  
  output$sentibar<-renderPlot({
    
    senti_bar_data <- sentiDataBar()
    
    senti_bar_data$Year <- as.factor(senti_bar_data$Year)
    
    p8 <- ggplot(senti_bar_data, aes(x=Year, y=Freq, group = Type, col = Type, fill=Type,width=.75))
    p8 + geom_bar(stat = "identity", position = "dodge") +
      scale_x_discrete(name="Year", breaks=seq(1935,2020, by=5)) + 
      scale_y_continuous(name='Sentiment Score') + 
      theme(axis.text.x = element_text(angle = 30, hjust = 1), 
            legend.position='bottom', legend.background = element_rect())
    
    
  })
  
##next up, add comparison to total words
  
  
})
