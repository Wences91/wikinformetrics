library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)

# Import data
df <- read.delim2('data/page_metrics_top.tsv',
                  stringsAsFactors = FALSE,
                  check.names = FALSE)

# Define UI
ui <- navbarPage(
  
  # Application title
  theme = shinytheme('cosmo'),
  title='Wikinformetrics',
  
  # Tab panels
  tabPanel('Scatterplot',
           titlePanel('Case study: informetric analysis of the English Wikipedia'),
           HTML('This R Shiny app is part of the paper \'<i>Wikinformetrics: Construction and description of an open Wikipedia knowledge graph dataset for informetric purposes</i>\' and provides interactive visualizations of the top Wikipedia articles by indicator to better understand the analytical dimension of these metrics.<br><br>'),
           sidebarLayout(
             sidebarPanel(
               selectInput('indicator1', 'Axis X:',
                           c('Editors', 'Edits', 'Linked', 'Links', 'Age', 'Length',
                             'Talkers', 'Talks', 'Views', 'References',
                             'Pub. referenced', 'URLs', 'URLs referenced'),
                           selected = 'Talks'),
               selectInput('indicator2', 'Axis Y:',
                           c('Editors', 'Edits', 'Linked', 'Links', 'Age', 'Length',
                             'Talkers', 'Talks', 'Views', 'References',
                             'Pub. referenced', 'URLs', 'URLs referenced'),
                           selected = 'Views'),
               selectInput('indicator3', 'Size:',
                           c('None', 'Editors', 'Edits', 'Linked', 'Links', 'Age', 'Length',
                             'Talkers', 'Talks', 'Views', 'References',
                             'Pub. referenced', 'URLs', 'URLs referenced'),
                           selected = 'None'),
               selectizeInput('toremove', 'Remove',
                              df$Title,
                              options = list(placeholder = 'Select a page name', maxItems = 10),
                              selected='Main Page'),
               radioButtons('log', 'Log scale:',
                            c('True' = 'True',
                              'False' = 'False')),
               HTML('<b>Note:</B> Only the top 1000 Wikipedia articles for each indicator are included in the plot, making a total of 7374 unique articles.')
             ),
             mainPanel(
               plotlyOutput('comparePlot', height = 600)
             )
           ),
           hr(),
           helpText(HTML('By Wenceslao Arroyo-Machado <a href="https://twitter.com/Wences91"><i class="fab fa-twitter"></i></a>, Daniel Torres-Salinas <a href="https://twitter.com/torressalinas"><i class="fab fa-twitter"></i></a> and Rodrigo Costas <a href="https://twitter.com/RodrigoCostas1/"><i class="fab fa-twitter"></i></a><br>Code available at <a href="https://github.com/Wences91/wikinformetrics"><i class="fab fa-github"></i></a>'))),
           
  tabPanel('Ranking',
           titlePanel('Case study: informetric analysis of the English Wikipedia'),
           HTML('This R Shiny app is part of the paper \'<i>Wikinformetrics: Construction and description of an open Wikipedia knowledge graph dataset for informetric purposes</i>\' and provides interactive visualizations of the top Wikipedia articles by indicator to better understand the analytical dimension of these metrics.<br><br>'),
           sidebarLayout(
             sidebarPanel(
               selectInput('filter_indicator', 'Order in descending order by:',
                           c('Editors', 'Edits', 'Linked', 'Links', 'Age', 'Length',
                             'Talkers', 'Talks', 'Views', 'References',
                             'Pub. referenced', 'URLs', 'URLs referenced'),
                           multiple = FALSE, selected=c('Views')),
               selectInput('select_indicators', 'Include:',
                           c('Editors', 'Edits', 'Linked', 'Links', 'Age', 'Length',
                             'Talkers', 'Talks', 'Views', 'References',
                             'Pub. referenced', 'URLs', 'URLs referenced'),
                           multiple = TRUE, selected=c('Age', 'Views')),
               downloadButton('downloadData', 'Download this table'),
               HTML('<br><br><b>Note:</B> Only the top 1000 Wikipedia articles for each indicator are included in the plot, making a total of 7374 unique articles.')
             ),
             mainPanel(
               column(12,
                      dataTableOutput('table')
               ))
           ),
           hr(),
           helpText(HTML('By Wenceslao Arroyo-Machado <a href="https://twitter.com/Wences91"><i class="fab fa-twitter"></i></a>, Daniel Torres-Salinas <a href="https://twitter.com/torressalinas"><i class="fab fa-twitter"></i></a> and Rodrigo Costas <a href="https://twitter.com/RodrigoCostas1/"><i class="fab fa-twitter"></i></a><br>Code available at <a href="https://github.com/Wences91/wikinformetrics"><i class="fab fa-github"></i></a>')))
)

# Define server 
server <- function(input, output) {
  
  output$comparePlot <- renderPlotly({
    
    df_2 <- df[which(!(df$Title %in% input$toremove)),]
    
    x <- reactive({
      df_2[,input$indicator1]
    })
    
    y <- reactive({
      df_2[,input$indicator2]
    })
    
    if(input$indicator3 != 'None'){
      df_2 <- df_2[,c('Title', 'Creation', input$indicator1, input$indicator2, input$indicator3)]
      
      size <- reactive({
        df_2[,input$indicator3]
      })
      
      p <- plotly::plot_ly(data = df_2, x = x(), y = y(), size = size(),
                           type = 'scatter', mode = 'markers',
                           text = ~paste('Title:', Title, '<br>Creation:', Creation),
                           color = I('#41a18b'),
                           alpha = 0.7)
    }else{
      df_2 <- df_2[,c('Title', 'Creation', input$indicator1, input$indicator2)]
      
      p <- plotly::plot_ly(data = df_2, x = x(), y = y(),
                           type = 'scatter', mode = 'markers',
                           text = ~paste('Title:', Title, '<br>Creation:', Creation),
                           color = I('#41a18b'),
                           alpha = 0.7)
    }
    
    if(input$log=='True'){
      layout(p, xaxis = list(title = input$indicator1,
                             type = 'log'),
             yaxis = list(title = input$indicator2,
                          type = 'log'))
    }else{
      layout(p, xaxis = list(title = input$indicator1),
             yaxis = list(title = input$indicator2))
    }
  })
  
  
  output$table <- renderDataTable(df[order(df[,input$filter_indicator], decreasing = TRUE), c('Title', input$select_indicators)])
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('save.tsv', sep = '')
    },
    content = function(file) {
      write.table(df[order(df[,input$filter_indicator], decreasing = TRUE), c('Title', input$select_indicators)], file, row.names = FALSE, sep='\t')
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
