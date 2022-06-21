library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

# Import data
df <- read.delim2('data/page_metrics_top.tsv', stringsAsFactors = FALSE)

# Define UI
ui <- navbarPage(
  
  # Application title
  theme = shinytheme('cosmo'),
  title='Wikimetrics',
  
  # Tab panels
  tabPanel('Comparative',
           sidebarLayout(
             sidebarPanel(
               selectInput('indicator1', 'Indicator 1:',
                           c('age', 'views', 'page_edits', 'editors', 'len', 'references',
                             'talks', 'talkers', 'links', 'linked', 'urls', 'ref_urls',
                             'ref_pubs'),
                           selected = 'talks'),
               selectInput('indicator2', 'Indicator 2:',
                           c('age', 'views', 'page_edits', 'editors', 'len', 'references',
                             'talks', 'talkers', 'links', 'linked', 'urls', 'ref_urls',
                             'ref_pubs'),
                           selected = 'views'),
               radioButtons('log', 'Log10 scale:',
                            c('True' = 'True',
                              'False' = 'False'))
             ),
             mainPanel(
               plotOutput('comparePlot', height = 600)
             )
           ),
           hr(),
           helpText(HTML('By Wenceslao Arroyo-Machado <a href="https://twitter.com/Wences91"><i class="fab fa-twitter"></i></a>'))),
           
  tabPanel('Data',
           sidebarLayout(
             sidebarPanel(
               selectInput('indicator', 'Indicator:',
                           c('age', 'views', 'page_edits', 'editors', 'len', 'references',
                             'talks', 'talkers', 'links', 'linked', 'urls', 'ref_urls',
                             'ref_pubs'),
                           multiple = FALSE, selected=c('views')),
               downloadButton('downloadData', 'Download all data')
             ),
             mainPanel(
               column(12,
                      dataTableOutput('table')
               ))
           ),
           hr(),
           helpText(HTML('By Wenceslao Arroyo-Machado <a href="https://twitter.com/Wences91"><i class="fab fa-twitter"></i></a>')))
)

# Define server 
server <- function(input, output) {
  
  output$comparePlot <- renderPlot({
    
    df_2 <- df[,c(input$indicator1, input$indicator2)]
    
    
    p <- ggplot(data=df_2,
             aes(x=get(input$indicator1),
                 y=get(input$indicator2)))+
        geom_point(alpha=0.5, color='#41a18b')+
        theme_minimal()+
        labs(x=input$indicator1,
             y=input$indicator2)+
        theme(legend.position = 'none',
              strip.background=element_rect(colour='#373a3c', fill='#373a3c'),
              text=element_text(family='Arial', size=16, color='black'),
              axis.text=element_text(color='black', size=13),
              axis.ticks=element_line(color='black'),
              strip.text = element_text(size=15))
    
    if(input$log=='True'){
      p <- p + scale_x_log10() + scale_y_log10()
    }
    
    p
    
  })
  
  
  output$table <- renderDataTable(df[order(df[,input$indicator], decreasing = TRUE),])
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('save.tsv', sep = '')
    },
    content = function(file) {
      write.table(df[order(df[,input$indicator], decreasing = TRUE),], file, row.names = FALSE, sep='\t')
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
