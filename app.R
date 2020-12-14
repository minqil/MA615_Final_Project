library(tidyverse)
library(lubridate)
library(bizdays)
library(tidyquant)
library(quantmod)
library(knitr)

# read data
load("portfolio1.RData")
load("portfolio_values.RData")
load("portfolio_growth_monthly_multi.RData")

min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]




ui<-navbarPage("Stock Portfolio",
               
                          tabPanel("Daily Stock Prices",
                                   br(),
                                   titlePanel(h4("Options")),
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "price_symbol1",
                                       label = "Symbol:",
                                       choices = c("AAPL", "TSLA", "GILD", "ZG", "Portfolio")  ),
                                     p("Note: stock selection was based on", a(href="https://capital.com/best-stocks-to-invest-in-right-now", "this article" ),"from capital.com website.")
                                     ),
                                     
                                   mainPanel(
                                     h2('Daily Stock Prices'),
                                     plotOutput('price1')
                                   )
                          ),
               
               navbarMenu("Portfolio Growth",
                 
                 tabPanel("Portfolio Weights",
                          br(),
                          titlePanel(h4("Weights Setting")),
                          sidebarPanel(
                            sliderInput("AAPL", "The weight of AAPL:",
                                        min = 0, max = 1,
                                        value = 0.2, step = 0.01),
                            
                            sliderInput("TSLA", "The weight of TSLA:",
                                        min = 0, max = 1,
                                        value = 0.1, step = 0.01),
                            
                            sliderInput("GILD", "The weight of GILD:",
                                        min = 0, max = 1,
                                        value = 0.5, step = 0.01),
                            
                            sliderInput("ZG", "The weight of ZG:",
                                        min = 0, max = 1,
                                        value = 0.2, step = 0.01),
                            
                            
                            HTML("Note: <p>1. The sum of weights must be equal to 1. <p>2. After choosing weights, you need to click the button to update the plot.</p>"),
                            
                            actionButton("update", "Update View")
                         
                          ),
                          
                          mainPanel(
                            h2("Portfolio Growth"),
                            plotOutput('return1')
                          )
                 ),
                 
                 tabPanel("Portfolio Optimization",
                          br(),
                          mainPanel(
                            h2("Portfolio Optimization"),
                            h5("We used historical price data from January 1, 2016 to June 30, 2020"),
                            plotOutput('opti1')
                          ),
                          
                          mainPanel(
                            br(),
                            plotOutput('opti2')
                          )
                          
                 )
                 
                 
               ),
               
                
                          tabPanel("DataTable",
                                   fluidRow(
                                     column(4,
                                            selectInput("symbol1",
                                                        "Symbol:",
                                                        c("All",
                                                          unique(as.character(portfolio1$symbol))))
                                     ),
                                     column(4,
                                            selectInput("company1",
                                                        "Company:",
                                                        c("All",
                                                          unique(as.character(portfolio1$company))))
                                     ),
                                     column(4,
                                            selectInput("date1",
                                                        "Date:",
                                                        c("All",
                                                          unique(as.character(portfolio1$date))))
                                     )
                                     
                                   ),
                                   DT::dataTableOutput("table1")
                          )

               
)



server <- function(input,output){
  
  # price1
  # Reactive value for selected symbol
  
  plot_price1 <- reactive({
    
    if (input$price_symbol1=="Portfolio"){
      p1<-portfolio1 %>%
        ggplot(aes(x = date, y = close, group = symbol)) +
        geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
        labs( y = "Closing Price", x = "") + 
        facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
        scale_y_continuous(labels = scales::dollar) +
        theme_tq()
    }else{
      p1<-portfolio1 %>%
        filter(symbol %in% input$price_symbol1) %>%
        ggplot(aes(x = date, y = close, group = symbol)) +
        geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
        labs( y = "Closing Price", x = "") + 
        facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
        scale_y_continuous(labels = scales::dollar) +
        theme_tq()
    }
    
    ggpubr::ggarrange(p1,ncol = 1)
  
    })
  output$price1=renderPlot(plot_price1())
  
  # returns 1
  
  weightsInput1<-eventReactive(input$update,{
    
    wts1 <- c(input$AAPL, input$TSLA, input$GILD, input$ZG)
  }, ignoreNULL = FALSE)
  
  plot_return1<-reactive({
    stock_returns_daily <- portfolio1 %>%
      group_by(symbol) %>%
      tq_transmute(select     = close, 
                   mutate_fun = periodReturn, 
                   period     = "daily", 
                   col_rename = "returns") %>% 
      tq_portfolio(assets_col   = symbol, 
                   returns_col  = returns, 
                   weights      = weightsInput1(), 
                   col_rename   = "investment.growth",
                   wealth.index = TRUE) %>%
      mutate(investment.growth = investment.growth * 250000)
    
    p2<-stock_returns_daily %>%
      ggplot(aes(x = date, y = investment.growth)) +
      geom_line(size = 1, color = palette_light()[[1]]) +
      labs(x = "Date", y = "Portfolio Value") +
      geom_smooth(method = "loess") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::dollar)
    
    ggpubr::ggarrange(p2,ncol = 1)
  
    })
  output$return1=renderPlot(plot_return1())
  
  # optimization
  
  plot_opti1<-reactive({
    
   
    p3 <- min_var %>%
      gather(AAPL:ZG, key = Asset,
             value = Weights) %>%
      mutate(Asset = as.factor(Asset)) %>%
      ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
      geom_bar(stat = 'identity') +
      geom_text(aes(label = round(Weights,2)), hjust = 0.5, colour = "black", position = position_dodge(.9), size = 4)+
      theme_minimal() +
      labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
      scale_y_continuous(labels = scales::percent)
    
    
    p4 <- max_sr %>%
      gather(AAPL:ZG, key = Asset,
             value = Weights) %>%
      mutate(Asset = as.factor(Asset)) %>%
      ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
      geom_bar(stat = 'identity') +
      geom_text(aes(label = round(Weights,2)), hjust = 0.5, colour = "black", position = position_dodge(.9), size = 4)+
      theme_minimal() +
      labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
      scale_y_continuous(labels = scales::percent)
     
    ggpubr::ggarrange(p3,p4,ncol = 2)
  })
  output$opti1=renderPlot(plot_opti1())
  
  
  plot_opti2<-reactive({
    
    p5 <- portfolio_values %>%
      ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
      geom_point() +
      theme_classic() +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = 'Annualized Risk',
           y = 'Annualized Returns',
           title = "Portfolio Optimization & Efficient Frontier") +
      geom_point(aes(x = Risk,
                     y = Return), data = min_var, color = 'red') +
      geom_point(aes(x = Risk,
                     y = Return), data = max_sr, color = 'red') +
      annotate('text', x = 0.30, y = 0.37, label = "Tangency Portfolio") +
      annotate('text', x = 0.29, y = 0.15, label = "Minimum variance portfolio")
    
    p6<-portfolio_growth_monthly_multi %>%
      ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) +
      geom_line(size = 1) +
      labs(title = "Portfolio Growth",
           caption = "The portfolio with the highest sharpe ratio is a Standout!",
           x = "", y = "Portfolio Value",
           color = "Portfolio") +
      geom_smooth(method = "loess") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::dollar)
    
    ggpubr::ggarrange(p5,p6,ncol = 2)
  })
  
  output$opti2=renderPlot(plot_opti2())
  
  # table1
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- portfolio1
    if (input$symbol1 != "All") {
      data <- data[data$symbol == input$symbol1,]
    }
    if (input$company1 != "All") {
      data <- data[data$company == input$company1,]
    }
    if (input$date1 != "All") {
      data <- data[data$date == input$date1,]
    }
    
    data
  }))
  
  
}

shinyApp(ui = ui, server = server)



