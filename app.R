install.packages("DT")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)


# Load dataset
final_data_us <- read.csv("clean_dashboard_data.csv")

# Convert date
final_data_us$Order.Date <- as.Date(final_data_us$Order.Date)
final_data_us$Year <- format(final_data_us$Order.Date, "%Y")

# =========================
# UI
# =========================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Superstore Dashboard"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("bar-chart")
      ),
      
      menuItem(
        "Data Table",
        tabName = "datatable",
        icon = icon("table")
      )
      
    ),
    
    br(),
    
    selectInput(
      "region",
      "Select Region:",
      choices = c("All", unique(final_data_us$Region)),
      selected = "All"
    ),
    
    selectInput(
      "category",
      "Select Category:",
      choices = c("All", unique(final_data_us$Category)),
      selected = "All"
    ),
    
    selectInput(
      "segment",
      "Select Segment:",
      choices = c("All", unique(final_data_us$Segment)),
      selected = "All"
    ),
    
    selectInput(
      "state",
      "Select State:",
      choices = c("All", unique(final_data_us$State)),
      selected = "All"
    ),
    
    selectInput(
      "shipmode",
      "Select Ship Mode:",
      choices = c("All", unique(final_data_us$Ship.Mode)),
      selected = "All"
    ),
    
    sliderInput(
      "year",
      "Select Year:",
      min = min(as.numeric(final_data_us$Year)),
      max = max(as.numeric(final_data_us$Year)),
      value = c(
        min(as.numeric(final_data_us$Year)),
        max(as.numeric(final_data_us$Year))
      ),
      sep = ""
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      # =========================
      # OVERVIEW TAB
      # =========================
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = NULL,
            
            h1(
              "Superstore Sales Performance Dashboard",
              style = "font-weight:bold;"
            ),
            
            h4(
              "Interactive Business Intelligence Dashboard using R Shiny",
              style = "color:gray;"
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("salesBox"),
          valueBoxOutput("profitBox"),
          valueBoxOutput("ordersBox")
        ),
        
        fluidRow(
          valueBoxOutput("discountBox"),
          valueBoxOutput("marginBox")
        ),
        
        fluidRow(
          
          box(
            plotlyOutput("topSalesPlot"),
            width = 6
          ),
          
          box(
            plotlyOutput("discountProfitPlot"),
            width = 6
          )
          
        ),
        
        fluidRow(
          
          box(
            plotlyOutput("salesTrendPlot"),
            width = 6
          ),
          
          box(
            plotlyOutput("pieChart"),
            width = 6
          )
          
        ),
        
        fluidRow(
          
          box(
            plotlyOutput("heatmapPlot"),
            width = 12
          )
          
        ),
        
        fluidRow(
          
          box(
            plotlyOutput("segmentPlot"),
            width = 12
          )
          
        )
        
      ),
      
      # =========================
      # DATA TABLE TAB
      # =========================
      
      tabItem(
        tabName = "datatable",
        
        fluidRow(
          
          box(
            title = "Interactive Data Table",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            DTOutput("dataTable")
          )
          
        )
        
      )
      
    )
    
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output) {
  
  filtered_data <- reactive({
    
    data <- final_data_us
    
    if(input$region != "All"){
      data <- data %>%
        filter(Region == input$region)
    }
    
    if(input$category != "All"){
      data <- data %>%
        filter(Category == input$category)
    }
    
    if(input$segment != "All"){
      data <- data %>%
        filter(Segment == input$segment)
    }
    
    if(input$state != "All"){
      data <- data %>%
        filter(State == input$state)
    }
    
    if(input$shipmode != "All"){
      data <- data %>%
        filter(Ship.Mode == input$shipmode)
    }
    
    data <- data %>%
      filter(
        as.numeric(Year) >= input$year[1],
        as.numeric(Year) <= input$year[2]
      )
    
    return(data)
    
  })
  
  # =========================
  # KPI BOXES
  # =========================
  
  output$salesBox <- renderValueBox({
    
    valueBox(
      paste0("$", round(sum(filtered_data()$Sales))),
      "Total Sales",
      icon = icon("dollar-sign"),
      color = "blue"
    )
    
  })
  
  output$profitBox <- renderValueBox({
    
    valueBox(
      round(sum(filtered_data()$Profit)),
      "Total Profit",
      icon = icon("line-chart"),
      color = "green"
    )
    
  })
  
  output$ordersBox <- renderValueBox({
    
    valueBox(
      nrow(filtered_data()),
      "Total Orders",
      icon = icon("shopping-cart"),
      color = "orange"
    )
    
  })
  
  output$discountBox <- renderValueBox({
    
    valueBox(
      round(mean(filtered_data()$Discount), 2),
      "Average Discount",
      icon = icon("percent"),
      color = "red"
    )
    
  })
  
  output$marginBox <- renderValueBox({
    
    margin <- sum(filtered_data()$Profit) /
      sum(filtered_data()$Sales)
    
    valueBox(
      paste0(round(margin * 100, 2), "%"),
      "Profit Margin",
      icon = icon("pie-chart"),
      color = "purple"
    )
    
  })
  
  # =========================
  # TOP SALES PLOT
  # =========================
  
  output$topSalesPlot <- renderPlotly({
    
    top_sales_data <- filtered_data() %>%
      group_by(State) %>%
      summarise(
        Total_Sales = sum(Sales),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Sales)) %>%
      slice(1:10)
    
    p <- ggplot(
      top_sales_data,
      aes(
        x = reorder(State, Total_Sales),
        y = Total_Sales
      )
    ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Top 10 States by Sales",
        x = "State",
        y = "Total Sales"
      ) +
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # =========================
  # DISCOUNT VS PROFIT
  # =========================
  
  output$discountProfitPlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = Discount,
        y = Profit
      )
    ) +
      geom_point(
        color = "darkred",
        alpha = 0.5
      ) +
      labs(
        title = "Discount vs Profit",
        x = "Discount",
        y = "Profit"
      ) +
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # =========================
  # SALES & PROFIT TREND
  # =========================
  
  output$salesTrendPlot <- renderPlotly({
    
    trend_data <- filtered_data() %>%
      group_by(Year) %>%
      summarise(
        Total_Sales = sum(Sales),
        Total_Profit = sum(Profit),
        .groups = "drop"
      )
    
    p <- ggplot(trend_data, aes(x = as.numeric(Year))) +
      
      geom_line(
        aes(y = Total_Sales, color = "Sales"),
        linewidth = 1.5
      ) +
      
      geom_point(
        aes(y = Total_Sales, color = "Sales"),
        size = 3
      ) +
      
      geom_line(
        aes(y = Total_Profit * 10, color = "Profit"),
        linewidth = 1.5
      ) +
      
      geom_point(
        aes(y = Total_Profit * 10, color = "Profit"),
        size = 3
      ) +
      
      scale_color_manual(
        values = c(
          "Sales" = "darkgreen",
          "Profit" = "blue"
        )
      ) +
      
      labs(
        title = "Sales and Profit Trend Over Time",
        x = "Year",
        y = "Value",
        color = "Metric"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # =========================
  # CATEGORY PIE CHART
  # =========================
  
  output$pieChart <- renderPlotly({
    
    category_sales <- filtered_data() %>%
      group_by(Category) %>%
      summarise(
        Total_Sales = sum(Sales),
        .groups = "drop"
      )
    
    plot_ly(
      category_sales,
      labels = ~Category,
      values = ~Total_Sales,
      type = "pie"
    )
    
  })
  
  # =========================
  # HEATMAP
  # =========================
  
  output$heatmapPlot <- renderPlotly({
    
    heatmap_data <- filtered_data() %>%
      group_by(Region, Category) %>%
      summarise(
        Total_Sales = sum(Sales),
        .groups = "drop"
      )
    
    p <- ggplot(
      heatmap_data,
      aes(
        x = Category,
        y = Region,
        fill = Total_Sales
      )
    ) +
      geom_tile() +
      labs(
        title = "Sales Heatmap by Region and Category",
        x = "Category",
        y = "Region"
      ) +
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # =========================
  # CUSTOMER SEGMENT
  # =========================
  
  output$segmentPlot <- renderPlotly({
    
    segment_data <- filtered_data() %>%
      group_by(Segment) %>%
      summarise(
        Total_Profit = sum(Profit),
        .groups = "drop"
      )
    
    p <- ggplot(
      segment_data,
      aes(
        x = Segment,
        y = Total_Profit,
        fill = Segment
      )
    ) +
      geom_col() +
      labs(
        title = "Profit by Customer Segment",
        x = "Segment",
        y = "Total Profit"
      ) +
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # =========================
  # DATA TABLE
  # =========================
  
  output$dataTable <- renderDT({
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  
}

# =========================
# RUN APP
# =========================

shinyApp(ui, server)