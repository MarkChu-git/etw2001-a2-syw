library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(scales)

# ── Load datasets ──────────────────────────────────────────────────────────────
# Cleaned by scripts/prepare_superstore.R from the raw data/superstore.xlsx
# (US market subset, 9,994 rows). Re-run that script to regenerate this file.
superstore <- read.csv("data/superstore_us_clean.csv", stringsAsFactors = FALSE)
superstore$Order.Date <- as.Date(superstore$Order.Date)
superstore$Year <- as.integer(format(superstore$Order.Date, "%Y"))

# Three external datasets, period-matched to 2011-2014 and keyed on State + Year.
# Built reproducibly by scripts/prepare_external_data.R (US Census PEP, US Census
# SAIPE, US BLS LAUS). See data/dataset_provenance.csv for sources and citations.
population <- read.csv("data/population_by_state.csv",   stringsAsFactors = FALSE)
income     <- read.csv("data/income_by_state.csv",       stringsAsFactors = FALSE)
unemploy   <- read.csv("data/unemployment_by_state.csv", stringsAsFactors = FALSE)

# Collapse the State+Year series to one 2011-2014 average per state, so each state
# contributes a single point to the cross-sectional economic-context charts.
econ_profile <- population %>%
  left_join(income,   by = c("State", "Year")) %>%
  left_join(unemploy, by = c("State", "Year")) %>%
  group_by(State) %>%
  summarise(
    Population              = mean(Population, na.rm = TRUE),
    Median_Household_Income = mean(Median_Household_Income, na.rm = TRUE),
    Unemployment_Rate       = mean(Unemployment_Rate, na.rm = TRUE),
    .groups = "drop"
  )

# State-level retail aggregates joined to the economic profile (join on State)
state_econ <- superstore %>%
  group_by(State) %>%
  summarise(
    Total_Sales   = sum(Sales, na.rm = TRUE),
    Total_Profit  = sum(Profit, na.rm = TRUE),
    Avg_Discount  = mean(Discount, na.rm = TRUE),
    Profit_Margin = sum(Profit) / sum(Sales),
    .groups = "drop"
  ) %>%
  left_join(econ_profile, by = "State")

# ── UI ──────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "Superstore Analysis"
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",          tabName = "overview",  icon = icon("chart-bar")),
      menuItem("Economic Context",  tabName = "economic",  icon = icon("chart-line")),
      menuItem("Data Table",        tabName = "datatable", icon = icon("table"))
    ),
    br(),
    selectInput("region",   "Region:",   choices = c("All", sort(unique(superstore$Region))),   selected = "All"),
    selectInput("category", "Category:", choices = c("All", sort(unique(superstore$Category))), selected = "All"),
    selectInput("segment",  "Segment:",  choices = c("All", sort(unique(superstore$Segment))),  selected = "All"),
    selectInput("state",    "State:",    choices = c("All", sort(unique(superstore$State))),     selected = "All"),
    selectInput("shipmode", "Ship Mode:",choices = c("All", sort(unique(superstore$Ship.Mode))), selected = "All"),
    sliderInput("year", "Year Range:",
      min = min(superstore$Year), max = max(superstore$Year),
      value = c(min(superstore$Year), max(superstore$Year)),
      step = 1, sep = ""
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .research-question { background:#1a5276; color:white; padding:12px 18px;
        border-radius:4px; margin-bottom:16px; font-size:14px; }
      .research-question strong { font-size:15px; }
      .no-data-msg { text-align:center; color:#888; padding:60px 20px; font-size:16px; }
    "))),

    tabItems(

      # ── OVERVIEW TAB ──────────────────────────────────────────────────────────
      tabItem(tabName = "overview",

        div(class = "research-question",
          strong("Research Question: "),
          "How do US state-level socioeconomic factors — population size, household income,
           and unemployment rate — relate to retail sales performance, profitability, and
           discount patterns across regions in the Superstore dataset (2011–2014)?"
        ),

        fluidRow(
          valueBoxOutput("salesBox"),
          valueBoxOutput("profitBox"),
          valueBoxOutput("ordersBox"),
          valueBoxOutput("discountBox"),
          valueBoxOutput("marginBox")
        ),

        fluidRow(
          # Chart 1: Top States by Sales (bar)
          box(title = "Fig 1 — Top 10 States by Sales", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("topSalesPlot", height = "320px")),
          # Chart 2: Discount vs Profit Margin (scatter)
          box(title = "Fig 2 — Discount Rate vs Profit Margin", width = 6, status = "warning", solidHeader = TRUE,
              plotlyOutput("discountProfitPlot", height = "320px"))
        ),

        fluidRow(
          # Chart 3: Sales & Profit Trend (line — proper dual-axis)
          box(title = "Fig 3 — Annual Sales and Profit Trend", width = 6, status = "success", solidHeader = TRUE,
              plotlyOutput("salesTrendPlot", height = "320px")),
          # Chart 4: Sales intensity vs economic context (external data — lollipop)
          box(title = "Fig 4 — Sales Intensity vs Economic Context", width = 6, status = "info", solidHeader = TRUE,
              plotlyOutput("econLollipop", height = "320px"))
        ),

        fluidRow(
          # Chart 5: Sales Heatmap by Region × Category
          box(title = "Fig 5 — Sales Heatmap: Region × Category", width = 12, status = "danger", solidHeader = TRUE,
              plotlyOutput("heatmapPlot", height = "320px"))
        )
      ),

      # ── ECONOMIC CONTEXT TAB ─────────────────────────────────────────────────
      tabItem(tabName = "economic",

        div(class = "research-question",
          strong("External Data Context: "),
          "State economic profiles — population (US Census Bureau PEP), median household
           income (US Census Bureau SAIPE), and unemployment rate (US BLS LAUS) — each
           averaged over 2011–2014 to match the Superstore transactions, shown here
           alongside state retail performance."
        ),

        fluidRow(
          box(title = "Median Household Income vs State Sales", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("incomeSalesPlot", height = "340px")),
          box(title = "Unemployment Rate vs Profit Margin", width = 6, status = "warning", solidHeader = TRUE,
              plotlyOutput("unempProfitPlot", height = "340px"))
        ),

        fluidRow(
          box(title = "Population vs Total Sales by State", width = 12, status = "success", solidHeader = TRUE,
              plotlyOutput("popSalesPlot", height = "340px"))
        )
      ),

      # ── DATA TABLE TAB ───────────────────────────────────────────────────────
      tabItem(tabName = "datatable",
        fluidRow(
          box(title = "Filtered Transaction Data", width = 12, status = "primary", solidHeader = TRUE,
              DTOutput("dataTable"))
        )
      )
    )
  )
)

# ── SERVER ──────────────────────────────────────────────────────────────────────
server <- function(input, output) {

  filtered <- reactive({
    d <- superstore
    if (input$region   != "All") d <- filter(d, Region   == input$region)
    if (input$category != "All") d <- filter(d, Category == input$category)
    if (input$segment  != "All") d <- filter(d, Segment  == input$segment)
    if (input$state    != "All") d <- filter(d, State    == input$state)
    if (input$shipmode != "All") d <- filter(d, Ship.Mode == input$shipmode)
    d <- filter(d, Year >= input$year[1], Year <= input$year[2])
    d
  })

  no_data_plot <- function() {
    plotly_empty() %>% layout(title = list(text = "No data for current filter selection", font = list(color = "#888")))
  }

  # ── KPI Boxes ────────────────────────────────────────────────────────────────
  output$salesBox <- renderValueBox({
    valueBox(dollar(round(sum(filtered()$Sales))), "Total Sales", icon("dollar-sign"), color = "blue")
  })
  output$profitBox <- renderValueBox({
    pval <- sum(filtered()$Profit)
    valueBox(dollar(round(pval)), "Total Profit", icon("chart-line"), color = if(pval >= 0) "green" else "red")
  })
  output$ordersBox <- renderValueBox({
    valueBox(comma(nrow(filtered())), "Total Orders", icon("shopping-cart"), color = "orange")
  })
  output$discountBox <- renderValueBox({
    valueBox(percent(mean(filtered()$Discount, na.rm = TRUE), accuracy = 0.1), "Avg Discount", icon("percent"), color = "red")
  })
  output$marginBox <- renderValueBox({
    m <- sum(filtered()$Profit) / sum(filtered()$Sales)
    valueBox(percent(m, accuracy = 0.1), "Profit Margin", icon("pie-chart"), color = "purple")
  })

  # ── Chart 1: Top 10 States by Sales (bar) ────────────────────────────────────
  output$topSalesPlot <- renderPlotly({
    d <- filtered()
    if (nrow(d) == 0) return(no_data_plot())
    top10 <- d %>%
      group_by(State) %>%
      summarise(Total_Sales = sum(Sales), .groups = "drop") %>%
      arrange(desc(Total_Sales)) %>%
      slice_head(n = 10)
    p <- ggplot(top10, aes(x = reorder(State, Total_Sales), y = Total_Sales,
                           text = paste0(State, ": ", dollar(round(Total_Sales))))) +
      geom_col(fill = "#2980b9") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
      labs(x = NULL, y = "Total Sales (USD)") +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = "text")
  })

  # ── Chart 2: Discount vs Profit Margin (scatter) ──────────────────────────────
  output$discountProfitPlot <- renderPlotly({
    d <- filtered()
    if (nrow(d) == 0) return(no_data_plot())
    p <- ggplot(d, aes(x = Discount, y = Profit_Margin, color = Category,
                       text = paste0("Discount: ", percent(Discount, 1),
                                     "<br>Margin: ", percent(Profit_Margin, 0.1),
                                     "<br>Category: ", Category))) +
      geom_point(alpha = 0.35, size = 1.2) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
      scale_x_continuous(labels = percent_format()) +
      scale_y_continuous(labels = percent_format()) +
      labs(x = "Discount Rate", y = "Profit Margin", color = NULL) +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = "text")
  })

  # ── Chart 3: Annual Sales & Profit Trend (dual-axis line) ─────────────────────
  output$salesTrendPlot <- renderPlotly({
    d <- filtered()
    if (nrow(d) == 0) return(no_data_plot())
    trend <- d %>%
      group_by(Year) %>%
      summarise(Total_Sales = sum(Sales), Total_Profit = sum(Profit), .groups = "drop")
    plot_ly(trend, x = ~Year) %>%
      add_trace(y = ~Total_Sales,  name = "Sales",  type = "scatter", mode = "lines+markers",
                line = list(color = "#27ae60", width = 2), marker = list(size = 6)) %>%
      add_trace(y = ~Total_Profit, name = "Profit", type = "scatter", mode = "lines+markers",
                line = list(color = "#2980b9", width = 2, dash = "dot"), marker = list(size = 6),
                yaxis = "y2") %>%
      layout(
        yaxis  = list(title = "Total Sales (USD)", tickformat = "$,.0f"),
        yaxis2 = list(title = "Total Profit (USD)", overlaying = "y", side = "right", tickformat = "$,.0f"),
        xaxis  = list(title = "Year", dtick = 1),
        legend = list(orientation = "h", x = 0, y = -0.2)
      )
  })

  # ── Chart 4: Sales intensity vs economic context (external data — lollipop) ───
  # Per-capita sales (population) with unemployment as colour and median income as
  # point size, so all three external datasets appear in an Overview-tab chart.
  output$econLollipop <- renderPlotly({
    d <- filtered()
    if (nrow(d) == 0) return(no_data_plot())
    ld <- d %>%
      group_by(State) %>%
      summarise(Total_Sales = sum(Sales), .groups = "drop") %>%
      left_join(econ_profile, by = "State") %>%
      filter(!is.na(Population)) %>%
      mutate(Sales_per_1k = Total_Sales / Population * 1000) %>%
      arrange(desc(Sales_per_1k)) %>%
      slice_head(n = 15)
    if (nrow(ld) == 0) return(no_data_plot())
    ld$State <- factor(ld$State, levels = rev(ld$State))
    p <- ggplot(ld, aes(x = State, y = Sales_per_1k,
                        text = paste0(State,
                                      "<br>Sales per 1,000: ", dollar(round(Sales_per_1k, 1)),
                                      "<br>Unemployment: ", round(Unemployment_Rate, 1), "%",
                                      "<br>Median income: ", dollar(round(Median_Household_Income)),
                                      "<br>Population: ", comma(round(Population))))) +
      geom_segment(aes(xend = State, y = 0, yend = Sales_per_1k), color = "grey75", linewidth = 0.6) +
      geom_point(aes(color = Unemployment_Rate, size = Median_Household_Income)) +
      coord_flip() +
      scale_color_gradient(low = "#1a9850", high = "#c0392b") +
      scale_size_continuous(range = c(3, 8), guide = "none") +
      labs(x = NULL, y = "Sales per 1,000 residents (USD)", color = "Unemp %") +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = "text")
  })

  # ── Chart 5: Heatmap (Region × Category) ─────────────────────────────────────
  output$heatmapPlot <- renderPlotly({
    d <- filtered()
    if (nrow(d) == 0) return(no_data_plot())
    hm <- d %>%
      group_by(Region, Category) %>%
      summarise(Total_Sales = sum(Sales), .groups = "drop")
    p <- ggplot(hm, aes(x = Category, y = Region, fill = Total_Sales,
                        text = paste0(Region, " / ", Category, ": ", dollar(round(Total_Sales))))) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#d6eaf8", high = "#1a5276", labels = dollar_format(scale = 1e-3, suffix = "K")) +
      labs(x = "Category", y = "Region", fill = "Sales (USD)") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  # ── Economic Context Charts ───────────────────────────────────────────────────
  output$incomeSalesPlot <- renderPlotly({
    d <- state_econ
    if (nrow(d) == 0 || all(is.na(d$Median_Household_Income))) return(no_data_plot())
    p <- ggplot(d, aes(x = Median_Household_Income, y = Total_Sales,
                       text = paste0(State, "<br>Income: ", dollar(Median_Household_Income),
                                     "<br>Sales: ", dollar(round(Total_Sales))))) +
      geom_point(aes(color = Total_Sales), size = 3, alpha = 0.85) +
      geom_smooth(method = "lm", se = TRUE, color = "#e74c3c", linewidth = 1) +
      scale_x_continuous(labels = dollar_format()) +
      scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
      scale_color_gradient(low = "#aed6f1", high = "#1a5276") +
      labs(x = "Median Household Income (USD, 2011–2014 avg)", y = "Total Retail Sales (USD)", color = "Sales") +
      theme_minimal(base_size = 11) + theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })

  output$unempProfitPlot <- renderPlotly({
    d <- state_econ
    if (nrow(d) == 0 || all(is.na(d$Unemployment_Rate))) return(no_data_plot())
    p <- ggplot(d, aes(x = Unemployment_Rate, y = Profit_Margin,
                       text = paste0(State, "<br>Unemployment: ", round(Unemployment_Rate, 1), "%",
                                     "<br>Profit Margin: ", percent(Profit_Margin, 0.1)))) +
      geom_point(aes(color = Profit_Margin), size = 3, alpha = 0.85) +
      geom_smooth(method = "lm", se = TRUE, color = "#e67e22", linewidth = 1) +
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      scale_y_continuous(labels = percent_format()) +
      scale_color_gradient2(low = "#e74c3c", mid = "#f9e79f", high = "#27ae60", midpoint = 0.1) +
      labs(x = "Unemployment Rate (%, 2011–2014 avg)", y = "Profit Margin", color = NULL) +
      theme_minimal(base_size = 11) + theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })

  output$popSalesPlot <- renderPlotly({
    d <- state_econ
    if (nrow(d) == 0 || all(is.na(d$Population))) return(no_data_plot())
    p <- ggplot(d, aes(x = Population / 1e6, y = Total_Sales,
                       text = paste0(State, "<br>Population: ", round(Population / 1e6, 1), "M",
                                     "<br>Sales: ", dollar(round(Total_Sales))))) +
      geom_point(aes(color = Profit_Margin, size = Total_Sales), alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "#8e44ad", linewidth = 1) +
      scale_x_continuous(labels = function(x) paste0(x, "M")) +
      scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
      scale_color_gradient(low = "#e74c3c", high = "#27ae60") +
      scale_size(range = c(2, 10), guide = "none") +
      labs(x = "State Population (millions, 2011–2014 avg)", y = "Total Retail Sales (USD)", color = "Profit Margin") +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = "text")
  })

  # ── Data Table ───────────────────────────────────────────────────────────────
  output$dataTable <- renderDT({
    d <- filtered()
    if (nrow(d) == 0) return(datatable(data.frame(Message = "No data for current filter selection")))
    datatable(
      d %>% select(Order.Date, Region, State, Category, Sub.Category, Segment,
                   Ship.Mode, Sales, Profit, Discount, Profit_Margin),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
