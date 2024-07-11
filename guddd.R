library(tidyverse)
library(ggplot2)
library(ggthemes)
library(shiny)
library(plotly)
library(shinydashboard)
library(DT)
library(scales)
library(kableExtra )
library(dplyr)
# Add this line to load the scales package

# Merging datasets
fulldf <- merge(merge(merge(dfgdp, dfs, by = c("Country", "Year"), all = TRUE), 
                      dfe, by = c("Country", "Year"), all = TRUE),
                dfgen, by = c("Country", "Year"), all = TRUE)

# Formatting GDP and filtering data
fulldf$GDP <- format(fulldf$GDP, scientific = FALSE)
fulldf <- fulldf %>% filter(Year >= 2000 & Year <= 2022) %>%
  arrange(desc(Year))
fulldf$GDP <- as.numeric(fulldf$GDP)
fulldf$Year <- as.numeric(fulldf$Year)
# Merge chart_gdp and chart_gen
chartdf <- merge(chart_gdp, chart_gen, by = c("Country", "Year", "Region"))

# Merge chart_seat to the merged_df
chartdf <- merge(chartdf, chart_seat, by = c("Country", "Year", "Region"))

# Merge chart_em to the merged_df
chartdf <- merge(chartdf, chart_em, by = c("Country", "Year", "Region"))

chartdf$GDP <- format(chartdf$GDP, scientific = FALSE)
chartdf <- subset(chartdf, Year >= 2000 & Year <= 2022) %>%
  arrange(desc(Year))
chartdf$GDP <- as.numeric(chartdf$GDP)



# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "SOFA Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-bar")),
      menuItem("Bubble charts", tabName = "bubble_charts", icon = tags$i(class = "fas fa-balloons"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                column(width = 3,
                       selectInput("year", "Chọn năm:", choices = rev(sort(unique(fulldf$Year))), selected = max(fulldf$Year))
                ),
                column(width = 3,
                       selectInput("variable", "Chọn biến quan sát:", choices = c("GDP", "Seat", "Employment", "Gender"), selected = "GDP")
                )
              ),
              fluidRow(
                column(width = 8,
                       plotlyOutput("map", height = "600px")
                ),
                column(width = 4,
                       plotlyOutput("h5bar", height = "300px")
                ),
                column(width = 4,
                       plotlyOutput("l5bar", height = "300px")
                )
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                column(width = 12,
                       selectInput("year", "Chọn năm:", choices = rev(sort(unique(fulldf$Year))), selected = max(fulldf$Year)),
                       plotlyOutput("table"),
                       tableOutput("summaryTable")
                )
              )
      ),
      tabItem(tabName = "charts",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    # Select variable for y-axis
                    selectInput(
                      inputId = "y",
                      label = "Y-axis:",
                      choices = c("Seat", "GDP", "Employment"),
                      selected = "Seat"
                    ),
                    # Select variable for x-axis
                    selectInput(
                      inputId = "x",
                      label = "X-axis:",
                      choices = c("Seat", "GDP", "Employment"),
                      selected = "GDP"
                    ),
                    # Select range of years
                    sliderInput(
                      inputId = "year_range",
                      label = "Year Range:",
                      min = 2000,
                      max = 2022,
                      value = c(2000, 2022),
                      step = 1
                    )
                  ),
                  mainPanel(
                    # Output: Show scatterplot
                    plotOutput(outputId = "scatterplot", width = "100%", height = "700px")
                  )
                )
              )
      ),
      tabItem(tabName = "bubble_charts",
              fluidPage(
                fluidRow(
                  column(width = 3,
                         selectInput("year", "Chọn năm:", choices = rev(sort(unique(fulldf$Year))), selected = max(fulldf$Year))
                  )
                ),
                plotly::plotlyOutput(outputId = "bubbleCharts", width = "100%", height = "700px")
              )
      )
                
              )
      )
    )


# Define server
server <- function(input, output) {
  # Map output
  output$map <- renderPlotly({
    filtered_data <- fulldf %>% filter(Year == input$year, !is.na(get(input$variable)))
    p <- plot_ly(filtered_data, z = ~get(input$variable), text = ~paste("Country: ", Country, "Value:", ~get(input$variable)),
                 locations = ~Country, type = "choropleth", locationmode = "country names") %>%
      layout(title = list(text = paste("World Map of", input$variable, "in", input$year), font = list(size = 18)))
    p <- p %>% colorbar(title = paste("Giá trị", input$variable))
    p
  })
  
  output$h5bar <- renderPlotly({
    filtered_data <- fulldf %>%
      filter(Year == input$year, !is.na(.data[[input$variable]]))
    sorted_data <- filtered_data %>%
      arrange(desc(.data[[input$variable]]))
    # Chọn top 5 nước
    top_5 <- head(sorted_data, 5)
    # Vẽ biểu đồ cột ngang
    ggplot(top_5, aes(x = Country, y = .data[[input$variable]])) +
      geom_col(fill = "skyblue") +
      labs(
        title = paste("5 Countries with Highest", input$variable, "in", input$year),
        x = "Country",
        y = input$variable
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 10, hjust = 1),
        plot.title = element_text(hjust = 0, size = 10)
      )
  })
  
  # Low5 output
  output$l5bar <- renderPlotly({
    filtered_data <- fulldf %>%
      filter(Year == input$year, !is.na(.data[[input$variable]]))
    sorted_data <- filtered_data %>%
      arrange(.data[[input$variable]])
    # Chọn low 5 nước
    low_5 <- head(sorted_data, 5)
    # Vẽ biểu đồ cột ngang
    ggplot(low_5, aes(x = Country, y = .data[[input$variable]])) +
      geom_col(fill = "skyblue") +
      labs(
        title = paste("5 Countries with Lowest", input$variable, "in", input$year),
        x = "Country",
        y = input$variable
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 10, hjust = 1),
        plot.title = element_text(hjust = 0, size = 10)
      )
  })
  output$table <- renderPlotly({
    filtered_data <- fulldf %>% filter(Year == input$year)
    
    header_values <- c("<b>Country</b>", "<b>Year</b>", "<b>GDP</b>", "<b>Seat</b>", "<b>Employment</b>", "<b>Gender</b>")  # Customize the header values
    
    data_values <- as.matrix(filtered_data[, c("Country", "Year", "GDP", "Seat", "Employment", "Gender")])  # Convert to matrix
    
    data_values <- t(data_values)  # Transpose the matrix
    
    plot_ly(type = 'table') %>%
      add_trace(
        header = list(
          values = header_values,
          align = c('left', rep('center', length(header_values) - 1)),
          line = list(width = 1, color = 'black'),
          fill = list(color = 'rgb(235, 100, 230)'),
          font = list(family = "Arial", size = 14, color = "white")
        ),
        cells = list(
          values = data_values,
          align = c('left', rep('center', length(header_values) - 1)),
          line = list(color = "black", width = 1),
          fill = list(color = c('rgb(235, 193, 238)', 'rgba(228, 222, 249, 0.65)')),
          font = list(family = "Arial", size = 12, color = c("black"))
        )
      )
  })
  
  output$summaryTable <- renderUI({
    filtered_data <- fulldf %>% filter(Year == input$year)
    summary_data <- data.frame(
      Variable = c("GDP", "Seat", "Employment","Gender"),
      Minimum = apply(filtered_data[, c("GDP", "Seat", "Employment","Gender")], 2, function(x) min(x, na.rm = TRUE)),
      Maximum = apply(filtered_data[, c("GDP", "Seat", "Employment","Gender")], 2, function(x) max(x, na.rm = TRUE)),
      Mean = apply(filtered_data[, c("GDP", "Seat", "Employment","Gender")], 2, function(x) mean(x, na.rm = TRUE)),
      Quartile_25 = apply(filtered_data[, c("GDP", "Seat", "Employment","Gender")], 2, function(x) quantile(x, probs = 0.25, na.rm = TRUE)),
      Quartile_50 = apply(filtered_data[, c("GDP", "Seat", "Employment","Gender")], 2, function(x) quantile(x, probs = 0.5, na.rm = TRUE)),
      Quartile_75 = apply(filtered_data[, c("GDP", "Seat", "Employment","Gender")], 2, function(x) quantile(x, probs = 0.75, na.rm = TRUE))
    )
    colnames(summary_data)[1] <- "Variable"
    
    # Format the numbers in the summary_data dataframe
    summary_data[, -1] <- lapply(summary_data[, -1], function(x) format(x, scientific = FALSE))
    
    # Decorate the table using kable and kableExtra
    table_html <- kable(summary_data, "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
      add_header_above(c("Summary Table" = 8)) %>%
      add_footnote("Note: This table provides summary statistics for the selected year.")
    
    # Return the table HTML
    HTML(table_html)
  })
  
  output$scatterplot <- renderPlot({
    chartdf %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>%
      ggplot(aes_string(x = input$x, y = input$y)) +
      geom_point(aes(size = Gender / 1e6, color = Region), alpha = 0.5) +
      geom_smooth(method = "auto") +
      scale_x_continuous(labels = scales::comma) +  # Fix: Use scales::comma
      labs(title = paste(input$x, "&", input$y, "of Nations for", input$year_range[1], "-", input$year_range[2])) +
      labs(x = input$x, y = input$y) +
      labs(color = "Continent", size = "Gender (M)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      facet_grid(Year ~ .)
  })
  
  output$bubbleCharts <- renderPlotly({
    slope <- 2.666051223553066e-05
    colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')
    chartdf$size <- sqrt(chartdf$Gender * slope)
    chartdf1 <- chartdf %>% filter(Year == input$year, !is.na(get(input$variable)))
    fig <- plot_ly(chartdf1, x = ~Seat, y = ~GDP, color = ~Region, size = ~size, colors = colors,
                   type = 'scatter', mode = 'markers', sizes = c(min(chartdf1$size), max(chartdf1$size)),
                   marker = list(symbol = 'circle', sizemode = 'diameter',
                                 line = list(width = 2, color = '#FFFFFF')),
                   text = ~paste('Country:', Country, '<br>Employment:', Employment, '<br>GDP:', GDP,
                                 '<br>Gender:', Gender))
    fig <- fig %>% layout(title = 'Employment v. GDP',
                          xaxis = list(title = 'GDP',
                                       gridcolor = 'rgb(255, 255, 255)',
                                       #range = c(2.003297660701705, 5.191505530708712),
                                       type = 'log',
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwidth = 2),
                          yaxis = list(title = 'Employment',
                                       gridcolor = 'rgb(255, 255, 255)',
                                       #range = c(36.12621671352166, 91.72921793264332),
                                       zerolinewidth = 1,
                                       ticklen = 5,
                                       gridwith = 2),
                          paper_bgcolor = 'rgb(243, 243, 243)',
                          plot_bgcolor = 'rgb(243, 243, 243)')
    
    fig
  })
}


# Run the application
shinyApp(ui = ui, server = server)
