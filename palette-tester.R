library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# ---- Palette Tester ----
#
# Author: Francesco Grassi
# GitHub: Fra-Gra
#
# A little shiny app to test color palettes.
# The app allows users to input colors in hex format and have a quick visualization of how the colors
# appear in different plot types.

# Define UI
ui <- fluidPage(
  titlePanel("Color Palette Tester"),
  sidebarLayout(
    # Sidebar panel to input color values:
    sidebarPanel(
      textInput("color1", "Color 1 (hex):", value = ""),
      textInput("color2", "Color 2 (hex):", value = ""),
      textInput("color3", "Color 3 (hex):", value = ""),
      textInput("color4", "Color 4 (hex):", value = "")
    ),
    # Main panel to display plots:
    mainPanel(
      plotOutput("barplot"),
      plotOutput("lineplot"),
      plotOutput("densityplot")
    ),
    position = "right"  # sidebar panel position relative to main panel
  )
)

# Define server
server <- function(input, output) {
  
  # Deal with color input --------
  
  # Reactive expression to define colors based on input boxes:
  color_palette <- reactive({
    
    # If no color is provided, assign 4 default colors:
    if(!isTruthy(c(input$color1, input$color2, input$color3, input$color4))){  
      color_palette <- brewer.pal(n = 4, name = "Dark2")
    } else {
      # Otherwise use only valid color inputs:
      color_palette <- c(input$color1, input$color2, input$color3, input$color4)
      color_palette <- color_palette[color_palette != ""]
    }
  })
  
  # Reactive expression to get length of defined palette:
  palette_length <- reactive({
    length(color_palette())
  })
    
  # Create data.frames for plots --------
  
  # Bar plot:
  df_bar <- reactive({
    data.frame(
      x = 1:palette_length(), 
      y = sample(1:5, palette_length())
      )
  })
  
  # Line plot:
  df_line <- reactive({
    data.frame(
      x = rep(1:10, times = palette_length()),
      y = runif(10 * palette_length()),
      col = factor(rep(1:palette_length(), each = 10))
    )
  })
  
  # Density plot:
  df_density <- reactive({
    data.frame(
      x = rnorm(1000 * palette_length(), 1:palette_length(), 1:palette_length()),
      col = factor(rep(1:palette_length(), times = 1000))
    )
  })

  # Functions to create plots --------
  
  # Bar plot:
  output$barplot <- renderPlot({
    ggplot(df_bar(), aes(x, y)) + 
      geom_bar(stat = "identity", fill = color_palette()) +
      theme_minimal() +
      ggtitle("Bar Plot")
  })
  
  # Line plot:
  output$lineplot <- renderPlot({
    ggplot(df_line(), aes(x, y, color = col)) +
      geom_line() +
      scale_color_manual(values = color_palette()) +
      theme_minimal() +
      ggtitle("Line Plot")
  })
  
  # Density plot:
  output$densityplot <- renderPlot({
    ggplot(df_density(), aes(x, fill = col)) +
      geom_density(alpha = .40, color = NA) +
      scale_fill_manual(values = color_palette()) +
      theme_minimal() +
      ggtitle("Density Plot")
  })
}

# Run app
shinyApp(ui, server)
