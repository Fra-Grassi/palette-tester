# ---- Palette Tester ----
#
# Author: Francesco Grassi
# GitHub: Fra-Gra
#
# A little shiny app to test color palettes.
# The app allows users to input colors in hex format and have a quick visualization of how the colors
# appear in different plot types.

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  
  titlePanel("Color Palette Tester"),
  
  hr(),  # horizontal rule
  
  # Row with three plots:
  fluidRow(
    column(
      width = 4,
      # Tabs to switch from manual to Coolors url input
      tabsetPanel(id = "input_source",
        tabPanel(
          "Manual",
          textInput("color1", "Color 1 (hex):", value = ""),
          textInput("color2", "Color 2 (hex):", value = ""),
          textInput("color3", "Color 3 (hex):", value = ""),
          textInput("color4", "Color 4 (hex):", value = ""),
          textInput("color5", "Color 5 (hex):", value = "")
        ),
        tabPanel(
          "Coolors.co",
          textInput(
           "colorUrl", 
           "Coolor.co palette url:",
           value = "https://coolors.co/palette/264653-2a9d8f-e9c46a-f4a261-e76f51"
          )
        )
      )
      
    ), 
    column(
      width = 6,
      offset = 2,
      plotOutput("barplot")
      )
  ),
  
  # Row with input boxes:
  fluidRow(
    column(
      width = 6,
      plotOutput("lineplot")
      ),
    column(
      width = 6,
      plotOutput("densityplot")
      )
  )
)

# Define server
server <- function(input, output) {
  
  # Deal with color input --------
  
  # Reactive expression to define colors based on input:
  color_palette <- reactive({
    
    # If "Manual" tab is selected, check how many colors are provided:
    if (input$input_source == "Manual"){
      # If no color is provided, assign 5 default colors:
      if(!isTruthy(c(input$color1, input$color2, input$color3, input$color4))){  
        color_palette <- brewer.pal(n = 5, name = "Dark2")
      } else {
        # Otherwise use only valid color inputs:
        color_palette <- c(input$color1, input$color2, input$color3, input$color4)
        color_palette <- color_palette[color_palette != ""]
      }
    } else{
      # If "Coolors" tab is selected, extract colors from url:
      color_palette <- paste0(
        "#",  # add "#"...
        str_split(
          str_remove(input$colorUrl, "https://coolors.co/palette/"),  # ... after removing unused url bit...
          "-"  # ... and splitting the rest into a vector
        )[[1]]
      )
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
