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
    # Tabs to switch from manual to Coolors url input
    column(
      width = 3,
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
    
    # Bar plot next to input
    column(
      width = 4,
      offset = 1,
      plotOutput("barplot")
      ),
    
    # Scatter plot next to bar plot
    column(
      width = 4,
      plotOutput("scatterplot")
      )
  ),
  
  fluidRow(
    # Line plot under bar plot
    column(
      width = 4,
      offset = 4,
      plotOutput("lineplot")
      ),
    
    # Density plot under scatter plot
    column(
      width = 4,
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
  
  # Scatter plot:
  df_scatter <- reactive({
    data.frame(
      x = rep(1:palette_length(), each = 10) + rnorm(10*palette_length()),
      y = rep(1:palette_length(), each = 10) + rnorm(10*palette_length()),
      col = factor(rep(1:palette_length(), each = 10))
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
      ggtitle("Bar Plot") +
      theme_void() +
      theme(
        plot.title = element_text(size = 30, hjust = 0.5)
      )
  })
  
  # Scatter plot:
  output$scatterplot <- renderPlot({
    ggplot(df_scatter(), aes(x, y, color = col)) + 
      geom_point(size = 3) +
      scale_color_manual(values = color_palette()) +
      ggtitle("Scatter Plot") +
      theme_void() +
      theme(
        plot.title = element_text(size = 30, hjust = 0.5),
          legend.position = "none"
      )
  })
  
  # Line plot:
  output$lineplot <- renderPlot({
    ggplot(df_line(), aes(x, y, color = col)) +
      geom_line(linewidth = 2) +
      scale_color_manual(values = color_palette()) +
      ggtitle("Line Plot") +
      theme_void() +
      theme(
        plot.title = element_text(size = 30, hjust = 0.5),
        legend.position = "none"
      )
  })
  
  # Density plot:
  output$densityplot <- renderPlot({
    ggplot(df_density(), aes(x, fill = col)) +
      geom_density(alpha = .40, color = NA) +
      scale_fill_manual(values = color_palette()) +
      ggtitle("Density Plot") +
      theme_void() +
      theme(
        plot.title = element_text(size = 30, hjust = 0.5),
        legend.position = "none"
      )
  })
}

# Run app
shinyApp(ui, server)
