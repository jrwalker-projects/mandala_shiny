library(shiny); library(viridis); library(dplyr); library(magick); library(shinyWidgets); library(markdown)
if(!require(MASS)){
  install.packages("MASS")
  library(MASS)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700'); 
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #45ADA8;
                    }
                    "), #app heading font
               type="text/css",
               "#mandala_image img {max-width: 100%; width: 100%; height: auto}" #lets image resize to window
               )
    ),
  headerPanel("Mandala Animate"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("iter",
                  "Iterations:",
                  min = 2,
                  max = 5,
                  value = 3),
      sliderInput("points", "Points:",
                  min = 3, max = 14, value = 6),
      sliderInput("radius", "Radius:",
                  min = 1.2, max = 5.0,
                  value = c(1.6, 3.6), step = 0.1),
      selectInput("palette", "Palette:",
                  c("Viridis" = "viridis",
                    "Topo" = "topo",
                    "Sugar" = "sugar",
                    "Spectral" = "spectral",
                    "Pancake" = "pancake",
                    "Lightopia" = "lightopia")),
      checkboxGroupButtons(
        inputId = "color", label = "Color:", 
        choices = c("Area", "Gradient"), 
        justified = TRUE, status = "primary", selected = "Gradient",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId = "options", label = "Options:", 
        choices = c("Reverse", "Morph"), 
        justified = TRUE, status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      ),
      
      selectInput("fps", "Frames / Second:",
                  c("2.5" = "2.5",
                    "1" = "1",
                    "2" = "2",
                    "5" = "5",
                    "10" = "10")),
      actionButton("draw", "Draw!", icon = icon("recycle"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Image", imageOutput("mandala_image"),
                           textOutput("display_text")),
                  tabPanel("About", 
                           withMathJax(),
                           includeMarkdown("about.md"))
      )
    )
  )
)
build_palette <- function(palette_choice, reverse){
  
  my_palette <- case_when(palette_choice == "viridis" ~ viridis(5),
                   palette_choice == "topo" ~ topo.colors(5),
                   palette_choice == "sugar" ~ c("#FE4365","#FC9D9A","#F9CDAD","#C8C8A9","#83AF9B"),
                   palette_choice == "pancake" ~ c("#594F4F","#547980","#45ADA8","#9DE0AD","#E5FCC2"),
                   palette_choice == 'spectral' ~ c("#1ed2f4","#f4f5f6","#eafc40","#254558","#2b2b3a"),
                   palette_choice == 'lightopia' ~ c("#e1e6f9","#f7e1f5","#e1dbf5","#e8f1f0","#d7dfe9"))
  if(reverse){my_palette <- rev(my_palette)}
  return(my_palette)
}
source(file="mandala.R")

server <- function(input, output, session) {

  new_mandala <- eventReactive(input$draw, {
    outfile <- "ani.gif"
    distance_color <- !("Area" %in% input$color)
    apply_gradient <- ("Gradient" %in% input$color)
    morphing <- ("Morph" %in% input$options)
    reverse_palette <- ("Reverse" %in% input$options)
    my_mandala <- mandala_ani(iter = input$iter, points = input$points, radius = input$radius,
                              palette = build_palette(input$palette, reverse_palette), border_poly = FALSE,
                              distance_colouring = distance_color, 
                              gradient_colouring = apply_gradient,
                              border = "black",
                              path = "ani",
                              fps = as.numeric(input$fps),
                              remove_previous = TRUE,
                              morph = morphing,
                              my_filename = outfile)
  }, ignoreNULL = FALSE)
  
  output$mandala_image <- renderImage({
    new_mandala()
    
    list(src = file.path("ani", "ani.gif"),
         contentType = "image/gif",
         width = session$clientData$output_image1_width,
         height = session$clientData$output_image1_height,
         alt = "mandala animated")
  }, deleteFile = TRUE)
  output$display_text <- renderText({
    ""
  })         
}
# Run the application 
shinyApp(ui = ui, server = server)
