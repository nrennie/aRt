# Define UI for random distribution app ----
ui <- fluidPage(
  theme = shinytheme("slate"),

  # App title ----
  titlePanel(
    h1("aRt: Generative Art in R", align = "center"),
    windowTitle = "aRt: Generative Art in R"
  ),

  hr(),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Add spacing
      br(),

      # Plot
      fluidRow(
        column(12, align="center",
               plotOutput("plot", height="auto")
               )
        ),

      # Add spacing
      br(),

      # Download button
      fluidRow(
        column(12, align="center",
               downloadButton('downloadPlot', 'Download')
        )
      ),

      # Edit width
      width=4

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs", id = "tabs",

                  #circles
                  tabPanel("Circles", id="circles",
                           br(),
                           numericInput("circles_n", "Number of circles", value = 10, min = 1, max = 100),
                           sliderInput("circles_smoothness", "Smoothness", min = 3, max = 100, value = 50),
                           selectInput("circles_col_palette", "Colour palette",
                                       c("Bold" = "Bold",
                                         "Antique" = "Antique",
                                         "Vivid" = "Vivid",
                                         "Safe"="Safe",
                                         "Prism"="Prism",
                                         "Pastel"="Pastel")),
                           selectInput("circles_bg_col", "Background colour",
                                       c("Pink" = "#e73f74",
                                         "White" = "white",
                                         "Black" = "black"))
                  ),

                  #fading
                  tabPanel("Fading", id="fading",
                           br(),
                           sliderInput("fading_n_layers", "Number of layers", min = 2, max = 10, value = 6),
                           sliderInput("fading_n_points", "Number of points", min = 1, max = 10, value = 1),
                           selectInput("fading_col_palette", "Colour palette",
                                       c("Sunset" = "Sunset",
                                         "Dark Sunset" = "SunsetDark",
                                         "Teal" = "Teal",
                                         "Peach"="Peach"))
                  ),

                  #bullseye
                  tabPanel("Bullseye", id="bullseye",
                           br(),
                           selectInput("bullseye_main_col", "Main colour",
                                       c("Black" = "black",
                                         "White" = "white",
                                         "Red" = "#9E1A1A")),
                           selectInput("bullseye_bg_col", "Background colour",
                                       c("White" = "white",
                                         "Black" = "black",
                                         "Grey" = "grey80"))
                  ),

                  #vortex
                  tabPanel("Vortex", id="vortex",
                           br(),
                           sliderInput("vortex_n", "Number of points", min = 1, max = 100, value = 25),
                           selectInput("vortex_start_val", "Starting value",
                                       c("0" = 0,
                                         "90" = 90)),
                           selectInput("vortex_col_scheme", "Colour scheme",
                                       c("Rainbow" = "rainbow",
                                         "Monochrome" = "mono")),
                           selectInput("vortex_bg_col", "Background colour",
                                       c("Black" = "black",
                                         "White" = "white",
                                         "Yellow" = "#edad08"))
                  ),

                  #waves
                  tabPanel("Waves", id="waves",
                           br(),
                           numericInput("waves_a", "Parameter 1", value = 23, min = 1, max = 25),
                           numericInput("waves_b", "Parameter 2", value = 6, min = 1, max = 25),
                           selectInput("waves_main_col", "Main colour",
                                       c("Black" = "black",
                                         "White" = "white",
                                         "Prism" = "Prism",
                                         "Bold" = "Bold",
                                         "Antique" = "Antique",
                                         "Vivid" = "Vivid",
                                         "Safe"="Safe",
                                         "Pastel"="Pastel")),
                           selectInput("waves_bg_col", "Background colour",
                                       c("White" = "white",
                                         "Black" = "black",
                                         "Yellow" = "#edad08"))
                  )
            ),
      width = 8
    )
  ),
  fluidRow(column(12, align="center", uiOutput("gh_link"))),
  fluidRow(column(12, align="center", textOutput("footer")))
)
