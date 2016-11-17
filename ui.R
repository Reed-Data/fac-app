library(shiny)
library(shinythemes)

shinyUI(navbarPage("Reed College Faculty Load",
  theme = shinytheme("paper"),
  # https://gallery.shinyapps.io/117-shinythemes/
  
  tabPanel("Equity",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "range_yrs",
          label = "Time span:",
          min = 2007,
          max = 2015,
          value = c(2012, 2015),
          sep = ""),
        hr(),
        checkboxInput("HUM", "Include HUM in total?", value = TRUE),
        checkboxInput("comb_lang", "Combine Languages?", value = FALSE),
        hr(),
        p("Departments to include:"),
        tags$head(
          # http://www.w3schools.com/cssref/css3_pr_column-count.asp
          tags$style(HTML("
            .multicol {
              -webkit-column-count: 2; /* Chrome, Safari, Opera */
              -moz-column-count: 2; /* Firefox */
              column-count: 2;

              -webkit-column-gap: 2px; /* Chrome, Safari, Opera */
              -moz-column-gap: 2px; /* Firefox */
              column-gap: 2px;
            }
            "))
          ),
        tags$div(class = "multicol",
          checkboxGroupInput("depts", NULL,
            unique(joined_total$Department),
            selected = c("Biology", "Chemistry", "Mathematics", "Physics", "Psychology")
          )
        )
      ),
      mainPanel(
        plotlyOutput("thesload_plot"),
        hr(),
        plotlyOutput("units_plot")
      )
    )
  ),
  
  tabPanel("Scatterplot",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "range_yrs2",
          label = "Time span:",
          min = 2007,
          max = 2015,
          value = c(2012, 2015),
          sep = ""),
        hr(),
        selectizeInput(inputId = "center",
          label = "Select Mean or Median",
          choices = c("Mean", "Median"),
          selected = "Mean"),
        checkboxInput("HUM2", "Include HUM in total?", value = TRUE)
        ),
    mainPanel(
      br(),
      plotOutput("scatter"))
      )
  ),
  
  tabPanel("Intro Sci",
    
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "range_yrs3",
          label = "Time span:",
          min = 2007,
          max = 2015,
          value = c(2012, 2015),
          sep = ""),
        hr(),
        checkboxGroupInput("courses", "Courses to include:", 
          unique(d$courseid),
          selected = unique(d$courseid)
        )
      ),
      
      mainPanel(
        br(),
        plotlyOutput("intro_sci_plot"),
        hr(),
        plotlyOutput("byFTE"))
      
    )
  )
)
)