library(shiny)
if (!("shinythemes" %in% names(installed.packages()[,"Package"]))) {install.packages("shinythemes")}
suppressMessages(library(shiny, quietly = TRUE))

shinyUI(navbarPage("Data Dashboard",
  theme = shinytheme("paper"),
  # https://gallery.shinyapps.io/117-shinythemes/
  
  tabPanel("Trends",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "range_yrs",
          label = "Time span:",
          min = 2008,
          max = 2016,
          value = c(2012, 2016),
          sep = ""),
        hr(),
        checkboxInput("HUM", "Include HUM in total?", value = TRUE),
   #     checkboxInput("comb_lang", "Combine Languages?", value = FALSE),
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
          min = 2008,
          max = 2016,
          value = c(2012, 2016),
          sep = ""),
        hr(),
        selectizeInput(inputId = "center",
          label = "Select Mean or Median",
          choices = c("Mean", "Median"),
          selected = "Mean"),
        checkboxInput("HUM2", "Include HUM in total?", value = TRUE),
        checkboxInput("data_view", "View the data table", value = FALSE)
        ),
    mainPanel(
      br(),
      plotOutput("scatter"),
      hr(),
      DT::dataTableOutput("scatter_data")
      )
      )
  ),
  
  tabPanel("Intro Sci",
    
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "range_yrs3",
          label = "Time span:",
          min = 2008,
          max = 2016,
          value = c(2012, 2016),
          sep = ""),
        hr(),
        tags$div(class = "multicol",
        checkboxGroupInput("courses", "Courses to include:", 
          unique(d$courseid),
          selected = unique(d$courseid)
        )
        )
      ),
      
      mainPanel(
        br(),
        plotlyOutput("intro_sci_plot"),
        hr(),
        plotlyOutput("byFTE"))
      
    )
  )#,
  
  #tabPanel("Test", plotlyOutput("test"))
)
)