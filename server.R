if (!("tidyverse" %in% names(installed.packages()[,"Package"]))) {install.packages("tidyverse")}
suppressMessages(library(tidyverse, quietly = TRUE))

library(readxl)
library(readr)
library(stringr)
library(forcats)

if (!("plotly" %in% names(installed.packages()[,"Package"]))) {install.packages("plotly")}
suppressMessages(library(plotly, quietly = TRUE))

if (!("DT" %in% names(installed.packages()[,"Package"]))) {install.packages("DT")}
suppressMessages(library(DT, quietly = TRUE))

if (!("shiny" %in% names(installed.packages()[,"Package"]))) {install.packages("shiny")}
suppressMessages(library(shiny, quietly = TRUE))

if (!("ggthemes" %in% names(installed.packages()[,"Package"]))) {install.packages("ggthemes")}
suppressMessages(library(ggthemes, quietly = TRUE))

if (!("ggrepel" %in% names(installed.packages()[,"Package"]))) {install.packages("ggrepel")}
suppressMessages(library(ggrepel, quietly = TRUE))

#source("data_prep.R")
source("file_read.R")

shinyServer(
  
  function(input, output, session){
    
    # First tab
    output$thesload_plot <- renderPlotly({
      start_year <- input$range_yrs[1]
      end_year <- input$range_yrs[2]
      if(input$HUM){# & input$y_var == "Thesis Load/FTE"){
        #start_year <- 2007
        #end_year <- 2014
        joined_total %>% 
          filter(year >= start_year, year <= end_year) %>%
          filter(Department %in% input$depts) %>%
          rename("Thesis Load per FTE" = thes_load_perFTE) %>%
          rename("Year" = year) %>% 
          ggplot(aes(x = Year, y = `Thesis Load per FTE`)) +
          geom_line(aes(color = Department)) +
          theme(legend.title = element_blank()) +
          coord_cartesian(ylim = c(0, 8)) +
          theme_minimal()
      }
      else {#if(!input$HUM)# & input$y_var == "Thesis Load/FTE"){
        joined_noHUM %>% 
          filter(year >= start_year, year <= end_year) %>%
          filter(Department %in% input$depts) %>%
          rename("Thesis Load per FTE" = thes_load_perFTE) %>%
          rename("Year" = year) %>%          
          ggplot(aes(x = Year, y = `Thesis Load per FTE`)) +
          geom_line(aes(color = Department)) +
          theme(legend.title = element_blank()) +
          coord_cartesian(ylim = c(0, 8)) +
          theme_minimal()
      }
    })
    
    output$units_plot <- renderPlotly({
      start_year <- input$range_yrs[1]
      end_year <- input$range_yrs[2]
      if(input$HUM){
        joined_total_units %>% 
          filter(year >= start_year, year <= end_year) %>%
          filter(Department %in% input$depts) %>%
          rename("Total Units per FTE" = Units_perFTE) %>%
          rename("Year" = year) %>%          
          ggplot(aes(x = Year, y = `Total Units per FTE`)) +
          geom_line(aes(color = Department)) +
          theme(legend.title = element_blank()) +
          coord_cartesian(ylim = c(20, 160)) + 
          theme_minimal()
      }
      else { #if(!input$HUM)# & input$y_var == "Thesis Load/FTE"){
        joined_noHUM_units %>% 
          filter(year >= start_year, year <= end_year) %>%
          filter(Department %in% input$depts) %>%
          rename("Total Units per FTE" = Units_perFTE) %>%
          rename("Year" = year) %>%          
          ggplot(aes(x = Year, y = `Total Units per FTE`)) +
          geom_line(aes(color = Department)) +
          theme(legend.title = element_blank()) +
          coord_cartesian(ylim = c(20, 160)) + 
          theme_minimal()
      }
    })
    
    # Second tab
    output$scatter <- renderPlot({
      start_year <- input$range_yrs2[1]
      end_year <- input$range_yrs2[2]
      if(input$center == "Mean" & input$HUM2){
        join_for_scatter %>% 
          filter(year >= start_year, year <= end_year) %>%        
          group_by(Department, Division) %>% 
          summarize(
            `Mean Thesis Load per FTE` = mean(thes_load_perFTE),
            `Mean Student Units per FTE` = mean(Units_perFTE)
          ) %>%
          ggplot(aes(x = `Mean Student Units per FTE`, y = `Mean Thesis Load per FTE`)) +
          geom_point(size = 2, aes(color = Division))  +
          geom_text_repel(mapping = aes(label = Department), size = 5) +
          coord_cartesian(xlim = c(35, 135), ylim = c(0, 5)) + 
          theme_minimal()
      }
      else if(input$center == "Median" & input$HUM2){
        join_for_scatter %>% 
          filter(year >= start_year, year <= end_year) %>%        
          group_by(Department, Division) %>% 
          summarize(
            `Median Thesis Load per FTE` = median(thes_load_perFTE),
            `Median Student Units per FTE` = median(Units_perFTE)
          ) %>%
          ggplot(aes(x = `Median Student Units per FTE`, y = `Median Thesis Load per FTE`)) +
          geom_point(size = 2, aes(color = Division))  +
          geom_text_repel(mapping = aes(label = Department), size = 5) +
          coord_cartesian(xlim = c(35, 135), ylim = c(0, 5)) + 
          theme_minimal()
      }
      else if(input$center == "Mean" & !input$HUM2){
        join_for_scatter_noHUM %>% 
          filter(year >= start_year, year <= end_year) %>%        
          group_by(Department, Division) %>% 
          summarize(
            `Mean Thesis Load per FTE` = mean(thes_load_perFTE),
            `Mean Student Units per FTE` = mean(Units_perFTE)
          ) %>%
          ggplot(aes(x = `Mean Student Units per FTE`, y = `Mean Thesis Load per FTE`)) +
          geom_point(size = 2, aes(color = Division))  +
          geom_text_repel(mapping = aes(label = Department), size = 5) +
          coord_cartesian(xlim = c(35, 135), ylim = c(0, 5)) + 
          theme_minimal()
      }
      else {
        join_for_scatter_noHUM %>% 
          filter(year >= start_year, year <= end_year) %>%        
          group_by(Department, Division) %>% 
          summarize(
            `Median Thesis Load per FTE` = median(thes_load_perFTE),
            `Median Student Units per FTE` = median(Units_perFTE)
          ) %>%
          ggplot(aes(x = `Median Student Units per FTE`, y = `Median Thesis Load per FTE`)) +
          geom_point(size = 2, aes(color = Division))  +
          geom_text_repel(mapping = aes(label = Department), size = 5) +
          coord_cartesian(xlim = c(35, 135), ylim = c(0, 5)) + 
          theme_minimal()
      }
    })
    
    output$scatter_data <- DT::renderDataTable(
      if(input$center == "Mean" & input$HUM2 & input$data_view){
        datatable(join_for_scatter %>% 
            filter(year >= input$range_yrs2[1], year <= input$range_yrs2[2]) %>%
            group_by(Department, Division) %>%
            summarize(
              `Mean Thesis Load per FTE` = mean(thes_load_perFTE),
              `Mean Student Units per FTE` = mean(Units_perFTE)
            ) %>% 
            round_df(digits = 2),
          rownames = FALSE,
          options = list(pageLength = nrow(join_for_scatter_noHUM),
            dom = 'ft')
        )
      }
      else if(input$center == "Median" & input$HUM2 & input$data_view){
        datatable(join_for_scatter %>%
            filter(year >= input$range_yrs2[1], year <= input$range_yrs2[2]) %>%
            group_by(Department, Division) %>%
            summarize(
              `Median Thesis Load per FTE` = median(thes_load_perFTE),
              `Median Student Units per FTE` = median(Units_perFTE)
            ) %>% 
            round_df(digits = 2),
          rownames = FALSE,
          options = list(pageLength = nrow(join_for_scatter_noHUM),
            dom = 'ft')
        )
      }
      else if(input$center == "Mean" & !input$HUM2 & input$data_view){
        datatable(join_for_scatter_noHUM %>%
            filter(year >= input$range_yrs2[1], year <= input$range_yrs2[2]) %>%
            group_by(Department, Division) %>%
            summarize(
              `Mean Thesis Load per FTE` = mean(thes_load_perFTE),
              `Mean Student Units per FTE` = mean(Units_perFTE)
              ) %>% 
            round_df(digits = 2),
          rownames = FALSE,
          options = list(pageLength = nrow(join_for_scatter_noHUM),
            dom = 'ft')
        )
      }
    else if(input$center == "Median" & !input$HUM2 & input$data_view){
        datatable(join_for_scatter_noHUM %>%
            filter(year >= input$range_yrs2[1], year <= input$range_yrs2[2]) %>%
            group_by(Department, Division) %>%
            summarize(
              `Median Thesis Load per FTE` = median(thes_load_perFTE),
              `Median Student Units per FTE` = median(Units_perFTE)
            ) %>% 
            round_df(digits = 2),
          rownames = FALSE,
          options = list(pageLength = nrow(join_for_scatter_noHUM),
            dom = 'ft')
          )
      }
    )
    
    # Third tab
    output$intro_sci_plot <- renderPlotly({
      start_year <- input$range_yrs3[1]
      end_year <- input$range_yrs3[2]
      #intro_sci <- 
        d %>% 
        filter(year >= start_year, year <= end_year) %>%
        group_by(Subj) %>%
        filter(courseid %in% input$courses) %>%
        summarize(interest_enr = sum(Census_enrlment)) %>%
        ggplot(aes(Subj, interest_enr)) +
        geom_bar(stat = "identity")  +
        ggtitle("Interest + Enrollment by Subject") +
        xlab("Subject") +
        ylab("Interest + Enrollment") +
        theme_minimal()
      #ggplotly(intro_sci)
    })
    
    output$byFTE <- renderPlotly({
      start_year <- input$range_yrs3[1]
      end_year <- input$range_yrs3[2]
      if(input$HUM){
       # FTE1 <- 
          d %>%
          filter(year >= start_year, year <= end_year) %>%
          group_by(Department, SchoolYear) %>%
          filter(courseid %in% input$courses) %>%
          summarize(interest_enr_by_year = sum(Census_enrlment)) %>% 
          inner_join(total_FTE, by = c("Department", "SchoolYear")) %>% 
          group_by(Department) %>% 
          summarize(interest_enr = sum(interest_enr_by_year),
            interest_FTE = sum(FTE)) %>% 
          mutate(perFTE = interest_enr / interest_FTE) %>% 
          rename("(Interest + Enrollment) / FTE" = perFTE) %>% 
          mutate(Department = fct_rev(Department)) %>% 
          ggplot(aes(Department, `(Interest + Enrollment) / FTE`)) +
          geom_bar(stat = "identity") +
          ggtitle("(Interest + Enrollment)/FTE by Department") +
          xlab("") +
          coord_flip()  +
          theme_minimal()
       # ggplotly(FTE1)
      }
      else{
        #FTE2 <- 
          d %>%
          filter(year >= start_year, year <= end_year) %>%
          group_by(Department, SchoolYear) %>%
          filter(courseid %in% input$courses) %>%
          summarize(interest_enr_by_year = sum(Census_enrlment)) %>% 
          inner_join(DEPT_FTE, by = c("Department", "SchoolYear")) %>% 
          group_by(Department) %>% 
          summarize(interest_enr = sum(interest_enr_by_year),
            interest_FTE = sum(FTE)) %>% 
          mutate(perFTE = interest_enr / interest_FTE) %>% 
          ggplot(aes(fct_rev(Department), perFTE)) +
          geom_bar(stat = "identity") +
          xlab("") +
          coord_flip()
       # ggplotly(FTE2)
      }
    })
    
    output$test <- renderPlotly(
      qplot(x = Sepal.Length, y = Sepal.Width, data = iris)
    )
    
  })
