library(shiny)
library(tidyverse)
library(stringr)
library(plotly)

shinyServer(
  
  function(input, output, session){
    
    output$intro_sci_plot <- renderPlotly({
      start_year <- input$range_yrs[1]
      end_year <- input$range_yrs[2]
      intro_sci <- d %>%
        filter(year >= start_year, year <= end_year) %>%
        group_by(Subj) %>%
        filter(courseid %in% input$courses) %>%
        summarize(interest_enr = sum(Census_enrlment)) %>%
        ggplot(aes(Subj, interest_enr)) +
        geom_bar(stat = "identity")
      ggplotly(intro_sci)
    })
    
    output$byFTE <- renderPlotly({
      start_year <- input$range_yrs[1]
      end_year <- input$range_yrs[2]
      if(input$HUM){
        FTE1 <- d %>%
          filter(year >= start_year, year <= end_year) %>%
          group_by(Department, SchoolYear) %>%
          filter(courseid %in% input$courses) %>%
          summarize(interest_enr_by_year = sum(Census_enrlment)) %>% 
          inner_join(total_FTE, by = c("Department", "SchoolYear")) %>% 
          group_by(Department) %>% 
          summarize(interest_enr = sum(interest_enr_by_year),
            interest_FTE = sum(FTE)) %>% 
          mutate(perFTE = interest_enr / interest_FTE) %>% 
          ggplot(aes(fct_rev(Department), perFTE)) +
          geom_bar(stat = "identity") +
          coord_flip()
        ggplotly(FTE1)
      }
      else{
        FTE2 <- d %>%
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
          coord_flip()
        ggplotly(FTE2)
      }
    })
    
    output$equity_plot <- renderPlotly({
      start_year <- input$range_yrs[1]
      end_year <- input$range_yrs[2]
      if(input$HUM & input$y_var == "Thesis Load/FTE"){
        #start_year <- 2007
        #end_year <- 2014
        #input$depts
        tl_plot <- thes_dept %>% 
          filter(year >= start_year, year <= end_year) %>%
          inner_join(yt_small, by = c("year" = "ThesisYear")) %>% 
          inner_join(total_FTE, 
            by = c("Department" = "DepartmentLong", "SchoolYear")) %>%
          mutate(thes_load_perFTE = advisees / FTE) %>% 
          filter(Department.y %in% input$depts) %>%
          ggplot(aes(x = year, y = thes_load_perFTE)) +
          geom_line(aes(color = Department.y))
        ggplotly(tl_plot)
      }
      else{
        # Placeholder
       ggplotly(iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
          geom_point(aes(color = Species)))
      }
    })
    
  })
