# Stats
stats_sep <- read_delim("data/Schedule-Data-Fall-2007-Fall-2017_20161011_114934.txt", delim = "|") %>%
  mutate(year = str_sub(Semester, -4),
         courseid = paste(Subj, Numb, sep = " ")) %>%
  mutate(stats = ifelse(courseid %in% c("MATH 141", "MATH 241", "MATH 243", "MATH 392", "MATH 391"),
                        yes = "yes",
                        no = "no")) %>% 
  filter(year >= 2013, Semester != "Spring 2013") %>% 
  filter(stats == "yes") %>% 
  filter(!grepl("L", Sect)) %>% 
  filter(!(Semester == "Fall 2015" & courseid == "MATH 391"))
#write_csv(stats_sep, "stats_sep.csv")

stats_sep_read <- read_csv("stats_sep.csv") %>% 
  inner_join(yt) %>% 
  mutate(Department = "Mathematics")

stats_to_add <- stats_sep_read  %>% 
  group_by(SchoolYear) %>% 
  summarize(FTE = 0.2 * n()) %>%
  mutate(Department = "Statistics") %>% 
  mutate(DepartmentLong = "Statistics Faculty in Mathematics") %>% 
  select(Department, SchoolYear, FTE, DepartmentLong) 
stats_to_add[1, 2] <- "2012-13"
stats_to_add[2, 2] <- "2013-14"
stats_to_add[3, 2] <- "2014-15"
stats_to_add[4, 2] <- "2015-16"
stats_to_add[4, 3] <- 1.4
stats2 <- stats_to_add %>% mutate(FTE = 2)

total_FTE_wStats2 <- rbind(total_FTE, stats2)
total_FTE_wStats <- rbind(total_FTE, stats_to_add)

stats_thes <- thes_fac %>% 
  filter(year >= 2014, department == "math") %>% 
  filter(faculty %in% c("Jones, Albyn", "Kim, Albert", "Bray, Andrew")) %>% 
  filter(advisees != 0)

stats_thes_dept <- stats_thes  %>% group_by(year)  %>% 
  summarize(advisees = sum(advisees)) %>% 
  add_row(year = 2017, advisees = 11) %>% 
  mutate(Department = "Statistics Faculty in Mathematics") %>% 
  select(Department, year, advisees) %>% 
  mutate(year = year - 1)

joined_total_s <- thes_dept %>% 
  bind_rows(stats_thes_dept) %>% 
  inner_join(yt_small, by = c("year" = "ThesisYear")) %>% 
  inner_join(total_FTE_wStats2, 
             by = c("Department" = "DepartmentLong", "SchoolYear")) %>%
  mutate(thes_load_perFTE = advisees / FTE) %>% 
  rename("DepartmentLong" = Department,
         "Department" = Department.y)

stats_sep2 <- stats_sep_read %>% 
  mutate(unit_temp = EndEnrollment + WaitlistCount) %>% 
  group_by(SchoolYear) %>% 
  summarize(Units = sum(unit_temp)) %>% 
  mutate(Department = "Statistics") %>% 
  mutate(DepartmentLong = "Statistics Faculty in Mathematics") %>%
  select(Department, SchoolYear, Units, DepartmentLong)
stats_sep2[1, 2] <- "2012-13"
stats_sep2[2, 2] <- "2013-14"
stats_sep2[3, 2] <- "2014-15"
stats_sep2[4, 2] <- "2015-16"

stats_sep3 <- stats_sep_read %>% 
  mutate(unit_temp = Census_enrlment + WaitlistCount) %>% 
  group_by(SchoolYear) %>% 
  summarize(Units = sum(unit_temp)) %>% 
  mutate(Department = "Statistics") %>% 
  mutate(DepartmentLong = "Statistics Faculty in Mathematics") %>%
  select(Department, SchoolYear, Units, DepartmentLong)

joined_total_units_s <- total_Units %>% 
  bind_rows(stats_sep2) %>% 
  inner_join(yt_small, by = "SchoolYear") %>% 
  left_join(total_FTE_wStats, 
            by = c("Department", "SchoolYear", "DepartmentLong")) %>%
  mutate(Units_perFTE = Units / FTE) %>% 
  rename("year" = ThesisYear)

join_for_scatter_s <- joined_total_s %>%
  inner_join(joined_total_units_s, by = c("DepartmentLong", "Department", "SchoolYear", "year"))

start_year <- 2013
end_year <- 2016
#if(input$center == "Mean" & input$HUM2){
join_for_scatter_s %>% 
  filter(year >= start_year, year <= end_year) %>%        
  group_by(Department) %>% #, Division) %>% 
  summarize(
    `Mean Thesis Load per FTE` = mean(thes_load_perFTE),
    `Mean Student Units per FTE` = mean(Units_perFTE)
  ) %>%
  ggplot(aes(x = `Mean Student Units per FTE`, y = `Mean Thesis Load per FTE`)) +
  geom_point(size = 2) +#, aes(color = Division))  +
  #   geom_text_repel(mapping = aes(label = Department), size = 5) +
  annotate("text", x = 120, y = 3.5, label = "Statistics") +
  #   annotate("point", x = sum(stats_sep3$Units)/sum(stats_to_add$FTE), 
  #             y = sum(stats_thes_dept$advisees)/sum(stats2$FTE)) +
  coord_cartesian(xlim = c(35, 135), ylim = c(0, 4.5)) + 
  theme_bw()
ggsave("stat_scatter1.pdf", height = 5, width = 6)  