library(shiny)
library(tidyverse)
library(gt)

# FOR NOW: just read in group data

group_data <- read_csv("test_data.csv", na = "0")



group_data$Birthdate <-as.Date(group_data$Birthdate, "%m/%d/%Y")

group_data <- group_data %>%
  mutate(zodiac = case_when(between(group_data$Birthdate, as.Date("2003-07-23"), as.Date("2003-08-22")) ~ "Leo",
                            between(group_data$Birthdate, as.Date("2003-08-23"), as.Date("2003-09-22")) ~ "Virgo",
                            between(group_data$Birthdate, as.Date("2003-09-23"), as.Date("2003-10-22")) ~ "Libra",
                            between(group_data$Birthdate, as.Date("2003-10-23"), as.Date("2003-11-21")) ~ "Scorpio",
                            between(group_data$Birthdate, as.Date("2003-11-22"), as.Date("2003-12-21")) ~ "Sagittarius",
                            between(group_data$Birthdate, as.Date("2003-12-22"), as.Date("2004-01-19")) ~ "Capricorn",
                            between(group_data$Birthdate, as.Date("2004-01-20"), as.Date("2004-02-18")) ~ "Aquarius",
                            between(group_data$Birthdate, as.Date("2004-02-19"), as.Date("2004-03-20")) ~ "Pisces",
                            between(group_data$Birthdate, as.Date("2004-03-21"), as.Date("2004-04-19")) ~ "Aries",
                            between(group_data$Birthdate, as.Date("2004-04-20"), as.Date("2004-05-20")) ~ "Taurus",
                            between(group_data$Birthdate, as.Date("2004-05-21"), as.Date("2004-06-20")) ~ "Gemini",
                            between(group_data$Birthdate, as.Date("2004-06-21"), as.Date("2004-07-22")) ~ "Cancer",
                            between(group_data$Birthdate, as.Date("2004-07-23"), as.Date("2004-08-22")) ~ "Leo",
                            between(group_data$Birthdate, as.Date("2004-08-23"), as.Date("2004-09-22")) ~ "Virgo",
                            TRUE ~ "other")
  )

# get essentials

student_overview <- group_data %>%
  transmute(
    # I'm not using stringr::str_c for these because I don't want NA's to corrupt the concatenated strings
    Name = paste0(Preferred, " ", Last),  # i dont think we'll ever get NA names...
    Pronouns = `Pronouns (WW Info Form Response)`, 
    
    From = case_when(
      # just don't print an NA if it exists (sometimes no region)
      is.na(Region) ~ paste0(City, ", ", Country),
      # or just combine them
      TRUE ~ paste0(City, ", ", Region, ", ", Country)),
    
    `Favorite Song` = case_when(
      # leave NA as NA if both
      is.na(`Fave Song - Title`) & is.na(`Fave Song - Artist`) ~ NA,
      # or just combine them
      TRUE ~ paste0(`Fave Song - Title`, " by ", `Fave Song - Artist`)),
    
    DOB = paste0(Birthdate, " (", zodiac, ")")
  )

# Get data in format for table:
# PIVOT LONGER SLAY

longer_overview <- student_overview %>%
  pivot_longer(2:5, names_to = "Question", values_to = "Response") %>%
  drop_na()

longer_overview %>%
  gt::gt(groupname_col = "Name") %>%
  gt::tab_options(column_labels.hidden = TRUE) %>%
  gt::tab_style(style = list(
    cell_fill(color = "blue", alpha = 0.2),
    cell_text(style = "italic", weight = "bold")
  ),
  locations = cells_row_groups())

## make group overview table

get_iso <- group_data %>%
  filter(`Do you plan to attend International student Orientation?` == "Yes") %>%
  transmute(Name = str_c(Preferred, " ", Last))
  

major_counts <- group_data %>%
  group_by(`Major 1`) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

most_pop_major <- major_counts %>%
  filter(count == max(major_counts$count)) %>%
  select(`Major 1`) 



group_stats <- group_data %>%
  head(n=1) %>%
  transmute(
    group_number = as.character(`Group Number`),
    num_students = as.character(`Group Size`),
    board_mem = `Board 1`,
    pop_major = 
      # can't use `case_when` here because it evaluates ALL RHS cases, even if LHS is false
      # this wont work bc not all our RHS inputs exist when LHS is false
      if (nrow(most_pop_major) == 1) most_pop_major[[1]][[1]]
      
      else if (nrow(most_pop_major) == 2) str_c(most_pop_major[[1]][[1]], " and ", most_pop_major[[1]][[2]])
      
      else if (nrow(most_pop_major) == 3) str_c(most_pop_major[[1]][[1]],", ", most_pop_major[[1]][[2]],", and ", most_pop_major[[1]][[3]])
      
      else "4+ way tie"
    
  ) %>%
  pivot_longer(everything(), names_to = "statistic")


# get emails to display and copy ----

get_emails <- group_data %>%
  select(`NU Email`) %>%
  as.vector() 
  

get_emails[[1]] %>% str_flatten_comma()

# get answers to longer question responses ----

bigger_answers <- group_data %>%
  select(Preferred, Last, `When thinking about your first year at Northwestern, what are you most looking forward to?`, 
         `What are you most nervous about as you think about your first year at Northwestern?`, 
         `Is there anything you would like your Peer Adviser or Student Transition Experiences to know as you consider Wildcat Welcome? Is there any specific support you want as you prepare for your first year at Northwestern?`)


# part 2 of that: Gap year and anything else

gap_year <- group_data %>%
  filter(`Did you take a gap year (or years) between your last school and coming to Northwestern?` == "Yes") %>%
  transmute(
    gap_yr_activities = str_c(Preferred, " ", Last, ": ", `If yes to above, what did you do during your gap year?`))

anything_else <- group_data %>%
  filter(!is.na(`Is there anything you would like your Peer Adviser or Student Transition Experiences to know as you consider Wildcat Welcome? Is there any specific support you want as you prepare for your first year at Northwestern?`)) %>%
  transmute(
    anything_to_add = str_c(Preferred, " ", Last, ": ", `Is there anything you would like your Peer Adviser or Student Transition Experiences to know as you consider Wildcat Welcome? Is there any specific support you want as you prepare for your first year at Northwestern?`)
  )

