###############################################################################
# PA INFO SHINY APP ----
###############################################################################


# future plans would be have people login

# app based on using `gt` to help visualize student data for PA groups

# but I think for now the best thing will be to just ask folks for their file
# maybe even a google drive link... hmm

##------------------------------------------------------------------------------
## Loading Packages----
##------------------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(gt)
library(rclipboard) # used to make email copy button


###############################################################################
# UI ----
###############################################################################


ui <- fluidPage(theme = shinythemes::shinytheme("yeti"),
  
  

    # Application title
    titlePanel("PA Group Intro"),
    
    rclipboardSetup(), # get rclipboard all ready to go
    
    fluidRow(
      column(12,
             fluidRow(
               column(5,
                      wellPanel(
                        p(HTML("<em><strong>REMINDER:</strong></em> Please check your assigned google sheet regularly for group changes.
                           This dashboard will <em><strong>NOT</strong></em> update automatically.</br></br>
                           Please email seanpascoe2024@u.northwestern.edu with any questions/concerns!")),
                        fileInput("file1", "Please Upload Your Roster (.CSV)", accept = ".csv"),
                        htmlOutput("welcome_message"),
                        gt_output("overview")
                      )
               ),
               column(7,
                      htmlOutput("instructions"),
                      fluidRow(
                        column(9,
                               gt_output("group_stats")),
                        column(3,
                               uiOutput("emails", inline = TRUE)) # using rclipboard
                      ),
                      gt_output("big_questions"),
                      # SMALL ANSWERS HERE
                      htmlOutput("gap_year_text"),
                      htmlOutput("phone_avail_text"),
                      htmlOutput("schedule_text"),
                      htmlOutput("diet_text"),
                      htmlOutput("other_notes_text")
                      )
             ),
             DT::dataTableOutput("maintable")
      )
    )
    
)


###############################################################################
# SERVER ----
###############################################################################

server <- function(input, output) {
  
##########################
# SERVER TABLE OF CONTENTS:
  
# 1. get input data
# 2. main table print
# 3. instructions print
# 4. welcome message print
# 5. sidebar table render
# 6. group overview table
# 7. copy email button
# 8. long answer questions
# 9. gap year info
# 10. phone availability info
# 11. dietary needs
# 12. Other notes
  
##########################
  
  ####################################
  ###  1. get input data ----
  ####################################
  
  # reactive call stores our data in group_data, which can be called within
  # the server function definition as `group_data()` (see output$maintable for example)
  
  group_data <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    # make sure a .csv was uploaded
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read_csv(file$datapath, na = c("","0", "#N/A"))
  })

    ####################################
    ### 2. main table print ----
    ####################################
  
    output$maintable <- DT::renderDataTable({
      group_data()
    })
    
    ####################################
    ### 3. instructions print ----
    ####################################
    
    output$instructions <- renderPrint({
      # if there is a file input, print nothing
      # if there isn't, print some instructions
      instructions <- ifelse(isTruthy(input$file1),
             "",
             "<h2>Welcome to the PA Group Data Visualization App!</h2><br>
             To get started, please do the following: <br><br>
             1. Open the google sheet sent to you by your board member<br><br>
             2. Download the sheet as a .csv document by going to File > Download > Comma Separated Values (.csv)<br><br>
             2.5 (optional): Manually fix any phone numbers that populate as #ERROR! due to inclusion of + with country code<br><br>
             3. Load the .csv file in the box to the left
             <br><br>
             Happy PA-ing!!"
             )
      
      HTML(instructions)
      
    })
    
    ####################################
    ###  4. welcome message print ----
    ####################################
    
    output$welcome_message <- renderPrint({
      # get PA name
      
      name <- group_data() %>%
        transmute(PAs = case_when(
          is.na(`PA2`) ~ `PA1`,
          TRUE ~ str_c(`PA1`, " and ", `PA2`)
        )) %>%
        head(n=1) %>%
        as.character()
      
      HTML(str_c("<h3><em>Welcome, ", name, "</em></h3>",
                 "<h5>Here is some information about your students:</h5>"))
      
    })

    ####################################
    ###  5. sidebar table render ----
    ####################################
    
    output$overview <- render_gt({
      
      # get the reactive input group data
      group_data <- group_data()
      
      # add zodiac sign
      group_data <- group_data %>%
        mutate(zodiac = DescTools::Zodiac(as.Date(Birthdate, "%m/%d/%Y"), stringsAsFactors = F))
      
      # get essentials
      
      student_overview <- group_data %>%
        transmute(
          # I'm not using stringr::str_c for these because I don't want NA's to corrupt the concatenated strings
          Name = paste0(Preferred, " ", Last),  # i dont think we'll ever get NA names...
          Pronouns = `Pronouns...7`, 
          
          `Active City/Region` = case_when(
            # just don't print an NA if it exists (sometimes no region)
            is.na(`Active Region`) ~ `Active City`,
            # or just combine them
            TRUE ~ paste0(`Active City`, ", ", `Active Region`)),
          
          `Primary Citizenship` = `Primary Citizenship`,
          
          `Favorite Song` = case_when(
            # leave NA as NA if both
            is.na(`Song Title`) & is.na(`Artist`) ~ NA,
            # or just combine them
            TRUE ~ paste0(`Song Title`, " by ", `Artist`)),
          
          DOB = paste0(Birthdate, " (", zodiac, ")"),
          `Phone #` = `Primary`,
          
          # if no major 2, then just give their one major. If 2 majors, give both
          `Intended Major(s)` = case_when(
            is.na(`Major 2`) ~ `Major 1`,
            TRUE ~ str_c(`Major 1`, " and ", `Major 2`)
          ),
          `Last School` = `High School`
        )
      
      # Get data in format for table:
      # PIVOT LONGER SLAY
      
      longer_overview <- student_overview %>%
        pivot_longer(2:9, names_to = "Question", values_to = "Response") %>%
        drop_na()
      
      longer_overview %>%
        gt::gt(groupname_col = "Name") %>%
        gt::tab_options(column_labels.hidden = TRUE) %>%
        gt::tab_style(style = list(
          cell_fill(color = "blue", alpha = 0.2),
          cell_text(style = "italic", weight = "bold")
        ),
        locations = cells_row_groups())
      
      
 
    })
    
    ####################################
    ###  6. group overview table ----
    ####################################
    
    output$group_stats <- render_gt({
   

      group_data <- group_data()
      
      group_size = nrow(group_data)
      
      # getting most popular major(s)
      major_counts <- group_data %>%
        group_by(`Major 1`) %>%
        summarize(count = n()) %>%
        arrange(desc(count))
      
      most_pop_major <- major_counts %>%
        filter(count == max(major_counts$count)) %>%
        select(`Major 1`) 
      
      # get info about ISO attendance
      going_to_iso <- group_data %>%
        filter(`Do you plan to attend International student Orientation?` == "Yes") %>%
        transmute(Name = str_c(Preferred, " ", Last))
      
      iso_names <- going_to_iso[[1]] %>% str_flatten_comma()
      
      # make table
      group_stats <- group_data %>%
        head(n=1) %>%
        transmute(
         # `PA Group #` = as.character(`Group Number`),
          `# of Students` = as.character(group_size),
          `Your Board Member` = `PA1 Board Member`,
          `Most Popular Major(s)` = 
            # can't use `case_when` here because it evaluates ALL RHS cases, even if LHS is false
            # this wont work bc not all our RHS inputs exist when LHS is false
            if (nrow(most_pop_major) == 1) most_pop_major[[1]][[1]]
          
          else if (nrow(most_pop_major) == 2) str_c(most_pop_major[[1]][[1]], " and ", most_pop_major[[1]][[2]])
          
          else if (nrow(most_pop_major) == 3) str_c(most_pop_major[[1]][[1]],", ", most_pop_major[[1]][[2]],", and ", most_pop_major[[1]][[3]])
          
          else "4+ way tie",
          
          # add ISO students (or "None" if no one attending ISO)
          `Students Planning to go to ISO` = case_when(
            iso_names == "" ~ "None",
            TRUE ~ iso_names
          )
            
          ) 
      
      group_stats %>%
        gt() %>%
        tab_header(
          title = "Your PA Group: Some Stats"
        ) %>%
        gtExtras::gt_theme_pff()
      
    })
    
    ####################################
    ###  7. copy email button ----
    ####################################
    
    # I really have no idea how this works, or why there are two renderUI calls.
    # just copied code from the CRAN description of `rclipboard()` ¯\_(ツ)_/¯
    output$emails <- renderUI({
      output$emails <- renderUI({
        
        group_data <- group_data()
        
        get_emails <- group_data %>%
          select(`NU Email`) %>%
          as.vector() 
        
        
        emailz <- get_emails[[1]] %>% str_flatten_comma()
        
        rclipButton(
          inputId = "clipbtn",
          label = "Copy Emails",
          clipText = emailz, 
          icon = icon("clipboard")
        )
      })
    })
    
    ####################################
    ### 8. long answer questions ----
    ####################################    
    
    output$big_questions <- render_gt({
      # get data
      group_data <- group_data()
      
      bigger_answers <- group_data %>%
        transmute(
          Name = str_c(Preferred, " ", Last),
          `When thinking about your first year at Northwestern, what are you most looking forward to?`, 
          `What are you most nervous about as you think about your first year at Northwestern?`) 
      
      bigger_answers %>%
        gt() %>%
        tab_header(title = "Initial Thoughts") %>%
        gtExtras::gt_theme_pff()
      
      
    })
    
    ####################################
    ###  9. gap year info ----
    ####################################
    
    output$gap_year_text <- renderPrint({
      
      group_data <- group_data()
      
      gap_year <- group_data %>%
        filter(`Did you take a gap year (or years) between your last school and coming to Northwestern?` == "Yes") %>%
        transmute(
          gap_yr_activities = str_c(Preferred, " ", Last, ": ", `If yes to above, what did you do during your gap year?`))
      
      if (nrow(gap_year) == 0)
      {HTML("<h4><strong>From Students that took Gap Years:</strong></h4>
             <br>
             None of your Students took gap years.")}
      else 
      {HTML(str_c("<h4><strong>From Students that took Gap Years:</strong></h4>",
                  "<br>",
                  gap_year[[1]] %>% str_flatten(collapse = "<br><br>")))}
      
      
    })
    
    ####################################
    ###  10. phone availability info ----
    ####################################
    
    output$phone_avail_text <- renderPrint({
      
      group_data <- group_data()
      
      smart_avail <- group_data %>%
        filter(`Do you have a smartphone with internet connectivity (that operates in the US)?` != "Yes") %>%
        transmute(
          smartphone = str_c(Preferred, " ", Last, ": ", `Do you have a smartphone with internet connectivity (that operates in the US)?`))
      
      if (nrow(smart_avail) == 0)
      {HTML("<h4><strong>All students have US smartphones.</strong></h4>")}
      else 
      {HTML(str_c("<h4><strong>Students that don't have US smartphones:</strong></h4>",
                  "<br>",
                  smart_avail[[1]] %>% str_flatten(collapse = "<br><br>")))}
      
      
    })

    ####################################
    ###  11. Scheduling Concerns ----
    ####################################
    
    output$schedule_text <- renderPrint({
      
      group_data <- group_data()
      
      sched_concern <- group_data %>%
        filter(!is.na(`Do you have any scheduling concerns as you prepare to participate in Wildcat Welcome from Monday, September 11 through Monday, September 18? If so, provide your concerns below.`)) %>%
        transmute(
          smartphone = str_c(Preferred, " ", Last, ": ", `Do you have any scheduling concerns as you prepare to participate in Wildcat Welcome from Monday, September 11 through Monday, September 18? If so, provide your concerns below.`)
        )
      
      if (nrow(sched_concern) == 0)
      {HTML("<h4><strong>Your students don't have any scheduling concerns.</strong></h4>")}
      else 
      {HTML(str_c("<h4><strong>Students with WW scheduling concerns:</strong></h4>",
                  "<br>",
                  sched_concern[[1]] %>% str_flatten(collapse = "<br><br>")))}
      
      
    })
    
    
    
     
    ####################################
    ###  12. Dietary needs ----
    ####################################
    
    output$diet_text <- renderPrint({
      
      group_data <- group_data()
      
      diet_needs <- group_data %>%
        filter(!is.na(`Do you have any dietary needs for event organizers to be aware of?`)) %>%
        transmute(
          smartphone = str_c(Preferred, " ", Last, ": ", `Do you have any dietary needs for event organizers to be aware of?`)
          )
      
      if (nrow(diet_needs) == 0)
      {HTML("<h4><strong>Your students don't have any dietary restrictions.</strong></h4>")}
      else 
      {HTML(str_c("<h4><strong>Students with dietary restrictions:</strong></h4>",
                  "<br>",
                  diet_needs[[1]] %>% str_flatten(collapse = "<br><br>")))}
      
      
    })
    
    
    ####################################
    ###  13. Other Notes ----
    ####################################
    
    output$other_notes_text <- renderPrint({
      
      group_data <- group_data()
      
      anything_else <- group_data %>%
        filter(!is.na(`Is there anything you would like your Peer Adviser or Student Transition Experiences to know as you consider Wildcat Welcome? Is there any specific support you want as you prepare for your first year at Northwestern?`)) %>%
        transmute(
          anything_to_add = str_c(Preferred, " ", Last, ": ", `Is there anything you would like your Peer Adviser or Student Transition Experiences to know as you consider Wildcat Welcome? Is there any specific support you want as you prepare for your first year at Northwestern?`)
        )
      
      
      if (nrow(anything_else) == 0)
      {HTML("<h4><strong>Other Notes from Students:</strong></h4> 
             <br>
             None of your Students had more to add.")}
      else 
      {HTML(str_c("<h4><strong>Other Notes from Students:</strong></h4>",
                  "<br>",
                  anything_else[[1]] %>% str_flatten(collapse = "<br><br>")))}
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
