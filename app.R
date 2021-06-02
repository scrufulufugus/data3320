# Note: Hi Mia, I finished setting up the filter buttons... for now.
#       So far I have just added the one graph and added sliders and check boxes
#       (mostly check boxes) for all the possible filtering that could be done.
#       But depending on what the groups are doing for their graphs our current
#       set up might change.
#       I also asked the prof for advice on setup and so far he has suggested a
#       way to more easily select a single box without having to uncheck all the
#       other ones too.
#       I added select all / reset buttons for all the check box groups to make
#       that possible.
#       Feel free to change and add anything...

library(shiny)
library(shinydashboard)
library(readr)
library(tidyverse)
library(lubridate)

# Define our main dataframe structure
raw_data <- data.frame(ID = character(),
                       Incident_Num = double(),
                       Incident_Type = character(),
                       Occured_date_time = POSIXct(),
                       Precinct = character(),
                       Sector = character(),
                       Beat = character(),
                       Officer_ID = double(),
                       Subject_ID = double(),
                       Subject_Race = character(),
                       Subject_Gender = character())

PAGE_SIZE <- 5000 # How big we want each page to be
index <- 0 # The last processed record

query_api <- function() {
  repeat {
      query <- paste("https://data.seattle.gov/resource/ppi5-g2bj.csv?",
                     "$order=uniqueid&",
                     "$limit=", PAGE_SIZE,
                     "&$offset=", index, sep = "")

      # Load a page of the dataset, also do some error handling so a network
      #  timeout during a refresh does not take out the application
      error_bit <- FALSE
      df_page <- tryCatch({ read_csv(query) },
                          error = function(err) {
                            error_bit <<- TRUE
                          })
      #df_page <- read_csv(query)

      # DEBUG: Shows current index and new indexed rows, -1 implies network error
      message("`df_page`: Current index is ", index, "; adding ",
              ifelse(is.data.frame(df_page), nrow(df_page), -1), " new rows")

      # If the current page is blank or there was an error: break the loop
      if (error_bit || nrow(df_page) == 0) {
        break
      }

      # Rename column to match old csv
      df_page <- df_page %>%
        rename(ID = uniqueid,
               Incident_Num = incident_num,
               Incident_Type = incident_type,
               Occured_date_time = occured_date_time,
               Precinct = precinct,
               Sector = sector,
               Beat = beat,
               Officer_ID = officer_id,
               Subject_ID = subject_id,
               Subject_Race = subject_race,
               Subject_Gender = subject_gender)

      # Bind our new data to the end of the main dataframe and remove duplicates (just incase)
      raw_data <<- distinct(rbind(raw_data, df_page), ID, .keep_all = TRUE)

      # Increment index to the tail of df
      index <<- nrow(raw_data)
    }
  return(raw_data)
}

derive_columns <- function(df) {
  # Create New colums for year, month, day, hour, date from POSIX date time column
  df_tidy <- df %>%
    mutate(year = year(Occured_date_time),
           month = month(Occured_date_time, label = TRUE),
           day = wday(Occured_date_time, label = TRUE),
           hour = hour(Occured_date_time),
           date = as_date(Occured_date_time),
           weekday = strftime(Occured_date_time, format="%A", "UTC"),
           monthName = strftime(Occured_date_time, format="%B", "UTC"))
    df_tidy$Incident_Type <- recode(df_tidy$Incident_Type,
                                    "Level 1 - Use of Force" = "Level 1: Temporary Pain",
                                    "Level 2 - Use of Force" = "Level 2: Physical Injury",
                                    "Level 3 - Use of Force" = "Level 3: Substantial Injury",
                                    "Level 3 - OIS" = "Level 3 (IOS): Officer Involved Shooting")
  return(df_tidy)
}

Use_Of_Force <- derive_columns(query_api())

ForceLevels <- c("Level 1: Temporary Pain", "Level 2: Physical Injury", "Level 3: Substantial Injury", "Level 3 (IOS): Officer Involved Shooting")
Weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")
Months <- c("January", "February", "March", "April", "May","June","July", "August","September","October","November","December")
Gender <- c("Female","Male", "Not Specified")
Race <- c("American Indian/Alaska Native","Asian","Black or African American","Hispanic or Latino","Nat Hawaiian/Oth Pac Islander","Not Specified","White")
Precinct <- c("X","West","Southwest","South","OOJ","North","East","0","-")
Sector <- c("BOY","CHARLIE","DAVID","EDWARD","FRANK",'GEORGE',"JOHN","KING","LINCOLN","MARY","NORA","OCEAN","QUEEN","ROBERT","SAM","UNION","WILLIAM","")
Beat <- c("-","0","99","B1","B2","B3","C1","C2","C3","D1","D2","D3","E1","E2","E3","F1","F2","F3","XX")

ui <- dashboardPage(
    dashboardHeader(title = "Use of Force in Seattle"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Filter by Date and Time", icon = icon("calendar"), startExpanded = FALSE,
                     dateRangeInput(inputId = 'date',
                                    label = 'Filter by Date',
                                    min = min(Use_Of_Force$date), max = max(Use_Of_Force$date),
                                    width = "100%",
                                    start = min(Use_Of_Force$date),
                                    end = max(Use_Of_Force$date)
                     ),
                     checkboxGroupInput(inputId = 'months',
                                        label = "Filter by Month",
                                        choices = Months, 
                                        selected = Months
                                        
                     ),
                     actionButton("resetMonths", label = "Reset"),
                     
                     checkboxGroupInput(inputId = 'days',
                                        label = "Filter by Weekday",
                                        choices = Weekdays, 
                                        selected = Weekdays
                     ), 
                     actionButton("resetWeekdays", label = "Reset"),
                     sliderInput(inputId = 'hour',
                                 label = 'Filter by Hour',
                                 min = min(Use_Of_Force$hour), 
                                 max = max(Use_Of_Force$hour),
                                 value= c(min(Use_Of_Force$hour),max(Use_Of_Force$hour)),  
                                 width = "100%",
                                 step = 1
                     )
            ),
            menuItem("Filter by Demographs", icon = icon("user"), startExpanded = FALSE,
                     checkboxGroupInput(inputId = 'gender',
                                        label = "Filter by Gender",
                                        choices = Gender, 
                                        selected = Gender
                     ),
                     actionButton("resetGender", label = "Reset"),
                     checkboxGroupInput(inputId = 'race',
                                        label = "Filter by Race",
                                        choices = Race, 
                                        selected = Race 
                     ),
                     actionButton("resetRace", label = "Reset")
            ),
            menuItem("Filter by Region", icon = icon("globe"), startExpanded = FALSE,
                     menuItem("Filter by Precinct",  startExpanded = FALSE,
                              checkboxGroupInput(inputId = 'precinct',
                                                 label = "",
                                                 choices = Precinct, 
                                                 selected = Precinct
                              ),
                              actionButton("resetPrecinct", label = "Reset")
                     ),
                     menuItem("Filter by Sector",  startExpanded = FALSE,
                              checkboxGroupInput(inputId = 'sector',
                                                 label = "",
                                                 choices = Sector, 
                                                 selected = Sector
                              ),
                              actionButton("resetSector", label = "Reset")
                     ),
                     menuItem("Filter by Beat",  startExpanded = FALSE,
                              checkboxGroupInput(inputId = 'beat',
                                                 label = "",
                                                 choices = Beat, 
                                                 selected = Beat
                              ),
                              actionButton("resetBeat", label = "Reset")
                     )
            ),
            checkboxGroupInput(inputId = 'force',
                               label = "Filter by Type of Force",
                               choices = ForceLevels, 
                               selected = ForceLevels 
            ),
            actionButton("resetForce", label = "Reset")
        )
    ),
    dashboardBody(
        plotOutput('forceByHour') 
    )
)

server <- function(input, output, session) {

  generate_df <- reactive({
    invalidateLater(1.2e+6, session) # Reload every 20 minutes
    #invalidateLater(1.5e+4, session) # Reload every 15 seconds

    df <- derive_columns(query_api()) %>%
      filter(hour >= input$hour[1] & hour <= input$hour[2]) %>%
      filter(date >= input$date[1] & date <= input$date[2]) %>%
      filter(monthName %in% input$months) %>%
      filter(weekday %in% input$days) %>%
      filter(Incident_Type %in% input$force) %>%
      filter(Subject_Gender %in% input$gender) %>%
      filter(Subject_Race %in% input$race) %>%
      filter(Precinct %in% input$precinct) %>%
      filter(Sector %in% input$sector) %>%
      filter(Beat %in% input$beat)

    result <- df
  })

    output$menu <- renderMenu({
        sidebarMenu(
            menuItem("Menu item", icon = icon("calendar"))
        )
    })
    output$forceByHour <- renderPlot({

      Use_Of_Force <<- generate_df()
        
        Use_Of_Force_fbh <- Use_Of_Force %>%
            group_by(hour, Incident_Type) %>%
            summarise(count = n())
        
        p <- ggplot(data = Use_Of_Force_fbh, mapping = aes(x = hour, y = count, color = Incident_Type))
        
        p + geom_line() + 
            scale_x_continuous(breaks = seq(0,23, by = 1)) +
            labs(x = "Hour", y = "Count of Incidents")
    })
    observeEvent(
        input$resetMonths, {
            if (is.null(input$months) == TRUE) {
                updateCheckboxGroupInput(session, "months", choices = Months, selected = Months)
                updateActionButton(session, "resetMonths", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "months", choices = Months, selected = NULL)
                updateActionButton(session, "resetMonths", label = "Select All")
            }
        }
    )
    observe({
        if (is.null(input$months) == TRUE) {
            updateActionButton(session, "resetMonths", label = "Select All")
        } else {
            updateActionButton(session, "resetMonths", label = "Reset")
        }
    })
    observeEvent(
        input$resetWeekdays, {
            if (is.null(input$days) == TRUE) {
                updateCheckboxGroupInput(session, "days", choices = Weekdays, selected = Weekdays)
                updateActionButton(session, "resetWeekdays", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "days", choices = Weekdays, selected = NULL)
                updateActionButton(session, "resetWeekdays", label = "Select All")
            }
        }
    )
    observe({
        if (is.null(input$days) == TRUE) {
            updateActionButton(session, "resetWeekdays", label = "Select All")
        } else {
            updateActionButton(session, "resetWeekdays", label = "Reset")
        }
    })
    observeEvent(
        input$resetGender, {
            if (is.null(input$gender) == TRUE) {
                updateCheckboxGroupInput(session, "gender", choices = Gender, selected = Gender)
                updateActionButton(session, "resetGender", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "gender", choices = Gender, selected = NULL)
                updateActionButton(session, "resetGender", label = "Select All")
            }
        }
    )
    observe({
        if (is.null(input$gender) == TRUE) {
            updateActionButton(session, "resetGender", label = "Select All")
        } else {
            updateActionButton(session, "resetGender", label = "Reset")
        }
    })
    observeEvent(
        input$resetRace, {
            if (is.null(input$race) == TRUE) {
                updateCheckboxGroupInput(session, "race", choices = Race, selected = Race)
                updateActionButton(session, "resetRace", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "race", choices = Race, selected = NULL)
                updateActionButton(session, "resetRace", label = "Select All")
            }
        }
    )
    observe({
        if (is.null(input$race) == TRUE) {
            updateActionButton(session, "resetRace", label = "Select All")
        } else {
            updateActionButton(session, "resetRace", label = "Reset")
        }
    })
    observeEvent(
        input$resetPrecinct, {
            if (is.null(input$precinct) == TRUE) {
                updateCheckboxGroupInput(session, "precinct", choices = Precinct, selected = Precinct)
                updateActionButton(session, "resetPrecinct", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "precinct", choices = Precinct, selected = NULL)
                updateActionButton(session, "resetPrecinct", label = "Select All")
            }
        }        
    )
    observe({
        if (is.null(input$precinct) == TRUE) {
            updateActionButton(session, "resetPrecinct", label = "Select All")
        } else {
            updateActionButton(session, "resetPrecinct", label = "Reset")
        }
    })
    observeEvent(
        input$resetSector, {
            if (is.null(input$sector) == TRUE) {
                updateCheckboxGroupInput(session, "sector", choices = Sector, selected = Sector)
                updateActionButton(session, "resetSector", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "sector", choices = Sector, selected = NULL)
                updateActionButton(session, "resetSector", label = "Select All")
            }
        }        
    )
    observe({
        if (is.null(input$sector) == TRUE) {
            updateActionButton(session, "resetSector", label = "Select All")
        } else {
            updateActionButton(session, "resetSector", label = "Reset")
        }
    })
    observeEvent(
        input$resetBeat, {
            if (is.null(input$beat) == TRUE) {
                updateCheckboxGroupInput(session, "beat", choices = Beat, selected = Beat)
                updateActionButton(session, "resetBeat", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "beat", choices = Beat, selected = NULL)
                updateActionButton(session, "resetBeat", label = "Select All")
            }
        }        
    )
    observe({
        if (is.null(input$beat) == TRUE) {
            updateActionButton(session, "resetBeat", label = "Select All")
        } else {
            updateActionButton(session, "resetBeat", label = "Reset")
        }
    })
    observeEvent(
        input$resetForce, {
            if (is.null(input$force) == TRUE) {
                updateCheckboxGroupInput(session, "force", choices = ForceLevels, selected = ForceLevels)
                updateActionButton(session, "resetForce", label = "Reset")
            } else {
                updateCheckboxGroupInput(session, "force", choices = ForceLevels, selected = NULL)
                updateActionButton(session, "resetForce", label = "Select All")
            }
        }
            
    )
    observe({
            if (is.null(input$force) == TRUE) {
                updateActionButton(session, "resetForce", label = "Select All")
            } else {
                updateActionButton(session, "resetForce", label = "Reset")
            }
    })

}

shinyApp(ui, server)

