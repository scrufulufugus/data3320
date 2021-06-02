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

# Load Data
Use_Of_Force <- read_csv("Use_Of_Force.csv")

# Create New column with POSIX date time format from given date time in character format
Use_Of_Force$new_occured_date_time <- mdy_hms(Use_Of_Force$Occured_date_time)

# Create New colums for year, month, day, hour, date from POSIX date time column
Use_Of_Force <- Use_Of_Force %>% 
    mutate(year = year(new_occured_date_time),
           month = month(new_occured_date_time, label = TRUE), 
           day = wday(new_occured_date_time, label = TRUE), 
           hour = hour(new_occured_date_time),
           date = as.Date(new_occured_date_time, format="%Y/%m/%d"),
           weekday = strftime(new_occured_date_time, format="%A", "UTC"),
           monthName = strftime(new_occured_date_time, format="%B", "UTC")) 
Use_Of_Force$Incident_Type[Use_Of_Force$Incident_Type=="Level 1 - Use of Force"]<-"Level 1: Temporary Pain"
Use_Of_Force$Incident_Type[Use_Of_Force$Incident_Type=="Level 2 - Use of Force"]<-"Level 2: Physical Injury"
Use_Of_Force$Incident_Type[Use_Of_Force$Incident_Type=="Level 3 - Use of Force"]<-"Level 3: Substantial Injury"
Use_Of_Force$Incident_Type[Use_Of_Force$Incident_Type=="Level 3 - OIS"]<-"Level 3 (IOS): Officer Involved Shooting"

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
    output$menu <- renderMenu({
        sidebarMenu(
            menuItem("Menu item", icon = icon("calendar"))
        )
    })
    output$forceByHour  <- renderPlot({
        
        
        Use_Of_Force <- Use_Of_Force  %>%
            filter(hour >= input$hour[1] & hour <= input$hour[2]) %>%
            filter(date >= input$date[1] & date <= input$date[2]) %>%
            filter(monthName %in% input$months) %>%
            filter(weekday %in% input$days) %>%
            filter(Incident_Type %in% input$force) %>%
            filter(Subject_Gender %in% input$gender) %>%
            filter(Subject_Race %in% input$race) %>%
            filter(Precinct %in% input$precinct) %>%
            filter(Sector %in% input$sector) %>%
            filter(Beat %in% input$beat) %>%
            group_by(hour, Incident_Type) %>%
            summarise(count = n())
        
        p <- ggplot(data = Use_Of_Force, mapping = aes(x = hour, y = count, color = Incident_Type)) 
        
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

