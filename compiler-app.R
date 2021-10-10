# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# JOSEPHS PATH "C:\\rProjects\\suicide\\suicides2.0\\all_suicides.csv"
# RASHMIS PATH "/Users/rashmijha/Desktop/suicide project/VH8/all_suicides.csv"

# temp <- tempfile()
# download.file("https://github.com/josephfsexton/U.S.-Suicide-Compiler/raw/main/all_suicides.zip", temp)
# suicides <- read.csv(unz(temp, "all_suicides.csv"))
#pop_params <- read.csv("https://github.com/josephfsexton/U.S.-Suicide-Compiler/raw/main/pop_param.csv")

library(shiny)
library(dplyr)
library(shinyjs)
library(ggplot2)

#install.packages('shinyjs')

########################################################################################################

# Labels and other lists

########################################################################################################

sexes <- c("Male", "Female")
sex_code = list('Male' = 'M', 'Female' = 'F')

races <-
  c(
    "White",
    "Black",
    "American Indian / Alaskan Native (AIAN)",
    "Asian",
    "Mixed",
    "Pacific Islander"
  )
race_code = list(
  'White' = 'white',
  'American Indian / Alaskan Native (AIAN)' = 'aian',
  'Asian' = 'asian',
  'Mixed' = 'mixed',
  'Pacific Islander' = 'pacific'
)

ethnicities <- c("Hispanic", "Non-Hispanic")
ethnic_code = list('Hispanic' = 'hispanic', 'Non-Hispanic' = 'non-hispanic')

marital_statuses <- c("Married", "Single", "Divorced", "Widowed")
marital_statuses_code = list(
  'Married' = 'M',
  "Single" = 'S',
  "Divorced" = 'D',
  "Widowed" = 'W'
)

ed_statuses <-
  c(
    "Did not finish high school",
    "High school educated",
    "College educated"
  )
ed_code <-
  c(
    "Did not finish high school" = c("000", "010", "020", "030", "040", "050", "060",
                                     "070", "080", "90", "100", "110", "11", "21"),
    "High school educated" = c("120", "31"),
    "Post-secondary education (any)" = c("130", "140", "150", "160", "170", "41", "51",
                                         "61", "71", "81", "91")
  )
#ed_statuses_code = list('Married' = 'M', "Single"='S', "Divorced"='D', "Widowed"='W')

POD_statuses <-
  c(
    "Inpatient",
    "Outpatient",
    "Dead on Arrival",
    "Home",
    "Hospice",
    "Nursing home/long term care",
    "Other/Unknown"
  )
POD_code <-
  c(
    "Inpatient" = 1,
    "Outpatient" = 2,
    "Dead on Arrival" = 3,
    "Home" = 4,
    "Hospice" = 5,
    "Nursing home/long term care" = 6,
    "Other/Unknown" = c(7,9)
  )

means_stat <-
  c(
    "Poisoning",
    "Hanging, strangulation, suffocation",
    "Drowning",
    "Firearm",
    "Explosive",
    "Smoke, burning, flames",
    "Stabbing",
    "Blunt force",
    "Jumping from a high place",
    "Hit by moving object",
    "Crashed motor vehicle",
    "Other"
  )

means_code <-
  c(
    "Poisoning" = c("X60", "X61", "X62", "X63", "X64", "X65", "X66", "X67",
                    "X68", "X69"),
    "Hanging, strangulation, suffocation" = "X70",
    "Drowning" = "X71",
    "Firearm" = c("X72", "X73", "X74"),
    "Explosive" = "X75",
    "Smoke, burning, flames" = c("X76", "X77"),
    "Stabbing" = "X78",
    "Blunt force" = "X79",
    "Jumping from a high place" = "X80",
    "Hit by moving object" = "X81",
    "Crashed motor vehicle" = "X82",
    "Other" = c("X83", "X84", "Y87")
  )

days <-
  c("Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday")

days_code <-
  c(
    "Sunday" = 1,
    "Monday"= 2,
    "Tuesday" = 3,
    "Wednesday" = 4,
    "Thursday" = 5,
    "Friday" = 6,
    "Saturday" = 7
  )

demographics <-
  c("Sex", "Age", "Race", "Ethnicity", "Marital Status", "Education")
dem_code = list(
  "Sex" = sexes,
  "Race" = races,
  "Ethnicity" = ethnicities,
  "Marital Status" = marital_statuses,
  "Education" = ed_statuses,
  "Means" = means_stat
)
death_details <- c("Place", "Day of Week", "Month", "Year", "Means")
chosen_factors <- c(demographics, death_details)

outcomes <-
  c("Suicide rate", "Number of suicides", "Percent by method")





########################################################################################################

# UI 

########################################################################################################

ui <- navbarPage(
  "U.S. Suicide Compiler",
  tabPanel("HOME",
           fluidPage(
             titlePanel("Welcome to the U.S. Suicide Compiler!"),
             fluidRow(
               column(
                 9,
                 "Suicide varies by population, but basic demographic
                                         questions remain to be answered. The U.S. Suicide Compiler
                                         brings together data on your specific demographic of interest,
                                         using data from the Centers for Disease Control and Prevention,
                                         to look at intra-population differences. For instance, age is a
                                         risk factor for White males, but not Black females. Our website
                                         lets you plot differences by any of 70+ variables of interest,
                                         including race, ethnicity, means of suicide, place of death,
                                         age, and a bunch more."
               )
             )
           )),
  tabPanel(
    "SELECT SAMPLE",
    fluidPage(
      titlePanel("First, select the groups to include in your sample."),
      fluidRow(
        column(
          9,
          "Later on, you'll do the actual comparison. You might look at how
                                  age relates to suicide rate, and how this relation varies by sex. But what if
                                  you're specifically interested in this potential sex difference in the Black
                                  population? Then, here, you'd select Black under the Demographic -> Race dropdown.
                                  By default, we'll assume you're interested in all people who died by suicide."
        )
      ),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "parameters",
            "What demographic variables do you want to filter by?",
            demographics,
            multiple = TRUE
          ),
          selectInput(
            "suicide_info",
            "What suicide details do you want to filter by?",
            death_details,
            multiple = TRUE
          ),
          selectInput(
            "x_vars",
            "What variable do you want on your x-axis?",
            chosen_factors,
            multiple = FALSE
          ),
          selectInput(
            "y_vars",
            "What variable do you want on your y-axis?",
            outcomes,
            multiple = FALSE
          ),
          actionButton("graph_gen", "Generate Graph", onclick=print("CLICKED"))
        ),
        mainPanel (
          column(
            4,
            tabsetPanel(
              id = "Age_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody("Age",
                           splitLayout(
                             numericInput(
                               "min-age",
                               "Min Age (0-120)",
                               value = 0,
                               min = 0,
                               max = 120,
                               step = 1,
                             ),
                             numericInput(
                               "max-age",
                               "Max Age (0-120)",
                               value = 120,
                               min = 0,
                               max = 120,
                               step = 1,
                             )
                           ))
            ),
            tabsetPanel(
              id = "Sex_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody("Sex", checkboxGroupInput("sex", "Sex of interest?", sexes))
            ),
            tabsetPanel(
              id = "Race_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody("Race", checkboxGroupInput("Race", "Race of interest?", races))
            ),
            tabsetPanel(
              id = "Ethnicity_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody(
                "Ethnicity",
                checkboxGroupInput("ethnicity", "Ethnic status of interest?", ethnicities)
              )
            ),
            tabsetPanel(
              id = "MaritalStatus_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody(
                "Marital Status",
                checkboxGroupInput(
                  "marital_status",
                  "Marital status of interest?",
                  marital_statuses
                )
              )
            ),
            tabsetPanel(
              id = "Education_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody(
                "Education",
                checkboxGroupInput(
                  "education_status",
                  "Educational status of interest?*",
                  ed_statuses
                )
              )
            )
          ),
          column(
            4,
            tabsetPanel(
              id = "DayOfWeek_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody(
                "Day of Week",
                checkboxGroupInput("day_of_week", "Day of week?", days)
              )
            ),
            tabsetPanel(
              id = "Place_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody(
                "Place",
                checkboxGroupInput("pod", "Place of death?", POD_statuses)
              )
            ),
            tabsetPanel(
              id = "Month_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody("Month", sliderInput(
                "mod",
                "Months?",
                value = c(1, 12),
                min = 1,
                max = 12
              ))
            ),
            tabsetPanel(
              id = "Year_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody("Year", splitLayout(
                numericInput(
                  "yod_start",
                  "Start Year",
                  value = 2009,
                  min = 2009,
                  max = 2019,
                  step = 1,
                ),
                numericInput(
                  "yod_end",
                  "End Year",
                  value = 2019,
                  min = 2009,
                  max = 2019,
                  step = 1,
                )
              ))
            ),
            tabsetPanel(
              id = "Means_tab",
              type = "hidden",
              tabPanelBody("null", ""),
              tabPanelBody("Means", checkboxGroupInput("means", "Means", means_stat))
            ),
          )
          
        )
      )
      
    )
  ),
  tabPanel("GRAPH VISUALIZATION",
           id="graph_tab",
           plotOutput("plot"))
)

# Define server logic
server <- function(input, output, session) {
  toListen <- reactive({
    list(input$parameters, input$suicide_info, input$graph_gen)
  })
  output$plot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
  
  observeEvent(toListen(), {
    
    for (i in chosen_factors) {
      if (i %in% input$suicide_info | i %in% input$parameters) {
        if (i == "Day of Week") {
          updateTabsetPanel(inputId = "DayOfWeek_tab", selected = i)
        } else if (i == "Marital Status") {
          updateTabsetPanel(inputId = "MaritalStatus_tab", selected = i)
        } else {
          updateTabsetPanel(inputId = paste(i, "_tab", sep = ""),
                            selected = i)
        }
      } else {
        if (i == "Day of Week") {
          updateTabsetPanel(inputId = "DayOfWeek_tab", selected = "null")
        } else if (i == "Marital Status") {
          updateTabsetPanel(inputId = "MaritalStatus_tab", selected = "null")
        } else {
          updateTabsetPanel(inputId = paste(i, "_tab", sep = ""),
                            selected = "null")
        }
      }
    }
  })
  
  
  
  
}

# filter(suicides, race_cat %in% race_code[input$race], sex %in% sex_code[input$sex],
#         age>=input$age[1], age<=input$age[2], hispanic_cat %in% ethnic_code[input$ethnicity])


# Run the application
shinyApp(ui = ui, server = server)
