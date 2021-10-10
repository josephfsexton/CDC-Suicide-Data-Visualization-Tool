# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# JOSEPHS PATH "C:\\rProjects\\suicide\\suicides2.0\\all_suicides.csv"
# RASHMIS PATH "/Users/rashmijha/Desktop/suicide project/VH8/all_suicides.csv"
# suicides <- read.csv("https://drive.google.com/uc?id=1B190IntsI3pLGygAvRmwkq3DMYjHy1cI")

library(shiny)
library(dplyr)
library(shinyjs)

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
    "High school educated (1-4 years)",
    "College educated (1-4 years)"
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
    "Crashing motor vehicle",
    "Other"
  )

days <-
  c("Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday")

demographics <-
  c("Sex", "Age", "Race", "Ethnicity", "Marital Status", "Education")
dem_code = list(
  "Sex" = sexes,
  "Race" = races,
  "Ethnicity" = ethnicities,
  "Marital Status" = marital_statuses,
  "Education",
  "Means"
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
            "x-vars",
            "What variable do you want on your x-axis?",
            chosen_factors,
            multiple = FALSE
          ),
          selectInput(
            "y-vars",
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
                  "yod-start",
                  "Start Year",
                  value = 2009,
                  min = 2009,
                  max = 2019,
                  step = 1,
                ),
                numericInput(
                  "yod-end",
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
  tabPanel(id="graph_tab", type="hidden", tabPanelBody("null", ""), tabPanelBody("GraphO", "TEST GRPAH"))
)

generateGraph <- function(input){
  print("YOU HAVE ENTERED THIS FUNCTOI")
  updateTabsetPanel(inputId = "graph_tab", selected = "GraphO")
  
}

# Define server logic
server <- function(input, output, session) {
  toListen <- reactive({
    list(input$parameters, input$suicide_info, input$graph_gen)
  })
  
  observeEvent(toListen(), {
    onclick("graph_gen", generateGraph())
    
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
  
  onclick("graph_gen", generateGraph())
  
  
  
  
}

# filter(suicides, race_cat %in% race_code[input$race], sex %in% sex_code[input$sex],
#         age>=input$age[1], age<=input$age[2], hispanic_cat %in% ethnic_code[input$ethnicity])


# Run the application
shinyApp(ui = ui, server = server)
