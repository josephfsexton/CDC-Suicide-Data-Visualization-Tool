---
output: html_document
runtime: shiny
---
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

#suicides <- read.csv("C:\\rProjects\\suicide\\suicides2.0\\all_suicides.csv")

races <- c("White", "Black", "American Indian / Alaskan Native (AIAN)",
           "Asian", "Mixed", "Pacific Islander")
sexes <- c("Male", "Female")
ethnicities <- c("Hispanic", "Non-Hispanic")
race_code = list('White'='white', 'American Indian / Alaskan Native (AIAN)'='aian',
                 'Asian'='asian', 'Mixed'='mixed', 'Pacific Islander'='pacific')
sex_code = list('Male'='M', 'Female'='F')
ethnic_code = list('Hispanic'='hispanic', 'Non-Hispanic'='non-hispanic')

demographics <- c("Sex", "Age", "Race", "Ethnicity", "Marital Status", "Education")
death_details <- c("Place", "Day of Week", "Month", "Year", "Means")


ui <- navbarPage("U.S. Suicide Compiler",
                 tabPanel("HOME",
                          fluidPage(
                              titlePanel("Welcome to the U.S. Suicide Compiler!"),
                              fluidRow(
                                  column(9, "Suicide varies by population, but basic demographic
                                         questions remain to be answered. The U.S. Suicide Compiler
                                         brings together data on your specific demographic of interest,
                                         using data from the Centers for Disease Control and Prevention,
                                         to look at intra-population differences. For instance, age is a
                                         risk factor for White males, but not Black females. Our website
                                         lets you plot differences by any of 70+ variables of interest,
                                         including race, ethnicity, means of suicide, place of death,
                                         age, and a bunch more.")
                              )
                          )
                 ),
                 tabPanel("SELECT SAMPLE",
                          fluidPage(
                              titlePanel("First, select the groups to include in your sample."),
                              fluidRow(
                                  column(9, "Later on, you'll do the actual comparison. You might look at how
                                  age relates to suicide rate, and how this relation varies by sex. But what if
                                  you're specifically interested in this potential sex difference in the Black
                                  population? Then, here, you'd select Black under the Demographic -> Race dropdown.
                                  
                                  By default, we'll assume you're interested in all people who died by suicide."
                                  )
                              ),
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("demo", "What demographic variables do you want to filter by?",
                                                  demographics, multiple=TRUE),
                                      selectInput("suicide_info", "What suicide details do you want to filter by?",
                                                  death_details, multiple=TRUE)
                                  ),
                                  mainPanel(
                                      dataTableOutput("data")
                                  )
                              )
                          )
                 )
)
# Application title
#tabPanel("Download Your Dataset", fluidPage(
#    titlePanel("Select your parameters and download your dataset."),
#    sidebarPanel(
#        checkboxGroupInput("race", "Race of interest?", races, selected=races),
#        checkboxGroupInput("sex", "Sex of interest?", sexes, selected=sexes),
#        sliderInput("age", "Age range of interest?", value=c(1,120), min=1, max=120),
#        checkboxGroupInput("ethnicity", "Ethnic status of interest?", ethnicities, selected=ethnicities)
#    ),
#    mainPanel(
#        dataTableOutput("data")
#    ))
#),
#tabPanel("Data Visualization", 1)
#)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$data <- renderDataTable({
        filter(suicides, race_cat %in% race_code[input$race], sex %in% sex_code[input$sex], 
               age>=input$age[1], age<=input$age[2], hispanic_cat %in% ethnic_code[input$ethnicity])
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
