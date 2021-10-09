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

suicides <- read.csv("C:\\rProjects\\suicide\\suicides2.0\\all_suicides.csv")

races <- c("White", "Black", "American Indian / Alaskan Native (AIAN)",
           "Asian", "Mixed", "Pacific Islander")
sexes <- c("Male", "Female")
ethnicities <- c("Hispanic", "Non-Hispanic")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("U.S. Suicide Compiler"),
    
    # Sidebar with a selection input for demographic parameters
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("race", "Race of interest?", races),
            checkboxGroupInput("sex", "Sex of interest?", sexes),
            sliderInput("age", "Age range of interest?", value=c(1,120), min=1, max=120),
            checkboxGroupInput("ethnicity", "Ethnic status of interest?", ethnicities),
        ),
        mainPanel(
            dataTableOutput("data")
        )
    )
)

race_code = list('White'='white', 'American Indian / Alaskan Native (AIAN)'='aian',
                 'Asian'='asian', 'Mixed'='mixed', 'Pacific Islander'='pacific')
sex_code = list('Male'='M', 'Female'='F')
ethnic_code = list('Hispanic'='hispanic', 'Non-Hispanic'='non-hispanic')
race_code['White']

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    race_code = list('White'='white', 'American Indian / Alaskan Native (AIAN)'='aian',
                     'Asian'='asian', 'Mixed'='mixed', 'Pacific Islander'='pacific')
    sex_code = list('Male'='M', 'Female'='F')
    ethnic_code = list('Hispanic'='hispanic', 'Non-Hispanic'='non-hispanic')
    output$data <- renderDataTable({
        head(mtcars)
    })
    output$data <- renderDataTable({
        head(filter(suicides, race_cat==race_code[input$race], sex==sex_code[input$sex],
               age>=input$age[1], age<=input$age[2], hispanic_cat==ethnic_code[input$ethnicity]))
    })
}
nrow(filter(suicides, race_cat==race_code['White']))
nrow(filter(suicides, race_cat=='aian'))
# Run the application 
shinyApp(ui = ui, server = server)
