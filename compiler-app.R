    # This is a Shiny web application. You can run the application by clicking
    # the 'Run App' button above.
    #
    # Find out more about building applications with Shiny here:
    #
    #    http://shiny.rstudio.com/
    #
# JOSEPHS PATH "C:\\rProjects\\suicide\\suicides2.0\\all_suicides.csv"
# RASHMIS PATH "/Users/rashmijha/Desktop/suicide project/VH8/all_suicides.csv"
#suicides <- read.csv("/Users/rashmijha/Desktop/suicide project/VH8/all_suicides.csv")

library(dplyr)
library(shiny)

races <- c("White", "Black", "American Indian / Alaskan Native (AIAN)",
           "Asian", "Mixed", "Pacific Islander")
sexes <- c("Male", "Female")
ethnicities <- c("Hispanic", "Non-Hispanic")
race_code = list('White'='white', 'Black'='black', 'American Indian / Alaskan Native (AIAN)'='aian',
                 'Asian'='asian', 'Mixed'='mixed', 'Pacific Islander'='pacific')
sex_code = list('Male'='M', 'Female'='F')
ethnic_code = list('Hispanic'='hispanic', 'Non-Hispanic'='non-hispanic')

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("U.S. Suicide Compiler"),
    
    # Sidebar with a selection input for demographic parameters
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("race", "Race of interest?", races, selected=races),
            checkboxGroupInput("sex", "Sex of interest?", sexes, selected=sexes),
            sliderInput("age", "Age range of interest?", value=c(1,120), min=1, max=120),
            checkboxGroupInput("ethnicity", "Ethnic status of interest?", ethnicities, selected=ethnicities),
        ),
        mainPanel(
            dataTableOutput("data")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$data <- renderDataTable({
        
        filter(suicides, race_cat %in% race_code[input$race], sex %in% sex_code[input$sex], 
               age>=input$age[1], age<=input$age[2], hispanic_cat %in% ethnic_code[input$ethnicity])
        })
}


# Run the application 
shinyApp(ui = ui, server = server)
