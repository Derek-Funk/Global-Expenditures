#library to deploy app: rsconnect
#command to deploy app: deployApp("C:\\Users\\derek.funk\\Desktop\\MSDS\\2019 Spring\\Visualization\\Individual Project")

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Global Expenditures"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Preface",
        tabName = "preface",
        icon = icon("globe")
      ),
      menuItem(
        text = "Data Source",
        tabName = "data",
        icon = icon("download")
      ),
      menuItem(
        text = "Education",
        tabName = "education",
        icon = icon("school")
      ),
      menuItem(
        text = "Healthcare",
        tabName = "healthcare",
        icon = icon("briefcase-medical")
      ),
      menuItem(
        text = "Military",
        tabName = "military",
        icon = icon("skull")
      ),
      menuItem(
        text = "2015 Comparisons",
        tabName = "comparisons",
        icon = icon("balance-scale")
      ),
      menuItem(
        text = "Contact",
        tabName = "contact",
        icon = icon("at")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "preface",
        includeHTML(path = "www/preface.html")
      ),
      tabItem(
        tabName = "data",
        includeHTML(path = "www/dataSources.html")
      ),
      tabItem(
        tabName = "education",
        tabsetPanel(
          id = "education",
          tabPanel(
            title = "START", br(),
            actionButton(inputId = "START", label = "Generate Plots"),
            htmlOutput("initialBlank")
          ),
          tabPanel(
            title = "Snapshot",
            icon = icon("image"),
            htmlOutput("education_snapshot")
          ),
          tabPanel(
            title = "Total Spending",
            icon = icon("dollar-sign"),
            htmlOutput("education_total")
          ),
          tabPanel(
            title = "Spending %",
            icon = icon("chart-pie"),
            htmlOutput("education_percentOfExpenses")
          ),
          tabPanel(
            title = "Per Capita",
            icon = icon("users"),
            htmlOutput("education_perCapita")
          )
        )
      ),
      tabItem(
        tabName = "healthcare",
        tabsetPanel(
          tabPanel(
            title = "Snapshot",
            icon = icon("image"),
            htmlOutput("health_snapshot")
          ),
          tabPanel(
            title = "Total Spending",
            icon = icon("dollar-sign"),
            htmlOutput("health_total")
          ),
          tabPanel(
            title = "Spending %",
            icon = icon("chart-pie"),
            htmlOutput("health_percentOfExpenses")
          ),
          tabPanel(
            title = "Per Capita",
            icon = icon("users"),
            htmlOutput("health_perCapita")
          )
        )
      ),
      tabItem(
        tabName = "military",
        tabsetPanel(
          tabPanel(
            title = "Snapshot",
            icon = icon("image"),
            htmlOutput("military_snapshot")
          ),
          tabPanel(
            title = "Total Spending",
            icon = icon("dollar-sign"),
            htmlOutput("military_total")
          ),
          tabPanel(
            title = "Spending %",
            icon = icon("chart-pie"),
            htmlOutput("military_percentOfExpenses")
          ),
          tabPanel(
            title = "Per Capita",
            icon = icon("users"),
            htmlOutput("military_perCapita")
          )
        )
      ),
      tabItem(
        tabName = "comparisons",
        tabsetPanel(
          tabPanel(
            title = "GDP vs. Expenses",
            htmlOutput("comparisons_expensesVsGdp2015")
          ),
          tabPanel(
            title = "Per Capita Comparisons",
            htmlOutput("comparisons_perCapita2015")
          ),
          tabPanel(
            title = "Expenses Breakdown",
            htmlOutput("comparisons_expensesBreakdown2015")
          )
        )
      ),
      tabItem(
        tabName = "contact",
        HTML("
          My name is Derek Funk. I currently work at the low-code software company Appian
          in Reston, Virginia as a software consultant. I am also a student at The
          George Washington University's MS in Data Science Program, where I hope to
          graduate by the summer of 2020.<br><br>
          work email: derek.funk@appian.com<br>
          school email: derek_funk@gwu.com<br>
          personal email: dfunk0923@gmail.com
        ")
      )
    )
  )
)

server <- function(input, output, session) {
  # observeEvent(
  #   input$initialBlank,
  #   hideTab("initialBlank", "education_snapshot")  
  # )
  
  observeEvent(
    input$START,
    updateTabsetPanel(session, "education", selected = "Snapshot")  
  )
  
  
  
  output$initialBlank <- renderUI(
    expr = {
      includeHTML("www/initialBlank.html")
    }
  )
  
  output$education_snapshot <- renderUI(
    expr = {
      includeHTML("www/education/education_snapshot.html")
    }
  )

  output$education_total <- renderUI(
    expr = {
      includeHTML("www/education/education_total.html")
    }
  )

  output$education_percentOfExpenses <- renderUI(
    expr = {
      includeHTML("www/education/education_percentOfExpenses.html")
    }
  )
  
  output$education_perCapita <- renderUI(
    expr = {
      includeHTML("www/education/education_perCapita.html")
    }
  )
  
  output$health_snapshot <- renderUI(
    expr = {
      includeHTML("www/health/health_snapshot.html")
    }
  )
  
  output$health_total <- renderUI(
    expr = {
      includeHTML("www/health/health_total.html")
    }
  )
  
  output$health_percentOfExpenses <- renderUI(
    expr = {
      includeHTML("www/health/health_percentOfExpenses.html")
    }
  )
  
  output$health_perCapita <- renderUI(
    expr = {
      includeHTML("www/health/health_perCapita.html")
    }
  )
  
  output$military_snapshot <- renderUI(
    expr = {
      includeHTML("www/military/military_snapshot.html")
    }
  )
  
  output$military_total <- renderUI(
    expr = {
      includeHTML("www/military/military_total.html")
    }
  )
  
  output$military_percentOfExpenses <- renderUI(
    expr = {
      includeHTML("www/military/military_percentOfExpenses.html")
    }
  )
  
  output$military_perCapita <- renderUI(
    expr = {
      includeHTML("www/military/military_perCapita.html")
    }
  )
  
  output$comparisons_expensesVsGdp2015 <- renderUI(
    expr = {
      includeHTML("www/comparisons/comparisons_expensesVsGdp2015.html")
    }
  )
  
  output$comparisons_perCapita2015 <- renderUI(
    expr = {
      includeHTML("www/comparisons/comparisons_perCapita2015.html")
    }
  )
  
  output$comparisons_expensesBreakdown2015 <- renderUI(
    expr = {
      includeHTML("www/comparisons/comparisons_expensesBreakdown2015.html")
    }
  )
}

shinyApp(ui, server)