library(shiny)
library(shinydashboard)
library(googlesheets)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "useR 2018 Abstract Review",
      titleWidth = "450px"
    ),
    dashboardSidebar(
      width = "450px",
      uiOutput("auth"),
      uiOutput("sync"),
      radioButtons("net_mode", "Net Mode", c("Online", "Offline"), inline = TRUE),
      radioButtons("admin_mode", "Operation Mode", c("Reviewer", "Administrator"), inline = TRUE),
      sidebarMenu(
        tags$li(
          actionLink("btn_debug",
                     style = "margin: 0;",
                     label = NULL,
                     class = "",
                     icon("bug"),
                     span("Debug")
          )
        ),
        tags$li(
          actionLink("btn_sync",
                     style = "margin: 0;",
                     label = NULL,
                     class = "",
                     icon("refresh"),
                     span("Synchronise")
          )
        )
      ),
      # uiOutput("n_entries"),
      hr(),
      fluidRow(
        column(4, radioButtons("filter_decisions", "Decision filter", c("On", "Off"))),
        column(4, sliderInput("slider_reviews", "Total reviews", min = 0, max = 1, value = c(0, 2), step=1)),
        column(4, checkboxGroupInput("pres_format", "Presentation formats", choices = c("presentation", "poster", "lightning talk"), selected = c("presentation", "poster", "lightning talk")))
      ),
      textInput("text_match", "Fuzzy text sorting"),
      DT::dataTableOutput("tbl_applicants")
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      uiOutput("abstract"),
      uiOutput("feedback"),
      uiOutput("review")
    )
  )
)
