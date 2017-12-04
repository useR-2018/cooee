library(shiny)
library(shinydashboard)
library(googlesheets)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "useR 2018 Application Review",
      titleWidth = "33vw"
    ),
    dashboardSidebar(
      width = "33vw",
      uiOutput("auth"),
      uiOutput("sync"),
      sidebarSearchForm("txt_search", "btn_search"),
      DT::dataTableOutput("tbl_applicants"),
      actionButton("btn_debug", "debug")
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      )
    )
  )
)
