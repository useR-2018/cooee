library(shiny)
library(dplyr)
library(googlesheets)
library(DT)
library(jsonlite)
library(tidyr)
library(purrr)

shinyServer(
  function(input, output, session) {
    
    source("helpers.R")
    
    if("cache.Rdata" %in% list.files()){
      load("cache.Rdata")
    }
    else{
      v <- reactiveValues(
        reviews = list(),
        data = list(),
        changes = list(),
        online = FALSE,
        lastSync = "Never"
      )
    }
    
    output$auth <- renderUI({
      if (is.null(isolate(access_token()))) {
        sidebarUserPanel(
          span("Currently not authenticated"),
          subtitle = a(icon("sign-in"), "Login", 
                       href = gs_webapp_auth_url(access_type = "offline"))
        )
      } else {
        sidebarUserPanel(
          span("Authenticated as", gs_user()$user$displayName),
          subtitle = a(icon("sign-out"), "Logout", 
                       href = getOption("googlesheets.webapp.redirect_uri")),
          image = fromJSON(paste0("http://picasaweb.google.com/data/entry/api/user/", gs_user()$user$emailAddress, "?alt=json"))$entry$`gphoto$thumbnail`$`$t`
        )
      }
    })
    
    ## Get auth code from return URL
    access_token  <- reactive({
      ## gets all the parameters in the URL. The auth code should be one of them.
      pars <- parseQueryString(session$clientData$url_search)
      if (length(pars$code) > 0) {
        ## extract the authorization code
        gs_webapp_get_token(auth_code = pars$code)
      }
    })
    
    output$sync <- renderUI({
      h4("Last synchronised... %TODO")
    })
    
    observe(
      if (!is.null(isolate(access_token()))) {
        v$reviews <- gs_key("1Jjq70cLXfMZj5mXwau_Zhbw-N0d34nrw3qGvoeL4DdQ") %>% gs_read_csv(ws=2)
        v$data <- v$reviews %>%
          group_by(id) %>%
          summarise(Reviews = n()) %>%
          full_join(gs_key("1Jjq70cLXfMZj5mXwau_Zhbw-N0d34nrw3qGvoeL4DdQ") %>% gs_read_csv(ws=1) %>% mutate(id = row_number()),
                    by = "id") %>%
          replace_na(list(Reviews = 0)) %>%
          arrange(Reviews, `Surname`)
      }
      else{
        v$data <- NULL
        v$reviews <- NULL
      }
    )
    
    output$tbl_applicants <- DT::renderDataTable({
      if(length(v$data) > 0){
        v$data %>%
          transmute(Entrant = paste(`First name`, `Surname`), Reviews = Reviews) %>%
          datatable(rownames = FALSE, selection = "single", style = "bootstrap", class = "hover")
      }
      else{
        NULL
      }
    })
    
    output$abstract <- renderUI({
      if(is.null(input$tbl_applicants_rows_selected)){
        return()
      }
      applicant_data <- v$data[input$tbl_applicants_rows_selected, ]
      tagList(
        box(
          title = applicant_data$`Title (of tutorial)`,
          formText(applicant_data$`Outline (provide a paragraph with topics to be covered and rough timing)`),
          hr(),
          formText("Keywords:", applicant_data$`Keywords (give us five)`),
          formText("Hands-on / Requires computer:", applicant_data$`Will the tutorial be hands-on, participants bring a computer?`)
        ),
        box(
          title = "Personal information",
          formText(applicant_data$`First name`, applicant_data$Surname),
          formText("Affiliation:", applicant_data$Affiliation),
          formText("Gender:", applicant_data$Gender),
          formText("Education:", applicant_data$Education),
          formText("Age:", applicant_data$Age)
        ),
        box(
          title = "Equipment",
          formText(applicant_data$`Equipment (what do you and participants need)`)
        ),
        box(
          title = "Participants",
          formText("R background:", applicant_data$`R background of participants`),
          formText("Other background:",applicant_data$`Other background needed`),
          formText("Target audience:", applicant_data$`Target audience`)
        ),
        box(
          title = "Motivation",
          formText(applicant_data$`Motivation (why do you think this would be a good tutorial for participants of useR! 2018)`)
        ),
        box(
          title = "Other information",
          formText(applicant_data$`Anything else you would like to tell us`)
        )
      )
    })
    
    
    output$review <- renderUI({
      if(is.null(input$tbl_applicants_rows_selected)){
        return()
      }
      box(width = 12,
          title = "Evaluation",
          solidHeader = TRUE,
          status = ifelse(any(input$accept=="Accept"), "success", "danger"),
          column(2,
                 radioButtons("accept", label = "Decision", choices = c("Accept", "Reject"), selected = input$accept)),
          column(10,
                 textAreaInput("comment", label = "Comments", rows = 10)
                 )
      )
    })
    
    observeEvent(input$btn_debug, {
      browser()
    })
    
    
    onStop(function(){
      save(v, file = "cache.Rdata")
    })
  }
)