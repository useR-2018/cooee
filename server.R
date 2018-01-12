library(shiny)
library(tidyverse)
library(googlesheets)
library(DT)
library(jsonlite)

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
        lastSync = "Never",
        firstRun = TRUE
      )
    }
    
    curInputs <- reactiveValues(
      accept = character(0L),
      comment = ""
    )
    
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
      strong(paste0("Last synchronised: ", v$lastSync))
    })

    observe({
      if(!v$firstRun & input$btn_sync == 0){
        return()
      }
      invalidateLater(300000)
      if (!is.null(access_token())) {
        v$reviews <- gs_key("1Jjq70cLXfMZj5mXwau_Zhbw-N0d34nrw3qGvoeL4DdQ") %>% gs_read_csv(ws=2)
        v$data <- isolate(v$reviews) %>%
          group_by(id) %>%
          summarise(Reviews = n()) %>%
          full_join(gs_key("1Jjq70cLXfMZj5mXwau_Zhbw-N0d34nrw3qGvoeL4DdQ") %>% gs_read_csv(ws=1) %>% mutate(id = row_number()),
                    by = "id") %>%
          replace_na(list(Reviews = 0)) %>%
          arrange(Reviews, `Surname`)
        v$reviews <- v$reviews %>%
          filter(reviewer == gs_user()$user$emailAddress) #%>%
          # group_by(id) %>%
          # filter(Timestamp == max(Timestamp))
        
        if(NROW(v$changes) > 0){
          gs_add_row(gs_key("1Jjq70cLXfMZj5mXwau_Zhbw-N0d34nrw3qGvoeL4DdQ"), ws = 2, input = v$changes)
          isolate(v$changes <- list())
        }
        
        v$firstRun <- FALSE
        v$lastSync <- Sys.time()
      }
    })

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
    
    observeEvent(input$tbl_applicants_rows_selected,{
      # Save
      if(length(curInputs$accept) > 0){
        v$changes <- v$changes %>%
          bind_rows(
            data.frame(
              id = v$data %>% 
                filter(row_number() == input$tbl_applicants_rows_selected) %>%
                pull(id),
              Timestamp = format(Sys.time(), tz="GMT"),
              reviewer = gs_user()$user$emailAddress,
              accept = input$accept,
              comment = input$comment
            )
          )
      }
      
      output$abstract <- renderUI({
        if(is.null(input$tbl_applicants_rows_selected)){
          return(
            box(title = "Select an applicant from the sidebar to review their abstract",
                width = 12)
          )
        }
        applicant_data <- v$data %>%
          filter(row_number() == input$tbl_applicants_rows_selected)
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
        review_data <- v$reviews %>%
          bind_rows(v$changes) %>%
          filter(id == input$tbl_applicants_rows_selected)
        curInputs$accept <- review_data %>% pull(accept) %>% as.character
        curInputs$comment <- review_data %>% pull(comment)
        box(width = 12,
            title = "Evaluation",
            solidHeader = TRUE,
            status = "info", #ifelse(length(curInputs$accept)==0, "info", ifelse(any(curInputs$accept=="Accept"), "success", "danger")),
            column(2,
                   radioButtons("accept", label = "Decision", choices = c("Accept", "Reject"), selected = curInputs$accept)#,
                   # actionLink(
                   #   "save",
                   #   box(
                   #     p("Save", style="text-align: center;"),
                   #     width = NULL,
                   #     background = ifelse(any(input$accept=="Accept"), "green", "red")
                   #   )
                   #       )
            ),
            column(10,
                   textAreaInput("comment", label = "Comments", value = ifelse(length(curInputs$comment)==1, curInputs$comment, ""), rows = 6)
            )
        )
      })
    })
    
    
    observeEvent(input$btn_debug, {
      browser()
    })
    
    # observeEvent(input$accept,{
    #   curInputs$accept <- input$accept
    # })
    # observeEvent(input$comment,{
    #   curInputs$comment <- input$comment
    # })
    
    # session$onFlushed(function(){
    #   updater$resume()
    # }, once = TRUE)
    
    onStop(function(){
      save(v, file = "cache.Rdata")
    })
  }
)