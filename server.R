library(shiny)
library(tidyverse)
library(googlesheets)
library(DT)
library(jsonlite)
library(purrr)

shinyServer(
  function(input, output, session) {
    source("helpers.R")
    
    if("cache.Rdata" %in% list.files()){
      notif_cache <- showNotification("Loading cache")
      load("cache.Rdata")
      removeNotification(notif_cache)
    }
    else{
      v <- reactiveValues(
        reviews = list(),
        data = list(),
        changes = list(),
        online = FALSE,
        lastSync = "Never",
        ID = 1,
        email = "none",
        firstRun = TRUE
      )
    }
    
    notif_ui <- showNotification("Building UI")
    
    latest_reviews <- reactive({
      notif_data <- showNotification("Constructing dataset")
      out <- v$reviews %>%
        bind_rows(v$changes) %>%
        filter(reviewer == v$email) %>%
        group_by(id) %>%
        filter(timestamp == max(timestamp)) %>%
        ungroup
      removeNotification(notif_data)
      out
    })
    
    tbl_data <- reactive({
      notif_tbl <- showNotification("Updating table")
      out <- v$reviews %>%
        bind_rows(v$changes) %>%
        group_by(id, reviewer) %>%
        filter(timestamp == max(timestamp)) %>%
        group_by(id) %>%
        summarise(Reviews = n(),
                  Status = tibble(reviewer, accept) %>%
                    filter(reviewer == v$email) %>%
                    pull(accept) %>% 
                    {if(length(.) == 0) "None" else .}
        ) %>%
        full_join(v$data, by = "id") %>%
        replace_na(list(Reviews = 0, Status = "None")) %>%
        mutate(similarity = fuzzyMatching(input$text_match, .)) %>%
        arrange(desc(similarity), Reviews, `Surname`)
      removeNotification(notif_tbl)
      out
    })
    
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
        notif_auth <- showNotification("Authenticating...")
        out <- gs_webapp_get_token(auth_code = pars$code)
        removeNotification(notif_auth)
        out
      }
    })
    
    output$sync <- renderUI({
      strong(paste0("Last synchronised: ", v$lastSync))
    })

    observe({
      if(!v$firstRun & input$btn_sync == 0 & !is.null(v$email)){
        return()
      }
      if (!is.null(access_token())) {
        notif_sync <- showNotification("Synchronising... Please wait", duration = NULL)
        isolate({
          ## Upload changes
          if(NROW(v$changes) > 0){
            gs_add_row(gs_key("1Jjq70cLXfMZj5mXwau_Zhbw-N0d34nrw3qGvoeL4DdQ"), ws = 2, input = v$changes)
            v$changes <- list()
          }
          
          ## Download data
          v$data <- gs_key("1Jjq70cLXfMZj5mXwau_Zhbw-N0d34nrw3qGvoeL4DdQ") %>% gs_read_csv(ws=1) %>% mutate(id = row_number())
          
          ## Download reviews
          v$reviews <- gs_key("1Jjq70cLXfMZj5mXwau_Zhbw-N0d34nrw3qGvoeL4DdQ") %>% gs_read_csv(ws=2) %>% tail(-1)
          
          v$email <- gs_user()$user$emailAddress
          v$firstRun <- FALSE
          v$lastSync <- Sys.time()
        })
        removeNotification(notif_sync)
      }
    })

    output$tbl_applicants <- DT::renderDataTable({
      if(length(v$data) > 0){
        ui_tbl_selector <- showNotification("Building table selector")
        out <- {if (input$show_personal == "Shown")
            tbl_data() %>%
            transmute(Entrant = paste(`First name`, `Surname`),
                      Reviews = Reviews, 
                      Status = Status
            )
            else
            tbl_data() %>% 
              transmute(Title = `Title (of tutorial)`,
                        Reviews = Reviews, 
                        Status = Status
              )
          } %>%
          datatable(rownames = FALSE, 
                    selection = list(mode = "single", selected = which((tbl_data()%>%pull(id)) == isolate(v$ID))),
                    style = "bootstrap", 
                    class = "hover",
                    options = list(sDom  = '<"top">irt<"bottom">p'))
        removeNotification(ui_tbl_selector)
        out
      }
      else{
        NULL
      }
    })
    
    observeEvent(input$tbl_applicants_rows_selected,{
      # Update
      v$ID <- tbl_data() %>% 
        filter(row_number() == input$tbl_applicants_rows_selected) %>%
        pull(id)
      
      print(tbl_data() %>% 
              filter(row_number() == input$tbl_applicants_rows_selected))
        
      output$abstract <- renderUI({
        if(is.null(input$tbl_applicants_rows_selected)){
          return(
            box(title = "Select an applicant from the sidebar to review their abstract",
                width = 12)
          )
        }
        applicant_data <- tbl_data() %>%
          filter(id == v$ID)
        tagList(
          box(
            title = applicant_data$`Title (of tutorial)`,
            formText(applicant_data$`Outline (provide a paragraph with topics to be covered and rough timing)`),
            hr(),
            formText("Keywords:", applicant_data$`Keywords (give us five)`),
            formText("Hands-on / Requires computer:", applicant_data$`Will the tutorial be hands-on, participants bring a computer?`)
          ),
          {if(input$show_personal == "Shown") 
            box(
              title = "Personal information",
              formText(applicant_data$`First name`, applicant_data$Surname),
              formText("Affiliation:", applicant_data$Affiliation),
              formText("Gender:", applicant_data$Gender),
              formText("Education:", applicant_data$Education),
              formText("Age:", applicant_data$Age)
            )
            else
              div(style = "display:none;")
          },
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
        review_data <- latest_reviews() %>%
          filter(id == v$ID)
        print(review_data)
        box(width = 12,
            title = "Evaluation",
            solidHeader = TRUE,
            status = "info", #ifelse(length(curInputs$accept)==0, "info", ifelse(any(curInputs$accept=="Accept"), "success", "danger")),
            column(2,
                   radioButtons("accept", 
                                label = "Decision", 
                                choices = c("Undecided", "Accept", "Reject"), 
                                selected = ifelse(length(review_data %>% pull(accept))==1, review_data %>% pull(accept), "Undecided")
                   ),
                   uiOutput("ui_save")
            ),
            column(10,
                   textAreaInput("comment",
                                 label = "Comments", 
                                 value = ifelse(length(review_data %>% pull(comment))==1, review_data %>% pull(comment), ""), 
                                 rows = 6
                  )
            )
        )
      })
      
      
      output$ui_save <- renderUI({
        actionLink(
          "save",
          box(
            p("Save", style="text-align: center;"),
            width = NULL,
            background = switch(input$accept,
                                Undecided = "aqua",
                                Accept = "green",
                                Reject = "red")
          )
        )
      })
    })
  
    
    observeEvent(input$btn_debug, {
      browser()
    })
    
    observeEvent(input$save, {
      v$changes <- v$changes %>%
        bind_rows(
          tibble(
            id = v$ID,
            timestamp = format(Sys.time(), tz="GMT"),
            reviewer = v$email,
            accept = input$accept,
            comment = input$comment
          )
        )
      if(input$net_mode == "Online"){
        notif_save <- showNotification("Uploading review.")
        gs_add_row(gs_key("1Jjq70cLXfMZj5mXwau_Zhbw-N0d34nrw3qGvoeL4DdQ"), ws = 2, input = v$changes)
        removeNotification(notif_save)
        v$reviews <- v$reviews %>%
          bind_rows(v$changes)
        v$changes <- list()
      }
    })
    
    # session$onFlushed(function(){
    #   updater$resume()
    # }, once = TRUE)
    
    onStop(function(){
      save(v, file = "cache.Rdata")
    })
    
    removeNotification(notif_ui)
  }
)