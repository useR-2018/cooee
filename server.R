library(shiny)
library(tidyverse)
library(googlesheets)
library(DT)
library(jsonlite)
library(purrr)

shinyServer(
  function(input, output, session) {
    source("helpers.R")
    
    if("cache_scholarships.Rdata" %in% list.files()){
      notif_cache <- showNotification("Loading cache")
      load("cache_scholarships.Rdata")
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
                  Rejects = sum(accept == "No"),
                  Status = {if(input$admin_mode == "Reviewer") 
                              tibble(reviewer, accept) %>%
                              filter(reviewer == v$email) %>%
                              pull(accept) %>% 
                              {if(length(.) == 0) "None" else .}
                            else
                              tibble(reviewer, accept) %>%
                              pull(accept) %>% 
                              {if(length(.) == 0) "None" else round(mean(recode(., "Yes" = 1, "Maybe" = 0, "No" = -1)), 2)} %>%
                              as.numeric
                            }
        ) %>%
        full_join(v$data, by = "id") %>%
        replace_na(list(Reviews = 0, Status = ifelse(input$admin_mode == "Reviewer", "None", 0), Rejects = 0)) %>%
        mutate(similarity = fuzzyMatching(input$text_match, .)) %>%
        arrange(desc(similarity), Reviews)
      
      if(input$filter_rejections == "On"){
        out <- out %>%
          filter(Rejects < 2)
      }
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
            gs_add_row(gs_key("1YtukfSlAy56hjrMSFGNGnbQbT1WsQ5NBV0_etfRj_xQ"), ws = 2, input = v$changes)
            v$changes <- list()
          }
          
          ## Download data
          v$data <- gs_key("1YtukfSlAy56hjrMSFGNGnbQbT1WsQ5NBV0_etfRj_xQ") %>% gs_read_csv(ws=1) %>% mutate(id = row_number())
          
          ## Download reviews
          v$reviews <- gs_key("1YtukfSlAy56hjrMSFGNGnbQbT1WsQ5NBV0_etfRj_xQ") %>% gs_read_csv(ws=2) %>% tail(-1)
          
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
        out <- tbl_data() %>% 
              transmute(Name = Name,
                        Reviews = Reviews, 
                        Status = Status
              ) %>%
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
            width = 6,
            title = "Tell us a little about yourself, especially about your R and open source or open data activities.",
            formText(applicant_data$`Tell us a little about yourself, especially about your R and open source or open data activities.`)
          ),
          box(
            width = 6,
            title = "Why are you applying for a scholarship?",
            formText(applicant_data$`Why are you applying for a scholarship?`)
          ),
          box(
            width = 6,
            title = "Tell us why attending useR! would help you, and impact the community",
            formText(applicant_data$`Tell us why attending useR! would help you, and impact the community`)
          ),
          box(
            width = 6,
            title = applicant_data$`Name`,
            formText("Disadvantaged:", applicant_data$`Do you identify with an under-represented,  minority, socially or geographically disadvantaged group? Provide some details.`),
            formText("Travel:", applicant_data$`Where would you be traveling from and to?`),
            formText("R Ladies:", applicant_data$`I would like to be considered for an R Ladies Australia scholarship`)
          )
        )
      })
      
      
      output$review <- renderUI({
        if(is.null(input$tbl_applicants_rows_selected) | input$admin_mode == "Administrator"){
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
                                choices = c("Yes", "Maybe", "No"), 
                                selected = ifelse(length(review_data %>% pull(accept))==1, review_data %>% pull(accept), "Maybe")
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
                                Yes = "green",
                                Maybe = "light-blue",
                                No = "red")
          )
        )
      })
      
      
      output$feedback <- renderUI({
        if(is.null(input$tbl_applicants_rows_selected) | input$admin_mode == "Reviewer"){
          return()
        }
        
        review_data <- v$reviews %>%
          bind_rows(v$changes) %>%
          group_by(id, reviewer) %>%
          filter(timestamp == max(timestamp)) %>%
          ungroup %>%
          filter(id == v$ID)
        
        print(review_data)
        
        review_data %>%
          split(seq_len(NROW(.))) %>% 
          map(~ box(width = 6,
                    title = .$reviewer,
                    background = switch(.$accept,
                                        Yes = "green",
                                        Maybe = "light-blue",
                                        No = "red"),
                    formText(.$accept, ": ", .$comment))) %>% 
          do.call("tagList", .) %>%
          fluidRow()
      })
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
        gs_add_row(gs_key("1YtukfSlAy56hjrMSFGNGnbQbT1WsQ5NBV0_etfRj_xQ"), ws = 2, input = v$changes)
        removeNotification(notif_save)
        v$reviews <- v$reviews %>%
          bind_rows(v$changes)
        v$changes <- list()
      }
    })
    

    # observeEvent(input$btn_debug, {
    #  browser()
    # })
    
    onStop(function(){
      save(v, file = "cache_scholarships.Rdata")
    })
    
    removeNotification(notif_ui)
  }
)
