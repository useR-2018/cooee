library(shiny)
library(tidyverse)
library(googlesheets)
library(DT)
library(jsonlite)
library(purrr)

shinyServer(
  function(input, output, session) {
    source("helpers.R")
    
    if("cache_abstracts.Rdata" %in% list.files()){
      notif_cache <- showNotification("Loading cache")
      load("cache_abstracts.Rdata")
      if(exists("v")){
        if(is.reactivevalues(v)){
          # Update cache format
          cache <- isolate(reactiveValuesToList(v))
        }
      }
      v <- do.call(reactiveValues, cache)
      
      removeNotification(notif_cache)
    }
    else{
      v <- reactiveValues(
        data = list(),
        reviews = list(),
        decisions = list(),
        changes = list(reviews = list(), decisions = list()),
        online = FALSE,
        lastSync = "Never",
        ID = 1,
        email = "none",
        firstRun = TRUE
      )
    }
    
    # Update old caches for new structure
    observe({
      if(is.null(v$decisions)){
        v$decisions <- list()
        v$changes <- list(reviews = v$changes, decisions = list())
        v$firstRun <- TRUE
      }
    })
    
    notif_ui <- showNotification("Building UI")
    
    latest_reviews <- reactive({
      notif_data <- showNotification("Constructing dataset")
      out <- v$reviews %>%
        as_tibble %>%
        bind_rows(v$changes$reviews) %>%
        filter(reviewer == v$email) %>%
        group_by(id) %>%
        filter(timestamp == max(timestamp)) %>%
        ungroup
      removeNotification(notif_data)
      out
    })
    
    latest_decisions <- reactive({
      notif_data <- showNotification("Constructing dataset")
      out <- v$decisions %>%
        as_tibble %>%
        bind_rows(v$changes$decisions) %>%
        group_by(id) %>%
        filter(timestamp == max(timestamp)) %>%
        ungroup
      removeNotification(notif_data)
      out
    })
    
    
    tbl_data <- reactive({
      notif_tbl <- showNotification("Updating table")
      out <- v$reviews %>%
        bind_rows(v$changes$reviews) %>%
        group_by(id, reviewer) %>%
        filter(timestamp == max(timestamp)) %>%
        group_by(id) %>%
        summarise(Reviews = n(),
                  Rejects = sum(accept == "Sorry"),
                  Status = {if(input$admin_mode == "Reviewer") 
                              tibble(reviewer, accept) %>%
                              filter(reviewer == v$email) %>%
                              pull(accept) %>% 
                              {if(length(.) == 0) "None" else .}
                            else
                              tibble(reviewer, accept) %>%
                              pull(accept) %>% 
                              {if(length(.) == 0) "None" else round(mean(recode(., "Bloody ripper" = 2, "Beaut" = 1, "Okey-dokey" = 0, "Sorry" = -1)), 2)} %>%
                              as.numeric
                            }
        ) %>%
        full_join(v$data %>%
                    rowwise %>%
                    mutate(`Preferred format (choose 1, or more if you have no preference)` = list(strsplit(`Preferred format (choose 1, or more if you have no preference)`, ",")[[1]] %>% trimws)) %>%
                    ungroup
                  , by = "id") %>%
        replace_na(list(Reviews = 0, Status = ifelse(input$admin_mode == "Reviewer", "None", 0), Rejects = 0)) %>%
        mutate(similarity = fuzzyMatching(input$text_match, .)) %>%
        arrange(desc(similarity), Reviews)
      
      removeNotification(notif_tbl)
      out
    })
    
    tbl_filtered_data <- reactive({
      notif_tbl <- showNotification("Filtering table")
      out <- tbl_data() %>% 
        filter(between(Reviews, input$slider_reviews[1], input$slider_reviews[2])) %>%
        rowwise %>%
        filter(all(`Preferred format (choose 1, or more if you have no preference)` %in% input$pres_format)) %>%
        ungroup
      if(input$filter_decisions == "On"){
        out <- out %>%
          anti_join(v$decisions %>%
                      bind_rows(v$changes$decisions) %>%
                      bind_rows(tibble(id = integer())), # Make sure that there exists an id variable
                    by = "id"
          )
      }
      removeNotification(notif_tbl)
      out
    })
    
    uploadChanges <- function(changes){
      notif_save <- showNotification("Uploading review.")
      if(NROW(changes$reviews) > 0){
        gs_add_row(gs_key("11p2FCo0ZNpbovVb9u55wm7mjOpM2aOrdJCT1ohnhYC8"), ws = 2, input = changes$reviews)
        changes$reviews <- list()
      }
      if(NROW(changes$decisions) > 0){
        gs_add_row(gs_key("11p2FCo0ZNpbovVb9u55wm7mjOpM2aOrdJCT1ohnhYC8"), ws = 3, input = changes$decisions)
        changes$decisions <- list()
      }
      removeNotification(notif_save)
      return(changes)
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
          uploadChanges(v$changes)
          
          ## Download data
          v$data <- gs_key("11p2FCo0ZNpbovVb9u55wm7mjOpM2aOrdJCT1ohnhYC8") %>% gs_read_csv(ws=1) %>% mutate(id = seq_len(NROW(.)))
          
          ## Download reviews
          v$reviews <- gs_key("11p2FCo0ZNpbovVb9u55wm7mjOpM2aOrdJCT1ohnhYC8") %>% gs_read_csv(ws=2) %>% tail(-1)
          
          ## Download decisions
          v$decisions <- gs_key("11p2FCo0ZNpbovVb9u55wm7mjOpM2aOrdJCT1ohnhYC8") %>% gs_read_csv(ws=3) %>% tail(-1)
          
          v$email <- gs_user()$user$emailAddress
          v$firstRun <- FALSE
          v$lastSync <- Sys.time()
        })
        removeNotification(notif_sync)
      }
    })
    
    observe({
      if(length(v$reviews) > 0){
        dt <- tbl_data()
        updateSliderInput(session, "slider_reviews", min = min(dt$Reviews), max = max(dt$Reviews))
      }
    })

    output$tbl_applicants <- DT::renderDataTable({
      if(NROW(v$data) > 0){
        ui_tbl_selector <- showNotification("Building table selector")
        
        out <- tbl_filtered_data() %>% 
          transmute(Title = `Title of presentation`,
                    Reviews = Reviews, 
                    Status = Status
          ) %>%
          datatable(rownames = FALSE, 
                    selection = list(mode = "single", selected = which((tbl_filtered_data()%>%pull(id)) == isolate(v$ID))),
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
      v$ID <- tbl_filtered_data() %>% 
        filter(row_number() == input$tbl_applicants_rows_selected) %>%
        pull(id)
      
      print(tbl_data() %>% 
              filter(id == v$ID))
        
      output$abstract <- renderUI({
        if(is.null(input$tbl_applicants_rows_selected)){
          return(
            box(title = "Select an applicant from the sidebar to review their abstract",
                width = 12)
          )
        }
        applicant_data <- tbl_filtered_data() %>%
          filter(id == v$ID)
        box(
          width = 12,
          title = applicant_data$`Title of presentation`,
          formText(applicant_data$`Abstract (text only, 1200 characters)`),
          hr(),
          formText("Keywords:", applicant_data$`Keywords (pick at least one)`),
          formText("Format(s):", paste0(applicant_data$`Preferred format (choose 1, or more if you have no preference)`[[1]], collapse=", "))
        )
      })
      
      
      output$review <- renderUI({
        if(is.null(input$tbl_applicants_rows_selected)){
          return()
        }
        review_data <- if(input$admin_mode == "Administrator") latest_decisions() else latest_reviews()
        review_data <- review_data %>% filter(id == v$ID)
        print(review_data)
        box(width = 12,
            title = ifelse(input$admin_mode == "Administrator", "Decision", "Review"),
            solidHeader = TRUE,
            status = "info",
            column(2,
                   radioButtons("accept", 
                                label = "Decision", 
                                choices = c("Bloody ripper", "Beaut", "Okey-dokey", "Sorry"), 
                                selected = ifelse(length(review_data %>% pull(accept))==1, review_data %>% pull(accept), "Okey-dokey")
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
                                `Bloody ripper` = "green",
                                Beaut = "light-blue",
                                `Okey-dokey` = "orange",
                                Sorry = "red")
          )
        )
      })
      
      
      output$feedback <- renderUI({
        if(is.null(input$tbl_applicants_rows_selected) | input$admin_mode == "Reviewer"){
          return()
        }
        
        review_data <- v$reviews %>%
          bind_rows(v$changes$reviews) %>%
          filter(id == v$ID) %>%
          group_by(reviewer) %>%
          filter(timestamp == max(timestamp)) %>%
          ungroup
        
        decision_data <- v$decisions %>%
          bind_rows(v$changes$decisions) %>%
          filter(id == v$ID) %>%
          filter(timestamp == max(timestamp)) %>%
          mutate(reviewer = "Decision")
        
        decision_data %>%
          bind_rows(review_data) %>%
          split(seq_len(NROW(.))) %>% 
          map(~ box(width = 6,
                    title = .$reviewer,
                    background = switch(.$accept,
                                        `Bloody ripper` = "green",
                                        Beaut = "light-blue",
                                        `Okey-dokey` = "orange",
                                        Sorry = "red"),
                    formText(.$accept, ": ", .$comment)))%>%
          do.call("tagList", .) %>%
          fluidRow()
      })
    })
    
    observeEvent(input$save, {
      if(input$admin_mode == "Administrator"){
        v$changes$decisions <- v$changes$decisions %>%
          bind_rows(
            tibble(
              id = v$ID,
              timestamp = format(Sys.time(), tz="GMT"),
              reviewer = v$email,
              accept = input$accept,
              comment = input$comment
            )
          )
      }
      else{
        v$changes$reviews <- v$changes$reviews %>%
          bind_rows(
            tibble(
              id = v$ID,
              timestamp = format(Sys.time(), tz="GMT"),
              reviewer = v$email,
              accept = input$accept,
              comment = input$comment
            )
          )
      }
      if(input$net_mode == "Online"){
        v$reviews <- v$reviews %>%
          bind_rows(v$changes$reviews)
        v$decisions <- v$decisions %>%
          bind_rows(v$changes$decisions)
        v$changes <- uploadChanges(v$changes)
      }
    })
    

    observeEvent(input$btn_debug, {
      browser()
    })
    
    onStop(function(){
      cache <- isolate(reactiveValuesToList(v))
      save(cache, file = "cache_abstracts.Rdata")
    })
    
    removeNotification(notif_ui)
  }
)