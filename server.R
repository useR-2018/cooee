library(shiny)
library(dplyr)
library(googlesheets)
library(DT)
library(jsonlite)

shinyServer(
  function(input, output, session) {
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
    
    output$tbl_applicants <- DT::renderDataTable({
      if (!is.null(isolate(access_token()))) {
        gs_key("1Jjq70cLXfMZj5mXwau_Zhbw-N0d34nrw3qGvoeL4DdQ") %>% gs_read_csv
      }
      else{
        NULL
      }
    })
    
    observeEvent(input$btn_debug, {
      browser()
    })
  }
)