has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}

formText <- function(...){
  paste(...) %>%
    strsplit("\n", fixed=TRUE) %>%
    unlist %>%
    map(~ p(.)) %>%
    tagList
}