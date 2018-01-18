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

fuzzyMatching <- function(pattern, dataset){
  if(pattern == ""){
    return(0)
  }
  dataset %>%
    split(seq_len(NROW(.))) %>% 
    map(~ as.character(.) %>% strsplit(" ") %>% unlist) %>%
    map_dbl(~ sum(agrepl(gsub(" ", "|", pattern), .))/length(.))
}