has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}
