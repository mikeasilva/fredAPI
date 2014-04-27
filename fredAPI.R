# FRED API
# Written by Mike Silva
# This is a wrapper for the FRED API service.  By default it returns XML,
# however it can return JSON
fredAPI <- function(){
  # Load in the libraries needed
  require(RCurl)
  
  # Initialize the FRED API key variable
  api.key <- NULL
  
  # Does the user want JSON returned?  If so this needs to be set to TRUE
  json.return <- FALSE
  
  # This function makes the API request and returns the response.
  # It takes in two arguments: url.vector which is a character vector and 
  # api.args which is a list.
  get <- function(url.vector, api.args.list){
    # Build the API url
    api.url <- paste(c('http://api.stlouisfed.org/fred',url.vector), collapse ="/")
    
    # Check to ensure API key is valid
    if(is.null(api.key)){
      message("fredAPI: API key is not set. Use the key method to set the key.")
    } else {
      # Add the API key
      api.args.list$api_key <- api.key
      
      # Check if the user wants JSON instead of XML returned
      if(json.return){
        api.args.list$file_type <- 'json'
      }
      
      # This turns the api.args list and builds a url encoded
      url.args <- paste(names(api.args.list), api.args.list, sep="=", collapse="&")
      
      # Build the API request URL
      api.url <- paste(c(api.url,url.args), collapse="?")
      
      # Return API response
      getURL(api.url)
    }
  }
  
  # This function is called when the user wants the API call to return JSON
  json <- function(){
    json.return <<- TRUE
  }
  
  # This function is called when the user wants the API call to return XML
  xml <- function(){
    json.return <<- FALSE
  }
  
  # This function sets the API key
  key <- function(x){
    api.key <<- x
  }
  
  # The following group of functions interact with economic data categories.
  category <- function(category.id = NULL, args.list = list(), url.vector = F){
    # Check for series id
    if(is.null(category.id)){
      message("fredAPI: category id not defined")
    } else{
      if(class(url.vector) == "logical"){
        # the url.vector is set to FALSE
        url.vector <- c("category")
      }
      args.list$category_id <- category.id
      return (get(url.vector, args.list))
    }
  }
  
  categories <- function(category.id = NULL, args.list){
    url.vector <- c("category", )
    return(category(args.list))
  }
  
  children <- function(category.id = NULL){
    url.vector <- c("category", "children")
    return(category(category.id, NULL, url.vector))
  }
  
  related <- function(category.id){
    
  }
  
  category_series <- function(category.id){
    
  }
  
  
  # Interact with economic data releases.
  releases <- function(){
    
  }
  
  release <- function(id){
    
  }
  
  dates <- function(){
    
  }
  
  
  # The following functions interact with economic data series.
  
  
  series <- function(series.id = NULL, release = FALSE){
    # Check for series id
    if(is.null(series.id)){
      message("fredAPI: series id not defined")
    } else {
      args.list <- list(series_id = series.id)
      url.vector <- c("series")
      if(release)
        url.vector <- c(url.vector,"release")
      return (get(url.vector, args.list))
    }
  }
  
  observations <- function(series.id = NULL){
    # Check for series id
    if(is.null(series.id)){
      message("fredAPI: series id not defined")
    } else {
      args.list <- list(series_id = series.id)
      url.vector <- c("series","observations")
      return (get(url.vector, args.list))
    }
  }
  
  search <- function(term){
    return(term)
  }
  
  updates <- function(){
    
  }
  
  vintage <- function(x){
    return(x)
  }
  
  
  # Query economic data sources.
  sources <- function(){
    
  }
  
  fredSource <- function(x){
    return(x)
  }
  
  nc <- list(key = key,
       get = get,
       json = json,
       xml = xml,
       category = category, 
       children = children,
       related = related,
       category_series = category_series,
       category_tags = category_tags,
       category_related_tags = category_related_tags,
       releases = releases,
       releases_dates = releases_dates,
       release = release,
       release_dates = release_dates,
       release_series = release_series,
       relase_sources = release_sources,
       release_tags = relase_tags,
       search = search,
       updates = updates,
       vintage = vintage,
       sources = sources,
       source = fredSource)
  nc <- list2env(nc)
  class(nc) <- "fredAPI"
  return(nc)  
}
