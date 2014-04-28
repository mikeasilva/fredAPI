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
  key <- function(api.key){
    api.key <<- api.key
  }
  
  # The following group of functions interact with economic data categories.
  category <- function(category.id = NULL, args.list = list(), url.vector = F){
    # Check for series id
    if(is.null(category.id)){
      message("fredAPI: category is not defined")
    } else{
      if(class(url.vector) == "logical"){
        # the url.vector is set to FALSE
        url.vector <- c("category")
      }
      args.list$category_id <- category.id
      return (get(url.vector, args.list))
    }
  }
  
  category_children <- function(category.id = NULL){
    url.vector <- c("category", "children")
    return(category(category.id, NULL, url.vector))
  }
  
  category_related <- function(category.id = NULL){
    url.vector <- c("category", "related")
    return(category(category.id, NULL, url.vector))
  }
  
  category_series <- function(category.id = NULL){
    url.vector <- c("category", "series")
    return(category(category.id, NULL, url.vector))
  }
  
  category_tags <- function(category.id = NULL){
    url.vector <- c("category", "tags")
    return(category(category.id, NULL, url.vector))
  }
  
  category_related_tags <- function(category.id = NULL, tag.names = NULL){
    url.vector <- c("category", "related_tags")
    args.list$tag_names <- tag.names
    return(category(category.id, agrs.list, url.vector))
  }
  
  # Interact with economic data releases.
  releases <- function(){
    url.vector <- c("releases")
    args.list <- list()
    return (get(url.vector, args.list))
  }
  
  releases_dates <- function(){
    url.vector <- c("releases", "related_tags")
    args.list <- list()
    return (get(url.vector, args.list))
  }
  
  release <- function(release.id = NULL, args.list = list(), url.vector = F){
    # Check for release id
    if(is.null(release.id)){
      message("fredAPI: release is not defined")
    } else{
      if(class(url.vector) == "logical"){
        # the url.vector is set to FALSE
        url.vector <- c("release")
      }
      args.list$release_id <- release.id
      return (get(url.vector, args.list))
    }
  }
  
  release_dates <- function(release.id = NULL){
    url.vector <- c("release", "dates")
    return(release(release.id, NULL, url.vector))    
  }
  
  release_series <- function(release.id = NULL){
    url.vector <- c("release", "series")
    return(release(release.id, NULL, url.vector))    
  }
  
  release_sources <- function(release.id = NULL){
    url.vector <- c("release", "sources")
    return(release(release.id, NULL, url.vector))    
  }
  
  release_tags <- function(release.id = NULL){
    url.vector <- c("release", "tags")
    return(release(release.id, NULL, url.vector))    
  }
  
  release_related_tags <- function(release.id = NULL, tag.names = NULL){
    url.vector <- c("release", "dates")
    args.list$tag_names <- tag.names
    return(release(release.id, args.list, url.vector))    
  }
  
  series <- function(series.id = NULL, args.list = list(), url.vector = F){
    # Check for series id
    if(is.null(series.id)){
      message("fredAPI: series is not defined")
    } else{
      if(class(url.vector) == "logical"){
        # the url.vector is set to FALSE
        url.vector <- c("series")
      }
      args.list$series_id <- series.id
      return (get(url.vector, args.list))
    }
  }
  
  series_categories <- function(series.id = NULL){
    url.vector <- c("series", "categories")
    return(series(series.id, NULL, url.vector))    
  }
  
  series_observations <- function(series.id = NULL){
    url.vector <- c("series","observations")
    return(series(series.id, NULL, url.vector))
  }
  
  series_release <- function(series.id = NULL){
    url.vector <- c("series", "release")
    return(series(series.id, NULL, url.vector))
  }
  
  series_search <- function(search.text = NULL){
    # Check for search text
    if(is.null(search.text)){
      message("fredAPI: search text is not defined")
    } else{
      url.vector <- c("series","search")
      args.list$search_text <- search.text
      return (get(url.vector, args.list))
    }
  }
  
  series_search_tags <- function(series.search.text = NULL){
    # Check for series search text
    if(is.null(series.search.text)){
      message("fredAPI: series search text is not defined")
    } else{
      url.vector <- c("series","search", "tags")
      args.list$series_search_text <- series.search.text
      return (get(url.vector, args.list))
    }
  }
  
  series_search_related_tags <- function(series.search.text = NULL, tag.names = NULL){
    # Check for series search text
    if(is.null(series.search.text)){
      message("fredAPI: series search text is not defined")
    } else if(is.null(tag.names)){
      message("fredAPI: tag names text is not defined")
    } else{
      url.vector <- c("series","search", "related_tags")
      args.list$series_search_text <- series.search.text
      args.list$tag_names <- tag.names
      return (get(url.vector, args.list))
    }
  }
  
  series_tags <- function(series.id = NULL){
    url.vector <- c("series", "tags")
    return(series(series.id, NULL, url.vector))
  }
  
  series_updates <- function(){
    url.vector <- c("series", "updates")
    args.list <- list()
    return (get(url.vector, args.list))
  }
  
  series_vintagedates <- function(series.id = NULL){
    url.vector <- c("series", "vintagedates")
    return(series(series.id, NULL, url.vector))
  }

  sources <- function(){
    url.vector <- c("sources")
    args.list = list()
    return (get(url.vector, args.list)) 
  }
  
  fred_source <- function(source.id = NULL, args.list = list(), url.vector = F){
    # Check for series id
    if(is.null(source.id)){
      message("fredAPI: source is not defined")
    } else{
      if(class(url.vector) == "logical"){
        # the url.vector is set to FALSE
        url.vector <- c("source")
      }
      args.list$source_id <- source.id
      return (get(url.vector, args.list))
    }
  }
  
  source_releases <- function(source.id = NULL){
    url.vector <- c("source", "releases")
    return(fred_source(source.id, NULL, url.vector))
  }
  
  tags <- function(args.list = list(), url.vector = F){
    if(class(url.vector) == "logical"){
      # the url.vector is set to FALSE
      url.vector <- c("tags")
    }
    args.list$tag_names <- tag.names
    return (get(url.vector, args.list))
  }
  
  related_tags <- function(tag.names = NULL){
    # Check for series id
    if(is.null(tag.names)){
      message("fredAPI: tag names is not defined")
    } else{
      url.vector <- c("related_tags")
      args.list$tag_names <- tag.names
      return (get(url.vector, args.list))
    }
  }
  
  tags_series <- function(tag.names = NULL){
    url.vector <- c("tags", "series")
    args.list <- list("tag_names" = tag.names)
    return(tags(args.list, url.vector))
  }
  
  nc <- list(key = key,
       get = get,
       json = json,
       xml = xml,
       category = category, 
       category_children = category_children,
       category_related = category_related,
       category_series = category_series,
       category_tags = category_tags,
       category_related_tags = category_related_tags,
       releases = releases,
       releases_dates = releases_dates,
       release = release,
       release_dates = release_dates,
       release_series = release_series,
       relase_sources = release_sources,
       release_tags = release_tags,
       release_related_tags = release_related_tags,
       series = series,
       series_categories = series_categories,
       series_observations = series_observations,
       series_release = series_release,
       series_search = series_search,
       series_search_tags = series_search_tags,
       series_search_releated_tags = series_search_related_tags,
       series_tags = series_tags,
       series_updates = series_updates,
       series_vintagedates = series_vintagedates,
       sources = sources,
       source = fred_source,
       source_releases = source_releases,
       tags = tags,
       related_tags = related_tags,
       tags_series = tags_series,
       observations = series_observations)
  nc <- list2env(nc)
  class(nc) <- "fredAPI"
  return(nc)  
}
