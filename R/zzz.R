.onLoad <- function(libname, pkgname) {
  # if we don't have api URIs set in the options we set them now
  # this should allow user to overide them if they want to switch
  if( !"wfo.api_uri" %in% names(options()) ){
    options("wfo.api_uri" = c("https://list.worldfloraonline.org/gql.php"))
  }
}

.onUnload <- function(libname, pkgname){
  options("wfo.api_uri" = NULL)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
"Welcome to the World Flora Online Plant List Package
 The default API URI is stored in options(\"wfo.api_uri\")
 and can be overridden if needed."
)
}


