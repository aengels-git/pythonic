#' py_export
#'
#' @param pkgName Name under which functions should be made available
#' @param libPath alternative path to load saved pythonic.Rds file from
#'
#' @return
#' @export
#'
#' @examples
#' # Clear global enviroment
#' rm(list = ls())
#' # Create a function
#' t<-function(){
#'   print("hello world")
#' }
#' # Export it to an arbitrary package name
#' py_export(pkgName = "hello")
#' # Clear global enviroment
#' rm(list = ls()) #t is no longer available
#' 
#' # Reload based on package name
#' py_import(pkgName = "hello")
#' t()

py_export<-function(pkgName,libPath="default"){
  if(libPath=="default"){
    libPath<-paste0(find_pythonic(),"/pythonic")
  }
  liste<-env()
  walk(ls(envir = global_env()),function(x){
    object<-env_get(env = global_env(),nm = x)
    if((is.function(object)|class(object)=="R6ClassGenerator") && x!="export"){
      env_poke(env = liste,nm=x,value = object)
    }
  })
  assign(x=pkgName,value = as.list(liste))
  filename=str_c(libPath,"/",pkgName,".rds")
  print(filename)
  print(env_get(nm=pkgName))
  saveRDS(env_get(nm=pkgName),file = filename)
}