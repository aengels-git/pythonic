
find_pythonic <- function(){
  map(.libPaths(),function(current_lib){
    if(any(list.files(current_lib) %in% "pythonic")){
      return(current_lib)
    }
  })%>%reduce(c)
}


#' py_import
#'
#' @param pkgName Name under which functions have been saved 
#' @param global load each function in the global_enviroment (True) or into a list of function (false)
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

py_import<-function(pkgName,global=T,libPath="default"){
  #Function to detect whether the current call wants to assign the result to 
  #a value
  was_assign_call<-function(call="py_import"){
    f1 <- tempfile()
    try( savehistory(f1), silent=TRUE ) 
    try( rawhist <- readLines(f1), silent=TRUE )
    unlink(f1)
    rm(f1)
    if( exists('rawhist') ) { 
      code = rawhist[str_detect(rawhist,call)]
      assign_call = str_detect(code[length(code)],"<-|=.{0,40}(?=\\()")
    } else {
      assign_call=F
    }
    return(assign_call)
  }
  
  if(libPath=="default"){
    libPath<-paste0(find_pythonic(),"/pythonic")
  }
  filename=str_c(libPath,"/",pkgName,".rds")
  liste <- readRDS(file = filename)
  if(any(names(liste)=="load_dependencies")){
    #Call load_depencies function just once
    loader<-liste[names(liste)=="load_dependencies"][[1]]
    do.call("loader",args = list())
    print(names(liste)!="load_dependencies")
    print(liste[names(liste)!="load_dependencies"])
    liste<- liste[names(liste)!="load_dependencies"]
  }
  if(global==F){
    env_poke(env = global_env(),value = liste,nm = pkgName)
  } else {
    #Option 1: Save as List
    
    assign_call<-was_assign_call()
    if(assign_call){
      return(liste)
    } else {
      #Option to unpack in global enviroment
      walk2(liste,names(liste),function(x,y){
        env_poke(env = global_env(),nm = y,value = x)
      })
    }
  }
}
