#' py_list_all
#'
#' @param libPath alternative path to load saved pythonic.Rds file from
#' @param to_clipboard Boolean: Save the result to the clipboard?
#' @param as_code Boolean: Return the result as executable r code (T) or as a character vector with package names (F)   
#' @param pattern Searchpattern to filter packages by
#'
#' @return
#' @export
#'
#' @examples
#' # Prints the code to import all available packages to the console:
#' py_list_all(to_clipboard = F,as_code = T)
py_list_all<-function(libPath="default",to_clipboard=T,as_code=T,pattern=".+"){
  if(libPath=="default"){
    libPath<-paste0(find_pythonic(),"/pythonic")
  }
  all_files<-list.files(libPath)
  all_available_packages<-all_files[str_detect(all_files,"\\.rds")]
  all_available_packages<-tools::file_path_sans_ext(all_available_packages)
  if(as_code==F){
    result<-all_available_packages[str_detect(all_available_packages,pattern = pattern)]
    if(to_clipboard){
      writeClipboard(result)
    }else {
      return(result)
    }
  } else  {
    result<-map_chr(all_available_packages[str_detect(all_available_packages,pattern = pattern)],
                    ~glue::glue("py_import(\"{.x}\")"))%>%
      str_c(.,collapse = "\n")
    if(to_clipboard){
      writeClipboard(result)
    }else {
      result%>%cat()
    }
  }
}