#' py_jupyter
#'
#' @param path get current working directory
#' @param conda_env conda enviroment to use
#'
#' @return
#' @export
#'
#' @examples
py_jupyter <- function(path=getwd(),conda_env="spyder-env"){
  shell(glue("cd {path} && conda activate {conda_env} && jupyter-notebook"),wait=FALSE)
}