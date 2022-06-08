#' @title Function to return objects created in a specific script that exist in the environment
#'
#' @description The function identify the objects created by a script, and return those present in the environment. The function can take some time to run if the script is long!
#'
#' @author Rosalie Bruel
#'
#' @param path2file The path to the .R or .Rmd file to be evaluated. If input is .Rmd, the function will first extract the code from chunk (creating a copy of the code in the working directory).
#' @param exception Vector of objects to not return. Default = NULL.
#' @param source Logical argument. Source the file before running: necessary to put the objects in the environment. Default = FALSE.
#' @param message Logical argument. Default = TRUE
#'
#' @example get.objects()
#' @example get.objects(source = TRUE, message = FALSE)

if(!"utils" %in% rownames(installed.packages())) install.packages("utils")
if(!"tools" %in% rownames(installed.packages())) install.packages("tools")
if(!"knitr" %in% rownames(installed.packages())) install.packages("knitr")

get.objects <- function(path2file = NULL, exception = NULL, source = FALSE, message = TRUE) {
  library("utils")
  library("tools")

  # Step 0-1: Possibility to leave path2file = NULL if using RStudio.
  # We are using rstudioapi to get the path to the current file
  if(is.null(path2file)) path2file <- rstudioapi::getSourceEditorContext()$path

  # Check that file exists
  if (!file.exists(path2file)) {
    stop("couldn't find file ", path2file)
  }

  # Step 0-2: If .Rmd file, need to extract the code in R chunks first
  # Use code in https://felixfan.github.io/extract-r-code/
  if(file_ext(path2file)=="Rmd") {
    library("knitr")
    tmp <- purl(path2file)
    path2file <- paste(getwd(),tmp,sep="/")
    source = TRUE # Must be changed to TRUE here
  }

  # Step 0-3: Start by running the script if you are calling an external script.
  if(source) source(path2file)

  # Step 1: screen the script
  summ_script <- getParseData(parse(path2file, keep.source = TRUE))

  # Step 2: extract the objects
  list_objects <- summ_script$text[which(summ_script$token == "SYMBOL")]
  # List unique
  list_objects <- unique(list_objects)

  # Step 3: find where the objects are.
  src <- paste(as.vector(sapply(list_objects, find)))
  src <- tapply(list_objects, factor(src), c)

  # List of the objects in the Global Environment
  # They can be in both the Global Environment and some packages.
  src_names <- names(src)

  list_objects = NULL
  for (i in grep("GlobalEnv", src_names)) {
    list_objects <- c(list_objects, src[[i]])
  }

  # Step 3bis: if any exception, remove from the list
  if(!is.null(exception)) {
    list_objects <- list_objects[!list_objects %in% exception]
  }

  # Step 4: done!
  # If message, print message:
  if(message) {
    cat(paste0("  ",(length(list_objects)+length(exception))," objects  were created in the script \n  ", path2file,"\n"))
  }

  return(list_objects)
}

