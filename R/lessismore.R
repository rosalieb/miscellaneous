# Function to evaluate if some packages could be unloaded
# The function can take some time to run if the script is long!

# Require the package "sos" for some functions that were not identified by NCmisc
# Require "knitr" if input file is .Rmd
# Require ggplot2 and plotly if argument plot_output == TRUE

# packages is a vector of packages, typically the one loaded for a script
# path2file is the path to the .R file to be evaluated
# plot_output is just for fun. Logical argument, if turned to TRUE, need ggplot2 and plotly
# thres set the minimum pages in which the function must be found (linked to the specification in the "sos" package). If thresh == 1, will select everything.

# Works for .R and .Rmd. If input is .Rmd, the function will first extract the code from chunk (creating a copy of the code in the working directory)


lessismore <- function(packages = NULL, path2file = NULL, plot_output = FALSE, thresh = 2) {
  if (!file.exists(path2file)) {
    stop("couldn't find file ", path2file)
  }

  packages <- sort(packages)

  require("sos")
  list_out <- list()


  # Step 0: If .Rmd file, need to extract the code in R chunks first
  # Use code in https://felixfan.github.io/extract-r-code/
  if(get.ext(path2file)=="Rmd") {
    require("knitr")
    tmp <- purl(path2file)
    path2file <- paste(getwd(),tmp,sep="/")
  }

  # Step 1, screen the script
  summ_script <- getParseData(parse(path2file, keep.source = TRUE))

  # Step 2, extract the function
  list_fun <- summ_script$text[which(summ_script$token == "SYMBOL_FUNCTION_CALL")]
  # just for fun, get some summary stats:
  summ_fun <- data.frame("count"=summary(as.factor(list_fun)))
  list_out$summary_functions <- summ_fun
  if(plot_output) {
    require("ggplot2")
    require("plotly")
    summ_fun$name <- rownames(summ_fun)
    summ_fun <- summ_fun[order(summ_fun$count, decreasing = T),]
    p <- ggplot(summ_fun, aes(index(summ_fun),count, label=summ_fun$name)) + geom_point() + geom_line() +
      theme_bw() + labs(x="Index", y="Count", title="Occurence of each function in the script \n(Hoover to see name of the functions)")
    list_out$summary_functions_plot <- ggplotly(p)
  }

  list_fun <- unique(list_fun)
  src <- paste(as.vector(sapply(list_fun, find)))
  src <- tapply(list_fun, factor(src), c)
  list_pack1 <- names(src)

  if("character(0)" %in% list_pack1) vec_fun <- src$`character(0)` else vec_fun <- NULL

  # If some functions were not matched, try to use the "sos" package that searches more widely
  options(warn=-1)
  list_pack2 <- NULL
  no_match <- NULL
  if (!is.null(vec_fun)) {
    for (i in 1:length(vec_fun)) {
    output_sos <- findFn(vec_fun[i])
    output_sos <- output_sos[output_sos$Count>thresh,]
    if("FSA" %in% output_sos$Package) stop()

    if(length(output_sos$Package>0)) {
      list_pack2 <- c(list_pack2,unique(output_sos$Package))
    } else {
      no_match <- c(no_match, vec_fun[i])
    }
    }
    }
  options(warn=0)

  # Find which packages are probably used at least once
  which_useful <- rep(FALSE, length(packages))
  for (i in 1:length(packages)) {
    if(length(grep(packages[i], c(list_pack1,list_pack2)))>0) which_useful[i] <- TRUE
  }

  # Outputs
  list_out$packages_used <- packages[which_useful]
  list_out$packages_non_used <- packages[!which_useful]
  list_out$functions_non_matched <- no_match
  list_out$summary <- paste0("Out of the ",length(packages)," packages in the input vector:\n   ✓ ",length(list_out$packages_used)," are used within the script,\n   ✕ ",length(list_out$packages_non_used)," do not appear to be used within the script,\n",length(list_out$functions_non_matched)," functions are used within the script but do not seem to be part of any package or the Global Environment.")

  return(list_out)

}

