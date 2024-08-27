ladder.info.attach <- function(stored, ladder, 
                               channel.ladder=NULL, method="iter2", 
                               ladd.init.thresh=NULL, env = parent.frame(), 
                               prog=TRUE, draw=TRUE, attempt=10) {
  all.names <- names(stored)
  
  dev = 50
  warn = FALSE
  
  if (is.null(channel.ladder)) {
    channel.ladder <- dim(stored[[1]])[2]
  } else {
    channel.ladder <- channel.ladder
  }
  
  layout(matrix(1:3, nrow = 3, ncol = 1))
  
  # Extract all ladder models for each plant
  list.ladders <- lapply(stored, function(x) {
    y <- x[, channel.ladder]
    return(y)
  })
  
  # Set attributes for each ladder
  for (t in 1:length(list.ladders)) {
    attributes(list.ladders[[t]]) <- list(mycomm = names(list.ladders)[t])
  }
  
  if (prog == TRUE) {
    res <- lapply_pb(list.ladders, find.ladder, init.thresh = ladd.init.thresh, ladder = ladder, dev = dev, warn = warn, method = method, draw = draw, attempt = attempt)
  } else {
    res <- lapply(list.ladders, find.ladder, ladder = ladder, dev = dev, warn = warn, method = method, init.thresh = ladd.init.thresh, draw = draw, attempt = attempt)
  }
  
  env$list.data.covarrubias <- res
  
  cat("\nSizing process complete. Information has been stored in the environment for posterior functions.\nFor example to be used by the overview2() or score.markers() functions.")
  
  correlations <- unlist(lapply(res, function(x) { x$corr }))
  
  if (length(which(correlations < .92)) > 0) {
    cat(paste("\nWe did not find a good ladder in", length(which(correlations < .92)), "sample(s). \nIf you wish to correct it you can try one of the following:\n"))
    cat("\na) The value of ladd.init.thresh might be too low, making noisy peaks too abundant \n     Solution-- make sure your initial value 'init.thresh' is not below 200 RFUs\nb) You can continue your analysis without worrying for those samples or removing. Identify them as:\n     corro <- unlist(lapply(list.data.covarrubias, function(x) { x$corr }))\n     (bad <- which(corro < .9999))\nc) MOST IMPORTANT! You can correct manually the bad samples using the 'ladder.corrector()' function providing the names of the bad samples (below), your ladder, and the information from the 'storing.inds' function. Type ?ladder.corrector\n\nNames of the bad sample(s):\n")
  }
  
  layout(matrix(1, 1, 1))
  
  bads <- all.names[which(correlations < .92)]
  
  if (length(bads) > 0) {
    return(bads) 
  }
}

##################################################################################################
# Startup function
# This function is executed once the library is loaded
.onAttach = function(libname, pkgname) {
  Rv = R.Version()
  
  if (!exists("getRversion", baseenv()) || (getRversion() < "2.1")) {
    stop("This package requires R 2.1 or later")
  }
  
  # Assign path for Fastfrag package
  assign(".Fastfrag.home", file.path(libname, pkgname), pos = match("package:Fastfrag", search()))
  
  Fastfrag.version = "0.0.0.9000"
  
  if (interactive()) {
    packageStartupMessage(paste("## ========================================================= ## "), appendLF = TRUE)
    packageStartupMessage(paste("## ========================================================= ## "), appendLF = TRUE)
    packageStartupMessage(paste("# Fastfrag: An R package for Fragment Analysis ", Fastfrag.version, ". ", sep = ""), appendLF = TRUE)
    packageStartupMessage("# Author: Covarrubias-Pazaran et al. (original Fragman authors)", appendLF = TRUE)
    packageStartupMessage("# Modifications by Shane Liu", appendLF = TRUE)
    packageStartupMessage("# Type 'help(Fastfrag)' for summary information", appendLF = TRUE)
    packageStartupMessage("# Type 'citation(Fastfrag)' to know how to cite this package", appendLF = TRUE)
    packageStartupMessage(paste("## ========================================================= ## "), appendLF = TRUE)
    packageStartupMessage(paste("## ========================================================= ## "), appendLF = TRUE)
  }
  
  invisible()
}
