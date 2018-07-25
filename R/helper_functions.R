#' Writes to file, appending data
#' 

dummyData <- function(n = 1000, seed = 1284232) { # OAK 20180720 delete
  set.seed(seed)
  
  df2 <- data.frame(
    id = seq(1, n, by = 1), 
    char = LETTERS[1:8],
    fact = factor(letters[1:4]),
    num = sample(1:600, n, replace = TRUE), 
    log = ifelse(sign(rnorm(n)) == -1, T, F),
    dt = seq(from = as.Date("1970/1/1"), by = "day", length.out = 1000),
    stringsAsFactors = F
  )
  return(df2)
}

###

# OAK 20180720 add documentation to each function

writer <- function(x, ..., outfile = fileConn, sep = "\n") {
  cat(paste0(x, ...), file = outfile, append = TRUE, sep = sep)
}

#'  reates a rmarkdown wrapper  
#'  

chunk.wrapper <- function(x, ..., outfile = fileConn, options = c("echo = FALSE", "warning = FALSE"), label = NULL) {
  writer(
    paste0(
      "```{r ", 
      ifelse(
        is.null(label), 
        ", ", 
        paste0(label, ", ")
      ),
      paste0(
        options, 
        collapse = ", "
      ), 
      "}"
    ),
    outfile = outfile
  )
  
  writer(x, ..., outfile = outfile)
  writer("```", outfile = outfile)
  writer("")
}

#' An rmarkdown wrapper for a figure
#' 

fig.wrapper <- function(x, ..., outfile = fileConn, options = c("echo = FALSE", "fig.width = 4", "fig.height = 3", "message = FALSE", "warning = FALSE"), label = NULL) {
  chunk.wrapper(x, outfile = outfile, options = options, label = label)
  #I get an error when label stuff is there
}

#' A special wrapper for the library section
#' 

secretChunk.wrapper <- function(x, ..., outfile = fileConn, options = c("echo = FALSE", "include = FALSE", "warning = FALSE", "message = FALSE", "error = FALSE"), label = NULL) {
  chunk.wrapper(x, outfile = outfile, options = options, label = label)
}

#' Wraps two pieces of code into a 2/3rds column row. 
#' Note these rows won't show in built in browser but will show up in regular browser 
#' 

row.wrapper <- function(leftCol, rightCol) { # OAK 20180720 rename to 2 col version
  writer("<div class = \"row\">")
  writer("<div class = \"col-lg-8\", style=\"background-color: lightblue\">")
  writer(leftCol)
  writer("</div>")
  writer("<div class = \"col-lg-4\", style=\"background-color: lightgreen\">")
  writer(rightCol)
  writer("</div>")
  writer("</div>")
}

row.wrapper1 <- function(leftCol, midCol, rightCol) { # OAK 20180720 rename to 3 col version
  writer("<div class = \"row\">")
  writer("<div class = \"col-lg-4\">")
  writer(leftCol)
  writer("</div>")
  writer("<div class = \"col-lg-4\">")
  writer(midCol)
  writer("</div>")
  writer("<div class = \"col-lg-4\">")
  writer(rightCol)
  writer("</div>")
  writer("</div>")
}

#' takes a vector and determines type, with some special considerations
#' @param var Varible to determine display type

getDisplayType <- function(var) {
  displayType = ""
  
  #regardless of type, calculate number of unique values
  numlevels <- length(unique(var))
  print(numlevels)
  #if <15 levels return barlow
  if (numlevels < 15) {
    displayType <- "barLow"
  }
  #else if factor/character return barHigh
  else if (is.factor(var) | is.character(var)) {
    displayType <- "barHigh"
  }
  #else if numeric return histogram
  else if (is.numeric(var)) {
    displayType <- "histogram"
  } else {
    displayType <- "other"
  }
  
  return(displayType)
}

codebookMetadataSummarize <- function(var) { # OAK 20180720 modify with own metadata

  # calculate basic information
  variableType = setdiff(class(var), "labelled")
  numMissing = sum(is.na(var))
  numValid = sum(!is.na(var))
  numMissing = paste0(
    numMissing,
    " (",
    round((numMissing/length(var)) * 100, 3),
    "%)"
  )
  numUnique = length(unique(var))
  is.derived <- ifelse(is.null(attr(var, "is.derived")), "FALSE", attr(var, "is.derived"))

  # Construct dataset of basic information
  results <- data.frame(
    Attribute = c(
      "Class",
      "Valid obs.",
      "NA obs.",
      "Unique",
      "Derived"
    ),
    Value = c(
      variableType,
      numValid,
      numMissing,
      numUnique,
      is.derived
    )
  )
  return(results)
}

codebookVisualize <- function(var, varName) { # OAK 20180720 rename to codebookFigure
  varClass <- setdiff(class(var), "labelled")
  numUnique <- length(unique(var))
  
  #if there are fewer than 10 levels or it's a character/factor, we'll use a bar
  if (varClass %in% c("Date")) {
    vis <- sprintf(
      "dftest[,'%s'] <- dftest[['%s']]\n
      
      ggplot(data = na.omit(dftest)) +
      geom_histogram(aes(%s), color = 'black', bins = 50) +
      scale_x_date(labels = scales::date_format('%%Y-%%b'), date_breaks = '3 months') +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))",
      varName, 
      varName, 
      varName
    )
  } else if (numUnique <= 10 | varClass %in% c("factor", "character")) {
    
    # if (!all(is.na(as.Date(as.character(var))))) {
    #   vis <- sprintf(
    #     "dftest[,'%s'] <- as.Date(dftest[['%s']])\n
    # 
    #      ggplot(data = na.omit(dftest)) +
    #        geom_histogram(aes(%s), color = 'black', bins = 50) +
    #        theme_minimal() +
    #        scale_x_date(labels = date_format('%Y-%b'), breaks = '1 month')",
    #     varName, 
    #     varName, 
    #     varName
    #   )
    # } else {
      vis <- sprintf(
        "dftest[,'%s'] <- factor(dftest[['%s']])\n
        
        ggplot(data = na.omit(dftest)) +
        geom_bar(aes(%s)) +
        theme_minimal()",
        paste0(varName, "_new"),
        varName,
        paste0(varName, "_new")
      )
    } else {
    #all other variables get a histogram
    vis <- sprintf(
       "var <- dftest[['%s']]\n

         bw = (max(var, na.rm = T) - min(var, na.rm = T)) / 30\n 
         m <- mean(var, na.rm = T)\n
         sd <- sd(var, na.rm = T)\n
         n <- length(!is.na(var))\n
                   
         class(dftest[['%s']]) <- NULL

         ggplot(data = na.omit(dftest)) +
           geom_histogram(aes(%s), color = 'black', bins = 30) +
           theme_minimal() +
           stat_function(fun = function(x, mean, sd, n, bw) {
             dnorm(x = x, mean = mean, sd = sd) * bw * n},
             args = c(mean = m, sd = sd, n = n, bw = bw), color = 'red')",
       varName, 
       varName, 
       varName
    )
  }
}

codebookDataTableSummarize <- function(var) { # OAK 20180720 add own metadata
  varClass <- setdiff(class(var), "labelled")
  
  if (varClass %in% c("numeric", "integer")) {
    varMean = mean(var, na.rm = TRUE)
    varSD = sd(var, na.rm = TRUE)
    p100 = quantile(var, 1, na.rm = TRUE)
    p75 = quantile(var, 0.75, na.rm = TRUE)
    p50 = quantile(var, 0.50, na.rm = TRUE)
    p25 = quantile(var, 0.25, na.rm = TRUE)
    p0 = quantile(var, 0, na.rm = TRUE)
    
    results <- data.frame(
      Statistic = c(
        "Mean",
        "Standard Deviation",
        "Min",
        "0.25",
        "Median",
        "0.75",
        "Max"
      ), 
      Value = c(
        varMean,
        varSD,
        p0,
        p25,
        p50,
        p75,
        p100
      )
    )
  } else if (varClass %in% c("Date")) {
    varMean = mean(var, na.rm = TRUE)
    p100 = max(var, na.rm = TRUE)
    p50 = median(var, na.rm = TRUE)
    p0 = min(var, na.rm = TRUE)
    
    results <- data.frame(
      Statistic = c(
        "Mean",
        "Min",
        "Median",
        "Max"
      ), 
      Value = c(
        varMean,
        p0,
        p50,
        p100
      )
    )
  } 
  # else {
  #   if (!all(is.na(as.Date(as.character(var))))) {
  #     var <- as.Date(var)
  #     
  #     varMean = mean(var, na.rm = TRUE)
  #     p100 = max(var, na.rm = TRUE)
  #     p50 = median(var, na.rm = TRUE)
  #     p0 = min(var, na.rm = TRUE)
  #     
  #     results <- data.frame(
  #       Statistic = c(
  #         "Mean",
  #         "Min",
  #         "Median",
  #         "Max"
  #       ), 
  #       Value = c(
  #         varMean,
  #         p0,
  #         p50,
  #         p100
  #       )
  #     )
  #   } else {
  else {
      x <- as.data.frame(table(var))
      x$percent <- x$Freq / length(var)
      x$validPercent <- x$Freq / sum(!is.na(var))
      results <- x
      colnames(results) <- c("Level", "Frequency", "Percent", "Valid Percent")
    }
  return(results)
}

calcSummaryTable <- function(df) { # OAK 20180720 add own metadata
  #construct a dataset of labels, variables, class, unique, and missings
  # vars <- names(df)
  vars <- sapply(names(df), function(x) paste0('[', x, '](#', x, ')')) # this includes the link to the section
  
  uniques <- sapply(
    df, 
    function(x) length(unique(x))
  )
  
  missings <- sapply(
    df, 
    # function(x) sum(is.na(x)) / length(x)
    function(x) paste0(sum(is.na(x)), " (", round((sum(is.na(x)) / length(x)) * 100, 3), "%)")
  )
  is.derived <- sapply(
    df,
    #function(x) attr(x, "is.derived")
    function(x) ifelse(is.null(attr(x, "is.derived")), "FALSE", attr(x, "is.derived"))
  )
  labs <- sapply(
    df, 
    function(x) ifelse(is.null(attr(x, "label")[[1]]), "", attr(x, "label")[[1]])
  )
  labMissing <- sum(
    sapply(
      df, 
      function(x) is.na(ifelse(is.null(attr(x, "label")[[1]]), NA, attr(x, "label")[[1]]))
    )
  )
  classes <- sapply(
    df, 
    function(x) setdiff(class(x), "labelled")
  )
  
  #if all labels are missing
  if (length(vars) == labMissing) {
    sumTab <- data.frame(
      Variable = vars,
      Class = classes,
      Unique = uniques,
      Missing = missings,
      Derived = is.derived,
      row.names = NULL
    )
  } else {
    sumTab <- data.frame(
      Variable = vars, 
      Label = labs,
      Class = classes, 
      Unique = uniques, 
      Missing = missings, 
      Derived = is.derived,
      row.names = NULL
    )
 }
  return(sumTab)
}
