# writer: writes to file, appending data
writer <- function(x, ..., out_file = file_connection, sep = "\n") {
  cat(paste0(x, ...), file = out_file, append = TRUE, sep = sep)
}

# chunk_wrapper: creates a wrapper around R Markdown code
chunk_wrapper <- function(x, ..., out_file = file_connection, options = c("echo = FALSE", "warning = FALSE"), label = NULL) {
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
    out_file = out_file
  )
  
  writer(x, ..., out_file = out_file)
  writer("```", out_file = out_file)
  writer("")
}

# fig_wrapper: an R Markdown wrapper for a figure
fig_wrapper <- function(x, ..., out_file = file_connection, options = c("echo = FALSE", "fig.width = 4", "fig.height = 3", "message = FALSE", "warning = FALSE"), label = NULL) {
  chunk_wrapper(x, out_file = out_file, options = options, label = label)
}

# secret_chunk_wrapper: an R markdown wrapper for creating hidden chunks
secret_chunk_wrapper <- function(x, ..., out_file = file_connection, options = c("echo = FALSE", "include = FALSE", "warning = FALSE", "message = FALSE", "error = FALSE"), label = NULL) {
  chunk_wrapper(x, out_file = out_file, options = options, label = label)
}

# row_wrapper: creates a row with 3 columns
row_wrapper <- function(left_col, mid_col, right_col) {
  writer("<div class = \"row\">")
  writer("<div class = \"col-lg-4\">")
  writer(left_col)
  writer("</div>")
  writer("<div class = \"col-lg-4\">")
  writer(mid_col)
  writer("</div>")
  writer("<div class = \"col-lg-4\">")
  writer(right_col)
  writer("</div>")
  writer("</div>")
}

# summarize_var_metadata: calculates and tabulates metadata for a specified variable
summarize_var_metadata <- function(var) {
  var_class = setdiff(class(var), "labelled")
  num_missing = sum(is.na(var))
  num_valid = sum(!is.na(var))
  num_missing = paste0(
    num_missing,
    " (",
    round((num_missing/length(var)) * 100, 3),
    "%)"
  )
  num_unique = length(unique(var))
  is_derived <- ifelse(is.null(attr(var, "is_derived")), "FALSE", attr(var, "is_derived"))

  # construct dataset of variable metadata
  results <- data.frame(
    Attribute = c(
      "Class",
      "Valid obs.",
      "NA obs.",
      "Unique",
      "Derived"
    ),
    Value = c(
      var_class,
      num_valid,
      num_missing,
      num_unique,
      is_derived
    )
  )
  return(results)
}

# draw_plot: flexibly generate plots for the most common types of variables
draw_plot <- function(var, var_name) {
  var_class <- setdiff(class(var), "labelled")
  num_unique <- length(unique(var))
  
  if (var_class %in% c("Date")) { # date: histogram
    vis <- sprintf(
      "df[,'%s'] <- df[['%s']]\n
      
       ggplot(data = na.omit(df)) +
         geom_histogram(aes(%s), color = 'black', bins = 50) +
         scale_x_date(labels = scales::date_format('%%Y-%%b'), date_breaks = '3 months') +
         theme_minimal() + 
         theme(axis.text.x = element_text(angle = 45, hjust = 1))",
      var_name, 
      var_name, 
      var_name
    )
  } else if (num_unique <= 10 | var_class %in% c("factor", "character")) { # factor or character or <= 10 unique values: bar chart
      vis <- sprintf(
        "df[,'%s'] <- factor(df[['%s']])\n
        
         ggplot(data = na.omit(df)) +
           geom_bar(aes(%s)) +
           theme_minimal()",
        var_name,
        var_name,
        var_name
      )
    } else { 
    vis <- sprintf( # all other variable types: histogram
       "var <- df[['%s']]\n

        bw <- (max(var, na.rm = T) - min(var, na.rm = T)) / 30\n 
        m <- mean(var, na.rm = T)\n
        sd <- sd(var, na.rm = T)\n
        n <- length(!is.na(var))\n
                   
        class(df[['%s']]) <- NULL

        ggplot(data = na.omit(df)) +
          geom_histogram(aes(%s), color = 'black', bins = 30) +
          theme_minimal() +
          stat_function(fun = function(x, mean, sd, n, bw) {
            dnorm(x = x, mean = mean, sd = sd) * bw * n},
            args = c(mean = m, sd = sd, n = n, bw = bw), color = 'red')",
       var_name, 
       var_name, 
       var_name
    )
  }
}

# summarize_var_data: calculates and tabulates appropriate basic summary statistics for a specified variable
summarize_var_data <- function(var) {
  var_class <- setdiff(class(var), "labelled")
  
  if (var_class %in% c("numeric", "integer")) {
    var_mean = mean(var, na.rm = TRUE)
    var_sd = sd(var, na.rm = TRUE)
    var_min = min(var, na.rm = TRUE)
    var_p25 = quantile(var, 0.25, na.rm = TRUE)
    var_p50 = quantile(var, 0.50, na.rm = TRUE)
    var_p75 = quantile(var, 0.75, na.rm = TRUE)
    var_max = max(var, na.rm = TRUE)
    
    results <- data.frame(
      Statistic = c(
        "Mean",
        "SD",
        "Min",
        "0.25",
        "Median",
        "0.75",
        "Max"
      ), 
      Value = c(
        var_mean,
        var_sd,
        var_min,
        var_p25,
        var_p50,
        var_p75,
        var_max
      )
    )
  } else if (var_class %in% c("Date")) {
    var_mean = mean(var, na.rm = TRUE)
    var_min = min(var, na.rm = TRUE)
    var_p50 = quantile(var, 0.50, na.rm = TRUE)
    var_max = max(var, na.rm = TRUE)
    
    results <- data.frame(
      Statistic = c(
        "Mean",
        "Min",
        "Median",
        "Max"
      ), 
      Value = c(
        var_mean,
        var_min,
        var_p50,
        var_max
      )
    )
  } else {
    x <- as.data.frame(table(var))
    x$percent <- x$Freq / length(var)
    x$validPercent <- x$Freq / sum(!is.na(var))
    results <- x
    colnames(results) <- c("Level", "Frequency", "Percent", "Valid Percent")
  }
  return(results)
}

# create_summary_table: calculates and tabulates variable metadata for an entire data frame
create_summary_table <- function(df) {
  # vars <- names(df)
  vars <- sapply(names(df), function(x) paste0('[', x, '](#', x, ')')) # this adds hyperlinks to the summary table
  
  num_unique <- sapply(
    df, 
    function(x) length(unique(x))
  )
  
  num_missing <- sapply(
    df, 
    function(x) paste0(sum(is.na(x)), " (", round((sum(is.na(x)) / length(x)) * 100, 3), "%)")
  )
  
  is_derived <- sapply(
    df,
    function(x) ifelse(is.null(attr(x, "is_derived")), "FALSE", attr(x, "is_derived"))
  )
  
  var_labels <- sapply(
    df, 
    function(x) ifelse(is.null(attr(x, "label")[[1]]), "", attr(x, "label")[[1]])
  )
  
  num_missing_labels <- sum(
    sapply(
      df, 
      function(x) is.na(ifelse(is.null(attr(x, "label")[[1]]), NA, attr(x, "label")[[1]]))
    )
  )
  
  var_class <- sapply(
    df, 
    function(x) setdiff(class(x), "labelled")
  )
  
  if (length(vars) == num_missing_labels) { # if all labels are missing
    sumTab <- data.frame(
      Variable = vars,
      Class = var_class,
      Unique = num_unique,
      Missing = num_missing,
      Derived = is_derived,
      row.names = NULL
    )
  } else {
    sumTab <- data.frame(
      Variable = vars, 
      Label = var_labels,
      Class = var_class, 
      Unique = num_unique, 
      Missing = num_missing, 
      Derived = is_derived,
      row.names = NULL
    )
 }
  return(sumTab)
}