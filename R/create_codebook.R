# create_codebook: workhorse function that creates a codebook from an appropriate data frame
create_codebook <- function(df, 
                            file_name = paste0("codebook_", format(Sys.time(),'%Y%m%d_%H%M')), 
                            save_directory = "~/dev", 
                            overwrite = T, 
                            report_title = "Codebook", 
                            data_download_date = NULL, 
                            data_merge_date = NULL, 
                            output = "html") {

  # construct file names and paths
  rmd_file_name = paste0(file_name, ".Rmd")
  html_file_name = paste0(file_name, ".html")
  
  rmd_file_path <- file.path(save_directory, rmd_file_name)
  html_file_path <- file.path(save_directory, html_file_name)
  
  # delete existing files, if requested
  if (overwrite & file.exists(rmd_file_path)) {
    file.remove(rmd_file_path)
  }
  
  if (overwrite & file.exists(html_file_path)) {
    file.remove(html_file_path)
  }
  
  # open file connection
  file_connection <<- file(rmd_file_path, "w")

  ### HEADER ###
  
  if (!is.null(data_download_date) & !is.null(data_merge_date)) {
    subtitle <- paste0("Data Download Date: ", data_download_date, " | Data Merge Date: ", data_merge_date)
  } else {
    subtitle <- NULL
  }
  
  # write YAML preamble
  writer("---")
  writer(paste("title:", report_title))
  if (!is.null(subtitle)) writer(paste0('subtitle: \"', subtitle, '\"'))
  writer(paste("date:", format(Sys.time(),'%B %d, %Y %H:%M')))
  if (output == "pdf") writer("output: pdf_document")
  if (output == "html") writer("output: html_document")
  if (output == "docx") writer("output: word_document")
  writer("---")
  
  # load required packages in a hidden chunk
  secret_chunk_wrapper("library(ggplot2)\nlibrary(pander)")

  # write data overview
  writer("## Overview")
  labels <- c("Number of observations", "Number of variables")
  values <- c(nrow(df), length(df))
  overview <- data.frame(Feature = labels, Value = values)
  writer(pander::pandoc.table.return(overview))
  writer("***")
  
  # write TOC with selected metadata
  writer("## Summary Table")
  
  tab <- create_summary_table(df)
  writer(pander::pandoc.table.return(tab, split.tables = Inf))
  writer("***")
  
  ### LOOP: MAIN VARIABLE ###
  
  writer("## Variables")
  var_names <- names(df)
  
  # loop: for each variable, write out results
  for (i in 1:length(df)) {
    
    # determine type
    var <- df[[i]]
    var_name <- var_names[i]
    
    # write variable header
    writer(paste0('### ', var_name, ' <a id="', var_name, '"></a>\n')) 
    
    # write variable label (if defined)
    if (!is.null(attr(var, "label", exact = T))) {
      writer(paste("####", attr(var, "label", exact = T)))
    }

    # compute and tabulate variable metadata 
    var_meta <- summarize_var_metadata(var)
    var_meta <- pander::pandoc.table.return(var_meta)
    
    # compute and tabulate variable summary data 
    var_summary <- summarize_var_data(var)
    var_summary <- pander::pandoc.table.return(var_summary)
   
    # combine metadata table and data table into single string
    tab <- paste(var_meta, "\n\n", var_summary)

    # create variable plot
    vis <- draw_plot(var, var_name)

    # write tables in 3 columns
    row_wrapper(
      var_meta,
      var_summary,
      fig_wrapper(vis)
    )
    
    # write variable comments (if defined)
    if (!is.null(attr(var, "comments"))) {
      writer(paste0('\n**Comments:** ', attr(var, "comments")))
    }
    
    # write variable derivation (if defined)
    if (!is.null(attr(var, "derivation"))) {
      writer(paste0('\n**Derivation:** ', attr(var, "derivation")))
    }
    
    # write "Scroll to Top" link
    writer('\n<a href="#top">Scroll to Top</a>\n')
    
    # write divider before moving to next variable
    writer("***")
    writer("\n")
  }

  ### FOOTER ###
  
  # force flush and close connection
  flush(file_connection)
  close(file_connection)
  
  # knit document
  rmarkdown::render(rmd_file_path, 'html_document', html_file_name)
}