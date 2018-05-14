# Data Prep

raw.df <- readRDS("/Users/khanoa/box/VMAC BIOSTAT/DATA/MAP/rawData/temp/MAPfreeze_20180629.rds")
merged.df <- readRDS("/Users/khanoa/box/VMAC BIOSTAT/DATA/MAP/mergedData/interim/MAP_bh_d20180629_m20180629_interim.rds")

# move "label" attribute to "labels" or change code to work with "label" attribute
# remove "labelled" from variable classes

# all should be in static, epoch 0, and epoch 1

if (FALSE) {
  "
MAP ABP Consent (Static)
MAP APOE (Static)
MAP Selected Eligibility (Static)

MAP Eligibility
MAP APOE

MAP Enrollment
MAP Cardiac MRI Analysis
MAP Manual 3T Brain MRI
MAP Manual 3T Brain MRI Breath Hold
MAP Automated 3T Brain MRI Epoch 1
MAP Automated 3T Brain MRI Breath Hold
MAP Automated 3T Brain MRI Multiple Sessions
MAP Clinical CSF Epoch 1
MAP Biomarkers Epoch 1
MAP SRT Error Analysis Epoch 1
MAP Addendum Epoch 1
  "
}

projects.names <- c(
  "tracking.static", "apoe.static", "eligibility.static", "eligibility", "apoe", # epoch 0
  "main", "cardiac.mri", "man3T", "man3T.bh", "auto3T", "auto3T.bh", # epoch 1
  "auto3T.multiple.sessions", "csf", "biomarkers", "srt", "addendum" # epoch 1
)

projects.list <- sapply(projects.names, function(x) NULL)

for (i in seq_along(projects.names)) {
  project.name <- projects.names[i]
  epoch <- paste0("epoch_", ifelse(project.name %in% c("tracking.static", "apoe.static", "eligibility.static", "eligibility", "apoe"), 0, 1))
  
  projects.list[[i]] <- names(merged.df)[names(merged.df) %in% gsub("\\_", "\\.", names(raw.df[[epoch]][["data"]][[project.name]]))]
}

###

raw.var <- unique(unname(unlist(projects.list)))[order(match(unique(unname(unlist(projects.list))), names(merged.df)))]
derived.var <- setdiff(names(merged.df), raw.var)

###

mydat <- merged.df

for (i in seq_along(names(mydat))) { # remove labelled class
  class(mydat[[i]]) <- setdiff(class(mydat[[i]]), 'labelled') 
}

###

for (i in seq_along(raw.var)) {
  attr(mydat[[raw.var[i]]], "is.derived") <- "FALSE"
}

for (i in seq_along(derived.var)) {
  attr(mydat[[derived.var[i]]], "is.derived") <- "TRUE"
}

###

for (i in seq_along(names(mydat))) {
  attr(mydat[[i]], "comments") <- "None"
}
