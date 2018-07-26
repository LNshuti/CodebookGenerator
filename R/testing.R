testing <- function(){

## testing
library(forcats)
data(gss_cat)
#createCodebook(iris, outname="iris")

# gss <- readRDS("gss_short.rds")
# df <-gss
createCodebook(gss, outname = "gss")
#createCodebook(dummyData(), outname="dummyData")
createCodebook(iris, outname="iris")
createCodebook(mtcars, outname="mtcars")


library(haven)
reach <- read_sas("/researchdata/CPS/assessments/reach/reach_2017.sas7bdat")
createCodebook(reach)
}

###

varnames <- c("map.id", "epoch", sample(grep("np\\.", names(mydat), v = T), 20, replace = FALSE))

attr(mydat$np.strp.ucerr.elig, "derivation") <- "Code will go here."
 
createCodebook(mydat[, varnames], outname = "datadict-neuropsych", reportTitle = "Data Dictionary (Selected Neuropsych Variables)")

###

vars_w_proj <- data.frame(
  var = character(),
  proj = character(),
  stringsAsFactors = FALSE
)

for (i in seq_along(names(projects.list))) {
  for (j in seq_along(projects.list[[i]])) {
    temp <- data.frame(var = projects.list[[i]][[j]], proj = names(projects.list)[i])
    vars_w_proj <- rbind(vars_w_proj, temp, make.row.names = FALSE, stringsAsFactors = FALSE)
  }
}

vars_w_proj <- vars_w_proj[order(match(vars_w_proj$var, names(merged.df)), match(vars_w_proj$proj, names(projects.list))), ]

vars_w_proj <- vars_w_proj %>%
  group_by(var) %>%
  summarize(projects = paste(proj, collapse = ", "))

write.csv(vars_w_proj, file = "~/dev/vars_w_proj.csv", row.names = FALSE)

###

df <- mydat[, c("map.id", "epoch", grep("\\.notes", grep("^np\\.", names(mydat), v = T), invert = T, v = T))]

start_time <- Sys.time()
createCodebook(df, outname = "neuropsych-all")
end_time <- Sys.time()
end_time - start_time