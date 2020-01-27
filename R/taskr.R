# rm("fabioLastItem")
if(!exists("fabioLastItem")) {
  source("common.R")
}

cat("Most of this script is for copying and pasting on the console. Stopping now.\n")

function() {
  load("taskr.RData")
  # taskr <- list(t = data.table(
  #   date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02", NA)),
  #   name = c("Cooking", "Working out", "Studying", "Example task"),
  #   state = factor(c(1, 1, 2, 1), labels = c("Pending", "Done")),
  #   projectName = c("Hosekeeping", "Loosing weight", "Studying", "Example project"),
  #   id = 1:4,
  #   parentTaskId = c(NA, NA, NA, 1)
  # ))
  save(taskr, file = "taskr.RData")
}

taskr$t

NULL
