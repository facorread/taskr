# rm("fabioLastItem")
if(!exists("fabioLastItem")) {
  source("common.R")
}

if(exists("taskr")) {
  cat("Sourcing script: no action taken.\n")
} else {
  if(file.exists("taskr.RData")) {
    # cat("taskr: ")
    load("taskr.RData", verbose = TRUE)
  } else {
    cat("Creating a new taskr dataset with a few example tasks.\n")
    taskr <- list(t = data.table(
      date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02", NA)),
      deadline = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02", NA)),
      name = c("Cooking", "Working out", "Studying", "Example task"),
      state = c("Pending", "Done"),
      projectName = c("Hosekeeping", "Loosing weight", "Studying", "Example project"),
      id = 1:4,
      parentTaskId = c(NA, NA, NA, 1),
      recurring = c(TRUE, TRUE, TRUE, FALSE)
    ))
    setkey(taskr$t, id)
  }
}

cat("Please copy commands from this script into the R console. Remember to save your work!\nStopping now.\n")

# Important notes
# Do not save colors as data. It is just a hassle. Go with ggplot2 default colors.
# The difference between deadline and date is that date is flexible: it can be today or the day I completed the task. The deadline usually comes from external info.

function() {
return(NULL)
save(taskr, file = "taskr.RData")

# Show recent tasks
tid <- taskr$t[date == Sys.Date(), id]
tid <- taskr$t[is.na(date), id]
tid <- taskr$t[taskr.re(date, -30, -1) & (name == "Workout"), id]
tid <- taskr$t[taskr.re(date) & (state == "Pending"), id]
tid <- taskr$t[taskr.re(date, -30, 0) & (state == "Pending"), id]
tid <- taskr$t[taskr.re(date, -30, 10) & (state == "Pending") & !recurring, id]
taskr$t[tid, .N, by = name]
taskr.show(taskr$t[tid])
taskr$t[tid]

# Search
tid <- taskr$t[projectName %like% "proposals", id]
tid <- taskr$t[name %like% "rade", id]
tid <- taskr$t[(name %like% "hinese") & (date == "2020-01-31"), id]
tid <- taskr$t[projectName %like% "ayes", id]
tid <- taskr$t[is.na(projectName), id]
tid <- taskr$t[taskr.re(date) & (state == "Pending") & (name == "Study Chinese"), id]
tid <- taskr$t[(date == Sys.Date()) & (state == "Pending"), id]
tid <- taskr$t[(date == Sys.Date()) & (name == "Banking"), id]
taskr.show(taskr$t[tid])
taskr$t[tid]

# Show a project
tid <- taskr$t[projectName == "ABM Paper", id]
tid <- taskr$t[projectName == "Writing proposals", id]
tid <- taskr$t[(projectName == "Weight loss") & (is.na(date) | date <= Sys.Date()), id]
taskr.show(taskr$t[tid])
taskr$t[tid]

# Get things done
taskr$t[tid]
taskr$t[tid, state := "Done"]
taskr$t[tid, state := "Abandoned"]

# New task, run each line separately as needed
tid <- 1 + max(taskr$t$id)
taskr$t <- rbindlist(list(taskr$t, data.table(id = tid, state = "Pending", recurring = FALSE)), fill = TRUE) # https://stackoverflow.com/a/16797392/870609
taskr$t[tid, date := Sys.Date()]
taskr$t[tid, date := as.Date("2020-01-29")]
taskr$t[tid, name := "Banking"]
taskr$t[tid, name := "Class prep"]
taskr$t[tid, name := "Cleaning the carpet"]
taskr$t[tid, name := "Cleaning the shower"]
taskr$t[tid, name := "Reshape worklog script"]
taskr$t[tid, name := "Grade labs"]
taskr$t[tid, name := "Laundry"]
taskr$t[tid, name := "Redo problem description WG"]
taskr$t[tid, name := "Redo theoretical page WG"]
taskr$t[tid, name := "Schedule consultation with the Writing Center"]
taskr$t[tid, name := "Study Chinese"]
taskr$t[tid, name := "Washing the carpet"]
taskr$t[tid, name := paste0("Weigh less than ", newWeights, " lb")]
taskr$t[tid, name := "Workout"]
taskr$t[tid, name := "Write methodology"]
taskr$t[tid, state := "Pending"]
taskr$t[tid, state := "Done"]
taskr$t[tid, projectName := "Housekeeping"]
taskr$t[tid, projectName := "ABM paper"]
taskr$t[tid, projectName := "Bayes paper"]
taskr$t[tid, projectName := "Study Chinese"]
taskr$t[tid, projectName := "Weight loss"]
taskr$t[tid, projectName := "Writing proposals"]
taskr$t[tid, projectName := "Other"]
taskr$t[tid, projectName := "Teaching"]
taskr$t[tid, projectName := "Teaching nomination"]
taskr$t[tid, projectName := "OSU"]
taskr$t[tid, projectName := "HCLab"]
taskr$t[tid, parentTaskId := 0]
taskr$t[tid, recurring := FALSE]
taskr$t[tid, recurring := TRUE]

# Show the new task
taskr.show(taskr$t[tid])
taskr$t[tid]

# New recurring task
newDates = seq(Sys.Date(), by = 1, length.out = 20)
newDates = seq(Sys.Date(), to = as.Date("2020-12-31"), by = 1)
newDates = seq(as.Date("2020-02-02"), to = as.Date("2020-12-31"), by = 14)
newDates = seq(as.Date("2020-01-28"), length.out = 7, by = 14)
tid <- seq_along(newDates) + max(taskr$t$id)
taskr$t <- rbindlist(list(taskr$t, data.table(id = tid, state = "Pending", recurring = TRUE)), fill = TRUE) # https://stackoverflow.com/a/16797392/870609
taskr$t[tid, date := newDates]
# Reuse the code above to set the other fields of the task

# Full list
taskr$t
names(taskr$t)

# Troubleshooting
duplicatedIds <- taskr$t[duplicated(id), id]
taskr$t[(id %in% duplicatedIds)]
taskr$t[(id %in% duplicatedIds) & is.na(name),]
tid <- 1 + max(taskr$t$id)
taskr$t[(id %in% duplicatedIds) & is.na(name), id := tid]
taskr$t[(id %in% duplicatedIds) & name == "Redo problem description WG", id := tid]
setkey(taskr$t, id)

}

# Leave this space empty!
NULL
