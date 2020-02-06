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
      id = 1:4,
      state = c("Pending", "Done"),
      name = c("Cooking", "Working out", "Studying", "Example task"),
      projectName = c("Hosekeeping", "Loosing weight", "Studying", "Example project"),
      date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02", NA)),
      deadline = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02", NA)),
      parentTaskId = c(NA, NA, NA, 1),
      recurring = c(TRUE, TRUE, TRUE, FALSE)
    ))
    setkey(taskr$t, id)
  }
}

cat("Please copy commands from this script into the R console. Remember to save your work!\nStopping now.\n")

# Important notes
# RStudio has glitches with .RData. Do not use.
# o RStudio prioritizes the data.frame class, ignoring [.data.table.
# o The environment explorer does not show the loaded .RData; you have to click the refresh button. Run this script instead.
# o .RData keeps leftovers from old versions of taskr.
# The difference between deadline and date is that date is flexible: it can be today or the day I completed the task. The deadline usually comes from external info.
# Do not save colors as data. It is just a hassle. Go with ggplot2 default colors.

function() {
return(NULL)
save(taskr, file = "taskr.RData")

# Show recent tasks
tid <- taskr$t[date == Sys.Date(), id]
tid <- taskr$t[is.na(date), id]
tid <- taskr$t[taskr.re(date, -30, -1), id]
tid <- taskr$t[taskr.re(date, -30, -1) & (name == "Workout"), id]
tid <- taskr$t[taskr.re(date) & (state == "Pending"), id]
tid <- taskr$t[(date < Sys.Date()) & (state == "Pending"), id]
tid <- taskr$t[taskr.re(date, -30, 10) & (state == "Pending") & !recurring, id]
taskr$t[tid, .N, by = name]
taskr.show(taskr$t[tid])
taskr$t[tid]

# Search
tid <- taskr$t[projectName %like% "proposals", id]
tid <- taskr$t[projectName == "OSU", id]
tid <- taskr$t[name %like% "Class prep", id]
tid <- taskr$t[(name %like% "orkout") & (state == "Pending") & (date < Sys.Date()), id]
tid <- taskr$t[is.na(date), id]
tid <- taskr$t[taskr.re(date) & (state == "Pending") & (name == "Study Chinese"), id]
tid <- taskr$t[(date < Sys.Date()) & (state == "Pending"), id]
tid <- taskr$t[(date == Sys.Date()) & (name == "Banking"), id]
taskr$t[, .N, by = projectName]
taskr.show(taskr$t[tid])
taskr$t[tid]

# Show a project
tid <- taskr$t[projectName == "ABM Paper", id]
tid <- taskr$t[projectName == "Writing proposals", id]
tid <- taskr$t[(projectName == "Weight loss") & (is.na(date) | date <= Sys.Date()), id]
taskr.show(taskr$t[tid])
taskr$t[tid]

# Get things done
# taskr.show(taskr$t[tid <- 353]) # Do not do this

tid <- 13; taskr.show(taskr$t[tid])
taskr$t[tid]
taskr$t[tid, state := "Done"]
taskr$t[tid, state := "Abandoned"]

# Create new task
tid <- 1 + max(taskr$t$id)
taskr$t <- rbindlist(list(taskr$t, data.table(id = tid, state = "Pending", recurring = FALSE)), fill = TRUE) # https://stackoverflow.com/a/16797392/870609
taskr$t[tid, date := Sys.Date()]
taskr$t[tid, deadline := as.Date("2020-04-15")]
taskr$t[tid, name := "Banking"]
taskr$t[tid, name := "Candidacy improvement plan"]
taskr$t[tid, name := "Class prep"]
taskr$t[tid, name := "Cleaning the carpet"]
taskr$t[tid, name := "Cleaning the shower"]
taskr$t[tid, name := "Create teaching portfolio"]
taskr$t[tid, name := "Reshape worklog script"]
taskr$t[tid, name := "Grade labs"]
taskr$t[tid, name := "Laundry"]
taskr$t[tid, name := "Mail my taxes"]
taskr$t[tid, name := "Peer review"]
taskr$t[tid, name := "Qualifications WG"]
taskr$t[tid, name := "Schedule consultation with the Writing Center"]
taskr$t[tid, name := "Study Chinese"]
taskr$t[tid, name := "Study of empty households"]
taskr$t[tid, name := "Washing the carpet"]
taskr$t[tid, name := paste0("Weigh less than ", newWeights, " lb")]
taskr$t[tid, name := "Workout"]
taskr$t[tid, name := "Write methodology"]
taskr$t[tid, name := "Yearly report"]
taskr$t[tid, state := "Pending"]
taskr$t[tid, state := "Done"]
taskr$t[tid, projectName := "Candidacy"]
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
taskr.show(tail(taskr$t))
tail(taskr$t)

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

# Housekeeping
setcolorder(taskr$t, c(
"id", "state", "name", "projectName", "date", "deadline", "parentTaskId", "recurring"
))

# Any new content must go above this line.
}
NULL
