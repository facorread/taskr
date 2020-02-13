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
    setkey(taskr$t, id)
    setkey(taskr$tl, Date)
  } else {
    cat("Creating a new taskr dataset with a few example tasks.\n")
    taskr <- list(t = data.table(
      id = 1:4,
      state = c("Pending", "Done"),
      Task = c("Cooking", "Working out", "Studying", "Example task"),
      Project = c("Housekeeping", "Weight loss", "Studying", "Example project"),
      Date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02", NA)),
      Deadline = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02", NA)),
      parentTaskId = c(NA, NA, NA, 1),
      recurring = c(TRUE, TRUE, TRUE, FALSE)
    ),
      tl = data.table(id = 1, # Mandatory, for data.table indexing
        Date = as.Date("2020-02-10"),
        Task = "Yearly report",
        totalWords = 2591 + 636,
        newWordsFromElsewhere = 0,
        wordsInRemovedDocuments = 0
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
tid <- taskr$t[is.na(Date) & (state == "Pending"), id]; taskr.show(taskr$t[tid])
tid <- taskr$t[(Date < Sys.Date()) & (state == "Pending"), id]
tid <- taskr$t[Date == Sys.Date(), id]; taskr.show(taskr$t[tid])
tid <- taskr$t[taskr.re(Date, -30, -1), id]
tid <- taskr$t[taskr.re(Date, -30, -1) & (Task == "Workout"), id]
tid <- taskr$t[taskr.re(Date) & (state == "Pending"), id]
tid <- taskr$t[(Date < Sys.Date()) & (state == "Pending") & (Task == "Study Chinese"), id]
tid <- taskr$t[taskr.re(Date, -30, 10) & (state == "Pending") & !recurring, id]
taskr$t[tid, .N, by = Task]
taskr.show(taskr$t[tid])
taskr$t[tid]

# Search
tid <- taskr$t[Project %like% "proposals", id]
tid <- taskr$t[Project == "Studying", id]
tid <- taskr$t[Task %like% "eer", id]
tid <- taskr$t[(Task %like% "orkout") & (state == "Pending") & (Date < Sys.Date()), id]
tid <- taskr$t[is.na(Date), id]
tid <- taskr$t[Date == as.Date("2020-03-01"), id]
tid <- taskr$t[taskr.re(Date) & (state == "Pending") & (Task == "Study Chinese"), id]
tid <- taskr$t[(Date < Sys.Date()) & (state == "Pending"), id]
tid <- taskr$t[(Date == Sys.Date()) & (Task == "Banking"), id]
taskr$t[, .N, by = Project]
taskr.show(taskr$t[tid])
taskr$t[tid]

# Show a project
tid <- taskr$t[Task == "Schedule consultation with the Writing Center", id]
tid <- taskr$t[(Project == "Writing proposals") & (Date > "2020-02-15"), id]
tid <- taskr$t[(Project == "Weight loss") & (is.na(Date) | Date <= Sys.Date()), id]
taskr.show(taskr$t[tid])
taskr$t[tid]

# Get things done
# taskr.show(taskr$t[tid <- 353]) # Do not do this

tid <- c(361, 21); taskr.show(taskr$t[tid])
taskr$t[tid, state := "Done"]
taskr$t[tid, state := "Abandoned"]

# Create new task
tid <- 1 + max(taskr$t$id)
taskr$t <- rbindlist(list(taskr$t, data.table(id = tid, state = "Pending", recurring = FALSE)), fill = TRUE) # https://stackoverflow.com/a/16797392/870609
setkey(taskr$t, id)
taskr$t[tid, Date := Sys.Date()]
taskr$t[tid, Date := as.Date("2020-04-15")]
taskr$t[tid, Deadline := Date]
taskr$t[tid, Deadline := as.Date("2020-04-15")]
taskr$t[tid, Task := "Banking"]
taskr$t[tid, Task := "Candidacy improvement plan"]
taskr$t[tid, Task := "Class prep"]
taskr$t[tid, Task := "Cleaning the carpet"]
taskr$t[tid, Task := "Create teaching portfolio"]
taskr$t[tid, Task := "Reshape worklog script"]
taskr$t[tid, Task := "Grade labs"]
taskr$t[tid, Task := "Grade exams"]
taskr$t[tid, Task := "Laundry"]
taskr$t[tid, Task := "Mail my taxes"]
taskr$t[tid, Task := "Peer review"]
taskr$t[tid, Task := "Qualifications WG"]
taskr$t[tid, Task := "Schedule Writing Center"]
taskr$t[tid, Task := "Study Chinese"]
taskr$t[tid, Task := "Study of empty households"]
taskr$t[tid, Task := "Washing the carpet"]
taskr$t[tid, Task := paste0("Weigh less than ", newWeights, " lb")]
taskr$t[tid, Task := "Workout"]
taskr$t[tid, Task := "Write methodology"]
taskr$t[tid, Task := "Yearly report"]
taskr$t[tid, Project := "ABM paper"]
taskr$t[tid, Project := "Bayes paper"]
taskr$t[tid, Project := "Candidacy"]
taskr$t[tid, Project := "HCLab"]
taskr$t[tid, Project := "Housekeeping"]
taskr$t[tid, Project := "OSU"]
taskr$t[tid, Project := "Other"]
taskr$t[tid, Project := "Teaching"]
taskr$t[tid, Project := "Teaching nomination"]
taskr$t[tid, Project := "Study Chinese"]
taskr$t[tid, Project := "Weight loss"]
taskr$t[tid, Project := "Writing proposals"]
taskr$t[tid, parentTaskId := 0]
taskr$t[tid, state := "Pending"]
taskr$t[tid, state := "Done"]
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
setkey(taskr$t, id)
taskr$t[tid, Date := newDates]
# Reuse the code above to set the other fields of the task

# Full list
taskr$t
names(taskr$t)

# Timeline: new record
tlid <- 1 + max(taskr$tl$id)
taskr$tl <- rbindlist(list(taskr$tl, data.table(id = tlid)), fill = TRUE) # https://stackoverflow.com/a/16797392/870609
setkey(taskr$tl, id) # We cannot use the field Date as a key; we can only use the integer "id."
taskr$tl[tlid, Date := Sys.Date()]
taskr$tl[tlid, Task := "Academic portfolio"]
taskr$tl[tlid, totalWords := 5000 + 1000]
taskr$tl[tlid, newWordsFromElsewhere := 0]
taskr$tl[tlid, wordsInRemovedDocuments := 0]
print(taskr$tl, row.names = FALSE)

# Troubleshooting
duplicatedIds <- taskr$t[duplicated(id), id]
taskr$t[(id %in% duplicatedIds)]
taskr$t[(id %in% duplicatedIds) & is.na(Task),]
tid <- 1 + max(taskr$t$id)
taskr$t[(id %in% duplicatedIds) & is.na(Task), id := tid]
taskr$t[(id %in% duplicatedIds) & Task == "Redo problem description WG", id := tid]
setkey(taskr$t, id)

# Housekeeping
setcolorder(taskr$t, c(
"id", "state", "Task", "Project", "Date", "Deadline", "parentTaskId", "recurring"
))
setcolorder(taskr$tl, c(
"id", "Date", "Task"
))
names(taskr$t)

# Any new content must go above this line.
}
NULL
