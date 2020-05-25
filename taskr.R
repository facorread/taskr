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
      Task = c("Cooking", "Working out", "Studying", "Example task"),
      Project = c("Housekeeping", "Weight loss", "Studying", "Example project"),
      Date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02", NA)),
      Deadline = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02", NA)),
      recurring = c(TRUE, TRUE, TRUE, FALSE)
    ),
      tl = data.table(id = 1, # Mandatory, for data.table indexing
        Date = as.Date("2020-02-10"),
        Task = "Yearly report",
        Words = 2591 + 636,
        Forms = 0, # New words that come from elsewhere; for example, forms.
        Remove = 0 # Words in removed documents; for example, completed homework.
      ),
      pb = data.table(id = 1, # Mandatory, for data.table indexing
        Task = "Restroom",
        Start = ISOdatetime(2020, 05, 19, 06, 00, 00),
        End = ISOdatetime(2020, 05, 19, 07, 00, 00)
        )
      )
  }
  setkey(taskr$t, id)
  setkey(taskr$tl, id)
  setkey(taskr$pb, id)
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
tid <- taskr$t[is.na(Date) & (state == "Pending"), id]; showt(taskr$t[tid])
tid <- taskr$t[(is.na(Date) | (Date <= Sys.Date())) & (state == "Pending"), id]; showt(taskr$t[tid])
tid <- taskr$t[(Date <= Sys.Date()) & (state == "Pending"), id]; showt(taskr$t[tid])
tid <- taskr$t[Date == Sys.Date(), id]; showt(taskr$t[tid])
tid <- taskr$t[(Date < Sys.Date()) & (state == "Pending") & (Task == "Study Chinese"), id]; showt(taskr$t[tid])
tid <- taskr$t[(Date < Sys.Date()) & (state == "Pending") & (Task == "Workout"), id]; showt(taskr$t[tid])
tid <- taskr$t[(Date < Sys.Date()) & (state == "Pending") & (Project == "Teaching"), id]; showt(taskr$t[tid])
tid <- taskr$t[(Date <= Sys.Date()) & (state == "Pending") & (Project == "Housekeeping"), id]; showt(taskr$t[tid])
taskr$t[tid, .N, by = Task]
showt(taskr$t[tid])
taskr$t[tid]

# Get things done
# showt(taskr$t[tid <- 353]) # Do not do this

tid <- 865; showt(taskr$t[tid])
taskr$t[tid, state := "Done"]
taskr$t[tid, state := "Abandoned"]

# Create new task
tid <- 1 + max(taskr$t$id)
taskr$t <- rbindlist(list(taskr$t, data.table(id = tid, state = "Pending", recurring = FALSE)), fill = TRUE) # https://stackoverflow.com/a/16797392/870609
setkey(taskr$t, id)
taskr$t[tid, Date := Sys.Date()]
taskr$t[tid, Date := as.Date("2020-08-27")]
taskr$t[tid, Deadline := Date]
taskr$t[tid, Deadline := as.Date("2020-04-15")]
taskr$t[tid, Task := "Banking"]
taskr$t[tid, Task := "Class prep"]
taskr$t[tid, Task := "Cleaning the carpet"]
taskr$t[tid, Task := "Create teaching portfolio"]
taskr$t[tid, Task := "Reshape worklog script"]
taskr$t[tid, Task := "Grade labs"]
taskr$t[tid, Task := "Grade exams"]
taskr$t[tid, Task := "Laundry"]
taskr$t[tid, Task := "Peer review"]
taskr$t[tid, Task := "Qualifications WG"]
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
taskr$t[tid, state := "Pending"]
taskr$t[tid, state := "Done"]
taskr$t[tid, recurring := FALSE]
taskr$t[tid, recurring := TRUE]

# Show the new task
showt(taskr$t[tid])
taskr$t[tid]
print(tail(taskr$t), row.names = FALSE)

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
taskr$tl[tlid, Date := Sys.Date() - 1]
taskr$tl[tlid, Date := as.Date("2020-02-24")]
taskr$tl[tlid, Task := "Proposals: WG5 Contributions Notes"]
taskr$tl[tlid, Words := 0]
taskr$tl[tlid, Task := "Exam Q3"]
taskr$tl[tlid, Words := 287]
taskr$tl[tlid, Forms := 0]
taskr$tl[tlid, Task := "Exam Q3 Notes"]
taskr$tl[tlid, Words := 382]
taskr$tl[tlid, Forms := 0]
# Forms: Positive number, enter only once the day of introduction: New words that come from elsewhere; for example, forms. Another example: the references section.

# Timeline: Copy documents from last time into yesterday
tmp <- list()
tmp$newTasks <- taskr$tl[(Date == fabioLarger(Date, 1)) & (Words > 0) & !(Task %like% "Q3") & !(Task %like% "ntee")]
tmp$newTasks
setkey(tmp$newTasks, NULL)
tmp$newTasks[, id := id + max(taskr$tl$id) + 1 - min(id)]
tmp$newTasks[, Date := as.Date("2020-02-24")]
tmp$newTasks[, Remove := Words]
tmp$newTasks[, Words := 0]
tmp$newTasks[, Forms := 0]
tmp$newTasks

taskr$tl <- rbindlist(list(taskr$tl, tmp$newTasks)) # https://stackoverflow.com/a/16797392/870609
setkey(taskr$tl, id) # We cannot use the field Date as a key; we can only use the integer "id."


# Remove: Positive number, enter only once the day after last writing: Words in removed documents; for example, completed homework.



print(tail(taskr$tl), row.names = FALSE)
print(taskr$tl[Date %in% c(NA, fabioMaxima(Date, 2))], row.names = FALSE)
print(taskr$tl[Task %like% "WG5"], row.names = FALSE)
print(taskr$tl[tlid], row.names = FALSE)
print(taskr$tl, row.names = FALSE)
tlid <- 1; print(taskr$tl[tlid], row.names = FALSE)

# Re-sorting the timeline can be useful
tlid <- NULL
taskr$tl[, idate := as.integer(Date)]
setkey(taskr$tl, idate, Words, Forms, Remove)
taskr$tl[, id := .I]
setkey(taskr$tl, id) # We cannot use the field Date as a key; we can only use the integer "id."
taskr$tl[, idate := NULL]
print(taskr$tl, row.names = FALSE)

# Personal best: new record
pid <- 1 + max(taskr$pb$id)
taskr$pb <- rbindlist(list(taskr$pb, data.table(id = pid, Start = Sys.time())), fill = TRUE) # https://stackoverflow.com/a/16797392/870609
setkey(taskr$pb, id)
taskr$pb[pid, Task := "Cleaning the kitchen"]
taskr$pb[pid, Task := "Restroom"]
taskr$pb[pid, Task := "Cooking"]
taskr$pb[pid, Task := "Haircut"]
taskr$pb[pid, Task := "Ironing"]
taskr$pb[pid, Task := "Making breakfast"]
taskr$pb[pid, Task := "Meeting prep"]
taskr$pb[pid, Task := "Pack and take out the trash"]
taskr$pb[pid, Task := "Shower"]
taskr$pb[pid, End := Sys.time()]
taskr$pb[pid, secs := as.integer(difftime(End, Start, units = "secs"))]
taskr$pb[pid, Duration := paste0(secs %/% 60, "m", secs %% 60, "s")]
save(taskr, file = "taskr.RData")
showpb(taskr$pb, pid)
taskr$pb

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
"id", "state", "Task", "Project", "Date", "Deadline", "recurring"
))
setcolorder(taskr$tl, c(
"id", "Date", "Task", "Words", "Forms", "Remove"
))

# Any new content must go above this line.
}
NULL
