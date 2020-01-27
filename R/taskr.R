# rm("fabioLastItem")
if(!exists("fabioLastItem")) {
  source("common.R")
}

cat("Most of this script is for copying and pasting on the console. Stopping now.\n")

function() {
  load("taskr.RData")
  taskr <- list(t = data.table(
    date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02", NA)),
    name = c("Cooking", "Working out", "Studying", "Example task"),
    state = c("Pending", "Done"),
    projectName = c("Hosekeeping", "Loosing weight", "Studying", "Example project"),
    id = 1:4,
    parentTaskId = c(NA, NA, NA, 1),
    recurring = c(TRUE, TRUE, TRUE, FALSE),
    colorh = c(0.3, 0.5, 0.7, 0.8),
    colors = 1,
    colorv = 0.6
  ))
  setkey(taskr$t, id)
  save(taskr, file = "taskr.RData")
}

# Show recent tasks
taskr$t[date == Sys.Date()]
taskr$t[date > Sys.Date() - 30]
taskr$t[taskr.re(date) & (state == "Pending")]

# New task, run each line separately as needed
newId <- 1 + max(taskr$t$id)
taskr$t <- rbindlist(list(taskr$t, data.table(id = newId, state = "Pending", recurring = FALSE)), fill = TRUE) # https://stackoverflow.com/a/16797392/870609
taskr$t[newId, date := Sys.Date()]
taskr$t[newId, date := as.Date("2020-01-28")]
taskr$t[newId, name := "Study Chinese"]
taskr$t[newId, name := "Workout"]
taskr$t[newId, state := "Pending"]
taskr$t[newId, state := "Done"]
taskr$t[newId, projectName := "Housekeeping"]
taskr$t[newId, projectName := "ABM paper"]
taskr$t[newId, projectName := "Bayes paper"]
taskr$t[newId, projectName := "Study Chinese"]
taskr$t[newId, projectName := "Weight loss"]
taskr$t[newId, projectName := "Writing proposals"]
taskr$t[newId, projectName := "Other"]
taskr$t[newId, projectName := "Teaching nomination"]
taskr$t[newId, projectName := "Review Mark Moritz"]
taskr$t[newId, projectName := "Review Julie Field"]
taskr$t[newId, projectName := "Review Ian Hamilton"]
taskr$t[newId, projectName := "Review Sean Downey"]
taskr$t[newId, projectName := "OSU"]
taskr$t[newId, projectName := "HCLab"]
taskr$t[newId, parentTaskId := 0]
taskr$t[newId, recurring := FALSE]
taskr$t[newId, recurring := TRUE]
taskr$t[newId, colorh := 0.4]
taskr$t[newId, colors := 1]
taskr$t[newId, colorv := 0.6]

# Show the new task
taskr.show(taskr$t[newId])
taskr$t[newId]

# New recurring task
newDates = seq(Sys.Date(), by = 1, length.out = 20)
newDates = seq(Sys.Date(), to = as.Date("2020-12-31"), by = 1)
newDates = seq(Sys.Date(), to = as.Date("2020-04-20"), by = 1)
newId <- seq_along(newDates) + max(taskr$t$id)
taskr$t <- rbindlist(list(taskr$t, data.table(id = newId, state = "Pending", recurring = TRUE)), fill = TRUE) # https://stackoverflow.com/a/16797392/870609
taskr$t[newId, date := newDates]
# Reuse the code above to set the other fields of the task

# Full list
taskr$t
names(taskr$t)

NULL
