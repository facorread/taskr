# This looks alright but it overcomplicates the code.
overdueE <- parse(text='(date < Sys.Date()) & (state == "Pending")')
taskr$t[eval(overdueE)]
