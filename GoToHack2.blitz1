structure <- read.csv(file = "/home/aleksandr/R/structure.csv", header = TRUE, sep = "," )
events <- read.csv(file = "/home/aleksandr/R/events.csv", header = TRUE, sep = ",")
point_events <- subset(structure, subset = step_cost == 1)
# steps that gives points
points_ids <- point_events[, "step_id"]
# event that gives points
p_events <- subset(events, subset = action == "passed" & step_id %in% points_ids)
#users with points
users <- levels(factor(p_events[, "user_id"]))
#users that passed the course 
compl_users <- matrix(, nrow = 0, ncol = 2)
for(i in users){
  time <- find.time(i)
  if(time != -1){
    compl_users <- rbind(compl_users, c(i , time))
  }
}
#display 10 fastest users 
compl_users[sort.list(as.numeric(compl_users[, 2])), ][1:10]
#find passing time of n-user
find.time <- function(n){
  curr.list <- subset(p_events, subset = user_id == n)
  curr.time <- curr.list[, "time"]
  curr.time
  if(length(curr.time) >= 24){
    full.list <- subset(events, subset = user_id ==n)
    first.time <- min(full.list[, "time"])
    final.time <- sort(curr.time)[24]
    final.time-first.time
  } else{
    -1
  }
}
