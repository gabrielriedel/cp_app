library(tidyverse)
library(collegebaseball)
library(stringr)

ncaa_school_id_lookup(team_name = "Cal Poly", season =  2025)
  
cp_schedule <- ncaa_schedule(596504, year=2025)

cp_games <- cp_schedule$contest_id



ses <- chromote::ChromoteSession$new()
game_pbp <- ncaa_pbp(6300616)
ses$close()

write.csv(game_pbp, "game_pbp.csv", row.names=FALSE)



View(mapped_table)


library(data.table)

dt <- fread("game_pbp.csv")
dt[, description := fifelse(is.na(description), "", description)]
setorder(dt, game_id, date, inning, inning_top_bot)  # keep stable ordering

# helper: split NCAA-style advancement clauses
get_clauses <- function(desc) {
  parts <- unlist(strsplit(desc, "3a\\s*"))
  if (length(parts) <= 1) character(0) else trimws(gsub("[.]$", "", parts[-1]))
}

# update state from one row
update_state <- function(desc, on1, on2, on3) {
  d <- tolower(desc)

  # --- Batter outcome (very lightweight rules)
  if (grepl("home run|homered", d)) {
    on1 <- on2 <- on3 <- FALSE
  } else if (grepl("\\btripled\\b", d)) {
    on3 <- TRUE
  } else if (grepl("\\bdoubled\\b", d)) {
    on2 <- TRUE
  } else if (grepl("\\bsingled\\b|\\bwalked\\b|hit by pitch|reached on .*error|reached on fielder", d)) {
    on1 <- TRUE
  }

  # --- Advancement / scoring clauses
  clauses <- get_clauses(desc)
  for (cl in clauses) {
    c <- tolower(cl)

    if (grepl("advanced to second", c)) {
      if (on1) { on1 <- FALSE; on2 <- TRUE }
    } else if (grepl("advanced to third", c)) {
      if (on2) { on2 <- FALSE; on3 <- TRUE }
      else if (on1 && !on2) { on1 <- FALSE; on3 <- TRUE }  # fallback
    } else if (grepl("\\bscored\\b", c)) {
      if (on3) on3 <- FALSE else if (on2) on2 <- FALSE else if (on1) on1 <- FALSE
    } else if (grepl("caught stealing", c) || grepl("picked off", c)) {
      # heuristic: remove the “most likely” runner
      if (grepl("second", c) && on1) on1 <- FALSE
      else if (grepl("third", c) && on2) on2 <- FALSE
      else if (on1) on1 <- FALSE else if (on2) on2 <- FALSE else if (on3) on3 <- FALSE
    }
  }

  list(on1=on1, on2=on2, on3=on3)
}

# run state machine within each half-inning
dt[, c("on1_pre","on2_pre","on3_pre","on1_post","on2_post","on3_post") := {
  on1 <- on2 <- on3 <- FALSE
  on1_pre <- on2_pre <- on3_pre <- logical(.N)
  on1_post <- on2_post <- on3_post <- logical(.N)

  for (i in seq_len(.N)) {
    on1_pre[i] <- on1; on2_pre[i] <- on2; on3_pre[i] <- on3
    s <- update_state(description[i], on1, on2, on3)
    on1 <- s$on1; on2 <- s$on2; on3 <- s$on3
    on1_post[i] <- on1; on2_post[i] <- on2; on3_post[i] <- on3
  }

  list(on1_pre,on2_pre,on3_pre,on1_post,on2_post,on3_post)
}, by=.(game_id, inning, inning_top_bot)]



library(baseballr)
library(dplyr)

