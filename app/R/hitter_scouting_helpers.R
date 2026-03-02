# Hitter Scouting Report Helper Functions
# Helper functions for the opposing hitter scouting report feature

library(cachem)
library(memoise)
library(jsonlite)

# Use the same cache as scouting_helpers.R
if (!exists("scouting_cache")) {
  scouting_cache <- cache_mem(max_size = 500 * 1024^2, max_age = 3600)
}

# Pitch type mapping for hitter scouting
pitch_type_map <- list(
  "4SM" = c("Fastball", "Four-Seam", "FourSeam", "FourSeamFastBall"),
  "2SM" = c("Sinker", "Two-Seam", "TwoSeam", "TwoSeamFastBall"),
  "CT" = c("Cutter"),
  "CB" = c("Curveball", "Curve"),
  "SL" = c("Slider", "Sweeper"),
  "CH" = c("Changeup", "ChangeUp", "Splitter")
)

#' Map a pitch type to its abbreviated form
#' @param pitch_type The original pitch type name
#' @return The abbreviated pitch type (4SM, 2SM, CT, CB, SL, CH) or original if no match
map_pitch_type <- function(pitch_type) {
  if (is.na(pitch_type) || is.null(pitch_type)) return(NA_character_)
  for (abbrev in names(pitch_type_map)) {
    if (pitch_type %in% pitch_type_map[[abbrev]]) {
      return(abbrev)
    }
  }
  pitch_type  # Return original if no match
}

#' Get all batters from a team that have faced CAL_MUS pitchers
#' @param pool Database connection pool
#' @param team_code Team code
#' @return Character vector of batter names
get_opposing_batters <- function(pool, team_code) {
  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT DISTINCT batter
      FROM core_level.trackman_event
      WHERE batterteam = $1
        AND batter IS NOT NULL
        AND batter != ''
        AND EXTRACT(YEAR FROM date) >= 2026
      ORDER BY batter
    ", params = list(team_code))$batter
  }, error = function(e) {
    message("Error fetching opposing batters: ", e$message)
    character(0)
  })
  result
}

#' Get pitch data for a specific batter filtered by pitcher handedness
#' @param pool Database connection pool
#' @param batter_name Name of the batter
#' @param start_date Start date for filtering
#' @param end_date End date for filtering
#' @param pitcher_hand Pitcher handedness ("Right" or "Left")
#' @return Data frame with pitch data
get_batter_pitch_data <- function(pool, batter_name, start_date, end_date, pitcher_hand) {
  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT
        taggedpitchtype,
        platelocside,
        platelocheight,
        pitchcall,
        playresult,
        exitspeed,
        angle,
        balls,
        strikes,
        pitcherthrows
      FROM core_level.trackman_event
      WHERE batter = $1
        AND date BETWEEN $2 AND $3
        AND pitcherthrows = $4
        AND taggedpitchtype IS NOT NULL
        AND platelocside IS NOT NULL
        AND platelocheight IS NOT NULL
    ", params = list(batter_name, start_date, end_date, pitcher_hand))
  }, error = function(e) {
    message("Error fetching batter pitch data: ", e$message)
    data.frame()
  })

  # Add mapped pitch type
  if (nrow(result) > 0) {
    result$pitch_type_abbrev <- sapply(result$taggedpitchtype, map_pitch_type)
  }

  result
}

#' Generate swing/miss heatmap for a specific pitch type
#' @param df Data frame with pitch data
#' @param pitch_type Abbreviated pitch type (4SM, 2SM, CT, CB, SL, CH)
#' @param title Optional title override
#' @return ggplot2 object
generate_swingmiss_heatmap <- function(df, pitch_type, title = NULL) {
  # Filter to swinging strikes for the pitch type
  plot_data <- df |>
    filter(pitch_type_abbrev == pitch_type & pitchcall == "StrikeSwinging") |>
    filter(!is.na(platelocside) & !is.na(platelocheight))

  if (is.null(title)) {
    title <- paste0("S/M ", pitch_type)
  }

  if (nrow(plot_data) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 3, color = "#888") +
        theme_void() +
        labs(title = title) +
        theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
    )
  }

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  ggplot(plot_data, aes(x = platelocside, y = platelocheight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      h = c(0.55, 0.55),
      n = 60
    ) +
    scale_fill_gradientn(colors = cols, guide = "none") +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 0.5) +
    annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    coord_fixed(xlim = c(-2, 2), ylim = c(-0.5, 4.5), expand = FALSE) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 7, face = "bold"),
      plot.margin = margin(1, 1, 1, 1)
    )
}

#' Generate SLG-based heatmap for a specific pitch type
#' @param df Data frame with pitch data
#' @param pitch_type Abbreviated pitch type (4SM, 2SM, CT, CB, SL, CH)
#' @param title Optional title override
#' @return ggplot2 object
generate_slg_heatmap_batter <- function(df, pitch_type, title = NULL) {
  # Filter to hits for the pitch type
  plot_data <- df |>
    filter(pitch_type_abbrev == pitch_type & pitchcall == "InPlay") |>
    filter(playresult %in% c("Single", "SIngle", "Double", "Triple", "triple", "HomeRun", "Homerun")) |>
    filter(!is.na(platelocside) & !is.na(platelocheight))

  if (is.null(title)) {
    title <- paste0("AVG ", pitch_type)
  }

  if (nrow(plot_data) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 3, color = "#888") +
        theme_void() +
        labs(title = title) +
        theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
    )
  }

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  ggplot(plot_data, aes(x = platelocside, y = platelocheight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      h = c(0.55, 0.55),
      n = 60
    ) +
    scale_fill_gradientn(colors = cols, guide = "none") +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 0.5) +
    annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    coord_fixed(xlim = c(-2, 2), ylim = c(-0.5, 4.5), expand = FALSE) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 7, face = "bold"),
      plot.margin = margin(1, 1, 1, 1)
    )
}

#' Generate exit velocity heatmap
#' @param df Data frame with pitch data
#' @param title Optional title override
#' @return ggplot2 object
generate_exitvelo_heatmap <- function(df, title = "EXITVELO") {
  plot_data <- df |>
    filter(pitchcall == "InPlay" & !is.na(exitspeed)) |>
    filter(!is.na(platelocside) & !is.na(platelocheight))

  if (nrow(plot_data) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 3, color = "#888") +
        theme_void() +
        labs(title = title) +
        theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
    )
  }

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  ggplot(plot_data, aes(x = platelocside, y = platelocheight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      h = c(0.55, 0.55),
      n = 60
    ) +
    scale_fill_gradientn(colors = cols, guide = "none") +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 0.5) +
    annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    coord_fixed(xlim = c(-2, 2), ylim = c(-0.5, 4.5), expand = FALSE) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 7, face = "bold"),
      plot.margin = margin(1, 1, 1, 1)
    )
}

#' Generate popup heatmap (high launch angle, low exit velo balls in play)
#' @param df Data frame with pitch data
#' @param title Optional title override
#' @return ggplot2 object
generate_popup_heatmap <- function(df, title = "POPUP") {
  # Popups typically have high launch angle (>50) and lower exit velo
  plot_data <- df |>
    filter(pitchcall == "InPlay" & !is.na(angle)) |>
    filter(angle > 50) |>  # High launch angle for popups
    filter(!is.na(platelocside) & !is.na(platelocheight))

  if (nrow(plot_data) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 3, color = "#888") +
        theme_void() +
        labs(title = title) +
        theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
    )
  }

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  ggplot(plot_data, aes(x = platelocside, y = platelocheight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      h = c(0.55, 0.55),
      n = 60
    ) +
    scale_fill_gradientn(colors = cols, guide = "none") +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 0.5) +
    annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    coord_fixed(xlim = c(-2, 2), ylim = c(-0.5, 4.5), expand = FALSE) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 7, face = "bold"),
      plot.margin = margin(1, 1, 1, 1)
    )
}

#' Generate 2-strike swing/miss heatmap
#' @param df Data frame with pitch data
#' @param title Optional title override
#' @return ggplot2 object
generate_2k_miss_heatmap <- function(df, title = "2K MISS") {
  plot_data <- df |>
    filter(strikes == 2 & pitchcall == "StrikeSwinging") |>
    filter(!is.na(platelocside) & !is.na(platelocheight))

  if (nrow(plot_data) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 3, color = "#888") +
        theme_void() +
        labs(title = title) +
        theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
    )
  }

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  ggplot(plot_data, aes(x = platelocside, y = platelocheight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      h = c(0.55, 0.55),
      n = 60
    ) +
    scale_fill_gradientn(colors = cols, guide = "none") +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 0.5) +
    annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    coord_fixed(xlim = c(-2, 2), ylim = c(-0.5, 4.5), expand = FALSE) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 7, face = "bold"),
      plot.margin = margin(1, 1, 1, 1)
    )
}

#' Generate 2-strike called strike (take) heatmap
#' @param df Data frame with pitch data
#' @param title Optional title override
#' @return ggplot2 object
generate_2k_take_heatmap <- function(df, title = "2K TAKE") {
  plot_data <- df |>
    filter(strikes == 2 & pitchcall == "StrikeCalled") |>
    filter(!is.na(platelocside) & !is.na(platelocheight))

  if (nrow(plot_data) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 3, color = "#888") +
        theme_void() +
        labs(title = title) +
        theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"))
    )
  }

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  ggplot(plot_data, aes(x = platelocside, y = platelocheight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      h = c(0.55, 0.55),
      n = 60
    ) +
    scale_fill_gradientn(colors = cols, guide = "none") +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 0.5) +
    annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black", linewidth = 0.3) +
    annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black", linewidth = 0.3) +
    coord_fixed(xlim = c(-2, 2), ylim = c(-0.5, 4.5), expand = FALSE) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 7, face = "bold"),
      plot.margin = margin(1, 1, 1, 1)
    )
}

#' Get hitter scouting notes for a batter from the database
#' @param pool Database connection pool
#' @param batter_name Name of the batter
#' @param team_code Team code
#' @param pitcher_hand Pitcher handedness ("Right" or "Left")
#' @return Named list with all notes and settings
get_hitter_scouting_notes <- function(pool, batter_name, team_code, pitcher_hand) {
  # Create composite key with pitcher hand
  team_key <- paste0(team_code, "::", pitcher_hand)

  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT count_data, highlights, stats_slg, stats_run, stats_k, stats_bb,
             stats_hbp, stats_hr, stats_fly, stats_ground, notes_main, notes_action,
             img_box, img_contact_point, img_spray_chart, img_risp_gb
      FROM hitter_scouting_notes
      WHERE batter_name = $1 AND team_name = $2
      LIMIT 1
    ", params = list(batter_name, team_key))
  }, error = function(e) {
    message("Error fetching hitter scouting notes: ", e$message)
    return(data.frame())
  })

  if (nrow(result) == 0) {
    return(list(
      count_data = list(),
      highlights = list(),
      stats_slg = "",
      stats_run = "",
      stats_k = "",
      stats_bb = "",
      stats_hbp = "",
      stats_hr = "",
      stats_fly = "",
      stats_ground = "",
      notes_main = "",
      notes_action = "",
      img_box = "",
      img_contact_point = "",
      img_spray_chart = "",
      img_risp_gb = ""
    ))
  }

  # Parse JSONB fields
  count_data <- tryCatch({
    if (!is.null(result$count_data[1]) && !is.na(result$count_data[1])) {
      jsonlite::fromJSON(result$count_data[1], simplifyVector = FALSE)
    } else {
      list()
    }
  }, error = function(e) list())

  highlights <- tryCatch({
    if (!is.null(result$highlights[1]) && !is.na(result$highlights[1])) {
      jsonlite::fromJSON(result$highlights[1], simplifyVector = FALSE)
    } else {
      list()
    }
  }, error = function(e) list())

  list(
    count_data = count_data,
    highlights = highlights,
    stats_slg = result$stats_slg[1] %||% "",
    stats_run = result$stats_run[1] %||% "",
    stats_k = result$stats_k[1] %||% "",
    stats_bb = result$stats_bb[1] %||% "",
    stats_hbp = result$stats_hbp[1] %||% "",
    stats_hr = result$stats_hr[1] %||% "",
    stats_fly = result$stats_fly[1] %||% "",
    stats_ground = result$stats_ground[1] %||% "",
    notes_main = result$notes_main[1] %||% "",
    notes_action = result$notes_action[1] %||% "",
    img_box = result$img_box[1] %||% "",
    img_contact_point = result$img_contact_point[1] %||% "",
    img_spray_chart = result$img_spray_chart[1] %||% "",
    img_risp_gb = result$img_risp_gb[1] %||% ""
  )
}

#' Save hitter scouting notes for a batter to the database
#' @param pool Database connection pool
#' @param batter_name Name of the batter
#' @param team_code Team code
#' @param pitcher_hand Pitcher handedness ("Right" or "Left")
#' @param data Named list with all notes and settings
#' @return TRUE on success, FALSE on failure
save_hitter_scouting_notes <- function(pool, batter_name, team_code, pitcher_hand, data) {
  # Create composite key with pitcher hand
  team_key <- paste0(team_code, "::", pitcher_hand)

  # Convert lists to JSON
  count_data_json <- jsonlite::toJSON(data$count_data %||% list(), auto_unbox = TRUE)
  highlights_json <- jsonlite::toJSON(data$highlights %||% list(), auto_unbox = TRUE)

  tryCatch({
    dbExecute(pool, "
      INSERT INTO hitter_scouting_notes (
        batter_name, team_name, count_data, highlights,
        stats_slg, stats_run, stats_k, stats_bb, stats_hbp, stats_hr,
        stats_fly, stats_ground, notes_main, notes_action,
        img_box, img_contact_point, img_spray_chart, img_risp_gb, updated_at
      )
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, NOW())
      ON CONFLICT (batter_name, team_name)
      DO UPDATE SET
        count_data = EXCLUDED.count_data,
        highlights = EXCLUDED.highlights,
        stats_slg = EXCLUDED.stats_slg,
        stats_run = EXCLUDED.stats_run,
        stats_k = EXCLUDED.stats_k,
        stats_bb = EXCLUDED.stats_bb,
        stats_hbp = EXCLUDED.stats_hbp,
        stats_hr = EXCLUDED.stats_hr,
        stats_fly = EXCLUDED.stats_fly,
        stats_ground = EXCLUDED.stats_ground,
        notes_main = EXCLUDED.notes_main,
        notes_action = EXCLUDED.notes_action,
        img_box = EXCLUDED.img_box,
        img_contact_point = EXCLUDED.img_contact_point,
        img_spray_chart = EXCLUDED.img_spray_chart,
        img_risp_gb = EXCLUDED.img_risp_gb,
        updated_at = NOW()
    ", params = list(
      batter_name,
      team_key,
      count_data_json,
      highlights_json,
      data$stats_slg %||% "",
      data$stats_run %||% "",
      data$stats_k %||% "",
      data$stats_bb %||% "",
      data$stats_hbp %||% "",
      data$stats_hr %||% "",
      data$stats_fly %||% "",
      data$stats_ground %||% "",
      data$notes_main %||% "",
      data$notes_action %||% "",
      data$img_box %||% "",
      data$img_contact_point %||% "",
      data$img_spray_chart %||% "",
      data$img_risp_gb %||% ""
    ))
    TRUE
  }, error = function(e) {
    message("Error saving hitter scouting notes: ", e$message)
    FALSE
  })
}

#' Update a single field in hitter scouting notes
#' @param pool Database connection pool
#' @param batter_name Name of the batter
#' @param team_code Team code
#' @param pitcher_hand Pitcher handedness ("Right" or "Left")
#' @param field_name Name of the field to update
#' @param value New value for the field
#' @return TRUE on success, FALSE on failure
update_hitter_scouting_field <- function(pool, batter_name, team_code, pitcher_hand, field_name, value) {
  team_key <- paste0(team_code, "::", pitcher_hand)

  # Validate field name to prevent SQL injection
  valid_fields <- c("count_data", "highlights", "stats_slg", "stats_run", "stats_k",
                    "stats_bb", "stats_hbp", "stats_hr", "stats_fly", "stats_ground",
                    "notes_main", "notes_action", "img_box", "img_contact_point",
                    "img_spray_chart", "img_risp_gb")

  if (!field_name %in% valid_fields) {
    message("Invalid field name: ", field_name)
    return(FALSE)
  }

  # Convert to JSON if it's a list
  if (field_name %in% c("count_data", "highlights") && is.list(value)) {
    value <- jsonlite::toJSON(value, auto_unbox = TRUE)
  }

  tryCatch({
    # First ensure the row exists
    dbExecute(pool, "
      INSERT INTO hitter_scouting_notes (batter_name, team_name, updated_at)
      VALUES ($1, $2, NOW())
      ON CONFLICT (batter_name, team_name) DO NOTHING
    ", params = list(batter_name, team_key))

    # Then update the specific field
    query <- sprintf("
      UPDATE hitter_scouting_notes
      SET %s = $3, updated_at = NOW()
      WHERE batter_name = $1 AND team_name = $2
    ", field_name)

    dbExecute(pool, query, params = list(batter_name, team_key, value))
    TRUE
  }, error = function(e) {
    message("Error updating hitter scouting field: ", e$message)
    FALSE
  })
}

#' Get highlight colors available for count boxes
#' @return Named list of color options
get_highlight_colors <- function() {
  list(
    "none" = "transparent",
    "yellow" = "#fef08a",
    "green" = "#bbf7d0",
    "red" = "#fecaca",
    "blue" = "#bfdbfe",
    "orange" = "#fed7aa",
    "purple" = "#e9d5ff"
  )
}

#' Get count labels in the correct order
#' @return Character vector of count labels
get_count_labels <- function() {
  c("0-0", "RISP", "0-1", "1-0", "1-1", "2-0", "2-1", "3-1", "0-2", "1-2", "2-2", "3-2")
}
