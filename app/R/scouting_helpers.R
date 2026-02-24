# Scouting Report Helper Functions
# Helper functions for the opposing pitcher scouting report feature

library(cachem)
library(memoise)
library(jsonlite)

# Create a cache with 1 hour timeout and 500MB max size
scouting_cache <- cache_mem(max_size = 500 * 1024^2, max_age = 3600)

#' Clear the scouting cache
#' @export
clear_scouting_cache <- function() {
  scouting_cache$reset()
  message("Scouting cache cleared")
}

#' Get pitch arsenal summary for a pitcher (internal, uncached)
.get_arsenal_summary_raw <- function(pool, pitcher_name, start_date, end_date) {
  dbGetQuery(pool, "
    SELECT
      taggedpitchtype as pitch_type,
      COUNT(*) as n,
      ROUND(AVG(relspeed)::numeric, 1) as velo_avg,
      ROUND(MIN(relspeed)::numeric, 0) as velo_min,
      ROUND(MAX(relspeed)::numeric, 0) as velo_max,
      ROUND(AVG(inducedvertbreak)::numeric, 1) as ivb,
      ROUND(AVG(horzbreak)::numeric, 1) as hb,
      ROUND(AVG(spinrate)::numeric, 0) as spin,
      ROUND(AVG(extension)::numeric, 1) as extension,
      ROUND(AVG(relheight)::numeric, 1) as rel_height
    FROM core_level.trackman_event
    WHERE pitcher = $1
      AND date BETWEEN $2 AND $3
      AND taggedpitchtype IS NOT NULL
    GROUP BY taggedpitchtype
    ORDER BY COUNT(*) DESC
  ", params = list(pitcher_name, start_date, end_date))
}

#' Get pitch arsenal summary for a pitcher (cached)
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param start_date Start date for filtering
#' @param end_date End date for filtering
#' @return Data frame with pitch type summary statistics
get_arsenal_summary <- memoise(.get_arsenal_summary_raw, cache = scouting_cache, omit_args = "pool")

#' Get pitch validation summary (internal, uncached)
.get_pitch_validation_summary_raw <- function(pool, pitcher_name, start_date, end_date) {
  dbGetQuery(pool, "
    SELECT
      taggedpitchtype as pitch_type,
      COUNT(*) as count,
      ROUND(AVG(inducedvertbreak)::numeric, 1) as avg_ivb,
      ROUND(AVG(horzbreak)::numeric, 1) as avg_hb,
      ROUND(AVG(relspeed)::numeric, 1) as avg_velo
    FROM core_level.trackman_event
    WHERE pitcher = $1
      AND date BETWEEN $2 AND $3
      AND taggedpitchtype IS NOT NULL
    GROUP BY taggedpitchtype
    ORDER BY COUNT(*) DESC
  ", params = list(pitcher_name, start_date, end_date))
}

#' Get pitch validation summary with movement profiles (cached)
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param start_date Start date for filtering
#' @param end_date End date for filtering
#' @return Data frame with pitch type movement profiles for validation
get_pitch_validation_summary <- memoise(.get_pitch_validation_summary_raw, cache = scouting_cache, omit_args = "pool")

#' Get pitch locations for heatmap generation
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param start_date Start date for filtering
#' @param end_date End date for filtering
#' @param batter_side Optional: "Left" or "Right" for batter handedness filter
#' @param count_filter Optional: "first_pitch", "hitter_advantage", or "2k"
#' @return Data frame with plate location data
get_pitch_locations <- function(pool, pitcher_name, start_date, end_date,
                                batter_side = NULL, count_filter = NULL) {

  base_query <- "
    SELECT
      platelocside,
      platelocheight,
      taggedpitchtype,
      batterside,
      balls,
      strikes,
      pitchcall,
      korbb
    FROM core_level.trackman_event
    WHERE pitcher = $1
      AND date BETWEEN $2 AND $3
      AND taggedpitchtype IS NOT NULL
      AND platelocside IS NOT NULL
      AND platelocheight IS NOT NULL
  "

  # Add batter side filter
  if (!is.null(batter_side)) {
    base_query <- paste0(base_query, " AND batterside = '", batter_side, "'")
  }

  # Add count filter
  if (!is.null(count_filter)) {
    count_clause <- switch(count_filter,
      "first_pitch" = " AND balls = 0 AND strikes = 0",
      "hitter_advantage" = " AND ((balls = 1 AND strikes = 0) OR (balls = 2 AND strikes IN (0,1)) OR (balls = 3 AND strikes IN (0,1)))",
      "2k" = " AND strikes = 2",
      ""
    )
    base_query <- paste0(base_query, count_clause)
  }

  dbGetQuery(pool, base_query, params = list(pitcher_name, start_date, end_date))
}

#' Get all raw pitch data for a pitcher (internal, uncached)
.get_pitcher_data_raw <- function(pool, pitcher_name, start_date, end_date) {
  dbGetQuery(pool, "
    SELECT
      taggedpitchtype,
      relspeed,
      inducedvertbreak,
      horzbreak,
      spinrate,
      platelocside,
      platelocheight,
      relside,
      relheight,
      extension,
      batterside,
      balls,
      strikes,
      pitchcall,
      korbb,
      playresult,
      CASE
        WHEN platelocside BETWEEN -0.83 AND 0.83
         AND platelocheight BETWEEN 1.5 AND 3.5
        THEN 1 ELSE 0
      END as in_zone
    FROM core_level.trackman_event
    WHERE pitcher = $1
      AND date BETWEEN $2 AND $3
      AND taggedpitchtype IS NOT NULL
  ", params = list(pitcher_name, start_date, end_date))
}

#' Get all raw pitch data for a pitcher (cached, optimized - only needed columns)
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param start_date Start date for filtering
#' @param end_date End date for filtering
#' @return Data frame with pitch data
get_pitcher_data <- memoise(.get_pitcher_data_raw, cache = scouting_cache, omit_args = "pool")

#' Calculate zone percentage (pitches in strike zone)
#' @param df Data frame with platelocside and platelocheight columns
#' @return Numeric zone percentage
calculate_zone_pct <- function(df) {
  if (nrow(df) == 0) return(NA_real_)

  # Standard strike zone boundaries
  zone_left <- -0.83
  zone_right <- 0.83
  zone_bottom <- 1.5
  zone_top <- 3.5

  in_zone <- df |>
    filter(
      platelocside >= zone_left,
      platelocside <= zone_right,
      platelocheight >= zone_bottom,
      platelocheight <= zone_top
    ) |>
    nrow()

  round(in_zone / nrow(df) * 100, 0)
}

#' Generate strike zone heatmap
#' @param df Data frame with platelocside and platelocheight columns
#' @param pitch_type Optional: filter to specific pitch type
#' @param title Plot title
#' @return ggplot2 object
generate_heatmap <- function(df, pitch_type = NULL, title = "", show_legend = FALSE) {
  # Filter to pitch type if specified
  plot_data <- if (!is.null(pitch_type) && "pitch_type_display" %in% names(df)) {
    df |> filter(pitch_type_display == pitch_type)
  } else if (!is.null(pitch_type) && "taggedpitchtype" %in% names(df)) {
    df |> filter(taggedpitchtype == pitch_type)
  } else {
    df
  }

  # Filter out NA locations

  plot_data <- plot_data |>
    filter(!is.na(platelocside), !is.na(platelocheight))

  if (nrow(plot_data) < 3) {
    # Return empty plot if insufficient data
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 4) +
        theme_void() +
        labs(title = title)
    )
  }

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  p <- ggplot(plot_data, aes(x = platelocside, y = platelocheight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      h = c(0.55, 0.55),
      n = 80  # Reduced from 150 for faster rendering
    ) +
    scale_fill_gradientn(colors = cols, guide = if (show_legend) "colorbar" else "none") +
    # Strike zone box (using annotate instead of geom_rect with aes)
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 1) +
    # Home plate
    annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black") +
    annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black") +
    annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black") +
    annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black") +
    annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black") +
    coord_fixed(xlim = c(-2, 2), ylim = c(-0.5, 4.5), expand = FALSE) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      legend.position = if (show_legend) "right" else "none"
    )

  return(p)
}

#' Generate release point plot
#' @param df Data frame with relside and relheight columns
#' @return ggplot2 object
generate_release_plot <- function(df) {
  if (nrow(df) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 5, label = "Insufficient data", size = 4) +
        theme_void() +
        labs(title = "Release Point")
    )
  }

  # Use remapped pitch type if available
  pitch_col <- if ("pitch_type_display" %in% names(df)) "pitch_type_display" else "taggedpitchtype"

  ggplot(df, aes(x = relside, y = relheight, color = .data[[pitch_col]])) +
    geom_point(alpha = 0.6, size = 2) +
    stat_ellipse(level = 0.68, linewidth = 1) +
    coord_fixed(xlim = c(-4, 4), ylim = c(3, 8)) +
    labs(
      x = "Release Side (ft)",
      y = "Release Height (ft)",
      title = "Release Point",
      color = "Pitch Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}

#' Get upcoming opponents from the upcoming_opponents table
#' Only returns teams that are in the upcoming_opponents table AND have data
#' @param pool Database connection pool
#' @return Character vector of team codes
get_upcoming_teams <- function(pool) {
  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT DISTINCT uo.team_code
      FROM upcoming_opponents uo
      INNER JOIN core_level.trackman_event te
        ON te.pitcherteam = uo.team_code
      WHERE te.batterteam = 'CAL_MUS'
      ORDER BY uo.team_code
    ")$team_code
  }, error = function(e) {
    message("Error fetching upcoming teams: ", e$message)
    character(0)
  })
  result
}

#' Add a team to the upcoming opponents list
#' @param pool Database connection pool
#' @param team_code Team code
#' @param team_name Optional team name
#' @param game_date Optional game date
#' @return TRUE on success, FALSE on failure
add_upcoming_opponent <- function(pool, team_code, team_name = NULL, game_date = NULL) {
  tryCatch({
    dbExecute(pool, "
      INSERT INTO upcoming_opponents (team_code, team_name, game_date)
      VALUES ($1, $2, $3)
      ON CONFLICT (team_code) DO UPDATE SET
        team_name = COALESCE(EXCLUDED.team_name, upcoming_opponents.team_name),
        game_date = COALESCE(EXCLUDED.game_date, upcoming_opponents.game_date)
    ", params = list(team_code, team_name, game_date))
    TRUE
  }, error = function(e) {
    message("Error adding upcoming opponent: ", e$message)
    FALSE
  })
}

#' Remove a team from the upcoming opponents list
#' @param pool Database connection pool
#' @param team_code Team code to remove
#' @return TRUE on success, FALSE on failure
remove_upcoming_opponent <- function(pool, team_code) {
  tryCatch({
    dbExecute(pool, "DELETE FROM upcoming_opponents WHERE team_code = $1",
              params = list(team_code))
    TRUE
  }, error = function(e) {
    message("Error removing upcoming opponent: ", e$message)
    FALSE
  })
}

#' Get list of current upcoming opponents with details
#' @param pool Database connection pool
#' @return Data frame with team_code, team_name, game_date
get_upcoming_opponents_list <- function(pool) {
  tryCatch({
    dbGetQuery(pool, "
      SELECT team_code, team_name, game_date
      FROM upcoming_opponents
      ORDER BY game_date NULLS LAST, team_code
    ")
  }, error = function(e) {
    message("Error fetching upcoming opponents list: ", e$message)
    data.frame(team_code = character(), team_name = character(), game_date = as.Date(character()))
  })
}

#' Get opposing teams (internal, uncached)
.get_opposing_teams_raw <- function(pool) {
  result <- dbGetQuery(pool, "
    SELECT DISTINCT pitcherteam
    FROM core_level.trackman_event
    WHERE batterteam = 'CAL_MUS'
      AND pitcherteam IS NOT NULL
      AND pitcherteam != ''
    ORDER BY pitcherteam
  ")
  result$pitcherteam
}

#' Get opposing teams that have faced CAL_MUS (cached - 1 hour)
#' @param pool Database connection pool
#' @return Character vector of team names
get_opposing_teams <- memoise(.get_opposing_teams_raw, cache = scouting_cache, omit_args = "pool")

#' Get pitchers from a team (internal, uncached)
.get_team_pitchers_raw <- function(pool, team) {
  result <- dbGetQuery(pool, "
    SELECT DISTINCT pitcher
    FROM core_level.trackman_event
    WHERE pitcherteam = $1
      AND batterteam = 'CAL_MUS'
      AND pitcher IS NOT NULL
      AND pitcher != ''
    ORDER BY pitcher
  ", params = list(team))
  result$pitcher
}

#' Get pitchers from a specific team that have faced CAL_MUS (cached)
#' @param pool Database connection pool
#' @param team Team name
#' @return Character vector of pitcher names
get_team_pitchers <- memoise(.get_team_pitchers_raw, cache = scouting_cache, omit_args = "pool")

#' Compute arsenal summary from data frame with remapped pitch types
#' @param df Data frame with pitch data
#' @param pitch_col Column name for pitch type (default: pitch_type_display)
#' @return Data frame with arsenal summary
compute_arsenal_summary <- function(df, pitch_col = "pitch_type_display") {
  # Filter out rows with NA pitch type

  df <- df |> filter(!is.na(.data[[pitch_col]]))

  total_pitches <- nrow(df)
  if (total_pitches == 0) {
    return(data.frame(
      pitch_type = character(),
      count = integer(),
      usage = numeric(),
      velo = character(),
      zone_pct = numeric(),
      ivb = numeric(),
      hb = numeric()
    ))
  }

  df |>
    group_by(.data[[pitch_col]]) |>
    summarize(
      count = n(),
      usage = round(n() / total_pitches * 100, 0),
      velo = paste0(
        round(quantile(relspeed, 0.10, na.rm = TRUE), 0), "-",
        round(quantile(relspeed, 0.90, na.rm = TRUE), 0),
        " (", round(max(relspeed, na.rm = TRUE), 0), ")"
      ),
      zone_pct = round(sum(in_zone, na.rm = TRUE) / n() * 100, 0),
      ivb = round(mean(inducedvertbreak, na.rm = TRUE), 1),
      hb = round(mean(horzbreak, na.rm = TRUE), 1),
      .groups = "drop"
    ) |>
    rename(pitch_type = all_of(pitch_col)) |>
    arrange(desc(count))
}

#' Helper to create shiny input within DT table
#' @param FUN Input function (e.g., selectInput)
#' @param len Number of inputs to create
#' @param id Base ID for inputs
#' @param ... Additional arguments passed to FUN
#' @return Character vector of HTML input elements
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

#' Get scouting notes for a pitcher from the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @return Named list with notes (gameplan, attack, first_pitch, hitter_adv, two_k)
get_scouting_notes <- function(pool, pitcher_name, team_name) {
  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT notes_gameplan, notes_attack, notes_first_pitch, notes_hitter_adv, notes_2k
      FROM scouting_notes
      WHERE pitcher_name = $1 AND team_name = $2
      LIMIT 1
    ", params = list(pitcher_name, team_name))
  }, error = function(e) {
    message("Error fetching scouting notes: ", e$message)
    return(data.frame())
  })

  if (nrow(result) == 0) {
    return(list(
      gameplan = "",
      attack = "",
      first_pitch = "",
      hitter_adv = "",
      two_k = ""
    ))
  }

  list(
    gameplan = result$notes_gameplan[1] %||% "",
    attack = result$notes_attack[1] %||% "",
    first_pitch = result$notes_first_pitch[1] %||% "",
    hitter_adv = result$notes_hitter_adv[1] %||% "",
    two_k = result$notes_2k[1] %||% ""
  )
}

#' Save scouting notes for a pitcher to the database (upsert)
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param notes_list Named list with notes (gameplan, attack, first_pitch, hitter_adv, two_k)
#' @return TRUE on success, FALSE on failure
save_scouting_notes <- function(pool, pitcher_name, team_name, notes_list) {
  tryCatch({
    dbExecute(pool, "
      INSERT INTO scouting_notes (pitcher_name, team_name, notes_gameplan, notes_attack, notes_first_pitch, notes_hitter_adv, notes_2k, updated_at)
      VALUES ($1, $2, $3, $4, $5, $6, $7, NOW())
      ON CONFLICT (pitcher_name, team_name)
      DO UPDATE SET
        notes_gameplan = EXCLUDED.notes_gameplan,
        notes_attack = EXCLUDED.notes_attack,
        notes_first_pitch = EXCLUDED.notes_first_pitch,
        notes_hitter_adv = EXCLUDED.notes_hitter_adv,
        notes_2k = EXCLUDED.notes_2k,
        updated_at = NOW()
    ", params = list(
      pitcher_name,
      team_name,
      notes_list$gameplan %||% "",
      notes_list$attack %||% "",
      notes_list$first_pitch %||% "",
      notes_list$hitter_adv %||% "",
      notes_list$two_k %||% ""
    ))
    TRUE
  }, error = function(e) {
    message("Error saving scouting notes: ", e$message)
    FALSE
  })
}

#' Generate SLG (Slugging %) heatmap - shows density of extra-base hits
#' @param df Data frame with pitch data including playresult and plate location
#' @param title Plot title
#' @return ggplot2 object
generate_slg_heatmap <- function(df, title = "SLG Heatmap") {
  # Filter to hits only (where damage occurred)
  hit_df <- df |>
    filter(pitchcall == "InPlay" & !is.na(platelocside) & !is.na(platelocheight)) |>
    filter(playresult %in% c("Single", "SIngle", "Double", "Triple", "triple", "HomeRun", "Homerun"))

  if (nrow(hit_df) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 4) +
        theme_void() +
        labs(title = title)
    )
  }

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  ggplot(hit_df, aes(x = platelocside, y = platelocheight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      h = c(0.55, 0.55),
      n = 80
    ) +
    scale_fill_gradientn(colors = cols, guide = "none") +
    # Strike zone box
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 1) +
    # Home plate
    annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black") +
    annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black") +
    annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black") +
    annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black") +
    annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black") +
    coord_fixed(xlim = c(-2, 2), ylim = c(-0.5, 4.5), expand = FALSE) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
}

#' Generate Whiff Heatmap - shows density of swinging strikes (same structure as SLG)
#' @param df Data frame with pitch data including pitchcall and plate location
#' @param title Plot title
#' @return ggplot2 object
generate_whiff_heatmap <- function(df, title = "Whiffs") {
  # Filter to swinging strikes only
  whiff_df <- df |>
    filter(pitchcall == "StrikeSwinging" & !is.na(platelocside) & !is.na(platelocheight))

  if (nrow(whiff_df) < 3) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 4) +
        theme_void() +
        labs(title = title)
    )
  }

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  ggplot(whiff_df, aes(x = platelocside, y = platelocheight)) +
    stat_density_2d(
      aes(fill = after_stat(ndensity)),
      geom = "raster",
      contour = FALSE,
      h = c(0.55, 0.55),
      n = 80
    ) +
    scale_fill_gradientn(colors = cols, guide = "none") +
    # Strike zone box
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 1) +
    # Home plate
    annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black") +
    annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black") +
    annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black") +
    annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black") +
    annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black") +
    coord_fixed(xlim = c(-2, 2), ylim = c(-0.5, 4.5), expand = FALSE) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
}

#' Get pitch descriptions for a pitcher from the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @return Named list with pitch type as key and description as value
get_pitch_descriptions <- function(pool, pitcher_name, team_name) {
  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT pitch_descriptions FROM scouting_notes
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_name))
  }, error = function(e) {
    message("Error fetching pitch descriptions: ", e$message)
    return(data.frame())
  })

  if (nrow(result) == 0 || is.null(result$pitch_descriptions) || is.na(result$pitch_descriptions[1])) {
    return(list())
  }

  tryCatch({
    jsonlite::fromJSON(result$pitch_descriptions[1])
  }, error = function(e) {
    list()
  })
}

#' Save pitch descriptions for a pitcher to the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param descriptions Named list with pitch type as key and description as value
#' @return TRUE on success, FALSE on failure
save_pitch_descriptions <- function(pool, pitcher_name, team_name, descriptions) {
  json_str <- jsonlite::toJSON(descriptions, auto_unbox = TRUE)

  tryCatch({
    # First ensure the row exists
    dbExecute(pool, "
      INSERT INTO scouting_notes (pitcher_name, team_name, updated_at)
      VALUES ($1, $2, NOW())
      ON CONFLICT (pitcher_name, team_name) DO NOTHING
    ", params = list(pitcher_name, team_name))

    # Then update the pitch_descriptions
    dbExecute(pool, "
      UPDATE scouting_notes
      SET pitch_descriptions = $3, updated_at = NOW()
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_name, json_str))
    TRUE
  }, error = function(e) {
    message("Error saving pitch descriptions: ", e$message)
    FALSE
  })
}

#' Get RISP images for a pitcher from the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @return Named list with pitch type as key and image URL as value
get_risp_images <- function(pool, pitcher_name, team_name) {
  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT risp_images FROM scouting_notes
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_name))
  }, error = function(e) {
    message("Error fetching RISP images: ", e$message)
    return(data.frame())
  })

  if (nrow(result) == 0 || is.null(result$risp_images) || is.na(result$risp_images[1])) {
    return(list())
  }

  tryCatch({
    jsonlite::fromJSON(result$risp_images[1])
  }, error = function(e) {
    list()
  })
}

#' Save RISP images for a pitcher to the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param risp_images Named list with pitch type as key and image URL as value
#' @return TRUE on success, FALSE on failure
save_risp_images <- function(pool, pitcher_name, team_name, risp_images) {
  json_str <- jsonlite::toJSON(risp_images, auto_unbox = TRUE)

  tryCatch({
    # First ensure the row exists
    dbExecute(pool, "
      INSERT INTO scouting_notes (pitcher_name, team_name, updated_at)
      VALUES ($1, $2, NOW())
      ON CONFLICT (pitcher_name, team_name) DO NOTHING
    ", params = list(pitcher_name, team_name))

    # Then update the risp_images
    dbExecute(pool, "
      UPDATE scouting_notes
      SET risp_images = $3, updated_at = NOW()
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_name, json_str))
    TRUE
  }, error = function(e) {
    message("Error saving RISP images: ", e$message)
    FALSE
  })
}
