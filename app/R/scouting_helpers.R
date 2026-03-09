# Scouting Report Helper Functions
# Helper functions for the opposing pitcher scouting report feature

library(cachem)
library(memoise)
library(jsonlite)

# Create a cache with 1 hour timeout and 500MB max size
scouting_cache <- cache_mem(max_size = 500 * 1024^2, max_age = 3600)

# Pitch types considered fastballs — used for consistent ordering across tables and heatmaps
FASTBALL_TYPES <- c("Fastball", "FourSeamFastBall", "TwoSeamFastBall", "OneSeamFastBall", "Sinker")

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

#' Nadaraya-Watson 2D kernel regression helper
#' @param x Vector of x coordinates
#' @param y Vector of y coordinates
#' @param values Numeric values to smooth
#' @param bw Bandwidth (default 0.55)
#' @param n Grid resolution
#' @param xlim x axis limits
#' @param ylim y axis limits
#' @return Data frame with columns gx, gy, value
nadaraya_watson_2d <- function(x, y, values, bw = 0.55, n = 60,
                                xlim = c(-2, 2), ylim = c(-0.5, 4.5)) {
  x_grid <- seq(xlim[1], xlim[2], length.out = n)
  y_grid <- seq(ylim[1], ylim[2], length.out = n)
  grid <- expand.grid(gx = x_grid, gy = y_grid)
  dx_mat <- outer(grid$gx, x, FUN = "-")
  dy_mat <- outer(grid$gy, y, FUN = "-")
  K <- exp(-(dx_mat^2 + dy_mat^2) / (2 * bw^2))
  denom <- rowSums(K)
  grid$value <- ifelse(denom < 1e-6, NA_real_, as.vector(K %*% values) / denom)
  grid
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
    ", params = list(team_code, team_name %||% NA, game_date %||% NA))
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
      AND pitcher IS NOT NULL
      AND pitcher != ''
      AND EXTRACT(YEAR FROM date) >= 2026
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
#' @param df Data frame with pitch data (used for usage% and zone% — split-specific)
#' @param pitch_col Column name for pitch type (default: pitch_type_display)
#' @param movement_df Optional data frame for velo/IVB/HB (both splits combined).
#'   If NULL, uses df for all stats.
#' @return Data frame with arsenal summary
compute_arsenal_summary <- function(df, pitch_col = "pitch_type_display", movement_df = NULL) {
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

  # Movement stats (velo, IVB, HB) come from movement_df if provided (both splits),
  # otherwise fall back to df
  mov_df <- if (!is.null(movement_df)) {
    movement_df |> filter(!is.na(.data[[pitch_col]]))
  } else {
    df
  }

  # Split-specific: count, usage%, zone%
  usage_stats <- df |>
    group_by(.data[[pitch_col]]) |>
    summarize(
      count    = n(),
      usage    = round(n() / total_pitches * 100, 0),
      zone_pct = round(sum(in_zone, na.rm = TRUE) / n() * 100, 0),
      .groups  = "drop"
    ) |>
    rename(pitch_type = all_of(pitch_col))

  # Combined: velo range, IVB, HB
  movement_stats <- mov_df |>
    group_by(.data[[pitch_col]]) |>
    summarize(
      velo = paste0(
        round(quantile(relspeed, 0.10, na.rm = TRUE), 0), "-",
        round(quantile(relspeed, 0.90, na.rm = TRUE), 0)
      ),
      velo_max = round(max(relspeed, na.rm = TRUE), 0),
      ivb = round(mean(inducedvertbreak, na.rm = TRUE), 1),
      hb  = round(mean(horzbreak,        na.rm = TRUE), 1),
      .groups = "drop"
    ) |>
    rename(pitch_type = all_of(pitch_col))

  usage_stats |>
    left_join(movement_stats, by = "pitch_type") |>
    mutate(
      is_fb = pitch_type %in% FASTBALL_TYPES | grepl("fastball|sinker", pitch_type, ignore.case = TRUE),
      velo  = if_else(is_fb, paste0(velo, " (", velo_max, ")"), velo)
    ) |>
    arrange(desc(is_fb), desc(count)) |>
    select(-is_fb, -velo_max)
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
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return Named list with notes (gameplan, attack, first_pitch, hitter_adv, two_k, risp)
get_scouting_notes <- function(pool, pitcher_name, team_name, split = "Both") {
  # Create composite key with split for distinct notes per handedness
  team_key <- paste0(team_name, "::", split)

  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT notes_gameplan, notes_attack, notes_first_pitch, notes_hitter_adv, notes_2k, notes_risp,
             pitcher_grade, out_pitch
      FROM scouting_notes
      WHERE pitcher_name = $1 AND team_name = $2
      LIMIT 1
    ", params = list(pitcher_name, team_key))
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
      two_k = "",
      risp = "",
      pitcher_grade = "",
      out_pitch = ""
    ))
  }

  list(
    gameplan = result$notes_gameplan[1] %||% "",
    attack = result$notes_attack[1] %||% "",
    first_pitch = result$notes_first_pitch[1] %||% "",
    hitter_adv = result$notes_hitter_adv[1] %||% "",
    two_k = result$notes_2k[1] %||% "",
    risp = if ("notes_risp" %in% names(result)) result$notes_risp[1] %||% "" else "",
    pitcher_grade = if ("pitcher_grade" %in% names(result)) result$pitcher_grade[1] %||% "" else "",
    out_pitch = if ("out_pitch" %in% names(result)) result$out_pitch[1] %||% "" else ""
  )
}

#' Save scouting notes for a pitcher to the database (upsert)
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param notes_list Named list with notes (gameplan, attack, first_pitch, hitter_adv, two_k, risp)
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return TRUE on success, FALSE on failure
save_scouting_notes <- function(pool, pitcher_name, team_name, notes_list, split = "Both") {
  # Create composite key with split for distinct notes per handedness
  team_key <- paste0(team_name, "::", split)

  # Try with all columns including pitcher_grade and out_pitch
  result <- tryCatch({
    dbExecute(pool, "
      INSERT INTO scouting_notes (pitcher_name, team_name, notes_gameplan, notes_attack, notes_first_pitch, notes_hitter_adv, notes_2k, notes_risp, pitcher_grade, out_pitch, updated_at)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, NOW())
      ON CONFLICT (pitcher_name, team_name)
      DO UPDATE SET
        notes_gameplan = EXCLUDED.notes_gameplan,
        notes_attack = EXCLUDED.notes_attack,
        notes_first_pitch = EXCLUDED.notes_first_pitch,
        notes_hitter_adv = EXCLUDED.notes_hitter_adv,
        notes_2k = EXCLUDED.notes_2k,
        notes_risp = EXCLUDED.notes_risp,
        pitcher_grade = EXCLUDED.pitcher_grade,
        out_pitch = EXCLUDED.out_pitch,
        updated_at = NOW()
    ", params = list(
      pitcher_name,
      team_key,
      notes_list$gameplan %||% "",
      notes_list$attack %||% "",
      notes_list$first_pitch %||% "",
      notes_list$hitter_adv %||% "",
      notes_list$two_k %||% "",
      notes_list$risp %||% "",
      notes_list$pitcher_grade %||% "",
      notes_list$out_pitch %||% ""
    ))
    TRUE
  }, error = function(e) {
    # Fallback: try with notes_risp only (pitcher_grade/out_pitch columns may not exist yet)
    if (grepl("pitcher_grade|out_pitch", e$message)) {
      message("pitcher_grade/out_pitch columns not found. Run ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS pitcher_grade TEXT; out_pitch TEXT;")
      tryCatch({
        dbExecute(pool, "
          INSERT INTO scouting_notes (pitcher_name, team_name, notes_gameplan, notes_attack, notes_first_pitch, notes_hitter_adv, notes_2k, notes_risp, updated_at)
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8, NOW())
          ON CONFLICT (pitcher_name, team_name)
          DO UPDATE SET
            notes_gameplan = EXCLUDED.notes_gameplan,
            notes_attack = EXCLUDED.notes_attack,
            notes_first_pitch = EXCLUDED.notes_first_pitch,
            notes_hitter_adv = EXCLUDED.notes_hitter_adv,
            notes_2k = EXCLUDED.notes_2k,
            notes_risp = EXCLUDED.notes_risp,
            updated_at = NOW()
        ", params = list(
          pitcher_name, team_key,
          notes_list$gameplan %||% "",
          notes_list$attack %||% "",
          notes_list$first_pitch %||% "",
          notes_list$hitter_adv %||% "",
          notes_list$two_k %||% "",
          notes_list$risp %||% ""
        ))
        TRUE
      }, error = function(e2) {
        message("Error saving scouting notes (fallback): ", e2$message)
        FALSE
      })
    } else if (grepl("notes_risp", e$message)) {
      message("notes_risp column not found, saving without it.")
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
          pitcher_name, team_key,
          notes_list$gameplan %||% "",
          notes_list$attack %||% "",
          notes_list$first_pitch %||% "",
          notes_list$hitter_adv %||% "",
          notes_list$two_k %||% ""
        ))
        TRUE
      }, error = function(e2) {
        message("Error saving scouting notes (fallback): ", e2$message)
        FALSE
      })
    } else {
      message("Error saving scouting notes: ", e$message)
      FALSE
    }
  })

  result
}

#' Generate SLG (Slugging %) heatmap - TruMedia-style Nadaraya-Watson rate map
#' @param df Data frame with pitch data including playresult and plate location
#' @param title Plot title
#' @return ggplot2 object
generate_slg_heatmap <- function(df, title = "Damage") {
  valid_df <- df |>
    filter(!is.na(platelocside), !is.na(platelocheight))

  if (nrow(valid_df) < 5) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 4) +
        theme_void() +
        labs(title = title)
    )
  }

  # Assign SLG weights: HR=4, 3B=3, 2B=2, 1B=1, else=0
  slg_weights <- dplyr::case_when(
    valid_df$playresult %in% c("HomeRun", "Homerun") ~ 4,
    valid_df$playresult %in% c("Triple", "triple") ~ 3,
    valid_df$playresult == "Double" ~ 2,
    valid_df$playresult %in% c("Single", "SIngle") ~ 1,
    TRUE ~ 0
  )

  grid_df <- nadaraya_watson_2d(valid_df$platelocside, valid_df$platelocheight, slg_weights, bw = 0.30, n = 80)

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  ggplot() +
    geom_raster(data = grid_df, aes(x = gx, y = gy, fill = value)) +
    scale_fill_gradientn(colors = cols, na.value = "white", guide = "none") +
    geom_point(data = valid_df, aes(x = platelocside, y = platelocheight),
               color = "white", size = 0.25, alpha = 0.35, inherit.aes = FALSE) +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 1) +
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

#' Generate Whiff Heatmap - TruMedia-style Nadaraya-Watson whiff rate map
#' @param df Data frame with pitch data including pitchcall and plate location
#' @param title Plot title
#' @return ggplot2 object
generate_whiff_heatmap <- function(df, title = "Swing & Miss") {
  valid_df <- df |>
    filter(!is.na(platelocside), !is.na(platelocheight))

  if (nrow(valid_df) < 5) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 2.5, label = "N/A", size = 4) +
        theme_void() +
        labs(title = title)
    )
  }

  # Assign whiff indicator: 1 if StrikeSwinging, else 0
  is_whiff <- as.numeric(valid_df$pitchcall == "StrikeSwinging")

  grid_df <- nadaraya_watson_2d(valid_df$platelocside, valid_df$platelocheight, is_whiff, bw = 0.30, n = 80)

  cols <- viridisLite::turbo(256)
  cols[1] <- "white"

  ggplot() +
    geom_raster(data = grid_df, aes(x = gx, y = gy, fill = value)) +
    scale_fill_gradientn(colors = cols, na.value = "white", guide = "none") +
    geom_point(data = valid_df, aes(x = platelocside, y = platelocheight),
               color = "white", size = 0.25, alpha = 0.35, inherit.aes = FALSE) +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 1) +
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
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return Named list with pitch type as key and description as value
get_pitch_descriptions <- function(pool, pitcher_name, team_name, split = "Both") {
  # Create composite key with split for distinct notes per handedness
  team_key <- paste0(team_name, "::", split)

  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT pitch_descriptions FROM scouting_notes
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key))
  }, error = function(e) {
    message("Error fetching pitch descriptions: ", e$message)
    return(data.frame())
  })

  if (nrow(result) == 0 || is.null(result$pitch_descriptions) || is.na(result$pitch_descriptions[1])) {
    return(list())
  }

  tryCatch({
    parsed <- jsonlite::fromJSON(result$pitch_descriptions[1], simplifyVector = FALSE)
    # Ensure we return a proper named list with scalar values
    if (is.list(parsed)) {
      lapply(parsed, function(x) if (length(x) > 0) as.character(x[1]) else "")
    } else if (is.character(parsed) && !is.null(names(parsed))) {
      # Named vector - convert to list
      as.list(parsed)
    } else {
      list()
    }
  }, error = function(e) {
    list()
  })
}

#' Save pitch descriptions for a pitcher to the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param descriptions Named list with pitch type as key and description as value
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return TRUE on success, FALSE on failure
save_pitch_descriptions <- function(pool, pitcher_name, team_name, descriptions, split = "Both") {
  # Create composite key with split for distinct notes per handedness
  team_key <- paste0(team_name, "::", split)
  json_str <- jsonlite::toJSON(descriptions, auto_unbox = TRUE)

  tryCatch({
    # First ensure the row exists
    dbExecute(pool, "
      INSERT INTO scouting_notes (pitcher_name, team_name, updated_at)
      VALUES ($1, $2, NOW())
      ON CONFLICT (pitcher_name, team_name) DO NOTHING
    ", params = list(pitcher_name, team_key))

    # Then update the pitch_descriptions
    dbExecute(pool, "
      UPDATE scouting_notes
      SET pitch_descriptions = $3, updated_at = NOW()
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key, json_str))
    TRUE
  }, error = function(e) {
    message("Error saving pitch descriptions: ", e$message)
    FALSE
  })
}

#' Get pitch deletions and remaps for a pitcher from the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return Named list with deletions (character vector) and remaps (named list)
get_pitch_edits <- function(pool, pitcher_name, team_name, split = "Both") {
  team_key <- paste0(team_name, "::", split)

  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT pitch_deletions, pitch_remaps FROM scouting_notes
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key))
  }, error = function(e) {
    message("Error fetching pitch edits: ", e$message)
    return(data.frame())
  })

  default <- list(deletions = character(0), remaps = list())
  if (nrow(result) == 0) return(default)

  deletions <- tryCatch({
    if (is.null(result$pitch_deletions) || is.na(result$pitch_deletions[1])) {
      character(0)
    } else {
      parsed <- jsonlite::fromJSON(result$pitch_deletions[1], simplifyVector = TRUE)
      if (length(parsed) == 0) character(0) else as.character(parsed)
    }
  }, error = function(e) character(0))

  remaps <- tryCatch({
    if (is.null(result$pitch_remaps) || is.na(result$pitch_remaps[1])) {
      list()
    } else {
      parsed <- jsonlite::fromJSON(result$pitch_remaps[1], simplifyVector = FALSE)
      if (is.list(parsed)) parsed else list()
    }
  }, error = function(e) list())

  list(deletions = deletions, remaps = remaps)
}

#' Save pitch deletions and remaps for a pitcher to the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param deletions Character vector of deleted pitch types
#' @param remaps Named list of pitch type remaps
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return TRUE on success, FALSE on failure
save_pitch_edits <- function(pool, pitcher_name, team_name, deletions, remaps, split = "Both") {
  team_key <- paste0(team_name, "::", split)
  deletions_json <- jsonlite::toJSON(as.character(deletions), auto_unbox = FALSE)
  remaps_json <- jsonlite::toJSON(remaps, auto_unbox = TRUE)

  tryCatch({
    # Ensure row exists
    dbExecute(pool, "
      INSERT INTO scouting_notes (pitcher_name, team_name, updated_at)
      VALUES ($1, $2, NOW())
      ON CONFLICT (pitcher_name, team_name) DO NOTHING
    ", params = list(pitcher_name, team_key))

    # Update pitch_deletions and pitch_remaps
    dbExecute(pool, "
      UPDATE scouting_notes
      SET pitch_deletions = $3, pitch_remaps = $4, updated_at = NOW()
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key, deletions_json, remaps_json))
    TRUE
  }, error = function(e) {
    message("Error saving pitch edits: ", e$message)
    FALSE
  })
}

#' Get RISP images for a pitcher from the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return Named list with pitch type as key and image URL as value
get_risp_images <- function(pool, pitcher_name, team_name, split = "Both") {
  # Create composite key with split for distinct notes per handedness
  team_key <- paste0(team_name, "::", split)

  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT risp_images FROM scouting_notes
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key))
  }, error = function(e) {
    message("Error fetching RISP images: ", e$message)
    return(data.frame())
  })

  if (nrow(result) == 0 || is.null(result$risp_images) || is.na(result$risp_images[1])) {
    return(list())
  }

  tryCatch({
    parsed <- jsonlite::fromJSON(result$risp_images[1], simplifyVector = FALSE)
    # Ensure we return a proper named list with scalar values
    if (is.list(parsed)) {
      lapply(parsed, function(x) if (length(x) > 0) as.character(x[1]) else "")
    } else if (is.character(parsed) && !is.null(names(parsed))) {
      # Named vector - convert to list
      as.list(parsed)
    } else {
      list()
    }
  }, error = function(e) {
    list()
  })
}

#' Save RISP images for a pitcher to the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param risp_images Named list with pitch type as key and image URL as value
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return TRUE on success, FALSE on failure
save_risp_images <- function(pool, pitcher_name, team_name, risp_images, split = "Both") {
  # Create composite key with split for distinct notes per handedness
  team_key <- paste0(team_name, "::", split)
  json_str <- jsonlite::toJSON(risp_images, auto_unbox = TRUE)

  tryCatch({
    # First ensure the row exists
    dbExecute(pool, "
      INSERT INTO scouting_notes (pitcher_name, team_name, updated_at)
      VALUES ($1, $2, NOW())
      ON CONFLICT (pitcher_name, team_name) DO NOTHING
    ", params = list(pitcher_name, team_key))

    # Then update the risp_images
    dbExecute(pool, "
      UPDATE scouting_notes
      SET risp_images = $3, updated_at = NOW()
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key, json_str))
    TRUE
  }, error = function(e) {
    message("Error saving RISP images: ", e$message)
    FALSE
  })
}

#' Get RISP usages for a pitcher from the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return Named list with pitch type as key and usage percentage as value
get_risp_usages <- function(pool, pitcher_name, team_name, split = "Both") {
  team_key <- paste0(team_name, "::", split)

  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT risp_usages FROM scouting_notes
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key))
  }, error = function(e) {
    message("Error fetching RISP usages: ", e$message)
    return(data.frame())
  })

  if (nrow(result) == 0 || is.null(result$risp_usages) || is.na(result$risp_usages[1])) {
    return(list())
  }

  tryCatch({
    parsed <- jsonlite::fromJSON(result$risp_usages[1], simplifyVector = FALSE)
    if (is.list(parsed)) {
      lapply(parsed, function(x) if (length(x) > 0) as.numeric(x[1]) else NA)
    } else {
      list()
    }
  }, error = function(e) {
    list()
  })
}

#' Save RISP usages for a pitcher to the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param risp_usages Named list with pitch type as key and usage percentage as value
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return TRUE on success, FALSE on failure
save_risp_usages <- function(pool, pitcher_name, team_name, risp_usages, split = "Both") {
  team_key <- paste0(team_name, "::", split)
  json_str <- jsonlite::toJSON(risp_usages, auto_unbox = TRUE)

  tryCatch({
    dbExecute(pool, "
      INSERT INTO scouting_notes (pitcher_name, team_name, updated_at)
      VALUES ($1, $2, NOW())
      ON CONFLICT (pitcher_name, team_name) DO NOTHING
    ", params = list(pitcher_name, team_key))

    dbExecute(pool, "
      UPDATE scouting_notes
      SET risp_usages = $3, updated_at = NOW()
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key, json_str))
    TRUE
  }, error = function(e) {
    message("Error saving RISP usages: ", e$message)
    FALSE
  })
}

#' Get pitcher stats (IP, ERA, K, BB, BAA) from the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return Named list with stats (ip, era, k, bb, baa)
get_pitcher_stats <- function(pool, pitcher_name, team_name, split = "Both") {
  team_key <- paste0(team_name, "::", split)

  result <- tryCatch({
    dbGetQuery(pool, "
      SELECT pitcher_stats FROM scouting_notes
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key))
  }, error = function(e) {
    message("Error fetching pitcher stats: ", e$message)
    return(data.frame())
  })

  if (nrow(result) == 0 || is.null(result$pitcher_stats) || is.na(result$pitcher_stats[1])) {
    return(list(ip = "", era = "", k = "", bb = "", baa_lhh = "", baa_rhh = ""))
  }

  tryCatch({
    parsed <- jsonlite::fromJSON(result$pitcher_stats[1], simplifyVector = FALSE)
    list(
      ip = if (!is.null(parsed$ip)) as.character(parsed$ip) else "",
      era = if (!is.null(parsed$era)) as.character(parsed$era) else "",
      k = if (!is.null(parsed$k)) as.character(parsed$k) else "",
      bb = if (!is.null(parsed$bb)) as.character(parsed$bb) else "",
      baa_lhh = if (!is.null(parsed$baa_lhh)) as.character(parsed$baa_lhh) else "",
      baa_rhh = if (!is.null(parsed$baa_rhh)) as.character(parsed$baa_rhh) else ""
    )
  }, error = function(e) {
    list(ip = "", era = "", k = "", bb = "", baa_lhh = "", baa_rhh = "")
  })
}

#' Save pitcher stats (IP, ERA, K, BB, BAA) to the database
#' @param pool Database connection pool
#' @param pitcher_name Name of the pitcher
#' @param team_name Name of the team
#' @param stats Named list with stats (ip, era, k, bb, baa)
#' @param split Batter handedness split ("Both", "Left", "Right")
#' @return TRUE on success, FALSE on failure
save_pitcher_stats <- function(pool, pitcher_name, team_name, stats, split = "Both") {
  team_key <- paste0(team_name, "::", split)
  json_str <- jsonlite::toJSON(stats, auto_unbox = TRUE)

  tryCatch({
    dbExecute(pool, "
      INSERT INTO scouting_notes (pitcher_name, team_name, updated_at)
      VALUES ($1, $2, NOW())
      ON CONFLICT (pitcher_name, team_name) DO NOTHING
    ", params = list(pitcher_name, team_key))

    dbExecute(pool, "
      UPDATE scouting_notes
      SET pitcher_stats = $3, updated_at = NOW()
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key, json_str))
    TRUE
  }, error = function(e) {
    message("Error saving pitcher stats: ", e$message)
    FALSE
  })
}

#' Save velocity overrides per pitch type to database
save_velo_overrides <- function(pool, pitcher_name, team_name, overrides, split = "Both") {
  team_key <- paste0(team_name, "::", split)
  json_str <- jsonlite::toJSON(overrides, auto_unbox = TRUE)

  tryCatch({
    dbExecute(pool, "
      INSERT INTO scouting_notes (pitcher_name, team_name, updated_at)
      VALUES ($1, $2, NOW())
      ON CONFLICT (pitcher_name, team_name) DO NOTHING
    ", params = list(pitcher_name, team_key))

    dbExecute(pool, "
      UPDATE scouting_notes
      SET velo_overrides = $3, updated_at = NOW()
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key, json_str))
    TRUE
  }, error = function(e) {
    message("Error saving velo overrides: ", e$message)
    FALSE
  })
}

#' Get velocity overrides from database
get_velo_overrides <- function(pool, pitcher_name, team_name, split = "Both") {
  team_key <- paste0(team_name, "::", split)
  tryCatch({
    res <- dbGetQuery(pool, "
      SELECT velo_overrides FROM scouting_notes
      WHERE pitcher_name = $1 AND team_name = $2
    ", params = list(pitcher_name, team_key))
    if (nrow(res) == 0 || is.na(res$velo_overrides[1])) return(list())
    jsonlite::fromJSON(res$velo_overrides[1], simplifyVector = FALSE)
  }, error = function(e) {
    list()
  })
}

#' Apply velocity overrides to an arsenal data frame
apply_velo_overrides <- function(arsenal, overrides, fastball_types = NULL) {
  if (is.null(fastball_types)) {
    fastball_types <- c("Fastball", "Sinker", "Two-Seam", "Four-Seam", "FourSeam", "TwoSeam")
  }
  if (length(overrides) == 0) return(arsenal)
  for (i in seq_len(nrow(arsenal))) {
    pt <- arsenal$pitch_type[i]
    if (!is.null(overrides[[pt]])) {
      ov <- overrides[[pt]]
      mn  <- ov[["min"]];  mx  <- ov[["max"]];  pk  <- ov[["peak"]]
      if (!is.null(mn) && !is.null(mx) && !is.na(mn) && !is.na(mx)) {
        is_fb <- toupper(pt) %in% toupper(fastball_types)
        range_str <- paste0(mn, "-", mx)
        if (is_fb && !is.null(pk) && !is.na(pk) && pk != "") {
          range_str <- paste0(range_str, " (", pk, ")")
        }
        arsenal$velo[i] <- range_str
      }
    }
  }
  arsenal
}

#' Format a decimal feet value as feet and inches (e.g. 5.5 -> "5' 6\"")
#' @param ft Numeric value in decimal feet
#' @return Character string formatted as feet and inches
format_feet_inches <- function(ft) {
  if (is.na(ft) || !is.finite(ft)) return("N/A")
  total_inches <- round(ft * 12)
  feet   <- total_inches %/% 12L
  inches <- total_inches %% 12L
  sprintf("%d' %d\"", feet, inches)
}

#' Get pitch type color scheme
#' @param pitch_type The pitch type name
#' @return Named list with bg (background) and text colors
get_pitch_color <- function(pitch_type) {
  pt_upper <- toupper(pitch_type)

  colors <- list(
    # Fastballs - Red
    "FASTBALL" = list(bg = "#fee2e2", text = "#dc2626"),
    "FB" = list(bg = "#fee2e2", text = "#dc2626"),
    "FOUR-SEAM" = list(bg = "#fee2e2", text = "#dc2626"),
    "FOURSEAM" = list(bg = "#fee2e2", text = "#dc2626"),

    # Changeup - Green
    "CHANGEUP" = list(bg = "#dcfce7", text = "#16a34a"),
    "CH" = list(bg = "#dcfce7", text = "#16a34a"),
    "CHANGE" = list(bg = "#dcfce7", text = "#16a34a"),

    # Slider - Blue
    "SLIDER" = list(bg = "#dbeafe", text = "#2563eb"),
    "SL" = list(bg = "#dbeafe", text = "#2563eb"),

    # Curveball - Purple
    "CURVEBALL" = list(bg = "#f3e8ff", text = "#9333ea"),
    "CB" = list(bg = "#f3e8ff", text = "#9333ea"),
    "CU" = list(bg = "#f3e8ff", text = "#9333ea"),
    "CURVE" = list(bg = "#f3e8ff", text = "#9333ea"),

    # Cutter - Orange
    "CUTTER" = list(bg = "#ffedd5", text = "#ea580c"),
    "FC" = list(bg = "#ffedd5", text = "#ea580c"),
    "CUT" = list(bg = "#ffedd5", text = "#ea580c"),

    # Sinker - Tan/Brown
    "SINKER" = list(bg = "#fef3c7", text = "#d97706"),
    "SI" = list(bg = "#fef3c7", text = "#d97706"),

    # Splitter - Teal
    "SPLITTER" = list(bg = "#ccfbf1", text = "#0d9488"),
    "FS" = list(bg = "#ccfbf1", text = "#0d9488"),
    "SPLIT" = list(bg = "#ccfbf1", text = "#0d9488"),

    # Sweeper - Indigo
    "SWEEPER" = list(bg = "#e0e7ff", text = "#4f46e5"),
    "SW" = list(bg = "#e0e7ff", text = "#4f46e5")
  )

  # Return matching color or default gray
  if (pt_upper %in% names(colors)) {
    colors[[pt_upper]]
  } else {
    list(bg = "#f3f4f6", text = "#374151")
  }
}

get_mech_color <- function(val) {
  if (is.na(val) || is.nan(val) || !is.finite(val)) {
    return(list(bg = "#e2e8f0", text = "#1a1a1a"))
  }
  if      (val >= 6.75)  list(bg = "#39FF14", text = "#1a1a1a")  # neon green
  else if (val >= 6.5)   list(bg = "#16a34a", text = "#ffffff")   # darker green
  else if (val >= 6.25)  list(bg = "#bbf7d0", text = "#14532d")   # light soft green
  else if (val >= 5.833) list(bg = "#e2e8f0", text = "#1a1a1a")   # neutral gray
  else if (val >= 5.583) list(bg = "#fecaca", text = "#991b1b")   # light pink/red
  else if (val >= 5.333) list(bg = "#dc2626", text = "#ffffff")   # darker red
  else                   list(bg = "#ff0000", text = "#ffffff")   # bright red
}
