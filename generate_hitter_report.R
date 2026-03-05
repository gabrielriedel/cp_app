# =============================================================================
# generate_hitter_report.R
# Auto-generates a hitter scouting report Excel from Trackman data
#
# Usage (from RStudio or terminal in cp_app/ directory):
#   source("generate_hitter_report.R")
#   generate_hitter_report("HAW_RAI", pitcher_hand = "Right")  # vs RHP
#   generate_hitter_report("HAW_RAI", pitcher_hand = "Left")   # vs LHP
#
# Or run both at once:
#   generate_series_reports("HAW_RAI")
# =============================================================================

suppressPackageStartupMessages({
  library(openxlsx2)
  library(ggplot2)
  library(dplyr)
  library(viridisLite)
  library(DBI)
  library(RPostgres)
})

# ── Config ─────────────────────────────────────────────────────────────────────

PITCH_TYPES  <- c("4SM", "2SM", "CT", "CB", "SL", "CH")
COUNT_LABELS <- c("0-0", "RISP", "0-1", "1-0", "1-1", "2-0",
                  "2-1", "3-1", "0-2", "1-2", "2-2", "3-2")

PITCH_MAP <- c(
  "FourSeamFastBall" = "4SM", "Fastball"  = "4SM",
  "TwoSeamFastBall"  = "2SM", "Sinker"    = "2SM",
  "Cutter"           = "CT",
  "Curveball"        = "CB",  "Curve"     = "CB",
  "Slider"           = "SL",  "Sweeper"   = "SL",
  "ChangeUp"         = "CH",  "Changeup"  = "CH",  "Splitter" = "CH"
)

HITS <- c("Single", "SIngle", "Double", "Triple", "triple", "HomeRun", "Homerun")

map_pitch <- function(x) {
  m <- PITCH_MAP[x]
  ifelse(is.na(m), "Other", m)
}

# ── Data fetching ──────────────────────────────────────────────────────────────

#' Connect to Supabase and pull batter data for a team
get_data_db <- function(team_code, pitcher_hand, start_date, end_date) {
  con <- dbConnect(
    Postgres(),
    host     = Sys.getenv("SUPABASE_HOST"),
    port     = as.integer(Sys.getenv("SUPABASE_PORT")),
    dbname   = Sys.getenv("SUPABASE_DB"),
    user     = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASS"),
    sslmode  = "require"
  )
  on.exit(dbDisconnect(con))

  df <- dbGetQuery(con, "
    SELECT batter, batterside,
           taggedpitchtype, pitchcall, playresult, korbb,
           taggedhittype, exitspeed, angle, direction, distance,
           balls, strikes, platelocside, platelocheight, pitcherthrows
    FROM core_level.trackman_event
    WHERE batterteam = $1
      AND pitcherthrows = $2
      AND date BETWEEN $3 AND $4
      AND batter IS NOT NULL AND batter != ''
    ORDER BY batter
  ", params = list(team_code, pitcher_hand, start_date, end_date))

  df$pitch_abbrev <- map_pitch(df$taggedpitchtype)
  df
}

#' Load from local CSV (for dev/testing without DB connection)
get_data_csv <- function(csv_path, team_code, pitcher_hand, start_date, end_date) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)

  df <- df |>
    filter(
      BatterTeam   == team_code,
      PitcherThrows == pitcher_hand,
      Date >= as.Date(start_date),
      Date <= as.Date(end_date)
    ) |>
    rename(
      batter         = Batter,
      batterside     = BatterSide,
      taggedpitchtype = TaggedPitchType,
      pitchcall      = PitchCall,
      playresult     = PlayResult,
      korbb          = KorBB,
      taggedhittype  = TaggedHitType,
      exitspeed      = ExitSpeed,
      angle          = Angle,
      direction      = Direction,
      distance       = Distance,
      balls          = Balls,
      strikes        = Strikes,
      platelocside   = PlateLocSide,
      platelocheight = PlateLocHeight,
      pitcherthrows  = PitcherThrows
    )

  df$pitch_abbrev <- map_pitch(df$taggedpitchtype)
  df
}

# ── Statistics ─────────────────────────────────────────────────────────────────

calculate_batter_stats <- function(df) {
  df <- df |> mutate(
    is_pa_end = korbb %in% c("Strikeout", "Walk") |
                pitchcall %in% c("InPlay", "HitByPitch"),
    is_k      = korbb == "Strikeout",
    is_bb     = korbb == "Walk",
    is_hbp    = pitchcall == "HitByPitch",
    is_hr     = playresult %in% c("HomeRun", "Homerun"),
    is_inplay = pitchcall == "InPlay",
    is_hit    = playresult %in% HITS,
    tb = case_when(
      playresult %in% c("Single", "SIngle")   ~ 1,
      playresult == "Double"                  ~ 2,
      playresult %in% c("Triple", "triple")   ~ 3,
      playresult %in% c("HomeRun", "Homerun") ~ 4,
      TRUE ~ 0
    ),
    is_gb  = tolower(taggedhittype) %in% c("groundball", "ground ball"),
    is_fly = tolower(taggedhittype) %in% c("flyball", "fly ball",
                                            "linedrive", "line drive"),
    is_ab  = is_pa_end & !is_bb & !is_hbp
  )

  n_pa  <- sum(df$is_pa_end, na.rm = TRUE)
  n_ab  <- sum(df$is_ab,     na.rm = TRUE)
  n_bip <- sum(df$is_inplay, na.rm = TRUE)
  n_hit <- sum(df$is_hit,    na.rm = TRUE)

  fmt3 <- function(x) if (is.na(x) || is.nan(x)) "---" else sprintf(".%03d", round(x * 1000))
  fmtp <- function(x) if (is.na(x) || is.nan(x)) "---" else paste0(round(x * 100), "%")

  slg    <- if (n_ab > 0) sum(df$tb,    na.rm=TRUE) / n_ab else NA
  ba     <- if (n_ab > 0) n_hit / n_ab else NA

  df2k <- df |> filter(strikes == 2)
  n_ab_2k <- sum(df2k$is_ab, na.rm=TRUE)
  slg_2k  <- if (n_ab_2k > 0) sum(df2k$tb, na.rm=TRUE) / n_ab_2k else NA

  n_gb  <- sum(df$is_gb,  na.rm=TRUE)
  n_fly <- sum(df$is_fly, na.rm=TRUE)

  list(
    slg     = fmt3(slg),
    slg_2k  = fmt3(slg_2k),
    ba      = fmt3(ba),
    k       = sum(df$is_k,   na.rm=TRUE),
    bb      = sum(df$is_bb,  na.rm=TRUE),
    hbp     = sum(df$is_hbp, na.rm=TRUE),
    hr      = sum(df$is_hr,  na.rm=TRUE),
    fly_pct = if (n_bip > 0) fmtp(n_fly / n_bip) else "---",
    gb_pct  = if (n_bip > 0) fmtp(n_gb  / n_bip) else "---",
    n_pa    = n_pa,
    n_pitches = nrow(df)
  )
}

calculate_count_stats <- function(df) {
  total_pitches <- nrow(df)
  result <- list()

  for (cnt in COUNT_LABELS) {
    if (cnt == "RISP") {
      # RISP: approximate as any pitch where play context suggests runners on
      # (Trackman doesn't always have baserunner state — use PA-level proxy)
      # Fallback: show overall swing% as placeholder
      swings <- sum(df$pitchcall %in% c("InPlay","FoulBallNotFieldable",
                                         "FoulBallFieldable","StrikeSwinging"),
                    na.rm=TRUE)
      swing_pct <- if (total_pitches > 0)
        paste0(round(swings / total_pitches * 100), "%") else "---"
      result[["RISP"]] <- c("--", swing_pct)
      next
    }

    parts <- strsplit(cnt, "-")[[1]]
    b <- as.integer(parts[1])
    s <- as.integer(parts[2])
    in_count <- df |> filter(balls == b, strikes == s)
    n <- nrow(in_count)

    if (n == 0) {
      result[[cnt]] <- c("0%", "---")
      next
    }

    seen_pct  <- paste0(round(n / total_pitches * 100), "%")
    swings    <- sum(in_count$pitchcall %in% c("InPlay","FoulBallNotFieldable",
                                                "FoulBallFieldable","StrikeSwinging"),
                     na.rm=TRUE)
    swing_pct <- paste0(round(swings / n * 100), "%")

    result[[cnt]] <- c(seen_pct, swing_pct)
  }

  result
}

# ── Plots ──────────────────────────────────────────────────────────────────────

sz_lines <- list(
  ggplot2::annotate("rect",    xmin=-0.83, xmax=0.83, ymin=1.5, ymax=3.5,
                    fill=NA, color="black", linewidth=0.5),
  ggplot2::annotate("segment", x=-0.85, xend=0.85, y=0, yend=0,
                    color="black", linewidth=0.3),
  ggplot2::annotate("segment", x=-0.85, xend=-0.85, y=0, yend=-0.15,
                    color="black", linewidth=0.3),
  ggplot2::annotate("segment", x=0.85,  xend=0.85,  y=0, yend=-0.15,
                    color="black", linewidth=0.3),
  ggplot2::annotate("segment", x=-0.85, xend=0,  y=-0.15, yend=-0.3,
                    color="black", linewidth=0.3),
  ggplot2::annotate("segment", x=0.85,  xend=0,  y=-0.15, yend=-0.3,
                    color="black", linewidth=0.3)
)

plot_theme <- ggplot2::theme_void() +
  ggplot2::theme(
    plot.title  = ggplot2::element_text(hjust=0.5, size=7, face="bold",
                                        margin=ggplot2::margin(b=1)),
    plot.margin = ggplot2::margin(1,1,1,1),
    plot.background = ggplot2::element_rect(fill="white", color=NA)
  )

turbo_fill <- function() {
  cols    <- viridisLite::turbo(256)
  cols[1] <- "white"
  ggplot2::scale_fill_gradientn(colors=cols, guide="none")
}

na_plot <- function(title) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x=0, y=2.5, label="N/A", size=2.5, color="#aaa") +
    ggplot2::theme_void() +
    ggplot2::labs(title=title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust=0.5, size=7, face="bold"),
      plot.background = ggplot2::element_rect(fill="white", color=NA)
    )
}

density_heatmap <- function(data, title, min_n=3) {
  if (nrow(data) < min_n) return(na_plot(title))
  ggplot2::ggplot(data, ggplot2::aes(x=platelocside, y=platelocheight)) +
    ggplot2::stat_density_2d(
      ggplot2::aes(fill=ggplot2::after_stat(ndensity)),
      geom="raster", contour=FALSE, h=c(0.55,0.55), n=60
    ) +
    turbo_fill() +
    sz_lines +
    ggplot2::coord_fixed(xlim=c(-2,2), ylim=c(-0.5,4.5), expand=FALSE) +
    ggplot2::labs(title=title) +
    plot_theme
}

plot_sm <- function(df, pt) {
  d <- df |> filter(pitch_abbrev==pt, pitchcall=="StrikeSwinging",
                    !is.na(platelocside), !is.na(platelocheight))
  density_heatmap(d, paste0("S/M ", pt))
}

plot_avg <- function(df, pt) {
  d <- df |> filter(pitch_abbrev==pt, pitchcall=="InPlay",
                    playresult %in% HITS,
                    !is.na(platelocside), !is.na(platelocheight))
  density_heatmap(d, paste0("AVG ", pt))
}

plot_exitvelo <- function(df) {
  d <- df |> filter(pitchcall=="InPlay", !is.na(exitspeed),
                    !is.na(platelocside), !is.na(platelocheight))
  density_heatmap(d, "EXIT VELO")
}

plot_2k_miss <- function(df) {
  d <- df |> filter(strikes==2, pitchcall=="StrikeSwinging",
                    !is.na(platelocside), !is.na(platelocheight))
  density_heatmap(d, "2K MISS")
}

plot_2k_take <- function(df) {
  d <- df |> filter(strikes==2, pitchcall=="StrikeCalled",
                    !is.na(platelocside), !is.na(platelocheight))
  density_heatmap(d, "2K TAKE")
}

plot_spray <- function(df) {
  d <- df |>
    filter(pitchcall=="InPlay", !is.na(direction), !is.na(distance),
           distance > 0) |>
    mutate(
      x = distance * sin(direction * pi / 180),
      y = distance * cos(direction * pi / 180),
      result_cat = case_when(
        playresult %in% c("HomeRun","Homerun")        ~ "HR",
        playresult %in% c("Triple","triple")          ~ "3B",
        playresult == "Double"                        ~ "2B",
        playresult %in% c("Single","SIngle")          ~ "1B",
        TRUE                                          ~ "Out"
      )
    )

  if (nrow(d) == 0) return(na_plot("SPRAY"))

  foul_rad <- 45 * pi / 180
  arc_r    <- 330

  ggplot2::ggplot(d, ggplot2::aes(x=x, y=y, color=result_cat)) +
    # outfield arc
    ggplot2::annotate("path",
      x = arc_r * sin(seq(-foul_rad, foul_rad, length.out=100)),
      y = arc_r * cos(seq(-foul_rad, foul_rad, length.out=100)),
      color="grey70", linewidth=0.4) +
    # foul lines
    ggplot2::annotate("segment",
      x=0, xend=-arc_r*sin(foul_rad),
      y=0, yend= arc_r*cos(foul_rad), color="grey70", linewidth=0.4) +
    ggplot2::annotate("segment",
      x=0, xend= arc_r*sin(foul_rad),
      y=0, yend= arc_r*cos(foul_rad), color="grey70", linewidth=0.4) +
    # infield diamond (90 ft bases, 127.28 ft diagonal)
    ggplot2::annotate("polygon",
      x = c(0, 63.64, 0, -63.64),
      y = c(0, 63.64, 127.28, 63.64),
      fill=NA, color="grey70", linewidth=0.4) +
    ggplot2::geom_point(size=1.2, alpha=0.85) +
    ggplot2::scale_color_manual(
      values = c("HR"="#dc2626","3B"="#ea580c","2B"="#ca8a04",
                 "1B"="#16a34a","Out"="#94a3b8"),
      name = NULL) +
    ggplot2::coord_fixed(xlim=c(-220,220), ylim=c(-15,380)) +
    ggplot2::labs(title="SPRAY") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust=0.5, size=7, face="bold"),
      legend.position = "bottom",
      legend.text     = ggplot2::element_text(size=4.5),
      legend.key.size = ggplot2::unit(4, "pt"),
      legend.margin   = ggplot2::margin(0,0,0,0),
      plot.background = ggplot2::element_rect(fill="white", color=NA),
      plot.margin     = ggplot2::margin(1,1,2,1)
    )
}

# Save a ggplot to a temp PNG and return the path
save_png <- function(p, w=1.05, h=1.05, dpi=150) {
  f <- tempfile(fileext=".png")
  ggplot2::ggsave(f, p, width=w, height=h, dpi=dpi, bg="white")
  f
}

# Generate all plots for one batter and return list of temp file paths
generate_plots <- function(df) {
  list(
    sm      = setNames(
      lapply(PITCH_TYPES, function(pt) save_png(plot_sm(df, pt))),
      PITCH_TYPES
    ),
    avg     = setNames(
      lapply(PITCH_TYPES, function(pt) save_png(plot_avg(df, pt))),
      PITCH_TYPES
    ),
    exitvelo = save_png(plot_exitvelo(df)),
    spray    = save_png(plot_spray(df),    w=1.1, h=1.1),
    miss2k   = save_png(plot_2k_miss(df)),
    take2k   = save_png(plot_2k_take(df))
  )
}

cleanup_plots <- function(plots) {
  lapply(plots$sm,  unlink)
  lapply(plots$avg, unlink)
  unlink(plots$exitvelo)
  unlink(plots$spray)
  unlink(plots$miss2k)
  unlink(plots$take2k)
}

# ── Excel layout constants ─────────────────────────────────────────────────────

ROWS_PER_BATTER <- 20   # rows allocated per batter block

# Column positions (1-indexed)
COL_SM_START  <- 1      # A: S/M heatmaps use cols 1-12 (pairs: 1-2, 3-4, ..., 11-12)
COL_COUNT_00  <- 13     # M
COL_RISP      <- 14     # N
# Counts 0-1 through 3-2: two cols each starting at col 15
COUNT_COL_MAP <- c(
  "0-1"=15, "1-0"=17, "1-1"=19, "2-0"=21,
  "2-1"=23, "3-1"=25, "0-2"=27, "1-2"=29, "2-2"=31, "3-2"=33
)
COL_SLG       <- 35
COL_K         <- 37
COL_BB        <- 38
COL_HBP       <- 39
COL_HR        <- 40

# Bottom section columns
COL_EXITVELO  <- 13     # M (4-col span for images)
COL_SPRAY     <- 17     # Q
COL_NOTES     <- 21     # U (leave blank for manual)
COL_MISS2K    <- 25     # Y
COL_TAKE2K    <- 29     # AC
COL_SLG2K     <- 33     # AG (text value)
COL_FLY       <- 37
COL_GROUND    <- 39

# Row offsets within a batter block (0-indexed from block start)
OFF_HEADER     <- 0
OFF_COL_HDR    <- 1
OFF_SUBHDR     <- 2
OFF_DATA1      <- 3   # main data row (stats, count %s)
OFF_DATA2      <- 4   # second values row
OFF_DATA3      <- 5
OFF_DATA4      <- 6
OFF_SEP        <- 7   # thin separator rows (7-9)
OFF_AVG_HDR    <- 10
OFF_IMG_BOT    <- 11  # bottom images start here
OFF_BOT_DATA   <- 16  # FLY/GROUND/2KSLG values
OFF_END        <- 19  # last row of block

# Style helpers (wb_color uses hex WITHOUT #)
dark_blue  <- wb_color(hex="1a365d")
white_col  <- wb_color(hex="ffffff")
light_gray <- wb_color(hex="f1f5f9")
mid_gray   <- wb_color(hex="cbd5e1")
hdr_bg     <- wb_color(hex="e2e8f0")

d <- function(r_offset, r_base) r_base + r_offset   # absolute row helper

# ── Write one batter block to the workbook ────────────────────────────────────

write_batter_block <- function(wb, sheet, row_base,
                               batter_name, hand_label,
                               stats, counts, plots) {

  # Short helper: absolute row
  R <- function(off) row_base + off

  # ── Header row ──────────────────────────────────────────────────────────────
  wb <- wb_merge_cells(wb, sheet=sheet,
                       dims=wb_dims(rows=R(OFF_HEADER), cols=1:40))
  wb <- wb_add_data(wb, sheet=sheet,
                    x=paste0(hand_label, "  |  ", batter_name,
                              "  |  ", stats$n_pa, " PA  |  ",
                              stats$n_pitches, " pitches"),
                    dims=wb_dims(rows=R(OFF_HEADER), cols=1),
                    col_names=FALSE)
  wb <- wb_add_fill(wb, sheet=sheet,
                    dims=wb_dims(rows=R(OFF_HEADER), cols=1),
                    color=dark_blue)
  wb <- wb_add_font(wb, sheet=sheet,
                    dims=wb_dims(rows=R(OFF_HEADER), cols=1),
                    bold=TRUE, size=9, color=white_col)

  # ── Column headers row ──────────────────────────────────────────────────────
  r_hdr <- R(OFF_COL_HDR)
  for (i in seq_along(PITCH_TYPES)) {
    wb <- wb_add_data(wb, sheet=sheet,
                      x=paste0("S/M ", PITCH_TYPES[i]),
                      dims=wb_dims(rows=r_hdr, cols=(i-1)*2+1),
                      col_names=FALSE)
  }
  wb <- wb_add_data(wb, sheet=sheet, x="0-0",
                    dims=wb_dims(rows=r_hdr, cols=COL_COUNT_00), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="RISP",
                    dims=wb_dims(rows=r_hdr, cols=COL_RISP), col_names=FALSE)
  for (cnt in names(COUNT_COL_MAP)) {
    wb <- wb_add_data(wb, sheet=sheet, x=cnt,
                      dims=wb_dims(rows=r_hdr, cols=COUNT_COL_MAP[[cnt]]),
                      col_names=FALSE)
  }
  wb <- wb_add_data(wb, sheet=sheet, x="SLG",
                    dims=wb_dims(rows=r_hdr, cols=COL_SLG), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="K",
                    dims=wb_dims(rows=r_hdr, cols=COL_K),   col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="BB",
                    dims=wb_dims(rows=r_hdr, cols=COL_BB),  col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="HBP",
                    dims=wb_dims(rows=r_hdr, cols=COL_HBP), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="HR",
                    dims=wb_dims(rows=r_hdr, cols=COL_HR),  col_names=FALSE)

  # Style header row
  wb <- wb_add_fill(wb, sheet=sheet,
                    dims=wb_dims(rows=r_hdr, cols=1:40),
                    color=hdr_bg)
  wb <- wb_add_font(wb, sheet=sheet,
                    dims=wb_dims(rows=r_hdr, cols=1:40),
                    bold=TRUE, size=7)

  # ── Stat values ─────────────────────────────────────────────────────────────
  r_data <- R(OFF_DATA1)
  wb <- wb_add_data(wb, sheet=sheet, x=stats$slg,
                    dims=wb_dims(rows=r_data, cols=COL_SLG), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x=stats$ba,
                    dims=wb_dims(rows=r_data, cols=COL_SLG+1), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x=stats$k,
                    dims=wb_dims(rows=r_data, cols=COL_K),   col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x=stats$bb,
                    dims=wb_dims(rows=r_data, cols=COL_BB),  col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x=stats$hbp,
                    dims=wb_dims(rows=r_data, cols=COL_HBP), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x=stats$hr,
                    dims=wb_dims(rows=r_data, cols=COL_HR),  col_names=FALSE)

  # ── Count data (seen% and swing%) ───────────────────────────────────────────
  # 0-0 and RISP: single column, two rows
  c00 <- counts[["0-0"]]
  wb <- wb_add_data(wb, sheet=sheet, x=c00[1],
                    dims=wb_dims(rows=r_data,   cols=COL_COUNT_00), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x=c00[2],
                    dims=wb_dims(rows=r_data+1, cols=COL_COUNT_00), col_names=FALSE)

  risp <- counts[["RISP"]]
  wb <- wb_add_data(wb, sheet=sheet, x=risp[1],
                    dims=wb_dims(rows=r_data,   cols=COL_RISP), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x=risp[2],
                    dims=wb_dims(rows=r_data+1, cols=COL_RISP), col_names=FALSE)

  # Remaining counts: two columns each (seen%, swing%)
  for (cnt in names(COUNT_COL_MAP)) {
    col1 <- COUNT_COL_MAP[[cnt]]
    col2 <- col1 + 1
    vals <- counts[[cnt]]
    wb <- wb_add_data(wb, sheet=sheet, x=vals[1],
                      dims=wb_dims(rows=r_data,   cols=col1), col_names=FALSE)
    wb <- wb_add_data(wb, sheet=sheet, x=vals[2],
                      dims=wb_dims(rows=r_data+1, cols=col2), col_names=FALSE)
  }

  # Count sub-label (seen% / swing%)
  wb <- wb_add_data(wb, sheet=sheet, x="seen%",
                    dims=wb_dims(rows=R(OFF_SUBHDR), cols=COL_COUNT_00), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="swing%",
                    dims=wb_dims(rows=R(OFF_SUBHDR), cols=COL_RISP), col_names=FALSE)

  # ── S/M heatmap images (top section) ────────────────────────────────────────
  IMG_W <- 0.95  # inches
  IMG_H <- 0.95

  for (i in seq_along(PITCH_TYPES)) {
    pt  <- PITCH_TYPES[i]
    img <- plots$sm[[pt]]
    if (!is.null(img) && file.exists(img)) {
      wb <- wb_add_image(wb, sheet=sheet, file=img,
                         dims=wb_dims(rows=R(OFF_COL_HDR+1), cols=(i-1)*2+1),
                         width=IMG_W, height=IMG_H, units="in")
    }
  }

  # ── AVG section header (bottom) ─────────────────────────────────────────────
  r_avg_hdr <- R(OFF_AVG_HDR)
  for (i in seq_along(PITCH_TYPES)) {
    wb <- wb_add_data(wb, sheet=sheet,
                      x=paste0("AVG ", PITCH_TYPES[i]),
                      dims=wb_dims(rows=r_avg_hdr, cols=(i-1)*2+1),
                      col_names=FALSE)
  }
  wb <- wb_add_data(wb, sheet=sheet, x="EXIT VELO",
                    dims=wb_dims(rows=r_avg_hdr, cols=COL_EXITVELO), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="SPRAY",
                    dims=wb_dims(rows=r_avg_hdr, cols=COL_SPRAY), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="NOTES (manual)",
                    dims=wb_dims(rows=r_avg_hdr, cols=COL_NOTES), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="2K MISS",
                    dims=wb_dims(rows=r_avg_hdr, cols=COL_MISS2K), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="2K TAKE",
                    dims=wb_dims(rows=r_avg_hdr, cols=COL_TAKE2K), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="2K SLG",
                    dims=wb_dims(rows=r_avg_hdr, cols=COL_SLG2K), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="FLY%",
                    dims=wb_dims(rows=r_avg_hdr, cols=COL_FLY), col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x="GB%",
                    dims=wb_dims(rows=r_avg_hdr, cols=COL_GROUND), col_names=FALSE)

  # Style AVG header
  wb <- wb_add_fill(wb, sheet=sheet,
                    dims=wb_dims(rows=r_avg_hdr, cols=1:40),
                    color=hdr_bg)
  wb <- wb_add_font(wb, sheet=sheet,
                    dims=wb_dims(rows=r_avg_hdr, cols=1:40),
                    bold=TRUE, size=7)

  # ── Bottom stat values ──────────────────────────────────────────────────────
  r_bot <- R(OFF_BOT_DATA)
  wb <- wb_add_data(wb, sheet=sheet, x=stats$slg_2k,
                    dims=wb_dims(rows=r_bot, cols=COL_SLG2K),  col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x=stats$fly_pct,
                    dims=wb_dims(rows=r_bot, cols=COL_FLY),    col_names=FALSE)
  wb <- wb_add_data(wb, sheet=sheet, x=stats$gb_pct,
                    dims=wb_dims(rows=r_bot, cols=COL_GROUND), col_names=FALSE)

  # ── AVG + special heatmap images (bottom section) ───────────────────────────
  r_img_bot <- R(OFF_IMG_BOT)

  for (i in seq_along(PITCH_TYPES)) {
    pt  <- PITCH_TYPES[i]
    img <- plots$avg[[pt]]
    if (!is.null(img) && file.exists(img)) {
      wb <- wb_add_image(wb, sheet=sheet, file=img,
                         dims=wb_dims(rows=r_img_bot, cols=(i-1)*2+1),
                         width=IMG_W, height=IMG_H, units="in")
    }
  }

  special <- list(
    list(img=plots$exitvelo, col=COL_EXITVELO),
    list(img=plots$spray,    col=COL_SPRAY),
    list(img=plots$miss2k,   col=COL_MISS2K),
    list(img=plots$take2k,   col=COL_TAKE2K)
  )
  for (sp in special) {
    if (!is.null(sp$img) && file.exists(sp$img)) {
      wb <- wb_add_image(wb, sheet=sheet, file=sp$img,
                         dims=wb_dims(rows=r_img_bot, cols=sp$col),
                         width=IMG_W, height=IMG_H, units="in")
    }
  }

  # ── Bottom border on last row of batter block ────────────────────────────────
  wb <- wb_add_border(wb, sheet=sheet,
                      dims=wb_dims(rows=R(OFF_END), cols=1:40),
                      bottom_color=mid_gray, bottom_border="thin")

  invisible(wb)
}

# ── Main function ──────────────────────────────────────────────────────────────

#' Generate a hitter scouting report Excel for one opponent team + handedness
#'
#' @param team_code  Trackman team code, e.g. "HAW_RAI"
#' @param pitcher_hand  "Right" or "Left"
#' @param output_file  Output .xlsx path (auto-named if NULL)
#' @param use_db  TRUE = pull from Supabase; FALSE = use local CSV
#' @param csv_path  Path to CSV (used when use_db=FALSE)
#' @param start_date  Filter start date (string "YYYY-MM-DD")
#' @param end_date  Filter end date (default = today)
generate_hitter_report <- function(
    team_code,
    pitcher_hand  = "Right",
    output_file   = NULL,
    use_db        = TRUE,
    csv_path      = "app/data/cp_df.csv",
    start_date    = "2025-01-01",
    end_date      = as.character(Sys.Date())) {

  hand_label <- ifelse(pitcher_hand == "Right", "RHP", "LHP")

  if (is.null(output_file)) {
    safe_team   <- gsub("[^A-Za-z0-9]", "_", team_code)
    output_file <- paste0(safe_team, "_vs_", hand_label, ".xlsx")
  }

  cat(sprintf("\n=== Hitter Report: %s vs %s ===\n", team_code, hand_label))
  cat(sprintf("Date range: %s to %s\n", start_date, end_date))

  # Load data
  cat("Loading data...\n")
  if (use_db) {
    df_all <- get_data_db(team_code, pitcher_hand, start_date, end_date)
  } else {
    df_all <- get_data_csv(csv_path, team_code, pitcher_hand, start_date, end_date)
  }

  if (nrow(df_all) == 0) {
    stop("No data found for team='", team_code, "' hand='", pitcher_hand, "'")
  }

  batters <- sort(unique(df_all$batter))
  cat(sprintf("Found %d batters: %s\n\n", length(batters),
              paste(batters, collapse=", ")))

  # Create workbook + sheet
  wb     <- wb_workbook(creator="Shilo / Cal Poly Baseball")
  sheet  <- hand_label
  wb     <- wb_add_worksheet(wb, sheet=sheet)

  # Column widths
  for (i in 1:12) wb <- wb_set_col_widths(wb, sheet=sheet, cols=i, widths=6.0)   # heatmap pairs
  for (i in 13:34) wb <- wb_set_col_widths(wb, sheet=sheet, cols=i, widths=5.0)  # count cols
  for (i in 35:40) wb <- wb_set_col_widths(wb, sheet=sheet, cols=i, widths=7.5)  # stat cols

  # Process each batter — accumulate all temp plot files, clean up after save
  all_plots <- list()

  for (idx in seq_along(batters)) {
    batter <- batters[idx]
    cat(sprintf("[%d/%d] %s\n", idx, length(batters), batter))

    df_b   <- df_all |> filter(batter == !!batter)
    stats  <- calculate_batter_stats(df_b)
    counts <- calculate_count_stats(df_b)

    cat(sprintf("       %d PA | SLG %s | K %d | BB %d | HR %d | GB %s | FLY %s\n",
                stats$n_pa, stats$slg, stats$k, stats$bb, stats$hr,
                stats$gb_pct, stats$fly_pct))

    plots  <- generate_plots(df_b)
    all_plots[[idx]] <- plots   # keep alive until after wb_save

    row_base <- (idx - 1) * ROWS_PER_BATTER + 1

    # Set row heights for this block
    for (off in 0:OFF_END) {
      h <- dplyr::case_when(
        off == OFF_HEADER                          ~ 13,   # name header
        off %in% c(OFF_COL_HDR, OFF_SUBHDR)       ~  9,   # column headers
        off %in% c(OFF_SEP, OFF_SEP+1, OFF_SEP+2) ~  3,   # thin separators
        off == OFF_AVG_HDR                         ~  9,   # AVG header
        off >= OFF_IMG_BOT & off < OFF_BOT_DATA    ~ 55,   # image rows (pts)
        off == OFF_BOT_DATA                        ~  9,   # stat values
        off > OFF_BOT_DATA                         ~  3,   # trailing space
        TRUE                                       ~ 55    # image rows (top)
      )
      wb <- wb_set_row_heights(wb, sheet=sheet,
                               rows=row_base+off, heights=h)
    }

    wb <- write_batter_block(wb, sheet, row_base,
                             batter_name=batter,
                             hand_label=hand_label,
                             stats=stats, counts=counts, plots=plots)
  }

  # Save first, then clean up temp image files
  cat(sprintf("\nSaving %s...\n", output_file))
  wb_save(wb, output_file)
  cat("Done! Open:", normalizePath(output_file, mustWork=FALSE), "\n")

  lapply(all_plots, cleanup_plots)
  invisible(output_file)
}



#' Convenience: generate both RHP and LHP reports for a series
generate_series_reports <- function(team_code, ...) {
  generate_hitter_report(team_code, pitcher_hand="Right", ...)
  generate_hitter_report(team_code, pitcher_hand="Left",  ...)
}

# ── Example usage (uncomment to run) ──────────────────────────────────────────
# setwd("~/path/to/cp_app")
#
# # From database (production):
# generate_series_reports("HAW_RAI")
#
# # From local CSV (dev/testing, will filter to team in data):
# generate_hitter_report("HAW_RAI", use_db=FALSE, csv_path="app/data/cp_df.csv")
