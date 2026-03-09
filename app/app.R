if (file.exists(".Renviron")) readRenviron(".Renviron")
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(pool)
library(RPostgres)
library(DT)
library(rmarkdown)
library(kableExtra)
library(gridExtra)
library(viridisLite)
library(cachem)
library(memoise)
library(httr)
library(jsonlite)

# Source helper functions (includes caching setup)
source("R/scouting_helpers.R")
source("R/hitter_scouting_helpers.R")

pool <- dbPool(
  Postgres(),
  host     = Sys.getenv("SUPABASE_HOST"),
  port     = as.integer(Sys.getenv("SUPABASE_PORT")),
  dbname   = Sys.getenv("SUPABASE_DB"),
  user     = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PASS"),
  sslmode  = "require"
)

onStop(function() {
  poolClose(pool)
})

cp_df <- read.csv("data/cp_season_26.csv")

# Ensure Date is Date everywhere 
if (!inherits(cp_df$Date, "Date")) {
  cp_df$Date <- as.Date(cp_df$Date)
}

cp_pitchers <- cp_df |>
  filter(PitcherTeam == "CAL_MUS",
         Date >= as.Date("2026-01-01"))

cp_hitters <- cp_df |>
  filter(BatterTeam == "CAL_MUS")

player_summary <- function(df,
                           start_date = as.Date("2025-09-29"),
                           end_date   = Sys.Date()) {
  
  clean_replace_map <- c(
    "Spirdonoff, Gavin" = "Spiridonoff, Gavin",
    "Vonderhaar, Coco"  = "VonderHaar, Coco"
  )
  
  df |>
    filter(
      Date >= start_date,
      Date <= end_date,
      Batter != "Blood, Jason"
    ) |>
    mutate(
      Batter = str_trim(Batter),
      Batter = recode(Batter, !!!clean_replace_map),
      
      is_swing = if_else(
        PitchCall %in% c(
          "InPlay",
          "FoulBallNotFieldable",
          "FoulBallFieldable",
          "StrikeSwinging"
        ),
        1L, 0L
      ),
      
      is_walk = if_else(KorBB == "Walk", 1L, 0L),
      is_hbp  = if_else(PitchCall == "HitByPitch", 1L, 0L),
      
      k_miss = if_else(
        KorBB == "Strikeout" & PitchCall == "StrikeSwinging",
        1L, 0L
      ),
      
      k_called = if_else(
        KorBB == "Strikeout" & PitchCall == "StrikeCalled",
        1L, 0L
      ),
      
      `<2k_foul` = if_else(
        PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable") &
          Strikes < 2,
        1L, 0L
      ),
      
      `2k_foul` = if_else(
        PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable") &
          Strikes == 2,
        1L, 0L
      ),
      
      is_inplay = if_else(PitchCall == "InPlay", 1L, 0L),
      
      sm = if_else(PitchCall == "StrikeSwinging", 1L, 0L),
      
      sm_fb = if_else(
        PitchCall == "StrikeSwinging" &
          TaggedPitchType %in% c(
            "FourSeamFastBall",
            "Fastball",
            "TwoSeamFastBall",
            "Sinker"
          ),
        1L, 0L
      ),
      
      sm_sl_cb = if_else(
        PitchCall == "StrikeSwinging" &
          TaggedPitchType %in% c(
            "Slider",
            "Curveball",
            "Sweeper"
          ),
        1L, 0L
      ),
      
      sm_ch = if_else(
        PitchCall == "StrikeSwinging" &
          TaggedPitchType %in% c(
            "ChangeUp",
            "Splitter"
          ),
        1L, 0L
      )
    ) |>
    group_by(Batter) |>
    summarise(
      `Total Swings` = sum(is_swing),
      Walks          = sum(is_walk),
      HBP            = sum(is_hbp),
      K              = sum(k_miss),
      `>|`           = sum(k_called),
      `<2K Foul`     = sum(`<2k_foul`),
      `2K Foul`      = sum(`2k_foul`),
      `In Play`      = sum(is_inplay),
      `S&M FB`       = sum(sm_fb),
      `S&M CB/SL`    = sum(sm_sl_cb),
      `S&M CH`       = sum(sm_ch),
      `S&M Total`    = sum(sm),
      .groups = "drop"
    ) |>
    mutate(
      `S&M %` = paste0(
        round(`S&M Total` / `Total Swings` * 100, 2),
        "%"
      )
    )
}

header <- dashboardHeader(
  title = "Cal Poly Baseball Application"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Pitcher Dashboard", tabName='pitcher_dash'),
    menuItem("Hitter Dashboard", tabName='hitter_dash'),
    menuItem("Scouting Reports",
             menuSubItem("Opposing Pitchers", tabName='pitcher_scout'),
             menuSubItem("Opposing Hitters", tabName='hitter_scout'),
             tabName='scouting'
    ),
    menuItem("Live Dashboards",
             menuSubItem("Live Trackman Dashboard", tabName='live_trackman'),
             menuSubItem("Live Rapsodo Dashboard", tabName='live_rapsodo'),
             tabName='live_dash'),
    menuItem("Coach Lee - Hitter Table", tabName = "coach_lee_hitters", icon = icon("table")),
    menuItem("Student Manager Dashboard", tabName = "student_manager_dashboard", icon = icon("tachometer-alt"))
  )
)

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem("pitcher_dash",
            fluidRow(box(selectInput("pitcher_drop",
                                     "Select Pitcher",
                                     choices=sort(unique(cp_pitchers$Pitcher))
            )
            ),
            
            box(dateRangeInput("pitcher_game_range",
                               "Select Date Range",
                               start="2026-02-12",
                               end = Sys.Date())
            )
            ),
            tabsetPanel(
              
              tabPanel("Summary Dashboard",
                       fluidRow(
                         box(
                           title = "Pitch Summary",
                           width = 12,
                           status = "primary",
                           solidHeader = TRUE,
                           DT::DTOutput("table")
                         )
                       ),
                       fluidRow(
                         box(
                           width = 3,
                           checkboxGroupInput(
                             "heat_pitch",
                             "Select Pitch Type",
                             choices=sort(unique(cp_pitchers$TaggedPitchType)),
                             selected=NULL),
                           checkboxGroupInput(
                             "heat_hit_side",
                             "Select Batter Handedness",
                             choices=c("Right", "Left"),
                             selected=NULL),
                         ),
                         box(
                           width = 3,
                           plotOutput("pitcher_heat")
                         ),
                       ),
                       fluidRow(
                         box(plotly::plotlyOutput("movement_plot")),
                         box(plotly::plotlyOutput("release_plot"))
                       ),
                       value="pitcher_summary"
              ),
              
              tabPanel("Scouting Report",
                       fluidRow(
                       ),
                       value="pitcher_scouting"
              ),
              
              tabPanel("KinaTrax Report",
                       fluidRow(
                       ),
                       value="pitcher_kinatrax"
              ),
              
              tabPanel("Outing Notes",
                       fluidRow(
                         box(
                           dateInput("note_date", "Session Date"),
                           selectInput("session_type",
                                       "Session Type",
                                       choices=c("Bullpen", "Scrimmage", "Game", "Other")),
                           textAreaInput(
                             "note_text",
                             "Add notes",
                             width = "100%",
                             height = "150px",
                             resize = "vertical"
                           ),
                           actionButton("submit_note", "Submit"),
                           title="New Note",
                           width=4
                         ),
                         box(
                           title=textOutput("previous_notes_title"),
                           width=8,
                           uiOutput("notes_list")
                         )
                       ),
                       value="pitcher_bullpen"
              ),
            )
    ),
    tabItem("hitter_dash",
            fluidRow(box(selectInput("hitter_drop",
                                     "Select Hitter",
                                     choices=sort(unique(cp_hitters$Batter)))
            ),
            box(dateRangeInput("hitter_game_range",
                               "Select Date Range",
                               start="2026-02-12",
                               end = Sys.Date())
            )
            ),
            tabsetPanel(
              tabPanel("Summary Dashboard",
                       fluidRow(
                         box(title = "Batter Summary",
                             width = 12,
                             status = "primary",
                             solidHeader = TRUE,
                             DT::DTOutput("hitter_sum_table"))
                       ),
                       fluidRow(
                         box(
                           width = 3,
                           checkboxGroupInput(
                             "heat_pitch_hit",
                             "Select Pitch Type",
                             choices=sort(unique(cp_hitters$TaggedPitchType)),
                             selected=NULL),
                           checkboxGroupInput(
                             "heat_pitch_side",
                             "Select Pitcher Handedness",
                             choices=c("Right", "Left"),
                             selected=NULL),
                         ),
                         box(
                           width = 3,
                           plotOutput("hitter_heat")),
                       ),
                       value="hitter_summary"
              ),
              
              tabPanel("Scouting Report",
                       fluidRow(
                       ),
                       value="hitter_scouting"
              ),
              
              tabPanel("KinaTrax Report",
                       fluidRow(
                       ),
                       value="hitter_kinatrax"
              ),
            )
    ),
    tabItem("pitcher_scout",
      # Row 1: Controls
      fluidRow(
        box(
          width = 3,
          title = "Select Pitcher",
          status = "primary",
          solidHeader = TRUE,
          selectInput("opp_team", "Select Team", choices = NULL),
          downloadButton("download_team_reports", "Download All Team Reports (HTML)",
                         class = "btn-warning", style = "width: 100%; margin-bottom: 4px;"),
          tags$small(class = "text-muted", "RHH + LHH for all pitchers with saved notes"),
          br(), br(),
          selectInput("opp_pitcher", "Select Pitcher", choices = NULL),
          dateRangeInput("opp_dates", "Date Range",
                         start = "2026-02-12", end = Sys.Date()),
          radioButtons("opp_split", "Scouting vs.",
                       choices = c("LHH" = "Left", "RHH" = "Right"),
                       selected = "Right",
                       inline = TRUE),
          hr(),
          fileInput("pitcher_image", "Upload Pitcher Image",
                    accept = c("image/png", "image/jpeg", "image/jpg")),
          uiOutput("pitcher_image_preview"),
          hr(),
          tags$strong("Pitcher Stats (manual entry)"),
          fluidRow(
            column(3, textInput("stat_ip", "IP", value = "", placeholder = "45.2")),
            column(3, textInput("stat_era", "ERA", value = "", placeholder = "3.21")),
            column(3, textInput("stat_k", "K", value = "", placeholder = "52")),
            column(3, textInput("stat_bb", "BB", value = "", placeholder = "12"))
          ),
          fluidRow(
            column(4, textInput("stat_baa_lhh", "BAA vs L", value = "", placeholder = ".234")),
            column(4, textInput("stat_baa_rhh", "BAA vs R", value = "", placeholder = ".245")),
            column(4, actionButton("save_pitcher_stats", "Save", class = "btn-sm btn-info", style = "margin-top: 25px;"))
          )
        ),
        # Pitch Validation Panel
        box(
          width = 9,
          title = "Step 1: Validate Pitch Classifications",
          status = "warning",
          solidHeader = TRUE,
          p("Review pitch types below. If a pitch type appears misclassified based on movement,
             use the 'Remap To' dropdown to reassign it."),
          DT::DTOutput("pitch_validation_table"),
          br(),
          actionButton("apply_remap", "Apply & Generate Report",
                       class = "btn-primary btn-lg")
        )
      ),

      # Row 2: Report Preview (hidden until Apply clicked; re-hidden when split changes)
      fluidRow(
        shinyjs::hidden(
          div(id = "step2_preview",
          box(
            width = 12,
            title = "Step 2: Scouting Report Preview",
            status = "success",
            solidHeader = TRUE,
            downloadButton("download_report", "Download Report (HTML)", class = "btn-success"),
            tags$small(class = "text-muted", " Open in browser, then Print (Ctrl+P) to save as PDF"),
            hr(),
            # Arsenal Summary
            fluidRow(
              column(3, uiOutput("pitcher_info_box")),
              column(5, DT::DTOutput("arsenal_table")),
              column(4,
                plotOutput("slg_heatmap", height = "180px"),
                plotOutput("whiff_heatmap", height = "180px"),
                uiOutput("zone_pct_display")
              )
            ),
            # Active-split banner
            fluidRow(
              column(12, uiOutput("active_split_banner"))
            ),
            # Grade + Out Pitch + Gameplan Notes
            fluidRow(
              column(12,
                tags$div(
                  style = "background: #fffbeb; border: 1px solid #f6e05e; border-radius: 4px; padding: 10px; margin: 10px 0;",
                  tags$strong(style = "color: #744210; font-size: 14px;", "Gameplan / Attack Notes"),
                  fluidRow(
                    column(4, textInput("pitcher_grade", "Mix", value = "", placeholder = "e.g., FB/CH Mix")),
                    column(4, textInput("out_pitch", "Out Pitch", value = "", placeholder = "e.g., CH"))
                  ),
                  textAreaInput("notes_gameplan", NULL, rows = 3, width = "100%",
                                placeholder = "Enter gameplan and attack strategy notes here...")
                )
              )
            ),
            hr(),
            # Heatmaps - Overall Attack
            h4("OVERALL ATTACK"),
            fluidRow(
              column(2, textAreaInput("notes_attack", "Attack Notes", rows = 6, width = "100%")),
              column(2, plotOutput("heat_overall_1", height = "180px")),
              column(2, plotOutput("heat_overall_2", height = "180px")),
              column(2, plotOutput("heat_overall_3", height = "180px")),
              column(2, plotOutput("heat_overall_4", height = "180px"))
            ),
            hr(),
            # First Pitch
            h4("FIRST PITCH"),
            fluidRow(
              column(2, textAreaInput("notes_first_pitch", "First Pitch Notes", rows = 6, width = "100%")),
              column(2, plotOutput("heat_first_1", height = "180px")),
              column(2, plotOutput("heat_first_2", height = "180px")),
              column(2, plotOutput("heat_first_3", height = "180px")),
              column(2, plotOutput("heat_first_4", height = "180px"))
            ),
            hr(),
            # Hitter Advantage
            h4("COUNT SPECIFIC (Hitter Advantage: 1-0/2-0/2-1/3-0/3-1)"),
            fluidRow(
              column(2, textAreaInput("notes_hitter_adv", "Hitter Advantage Notes", rows = 6, width = "100%")),
              column(2, plotOutput("heat_adv_1", height = "180px")),
              column(2, plotOutput("heat_adv_2", height = "180px")),
              column(2, plotOutput("heat_adv_3", height = "180px")),
              column(2, plotOutput("heat_adv_4", height = "180px"))
            ),
            hr(),
            # 2K
            h4("COUNT SPECIFIC (2K)"),
            fluidRow(
              column(2, textAreaInput("notes_2k", "2K Notes", rows = 6, width = "100%")),
              column(2, plotOutput("heat_2k_1", height = "180px")),
              column(2, plotOutput("heat_2k_2", height = "180px")),
              column(2, plotOutput("heat_2k_3", height = "180px")),
              column(2, plotOutput("heat_2k_4", height = "180px"))
            ),
            hr(),
            # RISP Image Uploads - aligned with other heatmap rows
            h4("RISP (Runners in Scoring Position)"),
            p(class = "text-muted", style = "font-size: 11px; margin-bottom: 8px;",
              "Enter usage percentages and upload heatmap images for RISP situations."),
            fluidRow(
              column(2, textAreaInput("notes_risp", "RISP Notes", rows = 4, width = "100%",
                                      placeholder = "Notes for RISP...")),
              column(2, uiOutput("risp_slot_1")),
              column(2, uiOutput("risp_slot_2")),
              column(2, uiOutput("risp_slot_3")),
              column(2, uiOutput("risp_slot_4"))
            ),
            # RISP Usage inputs
            fluidRow(
              column(2),  # Empty to align with notes column
              column(8, uiOutput("risp_usage_inputs"))
            )
          )
          )
        )
      )
    ),
    tabItem(
      "hitter_scout",
      # Row 1: Controls
      fluidRow(
        box(
          width = 3,
          title = "Hitter Scouting Controls",
          status = "primary",
          solidHeader = TRUE,
          selectInput("hitter_opp_team", "Select Team", choices = NULL),
          radioButtons("hitter_pitcher_hand", "Pitcher Handedness",
                       choices = c("vs RHP" = "Right", "vs LHP" = "Left"),
                       selected = "Right", inline = TRUE),
          dateRangeInput("hitter_opp_dates", "Date Range",
                         start = "2026-02-12", end = Sys.Date()),
          hr(),
          actionButton("load_hitter_report", "Load Batters",
                       class = "btn-primary btn-lg", style = "width: 100%;"),
          hr(),
          downloadButton("download_hitter_report", "Download Report (HTML)",
                         class = "btn-success", style = "width: 100%;")
        ),
        box(
          width = 9,
          title = "Hitter Scouting Report",
          status = "success",
          solidHeader = TRUE,
          tags$div(
            style = "margin-bottom: 10px;",
            tags$small(class = "text-muted",
              "Select a team and pitcher handedness, then click 'Load Batters' to generate the scouting report. ",
              "Notes and percentages auto-save. Click counts to toggle highlight colors."
            )
          ),
          uiOutput("hitter_scout_content")
        )
      )
    ),
    tabItem(
      "coach_lee_hitters",
      fluidRow(
        box(
          width = 3,
          title = "Coach Lee Controls",
          
          selectInput(
            "coach_scope",
            "Select Period",
            choices = c("Fall", "Winter", "Season"),
            selected = "Fall"
          ),
          
          selectInput(
            "coach_view",
            "View",
            choices = c("Overall (period total)", "Weekly breakdown"),
            selected = "Overall (period total)"
          ),
          
          selectInput(
            "coach_player",
            "Select Player",
            choices = "All Players",
            selected = "All Players"
          ),
          
          selectInput(
            "coach_week",
            "Select Week (weekly view only)",
            choices = "All weeks",
            selected = "All weeks"
          ),
          
          helpText("Overall = totals for the whole period. Weekly = totals by week.")
        ),
        box(
          width = 9,
          title = "Coach Lee Hitter Summary",
          DT::DTOutput("coach_lee_table")
        )
      )
    ),
    tabItem(
      "student_manager_dashboard",
      fluidRow(
        box(
          width = 6,
          title = "Manage Upcoming Opponents",
          status = "primary",
          solidHeader = TRUE,
          p("Add teams to the upcoming opponents list. Only these teams will appear in the scouting report dropdown."),
          hr(),
          fluidRow(
            column(6, selectInput("add_opponent_team", "Add Team:", choices = NULL)),
            column(4, dateInput("add_opponent_date", "Game Date:", value = NULL)),
            column(2, br(), actionButton("add_opponent_btn", "Add", class = "btn-success", style = "margin-top: 0px;"))
          ),
          tags$small(class = "text-muted", "If no upcoming opponents are set, all teams will be shown in scouting."),
          hr(),
          h4("Current Upcoming Opponents"),
          uiOutput("upcoming_opponents_list")
        ),
        box(
          width = 6,
          title = "Quick Links",
          status = "info",
          solidHeader = TRUE,
          p("Student manager tools and quick access links."),
          tags$ul(
            tags$li("Use 'Manage Upcoming Opponents' to control which teams appear in scouting dropdowns"),
            tags$li("Add game dates to help track the schedule"),
            tags$li("Remove teams after series are complete")
          )
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin="blue")

server <- function(input, output, session) {
  
  #########################################
  ####### Pitcher Dashboard Graphics ######
  #########################################
  
  rval_pitcher_df <- reactive({
    cp_pitchers |>
      filter(Pitcher == input$pitcher_drop
             & Date >= input$pitcher_game_range[1]
             & Date <= input$pitcher_game_range[2])
  })
  
  rval_pitcher_summary_df <- reactive({
    df <- rval_pitcher_df()
    req(nrow(df) > 0)

    df |>
      group_by(TaggedPitchType) |>
      summarize(
        PitchCount = n(),
        Usage = PitchCount/nrow(df),
        # Calculate whiff rate from PitchCall
        Swings = sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay"), na.rm = TRUE),
        Whiffs = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
        WhiffRate = if_else(Swings > 0, Whiffs / Swings, NA_real_),
        Velo = round(mean(RelSpeed, na.rm = TRUE), 1),
        SpinRate = round(mean(SpinRate, na.rm = TRUE), 0),
        IVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
        HB = round(mean(HorzBreak, na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      select(-Swings, -Whiffs)
  })
  
  output$table <- DT::renderDT({
    rval_pitcher_summary_df()  |>
      filter(Usage > 0.01) |>
      arrange(desc(Usage)) |>
      mutate(
        Usage = scales::percent(Usage, 1),
        WhiffRate = scales::percent(WhiffRate, 1)
      )
  })
  
  output$movement_plot <- plotly::renderPlotly({
    gg_move <- rval_pitcher_df() |>
      left_join(rval_pitcher_summary_df(), by="TaggedPitchType") |>
      filter(Usage > 0.01) |>
      ggplot() +
      geom_point(aes(
        x=HorzBreak,
        y=InducedVertBreak,
        color=TaggedPitchType,
        text = paste(
          "Pitch Type:", TaggedPitchType,
          "<br>V Break:", round(InducedVertBreak, 2),
          "<br>H Break:", round(HorzBreak, 2),
          "<br>Release Speed:", round(RelSpeed, 1),
          "<br>Date:", Date
        ))) +
      labs(
        x="Horizontal Break",
        y="Induced Vertical Break",
        title="Movement Plot") +
      scale_color_discrete(name = "Pitch Type") +
      theme_minimal() +
      coord_fixed(ratio = 1)
    
    plotly::ggplotly(gg_move, tooltip="text")
  })
  
  output$release_plot <- plotly::renderPlotly({
    gg_release <- rval_pitcher_df() |>
      left_join(rval_pitcher_summary_df(), by="TaggedPitchType") |>
      filter(Usage > 0.01) |>
      ggplot() +
      geom_point(aes(
        x=RelSide,
        y=RelHeight,
        color=TaggedPitchType,
        text = paste(
          "Pitch Type:", TaggedPitchType,
          "<br>Rel Side:", round(RelSide, 1),
          "<br>Rel Height:", round(RelHeight, 1),
          "<br>Release Speed:", round(RelSpeed, 1),
          "<br>Date:", Date
        ))) +
      labs(
        x="Relase Side (Ft)",
        y="Release Height (Ft)",
        title="Release Plot") +
      scale_color_discrete(name = "Pitch Type") +
      theme_minimal() +
      coord_fixed(ratio = 1)
    
    plotly::ggplotly(gg_release, tooltip="text")
  })
  
  output$pitcher_heat <- renderPlot({
    
    pitch_sel    <- input$heat_pitch
    hit_side_sel <- input$heat_hit_side
    cols <- viridisLite::turbo(256)
    cols[1] <- "white"
    
    if (is.null(pitch_sel) || length(pitch_sel) == 0) {
      pitch_sel <- unique(cp_pitchers$TaggedPitchType)
    }
    
    if (is.null(hit_side_sel) || length(hit_side_sel) == 0) {
      hit_side_sel <- unique(cp_pitchers$BatterSide)
    }
    
    rval_pitcher_df() |>
      left_join(rval_pitcher_summary_df(), by="TaggedPitchType") |>
      filter(Usage > 0.01 & TaggedPitchType %in% pitch_sel & BatterSide %in% hit_side_sel) |>
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(
        aes(fill = after_stat(ndensity)),
        geom     = "raster",
        contour  = FALSE,
        h        = c(0.55, 0.55),
        n        = 300
      ) +
      scale_fill_gradientn(colors = cols) +
      coord_fixed(
        xlim   = c(-2.3, 2.3),
        ylim   = c(-1, 5),
        expand = FALSE
      ) +
      labs(x = "Horizontal", y = "Vertical", title = "Pitch Usage Heatmap") +
      annotate("segment", x = -0.85, xend = 0.85,  y = 1.6, yend = 1.6, color = "black", linewidth = 1.2) +
      annotate("segment", x = -0.85, xend = 0.85,  y = 3.5, yend = 3.5, color = "black", linewidth = 1.2) +
      annotate("segment", x = -0.85, xend = -0.85, y = 1.6, yend = 3.5, color = "black", linewidth = 1.2) +
      annotate("segment", x = 0.85,  xend = 0.85,  y = 1.6, yend = 3.5, color = "black", linewidth = 1.2) +
      annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black") +
      annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black") +
      annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black") +
      annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black") +
      annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black") +
      theme_minimal()
  })
  
  ##########################################
  ####### Pitcher Notes Functionality ######
  ##########################################
  
  output$previous_notes_title <- renderText({
    paste("Previous Notes for", input$pitcher_drop)
  })
  
  notes_trigger <- reactiveVal(0)
  
  notes_df <- reactive({
    req(input$pitcher_drop)
    notes_trigger()
    input$submit_note
    
    dbGetQuery(
      pool,
      "
          SELECT id, pitcher, date, session_type, note_text
          FROM moir_notes
          WHERE pitcher = $1
          ORDER BY date DESC, created_at DESC
        ",
      params = list(input$pitcher_drop)
    )
  })
  
  observeEvent(input$submit_note, {
    req(input$pitcher_drop, input$note_date, input$session_type, input$note_text)
    
    dbExecute(
      pool,
      "
          INSERT INTO moir_notes (pitcher, date, session_type, note_text)
          VALUES ($1, $2, $3, $4)
        ",
      params = list(
        input$pitcher_drop,
        input$note_date,
        input$session_type,
        input$note_text
      )
    )
    
    updateTextAreaInput(session, "note_text", value = "")
    notes_trigger(notes_trigger() + 1)
  })
  
  observeEvent(input$delete_note_id, {
    req(input$delete_note_id)
    
    dbExecute(
      pool,
      "DELETE FROM moir_notes WHERE id = $1",
      params = list(input$delete_note_id)
    )
    
    notes_trigger(notes_trigger() + 1)
  })
  
  editing_note_id <- reactiveVal(NULL)
  
  observeEvent(input$edit_note_id, {
    req(input$edit_note_id)
    editing_note_id(input$edit_note_id)
    
    df <- notes_df()
    this_id <- input$edit_note_id
    current_text <- df$note_text[df$id == this_id][1]
    
    showModal(
      modalDialog(
        title = "Edit Note",
        textAreaInput(
          "edit_note_text",
          "Note",
          value = current_text,
          width = "100%",
          height = "200px"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_edit", "Save")
        ),
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$save_edit, {
    req(editing_note_id(), input$edit_note_text)
    
    dbExecute(
      pool,
      "UPDATE moir_notes SET note_text = $1 WHERE id = $2",
      params = list(input$edit_note_text, editing_note_id())
    )
    
    removeModal()
    notes_trigger(notes_trigger() + 1)
  })
  
  output$notes_list <- renderUI({
    df <- notes_df()
    
    if (nrow(df) == 0) {
      return(tags$em("No notes yet for this pitcher."))
    }
    
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        
        shinydashboard::box(
          title       = paste(row$session_type, "-", row$date),
          collapsible = TRUE,
          collapsed   = TRUE,
          width       = 12,
          p(row$note_text),
          div(
            style = "margin-top: 10px;",
            actionButton(
              inputId = paste0("edit_", row$id),
              label   = "Edit",
              class   = "btn-sm",
              onclick = sprintf(
                "Shiny.setInputValue('edit_note_id', '%s', {priority: 'event'})",
                row$id
              )
            ),
            actionButton(
              inputId = paste0("delete_", row$id),
              label   = "Delete",
              class   = "btn-sm btn-danger",
              onclick = sprintf(
                "Shiny.setInputValue('delete_note_id', '%s', {priority: 'event'})",
                row$id
              )
            )
          )
        )
      })
    )
  })
  
  ##########################################
  ####### Hitter Dashboard Graphics  #######
  ##########################################
  
  rval_hitter_df <- reactive({
    cp_hitters |>
      filter(Batter == input$hitter_drop
             & Date >= input$hitter_game_range[1]
             & Date <= input$hitter_game_range[2]) |>
      mutate(IsSwing  = if_else(PitchCall %in% c('StrikeSwinging','FoulBall','FoulBallNotFieldable','FoulBallFieldable','InPlay'), 1, 0),
             IsWhiff  = if_else(PitchCall == 'StrikeSwinging', 1, 0),
             IsWalk   = if_else(KorBB == 'Walk', 1, 0),
             IsHBP    = if_else(PitchCall == 'HitByPitch', 1, 0),
             IsKWhiff = if_else((PitchCall == 'StrikeSwinging') & (KorBB == 'Strikeout'), 1, 0),
             IsKCalled = if_else((PitchCall %in% c('StrikeCalled', 'Strikecalled') & (KorBB == 'Strikeout')), 1, 0),
             LessTwoKFoul = if_else((PitchCall %in% c('FoulBallNotFieldable', 'FoulBallFieldable','FoulBall') & (Strikes < 2)), 1, 0),
             TwoKFoul = if_else((PitchCall %in% c('FoulBallNotFieldable', 'FoulBallFieldable','FoulBall') & (Strikes == 2)), 1, 0),
             Is_InPlay = if_else(PitchCall == "InPlay", 1, 0),
             Whiff_FB = if_else((IsWhiff == 1) & (TaggedPitchType %in% c('Fastball', 'FourSeamFastBall', 'OneSeamFastBall', 'Sinker', 'TwoSeamFastBall')), 1, 0),
             Whiff_CB_SL = if_else((IsWhiff == 1) & (TaggedPitchType %in% c('Curveball', 'Slider', 'Sweeper')), 1, 0),
             Whiff_CH = if_else((IsWhiff == 1) & (TaggedPitchType %in% c('ChangeUp', 'Splitter')), 1, 0)
      )
  })
  
  output$hitter_sum_table <- DT::renderDT({
    rval_hitter_df() |>
      group_by(Batter) |>
      summarize(
        Swings = sum(IsSwing),
        Walks = sum(IsWalk),
        HBP = sum(IsHBP),
        K = sum(IsKWhiff),
        K_Looking = sum(IsKCalled),
        Less_Two_K_Foul = sum(LessTwoKFoul),
        Two_K_Foul = sum(TwoKFoul),
        InPlay = sum(Is_InPlay),
        Whiff_FB = sum(Whiff_FB),
        Whiff_CB_SL = sum(Whiff_CB_SL),
        Whiff_CH = sum(Whiff_CH),
        Whiff_Rate = sum(IsWhiff)/Swings,
        .groups = "drop"
      ) |>
      mutate(
        Whiff_Rate = scales::percent(Whiff_Rate, 1)
      )
  })
  
  output$hitter_heat <- renderPlot({
    
    pitch_sel      <- input$heat_pitch_hit
    pitch_side_sel <- input$heat_pitch_side
    cols <- viridisLite::turbo(256)
    cols[1] <- "white"
    
    if (is.null(pitch_sel) || length(pitch_sel) == 0) {
      pitch_sel <- unique(cp_hitters$TaggedPitchType)
    }
    
    if (is.null(pitch_side_sel) || length(pitch_side_sel) == 0) {
      pitch_side_sel <- unique(cp_hitters$PitcherThrows)
    }
    
    rval_hitter_df() |>
      filter(TaggedPitchType %in% pitch_sel & PitcherThrows %in% pitch_side_sel) |>
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(
        aes(fill = after_stat(ndensity)),
        geom     = "raster",
        contour  = FALSE,
        h        = c(0.55, 0.55),
        n        = 300
      ) +
      scale_fill_gradientn(colors = cols) +
      coord_fixed(
        xlim   = c(-2.3, 2.3),
        ylim   = c(-1, 5),
        expand = FALSE
      ) +
      labs(x = "Horizontal", y = "Vertical", title = "Pitches Seen Heatmap") +
      annotate("segment", x = -0.85, xend = 0.85,  y = 1.6, yend = 1.6, color = "black", linewidth = 1.2) +
      annotate("segment", x = -0.85, xend = 0.85,  y = 3.5, yend = 3.5, color = "black", linewidth = 1.2) +
      annotate("segment", x = -0.85, xend = -0.85, y = 1.6, yend = 3.5, color = "black", linewidth = 1.2) +
      annotate("segment", x = 0.85,  xend = 0.85,  y = 1.6, yend = 3.5, color = "black", linewidth = 1.2) +
      annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, color = "black") +
      annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, color = "black") +
      annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, color = "black") +
      annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, color = "black") +
      annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, color = "black") +
      theme_minimal()
  })
  
  ##########################################
  ####### Coach Lee Hitter Summary Table ###
  ##########################################
  
  coach_periods <- reactive({
    list(
      Fall   = c(as.Date("2025-09-25"), as.Date("2025-12-31")),
      Winter = c(as.Date("2026-01-01"), as.Date("2026-02-12")),
      Season = c(as.Date("2026-02-13"), as.Date("2026-05-16"))
    )
  })
  
  coach_df <- reactive({
    df <- cp_df
    
    # Ensure Date is Date (adjust format here if your CSV is m/d/Y)
    if (!inherits(df$Date, "Date")) df$Date <- as.Date(df$Date)
    
    df |>
      filter(BatterTeam == "CAL_MUS") |>
      filter(Batter != "Blood, Jason")
  })
  
  coach_range <- reactive({
    req(input$coach_scope)
    rng <- coach_periods()[[input$coach_scope]]
    list(start = rng[1], end = rng[2])
  })
  
  # Update player dropdown when period changes (only players in that period)
  observeEvent(input$coach_scope, {
    rng <- coach_range()
    
    players <- coach_df() |>
      filter(Date >= rng$start, Date <= rng$end) |>
      pull(Batter) |>
      unique() |>
      sort()
    
    updateSelectInput(
      session,
      "coach_player",
      choices = c("All Players", players),
      selected = "All Players"
    )
  }, ignoreInit = FALSE)
  
  # Build available weeks for the chosen period
  coach_weeks_df <- reactive({
    rng <- coach_range()
    
    coach_df() |>
      filter(Date >= rng$start, Date <= rng$end) |>
      mutate(
        week_start = Date - (as.integer(format(Date, "%u")) - 1L), # Monday start
        week_end   = week_start + 6L,
        week_label = paste0("Week of ", week_start, " (", week_start, " to ", week_end, ")")
      ) |>
      distinct(week_start, week_end, week_label) |>
      arrange(week_start)
  })
  
  # Update week dropdown when period OR view changes
  observeEvent(list(input$coach_scope, input$coach_view), {
    w <- coach_weeks_df()
    
    updateSelectInput(
      session,
      "coach_week",
      choices = c("All weeks", w$week_label),
      selected = "All weeks"
    )
  }, ignoreInit = FALSE)
  
  # Core filtered dataset based on period + optional player
  coach_filtered <- reactive({
    rng <- coach_range()
    
    df <- coach_df() |>
      filter(Date >= rng$start, Date <= rng$end)
    
    if (!is.null(input$coach_player) && input$coach_player != "All Players") {
      df <- df |>
        filter(Batter == input$coach_player)
    }
    
    df
  })
  
  # Overall period table (one summary per batter)
  coach_overall_table <- reactive({
    df <- coach_filtered()
    
    # player_summary already groups by Batter
    out <- player_summary(df, start_date = min(df$Date), end_date = max(df$Date))
    
    # If they picked one player, still fine (one row)
    out |>
      arrange(Batter)
  })
  
  # Weekly table (summary per batter per week)
  coach_weekly_table <- reactive({
    df <- coach_filtered()
    
    df2 <- df |>
      mutate(
        week_start = Date - (as.integer(format(Date, "%u")) - 1L),
        week_end   = week_start + 6L,
        Week = paste0("Week of ", week_start, " (", week_start, " to ", week_end, ")")
      )
    
    # If a specific week is selected, filter to it
    if (!is.null(input$coach_week) && input$coach_week != "All weeks") {
      df2 <- df2 |>
        filter(Week == input$coach_week)
    }
    
    weekly <- df2 |>
      group_by(Week) |>
      group_modify(~ player_summary(.x, start_date = min(.x$Date), end_date = max(.x$Date))) |>
      ungroup()
    
    weekly |>
      select(Week, everything()) |>
      arrange(Week, Batter)
  })
  
  output$coach_lee_table <- DT::renderDT({
    req(input$coach_view)

    tbl <- if (input$coach_view == "Overall (period total)") {
      coach_overall_table()
    } else {
      coach_weekly_table()
    }

    DT::datatable(
      tbl,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })

  ##########################################
  ####### Scouting Report - Opposing Pitchers
  ##########################################

  # Reactive to store uploaded pitcher image URL
  rval_pitcher_image_url <- reactiveVal(NULL)

  # Reactive to store pitch descriptions
  rval_pitch_descriptions <- reactiveVal(list())

  # Reactive to store per-pitch velocity overrides
  rval_velo_overrides <- reactiveVal(list())

  # Reactive to store processed data for description inputs
  rval_processed_data <- reactiveVal(NULL)
  rval_saved_pitch_edits <- reactiveVal(list(deletions = character(0), remaps = list()))

  # Reactive trigger for refreshing upcoming opponents
  upcoming_trigger <- reactiveVal(0)

  # Populate team dropdown from database (uses upcoming opponents if available)
  observe({
    upcoming_trigger()  # Re-run when upcoming opponents change
    upcoming_teams <- get_upcoming_teams(pool)
    if (length(upcoming_teams) == 0) {
      # Fallback to all teams if no upcoming opponents set
      teams <- get_opposing_teams(pool)
    } else {
      teams <- upcoming_teams
    }
    updateSelectInput(session, "opp_team", choices = teams)
  })

  # Populate the "Add Team" dropdown with all available teams
  observe({
    all_teams <- get_opposing_teams(pool)
    updateSelectInput(session, "add_opponent_team", choices = all_teams)
  })

  # Display current upcoming opponents list
  output$upcoming_opponents_list <- renderUI({
    upcoming_trigger()  # Re-run when upcoming opponents change
    opponents <- get_upcoming_opponents_list(pool)

    if (nrow(opponents) == 0) {
      return(tags$em(style = "color: #888;", "None set (showing all teams)"))
    }

    tagList(
      lapply(seq_len(nrow(opponents)), function(i) {
        row <- opponents[i, ]
        date_text <- if (!is.na(row$game_date)) {
          format(row$game_date, "%m/%d/%Y")
        } else {
          ""
        }
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 3px 0; border-bottom: 1px solid #eee;",
          tags$span(
            tags$strong(row$team_code),
            if (date_text != "") tags$small(style = "color: #888; margin-left: 5px;", paste0("(", date_text, ")"))
          ),
          actionButton(
            inputId = paste0("remove_opp_", row$team_code),
            label = "X",
            class = "btn-xs btn-danger",
            style = "padding: 1px 5px; font-size: 10px;",
            onclick = sprintf(
              "Shiny.setInputValue('remove_opponent_team', '%s', {priority: 'event'})",
              row$team_code
            )
          )
        )
      })
    )
  })

  # Handle adding an upcoming opponent
  observeEvent(input$add_opponent_btn, {
    req(input$add_opponent_team)
    game_date <- if (!is.null(input$add_opponent_date) && !is.na(input$add_opponent_date)) {
      input$add_opponent_date
    } else {
      NULL
    }
    success <- add_upcoming_opponent(pool, input$add_opponent_team, team_name = NULL, game_date = game_date)
    if (success) {
      showNotification(paste("Added", input$add_opponent_team, "to upcoming opponents"), type = "message")
      upcoming_trigger(upcoming_trigger() + 1)
      # Clear the date input
      updateDateInput(session, "add_opponent_date", value = NA)
    } else {
      showNotification("Failed to add opponent", type = "error")
    }
  })

  # Handle removing an upcoming opponent
  observeEvent(input$remove_opponent_team, {
    req(input$remove_opponent_team)
    success <- remove_upcoming_opponent(pool, input$remove_opponent_team)
    if (success) {
      showNotification(paste("Removed", input$remove_opponent_team, "from upcoming opponents"), type = "message")
      upcoming_trigger(upcoming_trigger() + 1)
    } else {
      showNotification("Failed to remove opponent", type = "error")
    }
  })

  # Update pitcher dropdown when team changes
  observeEvent(input$opp_team, {
    req(input$opp_team)
    pitchers <- get_team_pitchers(pool, input$opp_team)
    updateSelectInput(session, "opp_pitcher", choices = pitchers)
    # Reset pitcher image when team changes
    rval_pitcher_image_url(NULL)
    # Reset saved pitch edits when team changes
    rval_saved_pitch_edits(list(deletions = character(0), remaps = list()))
    # Reset delete checkboxes when team changes
    shinyjs::runjs("
      $('.delete-check').prop('checked', false);
      for (var i = 1; i <= 20; i++) {
        Shiny.setInputValue('delete_pitch_' + i, false, {priority: 'event'});
      }
    ")
  })

  # Check for existing pitcher image when pitcher is selected
  observeEvent(input$opp_pitcher, {
    req(input$opp_team, input$opp_pitcher)

    supabase_url <- Sys.getenv("SUPABASE_URL")
    bucket_name <- "scouting-images"

    if (supabase_url == "") return()

    # Try common extensions
    safe_team <- gsub("[^A-Za-z0-9_-]", "_", input$opp_team)
    safe_pitcher <- gsub("[^A-Za-z0-9_-]", "_", input$opp_pitcher)

    # Try to find existing image (png or jpg)
    for (ext in c("png", "jpg", "jpeg")) {
      storage_path <- paste0(safe_team, "/", safe_pitcher, ".", ext)
      public_url <- paste0(supabase_url, "/storage/v1/object/public/", bucket_name, "/", storage_path)

      # Check if image exists with a HEAD request
      response <- tryCatch({
        httr::HEAD(public_url)
      }, error = function(e) NULL)

      if (!is.null(response) && httr::status_code(response) == 200) {
        rval_pitcher_image_url(public_url)
        return()
      }
    }

    # No existing image found
    rval_pitcher_image_url(NULL)
  })

  # Auto-load scouting notes when pitcher, team, OR split is selected
  observeEvent(c(input$opp_pitcher, input$opp_team, input$opp_split), {
    req(input$opp_team, input$opp_pitcher, input$opp_split)

    # Reset delete checkboxes and remap dropdowns via JavaScript
    shinyjs::runjs("
      // Uncheck all delete checkboxes
      $('.delete-check').prop('checked', false);
      // Reset Shiny input values for delete checkboxes
      for (var i = 1; i <= 20; i++) {
        Shiny.setInputValue('delete_pitch_' + i, false, {priority: 'event'});
      }
    ")

    # Fetch notes from database (with split for distinct notes per handedness)
    notes <- tryCatch({
      get_scouting_notes(pool, input$opp_pitcher, input$opp_team, input$opp_split)
    }, error = function(e) {
      list(gameplan = "", attack = "", first_pitch = "", hitter_adv = "", two_k = "")
    })

    # Update text area inputs with saved notes
    updateTextAreaInput(session, "notes_gameplan", value = notes$gameplan)
    updateTextAreaInput(session, "notes_attack", value = notes$attack)
    updateTextAreaInput(session, "notes_first_pitch", value = notes$first_pitch)
    updateTextAreaInput(session, "notes_hitter_adv", value = notes$hitter_adv)
    updateTextAreaInput(session, "notes_2k", value = notes$two_k)
    updateTextAreaInput(session, "notes_risp", value = notes$risp %||% "")
    updateTextInput(session, "pitcher_grade", value = notes$pitcher_grade %||% "")
    updateTextInput(session, "out_pitch", value = notes$out_pitch %||% "")

    # Load pitch descriptions (with split)
    descriptions <- get_pitch_descriptions(pool, input$opp_pitcher, input$opp_team, input$opp_split)
    rval_pitch_descriptions(descriptions)

    # Load saved pitch deletions/remaps (with split)
    saved_edits <- get_pitch_edits(pool, input$opp_pitcher, input$opp_team, input$opp_split)
    rval_saved_pitch_edits(saved_edits)

    # Load pitcher stats (shared across splits — IP/ERA/K/BB are pitcher-level, not split-specific)
    stats <- tryCatch({
      get_pitcher_stats(pool, input$opp_pitcher, input$opp_team, "global")
    }, error = function(e) {
      list(ip = "", era = "", k = "", bb = "", baa_lhh = "", baa_rhh = "")
    })
    updateTextInput(session, "stat_ip", value = stats$ip)
    updateTextInput(session, "stat_era", value = stats$era)
    updateTextInput(session, "stat_k", value = stats$k)
    updateTextInput(session, "stat_bb", value = stats$bb)
    updateTextInput(session, "stat_baa_lhh", value = stats$baa_lhh)
    updateTextInput(session, "stat_baa_rhh", value = stats$baa_rhh)
  }, ignoreInit = TRUE)

  # Reactive value to track last save timestamp to avoid duplicate saves
  rval_last_notes_save <- reactiveVal(NULL)

  # Debounced auto-save for notes using reactive timer pattern
  # This creates a reactive that returns the current notes after a 2-second delay
  notes_debounced <- reactive({
    req(input$opp_pitcher, input$opp_team, input$opp_split)

    # Capture current notes values (including split for distinct notes per handedness)
    list(
      gameplan = input$notes_gameplan %||% "",
      attack = input$notes_attack %||% "",
      first_pitch = input$notes_first_pitch %||% "",
      hitter_adv = input$notes_hitter_adv %||% "",
      two_k = input$notes_2k %||% "",
      risp = input$notes_risp %||% "",
      pitcher_grade = input$pitcher_grade %||% "",
      out_pitch = input$out_pitch %||% "",
      pitcher = input$opp_pitcher,
      team = input$opp_team,
      split = input$opp_split
    )
  }) |> debounce(2000)

  # Observer that triggers on debounced notes changes
  observeEvent(notes_debounced(), {
    notes_data <- notes_debounced()
    req(notes_data$pitcher, notes_data$team, notes_data$split)

    # Create unique key for this save (including split)
    save_key <- paste(notes_data$pitcher, notes_data$team, notes_data$split,
                      notes_data$gameplan, notes_data$attack,
                      notes_data$first_pitch, notes_data$hitter_adv,
                      notes_data$two_k, notes_data$risp,
                      notes_data$pitcher_grade, notes_data$out_pitch, sep = "|")

    # Only save if content has changed since last save
    if (is.null(rval_last_notes_save()) || rval_last_notes_save() != save_key) {
      notes_list <- list(
        gameplan = notes_data$gameplan,
        attack = notes_data$attack,
        first_pitch = notes_data$first_pitch,
        hitter_adv = notes_data$hitter_adv,
        two_k = notes_data$two_k,
        risp = notes_data$risp,
        pitcher_grade = notes_data$pitcher_grade,
        out_pitch = notes_data$out_pitch
      )

      save_success <- save_scouting_notes(pool, notes_data$pitcher, notes_data$team, notes_list, notes_data$split)
      if (save_success) {
        rval_last_notes_save(save_key)
        showNotification("Notes auto-saved", type = "message", duration = 1)
      }
    }
  }, ignoreInit = TRUE)

  # Save pitcher stats when Save button clicked
  observeEvent(input$save_pitcher_stats, {
    req(input$opp_pitcher, input$opp_team, input$opp_split)

    stats <- list(
      ip = input$stat_ip %||% "",
      era = input$stat_era %||% "",
      k = input$stat_k %||% "",
      bb = input$stat_bb %||% "",
      baa_lhh = input$stat_baa_lhh %||% "",
      baa_rhh = input$stat_baa_rhh %||% ""
    )

    save_success <- save_pitcher_stats(pool, input$opp_pitcher, input$opp_team, stats, "global")
    if (save_success) {
      showNotification("Pitcher stats saved", type = "message", duration = 2)
    } else {
      showNotification("Failed to save pitcher stats", type = "error")
    }
  })

  # Reactive to store validation summary for remap dropdowns
  rval_validation_summary <- reactive({
    req(input$opp_pitcher, input$opp_dates)
    get_pitch_validation_summary(pool, input$opp_pitcher,
                                  input$opp_dates[1], input$opp_dates[2])
  })

  # Pitch validation table with remap dropdowns and delete checkboxes
  output$pitch_validation_table <- DT::renderDT({
    summary_df <- rval_validation_summary()
    saved_edits <- rval_saved_pitch_edits()

    if (nrow(summary_df) == 0) {
      return(datatable(data.frame(Message = "No pitch data found for this pitcher in the selected date range")))
    }

    # Get unique pitch types for dropdown options
    pitch_types <- summary_df$pitch_type
    saved_deletions <- saved_edits$deletions
    saved_remaps <- saved_edits$remaps

    # Build HTML select options
    build_options <- function(choices, selected) {
      opts <- sapply(choices, function(ch) {
        sel <- if (ch == selected) ' selected' else ''
        sprintf('<option value="%s"%s>%s</option>', ch, sel, ch)
      })
      paste(opts, collapse = "")
    }

    # Add remap dropdown column using plain HTML (pre-select saved remap if exists)
    summary_df$remap_to <- sapply(seq_len(nrow(summary_df)), function(i) {
      pt <- summary_df$pitch_type[i]
      selected_remap <- if (!is.null(saved_remaps[[pt]])) saved_remaps[[pt]] else pt
      sprintf(
        '<select id="remap_%d" class="remap-select" style="width:120px;" onchange="Shiny.setInputValue(\'remap_%d\', this.value, {priority: \'event\'})">%s</select>',
        i, i, build_options(pitch_types, selected_remap)
      )
    })

    # Add delete checkbox column using plain HTML (pre-check if pitch was saved as deleted)
    summary_df$delete <- sapply(seq_len(nrow(summary_df)), function(i) {
      pt <- summary_df$pitch_type[i]
      checked <- if (pt %in% saved_deletions) 'checked="checked"' else ''
      sprintf(
        '<input type="checkbox" id="delete_pitch_%d" class="delete-check" %s onchange="Shiny.setInputValue(\'delete_pitch_%d\', this.checked, {priority: \'event\'})">',
        i, checked, i
      )
    })

    datatable(
      summary_df,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $('.delete-check:checked').each(function() {",
          "    Shiny.setInputValue(this.id, true, {priority: 'event'});",
          "  });",
          "  $('.remap-select').each(function() {",
          "    Shiny.setInputValue(this.id, this.value, {priority: 'event'});",
          "  });",
          "}"
        )
      ),
      colnames = c("Pitch Type", "Count", "Avg IVB", "Avg HB", "Avg Velo", "Remap To", "Delete")
    )
  }, server = FALSE)

  # Reactive to collect remap selections
  rval_pitch_remap <- reactive({
    req(input$apply_remap)

    summary_df <- rval_validation_summary()
    if (nrow(summary_df) == 0) return(NULL)

    # Build remap lookup from inputs
    remap <- setNames(
      sapply(seq_len(nrow(summary_df)), function(i) {
        val <- input[[paste0("remap_", i)]]
        if (is.null(val) || val == "") summary_df$pitch_type[i] else val
      }),
      summary_df$pitch_type
    )
    remap
  })

  # Reactive to collect deleted pitch types
  rval_pitch_deletions <- reactive({
    req(input$apply_remap)

    summary_df <- rval_validation_summary()
    if (nrow(summary_df) == 0) return(character(0))

    # Collect pitch types marked for deletion
    deleted <- sapply(seq_len(nrow(summary_df)), function(i) {
      is_deleted <- input[[paste0("delete_pitch_", i)]]
      if (!is.null(is_deleted) && isTRUE(is_deleted)) {
        summary_df$pitch_type[i]
      } else {
        NA_character_
      }
    })

    # Return only non-NA (deleted) pitch types
    deleted[!is.na(deleted)]
  })

  # Show Step 2 preview when Apply is clicked
  observeEvent(input$apply_remap, {
    shinyjs::show("step2_preview")
  })

  # Hide Step 2 preview when split changes (force re-apply)
  observeEvent(input$opp_split, {
    shinyjs::hide("step2_preview")
  }, ignoreInit = TRUE)

  # Main reactive data for scouting report (with deletions and remapping applied)
  rval_scout_data <- eventReactive(input$apply_remap, {
    req(input$opp_pitcher, input$opp_dates)

    withProgress(message = 'Loading scouting data...', value = 0, {

      # Get remap lookup and deletions
      incProgress(0.1, detail = "Getting pitch mappings")
      remap <- rval_pitch_remap()
      deletions <- rval_pitch_deletions()

      # Fetch raw pitch data
      incProgress(0.2, detail = "Fetching pitch data")
      raw_df <- get_pitcher_data(pool, input$opp_pitcher,
                                  input$opp_dates[1], input$opp_dates[2])

      if (nrow(raw_df) == 0) return(NULL)

      # Step 1: Filter out deleted pitch types FIRST
      incProgress(0.2, detail = "Applying deletions")
      if (length(deletions) > 0) {
        raw_df <- raw_df |> filter(!taggedpitchtype %in% deletions)
      }

      if (nrow(raw_df) == 0) return(NULL)

      # Step 2: Apply pitch type remapping (vectorized approach)
      incProgress(0.2, detail = "Applying remapping")
      if (!is.null(remap)) {
        raw_df$pitch_type_display <- remap[raw_df$taggedpitchtype]
        # Fill in any NAs with original pitch type
        raw_df$pitch_type_display[is.na(raw_df$pitch_type_display)] <-
          raw_df$taggedpitchtype[is.na(raw_df$pitch_type_display)]
      } else {
        raw_df$pitch_type_display <- raw_df$taggedpitchtype
      }

      # Step 3: Compute arsenal summary using remapped types (from filtered data)
      incProgress(0.2, detail = "Computing arsenal summary")
      arsenal <- compute_arsenal_summary(raw_df, "pitch_type_display")

      # Step 4: Save scouting notes to database (with split for distinct notes per handedness)
      incProgress(0.05, detail = "Saving notes")
      notes_list <- list(
        gameplan = input$notes_gameplan,
        attack = input$notes_attack,
        first_pitch = input$notes_first_pitch,
        hitter_adv = input$notes_hitter_adv,
        two_k = input$notes_2k,
        risp = input$notes_risp,
        pitcher_grade = input$pitcher_grade %||% "",
        out_pitch = input$out_pitch %||% ""
      )
      save_success <- save_scouting_notes(pool, input$opp_pitcher, input$opp_team, notes_list, input$opp_split)
      if (save_success) {
        showNotification("Notes saved", type = "message", duration = 2)
      }

      # Save pitch deletions and remaps
      save_pitch_edits(pool, input$opp_pitcher, input$opp_team, deletions, as.list(remap), input$opp_split)

      incProgress(0.05, detail = "Done!")
    })

    result <- list(
      raw = raw_df,
      arsenal = arsenal,
      remap = remap,
      deletions = deletions,
      pitcher = input$opp_pitcher,
      team = input$opp_team,
      split = input$opp_split
    )

    # Update processed data reactive for description inputs
    rval_processed_data(result)

    # Load saved velocity overrides for this pitcher/team/split
    overrides <- get_velo_overrides(pool, input$opp_pitcher, input$opp_team, input$opp_split)
    rval_velo_overrides(overrides)

    result
  })

  # Filter data by batter side and ensure remapped pitch types are used
  rval_scout_filtered <- reactive({
    data <- rval_scout_data()
    req(data)

    df <- data$raw

    # Filter out rows with NA pitch_type_display (after remapping)
    df <- df |> filter(!is.na(pitch_type_display))

    if (input$opp_split != "Both") {
      df <- df |> filter(batterside == input$opp_split)
    }

    df
  })

  # Handle pitcher image upload to Supabase
  observeEvent(input$pitcher_image, {
    req(input$pitcher_image)
    req(input$opp_team, input$opp_pitcher)

    file <- input$pitcher_image
    file_ext <- tools::file_ext(file$name)

    # Supabase Storage credentials
    supabase_url <- Sys.getenv("SUPABASE_URL")
    supabase_key <- Sys.getenv("SUPABASE_ANON_KEY")
    bucket_name <- "scouting-images"

    if (supabase_url == "" || supabase_key == "") {
      showNotification("Supabase credentials not configured. Set SUPABASE_URL and SUPABASE_ANON_KEY.", type = "error")
      return()
    }

    # Create file path: {team}/{pitcher_name}.{ext}
    # Sanitize names for file path
    safe_team <- gsub("[^A-Za-z0-9_-]", "_", input$opp_team)
    safe_pitcher <- gsub("[^A-Za-z0-9_-]", "_", input$opp_pitcher)
    storage_path <- paste0(safe_team, "/", safe_pitcher, ".", file_ext)

    # Upload to Supabase Storage
    upload_url <- paste0(supabase_url, "/storage/v1/object/", bucket_name, "/", storage_path)

    withProgress(message = 'Uploading image...', value = 0.5, {
      response <- tryCatch({
        httr::PUT(
          upload_url,
          httr::add_headers(
            Authorization = paste("Bearer", supabase_key),
            `Content-Type` = file$type,
            `x-upsert` = "true"
          ),
          body = httr::upload_file(file$datapath)
        )
      }, error = function(e) {
        showNotification(paste("Upload error:", e$message), type = "error")
        NULL
      })

      if (!is.null(response)) {
        if (httr::status_code(response) %in% c(200, 201)) {
          # Construct public URL
          public_url <- paste0(supabase_url, "/storage/v1/object/public/", bucket_name, "/", storage_path)
          rval_pitcher_image_url(public_url)
          showNotification("Image uploaded successfully!", type = "message")
        } else {
          error_content <- httr::content(response, as = "text", encoding = "UTF-8")
          showNotification(paste("Upload failed:", error_content), type = "error")
        }
      }
    })
  })

  # Active-split banner
  output$active_split_banner <- renderUI({
    split_label <- if (input$opp_split == "Left") "vs LHH" else "vs RHH"
    split_color <- if (input$opp_split == "Left") "#2563eb" else "#dc2626"
    tags$div(
      style = paste0("background:", split_color, "; color:white; border-radius:4px; padding:6px 12px; margin-bottom:8px; font-weight:bold; font-size:13px;"),
      paste0("Editing notes: ", split_label)
    )
  })

  # Preview uploaded image in sidebar
  output$pitcher_image_preview <- renderUI({
    img_url <- rval_pitcher_image_url()
    if (!is.null(img_url)) {
      tags$div(
        style = "margin-top: 10px;",
        tags$img(src = img_url, width = "100%", style = "border-radius: 4px;"),
        tags$small(class = "text-muted", "Image uploaded")
      )
    }
  })

  # Pitcher info box
  output$pitcher_info_box <- renderUI({
    data <- rval_scout_data()
    req(data)

    df <- data$raw
    pitcher_name <- data$pitcher
    team <- data$team

    # Calculate average extension and release height
    avg_ext <- mean(df$extension, na.rm = TRUE)
    avg_rel_height <- mean(df$relheight, na.rm = TRUE)

    # Get pitcher image URL if available
    img_url <- rval_pitcher_image_url()

    tagList(
      # Show pitcher image if uploaded
      if (!is.null(img_url)) {
        tags$div(
          style = "text-align: center; margin-bottom: 10px;",
          tags$img(src = img_url, width = "160px", height = "200px",
                   style = "border-radius: 8px; object-fit: cover; border: 2px solid #1a365d;")
        )
      },
      h4(pitcher_name),
      p(strong("Team: "), team),
      {
        ext_col <- get_mech_color(avg_ext)
        rel_col <- get_mech_color(avg_rel_height)
        tagList(
          tags$div(
            style = paste0("background:", ext_col$bg, "; color:", ext_col$text,
                           "; border-radius:4px; padding:5px 8px; margin-bottom:4px;"),
            tags$strong("Extension: "), format_feet_inches(avg_ext)
          ),
          tags$div(
            style = paste0("background:", rel_col$bg, "; color:", rel_col$text,
                           "; border-radius:4px; padding:5px 8px; margin-bottom:4px;"),
            tags$strong("Release Height: "), format_feet_inches(avg_rel_height)
          )
        )
      },
      hr(),
      plotOutput("release_plot_output", height = "250px")
    )
  })

  # Release plot
  output$release_plot_output <- renderPlot({
    df <- rval_scout_filtered()
    req(df, nrow(df) > 0)
    generate_release_plot(df)
  })

  # Arsenal table - recompute from filtered data for correct usage percentages
  output$arsenal_table <- DT::renderDT({
    df <- rval_scout_filtered()
    req(df, nrow(df) > 0)

    # Usage/zone% from split-filtered data; velo/IVB/HB from combined (both splits)
    data <- rval_scout_data()
    movement_df <- if (!is.null(data)) data$raw else NULL
    arsenal <- compute_arsenal_summary(df, "pitch_type_display", movement_df = movement_df)

    # Get existing pitch descriptions (ensure it's a proper list)
    descriptions <- rval_pitch_descriptions()
    if (is.null(descriptions) || length(descriptions) == 0) {
      descriptions <- list()
    } else if (!is.list(descriptions)) {
      descriptions <- as.list(descriptions)
    }

    # Apply any saved velocity overrides before display
    overrides <- rval_velo_overrides()
    arsenal <- apply_velo_overrides(arsenal, overrides)

    # Keep zone_pct numeric for conditional formatting, add display column
    arsenal <- arsenal |>
      mutate(
        usage_display = paste0(usage, "%"),
        zone_pct_display = paste0(zone_pct, "%")
      )

    # Make velo column clickable — overridden cells shown in blue
    arsenal$velo_display <- sapply(seq_len(nrow(arsenal)), function(i) {
      pt  <- arsenal$pitch_type[i]
      val <- arsenal$velo[i]
      is_overridden <- !is.null(overrides[[pt]])
      color <- if (is_overridden) "#1d4ed8" else "#374151"
      title <- if (is_overridden) "Click to edit (override active)" else "Click to edit velocity"
      sprintf(
        '<span style="cursor:pointer; color:%s; font-weight:%s; border-bottom:1px dashed %s;" title="%s" onclick="Shiny.setInputValue(\'velo_edit_row\', %d, {priority:\'event\'})">%s</span>',
        color, if (is_overridden) "bold" else "normal", color, title, i, htmltools::htmlEscape(val)
      )
    })

    # Add Notes column with inline text inputs
    desc_names <- names(descriptions)
    arsenal$notes <- sapply(seq_len(nrow(arsenal)), function(i) {
      pt <- arsenal$pitch_type[i]
      existing <- ""
      if (!is.null(desc_names) && pt %in% desc_names) {
        val <- descriptions[[pt]]
        if (!is.null(val) && length(val) > 0) existing <- as.character(val[1])
      }
      sprintf(
        '<input type="text" id="pitch_desc_%d" class="pitch-desc-input" value="%s" placeholder="Add note..." style="width:150px; padding:2px 5px; font-size:12px; border:1px solid #ccc; border-radius:3px;" onchange="Shiny.setInputValue(\'pitch_desc_%d\', this.value, {priority: \'event\'})">',
        i, htmltools::htmlEscape(existing), i
      )
    })

    arsenal <- arsenal |>
      select(pitch_type, count, usage_display, velo_display, zone_pct_display, zone_pct, ivb, hb, notes) |>
      rename(
        `Pitch Type` = pitch_type,
        `#` = count,
        `Usage` = usage_display,
        `Velo` = velo_display,
        `Zone%` = zone_pct_display,
        `zone_pct_num` = zone_pct,
        `IVB` = ivb,
        `HB` = hb,
        `Notes` = notes
      )

    datatable(
      arsenal,
      rownames = FALSE,
      escape = FALSE,
      selection = 'none',
      class = 'cell-border stripe',
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = 10,
        columnDefs = list(
          list(className = 'dt-center', targets = 0:7),
          list(className = 'dt-left', targets = 8),
          list(visible = FALSE, targets = 5)  # Hide zone_pct_num column (0-indexed)
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().container()).css({'font-size': '14px'});",
          "  $(this.api().table().header()).css({'font-size': '14px'});",
          "}"
        )
      )
    ) |>
      formatStyle(
        columns = c('Pitch Type', '#', 'Usage', 'Velo', 'Zone%', 'IVB', 'HB'),
        fontSize = '14px',
        lineHeight = '1.5'
      ) |>
      formatStyle(
        'Zone%',
        valueColumns = 'zone_pct_num',
        backgroundColor = styleInterval(39, c('rgba(255, 200, 200, 0.8)', 'transparent')),
        color = styleInterval(39, c('darkred', 'inherit')),
        fontWeight = styleInterval(39, c('bold', 'normal'))
      )
  }, server = FALSE)

  # SLG Heatmap output
  output$slg_heatmap <- renderPlot({
    req(input$apply_remap > 0)
    df <- rval_scout_filtered()
    req(df, nrow(df) > 0)
    generate_slg_heatmap(df, title = "SLG by Zone")
  })

  # Whiff Rate Heatmap output
  output$whiff_heatmap <- renderPlot({
    req(input$apply_remap > 0)
    df <- rval_scout_filtered()
    req(df, nrow(df) > 0)
    generate_whiff_heatmap(df, title = "Whiff % by Zone")
  })

  # Zone % display
  output$zone_pct_display <- renderUI({
    req(input$apply_remap > 0)
    df <- rval_scout_filtered()
    req(df, nrow(df) > 0)

    zone_pct <- calculate_zone_pct(df)

    tags$div(
      style = "background: #f7fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 15px; text-align: center; margin-top: 10px;",
      tags$h4(style = "margin: 0 0 5px 0; color: #2d3748;", "Overall Zone %"),
      tags$span(
        style = paste0(
          "font-size: 28px; font-weight: bold; color: ",
          if (!is.na(zone_pct) && zone_pct < 40) "darkred;" else "#2d3748;"
        ),
        paste0(zone_pct, "%")
      )
    )
  })

  # Pitch description inputs are now inline in the arsenal table
  # The inputs use IDs pitch_desc_1, pitch_desc_2, etc.

  # Track last saved descriptions to avoid duplicate saves
  rval_last_desc_save <- reactiveVal(NULL)

  # Reactive to collect all pitch descriptions
  all_pitch_descriptions <- reactive({
    data <- rval_processed_data()
    if (is.null(data) || is.null(data$arsenal)) return(NULL)

    descriptions <- list()
    for (i in seq_len(nrow(data$arsenal))) {
      pt <- data$arsenal$pitch_type[i]
      val <- input[[paste0("pitch_desc_", i)]]
      if (!is.null(val)) {
        descriptions[[pt]] <- val
      }
    }

    list(
      descriptions = descriptions,
      pitcher = input$opp_pitcher,
      team = input$opp_team,
      split = input$opp_split
    )
  }) |> debounce(2000)

  # Observer that triggers on debounced description changes
  observeEvent(all_pitch_descriptions(), {
    desc_data <- all_pitch_descriptions()
    req(desc_data, desc_data$pitcher, desc_data$team, desc_data$split)

    # Filter to non-empty descriptions
    descriptions <- desc_data$descriptions
    if (is.list(descriptions) && length(descriptions) > 0) {
      # Filter to non-empty values
      keep_idx <- sapply(descriptions, function(x) !is.null(x) && length(x) > 0 && x != "")
      if (is.logical(keep_idx)) {
        descriptions <- descriptions[keep_idx]
      }
    } else if (!is.list(descriptions)) {
      descriptions <- list()
    }

    # Create unique key for this save (including split)
    desc_str <- if (length(descriptions) > 0) {
      paste(names(descriptions), unlist(descriptions), collapse = "|")
    } else {
      ""
    }
    save_key <- paste(desc_data$pitcher, desc_data$team, desc_data$split, desc_str, sep = "||")

    # Only save if content has changed since last save
    if (is.null(rval_last_desc_save()) || rval_last_desc_save() != save_key) {
      if (length(descriptions) > 0) {
        save_pitch_descriptions(pool, desc_data$pitcher, desc_data$team, descriptions, desc_data$split)
        rval_pitch_descriptions(descriptions)
        rval_last_desc_save(save_key)
        showNotification("Pitch descriptions auto-saved", type = "message", duration = 1)
      }
    }
  }, ignoreInit = TRUE)

  # ---- Velocity override modal ----

  # Track which row is being edited and its pitch type
  rval_velo_edit_row <- reactiveVal(NULL)

  observeEvent(input$velo_edit_row, {
    i <- input$velo_edit_row
    data <- rval_processed_data()
    req(data, i, i >= 1, i <= nrow(data$arsenal))

    pt       <- data$arsenal$pitch_type[i]
    overrides <- rval_velo_overrides()
    ov        <- overrides[[pt]]

    # Pre-fill with existing override, or parse auto-calculated velo
    auto_velo <- data$arsenal$velo[i]  # e.g. "92-95 (96)" or "84-87"
    parsed <- regmatches(auto_velo, regexpr("^(\\d+)-(\\d+)(?:\\s*\\((\\d+)\\))?", auto_velo, perl = TRUE))
    parts  <- regmatches(auto_velo, regexec("^(\\d+)-(\\d+)(?:\\s*\\((\\d+)\\))?", auto_velo, perl = TRUE))[[1]]

    default_min  <- if (!is.null(ov$min))  ov$min  else if (length(parts) >= 2) as.integer(parts[2]) else NA
    default_max  <- if (!is.null(ov$max))  ov$max  else if (length(parts) >= 3) as.integer(parts[3]) else NA
    default_peak <- if (!is.null(ov$peak)) ov$peak else if (length(parts) >= 4 && parts[4] != "") as.integer(parts[4]) else NA

    rval_velo_edit_row(list(i = i, pt = pt))

    showModal(modalDialog(
      title = tags$span(
        style = "font-size:15px; font-weight:bold;",
        paste0("Edit Velocity — ", pt)
      ),
      size = "s",
      tags$p(
        style = "font-size:12px; color:#6b7280; margin-bottom:10px;",
        "Range = 10th-90th percentile. Peak = max (shown in parentheses for fastballs only)."
      ),
      fluidRow(
        column(4, numericInput("velo_edit_min",  "Range Min", value = default_min,  min = 50, max = 110, step = 1)),
        column(4, numericInput("velo_edit_max",  "Range Max", value = default_max,  min = 50, max = 110, step = 1)),
        column(4, numericInput("velo_edit_peak", "Peak Max",  value = default_peak, min = 50, max = 110, step = 1))
      ),
      footer = tagList(
        actionButton("velo_save",  "Save",         class = "btn-primary btn-sm"),
        actionButton("velo_reset", "Reset to Auto", class = "btn-warning btn-sm"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$velo_save, {
    req(input$velo_edit_min, input$velo_edit_max)
    ctx <- rval_velo_edit_row()
    req(ctx)

    overrides       <- rval_velo_overrides()
    overrides[[ctx$pt]] <- list(
      min  = as.integer(input$velo_edit_min),
      max  = as.integer(input$velo_edit_max),
      peak = if (!is.na(input$velo_edit_peak)) as.integer(input$velo_edit_peak) else NA
    )
    rval_velo_overrides(overrides)

    save_velo_overrides(pool, input$opp_pitcher, input$opp_team, overrides, input$opp_split)
    showNotification(paste0("Velocity override saved for ", ctx$pt), type = "message", duration = 2)
    removeModal()
  })

  observeEvent(input$velo_reset, {
    ctx <- rval_velo_edit_row()
    req(ctx)

    overrides <- rval_velo_overrides()
    overrides[[ctx$pt]] <- NULL
    rval_velo_overrides(overrides)

    save_velo_overrides(pool, input$opp_pitcher, input$opp_team, overrides, input$opp_split)
    showNotification(paste0("Velocity reset to auto for ", ctx$pt), type = "message", duration = 2)
    removeModal()
  })

  # ---- End velocity override modal ----

  # Shared reactive for RISP images — avoids 4x identical DB queries per render cycle
  rval_risp_images <- reactive({
    req(input$opp_pitcher, input$opp_team, input$opp_split)
    get_risp_images(pool, input$opp_pitcher, input$opp_team, input$opp_split)
  })

  # Helper function to create individual RISP slot UI
  create_risp_slot <- function(slot_num) {
    renderUI({
      data <- rval_processed_data()
      req(data, input$opp_split)
      arsenal <- data$arsenal

      if (is.null(arsenal) || nrow(arsenal) < slot_num) {
        return(tags$div(style = "height: 180px;"))  # Empty placeholder
      }

      pt <- arsenal$pitch_type[slot_num]

      # Load existing RISP images (shared reactive — 1 query per flush, not 4)
      risp_images <- rval_risp_images()
      if (!is.list(risp_images)) risp_images <- as.list(risp_images)

      # Safely extract URL
      existing_url <- NULL
      if (pt %in% names(risp_images)) {
        val <- risp_images[[pt]]
        if (!is.null(val) && length(val) > 0) existing_url <- as.character(val[1])
      }

      tags$div(
        style = "text-align: center; height: 180px;",
        tags$strong(style = "font-size: 12px; display: block; margin-bottom: 5px;", pt),
        if (!is.null(existing_url) && existing_url != "") {
          tags$div(
            tags$img(src = existing_url, style = "width: 100%; max-height: 120px; object-fit: contain; border-radius: 4px; border: 1px solid #ddd;")
          )
        } else {
          tags$div(
            style = "height: 100px; display: flex; align-items: center; justify-content: center; background: #f5f5f5; border: 1px dashed #ccc; border-radius: 4px;",
            tags$small(style = "color: #888;", "No image")
          )
        },
        fileInput(paste0("risp_upload_", slot_num), NULL, accept = c("image/png", "image/jpeg"), width = "100%")
      )
    })
  }

  # Create individual RISP slot outputs (matching the 4 heatmap columns)
  output$risp_slot_1 <- create_risp_slot(1)
  output$risp_slot_2 <- create_risp_slot(2)
  output$risp_slot_3 <- create_risp_slot(3)
  output$risp_slot_4 <- create_risp_slot(4)

  # Reactive value for RISP usages
  rval_risp_usages <- reactiveVal(list())

  # RISP Usage inputs UI
  output$risp_usage_inputs <- renderUI({
    data <- rval_processed_data()
    req(data, input$opp_split)
    arsenal <- data$arsenal

    if (is.null(arsenal) || nrow(arsenal) == 0) return(NULL)

    pitch_types <- head(arsenal$pitch_type, 4)

    # Load existing RISP usages
    risp_usages <- get_risp_usages(pool, input$opp_pitcher, input$opp_team, input$opp_split)
    if (!is.list(risp_usages)) risp_usages <- list()
    rval_risp_usages(risp_usages)

    tags$div(
      style = "display: flex; gap: 10px; margin-top: 10px; padding: 10px; background: #f8f9fa; border-radius: 4px;",
      tags$strong(style = "margin-right: 10px; line-height: 32px;", "RISP Usage %:"),
      lapply(seq_along(pitch_types), function(i) {
        pt <- pitch_types[i]
        existing_val <- if (pt %in% names(risp_usages)) as.numeric(risp_usages[[pt]]) else NA
        colors <- get_pitch_color(pt)

        tags$div(
          style = "display: flex; align-items: center; gap: 5px;",
          tags$span(
            style = sprintf("background: %s; color: %s; padding: 2px 8px; border-radius: 3px; font-weight: bold; font-size: 11px;",
                            colors$bg, colors$text),
            pt
          ),
          tags$input(
            type = "number",
            id = paste0("risp_usage_", i),
            class = "form-control",
            style = "width: 60px; padding: 4px; font-size: 12px;",
            min = 0, max = 100,
            value = if (!is.na(existing_val)) existing_val else "",
            placeholder = "%",
            onchange = sprintf("Shiny.setInputValue('risp_usage_%d', this.value, {priority: 'event'})", i)
          )
        )
      }),
      actionButton("save_risp_usages", "Save", class = "btn-sm btn-primary", style = "margin-left: 10px;")
    )
  })

  # Save RISP usages when button clicked
  observeEvent(input$save_risp_usages, {
    data <- rval_processed_data()
    req(data, input$opp_pitcher, input$opp_team, input$opp_split)
    arsenal <- data$arsenal

    if (is.null(arsenal) || nrow(arsenal) == 0) return()

    pitch_types <- head(arsenal$pitch_type, 4)

    # Collect usage values
    usages <- list()
    for (i in seq_along(pitch_types)) {
      pt <- pitch_types[i]
      val <- input[[paste0("risp_usage_", i)]]
      if (!is.null(val) && val != "") {
        usages[[pt]] <- as.numeric(val)
      }
    }

    # Save to database
    if (length(usages) > 0) {
      save_risp_usages(pool, input$opp_pitcher, input$opp_team, usages, input$opp_split)
      rval_risp_usages(usages)
      showNotification("RISP usages saved!", type = "message", duration = 2)
    }
  })

  # Handle RISP image uploads
  observe({
    data <- rval_processed_data()
    if (is.null(data) || is.null(data$arsenal)) return()

    for (i in seq_len(nrow(data$arsenal))) {
      local({
        idx <- i
        pt <- data$arsenal$pitch_type[idx]

        observeEvent(input[[paste0("risp_upload_", idx)]], {
          file <- input[[paste0("risp_upload_", idx)]]
          if (!is.null(file)) {
            # Upload to Supabase storage
            supabase_url <- Sys.getenv("SUPABASE_URL")
            supabase_key <- Sys.getenv("SUPABASE_ANON_KEY")
            bucket_name <- "risp-heatmaps"

            if (supabase_url == "" || supabase_key == "") {
              showNotification("Supabase credentials not configured.", type = "error")
              return()
            }

            # Include split in storage path for distinct images per handedness
            file_ext <- tools::file_ext(file$name)
            safe_team <- gsub("[^A-Za-z0-9_-]", "_", input$opp_team)
            safe_pitcher <- gsub("[^A-Za-z0-9_-]", "_", input$opp_pitcher)
            safe_split <- gsub("[^A-Za-z0-9_-]", "_", input$opp_split)
            safe_pitch <- gsub("[^A-Za-z0-9_-]", "_", pt)
            storage_path <- paste0(safe_team, "/", safe_pitcher, "/", safe_split, "/", safe_pitch, ".", file_ext)

            upload_url <- paste0(supabase_url, "/storage/v1/object/", bucket_name, "/", storage_path)

            withProgress(message = paste0("Uploading ", pt, " RISP image..."), value = 0.5, {
              response <- tryCatch({
                httr::PUT(
                  upload_url,
                  httr::add_headers(
                    Authorization = paste("Bearer", supabase_key),
                    `Content-Type` = file$type,
                    `x-upsert` = "true"
                  ),
                  body = httr::upload_file(file$datapath)
                )
              }, error = function(e) {
                showNotification(paste("Upload error:", e$message), type = "error")
                NULL
              })

              if (!is.null(response) && httr::status_code(response) %in% c(200, 201)) {
                public_url <- paste0(supabase_url, "/storage/v1/object/public/", bucket_name, "/", storage_path)

                # Save URL to database (with split)
                risp_images <- get_risp_images(pool, input$opp_pitcher, input$opp_team, input$opp_split)
                if (!is.list(risp_images)) risp_images <- as.list(risp_images)
                risp_images[[pt]] <- public_url
                save_risp_images(pool, input$opp_pitcher, input$opp_team, risp_images, input$opp_split)

                showNotification(paste(pt, "RISP image uploaded!"), type = "message")
              } else if (!is.null(response)) {
                error_content <- httr::content(response, as = "text", encoding = "UTF-8")
                showNotification(paste("Upload failed:", error_content), type = "error")
              }
            })
          }
        }, ignoreInit = TRUE)
      })
    }
  })

  # Helper to filter data by count
  filter_by_count <- function(df, count_filter) {
    if (is.null(count_filter)) return(df)
    switch(count_filter,
      "first_pitch" = df |> filter(balls == 0 & strikes == 0),
      "hitter_advantage" = df |> filter(
        (balls == 1 & strikes == 0) |
        (balls == 2 & strikes %in% c(0, 1)) |
        (balls == 3 & strikes %in% c(0, 1))
      ),
      "2k" = df |> filter(strikes == 2),
      df
    )
  }

  # Helper to get pitch usage for a filtered dataset
  get_pitch_usage <- function(df) {
    df |>
      count(pitch_type_display) |>
      mutate(
        pct = round(n / sum(n) * 100, 0),
        is_fb = pitch_type_display %in% FASTBALL_TYPES | grepl("fastball|sinker", pitch_type_display, ignore.case = TRUE)
      ) |>
      arrange(desc(is_fb), desc(n)) |>
      select(-is_fb)
  }

  # Canonical pitch order from the full split-filtered data — matches the arsenal table
  rval_canonical_order <- reactive({
    data <- rval_scout_data()
    req(data)
    data$arsenal$pitch_type
  })

  # Create a reactive for each heatmap section's data
  rval_overall_data <- reactive({
    df <- rval_scout_filtered()
    req(df, nrow(df) > 0)
    list(df = df, usage = get_pitch_usage(df))
  })

  rval_first_data <- reactive({
    df <- rval_scout_filtered()
    req(df, nrow(df) > 0)
    filtered <- filter_by_count(df, "first_pitch")
    if (nrow(filtered) < 3) return(NULL)
    list(df = filtered, usage = get_pitch_usage(filtered))
  })

  rval_adv_data <- reactive({
    df <- rval_scout_filtered()
    req(df, nrow(df) > 0)
    filtered <- filter_by_count(df, "hitter_advantage")
    if (nrow(filtered) < 3) return(NULL)
    list(df = filtered, usage = get_pitch_usage(filtered))
  })

  rval_2k_data <- reactive({
    df <- rval_scout_filtered()
    req(df, nrow(df) > 0)
    filtered <- filter_by_count(df, "2k")
    if (nrow(filtered) < 3) return(NULL)
    list(df = filtered, usage = get_pitch_usage(filtered))
  })

  # Helper: generate heatmap at canonical position idx, with situation-specific pct label
  safe_heatmap <- function(data, idx, canonical_order) {
    tryCatch({
      if (is.null(data) || is.null(canonical_order) || idx > length(canonical_order)) return(NULL)
      pt <- canonical_order[idx]
      usage_row <- data$usage[data$usage$pitch_type_display == pt, ]
      if (nrow(usage_row) == 0) return(NULL)
      pct <- usage_row$pct[1]
      generate_heatmap(data$df, pitch_type = pt, title = paste0(pt, " (", pct, "%)"))
    }, error = function(e) {
      message("Heatmap error: ", e$message)
      NULL
    })
  }

  # Overall heatmaps (4 fixed outputs)
  output$heat_overall_1 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_overall_data(), 1, rval_canonical_order())
  })
  output$heat_overall_2 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_overall_data(), 2, rval_canonical_order())
  })
  output$heat_overall_3 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_overall_data(), 3, rval_canonical_order())
  })
  output$heat_overall_4 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_overall_data(), 4, rval_canonical_order())
  })

  # First pitch heatmaps
  output$heat_first_1 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_first_data(), 1, rval_canonical_order())
  })
  output$heat_first_2 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_first_data(), 2, rval_canonical_order())
  })
  output$heat_first_3 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_first_data(), 3, rval_canonical_order())
  })
  output$heat_first_4 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_first_data(), 4, rval_canonical_order())
  })

  # Hitter advantage heatmaps
  output$heat_adv_1 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_adv_data(), 1, rval_canonical_order())
  })
  output$heat_adv_2 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_adv_data(), 2, rval_canonical_order())
  })
  output$heat_adv_3 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_adv_data(), 3, rval_canonical_order())
  })
  output$heat_adv_4 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_adv_data(), 4, rval_canonical_order())
  })

  # 2K heatmaps
  output$heat_2k_1 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_2k_data(), 1, rval_canonical_order())
  })
  output$heat_2k_2 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_2k_data(), 2, rval_canonical_order())
  })
  output$heat_2k_3 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_2k_data(), 3, rval_canonical_order())
  })
  output$heat_2k_4 <- renderPlot({
    req(input$apply_remap > 0)
    safe_heatmap(rval_2k_data(), 4, rval_canonical_order())
  })

  # HTML Report Download handler (print to PDF from browser)
  output$download_report <- downloadHandler(
    filename = function() {
      # Parse "Last, First" → "Last_First"
      pitcher_raw <- input$opp_pitcher
      parts <- strsplit(pitcher_raw, ",\\s*")[[1]]
      if (length(parts) == 2) {
        pitcher_name <- paste0(trimws(parts[1]), "_", trimws(parts[2]))
      } else {
        pitcher_name <- gsub("[^A-Za-z0-9]", "_", pitcher_raw)
      }
      split_label <- if (input$opp_split == "Left") "LHH" else "RHH"
      paste0(pitcher_name, "_", split_label, "_report.html")
    },
    content = function(file) {
      # Create a temporary directory for rendering
      temp_dir <- tempdir()
      temp_rmd <- file.path(temp_dir, "report_template.Rmd")

      # Copy the template
      file.copy("report_template.Rmd", temp_rmd, overwrite = TRUE)

      # Get data
      data <- rval_scout_data()
      df <- rval_scout_filtered()

      # Usage/zone% from split-filtered data; velo/IVB/HB from both splits combined
      arsenal_filtered <- compute_arsenal_summary(df, "pitch_type_display", movement_df = data$raw)

      # Apply any velocity overrides before passing to report
      arsenal_filtered <- apply_velo_overrides(arsenal_filtered, rval_velo_overrides())

      # Get pitcher image URL
      img_url <- rval_pitcher_image_url()

      # Get pitch descriptions, RISP images, RISP usages, and pitcher stats (with split for distinct per handedness)
      pitch_descs <- get_pitch_descriptions(pool, input$opp_pitcher, input$opp_team, input$opp_split)
      risp_imgs <- get_risp_images(pool, input$opp_pitcher, input$opp_team, input$opp_split)
      risp_usage <- get_risp_usages(pool, input$opp_pitcher, input$opp_team, input$opp_split)
      pitcher_stats <- get_pitcher_stats(pool, input$opp_pitcher, input$opp_team, "global")

      # Render the report as HTML
      rmarkdown::render(
        temp_rmd,
        output_file = file,
        output_format = "html_document",
        params = list(
          pitcher = input$opp_pitcher,
          team = input$opp_team,
          dates = input$opp_dates,
          split = input$opp_split,
          arsenal = arsenal_filtered,
          pitch_data = df,
          pitcher_image = img_url,
          notes = list(
            gameplan = input$notes_gameplan,
            attack = input$notes_attack,
            first_pitch = input$notes_first_pitch,
            hitter_adv = input$notes_hitter_adv,
            two_k = input$notes_2k,
            risp = input$notes_risp
          ),
          pitch_descriptions = pitch_descs,
          risp_images = risp_imgs,
          risp_usages = risp_usage,
          pitcher_stats = pitcher_stats,
          grade = input$pitcher_grade %||% "",
          out_pitch = input$out_pitch %||% ""
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )

  # Batch team report download — one HTML file with all pitchers (RHH + LHH)
  output$download_team_reports <- downloadHandler(
    filename = function() {
      paste0(input$opp_team, "_reports.html")
    },
    content = function(file) {
      req(input$opp_team, input$opp_dates)

      withProgress(message = paste0("Generating ", input$opp_team, " reports..."), value = 0, {

        pitchers <- get_team_pitchers(pool, input$opp_team)
        if (length(pitchers) == 0) {
          cat("<html><body><p>No pitchers found for team: ", input$opp_team, "</p></body></html>",
              file = file)
          return()
        }

        temp_dir <- tempdir()
        temp_rmd <- file.path(temp_dir, "report_template_batch.Rmd")
        file.copy("report_template.Rmd", temp_rmd, overwrite = TRUE)

        supabase_url <- Sys.getenv("SUPABASE_URL")
        bucket_name <- "scouting-images"

        html_bodies <- list()
        shared_head <- NULL
        n_total <- length(pitchers) * 2
        done <- 0

        for (pitcher in pitchers) {
          for (split in c("Right", "Left")) {
            done <- done + 1
            split_label <- if (split == "Right") "RHH" else "LHH"
            incProgress(done / n_total,
                        detail = paste0(pitcher, " (", split_label, ")"))

            # Fetch notes first — skip pitcher+split with no saved content
            notes <- tryCatch(
              get_scouting_notes(pool, pitcher, input$opp_team, split),
              error = function(e) list(gameplan = "", attack = "", first_pitch = "",
                                       hitter_adv = "", two_k = "", risp = "",
                                       pitcher_grade = "", out_pitch = "")
            )
            pitch_descs <- tryCatch(
              get_pitch_descriptions(pool, pitcher, input$opp_team, split),
              error = function(e) list()
            )

            has_data <- any(nchar(trimws(c(
              notes$gameplan %||% "",
              notes$attack %||% "",
              notes$first_pitch %||% "",
              notes$hitter_adv %||% "",
              notes$two_k %||% ""
            ))) > 0) || length(pitch_descs) > 0

            if (!has_data) next

            # Fetch and process pitch data
            raw_df <- tryCatch(
              get_pitcher_data(pool, pitcher, input$opp_dates[1], input$opp_dates[2]),
              error = function(e) data.frame()
            )
            if (nrow(raw_df) == 0) next

            # Apply saved deletions
            saved_edits <- tryCatch(
              get_pitch_edits(pool, pitcher, input$opp_team, split),
              error = function(e) list(deletions = character(0), remaps = list())
            )
            if (length(saved_edits$deletions) > 0) {
              raw_df <- raw_df |> dplyr::filter(!taggedpitchtype %in% saved_edits$deletions)
            }
            if (nrow(raw_df) == 0) next

            # Apply remaps
            if (length(saved_edits$remaps) > 0) {
              remap_vec <- unlist(saved_edits$remaps)
              matched <- raw_df$taggedpitchtype %in% names(remap_vec)
              raw_df$pitch_type_display <- raw_df$taggedpitchtype
              raw_df$pitch_type_display[matched] <- remap_vec[raw_df$taggedpitchtype[matched]]
            } else {
              raw_df$pitch_type_display <- raw_df$taggedpitchtype
            }

            # Filter by handedness split
            df_split <- raw_df |> dplyr::filter(batterside == split)
            if (nrow(df_split) == 0) next

            # Compute arsenal: usage/zone% from split data, velo/IVB/HB from both splits
            arsenal_filtered <- compute_arsenal_summary(df_split, "pitch_type_display", movement_df = raw_df)

            # Apply velocity overrides
            overrides <- tryCatch(
              get_velo_overrides(pool, pitcher, input$opp_team, split),
              error = function(e) list()
            )
            arsenal_filtered <- apply_velo_overrides(arsenal_filtered, overrides)

            # Fetch remaining scouting data
            risp_imgs   <- tryCatch(get_risp_images(pool, pitcher, input$opp_team, split), error = function(e) list())
            risp_usage  <- tryCatch(get_risp_usages(pool, pitcher, input$opp_team, split), error = function(e) list())
            pitcher_stats <- tryCatch(
              get_pitcher_stats(pool, pitcher, input$opp_team, "global"),
              error = function(e) list(ip = "", era = "", k = "", bb = "", baa_lhh = "", baa_rhh = "")
            )

            # Look up pitcher image URL
            img_url <- NULL
            if (supabase_url != "") {
              safe_team    <- gsub("[^A-Za-z0-9_-]", "_", input$opp_team)
              safe_pitcher <- gsub("[^A-Za-z0-9_-]", "_", pitcher)
              for (ext in c("png", "jpg", "jpeg")) {
                storage_path <- paste0(safe_team, "/", safe_pitcher, ".", ext)
                pub_url <- paste0(supabase_url, "/storage/v1/object/public/",
                                   bucket_name, "/", storage_path)
                resp <- tryCatch(httr::HEAD(pub_url), error = function(e) NULL)
                if (!is.null(resp) && httr::status_code(resp) == 200) {
                  img_url <- pub_url
                  break
                }
              }
            }

            # Render report to temp HTML
            temp_html <- tempfile(fileext = ".html")
            tryCatch({
              rmarkdown::render(
                temp_rmd,
                output_file = temp_html,
                output_format = "html_document",
                params = list(
                  pitcher         = pitcher,
                  team            = input$opp_team,
                  dates           = input$opp_dates,
                  split           = split,
                  arsenal         = arsenal_filtered,
                  pitch_data      = df_split,
                  pitcher_image   = img_url,
                  notes           = list(
                    gameplan    = notes$gameplan    %||% "",
                    attack      = notes$attack      %||% "",
                    first_pitch = notes$first_pitch %||% "",
                    hitter_adv  = notes$hitter_adv  %||% "",
                    two_k       = notes$two_k       %||% "",
                    risp        = notes$risp        %||% ""
                  ),
                  pitch_descriptions = pitch_descs,
                  risp_images        = risp_imgs,
                  risp_usages        = risp_usage,
                  pitcher_stats      = pitcher_stats,
                  grade              = notes$pitcher_grade %||% "",
                  out_pitch          = notes$out_pitch     %||% ""
                ),
                quiet = TRUE,
                envir = new.env(parent = globalenv())
              )

              html_content <- paste(readLines(temp_html, encoding = "UTF-8", warn = FALSE),
                                    collapse = "\n")

              # Capture head section from the first successful render
              if (is.null(shared_head)) {
                m <- regmatches(html_content,
                                regexpr("(?s)<head>.*?</head>", html_content, perl = TRUE))
                if (length(m) > 0) shared_head <- m
              }

              # Extract body inner content
              bm <- regmatches(html_content,
                               regexpr("(?s)<body[^>]*>.*</body>", html_content, perl = TRUE))
              if (length(bm) > 0) {
                body_inner <- bm
                body_inner <- sub("^<body[^>]*>", "", body_inner, perl = TRUE)
                body_inner <- sub("</body>\\s*$", "", body_inner, perl = TRUE)
                html_bodies[[length(html_bodies) + 1]] <- body_inner
              }

              if (file.exists(temp_html)) file.remove(temp_html)

            }, error = function(e) {
              message("Batch render failed for ", pitcher, " (", split_label, "): ", e$message)
            })
          }
        }

        if (length(html_bodies) == 0) {
          cat("<html><body><p>No reports with saved scouting data found for: ",
              input$opp_team, "</p></body></html>", file = file)
          return()
        }

        # Wrap each report section with a page break (except the last)
        pages <- lapply(seq_along(html_bodies), function(i) {
          if (i < length(html_bodies)) {
            paste0('<div style="page-break-after: always;">', html_bodies[[i]], '</div>')
          } else {
            paste0('<div>', html_bodies[[i]], '</div>')
          }
        })

        full_html <- paste0(
          "<!DOCTYPE html>\n<html>\n",
          if (!is.null(shared_head)) shared_head else "<head></head>",
          "\n<body>\n",
          paste(pages, collapse = "\n"),
          "\n</body>\n</html>"
        )

        con <- file(file, open = "w", encoding = "UTF-8")
        writeLines(full_html, con = con)
        close(con)
      })
    }
  )

  ##########################################
  ####### Hitter Scouting Report - Opposing Hitters
  ##########################################

  # Populate hitter team dropdown from upcoming opponents
  observe({
    upcoming_trigger()  # Re-run when upcoming opponents change
    upcoming_teams <- get_upcoming_teams(pool)
    if (length(upcoming_teams) == 0) {
      teams <- get_opposing_teams(pool)
    } else {
      teams <- upcoming_teams
    }
    updateSelectInput(session, "hitter_opp_team", choices = teams)
  })

  # Reactive values for hitter scouting
  rval_hitter_batters <- reactiveVal(NULL)
  rval_hitter_data <- reactiveVal(list())
  rval_hitter_notes <- reactiveVal(list())

  # Load batters when button clicked
  observeEvent(input$load_hitter_report, {
    req(input$hitter_opp_team, input$hitter_pitcher_hand, input$hitter_opp_dates)

    withProgress(message = 'Loading hitter scouting data...', value = 0, {
      # Get all batters from the team
      incProgress(0.1, detail = "Fetching batters")
      batters <- get_opposing_batters(pool, input$hitter_opp_team)

      if (length(batters) == 0) {
        showNotification("No batters found for this team", type = "warning")
        rval_hitter_batters(NULL)
        return()
      }

      rval_hitter_batters(batters)

      # Batch fetch: 1 query for all pitch data
      incProgress(0.2, detail = "Loading pitch data")
      all_pitch_data <- get_team_pitch_data(
        pool, input$hitter_opp_team,
        input$hitter_opp_dates[1], input$hitter_opp_dates[2],
        input$hitter_pitcher_hand
      )
      pitch_by_batter <- if (nrow(all_pitch_data) > 0) {
        split(all_pitch_data, all_pitch_data$batter)
      } else {
        list()
      }

      # Batch fetch: 1 query for all notes
      incProgress(0.2, detail = "Loading notes")
      all_notes <- get_team_hitter_notes(pool, input$hitter_opp_team, input$hitter_pitcher_hand)

      # In-memory split (no DB calls in the loop)
      incProgress(0.4, detail = "Organizing data")
      batter_data  <- list()
      batter_notes <- list()
      for (batter in batters) {
        batter_data[[batter]]  <- pitch_by_batter[[batter]] %||% data.frame()
        batter_notes[[batter]] <- all_notes[[batter]] %||%
          get_hitter_scouting_notes(pool, batter, input$hitter_opp_team, input$hitter_pitcher_hand)
      }

      rval_hitter_data(batter_data)
      rval_hitter_notes(batter_notes)

      incProgress(0.1, detail = "Done!")
    })

    showNotification(paste("Loaded", length(batters), "batters"), type = "message")
  })

  # Generate the hitter scouting content UI
  output$hitter_scout_content <- renderUI({
    batters <- rval_hitter_batters()
    if (is.null(batters) || length(batters) == 0) {
      return(tags$div(
        style = "text-align: center; padding: 50px; color: #888;",
        tags$h4("No batters loaded"),
        tags$p("Select a team and click 'Load Batters' to generate the scouting report.")
      ))
    }

    batter_data <- rval_hitter_data()
    batter_notes <- rval_hitter_notes()
    count_labels <- get_count_labels()
    highlight_colors <- get_highlight_colors()

    # Generate UI for each batter
    batter_uis <- lapply(seq_along(batters), function(idx) {
      batter <- batters[idx]
      safe_batter_id <- gsub("[^A-Za-z0-9]", "_", batter)
      notes <- batter_notes[[batter]]
      df <- batter_data[[batter]]

      # Batter header
      batter_header <- tags$div(
        style = "background: #1a365d; color: white; padding: 8px 15px; margin-bottom: 10px; border-radius: 4px;",
        tags$h4(style = "margin: 0; font-size: 16px;", batter)
      )

      # S/M Heatmaps row (6 pitch types)
      sm_heatmaps <- tags$div(
        style = "display: flex; gap: 5px; margin-bottom: 5px;",
        lapply(c("4SM", "2SM", "CT", "CB", "SL", "CH"), function(pt) {
          tags$div(
            style = "flex: 1; text-align: center;",
            tags$div(style = "font-size: 9px; font-weight: bold; margin-bottom: 2px;", paste0("S/M ", pt)),
            plotOutput(paste0("hitter_sm_", safe_batter_id, "_", pt), height = "80px", width = "100%")
          )
        })
      )

      # AVG Heatmaps row (6 pitch types)
      avg_heatmaps <- tags$div(
        style = "display: flex; gap: 5px; margin-bottom: 10px;",
        lapply(c("4SM", "2SM", "CT", "CB", "SL", "CH"), function(pt) {
          tags$div(
            style = "flex: 1; text-align: center;",
            tags$div(style = "font-size: 9px; font-weight: bold; margin-bottom: 2px;", paste0("AVG ", pt)),
            plotOutput(paste0("hitter_avg_", safe_batter_id, "_", pt), height = "80px", width = "100%")
          )
        })
      )

      # Count columns with percentage inputs and highlight toggles
      count_inputs <- tags$div(
        style = "display: flex; gap: 3px; margin-bottom: 10px; flex-wrap: wrap;",
        lapply(count_labels, function(count) {
          input_id1 <- paste0("hitter_count_", safe_batter_id, "_", gsub("-", "_", count), "_1")
          input_id2 <- paste0("hitter_count_", safe_batter_id, "_", gsub("-", "_", count), "_2")
          highlight_id <- paste0("hitter_highlight_", safe_batter_id, "_", gsub("-", "_", count))

          # Get saved values
          saved_vals <- notes$count_data[[count]]
          val1 <- if (!is.null(saved_vals) && length(saved_vals) >= 1) saved_vals[[1]] else ""
          val2 <- if (!is.null(saved_vals) && length(saved_vals) >= 2) saved_vals[[2]] else ""
          highlight_color <- notes$highlights[[count]] %||% "none"
          bg_color <- highlight_colors[[highlight_color]] %||% "transparent"

          tags$div(
            id = highlight_id,
            style = paste0("flex: 0 0 70px; text-align: center; padding: 5px; border: 1px solid #ddd; border-radius: 4px; background: ", bg_color, "; cursor: pointer;"),
            onclick = sprintf("Shiny.setInputValue('toggle_highlight_%s_%s', Math.random(), {priority: 'event'})", safe_batter_id, gsub("-", "_", count)),
            tags$div(style = "font-size: 10px; font-weight: bold; margin-bottom: 3px;", count),
            tags$div(
              style = "display: flex; gap: 2px;",
              tags$input(
                type = "text",
                id = input_id1,
                value = val1,
                style = "width: 28px; font-size: 10px; padding: 2px; text-align: center;",
                placeholder = "%",
                onchange = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'})", input_id1)
              ),
              tags$input(
                type = "text",
                id = input_id2,
                value = val2,
                style = "width: 28px; font-size: 10px; padding: 2px; text-align: center;",
                placeholder = "%",
                onchange = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'})", input_id2)
              )
            )
          )
        })
      )

      # Image uploads section
      image_uploads <- tags$div(
        style = "display: flex; gap: 10px; margin-bottom: 10px;",

        # Manual upload images: BOX, CONTACT POINT, SPRAY CHART, RISP GB
        lapply(list(
          list(id = "box", label = "BOX"),
          list(id = "contact_point", label = "CONTACT PT"),
          list(id = "spray_chart", label = "SPRAY"),
          list(id = "risp_gb", label = "RISP GB")
        ), function(img_info) {
          img_url <- notes[[paste0("img_", img_info$id)]]
          upload_id <- paste0("hitter_img_", safe_batter_id, "_", img_info$id)

          tags$div(
            style = "flex: 1; text-align: center;",
            tags$div(style = "font-size: 9px; font-weight: bold; margin-bottom: 2px;", img_info$label),
            if (!is.null(img_url) && img_url != "") {
              tags$img(src = img_url, style = "width: 100%; max-height: 70px; object-fit: contain; border: 1px solid #ddd; border-radius: 3px;")
            } else {
              tags$div(
                style = "height: 50px; background: #f5f5f5; border: 1px dashed #ccc; border-radius: 3px; display: flex; align-items: center; justify-content: center;",
                tags$small(style = "color: #888; font-size: 8px;", "Upload")
              )
            },
            fileInput(upload_id, NULL, accept = c("image/png", "image/jpeg"), width = "100%")
          )
        }),

        # Auto-generated heatmaps: EXITVELO, POPUP, 2K MISS, 2K TAKE
        tags$div(
          style = "flex: 1; text-align: center;",
          tags$div(style = "font-size: 9px; font-weight: bold; margin-bottom: 2px;", "EXITVELO"),
          plotOutput(paste0("hitter_exitvelo_", safe_batter_id), height = "70px", width = "100%")
        ),
        tags$div(
          style = "flex: 1; text-align: center;",
          tags$div(style = "font-size: 9px; font-weight: bold; margin-bottom: 2px;", "POPUP"),
          plotOutput(paste0("hitter_popup_", safe_batter_id), height = "70px", width = "100%")
        ),
        tags$div(
          style = "flex: 1; text-align: center;",
          tags$div(style = "font-size: 9px; font-weight: bold; margin-bottom: 2px;", "2K MISS"),
          plotOutput(paste0("hitter_2kmiss_", safe_batter_id), height = "70px", width = "100%")
        ),
        tags$div(
          style = "flex: 1; text-align: center;",
          tags$div(style = "font-size: 9px; font-weight: bold; margin-bottom: 2px;", "2K TAKE"),
          plotOutput(paste0("hitter_2ktake_", safe_batter_id), height = "70px", width = "100%")
        )
      )

      # Stats and notes section
      stats_notes <- tags$div(
        style = "display: flex; gap: 10px;",

        # Stats inputs (compact grid)
        tags$div(
          style = "flex: 0 0 300px;",
          tags$div(
            style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 5px;",
            lapply(list(
              list(id = "slg", label = "SLG"),
              list(id = "run", label = "RUN"),
              list(id = "k", label = "K"),
              list(id = "bb", label = "BB"),
              list(id = "hbp", label = "HBP"),
              list(id = "hr", label = "HR"),
              list(id = "fly", label = "FLY%"),
              list(id = "ground", label = "GB%")
            ), function(stat) {
              stat_id <- paste0("hitter_stat_", safe_batter_id, "_", stat$id)
              saved_val <- notes[[paste0("stats_", stat$id)]] %||% ""

              tags$div(
                style = "text-align: center;",
                tags$div(style = "font-size: 9px; font-weight: bold;", stat$label),
                tags$input(
                  type = "text",
                  id = stat_id,
                  value = saved_val,
                  style = "width: 100%; font-size: 11px; padding: 3px; text-align: center;",
                  onchange = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'})", stat_id)
                )
              )
            })
          )
        ),

        # Notes text areas
        tags$div(
          style = "flex: 1;",
          tags$div(style = "font-size: 9px; font-weight: bold; margin-bottom: 2px;", "NOTES"),
          tags$textarea(
            id = paste0("hitter_notes_", safe_batter_id),
            style = "width: 100%; height: 60px; font-size: 10px; padding: 5px; resize: none;",
            placeholder = "General notes...",
            onchange = sprintf("Shiny.setInputValue('hitter_notes_%s', this.value, {priority: 'event'})", safe_batter_id),
            notes$notes_main %||% ""
          )
        ),
        tags$div(
          style = "flex: 1;",
          tags$div(style = "font-size: 9px; font-weight: bold; margin-bottom: 2px;", "ACTION"),
          tags$textarea(
            id = paste0("hitter_action_", safe_batter_id),
            style = "width: 100%; height: 60px; font-size: 10px; padding: 5px; resize: none;",
            placeholder = "Action items...",
            onchange = sprintf("Shiny.setInputValue('hitter_action_%s', this.value, {priority: 'event'})", safe_batter_id),
            notes$notes_action %||% ""
          )
        )
      )

      # Combine all sections for this batter
      tags$div(
        style = "border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px; margin-bottom: 15px; background: #fafafa;",
        batter_header,
        sm_heatmaps,
        avg_heatmaps,
        count_inputs,
        image_uploads,
        stats_notes
      )
    })

    do.call(tagList, batter_uis)
  })

  # Dynamic plot outputs for each batter
  observe({
    batters <- rval_hitter_batters()
    req(batters)

    batter_data <- rval_hitter_data()

    for (batter in batters) {
      local({
        b <- batter
        safe_id <- gsub("[^A-Za-z0-9]", "_", b)
        df <- batter_data[[b]]

        # S/M heatmaps for each pitch type
        for (pt in c("4SM", "2SM", "CT", "CB", "SL", "CH")) {
          local({
            pitch_type <- pt
            output[[paste0("hitter_sm_", safe_id, "_", pitch_type)]] <- renderPlot({
              req(df)
              if (nrow(df) > 0) {
                generate_swingmiss_heatmap(df, pitch_type)
              }
            }, bg = "transparent")
          })
        }

        # AVG heatmaps for each pitch type
        for (pt in c("4SM", "2SM", "CT", "CB", "SL", "CH")) {
          local({
            pitch_type <- pt
            output[[paste0("hitter_avg_", safe_id, "_", pitch_type)]] <- renderPlot({
              req(df)
              if (nrow(df) > 0) {
                generate_slg_heatmap_batter(df, pitch_type)
              }
            }, bg = "transparent")
          })
        }

        # EXITVELO heatmap
        output[[paste0("hitter_exitvelo_", safe_id)]] <- renderPlot({
          req(df)
          if (nrow(df) > 0) {
            generate_exitvelo_heatmap(df)
          }
        }, bg = "transparent")

        # POPUP heatmap
        output[[paste0("hitter_popup_", safe_id)]] <- renderPlot({
          req(df)
          if (nrow(df) > 0) {
            generate_popup_heatmap(df)
          }
        }, bg = "transparent")

        # 2K MISS heatmap
        output[[paste0("hitter_2kmiss_", safe_id)]] <- renderPlot({
          req(df)
          if (nrow(df) > 0) {
            generate_2k_miss_heatmap(df)
          }
        }, bg = "transparent")

        # 2K TAKE heatmap
        output[[paste0("hitter_2ktake_", safe_id)]] <- renderPlot({
          req(df)
          if (nrow(df) > 0) {
            generate_2k_take_heatmap(df)
          }
        }, bg = "transparent")
      })
    }
  })

  # Auto-save observers for hitter notes - debounced
  hitter_notes_debounced <- reactive({
    batters <- rval_hitter_batters()
    if (is.null(batters)) return(NULL)

    # Collect all input values
    all_inputs <- list()
    for (batter in batters) {
      safe_id <- gsub("[^A-Za-z0-9]", "_", batter)
      batter_inputs <- list(
        batter = batter,
        notes_main = input[[paste0("hitter_notes_", safe_id)]],
        notes_action = input[[paste0("hitter_action_", safe_id)]]
      )

      # Collect stats
      for (stat in c("slg", "run", "k", "bb", "hbp", "hr", "fly", "ground")) {
        batter_inputs[[paste0("stats_", stat)]] <- input[[paste0("hitter_stat_", safe_id, "_", stat)]]
      }

      # Collect count data
      count_data <- list()
      for (count in get_count_labels()) {
        count_key <- gsub("-", "_", count)
        val1 <- input[[paste0("hitter_count_", safe_id, "_", count_key, "_1")]]
        val2 <- input[[paste0("hitter_count_", safe_id, "_", count_key, "_2")]]
        if (!is.null(val1) || !is.null(val2)) {
          count_data[[count]] <- list(val1 %||% "", val2 %||% "")
        }
      }
      batter_inputs$count_data <- count_data

      all_inputs[[batter]] <- batter_inputs
    }

    list(
      inputs = all_inputs,
      team = input$hitter_opp_team,
      pitcher_hand = input$hitter_pitcher_hand
    )
  }) |> debounce(2000)

  # Save hitter notes when debounced values change
  observeEvent(hitter_notes_debounced(), {
    data <- hitter_notes_debounced()
    req(data, data$team, data$pitcher_hand, data$inputs)

    for (batter in names(data$inputs)) {
      batter_data <- data$inputs[[batter]]
      if (is.null(batter_data)) next

      # Get current notes
      current_notes <- rval_hitter_notes()[[batter]]
      if (is.null(current_notes)) current_notes <- list()

      # Update with new values
      current_notes$notes_main <- batter_data$notes_main %||% ""
      current_notes$notes_action <- batter_data$notes_action %||% ""

      for (stat in c("slg", "run", "k", "bb", "hbp", "hr", "fly", "ground")) {
        current_notes[[paste0("stats_", stat)]] <- batter_data[[paste0("stats_", stat)]] %||% ""
      }

      current_notes$count_data <- batter_data$count_data %||% list()

      # Save to database
      save_hitter_scouting_notes(
        pool, batter, data$team, data$pitcher_hand, current_notes
      )
    }
  }, ignoreInit = TRUE)

  # Handle highlight toggles
  observe({
    batters <- rval_hitter_batters()
    if (is.null(batters)) return()

    highlight_colors <- names(get_highlight_colors())

    for (batter in batters) {
      local({
        b <- batter
        safe_id <- gsub("[^A-Za-z0-9]", "_", b)

        for (count in get_count_labels()) {
          local({
            cnt <- count
            count_key <- gsub("-", "_", cnt)
            toggle_id <- paste0("toggle_highlight_", safe_id, "_", count_key)

            observeEvent(input[[toggle_id]], {
              # Get current highlight state
              current_notes <- rval_hitter_notes()[[b]]
              if (is.null(current_notes)) current_notes <- list(highlights = list())
              if (is.null(current_notes$highlights)) current_notes$highlights <- list()

              current_color <- current_notes$highlights[[cnt]] %||% "none"
              current_idx <- which(highlight_colors == current_color)
              if (length(current_idx) == 0) current_idx <- 1

              # Cycle to next color
              next_idx <- (current_idx %% length(highlight_colors)) + 1
              new_color <- highlight_colors[next_idx]

              # Update notes
              current_notes$highlights[[cnt]] <- new_color

              # Update reactive value
              notes <- rval_hitter_notes()
              notes[[b]] <- current_notes
              rval_hitter_notes(notes)

              # Save to database
              update_hitter_scouting_field(
                pool, b, input$hitter_opp_team, input$hitter_pitcher_hand,
                "highlights", current_notes$highlights
              )

              # Update UI color via JavaScript
              bg_color <- get_highlight_colors()[[new_color]] %||% "transparent"
              shinyjs::runjs(sprintf(
                "document.getElementById('hitter_highlight_%s_%s').style.background = '%s';",
                safe_id, count_key, bg_color
              ))
            }, ignoreInit = TRUE)
          })
        }
      })
    }
  })

  # Handle image uploads for hitter scouting
  observe({
    batters <- rval_hitter_batters()
    if (is.null(batters)) return()

    for (batter in batters) {
      local({
        b <- batter
        safe_id <- gsub("[^A-Za-z0-9]", "_", b)

        for (img_type in c("box", "contact_point", "spray_chart", "risp_gb")) {
          local({
            img_t <- img_type
            upload_id <- paste0("hitter_img_", safe_id, "_", img_t)

            observeEvent(input[[upload_id]], {
              file <- input[[upload_id]]
              if (is.null(file)) return()

              supabase_url <- Sys.getenv("SUPABASE_URL")
              supabase_key <- Sys.getenv("SUPABASE_ANON_KEY")
              bucket_name <- "hitter-scouting-images"

              if (supabase_url == "" || supabase_key == "") {
                showNotification("Supabase credentials not configured.", type = "error")
                return()
              }

              file_ext <- tools::file_ext(file$name)
              safe_team <- gsub("[^A-Za-z0-9_-]", "_", input$hitter_opp_team)
              safe_batter <- gsub("[^A-Za-z0-9_-]", "_", b)
              safe_hand <- gsub("[^A-Za-z0-9_-]", "_", input$hitter_pitcher_hand)
              storage_path <- paste0(safe_team, "/", safe_batter, "/", safe_hand, "/", img_t, ".", file_ext)

              upload_url <- paste0(supabase_url, "/storage/v1/object/", bucket_name, "/", storage_path)

              withProgress(message = paste0("Uploading ", img_t, " image..."), value = 0.5, {
                response <- tryCatch({
                  httr::PUT(
                    upload_url,
                    httr::add_headers(
                      Authorization = paste("Bearer", supabase_key),
                      `Content-Type` = file$type,
                      `x-upsert` = "true"
                    ),
                    body = httr::upload_file(file$datapath)
                  )
                }, error = function(e) {
                  showNotification(paste("Upload error:", e$message), type = "error")
                  NULL
                })

                if (!is.null(response) && httr::status_code(response) %in% c(200, 201)) {
                  public_url <- paste0(supabase_url, "/storage/v1/object/public/", bucket_name, "/", storage_path)

                  # Update notes
                  current_notes <- rval_hitter_notes()[[b]]
                  if (is.null(current_notes)) current_notes <- list()
                  current_notes[[paste0("img_", img_t)]] <- public_url

                  notes <- rval_hitter_notes()
                  notes[[b]] <- current_notes
                  rval_hitter_notes(notes)

                  # Save to database
                  update_hitter_scouting_field(
                    pool, b, input$hitter_opp_team, input$hitter_pitcher_hand,
                    paste0("img_", img_t), public_url
                  )

                  showNotification(paste(img_t, "image uploaded!"), type = "message")
                } else if (!is.null(response)) {
                  error_content <- httr::content(response, as = "text", encoding = "UTF-8")
                  showNotification(paste("Upload failed:", error_content), type = "error")
                }
              })
            }, ignoreInit = TRUE)
          })
        }
      })
    }
  })

  # Download hitter scouting report
  output$download_hitter_report <- downloadHandler(
    filename = function() {
      paste0("hitter_scout_", gsub("[^A-Za-z0-9]", "_", input$hitter_opp_team), "_vs",
             input$hitter_pitcher_hand, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Create a temporary directory for rendering
      temp_dir <- tempdir()
      temp_rmd <- file.path(temp_dir, "hitter_report_template.Rmd")

      # Copy the template
      file.copy("hitter_report_template.Rmd", temp_rmd, overwrite = TRUE)

      # Prepare data
      batters <- rval_hitter_batters()
      batter_data <- rval_hitter_data()
      batter_notes <- rval_hitter_notes()

      # Render the report as HTML
      rmarkdown::render(
        temp_rmd,
        output_file = file,
        output_format = "html_document",
        params = list(
          team = input$hitter_opp_team,
          pitcher_hand = input$hitter_pitcher_hand,
          dates = input$hitter_opp_dates,
          batters = batters,
          batter_data = batter_data,
          batter_notes = batter_notes
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui = ui, server = server)
