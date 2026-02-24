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

cp_df <- read.csv("data/cp_df.csv")

# Ensure Date is Date everywhere 
if (!inherits(cp_df$Date, "Date")) {
  cp_df$Date <- as.Date(cp_df$Date)
}

cp_pitchers <- cp_df |>
  filter(PitcherTeam == "CAL_MUS")

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
                               start="2025-10-01",
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
                               start="2025-10-01",
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
          selectInput("opp_pitcher", "Select Pitcher", choices = NULL),
          dateRangeInput("opp_dates", "Date Range",
                         start = "2026-02-13", end = Sys.Date()),
          radioButtons("opp_split", "Batter Handedness",
                       choices = c("vs LHH" = "Left", "vs RHH" = "Right", "Both" = "Both"),
                       selected = "Both"),
          hr(),
          fileInput("pitcher_image", "Upload Pitcher Image",
                    accept = c("image/png", "image/jpeg", "image/jpg")),
          uiOutput("pitcher_image_preview")
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

      # Row 2: Report Preview (hidden until Apply clicked)
      fluidRow(
        conditionalPanel(
          condition = "input.apply_remap > 0",
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
            # Gameplan Notes
            fluidRow(
              column(12,
                tags$div(
                  style = "background: #fffbeb; border: 1px solid #f6e05e; border-radius: 4px; padding: 10px; margin: 10px 0;",
                  tags$strong(style = "color: #744210; font-size: 14px;", "Gameplan / Attack Notes"),
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
            # RISP Image Uploads
            h4("RISP (Runners in Scoring Position)"),
            fluidRow(
              column(12,
                p(style = "color: #666; font-size: 12px;",
                  "Upload heatmap images for RISP situations (we don't have baserunner data to generate these automatically)."),
                uiOutput("risp_upload_slots")
              )
            ),
            hr(),
            # Pitch Descriptions
            h4("PITCH DESCRIPTIONS"),
            fluidRow(
              column(12,
                p(style = "color: #666; font-size: 12px;",
                  "Add short descriptions for each pitch type (e.g., 'Power FB, run inside to RHH')."),
                uiOutput("pitch_description_inputs")
              )
            )
          )
        )
      )
    ),
    tabItem(
      "coach_lee_hitters",
      fluidRow(
        box(
          width = 4,
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
          width = 8,
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
    rval_pitcher_df() |>
      group_by(TaggedPitchType) |>
      summarize(
        PitchCount = n(),
        Usage = PitchCount/nrow(rval_pitcher_df()),
        WhiffRate = sum(IsWhiff)/sum(IsStrike),
        Velo = round(mean(RelSpeed, na.rm = TRUE),1),
        SpinRate = round(mean(SpinRate, na.rm = TRUE),0),
        IVB = round(mean(InducedVertBreak, na.rm = TRUE),1),
        HB = round(mean(HorzBreak, na.rm = TRUE),1),
        .groups= "drop"
      )
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
      mutate(IsKWhiff = if_else((PitchCall == 'StrikeSwinging') & (KorBB == 'Strikeout'), 1, 0),
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

  # Reactive to store processed data for description inputs
  rval_processed_data <- reactiveVal(NULL)

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
    game_date <- if (!is.na(input$add_opponent_date)) input$add_opponent_date else NULL
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

  # Auto-load scouting notes when pitcher OR team is selected
  observeEvent(c(input$opp_pitcher, input$opp_team), {
    req(input$opp_team, input$opp_pitcher)

    # Reset delete checkboxes and remap dropdowns via JavaScript
    shinyjs::runjs("
      // Uncheck all delete checkboxes
      $('.delete-check').prop('checked', false);
      // Reset Shiny input values for delete checkboxes
      for (var i = 1; i <= 20; i++) {
        Shiny.setInputValue('delete_pitch_' + i, false, {priority: 'event'});
      }
    ")

    # Fetch notes from database
    notes <- tryCatch({
      get_scouting_notes(pool, input$opp_pitcher, input$opp_team)
    }, error = function(e) {
      list(gameplan = "", attack = "", first_pitch = "", hitter_adv = "", two_k = "")
    })

    # Update text area inputs with saved notes
    updateTextAreaInput(session, "notes_gameplan", value = notes$gameplan)
    updateTextAreaInput(session, "notes_attack", value = notes$attack)
    updateTextAreaInput(session, "notes_first_pitch", value = notes$first_pitch)
    updateTextAreaInput(session, "notes_hitter_adv", value = notes$hitter_adv)
    updateTextAreaInput(session, "notes_2k", value = notes$two_k)

    # Load pitch descriptions
    descriptions <- get_pitch_descriptions(pool, input$opp_pitcher, input$opp_team)
    rval_pitch_descriptions(descriptions)
  }, ignoreInit = TRUE)

  # Reactive value to track last save timestamp to avoid duplicate saves
  rval_last_notes_save <- reactiveVal(NULL)

  # Debounced auto-save for notes using reactive timer pattern
  # This creates a reactive that returns the current notes after a 2-second delay
  notes_debounced <- reactive({
    req(input$opp_pitcher, input$opp_team)

    # Capture current notes values
    list(
      gameplan = input$notes_gameplan %||% "",
      attack = input$notes_attack %||% "",
      first_pitch = input$notes_first_pitch %||% "",
      hitter_adv = input$notes_hitter_adv %||% "",
      two_k = input$notes_2k %||% "",
      pitcher = input$opp_pitcher,
      team = input$opp_team
    )
  }) |> debounce(2000)

  # Observer that triggers on debounced notes changes
  observeEvent(notes_debounced(), {
    notes_data <- notes_debounced()
    req(notes_data$pitcher, notes_data$team)

    # Create unique key for this save
    save_key <- paste(notes_data$pitcher, notes_data$team,
                      notes_data$gameplan, notes_data$attack,
                      notes_data$first_pitch, notes_data$hitter_adv,
                      notes_data$two_k, sep = "|")

    # Only save if content has changed since last save
    if (is.null(rval_last_notes_save()) || rval_last_notes_save() != save_key) {
      notes_list <- list(
        gameplan = notes_data$gameplan,
        attack = notes_data$attack,
        first_pitch = notes_data$first_pitch,
        hitter_adv = notes_data$hitter_adv,
        two_k = notes_data$two_k
      )

      save_success <- save_scouting_notes(pool, notes_data$pitcher, notes_data$team, notes_list)
      if (save_success) {
        rval_last_notes_save(save_key)
        showNotification("Notes auto-saved", type = "message", duration = 1)
      }
    }
  }, ignoreInit = TRUE)

  # Reactive to store validation summary for remap dropdowns
  rval_validation_summary <- reactive({
    req(input$opp_pitcher, input$opp_dates)
    get_pitch_validation_summary(pool, input$opp_pitcher,
                                  input$opp_dates[1], input$opp_dates[2])
  })

  # Pitch validation table with remap dropdowns and delete checkboxes
  output$pitch_validation_table <- DT::renderDT({
    summary_df <- rval_validation_summary()

    if (nrow(summary_df) == 0) {
      return(datatable(data.frame(Message = "No pitch data found for this pitcher in the selected date range")))
    }

    # Get unique pitch types for dropdown options
    pitch_types <- summary_df$pitch_type

    # Build HTML select options
    build_options <- function(choices, selected) {
      opts <- sapply(choices, function(ch) {
        sel <- if (ch == selected) ' selected' else ''
        sprintf('<option value="%s"%s>%s</option>', ch, sel, ch)
      })
      paste(opts, collapse = "")
    }

    # Add remap dropdown column using plain HTML
    summary_df$remap_to <- sapply(seq_len(nrow(summary_df)), function(i) {
      sprintf(
        '<select id="remap_%d" class="remap-select" style="width:120px;" onchange="Shiny.setInputValue(\'remap_%d\', this.value, {priority: \'event\'})">%s</select>',
        i, i, build_options(pitch_types, summary_df$pitch_type[i])
      )
    })

    # Add delete checkbox column using plain HTML
    summary_df$delete <- sapply(seq_len(nrow(summary_df)), function(i) {
      sprintf(
        '<input type="checkbox" id="delete_pitch_%d" class="delete-check" onchange="Shiny.setInputValue(\'delete_pitch_%d\', this.checked, {priority: \'event\'})">',
        i, i
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

      # Step 4: Save scouting notes to database
      incProgress(0.05, detail = "Saving notes")
      notes_list <- list(
        gameplan = input$notes_gameplan,
        attack = input$notes_attack,
        first_pitch = input$notes_first_pitch,
        hitter_adv = input$notes_hitter_adv,
        two_k = input$notes_2k
      )
      save_success <- save_scouting_notes(pool, input$opp_pitcher, input$opp_team, notes_list)
      if (save_success) {
        showNotification("Notes saved", type = "message", duration = 2)
      }

      incProgress(0.05, detail = "Done!")
    })

    result <- list(
      raw = raw_df,
      arsenal = arsenal,
      remap = remap,
      deletions = deletions,
      pitcher = input$opp_pitcher,
      team = input$opp_team
    )

    # Update processed data reactive for description inputs
    rval_processed_data(result)

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

    # Find out pitch (most common secondary pitch based on usage)
    arsenal <- data$arsenal
    if (nrow(arsenal) > 1) {
      out_pitch <- arsenal$pitch_type[2]
    } else {
      out_pitch <- arsenal$pitch_type[1]
    }

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
      p(strong("Extension: "), sprintf("%.1f ft", avg_ext)),
      p(strong("Release Height: "), sprintf("%.1f ft", avg_rel_height)),
      p(strong("Out Pitch: "), out_pitch),
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

    # Recompute arsenal from filtered data so usage reflects current split
    arsenal <- compute_arsenal_summary(df, "pitch_type_display")

    # Keep zone_pct numeric for conditional formatting, add display column
    arsenal <- arsenal |>
      mutate(
        usage_display = paste0(usage, "%"),
        zone_pct_display = paste0(zone_pct, "%")
      ) |>
      select(pitch_type, count, usage_display, velo, zone_pct_display, zone_pct, ivb, hb) |>
      rename(
        `Pitch Type` = pitch_type,
        `#` = count,
        `Usage` = usage_display,
        `Velo` = velo,
        `Zone%` = zone_pct_display,
        `zone_pct_num` = zone_pct,
        `IVB` = ivb,
        `HB` = hb
      )

    datatable(
      arsenal,
      rownames = FALSE,
      class = 'cell-border stripe',
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = 10,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
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
        columns = names(arsenal),
        fontSize = '14px',
        lineHeight = '1.6'
      ) |>
      formatStyle(
        'Zone%',
        valueColumns = 'zone_pct_num',
        backgroundColor = styleInterval(39, c('rgba(255, 200, 200, 0.8)', 'transparent')),
        color = styleInterval(39, c('darkred', 'inherit')),
        fontWeight = styleInterval(39, c('bold', 'normal'))
      )
  })

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

  # Pitch Description Inputs UI
  output$pitch_description_inputs <- renderUI({
    data <- rval_processed_data()
    req(data)
    arsenal <- data$arsenal

    if (is.null(arsenal) || nrow(arsenal) == 0) return(NULL)

    # Load existing descriptions
    descriptions <- rval_pitch_descriptions()

    tagList(
      tags$div(
        style = "display: flex; flex-wrap: wrap; gap: 10px;",
        lapply(seq_len(nrow(arsenal)), function(i) {
          pt <- arsenal$pitch_type[i]
          existing <- descriptions[[pt]] %||% ""

          tags$div(
            style = "flex: 1; min-width: 200px; max-width: 300px;",
            textInput(
              paste0("pitch_desc_", i),
              label = pt,
              value = existing,
              width = "100%",
              placeholder = "Short description..."
            )
          )
        })
      )
    )
  })

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
      team = input$opp_team
    )
  }) |> debounce(2000)

  # Observer that triggers on debounced description changes
  observeEvent(all_pitch_descriptions(), {
    desc_data <- all_pitch_descriptions()
    req(desc_data, desc_data$pitcher, desc_data$team)

    # Filter to non-empty descriptions
    descriptions <- desc_data$descriptions
    descriptions <- descriptions[sapply(descriptions, function(x) !is.null(x) && x != "")]

    # Create unique key for this save
    save_key <- paste(desc_data$pitcher, desc_data$team,
                      paste(names(descriptions), descriptions, collapse = "|"), sep = "||")

    # Only save if content has changed since last save
    if (is.null(rval_last_desc_save()) || rval_last_desc_save() != save_key) {
      if (length(descriptions) > 0) {
        save_pitch_descriptions(pool, desc_data$pitcher, desc_data$team, descriptions)
        rval_pitch_descriptions(descriptions)
        rval_last_desc_save(save_key)
        showNotification("Pitch descriptions auto-saved", type = "message", duration = 1)
      }
    }
  }, ignoreInit = TRUE)

  # RISP Upload Slots UI
  output$risp_upload_slots <- renderUI({
    data <- rval_processed_data()
    req(data)
    arsenal <- data$arsenal

    if (is.null(arsenal) || nrow(arsenal) == 0) return(NULL)

    pitch_types <- arsenal$pitch_type

    # Load existing RISP images
    risp_images <- get_risp_images(pool, input$opp_pitcher, input$opp_team)

    tagList(
      tags$div(
        style = "display: flex; flex-wrap: wrap; gap: 15px;",
        lapply(seq_along(pitch_types), function(i) {
          pt <- pitch_types[i]
          existing_url <- risp_images[[pt]]

          tags$div(
            style = "text-align: center; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px; min-width: 150px;",
            tags$strong(pt),
            tags$br(),
            if (!is.null(existing_url) && existing_url != "") {
              tags$div(
                tags$img(src = existing_url, style = "max-width: 120px; max-height: 120px; margin: 5px 0;"),
                tags$br(),
                tags$small(class = "text-muted", "Uploaded")
              )
            } else {
              tags$div(style = "height: 60px; display: flex; align-items: center; justify-content: center;",
                tags$small(class = "text-muted", "No image")
              )
            },
            fileInput(paste0("risp_upload_", i), NULL, accept = c("image/png", "image/jpeg"), width = "100%")
          )
        })
      )
    )
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

            file_ext <- tools::file_ext(file$name)
            safe_team <- gsub("[^A-Za-z0-9_-]", "_", input$opp_team)
            safe_pitcher <- gsub("[^A-Za-z0-9_-]", "_", input$opp_pitcher)
            safe_pitch <- gsub("[^A-Za-z0-9_-]", "_", pt)
            storage_path <- paste0(safe_team, "/", safe_pitcher, "/", safe_pitch, ".", file_ext)

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

                # Save URL to database
                risp_images <- get_risp_images(pool, input$opp_pitcher, input$opp_team)
                risp_images[[pt]] <- public_url
                save_risp_images(pool, input$opp_pitcher, input$opp_team, risp_images)

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
      mutate(pct = round(n / sum(n) * 100, 0)) |>
      arrange(desc(n))
  }

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

  # Helper function to safely generate a heatmap
  safe_heatmap <- function(data, idx) {
    tryCatch({
      if (is.null(data) || is.null(data$usage) || nrow(data$usage) < idx) {
        return(NULL)
      }
      pt <- data$usage$pitch_type_display[idx]
      pct <- data$usage$pct[idx]
      if (is.na(pt)) return(NULL)
      generate_heatmap(data$df, pitch_type = pt, title = paste0(pt, " (", pct, "%)"))
    }, error = function(e) {
      message("Heatmap error: ", e$message)
      NULL
    })
  }

  # Overall heatmaps (4 fixed outputs)
  output$heat_overall_1 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_overall_data()
    req(data)
    safe_heatmap(data, 1)
  })
  output$heat_overall_2 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_overall_data()
    req(data)
    safe_heatmap(data, 2)
  })
  output$heat_overall_3 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_overall_data()
    req(data)
    safe_heatmap(data, 3)
  })
  output$heat_overall_4 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_overall_data()
    req(data)
    safe_heatmap(data, 4)
  })

  # First pitch heatmaps
  output$heat_first_1 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_first_data()
    req(data)
    safe_heatmap(data, 1)
  })
  output$heat_first_2 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_first_data()
    req(data)
    safe_heatmap(data, 2)
  })
  output$heat_first_3 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_first_data()
    req(data)
    safe_heatmap(data, 3)
  })
  output$heat_first_4 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_first_data()
    req(data)
    safe_heatmap(data, 4)
  })

  # Hitter advantage heatmaps
  output$heat_adv_1 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_adv_data()
    req(data)
    safe_heatmap(data, 1)
  })
  output$heat_adv_2 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_adv_data()
    req(data)
    safe_heatmap(data, 2)
  })
  output$heat_adv_3 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_adv_data()
    req(data)
    safe_heatmap(data, 3)
  })
  output$heat_adv_4 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_adv_data()
    req(data)
    safe_heatmap(data, 4)
  })

  # 2K heatmaps
  output$heat_2k_1 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_2k_data()
    req(data)
    safe_heatmap(data, 1)
  })
  output$heat_2k_2 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_2k_data()
    req(data)
    safe_heatmap(data, 2)
  })
  output$heat_2k_3 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_2k_data()
    req(data)
    safe_heatmap(data, 3)
  })
  output$heat_2k_4 <- renderPlot({
    req(input$apply_remap > 0)
    data <- rval_2k_data()
    req(data)
    safe_heatmap(data, 4)
  })

  # HTML Report Download handler (print to PDF from browser)
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("scout_", gsub("[^A-Za-z0-9]", "_", input$opp_pitcher), "_", Sys.Date(), ".html")
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

      # Recompute arsenal from filtered data so usage reflects current split
      arsenal_filtered <- compute_arsenal_summary(df, "pitch_type_display")

      # Get pitcher image URL
      img_url <- rval_pitcher_image_url()

      # Get pitch descriptions and RISP images
      pitch_descs <- get_pitch_descriptions(pool, input$opp_pitcher, input$opp_team)
      risp_imgs <- get_risp_images(pool, input$opp_pitcher, input$opp_team)

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
            two_k = input$notes_2k
          ),
          pitch_descriptions = pitch_descs,
          risp_images = risp_imgs
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui = ui, server = server)
