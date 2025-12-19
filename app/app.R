library(shiny)
library(shinydashboard)
library(tidyverse)
library(pool)
library(RPostgres)


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

# cp_df <- dbGetQuery(
#   pool,
#   sql("
#     SELECT
#         \"Pitcher\", \"Date\", \"BatterSide\", \"BatterTeam\", \"Inning\", \"Outs\",
#         \"Balls\", \"Strikes\", \"TaggedPitchType\", \"PitchCall\", \"KorBB\",
#         \"TaggedHitType\", \"PlayResult\", \"RelSpeed\", \"VertRelAngle\", \"HorzRelAngle\",
#         \"SpinRate\", \"SpinAxis\", \"RelHeight\", \"RelSide\", \"Extension\",
#         \"InducedVertBreak\", \"HorzBreak\", \"PlateLocHeight\", \"PlateLocSide\",
#         \"VertApprAngle\", \"HorzApprAngle\", \"ExitSpeed\", \"Angle\", \"Direction\",
#         \"Distance\", \"Batter\", \"PitcherTeam\"
#     FROM all_college_2025
#     WHERE \"PitcherTeam\" = 'CAL_MUS' OR \"BatterTeam\" = 'CAL_MUS'
#   ")
# ) |>
#   bind_rows(cp_fall) |>
#   mutate(IsStrike = if_else(PitchCall %in% c('StrikeSwinging', 'StrikeCalled', 'Strikecalled',
#                                              'FoulBallNotFieldable', 'InPlay', 'FoulBallFieldable',
#                                              'FoulBall', 'AutomaticStrike'), 1, 0),
#          IsBall = if_else(PitchCall %in% c('AutomaticBall', 'BallAutomatic', 'BallCalled',
#                                            'BallInDirt', 'BallinDirt', 'BallIntentional',
#                                            'Ballintentional', 'HitByPitch', 'HItByPitch'), 1, 0),
#          IsSwing = if_else(PitchCall %in% c('StrikeSwinging', 'FoulBallNotFieldable', 'InPlay', 'FoulBallFieldable',
#                                             'FoulBall'), 1, 0),
#          IsWhiff = if_else(PitchCall == 'StrikeSwinging', 1, 0),
#          IsWalk = if_else(KorBB == 'Walk', 1, 0),
#          IsK = if_else(KorBB == 'Strikeout', 1, 0),
#          IsHBP = if_else(PitchCall %in% c('HitByPitch', 'HItByPitch'), 1, 0),
#          IsHit = if_else(PlayResult %in% c('Single', 'SIngle', 'Double', 'triple','Triple', 'Homerun', 'HomeRun'), 1, 0),
#          Is_Single = if_else(PlayResult %in% c('Single', 'SIngle'), 1, 0),
#          Is_Double = if_else(PlayResult == 'Double', 1, 0),
#          Is_Triple = if_else(PlayResult %in% c('Triple', 'triple'), 1, 0),
#          Is_HomeRun = if_else(PlayResult %in% c('HomeRun', 'Homerun'), 1, 0),
#          Is_FlyBall = if_else(TaggedHitType %in% c('FlyBall', 'Flyball'), 1, 0),
#          Is_GroundBall = if_else(TaggedHitType %in% c('Groundball', 'GroundBall'), 1, 0)
#   )

cp_df <- read.csv("data/cp_df.csv")

cp_pitchers <- cp_df |>
  filter(PitcherTeam == "CAL_MUS")

cp_hitters <- cp_df |>
  filter(BatterTeam == "CAL_MUS")



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
             tabName='live_dash')
  )
)
  
body <- dashboardBody(
  
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
                         box(width = 10,
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
            
            # Want a search instead
            fluidRow(box(selectInput("opp_pitch_team_drop",
                                     "Select Team",
                                     choices=unique(cp_pitchers$PitcherTeam))
                          ),
            
                    box(dateRangeInput("opp_pitch_game_range",
                                       "Select Date Range",
                                       start="2025-10-01",
                                       end = Sys.Date())
                        )
                    ),
            ),
    tabItem("hitter_scout",
            fluidRow(box(selectInput("opp_hit_team_drop",
                                     "Select Team",
                                     choices=unique(cp_df$PitcherTeam))
                        ),
                    
                    box(dateRangeInput("opp_hit_game_range",
                                       "Select Date Range",
                                       start="2025-10-01",
                                       end = Sys.Date())
                        )
                    ),
            )
  )
  

)

ui <- dashboardPage(header, sidebar, body, skin="blue")

server <- function(input, output, session) {
  
  ##########################################
  ####### GENERIC GRAPHICS FUNCTIONS  ######
  ##########################################
  
  
  
  #########################################
  ####### Pitcher Dashboard Graphics ######
  #########################################
  
  
    # DATAFRAME OF PITCHES FOR SELECTED PITCHER  
    rval_pitcher_df <- reactive({
      cp_pitchers |>
        filter(Pitcher == input$pitcher_drop
               & Date >= input$pitcher_game_range[1]
               & Date <= input$pitcher_game_range[2])
    })
    
    # DATAFRAME OF SUMMARY STATS FOR SELECTED PITCHER
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
    
    
    # PER PITCH SUMMARY TABLE
    output$table <- DT::renderDT({
      rval_pitcher_summary_df()  |>
        filter(Usage > 0.01) |>
        arrange(desc(Usage)) |>
        mutate(
          Usage = scales::percent(Usage, 1),
          WhiffRate = scales::percent(WhiffRate, 1)
        )
    })
    
    # PITCH MOVEMENT PLOT
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
    
    # PITCHER RELEASE PLOT
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
    
    # PITCH USAGE HEATMAP
    output$pitcher_heat <- renderPlot({
      
      pitch_sel    <- input$heat_pitch
      hit_side_sel <- input$heat_hit_side
      cols <- viridisLite::turbo(256)
      cols[1] <- "white"
      
      # if nothing selected → include all pitch types
      if (is.null(pitch_sel) || length(pitch_sel) == 0) {
        pitch_sel <- unique(cp_pitchers$TaggedPitchType)
      }
      
      # if nothing selected → include all batter sides
      if (is.null(hit_side_sel) || length(hit_side_sel) == 0) {
        hit_side_sel <- unique(cp_pitchers$BatterSide)
      }
      
      rval_pitcher_df() |>
        left_join(rval_pitcher_summary_df(), by="TaggedPitchType") |>
        filter(Usage > 0.01 & TaggedPitchType %in% pitch_sel & BatterSide %in% hit_side_sel) |>
      ggplot( aes(x = PlateLocSide, y = PlateLocHeight)) +
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
    
    # PITCHER NAME
    output$previous_notes_title <- renderText({
      paste("Previous Notes for", input$pitcher_drop)
    })
    
    notes_trigger <- reactiveVal(0)
    
    # READ IN PREVIOUS NOTES
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
    
    # ADD A NEW NOTE
    rv_add_note <- observeEvent(input$submit_note, {
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
      
      updateTextAreaInput(session, "note_text", value = "")  # or updateTextInput if still using textInput
      notes_trigger(notes_trigger() + 1)
    })
    
    # DELETE NOTE
    observeEvent(input$delete_note_id, {
      req(input$delete_note_id)
      
      dbExecute(
        pool,
        "DELETE FROM moir_notes WHERE id = $1",
        params = list(input$delete_note_id)
      )
      
      notes_trigger(notes_trigger() + 1)
    })
    
    
    
    # EDIT NOTE
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
    
    # SAVE EDIT
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
    
    
    
    # RENDER EDIT/DELETE INTERFACE
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
              # Edit button
              actionButton(
                inputId = paste0("edit_", row$id),
                label   = "Edit",
                class   = "btn-sm",
                onclick = sprintf(
                  "Shiny.setInputValue('edit_note_id', '%s', {priority: 'event'})",
                  row$id
                )
              ),
              # Delete button
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
    
    # DATA FRAME OF PITCHES FOR SELECTED HITTER
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
    
    # HITTER SUMMARY STATS TABLE
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
    
    # Heat Zone Plot
      # output$hitter_heat <- plotly::renderPlotly({
      #   rval_hitter_df() |>
      #   ggplot() + 
      #     geom_point(aes(x=PlateLocSide, y=PlateLocHeight, color=TaggedPitchType)) +
      #     annotate("segment", x = -0.85, xend = 0.85,  y = 1.6, yend = 1.6, colour = "black", size = 1.2) +
      #     annotate("segment", x = -0.85, xend = 0.85,  y = 3.5, yend = 3.5, colour = "black", size = 1.2) +
      #     annotate("segment", x = -0.85, xend = -0.85, y = 1.6, yend = 3.5, colour = "black", size = 1.2) +
      #     annotate("segment", x = 0.85,  xend = 0.85,  y = 1.6, yend = 3.5, colour = "black", size = 1.2) +
      #     annotate("segment", x = -0.85, xend = 0.85, y = 0, yend = 0, colour = "black") +
      #     annotate("segment", x = -0.85, xend = -0.85, y = 0, yend = -0.15, colour = "black") +
      #     annotate("segment", x = 0.85, xend = 0.85, y = 0, yend = -0.15, colour = "black") +
      #     annotate("segment", x = -0.85, xend = 0, y = -0.15, yend = -0.3, colour = "black") +
      #     annotate("segment", x = 0.85, xend = 0, y = -0.15, yend = -0.3, colour = "black") +
      #     labs(
      #       x="",
      #       y=""
      #     ) +
      #   coord_fixed(ratio = 1)
      # })
      
    
      
      # PITCHES SEEN HEATMAP
      output$hitter_heat <- renderPlot({
        
        pitch_sel    <- input$heat_pitch_hit
        pitch_side_sel <- input$heat_pitch_side
        cols <- viridisLite::turbo(256)
        cols[1] <- "white"
        
        # if nothing selected → include all pitch types
        if (is.null(pitch_sel) || length(pitch_sel) == 0) {
          pitch_sel <- unique(cp_hitters$TaggedPitchType)
        }
        
        # if nothing selected → include all batter sides
        if (is.null(pitch_side_sel) || length(pitch_side_sel) == 0) {
          pitch_side_sel <- unique(cp_hitters$PitcherThrows)
        }
        
        rval_hitter_df() |>
          filter(TaggedPitchType %in% pitch_sel & PitcherThrows %in% pitch_side_sel) |>
          ggplot( aes(x = PlateLocSide, y = PlateLocHeight)) +
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)
