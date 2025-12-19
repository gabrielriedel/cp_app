# Save fall data from local device

folder_path <- "/Users/gaberiedel/Downloads/Fall-25"

files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

all_data <- files |>
  map_dfr(read_csv) |>
  filter(Batter != "Volmerding, Joshua") |>
  mutate(Batter = case_when(
    Batter == "Vonderhaar, Coco" ~ "VonderHaar, Coco",
    Batter == "Spiridonoff, Gavin" ~ "Spirdonoff, Gavin",
    TRUE ~ Batter
  ))

write.csv(all_data, "app/data/all_fall_25.csv", row.names=FALSE)

write.csv(cp_df, "app/data/cp_df.csv", row.names=FALSE)


cp_fall <- read.csv("app/data/all_fall_25.csv")
fall |>
  filter(Pitcher == "Volmerding, Joshua" & TaggedPitchType == "Slider") |>
  mutate(IsWhiff = if_else(PitchCall == 'StrikeSwinging', 1, 0),
         IsSwing = if_else(PitchCall %in% c('StrikeSwinging', 'FoulBallNotFieldable', 'InPlay', 'FoulBallFieldable','FoulBall'), 1, 0)) |>
  summarize(
    num_whiff = sum(IsWhiff),
    num_pitches = sum(IsSwing)
  )
