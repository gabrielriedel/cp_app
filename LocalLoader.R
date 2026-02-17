library(ggplot2)
library(dplyr)

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

fall_data <- files |>
  map_dfr(read_csv) |>
  filter(Batter != "Volmerding, Joshua") |>
  mutate(Batter = case_when(
    Batter == "Vonderhaar, Coco" ~ "VonderHaar, Coco",
    Batter == "Spiridonoff, Gavin" ~ "Spirdonoff, Gavin",
    TRUE ~ Batter
  ))

unique(isquad_data$Date) <- fall_data |> rbind(all_data)

write.csv(all_data, "app/data/all_fall_25.csv", row.names=FALSE)

write.csv(cp_df, "app/data/cp_df.csv", row.names=FALSE)

write.csv(isquad_data, "app/data/isquad_data.csv", row.names=FALSE)


cp_fall <- read.csv("app/data/all_fall_25.csv")
fall |>
  filter(Pitcher == "Volmerding, Joshua" & TaggedPitchType == "Slider") |>
  mutate(IsWhiff = if_else(PitchCall == 'StrikeSwinging', 1, 0),
         IsSwing = if_else(PitchCall %in% c('StrikeSwinging', 'FoulBallNotFieldable', 'InPlay', 'FoulBallFieldable','FoulBall'), 1, 0)) |>
  summarize(
    num_whiff = sum(IsWhiff),
    num_pitches = sum(IsSwing)
  )


# Heatmap

isquad <- read.csv("app/data/isquad_data.csv")

segments_df <- within(data.frame(
  x    = c(-0.85, -0.85, -0.85,  0.85),
  xend = c( 0.85,  0.85, -0.85,  0.85),
  y    = c( 1.60,  3.50,  1.60,  1.60),
  yend = c( 1.60,  3.50,  3.50,  3.50)
), {kind <- "grid"})

heatmap <- function(Data, pitcher_name){
  Data |>
    filter(Pitcher == pitcher_name) |>
    ggplot(aes(x="PlateLocHeight", y="PlateLocSide")) +
    geom_point() +
    geom_segment(
      data = segments_df,
      aes(x = x, y = y, xend = xend, yend = yend),
      linewidth = 1.2,
      color = "black",
      lineend = "round"
    ) +
    geom_density_2d()
}

heatmap(isquad, "Souza, Henry")


isquad |>
  filter(Pitcher == "Souza, Henry") |>
  ggplot(aes(x="PlateLocHeight", y="PlateLocSide")) +
  geom_point() +
  geom_segment(
    data = segments_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 1.2,
    color = "black",
    lineend = "round"
  ) +
  geom_density_2d()


ggplot(isquad |> filter(Pitcher == "Souza, Henry"), aes(x='PlateLocSide', y='PlateLocHeight', ymin=0, ymax=5)) 
+ stat_density_2d(aes(fill='..density..'), geom='raster', contour=False) 
+ scale_fill_distiller(type="div") 
+ xlim(-2, 2) 
+ coord_fixed(ratio=(5 / 4)) 
+ annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1) 
+ theme_classic() 
+ labs(title = "Naess Pitch Location",
       x="Horizontal Location",
       y="Vertical Location")
