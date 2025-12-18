# Save fall data from local device

folder_path <- "/Users/gaberiedel/Downloads/Fall-25"

data <- read_csv(folder_path)

View(data)

files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

all_data <- files %>%
  map_dfr(read_csv)

write.csv(all_data, "all_fall_25.csv", row.names=FALSE)

write.csv(cp_df, "cp_df.csv", row.names=FALSE)
