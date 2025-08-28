##
library(dplyr)
library(tidyr)

# Path to your main folder
main_dir <- "Twitter Data Collection Logs/Twitter Data Collection Logs"
subfolders <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)


## Sum of tweets per handle ----
for (folder in subfolders) {

  folder_name <- basename(folder)
  files <- list.files(folder, pattern = "_collection_stats.csv$", full.names = TRUE)
  df <- files %>%
    lapply(read.csv) %>%
    bind_rows()
  df_summary <- df %>%
    group_by(handle) %>%
    summarise(num_tweets = sum(num_tweets, na.rm = TRUE), .groups = "drop")
  assign(folder_name, df_summary, envir = .GlobalEnv)
}


## Appearence in multiple elections ----

all_dfs <- mget(subfolders %>% basename(), envir = .GlobalEnv)
handles_list <- lapply(names(all_dfs), function(name) {
  data.frame(handle = all_dfs[[name]]$handle, folder = name, stringsAsFactors = FALSE)
})
all_handles <- bind_rows(handles_list)
handle_counts <- all_handles %>%
  distinct(handle, folder) %>%        
  count(handle, name = "num_folders") %>%
  filter(num_folders > 1) ## max appearance is 2 elections 


## Summary num_tweets accross all elections ---- 
handles_with_counts <- lapply(names(all_dfs), function(name) {
  df <- all_dfs[[name]]
  df$folder <- name
  df
})
long_df <- bind_rows(handles_with_counts)
wide_df <- long_df %>%
  select(handle, folder, num_tweets) %>%
  pivot_wider(names_from = folder, values_from = num_tweets, values_fill = 0)

## Completeness (share of active accounts per election for 70 days prior)
pct_complete_list <- lapply(names(all_dfs), function(name) {
  df <- all_dfs[[name]]
  total_handles <- nrow(df)
  nonzero_handles <- sum(df$num_tweets > 0, na.rm = TRUE)
  pct_complete <- nonzero_handles / total_handles * 100
  data.frame(
    election = name,
    pct_complete = pct_complete
  )
})
pct_complete <- bind_rows(pct_complete_list)