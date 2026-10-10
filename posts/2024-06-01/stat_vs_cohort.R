
library(nba.dataRub)
library(tidyverse)
library(tidyclust)
library(tidymodels)


# data --------------------------------------------------------------------

db_con <- dh_createCon("postgres")
df_raw <- dh_getQuery(db_con, "seasonal.sql")
interest_stats <- c("ast", "blk", "fg3_m", "fgm", "ftm", "pts", "reb", "stl", "tov", "pf")

df <- slice_max(df_raw, season_id, n = 1, by = player_name)


# Recipe ------------------------------------------------------------------

df_rec <- recipe(~ ., data = df) |>
  step_select(player_name, all_of(interest_stats)) |>
  update_role(player_name, new_role = "id") |>
  step_normalize(all_numeric_predictors())

# bake(prep(df_rec), new_data = NULL)
  

# Clustering --------------------------------------------------------------

# k_spec <- k_means(num_clusters = tune())


# Workflow ----------------------------------------------------------------

# wflow <- workflow() |>
#   add_recipe(df_rec) |>
#   add_model(k_spec)


# Grid --------------------------------------------------------------------

# grid <- tibble(num_clusters = 3:10)
# folds <- vfold_cv(df, v = 5)


# Tuning ------------------------------------------------------------------

# res <- tune_cluster(
#   wflow,
#   resamples = folds,
#   grid = grid
# )


# Cluster plot ------------------------------------------------------------

# collect_metrics(res) |>
#   filter(.metric == "sse_within_total") |>
#   ggplot(aes(x = num_clusters, y = mean)) +
#   geom_line() +
#   geom_point() +
#   theme_bw()


# Final -------------------------------------------------------------------

set.seed(123)
mod_fit <- workflow() |> 
  add_model(k_means(num_clusters = 5) ) |> 
  add_recipe(df_rec) |> 
  fit(df)


# Assignment --------------------------------------------------------------

df <- bind_cols(df, extract_cluster_assignment(mod_fit))


# Jokic Cluster -----------------------------------------------------------

df_anl <- na.omit(df_raw) |> 
  left_join(
    select(df, player_name, .cluster),
    by = join_by(player_name),
    relationship = "many-to-many"
  ) |>
  arrange(player_name, season_id) |> 
  mutate(ix = row_number(player_name), .by = player_name)

j_clus <- (filter(df_anl, str_detect(player_name, "Jokic")))$.cluster
j_ix <- max((filter(df_anl, str_detect(player_name, "Jokic")))$ix)


# Avg calc ----------------------------------------------------------------

min_cols <- c("fgm", "fga", "fg3_m", "fg3_a", "ftm", "fta", "pts", "pf", "reb", "ast", "stl", "blk", "tov")
anl_cols <- c("min", "fga", "fg3_m", "ft", "pts", "pf", "reb", "ast", "stl", "blk", "tov")

df_t <- map(min_cols, \(x){
  s <- sym(x)
  mutate(df_anl, !!s := !!s/ min) |> 
    select(!!s)
})

df_anl <- df_anl |>  
  select(season_id, player_name, ix, min, .cluster) |> 
  bind_cols(list_cbind(df_t)) |> 
  mutate(
    fg_p = fgm / fga,
    ft_p = ftm / fta,
    fg3_p = fg3_m / fg3_a
  )


plt_jok_coh <- map(set_names(c("min", min_cols)), \(x){
  var <- sym(x)
  
  filter(df_anl, str_detect(player_name, "Jokic")) |>
    ggplot(aes(x = ix, y = !!var)) + 
    geom_boxplot(
      aes(x = ordered(ix), y = !!var, fill = .cluster), 
      filter(df_anl, !str_detect(player_name, "Jokic")),
      outliers = FALSE
    ) +
    geom_path(aes(group = 1), colour = "red") +
    geom_point(colour = "red", line = "black") +
    scale_x_discrete(limits = 1:max(df_anl$ix)) +
    theme_bw() +
    guides(fill = FALSE) +
    facet_grid(cols = vars(.cluster)) +
    labs(
      title = paste("Season", str_to_upper(x), "Distribution: Jokic vs Clusters"), 
      subtitle = "Across Years", 
      x = "Years in NBA", 
      y = str_to_upper(x),
      caption = substitute(paste(italic("Nikola Jokic in Cluster_2")))
    ) +
    theme(plot.caption = element_text(hjust = 0))
})

plt_jok_coh[1]


df_anl <- filter(df_anl, ix <= j_ix)
df_anl <- filter(df_anl, .cluster == j_clus)
write_csv(df_anl, here("data", "df_cluster.csv"))


# rm(list = str_subset(objects(), "plt_", negate = TRUE))
