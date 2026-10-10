
library(tidyverse) 
library(forcats)
library(here)

stat_cols <- c("min", "fgm", "fga", "fg3_m", "fg3_a", "ftm", "fta", "pts", "pf", "reb", "ast", "stl", "blk", "tov")
anl_cols <- c("min", "fg_p", "fg3_p", "ft_p", "pts", "pf", "reb", "ast", "stl", "blk", "tov")

df_anl <- read.csv(here("data", "df_cluster.csv"))
df_anl <- filter(df_anl, str_detect(player_name, "Jokic")) |> 
  mutate(.cluster = player_name) |> 
  bind_rows(df_anl)


# TTEST -------------------------------------------------------------------

db_con <- dh_createCon("postgres")
df_raw <- dh_getQuery(db_con, "game_log.sql")

df_anova <- df_raw |> 
  inner_join(
    select(df_anl, player_name, season_id, ix, .cluster) |> 
      filter(str_detect(.cluster, "Cluster")),
    by = join_by(player_name, slug_season == season_id)
  ) |> 
  mutate(.cluster = if_else(str_detect(player_name, "Jokic"), player_name, .cluster)) |> 
  # mutate(across(all_of(stat_cols), \(x) x / min)) |> 
  nest_by(ix)


df_anova <- map(stat_cols, \(x){
  
  col_name <- sym(paste0(x, "_tp"))
  jok_mean <- sym(paste0(x, "_jok"))
  clust_mean <- sym(paste0(x, "_cluster2"))
  clust_qnt25 <- sym(paste0(x, "_qnt25_cluster2"))
  clust_qnt75 <- sym(paste0(x, "_qnt75_cluster2"))
  
  df_anova |> 
    mutate(!!col_name := t.test(
      x = filter(data, str_detect(.cluster, "Jokic")) |> pull(!!sym(x)),
      y = filter(data, !str_detect(.cluster, "Jokic")) |> pull(!!sym(x))
    )$p.value) |> 
    mutate(
      !!jok_mean := filter(data, str_detect(.cluster, "Jokic")) |> pull(!!sym(x)) |> mean(),
      !!clust_mean := filter(data, !str_detect(.cluster, "Jokic")) |> pull(!!sym(x)) |> mean(),
      !!clust_qnt25 := filter(data, !str_detect(.cluster, "Jokic")) |> 
        summarise(mean = mean(!!sym(x)), .by = player_name) |> 
        pull(mean) |> 
        quantile(0.25),
      !!clust_qnt75 := filter(data, !str_detect(.cluster, "Jokic")) |> 
        summarise(mean = mean(!!sym(x)), .by = player_name) |> 
        pull(mean) |> 
        quantile(0.75)
    ) |>
    ungroup() |> 
    select(starts_with(x))
  
}) |> 
  bind_cols() |> 
  bind_cols(df_anova) |> 
  select(ix, matches("_tp|_jok|_clust"))

df_pre_plt <- df_anova |> 
  pivot_longer(ends_with("_tp"), names_to = "stat", values_to = "p.value") |> 
  pivot_longer(matches("_jok|_clust"), values_to = "mean") |>
  mutate(stat = str_remove(stat, "_tp")) |> 
  filter(str_detect(name, stat)) |> 
  mutate(name = str_remove(name, paste0(stat, "_"))) |> 
  pivot_wider(names_from = name, values_from = mean, names_prefix = "mean_") |>
  relocate(p.value, .after= last_col()) |> 
  mutate(significant = if_else(p.value < 0.05, 1, NA)) |> 
  mutate(significant = if_else(mean_jok < mean_cluster2, significant * -1, significant)) |> 
  mutate(diff = abs(mean_jok - mean_cluster2))


# PLOT --------------------------------------------------------------------

plt_jok_trend <- map(set_names(stat_cols), \(x){
  
  df_plt <- filter(df_pre_plt, stat == x)

  ggplot(df_plt, aes(x = ix)) +
    geom_point(aes(y = mean_jok, colour = "Nikola Jokic")) +
    geom_point(aes(y = mean_cluster2, colour = "Cluster2")) +
    geom_path(aes(y = mean_jok, colour = "Nikola Jokic")) +
    geom_path(aes(y = mean_cluster2, colour = "Cluster2")) +
    geom_ribbon(aes(ymin = mean_qnt25_cluster2, ymax = mean_qnt75_cluster2), alpha = 0.2, fill = "grey70") +
     geom_path(
      aes(x = ix, y = mean, colour = NULL, group = ix),
      data = filter(df_plt, !is.na(significant)) |> 
        select(1:4) |> 
        pivot_longer(starts_with("mean"), values_to = "mean"),
      arrow = arrow(angle = 25, length = unit(0.08, "inches"), ends = "both", type = "closed"),
      linetype = "dashed",
      show.legend = FALSE
    ) +
    geom_label(
      aes(x = ix, y = min_mean + (diff / 2.2), label = round(diff, 2), colour = NULL),
      data = filter(df_plt, !is.na(significant)) |> 
        mutate(min_mean = min(mean_jok, mean_cluster2), .by = ix),
      fill = "lightgreen",
      show.legend = FALSE
    ) +
    scale_x_discrete(limits = factor(1:max(df_plt$ix))) +
    theme_bw() +
    theme(legend.title = element_blank(), plot.caption = element_text(hjust = 0)) +
    labs(
      title = "Jokic vs Cluster 2",
      subtitle = paste("Mean", str_to_upper(x), "per Game"),
      x = "Years in NBA",
      y = paste("Mean", str_to_upper(x)),
      caption = substitute(paste(italic("Dashed line indicates the significant difference, line absence indicates no significant difference was found.")))
    )
})

plt_jok_trend
# rm(list = str_subset(objects(), "plt_", negate = TRUE))