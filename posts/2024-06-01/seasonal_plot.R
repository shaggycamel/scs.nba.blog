
library(nba.dataRub)
library(tidyverse)
library(patchwork)


# data --------------------------------------------------------------------

db_con <- dh_createCon("postgres")
df <- dh_getQuery(db_con, "seasonal.sql") |> 
  filter(player_name == "Nikola Jokic")


# Top Minutes Plot ------------------------------------------------------

nudge <- 5
plt_top <- ggplot(df, aes(x = season_id)) +
  geom_col(aes(y = gp, fill = "Played"), colour = "black", alpha = 0.65) +
  geom_col(aes(y = gs, fill = "Started"), colour = "black", alpha = 0.65) +
  geom_text(aes(y = gp, label = gp), nudge_y = nudge) +
  geom_text(data = filter(df, gp != gs), aes(y = gs, label = gs), nudge_y = nudge * -1) +
  scale_fill_manual(NULL, values = c("Played"="azure", "Started"="green")) +
  theme_bw() +
  theme(axis.text.x = element_blank()) + 
  labs(title = "Games Played & Started Per Season", x = NULL, y = "Game Count")


# Bottom Games Plot -------------------------------------------------------

plt_bottom <- ggplot(df, aes(x = season_id, y = min, group = player_name, label = min)) +
  geom_path(colour = "red") +
  geom_point() +
  geom_text(nudge_y = 80) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold")) +
  labs(title = "Total Minutes Per Season", x = NULL, y = "Minutes")


# Patchwork ---------------------------------------------------------------

plt_seasonal <- wrap_elements(plt_top / plt_bottom) &
  labs(title = "Nikola Jokic") &
  theme(plot.title = element_text(hjust = 0.5, size = 20))


# Cleanup -----------------------------------------------------------------

rm(list = c("plt_top", "plt_bottom", "df", "db_con"))
