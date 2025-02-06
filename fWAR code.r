library(tidyverse)
library(arsenal)
library(baseballr)

# Player metrics: Season, team_name, xMLBAMID, PlayerNameRoute, PlayerName, 
# playerid, G, GS, IP, HR, BB, HBP, SO, WAR, IFFB
player_metrics = c("Season", "team_name", "xMLBAMID", "PlayerNameRoute",
    "PlayerName", "playerid", "G", "GS", "IP", "HR", "BB", "HBP",
    "SO", "WAR", "IFFB")

# Park metrics: season, home_team, FIP
park_metrics = c("season", "home_team", "FIP")

# Import data from FanGraphs
pitcher_data <- bind_rows(
    fg_pitcher_leaders(startseason = 2024, endseason = 2024, ind = 1),
    fg_pitcher_leaders(startseason = 2023, endseason = 2023, ind = 1),
    fg_pitcher_leaders(startseason = 2022, endseason = 2022, ind = 1))

park_data <- bind_rows(fg_park(2024), fg_park(2023), fg_park(2022))

payroll_data <- read_csv("spotrac_payroll_data.csv", show_col_types = FALSE)

mlb_data <- read_csv("fg_league_data.csv", show_col_types = FALSE)

league_data <- read_csv("fg_lg_specific_avg.csv", show_col_types = FALSE)

bullpen_data <- read_csv("fg_bullpen_data.csv", show_col_types = FALSE)

abr <- read_csv("Abbreviations.csv", show_col_types = FALSE)

# Clean up data: select useful columns, change 
# column names, correct IP, add abbreviations, etc

pitcher_data <- pitcher_data %>% 
    select(all_of(player_metrics)) %>%
    mutate(fWAR = WAR, K = SO, Team = team_name, Player = PlayerNameRoute,
        `IP/GS` = IP/GS,
        IP = trunc(IP) + (IP - trunc(IP)) / 0.3) %>%
    mutate(Team = replace(Team, Team == "- - -", "TOT")) %>%
    select(-team_name, -PlayerNameRoute, -SO, -WAR, -PlayerName, -xMLBAMID, -playerid)

avg_park_rows <- tibble(League = c("2LG", "2LG", "2LG"), 
    Team = c("TOT", "TOT", "TOT"), 
    Season = c(2024, 2023, 2022), 
    PF = c(100 , 100, 100))

park_data <- park_data %>%
    select(all_of(park_metrics)) %>%
    left_join(abr, by = c("home_team" = "Name")) %>%
    mutate(Team = ifelse(is.na(Abbreviation), home_team, Abbreviation),
        Season = season, PF = FIP) %>%
    select(-Abbreviation, -season, -FIP, -home_team)
    
park_data <- bind_rows(park_data, avg_park_rows)

# Calculate ifFIPc for each season
mlb_data <- mlb_data %>%
    mutate(ifFIPc = mlbERA - (13*mlbHR + 3*(mlbBB + mlbHBP) - 2*(mlbK + mlbIFFB)) / mlbIP)

# Calculate league-specific pFIPR9
league_data <- league_data %>%
    left_join(select(mlb_data, Year, mlbIP, ifFIPc, mlbERA, mlbRA9),
    by = c("Year" = "Year")) %>%
    mutate(lgpFIPR9 = (13*lgHR + 3*(lgBB + lgHBP) - 2*(lgK + lgIFFB)) / lgIP + ifFIPc + mlbRA9 - mlbERA)

# Calculate MLB-wide stats by year
summary_rows <- league_data %>% 
    group_by(Year) %>%
    summarize(across(.cols = c(lgERA, lgRA9, lgpFIPR9, ifFIPc, mlbERA, mlbRA9, mlbIP), .fns = mean),    
        across(.cols = c(lgBB, lgHBP, lgK, lgIFFB, lgIP, lgHR), .fns = sum)) %>%
    mutate(League = "2LG")

league_data <- bind_rows(league_data, summary_rows)

# Merge all data
pitcher_data <- pitcher_data %>%
    left_join(park_data, by = c("Team" = "Team", "Season" = "Season")) %>%
    left_join(league_data, by = c("Season" = "Year", "League" = "League"))

bullpen_data <- bullpen_data %>%
    left_join(park_data, by = c("Team" = "Team", "Year" = "Season", "League" = "League")) %>%
    left_join(league_data, by = c("Year" = "Year", "League" = "League"))

# Calculate fWAR components
pitcher_data <- pitcher_data %>%
    mutate(ifFIP = (13*HR + 3*(BB + HBP) - 2*(K + IFFB)) / IP + ifFIPc) %>%
    mutate(pFIPR9 = (ifFIP + mlbRA9 - mlbERA) / (PF / 100)) %>%
    mutate(dRPW = 1.5 * (((lgpFIPR9 * (18 - (IP/G))) + (pFIPR9 * (IP/G))) / 18 + 2)) %>%
    mutate(`pFIPR9/dRPW` = pFIPR9/dRPW) %>%
    mutate(RL = 0.03*(1-GS/G) + 0.12*(GS/G)) %>%
    mutate(calc_fWAR_raw = ((lgpFIPR9 - pFIPR9) / dRPW + RL) * (IP/9))

bullpen_data <- bullpen_data %>%
    mutate(`IP/GR` = IP/G) %>%
    mutate(ifFIP = (13*HR + 3*(BB + HBP) - 2*(K + IFFB)) / IP + ifFIPc) %>%
    mutate(bpFIPR9 = (ifFIP + mlbRA9 - mlbERA) / (PF / 100)) %>%
    mutate(dRPW = 1.5 * (((lgpFIPR9 * (18 - (`IP/GR`))) + (bpFIPR9 * (`IP/GR`))) / 18 + 2)) %>%
    mutate(`pFIPR9/dRPW` = bpFIPR9/dRPW)

# Calculate league correction and final fWAR
correction_rows <- pitcher_data %>%
    group_by(Season) %>%
    summarize(total_raw_fWAR = sum(calc_fWAR_raw, na.rm = TRUE)) %>%
    left_join(select(mlb_data, mlbIP, Year), by = c("Season" = "Year")) %>%
    mutate(WARIP = (430 - total_raw_fWAR) / mlbIP)

pitcher_data <- pitcher_data %>%
    left_join(correction_rows, by = c("Season" = "Season", "mlbIP" = "mlbIP")) %>%
    mutate(Lgc = WARIP * IP) %>%
    mutate(calc_fWAR = calc_fWAR_raw + Lgc)

# Starting pitcher filters: GS >= 7, GS/G >= 0.8
filtered <- pitcher_data %>%
    filter(GS >= 7 & GS/G >= 0.8)

# Expected replacement contribution; taken from filtered players with -0.5 <= fWAR <= 0.5
rl_IP_GS = mean(select(filter(filtered, calc_fWAR <= 0.5 & calc_fWAR >= -0.5), `IP/GS`)[[1]])

# Calculate RL, bullpen contributions, new raw fWAR
filtered <- filtered %>%
    mutate(bp_IP_GS = `IP/GS` - rl_IP_GS) %>%
    mutate(rl_contribution = ((lgpFIPR9 - pFIPR9) / dRPW + RL) * (rl_IP_GS/9) * GS) %>%
    left_join(select(bullpen_data, Year, Team, bpFIPR9), 
        by = c("Team" = "Team", "Season" = "Year")) %>%
    mutate(bp_contribution = ((bpFIPR9 - pFIPR9) / dRPW) * ((`IP/GS` - rl_IP_GS) / 9 * GS)) %>%
    mutate(new_fWAR_raw = (rl_contribution + bp_contribution))

# Calculate new fWAR using old league constant
filtered <- filtered %>%
    mutate(new_fWAR = new_fWAR_raw + Lgc) %>%
    mutate(diff_calc_fWAR = abs(calc_fWAR - new_fWAR)) %>%
    mutate(diff_fWAR = abs(fWAR - calc_fWAR))

#Correlations: IP/GS ~ pFIPR9, IP/GS ~ `pFIPR9/dRPW`, fWAR ~ calc_fWAR
pFIPR9_lm <- lm(`IP/GS` ~ pFIPR9, data = filtered)
pFIPR9_r_squared <- summary(pFIPR9_lm)$r.squared

`pFIPR9/dRPW_lm` <- lm(`IP/GS` ~ `pFIPR9/dRPW`, data = filtered)
`pFIPR9/dRPW_r_squared` <- summary(`pFIPR9/dRPW_lm`)$r.squared

calc_fWAR_lm <- lm(fWAR ~ calc_fWAR, data = filtered)
calc_fWAR_r_squared <- summary(calc_fWAR_lm)$r.squared

# Plots

# Correlation between pFIPR9 and IP/GS for filtered

# plot <- ggplot(filtered, aes(x = pFIPR9, y = `IP/GS`)) +
#   geom_point(color = "blue") +
#   geom_smooth(method = "lm", color = "red", se = TRUE) +
#   labs(title = "Correlation between IP/GS and pFIPR9, filtered",
#     x = "pFIPR9", y = "IP/GS") +
#   annotate("text", 
#     x = max(filtered$pFIPR9) - 1, y = max(filtered$`IP/GS`), 
#     label = paste("R² =", round(pFIPR9_r_squared, 3)))

# ggsave("Correlation\ between\ pFIPR9\ and\ IPGS\ filtered.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)

# Correlation between pFIPR9/dRPW and IP/GS for filtered

# plot <- ggplot(filtered, aes(x = `pFIPR9/dRPW`, y = `IP/GS`)) +
#   geom_point(color = "blue") +
#   geom_smooth(method = "lm", color = "red", se = TRUE) +
#   labs(title = "Correlation between IP/GS and pFIPR9/dRPW, filtered",
#     x = "pFIPR9/dRPW", y = "IP/GS") +
#   annotate("text", 
#     x = max(filtered$`pFIPR9/dRPW`) - 0.2, y = max(filtered$`IP/GS`), 
#     label = paste("R² =", round(`pFIPR9/dRPW_r_squared`, 3)))

# ggsave("Correlation\ between\ pFIPR9_on_dRPW\ and IPGS.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)

# Illustrative graph with averages

# mean_ip_gs <- mean(filtered$`IP/GS`, na.rm = TRUE)
# mean_ip_gs_fwar_mid <- mean(filtered$`IP/GS`[filtered$calc_fWAR >= -0.5 & filtered$calc_fWAR <= 0.5], na.rm = TRUE)
# mean_ip_gs_fwar_high <- mean(filtered$`IP/GS`[filtered$calc_fWAR > 2.8], na.rm = TRUE)

# mean_pfipr9 <- mean(filtered$pFIPR9, na.rm = TRUE)
# mean_pfipr9_mid <- mean(filtered$pFIPR9[filtered$calc_fWAR >= -0.5 & filtered$calc_fWAR <= 0.5], na.rm = TRUE)
# mean_pfipr9_high <- mean(filtered$pFIPR9[filtered$calc_fWAR > 2.8], na.rm = TRUE)
# mean_bp_pfipr9 <- mean(bullpen_data$bpFIPR9, na.rm = TRUE)

# plot <- ggplot(filtered, aes(x = pFIPR9, y = `IP/GS`)) +
#   geom_point(color = "blue") +
#   labs(title = "Relationship between IP/GS and pFIPR9 with benchmarks",
#     x = "pFIPR9", y = "IP/GS") +
#   geom_hline(yintercept = mean_ip_gs, 
#     color = "#006713", linetype = "solid", linewidth = 1) +
#   annotate("text", x = 7, y = mean_ip_gs + 0.03, 
#     label = "Average", 
#     hjust = 0, vjust = 0, size = 5, color = "#006713") + 
#   geom_hline(yintercept = mean_ip_gs_fwar_mid, 
#     color = "#006713", linetype = "dashed", linewidth = 1) +
#   annotate("text", x = 7, y = mean_ip_gs_fwar_mid + 0.03, 
#     label = "Replacement", 
#     hjust = 0, vjust = 0, size = 5, color = "#006713") + 
#   geom_hline(yintercept = mean_ip_gs_fwar_high, 
#     color = "#006713", linetype = "dotted", linewidth = 1) +
#   annotate("text", x = 7, y = mean_ip_gs_fwar_high + 0.03, 
#     label = "Elite", 
#     hjust = 0, vjust = 0, size = 5, color = "#006713") + 
#   geom_vline(xintercept = mean_pfipr9, 
#     color = "#ff0000", linetype = "solid", linewidth = 1) +
#   annotate("text", x = mean_pfipr9 + 0.3, y = 6.9, 
#     label = "Average", size = 5, color = "#ff0000", angle = 45) + 
#   geom_vline(xintercept = mean_pfipr9_mid, 
#     color = "#ff0000", linetype = "dashed", linewidth = 1) + 
#   annotate("text", x = mean_pfipr9_mid + 0.5, y = 6.9, 
#     label = "Replacement", size = 5, color = "#ff0000", angle = 45) + 
#   geom_vline(xintercept = mean_pfipr9_high, 
#     color = "#ff0000", linetype = "dotted", linewidth = 1) +
#   annotate("text", x = mean_pfipr9_high - 0.3, y = 6.9, 
#     label = "Elite", size = 5, color = "#ff0000", angle = 45) + 
#   geom_vline(xintercept = mean_bp_pfipr9, 
#     color = "#ffc400", linetype = "solid", linewidth = 1) + 
#   annotate("text", x = mean_bp_pfipr9 - 0.3, y = 6.9, 
#     label = "Bullpen", size = 5, color = "#ffc400", angle = 45)

# ggsave("Correlation\ between\ pFIPR9\ and\ IPGS\ illustrated.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)

# Correlation between fWAR and calc_fWAR

# plot <- ggplot(filtered, aes(x = fWAR, y = calc_fWAR)) +
#   geom_point(color = "blue") +
#   geom_smooth(method = "lm", color = "red", se = TRUE) +
#   labs(title = "Correlation between fWAR and calc_fWAR",
#     x = "calc_fWAR", y = "fWAR") +
#   annotate("text", x = mean(filtered$calc_fWAR), y = max(filtered$fWAR), 
#     label = paste("R² =", round(calc_fWAR_r_squared, 3)), 
#     hjust = 0, vjust = 1, size = 5, color = "black")

# ggsave("Correlation\ between\ fWAR\ and\ calc_fWAR.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)

# Difference between calc_fWAR and new_fWAR for all filtered players, 10 bins

# filtered_deciles <- filtered %>%
#   mutate(bin = ntile(calc_fWAR, 10)) %>%
#   mutate(bin = case_when(
#     bin == 1 ~ "1st",
#     bin == 2 ~ "2nd",
#     bin == 3 ~ "3rd",
#     bin == 4 ~ "4th",
#     bin == 5 ~ "5th",
#     bin == 6 ~ "6th",
#     bin == 7 ~ "7th",
#     bin == 8 ~ "8th",
#     bin == 9 ~ "9th",
#     bin == 10 ~ "10th")) %>%
#     mutate(bin = factor(bin, levels = c(
#     "10th", "9th", "8th", "7th", "6th", 
#     "5th", "4th", "3rd", "2nd", "1st")))

# plot_data <- filtered_deciles %>%
#   select(bin, calc_fWAR, new_fWAR) %>%
#   pivot_longer(cols = c(calc_fWAR, new_fWAR), names_to = "metric", values_to = "value")

# plot <- ggplot(plot_data, aes(x = factor(bin), y = value, fill = metric)) +
#   geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.7) +
#   labs(title = "Comparison of calc_fWAR and new_fWAR by Bins",
#     x = "Deciles (sorted by calc_fWAR)", y = "Mean fWAR Value", fill = "Metric") +
# theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("Difference\ between\ calc_fWAR\ and\ new_fWAR,\ deciles.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)

# Difference between calc_fWAR and new_fWAR for top 15 players by calc_fWAR

# top_15_players_fWAR <- filtered %>%
#   arrange(desc(calc_fWAR)) %>%
#   slice(1:15) %>%
#   mutate(Player_Year = paste(Player, " (", Season, ")", sep = ""))

# plot_data <- top_15_players_fWAR %>%
#   select(Player_Year, calc_fWAR, new_fWAR) %>%
#   pivot_longer(cols = c(calc_fWAR, new_fWAR), names_to = "metric", values_to = "value")

# plot <- ggplot(plot_data, aes(x = reorder(Player_Year, -value), y = value, fill = metric)) +
#   geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
#   labs(title = "Comparison of calc_fWAR and new_fWAR for Top 15 Players by calc_fWAR",
#     x = "Player (Season)", y = "fWAR Value", fill = "Metric") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("Difference\ between\ calc_fWAR\ and\ new_fWAR\ for\ top\ 15\ players\ by\ calc_fWAR.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)

# Difference between calc_fWAR and new_fWAR for top 15 players by difference

# top_15_players_diff <- filtered %>%
#   arrange(desc(diff_calc_fWAR)) %>%
#   slice(1:15) %>%
#   mutate(Player_Year = paste(Player, " (", Season, ")", sep = ""))

# plot_data <- top_15_players_diff %>%
#   select(Player_Year, calc_fWAR, new_fWAR, diff_calc_fWAR) %>%
#   pivot_longer(cols = c(calc_fWAR, new_fWAR), names_to = "metric", values_to = "value")

# plot <- ggplot(plot_data, aes(x = reorder(Player_Year, -diff_calc_fWAR), y = value, fill = metric)) +
#   geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
#   labs(title = "Comparison of calc_fWAR and new_fWAR for Top 15 Players by calc_fWAR - new_fWAR",
#     x = "Player (Season)", y = "fWAR Value", fill = "Metric") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("Difference\ between\ calc_fWAR\ and\ new_fWAR\ for\ top\ 15\ players\ by\ change.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)
