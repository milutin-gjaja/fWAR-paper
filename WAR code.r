library(tidyverse)
library(arsenal)

#WAR Calculations

setwd("C:\\Users\\milut\\Desktop\\Misc\\Various bs\\WAR calcs\\New Data")

#BRef: G, GS, IP, HR, BB, HBP, K
bref_22 <- read_csv("2022_BRef, G, GS, IP, HR, BB, HBP, K.csv", show_col_types = FALSE)
bref_23 <- read_csv("2023_BRef, G, GS, IP, HR, BB, HBP, K.csv", show_col_types = FALSE)
bref_24 <- read_csv("2024_BRef, G, GS, IP, HR, BB, HBP, K.csv", show_col_types = FALSE)

#FanGraphs: fWAR, IFFB, PF, league averages, bullpen data
pf_23 <- read_csv("2023_FanGraphs, PF.csv", show_col_types = FALSE)
fg_league_data <- read_csv("fg_league_data.csv", show_col_types = FALSE)
fg_22 <- read_csv("2022_FanGraphs, fWAR, IFFB.csv", show_col_types = FALSE)
fg_23 <- read_csv("2023_FanGraphs, fWAR, IFFB.csv", show_col_types = FALSE)
fg_24 <- read_csv("2024_FanGraphs, fWAR, IFFB.csv", show_col_types = FALSE)
bullpen_data <- read_csv("fg_bullpen_data.csv", show_col_types = FALSE)
fg_lg_spec <- read_csv("fg_lg_specific_avg.csv", show_col_types = FALSE)

#Spotrac: payroll data
payroll_data <- read_csv("spotrac_payroll_data.csv", show_col_types = FALSE)

#Standardized abbreviations
abr <- read_csv("Abbreviations.csv", show_col_types = FALSE)

#Standardize team abbreviations
bref_22 <- bref_22 %>%
    left_join(abr, by = c("Team" = "Name")) %>%
    mutate(Team = ifelse(is.na(Abbreviation), Team, Abbreviation)) %>%
    select(-Abbreviation)

bref_23 <- bref_23 %>%
    left_join(abr, by = c("Team" = "Name")) %>%
    mutate(Team = ifelse(is.na(Abbreviation), Team, Abbreviation)) %>%
    select(-Abbreviation)

bref_24 <- bref_24 %>%
    left_join(abr, by = c("Team" = "Name")) %>%
    mutate(Team = ifelse(is.na(Abbreviation), Team, Abbreviation)) %>%
    select(-Abbreviation)

pf_23 <- pf_23 %>%
    left_join(abr, by = c("Team" = "Name")) %>%
    mutate(Team = ifelse(is.na(Abbreviation), Team, Abbreviation)) %>%
    select(-Abbreviation)

#Join Bref and FG data
player_data_22 <- bref_22 %>%
    left_join(fg_22, by = c("Player" = "Name", "Team" = "Team")) %>%
    mutate(Year = 2022)

player_data_23 <- bref_23 %>%
    left_join(fg_23, by = c("Player" = "Name", "Team" = "Team")) %>%
    mutate(Year = 2023)

player_data_24 <- bref_24 %>%
    left_join(fg_24, by = c("Player" = "Name", "Team" = "Team")) %>%
    mutate(Year = 2024)

#Calc ifFIPc for each season 
fg_league_data <- fg_league_data %>%
    mutate(ifFIPc = mlbERA - (13*mlbHR + 3*(mlbBB + mlbHBP) - 2*(mlbK + mlbIFFB)) / mlbIP)

#Merge league data, calc league-specific pFIPR9, add rows for leage averages
fg_lg_spec <- fg_lg_spec %>%
    left_join(select(fg_league_data, Year, mlbIP, ifFIPc, mlbERA, mlbRA9),
    by = c("Year" = "Year")) %>%
    mutate(lgpFIPR9 = (13*lgHR + 3*(lgBB + lgHBP) - 2*(lgK + lgIFFB)) / lgIP + ifFIPc + mlbRA9 - mlbERA)
  
grouped_data <- fg_lg_spec %>%
    group_by(Year)

new_rows <- grouped_data %>%
    select(-League) %>%
    summarize(across(c(lgERA, lgRA9, lgpFIPR9, ifFIPc, mlbERA, mlbRA9, mlbIP), 
        \(x) mean(x, na.rm = TRUE)),
      across(-c(lgERA, lgRA9, lgpFIPR9, ifFIPc, mlbERA, mlbRA9, mlbIP), 
        \(x) sum(x, na.rm = TRUE))) %>%
    mutate(League = "2LG") %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

fg_lg_spec <- bind_rows(fg_lg_spec, new_rows)

#Merge all data, including lg stats and park factor
total_p_data <- bind_rows(player_data_22, player_data_23, player_data_24) %>%
    left_join(fg_lg_spec, by = c("Year" = "Year", "Lg" = "League")) %>%
    left_join(select(pf_23, Team, pfFIP), by = c("Team" = "Team")) %>%
    rename('Park Factor' = pfFIP, K = SO) %>%
    mutate(`IP/GS` = IP/GS)

total_bp_data <- bullpen_data %>%
    left_join(fg_lg_spec, by = c("Year" = "Year", "League" = "League")) %>%
    left_join(select(pf_23, Team, pfFIP), by = c("Team" = "Team")) %>%
    rename('Park Factor' = pfFIP, K = SO)

#Calc fWAR components
total_p_data <- total_p_data %>%
    mutate(ifFIP = (13*HR + 3*(BB + HBP) - 2*(K + IFFB)) / IP + ifFIPc) %>%
    mutate(pFIPR9 = (ifFIP + mlbRA9 - mlbERA) / (`Park Factor` / 100)) %>%
    mutate(dRPW = 1.5 * (((lgpFIPR9 * (18 - (IP/G))) + (pFIPR9 * (IP/G))) / 18 + 2)) %>%
    mutate(`pFIPR9/dRPW` = pFIPR9/dRPW) %>%
    mutate(RL = 0.03*(1-GS/G) + 0.12*(GS/G)) %>%
    mutate(calc_fWAR_raw = ((lgpFIPR9 - pFIPR9) / dRPW + RL) * (IP/9))

total_bp_data <- total_bp_data %>%
    mutate(`IP/GR` = IP/G) %>%
    mutate(ifFIP = (13*HR + 3*(BB + HBP) - 2*(K + IFFB)) / IP + ifFIPc) %>%
    mutate(bpFIPR9 = (ifFIP + mlbRA9 - mlbERA) / (`Park Factor` / 100)) %>%
    mutate(dRPW = 1.5 * (((lgpFIPR9 * (18 - (`IP/GR`))) + (bpFIPR9 * (`IP/GR`))) / 18 + 2)) %>%
    mutate(`pFIPR9/dRPW` = bpFIPR9/dRPW)

#Calc league correction each year/player and final fWAR
sum_fWAR_raw_season <- total_p_data %>%
    group_by(Year) %>%
    summarize(total_raw_fWAR = sum(calc_fWAR_raw, na.rm = TRUE))

payroll_data <- payroll_data %>%
    left_join(sum_fWAR_raw_season, by = "Year") %>%
    left_join(select(fg_league_data, Year, mlbIP), by = "Year") %>%
    mutate(WARIP = (P_WAR - total_raw_fWAR) / mlbIP)

total_p_data <- total_p_data %>%
    left_join(select(payroll_data, -`Total payroll`, -`Pitcher payroll`, -`Fielder Payroll`), 
        by = c("Year" = "Year")) %>%
    mutate(Lgc = WARIP * IP) %>%
    mutate(calc_fWAR = calc_fWAR_raw + Lgc)

#Starting pitcher filters: GS >= 7, GS/G >= 0.8
filtered_sp <- total_p_data %>%
    filter(GS >= 7 & GS/G >= 0.8)

#Expected replacement contribution; taken from filtered players with -0.5 <= fWAR <= 0.5
rl_IP_GS = mean(select(filter(filtered_sp, fWAR <= 0.5 & fWAR >= -0.5), `IP/GS`)[[1]])
filtered_sp <- filtered_sp %>%
    mutate(bp_IP_GS = `IP/GS` - rl_IP_GS)

#Calculate RL, bullpen contributions, new raw fWAR
filtered_sp <- filtered_sp %>%
    mutate(rl_contribution = ((lgpFIPR9 - pFIPR9) / dRPW + RL) * (rl_IP_GS/9) * GS) %>%
    left_join(select(total_bp_data, Year, Team, bpFIPR9), 
        by = c("Team" = "Team", "Year" = "Year")) %>%
    mutate(bp_contribution = (bpFIPR9 - pFIPR9) / dRPW * (`IP/GS` - rl_IP_GS) / 9 * GS) %>%
    mutate(new_fWAR_raw = (rl_contribution + bp_contribution))

#At this point, there are two options: calculate new_fWAR using the old Lgc,
#or calculate nLgc separately and use that for new_fWAR. Both methods have flaws.

#Calculate nLgc method

# total_p_data <- total_p_data %>%
#     left_join(select(filtered_sp, Player, Year, new_fWAR_raw), by = c("Player" = "Player", "Year" = "Year")) %>%
#     mutate(new_fWAR_raw = coalesce(new_fWAR_raw, calc_fWAR_raw))

# sum_new_fWAR_raw_season <- total_p_data %>%
#     group_by(Year) %>%
#     summarize(total_new_raw_fWAR = sum(new_fWAR_raw, na.rm = TRUE))

# payroll_data <- payroll_data %>%
#     left_join(sum_new_fWAR_raw_season, by = "Year") %>%
#     mutate(new_WARIP = (P_WAR - total_new_raw_fWAR) / mlbIP)

# total_p_data <- total_p_data %>%
#     left_join(select(payroll_data, Year, new_WARIP), by = c("Year" = "Year")) %>%
#     mutate(nLgc = new_WARIP * IP) %>%
#     mutate(new_fWAR = new_fWAR_raw + nLgc)

# filtered_sp <- filtered_sp %>%
#     inner_join(select(total_p_data, Player, Year, new_fWAR), 
#       by = c("Player" = "Player", "Year" = "Year")) %>%
#     mutate(diff_calc_fWAR = abs(calc_fWAR - new_fWAR)) %>%
#     mutate(diff_fWAR = abs(fWAR - calc_fWAR))

#Use Lgc method

filtered_sp <- filtered_sp %>%
    mutate(new_fWAR = new_fWAR_raw + Lgc) %>%
    mutate(diff_calc_fWAR = abs(calc_fWAR - new_fWAR)) %>%
    mutate(diff_fWAR = abs(fWAR - calc_fWAR))

#Correlations: IP/GS ~ pFIPR9, IP/GS ~ `pFIPR9/dRPW`, fWAR ~ calc_fWAR
pFIPR9_lm <- lm(`IP/GS` ~ pFIPR9, data = filtered_sp)
`pFIPR9/dRPW_lm` <- lm(`IP/GS` ~ `pFIPR9/dRPW`, data = filtered_sp)
calc_fWAR_lm <- lm(fWAR ~ calc_fWAR, data = filtered_sp)




# Graphs

setwd("C:\\Users\\milut\\Desktop\\Misc\\Various bs\\WAR calcs\\New Data\\Graphs")

# Correlation between pFIPR9 and IP/GS for filtered

# pFIPR9_r_squared <- summary(pFIPR9_lm)$r.squared

# plot <- ggplot(filtered_sp, aes(x = pFIPR9, y = `IP/GS`)) +
#   geom_point(color = "blue") +
#   geom_smooth(method = "lm", color = "red", se = TRUE) +
#   labs(title = "Correlation between IP/GS and pFIPR9, filtered",
#     x = "pFIPR9", y = "IP/GS") +
#   annotate("text", 
#     x = max(filtered_sp$pFIPR9) - 1, y = max(filtered_sp$`IP/GS`), 
#     label = paste("R² =", round(pFIPR9_r_squared, 3)), 
#     hjust = 0, vjust = 1, size = 5, color = "black") 

# ggsave("Correlation\ between\ pFIPR9\ and\ IPGS\ filtered.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)

# # Correlation between pFIPR9/dRPW and IP/GS for filtered

# wpg_r_squared <- summary(`pFIPR9/dRPW_lm`)$r.squared

# plot <- ggplot(filtered_sp, aes(x = `pFIPR9/dRPW`, y = `IP/GS`)) +
#   geom_point(color = "blue") +
#   geom_smooth(method = "lm", color = "red", se = TRUE) +
#   labs(title = "Correlation between IP/GS and pFIPR9/dRPW, filtered",
#     x = "pFIPR9/dRPW", y = "IP/GS") +
#   annotate("text", 
#     x = max(filtered_sp$`pFIPR9/dRPW`) - 0.2, y = max(filtered_sp$`IP/GS`), 
#     label = paste("R² =", round(wpg_r_squared, 3)), 
#     hjust = 0, vjust = 1, size = 5, color = "black")
#   geom_hline(yintercept = mean_ip_gs, 
#     color = "#006713", linetype = "solid", linewidth = 1) +
#   geom_hline(yintercept = mean_ip_gs_fwar_mid, 
#     color = "#006713", linetype = "dashed", linewidth = 1) +
#   geom_hline(yintercept = mean_ip_gs_fwar_high, 
#     color = "#006713", linetype = "dotted", linewidth = 1)

# ggsave("Correlation\ between\ pFIPR9_on_dRPW\ and IPGS.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)

# Illustrative graph with averages

# mean_ip_gs <- mean(filtered_sp$`IP/GS`, na.rm = TRUE)
# mean_ip_gs_fwar_mid <- mean(filtered_sp$`IP/GS`[filtered_sp$calc_fWAR >= -0.5 & filtered_sp$calc_fWAR <= 0.5], na.rm = TRUE)
# mean_ip_gs_fwar_high <- mean(filtered_sp$`IP/GS`[filtered_sp$calc_fWAR > 2.8], na.rm = TRUE)

# mean_pfipr9 <- mean(filtered_sp$pFIPR9, na.rm = TRUE)
# mean_pfipr9_mid <- mean(filtered_sp$pFIPR9[filtered_sp$calc_fWAR >= -0.5 & filtered_sp$calc_fWAR <= 0.5], na.rm = TRUE)
# mean_pfipr9_high <- mean(filtered_sp$pFIPR9[filtered_sp$calc_fWAR > 2.8], na.rm = TRUE)
# mean_bp_pfipr9 <- mean(total_bp_data$bpFIPR9, na.rm = TRUE)

# plot <- ggplot(filtered_sp, aes(x = pFIPR9, y = `IP/GS`)) +
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

# # Correlation between fWAR and calc_fWAR

# fWAR_r_squared <- summary(calc_fWAR_lm)$r.squared

# plot <- ggplot(filtered_sp, aes(x = fWAR, y = calc_fWAR)) +
#   geom_point(color = "blue") +
#   geom_smooth(method = "lm", color = "red", se = TRUE) +
#   labs(title = "Correlation between fWAR and calc_fWAR",
#     x = "calc_fWAR", y = "fWAR") +
#   annotate("text", x = mean(filtered_sp$calc_fWAR), y = max(filtered_sp$fWAR), 
#     label = paste("R² =", round(fWAR_r_squared, 3)), 
#     hjust = 0, vjust = 1, size = 5, color = "black")

# # ggsave("Correlation\ between\ fWAR\ and\ calc_fWAR.png", 
# #     plot = plot, width = 8, height = 6, dpi = 300)

# # Difference between calc_fWAR and new_fWAR for all filtered players, 10 bins

# filtered_sp <- filtered_sp %>%
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

# plot_data <- filtered_sp %>%
#   select(bin, calc_fWAR, new_fWAR) %>%
#   pivot_longer(cols = c(calc_fWAR, new_fWAR), names_to = "metric", values_to = "value")

# plot <- ggplot(plot_data, aes(x = factor(bin), y = value, fill = metric)) +
#   geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.7) +
#   labs(title = "Comparison of calc_fWAR and new_fWAR by Bins",
#     x = "Deciles (sorted by calc_fWAR)", y = "Mean fWAR Value", fill = "Metric") +
# theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("Difference\ between\ calc_fWAR\ and\ new_fWAR,\ deciles.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)

# # Difference between calc_fWAR and new_fWAR for top 15 players by calc_fWAR

# top_15_players_fWAR <- filtered_sp %>%
#   arrange(desc(calc_fWAR)) %>%
#   slice(1:15) %>%
#   mutate(Player_Year = paste(Player, " (", Year, ")", sep = ""))

# plot_data <- top_15_players_fWAR %>%
#   select(Player_Year, calc_fWAR, new_fWAR) %>%
#   pivot_longer(cols = c(calc_fWAR, new_fWAR), names_to = "metric", values_to = "value")

# plot <- ggplot(plot_data, aes(x = reorder(Player_Year, -value), y = value, fill = metric)) +
#   geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
#   labs(title = "Comparison of calc_fWAR and new_fWAR for Top 15 Players by calc_fWAR",
#     x = "Player (Year)", y = "fWAR Value", fill = "Metric") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# # ggsave("Difference\ between\ calc_fWAR\ and\ new_fWAR\ for\ top\ 15\ players\ by\ calc_fWAR.png", 
# #     plot = plot, width = 8, height = 6, dpi = 300)

# # Difference between calc_fWAR and new_fWAR for top 20 players by difference

# top_15_players_diff <- filtered_sp %>%
#   arrange(desc(diff_calc_fWAR)) %>%
#   slice(1:15) %>%
#   mutate(Player_Year = paste(Player, " (", Year, ")", sep = ""))

# plot_data <- top_15_players_diff %>%
#   select(Player_Year, calc_fWAR, new_fWAR, diff_calc_fWAR) %>%
#   pivot_longer(cols = c(calc_fWAR, new_fWAR), names_to = "metric", values_to = "value")

# plot <- ggplot(plot_data, aes(x = reorder(Player_Year, -diff_calc_fWAR), y = value, fill = metric)) +
#   geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
#   labs(title = "Comparison of calc_fWAR and new_fWAR for Top 15 Players by calc_fWAR - new_fWAR",
#     x = "Player (Year)", y = "fWAR Value", fill = "Metric") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("Difference\ between\ calc_fWAR\ and\ new_fWAR\ for\ top\ 15\ players\ by\ change.png", 
#     plot = plot, width = 8, height = 6, dpi = 300)

# # # Additional: correlation screenshots, stats between calc_fWAR and new_fWAR