source("01_wrangle.R")
library(gghdx)
gghdx()

##################################################
#### GENERAL LOOK AT DISPLACEMENT PREDICTIONS ####
##################################################

p_displacement <- scenarios_list$adm_0 %>%
  mutate(
    date = dmy(paste("15", as.numeric(month) + 4, "2023"))
  ) %>%
  ggplot(
    aes(
      x = date,
      y = displacement
    )
  ) +
  geom_line(
    aes(
      group = scenario
    ),
    color = "black"
  ) +
  geom_text(
    data = data.frame(
      date = as.Date("2023-10-26"),
      displacement = c(3270000, 1850000, 120000),
      label = c("Worst case", "Status quo", "Best case")
    ),
    mapping = aes(
      label = label
    ),
    fontface = "bold",
    color = "black"
  ) +
  expand_limits(
    y = 0
  ) +
  scale_y_continuous_hdx(
    labels = scales::label_comma()
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    x = "",
    y = "Total IDPS",
    title = "IDP forecasting scenarios in Sudan",
    subtitle = "3 scenarios were developed immediately following the April 2023 crisis"
  )

p_displacement

# add in data that we have collected from DTM

df_dtm_disp_overall <- df_dtm_disp %>%
  group_by(date) %>%
  summarize(
    displacement = sum(idps)
  ) %>%
  mutate(
    percent_change = (displacement - lag(displacement)) / lag(displacement),
    days = as.numeric(date - lag(date)),
    percent_change_daily = (1 + percent_change)^(1/days) - 1
  )

p_displacement$layers[[1]]$aes_params$colour <- hdx_hex("gray-medium")
p_displacement$layers[[2]]$aes_params$colour <- hdx_hex("gray-medium")

p_displacement +
  geom_line(
    data = df_dtm_disp_overall,
    color = hdx_hex("tomato-hdx"),
    linewidth = 1.5
  ) +
  geom_text(
    data = data.frame(
      date = as.Date("2023-06-10"),
      displacement = 2000000,
      label = "Reported displacement (DTM)"
    ),
    mapping = aes(
      label = label
    ),
    angle = 40,
    fontface = "bold",
    color = hdx_hex("tomato-hdx"),
    size = 4
  )

##########################################
#### LOOK AT % CHANGE IN DISPLACEMENT ####
##########################################

df_scen_growth <- data.frame(
  percent_change_daily = c(
    (1 + (0.7)^12-1)^(1/365) - 1,
    (1 + (1.2)^12-1)^(1/365) - 1,
    (1 + (1.35)^12-1)^(1/365)-1
  ),
  label = c("Best case", "Status quo", "Worst case")
)

df_dtm_disp_overall %>%
  ggplot(
    aes(
      x = date,
      y = percent_change_daily
    )
  ) +
  geom_hline(
    data = df_scen_growth,
    mapping = aes(
      yintercept = percent_change_daily
    ),
    color = hdx_hex("gray-dark")
  ) +
  geom_text(
    data = df_scen_growth,
    mapping = aes(
      y = percent_change_daily + 0.002,
      label = label
    ),
    x = as.Date("2023-05-01")
  ) +
  geom_line(
    color = hdx_hex("tomato-hdx"),
    linewidth = 1.5
  ) +
  labs(
    x = "",
    y = "Approximate daily growth rate (%)",
    title = "Observed daily growth rate in displacement since the crisis began"
  ) +
  scale_y_continuous(
    breaks = c(0, df_scen_growth$percent_change_daily),
    labels = scales::label_percent()
  )

#########################################
#### PERCENT CHANGE OF REFUGEES DATA ####
#########################################

# first look at raw figures

df_unhcr %>%
  ggplot(
    aes(
      x = date,
      y = refugees
    )
  ) +
  geom_line(
    color = hdx_hex("tomato-hdx")
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    x = "",
    y = "Refugees",
    title = "Sudanese refugees displaced since April 15"
  )

# now look at percent change

p_growth_refugees <- df_unhcr %>%
  mutate(
    daily_percent_change = (refugees - lag(refugees)) / lag(refugees)
  ) %>%
  ggplot(
    aes(
      x = date,
      y = daily_percent_change
    )
  ) +
  geom_line(
    color = hdx_hex("tomato-hdx")
  ) +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  labs(
    x = "",
    y = "Approximate daily growth rate (%)",
    title = "Daily growth rate in Sudanese refugee populations"
  )

p_growth_refugees

# and look just at the last dates

p_growth_refugees +
  scale_x_date(
    limits = as.Date(c("2023-08-01", "2023-08-14"))
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    limits = c(0, 0.015)
  )

##########################
#### LOOK AT CONFLICT ####
##########################

df_acled_summary <- df_acled %>%
  mutate(
    event_date = as.Date(event_date)
  ) %>%
  group_by(
    event_date
  ) %>%
  summarize(
    num_events = n(),
    fatalities = sum(fatalities),
    .group = "drop"
  )

# number of events
df_acled_summary %>%
  ggplot(
    aes(
      x = event_date,
      y = num_events
    )
  ) +
  geom_smooth(
    color = hdx_hex("tomato-hdx"),
    fill = hdx_hex("tomato-light")
  ) +
  labs(
    x = "",
    y = "# of events (daily)",
    title = "Number of reported conflict events per day from ACLED for all of 2023"
  )

# number of fatalities
df_acled_summary %>%
  ggplot(
    aes(
      x = event_date,
      y = fatalities
    )
  ) +
  geom_smooth(
    color = hdx_hex("tomato-hdx"),
    fill = hdx_hex("tomato-light")
  ) +
  labs(
    x = "",
    y = "# of fatalities (daily)",
    title = "Number of reported fatalities per day from ACLED for all of 2023"
  )

####################################################
#### PERCENT CHANGE DISPLACEMENT GEOGRAPHICALLY ####
####################################################

df_state_disp <- df_dtm_disp %>%
  group_by(
    ADM1_PCODE,
    date
  ) %>%
  summarize(
    idps = sum(idps, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    percent_change = (idps - lag(idps)) / lag(idps),
    percent_change_annual = (1 + percent_change)^12 - 1,
    percent_change_daily = (1 + percent_change_annual)^(1/365) - 1
  ) %>%
  left_join(
    df_adm1 %>%
      select(
        ADM1_PCODE,
        ADM1_EN
      )
  )

p_disp_state <- df_state_disp %>%
  ggplot(
    aes(
      x = date,
      y = idps,
      group = ADM1_PCODE
    )
  ) +
  geom_line(
    color = hdx_hex("tomato-hdx"),
    alpha = 0.2
  ) +
  labs(
    x = "",
    y = "IDP population",
    title = "IDP population growth by state in Sudan"
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  )

p_disp_state

# look at states that have unique patterns

p_disp_state +
  geom_line(
    data = filter(df_state_disp, ADM1_EN %in% c("East Darfur", "River Nile")),
    color = hdx_hex("tomato-hdx"),
    linewidth = 1.1
  ) +
  geom_text(
    data = filter(
      df_state_disp,
      ADM1_EN %in% c("East Darfur", "River Nile"),
      date == max(date)
    ) %>%
      mutate(
        date = date + days(7)
      ),
    mapping = aes(
      label = ADM1_EN
    ),
    fontface = "bold",
    color = hdx_hex("tomato-hdx")
  ) +
  coord_cartesian(
    clip = "off"
  )

# look at states that have seen decreases recently

p_disp_state +
  geom_line(
    data = filter(df_state_disp, ADM1_EN %in% c("West Darfur", "Northern")),
    color = hdx_hex("tomato-hdx"),
    linewidth = 1.1
  ) +
  geom_text(
    data = filter(
      df_state_disp,
      ADM1_EN %in% c("West Darfur", "Northern"),
      date == max(date)
    ) %>%
      mutate(
        date = date + days(7)
      ),
    mapping = aes(
      label = ADM1_EN
    ),
    fontface = "bold",
    color = hdx_hex("tomato-hdx")
  ) +
  coord_cartesian(
    clip = "off"
  )

##################################################
#### POPULATIONS RELATIVE TO PRECRISIS LEVELS ####
##################################################

df_idp_pre %>%
  group_by(
    ADM1_PCODE,
    state
  ) %>%
  summarize(
    idps_precrisis = sum(idps)
  ) %>%
  left_join(
    df_state_disp %>%
      filter(date == max(date))
  ) %>%
  mutate(
    idps_pct_pre = idps / idps_precrisis
  ) %>%
  ggplot(
    aes(
      y = fct_reorder(as.factor(state), idps_pct_pre),
      x = idps_pct_pre
    )
  ) +
  geom_bar(
    stat = "identity",
    fill = hdx_hex("tomato-hdx")
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    y = "",
    x = "IDPs as % of pre-crisis IDPS",
    title = "IDPs in latest situation report from DTM relative to pre-crisis IDP population levels"
  )

#############################
#### POTENTIAL SCENARIOS ####
#############################

df_scenarios <- df_dtm_disp_overall %>%
  bind_rows(
    data.frame(
      date = as.Date(
        c(
          "2023-09-10",
          "2023-10-10",
          "2023-11-10",
          "2023-12-10",
          "2024-01-10",
          "2024-02-10"
        )
      ),
      scenario_1_change = jitter(650000 * (1:6)),
      scenario_2_change = cumsum(jitter(
        c(
          650000,
          575000,
          475000,
          350000,
          200000,
          50000
        )
      )),
      scenario_3_change = cumsum(jitter(
        c(
          650000,
          500000,
          200000,
          0,
          -100000,
          -200000
        )
      )
    ))
  )


df_scenarios_final <- df_scenarios %>% mutate(
    scenario_1 = ifelse(
      is.na(scenario_1_change),
      displacement,
      last(displacement[!is.na(displacement)]) + scenario_1_change
    ),
    scenario_2 = ifelse(
      is.na(scenario_2_change),
      displacement,
      last(displacement[!is.na(displacement)]) + scenario_2_change
    ),
    scenario_3 = ifelse(
      is.na(scenario_3_change),
      displacement,
      last(displacement[!is.na(displacement)]) + scenario_3_change
    )
  ) %>%
  slice(
    16:22
  )


df_scenarios_final %>%
  ggplot(
    aes(
      x = date
    )
  ) +
  geom_line(
    data = df_dtm_disp_overall,
    mapping = aes(
      y = displacement
    ),
    color = "black"
  ) +
  geom_line(
    mapping = aes(
      y = scenario_1
    ),
    linewidth = 1.2,
    color = hdx_hex("tomato-hdx")
  ) +
  geom_line(
    mapping = aes(
      y = scenario_2
    ),
    linewidth = 1.2,
    color = hdx_hex("tomato-light")
  ) +
  geom_line(
    mapping = aes(
      y = scenario_3
    ),
    linewidth = 1.2,
    color = hdx_hex("mint-hdx")
  ) +
  geom_text(
    data = data.frame(
      date = as.Date("2024-02-25"),
      y = c(4200000, 5800000, 7500000),
      label = c("Reduction", "Leveling", "Linear")
    ),
    mapping = aes(
      y = y,
      label = label
    ),
    hjust = 0.5,
    fontface = "bold"
  ) +
  labs(
    x = "",
    y = "IDPs",
    title = "Example IDP scenarios for the updated HRP Sudan"
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  )
