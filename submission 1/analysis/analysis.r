if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, modelsummary, kableExtra, broom, cobalt, fixest)

final.data <- read_tsv('data/output/acs_medicaid.txt') %>%
  mutate(
    year = as.numeric(as.character(year)),  # Ensure 'year' is numeric
    private = (ins_employer + ins_direct) / adult_pop,
    public = (ins_medicare + ins_medicaid) / adult_pop,
    insured = (adult_pop - uninsured) / adult_pop,
    unins = uninsured / adult_pop,
    employer = ins_employer / adult_pop,
    medicaid = ins_medicaid / adult_pop,
    medicare= ins_medicare / adult_pop,
    direct = ins_direct / adult_pop
  ) %>%
  filter(!State %in% c("Puerto Rico", "District of Columbia"))

# q1
DirectPurchasePlot <- final.data %>% group_by(year) %>% summarize(mean=mean(direct)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_minimal() +
  labs(x="Year", y="Direct Purchase total proportion", title="Share of Direct Purchase Insurance over Time") +
  geom_vline(xintercept=2013.5, color="#00a261")

  print (DirectPurchasePlot)

medicaid_plot <- final.data %>% group_by(year) %>% summarize(mean=mean(medicaid)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_minimal() +
  labs(x="Year", y="Medicaid total proportion", title="Share of Medicaid Insurance over Time") +
  geom_vline(xintercept=2013.5, color="green")

ins.plot.dat <- final.data %>%
  filter(is.na(expand_year) | expand_year == 2014) %>%
  group_by(expand_ever, year) %>%
  summarize(mean_uninsured = mean(unins), .groups = 'drop')

  print (medicaid_plot)

# q4
UnInsurancePlot <- ggplot(ins.plot.dat, aes(x=year, y=mean_uninsured, group=expand_ever, linetype=expand_ever)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  geom_vline(xintercept=2013.5, color="green") +
  geom_text(
    data = ins.plot.dat %>% filter(year == 2016), 
    aes(label = ifelse(expand_ever, "Expansion", "Non-expansion"), 
        x = year + 1, 
        y = mean_uninsured),
    hjust = -0.1 
  ) +
  guides(linetype = "none") +
  labs(
    x = "Year",
    y = "Uninsured Total fraction",
    title = "Share of Uninsured over Time"
  )
  print(UnInsurancePlot)
#did-q5&6
DiD.dt <- final.data %>%
  filter(is.na(expand_year) | expand_year == 2014) %>%
  filter(year %in% c(2012, 2015)) %>%
  group_by(expand_ever, year) %>%
  summarize(mean_uninsured = mean(unins), .groups = 'drop')

DiD.tb <- pivot_wider(DiD.dt, names_from = "year", names_prefix = "year", values_from = "mean_uninsured") %>%
  ungroup() %>%
  mutate(expand_ever = case_when(expand_ever == FALSE ~ 'Non-expansion', expand_ever == TRUE ~ 'Expansion')) %>%
  rename(Group = expand_ever, Pre = year2012, Post = year2015)
  print(DiD.tb)

Medicaid.Dt <- read_tsv("data/output/acs_medicaid.txt")
reg.data <- Medicaid.Dt %>%
  filter((expand_year == 2014 | is.na(expand_year)) & !is.na(expand_ever)) %>%
  mutate(
    unins = uninsured / adult_pop,
    post = (year >= 2014),
    treat = post * expand_ever
  )

#q6&7
DiD.reg1 <- lm(unins ~ post + expand_ever + post * expand_ever, data = reg.data)

modelsummary(DiD.reg1)

DiD.reg2 <- lm(unins ~ post + expand_ever + treat, data = reg.data)

twfe <- feols(unins ~ treat | State + year, data = reg.data)

modelsummary(list("DD" = DiD.reg2, "TWFE" = twfe))

# q8
reg.data.all <- Medicaid.Dt %>%
  filter(!is.na(expand_ever)) %>%
  mutate(
    unins = uninsured / adult_pop,
    post = (year >= 2014),
    treat = post * expand_ever
  )
DiD.reg3 <- lm(unins ~ post + expand_ever + treat, data = reg.data.all)
twfe2 <- feols(unins ~ treat | State + year, data = reg.data.all)
fe.est2 <- feols(unins~treat | State + year, data=reg.data.all)

modelsummary(list("DD" = DiD.reg3, "TWFE" = twfe2,"Time Variance" = fe.est2))

#q9
mod.twfe <- feols(unins ~ i(year, expand_ever, ref = 2013) | State + year, cluster = ~State, data = reg.data)

#messimesssimessi(10)
reg.data2 <- reg.data %>%
  mutate(
    time_to_treat = ifelse(expand_ever == TRUE, year - expand_year, -1),
    time_to_treat = ifelse(time_to_treat <= -4, -4, time_to_treat)
  )
mod.twfe2 <- feols(unins ~ i(time_to_treat, expand_ever, ref = -1) | State + year, cluster = ~State, data = reg.data2)

save.image("submission 1/hw5_workspace.Rdata")
