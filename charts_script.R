##
## PIT ATR decade facet
##
ggplot(data = hist_taxable_income, aes(x = income, y = real_atr, colour = as.character(year_ending))) +
  geom_line() + 
  facet_wrap(vars(decade), nrow = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::dollar) +
  theme_light() + 
  labs(
    y = "",
    x = "",
    title = "Average personal income tax rates",
    subtitle = "1983-84 to 2016-17",
    caption = "Source: Tax Framework Division"
  ) + 
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80", linetype = "dashed"),
    axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
    legend.position = "none"
  )

##
## TFT real and nominal over time
##
ggplot(data = tft_history_ato_web_scrape, aes(x = years, y = `TFT Threshold`, colour = measure)) + 
  geom_point() + 
  geom_text(aes(x = 5, y = 14000), label = "TFT\n(real terms)", colour = "#440154FF") +
  geom_text(aes(x = 4, y = 7000), label = "TFT\n(nominal terms)", colour = "#21908CFF") +
  scale_y_continuous(labels = scales::dollar) +
  theme_light() + 
  labs(
    y = "",
    x = ""
  ) + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.4),
    legend.position = "none"
  ) + 
  scale_colour_manual(values=c("#21908CFF", "#440154FF"))

##
## Count of taxpayers by bracket 
##
indiv_tax_stats %>%
  filter(income_year == "2016–17") %>%
  group_by(bracket) %>%
  tally(number_of_individuals_no, name = "tax_payers") %>%
  ggplot(aes(x = bracket, y = tax_payers, fill = bracket)) +
  geom_col() +
  geom_text(
    aes(label = format(tax_payers, big.mark = ","), colour = bracket, y = tax_payers + 10000),
    vjust = -0.5
  ) +
  scale_y_continuous(scales::comma, expand = expand_scale(mult = c(0, 0.2))) +
  labs(
    x = "",
    y = "",
    title = "",
    caption = "Source: Tax Stats (2017) - Individuals table 2B"
  ) +
  theme_light() +
  theme(
    legend.position = 'none',
    panel.grid =  element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.6),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "grey80")
  )


##
## TFT effect on ATR (with/without) 
##
avg_rates_data %>%
  select(
    income,
    ATR_with_tft,
    ATR_without_tft
  ) %>%
  gather(key = ATR, value = value, - income) %>%
  mutate(ATR_label = case_when(income == 100001 & ATR == "ATR_with_tft" ~ "with TFT",
                               income == 125001 & ATR == "ATR_without_tft" ~ "without TFT",
                               TRUE ~ NA_character_)
  ) %>%
  ggplot(aes(x = income, y = value, colour = ATR)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expand_scale(c(0, 0.1))) +
  scale_x_continuous(labels = scales::dollar, expand = expand_scale(c(0, 0))) +
  theme_light() +
  labs(
    x = "",
    y = "",
    title = "",
    caption = "Source: Tax Framework Division"
  ) +
  theme(
    legend.position = 'none'
  ) +
  geom_text(aes(x = 120000, y = 0.325), label = "Without TFT", size = 3, colour = "#440154FF") +
  geom_text(aes(x = 150000, y = 0.25), label = "With TFT", size = 3, colour = "#21908CFF") +
  scale_colour_manual(values=c("#21908CFF", "#440154FF"))

##
## Revenue gains by bracket - replace 0% with 19% 
##
indiv_tax_stats %>%
  group_by(income_year, bracket) %>%
  summarise(
    "Total Additional Tax" = sum(total_pit_tft_diff/1000000000, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = bracket, y = `Total Additional Tax`, fill = bracket)) +
  geom_col() +
  geom_text(
    aes(label  = format(round(`Total Additional Tax`, 1), big.mark = ","), 
        colour = bracket, fontface = "bold", y = `Total Additional Tax`),
    vjust  = 0.25,
    hjust  = -0.3,
    size   = 3,
    angle  = 90
  ) +
  facet_grid(cols = vars(income_year)) +
  scale_y_continuous(
    labels = scales::dollar,
    expand = expand_scale(mult = c(0, 0.2)),
    breaks = c(10, 15, 20, 25)
  ) +
  labs(
    x = "",
    y = "",
    title = "Removing the Tax Free Threshold",
    subtitle = "Additional revenue ($bn) collected by tax bracket over period 2010–11 to 2016–17",
    caption = "Source: Tax Stats (2017) - Individuals table 2B"
  ) +
  theme_light() +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1, size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_text(size = 8),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(size = 8)
  )

## 
## OECD TFT members
##
oecd_tft_members %>% 
  ggplot(aes(x = income, y = atr, colour = country)) + 
  geom_line() + 
  geom_text_repel(aes(label = label),
                  nudge_x = 0,
                  direction = "y",
                  hjust = 0,
                  segment.size = 0.2,
                  na.rm = TRUE) +
  scale_colour_discrete(guide = 'none')  +    
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expand_scale(c(0, 0.1))) +
  scale_x_continuous(labels = scales::dollar, expand = expand_scale(c(0, 0.11))) +
  theme_light() +
  labs(
    title = "OECD TFT club members",
    subtitle = "Average Tax Rates (AUD terms)",
    caption = "Source: Tax Framework Division",
    y = ""
  )


## 
## OECD non-TFT members
##
oecd_tft_non_members %>% 
  ggplot(aes(x = income, y = atr, colour = country)) + 
  facet_wrap(vars(prog_or_not)) +
  geom_line() + 
  geom_text_repel(aes(label = label),
                  nudge_x = 0,
                  direction = "y",
                  hjust = 0,
                  segment.size = 0.2,
                  na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expand_scale(c(0, 0.1))) +
  scale_x_continuous(labels = scales::dollar, expand = expand_scale(c(0, 0.11))) +
  theme_light() +
  labs(
    title = "No TFT for us... OECD countries without TFT's",
    subtitle = "Average Tax Rates (AUD terms)",
    caption = "Source: Tax Framework Division",
    y = "",
    x = ""
  ) + 
  scale_fill_manual(values = mycolors) +
  theme(
    legend.position = "none"
  )
