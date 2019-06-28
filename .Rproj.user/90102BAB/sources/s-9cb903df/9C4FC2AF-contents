oecd_tft_members <- filter(oecd_PIT_liability, income == 8001, atr == 0) %>% 
  select(country) %>% 
  left_join(oecd_PIT_liability, by = c("country" = "country")) 

p_oecd_tft_members <-oecd_tft_members  %>% 
  ggplot(aes(x = income, y = atr, colour = country)) + 
  geom_line() + 
  geom_text_repel(aes(label = label),
                   nudge_x = 0,
                   direction = "y",
                   hjust = 0,
                   segment.size = 0.2,
                   size = 2.7,
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


oecd_tft_non_members <- filter(oecd_PIT_liability, income == 8001, atr != 0) %>% 
  select(country) %>% 
  left_join(oecd_PIT_liability, by = c("country" = "country"))
  

non_prog_non_tft <- oecd_tft_non_members %>% 
  group_by(country) %>% 
  mutate(
    change = atr / lag(atr)
  ) %>% 
  filter(
    income == 18001,
    change == 1
  ) %>% 
  select(
    country
  ) %>% 
  .[[1]]


prog_non_tft <- oecd_tft_non_members %>% 
  group_by(country) %>% 
  mutate(
    change = atr / lag(atr)
  ) %>% 
  filter(
    income == 18001,
    change != 1
  ) %>% 
  select(
    country
  ) %>% 
  .[[1]]

oecd_tft_non_members <- oecd_tft_non_members %>% 
  mutate(
    prog_or_not = case_when(
      country %in% non_prog_non_tft ~ "Flatter PIT",
      TRUE ~ "Progressive PIT"
    )
  )

nb.cols <- 22
mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)

p_oecd_tft_non_members <- oecd_tft_non_members %>% 
  ggplot(aes(x = income, y = atr, colour = country)) + 
  facet_wrap(vars(prog_or_not)) +
  geom_line() + 
  geom_text_repel(aes(label = label),
                  nudge_x = 0,
                  direction = "y",
                  hjust = 0,
                  segment.size = 0.2,
                  na.rm = TRUE,
                  size = 2.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expand_scale(c(0, 0.1))) +
  scale_x_continuous(labels = scales::dollar, expand = expand_scale(c(0, 0.11))) +
  theme_light() +
  labs(
    title = "No TFT for us... OECD countries without TFT's",
    subtitle = "Average Tax Rates - AUD terms",
    caption = "Source: Tax Framework Division",
    y = ""
  ) + 
  scale_fill_manual(values = mycolors) +
  theme(
    legend.position = "none"
  )


oecd_tft <- plot_grid(p_oecd_tft_members, p_oecd_tft_non_members, ncol = 1, 
          align = 'v', axis = 'l') 
