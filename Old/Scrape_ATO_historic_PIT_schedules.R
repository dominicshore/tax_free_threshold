## The PIT schedule that is scraped later in this script provides nominial values
## The Household Consumption Deflator has been chosen to convert historical PIT rates to current prices 
# Downloads yearly implicit price deflators (5204.0 - Table 4)
target_def = "https://www.abs.gov.au/AUSSTATS/ABS@ARCHIVE.NSF/log?openagent&5204004_expenditure_on_gdp_ipd.xls&5204.0&Time%20Series%20Spreadsheet&D94B4121516C4490CA258331000C3B8A&0&2017-18&26.10.2018&Latest"
dest_def = '6204_table_4_def.xls'

download.file(url = target_def, destfile = dest_def, mode='wb')

household_exp_deflator <- read_excel(path = "./6204_table_4_def.xls", sheet = "Data1", skip = 9) %>% 
  clean_names() %>% 
  select(series_id, a2420886c) %>% 
  rename(year = series_id, hh_exp_def = a2420886c)

household_exp_deflator <- household_exp_deflator[as.numeric(strftime(household_exp_deflator$year, "%Y")) %in% 2018:1984,] %>% 
  mutate(
    joining_year = substring(.$year, 0, 4),
    hh_exp_def = hh_exp_def/100
  )
##

## Scrapping the data on historical tax rates from the ATO website
## Mutates the PIT schedule data to include inflation adjusted PIT threshold amounts
##
url <- "https://www.ato.gov.au/Rates/Individual-income-tax-for-prior-years/"
pit_sch <- url %>%
  read_html() %>%
  html_table() %>%
  setNames(., url %>%
             read_html() %>%
             html_nodes("caption") %>%
             html_text()) %>% 
  map(.%>%
    mutate(`Tax on this income` = gsub(",", "", `Tax on this income`), 
            cumm_tax_amt = str_extract(`Tax on this income`, "(?<=^\\$)\\d+") %>% as.numeric(), 
            tax_rate = str_extract(`Tax on this income`, "\\d+.(\\d+)?(?=(\\s+)?c)") %>% as.numeric(), 
            threshold = str_extract(`Tax on this income`, "(?<=\\$)\\d+$") %>% as.numeric()
           )
    ) %>%
  map(~drop_na(.x, threshold)) %>% 
  map(function(x) { mutate_each(x, funs(replace(., is.na(.), 0))) }) %>%
  #
  # Mutation of dataset to include inflation adjusted thresholds begins here
  #
  bind_rows(., .id = "id") %>% 
  mutate(
    id  = str_replace(.$id, ".*(\\d{2})\\d{2}-(\\d{2})", "\\1\\2") %>% recode(., "192000" = "2000")
  ) %>% 
  nest(-id) %>% 
  left_join(household_exp_deflator, by = c("id" = "joining_year")) %>% 
  select(-year) %>% 
  unnest() %>% 
  mutate(
    real_threshold = threshold / hh_exp_def,
    real_cumm_tax_amt = cumm_tax_amt / hh_exp_def
  ) %>% 
  split(.$id)
##

## Establishing an income variable against which taxable income would be calculated
income <- seq(from = 1, to = 100000, by = 100)

#Define a function for calculating taxable income each year based upon nomninal thresholds
nominial_tax_calc <- function(data, income) {
  i <-tail(which(income >= data[, 7]), 1)
  if (length(i) > 0) 
    return(tibble(income_N = income, 
                  tax_N = as.numeric((income - data[i, 7]) * (data[i, 6] / 100) + data[i, 5])))
  else
    return(tibble(income_N = income, tax_N = 0))
}
# 

# As above but for for thresholds calculated in inflation adjusted (real) terms
real_tax_calc <- function(data, income) {
  i <-tail(which(income >= data[, 8]), 1)
  if (length(i) > 0) 
    return(tibble(income_R = income, 
                  tax_R = as.numeric((income - data[i, 8]) * (data[i, 6]/100) + data[i, 9])))
  else
    return(tibble(income_R = income, tax_R = 0))
}
##

## Applying above functions to the PIT schdule data scrapped from the ATO website (above)
# Tax payable based on nominal thresholds 
nom_tax_payable <- map(pit_sch,~map_df(income, nominial_tax_calc, data = .)) %>% 
  bind_rows(.id = 'id')

# As above, based on real thresholds
rel_tax_payable <- map(pit_sch,~map_df(income, real_tax_calc, data = .)) %>% 
  bind_rows(.id = 'id')
##

## Join real and nominial datasets then mutate to calculate nominal and real average tax rates 
## before assigning the relevant decade to each obs
hist_taxable_income <- left_join(rel_tax_payable, nom_tax_payable, by = c("id" = "id", "income_R" = "income_N")) %>% 
  mutate(
    real_atr = tax_R / income,
    nominal_atr = tax_N / income
    ) %>% 
  rename(year_ending = id, income = income_R) %>% 
  nest(-year_ending) %>% 
  arrange(desc(year_ending)) %>% 
  mutate(
    decade = c(rep("2010's", times = 7), rep("2000's", times = 10), rep("1990's", times = 10), rep("1980's", times = 7))
  ) %>% 
  unnest()
##

## Ploting
p1_real_atr_single_chart <- ggplot(data = hist_taxable_income, aes(x = income, y = real_atr, colour = year_ending)) +
  geom_line() + 
  #facet_wrap(vars(decade)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::dollar) +
  theme_light() + 
  labs(
    y = "",
    x = "",
    title = "Down, down. Prices are down...",
    subtitle = "Average personal income tax rates 1983-84 to 2016-17",
    caption = "Source: Tax Framework Division"
  ) + 
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80", linetype = "dashed"),
    axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1), 
    legend.title = element_blank()
  )

p2_real_atr_decade_chart <- ggplot(data = hist_taxable_income, aes(x = income, y = real_atr, colour = year_ending)) +
  geom_line() + 
  facet_wrap(vars(decade)) +
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
    axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)
  )


