df <- read_csv(file = "TABLE_I1_13062019043818530.csv") %>% 
  clean_names() %>% 
  filter(centgov_rates != "PERS_ALL_AMNT", centgov_rates != "TAX_CRED_AMNT", centgov_rates != "SURTAX_RATE", year == 2018) %>% 
  select(country, centgov_rates, value) %>% 
  arrange(country, centgov_rates) %>% 
  separate(centgov_rates, sep = "_", into = c("number", "type")) %>% 
  spread(type, value) %>% 
  group_by(country) 


## Downloads three digit country codes to match with FX data
## 
country_codes <- read_html("https://www.nationsonline.org/oneworld/country_code_list.htm") %>%
  html_table(fill = TRUE)
  
country_codes <- as_tibble(country_codes[[1]]) %>% 
  rename("test" = "X1", "full_name" = "X2", "cnt_code" = "X4") %>% 
  select(full_name, cnt_code) %>% 
  filter(full_name != cnt_code)

df_1 <- left_join(df, country_codes, by = c("country" = "full_name")) %>% 
  mutate(cnt_code = case_when(is.na(cnt_code) == T ~ "USA", TRUE ~ cnt_code))

from <- unique(df_1$cnt_code)[1:10]
to <- rep_len("USD", length.out = length(from))
getQuote(paste0(from, to, "=X"))
