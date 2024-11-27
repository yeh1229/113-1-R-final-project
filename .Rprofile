library(tidyverse)
data <- read_csv("臺北市政府主管人員性別統計表11310.csv")
glimpse(gdp_data)

# Parsing
`臺北市政府主管人員性別統計表11310` <- `臺北市政府主管人員性別統計表11310` %>%
  mutate(
    性別 = factor(性別, levels = c(1, 2), labels = c("Male", "Female")),
    性別比率 = parse_number(性別比率) / 100 # Convert to proportion
  )
glimpse(gdp_data)

# Finding dataset
filtered_data <- 臺北市政府主管人員性別統計表11310 %>%
  dplyr::filter(主管人數 > 2000)

# Summarising data
summary_data <- 臺北市政府主管人員性別統計表11310 %>%
  group_by(性別) %>%
  summarise(
    total_主管人數 = sum(主管人數, na.rm = TRUE),
    avg_性別比率 = mean(性別比率, na.rm = TRUE)
  )

# Comparison
ggplot(臺北市政府主管人員性別統計表11310, aes(x = 性別, y = 主管人數)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of 主管人數 by 性別", x = "Gender", y = "Number of Managers")
glimpse(gdp_data)
