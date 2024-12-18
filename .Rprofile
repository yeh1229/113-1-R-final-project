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

###########################不同資料分隔線#############################
######################################################################

library(tidyverse)
data <- read_csv("臺北市觀光遊憩.csv")
glimpse(data)

# Parsing
library(tidyverse)
library(lubridate)
data <- data |>
  dplyr::mutate(
    Taiwan_Year = lubridate::year(統計期) - 1911,  # Convert Western year to Taiwan year
    Month = lubridate::month(統計期),              # Extract month
    Western_year = lubridate::year(統計期),       # Western year (same as original year)
    Western_date = lubridate::floor_date(統計期, "month")  # Ensure day is set to the 1st
  )

glimpse(data)

##visualize and pivot
colnames(data)
data_long <- data %>%
  pivot_longer(cols = contains("visitors"), 
               names_to = "category", 
               values_to = "visitors")
glimpse(data_long)
ggplot(data_long, aes(x = 統計期, y = visitors, color = category)) +
  geom_line() +
  labs(title = "各景點參觀人次隨時間變化", x = "統計期", y = "參觀人次", color = "景點") +
  theme_minimal()

##年度總訪客數表格
data_yearly <- data |>
  dplyr::group_by(year) |>
  dplyr::summarize(total_visitors = sum(
    art_museum_visitors, zoo_visitors, children_park_visitors,
    astronomy_education_visitors, shilin_residence_visitors,
    water_park_visitors, hot_spring_museum_visitors, discovery_center_visitors,
    blue_water_route_visitors, na.rm = TRUE
  ))

ggplot(data_yearly, aes(x = year, y = total_visitors)) +
  geom_col(fill = "skyblue") +
  labs(title = "年度總訪客數", x = "年份", y = "總訪客數")

data_yearly <- data |>
  dplyr::group_by(year) |>
  dplyr::summarize(total_visitors = sum(
    art_museum_visitors, zoo_visitors, children_park_visitors,
    astronomy_education_visitors, shilin_residence_visitors,
    water_park_visitors, hot_spring_museum_visitors, discovery_center_visitors,
    blue_water_route_visitors, na.rm = TRUE
  ))

data_yearly


##疫情期間與其他年份訪客數比較表格
data <- data |>
  dplyr::mutate(
    period = dplyr::case_when(
      year < 2020 ~ "Before COVID-19",
      year >= 2020 & year <= 2022 ~ "COVID-19",
      year > 2022 ~ "After COVID-19"
    )
  )

data_period <- data |>
  dplyr::group_by(period) |>
  dplyr::summarize(
    total_visitors = sum(
      art_museum_visitors, zoo_visitors, children_park_visitors,
      astronomy_education_visitors, shilin_residence_visitors,
      water_park_visitors, hot_spring_museum_visitors, discovery_center_visitors,
      blue_water_route_visitors, na.rm = TRUE
    )
  )

ggplot(data_period, aes(x = period, y = total_visitors, fill = period)) +
  geom_col() +
  labs(title = "疫情期間與其他年份訪客數比較", x = "時期", y = "總訪客數")

##不同場所訪客數在疫情期間的變化表格
data_location <- data |>
  dplyr::group_by(period) |>
  dplyr::summarize(across(
    ends_with("_visitors"), sum, na.rm = TRUE
  ))

data_location_long <- tidyr::pivot_longer(
  data_location,
  cols = -period,
  names_to = "location",
  values_to = "visitors"
)

ggplot(data_location_long, aes(x = location, y = visitors, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "不同場所訪客數在疫情期間的變化", x = "場所", y = "訪客數")
