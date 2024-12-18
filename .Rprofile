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


##年度總訪客數表格 #Summarise data
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


##各年分四季參觀人次比較
# 步驟1：將月份分配到春、夏、秋、冬，並將季節設為有序類別
data_season_year <- data |>
  dplyr::mutate(
    season = dplyr::case_when(
      month %in% c(3, 4, 5) ~ "春",
      month %in% c(6, 7, 8) ~ "夏",
      month %in% c(9, 10, 11) ~ "秋",
      month %in% c(12, 1, 2) ~ "冬",
      TRUE ~ NA_character_
    ),
    season = factor(season, levels = c("春", "夏", "秋", "冬"))  # 確保季節順序為春、夏、秋、冬
  )

# 步驟2：使用 pivot_longer 進行資料轉換，將景點類別轉換為長格式
data_season_year_long <- data_season_year |>
  tidyr::pivot_longer(
    cols = matches(".*_visitors$"),
    names_to = "category",
    values_to = "visitors"
  )

# 步驟3：根據年份和季節彙總每個景點的訪客數
data_season_year_summary <- data_season_year_long |>
  dplyr::group_by(year, season) |>
  dplyr::summarize(total_visitors = sum(visitors, na.rm = TRUE), .groups = "drop")

# 步驟4：將結果轉換為寬格式，以便顯示為表格
data_season_year_table <- data_season_year_summary |>
  tidyr::pivot_wider(
    names_from = "season",
    values_from = "total_visitors"
  )

# 步驟5：計算每個季節的總和並加到表格的最後
season_totals <- data_season_year_table |>
  dplyr::summarize(across(starts_with("春"):starts_with("冬"), \(x) sum(x, na.rm = TRUE))) |>
  dplyr::mutate(year = "加總")

# 步驟6：將 `data_season_year_table` 中的 `year` 欄位轉換為字符型，並將結果附加到表格中
data_season_year_table <- data_season_year_table |>
  dplyr::mutate(year = as.character(year))

# 步驟7：將每個季節的總和附加到原表格
data_season_year_table_with_totals <- dplyr::bind_rows(data_season_year_table, season_totals)

# 查看結果
print(data_season_year_table_with_totals)


# 步驟8：繪製季節性比較圖，確保季節順序為春、夏、秋、冬
ggplot(data_season_year_summary, aes(x = season, y = total_visitors, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "各年份四季參觀人次比較",
    x = "季節",
    y = "總參觀人次",
    fill = "年份"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # 旋轉橫軸文字
    legend.position = "bottom"                        # 圖例移至下方
  )




##疫情期間與其他年份訪客數比較表格 #Comparison
data <- data |>
  dplyr::mutate(
    period = dplyr::case_when(
      year < 2020 ~ "Before COVID-19",
      year >= 2020 & year <= 2022 ~ "COVID-19",
      year > 2022 ~ "After COVID-19"
    )
  )
  #Group by summary
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
