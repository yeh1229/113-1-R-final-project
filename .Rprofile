library(tidyverse)
data <- read_csv("臺北市觀光遊憩.csv")
glimpse(data)

# Parsing
data <- data |>
  dplyr::mutate(
    年份 = stringr::str_extract(統計期, "\\d+(?=年)") |> as.integer() + 1911,  # 提取台灣年份並轉換為西元
    月份 = stringr::str_extract(統計期, "\\d+(?=月)") |> as.integer(),       # 提取月份
    統計期_parsed = paste0(年份, "-", 月份, "-01") |> lubridate::ymd(),       # 組合並解析為標準日期
    Taiwan_Year = 年份 - 1911,                                               # 台灣年份
    Month = 月份,                                                           # 月份
    Western_year = 年份,                                                    # 西元年份
    Western_date = lubridate::floor_date(統計期_parsed, "month")             # 確保日期設為月初
  )


glimpse(data)

####################################
##年度總訪客數表格 
# 按年份分組，計算每年的總參觀人次
data_yearly <- data |>
  dplyr::group_by(Western_year) |>
  dplyr::summarize(
    total_visitors = sum(
      `市立美術館參觀人次`, `市立動物園參觀人次`, `市立兒童新樂園入園人次`,
      `市立天文科學教育館參觀人次`, `士林官邸公園參觀人次`,
      `臺北自來水園區參觀人次`, `北投溫泉博物館參觀人次`, `台北探索館參觀人次`,
      `藍色水路載客人次`, na.rm = TRUE
    )
  ) |>
  dplyr::ungroup()

# 繪製年度總訪客數圖表
ggplot(data_yearly, aes(x = Western_year, y = total_visitors)) +
  geom_col(fill = "skyblue") +
  labs(
    title = "年度總訪客數",
    x = "年份",
    y = "總訪客數"
  ) +
  theme_minimal()

# 檢查結果
data_yearly

#############################################

#各年分四季參觀人次比較
# 步驟1：將月份分配到春、夏、秋、冬，並將季節設為有序類別
data_season_year <- data |>
  dplyr::mutate(
    season = dplyr::case_when(
      Month %in% c(3, 4, 5) ~ "春",
      Month %in% c(6, 7, 8) ~ "夏",
      Month %in% c(9, 10, 11) ~ "秋",
      Month %in% c(12, 1, 2) ~ "冬",
      TRUE ~ NA_character_
    ),
    season = factor(season, levels = c("春", "夏", "秋", "冬"))  # 確保季節順序為春、夏、秋、冬
  )

# 步驟2：使用 pivot_longer 進行資料轉換，將景點類別轉換為長格式
data_season_year_long <- data_season_year |>
  tidyr::pivot_longer(
    cols = matches(".*參觀人次$"),
    names_to = "category",
    values_to = "visitors"
  )

# 步驟3：根據年份和季節彙總每個景點的訪客數
data_season_year_summary <- data_season_year_long |>
  dplyr::group_by(Western_year, season) |>
  dplyr::summarize(
    total_visitors = sum(visitors, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

# 步驟4：將結果轉換為寬格式，以便顯示為表格
data_season_year_table <- data_season_year_summary |>
  tidyr::pivot_wider(
    names_from = "season",
    values_from = "total_visitors"
  )

# 步驟5：計算每個季節的總和並加到表格的最後
season_totals <- data_season_year_table |>
  dplyr::summarize(across(c("春", "夏", "秋", "冬"), \(x) sum(x, na.rm = TRUE))) |>
  dplyr::mutate(Western_year = "加總")

# 步驟6：將 `data_season_year_table` 中的 `Western_year` 欄位轉換為字符型，並將結果附加到表格中
data_season_year_table <- data_season_year_table |>
  dplyr::mutate(Western_year = as.character(Western_year))

# 步驟7：將每個季節的總和附加到原表格
data_season_year_table_with_totals <- dplyr::bind_rows(data_season_year_table, season_totals)

# 查看結果
print(data_season_year_table_with_totals)


# 步驟8：繪製季節性比較圖，確保季節順序為春、夏、秋、冬
ggplot(data_season_year_summary, aes(x = season, y = total_visitors, fill = as.factor(Western_year))) +
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


#######################

##各季節歷年來總參觀人次分布
# 明確列出景點欄位
景點欄位 <- c(
  "市立美術館參觀人次", "市立動物園參觀人次", "市立兒童新樂園入園人次",
  "市立天文科學教育館參觀人次", "士林官邸公園參觀人次", "臺北自來水園區參觀人次",
  "北投溫泉博物館參觀人次", "台北探索館參觀人次", "藍色水路載客人次"
)

# 步驟1：確保季節欄位存在並處理資料
data_season_year <- data |>
  dplyr::mutate(
    season = dplyr::case_when(
      Month %in% c(3, 4, 5) ~ "春",
      Month %in% c(6, 7, 8) ~ "夏",
      Month %in% c(9, 10, 11) ~ "秋",
      Month %in% c(12, 1, 2) ~ "冬",
      TRUE ~ NA_character_
    ),
    season = factor(season, levels = c("春", "夏", "秋", "冬"))  # 確保季節順序
  )

# 步驟2：將資料轉換為長格式
data_season_year_long <- data_season_year |>
  tidyr::pivot_longer(
    cols = all_of(景點欄位),  # 明確列出景點欄位
    names_to = "category",
    values_to = "visitors"
  )

# 步驟3：根據季節和景點彙總歷年訪客數
data_season_summary <- data_season_year_long |>
  dplyr::group_by(season, category) |>
  dplyr::summarize(
    total_visitors = sum(visitors, na.rm = TRUE),
    .groups = "drop"
  )

# 步驟4：找出每個季節最多人參觀的景點
season_top_attractions <- data_season_summary |>
  dplyr::group_by(season) |>
  dplyr::slice_max(total_visitors, n = 1)

# 查看結果
print(season_top_attractions)

# 步驟5：繪製視覺化比較
ggplot(data_season_summary, aes(x = season, y = total_visitors, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "各季節歷年來總參觀人次分布",
    x = "季節",
    y = "總參觀人次",
    fill = "景點"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # 旋轉橫軸文字
    legend.position = "bottom"                        # 圖例移至下方
  )

#######################################
##各景點疫情前、疫情期間與疫情後的訪客數變化
#疫情期間 vs 疫情前 和 疫情後 vs 疫情前

# 定義三個期間：疫情前（2019年及以前）、疫情期間（2020-2022年）、疫情後（2023年及以後）
data_covid <- data |>
  tidyr::pivot_longer(
    cols = matches(".*人次$"),  # 找到所有景點的訪客欄位
    names_to = "category",
    values_to = "visitors"
  ) |>
  dplyr::mutate(
    period = case_when(
      Western_year <= 2019 ~ "疫情前",
      Western_year >= 2020 & Western_year <= 2022 ~ "疫情期間",
      Western_year >= 2023 ~ "疫情後",
      TRUE ~ NA_character_
    )
  )

# 計算每個景點在這三個期間的每年平均訪客數
data_covid_avg <- data_covid |>
  dplyr::filter(!is.na(period)) |>
  dplyr::group_by(category, period, Western_year) |>
  dplyr::summarize(annual_visitors = sum(visitors, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(category, period) |>
  dplyr::summarize(avg_annual_visitors = mean(annual_visitors, na.rm = TRUE), .groups = "drop")

# 計算變化百分比：疫情期間和疫情後的平均年度訪客數與疫情前的比較
data_covid_impact <- data_covid_avg |>
  tidyr::pivot_wider(
    names_from = period,
    values_from = avg_annual_visitors,
    names_prefix = "avg_visitors_"
  ) |>
  dplyr::mutate(
    impact_rate_covid = (avg_visitors_疫情期間 - avg_visitors_疫情前) / avg_visitors_疫情前 * 100,
    impact_rate_post_covid = (avg_visitors_疫情後 - avg_visitors_疫情前) / avg_visitors_疫情前 * 100
  )
# 比較疫情期間和疫情後的變化百分比
data_covid_impact_long <- data_covid_impact |>
  tidyr::pivot_longer(
    cols = starts_with("impact_rate"),
    names_to = "period_comparison",
    values_to = "impact_rate"
  )

ggplot(data_covid_impact_long, aes(x = reorder(category, impact_rate), y = impact_rate, fill = period_comparison)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("red", "green"), labels = c("疫情期間變化", "疫情後變化")) +
  labs(
    title = "各景點疫情前、疫情期間與疫情後的變化比較",
    x = "景點",
    y = "變化百分比 (%)",
    fill = "變化比較"
  ) +
  theme_minimal()

