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

##########
##########

library(tidyverse)
data <- read_csv("臺北市觀光遊憩.csv")
glimpse(data)

# Parsing
library(tidyverse)
library(lubridate)

data <- data %>%
  mutate(
    統計期 = str_replace_all(統計期, "\\s", ""),           # 移除空白
    統計期 = str_replace(統計期, "年", "-"),              # 替換 "年" 為 "-"
    統計期 = str_replace(統計期, "月", ""),              # 移除 "月"
    統計期 = str_replace(統計期, "^(\\d+)-", function(x) {
      western_year <- as.numeric(str_extract(x, "\\d+")) + 1911
      str_replace(x, "^\\d+", as.character(western_year))
    }),
    統計期 = ym(統計期)                                    # 解析日期
  )
glimpse(data)
colnames(data)

summary(data)

##comparison
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
