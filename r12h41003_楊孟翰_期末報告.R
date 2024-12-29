# 讀取每年度的資料
library(haven)
library(dplyr)
data112 <- read_dta("D:/碩二上/計量/hw2/mu112.dta")
data111 <- read_dta("D:/碩二上/計量/hw2/mu111.dta")
data110 <- read_dta("D:/碩二上/計量/hw2/mu110.dta")
data109 <- read_dta("D:/碩二上/計量/hw2/mu109.dta")
data108 <- read_dta("D:/碩二上/計量/hw2/mu108.dta")
keep_columns <- c("b1_a", "a2", "a3", "area", "a4", "b2_a", "b2_b", "b3_y", "a5_3", "year")
# 定義前處理函數
process_data <- function(data, year) {
  data %>%
    # 添加年份資訊
    mutate(year = year) %>%
    # 篩選掉 b2_b == 0 #
    filter(b2_b != 0) %>%
    # 篩選掉 b3_y == 0
    filter(b3_y != 0) %>%
    # b1_a = 1 代表收入為負值，只篩選掉這部分資料
    filter(b1_a != 1) %>%
    # 篩選掉 b2_a == 0 #全職或打工
    filter(b2_a != 0) %>%
    # 教育程度重新編碼
    mutate(a5_3 = ifelse(a5_3 %in% c(8, 9), 1, 0)) %>%
    # 保留指定欄位
    select(all_of(keep_columns))
}

# 前處理所有年度資料
data112_clean <- process_data(data112, 112)
data111_clean <- process_data(data111, 111)
data110_clean <- process_data(data110, 110)
data109_clean <- process_data(data109, 109)
data108_clean <- process_data(data108, 108)

# 合併所有年度資料
combined_data <- bind_rows(data112_clean, data111_clean, data110_clean, data109_clean, data108_clean)

# 計算合併後的資料筆數
num_rows_combined <- nrow(combined_data)

# 印出結果
print(paste("合併後的資料筆數：", num_rows_combined))
#"合併後的資料筆數： 125351" -->  "合併後的資料筆數： 132113"

# 計算描述性統計表的函數
generate_summary_table <- function(data) {
  # 按 treatment 和 control 分組計算平均值與標準差
  summary_table <- data %>%
    group_by(a5_3) %>%
    summarise(
      `Mean Monthly Salary` = mean(b1_a, na.rm = TRUE),
      `SD Monthly Salary` = sd(b1_a, na.rm = TRUE),
      `Mean Gender` = mean(a2, na.rm = TRUE),
      `SD Gender` = sd(a2, na.rm = TRUE),
      `Mean Age` = mean(a3, na.rm = TRUE),
      `SD Age` = sd(a3, na.rm = TRUE),
      `Mean Region` = mean(area, na.rm = TRUE),
      `SD Region` = sd(area, na.rm = TRUE),
      `Mean Marital Status` = mean(a4, na.rm = TRUE),
      `SD Marital Status` = sd(a4, na.rm = TRUE),
      `Mean Employment Type` = mean(b2_a, na.rm = TRUE),
      `SD Employment Type` = sd(b2_a, na.rm = TRUE),
      `Mean Weekly Hours` = mean(b2_b, na.rm = TRUE),
      `SD Weekly Hours` = sd(b2_b, na.rm = TRUE),
      `Mean Work Years` = mean(b3_y, na.rm = TRUE),
      `SD Work Years` = sd(b3_y, na.rm = TRUE)
    ) %>%
    mutate(Group = ifelse(a5_3 == 1, "Treatment (Graduate and above)", "Control (Below Graduate)")) %>%
    select(Group, everything(), -a5_3)
  
  return(summary_table)
}

# 生成描述性統計表
summary_table2 <- generate_summary_table(combined_data)
View(summary_table2)

# 載入 ggplot2 套件
library(ggplot2)

# 繪製月收入分佈直方圖，並將 x 軸限制在 250,000 以下
ggplot(combined_data, aes(x = b1_a)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Monthly Income (Cleaned Data)",
    x = "Monthly Income (NTD)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  ) +
  scale_x_continuous(limits = c(0, 250000))  # 限制 x 軸範圍

# 繪製教育程度與月收入的關係
ggplot(data = combined_data, aes(x = factor(a5_3), y = b1_a)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +  # 散點圖
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +  # 箱型圖，隱藏異常值
  labs(
    title = "Relationship Between Education and Monthly Income (All Data)",
    x = "Education Level (0 = Below Graduate; 1 = Above Graduate)",
    y = "Monthly Income (NTD)"
  ) +
  scale_y_continuous(
    breaks = seq(0, max(combined_data$b1_a, na.rm = TRUE), by = 50000),
    limits = c(0, 250000)  # 限制 y 軸的範圍在 0 到 250,000
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )
# 計算各組中位數
median_values <- combined_data %>%
  group_by(a5_3) %>%
  summarise(Median_Monthly_Income = median(b1_a, na.rm = TRUE))

# 查看結果
print(median_values)

###########使用 matching method 來做###########################
#install.packages("MatchIt")
# 加載匹配套件
library(MatchIt)

#使用（Logistic Regression） 來計算 傾向分數（Propensity Score）
# 構建傾向分數模型 (使用 logistic regression)
ps_model <- glm(a5_3 ~ a2 + a3 + area + a4 + b2_a + b2_b + b3_y,
                data = combined_data,
                family = binomial(link = "logit"))

# 匹配    使用了最近鄰匹配法（nearest neighbor matching）
match_it <- matchit(a5_3 ~ a2 + a3 + area + a4 + b2_a + b2_b + b3_y, 
                    data = combined_data,  
                    method = "nearest", 
                    caliper = 0.2) #設置一個傾向分數的範圍限制（caliper = 0.2）
#透過 matching method 減少了很多control group 中的不合適樣本 87961
# 查看匹配結果
summary(match_it)  
#Sample Sizes:  
#  Control Treated
#All        106656   18695
#Matched     18695   18695
#Unmatched   87961       0
#Discarded       0       0

#Sample Sizes:
#  Control Treated
#All        113086   19027
#Matched     19027   19027
#Unmatched   94059       0
#Discarded       0       0
# 提取匹配後的數據
matched_data <- match.data(match_it)

# 計算 ATT
att <- with(matched_data, mean(b1_a[a5_3 == 1]) - mean(b1_a[a5_3 == 0]))
cat("ATT（平均處理效果）：", att)
# ATT（平均處理效果）： 16073.53
#install.packages("cobalt")
library(cobalt)
# 自定義變數名稱的對應表
var_names <- c(
  "distance" = "傾向分數",
  "a2" = "性別",
  "a3" = "年齡",
  "area" = "地區",
  "a4" = "婚姻狀況",
  "b2_a" = "工作性質（全職/兼職）",
  "b2_b" = "每週工作時數",
  "b3_y" = "工作年限"
)

# 使用 love.plot 並替換變數名稱
love.plot(
  match_it,
  var.names = var_names,  # 替換變數名稱
  title = "共變數平衡檢查",  # 自訂標題
  var.order = "adjusted",  # 按匹配後的平衡性排序
  stars = "raw",           # 區分 Raw Mean Differences（星號標記）
  xlab = "標準化平均差異（SMD） / 原始平均差異"  # 設置 x 軸標籤
)




# 匹配後描述性統計
generate_summary_table <- function(data) {
  # 按 treatment 和 control 分組計算平均值與標準差
  summary_table <- data %>%
    group_by(a5_3) %>%
    summarise(
      Mean_Monthly_Salary = mean(b1_a, na.rm = TRUE),
      SD_Monthly_Salary = sd(b1_a, na.rm = TRUE),
      Mean_Age = mean(a3, na.rm = TRUE),
      SD_Age = sd(a3, na.rm = TRUE),
      Mean_Weekly_Hours = mean(b2_b, na.rm = TRUE),
      SD_Weekly_Hours = sd(b2_b, na.rm = TRUE),
      Mean_Work_Years = mean(b3_y, na.rm = TRUE),
      SD_Work_Years = sd(b3_y, na.rm = TRUE)
    ) %>%
    mutate(
      Group = ifelse(a5_3 == 1, "Treatment (Graduate and above)", "Control (Below Graduate)")
    ) %>%
    select(Group, Mean_Monthly_Salary, SD_Monthly_Salary, Mean_Age, SD_Age, Mean_Weekly_Hours, SD_Weekly_Hours,Mean_Work_Years,SD_Work_Years)
  
  return(summary_table)
}

# 使用新的函數生成 summary table
matched_summary2 <- generate_summary_table(matched_data)

# 輸出結果
#print(matched_summary)
View(matched_summary2)



#異質性分析 
#分成男女兩組個別去做psm
#對比他們的結果
#或是年齡分成年輕組或是老人組


library(MatchIt)
library(dplyr)
library(cobalt)
library(knitr)

# 定義異質性分析函數：分性別進行 PSM
heterogeneity_analysis <- function(data, gender_value, gender_label) {
  # 篩選性別資料 (a2: 1 = 男, 2 = 女)
  data_gender <- data %>%
    filter(a2 == gender_value)
  
  cat("正在進行", gender_label, "組的 PSM...\n")
  
  # 建立傾向分數模型
  ps_model <- glm(a5_3 ~ a3 + area + a4 + b2_a + b2_b + b3_y,
                  data = data_gender,
                  family = binomial(link = "logit"))
  
  # 計算傾向分數並匹配
  match_it <- matchit(a5_3 ~ a3 + area + a4 + b2_a + b2_b + b3_y, 
                      data = data_gender,
                      method = "nearest", 
                      caliper = 0.2)
  
  # 匹配後的數據
  matched_data <- match.data(match_it)
  
  # 計算平均處理效果 (ATT)
  att <- with(matched_data, mean(b1_a[a5_3 == 1]) - mean(b1_a[a5_3 == 0]))
  cat(gender_label, "組的平均處理效果 (ATT)：", att, "\n")
  
  # 共變數平衡性檢查圖
  love.plot(
    match_it,
    var.names = var_names,
    title = paste(gender_label, "組的共變數平衡檢查"),
    var.order = "adjusted",
    stars = "raw",
    xlab = "標準化平均差異（SMD） / 原始平均差異"
  )
  
  # 匹配後的描述性統計表
  summary_table <- matched_data %>%
    group_by(a5_3) %>%
    summarise(
      Mean_Monthly_Salary = mean(b1_a, na.rm = TRUE),
      SD_Monthly_Salary = sd(b1_a, na.rm = TRUE),
      Mean_Age = mean(a3, na.rm = TRUE),
      SD_Age = sd(a3, na.rm = TRUE),
      Mean_Weekly_Hours = mean(b2_b, na.rm = TRUE),
      SD_Weekly_Hours = sd(b2_b, na.rm = TRUE),
      Mean_Work_Years = mean(b3_y, na.rm = TRUE),
      SD_Work_Years = sd(b3_y, na.rm = TRUE)
    ) %>%
    mutate(
      Group = ifelse(a5_3 == 1, "Treatment (Graduate and above)", "Control (Below Graduate)")
    ) %>%
    select(Group, Mean_Monthly_Salary, SD_Monthly_Salary, Mean_Age, SD_Age, Mean_Weekly_Hours, SD_Weekly_Hours, Mean_Work_Years, SD_Work_Years)
  
  return(summary_table)
}

# 分別對男性和女性進行分析
male_results <- heterogeneity_analysis(combined_data, gender_value = 1, gender_label = "男性")
female_results <- heterogeneity_analysis(combined_data, gender_value = 2, gender_label = "女性")

# 查看結果
cat("男性組的描述性統計：\n")
print(male_results)
cat("女性組的描述性統計：\n")
print(female_results)


# 美化輸出
kable(male_results, caption = "男性組的 PSM 匹配後描述性統計表")
kable(female_results, caption = "女性組的 PSM 匹配後描述性統計表")


# > male_results <- heterogeneity_analysis(combined_data, gender_value = 1, gender_label = "男性")
# 正在進行 男性 組的 PSM...
# 男性 組的平均處理效果 (ATT)： 17685.29 
# > female_results <- heterogeneity_analysis(combined_data, gender_value = 2, gender_label = "女性")
# 正在進行 女性 組的 PSM...
# 女性 組的平均處理效果 (ATT)： 12496.45 
# Table: 男性組的 PSM 匹配後描述性統計表


# > # 分別對男性和女性進行分析
#   > male_results <- heterogeneity_analysis(combined_data, gender_value = 1, gender_label = "男性")
# 正在進行 男性 組的 PSM...
# 男性 組的平均處理效果 (ATT)： 17573.92 
# > female_results <- heterogeneity_analysis(combined_data, gender_value = 2, gender_label = "女性")
# 正在進行 女性 組的 PSM...
# 女性 組的平均處理效果 (ATT)： 13785.51 
# 
# |Group                        | Mean_Monthly_Salary| SD_Monthly_Salary| Mean_Age|   SD_Age| Mean_Weekly_Hours| SD_Weekly_Hours| Mean_Work_Years| SD_Work_Years|
#   |:----------------------------|-------------------:|-----------------:|--------:|--------:|-----------------:|---------------:|---------------:|-------------:|
#   |Treatment (Below Graduate)   |            42592.42|          20772.52| 40.87460| 10.78140|          41.21643|        4.625178|        8.889955|      7.811674|
#   |Control (Graduate and above) |            60277.71|          40735.89| 40.77662| 10.64978|          41.35170|        5.338938|        8.939951|      7.775257|
#   > kable(female_results, caption = "女性組的 PSM 匹配後描述性統計表")
# 
# 
# Table: 女性組的 PSM 匹配後描述性統計表
# 
# |Group                        | Mean_Monthly_Salary| SD_Monthly_Salary| Mean_Age|    SD_Age| Mean_Weekly_Hours| SD_Weekly_Hours| Mean_Work_Years| SD_Work_Years|
#   |:----------------------------|-------------------:|-----------------:|--------:|---------:|-----------------:|---------------:|---------------:|-------------:|
#   |Treatment (Below Graduate)   |            35745.01|          13644.05| 39.13232| 10.221412|          40.46157|        3.826716|        8.786433|      7.769197|
#   |Control (Graduate and above) |            48241.46|          25558.98| 38.82009|  9.795115|          40.45306|        4.912381|        8.540366|      7.421422|



# Table2: 男性組的 PSM 匹配後描述性統計表
# 
# |Group                        | Mean_Monthly_Salary| SD_Monthly_Salary| Mean_Age|   SD_Age| Mean_Weekly_Hours| SD_Weekly_Hours| Mean_Work_Years| SD_Work_Years|
#   |:----------------------------|-------------------:|-----------------:|--------:|--------:|-----------------:|---------------:|---------------:|-------------:|
#   |Treatment (Below Graduate)   |            41765.51|          26429.30| 40.82131| 10.86620|          41.25553|        4.312631|        8.766961|      7.686728|
#   |Control (Graduate and above) |            59339.43|          41100.63| 40.65575| 10.67177|          41.37259|        5.350692|        8.881501|      7.748307|
#   > kable(female_results, caption = "女性組的 PSM 匹配後描述性統計表")
# 
# 
# Table: 女性組的 PSM 匹配後描述性統計表
# 
# |Group                        | Mean_Monthly_Salary| SD_Monthly_Salary| Mean_Age|   SD_Age| Mean_Weekly_Hours| SD_Weekly_Hours| Mean_Work_Years| SD_Work_Years|
#   |:----------------------------|-------------------:|-----------------:|--------:|--------:|-----------------:|---------------:|---------------:|-------------:|
#   |Treatment (Below Graduate)   |            33486.61|          16355.33| 39.06344| 10.22655|          40.55137|        4.120551|        8.722735|      7.791177|
#   |Control (Graduate and above) |            47272.12|          26190.87| 38.83900|  9.81808|          40.49084|        4.962977|        8.563882|      7.440886|


library(MatchIt)
library(dplyr)
library(cobalt)
library(knitr)

# 定義異質性分析函數：分年齡組進行 PSM
heterogeneity_analysis_age <- function(data, age_group_label, age_condition) {
  # 引用條件變數
  age_condition <- rlang::enquo(age_condition)
  
  # 篩選年齡組資料
  data_age_group <- data %>%
    filter(!!age_condition)
  
  cat("正在進行", age_group_label, "組的 PSM...\n")
  
  # 建立傾向分數模型
  ps_model <- glm(a5_3 ~ a2 + area + a4 + b2_a + b2_b + b3_y,
                  data = data_age_group,
                  family = binomial(link = "logit"))
  
  # 匹配
  match_it <- matchit(a5_3 ~ a2 + area + a4 + b2_a + b2_b + b3_y, 
                      data = data_age_group,
                      method = "nearest", 
                      caliper = 0.2)
  
  # 匹配後的數據
  matched_data <- match.data(match_it)
  
  # 計算平均處理效果 (ATT)
  att <- with(matched_data, mean(b1_a[a5_3 == 1]) - mean(b1_a[a5_3 == 0]))
  cat(age_group_label, "組的平均處理效果 (ATT)：", att, "\n")
  
  # 匹配後的描述性統計表
  summary_table <- matched_data %>%
    group_by(a5_3) %>%
    summarise(
      Mean_Monthly_Salary = mean(b1_a, na.rm = TRUE),
      SD_Monthly_Salary = sd(b1_a, na.rm = TRUE),
      Mean_Age = mean(a3, na.rm = TRUE),
      SD_Age = sd(a3, na.rm = TRUE),
      Mean_Weekly_Hours = mean(b2_b, na.rm = TRUE),
      SD_Weekly_Hours = sd(b2_b, na.rm = TRUE),
      Mean_Work_Years = mean(b3_y, na.rm = TRUE),
      SD_Work_Years = sd(b3_y, na.rm = TRUE)
    ) %>%
    mutate(
      Group = ifelse(a5_3 == 1, "Treatment (Graduate and above)", "Control (Below Graduate)")
    ) %>%
    select(Group, Mean_Monthly_Salary, SD_Monthly_Salary, Mean_Age, SD_Age, Mean_Weekly_Hours, SD_Weekly_Hours, Mean_Work_Years, SD_Work_Years)
  
  return(list(att = att, summary = summary_table))
}

# 分成年輕組 (<= 40) 和年長組 (> 40)
young_group <- heterogeneity_analysis_age(combined_data, "年輕組", a3 <= 40)
old_group <- heterogeneity_analysis_age(combined_data, "年長組", a3 > 40)

# 輸出結果
cat("\n年輕組的描述性統計：\n")
print(young_group$summary)
cat("\n年長組的描述性統計：\n")
print(old_group$summary)

# 美化輸出
kable(young_group$summary, caption = "年輕組的 PSM 匹配後描述性統計表")
kable(old_group$summary, caption = "年長組的 PSM 匹配後描述性統計表")

# 正在進行 年輕組 組的 PSM...
# 年輕組 組的平均處理效果 (ATT)： 9239.917 
# > old_group <- heterogeneity_analysis_age(combined_data, "年長組", a3 > 40)
# 正在進行 年長組 組的 PSM...
# 年長組 組的平均處理效果 (ATT)： 22728.29 

# Table1: 年輕組的 PSM 匹配後描述性統計表


# > # 分成年輕組 (<= 40) 和年長組 (> 40)
#   > young_group <- heterogeneity_analysis_age(combined_data, "年輕組", a3 <= 40)
# 正在進行 年輕組 組的 PSM...
# 年輕組 組的平均處理效果 (ATT)： 9572.588 
# > old_group <- heterogeneity_analysis_age(combined_data, "年長組", a3 > 40)
# 正在進行 年長組 組的 PSM...
# 年長組 組的平均處理效果 (ATT)： 23979.64 



# 
# |Group                        | Mean_Monthly_Salary| SD_Monthly_Salary| Mean_Age|   SD_Age| Mean_Weekly_Hours| SD_Weekly_Hours| Mean_Work_Years| SD_Work_Years|
#   |:----------------------------|-------------------:|-----------------:|--------:|--------:|-----------------:|---------------:|---------------:|-------------:|
#   |Treatment (Below Graduate)   |            36726.74|          16319.36| 31.89421| 5.660151|          41.02912|        4.631378|        4.918227|      3.617348|
#   |Control (Graduate and above) |            45966.66|          21769.58| 32.49206| 4.926901|          41.07724|        5.005565|        4.897996|      3.576141|
#   > kable(old_group$summary, caption = "年長組的 PSM 匹配後描述性統計表")
# 
# 
# Table: 年長組的 PSM 匹配後描述性統計表
# 
# |Group                        | Mean_Monthly_Salary| SD_Monthly_Salary| Mean_Age|   SD_Age| Mean_Weekly_Hours| SD_Weekly_Hours| Mean_Work_Years| SD_Work_Years|
#   |:----------------------------|-------------------:|-----------------:|--------:|--------:|-----------------:|---------------:|---------------:|-------------:|
#   |Treatment (Below Graduate)   |            44709.45|          34993.84| 52.12063| 7.720861|          40.96821|        4.560699|        13.97671|      8.704250|
#   |Control (Graduate and above) |            67437.74|          45423.47| 49.70712| 6.878998|          40.85559|        5.391748|        13.82713|      8.509706|



# Table2: 年輕組的 PSM 匹配後描述性統計表
# 
# |Group                        | Mean_Monthly_Salary| SD_Monthly_Salary| Mean_Age|   SD_Age| Mean_Weekly_Hours| SD_Weekly_Hours| Mean_Work_Years| SD_Work_Years|
#   |:----------------------------|-------------------:|-----------------:|--------:|--------:|-----------------:|---------------:|---------------:|-------------:|
#   |Treatment (Below Graduate)   |            35370.38|          17568.43| 31.84509| 5.696914|          41.06322|        4.664673|        4.912469|      3.617327|
#   |Control (Graduate and above) |            44942.97|          22568.96| 32.45411| 4.939175|          41.10140|        5.060274|        4.892319|      3.577233|
#   > kable(old_group$summary, caption = "年長組的 PSM 匹配後描述性統計表")
# 
# 
# Table: 年長組的 PSM 匹配後描述性統計表
# 
# |Group                        | Mean_Monthly_Salary| SD_Monthly_Salary| Mean_Age|   SD_Age| Mean_Weekly_Hours| SD_Weekly_Hours| Mean_Work_Years| SD_Work_Years|
#   |:----------------------------|-------------------:|-----------------:|--------:|--------:|-----------------:|---------------:|---------------:|-------------:|
#   |Treatment (Below Graduate)   |            42702.22|          33750.13| 52.16484| 7.791404|          40.97222|        4.649473|        13.97515|      8.755419|
#   |Control (Graduate and above) |            66681.86|          45722.78| 49.71491| 6.885784|          40.88000|        5.400955|        13.83346|      8.508665|
