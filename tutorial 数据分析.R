setwd ("C:/Users/20736/Desktop/")
m_d <- read.csv("Manchester_daily.csv")
m_d_TTN_mar_may <- subset(m_d, year == 2019 & ((month == 3 & day >= 23) | month == 4 | month == 5))
TTN_m_m_so2 <- m_d_TTN_mar_may$so2
m_d_TTT_mar_may <- subset(m_d, year == 2020 & ((month == 3 & day >= 23) | month == 4 | month == 5))
TTT_m_m_so2 <- m_d_TTT_mar_may$so2
summary(TTN_m_m_so2)
summary(TTT_m_m_so2)
t.test(TTN_m_m_so2, TTT_m_m_so2)
wilcox.test(TTN_m_m_so2, TTT_m_m_so2)
TTN_m_m_no <- m_d_TTN_mar_may$no
TTT_m_m_no <- m_d_TTT_mar_may$no
summary(TTN_m_m_no)
summary(TTT_m_m_no)
t.test(TTN_m_m_no, TTT_m_m_no)
wilcox.test(TTN_m_m_no, TTT_m_m_no)

TTN_m_m_no2 <- m_d_TTN_mar_may$no2
TTT_m_m_no2 <- m_d_TTT_mar_may$no2
summary(TTN_m_m_no2)
summary(TTT_m_m_no2)
t.test(TTN_m_m_no2, TTT_m_m_no2)
wilcox.test(TTN_m_m_no2, TTT_m_m_no2)

TTN_m_m_pm2.5 <- m_d_TTN_mar_may$pm2.5
TTT_m_m_pm2.5 <- m_d_TTT_mar_may$pm2.5
summary(TTN_m_m_pm2.5)
summary(TTT_m_m_pm2.5)
t.test(TTN_m_m_pm2.5, TTT_m_m_pm2.5)
wilcox.test(TTN_m_m_pm2.5, TTT_m_m_pm2.5)

TTN_m_m_o3 <- m_d_TTN_mar_may$o3
TTT_m_m_o3 <- m_d_TTT_mar_may$o3
summary(TTN_m_m_o3)
summary(TTT_m_m_o3)
t.test(TTN_m_m_o3, TTT_m_m_o3)
wilcox.test(TTN_m_m_o3, TTT_m_m_o3)

TTN_m_m_ws <- m_d_TTN_mar_may$ws
TTT_m_m_ws <- m_d_TTT_mar_may$ws
summary(TTN_m_m_ws)
summary(TTT_m_m_ws)
t.test(TTN_m_m_ws, TTT_m_m_ws)
wilcox.test(TTN_m_m_ws, TTT_m_m_ws)

TTN_m_m_wd <- m_d_TTN_mar_may$wd
TTT_m_m_wd <- m_d_TTT_mar_may$wd
summary(TTN_m_m_wd)
summary(TTT_m_m_wd)
t.test(TTN_m_m_wd, TTT_m_m_wd)
wilcox.test(TTN_m_m_wd, TTT_m_m_wd)

TTN_m_m_temp <- m_d_TTN_mar_may$temp
TTT_m_m_temp <- m_d_TTT_mar_may$temp
summary(TTN_m_m_temp)
summary(TTT_m_m_temp)
t.test(TTN_m_m_temp, TTT_m_m_temp)
wilcox.test(TTN_m_m_temp, TTT_m_m_temp)



m_d_TTN_feb_mar <- subset(m_d, year == 2019 & (month == 2 | month == 3 & day <= 22))
m_d_TTT_feb_mar <- subset(m_d, year == 2020 & (month == 2 | month == 3 & day <= 22))


m_d_TTN_jun_jul <- subset(m_d, year == 2019 & (month == 6 | month == 7))
m_d_TTT_jun_jul <- subset(m_d, year == 2020 & (month == 6 | month == 7))


data <- rbind(
  data.frame(year = 2019, period = "pre", so2 = m_d_TTN_feb_mar$so2),
  data.frame(year = 2020, period = "pre", so2 = m_d_TTT_feb_mar$so2),
  data.frame(year = 2019, period = "during", so2 = TTN_m_m_so2),
  data.frame(year = 2020, period = "during", so2 = TTT_m_m_so2),
  data.frame(year = 2019, period = "post", so2 = m_d_TTN_jun_jul$so2),
  data.frame(year = 2020, period = "post", so2 = m_d_TTT_jun_jul$so2)
)

 
library(fixest)


data$lockdown <- ifelse(data$period == "during", 1, 0)


did_model <- feols(so2 ~ lockdown:year | period + year, data = data)
summary(did_model)

install.packages("forecast", dependencies = TRUE)
library(forecast)


ts_data_2019 <- ts(m_d_TTN_mar_may$so2, start=c(2019, 3, 23), frequency=365)
ts_data_2020 <- ts(m_d_TTT_mar_may$so2, start=c(2020, 3, 23), frequency=365)


fit_2019 <- auto.arima(ts_data_2019)
fit_2020 <- auto.arima(ts_data_2020)


forecast_2019 <- forecast(fit_2019, h=60)
forecast_2020 <- forecast(fit_2020, h=60)


plot(forecast_2019, main="SO2 Levels Forecast for 2019")
plot(forecast_2020, main="SO2 Levels Forecast for 2020")

# 安装并加载必要的包
install.packages("ggplot2")

# 设置语言环境为英文
Sys.setlocale("LC_TIME", "English")

library(ggplot2)

# 创建日期列
m_d_TTN_mar_may$date <- as.Date(with(m_d_TTN_mar_may, paste(year, month, day, sep="-")), "%Y-%m-%d")
m_d_TTT_mar_may$date <- as.Date(with(m_d_TTT_mar_may, paste(year, month, day, sep="-")), "%Y-%m-%d")


# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN_mar_may$date, so2 = m_d_TTN_mar_may$so2, year = 2019),
  data.frame(date = m_d_TTT_mar_may$date, so2 = m_d_TTT_mar_may$so2, year = 2020)
)

# 绘制时间序列图
ggplot(data, aes(x = date, y = so2, color = factor(year))) +
  geom_line() +
  labs(title = "SO2 Levels: 2019 vs 2020", x = "Date", y = "SO2(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN_mar_may$date, no2 = m_d_TTN_mar_may$no2, year = 2019),
  data.frame(date = m_d_TTT_mar_may$date, no2 = m_d_TTT_mar_may$no2, year = 2020)
)


# 绘制时间序列图
ggplot(data, aes(x = date, y = no2, color = factor(year))) +
  geom_line() +
  labs(title = "no2 Levels: 2019 vs 2020", x = "Date", y = "no2(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN_mar_may$date, no = m_d_TTN_mar_may$no, year = 2019),
  data.frame(date = m_d_TTT_mar_may$date, no = m_d_TTT_mar_may$no, year = 2020)
)


# 绘制时间序列图
ggplot(data, aes(x = date, y = no, color = factor(year))) +
  geom_line() +
  labs(title = "no Levels: 2019 vs 2020", x = "Date", y = "no(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN_mar_may$date, pm2.5 = m_d_TTN_mar_may$pm2.5, year = 2019),
  data.frame(date = m_d_TTT_mar_may$date, pm2.5 = m_d_TTT_mar_may$pm2.5, year = 2020)
)


# 绘制时间序列图
ggplot(data, aes(x = date, y = pm2.5, color = factor(year))) +
  geom_line() +
  labs(title = "pm2.5 Levels: 2019 vs 2020", x = "Date", y = "pm2.5(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN_mar_may$date, o3 = m_d_TTN_mar_may$o3, year = 2019),
  data.frame(date = m_d_TTT_mar_may$date, o3 = m_d_TTT_mar_may$o3, year = 2020)
)


# 绘制时间序列图
ggplot(data, aes(x = date, y = o3, color = factor(year))) +
  geom_line() +
  labs(title = "o3 Levels: 2019 vs 2020", x = "Date", y = "o3(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN_mar_may$date, ws = m_d_TTN_mar_may$ws, year = 2019),
  data.frame(date = m_d_TTT_mar_may$date, ws = m_d_TTT_mar_may$ws, year = 2020)
)


# 绘制时间序列图
ggplot(data, aes(x = date, y = ws, color = factor(year))) +
  geom_line() +
  labs(title = "ws Levels: 2019 vs 2020", x = "Date", y = "ws(m/s)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN_mar_may$date, wd = m_d_TTN_mar_may$wd, year = 2019),
  data.frame(date = m_d_TTT_mar_may$date, wd = m_d_TTT_mar_may$wd, year = 2020)
)


# 绘制时间序列图
ggplot(data, aes(x = date, y = wd, color = factor(year))) +
  geom_line() +
  labs(title = "wd Levels: 2019 vs 2020", x = "Date", y = "wd(degrees)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN_mar_may$date, temp = m_d_TTN_mar_may$temp, year = 2019),
  data.frame(date = m_d_TTT_mar_may$date, temp = m_d_TTT_mar_may$temp, year = 2020)
)

lenth
# 绘制时间序列图
ggplot(data, aes(x = date, y = temp, color = factor(year))) +
  geom_line() +
  labs(title = "temp Levels: 2019 vs 2020", x = "Date", y = "temp(degree celcius)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")



m_d_TTN <- subset(m_d, year == 2019)
TTN_so2 <- m_d_TTN$so2
m_d_TTT <- subset(m_d, year == 2020)
# 找到2020年2月29日的位置
leap_day_index <- which(m_d_TTT$date == as.Date("2020-02-29"))
# 移除2020年2月29日的数据
m_d_TTT <- m_d_TTT[-leap_day_index, ]
TTT_so2 <- m_d_TTT$so2
summary(TTN_so2)
summary(TTT_so2)
t.test(TTN_so2, TTT_so2)
wilcox.test(TTN_so2, TTT_so2)


TTN_no <- m_d_TTN$no
TTT_no <- m_d_TTT$no
summary(TTN_no)
summary(TTT_no)
t.test(TTN_no, TTT_no)
wilcox.test(TTN_no, TTT_no)

TTN_no2 <- m_d_TTN$no2
TTT_no2 <- m_d_TTT$no2
summary(TTN_no2)
summary(TTT_no2)
t.test(TTN_no2, TTT_no2)
wilcox.test(TTN_no2, TTT_no2)

TTN_pm2.5 <- m_d_TTN$pm2.5
TTT_pm2.5 <- m_d_TTT$pm2.5
summary(TTN_pm2.5)
summary(TTT_pm2.5)
t.test(TTN_pm2.5, TTT_pm2.5)
wilcox.test(TTN_pm2.5, TTT_pm2.5)

TTN_o3 <- m_d_TTN$o3
TTT_o3 <- m_d_TTT$o3
summary(TTN_o3)
summary(TTT_o3)
t.test(TTN_o3, TTT_o3)
wilcox.test(TTN_o3, TTT_o3)

TTN_ws <- m_d_TTN$ws
TTT_ws <- m_d_TTT$ws
summary(TTN_ws)
summary(TTT_ws)
t.test(TTN_ws, TTT_ws)
wilcox.test(TTN_ws, TTT_ws)

TTN_wd <- m_d_TTN$wd
TTT_wd <- m_d_TTT$wd
summary(TTN_wd)
summary(TTT_wd)
t.test(TTN_wd, TTT_wd)
wilcox.test(TTN_wd, TTT_wd)

TTN_temp <- m_d_TTN$temp
TTT_temp <- m_d_TTT$temp
summary(TTN_temp)
summary(TTT_temp)
t.test(TTN_temp, TTT_temp)
wilcox.test(TTN_temp, TTT_temp)


# 找到2020年2月29日的位置
leap_day_index <- which(m_d_TTT$date == as.Date("2020-02-29"))

# 移除2020年2月29日的数据
m_d_TTT <- m_d_TTT[-leap_day_index, ]

# 创建数据框
data <- data.frame(
  Group = rep(c("2019", "2020"), each = 365),
  Value = c(m_d_TTN$so2, m_d_TTT$so2)
)


# 绘制箱线图
ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  labs(title = "daliy data of SO2 from whole year of 2019 and 2020", x = "Years", y = "SO2(μg/m^3)") +
  theme_minimal() +
  scale_fill_manual(values = c("2019" = "blue", "2020" = "red"))

# 创建数据框
data <- data.frame(
  Group = rep(c("2019", "2020"), each = 365),
  Value = c(m_d_TTN$no, m_d_TTT$no)
)
# 绘制箱线图
ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  labs(title = "daliy data of no from whole year of 2019 and 2020", x = "Years", y = "no(μg/m^3)") +
  theme_minimal() +
  scale_fill_manual(values = c("2019" = "blue", "2020" = "red"))

# 创建数据框
data <- data.frame(
  Group = rep(c("2019", "2020"), each = 365),
  Value = c(m_d_TTN$no2, m_d_TTT$no2)
)
# 绘制箱线图
ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  labs(title = "daliy data of no2 from whole year of 2019 and 2020", x = "Years", y = "no2(μg/m^3)") +
  theme_minimal() +
  scale_fill_manual(values = c("2019" = "blue", "2020" = "red"))

# 创建数据框
data <- data.frame(
  Group = rep(c("2019", "2020"), each = 365),
  Value = c(m_d_TTN$pm2.5, m_d_TTT$pm2.5)
)
# 绘制箱线图
ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  labs(title = "daliy data of pm2.5 from whole year of 2019 and 2020", x = "Years", y = "pm2.5(μg/m^3)") +
  theme_minimal() +
  scale_fill_manual(values = c("2019" = "blue", "2020" = "red"))

# 创建数据框
data <- data.frame(
  Group = rep(c("2019", "2020"), each = 365),
  Value = c(m_d_TTN$o3, m_d_TTT$o3)
)
# 绘制箱线图
ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  labs(title = "daliy data of o3 from whole year of 2019 and 2020", x = "Years", y = "o3(μg/m^3)") +
  theme_minimal() +
  scale_fill_manual(values = c("2019" = "blue", "2020" = "red"))

# 创建数据框
data <- data.frame(
  Group = rep(c("2019", "2020"), each = 365),
  Value = c(m_d_TTN$ws, m_d_TTT$ws)
)
# 绘制箱线图
ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  labs(title = "daliy data of windspeed from whole year of 2019 and 2020", x = "Years", y = "windspeed(m/s)") +
  theme_minimal() +
  scale_fill_manual(values = c("2019" = "blue", "2020" = "red"))

# 创建数据框
data <- data.frame(
  Group = rep(c("2019", "2020"), each = 365),
  Value = c(m_d_TTN$wd, m_d_TTT$wd)
)
# 绘制箱线图
ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  labs(title = "daliy data of wind direction from whole year of 2019 and 2020", x = "Years", y = "wind direction(degree)") +
  theme_minimal() +
  scale_fill_manual(values = c("2019" = "blue", "2020" = "red"))

# 创建数据框
data <- data.frame(
  Group = rep(c("2019", "2020"), each = 365),
  Value = c(m_d_TTN$temp, m_d_TTT$temp)
)
# 绘制箱线图
ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  labs(title = "daliy data of temperature from whole year of 2019 and 2020", x = "Years", y = "temperature(degree celcius)") +
  theme_minimal() +
  scale_fill_manual(values = c("2019" = "blue", "2020" = "red"))


# 设置语言环境为英文
Sys.setlocale("LC_TIME", "English")

library(ggplot2)

# 创建日期列
m_d_TTN$date <- as.Date(with(m_d_TTN, paste(year, month, day, sep="-")), "%Y-%m-%d")
m_d_TTT$date <- as.Date(with(m_d_TTT, paste(year, month, day, sep="-")), "%Y-%m-%d")


# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN$date, so2 = m_d_TTN$so2, year = 2019),
  data.frame(date = m_d_TTT$date, so2 = m_d_TTT$so2, year = 2020)
)

# 绘制时间序列图
ggplot(data, aes(x = date, y = so2, color = factor(year))) +
  geom_line() +
  labs(title = "SO2 Levels: 2019 vs 2020", x = "Date", y = "SO2(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b", 
               limits = as.Date(c("2019-01-01", "2020-12-31")))

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN$date, no = m_d_TTN$no, year = 2019),
  data.frame(date = m_d_TTT$date, no = m_d_TTT$no, year = 2020)
)

# 绘制时间序列图
ggplot(data, aes(x = date, y = no, color = factor(year))) +
  geom_line() +
  labs(title = "no Levels: 2019 vs 2020", x = "Date", y = "no(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b", 
               limits = as.Date(c("2019-01-01", "2020-12-31")))

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN$date, no2 = m_d_TTN$no2, year = 2019),
  data.frame(date = m_d_TTT$date, no2 = m_d_TTT$no2, year = 2020)
)

# 绘制时间序列图
ggplot(data, aes(x = date, y = no2, color = factor(year))) +
  geom_line() +
  labs(title = "no2 Levels: 2019 vs 2020", x = "Date", y = "no2(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b", 
               limits = as.Date(c("2019-01-01", "2020-12-31")))

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN$date, pm2.5 = m_d_TTN$pm2.5, year = 2019),
  data.frame(date = m_d_TTT$date, pm2.5 = m_d_TTT$pm2.5, year = 2020)
)

# 绘制时间序列图
ggplot(data, aes(x = date, y = pm2.5, color = factor(year))) +
  geom_line() +
  labs(title = "pm2.5 Levels: 2019 vs 2020", x = "Date", y = "pm2.5(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b", 
               limits = as.Date(c("2019-01-01", "2020-12-31")))

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN$date, o3 = m_d_TTN$o3, year = 2019),
  data.frame(date = m_d_TTT$date, o3 = m_d_TTT$o3, year = 2020)
)

# 绘制时间序列图
ggplot(data, aes(x = date, y = o3, color = factor(year))) +
  geom_line() +
  labs(title = "o3 Levels: 2019 vs 2020", x = "Date", y = "o3(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b", 
               limits = as.Date(c("2019-01-01", "2020-12-31")))

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN$date, ws = m_d_TTN$ws, year = 2019),
  data.frame(date = m_d_TTT$date, ws = m_d_TTT$ws, year = 2020)
)

# 绘制时间序列图
ggplot(data, aes(x = date, y = ws, color = factor(year))) +
  geom_line() +
  labs(title = "wind speed Levels: 2019 vs 2020", x = "Date", y = "wind speed(m/s)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b", 
               limits = as.Date(c("2019-01-01", "2020-12-31")))

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN$date, wd = m_d_TTN$wd, year = 2019),
  data.frame(date = m_d_TTT$date, wd = m_d_TTT$wd, year = 2020)
)

# 绘制时间序列图
ggplot(data, aes(x = date, y = wd, color = factor(year))) +
  geom_line() +
  labs(title = "wind direction Levels: 2019 vs 2020", x = "Date", y = "wind direction(degree)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b", 
               limits = as.Date(c("2019-01-01", "2020-12-31")))

# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN$date, temp = m_d_TTN$temp, year = 2019),
  data.frame(date = m_d_TTT$date, temp = m_d_TTT$temp, year = 2020)
)

# 绘制时间序列图
ggplot(data, aes(x = date, y = temp, color = factor(year))) +
  geom_line() +
  labs(title = "temperature Levels: 2019 vs 2020", x = "Date", y = "temperature(degree celsius)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b", 
               limits = as.Date(c("2019-01-01", "2020-12-31")))

m_d_TTE_mar_may <- subset(m_d, year == 2018 & ((month == 3 & day >= 23) | month == 4 | month == 5))
TTE_m_m_so2 <- m_d_TTE_mar_may$so2
m_d_TTT_mar_may <- subset(m_d, year == 2020 & ((month == 3 & day >= 23) | month == 4 | month == 5))
TTT_m_m_so2 <- m_d_TTT_mar_may$so2
summary(TTE_m_m_so2)
summary(TTT_m_m_so2)
t.test(TTE_m_m_so2, TTT_m_m_so2)
wilcox.test(TTE_m_m_so2, TTT_m_m_so2)
TTE_m_m_no <- m_d_TTE_mar_may$no
TTT_m_m_no <- m_d_TTT_mar_may$no
summary(TTE_m_m_no)
summary(TTT_m_m_no)
t.test(TTE_m_m_no, TTT_m_m_no)
wilcox.test(TTE_m_m_no, TTT_m_m_no)

TTE_m_m_no2 <- m_d_TTE_mar_may$no2
TTT_m_m_no2 <- m_d_TTT_mar_may$no2
summary(TTE_m_m_no2)
summary(TTT_m_m_no2)
t.test(TTE_m_m_no2, TTT_m_m_no2)
wilcox.test(TTE_m_m_no2, TTT_m_m_no2)

TTE_m_m_pm2.5 <- m_d_TTE_mar_may$pm2.5
TTT_m_m_pm2.5 <- m_d_TTT_mar_may$pm2.5
summary(TTE_m_m_pm2.5)
summary(TTT_m_m_pm2.5)
t.test(TTE_m_m_pm2.5, TTT_m_m_pm2.5)
wilcox.test(TTE_m_m_pm2.5, TTT_m_m_pm2.5)

TTE_m_m_o3 <- m_d_TTE_mar_may$o3
TTT_m_m_o3 <- m_d_TTT_mar_may$o3
summary(TTE_m_m_o3)
summary(TTT_m_m_o3)
t.test(TTE_m_m_o3, TTT_m_m_o3)
wilcox.test(TTE_m_m_o3, TTT_m_m_o3)

TTE_m_m_ws <- m_d_TTE_mar_may$ws
TTT_m_m_ws <- m_d_TTT_mar_may$ws
summary(TTE_m_m_ws)
summary(TTT_m_m_ws)
t.test(TTE_m_m_ws, TTT_m_m_ws)
wilcox.test(TTE_m_m_ws, TTT_m_m_ws)

TTE_m_m_wd <- m_d_TTE_mar_may$wd
TTT_m_m_wd <- m_d_TTT_mar_may$wd
summary(TTE_m_m_wd)
summary(TTT_m_m_wd)
t.test(TTE_m_m_wd, TTT_m_m_wd)
wilcox.test(TTE_m_m_wd, TTT_m_m_wd)

TTE_m_m_temp <- m_d_TTE_mar_may$temp
TTT_m_m_temp <- m_d_TTT_mar_may$temp
summary(TTE_m_m_temp)
summary(TTT_m_m_temp)
t.test(TTE_m_m_temp, TTT_m_m_temp)
wilcox.test(TTE_m_m_temp, TTT_m_m_temp)

m_d_TTN <- subset(m_d, year == 2019)
TTN_so2 <- m_d_TTN$so2
m_d_TTE <- subset(m_d, year == 2018)
TTE_so2 <- m_d_TTE$so2
summary(TTN_so2)
summary(TTE_so2)
t.test(TTN_so2, TTE_so2)
wilcox.test(TTN_so2, TTE_so2)

TTN_no <- m_d_TTN$no
TTE_no <- m_d_TTE$no
summary(TTN_no)
summary(TTE_no)
t.test(TTN_no, TTE_no)
wilcox.test(TTN_no, TTE_no)

TTN_no2 <- m_d_TTN$no2
TTE_no2 <- m_d_TTE$no2
summary(TTN_no2)
summary(TTE_no2)
t.test(TTN_no2, TTE_no2)
wilcox.test(TTN_no2, TTE_no2)

TTN_pm2.5 <- m_d_TTN$pm2.5
TTE_pm2.5 <- m_d_TTE$pm2.5
summary(TTN_pm2.5)
summary(TTE_pm2.5)
t.test(TTN_pm2.5, TTE_pm2.5)
wilcox.test(TTN_pm2.5, TTE_pm2.5)

TTN_o3 <- m_d_TTN$o3
TTE_o3 <- m_d_TTE$o3
summary(TTN_o3)
summary(TTE_o3)
t.test(TTN_o3, TTE_o3)
wilcox.test(TTN_o3, TTE_o3)

TTN_ws <- m_d_TTN$ws
TTE_ws <- m_d_TTE$ws
summary(TTN_ws)
summary(TTE_ws)
t.test(TTN_ws, TTE_ws)
wilcox.test(TTN_ws, TTE_ws)

TTN_wd <- m_d_TTN$wd
TTE_wd <- m_d_TTE$wd
summary(TTN_wd)
summary(TTE_wd)
t.test(TTN_wd, TTE_wd)
wilcox.test(TTN_wd, TTE_wd)

TTN_temp <- m_d_TTN$temp
TTE_temp <- m_d_TTE$temp
summary(TTN_temp)
summary(TTE_temp)
t.test(TTN_temp, TTE_temp)
wilcox.test(TTN_temp, TTE_temp)

m_d_TTN_mar_may <- subset(m_d, year == 2019 & ((month == 3 & day >= 23) | month == 4 | month == 5))
TTN_m_m_so2 <- m_d_TTN_mar_may$so2
m_d_TTE_mar_may <- subset(m_d, year == 2018 & ((month == 3 & day >= 23) | month == 4 | month == 5))
TTE_m_m_so2 <- m_d_TTE_mar_may$so2
summary(TTN_m_m_so2)
summary(TTE_m_m_so2)
t.test(TTN_m_m_so2, TTE_m_m_so2)
wilcox.test(TTN_m_m_so2, TTE_m_m_so2)
TTN_m_m_no <- m_d_TTN_mar_may$no
TTE_m_m_no <- m_d_TTE_mar_may$no
summary(TTN_m_m_no)
summary(TTE_m_m_no)
t.test(TTN_m_m_no, TTE_m_m_no)
wilcox.test(TTN_m_m_no, TTE_m_m_no)

TTN_m_m_no2 <- m_d_TTN_mar_may$no2
TTE_m_m_no2 <- m_d_TTE_mar_may$no2
summary(TTN_m_m_no2)
summary(TTE_m_m_no2)
t.test(TTN_m_m_no2, TTE_m_m_no2)
wilcox.test(TTN_m_m_no2, TTE_m_m_no2)

TTN_m_m_pm2.5 <- m_d_TTN_mar_may$pm2.5
TTE_m_m_pm2.5 <- m_d_TTE_mar_may$pm2.5
summary(TTN_m_m_pm2.5)
summary(TTE_m_m_pm2.5)
t.test(TTN_m_m_pm2.5, TTE_m_m_pm2.5)
wilcox.test(TTN_m_m_pm2.5, TTE_m_m_pm2.5)

TTN_m_m_o3 <- m_d_TTN_mar_may$o3
TTE_m_m_o3 <- m_d_TTE_mar_may$o3
summary(TTN_m_m_o3)
summary(TTE_m_m_o3)
t.test(TTN_m_m_o3, TTE_m_m_o3)
wilcox.test(TTN_m_m_o3, TTE_m_m_o3)

TTN_m_m_ws <- m_d_TTN_mar_may$ws
TTE_m_m_ws <- m_d_TTE_mar_may$ws
summary(TTN_m_m_ws)
summary(TTE_m_m_ws)
t.test(TTN_m_m_ws, TTE_m_m_ws)
wilcox.test(TTN_m_m_ws, TTE_m_m_ws)

TTN_m_m_wd <- m_d_TTN_mar_may$wd
TTE_m_m_wd <- m_d_TTE_mar_may$wd
summary(TTN_m_m_wd)
summary(TTE_m_m_wd)
t.test(TTN_m_m_wd, TTE_m_m_wd)
wilcox.test(TTN_m_m_wd, TTE_m_m_wd)

TTN_m_m_temp <- m_d_TTN_mar_may$temp
TTE_m_m_temp <- m_d_TTE_mar_may$temp
summary(TTN_m_m_temp)
summary(TTE_m_m_temp)
t.test(TTN_m_m_temp, TTE_m_m_temp)
wilcox.test(TTN_m_m_temp, TTE_m_m_temp)

# 设置语言环境为英文
Sys.setlocale("LC_TIME", "English")

library(ggplot2)

# 创建日期列
m_d_TTN$date <- as.Date(with(m_d_TTN, paste(year, month, day, sep="-")), "%Y-%m-%d")
m_d_TTE$date <- as.Date(with(m_d_TTE, paste(year, month, day, sep="-")), "%Y-%m-%d")


# 合并数据
data <- rbind(
  data.frame(date = m_d_TTN$date, so2 = m_d_TTN$so2, year = 2019),
  data.frame(date = m_d_TTE$date, so2 = m_d_TTE$so2, year = 2018)
)

# 绘制时间序列图
ggplot(data, aes(x = date, y = so2, color = factor(year))) +
  geom_line() +
  labs(title = "SO2 Levels: 2019 vs 2018", x = "Date", y = "SO2(μg/m^3)") +
  theme_minimal() +
  scale_color_manual(values = c("2019" = "red", "2018" = "blue"), name = "Year") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b", 
               limits = as.Date(c("2018-01-01","2019-12-31")))