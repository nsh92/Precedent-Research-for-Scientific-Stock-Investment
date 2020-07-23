#######################################################
if(!require("xlsx")) install.packages("xlsx"); library(xlsx)
if(!require("rJava")) install.packages("rJava"); library(rJava)
if(!require("dplyr")) install.packages("dplyr"); library(dplyr)
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if(!require("data.table")) install.packages("data.table"); library(data.table) # rbindlist
######################################################
삼성전자 <- read.xlsx("삼성전자.xlsx", stringAsfacotrs = F, sheetIndex = 1, encoding = "UTF-8")
삼성전자$compnay <- "삼성전자"
sk하이닉스 <- read.xlsx("sk하이닉스.xlsx", stringAsfacotrs = F, sheetIndex = 1, encoding = "UTF-8")
sk하이닉스$compnay <- "sk하이닉스"
KT <- read.xlsx("KT.xlsx", stringAsfacotrs = F, sheetIndex = 1, encoding = "UTF-8")
KT$compnay <- "KT"
aa <- vector(mode = "list", length = 2)
aa[[1]] <- 삼성전자
aa[[2]] <- sk하이닉스
bb <- rbindlist(aa)
str(bb)
View(bb)
var_names <- c("매출액", "영업이익", "당기순익", "EPS", "영익률", "주가", "compnay")
car <- bb %>% select(var_names)
car <- rename(car, company = compnay)
str(car)
car <- na.omit(car)

company_name <- names(table(car$company))
company_name
company_df <- vector(mode = "list", length = 2)
company_model_sm <- vector(mode = "list", length = 15)
company_model <- vector(mode = "list", length = 15)
cnt <- 1
coefficient <- vector(mode = "list", length = 10)
변수 <- character()
회사 <- character()
설명력 <- numeric()
pred <- vector(mode = "list", length = 10)
cor_re <- vector(mode = "list", length = 10)
mse <- vector(mode = "list", length = 10)
var <- colnames(car)[-7]
var <- var[-6]
##################################
for (i in 1:length(company_name)) {
  
  # 회사별 분류
  company_df[[i]] <- car %>% filter(company == company_name[i])
  re_car <- company_df[[1]][-7]
  # train / test set
  idx <- sample(nrow(company_df[[i]]), 0.7*nrow(company_df[[i]]))
  train <- company_df[[i]][idx,-7]
  test <- company_df[[i]][-idx,-7]
  for(r in 1:5){
    re_train <- train[,c(r,6)]
    re_test <- test[,c(r,6)]
    company_model[[cnt]] <- lm(주가 ~ ., re_train)
    company_model_sm[[cnt]] <- summary(company_model[[cnt]])
    pred[[cnt]] <- predict(company_model[[cnt]], re_test)
    cor_re[[cnt]] <- cor(re_test$주가, pred[[cnt]])
    설명력[[cnt]] <- company_model_sm[[cnt]]$adj.r.squared
    변수[[cnt]] <- var[r]
    회사[[cnt]] <- company_name[i]
    coefficient[[cnt]] <- company_model[[cnt]]$coefficients[2]
    cnt <- cnt + 1
  }
}
상관계수 <- unlist(cor_re)
회귀계수 <- round(unlist(coefficient), 1)
str(회귀계수)
f_result <- data.frame(회사, 변수, 설명력, 상관계수, 회귀계수)
f_result
write.csv(f_result, "반도체_result.csv", , row.names = F, quote = F, fileEncoding = "euc-kr")



# 단순회귀분석 결과 그래프
# 영업이익
range(kia$영업이익)
ggplot(kia, aes(x = 영업이익, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(0,12315)) + theme_minimal() + geom_rug() +
  labs(x = "영업이익", y = "주가") + ggtitle("기아차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("기아차_영업이익.png", dpi = 300)


# 당기순익
range(kia$당기순익)
ggplot(kia, aes(x = 당기순익, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(0,12013)) + theme_minimal() + geom_rug() +
  labs(x = "당기순익", y = "주가") + ggtitle("기아차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("기아차_당기순익익.png", dpi = 300)


# 매출액
range(kia$매출액)
ggplot(kia, aes(x = 매출액, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(20606,161054)) + theme_minimal() + geom_rug() +
  labs(x = "매출액", y = "주가") + ggtitle("기아차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("기아차_매출액.png", dpi = 300)

# EPS
range(kia$EPS)
ggplot(kia, aes(x = EPS, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(0,2968)) + theme_minimal() + geom_rug() +
  labs(x = "EPS", y = "주가") + ggtitle("기아차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("기아차_eps.png", dpi = 300)

# 영익률
range(kia$영익률)
ggplot(kia, aes(x = 영익률, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(0,10.39)) + theme_minimal() + geom_rug() +
  labs(x = "영익률", y = "주가") + ggtitle("기아차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("기아차_영익률.png", dpi = 300)



# hyundai
hyundai <- car %>% filter(company == "현대")
# 변수별 그래프

# 영업이익
range(hyundai$영업이익)
ggplot(hyundai, aes(x = 영업이익, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(0,25381)) + theme_minimal() + geom_rug() +
  labs(x = "영업이익", y = "주가") + ggtitle("현대차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("현대차_영업이익.png", dpi = 300)


# 당기순익
range(hyundai$당기순익)
ggplot(hyundai, aes(x = 당기순익, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(0,25480)) + theme_minimal() + geom_rug() +
  labs(x = "당기순익", y = "주가") + ggtitle("현대차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("현대차_당기순익.png", dpi = 300)


# 매출액
range(hyundai$매출액)
ggplot(hyundai, aes(x = 매출액, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(41896,278241)) + theme_minimal() + geom_rug() +
  labs(x = "매출액", y = "주가") + ggtitle("현대차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("현대차_매출액.png", dpi = 300)

# EPS
range(hyundai$EPS)
ggplot(hyundai, aes(x = EPS, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(0,8572)) + theme_minimal() + geom_rug() +
  labs(x = "EPS", y = "주가") + ggtitle("현대차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("현대차_eps.png", dpi = 300)

# 영익률
range(hyundai$영익률)
ggplot(hyundai, aes(x = 영익률, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(1.18,11.57)) + theme_minimal() + geom_rug() +
  labs(x = "영익률", y = "주가") + ggtitle("현대차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("현대차_영익률.png", dpi = 300)



# ssang
ssang <- car %>% filter(company == "쌍용")
# 변수별 그래프

# 영업이익
range(ssang$영업이익)
ggplot(ssang, aes(x = 영업이익, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(0,1102)) + theme_minimal() + geom_rug() +
  labs(x = "영업이익", y = "주가") + ggtitle("쌍용차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("쌍용차_영업이익.png", dpi = 300)


# 당기순익
range(ssang$당기순익)
ggplot(ssang, aes(x = 당기순익, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(0,1863)) + theme_minimal() + geom_rug() +
  labs(x = "당기순익", y = "주가") + ggtitle("쌍용차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("쌍용차_당기순익.png", dpi = 300)


# 매출액
range(ssang$매출액)
ggplot(ssang, aes(x = 매출액, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(2217,10527)) + theme_minimal() + geom_rug() +
  labs(x = "매출액", y = "주가") + ggtitle("쌍용차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("쌍용차_매출액.png", dpi = 300)

# EPS
range(ssang$EPS)
ggplot(ssang, aes(x = EPS, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(-1000,55428)) + theme_minimal() + geom_rug() +
  labs(x = "EPS", y = "주가") + ggtitle("쌍용차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("쌍용차_eps.png", dpi = 300)

# 영익률
range(ssang$영익률)
ggplot(ssang, aes(x = 영익률, y = 주가)) + geom_point() +
  stat_smooth(method = "lm") + xlim(c(-20,12.18)) + theme_minimal() + geom_rug() +
  labs(x = "영익률", y = "주가") + ggtitle("쌍용차") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) + theme(axis.title = element_text(face = "bold", size = 15, color = "darkblue"))
ggsave("쌍용차_영익률.png", dpi = 300)

