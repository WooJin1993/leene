`%!in%` <- Negate(`%in%`)

print_paired_results <- function(data, time, tewl = "greater", sh = "less", tem = "greater") {
    data <- data %>% filter(Time %in% c(glue("{time} Before"), glue("{time} After")))
    
    for (variable in c("TEWL", "SH", "Tem")) {
        if (variable == "TEWL") {
            alternative <- tewl
        } else if (variable == "SH") {
            alternative <- sh
        } else {
            alternative <- tem
        }
        
        before <- data[glue("{variable}_Mean")] %>% slice(seq(1, nrow(data), by = 2)) %>% pull(glue("{variable}_Mean"))
        after <- data[glue("{variable}_Mean")] %>% slice(seq(2, nrow(data), by = 2)) %>% pull(glue("{variable}_Mean"))
        diff <- after - before
        
        
        # 정규성 검정
        sw <- shapiro.test(diff)
        
        # 대응 표본 t-검정 또는 윌콕슨 부호 순위 검정
        if (sw$p.value > 0.05) {
            test_name <- "대응 표본 t-검정"
            test <- t.test(after, before, alternative = alternative, paired = TRUE)
        } else {
            test_name <- "윌콕슨 부호 순위 검정"
            test <- wilcox.test(after, before, alternative = alternative, paired = TRUE)
        }
        
        # 순열 검정
        test2 <- perm.paired.loc(x = after, y = before, parameter = mean, alternative = alternative, R = 10000)
        
        if (variable %in% c("TEWL", "Tem")) {
            if (test$p.value > 0.05) {
                result <- "크지 않다"
            } else {
                result <- "크다"
            }
            
            if (test2$p.value > 0.05) {
                result2 <- "크지 않다"
            } else {
                result2 <- "크다"
            }
        } else {
            if (test$p.value > 0.05) {
                result <- "작지 않다"
            } else {
                result <- "작다"
            }
            
            if (test2$p.value > 0.05) {
                result2 <- "작지 않다"
            } else {
                result2 <- "작다"
            }
        }
        
        print(glue("[{time} 조사 전후 {variable}의 차이에 대한 검정 결과]
                   정규성 검정의 p-value는 {sw$p.value}이고
                   {test_name}으로부터 p-value는 {test$p.value}이므로
                   플라즈마 {time} 조사 후({mean(after)}) {variable}이 {time} 조사 전({mean(before)})보다 {result}.
                   한편, 순열 검정으로부터 p-value는 {test2$p.value}이므로
                   플라즈마 {time} 조사 후({mean(after)}) {variable}이 {time} 조사 전({mean(before)})보다 {result2}.
                   
                   "))
    }
}

# print_multiple_results <- function(data, variable, times) {
#     test_data <- list()
#     Difference <- c()
#     Time <- rep(times, each = nrow(data) / 8)
#     Normality <- list()
#     
#     for (time in times) {
#         data_by_time <- data %>% filter(Time %in% c(glue("{time} Before"), glue("{time} After")))
#         y_before <- data_by_time[glue("{variable}_Mean")] %>% slice(seq(1, nrow(data_by_time), by = 2)) %>% pull(glue("{variable}_Mean"))
#         y_after <- data_by_time[glue("{variable}_Mean")] %>% slice(seq(2, nrow(data_by_time), by = 2)) %>% pull(glue("{variable}_Mean"))
#         y_diff <- y_after - y_before
#         Difference <- append(Difference, y_diff)
#         Normality[[time]] <- shapiro.test(y_diff)
#     }
#     
#     test_data[["Difference"]] <- Difference    
#     test_data[["Time"]] <- Time
#     test_data <- test_data %>% as_tibble()
#     
#     equivar_test <- levene.test(test_data$Difference, test_data$Time, location = "mean")
#     
#     if (all(map_dbl(Normality, "p.value") > 0.05)) {
#         if (equivar_test$p.value > 0.05) {
#             test_name <- "일원 분산분석"
#             multiple_test <- oneway.test(formula = Difference ~ Time, data = test_data, var.equal = TRUE)
#         } else {
#             test_name <- "Welch의 분산분석"
#             multiple_test <- oneway.test(formula = Difference ~ Time, data = test_data, var.equal = FALSE)
#         }
#     } else {
#         test_name <- "크루스칼-왈리스 검정"
#         multiple_test <- kruskal.test(Difference ~ Time, test_data)
#     }
#     
#     multiple_test2 <- perm.oneway.anova(x = test_data$Difference, y = test_data$Time, R = 10000)
#     time <- paste(times, collapse = ", ")
#     norm_pvalue <- paste(map_dbl(Normality, "p.value"), collapse = ", ")
#     
#     
#     if (multiple_test$p.value > 0.05) {
#         result <- glue("{time}의 플라즈마 조사 전후 {variable} 차이값에 유의미한 차이가 없음")
#     } else {
#         result <- glue("{time}의 플라즈마 조사 전후 {variable} 차이값에 유의미한 차이가 있음")
#     }
#     
#     if (multiple_test2$p.value > 0.05) {
#         result2 <- glue("{time}의 플라즈마 조사 전후 {variable} 차이값에 유의미한 차이가 없음")
#     } else {
#         result2 <- glue("{time}의 플라즈마 조사 전후 {variable} 차이값에 유의미한 차이가 있음")
#     }
#     
#     print(glue("[{time}에 따라 플라즈마 조사 전후 {variable}의 차잇값에 차이가 유의미한지에 대한 검정]
#                정규성 검정으로부터 p-value는 {norm_pvalue}이고
#                {test_name}으로부터 p-value는 {multiple_test$p.value}이므로
#                {result}
#                한편, 순열 검정으로부터 p-value는 {multiple_test2$p.value}이므로
#                {result2}"))
# }
# 
# 
# print_after_results <- function(data, variable, times) {
#     test_data <- list()
#     After <- c()
#     Time <- rep(times, each = nrow(data) / 8)
#     Normality <- list()
#     
#     for (time in times) {
#         data_by_time <- data %>% filter(Time %in% c(glue("{time} Before"), glue("{time} After")))
#         y_after <- data_by_time[glue("{variable}_Mean")] %>% slice(seq(2, nrow(data_by_time), by = 2)) %>% pull(glue("{variable}_Mean"))
#         After <- append(After, y_after)
#         Normality[[time]] <- shapiro.test(y_after)
#     }
#     
#     test_data[["After"]] <- After    
#     test_data[["Time"]] <- Time
#     test_data <- test_data %>% as_tibble()
#     
#     equivar_test <- levene.test(test_data$After, test_data$Time, location = "mean")
#     
#     if (all(map_dbl(Normality, "p.value") > 0.05)) {
#         if (equivar_test$p.value > 0.05) {
#             test_name <- "일원 분산분석"
#             multiple_test <- oneway.test(formula = After ~ Time, data = test_data, var.equal = TRUE)
#         } else {
#             test_name <- "Welch의 분산분석"
#             multiple_test <- oneway.test(formula = After ~ Time, data = test_data, var.equal = FALSE)
#         }
#     } else {
#         test_name <- "크루스칼-왈리스 검정"
#         multiple_test <- kruskal.test(After ~ Time, test_data)
#     }
#     
#     multiple_test2 <- perm.oneway.anova(x = test_data$After, y = test_data$Time, R = 10000)
#     time <- paste(times, collapse = ", ")
#     norm_pvalue <- paste(map_dbl(Normality, "p.value"), collapse = ", ")
#     
#     
#     if (multiple_test$p.value > 0.05) {
#         result <- glue("{time}의 플라즈마 조사 후 {variable} 값에 유의미한 차이가 없음")
#     } else {
#         result <- glue("{time}의 플라즈마 조사 후 {variable} 값에 유의미한 차이가 있음")
#     }
#     
#     if (multiple_test2$p.value > 0.05) {
#         result2 <- glue("{time}의 플라즈마 조사 후 {variable} 값에 유의미한 차이가 없음")
#     } else {
#         result2 <- glue("{time}의 플라즈마 조사 후 {variable} 값에 유의미한 차이가 있음")
#     }
#     
#     
#     print(glue("[{time}에 따라 플라즈마 조사 후 {variable} 값에 차이가 유의미한지에 대한 검정]
#                정규성 검정으로부터 p-value는 {norm_pvalue}이고
#                {test_name}으로부터 p-value는 {multiple_test$p.value}이므로
#                {result}
#                한편, 순열 검정으로부터 p-value는 {multiple_test2$p.value}이므로
#                {result2}"))
# }