`%!in%` <- Negate(`%in%`)

get_paired_table <- function(data, time, tewl = "greater", sh = "less", tem = "greater") {
    data <- data %>% filter(Time %in% c(glue("{time} Before"), glue("{time} After")))
    allegy <- ifelse(nrow(data) == 40, "알레르기 포함", "알레르기 제외")
    variables <- c("알레르기", "시간", "지표", "정규성 p-value", "정규성 결과", "검정 방법", "검정 p-value", "검정 결과", "순열 검정 p-value", "순열 검정 결과", "요약")
    df <- variables %>% map_dfc(setNames, object = list(character()))
    
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
            sw_result <- "만족"
            test_name <- "대응 표본 t-검정"
            test <- t.test(after, before, alternative = alternative, paired = TRUE)
        } else {
            sw_result <- "만족 X"
            test_name <- "윌콕슨 부호 순위 검정"
            test <- wilcox.test(after, before, alternative = alternative, paired = TRUE)
        }
        
        mean_after <- round(mean(after), 4)
        mean_before <- round(mean(before), 4)
        
        # 순열 검정
        test2 <- perm.paired.loc(x = after, y = before, parameter = mean, alternative = alternative, R = 10000)
        
        if (variable %in% c("TEWL", "Tem")) {
            if (test$p.value > 0.05) {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크지 않다")
                signif <- "X"
            } else {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크다")
                signif <- "O"
            }
            
            if (test2$p.value > 0.05) {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크지 않다")
                signif2 <- "X"
            } else {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크다")
                signif2 <- "O"
            }
        } else {
            if (test$p.value > 0.05) {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작지 않다")
                signif <- "X"
            } else {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작다")
                signif <- "O"
            }
            
            if (test2$p.value > 0.05) {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작지 않다")
                signif2 <- "X"
            } else {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작다")
                signif2 <- "O"
            }
        }
        
        row <- c(
            allegy,
            time, 
            variable, 
            round(sw$p.value, 4),
            sw_result,
            test_name,
            round(test$p.value, 4),
            result,
            round(test2$p.value, 4),
            result2,
            glue("{signif}/{signif2}")
        )
        names(row) <- variables
        df <- bind_rows(df, row)
    }
    
    return(df)
}

get_paired_table <- function(data, time, tewl = "greater", sh = "less", tem = "greater") {
    data <- data %>% filter(Time %in% c(glue("{time} Before"), glue("{time} After")))
    allegy <- ifelse(nrow(data) == 40, "알레르기 포함", "알레르기 제외")
    variables <- c("알레르기", "시간", "지표", "정규성 p-value", "정규성 결과", "검정 방법", "검정 p-value", "검정 결과", "순열 검정 p-value", "순열 검정 결과", "요약")
    df <- variables %>% map_dfc(setNames, object = list(character()))
    
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
            sw_result <- "만족"
            test_name <- "대응 표본 t-검정"
            test <- t.test(after, before, alternative = alternative, paired = TRUE)
        } else {
            sw_result <- "만족 X"
            test_name <- "윌콕슨 부호 순위 검정"
            test <- wilcox.test(after, before, alternative = alternative, paired = TRUE)
        }
        
        mean_after <- round(mean(after), 4)
        mean_before <- round(mean(before), 4)
        
        # 순열 검정
        test2 <- perm.paired.loc(x = after, y = before, parameter = mean, alternative = alternative, R = 10000)
        
        if (variable %in% c("TEWL", "Tem")) {
            if (test$p.value > 0.05) {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크지 않다")
                signif <- "X"
            } else {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크다")
                signif <- "O"
            }
            
            if (test2$p.value > 0.05) {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크지 않다")
                signif2 <- "X"
            } else {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크다")
                signif2 <- "O"
            }
        } else {
            if (test$p.value > 0.05) {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작지 않다")
                signif <- "X"
            } else {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작다")
                signif <- "O"
            }
            
            if (test2$p.value > 0.05) {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작지 않다")
                signif2 <- "X"
            } else {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작다")
                signif2 <- "O"
            }
        }
        
        row <- c(
            allegy,
            time, 
            variable, 
            round(sw$p.value, 4),
            sw_result,
            test_name,
            round(test$p.value, 4),
            result,
            round(test2$p.value, 4),
            result2,
            glue("{signif}/{signif2}")
        )
        names(row) <- variables
        df <- bind_rows(df, row)
    }
    
    return(df)
}

get_multiple_table <- function(data, variable, times) {
    allegy <- ifelse(nrow(data) == 160, "알레르기 포함", "알레르기 제외")
    test_data <- list()
    Difference <- c()
    Time <- rep(times, each = nrow(data) / 8)
    Normality <- list()
    variables <- c("알레르기", "시간", "지표", "정규성 p-value", "정규성 결과", "검정 방법", "검정 p-value", "검정 결과", "순열 검정 p-value", "순열 검정 결과", "요약")
    df <- variables %>% map_dfc(setNames, object = list(character()))
    
    for (time in times) {
        data_by_time <- data %>% filter(Time %in% c(glue("{time} Before"), glue("{time} After")))
        y_before <- data_by_time[glue("{variable}_Mean")] %>% slice(seq(1, nrow(data_by_time), by = 2)) %>% pull(glue("{variable}_Mean"))
        y_after <- data_by_time[glue("{variable}_Mean")] %>% slice(seq(2, nrow(data_by_time), by = 2)) %>% pull(glue("{variable}_Mean"))
        y_diff <- y_after - y_before
        Difference <- append(Difference, y_diff)
        Normality[[time]] <- shapiro.test(y_diff)
    }
    
    test_data[["Difference"]] <- Difference    
    test_data[["Time"]] <- Time
    test_data <- test_data %>% as_tibble()
    
    equivar_test <- levene.test(test_data$Difference, test_data$Time, location = "mean")
    
    if (all(map_dbl(Normality, "p.value") > 0.05)) {
        sw_result <- "만족"
        
        if (equivar_test$p.value > 0.05) {
            test_name <- "일원 분산분석"
            multiple_test <- oneway.test(formula = Difference ~ Time, data = test_data, var.equal = TRUE)
        } else {
            test_name <- "Welch의 분산분석"
            multiple_test <- oneway.test(formula = Difference ~ Time, data = test_data, var.equal = FALSE)
        }
    } else {
        sw_result <- "만족 X"
        test_name <- "크루스칼-왈리스 검정"
        multiple_test <- kruskal.test(Difference ~ Time, test_data)
    }
    
    multiple_test2 <- perm.oneway.anova(x = test_data$Difference, y = test_data$Time, R = 10000)
    time <- paste(times, collapse = ", ")
    norm_pvalue <- paste(round(map_dbl(Normality, "p.value"), 4), collapse = ", ")
    
    
    if (multiple_test$p.value > 0.05) {
        result <- glue("플라즈마 조사 전후 {variable} 차이값에 유의미한 차이가 없음")
        signif <- "X"
    } else {
        result <- glue("플라즈마 조사 전후 {variable} 차이값에 유의미한 차이가 있음")
        signif <- "O"
    }
    
    if (multiple_test2$p.value > 0.05) {
        result2 <- glue("플라즈마 조사 전후 {variable} 차이값에 유의미한 차이가 없음")
        signif2 <- "X"
    } else {
        result2 <- glue("플라즈마 조사 전후 {variable} 차이값에 유의미한 차이가 있음")
        signif2 <- "O"
    }
    
    row <- c(
        allegy,
        time, 
        variable, 
        norm_pvalue,
        sw_result,
        test_name,
        round(multiple_test$p.value, 4),
        result,
        round(multiple_test2$p.value, 4),
        result2,
        glue("{signif}/{signif2}")
    )
    names(row) <- variables
    df <- bind_rows(df, row)
}


get_after_table <- function(data, variable, times) {
    allegy <- ifelse(nrow(data) == 160, "알레르기 포함", "알레르기 제외")
    test_data <- list()
    After <- c()
    Time <- rep(times, each = nrow(data) / 8)
    Normality <- list()
    variables <- c("알레르기", "시간", "지표", "정규성 p-value", "정규성 결과", "검정 방법", "검정 p-value", "검정 결과", "순열 검정 p-value", "순열 검정 결과", "요약")
    df <- variables %>% map_dfc(setNames, object = list(character()))
    
    for (time in times) {
        data_by_time <- data %>% filter(Time %in% c(glue("{time} Before"), glue("{time} After")))
        y_after <- data_by_time[glue("{variable}_Mean")] %>% slice(seq(2, nrow(data_by_time), by = 2)) %>% pull(glue("{variable}_Mean"))
        After <- append(After, y_after)
        Normality[[time]] <- shapiro.test(y_after)
    }
    
    test_data[["After"]] <- After    
    test_data[["Time"]] <- Time
    test_data <- test_data %>% as_tibble()
    
    equivar_test <- levene.test(test_data$After, test_data$Time, location = "mean")
    
    if (all(map_dbl(Normality, "p.value") > 0.05)) {
        sw_result <- "만족"
        
        if (equivar_test$p.value > 0.05) {
            test_name <- "일원 분산분석"
            multiple_test <- oneway.test(formula = After ~ Time, data = test_data, var.equal = TRUE)
        } else {
            test_name <- "Welch의 분산분석"
            multiple_test <- oneway.test(formula = After ~ Time, data = test_data, var.equal = FALSE)
        }
    } else {
        sw_result <- "만족 X"
        test_name <- "크루스칼-왈리스 검정"
        multiple_test <- kruskal.test(After ~ Time, test_data)
    }
    
    multiple_test2 <- perm.oneway.anova(x = test_data$After, y = test_data$Time, R = 10000)
    time <- paste(times, collapse = ", ")
    norm_pvalue <- paste(round(map_dbl(Normality, "p.value"), 4), collapse = ", ")
    
    if (multiple_test$p.value > 0.05) {
        result <- glue("{time}의 플라즈마 조사 후 {variable} 값에 유의미한 차이가 없음")
        signif <- "X"
    } else {
        result <- glue("{time}의 플라즈마 조사 후 {variable} 값에 유의미한 차이가 있음")
        signif <- "O"
    }
    
    if (multiple_test2$p.value > 0.05) {
        result2 <- glue("{time}의 플라즈마 조사 후 {variable} 값에 유의미한 차이가 없음")
        signif2 <- "X"
    } else {
        result2 <- glue("{time}의 플라즈마 조사 후 {variable} 값에 유의미한 차이가 있음")
        signif2 <- "O"
    }
    
    row <- c(
        allegy,
        time, 
        variable, 
        norm_pvalue,
        sw_result,
        test_name,
        round(multiple_test$p.value, 4),
        result,
        round(multiple_test2$p.value, 4),
        result2,
        glue("{signif}/{signif2}")
    )
    names(row) <- variables
    df <- bind_rows(df, row)
}


get_paired_table <- function(data, time, tewl = "greater", sh = "less", tem = "greater") {
    data <- data %>% filter(Time %in% c(glue("{time} Before"), glue("{time} After")))
    allegy <- ifelse(nrow(data) == 40, "알레르기 포함", "알레르기 제외")
    variables <- c("알레르기", "시간", "지표", "정규성 p-value", "정규성 결과", "검정 방법", "검정 p-value", "검정 결과", "순열 검정 p-value", "순열 검정 결과", "요약")
    df <- variables %>% map_dfc(setNames, object = list(character()))
    
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
            sw_result <- "만족"
            test_name <- "대응 표본 t-검정"
            test <- t.test(after, before, alternative = alternative, paired = TRUE)
        } else {
            sw_result <- "만족 X"
            test_name <- "윌콕슨 부호 순위 검정"
            test <- wilcox.test(after, before, alternative = alternative, paired = TRUE)
        }
        
        mean_after <- round(mean(after), 4)
        mean_before <- round(mean(before), 4)
        
        # 순열 검정
        test2 <- perm.paired.loc(x = after, y = before, parameter = mean, alternative = alternative, R = 10000)
        
        if (variable %in% c("TEWL", "Tem")) {
            if (test$p.value > 0.05) {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크지 않다")
                signif <- "X"
            } else {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크다")
                signif <- "O"
            }
            
            if (test2$p.value > 0.05) {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크지 않다")
                signif2 <- "X"
            } else {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 크다")
                signif2 <- "O"
            }
        } else {
            if (test$p.value > 0.05) {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작지 않다")
                signif <- "X"
            } else {
                result <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작다")
                signif <- "O"
            }
            
            if (test2$p.value > 0.05) {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작지 않다")
                signif2 <- "X"
            } else {
                result2 <- glue("플라즈마 {time} 조사 후({mean_after}) {variable}이 {time} 조사 전({mean_before})보다 작다")
                signif2 <- "O"
            }
        }
        
        row <- c(
            allegy,
            time, 
            variable, 
            round(sw$p.value, 4),
            sw_result,
            test_name,
            round(test$p.value, 4),
            result,
            round(test2$p.value, 4),
            result2,
            glue("{signif}/{signif2}")
        )
        names(row) <- variables
        df <- bind_rows(df, row)
    }
    
    return(df)
}
