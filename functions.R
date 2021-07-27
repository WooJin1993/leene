`%!in%` <- Negate(`%in%`)

print_paired_results <- function(data, time, tewl = "two.sided", sh = "two.sided", tem = "two.sided") {
    data <- data %>% filter(Time %in% c(glue("{time} Before"), glue("{time} After")))
    
    # --- TEWL ---
    tewl_before <- data["TEWL_Mean"] %>% slice(seq(1, nrow(data), by = 2)) %>% pull("TEWL_Mean")
    tewl_after <- data["TEWL_Mean"] %>% slice(seq(2, nrow(data), by = 2)) %>% pull("TEWL_Mean")
    tewl_diff <- tewl_after - tewl_before
    
    # normality test (정규성 검정)
    tewl_sw <- shapiro.test(tewl_diff)
    
    # paired t-test (대응표본 t검정)
    if (tewl_sw$p.value > 0.05) {
        tewl_test <- t.test(tewl_before, tewl_after, alternative = tewl, paired = TRUE)
    } else {
        tewl_test <- wilcox.test(tewl_before, tewl_after, alternative = tewl, paired = TRUE)
    }
    
    if (tewl_test$p.value > 0.05) {
        result_tewl <- "유의미한 변화 없음"
    } else {
        result_tewl <- "유의미한 변화 있음"
    }
    
    # --- SH ---
    sh_before <- data["SH_Mean"] %>% slice(seq(1, nrow(data), by = 2)) %>% pull("SH_Mean")
    sh_after <- data["SH_Mean"] %>% slice(seq(2, nrow(data), by = 2)) %>% pull("SH_Mean")
    sh_diff <- sh_after - sh_before
    
    # normality test (정규성 검정)
    sh_sw <- shapiro.test(sh_diff) 
    
    # paired t-test (대응표본 t검정)
    if (sh_sw$p.value > 0.05) {
        sh_test <- t.test(sh_before, sh_after, alternative = sh, paired = TRUE)
    } else {
        sh_test <- wilcox.test(sh_before, sh_after, alternative = sh, paired = TRUE)
    }
    
    if (sh_test$p.value > 0.05) {
        result_sh <- "유의미한 변화 없음"
    } else {
        result_sh <- "유의미한 변화 있음"
    }
    
    # --- Tem ---
    tem_before <- data["Tem_Mean"] %>% slice(seq(1, nrow(data), by = 2)) %>% pull("Tem_Mean")
    tem_after <- data["Tem_Mean"] %>% slice(seq(2, nrow(data), by = 2)) %>% pull("Tem_Mean")
    tem_diff <- tem_after - tem_before
    
    # normality test (정규성 검정)
    tem_sw <- shapiro.test(tem_diff) 
    
    # paired t-test (대응표본 t검정)
    if (tem_sw$p.value > 0.05) {
        tem_test <- t.test(tem_before, tem_after, alternative = tem, paired = TRUE)
    } else {
        tem_test <- wilcox.test(tem_before, tem_after, alternative = tem, paired = TRUE)
    }
    
    if (tem_test$p.value > 0.05) {
        result_tem <- "유의미한 변화 없음"
    } else {
        result_tem <- "유의미한 변화 있음"
    }
    
    print(glue(
        "{time} 전후로 경피수분손실도(TEWL)에 {result_tewl}
    {time} 전후로 피부수화도(SH)에 {result_sh}
    {time} 전후로 체표면온도(Tem)에 {result_tem}"
    ))
}

print_multiple_results <- function(data, variable, times) {
    test_data <- list()
    Difference <- c()
    Time <- rep(times, each = 20)
    Normality <- list()
    
    for (time in times) {
        data_by_time <- data %>% filter(Time %in% c(glue("{time} Before"), glue("{time} After")))
        y_before <- data_by_time[glue("{variable}_Mean")] %>% slice(seq(1, nrow(data_by_time), by = 2)) %>% pull(glue("{variable}_Mean"))
        y_after <- data_by_time[glue("{variable}_Mean")] %>% slice(seq(2, nrow(data_by_time), by = 2)) %>% pull(glue("{variable}_Mean"))
        y_diff <- y_after - y_before
        Difference <- append(Difference, y_diff)
        Normality[[time]] <- shapiro.test(Difference)
    }
    
    test_data[["Difference"]] <- Difference    
    test_data[["Time"]] <- Time
    
    test_data <- test_data %>% as_tibble()
    equivar_test <- levene.test(test_data$Difference, test_data$Time, location = "mean")
    
    if (all(map_dbl(Normality, "p.value") > 0.05)) {
        if (equivar_test$p.value > 0.05) {
            multiple_test <- oneway.test(formula = Difference ~ Time, data = test_data, var.equal = TRUE)
        } else {
            multiple_test <- oneway.test(formula = Difference ~ Time, data = test_data, var.equal = FALSE)
        }
    } else {
        multiple_test <- kruskal.test(Difference ~ Time, test_data)
    }
    
    time <- paste(times, collapse = ", ")
    
    if (multiple_test$p.value > 0.05) {
        result <- glue("{time}의 플라즈마 조사 전후 {variable} 차이값에 유의미한 차이가 없음")
    } else {
        result <- glue("{time}의 플라즈마 조사 전후 {variable} 차이값에 유의미한 차이가 있음")
    }
    
    print(result)
}