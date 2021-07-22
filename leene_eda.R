library(dplyr)
library(readr)

data <- read_csv("./data/실험수치.csv")


# 30초 전후 비교 ---------------------------------------------------------------

data30 <- data %>% filter(Time %in% c("30초 Before", "30초 After"))

# TEWL
tewl_before30 <- data30["TEWL_Mean"] %>% slice(seq(1, nrow(data30), by = 2)) %>% pull("TEWL_Mean")
tewl_after30 <- data30["TEWL_Mean"] %>% slice(seq(2, nrow(data30), by = 2)) %>% pull("TEWL_Mean")
tewl_diff30 <- tewl_after30 - tewl_before30

# normality test (정규성 검정)
tewl30_sw <- shapiro.test(tewl_diff30) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (tewl30_sw$p.value > 0.05) {
    tewl30_t <- t.test(tewl_diff30, var.equal = TRUE)
} else {
    tewl30_t <- t.test(tewl_diff30, var.equal = FALSE)
}

if (tewl30_t$p.value > 0.025) {
    result_tewl30 <- "유의미한 변화 없음"
} else {
    result_tewl30 <- "유의미한 변화 있음"
}

# SH
sh_before30 <- data30["SH_Mean"] %>% slice(seq(1, nrow(data30), by = 2)) %>% pull("SH_Mean")
sh_after30 <- data30["SH_Mean"] %>% slice(seq(2, nrow(data30), by = 2)) %>% pull("SH_Mean")
sh_diff30 <- sh_after30 - sh_before30

# normality test (정규성 검정)
sh30_sw <- shapiro.test(sh_diff30) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (sh30_sw$p.value > 0.05) {
    sh30_t <- t.test(sh_diff30, var.equal = TRUE)
} else {
    sh30_t <- t.test(sh_diff30, var.equal = FALSE)
}

if (sh30_t$p.value > 0.025) {
    result_sh30 <- "유의미한 변화 없음"
} else {
    result_sh30 <- "유의미한 변화 있음"
}


# Tem
tem_before30 <- data30["Tem_Mean"] %>% slice(seq(1, nrow(data30), by = 2)) %>% pull("Tem_Mean")
tem_after30 <- data30["Tem_Mean"] %>% slice(seq(2, nrow(data30), by = 2)) %>% pull("Tem_Mean")
tem_diff30 <- tem_after30 - tem_before30

# normality test (정규성 검정)
tem30_sw <- shapiro.test(tem_diff30) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (tem30_sw$p.value > 0.05) {
    tem30_t <- t.test(tem_diff30, var.equal = TRUE)
} else {
    tem30_t <- t.test(tem_diff30, var.equal = FALSE)
}

if (tem30_t$p.value > 0.025) {
    result_tem30 <- "유의미한 변화 없음"
} else {
    result_tem30 <- "유의미한 변화 있음"
}

# 1분 전후 비교 ----------------------------------------------------------------

data60 <- data %>% filter(Time %in% c("1분 Before", "1분 After"))

# TEWL
tewl_before60 <- data60["TEWL_Mean"] %>% slice(seq(1, nrow(data60), by = 2)) %>% pull("TEWL_Mean")
tewl_after60 <- data60["TEWL_Mean"] %>% slice(seq(2, nrow(data60), by = 2)) %>% pull("TEWL_Mean")
tewl_diff60 <- tewl_after60 - tewl_before60

# normality test (정규성 검정)
tewl60_sw <- shapiro.test(tewl_diff60) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (tewl60_sw$p.value > 0.05) {
    tewl60_t <- t.test(tewl_diff60, var.equal = TRUE)
} else {
    tewl60_t <- t.test(tewl_diff60, var.equal = FALSE)
}

if (tewl60_t$p.value > 0.025) {
    result_tewl60 <- "유의미한 변화 없음"
} else {
    result_tewl60 <- "유의미한 변화 있음"
}

# SH
sh_before60 <- data60["SH_Mean"] %>% slice(seq(1, nrow(data60), by = 2)) %>% pull("SH_Mean")
sh_after60 <- data60["SH_Mean"] %>% slice(seq(2, nrow(data60), by = 2)) %>% pull("SH_Mean")
sh_diff60 <- sh_after60 - sh_before60

# normality test (정규성 검정)
sh60_sw <- shapiro.test(sh_diff60) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (sh60_sw$p.value > 0.05) {
    sh60_t <- t.test(sh_diff60, var.equal = TRUE)
} else {
    sh60_t <- t.test(sh_diff60, var.equal = FALSE)
}

if (sh60_t$p.value > 0.025) {
    result_sh60 <- "유의미한 변화 없음"
} else {
    result_sh60 <- "유의미한 변화 있음"
}


# Tem
tem_before60 <- data60["Tem_Mean"] %>% slice(seq(1, nrow(data60), by = 2)) %>% pull("Tem_Mean")
tem_after60 <- data60["Tem_Mean"] %>% slice(seq(2, nrow(data60), by = 2)) %>% pull("Tem_Mean")
tem_diff60 <- tem_after60 - tem_before60

# normality test (정규성 검정)
tem60_sw <- shapiro.test(tem_diff60) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (tem60_sw$p.value > 0.05) {
    tem60_t <- t.test(tem_diff60, var.equal = TRUE)
} else {
    tem60_t <- t.test(tem_diff60, var.equal = FALSE)
}

if (tem60_t$p.value > 0.025) {
    result_tem60 <- "유의미한 변화 없음"
} else {
    result_tem60 <- "유의미한 변화 있음"
}

# 2분 전후 비교 ----------------------------------------------------------------

data120 <- data %>% filter(Time %in% c("2분 Before", "2분 After"))

# TEWL
tewl_before120 <- data120["TEWL_Mean"] %>% slice(seq(1, nrow(data120), by = 2)) %>% pull("TEWL_Mean")
tewl_after120 <- data120["TEWL_Mean"] %>% slice(seq(2, nrow(data120), by = 2)) %>% pull("TEWL_Mean")
tewl_diff120 <- tewl_after120 - tewl_before120

# normality test (정규성 검정)
tewl120_sw <- shapiro.test(tewl_diff120) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (tewl120_sw$p.value > 0.05) {
    tewl120_t <- t.test(tewl_diff120, var.equal = TRUE)
} else {
    tewl120_t <- t.test(tewl_diff120, var.equal = FALSE)
}

if (tewl120_t$p.value > 0.025) {
    result_tewl120 <- "유의미한 변화 없음"
} else {
    result_tewl120 <- "유의미한 변화 있음"
}

# SH
sh_before120 <- data120["SH_Mean"] %>% slice(seq(1, nrow(data120), by = 2)) %>% pull("SH_Mean")
sh_after120 <- data120["SH_Mean"] %>% slice(seq(2, nrow(data120), by = 2)) %>% pull("SH_Mean")
sh_diff120 <- sh_after120 - sh_before120

# normality test (정규성 검정)
sh120_sw <- shapiro.test(sh_diff120) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (sh120_sw$p.value > 0.05) {
    sh120_t <- t.test(sh_diff120, var.equal = TRUE)
} else {
    sh120_t <- t.test(sh_diff120, var.equal = FALSE)
}

if (sh120_t$p.value > 0.025) {
    result_sh120 <- "유의미한 변화 없음"
} else {
    result_sh120 <- "유의미한 변화 있음"
}


# Tem
tem_before120 <- data120["Tem_Mean"] %>% slice(seq(1, nrow(data120), by = 2)) %>% pull("Tem_Mean")
tem_after120 <- data120["Tem_Mean"] %>% slice(seq(2, nrow(data120), by = 2)) %>% pull("Tem_Mean")
tem_diff120 <- tem_after120 - tem_before120

# normality test (정규성 검정)
tem120_sw <- shapiro.test(tem_diff120) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (tem120_sw$p.value > 0.05) {
    tem120_t <- t.test(tem_diff120, var.equal = TRUE)
} else {
    tem120_t <- t.test(tem_diff120, var.equal = FALSE)
}

if (tem120_t$p.value > 0.025) {
    result_tem120 <- "유의미한 변화 없음"
} else {
    result_tem120 <- "유의미한 변화 있음"
}

# 4분 전후 비교 ----------------------------------------------------------------

data240 <- data %>% filter(Time %in% c("4분 Before", "4분 After"))

# TEWL
tewl_before240 <- data240["TEWL_Mean"] %>% slice(seq(1, nrow(data240), by = 2)) %>% pull("TEWL_Mean")
tewl_after240 <- data240["TEWL_Mean"] %>% slice(seq(2, nrow(data240), by = 2)) %>% pull("TEWL_Mean")
tewl_diff240 <- tewl_after240 - tewl_before240

# normality test (정규성 검정)
tewl240_sw <- shapiro.test(tewl_diff240) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (tewl240_sw$p.value > 0.05) {
    tewl240_t <- t.test(tewl_diff240, var.equal = TRUE)
} else {
    tewl240_t <- t.test(tewl_diff240, var.equal = FALSE)
}

if (tewl240_t$p.value > 0.025) {
    result_tewl240 <- "유의미한 변화 없음"
} else {
    result_tewl240 <- "유의미한 변화 있음"
}

# SH
sh_before240 <- data240["SH_Mean"] %>% slice(seq(1, nrow(data240), by = 2)) %>% pull("SH_Mean")
sh_after240 <- data240["SH_Mean"] %>% slice(seq(2, nrow(data240), by = 2)) %>% pull("SH_Mean")
sh_diff240 <- sh_after240 - sh_before240

# normality test (정규성 검정)
sh240_sw <- shapiro.test(sh_diff240) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (sh240_sw$p.value > 0.05) {
    sh240_t <- t.test(sh_diff240, var.equal = TRUE)
} else {
    sh240_t <- t.test(sh_diff240, var.equal = FALSE)
}

if (sh240_t$p.value > 0.025) {
    result_sh240 <- "유의미한 변화 없음"
} else {
    result_sh240 <- "유의미한 변화 있음"
}


# Tem
tem_before240 <- data240["Tem_Mean"] %>% slice(seq(1, nrow(data240), by = 2)) %>% pull("Tem_Mean")
tem_after240 <- data240["Tem_Mean"] %>% slice(seq(2, nrow(data240), by = 2)) %>% pull("Tem_Mean")
tem_diff240 <- tem_after240 - tem_before240

# normality test (정규성 검정)
tem240_sw <- shapiro.test(tem_diff240) # p-value = 0.6736 -> 정규성 o

# paired t-test (대응표본 t검정)
if (tem240_sw$p.value > 0.05) {
    tem240_t <- t.test(tem_diff240, var.equal = TRUE)
} else {
    tem240_t <- t.test(tem_diff240, var.equal = FALSE)
}

if (tem240_t$p.value > 0.025) {
    result_tem240 <- "유의미한 변화 없음"
} else {
    result_tem240 <- "유의미한 변화 있음"
}