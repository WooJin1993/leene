rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggpubr)
library(glue)
library(lawstat)
library(purrr)
library(readr)
library(wPerm)

options(scipen = 999) # turn-off scientific notation like 1e+48
options(tibble.width = Inf)
source("./functions_table.R")


data <- read_csv("./data/실험수치.csv")

# 알레르기 있는 샛별이와 송이를 포함한 결과 (n = 20)
    
## 30초 전후 비교
    
result30 <- get_paired_table(data, time = "30초", tewl = "greater", sh = "less", tem = "greater")

## 1분 전후 비교
    
result60 <- get_paired_table(data, time = "1분", tewl = "greater", sh = "less", tem = "greater")

## 2분 전후 비교
    
result120 <- get_paired_table(data, time = "2분", tewl = "greater", sh = "less", tem = "greater")

## 4분 전후 비교

result240 <- get_paired_table(data, time = "4분", tewl = "greater", sh = "less", tem = "greater")

df <- bind_rows(result30, result60, result120, result240)

# 알레르기 있는 샛별이와 송이를 포함하지 않은 결과 (n = 18)
    
data <- data %>% filter(Name %!in% c("샛별이", "송이"))

## 30초 전후 비교

result30 <- get_paired_table(data, time = "30초", tewl = "greater", sh = "less", tem = "greater")

## 1분 전후 비교
    
result60 <- get_paired_table(data, time = "1분", tewl = "greater", sh = "less", tem = "greater")
    
## 2분 전후 비교
    
result120 <- get_paired_table(data, time = "2분", tewl = "greater", sh = "less", tem = "greater")

## 4분 전후 비교
    
result240 <- get_paired_table(data, time = "4분", tewl = "greater", sh = "less", tem = "greater")

df <- bind_rows(df, result30, result60, result120, result240)

