## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(purrr)
library(rlang)
library(tidyr)
library(recipes)

## ----tree, echo = FALSE, fig.align="center"-----------------------------------
knitr::include_graphics("rules-from-trees.svg")

## ---- eval = FALSE------------------------------------------------------------
#  split_1_low  <- filter(data, A <  a)
#  model_1_low  <- lm(y ~ A, data = split_1_low)
#  
#  split_1_high <- filter(data, A >= a)
#  model_1_high <- lm(y ~ A, data = split_1_high)
#  
#  split_2_low  <- filter(data, A <  a & B <  b)
#  model_2_low  <- lm(y ~ A + B, data = split_2_low)
#  
#  split_2_high <- filter(data, A <  a & B >= b)
#  model_2_high <- lm(y ~ A + B, data = split_2_high)

## ----cubist-penguins----------------------------------------------------------
library(rules)
data(penguins, package = "modeldata")

cubist_fit <- 
  cubist_rules(committees = 2) %>% 
  set_engine("Cubist") %>% 
  fit(body_mass_g ~ ., data = penguins)
cubist_fit

## ----cubist-summary-----------------------------------------------------------
summary(cubist_fit$fit)

## ----cubist-tidy--------------------------------------------------------------
cb_res <- tidy(cubist_fit)
cb_res

## ----stats--------------------------------------------------------------------
library(tidyr)
cb_res %>% 
  dplyr::select(committee, rule_num, statistic) %>% 
  unnest(cols = c(statistic))

## ----cubist-expr--------------------------------------------------------------
library(dplyr)
library(purrr)
library(rlang)

rule_4_filter <- 
  cb_res %>% 
  dplyr::filter(rule_num == 4) %>% 
  pluck("rule") %>%   # <- character string
  parse_expr() %>%    # <- R expression
  eval_tidy(penguins) # <- logical vector

penguins %>% 
  dplyr::slice(which(rule_4_filter))

## ----rule-fit, eval = FALSE---------------------------------------------------
#  data_with_rules <-
#    data %>%
#    mutate(
#      rule_1 = ifelse(A <  a & B <  b, 0, 1),
#      rule_2 = ifelse(A <  a & B >= b, 0, 1),
#      rule_2 = ifelse(A >= a,          0, 1)
#    )
#  
#  rule_fit_model <-
#    glmnet(x = data_with_rules %>% select(A, B, starts_with("rule_") %>% as.matrix(),
#           y = data_with_rules$y,
#           alpha = 1)

## ----preproc------------------------------------------------------------------
penguins <- 
  penguins %>% 
  mutate(body_mass_g = body_mass_g + 0.0) %>% 
  na.omit()

## ----rf-penguins, results = 'hide', warning=FALSE, message=FALSE--------------
rule_fit_spec <- 
  rule_fit(trees = 10, tree_depth = 5, penalty = 0.01) %>% 
  set_engine("xrf") %>%
  set_mode("regression") 

rule_fit_fit <- 
  rule_fit_spec %>% 
  fit(body_mass_g ~ ., data = penguins)

## ----rf-penguins-print--------------------------------------------------------
rule_fit_fit

## ----rf-tidy------------------------------------------------------------------
rf_res <- tidy(rule_fit_fit)
rf_res

## ----rf-cols------------------------------------------------------------------
rf_variable_res <- tidy(rule_fit_fit, unit = "columns")
rf_variable_res

## ----rf-imp-------------------------------------------------------------------
num_rules <- sum(grepl("^r[0-9]*_", unique(rf_res$rule_id))) + 1

rf_variable_res %>% 
  dplyr::filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  summarize(effect = sum(abs(estimate)), .groups = "drop") %>% 
  ungroup() %>% 
  # normalize by number of possible occurrences
  mutate(effect = effect / num_rules ) %>% 
  arrange(desc(effect))

