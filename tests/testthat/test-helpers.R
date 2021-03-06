library(modeldata)
library(dplyr)
library(rules)

# ------------------------------------------------------------------------------

data("Chicago")

Chicago$Cubs <- factor(ifelse(Chicago$Cubs_Home, "home", "away"))

chi_mod  <- Chicago %>% slice(-(1:10)) %>% select(ridership, Cubs, Austin, Clark_Lake)
chi_pred <- Chicago %>% slice(  1:10 ) %>% select(           Cubs, Austin, Clark_Lake)
chi_mod_x <-
  model.matrix(ridership ~ ., data = chi_mod)  %>%
  as.data.frame() %>%
  select(-1)
chi_pred_x <-
  model.matrix( ~ ., data = chi_pred)  %>%
  as.data.frame() %>%
  select(-1)

chi_mod <- as.data.frame(chi_mod)
chi_pred <- as.data.frame(chi_pred)

# ------------------------------------------------------------------------------

data("ad_data")

ad_mod  <- ad_data %>% slice(-(1:10)) %>% select(Class, Genotype, p_tau, MMP10)
ad_pred <- ad_data %>% slice(  1:10 ) %>% select(       Genotype, p_tau, MMP10)
ad_mod_x <-
  model.matrix(Class ~ ., data = ad_mod)  %>%
  as.data.frame() %>%
  select(-1)
ad_pred_x <-
  model.matrix( ~ ., data = ad_pred)  %>%
  as.data.frame() %>%
  select(-1)

ad_mod <- as.data.frame(ad_mod)
ad_pred <- as.data.frame(ad_pred)

# ------------------------------------------------------------------------------

data("hpc_data")

# ------------------------------------------------------------------------------

data(ames, package = "modeldata")

ames <-
  ames %>%
  mutate(Sale_Price = log10(ames$Sale_Price),
         Gr_Liv_Area = log10(ames$Gr_Liv_Area))

set.seed(1001)
keep <- sample(1:nrow(hpc_data), 1510)
in_mod  <- keep[1:1500]
in_pred <- keep[1501:1510]

hpc_mod  <- hpc_data %>% slice(in_mod ) %>% select(class, compounds, protocol, input_fields)
hpc_pred <- hpc_data %>% slice(in_pred) %>% select(       compounds, protocol, input_fields)

hpc_mod <- as.data.frame(hpc_mod)
hpc_pred <- as.data.frame(hpc_pred)
