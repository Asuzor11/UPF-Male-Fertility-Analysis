library(tidyverse)

setwd(dirname(file.choose()))
getwd()
setwd(dirname(file.choose()))
getwd()
setwd(dirname(file.choose()))
getwd()

nutrient <- read_csv("nutrient intake dataset.csv")
household <- read_csv("household.csv")
fertility <- read_csv("fertility dataset.csv")

nutrient_long <- nutrient %>%
  pivot_longer(
    cols = matches("^20"),   # all columns starting with a year
    names_to = "Year_raw",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(substr(Year_raw, 1, 4))  # convert "2001-02" → 2001
  ) %>%
  select(Code, Description, Units, Year, Value)

household_long <- household %>%
  pivot_longer(
    cols = matches("^20"),
    names_to = "Year_raw",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(substr(Year_raw, 1, 4))
  ) %>%
  select(Code, Food.Category = `Food Category`, Units, Year, Value)

fertility_clean <- fertility %>%
  rename(Year = Year) %>%
  mutate(Year = as.numeric(Year))

full_joined <- nutrient_long %>%
  full_join(household_long, by = "Year") %>%
  full_join(fertility_clean, by = "Year")

glimpse(full_joined)

write_csv(full_joined, "my_joined_dataset.csv")

nutrient_yearly <- nutrient_long %>%
  group_by(Year) %>%
  summarise(across(Value, mean, na.rm = TRUE))

household_yearly <- household_long %>%
  group_by(Year) %>%
  summarise(across(Value, sum, na.rm = TRUE))

joined <- fertility_clean %>%
  left_join(nutrient_yearly, by = "Year") %>%
  left_join(household_yearly, by = "Year")

write_csv(fertility_clean, "fertility_clean.csv")

joined <- read.csv("fertility_clean.csv", stringsAsFactors = FALSE)
summary(joined)
head(joined)

#Converts character columns to factors
joined <- joined %>%
  mutate(across(where(is.character), as.factor))

#Remove rows with all missing values
joined <- joined %>%
  filter(!if_all(everything(), is.na))

head(joined)

#Converts character columns to factors
joined_small <- joined %>%
  mutate(across(where(is.character), as.factor))

joined_small <- joined %>%
  filter(!if_all(everything(), is.na))

head(joined_small)

write_csv(joined_small, "joined_small.csv")

getwd()

#removing NA columns
joined_small_clean <- joined_small %>%
  mutate(across(everything(), ~ na_if(.x, "NA"))) %>%   # convert "NA" strings to NA
  select(where(~ !all(is.na(.x))))                      # drop columns that are all NA

library(tidyverse)

#removing NA columns
joined_small_clean <- joined_small %>%
  mutate(across(everything(), ~ na_if(.x, "NA"))) %>%   # convert "NA" strings to NA
  select(where(~ !all(is.na(.x)))) # drop columns that are all NA

ls()
joined_small <- read_csv("joined_small.csv")
setwd(dirname(file.choose()))

joined_small_clean <- joined_small %>%
  mutate(across(everything(), ~ na_if(.x, "NA"))) %>%   
  select(where(~ !all(is.na(.x))))

joined_small_clean <- joined_small %>%
  mutate(across(everything(), ~ na_if(.x, "NA"))) %>%   
  select(where(~ !all(is.na(.x))))

list.files()
joined_small <- read_csv("joined_small.csv")
head(joined_small)

joined_small_clean <- joined_small %>%
  mutate(across(everything(), ~ na_if(.x, "NA"))) %>%
  select(where(~ !all(is.na(.x))))

joined_small_clean <- joined_small %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  select(where(~ !all(is.na(.x))))

write_csv(joined_small_clean, "joined_small_clean.csv")
getwd()

food_categories <- household %>%
  distinct(`Food Category`) %>%
  arrange(`Food Category`)

names(household)

list.files()
household <- read_csv("household.csv")
names(household)

#Extracting the food categories
food_categories <- household %>%
  distinct(`Food Category`) %>%
  arrange(`Food Category`)

#viewing the food categories
View(food_categories)

food_categories

nova_lookup <- tribble(
  ~Food_Category, ~NOVA,
  "Brown and wholemeal bread", 4,
  "Fresh and processed fruit", 1,
  "Fresh and processed fruit and vegetables, excluding potatoes", 1,
  "Fresh and processed fruit and vegetables, including potatoes", 1,
  "Fresh and processed potatoes", 1,
  "Fresh and processed vegetables, excluding potatoes", 1,
  "Fresh and processed vegetables, including potatoes", 1,
  "Milk and milk products excluding cheese", 1,
  "Processed potatoes", 4,
  "Processed vegetables excluding processed potatoes", 3,
  "White bread", 4
)

household_nova <- household %>%
  left_join(nova_lookup, by = c("Food.Category" = "Food_Category"))

household_nova <- household %>%
  left_join(nova_lookup, by = c("Food Category" = "Food_Category"))

head(food_categories)
upf_yearly <- household_nova %>%
  filter(NOVA == 4) %>%
  pivot_longer(cols = matches("^20"), names_to = "Year_raw", values_to = "Value") %>%
  mutate(Year = as.numeric(substr(Year_raw, 1, 4))) %>%
  group_by(Year) %>%
  summarise(UPF = sum(Value, na.rm = TRUE))

head(upf_yearly)

food_categories <- household %>%
  distinct(`Food Category`) %>%
  arrange(`Food Category`)

food_categories

food_categories <- household %>%
  distinct(`Food Category`) %>%
  arrange(`Food Category`)

print(food_categories, n = Inf)

library(tidyverse)

nova_lookup <- tribble(
  ~Food_Category, ~NOVA,
  "Brown and wholemeal bread", 4,
  "Fresh and processed fruit", 1,
  "Fresh and processed fruit and vegetables, excluding potatoes", 1,
  "Fresh and processed fruit and vegetables, including potatoes", 1,
  "Fresh and processed potatoes", 1,
  "Fresh and processed vegetables, excluding potatoes", 1,
  "Fresh and processed vegetables, including potatoes", 1,
  "Milk and milk products excluding cheese", 1,
  "Processed potatoes", 4,
  "Processed vegetables excluding processed potatoes", 3,
  "White bread", 4
)

household_nova <- household %>%
  left_join(nova_lookup, by = c("Food Category" = "Food_Category"))

head(household_nova)

upf_yearly <- household_nova %>%
  filter(NOVA == 4) %>%
  pivot_longer(
    cols = matches("^[0-9]{4}"),
    names_to = "Year_raw",
    values_to = "Value"
  ) %>%
  mutate(Year = as.numeric(substr(Year_raw, 1, 4))) %>%
  group_by(Year) %>%
  summarise(UPF = sum(Value, na.rm = TRUE))

final_dataset <- joined_small_clean %>%
  left_join(upf_yearly, by = "Year")

write_csv(final_dataset, "final_dataset.csv")
getwd()

#Ordinary Least Squares (OLS) Regression — R Code
library(tidyverse)

model_ols <- lm(Fertility ~ UPF, data = final_dataset)
ls()
setwd(dirname(file.choose()))
final_dataset <- read_csv("final_dataset.csv")
model_ols <- lm(Fertility ~ UPF, data = final_dataset)
names(final_dataset)

model_ols <- lm(Standardised.mean.age.of.father ~ UPF, data = final_dataset)
summary(model_ols)

library(tidyverse)

# Select fertility columns
fertility_long <- final_dataset %>%
  select(Year, Under.201, X20.24, X25.29, X30.34, X35.39, 
         X40.44, X45.49, X50.54, X55.59, X60.and.over1) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Age_Group",
    values_to = "Fertility_Value"
  )

# Boxplot
ggplot(fertility_long, aes(x = Age_Group, y = Fertility_Value)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Distribution of Fertility Counts by Age Group",
    x = "Age Group",
    y = "Fertility Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(tidyverse)
library(reshape2)
library(ggplot2)

# Select numeric columns for correlation
corr_data <- final_dataset %>%
  select(UPF, Standardised.mean.age.of.father,
         Under.201, X20.24, X25.29, X30.34, X35.39,
         X40.44, X45.49, X50.54, X55.59, X60.and.over1)

# Compute correlation matrix
corr_matrix <- cor(corr_data, use = "complete.obs")

# Melt for ggplot
corr_melt <- melt(corr_matrix)

# Heatmap
ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1)
  ) +
  theme_minimal() +
  labs(
    title = "Correlation Heatmap: UPF and Fertility Indicators",
    x = "",
    y = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(final_dataset, aes(x = UPF, y = Standardised.mean.age.of.father)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Relationship Between UPF Exposure and Mean Age of Father",
    x = "UPF Exposure",
    y = "Standardised Mean Age of Father"
  )

qqnorm(residuals(model_ols), 
       main = "QQ Plot of OLS Residuals",
       pch = 19, col = "darkblue")
qqline(residuals(model_ols), col = "red", lwd = 2)

#Histogram of UPF exposure
ggplot(final_dataset, aes(x = UPF)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.8
  ) +
  theme_minimal() +
  labs(
    title = "Histogram of UPF Exposure",
    x = "UPF Exposure",
    y = "Frequency"
  )

#Histogram of standardised mean age of father
ggplot(final_dataset, aes(x = Standardised.mean.age.of.father)) +
  geom_histogram(
    bins = 10,
    fill = "darkgreen",
    color = "black",
    alpha = 0.8
  ) +
  theme_minimal() +
  labs(
    title = "Histogram of Standardised Mean Age of Father",
    x = "Mean Age of Father",
    y = "Frequency"
  )

setwd(dirname(file.choose()))
#time series regression
install.packages("prais")
library(prais)

model_ts <- prais_winsten(Standardised.mean.age.of.father ~ UPF, data = final_dataset)
ls()
final_dataset <- read_csv("final_dataset.csv")
library(tidyverse)
final_dataset <- read_csv("final_dataset.csv")
names(final_dataset)
model_ts <- prais_winsten(Standardised.mean.age.of.father ~ UPF, data = final_dataset)
library(prais)

model_ts <- prais_winsten(
  Standardised.mean.age.of.father ~ UPF,
  data = final_dataset,
  index = "Year"
)

summary(model_ts)

install.packages("sandwich")
install.packages("lmtest")

library(sandwich)
library(lmtest)
setwd(dirname(file.choose()))
final_dataset <- read_csv("final_dataset.csv")
model_ols <- lm(Standardised.mean.age.of.father ~ UPF, data = final_dataset)
coeftest(model_ols, vcov = NeweyWest(model_ols, lag = 1, prewhite = FALSE))
install.packages("lmtest")
library(lmtest)
library(sandwich)
coeftest(model_ols, vcov = NeweyWest(model_ols, lag = 1, prewhite = FALSE))
#ACF plot and PACF plot of residuals
ts_resid <- residuals(model_ols)

acf(ts_resid, main = "ACF of Residuals (Model 2)")
pacf(ts_resid, main = "PACF of Residuals (Model 2)")

#lagged regression model
final_dataset <- final_dataset %>%
  arrange(Year) %>%
  mutate(
    UPF_lag1 = lag(UPF, 1),
    UPF_lag2 = lag(UPF, 2),
    UPF_lag3 = lag(UPF, 3)
  )
write_csv(final_dataset, "final_dataset_with_lags.csv")
getwd()
#lag 1 model
model_lag1 <- lm(Standardised.mean.age.of.father ~ UPF_lag1, data = final_dataset)
coeftest(model_lag1, vcov = NeweyWest(model_lag1, lag = 1, prewhite = FALSE))

#lag 2 model
model_lag2 <- lm(Standardised.mean.age.of.father ~ UPF_lag2, data = final_dataset)
coeftest(model_lag2, vcov = NeweyWest(model_lag2, lag = 1, prewhite = FALSE))

#lag 3 model
model_lag3 <- lm(Standardised.mean.age.of.father ~ UPF_lag3, data = final_dataset)
coeftest(model_lag3, vcov = NeweyWest(model_lag3, lag = 1, prewhite = FALSE))

#Running the combined lag model
model_combined <- lm(
  Standardised.mean.age.of.father ~ UPF + UPF_lag1 + UPF_lag2 + UPF_lag3,
  data = final_dataset
)

coeftest(model_combined, vcov = NeweyWest(model_combined, lag = 1, prewhite = FALSE))

