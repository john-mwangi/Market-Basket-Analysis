## ----setup, include=FALSE------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------
rm(list = ls())


## ------------------------------------------------
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, arules, arulesViz, doParallel)


## ------------------------------------------------
receipts_raw <- read.csv("./75000-out1.csv", header = FALSE) %>% mutate_all(factor)
products <- read.csv("./goods_description.csv")

dim(products)
dim(receipts_raw)
length(unique(receipts_raw$V1))


## ------------------------------------------------
receipts_raw %>% 
  count(V1, sort = TRUE) %>% 
  head()

receipts_raw %>% 
  filter(V1 %in% c("43","45","46")) %>% 
  group_by(V1) %>% 
  sample_n(3)


## ------------------------------------------------
missing <- apply(X = receipts_raw, MARGIN = 1, FUN = function(x) sum(is.na(x)))

receipts <- receipts_raw %>%
  mutate(missing = missing) %>% 
  group_by(V1) %>% 
  arrange(missing, .by_group = TRUE) %>% 
  slice(1, .preserve = TRUE) %>% 
  select(-missing) %>% 
  ungroup()

dim(receipts)
length(unique(receipts$V1))
head(receipts)


## ------------------------------------------------
# Extract product ID
trans_id <- receipts$V1
receipts$V1 <- NULL


## ------------------------------------------------
# Start parallel computing
cl <- makePSOCKcluster(detectCores(logical = FALSE))
registerDoParallel(cl)


## ------------------------------------------------
# Creating dummy variables
gc()

receipts_dum <- receipts %>%
  mutate(purchases = glue::glue("{V2},{V3},{V4},{V5}")) %>% 
  fastDummies::dummy_cols(select_columns = "purchases", split = ",")

gc()
  
dim(receipts_dum)
head(receipts_dum)


## ------------------------------------------------
# End parallel computing
stopCluster(cl)
registerDoSEQ()


## ------------------------------------------------
# Matching names of dummy variables to product IDs
receipts_dum <- receipts_dum %>% select(-c(1:5),-purchases_NA)

headers <- names(receipts_dum)

headers <- str_remove_all(string = headers, pattern = "purchases_")

receipts_dum <- setNames(receipts_dum, headers)

dim(receipts_dum)
head(receipts_dum)


## ------------------------------------------------
products %>% 
  mutate(ID = factor(ID)) %>% 
  select(ID, ARTICLE, CATEGORY) %>% 
  bind_cols(data.frame(ID.y = colnames(receipts_dum))) %>% 
  mutate(match = ID==ID.y)


## ------------------------------------------------
# Re-order the columns
headers <- as.character(sort(as.integer(headers)))

receipts_dum <- receipts_dum %>% select(all_of(headers))

head(receipts_dum)


## ------------------------------------------------
# Conversion to matrix and assigning dimension names
trans <- as.matrix(receipts_dum)

item <- paste0(products$ARTICLE," ",products$CATEGORY)

dimnames(trans) <- list(trans_id,item)


## ------------------------------------------------
# Convert to transaction format
trans <- as(object = trans, Class = "transactions")

inspect(trans[1:6])


## ------------------------------------------------
itemFrequencyPlot(trans, topN = 15, type = "absolute")
itemFrequencyPlot(trans, topN = 15, type = "relative")


## ------------------------------------------------
itemFrequency(trans)[c("Green Tea","Hot Coffee","Coffee Eclair")]

sum(trans %in% "Green Tea")/length(trans)


## ------------------------------------------------
rules <- apriori(data = trans, parameter = list(support = 100/length(trans),
                                                confidence = 0.6))

length(rules)


## ------------------------------------------------
rules <- rules[!is.redundant(rules)]

length(rules)


## ------------------------------------------------
inspect(rules[23])


## ------------------------------------------------
subset(x = rules, rhs %in% c("Apple Pie"))

inspect(subset(x = rules, rhs %in% c("Apple Pie")))


## ------------------------------------------------
inspect(subset(x = rules, lhs %ain% c("Raspberry Cookie","Raspberry Lemonade")))


## ------------------------------------------------
inspect(head(sort(x = rules, by = "confidence", decreasing = TRUE),5))


## ------------------------------------------------
sum(trans %ain% c("Coffee Eclair","Almond Twist","Hot Coffee","Apple Pie"))

inspect(trans[c(2,75,938)])


## ------------------------------------------------
inspect(head(sort(x = rules, by="support", decreasing = TRUE)))

sum(trans %ain% c("Opera Cake","Apricot Danish","Cherry Tart"))

inspect(head(trans[trans %ain% c("Opera Cake","Apricot Danish","Cherry Tart")]))


## ------------------------------------------------
new_rules <- apriori(data = trans, parameter = list(support = 100/length(trans),
                                       confidence = 0.5,
                                       maxlen = 2))

inspect(sort(x = new_rules, by="confidence", decreasing = TRUE))


## ------------------------------------------------
rule_max <- which.max(unlist(inspect(new_rules)["confidence"]))


## ------------------------------------------------
itemFrequency(trans)["Cherry Tart"]

itemFrequency(trans)["Cherry Tart"]*6.436839


## ------------------------------------------------
is.significant(x = new_rules[rule_max], transactions = trans, alpha = 0.05)


## ------------------------------------------------
is.significant(x = new_rules, transactions = trans)


## ------------------------------------------------
set.seed(123)
plot(new_rules, method = "graph", control = list(cex=0.7))


## ------------------------------------------------
trans_lift <- function(n,nA,nB,nAB){
  (nAB/n)/(nA/n*nB/n)
}

trans_lift(5000,300,375,37)


## ------------------------------------------------
lift_pval <- function(n,nA,nB,nAB){
  1 - phyper(nAB - 1,nA,n-nA,nB)
}

lift_pval(5000,300,375,37)


## ------------------------------------------------
data(EX6.CLICK, package = "regclass")


## ------------------------------------------------
EX6.CLICK$x1 <- as.factor(EX6.CLICK$x1)

trans_clicks <- as(object = EX6.CLICK, Class = "transactions")

inspect(head(trans_clicks[1]))


## ------------------------------------------------
table(EX6.CLICK$Click)
table(EX6.CLICK$Click)/length(EX6.CLICK$Click)


## ------------------------------------------------
click_rules <- apriori(data = trans_clicks, 
        parameter = list(confidence=0.36, support=0.01),
        appearance = list(rhs="Click=Yes"))


## ------------------------------------------------
length(click_rules)

click_rules <- click_rules[!is.redundant(click_rules)]

length(click_rules)


## ------------------------------------------------
inspect(sort(x = click_rules, by="confidence", decreasing = TRUE))


## ------------------------------------------------
log_model <- glm(formula = Click ~ ., family = "binomial", data = EX6.CLICK)


## ------------------------------------------------
broom::tidy(log_model) %>% 
  mutate(perc = scales::percent(exp(estimate) - 1, accuracy = 1)) %>% 
  mutate(p_val = p.value<=0.05) %>% 
  filter(p_val) %>%
  arrange(term)


## ------------------------------------------------
chisq.test(x = EX6.CLICK$SiteID, y = EX6.CLICK$Click)


## ------------------------------------------------
chi_results_raw <- apply(X = EX6.CLICK,
      MARGIN = 2,
      FUN = function(x) chisq.test(x = x, y = EX6.CLICK$Click))


chi_results <- do.call(rbind,chi_results_raw)

data.frame(chi_results) %>% 
  select(statistic, p.value) %>% 
  rownames_to_column() %>% 
  unnest(cols = everything()) %>% 
  mutate(p_val = p.value<=0.05) %>% 
  filter(rowname!="Click") %>% 
  arrange(p.value)


## ------------------------------------------------
log_anova <- anova(object = log_model, test = "Chisq")


## ------------------------------------------------
broom::tidy(log_anova) %>% 
  mutate(p_val = p.value<=0.05) %>% 
  filter(p_val) %>% 
  arrange(p.value)


## ------------------------------------------------
EX6.CLICK_dum <- EX6.CLICK %>% 
  select(-Click) %>% 
  fastDummies::dummy_cols(remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE)


## ------------------------------------------------
pca_clicks <- prcomp(x = EX6.CLICK_dum, center = TRUE, scale. = TRUE)


## ------------------------------------------------
plot(pca_clicks, xlab="Principal Components")


## ------------------------------------------------
summary(pca_clicks)


## ------------------------------------------------
pca_clicks$rotation %>%
  as.data.frame() %>% 
  select(PC1) %>% 
  slice(1:5,70:74) %>% 
  arrange(-PC1) %>%
  rownames_to_column("var") %>% 
  mutate(var = str_replace(var, "_", ": ")) %>% 
  mutate(var = fct_reorder(var, PC1)) %>% 
  ggplot(aes(x = var, y = PC1, fill = PC1>0)) +
  geom_col() +
  coord_flip() +
  labs(title = "Combination of feature of the PC1 (14%)",
       subtitle = "Green represents combination of features most influential to CTR",
       fill = "Positive\ninfluence?",
       x = "") +
  theme_light()

