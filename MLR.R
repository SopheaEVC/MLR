libs = c("tidyverse", "rsample", "caret", "h2o", "modeldata")
lapply(libs, require, character.only = TRUE)

h2o.no_progress()
h2o.init()

# Loading: Ames houseing data
ames <- AmesHousing::make_ames()
# Job attribution data
churn <- modeldata::attrition %>%
    mutate_if(is.ordered, .funs = factor, ordered = FALSE)

ames.h2o <- as.h2o(ames)

set.seed(123)
index1 <- sample(1:nrow(ames), round(nrow(ames) * 0.7))
train1 <- ames[index1, ]
test1 <- ames[- index1, ]

# using caret pacakge
set.seed(123)
index2 <- createDataPartition(ames$Sale_Price, 
                               p = 0.7, 
                               list = FALSE)
train2 <- ames[index2, ]
test2 <- ames[-index2, ]

# using rsample package
split1 <- initial_split(ames, prop = 0.7)
training3 <- training(split1)
testing3 <- testing(split1)

# original response distribution
table(churn$Attrition) %>% prop.table()
set.seed(123)
split_strat <- initial_split(churn, 
                             prop = 0.7, 
                             strata = "Attrition")
train_strat <- training(split_strat)
test_strat <- testing(split_strat)

# Consistent response ratio between training and test 
table(train_strat$Attrition) %>% prop.table()
table(test_strat$Attrition) %>% prop.table()
