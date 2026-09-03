# Yuva Intern Week 4 - Comprehensive Data Analysis

library(ggplot2)
data(iris)

dim(iris)
str(iris)
summary(iris)
colSums(is.na(iris))

# Check and clean missing values
colSums(is.na(iris))

# Remove duplicate rows if present
iris_clean <- iris[!duplicated(iris), ]

# Check cleaned data
dim(iris_clean)
summary(iris_clean)

ggplot(iris_clean, aes(x = Species, y = Sepal.Length)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title = "Average Sepal Length by Iris Species",
       x = "Species", y = "Average sepal length (cm)") +
  theme_minimal()

ggplot(iris_clean, aes(x = Petal.Length, y = Petal.Width,
                 color = Species)) +
  geom_point(size = 2.5, alpha = 0.75) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Petal Length vs Petal Width",
       x = "Petal length (cm)", y = "Petal width (cm)",
       color = "Species") +
  theme_minimal()

ggplot(iris_clean, aes(x = Petal.Length)) +
  geom_histogram(bins = 12, color = "black") +
  labs(title = "Distribution of Petal Length",
       x = "Petal length (cm)", y = "Number of flowers") +
  theme_minimal()

ggplot(iris_clean, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot() +
  labs(title = "Sepal Width Distribution by Species",
       x = "Species", y = "Sepal width (cm)") +
  theme_minimal()

iris_clean$Observation <- seq_len(nrow(iris_clean))

ggplot(iris_clean, aes(x = Observation, y = Petal.Length)) +
  geom_line() +
  geom_vline(xintercept = c(50.5, 100.5), linetype = "dashed") +
  labs(title = "Petal Length Across Dataset Observations",
       x = "Observation number", y = "Petal length (cm)") +
  theme_minimal()

# Predictive modeling with a Decision Tree
set.seed(42)
idx <- sample(seq_len(nrow(iris_clean)), size = 0.8*nrow(iris_clean))
train <- iris_clean[idx, ]
test <- iris_clean[-idx, ]

library(rpart)
library(rpart.plot)

tree_model <- rpart(Species ~ Sepal.Length + Sepal.Width +
                    Petal.Length + Petal.Width,
                    data = train, method = "class")

pred <- predict(tree_model, test, type = "class")
mean(pred == test$Species)

table(Actual = test$Species, Predicted = pred)