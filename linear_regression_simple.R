# Data Preprocessing

# Importing the dataset
dataset = read.csv('Salary_Data.csv')
# dataset = dataset[, 2:3]


#Splitting the data into the Training and Test set
#install.packages('caTools')
library('caTools')
set.seed(123)
#primer parametro es y, la variable dependiente,
#segundo el tamaño del training set. devuelve true si va a training set y false si va a test set
split = sample.split(dataset$Salary, SplitRatio=2/3) 
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling. los salarios y edades tienen que estra en la misma escala
# training_set[, 2:3]= scale(training_set[, 2:3]) #las columnas en R empiezan por 1 y no 0. La 1 y 4 no son numericos, ya que eran strings cambiados a vectores
# test_set[, 2:3] = scale(test_set[, 2:3])

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary ~ YearsExperience, 
               data= training_set)

# Predicting the Test set results
y_pred = predict(regressor,newdata = test_set)

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary), colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor,newdata = training_set)), colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
    xlab('Years of experience') +
    ylab('Salary')


# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary), colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor,newdata = training_set)), colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')
              