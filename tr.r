#program1
prime_numbers <- function(n) {
if (n >= 2) {
 x = seq(2, n)
 prime_nums = c()
 for (i in seq(2, n)) {
 if (any(x == i)) {
 prime_nums = c(prime_nums, i)
 x = c(x[(x %% i) != 0], i)
 }
 }
 return(prime_nums)
 }
 else 
 {
 stop("Input number should be at least 2.")
 }
 } 
prime_numbers(12)

#program 2
revenue=c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28,
9766.09,10305.32, 14379.96, 10713.97, 15433.50)
expenses=c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73,
5821.12, 6976.93, 16618.61, 10054.37, 3803.96)
print("a.Profit of each month")
profit=c(revenue-expenses)
print(profit)
print("b.Profit after tax deduction")
aftertax=c(profit-(profit*0.3))
print(aftertax)
print("c. Profit margin for each month")
profitmargin=c(aftertax/revenue)
print(profitmargin)
m1=mean(c(aftertax))
print("mean")
print(m1)
print("d. Good Months")
ifelse(aftertax>m1,"goodmonths","---")
print("e. Bad Months")
ifelse(aftertax<m1,"Bad Months","--")
best=max(c(aftertax))
print(best)
print("f. Best Month")
ifelse(aftertax>best,"BestMonth","--")
print("g. Worst Month")
min1=min(c(aftertax))
print(min1)
ifelse(aftertax>min1, "Worst Month","--")


#program7

ex.data=data.frame(itemCode=c(1001:1005),ItemCategory=c("Electrics","Desktop
Supplies","Office Supplies", "USB", "CD Drive"),ItemPrice=c(700,300,350,400,800))
print("The table contents is")
print(ex.data)
print("a.Subset the data frame and display the details item price >=350")
ex.data[ex.data$ItemPrice>=350,]
print("b.Subset the data frame and display only the items where the categary is
office supplies or desktop supplies")
ex.data[ex.data$ItemCategory =="Office Supplies"|ex.data$ItemCategory=="Desktop
Supplies",]
print("c. Create another data frame called item-details")
ex1.data=data.frame(itemCode=c(1006),ItemCategory=c("Super
Computer"),ItemPrice=c(1000))
print("newly created data frame is")
print(ex1.data)
print("c.merge two data frames")
rbind(ex.data,ex1.data)

#program3

# Create two 3x3 matrixes.
m1 = matrix(c(1, 2, 3, 4, 5, 6,7,8,9), nrow = 3)
print("Matrix-1:")
print(m1)
m2 = matrix(c(0, 1, 2, 3, 0, 2,2,2,2), nrow = 3)
print("Matrix-2:")
print(m2)

result = m1 + m2
print("Result of addition")
print(result)

result = m1 - m2
print("Result of subtraction")
print(result)

result = m1 * m2
print("Result of multiplication")
print(result)

result = t(m1)
print("Transpose is:")
print(result)

#program1

1.	a) R data type programme

integer
x <- 123L


# print value of x
print(x)

# print type of x
print(class(x))
Boolean data type
bool1 <- TRUE

print(bool1)
print(class(bool1))

bool2 <- FALSE

print(bool2)
print(class(bool2))
numeric data type
# floating point values
weight <- 63.5

print(weight)
print(class(weight))

# real numbers
height <- 182

print(height)
print(class(height))
complex data type
# 2i represents imaginary part
complex_value <- 3 + 2i


# print class of complex_value
print(class(complex_value))
character data type
# create a string variable
fruit <- "Apple"

print(class(fruit))

# create a character variable
my_char <- 'A'

print(class(my_char))
1b. arithmetic and logical operations
x <- 5
y <- 16
x + y
x - y
x * y
y / x
y %/% x
y %% x
y ^ x
%% Modulus operator, %/% integer division
Relational operators
x <- 5
y <- 16
x < y
x > y
x <= 5
y >= 20
y == 16
x != 5


c)Sequence and creation of vectors
#Calling sequence() function 
sequence(1:4) 
  
# Performing operations 
x <- 2 * sequence(4) 
x 

#creation of vectors
vector("numeric",3)
vector("logical",5)
vector("character",2)
fruits <- c("banana", "apple", "orange", "mango", "lemon")
numbers <- c(13, 3, 5, 7, 20, 2)

sort(fruits)  # Sort a string
sort(numbers) # Sort numbers
d) # Create a matrix
thismatrix <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)

# Print the matrix
thismatrix
e) #define vectors
vector1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
vector2 <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
vector3 <- c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30)
#column-bind vectors together into matrix
my_matrix <- cbind(vector1, vector2, vector3)

#view resulting matrix
my_matrix

f) vector
x <- c(8, 4, 10)
x
## [1]  8  4 10
we can, for example, extract the 2nd element,
x[2]
## [1] 4

matrix
x <- matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3)
x
##      [,1] [,2] [,3]
## [1,]    1    3    5
## [2,]    2    4    6
we can, for example, extract the element in the 2nd row and 3rd column (the first index refers to the row, the second to the column),
x[2,3]
## [1] 6
the first row (make sure to include the comma!),
x[1,]
## [1] 1 3 5
or the third column (make sure to include the comma!).
x[,3]
## [1] 5 6

Arrays
# Create a 2-dimensional array
data <- matrix(1:6, nrow = 2)

# Access elements based on a condition
subset <- data[data > 3]

print(subset)

#program11

# Create the scatter plot
plot(data$Years_Exp, data$Salary,
     xlab = "Years Experienced",
     ylab = "Salary",
     main = "Scatter Plot of Years Experienced vs Salary")
install.packages('caTools')
library(caTools)
split = sample.split(data$Salary, SplitRatio = 0.7)
trainingset = subset(data, split == TRUE)
testset = subset(data, split == FALSE)
 
# Fitting Simple Linear Regression to the Training set
lm.r= lm(formula = Salary ~ Years_Exp,
         data = trainingset)
#Summary of the model
summary(lm.r)
# Create a data frame with new input values
new_data <- data.frame(Years_Exp = c(4.0, 4.5, 5.0))
 # Predict using the linear regression model
predicted_salaries <- predict(lm.r, newdata = new_data)
 # Display the predicted salaries
print(predicted_salaries)
# Visualising the Training set results
ggplot() + geom_point(aes(x = trainingset$Years_Ex, 
                          y = trainingset$Salary), colour = 'red') +
  geom_line(aes(x = trainingset$Years_Ex,
                y = predict(lm.r, newdata = trainingset)), colour = 'blue') +
   
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')
  # Visualising the Test set results
ggplot() +
  geom_point(aes(x = testset$Years_Exp, y = testset$Salary),
             colour = 'red') +
  geom_line(aes(x = trainingset$Years_Exp,
                y = predict(lm.r, newdata = trainingset)), 
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')

#program10

library(ggplot2)
library(gridExtra)
data(mtcars)
names(mtcars)
print("a. the total no. of obeservations")
summary(mtcars)
print("b. largest hp and least hp")
Data_Cars=mtcars
max(Data_Cars$hp)
min(Data_Cars$hp)     
aggregate(mpg~am, data=mtcars, FUN=function(x){NROW(x)})
aggregate(mpg~am*cyl, data=mtcars, FUN=function(x){NROW(x)})
aggregate(mpg~am*cyl, data=mtcars, mean)
aggregate(mpg~am*carb, data=mtcars, mean)
print("e which pair of variable has the highest pearson correlation")
sort(cor(mtcars)[1,])
print("c. plot histogram")
par(mfrow=c(1, 2))
# Histogram with Normal Curve
x <- mtcars$mpg
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon",
   main="Histogram Of MPG")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
# Kernel Density Plot
d <- density(mtcars$mpg)
plot(d, xlab="MPG", main ="Density Of MPG")

#program9
ss=read.csv("c://Users/kingo/Desktop/input.csv")
print(ss)
print("a. find number of rows and colums")
# print number of columns 
print(ncol(ss))  
  
# print number of rows 
print(nrow(ss)) 
print("b find maximum salary")
ss=read.csv("c://Users/kingo/Desktop/input.csv") 
maxsalary=max(ss$salary) 
print(maxsalary) 
print("c.Retrive the details of the employee with maximum salary") 
mdata=read.csv(“c://Users/SIRMVIT/Desktop/input.csv”) 
new_csv=subset(mdata, mdata$employee == "maxsalary") 
write.csv(new_csv, "new_sample.csv") 

# Creating a data frame.  
csv_data<- read.csv("input.csv")  
# Getting the maximum salary from data frame.  
max_sal<- max(csv_data$salary)  
print(max_sal)  

# Creating a data frame.  
csv_data<- read.csv("input.csv")  
# Getting the maximum salary from data frame.  
max_sal<- max(csv_data$salary)  
print(max_sal)  
  
#Getting the detais of the pweson who have maximum salary  
details <- subset(csv_data,salary==max(salary))  
print(details)  

# Creating a data frame.  
csv_data<- read.csv("input.csv")  
#Getting the detais of all the pweson who are working in IT department  
details <- subset(csv_data,dept=="IT")  
print(details)  


#Creating a data frame.  
csv_data<- read.csv("input.csv")    
#Getting the detais of all the pweson who are working in IT department  
details <- subset(csv_data,dept=="IT"&salary>600)  
print(details)  





