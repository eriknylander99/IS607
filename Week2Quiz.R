#### Question 1 #####
vector1 <- c(1:10, 5:-5)


#### Question 2 ####
vector1.char <- as.character(vector1)


#### Question 3 ####
vector1.fac <- as.factor(vector1)


#### Question 4 ####
levels(vector1.fac)


#### Question 5 ####
vector.formula <- 3*vector1^2 - 4*vector1 + 1


#### Question 6 ####
#I could not get R to split the definition of X over multiple lines.
X <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 5, 4, 6, 2, 3, 2, 7, 8, 8, 9, 4, 7, 4, 9, 6, 4), nrow = 8)
y <- matrix(c(45.2, 46.9, 31.0, 35.3, 25.0, 43.1, 41.0, 35.1), nrow=8)

vector.bhat <- solve(t(X) %*% X) %*% t(X) %*% y


#### Question 7 ####
named.list <- list(vector = vector1, charvector = vector1.char, factorvector = vector1.fac)
named.list$vector


#### Question 8 ####
char <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
fact <- factor(c("one", "two", "one", "three", "three", "two", "one", "two", "three", "one"))
num <- seq(1, 40, by=4)
date <- seq(as.Date("2014/8/28"), by = "week", length.out = 10)

df <- data.frame(char, fact, num, date)


#### Question 9 ####
data.add <- data.frame(char = "k", fact = factor("four"), num = 44, date = as.Date("2010/6/20"))
df <- rbind(df, data.add)


#### Question 10 ####
temps <- read.csv("temperatures.csv")


#### Question 11 ####
measurments <- read.table("D:/Erik/Documents/data/measurements.txt")


#### Question 12 ####
thefile <- "http://www.data.org/youwantdata/quizdata.txt"
data <- read.table(thefile, header = TRUE, sep = "|")


#### Question 13 ####
factorial <- value <- 12
for(i in 1:(value-1)){
    factorial <- factorial * (value - i)
}
factorial


#### Question 14 ####
years <- 6
compounds <- 12
principle <- 1500
interest <- .0324
for(i in 1:(years*compounds)){
    principle <- principle + (interest / compounds) * principle
}
amount <- round(principle, digits = 2)
amount


#### Question 15 ####
vector2 <- c(1:20)
#I am assuming that we take the first element and add every 3rd
#after that one
sum(vector2[seq(1, length(vector2), by=3)])
#If we want to add the 3rd and every 3 after then we use the following
sum(vector2[seq(3, length(vector2), by=3)])


#### Question 16 ####
start <-  1
end <- 10
x <- 2
sum <- 0
for(i in start:end){
    sum <- sum + x^i
}
sum


#### Question 17 ####
start <-  1
end <- 10
x <- 2
sum <- 0
while(start <= end){
    sum <- sum + x^start
    start <- start + 1
}
sum


#### Question 18 ####
vector.sum <- sum(2^c(1:10))
vector.sum


#### Question 19 ####
vector.by5 <- seq(20, 50, by=5)
class(vector.by5)


#### Question 20 ####
vector10.char <- rep("example", 10)
class(vector10.char)


### Question 21 ####
QuadraticFormula <- function(a, b, c){
    # Computes the roots of a quadratic using the quadratic formula.
    # 
    # Args:
    #   a: coefficient of x^2
    #   b: coefficient of x
    #   c: constant value
    #
    # Returns:
    #   The roots of the quadratic as vector of two values
    if(b^2 - (4 * a * c) < 0){
        x.1 <- (-b + sqrt(as.complex(b^2 - 4 * a * c))) / (2 * a)
        x.2 <- (-b - sqrt(as.complex(b^2 - 4 * a * c))) / (2 * a)
        roots <- c(x.1, x.2)
        return(roots)  
    }
    x.1 <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
    x.2 <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
    roots <- c(x.1, x.2)
    return(roots)
}

QuadraticFormula(4, 2, 2)
