#### Question 1 ####
meanCalc <- function(vec){
    # Calculates the mean value of a vector.
    #
    # Args:
    #   vec: a vector of numeric values
    #
    # Returns:
    #   The mean value of the vector
    # Error Check
    if(is.numeric(vec) == FALSE){
        return("Not a numeric vector.")
    }
    avg <- sum(vec)/length(vec)
    return(avg)
}

test1 <- c(2,5,6,2,1,4,8,6,7)
meanCalc(test1) # 4.555556
test2 <- c(1:10, NA)
meanCalc(test2) # NA


#### Question 2 ####
meanCalc2 <- function(vec){
    # Calculates the mean value of a vector removing NA's
    #
    # Args:
    #   vec: a vector of numeric values
    #
    # Returns:
    #   The mean value of the vector
    # Error Check
    if(is.numeric(vec) == FALSE){
        return("Not a numeric vector.")
    }
    vec <- vec[!is.na(vec)]
    avg <- sum(vec)/length(vec)
    return(avg)
}

test1 <- c(2,5,6,2,1,4,8,6,7)
meanCalc2(test1) # 4.555556
test2 <- c(1:10, NA)
meanCalc2(test2) # 5.5


#### Question 3 ####
gcd <- function(a, b){
    # Calculates the greatest common divisor of two numbers.
    #
    # Args:
    #   a, b: integer values
    #
    # Returns:
    #   The greatest common divisor of the two inputs.
    nums <- c(a, b)
    # Error Check
    if(is.numeric(nums) == FALSE){
        return("One or both of the inputs is not numeric.")
    }
    min <- abs(nums[which.min(nums)])
    max <- abs(nums[which.max(nums)])
    if(max %% min == 0){
        return(min)        
    } else {
        for(i in min:1){
            if(max %% i == 0 & min %% i == 0){
                return(i)
            }
        }
    }
}

gcd(36, 24) # 12
gcd(150, 60) # 30


#### Question 4 ####
gcdEuclid <- function(a, b){
    # Calculates the greatest common divisor using Euclid's Algorithm.
    #
    # Args:
    #   a, b: numberic values
    #
    # Returns:
    #   The greatest common divisor of the two inputs.
    nums <- c(a, b)
    # Error Check
    if(is.numeric(nums) == FALSE){
        return("One or both of the inputs is not numeric.")
    }
    min <- abs(nums[which.min(nums)])
    max <- abs(nums[which.max(nums)])
    if((max %% min) == 0){
        return(min)
    }else{
        max1 <- min
        min1 <- max %% min
        return(gcdEuclid(max1, min1))
    }
}

gcdEuclid(70, 35) # 35
gcdEuclid(54, 24) # 6
gcdEuclid(-20, -8) # 4


#### Question 5 ####
twoQuad <- function(x, y){
    # Calculates x^2*y + 2x*y - x*y^2
    #
    # Args:
    #   x, y: numeric values
    #
    # Return:
    #   The result of computing the above function.
    # Error Check
    if(is.numeric(c(x, y)) == FALSE){
        return("One or both inputs are not numeric.")
    }
    value <- (x^2*y) + (2*x*y) - (x*y^2)
    return(value)
}

twoQuad(4, 5) # 20
twoQuad(-7, 10) # 1050


#### Question 6 ####
setwd("D:/Erik/Documents/data/IS607")
price.data <- read.table("week-3-price-data.csv", header = TRUE, sep = ",")
make.model <- read.table("week-3-make-model-data.csv", header = TRUE, sep = ",")
price.make.model <- merge(x = price.data, y = make.model, 
                          by.x = "ModelNumber", by.y = "ModelNumber")
# The resulting data frame has 27 observations which is one less then the 28 that
# the price.data data frame. It appears the observation with model number 23120
# was dropped since it's model number is not in the make.model data.

#### Question 7 ####
setwd("D:/Erik/Documents/data/IS607")
price.data <- read.table("week-3-price-data.csv", header = TRUE, sep = ",")
make.model <- read.table("week-3-make-model-data.csv", header = TRUE, sep = ",")
price.make.model2 <- merge(x = price.data, y = make.model, 
                          by.x = "ModelNumber", by.y = "ModelNumber", all = TRUE)
# This resulted in all 28 observations with NA's for the observation with model
# number 23120


#### Question 8 ####
cars.2010 <- subset(price.make.model2, Year == 2010)
head(cars.2010)


#### Question 9 ####
cars.more.10k <- subset(price.make.model2, Color == "Red" & Price > 10000)
head(cars.more.10k)


#### Question 10 ####
cars.reduced <- subset(price.make.model2, select = c(ID, Mileage, Price, Make, Model, Year))
head(cars.reduced)


#### Question 11 ####
numCharacters <- function(vec){
    # Takes a character vector and returns the number of characters in each element of the vector.
    #
    # Args:
    #   vec: A character vector
    #
    # Return:
    #   The number of characters in each element of the vector.
    # Error Check
    if(is.character(vec) == FALSE){
        return("The input vector is not a character vector.")
    }
    num.chars <- nchar(vec)
    return(num.chars)
}

test <- c("Monday", "Friday", "Sat", "Wednesday", "How long is this string")
numCharacters(test)


#### Question 12 ####
addVectors <- function(vec1, vec2){
    # Takes two vectors of equal lengths and concatenates them element by element.
    #
    # Args:
    #   vec1, vec2: Character vectors of equal length.
    #
    # Return:
    #   A vector containing the results of the concatenation of the two vectors.
    # Error Check
    if(length(vec1) != length(vec2)){
        return("The inputs vectors are of unequal length.")
    }
    result <- paste(vec1, vec2, sep = " ")
    return(result)
}

test1 <- rep("Combine", 10)
test2 <- rep("Strings", 10)
addVectors(test1, test2)

test3 <- rep("Unequal", 10)
test4 <- rep("Lengths", 6)
addVectors(test3, test4)


#### Question 13 ####
require(stringr)
substrThree <- function(vec){
    # Returns a substring of length three that begins with the first vowel in the string.
    # 
    # Args:
    #   vec: Character vector used to create the vector of substrings returns NA if there are
    #   no vowels in the word or if the string has length < 3.
    # Errror Check
    if(is.character(vec) == FALSE){
        return("The vector is not a character vector.")
    }
    vowels <- c("a", "e", "i", "o", "u")
    sub <- character()
    vec.length = length(vec)
    for(i in vec){
        location <- str_locate(i, ignore.case(vowels))
        string <- str_sub(string = i, start = location[which.min(location)], end = location[which.min(location)] + 2)
        if(length(string) == 0 && typeof(string) == "character"){
            sub <- c(sub, NA)
        } else if(str_length(string) < 3){
            sub <- c(sub, NA)
        } else {
            sub <- c(sub, string)
        }         
    }
    return(sub)
}

test.new <- c("Monday", "Potato", "Three", "Pony", "Dragon", "Orange", "Thrhrhrhrh", "aeiou", "A I O")
substrThree(test.new)
# "ond" "ota" NA "ony" "ago" "Ora" NA "aei" "A I"


#### Question 14 ####
# Using the lubridate package as sugested in the Data Manipulation with R text.
require(lubridate)
month <- c(1, 6, 3, 12, 10, 1, 8)
day <- c(14, 12, 30, 14, 8, 23, 19)
year <- c(1981, 1994, 2001, 1976, 2010, 2014, 2008)
dates <- data.frame(Month = month, Day = day, Year = year)
dates$date.format <- mdy(paste(dates$Month, dates$Day, dates$Year))


#### Question 15 ####
require(lubridate)
date.string <- "09-06-2014"
date.date <- mdy(date.string)


#### Question 16 ####
require(lubridate)
date.string <- "09-06-2014"
date.date <- mdy(date.string)
month(date.date)


#### Question 17 ####
date.seq <- seq(mdy("1-1-2005"), mdy("12-31-2014"),  by = "day")
head(date.seq) # "2005-01-01 UTC" "2005-01-02 UTC" "2005-01-03 UTC" "2005-01-04 UTC" "2005-01-05 UTC" "2005-01-06 UTC"
tail(date.seq) # "2014-12-26 UTC" "2014-12-27 UTC" "2014-12-28 UTC" "2014-12-29 UTC" "2014-12-30 UTC" "2014-12-31 UTC"
