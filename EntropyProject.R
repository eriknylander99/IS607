setwd("D:/Erik/Documents/data/IS607")
dataset <- read.table("entropy-test-file.csv", header = TRUE, sep = ",")

entropy <- function(vec.d){
    # Calculates the entropy of a data set, treating the dataset as catagorical
    # data.
    #
    # Args:
    #   vec.d: a vector of data that the we will calculate the entropy of.
    #
    # Return:
    #   The entropy of the vector.
    cat <- table(vec.d)
    probs <- cat/length(vec.d)
    cat.transform <- probs*(log10(probs)/log10(2))
    return(-sum(cat.transform))
}

entropy(dataset$answer)

infogain <- function(vec.d, vec.a){
    # Calcualtes the information gain based on the attribute vector which
    # partions the data.
    #
    # Args:
    #   vec.d: a vector of data that we are calculating the entropy on
    #   vec.a: the vector of attributes that we will checking for the
    #       information gain.
    att.count <- table(vec.a)
    probs <- att.count/length(vec.a)
    att.name <- 
    result <- vector()
    for(i in 1:length(att.count)){
        newdata <- t(subset(dataset, vec.a == names(att.count[i]), select = answer))
        branch.entropy <- probs[i]*entropy(newdata)
        result <- c(result, branch.entropy)
    }
    gain <- entropy(vec.d) - sum(result)
    return(gain)
}

infogain(dataset$answer, dataset$attr1)
infogain(dataset$answer, dataset$attr2)
infogain(dataset$answer, dataset$attr3)

decide <- function(df, num){
    # Takes in a dataframe and computes the max information gain for a given
    # column in the dataframe.
    #
    # Args:
    #   df: The dataframe containing the attributes and test column
    #
    # Return:
    #   A named list containg the column index of the attribute with the 
    #   max information gain and a named vector of the information gains
    #   for each attribute.
    # Error Check
    if(is.data.frame(df) == FALSE){
        return("The object is not a dataframe.")
    }
    dataset <- df
    df.names <- names(df)
    info.gains <- vector()
    info.names <- vector()
    max <- 0
    index <- 0
    for(i in 1:ncol(dataset)){
        if(i != num){
            gain <- infogain(dataset[,num], dataset[,i])
            if(gain > max){
                max <- gain
                index <- i
            }
            info.names <- c(info.names, df.names[i])
            info.gains <- c(info.gains, gain)
        } else {
            next
        }
    }
    names(info.gains) <- info.names
    return(list(max = index, gains = info.gains))
}

decide(dataset, 4)

