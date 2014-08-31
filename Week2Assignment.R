#### Question 1 ####
queue <- c("James", "Mary", "Steve", "Alex", "Patricia")
queue <- c(queue, "Harold")
queue <- queue[-which(queue == "James")]
queue <- append(queue, "Pam", after = 1)
queue <- queue[-which(queue == "Harold")]
queue <- queue[-which(queue == "Alex")]
which(queue == "Patricia")
length(queue)
queue

#### Question 2 ####
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


#### Question 3 ####
vector1 <- c(1:1000)
sum.notdivisible <- sum(!((vector1 %% 3) == 0 | (vector1 %% 7) == 0 | (vector1 %% 11) == 0))
sum.notdivisible


#### Question 4 ####
PythagoreanTriple <- function(f, g, h){
    # Determines if the three constans form a Pythagorean Triple.
    #
    # Args:
    #   f: constant
    #   g: constant
    #   h: constant
    #
    # Returns:
    #   True or False
    triple <- c(f, g, h)
    max <- triple[which.max(triple)]
    double <- triple[-which.max(triple)] 
    if(double[1]^2 + double[2]^2 == max^2){
        return(TRUE)
    } else {
        return(FALSE)
    }
}

PythagoreanTriple(5, 13, 12)
PythagoreanTriple(4, 2, 1)
