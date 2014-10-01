# week6quiz.R
# [For your convenience], here is the provided code from Jared Lander's R for Everyone, 
# 6.7 Extract Data from Web Sites

install.packages("XML")
require(XML)
theURL <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
bowlPool <- readHTMLTable(theURL, which = 1, header = FALSE, stringsAsFactors = FALSE)
bowlPool

# 1. What type of data structure is bowlpool? 

class(bowlPool) #data.frame

# 2. Suppose instead you call readHTMLTable() with just the URL argument,
# against the provided URL, as shown below

theURL <- "http://www.w3schools.com/html/html_tables.asp"
hvalues <- readHTMLTable(theURL)

# What is the type of variable returned in hvalues?

class(hvalues) # a list

# 3. Write R code that shows how many HTML tables are represented in hvalues

sum(sapply(hvalues, class) == "data.frame") #2 tables are returned the rest are 'Null'

# 4. Modify the readHTMLTable code so that just the table with Number, 
# FirstName, LastName, # and Points is returned into a dataframe

hvaluesq4 <- readHTMLTable(theURL, which = 1) # returns the first table from the page.

# 5. Modify the returned data frame so only the Last Name and Points columns are shown.

twocols <- subset(hvaluesq4, select = c('Last Name', 'Points'))

# 6 Identify another interesting page on the web with HTML table values.  
# This may be somewhat tricky, because while
# HTML tables are great for web-page scrapers, many HTML designers now prefer 
# creating tables using other methods (such as <div> tags or .png files).  

# Wikipedia's NHL Statistics Page. Lots of fun tables.
URL <- "http://en.wikipedia.org/wiki/List_of_NHL_statistical_leaders_by_country"
nhlstats <- readHTMLTable(URL)

# 7 How many HTML tables does that page contain?

sum(sapply(nhlstats, class) == "data.frame") # 44 tables.

# 8 Identify your web browser, and describe (in one or two sentences) 
# how you view HTML page source in your web browser.
# Google Chrome - Use the Hamburger Menu and select Tools -> View Source. 
# You can also use the Control+U key combo to view the page source.
