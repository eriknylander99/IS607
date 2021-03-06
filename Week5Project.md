---
output: pdf_document
---

The Virginia End of Course Tests in Mathematics
========================================================


The data that I chose to look at for this project is the aggregated results on the Virginia Standards of Learning, End of Course Tests, in mathematics. These tests are given to students at the completion of a course of study in Algebra 1, Geometry, and Algebra II. Virginia elected not to adopt the Common Core Standards and use these test to determine a school's accreditation standing and are report these results to the federal government in compliance with the Elementary and Secondary Education Act (No Child Left Behind).

2013-2014 Test results
--------------------------------------------------------

```{r Loading data and tools:}
require(dplyr)
require(ggplot2)
require(psych)
matheoc <- read.csv("D:/Erik/Documents/data/2013-14_math_eoc.csv")
matheoc_df <- tbl_df(matheoc)
```


The data consists of 521,821 observations and 19 variables broken into columns. The data is relatively tidy with each of the columns representing a variable. However the LEVEL_CODE column contains the same data repeated first by school division and then again by individual school. The school division information is also contained in the by school section so I will be dropping the by school division section from the data set.
```{r Tidying data part 1}
math_df <- matheoc_df %>%
    filter(LEVEL_CODE == 'SCH')
math_df
```


Variable Analysis
---------------------------------------------------------

### SCHOOL_YEAR, LEVEL_CODE, SUBJECT, and TEST_LEVEL

Filtering the data set generates a new data set with 384,190 observations and eliminates the duplicated observations in the data. Next I need to deal with the different data columns and explore each of the variables. Starting with the columns that are just giving us basic information about the data set. SCHOOL_YEAR, LEVEL_CODE, SUBJECT, and TEST_LEVEL are all identifiers that can be used to combine the data with other data sets.
```{r Basic Identifiers}
summary(math_df$SCHOOL_YEAR)
summary(math_df$LEVEL_CODE)
summary(math_df$SUBJECT)
summary(math_df$TEST_LEVEL)
```


These three variables give us no useful information for the analysis since they are the same for every observation and I will select them out of the data. It is interesting to note that all of the test are classified as EOC but there are 236,103 tests for the TEST_LEVEL variable that were not coded.
```{r Tidying the data part 2}
math_df <- matheoc_df %>%
    filter(LEVEL_CODE == 'SCH') %>%
    select(-SCHOOL_YEAR, -LEVEL_CODE, -SUBJECT, -TEST_LEVEL)
```


### DIV_NUM, DIV_NAME, SCH_NUM, and SCHOOL_NAME

The next variables that I will look at are identifying variables that are used to determine the school systems and schools that the students are enrolled in. DIV_NUM is a unique division number that is given by the state and DIV_NAME is the name of the school division. SCH_NUM is also a unique number given to every school in the state of Virginia and SCH_NAME is the name of the school. These are fixed variables in our data set and will be used to group our further analysis.

One of the things that we see in this data is the number of students tested.
```{r Number of Students Teste}
student.tests <- math_df %>%
    group_by(DIV_NAME) %>%
    summarise(tests = n())
```
```{r}
ggplot(student.tests, aes(x = tests)) + 
    geom_density(fill = 'blue') +
    scale_x_log10() +
    ggtitle("EOC Math Tests Given by Division")
```


This graph shows that the majority of school systems test around 1000 students, but there is a small hump at both ends of the graph. The next thing we will do is explore what school systems are in these areas.
```{r Largest Schools}
math_df %>%
    group_by(DIV_NAME) %>%
    summarise(tests = n()) %>%
    filter(tests > mean(tests) + 3*sd(tests))
```


This gives us some unexpected information. There are 4 school systems that test 3 standard deviations more than the mean number of students tested by the rest of the state. These divisions are centered around the major population centers in the state. The first is the Northern Virginia/Washington DC area with Farifax County, Loudoun County, and Prince William County. The second the Virginia Beach/Norfolk/Hampton Rhodes area with Virginia Beach City. This is one of the interesting areas of Virginia in that Virginia Beach City is the entire incorporated county of Virginia Beach. These divisions also test almost a third of the students int the state.


The other interesting bump is on the low end.
```{r Smallest Schools}
math_df %>%
    group_by(DIV_NAME) %>%
    summarise(tests = n()) %>%
    filter(tests < 400)
```


This also gives some information about the size of the school systems in Virginia. There are 5 schools, Highland, Bath, Colonial Beach, Covington, and Lexington, that test less than 400 students with two that test less than 150. An question that we can't answer with this data is the percentage of students that are tested on the math End of Course tests in a given school system. There is the possibility that some school systems test a significantly smaller or larger percentage of their students then other schools.


### TEST

The next variable that we need to look at is the TEST variable. This is a categorical variable with 4 levels for the four possible tests, Algebra 1, Geometry, Algebra II, and Math. The Math test is representative of the 8th grade math end of course test and would only be given because a student failed the test in middle school but is trying to retake the test for a modified standard diploma.
```{r}
summary(math_df$TEST)
```

From the summary there are 88,016 tests that were not identified. We are going to re-label these as NA's and will in general exclude them in any analysis based on the test given.
```{r}
math_df$TEST[which(math_df$TEST == "")] = NA
```


### FEDRAL_RACE_CODE, GENDER, DISABILITY_FLAG, LEP_FLAG, DISADVANTAGED_FLAG

These variables are one of the areas that I will be looking at to see if there are relationships between these variables and the SOL_SCALE_SCORE. We do have a number of missing values for each of these variables so we will set them to NA. Some schools and some students elect not to report this information and as can be seen at http://www.doe.virginia.gov/statistics_reports/research_data/ the state suppress this information for any group that has less than ten members in it to keep individual students from being identified.
```{r Data Tidying 3}
math_df$FEDERAL_RACE_CODE[which(math_df$FEDERAL_RACE_CODE == "")] = NA
math_df$GENDER[which(math_df$GENDER == "")] = NA
math_df$DISABILITY_FLAG[which(math_df$DISABILITY_FLAG == "")] = NA
math_df$LEP_FLAG[which(math_df$LEP_FLAG == "")] = NA
math_df$DISADVANTAGED_FLAG[which(math_df$DISADVANTAGED_FLAG == "")] = NA
```


#### Explination for the variable values:

FEDERAL_RACE_CODE:

* 1 = Native American/Alaska Native
* 2 = Asian
* 3 = Black or African/American
* 4 = Hispanic of any race
* 5 = White
* 6 = Native Hawaiian/Other Pacific Islander
* 99 = Two or more races

DISABILITY_FLAG:

A person having intellectual disability; hearing impairment, including deafness; speech or language impairment; visual impairment, including blindness; serious emotional disturbance (hereafter referred to as emotional disturbance); orthopedic impairment; autism; traumatic brain injury; developmental delay; other health impairment; specific learning disability; deaf-blindness; or multiple disabilities and who, by reason thereof, receive special education and related services under the Individuals with Disabilities Education Act (IDEA) according to an Individualized Education Program (IEP), Individual Family Service Plan (IFSP), or service plan.

LEP_FLAG:

Students

A) who are ages 3 through 21;

(B) who are enrolled or preparing to enroll in an elementary school or a secondary school

(C) (who are i, ii, or iii) (i) who were not born in the United States or whose native languages are languages other than English; (ii) (who are I and II) (I) who are a Native American or Alaska Native, or a native resident of the outlying areas; and (II) who come from an environment where languages other than English have a significant impact on their level of language proficiency; or (iii) who are migratory, whose native languages are languages other than English, and who come from an environment where languages other than English are dominant; and

(D) whose difficulties in speaking, reading, writing, or understanding the English language may be sufficient to deny the individuals (who are denied i or ii or iii) (i) the ability to meet the state's proficient level of achievement on state assessments described in section 1111(b)(3); (ii) the ability to successfully achieve in classrooms where the language of instruction is English; or (iii) the opportunity to participate fully in society

DISADVANTAGED_FLAG:

A flag that identifies students as economically disadvantaged if they meet any one of the following: 1) is eligible for Free/Reduced Meals, or 2) receives TANF, or 3) is eligible for Medicaid, or 4) identified as either Migrant or experiencing Homelessness.


### AVG_SOL_SCALE_SCORE

This is the primary variable of interest in this data set. The score on this test is what is used to determine if a student has adequately mastered the course this the variable that I will be spending most of my time evaluating since it has the most meaning to this data set. The test has three levels; any score under 400 is a fail, any score greater than or equal to 400 is a pass, and any score greater than or equal to a 500 is an advanced pass. The maximum score on the test is a 600. The other variables in the data set are summary variables for looking at the entire states data and will be dropped for this analysis.

```{r Data Tidying 4}
math <- math_df %>%
    select(DIV_NUM:AVG_SOL_SCALE_SCORE)
```

Plotting the SOL Scores based on tests.

```{r Plotting SOL Scores}
EOC <- math %>%
    filter(TEST == 'Algebra I' | TEST == 'Geometry' | TEST == 'Algebra II')
```
```{r}
ggplot(EOC, aes(AVG_SOL_SCALE_SCORE, fill = TEST)) + 
    geom_density(alpha = 0.2) +
    ggtitle('EOC Math Tests')

ggplot(EOC, aes(AVG_SOL_SCALE_SCORE, fill = TEST)) +
    geom_histogram(binwidth = 10, alpha = 0.3) +
    ggtitle('EOC Math Tests')
```

This data set has an unusual feature. The Algebra I and Geometry tests are very tightly bunched just above the pass rate of a 400 for the SOL. Looking at the histogram we see that there is a smoother drop-off on scores above a 400. However if we look at the scores less than a 400 there is a very rapid drop off. This feature in a data set and often indicates that there is some type of manipulation, intentional or otherwise, going on with the data or the test results.

Looking at Correlations
---------------------------------------

The final thing that I will take a look at is the possibility for correlations between different variables. I will start this by looking at the pairs.panels plots on a random subset of the data and the explore any relationships with the full data set.
```{r Checking for Correlations}
set.seed(42)
math_sub <- math[sample(nrow(math), 3000),]
```
```{r}
pairs.panels(math_sub)
```

From the output of the pairs.panels there seems to be a correlations between the SOL Score, Disability Status, Limited English Proficiency, and Disadvantaged. Next I'll look at these with the full data set.

```{r Exploring SOL vs Disability}
disability <- math %>%
    filter(!is.na(DISABILITY_FLAG))
```
```{r}
ggplot(disability, aes(factor(DISABILITY_FLAG), AVG_SOL_SCALE_SCORE)) +
    geom_violin() +
    ggtitle('SOL Score vs Disability Status')
```

From this plot we see that there appears to be a correlation between disability status and achieving a passing score on the SOL. It appears to be significantly more difficult to pass the SOL if a student has a disability. You can also see the bulge at the bottom end that is also seen in the histograms. 

```{r Exploring SOL vs English Profficiency}
lep <- math %>%
    filter(!is.na(LEP_FLAG))
```
```{r}
ggplot(lep, aes(factor(LEP_FLAG), AVG_SOL_SCALE_SCORE)) +
    geom_violin() +
    ggtitle('SOL Score vs Limited English Proficiency')
```

From this plot we see that there appears to be some correlation between LEP status and SOL scores. With scores being slightly lower for students with a limited English proficiency. Judging from the plot it does put students more at risk to fail the SOL if they LEP status is yes.

```{r Exploring SOL vs Disadvantaged}
dis <- math %>%
    filter(!is.na(DISADVANTAGED_FLAG))
```
```{r}
ggplot(dis, aes(factor(DISADVANTAGED_FLAG), AVG_SOL_SCALE_SCORE)) +
    geom_violin() +
    ggtitle('SOL Score vs Disadvantaged')
```

This plot is very similar to the LEP plot and shows that being disadvantaged does correlate to a lower SOL score but there is also that very large bump just above the passing score of a 400. This is a very odd feature and not what I would expect to see from test data.

Are any Schools Outperforming the Rest?
--------------------------------------------

The final thing that I would like to take a look at is if there are any schools that are outperforming the state. To do this I will look at the average SOL score by schools and then look for the "best" schools.

```{r Finding the Best Performing Schools}
best <- math %>%
    group_by(SCH_NAME, TEST) %>%
    summarise(avg.score = mean(AVG_SOL_SCALE_SCORE)) %>%
    filter(!is.na(TEST))
```
```{r}
ggplot(best, aes(SCH_NAME, avg.score)) +
    geom_point(aes(colour = TEST)) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())
```

Now that the data is grouped in this fashion it's possible to find schools that are outperforming the rest. For this I will look for any schools that are averaging more than 2 standard deviations above the rest of the schools.

```{r Outperforming Schools}
best %>%
    group_by(SCH_NAME) %>%
    filter(avg.score > median(avg.score) +1.7*sd(avg.score)) 
```

After exploring the data I ended up having to adjust the the cutoff to 1.7 standard deviations. There were no schools that were performing more than 2 standard deviations better. There were no schools that performed significantly better on the Algebra I test but there were 11 schools that outperformed the rest in Algebra II and Geometry. One of the top schools is actually just a few miles from my location and they achieve these scores by only allowing a very small group of students into Algebra II. The top performing school is a magnet school that serves the Northern Virginia area.

Conclusions
----------------------------
This project was an enjoyable exercise of what we have covered so far this semester. I realized after completing most of my analysis that I had a data set that was more categorical in nature and did not lend itself to classical linear regression. That being said I think that I was able to gain some interesting information the data and I am now wondering why there is such a grouping of test scores just above the cutoff for passing. I remember seeing this same type of result in a data set that we talked about this summer where there was a bias to not fail students and it is unusual to see this in a test that is supposed to be performance based. This analysis has also led to a number of other questions that can't be answered with this data set which I take as a sign of a good initial review of the data.