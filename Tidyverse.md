---
layout: page
title: Tidyverse
---
Below is an old project I worked on to use some Tidyverse dplyr code in order to compare an answer key to student answers. 

_insert dplyr picture_

1.) Read in the data.
```r
#All data are CSV's so use read.csv
#FormA and FormB do not have headers
#Adomains and Bdomains have headers

library(knitr)
library(tidyverse)

B <- read.csv("C:\\Users\\Chia\\Documents\\Stats 123\\Data.and.Domains\\Data.and.Domains\\FormB.csv", sep = ",", stringsAsFactors = FALSE, header = FALSE)

A_Domains <- read.csv("C:\\Users\\Chia\\Documents\\Stats 123\\Data.and.Domains\\Data.and.Domains\\Domains FormA.csv", header=TRUE, stringsAsFactors = FALSE)

B_Domains <- read.csv("C:\\Users\\Chia\\Documents\\Stats 123\\Data.and.Domains\\Data.and.Domains\\Domains FormB.csv", header=TRUE, stringsAsFactors = FALSE)

A <- read.csv("C:\\Users\\Chia\\Documents\\Stats 123\\Data.and.Domains\\Data.and.Domains\\FormA.csv", header=FALSE, stringsAsFactors = FALSE)

glimpse(B)
glimpse(B_Domains)
```


    
    ## Observations: 50
    ## Variables: 151
    ## $ V1   <chr> "BBBBKEY", "2", "4", "6", "8", "10", "12", "14", "16", "18", "20", "22", "24", "26", "28", "30", "32", "3...
    ## $ V2   <chr> "B", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "B", "C", "C", "C", "C", "D", "C", "C", "C", "C", ...
    ## $ V3   <chr> "C", "C", "A", "C", "A", "C", "C", "A", "A", "A", "C", "A", "A", "A", "C", "C", "A", "A", "A", "A", "A", ...
    ## $ V4   <chr> "B", "B", "B", "C", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "D", "B", "B", "B", "D", ...
    ## 
    ## Observations: 150
    ## Variables: 4
    ## $ ItemId   <int> 49552, 49432, 49497, 49473, 49425, 49503, 49286, 49563, 49544, 49334, 49506, 49486, 49536, 49354, 495...
    ## $ Domain   <chr> "Organizational and Administrative Responsibilities", "Athlete Testing and Evaluation", "Organization...
    ## $ Domain.. <int> 5, 4, 5, 5, 2, 4, 2, 3, 5, 2, 2, 4, 3, 1, 3, 2, 1, 5, 1, 4, 1, 5, 3, 4, 3, 2, 3, 3, 4, 4, 1, 2, 3, 4,...
    ## $ X        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28...
    ## 

2.) Compare answer key to student responses. The answer key is the first row in the data, and the results are everything else.
```r
# function to make a matrix of correct scores
correct <- function(data) {
  
  key <- data[1,-1]
  results <- data[-1,-1]
  
  output <- mapply("==",key,results)
  output
}
```
3.) Overall score correct for each student as a percentage
```r
#score for A
A_matrix <- correct(A)

 studentscore <- apply(A_matrix, 1, mean)

student_score <- as.data.frame(studentscore)
student_score$student_ID <- as.numeric(A[-1,1])
student_score$form <- rep("A", 50)
student_score$overallscore <- apply(A_matrix, 1, sum)


#score for B

B_matrix <- correct(B)

  studentscoreb <- apply(B_matrix, 1, mean)

student_scoreB <- as.data.frame(studentscoreb)

student_scoreB$student_ID <- as.numeric(B[-1,1])
student_scoreB$form <- rep("B", 49)
student_scoreB$overallscore <- apply(B_matrix, 1, sum)
names(student_scoreB) <- c("studentscore","student_ID","form","overallscore")

#combine score for A&B

student_score_all <- rbind.data.frame(student_score, student_scoreB)

kable(student_score_all)
```
4.) Sort student scores by ID and best to last
```r
# sorted by ID
student_score_all_ID <- student_score_all %>% arrange(student_ID)

kable(student_score_all_ID, caption = "Student Score by ID")

# sorted by best to last

student_score_all_best <- student_score_all %>% arrange(desc(studentscore))

kable(student_score_all_best, caption = "Student Score by Percentage")
```
5.) Sort Question by question # and hardest questions first
```r
question_score_all_ID <- question_score_all %>% arrange(question_number)

kable(question_score_all_ID, caption = "Question Score by Number")

question_score_all_hard <- question_score_all %>% arrange(questionscore)


kable(question_score_all_hard, caption = "Question Score by Hardest Question")
```
6.) Link domains to question and merge overall score to domain scores
```r
#turn A_matrix into dataframe, add domains to the bottom row in data

Adf <- data.frame(A_matrix)

Adf <- rbind(Adf, A_Domains$Domain..) 

#find score for each domain through row sums where column has row51 == domains

sum1 <- rowSums(Adf[-51, Adf[51, ]==1])
sum2 <- rowSums(Adf[-51, Adf[51, ]==2])
sum3 <- rowSums(Adf[-51, Adf[51, ]==3])
sum4 <- rowSums(Adf[-51, Adf[51, ]==4])
sum5 <- rowSums(Adf[-51, Adf[51, ]==5])




#same but do row means


mean1 <- rowMeans(Adf[-51, Adf[51, ]==1])
mean2 <- rowMeans(Adf[-51, Adf[51, ]==2])
mean3 <- rowMeans(Adf[-51, Adf[51, ]==3])
mean4 <- rowMeans(Adf[-51, Adf[51, ]==4])
mean5 <- rowMeans(Adf[-51, Adf[51, ]==5])


domain_sum <- data.frame(cbind(sum1,sum2,sum3,sum4,sum5,mean1,mean2,mean3,mean4,mean5))
names(domain_sum) <- c("Domain1","Domain2","Domain3","Domain4","Domain5","%D1","%D2","%D3","%D4","%D5")



#do above steps except for B

Bdf <- data.frame(B_matrix)

Bdf <- rbind(Bdf, B_Domains$Domain..) 

sum1B <- rowSums(Bdf[-50, Bdf[50, ]==1])
sum2B <- rowSums(Bdf[-50, Bdf[50, ]==2])
sum3B <- rowSums(Bdf[-50, Bdf[50, ]==3])
sum4B <- rowSums(Bdf[-50, Bdf[50, ]==4])
sum5B <- rowSums(Bdf[-50, Bdf[50, ]==5])



mean1B <- rowMeans(Bdf[-50, Bdf[50, ]==1])
mean2B <- rowMeans(Bdf[-50, Bdf[50, ]==2])
mean3B <- rowMeans(Bdf[-50, Bdf[50, ]==3])
mean4B <- rowMeans(Bdf[-50, Bdf[50, ]==4])
mean5B <- rowMeans(Bdf[-50, Bdf[50, ]==5])

domain_sumB <- data.frame(cbind(sum1B,sum2B,sum3B,sum4B,sum5B,mean1B,mean2B,mean3B,mean4B,mean5B))
names(domain_sumB) <- c("Domain1","Domain2","Domain3","Domain4","Domain5","%D1","%D2","%D3","%D4","%D5")

#combine the A and B dataframes together

domain_all <- rbind(domain_sum, domain_sumB)



#cbind total domain sum with overall student score

all_student <- cbind(student_score_all, domain_all) 
```




Finally, we can look at the final output.




# Section A: Student Scores
```r
#ordering columns and arranging by student_ID


all_student <- all_student[c(2,3,4,1,5,6,7,8,9,10,11,12,13,14)]
all_student <- all_student %>% arrange(student_ID)
names(all_student) <- c("Student_ID", "Form", "Overall_Score", "Overall_Percent", "Domain_1", "Domain_2","Domain_3","Domain_4","Domain_5","Domain_1_P", "Domain_2_P","Domain_3_P","Domain_4_P","Domain_5_P")

#format percentages nnn.n%
all_student$Overall_Percent <- sprintf("%3.1f%%", 100*all_student$Overall_Percent)
all_student$Domain_1_P <- sprintf("%3.1f%%", 100*all_student$Domain_1_P)
all_student$Domain_2_P <- sprintf("%3.1f%%", 100*all_student$Domain_2_P)
all_student$Domain_3_P <- sprintf("%3.1f%%", 100*all_student$Domain_3_P)
all_student$Domain_4_P <- sprintf("%3.1f%%", 100*all_student$Domain_4_P)
all_student$Domain_5_P <- sprintf("%3.1f%%", 100*all_student$Domain_5_P)

kable(all_student, caption = "Student Scores by ID")
```

![](/assets/studentfinal.PNG)


```r
#boxplot

boxplot(domain_all$Domain1, domain_all$Domain2, domain_all$Domain3, domain_all$Domain4, domain_all$Domain5, ylab = "Scores", main = "Domain Comparison", xlab = "Domains", names = c("1", "2","3","4","5"))
```
![](/assets/domaincomparison.PNG)

# Section B: Question Analysis
Question Score by Number
```{r}
#order columns, sorted by form and then ID

question_score_all <- question_score_all[c(3, 2, 1)]
names(question_score_all) <- c("Form", "Question_No", "Overall_Percent")

question_score_all_p <- question_score_all[c(3,1,2)]

question_score_all$Overall_Percent <-  sprintf("%3.1f%%", 100*question_score_all$Overall_Percent)

kable(question_score_all, caption = "Question Score by Number")

```
![](/assets/questions.PNG)



