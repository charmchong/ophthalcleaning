#install all the packages you need first

install.packages(c("tidyverse"), dependencies = TRUE)

install.packages(c("openxlsx"), dependencies = TRUE)

library(tidyverse)

library(readxl)

#insert the excel sheet tabs one by one and commit them to a specific saved name, at the same time changing the column inputs to be read as numerals rather than text

Grader1 <- read_excel("Downloads/Baseline_data.xlsx",sheet = "Grader 1", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric"))

Grader1 <- Grader1[-c(65), ]

Grader2 <- read_excel("Downloads/Baseline_data.xlsx",sheet = "Grade 2", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric"))

Grader2 <- Grader2[-c(65), ]

Grader3 <- read_excel("Downloads/Baseline_data.xlsx",sheet = "Grader 3", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric"))

Grader3 <- Grader3[-c(65), ]

#add column for grader plus the numbers 1 2 or 3

Grader1 <- Grader1 %>% add_column(Grader = 1, .before = "NO")

Grader2 <- Grader2 %>% add_column(Grader = 2, .before = "NO")

Grader3 <- Grader3 %>% add_column(Grader = 3, .before = "NO")

#vertically stack the data according to grader 1 2 and 3

total <- rbind(Grader1, Grader2, Grader3)

#save this entire data frame

Transformed_Data <- tibble::rowid_to_column(total, "Index")

openxlsx::write.xlsx(Transformed_Data, file = "/Users/charmchong/Downloads/Transformed_DataCharm.xlsx", sheetName = "Transformed_Data")
# find mean

agg = aggregate(Transformed_Data,
                +                 by = list(Transformed_Data$Grader),
                +                 FUN = mean)

#reveal answer of mean computed 

agg

agg [c("Grader", "NO", "NC")]

#find mean and median of each NO and NC value per grader 

Transformed_Data %>% group_by(Grader) %>% summarise(mean_NO = mean(NO), mean_NC= mean (NC), median_NO = median(NO), median_NC = median(NC), total_imagecount = n())

#save it under the key name basic_analytics

basic_analytics <- Transformed_Data %>% group_by(Grader) %>% summarise(mean_NO = mean(NO), mean_NC= mean (NC), median_NO = median(NO), median_NC = median(NC), total_imagecount = n())

#one way to process manually is to run t-tests between graders 1 and 2, graders 2 and 3, and then graders 1 and 3 but not ideal. Just showing it’s possible to run this code

ttest_NO_1_2 <- t.test(Grader1["NO"], Grader2["NO"])

ttest_NO_2_3 <- t.test(Grader2["NO"], Grader3["NO"])

ttest_NO_1_3 <- t.test(Grader1["NO"], Grader3["NO"])

TransformedData_Grader1 <- Transformed_Data[ which(Transformed_Data$Grader=='1'),] 

t.test(Transformed_Data[ which(Transformed_Data$Grader=='1'),]["NO"], Transformed_Data[ which(Transformed_Data$Grader=='2'),]["NO"])

# best is to run an analysis of variance (ANOVA) test 

anovaresults <- aov (Grader ~ NO + NC, data = Transformed_Data)
> summary (anovaresults)

#visualising as box plot

install.packages("ggpubr")

ggboxplot(Transformed_Data, x = "Grader", y = "NC")

ggboxplot(Transformed_Data, x = "Grader", y = "NO")

#finding Q1 scores above 4

Transformed_Data$Q1above4 <- ifelse(Transformed_Data$Q1 >=4, 1, 0)

Transformed_Data$Q2above4 <- ifelse(Transformed_Data$Q2 >=4, 1, 0)

#summing the numbers of Q1 above 4 

sum(Transformed_Data$Q1above4)

#giving each Q1 above 4 a numerical value of 1 in order to calculate proportion

typeof(Transformed_Data$Q1above4[1])

#checking because there is one NA return under Q1 messing with the data 

sum(Transformed_Data$Q1above4, na.rm = TRUE)

nacheck <- Transformed_Data[is.na(Transformed_Data$Q1above4),]

nacheck

Q1count <- sum(Transformed_Data$Q1above4, na.rm = TRUE)

#summing the numbers of Q2 above 4 

Q2count <- sum(Transformed_Data$Q2above4, na.rm = TRUE)

#doing a two-tailed Z test to figure out significance of proportions of Q1 and Q2 

Ztest <- prop.test(x = c(Q1count, Q2count), n = c(590, 590))
View(Ztest)

#viewing Z-test results 

Ztest
