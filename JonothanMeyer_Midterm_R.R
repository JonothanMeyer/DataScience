choose.files()
#Extracting data
small_business <- read.csv("C:\\Users\\Jonothan\\Desktop\\MSU-Spring2021\\Data Science\\Midterm\\icesiv_contest.csv")

#Uploading nessisary packages
library(dplyr)
library(ggplot2)
names(small_business)
head(small_business)

#How  does  the  extent  to  which  income  derived  from  the  small  business  is  not  the  primarysource of income
#for a business owner vary by owner’s sex, ethnicity, race, and veteran statusand by business characteristics 
#(e.g., size, sector, location)?
small_business %>% filter(EMPLOYMENT_NOISY > 100) %>% summarize(mean_employ= mean(EMPLOYMENT_NOISY), mean_payroll = mean(PAYROLL_NOISY),
                                             mean_rec = mean(RECEIPTS_NOISY), mean_prof = mean_rec - mean_payroll)

#--------------------------Looking at how Sex relates to primary/secondary Income--------------------------------------
names(small_business)
sb_sex <- small_business %>% select(SEX1:SEX4, PRMINC1:PRMINC4) %>% filter(!is.na(SEX1) & !is.na(PRMINC1)) #basic filter of relevant cols

#The following is repeating the same process to group group number of primary/secondary owner m/f into 1 variable from all 4 cols of 'SEX1:4','PRMINC1:4'
sb_sex1 <- sb_sex %>% rename(SEX = SEX1, PRMINC = PRMINC1) %>% select(SEX, PRMINC) %>%
  group_by(PRMINC, SEX) %>% summarize(n = n()) 
sb_sex1
sb_sex2 <- sb_sex %>% rename(SEX = SEX2, PRMINC = PRMINC2) %>% filter(!is.na(SEX) & !is.na(PRMINC) & SEX != "") %>% select(SEX, PRMINC) %>%
  group_by(PRMINC, SEX) %>% summarize(n = n()) 

sb_sex3 <- sb_sex %>% rename(SEX = SEX3, PRMINC = PRMINC3) %>% filter(!is.na(SEX) & !is.na(PRMINC) & SEX != "") %>% select(SEX, PRMINC) %>%
  group_by(PRMINC, SEX) %>% summarize(n = n()) 

sb_sex4 <- sb_sex %>% rename(SEX = SEX4, PRMINC = PRMINC4) %>% filter(!is.na(SEX) & !is.na(PRMINC) & SEX != "") %>% select(SEX, PRMINC) %>%
  group_by(PRMINC, SEX) %>% summarize(n = n())

sb_sex1$n <- sb_sex1$n + sb_sex2$n + sb_sex3$n + sb_sex4$n #from all of the totals of M/W owners (+priminc & sex 1-4) catagorized
sb_sex_tot_n <- sb_sex1
sb_sex_tot_n

sb_sex_tot_n1 <- sb_sex_tot_n %>% filter(PRMINC == 1)
sb_sex_tot_n2 <- sb_sex_tot_n %>% filter(PRMINC == 2)
sb_sex_tot_proportion <- round(sb_sex_tot_n1$n/sb_sex_tot_n2$n, 2)
sb_sex_tot_proportion
labels_sex_prop <- (c("Women", "Men"))
labels_sex_prop <- paste(labels_sex_prop, sb_sex_tot_proportion)
pie(sb_sex_tot_proportion, main= "Proportion of Primary Business Owners by Sex", label = labels_sex_prop)

#The following creates a stacked bar plot for the number of 1st/2nd ary business owners by Sex
ggplot(sb_sex_tot_n, aes(fill=factor(PRMINC), y=n, x=SEX)) + 
  geom_bar(position="stack", stat="identity") + labs(x="Sex", y="Number of Buisness Owners") +
    scale_fill_discrete(name = "Buisness Income", labels = c("Primary", "Secondary")) +
    ggtitle("Number of Buisness Owners by Sex")
  
#The following uses the filtered and manipulated data of sex cols and PRIMA cols in var 'sb_sex_tot_n' to create a pie
#chart that demonstrates primary and secondary buisiness owners based on sex
totalN <- sum(sb_sex_tot_n$n)
sb_sex_tot_n <- mutate(sb_sex_tot_n, prop_tot = n/totalN) #Shows the proportion (%) by catagory of M/W where biz is primary source of income
sb_sex_tot_n <- arrange(sb_sex_tot_n, by_group = TRUE, desc(prop_tot)) #Largest percent group desending by sex and prima income
labels_sex <- c("M - Primary Income", "M - Secondary Income", "F - Primary Income", "F - Secondary Income")
pct <- round(sb_sex_tot_n$prop_tot * 100)
labels_sex <- paste(labels_sex, pct) # add percents to labels
labels_sex <- paste(labels_sex,"%",sep="") # ad % to labels
pie(sb_sex_tot_n$n, main = "Small Business Owners by Sex", label = labels_sex)

#--------------------------------------------------------------------------------------------------------------------------------
#This portion is used for analysis on the 'Eth1:4' var to see how it corresponds with 
names(small_business)
sb_ethnic <- small_business %>% select(ETH1:ETH4, PRMINC1:PRMINC4) %>% filter(!is.na(ETH1) & !is.na(PRMINC1))
head(sb_ethnic)

# Codevals is for swapping out char info with numeric info
CodeVals <- data.frame(code = c(0,1,2,3,4,5),ETH1 = c("H", "N", "NA", "NA", "NA", "NA"), ETH2 = c("H", "N", "NA", "NA", "NA", "NA"),
                       ETH3 = c("H", "N", "NA", "NA", "NA", "NA"),
                       ETH4 = c("H", "N", "NA", "NA", "NA", "NA"), 
                       RACE1 = c("W", "B", "I", "A", "P", "S"),
                       RACE2 = c("W", "B", "I", "A", "P", "S"),
                       RACE3 = c("W", "B", "I", "A", "P", "S"),
                       RACE4 = c("W", "B", "I", "A", "P", "S"),
                       SEX1 = c("F", "M", "NA", "NA", "NA", "NA"),
                       SEX2 = c("F", "M", "NA", "NA", "NA", "NA"),
                       SEX3 = c("F", "M", "NA", "NA", "NA", "NA"),
                       SEX4 = c("F", "M", "NA", "NA", "NA", "NA"))
names(small_business)
CodeVals

sb_ethnic_coded <- left_join(x = sb_ethnic,
                          y = select(CodeVals, code, ETH1),
                          by = c(ETH1 = "ETH1")) %>% select(-(ETH1)) %>% rename(ETH1 = "code") #Change H/N in ETH1 to 0/1
head(sb_ethnic_coded)

sb_ethnic_coded <- left_join(x = sb_ethnic_coded,
                             y = select(CodeVals, code, ETH2),
                             by = c(ETH2 = "ETH2")) %>% select(-(ETH2)) %>% rename(ETH2 = "code")
head(sb_ethnic_coded)

sb_ethnic_coded <- left_join(x = sb_ethnic_coded,
                             y = select(CodeVals, code, ETH3),
                             by = c(ETH3 = "ETH3")) %>% select(-(ETH3)) %>% rename(ETH3 = "code")
head(sb_ethnic_coded)

sb_ethnic_coded <- left_join(x = sb_ethnic_coded,
                             y = select(CodeVals, code, ETH4),
                             by = c(ETH4 = "ETH4")) %>% select(-(ETH4)) %>% rename(ETH4 = "code")
head(sb_ethnic_coded)
names(sb_ethnic_coded)

sb_ethnic_coded1 <- sb_ethnic_coded %>% select(ETH1, PRMINC1) %>% filter(PRMINC1 == 2, !is.na(PRMINC1), !is.na(ETH1)) %>%
  group_by(ETH1) %>% summarize(n = n())
head(sb_ethnic_coded1)

sb_ethnic_coded2 <- sb_ethnic_coded %>% select(ETH2, PRMINC2) %>% filter(PRMINC2 == 2, !is.na(PRMINC2), !is.na(ETH2)) %>%
  group_by(ETH2) %>% summarize(n = n())
head(sb_ethnic_coded2)

sb_ethnic_coded3 <- sb_ethnic_coded %>% select(ETH3, PRMINC3) %>% filter(PRMINC3 == 2, !is.na(PRMINC3), !is.na(ETH3)) %>%
  group_by(ETH3) %>% summarize(n = n())
head(sb_ethnic_coded3)

sb_ethnic_coded4 <- sb_ethnic_coded %>% select(ETH4, PRMINC4) %>% filter(PRMINC4 == 2, !is.na(PRMINC4), !is.na(ETH4)) %>%
  group_by(ETH4) %>% summarize(n = n())
head(sb_ethnic_coded4)

sb_eth_coded_sum <- sb_ethnic_coded1
sb_eth_coded_sum$n <- sb_eth_coded_sum$n + sb_ethnic_coded2$n + sb_ethnic_coded3$n + sb_ethnic_coded4$n
sb_eth_coded_sum

sb_eth_coded_sum <- mutate(sb_eth_coded_sum,prop = n/sum(sb_eth_coded_sum$n))

#Pie chart to demonstrate %of bisnessowners as buisness as secondary form of income by ethnicity
labels_eth <- c("Hispanic", "Non-Hispanic")
pct <- round(sb_eth_coded_sum$prop * 100)
labels_eth <- paste(labels_eth, pct) # add percents to labels
labels_eth <- paste(labels_eth,"%",sep="") # ad % to labels
pie(sb_eth_coded_sum$n, main = "Business Ownership as Secondary Income by Ethnicity", label = labels_eth)
#The above process was repeated by filtering PRINC by 1, to compare pie charts.
#Difference in representation of Latino small business owners by primary inc only varied by 1%

#-------------------------------------------------------------------------------------------------------------
#Next Part looks at how race relates to prmimary/secondary form of income
names(small_business)
sb_race <- small_business %>% select(RACE1:RACE4, PRMINC1:PRMINC4) %>% filter(!is.na(RACE1) & !is.na(PRMINC1))
head(sb_race)

sb_race_coded <- left_join(x = sb_race,
                             y = select(CodeVals, code, RACE1),
                             by = c(RACE1 = "RACE1")) %>% select(-(RACE1)) %>% rename(RACE1 = "code")
head(sb_race_coded)

sb_race_coded <- left_join(x = sb_race_coded,
                           y = select(CodeVals, code, RACE2),
                           by = c(RACE2 = "RACE2")) %>% select(-(RACE2)) %>% rename(RACE2 = "code")
head(sb_race_coded)

sb_race_coded <- left_join(x = sb_race_coded,
                           y = select(CodeVals, code, RACE3),
                           by = c(RACE3 = "RACE3")) %>% select(-(RACE3)) %>% rename(RACE3 = "code")
head(sb_race_coded)

sb_race_coded <- left_join(x = sb_race_coded,
                           y = select(CodeVals, code, RACE4),
                           by = c(RACE4 = "RACE4")) %>% select(-(RACE4)) %>% rename(RACE4 = "code")
head(sb_race_coded)

sb_race_coded1 <- sb_race_coded %>% select(RACE1, PRMINC1) %>% filter(!is.na(PRMINC1), !is.na(RACE1)) %>%
  group_by(RACE1, PRMINC1) %>% summarize(n = n())
sb_race_coded1

sb_race_coded2 <- sb_race_coded %>% select(RACE2, PRMINC2) %>% filter( !is.na(PRMINC2), !is.na(RACE2)) %>%
  group_by(RACE2, PRMINC2) %>% summarize(n = n())
sb_race_coded2

sb_race_coded3 <- sb_race_coded %>% select(RACE3, PRMINC3) %>% filter( !is.na(PRMINC3), !is.na(RACE3)) %>%
  group_by(RACE3, PRMINC3) %>% summarize(n = n())
sb_race_coded3

sb_race_coded4 <- sb_race_coded %>% select(RACE4, PRMINC4) %>% filter(!is.na(PRMINC4), !is.na(RACE4)) %>%
  group_by(RACE4, PRMINC4) %>% summarize(n = n())
sb_race_coded4

sb_race_coded_all <- sb_race_coded1
sb_race_coded_all$n <- sb_race_coded_all$n + sb_race_coded2$n + sb_race_coded3$n + sb_race_coded4$n
sb_race_coded_all
sb_race_coded_all1 <- sb_race_coded_all %>% filter(PRMINC1 == 1)
sb_race_coded_all1
sb_race_coded_all2 <- sb_race_coded_all %>% filter(PRMINC1 == 2)
sb_race_coded_all2
sb_race_coded_all_prop <- sb_race_coded_all1$n/sb_race_coded_all2$n #Finding prop difference in primary buisness owners vs secondayr
sb_race_coded_all_prop
sb_race_coded_all <- sb_race_coded_all %>% mutate(prop = n/sum(sb_race_coded_all$n)) %>% arrange(desc(n))
sb_race_coded_all <- sb_race_coded_all %>% mutate (ndiv_hund = n/100)
sb_race_coded_all 

#barplot 
barplot(height=sb_race_coded_all_prop, names=race_key$race, main= "Proportion of Owners with Business as Primary Income by Race")

labels_race <- c("White", "Asian", "Black", "Nat Amer","Other", "Hawaiian")
race_key <- data.frame(code = c(0,1,2,3,4,5), race = c("White", "Black", "Nat Amer", "Asian", "Hawaiian", "Other"))

pct <- round(sb_race_coded_all$prop * 100,2)
labels_race <- paste(labels_race, pct) # add percents to labels
labels_race <- paste(labels_race,"%",sep="") # ad % to labels
pie(sb_race_coded_all$n, main = "Business Ownership as Secondary Income by Race", label = labels_race)


#Trying my hand at barplot()
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
barplot_race <- barplot(height=round(sb_race_coded_all$prop*100,2), names=race_key$race, col=coul, main= "Business Ownership as Secondary Income by Race",
                xlab = "Race", ylab = "% of Owners", beside=TRUE)
text(x = barplot_race, y =sb_race_coded_all$prop, label = pct, pos = 3, cex = 0.8, col = "black") #adding numerica values to barplot

#Stacked barplot of number of owners by race, stacked w/primary/secondary income as legend
racelabeldataframe <- c("White", "White", "Black", "Black", "Nat Amer", "Nat Amer", "Asian", "Asian", "Hawaiian", "Hawaiian", "Other", "Other")
ggplot(sb_race_coded_all, aes(fill=factor(PRMINC1), y=n, x=reorder(racelabeldataframe,-n))) + 
  geom_bar(position="stack", stat="identity") + labs(x="Race", y="Number of Buisness Owners") +
  scale_fill_discrete(name = "Buisness Income", labels = c("Primary", "Secondary")) +
  ggtitle("Number of Buisness Owners by Race") + theme(legend.position=c(.8,.8))

sb_race_coded_all
#----------------------------------------------------------------------------------------------------------------------------
#Vetern Status as it relates to primary/secondary income, 1 = yes, 2 = no
names(small_business)
sb_vet <- small_business %>% select(VET1:VET4, PRMINC1:PRMINC4) %>% filter(!is.na(VET1) & !is.na(PRMINC1))
head(sb_vet)

sb_vet1 <- sb_vet %>% select(VET1, PRMINC1) %>% filter(PRMINC1 == 2, !is.na(PRMINC1), !is.na(VET1)) %>%
  group_by(VET1) %>% summarize(n = n())
sb_vet1

sb_vet2 <- sb_vet %>% select(VET2, PRMINC2) %>% filter(PRMINC2 == 2, !is.na(PRMINC2), !is.na(VET2)) %>%
  group_by(VET2) %>% summarize(n = n())
sb_vet2

sb_vet3 <- sb_vet %>% select(VET3, PRMINC3) %>% filter(PRMINC3 == 2, !is.na(PRMINC3), !is.na(VET3)) %>%
  group_by(VET3) %>% summarize(n = n())
sb_vet3

sb_vet4 <- sb_vet %>% select(VET4, PRMINC4) %>% filter(PRMINC4 == 2, !is.na(PRMINC4), !is.na(VET4)) %>%
  group_by(VET4) %>% summarize(n = n())
sb_vet4

sb_vet_all <- sb_vet1
sb_vet_all$n <- sb_vet_all$n + sb_vet2$n + sb_vet3$n + sb_vet4$n

sb_vet_all <- mutate(sb_vet_all, prop = n/sum(sb_vet_all$n))

#Pie chart of race distribution by % for buisness as secondary income
labels_vet <- c("Veteran", "Non-Vetarn")
pct <- round(sb_vet_all$prop * 100)
labels_vet <- paste(labels_vet, pct) # add percents to labels
labels_vet <- paste(labels_vet,"%",sep="") # ad % to labels
pie(sb_vet_all$n, main = "Business Ownership as Secondary Income by Veterans", label = labels_vet)


#---------------------------------------------------------------------------------------------------------------------
#This was an aside and not used. Looking at ANOVA of lin model. 
sb.lm <- lm(PAYROLL_NOISY~RACE1, data=small_business)
summary(sb.lm) #White Owners are best predictor of Payroll. Racial bias?
names(small_business)
anova(sb.lm)
summary(small_business)
head(small_business$PAYROLL_NOISY)
mean(small_business$PAYROLL_NOISY)
#------------------------------------------------------Problem 2 Stuff--------------------------------------------------
#How does the business size (establishment employment, establishment payroll, establishment
#receipts) vary by owner's sex, ethnicity, race, and veteran status and by other business char-
#acteristics (e.g., sector, location)?

#Looking at payroll total as it relates to Sex
M_payroll1 <- small_business %>% select(PAYROLL_NOISY, SEX1, EMPLOYMENT_NOISY) %>% filter(!is.na(PAYROLL_NOISY) & !is.na(PAYROLL_NOISY)) %>%
                                  group_by(SEX1) %>% summarize(n = n(), sum_pay = sum(PAYROLL_NOISY), mean_pay = sum_pay/n, sd = sd(PAYROLL_NOISY))
M_payroll2 <- small_business %>% select(PAYROLL_NOISY, SEX2, EMPLOYMENT_NOISY) %>% filter(!is.na(PAYROLL_NOISY) & !is.na(SEX2) & SEX2 != "") %>%
                                  group_by(SEX2) %>% summarize(n = n(), sum_pay = sum(PAYROLL_NOISY), mean_pay = sum_pay/n, sd = sd(PAYROLL_NOISY))
M_payroll3 <- small_business %>% select(PAYROLL_NOISY, SEX3, EMPLOYMENT_NOISY) %>% filter(!is.na(PAYROLL_NOISY) & !is.na(SEX3) & SEX3 != "") %>%
                                  group_by(SEX3) %>% summarize(n = n(), sum_pay = sum(PAYROLL_NOISY), mean_pay = sum_pay/n, sd = sd(PAYROLL_NOISY))
M_payroll4 <- small_business %>% select(PAYROLL_NOISY, SEX4, EMPLOYMENT_NOISY) %>% filter(!is.na(PAYROLL_NOISY) & !is.na(SEX4) & SEX4 != "") %>%
                                  group_by(SEX4) %>% summarize(n = n(), sum_pay = sum(PAYROLL_NOISY), mean_pay = sum_pay/n, sd = sd(PAYROLL_NOISY))


M_payroll_ALL <- data.frame(SEX = c(M_payroll1$SEX1), n = c(M_payroll1$n), sum_pay = c(M_payroll1$sum_pay), sd = c(M_payroll1$sd))
M_payroll_ALL$n <- M_payroll_ALL$n + M_payroll2$n + M_payroll3$n + M_payroll4$n
M_payroll_ALL$sum_pay <- M_payroll_ALL$sum_pay + M_payroll2$sum_pay + M_payroll3$sum_pay + M_payroll4$sum_pay
M_payroll_ALL$sd < (M_payroll2$sd + M_payroll3$sd + M_payroll4$sd)/4 #Average Standard deviation of all payroll
M_payroll_ALL


#The following produces a data set that shows the mean payroll by men and women from all sex catagories.
sex_payroll_table <- M_payroll_ALL %>% mutate(mean_pay = M_payroll_ALL$sum_pay/M_payroll_ALL$n)
sex_payroll_table #Final table of payroll stats as it relates to SEX

#--------------------------------------Sex and Employment------------------------------------------------------------

M_employ1 <- small_business %>% select(SEX1, EMPLOYMENT_NOISY) %>% filter(!is.na(EMPLOYMENT_NOISY) & !is.na(SEX1)) %>%
  group_by(SEX1) %>% summarize(n = n(), sum_employ = sum(EMPLOYMENT_NOISY), mean_employ = sum_employ/n, sd = sd(EMPLOYMENT_NOISY))
M_employ2 <- small_business %>% select(SEX2, EMPLOYMENT_NOISY) %>% filter(!is.na(EMPLOYMENT_NOISY) & !is.na(SEX2) & SEX2 != "") %>%
  group_by(SEX2) %>% summarize(n = n(), sum_employ = sum(EMPLOYMENT_NOISY), mean_employ = sum_employ/n, sd = sd(EMPLOYMENT_NOISY))
M_employ3 <- small_business %>% select(SEX3, EMPLOYMENT_NOISY) %>% filter(!is.na(EMPLOYMENT_NOISY) & !is.na(SEX3) & SEX3 != "") %>%
  group_by(SEX3) %>% summarize(n = n(), sum_employ = sum(EMPLOYMENT_NOISY), mean_employ = sum_employ/n, sd = sd(EMPLOYMENT_NOISY))
M_employ4 <- small_business %>% select(SEX4, EMPLOYMENT_NOISY) %>% filter(!is.na(EMPLOYMENT_NOISY) & !is.na(SEX4) & SEX4 != "") %>%
  group_by(SEX4) %>% summarize(n = n(), sum_employ = sum(EMPLOYMENT_NOISY), mean_employ = sum_employ/n, sd = sd(EMPLOYMENT_NOISY))
M_employ1

M_employ_ALL <- data.frame(SEX = c(M_employ1$SEX1), n = c(M_employ1$n), sum_employ = c(M_employ1$sum_employ), sd = c(M_employ1$sd))
M_employ_ALL$n <- M_employ_ALL$n + M_employ2$n + M_employ3$n + M_employ4$n
M_employ_ALL$sum_employ <- M_employ_ALL$sum_employ + M_employ2$sum_employ + M_employ3$sum_employ + M_employ4$sum_employ
M_employ_ALL$sd < (M_employ2$sd + M_employ3$sd + M_employ4$sd)/4 #Average Standard deviation of all payroll

M_employ_ALL
sex_employ_table <- M_employ_ALL %>% mutate(mean_employ = M_employ_ALL$sum_employ/M_employ_ALL$n)
sex_employ_table #gives a good table for difference in mean number of employees for men and woman
#----------------------------------------------------------------------------------------------------------------------------
#Here we are looking at Establishment Employment by Race
names(small_business)

employ_race <- small_business %>% select(RACE1:RACE4, EMPLOYMENT_NOISY) %>% filter(!is.na(RACE1) & !is.na(EMPLOYMENT_NOISY))
head(employ_race)

employ_race_coded <- left_join(x = employ_race,
                           y = select(CodeVals, code, RACE1),
                           by = c(RACE1 = "RACE1")) %>% select(-(RACE1)) %>% rename(RACE1 = "code")
head(employ_race_coded)

employ_race_coded<- left_join(x = employ_race_coded,
                           y = select(CodeVals, code, RACE2),
                           by = c(RACE2 = "RACE2")) %>% select(-(RACE2)) %>% rename(RACE2 = "code")


employ_race_coded <- left_join(x = employ_race_coded,
                           y = select(CodeVals, code, RACE3),
                           by = c(RACE3 = "RACE3")) %>% select(-(RACE3)) %>% rename(RACE3 = "code")
head(employ_race_coded)

employ_race_coded <- left_join(x = employ_race_coded,
                           y = select(CodeVals, code, RACE4),
                           by = c(RACE4 = "RACE4")) %>% select(-(RACE4)) %>% rename(RACE4 = "code")
head(employ_race_coded)


employ_racec1 <- employ_race_coded %>% filter(EMPLOYMENT_NOISY <= 1000) %>%
  group_by(RACE1) %>% summarize(n = n(), sum_employ = sum(EMPLOYMENT_NOISY, na.rm=TRUE),
                                sd = sd(EMPLOYMENT_NOISY, na.rm=TRUE)) #Finding outlier data point
employ_racec2

#Finding data point in race catagory that was causing the mean for hawaiians to be insanely high
max_employed_r2 <- small_business %>% filter(RACE2 == "P" & EMPLOYMENT_NOISY>=10000) 
View(max_employed_r2)

employ_racec2 <- employ_race_coded %>% filter(EMPLOYMENT_NOISY <= 1000) %>% group_by(RACE2) %>% summarize(n = n(), sum_employ = sum(EMPLOYMENT_NOISY, na.rm=FALSE), sd = sd(EMPLOYMENT_NOISY, na.rm=FALSE))

employ_racec3 <- employ_race_coded %>% filter(EMPLOYMENT_NOISY <= 1000) %>% group_by(RACE3) %>% summarize(n = n(), sum_employ = sum(EMPLOYMENT_NOISY, na.rm=FALSE), sd = sd(EMPLOYMENT_NOISY, na.rm=FALSE))

employ_racec4 <- employ_race_coded %>% filter(EMPLOYMENT_NOISY <= 1000) %>% group_by(RACE4) %>% summarize(n = n(), sum_employ = sum(EMPLOYMENT_NOISY, na.rm=FALSE), sd = sd(EMPLOYMENT_NOISY, na.rm=FALSE))

employ_race_all <- employ_racec1
employ_race_all$n <- employ_race_all$n + employ_racec2$n + employ_racec3$n + employ_racec4$n
employ_race_all$sum_employ <- employ_race_all$sum_employ + employ_racec2$sum_employ + employ_racec3$sum_employ + employ_racec4$sum_employ
employ_race_all$sd <- (employ_race_all$sd + employ_racec2$sd + employ_racec3$sd + employ_racec4$sd)/4

labels_race_diff <- c("White", "Black", "Nat Amer","Asian", "Hawaiian","Other", "NA")
employ_race_table <- cbind(employ_race_all %>% mutate(mean_employ = sum_employ/n), labels_race_diff)
employ_race_table <- employ_race_table[-7,] #removing row of 'NA' observations

employ_race_table
#Barplot of mean employee's by race
barplot(height=round(employ_race_table$mean_employ,2), names=employ_race_table$labels_race_diff, main= "Mean Amount of Employee's by Race",
                        xlab = "Race", ylab = "Averge Number of Employee's", beside=TRUE)
#------------------------------Look at race (recepts - payroll) =profit--------------------------------------------------------

race_profit <- small_business %>% select(RACE1:RACE4, PAYROLL_NOISY, RECEIPTS_NOISY) %>% mutate(rem_cap = RECEIPTS_NOISY - PAYROLL_NOISY)
head(race_profit) #"rem_cap"="remaining capital": defined as Receipts-Payroll in this context

race_profit_coded <- left_join(x = race_profit,
                               y = select(CodeVals, code, RACE1),
                               by = c(RACE1 = "RACE1")) %>% select(-(RACE1)) %>% rename(RACE1 = "code")

race_profit_coded <- left_join(x = race_profit_coded,
                               y = select(CodeVals, code, RACE2),
                               by = c(RACE2 = "RACE2")) %>% select(-(RACE2)) %>% rename(RACE2 = "code")

race_profit_coded <- left_join(x = race_profit_coded,
                               y = select(CodeVals, code, RACE3),
                               by = c(RACE3 = "RACE3")) %>% select(-(RACE3)) %>% rename(RACE3 = "code")

race_profit_coded <- left_join(x = race_profit_coded,
                               y = select(CodeVals, code, RACE4),
                               by = c(RACE4 = "RACE4")) %>% select(-(RACE4)) %>% rename(RACE4 = "code")

head(race_profit_coded)
small_business %>% filter(RECEIPTS_NOISY >= 3000000) #14/15 of the largest companies (by recieps) are white
small_business %>% filter(EMPLOYMENT_NOISY >= 15000)
names(small_business)

rpc1 <- race_profit_coded %>% group_by(RACE1) %>% summarize(n=n(), sum_profit = sum(rem_cap, na.rm = FALSE), sd = sd(rem_cap))
rpc2 <- race_profit_coded %>% group_by(RACE2) %>% summarize(n=n(), sum_profit = sum(rem_cap, na.rm = FALSE), sd = sd(rem_cap))
rpc3 <- race_profit_coded %>% group_by(RACE3) %>% summarize(n=n(), sum_profit = sum(rem_cap, na.rm = FALSE), sd = sd(rem_cap))
rpc4 <- race_profit_coded %>% group_by(RACE4) %>% summarize(n=n(), sum_profit = sum(rem_cap, na.rm = FALSE), sd = sd(rem_cap))

rpc_all <- rpc1
rpc_all$n <- rpc_all$n + rpc2$n + rpc3$n + rpc4$n
rpc_all$sum_profit <- rpc_all$sum_profit + rpc2$sum_profit + rpc3$sum_profit + rpc4$sum_profit
rpc_all$sd <- round((rpc_all$sd + rpc2$sd + rpc3$sd + rpc4$sd)/4)
rpc_all
labels_race_diff <- c("White", "Black", "Nat Amer","Asian", "Hawaiian","Other", "NA")
rpc_all_table <- cbind(rpc_all %>% mutate(mean_profit = round(sum_profit/n)), labels_race_diff)
rpc_all_table[-7,] #profit doesnt reflect actual profit, just a nameholder for what means is difference 
#in recepts noisy - payroll noisy
sum(rpc_all_table$n)
rpc_all_table <- rpc_all_table[-7,] %>% arrange(desc(mean_profit))

barplot(height=rpc_all_table$mean_profit, names=rpc_all_table$labels_race_diff, col=coul,
            main = "Mean Remaining Capital After Subtracting Payroll by Race",
            ylab = "Mean Remaining Capital $$")
#-------------------------------------------------------------------------------------------------------------------------
#Sex as it relates to reciepts
sb_sexrec <- small_business %>% select(SEX1:SEX4, RECEIPTS_NOISY) %>% filter(!is.na(SEX1) & !is.na(RECEIPTS_NOISY)) #basic filter of relevant cols

sb_sexrec1 <- sb_sexrec %>% rename(SEX = SEX1) %>% filter(SEX != "")  %>% group_by(SEX) %>% summarize(n = n(), sum_rec = sum(RECEIPTS_NOISY)/100, max =max(RECEIPTS_NOISY), min= min(RECEIPTS_NOISY)) 
sb_sexrec1
sb_sexrec2 <- sb_sexrec %>% rename(SEX = SEX2) %>% filter(SEX != "")  %>%  group_by(SEX)  %>% summarize(n = n(), sum_rec = sum(RECEIPTS_NOISY)/100, max = max(RECEIPTS_NOISY), min = min(RECEIPTS_NOISY)) 
sb_sexrec3 <- sb_sexrec %>% rename(SEX = SEX3) %>% filter(SEX != "")  %>%  group_by(SEX)  %>% summarize(n = n(), sum_rec = sum(RECEIPTS_NOISY)/100, max = max(RECEIPTS_NOISY), min = min(RECEIPTS_NOISY)) 
sb_sexrec4 <- sb_sexrec %>% rename(SEX = SEX4) %>% filter(SEX != "")  %>%  group_by(SEX)  %>% summarize(n = n(), sum_rec = sum(RECEIPTS_NOISY)/100, max = max(RECEIPTS_NOISY), min = min(RECEIPTS_NOISY)) 

sb_sr_all <- sb_sexrec1

sb_sr_all$n <- sb_sr_all$n + sb_sexrec2$n + sb_sexrec3$n + sb_sexrec4$n
sb_sr_all$sum_rec <- sb_sr_all$sum_rec + sb_sexrec2$sum_rec + sb_sexrec3$sum_rec + sb_sexrec4$sum_rec
sb_sr_all$max <- max(sb_sr_all$max, sb_sexrec2$max, sb_sexrec3$max, sb_sexrec4$max)
sb_sr_all$min <- min(sb_sr_all$min, sb_sexrec2$min, sb_sexrec3$min, sb_sexrec4$min)
sb_sr_all$sum_rec <- sb_sr_all$sum_rec * 100

sb_sr_all <- sb_sr_all %>% mutate(mean = sum_rec/n)
sb_sr_all <- sb_sr_all %>% rename(n_owners = n, total_receipts = sum_rec)
sb_sr_all <- data.frame(sb_sr_all)
sb_sr_all #table of sex as it relates to reciepts

meanrec_bar <- barplot(height=sb_sr_all$mean, names=sb_sr_all$SEX, col=coul, main="Mean Receipt Amount of Small Business by Sex",
                       ylab="Number of Owners", xlab="Sex")
text(x = meanrec_bar, y =.5, label = round(sb_sr_all$mean), pos = 3, col = "black") #adding numerica values to barplot
