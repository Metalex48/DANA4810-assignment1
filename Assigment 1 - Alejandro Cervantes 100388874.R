
#############       ASSIGNMENT 1        ############# 


#######       Bulimia VS Normal        ####### 

# Attching dataset:
attach(BULIMIA)
head(BULIMIA)

# Creating histograms for each type of student:
par(mfrow=c(1,2))
hist(FNESCORE[GROUP=='Bulimic'])
hist(FNESCORE[GROUP=='Normal'])
?boxplot # for boxplot
?qqnorm # dots for Q-Q plot
?qqline # line for Q-Q plot

#We take a look at the St.Dev for each sample.
tapply(FNESCORE, GROUP, sd)

# Conducting the F test to compare two variances. Normal goes first because it is larger.
var.test(FNESCORE[GROUP=="Normal"],FNESCORE[GROUP=="Bulimic"], alternative = "two.sided")

# Conducting a Two-Sample t-test:
t.test(FNESCORE~GROUP, alternative = c("greater"), mu = 0, var.equal = TRUE )



#######       Jellybean machines        ####### 

attach(BAGS)
head(BAGS)

# Creating histograms for each machine:
par(mfrow=c(1,2))
hist(Machine_1)
hist(Machine_2)
?boxplot # for boxplot
?qqnorm # dots for Q-Q plot
?qqline # line for Q-Q plot

#We take a look at the Variance for each sample.
var(Machine_1)
var(Machine_2, na.rm = TRUE)

# Conducting the F test to compare two variances. Machine1 goes first because it is larger.
var.test(Machine_1, Machine_2, ratio=1, c("two.sided"))

# Conducting a Two-Sample t-test:
t.test(Machine_1, Machine_2, alternative = c("two.sided"), mu = 0, var.equal = FALSE )




#######       SAT test scores        ####### 

attach(TestScores)
head(TestScores)

# calculating the mean for the population of each test

#### calculating the mean in the population for the math tests
math_mean = mean(Math.Score)
t.test(Math.Score, alternative = c("two.sided"), mu = math_mean)

#### calculating the mean in the population for the writing tests
write_mean = mean(Writing.Score)
t.test(Writing.Score, alternative = c("two.sided"), mu = write_mean)

# Conducting a paired Two-Sample t-test:
t.test(Math.Score, Writing.Score, alternative = c("two.sided"), mu = 0, paired = TRUE)


