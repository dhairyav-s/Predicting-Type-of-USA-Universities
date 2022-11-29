# Week 3 Assignment 
# Dhairyav Shah 

# Importing Libraries
library(ISLR)
library(psych)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(caTools)
library(caret)
library(regclass)
library(pROC)
library(stargazer)


# Importing the data set
df<-as.data.frame(College)

# Exploratory Data analysis

str(df)

df1 <- describe(df)
df2 <- df1 %>% select(n, mean, min, sd, median, max) # you still need to round decimals (see below)
write.csv(df2,"Summary_Popluation.csv")

# Sub-setting into Public and Private

df_pub<-subset(df,Private=="No")

df_priv<-subset(df,Private=="Yes")

df3 <- describe(df_pub)
df4 <- df3 %>% select(n, mean, min, sd, median, max) # you still need to round decimals (see below)
write.csv(df4,"Summary_Pub.csv")

df5 <- describe(df_priv)
df6 <- df5 %>% select(n, mean, min, sd, median, max) # you still need to round decimals (see below)
write.csv(df6,"Summary_Priv.csv")

# Visualization

#Bar plot Public and Private colleges 

g1<-ggplot(data = df,aes(fill = Private)) +
  geom_bar(mapping = aes(x=Private)) +
  ggtitle("Fig 1:Number of Colleges") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  theme_light()
  guide = guide_legend(title = "Private Type")

df_app<- df %>%
  group_by(Private) %>%
  summarise(Applications=sum(Apps))

g2<-ggplot(data = df_app, aes(x= Private, y=Applications, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 2: Number of Applications") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=Applications), vjust=2, color="black", size=3.5)+
  theme_light()

ggarrange(g1,g2,
          ncol = 2, nrow = 1)

# Box plots to study the Accepted and Enrolled 
  
# Applications

p1<-ggplot(data = df, aes(Private,Accept,Apps,fill = Private))+
  geom_boxplot(color = "black", fill = "green")+
  ggtitle("Fig 3: Number of Applications Accepted")+
  theme_light()

#Enrolled

p2<-ggplot(data = df, aes(Private,Enroll,Apps,fill = Private))+
  geom_boxplot(color = "black", fill = "green")+
  ggtitle("Fig 4: Number of Students Enrolled")+
  theme_light()

ggarrange(p1,p2,
          ncol = 2, nrow = 1)

# Number of students enrolled in part-time and full-time in public and private university

df_time<- df %>%
  group_by(Private) %>%
  summarise(full_time=sum(F.Undergrad),
            part_time=sum(P.Undergrad))

p3<-ggplot(data = df_time, aes(x= Private, y=full_time, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 5: Number of Full-time Students") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=full_time), vjust=2, color="black", size=3.5)+
  theme(legend.position = "none")+
  theme_light()

p4<-ggplot(data = df_time, aes(x= Private, y=part_time, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 6: Number of Part-time Students") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=part_time), vjust=2, color="black", size=3.5)+
  theme_light()

ggarrange(p3,p4,
          ncol = 2, nrow = 1)

df_expenses<- df %>%
  group_by(Private) %>%
  summarise(out_state=round(mean(Outstate),2),
            expense=round(mean(Expend),2),
            books=round(mean(Books),2),
            personal=round(mean(Personal),2))

p5<-ggplot(data = df_expenses, aes(x= Private, y=out_state, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 7: Average Cost of Out of State Tution") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=out_state), vjust=2, color="black", size=3.5)+
  theme(legend.position = "none")+
  theme_light()

p6<-ggplot(data = df_expenses, aes(x= Private, y=expense, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 8: Average Cost of Instructional Expense per Student") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=expense), vjust=2, color="black", size=3.5)+
  theme_light()

p7<-ggplot(data = df_expenses, aes(x= Private, y=books, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 9: Average Cost for books") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=books), vjust=2, color="black", size=3.5)+
  theme(legend.position = "none")+
  theme_light()

p8<-ggplot(data = df_expenses, aes(x= Private, y=personal, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 10: Average Cost of Personal Expenses") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=personal), vjust=2, color="black", size=3.5)+
  theme(legend.position = "none")+
  theme_light()

ggarrange(p5,p6,p7,p8,
          ncol = 2, nrow = 2)

df_misc<- df %>%
  group_by(Private) %>%
  summarise(PHD=round(sum(PhD),2),
            terminal=round(sum(Terminal),2),
            s.f=round(mean(S.F.Ratio),2),
            grad.rate=round(mean(Grad.Rate),2))

p9<-ggplot(data = df_misc, aes(x= Private, y=PHD, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 11: Number of Faculty with PHD") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=PHD), vjust=2, color="black", size=3.5)+
  theme(legend.position = "none")+
  theme_light()

p10<-ggplot(data = df_misc, aes(x= Private, y=terminal, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 12: Number of Faculty with Terminal Degree ") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=terminal), vjust=2, color="black", size=3.5)+
  theme_light()

p11<-ggplot(data = df_misc, aes(x= Private, y=s.f, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 13: Average Student Faculty Ratio") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=s.f), vjust=2, color="black", size=3.5)+
  theme(legend.position = "none")+
  theme_light()

p12<-ggplot(data = df_misc, aes(x= Private, y=grad.rate, fill= Private))+
  geom_bar(stat = "identity")+
  ggtitle("Fig 14: Average Graduation Rate") +
  scale_y_continuous(name = "Number of Colleges") +
  scale_x_discrete(name = "Private College") +
  scale_fill_manual(values = c("steelblue","Red")) +
  geom_text(aes(label=grad.rate), vjust=2, color="black", size=3.5)+
  theme(legend.position = "none")+
  theme_light()

ggarrange(p9,p10,p11,p12,
          ncol = 2, nrow = 2)

# Creating Test and Train Data set

set.seed(1234)
sample <- sample.split(df$Private, SplitRatio = 0.7)
train  <- subset(df, sample == TRUE)
test   <- subset(df, sample == FALSE)

# Logistic Regression Analysis

m<- glm(Private~.,data = train,family = "binomial")
summary(m)

m1<- glm(Private~ Apps+Outstate+PhD+perc.alumni,data = train,family = "binomial")
summary(m1)

#Creating a Confusion Matrix for the train data set

p.train<-predict(m1,newdata= train,type = "response")

pred.prob<-as.factor(ifelse(p.train>=0.5,"Yes","No"))

confusionMatrix(pred.prob,as.factor(train$Private),positive = "Yes")

#Creating a Confusion Matrix for the test data set

p.test<-predict(m1,newdata= test,type = "response")

pred.prob_test<-as.factor(ifelse(p.test>=0.5,"Yes","No"))

confusionMatrix(pred.prob_test,as.factor(test$Private),positive = "Yes")

# Plotting the ROC curve 

roc_curve<-roc(test$Private,p.test)

plot(roc_curve,col = "red", main = "ROC Chart", ylab = "Sensitivity", xlab = "Specificity")

# Calculating AUC

auc_curve<-auc(roc_curve)

auc_curve
