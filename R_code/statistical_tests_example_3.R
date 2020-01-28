#######Example 3, one way ANOVA##############
#input data
group<-c("A","A","B","B","B","C","C","C","D","D")
sales<-c(12,18,14,12,13,19,17,21,24,30)
data_ex3<-data.frame(group=group,sales=sales)

#to produce boxplot
library("ggpubr")
ggboxplot(data_ex3, x = "group", y = "sales", order = c("A", "B", "C", "D"),
          ylab = "Sales", xlab = "Group")

#conduct one-way ANOVA, treat sales as the response, group as the factor.
res.aov <- aov(sales ~ group, data = data_ex3)
# Summary of the analysis
summary(res.aov)

#All pairwise comparisons of factor level means
TukeyHSD(res.aov)

#Check homogeneity of variances
plot(res.aov, 1)

#Check normality
plot(res.aov,2)


########one way ANOVA regression approach#####
X0<-c(1,1,1,1,1,1,1,1,1,1,1)
X1<-c(1,1,0,0,0,0,0,0,-1,-1)
X2<-c(0,0,1,1,1,0,0,0,-1,-1)
X3<-c(0,0,0,0,0,1,1,1,-1,-1)

##create a new data frame
data_ex4<-data.frame(sales=sales,X0=rep(1,times=10),X1=X1,X2=X2,X3=X3)

model_ex4<-lm(formula = data_ex4$sales ~ data_ex4$X1+data_ex4$X2+data_ex4$X3, data = data_ex4)
summary(model_ex4)

pairwise.t.test(data_ex3$sales, data_ex3$group,p.adjust.method = "BH")