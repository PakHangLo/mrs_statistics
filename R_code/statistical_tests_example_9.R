#read data
demo_data<-read.csv("C:/Users/hang_/Documents/mrs/demo_data.csv")

#plot profile
with(demo_data, interaction.plot(time, sexe, GABAA_diff,
  ylim = c(-0.3, 0), lty = c(1, 12), lwd = 3,
  ylab = "mean of GABAA_diff", xlab = "time", trace.label = "sexe"))

#arrange the data set, so that for each participant, the GABAA_diff measured at time 1, time 2 and time 3 are in one row
for (i in c(1,4,7,10,13,16)){
demo_data$GABAA_diff1[i]=demo_data$GABAA_diff[i]
demo_data$GABAA_diff2[i]=demo_data$GABAA_diff[i+1]
demo_data$GABAA_diff3[i]=demo_data$GABAA_diff[i+2]
}

#delete those rows where GABAA_diff is measured at time 2 and time 3.
demo_data<-demo_data[-c(2,3,5,6,8,9,11,12,14,15,17,18),]

#extract the GABAA_diff at those 3 time points to form the response variable matrix.
demo_Y<-cbind(demo_data$GABAA_diff1,demo_data$GABAA_diff2,demo_data$GABAA_diff3)
#conduct MANOVA
demo_manova <- manova(demo_Y ~ demo_data$sexe, data=demo_data)
summary(demo_manova)
#Profile analysis, a series of hypothesis testings.
#To see if the profiles are parallel, coincide, horizontal.
library(profileR)
demo_prof<-pbg(demo_Y,demo_data$sexe, original.names=FALSE,profile.plot=FALSE)
summary(demo_prof)

#From here onwards, the code computes the matrix E and H, hence the confidence intervals...
data<-demo_data
data$group<-data$sexe
n_i <- as.data.frame(table(data$group))
no_of_group<-nrow(as.data.frame(table(data$group)))
p<-3

g1 <- data[ which(data$group=='M'), ]
g2 <- data[ which(data$group=='F'), ]
n1 <- nrow(g1)
n2 <- nrow(g2)

colMeans_g1<-colMeans(g1[,6:8])
colMeans_g2<-colMeans(g2[,6:8])

colMeans_data<-colMeans(data[,6:8])

H<-n1 * (colMeans_g1-colMeans_data) %*% t(colMeans_g1-colMeans_data)+
n2 * (colMeans_g2-colMeans_data) %*% t(colMeans_g2-colMeans_data)


A<-data[,6:8]
B<-as.numeric(unlist(A))
dim(B)<-c(nrow(data),p)
C<-t(B)
dim(C)<-c(p,1,nrow(data))
D<-C
dim(D)<-c(1,p,nrow(data))

E<-array(0,dim=c(p,p,nrow(data)))

for (i in 1:nrow(data)) {
E[,,i]<-C[,,i]%*%t(D[,,i])
}

colSums_g1<-colSums(g1[,6:8])
colSums_g2<-colSums(g2[,6:8])

E<-apply(E,MARGIN=c(1,2),sum)-1/n1*colSums_g1%*%t(colSums_g1)-
1/n2*colSums_g2%*%t(colSums_g2)
G<-solve(E)%*%H

y<-eigen(G)
y$val

#compute the confidence intervals of the difference of GABAA_diff1 between male and femaleat time 1
#set alpha (significance level) to be 0.05
a<-c(1,0,0)
colMeans_g1[1]-colMeans_g2[1]+qt(0.05/2,6-2)*sqrt(t(a)%*%E%*%a/(6-2)*2/3)
colMeans_g1[1]-colMeans_g2[1]+qt(1-0.05/2,6-2)*sqrt(t(a)%*%E%*%a/(6-2)*2/3)


#compute the confidence intervals of the difference of GABAA_diff2 between male and female at time 2
#set alpha (significance level) to be 0.05
a<-c(1,0,0)
colMeans_g1[2]-colMeans_g2[2]+qt(0.05/2,6-2)*sqrt(t(a)%*%E%*%a/(6-2)*2/3)
colMeans_g1[2]-colMeans_g2[2]+qt(1-0.05/2,6-2)*sqrt(t(a)%*%E%*%a/(6-2)*2/3)

#compute the confidence intervals of the difference of GABAA_diff3 between male and female at time 3
#set alpha (significance level) to be 0.05
a<-c(1,0,0)
colMeans_g1[3]-colMeans_g2[3]+qt(0.05/2,6-2)*sqrt(t(a)%*%E%*%a/(6-2)*2/3)
colMeans_g1[3]-colMeans_g2[3]+qt(1-0.05/2,6-2)*sqrt(t(a)%*%E%*%a/(6-2)*2/3)


#compute the confidence intervals of difference of average GABAA_diff for those 3 time points between male and female
#set alpha (significance level) to be 0.05
a<-c(1/3,1/3,1/3)
1/3*(colMeans_g1[1]+colMeans_g1[2]+colMeans_g1[3])-1/3*(colMeans_g2[1]+colMeans_g2[2]+colMeans_g2[3])+qt(0.05/2,6-2)*sqrt(t(a)%*%E%*%a/(6-2)*2/3)
1/3*(colMeans_g1[1]+colMeans_g1[2]+colMeans_g1[3])-1/3*(colMeans_g2[1]+colMeans_g2[2]+colMeans_g2[3])+qt(1-0.05/2,6-2)*sqrt(t(a)%*%E%*%a/(6-2)*2/3)
