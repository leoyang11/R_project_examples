#### Cohort analysis with R – Retention charts 
#### https://analyzecore.com/2014/07/03/cohort-analysis-in-r-retention-charts/


cohort.clients <- data.frame(cohort=c('Cohort01','Cohort02',
                                      'Cohort03','Cohort04','Cohort05','Cohort06','Cohort07',
                                      'Cohort08','Cohort09','Cohort10','Cohort11','Cohort12'),
                             M01=c(11000,0,0,0,0,0,0,0,0,0,0,0),
                             M02=c(1900,10000,0,0,0,0,0,0,0,0,0,0),
                             M03=c(1400,2000,11500,0,0,0,0,0,0,0,0,0),
                             M04=c(1100,1300,2400,13200,0,0,0,0,0,0,0,0),
                             M05=c(1000,1100,1400,2400,11100,0,0,0,0,0,0,0),
                             M06=c(900,900,1200,1600,1900,10300,0,0,0,0,0,0),
                             M07=c(850,900,1100,1300,1300,1900,13000,0,0,0,0,0),
                             M08=c(850,850,1000,1200,1100,1300,1900,11500,0,0,0,0),
                             M09=c(800,800,950,1100,1100,1250,1000,1200,11000,0,0,0),
                             M10=c(800,780,900,1050,1050,1200,900,1200,1900,13200,0,0),
                             M11=c(750,750,900,1000,1000,1180,800,1100,1150,2000,11300,0),
                             M12=c(740,700,870,1000,900,1100,700,1050,1025,1300,1800,20000))


#connect libraries
library(dplyr)
library(ggplot2)
library(reshape2)

cohort.clients.r <- cohort.clients #create new data frame
totcols <- ncol(cohort.clients.r) #count number of columns in data set
for (i in 1:nrow(cohort.clients.r)) { #for loop for shifting each row
  df <- cohort.clients.r[i,] #select row from data frame
  df <- df[ , !df[]==0] #remove columns with zeros
  partcols <- ncol(df) #count number of columns in row (w/o zeros)
  #fill columns after values by zeros
  if (partcols < totcols) df[, c((partcols+1):totcols)] <- 0
  cohort.clients.r[i,] <- df #replace initial row by new one
}

#calculate retention (1)
x <- cohort.clients.r[,c(2:13)]
y <- cohort.clients.r[,2]
reten.r <- apply(x, 2, function(x) x/y )
reten.r <- data.frame(cohort=(cohort.clients.r$cohort), reten.r)

#calculate retention (2)
c <- ncol(cohort.clients.r)
reten.r <- cohort.clients.r
for (i in 2:c) {
  reten.r[, (c+i-1)] <- reten.r[, i] / reten.r[, 2]
}
reten.r <- reten.r[,-c(2:c)]
colnames(reten.r) <- colnames(cohort.clients.r)


#charts
reten.r <- reten.r[,-2] #remove M01 data because it is always 100%

#dynamics analysis chart
cohort.chart1 <- melt(reten.r, id.vars = 'cohort')
colnames(cohort.chart1) <- c('cohort', 'month', 'retention')
cohort.chart1 <- filter(cohort.chart1, retention != 0)
p <- ggplot(cohort.chart1, aes(x=month, y=retention, group=cohort, colour=cohort))
p + geom_line(size=2, alpha=1/2) +
  geom_point(size=3, alpha=1) +
  geom_smooth(aes(group=1), method = 'loess', size=2, colour='red', se=FALSE) +
  labs(title="Cohorts Retention ratio dynamics")

#second month analysis chart
cohort.chart2 <- filter(cohort.chart1, month=='M02') #choose any month instead of M02
p <- ggplot(cohort.chart2, aes(x=cohort, y=retention, colour=cohort))
p + geom_point(size=3) +
  geom_line(aes(group=1), size=2, alpha=1/2) +
  geom_smooth(aes(group=1), size=2, colour='red', method = 'lm', se=FALSE) +
  labs(title="Cohorts Retention ratio for 2nd month")

#cycle plot
cohort.chart3 <- cohort.chart1
cohort.chart3 <- mutate(cohort.chart3, month_cohort = paste(month, cohort))
p <- ggplot(cohort.chart3, aes(x=month_cohort, y=retention, group=month, colour=month))

#choose any cohorts instead of Cohort07 and Cohort06
m1 <- filter(cohort.chart3, cohort=='Cohort07')
m2 <- filter(cohort.chart3, cohort=='Cohort06')
p + geom_point(size=3) +
  geom_line(aes(group=month), size=2, alpha=1/2) +
  labs(title="Cohorts Retention ratio cycle plot") +
  geom_line(data=m1, aes(group=1), colour='blue', size=2, alpha=1/5) +
  geom_line(data=m2, aes(group=1), colour='blue', size=2, alpha=1/5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

