#### Cohort analysis with R – “layer-cake graph” 
#### https://analyzecore.com/2014/05/31/cohort-analysis-in-r-layer-cake-graph/


cohort.sum <- data.frame(cohort=c('Cohort01', 'Cohort02', 'Cohort03', 'Cohort04', 'Cohort05', 'Cohort06', 'Cohort07', 'Cohort08', 'Cohort09', 'Cohort10', 'Cohort11', 'Cohort12'),
                         M1=c(270000,0,0,0,0,0,0,0,0,0,0,0),
                         M2=c(85000,275000,0,0,0,0,0,0,0,0,0,0),
                         M3=c(72000,63000,277000,0,0,0,0,0,0,0,0,0),
                         M4=c(52000,42000,76000,361000,0,0,0,0,0,0,0,0),
                         M5=c(50000,45000,60000,80000,288000,0,0,0,0,0,0,0),
                         M6=c(51000,52000,55000,51000,58000,253000,0,0,0,0,0,0),
                         M7=c(51000,69000,48000,45000,42000,54000,272000,0,0,0,0,0),
                         M8=c(46000,85000,77000,41000,38000,37000,74000,352000,0,0,0,0),
                         M9=c(38000,42000,72000,41000,31000,30000,49000,107000,285000,0,0,0),
                         M10=c(39000,38000,45000,33000,34000,34000,46000,83000,69000,279000,0,0),
                         M11=c(38000,42000,31000,32000,26000,28000,43000,82000,51000,87000,282000,0),
                         M12=c(35000,35000,38000,45000,35000,32000,48000,44000,47000,52000,92000,500000))
cohort.sum

#connect necessary libraries
library(ggplot2)
library(reshape2)
#we need to melt data
cohort.chart <- melt(cohort.sum, id.vars = "cohort")
colnames(cohort.chart) <- c('cohort', 'month', 'revenue')

#define palette
blues <- colorRampPalette(c('lightblue', 'darkblue'))

#plot data
p <- ggplot(cohort.chart, aes(x=month, y=revenue, group=cohort))
p + geom_area(aes(fill = cohort)) +
  scale_fill_manual(values = blues(nrow(cohort.sum))) +
  ggtitle('Total revenue by Cohort')


#### Cohort analysis with R – “layer-cake graph2” 
#### https://analyzecore.com/2014/06/05/cohort-analysis-in-r-layer-cake-graph-part-2/

cohort.clients <- data.frame(cohort=c('Cohort01', 'Cohort02', 'Cohort03', 'Cohort04', 'Cohort05', 'Cohort06', 'Cohort07', 'Cohort08', 'Cohort09', 'Cohort10', 'Cohort11', 'Cohort12'),
                             M1=c(11000,0,0,0,0,0,0,0,0,0,0,0),
                             M2=c(1900,10000,0,0,0,0,0,0,0,0,0,0),
                             M3=c(1400,2000,11500,0,0,0,0,0,0,0,0,0),
                             M4=c(1100,1300,2400,13200,0,0,0,0,0,0,0,0),
                             M5=c(1000,1100,1400,2400,11100,0,0,0,0,0,0,0),
                             M6=c(900,900,1200,1600,1900,10300,0,0,0,0,0,0),
                             M7=c(850,900,1100,1300,1300,1900,13000,0,0,0,0,0),
                             M8=c(850,850,1000,1200,1100,1300,1900,11500,0,0,0,0),
                             M9=c(800,800,950,1100,1100,1250,1000,1200,11000,0,0,0),
                             M10=c(800,780,900,1050,1050,1200,900,1200,1900,13200,0,0),
                             M11=c(750,750,900,1000,1000,1180,800,1100,1150,2000,11300,0),
                             M12=c(740,700,870,1000,900,1100,700,1050,1025,1300,1800,20000))

#connect necessary libraries
library(ggplot2)
library(reshape2)
#we need to melt data
cohort.chart.cl <- melt(cohort.clients, id.vars = 'cohort')
colnames(cohort.chart.cl) <- c('cohort', 'month', 'clients')

#define palette
reds <- colorRampPalette(c('pink', 'dark red'))

#plot data
p <- ggplot(cohort.chart.cl, aes(x=month, y=clients, group=cohort))
p + geom_area(aes(fill = cohort)) +
  scale_fill_manual(values = reds(nrow(cohort.clients))) +
  ggtitle('Active clients by Cohort')

#we need to divide the data frames (excluding cohort name)
rev.per.client <- cohort.sum[,c(2:13)]/cohort.clients[,c(2:13)]
rev.per.client[is.na(rev.per.client)] <- 0
rev.per.client <- cbind(cohort.sum[,1], rev.per.client)

#define palette
greens <- colorRampPalette(c('light green', 'dark green'))

#melt and plot data
cohort.chart.per.cl <- melt(rev.per.client, id.vars = 'cohort.sum[, 1]')
colnames(cohort.chart.per.cl) <- c('cohort', 'month', 'average_revenue')
p <- ggplot(cohort.chart.per.cl, aes(x=month, y=average_revenue, group=cohort))
p + geom_area(aes(fill = cohort)) +
  scale_fill_manual(values = greens(nrow(cohort.clients))) +
  ggtitle('Average revenue per client by Cohort')







