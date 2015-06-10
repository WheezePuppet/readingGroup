
require(MASS)

# Data stuff.
structure(list(Name = c("Baldanza, Gary", "Beale, Carson", "Beavers, Duncan", 
"Beckham, Dalina", "Blauvelt, Jon", "Brooks, Jack", "Cassine, Kenneth", 
"Chao, Teresa", "Clark, Mary", "Collins, Flannery", "Desormeaux, Michelle", 
"Dudek, Ashley", "Fuqua, Tyler", "Gaughan, Matthew", "Hensley, Tyler", 
"Hinton, Andrew", "Hooks, Kris", "Izaguirre, Austin", "Jilili, Yireda", 
"Joppich, David", "Kim, Julianna", "Lloyd, Chris", "Luebke, Chester", 
"Marche, George", "Mason, Tommy", "May, Evan", "May, Grace", 
"Murphy, James", "Mwandu, Josh", "Oconnor, Neil", "Ogden, Quinn", 
"Parker, Campbell", "Parker, Matt", "Payne, Zach", "Raczkowski, Jason", 
"Raze, Brittany", "Sebenaler, Nicholas", "Shannon, Austin", "Stello, Tim", 
"Stosch, Katherine", "Stuart, Savannah", "Thompson, Andre", "Tracy, Kyle", 
"Treacy, Ryan", "Truslow, Tyler", "Whipple, J.T.", "Will, Brian", 
"Wuepper, Abie", "Zimmerman, Chris"), quizzes = c(8.83, 7.92, 
6.5, 6.58, 10, 6.83, 8.17, 9.25, 7.92, 6.5, 8.58, 8.83, 8.92, 
9, 5.67, 2.83, 6, 10, 5.17, 8, 8.75, 10.33, 8.83, 8.17, 7.08, 
9.67, 6.5, 9.75, 6.17, 2.67, 6.42, 8.42, 8.75, 7.08, 8.92, 9, 
9.17, 9.67, 10.33, 8.83, 7.08, 8.33, 5.42, 7.67, 8.67, 8.83, 
8, 9.83, 10.5), HWs = c(71.75, 83, 84.38, 95.75, 107.5, 86.25, 
88.12, 99.38, 78.75, 66.88, 85.62, 71.88, 71.88, 79.88, 76.88, 
11.25, 97.5, 85.62, 58.75, 0, 75.62, 105.62, 98.12, 46.88, 69.38, 
86.88, 85.62, 98.12, 56.88, 56.25, 50, 91.25, 72.5, 70.62, 96.75, 
83.12, 97.5, 46.88, 106.25, 93.75, 88.75, 98.75, 81.25, 84.62, 
83.12, 86.88, 92.5, 85.38, 91.88), Labs = c(12.86, 6, 3.71, 11.79, 
13.43, 8.93, 6.86, 9.93, 9.71, 3.71, 9.29, 8.57, 8.57, 10, 7.5, 
7.64, 10.21, 10, 4.36, 6, 10, 9.93, 9.93, 7.21, 4.36, 9.71, 8.57, 
9.57, 7.21, 6.86, 11.43, 9.57, 8.36, 13.43, 9.93, 10.21, 9.93, 
10, 12.86, 9.93, 9.29, 12.21, 7.64, 8.36, 10, 8.57, 7.5, 10, 
8.93), Final = c(94, 57, 72, 69, NA, 69, 89, NA, 80, 54, 78, 
82, 82, 74, 67, 79, 60, 90, 60, 54, 79, NA, 79, 85, 81, 91, 69, 
NA, 77, 49, 64, 72, 73, 49, 77, 94, 89, 96, NA, 70, 78, 89, 79, 
79, 85, 85, 83, 85, NA), gender = structure(c(1L, 1L, 1L, 2L, 
1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 
2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 
1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L), .Label = c("M", 
"F"), class = "factor"), verdict = structure(c(1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("pass", 
"fail"), class = "factor"), type = structure(c(2L, 3L, 3L, 3L, 
2L, 3L, 2L, 2L, 1L, 3L, 1L, 1L, 2L, 3L, 3L, 3L, 1L, 2L, 3L, 3L, 
3L, 2L, 1L, 2L, 3L, 2L, 1L, 2L, 2L, 3L, 3L, 1L, 2L, 3L, 1L, 2L, 
2L, 2L, 1L, 1L, 1L, 1L, 3L, 3L, 1L, 1L, 1L, 2L, 2L), .Label = c("worker", 
"genius", "sucky"), class = "factor")), .Names = c("Name", "quizzes", 
"HWs", "Labs", "Final", "gender", "verdict", "type"), row.names = c(NA, 
-49L), class = c("tbl_df", "tbl", "data.frame")) -> grades

# Modeling stuff.
lda.fit <- lda(type~quizzes+HWs,data=grades)
lda.results <- tbl_df(cbind(grades$Name,data.frame(predicted=predict(lda.fit)$class),as.data.frame(round(predict(lda.fit)$posterior,2))))
qda.fit <- qda(type~quizzes+HWs,data=grades)
qda.results <- tbl_df(cbind(grades$Name,data.frame(predicted=predict(qda.fit)$class),as.data.frame(round(predict(qda.fit)$posterior,2))))

lda.plot.data <- cbind(grades,correct=lda.results$predicted==grades$type,
    classifier="LDA")
qda.plot.data <- cbind(grades,correct=qda.results$predicted==grades$type,
    classifier="QDA")
plot.data <- rbind(lda.plot.data,qda.plot.data)
plot.data$classifier <- as.factor(plot.data$classifier)

# Plotting stuff.
ggplot(plot.data, aes(x=quizzes,y=HWs,color=type,shape=correct)) + 
    geom_point(size=2.5) +
    facet_grid(.~classifier) +
    scale_shape_manual(values=c("TRUE"=20,"FALSE"=4)) +
    expand_limits(x=c(0,10),y=c(0,100)) +
    scale_y_continuous(breaks=seq(0,100,10)) + 
    scale_x_continuous(breaks=0:10) +
    scale_color_manual(values=c("blue","red","darkgrey")) -> p
p <- p + annotate("text",x=8.2,y=90,label="worker",size=4,family="serif") +
         annotate("text",x=9.2,y=82,label="genius",size=4,family="serif") +
         annotate("text",x=6.4,y=66.5,label="sucky",size=4,family="serif")
print(p)

# From Dave:
#m <- apply(lda.fit$means,2,mean)
#b <- -diff(lda.fit$means[,2])/diff(lda.fit$means[,1])
#a <- m[2] - b*m[1]
#p <- last_plot() + geom_abline(slope=b,intercept=a)
#print(p)

