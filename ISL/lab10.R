
USE.GENDER <- FALSE

unsup.grades <- grades %>% select(quizzes:gender)
unsup.grades$gender <- as.numeric(unsup.grades$gender)

if (!USE.GENDER) {
    unsup.grades <- unsup.grades %>% select(-gender)
}
row.names(unsup.grades) <- grades$Name
# (Now need to fix to ensure no dups.)

sc.unsup.grades <- scale(unsup.grades)

# cluster based on (scaled) original features
km.out <- kmeans(sc.unsup.grades,3,nstart=50)

# Plot first two principal components and cluster
g <- ggplot(data.frame(name=row.names(sc.unsup.grades),
                       x=prcomp(sc.unsup.grades)$x[,1:2],
                       y=factor(km.out$cluster))) +
    geom_text(aes(x=x.PC1,y=x.PC2,col=y,label=name),size=2.5) +
    ggtitle("First two principal components, and k-means cluster label")
print(g)

readline(prompt="Press Enter.")


hc.out <- hclust(dist(sc.unsup.grades), method="complete")

plot(hc.out)
