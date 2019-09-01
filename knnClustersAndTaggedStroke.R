x = cleanedData
clustdata = x[,8:9]

attach(x)

k2=kmeans(clustdata,2,nstart=10)
c2 = k2$cluster
ssw2=sum(k2$withinss)

k3=kmeans(clustdata,3,nstart=10)
c3 = k3$cluster
ssw3=sum(k3$withinss)

k4=kmeans(clustdata,4,nstart=10)
c4 = k4$cluster
ssw4=sum(k4$withinss)

k5=kmeans(clustdata,5,nstart=10)
c5 = k5$cluster
ssw5=sum(k5$withinss)

k6=kmeans(clustdata,6,nstart=10)
c6 = k6$cluster
ssw6=sum(k6$withinss)

k7=kmeans(clustdata,7,nstart=10)
c7 = k7$cluster
ssw7=sum(k7$withinss)

ssw <- c(ssw2, ssw3, ssw4, ssw5, ssw6, ssw7)

plot(c(2:7), ssw, type="l", xlab="Number of Clusters")


plot(c5)

strokes <- subset(x, stroke == 1)

plot(age, avg_glucose_level, pch = 19, col = 'blue')
points(strokes$age, strokes$avg_glucose_level, pch=19, col='green')


plot(bmi, avg_glucose_level, pch = 19, col = 'blue')
points(strokes$bmi, strokes$avg_glucose_level, pch=19, col='green')

plot(heart_disease, bmi, pch = 19, col = 'blue')
points(strokes$heart_disease, strokes$bmi, pch=19, col='green')
