# Do number 1
data = c(78,66,65,63,60,60,58,56,52,50);
print(data);

# Do number 2
raw_car_matrix<-matrix( c(
                        1.8,1.5,2.0,2.5,1.8,2.5,1.6,1.5,  # Cylinder Volume
                        51,51,115,150,126,150,118,106     # Horsepower
                        ), ncol=2)
MD<-as.data.frame(raw_car_matrix)
rownames(MD)<-c("Honda Civic","Toyota Pruis","VW Golf"
                ,"VW Beatle","Toyota Corolla","VW Jetta",
                "Mini Cooper","Toyota Yaris")
colnames(MD)<-c("Cylinder Volume","Horsepower")
print(MD)


# Do number 3

