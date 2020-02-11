#install.packages(c("lattice"))
library(lattice)
#install.packages(c("gclus"))
library(gclus)

ufc <- read.csv("/Users/vikasnair/Documents/PBA_Project/pba_coursework/data/preprocessing_PCA_features.csv",header = TRUE)
#ufc <- read.csv("/Users/vikasnair/Documents/PBA_Project/pba_coursework/data/1993_preprocessed_data.csv.csv",header = TRUE)

#Filtering data
ufc.data <- ufc[,c(3:43)]
#ufc.data <- ufc[,c(3:5,7:71,73:160)]

ufc.corr_vals_abs = abs(cor(ufc.data))
#ufc.color = dmat.color(ufc.corr_vals_abs)
#ufc.ordered = order.single(cor(ufc.data))
#cpairs(fighter_stats.data,fighter_stats.ordered,panel.colors =fighter_stats.color,gap=0.1)

#PCA and Plotting Component Variance
ufc.prc = prcomp(ufc.data, center = TRUE,scale = TRUE)
screeplot(ufc.prc,main = "Scree Plot",xlab = "Components")
screeplot(ufc.prc,main = "Scree Plot",xlab = "Line")

#Variables to consider Kaiser Criterion
variance = ufc.prc$sdev ^2

#Kaiser Criterion - 13 variables
print(variance)


#Scree Plot
screeplot(ufc.prc,main = "Scree Plot",xlab = "Components")
screeplot(ufc.prc,type = "line",main = "Scree Plot")

#PC1 Dot Plot
load = ufc.prc$rotation
sorted.loadings_pc1 = load[order(load[,1]),1]
pc1_title = "Loadings Plot for PC1"
pc1_xlab = "Variable Loadings"
dotplot(sorted.loadings_pc1,main=pc1_title,xlab=pc1_xlab,cex = 1.5,col="red")


#PC2 Dot Plot
load = ufc.prc$rotation
sorted.loadings_pc2 = load[order(load[,2]),2]
pc2_title = "Loadings Plot for PC2"
pc2_xlab = "Variable Loadings"
dotplot(sorted.loadings_pc2,main=pc2_title,xlab=pc2_xlab,cex = 1.5,col="red")


#PC3 Dot Plot
load = ufc.prc$rotation
sorted.loadings_pc3 = load[order(load[,3]),3]
pc3_title = "Loadings Plot for PC3"
pc3_xlab = "Variable Loadings"
dotplot(sorted.loadings_pc3,main=pc3_title,xlab=pc3_xlab,cex = 1.5,col="red")

#PC4 Dot Plot
load = ufc.prc$rotation
sorted.loadings_pc4 = load[order(load[,4]),4]
pc4_title = "Loadings Plot for PC4"
pc4_xlab = "Variable Loadings"
dotplot(sorted.loadings_pc4,main=pc4_title,xlab=pc4_xlab,cex = 1.5,col="red")

#PC5 Dot Plot
load = ufc.prc$rotation
sorted.loadings_pc5 = load[order(load[,5]),5]
pc5_title = "Loadings Plot for PC5"
pc5_xlab = "Variable Loadings"
dotplot(sorted.loadings_pc5,main=pc5_title,xlab=pc5_xlab,cex = 1.5,col="red")

#PC6 Dot Plot
load = ufc.prc$rotation
sorted.loadings_pc6 = load[order(load[,6]),6]
pc6_title = "Loadings Plot for PC6"
pc6_xlab = "Variable Loadings"
dotplot(sorted.loadings_pc6,main=pc6_title,xlab=pc6_xlab,cex = 1.6,col="red")

#PC7 Dot Plot
load = ufc.prc$rotation
sorted.loadings_pc7 = load[order(load[,7]),7]
pc7_title = "Loadings Plot for PC7"
pc7_xlab = "Variable Loadings"
dotplot(sorted.loadings_pc7,main=pc7_title,xlab=pc7_xlab,cex = 1.7,col="red")


#Varimax Rotation
ufc.varimax_rotation = varimax(ufc.prc$rotation)
#options(max.print=999999)

ufc.varimax_rotation$loadings
#write.table(ufc.varimax_rotation$loadings,file = "/Users/vikasnair/Documents/PBA_Project/pba_coursework/Results/varimax_rotation.txt", append = FALSE, sep = " ", dec = ".",row.names = TRUE, col.names = TRUE)

write.csv(ufc.varimax_rotation$loadings,file = "/Users/vikasnair/Documents/PBA_Project/pba_coursework/Results/varimax_rotation_data_reduced.csv")
