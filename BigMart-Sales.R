setwd("~/Documents/Big-Mart-Sales")

#Loading the Packages
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(xgboost)
library(cowplot)

#Reading the datasets
train = read.csv("Train_data.csv")
test = read.csv("Test_data.csv")

#train and test data column names
names(train)
names(test)

#Structure of train and test dataset
str(train)
str(test)

#Since test data does not have dependent avriable will create one for it and then combine the train and test data
test$Item_Outlet_Sales <- NA 
combi = rbind(train,test)
dim(combi)

#----- Univariate Analysis

#Plotting all the individual variables to gain some insights 
ggplot(train)+ geom_histogram(aes(train$Item_Outlet_Sales),binwidth = 100, fill= "darkgreen") +xlab("Item_Outlet_Sales")

#Checking the distribution of independant numeric variables using histogram
plot1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
plot2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
plot3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")
plot_grid(plot1, plot2,plot3, nrow = 1)

#Independent Variable(categorical)

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content,Count),stat = "identity", fill = "coral1")

#Combining the LF, low fat into "Low Fat" and reg to"Regular"
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content,Count),stat = "identity", fill = "coral1")

#plot for Item_Type
plot4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type,Count),stat = "identity", fill = "coral1") +
  xlab("") +geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,)) + ggtitle("Item_Type")

#Plot for Outlet_Identifier
plot5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier,Count),stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,)) + ggtitle("Item_Type")

#plot for Outlet_size
plot6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size,Count),stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,)) + ggtitle("Item_Type")
second_row = plot_grid(plot5, plot6, nrow = 1)
plot_grid(plot4, second_row, ncol = 1)

# plot fot Outlet_Establishment_Year
plot7= ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Establishment_Year,Count),stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Establishment_Year, Count, label = Count), vjust = 0.5) + xlab("Outlet_Establishment_Year")
  theme(axis.text.x = element_text(size = 8.5))

#plot for Outlet_Type  
plot8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(size = 8.5))

plot_grid(plot7, plot8, ncol = 2)  

# Bivariate Analysis
#Exctraxcting the train dataset for Combi
train = combi[1:nrow(train)]

#exploring target variable and independent numeric variable using scatter plot

#plot for Item_Weight and Item_outlet_Sales
plot9 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +     
   theme(axis.title = element_text(size = 8.5))

#plot for Item_Visibility and Item_outlet_Sales
plot10 = ggplot(train) +  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))

#plot for Item_MRP and Item_outlet_Sales
plot11 = ggplot(train) +  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +     
  theme(axis.title = element_text(size = 8.5))

#plotting plot9.plot10, plot11 into grid
second_row_2 = plot_grid(plot10, plot11, ncol = 2) 
plot_grid(plot9, second_row_2, nrow = 2)

#Lexploring the target variable and the independent categorical variable
# Item_Type vs Item_Outlet_Sales
plot12 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "lightblue") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6),  
        axis.title = element_text(size = 8.5))

# Item_Fat_Content vs Item_Outlet_Sales 
plot13 = ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8.5))

# Outlet_Identifier vs Item_Outlet_Sales 
plot14 = ggplot(train) + geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "lightblue") +     
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

second_row_3 = plot_grid(plot13, plot14, ncol = 2)
plot_grid(plot12, second_row_3, ncol = 1)

#In Univariate analysis we came acroos the column in the Outlet_size which was blank.
#Let's check its distrubution
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "lightgreen")

plot15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta") 
plot16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta") 
plot_grid(plot15, plot16, ncol = 1)

#--- Missing Value Imputation-----
colSums(is.na(combi))

missing_index = which(is.na(combi$Item_Weight)) 
for(i in missing_index){   
  item = combi$Item_Identifier[i]  
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T) 
  }

#Replacing 0’s in Item_Visibility variable
zero_index = which(combi$Item_Visibility == 0) 
for(i in zero_index){    
  item = combi$Item_Identifier[i]  
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T) 
  }
  