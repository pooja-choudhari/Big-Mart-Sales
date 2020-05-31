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
