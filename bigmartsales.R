# import libraries
library(data.table) # used for reading and manipulation of data 
library(dplyr)      # used for data manipulation and joining 
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling 
library(corrplot)   # used for making correlation plot 
library(xgboost)    # used for building XGBoost model 
library(cowplot)    #used for combining many plots
#-------------------------------------------------------------
# set working environment
getwd()
train = fread("Trainbig.csv") 
test = fread("Testbig.csv") 
submission = fread("SampleSubmissionbig.csv")
str(train)
names(train)
test[,Item_Outlet_Sales := NA] 
summary(train)
combi = rbind(train, test) # combining train and test datasets 
dim(combi)
str(combi)
#------------------------------------------------------------------

# Target Variable
# EDA

ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +  xlab("Item_Outlet_Sales")

# Independent Variables (numeric variables)
p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue") 
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue") 
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue") 
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package

# Independent Variables (categorical variables)
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular" 
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

# Now let’s check the other categorical variables.
str(combi)
# plot for Item_Type 
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +   geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +  xlab("") +  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  ggtitle("Item_Type")
# plot for Outlet_Identifier 
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# plot for Outlet_Size 
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
second_row = plot_grid(p5, p6, nrow = 1) 
plot_grid(p4, second_row, ncol = 1)

# We’ll also check the remaining categorical variables.

# plot for Outlet_Establishment_Year 
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +   geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +  xlab("Outlet_Establishment_Year") +  theme(axis.text.x = element_text(size = 8.5))
# plot for Outlet_Type 
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(size = 8.5))
# ploting both plots together 
plot_grid(p7, p8, ncol = 2)

# bivariate analysis
train = combi[1:nrow(train)] 
# extracting train data from the combined data

# Target Variable vs Independent 
# Item_Weight vs Item_Outlet_Sales 
p9 = ggplot(train) +geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) + theme(axis.title = element_text(size = 8.5))
# Item_Visibility vs Item_Outlet_Sales 
p10 = ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      theme(axis.title = element_text(size = 8.5))
# Item_MRP vs Item_Outlet_Sales 
p11 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      theme(axis.title = element_text(size = 8.5))
second_row_2 = plot_grid(p10, p11, ncol = 2) 
plot_grid(p9, second_row_2, nrow = 2)




# Target Variable vs Independent Categorical Variables
# Item_Type vs Item_Outlet_Sales 
p12 = ggplot(train) +geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 6), axis.title = element_text(size = 8.5))
# Item_Fat_Content vs Item_Outlet_Sales 
p13 = ggplot(train) +geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 8),axis.title = element_text(size = 8.5))
# Outlet_Identifier vs Item_Outlet_Sales 
p14 = ggplot(train) +geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") + theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 8),axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p13, p14, ncol = 2) 
plot_grid(p12, second_row_3, ncol = 1)
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")
p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta") 
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta") 
plot_grid(p15, p16, ncol = 1)

#----------------------------------------------------------------------------
#missing value treatment

colSums(is.na(combi))
sum(is.na(combi$Item_Weight))
missing_index = which(is.na(combi$Item_Weight)) 
for(i in missing_index){
  item = combi$Item_Identifier[i]  
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T) }

zero_index = which(combi$Item_Visibility == 0) 
for(i in zero_index){
item = combi$Item_Identifier[i]  
combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)  }
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

#------------------------------------------------------------------------------------
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")
# create a new feature 'Item_Type_new' 
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]
table(combi$Item_Type, substr(combi$Item_Identifier, 1,2))
combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"


# years of operation for outlets 
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year] 
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 
# Price per unit weight 
combi[,price_per_unit_wt := Item_MRP/Item_Weight]
# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st", ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]

combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0, ifelse(Outlet_Size == "Medium", 1, 2))] 

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0, ifelse(Outlet_Location_Type == "Tier 2", 1, 2))] 
# removing categorical variables after label encoding 

combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T) 
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")])) 
combi = cbind(combi[,"Item_Identifier"], ohe_df)
#----------------------------------------------------------------------------------
combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero 
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]
#--------------------------------------------------------------------------

# Scaling numeric predictors
num_vars = which(sapply(combi, is.numeric)) # index of numeric features 
num_vars_names = names(num_vars) 

combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F] 
prep_num = preProcess(combi_numeric, method=c("center", "scale")) 
combi_numeric_norm = predict(prep_num, combi_numeric)
combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables 
combi = cbind(combi, combi_numeric_norm)
#-----------------------------------------------
# Splitting the combined data combi back to train and test set.

train = combi[1:nrow(train)] 
test = combi[(nrow(train) + 1):nrow(combi)] 
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset
#------------------------------------------------------

#Correlated Variables
cor_train = cor(train[,-c("Item_Identifier")]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)
#--------------------------------------------------------

linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])
summary(linear_reg_mod)
#---------------------------------------------------------------
#Making Predictions on test Data
# preparing dataframe for submission and writing it in a csv file 
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")]) 
write.csv(submission, "Linear_Reg_submit.csv", row.names = F)
##############-----------------------------------------------------------
