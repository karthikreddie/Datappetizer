#read data sets and combine train and test
data = read.csv("train.csv", header = T, sep = ",", na.strings = " ")
test = read.csv("test.csv", header = T, sep = ",", na.strings = " ")
test$Item_Outlet_Sales = -999
comb = rbind(data,test)

#Basic cleaning of data/Setting things right
write.csv(comb,file = "comb.csv")
comb[comb ==""] = NA
comb[comb =="0"] = NA

index = which(comb$Item_Fat_Content =="LF" | comb$Item_Fat_Content =="low fat")
comb[index,"Item_Fat_Content"] = "Low Fat"

index2 = which(comb$Item_Fat_Content =="reg")
comb[index2,"Item_Fat_Content"] = "Regular"

str(comb)
write.csv(comb, file = "comb.csv")

#Data preprocessing
colSums(is.na(comb))
comb$Item_Weight[is.na(comb$Item_Weight)] = median(comb$Item_Weight, na.rm = T)
comb$Item_Visibility[is.na(comb$Item_Visibility)] = median(comb$Item_Visibility, na.rm = T)

install.packages("DMwR")
install.packages("gdata")
library(DMwR)
comb = knnImputation(comb, k = 4, scale = T, meth = "median", distData = NULL)
summary(comb)

install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)

#Split the data into train,validation and test
#train = comb[1:8523,]
#test = comb[8524:14204,]
#test$Item_Outlet_Sales = NULL

glimpse(comb)
summary(comb)
str(comb)

comb = comb %>% 
  mutate(Item_Identifier_str2 = substr(Item_Identifier,1,2),
         Outlet_age = 2018- Outlet_Establishment_Year,
         serial_num = row_number())
comb$Item_Fat_Content = ifelse(comb$Item_Fat_Content =="Regular", 1,0)

str(comb)

#dummifying the data
install.packages("dummies")
library(dummies)
new_comb = dummy.data.frame(comb,names = c("Outlet_Size","Outlet_Location_Type","Outlet_Type","Item_Identifier_str2"),sep = "_")
glimpse(new_comb)
str(new_comb)

#Delete character variables before checking for correlation
new_combi = select(new_comb, -c(Item_Identifier,Item_Identifier, Outlet_Identifier, Item_Fat_Content,Outlet_Establishment_Year, Item_Type,serial_num))
glimpse(new_combi)

#We divide our test and train data for looking correlation.
pred_train  = new_combi %>%
  filter(Item_Outlet_Sales != -999)
pred_test = new_combi %>%
  filter(Item_Outlet_Sales == -999)

library(corrplot)
library(RColorBrewer)

corrplot(cor(pred_train),diag = F, tl.cex = 0.5,method = "number", type = "upper")



#Split the data into train,validation and test
train = new_comb[1:8523,]
test = new_comb[8524:14204,]

set.seed(29)

train_rows = sample(x = 1:nrow(train),size=0.8*nrow(train))
train_data = train[train_rows,]
val_data = train[-train_rows,]

#Building Basic Model
train_data = select(train_data, -c(Item_Type,Item_Identifier, Outlet_Identifier,Outlet_Establishment_Year, serial_num))
model_basic = lm(Item_Outlet_Sales ~.,data = train_data)
summary(model_basic)

install.packages("MASS")
library(MASS)
model_aic = stepAIC(model_basic,direction = "both")
summary(model_aic)




