library(dplyr)
test=store_test
train=store_train

glimpse(train)
glimpse(test)


test$store=NA
train$data="train"
test$data="test"
x=rbind(train,test)
glimpse(x)

table(x$country)

x$store=as.factor(x$store)
glimpse(x)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}


names(x)[sapply(x,function(j) is.character(j))]

x=x%>%
  select(-countyname,-storecode,-Areaname,-countytownname)

cat_cols=c("state_alpha","store_Type")

for(cat in cat_cols){
  x=CreateDummies(x,cat,100) 
}

lapply(x,function(n)sum(is.na(n)))


for(col in names(x)){
  if(sum(is.na(x[,col]))>0 & !(col %in% c("data","store"))){
    x[is.na(x[,col]),col]=mean(x[x$data=='train',col],na.rm=T)
  }
}

lapply(x,function(n)sum(is.na(n)))

train=x %>% filter(data=="train") %>% select(-data)
test=x %>% filter(data=="test") %>% select(-data,-store)

set.seed(2)
x=sample(1:nrow(train),0.8*nrow(train))
train1=train[x,]
train2=train[-x,]


library(randomForest)
model_rf=randomForest(store~.-Id,data=train1,mtry=5,ntree=100)
model_rf

val.score=predict(model_rf,newdata=train2,type='response')

library(caret)
confusionMatrix(val.score,train2$store)

val.prob_score=predict(model_rf,newdata=train2,type='prob')

library(pROC)
auc_score=auc(roc(train2$store,val.prob_score[,1]))
auc_score

plot(roc(train2$store,val.prob_score[,1]))
model_rf_final=randomForest(store~.-Id,data=train,mtry=5,ntree=100)
model_rf_final

score=predict(model_rf_final,newdata = test,type='prob')[,2]
score






write.csv(test.score,"Somil_Taneja_P2_part2.csv")

