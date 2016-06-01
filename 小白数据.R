# install.packages("jsonlite")
# install.packages("XML")
# install.packages("stringr")
library(jsonlite) # 这个没有问题
library(XML)
library(stringr)
xx <- fromJSON("http://www.wdzj.com/wdzj/html/json/dangan_search.json")
dangan <- paste("http://www.wdzj.com/dangan/",xx$platPin,sep="")
for(i in 1:3239){

doc<-htmlTreeParse(dangan[i],useInternalNodes=T,encoding="UTF-8")
xpath_name <-"//ul[@class='recordList']//span[@class='name']"
name <- xpathSApply(doc,xpath_name,xmlValue)
xpath_con <- "//ul[@class='recordList']/li"
con <- xpathSApply(doc,xpath_con,xmlValue,recursive=T)
con <- gsub("\t|\r|\n","",con)
xingzhi_path <- "//div[@class='rTags']/span[@class='tag tag2']"
xingzhi <- xpathSApply(doc,xingzhi_path,xmlValue,recursive=T)
xingzhi <- ifelse(length(xingzhi)==0,NA,xingzhi)
con[1] <-substring(con[1],str_locate(con[1], "平均收益")[1])
con <- gsub(" ","",con)
con <- substring(con,5)
con<- c(con,xx$platPin[i],xingzhi)
df2 <- data.frame(t(con))
names(df2) <- c(name,"compname","xingzhi")
write.csv(df2,paste(i,xx$platPin[i],".csv",sep=""),row.names=F)
print(i)
Sys.sleep(0.2)
}
