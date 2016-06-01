url <- "http://shuju.wdzj.com/plat-info-60.html"
# 提取汉字首字母
#python资源：
http://stackoverflow.com/questions/3571480/converting-chinese-to-pinyin
# 怎么样定位网址，问题很好
<a href="javascript:select_list_sh(2);">2</a>

# library(rjson)
# library(RJSONIO)
# # install.packages("RJSONIO")

# getwd()
# xx <- fromJSON("dangan_search.json")

# install.packages("jsonlite")
# install.packages("XML")
# install.packages("stringr")
library(jsonlite) # 这个没有问题
library(XML)
library(stringr)


xx <- fromJSON("http://www.wdzj.com/wdzj/html/json/dangan_search.json")
head(xx)

# 这个是要读取的东西
dangan <- paste("http://www.wdzj.com/dangan/",xx$platPin,sep="")

shell.exec(dangan[8]) 
# x <- xx$platPin[11]
nrow(xx)
setwd("F:/Desktop/网贷之家数据/file2")
for(i in 3164:3239){

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
nrow(xx)
# rbind 合并所有列，其余列用NA补充
rbind.all.columns <- function(x, y) {
# x <- f1
# y <- f2
# t(x)
# t(y)
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    common_name <- union(colnames(x), colnames(y))
    x[, c(as.character(y.diff))] <- NA
 
    y[,c(as.character(x.diff))] <- NA
    x <- x[common_name]
	y <- y[common_name]
    return(rbind(x,y))
}
# ?rbind
filename <- list.files(pattern=".csv$")
head(filename)
flist <- lapply(filename,read.csv,na.strings=c(" ","-"))
library(plyr)
pingtai2 <- do.call("rbind.all.columns",flist)

write.csv(pingtai2,"pingtai2.csv",row.names=F)
head(pingtai2)
pingtai <- do.call("rbind.fill",flist)
head(pingtai)
nrow(pingtai)
write.csv(pingtai,"pingtai_new.csv",row.names=F)
# 

# tuoguan <- unique(pingtai$资金托管)
# tuoguan_s <- strsplit(tuoguan,)
tuoguan_s <- strsplit(pingtai$资金托管,"、")
user_cp <- sapply(tuoguan_s, function(x) x[grep("用户资金托管",x)[1]])
risk_cp <- sapply(tuoguan_s, function(x) x[grep("风险准备金托管",x)[1]])
user_capital <- substring(user_cp,str_locate(user_cp, "为")[,1]+1,)
risk_capital <- substring(risk_cp,str_locate(risk_cp, "为")[,1]+1)

if_own_index <- setdiff(grep("用户资金托管",pingtai$资金托管),which(!is.na(user_capital)))
user_capital[if_own_index] <- "有"
if_risk_index <- setdiff(grep("风险准备金托管",pingtai$资金托管),which(!is.na(risk_capital)))
risk_capital[if_risk_index] <- "有"
pingtai$risk_capital <- risk_capital
pingtai$user_capital <- user_capital
if_tuoguan <- rep(1,length(pingtai$资金托管))
if_tuoguan[grep("无托管",pingtai$资金托管)] <- 0
pingtai$if_tuoguan <- if_tuoguan
pingtai$xingzhi[grep("民营系",pingtai$xingzhi)] <- "民营系"
pingtai$xingzhi[grep("上市公司系",pingtai$xingzhi)] <- "上市公司系"
pingtai$xingzhi[grep("国资系",pingtai$xingzhi)] <- "国资系"
pingtai$xingzhi[grep("银行系",pingtai$xingzhi)] <- "银行系"

table(pingtai$xingzhi)
head(pingtai)
# 现在与跑路的平台合并
names(xx)[3] <- "compname"
head(xx)
pingtai <- pingtai[!duplicated(pingtai),]
xx <- xx[!duplicated(xx),]
xx[xx$compname%in% rpname,]
nrow(xx)
## 加入了中文名字
p1 <- merge(pingtai,xx,by="compname",all=TRUE)
# write.csv(p1,"p1.csv",row.names=F)
# read p1
p1 <- read.csv("p1.csv")
head(p1)
## 读取问题平台
setwd("F:/Desktop/网贷之家数据")
problem_platform <- read.csv("problem_platform.csv",na.strings=c("-"," "))
head(problem_platform)
sum(is.na(match(problem_platform$compname,p1$platName)))
p1$platName[duplicated(p1$platName)]
# 问题平台，查找不到资料了

write.csv(setdiff(problem_platform$compname,p1$platName),"pname_no_file.csv",row.names=F)

wenti <-vector(nrow(p1))
wenti[match(problem_platform$compname,p1$platName)] <-1
wenti[which(is.na(match(problem_platform$compname,p1$platName)))] <- NA

# length(problem_platform$compname)
table(wenti)
# use match function
# A=c(1:3,"a")
# B=c(3,1,"a",4)
# match(B,A)
names(problem_platform)[1] <- "platName"

pro_data <- merge(p1,problem_platform,by="platName",all=TRUE)
library(dplyr)
pname <- filter(pro_data,is.na(compname)) %>% select(platName)
178+3240
p1$platName
?setdiff
p2 <- setdiff(problem_platform$platName,p1$platName)
setdiff(p2,pname$platName)
# 找到一个重复值
pname[duplicated(pname$platName),]
filter(pro_data,platName=="祥富春投资")
problem_platform$事件类型 <- gsub("，",",",problem_platform$事件类型)
table(problem_platform$事件类型)

study_data <- filter(pro_data,!is.na(compname))
names(study_data)[26] <- "event"
study_data <- mutate(study_data,event=as.character(event)) 
study_data$event[is.na(study_data$event)] <- 1
write.csv(study_data,"study_data.csv",row.names=F)
# 
if_pro <- rep(0,length(study_data$event))
if_pro[study_data$event!=1] <- 1
## 性质问题
table(study_data$xingzhi,if_pro)
# 有无托管
# 无托管，第三方支付机构托管，银行托管
# 仅看有无托管
table(study_data$if_tuoguan,if_pro)
# 平均收益
names(study_data)[3] <- "avg_revenue"
write.csv(study_data,"study_data2.csv",row.names=F)
avg_revenue <- study_data$avg_revenue %>% gsub("%","",.) %>% as.numeric() 
summary(avg_revenue)
plot(avg_revenue~if_pro)
boxplot(avg_revenue~if_pro,xlab="平均利率(%)",yaxt="n",horizontal=T)
text(x=60,y=1:2,c("正常平台","问题平台"))

boxplot(avg_revenue~if_pro,ylab="平均利率(%)",xaxt="n",horizontal=F)
axis(1,1:2,c("正常平台","问题平台"))
?boxplot
abline(h=tapply(avg_revenue,if_pro,mean,na.rm=T)[2])
xlab=c("问题平台","正常平台")
?boxplot
nrow(pro_data)
head(pro_data)
which(is.na(pro_data$online_tine))
pro_data[is.na(pro_data$compname),]
head(pro_data)
nrow(p1)
nrow(pingtai)
setdiff(pingtai$compname,xx$compname)
setdiff(pingtai$compname,unique(pingtai$compname))
rpname <- pingtai$compname[duplicated(pingtai$compname)]
sum(duplicated(pingtai))
length(rpname)
pingtai[pingtai$compname %in% rpname,]
# sum(is.na(pingtai$xingzhi))
write.csv(pingtai,"pingtai3.csv",row.names=F)
# length(if_tuoguan)
# pingtai[2,]
# pingtai[if_own_index,]
# user_capital <- rep(0,nrow(pingtai))
# user_capital[grep(("用户资金托管",pingtai$资金托管)] <-1
# risk_capital <- rep(0,nrow(pingtai))
# grep("用户资金托管",tuoguan)
# |"风险准备金托管"
# 无托管
# length(tuoguan)
# tuoguan
# rbind 合并共和有列
# common.names <- intersect(colnames(database.one), colnames(database.two))
# combined.database <- rbind(database.one[, common.names], database.two[, common.names])

# head(filename)
options(stringsAsFactors=F)
# f1 <- read.csv(filename[1])
# f2 <- read.csv(filename[2])
# ncol(f1)
# ncol(f2)
# flist <- lapply(filename,read.csv)
# which(sapply(flist,ncol)!=12)
# rbind.all.columns(database.one, database.two)
# df1 <- data.frame(A=1:3,B=2:4)
# df2 <- data.frame(B=1:3,C=5:7)
# rbind.all.columns(df1,df2)
# rbind.fill(df1,df2)