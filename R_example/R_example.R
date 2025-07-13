
# C:\\Users\\Administrator\\Desktop\\metadata\\
# F:\\metadata\\

setwd("/Users/cathy/Documents/学习相关/大三下/循证医学/Meta_analysis/实例_R代码")

# csv file
data1<-read.csv("Data 1.csv")

# install.packages("readxl")
# install.packages("XQuartz")
library("readxl")
library(XQuartz)

# xls files
my_data0 <- read_excel("Data 1.xls")
# as.data.frame(my_data0)

head(my_data0)
str(my_data0)

# xlsx files
my_data1 <- read_excel("Data 1.xlsx")
str(my_data1)
head(my_data1)

# 自己选择文件
my_data2 <- read.csv("Data 2.csv")

# 修改数据
my_data3<-edit(my_data0)


# 二分类数据，实例数据

# install.packages("meta")
library(meta) 

met1<-metabin(
  event.e=N_hip_E,
  n.e=Total_E,
  event.c=N_hip_C,
  n.c=Total_C,
  studlab=Source,
  data =my_data0, 
  sm ="RR"
)
met1
summary(met1) 
forest(met1)

# 连续数据
# library(xlsx)
library(readxl)
data2<-read_excel(path ="/Users/cathy/Documents/学习相关/大三下/循证医学/Meta_analysis/实例_R代码/Data 2.xlsx",sheet =1)
data2 <- read.csv("Data 2.csv")
met2<-metacont( n.e=N_E,
                mean.e=Mean_E,
                sd.e=SD_E,
                n.c=N_C,
                mean.c=Mean_C,
                sd.c=SD_C,
                data=data2,
                common=TRUE,random=TRUE,
                studlab= Studies)
summary(met2)
forest(met2)

# HR with CI
data3<-read_excel(path ="example HR.xlsx",sheet =1)
met3<-metagen(TE= log(HR), 
              lower = log(lower.HR), 
              upper = log(upper.HR),
              data=data3,
              studlab = paste(Author, Year), 
              sm = "HR")
summary(met3)
forest(met3)
forest(met3,leftcols = c("studlab"),colgap.forest.left="30mm")

met3<-metagen(TE=HR, 
              lower = lower.HR, 
              upper = upper.HR,
              data=data3,
              transf = FALSE,
              studlab = paste(Author, Year), 
              sm = "HR")
summary(met3)
forest(met3)
forest(met3,leftcols = c("studlab"),colgap.forest.left="30mm")

# MD with SE
data4<-read_excel(path ="example MD.xlsx",sheet =1)
met4<-metagen(TE=MD, 
              seTE=seMD,
              data=data4,
              studlab = Studies, 
              sm = "MD")
summary(met4)
forest(met4)
forest(met4,leftcols = c("studlab"),colgap.forest.left="30mm")




# 亚组分析、meta回归
data1<-read.csv("Data 1.csv")

met1<-metabin(event.e=N_hip_E,
              n.e=Total_E,
              event.c=N_hip_C,
              n.c=Total_C,
              data=data1, 
              sm="RR",
              studlab= paste(Source, Year))
summary(met1) 

# 亚组分析
forest(update(met1,subgroup = Vitamin_D,subgroup.name = "Dose"))



# 练习1 data2中不同药物的亚组分析
data2<-read_excel(path ="C:\\Users\\Administrator\\Desktop\\metadata\\R\\Data 2.xlsx",sheet =1)

met2sub<-metacont( n.e=N_E,
                mean.e=Mean_E,
                sd.e=SD_E,
                n.c=N_C,
                mean.c=Mean_C,
                sd.c=SD_C,
                subgroup = Drug, subgroup.name = "Drug",
                data=data2, 
                studlab= Studies)
summary(met2sub) 
forest(met2sub) 

# meta regression
# data1<-data1[order(data1$Level),]
met1<-metabin(N_all_E,
              Total_E,
              N_all_C,
              Total_C,
              data=data1,
              studlab=paste(Source, Year))

mrg<-metareg(met1,~Level)
plot(mrg)
bubble(mrg)
forest(mrg,annotate=TRUE, addfit=TRUE, addcred=TRUE)


# 练习2
dataadd<-read_excel(file.choose())

metadd<-metabin(event.e=TB_E,
                n.e=Total_E,
                event.c=TB_C,
                n.c=Total_C,
                data=dataadd, 
                sm="RR",   
                studlab= paste(Study, Year))
mrgadd<-metareg(metadd,~Latitude)
summary(mrgadd)
plot(mrgadd)
bubble(mrgadd)
forest(mrgadd,annotate=TRUE, addfit=TRUE, addcred=TRUE)


# 发表偏移
funnel(met1) 

Begg<-metabias(met1,k.min=5,method.bias = "rank")
Egger<-metabias(met1,k.min=5,method.bias = "linreg")

tr<-trimfill(met1)
funnel(tr)


# 敏感分析
metinf<-metainf(met1,pooled="random")
forest(metinf)
metcum<-metacum(met1, sortvar = Year)
forest(metcum)


# 导出图
tiff(file = "C:\\Users\\Administrator\\Desktop\\metadata\\R\\Forest1.tif",width = 3300,height = 1000,res = 300,compression ="lzw")
forest(met1,label.e="Vitamin D",label.c="Placebo")
dev.off()


# 合并meta图
met1a<-metabin(N_hip_E , Total_E , N_hip_C , Total_C,
             data=data1,
             common=TRUE,random=TRUE,
             sm="RR", method="MH",
             title="Hip Fracture",
             allstudies=TRUE,
             studlab= paste(Source, Year))
summary(met1a)
met1b<-metabin(N_all_E , Total_E , N_all_C , Total_C,
             data=data1,
             common=TRUE,random=TRUE,
             sm="RR", method="MH",
             title="Hip Fracture",
             allstudies=TRUE,
             studlab= paste(Source, Year))
summary(met1b)
forest(metabind(met1a,met1b,name=c("Hip Fracture","All Fracture")))

  
 