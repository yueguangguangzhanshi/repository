#方差齐性检验包
bartlett.test(length ~ site,data = dfCRp)
#Fligner-Killeen
fligner.test()
#Brown-Forsythe检验(HH包中的函数)
hov()
#R的程序包car中提供了Levene检验的函数
levene.test()

#检验正态分布包
#通常用于样本容量n≤50时，检验样本是否符合正态分布。
shapiro.test(dfCRp$length)

#统计非缺失值的数量
#通过Hmisc包中的函数计算描述性统计量，可返回变量和观测的数量、缺失值和唯一值的数目
describe()
#通过psych包计算描述性统计量，可以计算非缺失值的数量。
describe()
#通过pastecs包中的函数计算描述性统计量，计算其中所有值、空值、缺失值的数量。
stat.desc()

#国内源列表
https://mirrors.tuna.tsinghua.edu.cn/CRAN/ 	TUNA Team, Tsinghua University
http://mirrors.tuna.tsinghua.edu.cn/CRAN/ 	TUNA Team, Tsinghua University
https://mirrors.ustc.edu.cn/CRAN/ 	University of Science and Technology of China
http://mirrors.ustc.edu.cn/CRAN/ 	University of Science and Technology of China
https://mirror-hk.koddos.net/CRAN/ 	KoDDoS in Hong Kong
https://mirrors.eliteu.cn/CRAN/ 	Elite Education
https://mirror.lzu.edu.cn/CRAN/ 	Lanzhou University Open Source Society
http://mirror.lzu.edu.cn/CRAN/ 	Lanzhou University Open Source Society
https://mirrors.tongji.edu.cn/CRAN/ 	Tongji University
https://mirrors.shu.edu.cn/CRAN/ 	Shanghai University

#安装本地源
install.packages("/home/liuhb/R/i686-pc-linux-gnu-library/3.4/org.Hs.eg.db_3.7.0.tar.gz", repos = NULL, type = "source")
#设置网络源
install.packages("xxxx",repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')
# 选择镜像
options(repos=structure(c(CRAN="https://cran.cnr.berkeley.edu/")))

#R语言添加Bioconductor源
utils::setRepositories(ind=1:2)

source("http://bioconductor.org/biocLite.R")
options(BioC_mirror="https://mirrors.ustc.edu.cn/bioc/")
biocLite("clusterProfiler")

if (!requireNamespace("BiocManager", quietly = TRUE))
	install.packages("BiocManager")
BiocManager::install("clusterProfiler")

#Github下载
devtools::install_github('rstudio/DT')

#导入中文表
read.xlsx2(data,sheetindex=1)


#重命名List
names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list")

#替换表格缺失值
	b[is.na(b)]<-0
	#替换其他内容
		FI[FI=='---']<-'NA'

#统计满足条件的数目
	sum()
	#统计非Na的数目
		sum(!is.na())

#查看数据库支持类型
keytypes(org.Hs.eg.db)

#查看系统变量
Sys.getenv("texlive")
Sys.which("python3")
#设置系统变量
Sys.setenv(FAME="/opt/fame")

#ggplot2系列
	#旋转坐标轴label的方向
		p+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
	#添加标题，坐标轴名称
		p+labs(x = "New x axis label", y = "New y axis label",title ="Add a title above the plot",subtitle = "Add a subtitle below title",caption = "Add a caption below plot")
	#居中标题
		p+theme(plot.title = element_text(hjust = 0.5))
	#更换图例的标题
		labs(fill="Add a title")
		b+guides(fill = guide_legend(title = NULL))
	#去掉图例中size标识
		p+guides(size=FALSE)
	#改变图中点的大小	
		p+geom_point(aes(color =xxx,fill=xxx,size=1))+scale_color_manual(values =  c("green","grey","red"))##设置颜色


#render系列
rmarkdown::render("Label_free.Rmd",output_file="Label_free.pdf",output_dir="/report_result")

py_config()

#升级R
install.packages("installr")
library(installr)
updateR() 
#升级R包
update.packages()

#去除有Na的行
na.omit(data)
#去除所有变量
rm(list = ls())

#缩小页边距
\usepackage[left=1in]{geometry}

#查看加载包
(.packages())

#保留小数点
round(123.456,digits = 1)
#保留有效数字
signif(y, n) # y为数字、n为保留的位置
#转变科学计数
format(5e+5, scientific = FALSE)

#替换数据框的内容
datanew<-data%>%mutate_each(funs(str_replace_all(., "\\*","")))  #替换*为""
#替换某列(行名)的字符
rownames(lgG)<-lapply(rownames(lgG),FUN =function(x) {
  gsub("-","_",x)
  })

#修改data.frame
fix(xxx)

# 生成序列
seq(1,10,2)

#查看版本信息
sessionInfo()

#R语言怎么将数据框中的 '多列' 字符型转换为数值型
name[,2:60]<-lapply(name[,2:60],as.numeric) 

# 返回特定条件的行
which(ratio_data1[,6]<1)

# 批量导入数据
data<-list()
fileName = dir("~/out/20191106/",pattern = '*.gpr')
data <- lapply(fileName, function(x){
  read.delim(x,skip=31)
  }
 )

#查看物种包数据类型
keytypes(org.Mm.eg.db)
#使用createKEGGdb下载物种KEGG新数据
library(createKEGGdb)
create_kegg_db('rno')

#创建文件夹
dir.create("img")

# 设置表格因子格式转化
options(stringsAsFactors=FALSE)

#分裂选取
strsplit(imgUrl, '"',fixed = T)[[1]]

# 文本文件的读写
text <- readLines("file.txt", encoding = "UTF-8") 
#默认设置，每个单词作为字符向量的一个元素
scan("file.txt", what = character(0),encoding = "UTF-8")  
#设置成每一行文本作为向量的一个元素，这类似于readLines
scan("file.txt", what = character(0), sep = "\n",encoding = "UTF-8")  

# 转义为\\

# R运行shell
system(command, intern = FALSE,
       ignore.stdout = FALSE, ignore.stderr = FALSE,
       wait = TRUE, input = NULL, show.output.on.console = TRUE,
       minimized = FALSE, invisible = TRUE)

shell(cmd, shell, flag="/c", intern=FALSE, wait=TRUE,
      translate=FALSE, mustWork=FALSE, ...)


# rmarkdown相关
##循环加入tab
```{r, echo = FALSE, results='asis'}
headers <- c("membership", "participation")
tabs <- c("age", "gender")

for (i in headers){

    cat("###", i, "{.tabset}", "\n")
    cat("\n\n\n")

    for (j in tabs) {

        cat("####", j, "\n")
        cat("\n\n\n") 
    }
}
```

#shiny相关
##datatable中的options设置
LengthMenu  #default [10, 25, 50, 100]，可以为一维数组，也可为二维数组，比如：[[10, 25, 50, -1], [10, 25, 50, "All"]]

##显示一个带链接的列表用escape = FALSE

##DT相关
### datatable中设置列宽要设置autoWidth=TRUE