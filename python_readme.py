#获取工作目录
os.getcwd()
#改变工作目录
os.chdir(path)
#创建目录
os.mkdir("file")
#复制文件
shutil.copyfile("oldfile","newfile")  #oldfile和newfile都只能是文件
shutil.copy("oldfile","newfile")   #oldfile只能是文件夹，newfile可以是文件，也可以是目标目录
#复制文件夹
shutil.copytree("olddir","newdir")   #olddir和newdir都只能是目录，且newdir必须不存在
#重命名文件（目录）
os.rename("oldname","newname") #文件或目录都是使用这条命令
#移动文件（目录）
shutil.move("oldpos","newpos")   
#删除文件
os.remove("file")
#删除目录
os.rmdir("dir")    #只能删除空目录
shutil.rmtree("dir")  #空目录、有内容的目录都可以删 

#dataframe操作
import pandas as pd
a=pd.DataFrame(data)
a.head(6)
a.describe()
a.T  ###转置
a.sort_index(axis=1,asceding=True)
a['x']
a[0:3]
a.loc[:,['a','b']] ##通过标签来索引
a.iloc[1:3,1:6]  ###通过位置来索引
a.iloc[1:2] 
a.iloc[[0,2,3],[1,2,7]]  ##选取任意的行和列
a.loc[:,1:3]=7  ##直接赋值或者改变内容
#数据导出至excel
writer = pd.ExcelWriter('test.xlsx','openpyxl') #要写入的路径
df.to_excel(writer,sheet_name='Sheet1') #Sheet1
df2.to_excel(writer,sheet_name='Sheet2') #Sheet2
writer.save()
writer.close()

#python获取数据类型
type()

#查看包的说明
help(time)                        # 很详细的模块文档
help(time.localtime())            # 很详细的函数文档
help(range)                       # 很详细的类的文档
print(time.__doc__)               # 较详细的模块文档
print(time.localtime().__doc__)   # 较详细的函数文档
print(range.__doc__)              # 较详细的类的文档
print(dir(time))                  # 简略的模块函数显示
print(dir(time.localtime()))      # 简略的函数参数显示
print(dir(range))                 # 简略的类构造函数参数显示

#设置plt.plot x刻度
x=range(2,20,2)
plt.xticks(x)
plt.xlim(0,20)

#频数直方图的bins设置
bins = range(min(data), max(data) + bins_interval - 1, bins_interval)

#字符串拼接
url="http://www.win4000.com/wallpaper_detail_160877_%d_"+str(num)+".html"

#设置纵坐标表现格式为百分比
def to_percent(temp, position):
    return '%1.1f'%(1000*temp) + '%'
plt.gca().yaxis.set_major_formatter(FuncFormatter(to_percent))

# %s：代表string 字符串
print ("today is %s" %“星期二”)
# %d：代表int 整型
num=123456
print("out=%d " %(num))
# %f  代表浮点
import math
num=123456
print("out=%.2f" %(num))
print("out=%10f" %(math.pi))
# %r  万能统配符
num=0.123456
print("out=%.4r" %(num))

#if name == 'main':的作用就是控制这两种情况执行代码的过程，在if name == 'main':下的代码只有在第一种情况下（即文件作为脚本直接执行）才会被执行，而import到其他脚本中是不会被执行
if __name__ == '__main__'：
    main（）

# "+="用法
# 1.相加，然后返回值给前一个变量
eg:
>>> a=1
>>> b=2
>>> a+=b
>>> a
3
# 2.字符串连接
>>> a='1'
>>> b='2'
>>> a+=b
>>> a
'12'

#for循环可以打印出list的内容
for x in list1:
    print(x)
    print(x,end='\n')
#多层List
for i in data:
    for j in i['cities']:
        print(j['cityName'])

# 下划线
_var #单个下划线是一个Python命名约定，表示这个名称是供内部使用的。 它通常不由Python解释器强制执行，仅仅作为一种对程序员的提示
var_ #单个末尾下划线（后缀）是一个约定，用来避免与Python关键字产生命名冲突
__var #双下划线前缀会导致Python解释器重写属性名称，以避免子类中的命名冲突
__var__ #表示python语言定义的特殊方法。避免在你自己的属性中使用这种命名方案
_ #临时或无意义变量的名称，表示REPL中最近一个表达式的结果

#装饰嵌套函数
#*args 和 **kwargs 作为装饰器内部嵌套函数的参数，*args 和 **kwargs 表示接受任意数量和类型的参数
def funA(fn):
    # 定义一个嵌套函数
    def say(*args,**kwargs):
        fn(*args,**kwargs)
    return say
@funA
def funB(arc):
    print("C语言中文网：",arc)
@funA
def other_funB(name,arc):
    print(name,arc)
funB("http://c.biancheng.net")
other_funB("Python教程：","http://c.biancheng.net/python")
#运行结果为：
C语言中文网： http://c.biancheng.net
Python教程： http://c.biancheng.net/python