# 第1部分 语言篇
## 第一章 程序设计入门
### 算术表达式

``` c
    printf("%.1lf\n", 8/5); --> 1.6
    printf("%d\n", 8.0/5.0); --> -1717986918
```
### 变量及其输入
尽量用const声明常数：const double pi = 4.0 * atan(1, 0);  
不要在竞赛中使用conio.h、getch()、getche()、gotoxy()、clrscr()  
printf("%3d", 25); --> 空格25  
printf("%03d", 25); --> 025  
printf("%-3d", 25); --> 25空格  
a和b互换：a^=b^=a^=b;  

## 第二章 循环结构程序设计
### for循环
判断浮点型m是否为整数：floor(m+0.5)==m  
### while循环和do-while循环
获得程序运行伊始至当前时间：printf("%.2lf", (double)clock()/CLOCK_PER_SEC);    // time.h  
管道技巧：echo inputValues | programName  
### 循环的代价
#### scanf返回值
不确定输入变量个数-->while(scanf("%d", &a)==1){}  
windows下结束输入：ctrl+z， 再enter  
linux下：ctrl+d  
#### 文件操作
#### 文件比较
fc（windows)/ diff(linux)
#### 使用文件
#####最简单
输入输出重定向。但不能同时读写文件和标准输入输出  
1. 在main函数入口处加上：

``` c
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);
```
2. 提交只需删除LOCAL。当然最好把LOCAL定义在编译选项中，则无需修改提交。

``` c
    #define LOCAL

    // in main
    #ifdef LOCAL    // 只在定义了LOCAL才编译
      fre...
      fre...
    #endif
```
##### fopen：稍繁琐，但灵活  
1.

``` c
    FILE *fin, *fout;
    fin = fopen("data.in", "rb");
    fout = fopen("data.out", "wb");
    ...
    while(fscanf(fin, "%d", &x)==1){}
    ...
    fprintf(fout, "%d %d\n", min, max);
    fclose(fin);
    fclose(fout);
```
2. 改写fopen程序成标准输入输出：  
    只需赋值：fin=stdin; fout=stdout;即可。
### 算法竞赛中的输入输出框架
1、浮点数比较陷阱：if(f != 10){}
2、64位整数
  a、int范围是-2^31~2^31-1，比-2\*10^9~2\*10^9略宽
     long long: -2^63~2^63-1，比-10^19~10^19略窄
  b、输出：
      linux下：统一%lld
      windows下：除vs2008是%lld，其余%I64d
3、C++输入输出
  a、
    #include<fstream>
    using namespace std;
    ifstream fin("aplusb.in");
    ofstream fout("aplusb.out");
    ...
  b、想再次使用cin和cout只需,
    // 删除fin、fout声明
    #define fin cin
    #define fout cout
4、标准输入流cin比文件流fin慢很多很多（与效率和操作系统相关）
### 练习
1、printf特殊用法

``` c
    char ch[20]; 
    printf("%*.*s\n",m,n,ch); //前边的*定义的是总的宽度，后边的定义的是输出的个数
```

## 第三章 数组和字符串
### 数组
1、超大的数组只能开在main函数外
2、数组不能直接赋值操作，除非：

``` c
    memcpy(b, a, sizeof(int)*k); // 从a复制k个元素到b    // string.h
    memcpy(b, a, sizeof(a));    // 复制整个a
```
3、memset(a, 0, sizeof(a));
### 字符数组
# TODOOOO
1、字符——特殊整数
  '\\'——反斜线
  '\''——单引号
  '\"'——双引号
2、scanf("%s", S);    // 不加&
3、

``` c
  int count=0;
  printf("%d %d %d\n", count++, count++, count++);    // 从右往左！
  printf("%d\n", count);
  printf("%d\n", count = count++);    // ++不执行！
  printf("%d\n", count);
  执行结果：2 1 0
            3
            3
```
4、

``` c
  char s[20], buf[99];
  sprintf(buf, "%d%d%d%d", a, b, c, d);    // abcd依次存入buf字符串
  strchr(s, buf[i]) == NULL    // buf[i]未在s字符串中出现
```
5、读取下一个字符：文件：fgetc(fin)/ 标准输入输出：getchar()
   陷阱：回车换行符：windows是'\r'和'\n'，linux是'\n'， macos是'\r'
   在window下读取windows文件时，它们会忽略'\r'，只剩下'\n'
   在linux下读取同一文件时，它们会逐一读取
6、文件：fgets(buf, MAXN, fin);    // 读取完整一行，末尾加上'\n'。遇到\n，读取终止。
   标准输入输出：gets(s);    // 不建议使用，它不管s的可用空间，可能造成缓冲区溢出漏洞
### 3.4
1、用Ascii编码表示字符
  用八进制：\o、\oo、\ooo
  用十六进制：\xh    // h为十六进制数字串
  printf("%d %o %x\n", a);
2、P47 3.4.6



## 第四章 函数和递归
## 第五章 基础题目选解
# 第2部分 算法篇
## 第六章 数据结构基础
## 第七章 暴力求解法
## 第八章 高效算法设计
## 第3部分 竞赛篇
## 第九章 动态规划初步
## 第十章 数学概念和方法
## 第十一章图论模型与算法

## 附录A 开发环境与方法
### 命令行
#### 文件系统
#### 进程
##### windows下
tasklist
ps不会显示系统进程，ps ax可以列出更多进程。
终止进程：taskkill/pid <PID> 或者 tasklill/im <映像名>
taskkill/?查看更多
##### under linux
kill command
killall <process_name>: to kill all processes relate to the process_name

作为一个好习惯，当程序非正常终止，或者系统表现异常时，应检查进程。 例如，若系
统反应特别慢，可能是有一些看似运行结束，但其实残留在系统中继续占用系统资源的进
程。

#### 程序的执行
#### 重定向和管道
可以使用重定向的技巧将输入文件塞到程序的标准输入中，然后再将程序输出保存在文件中.  
在Windows下可以使用abc < abc.in > abc.out。 而在Linux下则可以使用./abc < abc.in > abc.out。  
">"将覆盖原文件，若只想附加可用">>"。  

此外，如果有大量的文本输出到标准错误输出，还可以用“2>”将它们重定向，但需
注意，尽量不要在正式提交的程序中输出到标准错误输出，这样不仅可能会违反比赛规定，
还可能会因为大量文本的输出而占用宝贵的CPU资源，甚至导致超时。  

如若计算(10+20)^2： echo 10 20 | aplusb | sqr。 尽管也可以用 重定向来完成这个任务，但用管道明显要简单得多。

另一个常见用法是分页显示一个文本文件的内容。 在Windows下可以用type abc.txt | more，在Linux下则是用cat abc.txt | more。  

在Linux中，可以用time命令计时。 例如，运行time ./abc会执行abc并输出运行时间。 但
Windows中并没有这样的命令，幸好在大多数情况下只是在对自己编写的程序计时，因此只
需在程序的最后打印出clock()/(double)CLOCKS_PER_SEC即可(需要包含time.h)。  

#### 常用命令
                        Linux命令         Windows命令
改变/创建 删除目录      cd/mkdir/rmdir    cd/md/rd
显示文件内容            cat/more          type/more
比较文件内容            diff              fc
修改文件属性            chmod             attrib
复制文件                cp                copy/xcopy
删除文件                rm                del
文件改名                mv                ren
回显                    echo              echo
关闭命令行              exit              exit
在文件中查找字符串      grep              find
查看/修改环境变量       set               set
帮助                    man <command>     help <command>

### 操作系统脚本编程入门
不停地随机生成测试数据
#### windows下批处理
``` bat
    @echo off                       ; 表示命令本身并不回显
    :again
    r > input                       ; 生成随机输入
    a < input > output.a
    b < input > output.b
    fc output.a output.b > nul      ; 比较文件,因为输出我们不关心，所以输出到nul设备,它好比黑洞。。
    if not errorlevel 1 goto again  ; 相同时继续循环
```
if errorlevel num的意思是“如果返回码大于或者等于num”。
因此if not errorlevel 1的意思是，“如果返回码小于1”。 事实上，当且仅当文件相同时，fc程序 返回0。
如果不确定程序的返回码是多少，可以在程序执行完毕后用echo %errorlevel%命令输出返回码。  

上面的程序应以.bat为扩展名保存，并且在执行时也可以省略扩展名。 如果同时存在
abc.bat和abc.exe，将执行abc.exe。 但如果主文件名和系统命令重名，则连exe文件也无法执
行，如path.exe。

#### Linux下Bash脚本
``` shell
    #!/bin/bash
    while true; do
      ./r > input                      # 生成随机数据
      ./a < input > output.a
      ./b < input > output.b
      diff output.a output.b           # 文件比较
      if [ $? -ne 0 ] ; then break; fi # 判断返回值
    done
```
需要注意的是，如果在Windows下编写Linux脚本，复制到Linux后需要去掉所有的\r字符，否则解释器会报错。

把上述程序保存成test.sh后，再执行chmod +x test.sh，即可用./test.sh来执行它。 当然，扩
展名也不是必需的，完全可以以不带扩展名的test命名。  

上面的“true”和“[”都是程序。 前者的作用是直接返回0；而后者的作用是计算表达式(该
程序要求最后一个参数必须是“]”)，其中“$?”是bash内部变量，表示“上一个程序的返回码”。

#### 再谈随机数
如果做过测试，可能会发现上面的方法有一个问题：如果程序执行太快，随机数生成器
在相邻两次执行时，time(NULL)函数返回值相同，因而产生出完全相同的输入文件。 换句话
说，每隔一秒才能产生出一个不同的随机数据。 一个解决方案是利用系统自带的随机数发生
器：在Windows下是环境变量%random%，而在bash中是$RANDOM。 它们都是0～32767之间
的随机整数。 可以直接用脚本编写随机数生成器，也可以把它们传递到程序中。  

### 编译器和调试器
#### gcc的安装和调试
Linux在安装系统时即可选择安装gcc，g++，binutils等包。
windows下推荐MinGW环境下的gcc，好处是和linux下的gcc一致性较好。
安装完成后，在命令行中执行gcc命令。 如果显示gcc: no input files，则安装成功；如果
提示不存在这个命令，可能是因为没有把gcc所在目录加到搜索路径中。 

#### 常见编译选项
``` c
    #include<stdio.h>
    main()
    {
      int a, b;
      scanf("%d%d", &a, &b);
      int c = a+b;
      printf("%d%d\n", c);
    }
```
编译： gcc test.c
检查目录：ls/dir，多了a.out/a.exe
gcc test.c -0 test -> test.exe/test -> test/./test运行

编译选项：gcc -test.c -o test -Wall
这次，编 译器指出了3个警告：main函数没有返回类型、 没有返回值、 printf的格式字符串可能有问 题。
还可以进一步用-ansi-pedantic，它会检查代码是否符合ANSI标准(-ansi只是判断是否和 ANSI冲突，而-pedantic更加严格)。
它进一步指出了上述代码中的另外一个问题：ANSI C中不允许临时声明变量，而必须在语句块的首部声明变量。

在C语言中，另一个常用的编译选项是-lm，它让编译器连接数学库，从而允许程序使用
math.h中的数学函数。 C++编译器会自动连接数学库，但如果程序的扩展名是.c，且不连接数
学库，有时会出现意想不到的结果。  
另一个有用的选项是-DDEBUG，它在编译时定义符号DEBUG（可以换成其他，如-
DLOCAL将定义符号LOCAL），这样，位于#ifdef DEBUG和#endif中间的语句会被编译。 而
在通常情况下，这些语句将被编译器忽略（注意，不仅是不会执行，连编译都没有进行）。  

可以用-O1、 -O2和-O3对代码进行速度优化。 一般情况下，直接编译出的程序比用-O1
编译出的程序慢，而后者比-O2慢。 尽管理论上-O3编译出的程序更快，但由于某些优化可能
会误解程序员的意思，一般比赛中不推荐使用。  

另外，如果你的程序中有一些不确定因素 (如使用了未初始化的变量)，运行结果可能会和
编译选项有关——用-O1和-O2编译出的程序 也许不仅是速度有差异，答案甚至都有可能不同！
当然，这种情况出现的前提是程序有瑕 疵。 如果是一个规范的程序，运行结果不会和优化方式有关。  

#### gdb简介
gdb尽管只是一个文本界面的调试器，但功能十分强大。 不管是Linux和Windows下的 MinGW，gcc和gdb都是最佳拍档。  
gdb的使用方法很简单——用gcc编译成test.exe之后，执行gdb test.exe即可。
不过，如果 要用gdb调试，编译时应加上-g选项，生成调试用的符号表。  

接下来使用l命令，将看到部分源程序清单。 如果用l 15，将会显示第15行（以及它前后
的若干行）。 除此之外，还可以用函数名来定义，如l main将显示main函数开头的附近10
行。 如果不加参数执行l，将显示下10行；list -将显示上10行。 所有这些操作都可以用help
list命令来查看。 gdb中的命令可以简写（例如list简写成l），大家可以多尝试（提示：试一
下命令的前若干个字母） 

运行程序的命令是r(run)，但会一直执行到程序结束。 如何让它停下来呢？方法是用
b(break)命令设置断点。 例如，b main命令将在main函数的开始处设置一个断点，则用r命令
执行时会在这里停下来。 如果想继续运行，请用c(continue)命令，而不是继续用r命令。 和list
命令类似，b命令既可以指定行号，也可以在指定函数的首部停下来。 笔者在调试很多程序
时都是以命令b main和r开头的。  

如果希望逐条语句地执行程序，不停地用b和c命令太麻烦。 gdb提供了一些更加方便的
指令，其中最常用的有两个：next(简写为n)和step(简写为s)。 其作用都是执行当前行，区别
在于如果当前行涉及函数调用，则next是把它作为一个整体执行完毕，而step是进入函数内
部。 尽管n和s都只有一个字母，但有时还是稍显繁琐。 在gdb中，如果在提示符下直接按
Enter键，等价于再次执行上一条指令，因此如果需要连续执行s或者n，只需要第一次输入该
命令，然后直接连按Enter键即可。 另外，和命令行一样，可以按上下箭头来使用历史记录。  

另一个常用命令是until（简写为u），让程序执行到指定位置。 例如，u 9就是执行到第9
行，u doit就是执行到doit函数的开头位置。  

停下来以后便打印一些函数值，看看是否和想象的一致。 用p(print)命令可以打印出一些
变量的值，而info locals(可以简写为i lo)可以显示所有局部变量。 如果希望每次程序停下
来，则可以用display(简写为disp)命令。 例如，display i+1就可以方便地读取i+1的值。 它往往
和n、 s和u等单步执行指令配合使用。 如果需要列出所有display，可以用info display(简写为i
    disp)；还可以删除或者临时禁止/恢复一些display，相应的命令为delete display(d disp)、
disable display(dis disp)和enable display(en disp)。 类似地，也可以根据断点编号删除、 禁止
和恢复断点，还可以用clear(cl)命令，像b命令一样根据行号或者函数名直接删除断点。  

#### gdb的高级功能
gdb的功能远不止刚才所讲述的那些。 尽管很多功能是专为系统级调试所设，但还有很
多功能也能为算法程序的调试带来很大方便。  
首先是栈帧的相关命令，其中最常用的是bt，其他命令可以通过help stack来学习。 接下
来是断点控制命令。 commands(comm)命令可以指定在某个断点处停下来后所执行的gdb命
令，ignore(ig)命令可以让断点在前count次到达时都不停下来，而condition则可以给断点加一个条件。
例如，在下面的循环中：  

``` c
    10 for(i = 0; i < n; i++)
    11 printf("%d\n", i);
```
首先用b 11设置断点(假设编号为2)，然后用cond 2 i＝＝5让该断点仅当i＝5时有效。 这
样的条件断点在进行细致的调试时往往很有用。  
另外，gdb还支持一种特殊的断点——watchpoint。 例如，watch a（简写为wa a）可以在
变量a修改时停下，并显示出修改前后的变量值，而awatch a（简写为aw a）则是在变量被读
写时都会停下来。 类似地，rwatch a(rw a)则是在变量被读时停下。  
最后需要说明的是，gdb中可以自由调用函数（不管是源程序中新定义的函数还是库函
数）。 第一种方法是用call命令。 例如，如果想给包含10个元素的数组a排序，可以像这样直
接调用STL中的排序函数call sort(a, a+10)。  
遗憾的是，如果真的做过这个实验，会发现刚才所说完全是骗人的。 gdb会显示不存在
函数sort。 怎么会这样呢？如果学过宏和内联函数就会知道，很多看起来是函数的却不一定
真的是函数，或者说，不一定是调试器识别的函数。 为了在gdb中调用sort，可以将它打包：  

``` c
  void mysort(int*p, int*q)
  {
    sort(p, q);
  }
```
这样，就可以用call mysort(a, a+10)来给数组a排序了。print、 condition和display命令都可
以像这样使用C/C++函数。 例如，可以用p rand()来输出一个随机数，或是专门编写一个打印
二叉树的函数，然后在print或者display命令中使用它，还可以编写一个返回bool值的函数，
并作为断点的条件。 

注意，过分地依赖于gdb的调试功能让敏锐的直觉变得迟
钝。 事实上，笔者建议读者尽量只使用A.3.3节提到的基本功能，甚至尽量不要使用gdb——
用输出中间变量的方法，加上直觉和经验来调试算法程序。 如果是这样，编程速度和准确性
将大大提高。
### IDE
