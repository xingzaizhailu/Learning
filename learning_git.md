# Basic Git
## 建库：
使用git init命令初始化一个Git仓库

## 添加文件到Git仓库：
1. 使用命令git add <file>，注意，可反复多次使用，添加多个文件；
2. 使用命令git commit，完成。(-m参数添加描述信息）

## 操作
### `git status`
随时掌握工作区的状态
### `git diff`
如果git status告诉你有文件被修改过，用`git diff`查看修改内容。
### `git reset`
HEAD指向的版本就是当前版本。Git允许我们使用`git reset --hard commit_id`命令在历史版本之间穿梭。
### `git log`
穿梭前，用以查看提交历史，以便确定要回退到哪个版本。
### `git reflog`
要重返未来，用`git reflog`查看命令历史，以便确定要回到未来的哪个版本。
### `checkout -- file`
场景1：当你改乱了工作区某个文件的内容，想直接丢弃工作区的修改时，用命令`git checkout -- file`。
### `git reset HEAD file`
当你不但改乱了工作区某个文件的内容，还添加到了暂存区时，想丢弃修改  
1. 用命令`git reset HEAD file`，就回到了场景1  
2. 按场景1操作。
### `git rm`
用于删除一个文件。如果一个文件已经被提交到版本库，那么你永远不用担心误删，但是要小心，你只能恢复文件到最新版本，你会丢失最近一次提交后你修改的内容。

## 远程仓库：
### 关联远程库
	$ git remote add origin git@server-name:path/repo-name.git
### 查看远程库信息：
	$ git remote -v
### 删除远程库
	$ git remote rm origin
### 从远程抓取分支
	$ git pull
如果有冲突，要先处理冲突。
### 推送到远程库
推送master分支的所有内容  

	$ git push -u origin master
### 在本地创建和远程分支对应的分支
本地和远程分支的名称最好一致  

	$ git checkout -b branch-name origin/branch-name
此后，每次本地提交后，只要有必要，就可以使用命令`git push origin master`推送最新修改。


## 分支管理：
Git鼓励大量使用分支：  

- 查看分支：`git branch`
- 创建分支：`git branch <name>`
- 切换分支：`git checkout <name>`
- 创建+切换分支：`git checkout -b <name>`
- 合并某分支到当前分支：`git merge <name>`
- 合并分支时，加上--no-ff参数就可以用普通模式合并，合并后的历史有分支，能看出来曾经做过合并，而fast forward合并就看不出来曾经做过合并。  

		$ git merge --no-ff -m "merge with no-ff" dev
- 删除分支：`git branch -d <name>`
- 查看分支合并图：`git log --graph`  


### bug分支：
当手头工作没有完成时，先把工作现场 `git stash` 一下，然后去修复bug，修复后，再 `git stash pop` (或用 `git stash apply` 恢复，但是恢复后，stash内容并不删除，你需要用 `git stash drop` 来删除)，回到工作现场。
你可以多次stash，恢复的时候，先用 `git stash list` 查看，然后恢复指定的stash，用命令：  

	$ git stash apply stash@{0}

### feature分支：
开发一个新feature，最好新建一个分支；
如果要丢弃一个没有被合并过的分支，可以通过 `git branch -D <name>` 强行删除。

### 推送分支
推送分支，就是把该分支上的所有本地提交推送到远程库。推送时，要指定本地分支，这样，Git就会把该分支推送到远程库对应的远程分支上：

	$ git push origin master
如果要推送其他分支如 branch-name，就改成：

	$ git push origin branch-name
如果推送失败，先用 `git pull` 抓取远程的新提交。  
但是，并不是一定要把本地分支往远程推送，那么，哪些分支需要推送，哪些不需要呢？

- master分支是主分支，因此要时刻与远程同步；
- dev分支是开发分支，团队所有成员都需要在上面工作，所以也需要与远程同步；
- bug分支只用于在本地修复bug，就没必要推到远程了，除非老板要看看你每周到底修复了几个bug；
- feature分支是否推到远程，取决于你是否和你的小伙伴合作在上面开发。
	
####冲突： 
当Git无法自动合并分支时，就必须首先打开冲突文件。解决冲突后，再提交，合并完成。


## 多人协作模式：

1. 可以试图用git push origin branch-name推送自己的修改；
2. 如果推送失败，则因为远程分支比你的本地更新，需要先用git pull试图合并；
3. 如果合并有冲突，则解决冲突，并在本地提交；
4. 没有冲突或者解决掉冲突后，再用 `git push origin branch-name`推送就能成功！

如果git pull提示“no tracking information”，则说明__本地分支和远程分支的链接关系没有创建__，用命令:

	$ git branch --set-upstream branch-name origin/branch-name

## 标签：
### 新建标签: `git tag <name>`
默认为HEAD，也可以指定一个commit id
### 指定标签信息
	$ git tag -a <tagname> -m "blablabla..."
### 用PGP签名标签
	$ git tag -s <tagname> -m "blablabla..."
### 查看所有标签: `git tag`
### 推送一个本地标签
	$ git push origin <tagname>
### 推送全部未推送过的本地标签
	$ git push origin --tags
### 删除一个本地标签
	$ git tag -d <tagname>
### 删除一个远程标签
	$ git push origin :refs/tags/<tagname>

## 特殊忽略文件
`.gitignore`
现成[配置文件][ignore_config](http://github.com/github/gitignore)，自行组合使用  
windows下采用另存为保存.gitignore文件


### 忽略原则：
1. OS自动生成的文件，如缩略图等
2. 编译生成的中间文件、可执行文件等。如Java编译产生的`.class`文件
3. 私有的带有敏感信息的文件，如存放口令的配置文件

		# Windows
		Thumbs.db
		ehthumbs.db
		Desktop.ini

		# Elixir

		# My Configuration
		db.ini
		deploy_key_rsa

### 检查规则错误
	$ git check-ignore -v App.class
	.gitignore:3:*.class  App.class		// 第3行忽略了该文件
### 强制添加将被忽略的文件：
	$ git add -f App.class

## 配置别名
	$ git config --global alias.st status
	$ git config --global alias.co checkout
	$ git config --global alias.ci commit
	$ git config --global alias.br branch
	$ git config --global alias.unstage 'reset HEAD'
	$ git config --global alias.last 'log -1'
	$ git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
`--global` 是针对当前用户起作用，不加只对当前仓库有效
配置文件在`.git/config`文件中，当前用户配置文件在主目录下`.gitignore`中

## Github:
- 可以任意Fork开源仓库；
- 自己拥有Fork后的仓库的读写权限；
- 可以推送pull request给官方仓库来贡献代码。
