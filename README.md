# Skwad
*Before anything else make sure have have installed git on your local device, set-up a git hub account, and request and accepted editting permission from Jennifer.*

The README is meant to be a living document that (i) describes the file structure, (ii) best practices on how to contribute to and use this repository, and (iii) general tips and tricks on getting the most out of this ecosystem.

**README Contents:**
* [ Best Practice ](#best-practice)
* [ Typical Workflow ](#workflow)
* [ Using Git ](#Using-Git)
  * [ Cloning Repository ](#clone-git)
  * [ Pulling, committing and pushing ](#git-commit)
  * [ Branching ](#branching)
  * [ Other Useful Git Resources ](#git-resources)
 
<a name="best-practice"></a>
## Best Practrice
1. This repository should only be used as a centralised location for our finalised documents and codes.
2. Directly push to the master branch, and do not push your local branches to this repository!
3. When commiting changes to the master branch make sure your comments are sufficiently detailed that someonelse can make sense of what you have done a few months from now.
4. Feel free to have whatever git structure on your local device, but do not push your mess here! Keep this repository clean and accessible for everyone.

<a name="workflow"></a>
## Typical workflow example
1. Start your terminal and go to the location in the director where you have saved this repository `cd <directory>`.
2. Check if there are any updates to the master directory online `git status`.
3. If there are, pull the most up to date repository `git pull`.
4. Open a new branch to begin working locally on a project `git checkout -b branch_name`.
5. After working for a few hours, you would like to add your new files to the the online master repository. So you,
    1. Clean up your branch such that you don't have files that you don't want to be uploaded.
    2. Make sure your master is up to date by `git pull`.    
    3. Go to the local master branch `git branch master`.
    4. Merge this with your branch `git merge branch_name`.
    5. Add these changes `git add .`
    6. Commit this changes and give detailed comments `commit -m "George is a tosser"` (see [here](https://nexter.org/wp-content/uploads/2019/07/weird-stock-images-photos-funny-pic.jpg) form more details)
    7. Push these changes to git hub `git push`.

<a name="Using-Git"></a>
## Using Git<a name="clone-gitt"></a>
<a name="clone-git"></a>
### Cloning this repository
1. Open your terminal and go to the location in your directory where you would like to save this repository
`cd <directory location>`.
2. Then type the following code to create a clone of this repository on your device
`git clone https://github.com/jeannesorin/Skwad.git`.
3. If this is the first time you are pulling a repository from git hub, it will ask you to enter your git hub username and password.
<a name="git-commit"></a>
### Pulling, committing and pushing
1. To pull the most up-to-date master repository type `git pull'.
2. After making relevant changes add the files you would like to be committed.
    1. If you would like to add specific files type,
`git add <filenames>`.
    2. If you would like all changes to be committed type,
`git add .`.
3. To commit these changes to your local git type,
`git commit -m "comment"`.
4. To push these new changes to the shared git hub repository type,
`git push`.
<a name="branching"></a>
### Branches Basics
* To see the list of branches in your git and which branch you are currently on type `git branch`
* To create a new branch type `git branch <branch name>`. Alternatively, to create a new branch and jump to it type `git checkout -b <branch name>`.
* To jump to specific branch type `git checkout <branchname>`
* To delete a branch type `git branch -d <branchname>`
<a name="git-resources"></a>
### Other useful resources
* Here are a couple of youtube videos that provide a quick introduction on how to use git [part 1](https://www.youtube.com/watch?v=USjZcfj8yxE) and [part 2](https://www.youtube.com/watch?v=nhNq2kIvi9s)
* Mac terminal command cheat sheet [here](https://gist.github.com/poopsplat/7195274)
