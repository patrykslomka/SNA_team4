## SNA_team4
Repository for the Social Network Analysis course at JADS - Team 4

## Getting Started

In the next section some explanation will follow on how work on this repository on your own machine.

### Cloning the repository

1. Open your command prompt or your terminal switch to the directory where you want your code to be.
2. Clone the repository with the following command: `git clone https://github.com/patrykslomka/SNA_team4.git`
3. Now open the new directory with Pycharm or the IDE of your choice

### Creating a virtual environment

A Python virtual environment is a self-contained environment that includes a specific version of Python and a set of
installed packages, isolated from the system-wide Python installation.

Virtual environments allow you to create separate Python environments for different projects, each with its own Python
version and installed packages. This can be useful when you need to use different versions of packages or Python itself
for different projects, or when you want to avoid conflicts between packages that have conflicting dependencies.

1. First clone the repository into a specific directory: `git clone https://github.com/patrykslomka/SNA_team4.git`
2. Enter project directory: `cd algorithms`
3. Create a virtual environment: `python -m venv venv`
4. Activate virtual environment: `windows: venv\Scripts\activate` or `mac: source venv/bin/activate`
5. Install packages from requirements with pip: `pip install -r requirements.txt` (to be created)

Once the dependencies grow, add them to the `requirements.txt` file.

### Working with the data

1. Then add the csv files to the data directory on your machine.

#### Gitignore

In the Gitignore there are file patterns listed which match to directories or to files. Those matches are not pushed to
Github. If you have any files that do not belong to github (e.g. big data files or your personal test files, list them
in the Gitignore). For example: Different IDE's use files we do not want in our repo. Check your directory for hidden
files and add those to the `.gitignore` file.

#### Constants (to be created)

In the directory `config` there are two files: `constants.py` and `personal_constants.py`.  `personal_constants.py` is
included in`.gitignore` thus your confidential constants
are never pushed to Github. Make sure to define all constants in UPPERCASE. Whenever you need a confidential constant,
first import the personal constant in `constants.py`, then import `constants.py` in your script.

### Branching strategy

Here it is explained how you should work with branches. The main point is that nobody is allowed to push directly to the
`main` brach. So, for each task (feature), we will create a new branch. After the feature is done, you can create a pull
request (must be created on through graphical user interface of Github) which has to be reviewed by two other team members (make sure to assign them in your pull request). After approval, your feature can be merged with the main branch.

Here are some helpful commands to implement this strategy:

#### Creating your own branch

- `git branch` - shows all existing branches
- `git checkout` - allows you to switch to a different branch.
- `git checkout -b <your_name/branch_name>` - creates a new branch from the currently checked out branch & switches to it.
  Without `-b` it only switches to the existing branch. GitHub uses a tree based structure, so please consider from
  which branch you are creating a new one. If you are not building on top of a feature build by someone else, it is okay 
to make a new branch from the main branch.

#### Working on branches

- Different people can push to a branch (e.g. making changes). If you are working on that same branch run: `git pull`
  which pulls the latest changes to your local machine.

#### Commiting to a branch
First make sure you are working on the correct branch. You can check this by running: `git branch`. The branch you
are working on will be colored green, otherwise, see above.
- `git add .` - adds all files to the staging area
- `git status` - shows the files that are in the staging area. This is very helpful to check if you did not make any
  mistakes while pushing.
- `git commit -m "Your message"` - commits the files in the staging area to the branch you are working on.
- `git push` - pushes staged files to the branch you are working on.

#### Creating a Pull Request & Merging branches

- Once the feature branch is complete, you first have to make sure it is up to date with the remote repo on Github. To
  assure this, run in the CLI: `git pull`.
- Then you can create a pull request on the Github website.
- Important: select two developers to review your code.
- The reviewers will check your code and give you feedback by adding comments to your code.
- You can now resolve the comments by making new commits to the branch. When you are done, request the reviewers to
check your changes.
