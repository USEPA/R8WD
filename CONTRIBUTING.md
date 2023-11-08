# Contribute to R8WD


Pull requests, bug reports, and all other forms of contribution are welcomed and encouraged. Everyone is welcome to contribute and we value everybody's contribution. Code contributions are not the only way to help the community. Asking and answering questions, helping others, and improving the documentation are also immensely valuable.

It also helps us if you spread the word! One way to do this is to ⭐️ the repository.


## Ways to contribute

There are several ways you can contribute:

* Fix outstanding issues with the existing code.
* Submit issues related to bugs or desired new features.
* Contribute to the examples or to the documentation.


## Fixing existing issues

If you notice an issue with the existing code and have a fix in mind, feel free to [start contributing](https://github.com/USEPA/R8WD/blob/main/CONTRIBUTING.md/#create-a-pull-request) and open a Pull Request.

## Submitting an issue or feature request

Do your best to follow these guidelines when submitting an issue or a feature
request. It will make it easier for us to come back to you quickly and with good
feedback.

### Did you find a bug?

The R8WD package is robust and reliable thanks to users who report the problems they encounter.

Before you report an issue, make sure the bug was not already reported using the search bar on GitHub under Issues. Your issue should also be related to bugs in the library itself, and not your code. Please provide a reproducible example, if possible.

Once you've confirmed the bug hasn't already been reported, please include the following information in your issue so we can quickly resolve it:

* Your **OS type and version** and **R version**.
* A short, self-contained, code snippet that allows us to reproduce the bug.
* A description of what behavior is intended and how that differs from what is observed. Include the *full* error message or output.
* Attach any other additional information, like screenshots, you think may help.


To get the OS and R version automatically, run the following command:

```R
Sys.info()[1:3] # operating system info
R.Version()[c(1,14)] # R version and platform
```

### Do you want a new feature?

If there is a new feature you'd like to see, please open an issue and describe:

1. What is the *motivation* behind this feature? Is it related to a problem or frustration with the library? Is it a feature related to something you need for a project? Is it something you worked on and think it could benefit the broader community?

2. Describe your requested feature in as much detail as possible. The more you can tell us about it, the better we'll be able to help you.

3. Provide a *code snippet* that demonstrates the features usage.

4. If the feature is related to a paper, please include a link.


## Create a Pull Request

Before writing any code, we strongly advise you to search through the existing PRs or
issues to make sure nobody is already working on the same thing. If you are
unsure, it is always a good idea to open an issue to get some feedback.

Follow the steps below to start contributing:

1. Fork the [repository](https://github.com/troyhill/R8WD) by
   clicking on the **[Fork](https://github.com/troyhill/R8WD/fork)** button on the repository's page. This creates a copy of the code under your GitHub user account.

2. Clone your fork to your local disk, and add the base repository as a remote:

   ```bash
   git clone git@github.com:<your Github handle>/R8WD.git
   cd R8WD
   git remote add upstream https://github.com/USEPA/R8WD.git
   ```

3. Create a new branch to hold your development changes:

   ```bash
   git checkout -b a-descriptive-name-for-my-changes
   ```

   🚨 **Do not** work on the `main` branch!

4. Set up a new R Project as an R Package to develop your feature.

5. Develop the features on your branch.

   As you work on your code, you should make sure the package passes R CMD check (accessible in RStudio's Build pane).

   Once you're happy with your changes, add changed files with `git add` and
   record your changes locally with `git commit`:

   ```bash
   git add modified_file.R modified_file.Rd
   git commit
   ```

   Please remember to write [good commit
   messages](https://chris.beams.io/posts/git-commit/) to clearly communicate the changes you made.

   To keep your copy of the code up to date with the original
   repository, rebase your branch on `upstream/branch` *before* you open a pull request or if requested by a maintainer:

   ```bash
   git fetch upstream
   git rebase upstream/main
   ```

   Push your changes to your branch:

   ```bash
   git push -u origin a-descriptive-name-for-my-changes
   ```

   If you've already opened a pull request, you'll need to force push with the `--force` flag. Otherwise, if the pull request hasn't been opened yet, you can just push your changes normally.

6. Now you can go to your fork of the repository on GitHub and click on **Pull request** to open a pull request. Make sure you tick off all the boxes in our [checklist](https://github.com/USEPA/R8WD/blob/main/CONTRIBUTING.md/#pull-request-checklist) below. When you're ready, you can send your changes to the project maintainers for review.

7. It's ok if maintainers request changes, it's part of collaborating! So everyone can see the changes in the pull request, work in your local branch and push the changes to your fork. They will automatically appear in the pull request.

### Pull request checklist

☐ The pull request title should summarize your contribution.<br>
☐ If your pull request addresses an issue, please mention the issue number in the pull
request description to make sure they are linked (and people viewing the issue know you
are working on it).<br>
☐ To indicate a work in progress please prefix the title with `[WIP]`. These are
useful to avoid duplicated work, and to differentiate it from PRs ready to be merged.<br>
☐ Make sure the R package can be built and installed without errors.<br>



