# repository-dashboard-shiny
Shiny application to monitor Github Issues

# Requirements #
To personalize this shiny dashboard, the following changes are required:
### Github repo configuration
* Create the following [custom Github labels] (https://help.github.com/articles/creating-and-editing-labels-for-issues-and-pull-requests/) - that all share a unique hexidecimal color (herein refered to as the SIZE label):
	* 2hr
	* 4hr
	* 1day
	* 2day
* Use any combination of these labels to estimate the size of the task - the dashboard will look for these labels by unique hexidecimal color and weight the metrics accordingly.
* The dashboard also expects all issues to be tagged with milestones that enumerate the sprint involved. The nomenclature for this milestone is "sprint x" - where x is an ordinal integer.
* Additionally, the dashboard will look for labels with the term "duplicate". If an issue has a duplicate tag, it will be removed from consideration.
* Any issue that does not have both a SIZE label and a milestone will be dropped from the dashboard.

### global.R configuration
* Modify the repoURL variable to point to the new repo
* Modify the sprintDeadlines variable so that:
	* The first element of the array is the begin date
	* Every date thereafter references the end of the sprint
* Modify the sizeColor variable to match the hexidecimal color for the SIZE label in the github repo.

# Installation and Usage #
```
git clone https://github.com/Duke-Translational-Bioinformatics/repository-dashboard-shiny.git
```
# Features #

* Utilizes the Github API v3 to extract issues and create a dashboard that contains the following visualizations:
	* Backlog Burndown
	* Sprint Burndown
	* Velocity Overlay
	* Estimated Ship Date Indicator
