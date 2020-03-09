This is the README file.

The supplementary material consist of two packages:
1) supplementary_material
2) replication_package


-----------------------------
1)  supplementary_material
-----------------------------

- performance_refCount/_postCount/_refDense: shows the performance of Random Forest, Support Vector Machine and Logistic Regression for binary and tertiary classification in terms of accuracy, precision, recall, f-score, and auc.

- importance_refProne/_postCount/_refDense: shows the importance API properties for the classification of API classes into refProne/refDense for binary and tertiary classification. The selection of the variables from the three clusters is based on the highest correlation to the target variable.

- importance_iterativ_refProne/_postCount/_refDense: shows the importance of API properties for the classification of API classes into refProne/refDense for binary classification for all  54 runs of combinations of variables from the three clusters.



-----------------------------
2)  replication_package
----------------------------- 

*********************
* TXT and CSV Files *
*********************

- appIdsTop80_all.txt: list of ids of all downloaded apps

- changes_details.csv: details of the changes, such as changetype and fqn (of the changed class) for all changes of the 1030 API classes

- commit_details.csv: details of the commits, such as authoremail, commitdate, commitid, and commitmessage for all commits of the 1030 API classes

- corr_analysis.csv: shows the spearman correlation between all API properties, refCount, postCount, and refDensity

- manual_evaluation.csv: details of the manual evaulation, such as postid, problem and question type

- results_density.csv: results of the 54 runs over all combinations of variables for the classification of refDense and not refDense API classes

- results_refprone.csv: results of the 54 runs over all combinations of variables for the classification of refProne and not refProne API classes

- results_postprone.csv: results of the 54 runs over all combinations of variables for the classification of refProne and not postProne API classes

- smells_detail.csv: details about the smells, such as fqn of the API class, smellName, and severity.

- summary.csv: contains all API properties for our investigations. Please note, refDensity is not included. It is computed in the R-Scripts.

Note, we provided the data for all 1030 API classes. For our studies, we consider only those 945 API classes that are also used in apps (usageCount > 0).

*************
* R-Scripts * 
*************

- classification.R: Classifies API classes (read from summary.csv) into refProne/ not refProne (postProne/not postProne and refDense/not refDense) API classes and supports tertiary classification as well. 
For the binary classification with Logistic Regression, we used the methods provided from R, for the tertiary classification we used the MASS libary.
--> Before running the skript, please change "pathToFile" to the path of the summary.csv.

- iterative.R: 54 runs over all combinations of variables from the three clusters and not correlated variables to classify API classis into refProne/not refProne, postProne/ not postProne, or refDense/not refDense.
--> Before running the skript, please change base to the directory of summary.csv and change the registerDoMC(20) to number of cores you want to use
--> Takes quite some time!! 

- iterative_onlyCheckRuns.R: plots the the importance of variables over all combination of variables (see iterative.R) for the classification of refProne/postProne/refDense API classes read from results_refprone.csv, results_postprone.csv, and results_density.csv.
--> Before running the skript, please change "pathToFDirectory" to the directory of results_prone.csv and results_density.csv.