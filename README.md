# EnviroHarsh
Study on two sources of environmental harshness - Pathogen Prevalence and Resource Scarcity - and their impact on preference of facial masculinity

**Results in a nutshell:** <br>
Slight preference for facial masculinity in long-term male partners is the baseline that can be reinforced by high resource scarcity. High pathogen prevalence is the game changer that elicits preferences for higher facial femininity. Rated formidability and healthiness (both higher in masculinized faces) predict facial attractiveness.

**You can track the preprint here:**

**How to run the code:**<br>
You should initiate the session from the EnviroHarsh.Rproj and start perhaps from 01_data_prepare.R (if you are interested how the raw data are handeled) or directly from the evaluation of structural Bayesian model 02_posterior_evaluation.R that sources the data preparation script before tha analysis. Than build your way up thhrough the scripts that summarize and visualize the results. We used rethinking packege with STAN infrastructure, so make sure all neccesary elements are properly installed on your computer. See https://github.com/rmcelreath/rethinking for details about the package and installation. Custom-build functions for data visualizations are stored in a separate script, some details on posetrior visulaizations are stored in params....txt files.

Scripts starting with 00 deal with the pilot data (pilot_data.csv) and can be run independently on the sequentially numbered scripts (they, however, often source the experimental data rawdata.txt for comparative analysis and visualizations). Item descriptions of Pathogen prevalence and Resource scarcity scales are stored in respective ...items.txt files.


