# NAC-meta-analysis
Code for the (in review) publication: N-acetylcysteine as a treatment for substance use cravings: A meta-analysis (Winterlind et al., 2024)

GitHub page last updated August 2024

The R code was created to conduct all analyses related to the manuscript (i.e., meta-analysis, subgroup analysis, forest plots, etc.)

Code uses metafor package. For more info, see: Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of statistical software, 36(3), 1-48.

## **Additional notes for the different steps of the "meta-analysis-code" code which were not included as notes within the code itself:**

Step 1:
- Effect sizes (ES) were calculated using https://lbecker.uccs.edu

Step 2:
- Note that effect sizes and standard error were calculated beforehand by two authors (using link above) and that n represents number of participants analyzed (not baseline N)
- You can conduct same analyses by entering ES, SE, and N manually like we did here or by loading in your own dataset with these values

Step 3:
- RRs are calculated here and then converted to logRRs in the "forest-plots-code" code; note that these are calculated so that values below 0 indicate lower risk of AEs for NAC group
- Dataset used here was set up so that each study had its own row, with columns set up as:

Study ID       |  ae_pos_p  |  ae_neg_p  |  total1  |  ae_pos_e  |  ae_neg_e  |  total2 |    se     | ES
- ae_pos_p: # of AE events in the placebo group
- ae_neg_p: total1 - ae_pos_p
- total1: N in placebo group (defined as N at risk; includes participants which were not necessarily analyzed)
- ae_pos_e: # of AE events in the placebo group
- ae_neg_e: total2 - ae_pos_e
- total2: N in experimental group (defined as N at risk; includes participants which were not necessarily analyzed)
Note that for Back (2021) and (2023), the N for total1 and total2 reflect the N of the whole sample (not just those analyzed for outcome effects); that's just how they reported AE data
- RRs couldn't be calculated for McKetin (AEs exceed N), Roten (didn't report on AEs), Back 2016 (no between-group data), or Schulte (0 AEs)

No relevant notes for steps 4 & 5

## **Additional notes for "forest-plots-code" code:**
- Three different datasets were used for each plot

First plot (representing meta-analysis and subgroup analysis): 
- The dataset for the first plot was structured so that each study was listed twice (one row with allocation "Meta-analysis" and one row with allocation "Subgroup analysis"), except for McKetin et al. (2021) since the subgroup analysis was examing all studies except this one
- Therefore, each study had two rows (one with each allocation) and columns: Study ID; Weight (as calculated by influence analysis in step 2 of meta-analysis code); ES; SE; allocation
- RE Model printed on the bottom of figure removed in Adobe Illustrator as it's not relevant for this plot

Second plot (logRRs):
 - Same dataset as used in step 3 of meta-analysis code
 - Setting "atransf = exp, at=log(c(0.25, 1, 2))" converts RRs to logRRs
 
 Third plot (alcohol trials subgroup):
 - Dataset contained all studies listed once and was formatted similarly to that of the first plot (Study ID; ES; SE; alloc; Weight), but instead each study received allocation "alcohol" or "other" in the alloc column
