---
output: docusaurus-md
sidebar_position: 4
description: An example from Belman et al. (2020) showing a complex model with age-dependent relative risks.
# generated from example3.qmd
---

# 4) Breast cancer and BRCA1 {#breast-cancer-and-brca1}

This example is directly taken from [Belman et
al. (2020)](https://doi.org/10.1038/s41436-020-0920-4). It puts to the
test shinyseg’s more advanced capabilities by requiring a penetrance
model with age-dependent relative risks and the explicit declaration of
unobserved phenotypes.

## Pedigree table {#pedigree-table}

The data corresponding to the case is shown below:

``` text
 ped id fid mid sex phenotype carrier proband age
   1  1   0   0   1    nonaff       .       .  80
   1  2   0   0   2      BrCa       .       .  65
   1  3   1   2   2      BrCa     het       .  81
   1  4   1   2   1    nonaff       .       .  41
   1  5   0   0   2         .       .       .  89
   1  6   0   0   1    nonaff       .       .  80
   1  7   1   2   2    nonaff     het       .  75
   1  8   1   2   2    nonaff     neg       .  60
   1  9   4   5   2      BrCa     het       .  41
   1 10   4   5   2      BrCa     het       .  50
   1 11   4   5   2    nonaff     het       .  52
   1 12   6   7   2      BrCa     het       1  49
   1 13   6   7   1    nonaff     het       .  36
   1 14   6   7   2      BrCa     het       .  48
```

<img src={require("./img/ex4-ped.png").default} style={{maxHeight:"350px"}} />

Notice how the legend also includes ovarian and pancreatic cancer, even
though these are not present in any of the members of the pedigree. This
is because they are still relevant for computing the penetrances and
therefore must be included in the analysis. The next section explains
how to do so.

## Penetrance {#penetrance}

This analysis employs the default `Relative risk` mode and autosomal
dominant (`AD`) inheritance. It’s important to note that when uploading
previous data to the app, the relative risk table will only include a
row corresponding to breast cancer (`BrCa`) for both sexes.

<img src={require("./img/ex4-rrisk-tab1.png").default} style={{maxHeight:"400px"}} />

As mentioned, in this case it’s necessary to include other relevant
phenotypes for calculating the penetrance for the specific gene under
analysis (BRCA1). This can be achieved by entering their names into the
**Extra phenotypes** input, located at the bottom of the *Penetrance*
panel. Additionally, we need to specify sex-specific penetrances. This
can be easily done by clicking on the entries of the sex column and
changing `both` to any of the sexes. With that done, the table will look
like this:

<img src={require("./img/ex4-rrisk-tab3.png").default} style={{maxHeight:"600px"}} />

We are now ready to start filling in the data, using similar estimates
to those in [Belman et
al. (2020)](https://doi.org/10.1038/s41436-020-0920-4):

-   For the baseline parameters (`neg risk`, `neg mean`, `neg SD`), we
    have utilized the UK 2008-2012 estimates from Cancer Incidence in
    Five Continents (CI5). While not strictly necessary, in this case
    the data were fitted to the normal model employed by shinyseg to
    obtain the optimal parameters. This may be done using the
    [**Assistant**](../how-to/penetrance#optimal-parameters).

-   The `hazard ratios(s)` have also been adapted from [COOL’s
    website](https://fenglab.chpc.utah.edu/cool2/manual.html). The
    vectors shown below are of length six, corresponding to the hazard
    ratios at ages approximately 1, 20, 40, 60, 80, and 100. These are
    then interpolated/smoothed by the app, as shown in the plots below
    the table.

<img src={require("./img/ex4-rrisk-tab4.png").default} style={{maxHeight:"600px"}} />

## FLB {#flb}

Finally, we can **Calculate** the Bayes factor, which in this case
points to inconclusive evidence for pathogenicity. While this conclusion
aligns with that of [Belman et
al. (2020)](https://doi.org/10.1038/s41436-020-0920-4), we note a slight
quantitative deviation (5.7 vs. 3.9) which is primarily attributed to
how shinyseg utilizes model-based estimates instead of the raw data. The
payoff of shinyseg’s set up is that sensitivity analyses can now be
easily performed by altering a reasonable number of parameters.

<img src={require("./img/ex4-flb.png").default} style={{maxHeight:"150px"}} />

