
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyseg

> ***Try shinyseg here***: <https://chrcarrizosa.shinyapps.io/shinyseg>.

## What is shinyseg?

shinyseg is an interactive web application for clinical cosegregation
analysis, a method used to ascertain disease-associated genetic variants
from family data. More specifically, shinyseg calculates the
full-likelihood Bayes factor (FLB) for a variant’s pathogenicity
([Thompson et al., 2003](https://doi.org/10.1086/378100)), which can be
utilized within the ACMG-AMP framework for clinical variant
interpretation ([Richards et al.,
2015](https://doi.org/10.1038%2Fgim.2015.30)).

Analyses with shinyseg are easy and highly flexible. All you need to do
is upload a pedigree file, enter the genetic and clinical data, and
specify a suitable penetrance model. The following sections briefly
explain how to do so for an X-linked recessive case (`Example 2`), but
you can find more detailed information and tips within the app.

The application is powered by the [ped
suite](https://magnusdv.github.io/pedsuite/),
[Shiny](https://shiny.posit.co/), and
[rhandsontable](https://CRAN.R-project.org/package=rhandsontable).

## Uploading a pedigree

shinyseg works with pedigree files in *ped* format, like those created
by [QuickPed](https://magnusdv.shinyapps.io/quickped). Here is an
example of such a file and the corresponding family:

<div style="display:flex; height:250px;">

<div style="width: 35%;">

<img src="README_files/figure-gfm/ped-empty-1.png" width="95%" height="100%" style="display: block; margin: auto;" />

</div>

<div style="width:65%; margin-left:1.5rem; margin-top:1.5rem;">

     id fid mid sex
      1   0   0   1
      2   0   0   2
      3   1   2   1
      4   0   0   1
      5   1   2   2
      6   4   5   1
      7   4   5   1

</div>

</div>

Note that while other columns may be present, the application will only
utilize: `id` (individual ID), `fid` (father’s ID, 0 if not included in
the pedigree), `mid` (mother’s ID, 0 if not included in the pedigree)
and `sex` (1 = male; 2 = female).

## Genetic and clinical data

Once a pedigree is loaded, users are presented with a spreadsheet-like
table where they can enter the analysis data. This includes four
columns:

- **phenotype:** free-text field to specify disease phenotypes relevant
  to the analysis, e.g. `affected`, `breast cancer`, etc. The value
  `nonaff` is reserved for unaffected individuals. If empty, it
  indicates that the phenotype is unknown.
- **carrier:** `neg` for non-carriers, `het` for heterozygous (or
  hemizygous) carriers, `hom` for homozygous carriers. If empty, it
  indicates that the carriership state is unknown.
- **proband:** checkbox indicating the proband or index case.
- **age:** integer between 1-100 specifying the age of disease onset or
  censoring.

Using the same example from before:

<div style="display:flex; height:250px;">

<div style="width: 35%;">

<img src="README_files/figure-gfm/ped-filled-1.png" width="95%" height="100%" style="display: block; margin: auto;" />

</div>

<div style="width:65%; margin-left:1.5rem; margin-top:1.5rem;">

     id fid mid sex phenotype carrier proband age
      1   0   0   1                            80
      2   0   0   2                            80
      3   1   2   1  affected     het          40
      4   0   0   1    nonaff     neg          60
      5   1   2   2    nonaff     het          60
      6   4   5   1  affected     het       X  40
      7   4   5   1  affected     het          40

</div>

</div>

## Penetrance model

The inheritance pattern combines choices for chromosome (`A`: autosomal,
`X`: X-linked) and dominance (`D`: dominant, `R`: recessive, `I`:
incomplete). In this example, we select X-linked recessive (`XR`), which
will adjust the next steps accordingly.

Next, it is time to assign the probabilities of observing the analysis
phenotypes conditional on carriership status (hereafter referred to as
penetrances). This may be done in two ways:

### 1) Relative risk

This is a parametric version of the survival penetrances by [Belman et
al. (2020)](https://doi.org/10.1038/s41436-020-0920-4). It is based on:

- **Baseline lifetime risk, mean and SD:** the lifetime risk, mean, and
  standard deviation of disease onset in phenocopies, i.e., non-carriers
  and heterozygous carriers in recessive inheritance.
- **Hazard ratios:** the relative risks in homo-, hemi-, and
  heterozygous carriers in dominant inheritance, compared to
  phenocopies. They may also be specified through a **variant-associated
  lifetime risk**.

For example, the following defines that non-carrier men and heterozygous
women have a lifetime risk of 0.01 (1%) with onset at 70±15 years of
age, while hemizygous men and homozygous women have a lifetime risk of
0.75 (75%).

                                                                     
                       neg/♀het              ♂het/hom                
      sex  phenotype       risk  mu  sigma       risk hazard ratio(s)
     both   affected       0.01  70     15       0.75          137.93

Notice that the hazard ratio(s) column also indicates some value. In
this case, it was automatically updated by shinyseg based on the other
parameters, but it can be directly modified to specify more complex
models. However, this may be difficult to do. For this reason, the app
also features an assistant where users can instead enter cumulative
incidence data, and the model parameters will be optimized. For
instance:

<div style="display: flex;">

<div style="width: 37%;">

                            
          neg/♀het  ♂het/hom
     age        CI        CI
      20      0.01      0.01
      40      0.02      0.20
      60      0.03      0.60
      80      0.04      0.70

</div>

<div style="width: 63%; margin-left:1rem;">

    Optimal parameters:
     - neg/♀het risk: 0.0446
     - neg/♀het mean: 50.0096
     - neg/♀het SD: 20.0044
     - ♂het/hom risk: 0.7059
     - hazard ratio(s): 1.73, 1.87, 47.8, 51.47, 4.76, 2.74

</div>

</div>

### 2) Liability class

Alternatively, it is possible to manually define penetrances using a
table. The following defines the simplest case where there are no
phenocopies, and the chance of disease onset in hemizygous men and
homozygous women is 100%.

                                            
     neg/♀het ♂het/hom                      
         risk     risk  sex  phenotype  ages
            0        1                      

A more detailed specification dependent on sex, phenotype, and age can
be created by adding more rows.

## Calculating the FLB

Once all previous steps have been completed, obtaining the FLB is as
easy as clicking on the corresponding button. For instance, assuming a
variant population frequency of 0.001 and using the simple liability
class model shown before, we obtain an `FLB = 8.00`. This value would
indicate supporting evidence for pathogenicity according to [Jarvik and
Browning’s (2016)](https://doi.org/10.1016%2Fj.ajhg.2016.04.003)
thresholds.

The application then opens up more possibilities, including downloading
results in a custom HTML report or performing sensitivity analyses to
examine the robustness of cosegregation evidence. Explore them now with
[shinyseg](https://chrcarrizosa.shinyapps.io/shinyseg)!
