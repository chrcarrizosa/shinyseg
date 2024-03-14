---
output: docusaurus-md
sidebar_position: 2
# generated from quick-start.qmd
---

# Quick start {#quick-start}

Analyses with shinyseg are easy and highly flexible. All you need to do
is upload a pedigree file, enter the genetic/clinical data, and specify
a suitable penetrance model. The following sections briefly explain how
to do so with an example case, but you can find detailed guidance within
the app or in the [documentation](/how-to).

## Uploading a pedigree {#uploading-a-pedigree}

The application works with pedigree files in ped format, like those
created by [QuickPed](https://magnusdv.shinyapps.io/quickped). Here is
an example of such a file and the corresponding family:

``` text
 id fid mid sex
  1   0   0   1
  2   0   0   2
  3   1   2   1
  4   0   0   1
  5   1   2   2
  6   4   5   1
  7   4   5   1
```

![](img/ped-empty-1.png)

Note that other columns may be present, but you will at least need: `id`
(individual ID), `fid` (father’s ID, 0 if not included in the pedigree),
`mid` (mother’s ID, 0 if not included in the pedigree), and `sex` (1 =
male; 2 = female).

:::tip

You can even upload multiple families at the same time. Read
[here](/how-to/pedigree#multiple-families) how to do it!

:::

## Genetic and clinical data {#genetic-and-clinical-data}

Once a pedigree is loaded, users are presented with a table to enter the
analysis data:

-   **phenotype:** free-text field to specify disease phenotypes
    relevant to the analysis, e.g. `affected`, `breast cancer`, etc.
    `nonaff` for unaffected individuals, empty if unknown.
-   **carrier:** `neg` for non-carriers, `het` for heterozygous (or
    hemizygous) carriers, `hom` for homozygous carriers. Empty if
    unknown.
-   **proband:** checkbox indicating the proband or index case.
-   **age:** integer between 1-100 specifying the age of disease onset
    or censoring.

For instance:

``` text
 id fid mid sex phenotype carrier proband age
  1   0   0   1         .       .       .  80
  2   0   0   2         .       .       .  80
  3   1   2   1  affected     het       .  40
  4   0   0   1    nonaff     neg       .  60
  5   1   2   2    nonaff     het       .  60
  6   4   5   1  affected     het       1  40
  7   4   5   1  affected     het       .  40
```

![](img/ped-filled-1.png)

:::warning

The app will signal if some information is wrong. Look at the
requirements [here](/how-to/pedigree#clinical-and-genetic-data)!

:::

## Penetrance model {#penetrance-model}

The inheritance pattern combines choices for chromosome (`A`: autosomal,
`X`: X-linked) and dominance (`D`: dominant, `R`: recessive, `I`:
incomplete dominance). In this example we select X-linked recessive
(`XR`), which will adjust other inputs accordingly.

Next, it is time to assign the probabilities of observing the analysis
phenotypes conditional on carriership status. These will be hereafter
referred to as penetrances, and their specification may be done in two
ways:

### 1) Relative risk {#relative-risk}

A parametric version of the survival penetrances described in [Belman et
al. (2020)](https://doi.org/10.1038/s41436-020-0920-4). It is based on:

-   **Baseline lifetime risk, mean and SD:** the lifetime risk, mean,
    and standard deviation of disease onset in non-carriers and
    heterozygous carriers in recessive inheritance.
-   **Hazard ratios:** the relative risks in homo-, hemi-, and
    heterozygous carriers in dominant inheritance, compared to the
    baseline. They can be either constant or age-dependent and may also
    be specified through a **variant-associated lifetime risk**.

For example, the following describes a constant relative risk model in
which heterozygous women (`♀het`) and non-carriers (`neg`) have a
lifetime risk (phenocopy rate) of 1% with onset at 70±15 years of age,
while hemizygous men (`♂het`) and homozygous women (`hom`) have a
lifetime risk of 75%.

<img src={require("./img/rrisk-tab.png").default} style={{maxHeight:"300px"}} />

:::tip

To facilitate the specification, shinyseg can also optimize these
parameters based on user-provided cumulative incidence data. See
[here](/how-to/penetrance#optimal-parameters) how to do it!

:::

### 2) Liability class {#liability-class}

Alternatively, users can manually define the penetrances using a table.
The following represents the simplest case where there are no
phenocopies, and the chance of disease onset in hemizygous men and
homozygous women is 100%.

<img src={require("./img/lclass-tab.png").default} style={{maxHeight:"61px"}} />

More detailed specifications, dependent on sex, phenotype, and age, can
be created by adding more rows and filling in these columns accordingly.

## Cosegregation evidence {#cosegregation-evidence}

Calculating the FLB is then as easy as clicking a button. For instance,
with a population variant frequency of 0.001 and the simple liability
class model from before, we get an `FLB = 8.00`. shinyseg reports this
as supporting evidence for pathogenicity based on [Jarvik and Browning’s
(2016)](https://doi.org/10.1016%2Fj.ajhg.2016.04.003) thresholds.

<img src={require("./img/flb.png").default} style={{maxHeight:"70px"}} />

Afterward, the app opens up more possibilities, including performing
sensitivity analyses to assess the robustness of your results.

:::tip

Take a look at the [documentation](/how-to/flb#sensitivity-analyses) to
explore this feature!

:::

