---
output: docusaurus-md
sidebar_position: 2
description: A case example demonstrating how to perform an X-linked analysis.
# generated from example2.qmd
---

# 2) X-linked inheritance {#x-linked-inheritance}

This is a rather uneventful example meant to showcase the X-linked
inheritance analysis support of the app. Additionally, it demonstrates
how to set up models with incomplete dominance.

## Pedigree table {#pedigree-table}

The data corresponding to the case is shown below:

``` text
 ped id fid mid sex phenotype carrier proband age
   1  1   0   0   1         .       .       .  80
   1  2   0   0   2         .       .       .  80
   1  3   1   2   1  affected     het       .  40
   1  4   0   0   1    nonaff     neg       .  60
   1  5   1   2   2    nonaff     het       .  60
   1  6   4   5   1  affected     het       1  40
   1  7   4   5   1  affected     het       .  40
```

<img src={require("./img/ex2-ped.png").default} style={{maxHeight:"350px"}} />

There are three affected members: the proband, his brother, and the
maternal uncle. All three are hemizygous for a rare variant on the
X-chromosome, which the proband’s mother also carries. Ages are not
really important, as they won’t be the focus of this analysis.

## Recessive inheritance {#recessive-inheritance}

A basic model for these data would be to assume complete penetrance for
hemizygous and homozygous carriers, and no phenocopies. This can be
easily accomplished by switching to the `Liability class` mode and
selecting `XR` on the *Penetrance* panel’s header. In this case, the
table has two columns: `neg/♀het` refers to the penetrance for
non-carriers and heterozygous carriers, while `♂het/hom` is for the
hemizygous and homozygous carriers.

<img src={require("./img/ex2-xr-tab.png").default} style={{maxHeight:"150px"}} />

After setting these values, we can proceed to **Calculate** the evidence
for the variant’s pathogenicity.

<img src={require("./img/ex2-xr-flb.png").default} style={{maxHeight:"150px"}} />

## Dominant inheritance {#dominant-inheritance}

Sometimes it can be interesting to assume a dominant model where hemi-
and heterozygous carriers share the same penetrances. This can be easily
accomplished by switching to `XD`, and the table will adjust
accordingly.

<img src={require("./img/ex2-xd-tab.png").default} style={{maxHeight:"150px"}} />

In this particular case, however, attempting to compute cosegregation
evidence with these parameters will result in an error notification from
the app. This is because the proband’s mother cannot be unaffected and
carrier under complete penetrance.

## Incomplete dominance {#incomplete-dominance}

For certain analyses, it may be necessary to separate the penetrances of
hemi- and heterozygous carriers from others. This can be achieved by
switching to `XI` inheritance. Then, the penetrance table acquires three
columns referring to non-carriers (`neg`), hemi- and heterozygous
carriers (`het`), and homozygous carriers (`hom`). The penetrances of
hemi- and heterozygous carriers can be further differentiated by
defining sex-specific liability classes, as shown below.

<img src={require("./img/ex2-xi-tab.png").default} style={{maxHeight:"175px"}} />

Which should give the same results as the recessive model from before.

