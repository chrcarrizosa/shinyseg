---
output: docusaurus-md
sidebar_position: 3
description: An example with two phenotypes in a consanguineous family.
# generated from example3.qmd
---

# 3) Two phenotypes {#two-phenotypes}

This example demonstrates how to incorporate multiple phenotypes into
the cosegregation analysis. It also showcases how the app can deal with
consanguineous families.

## Pedigree table {#pedigree-table}

The data corresponding to the case is shown below:

``` text
 ped id fid mid sex phenotype carrier proband age
   1  1   0   0   1         .       .       .  50
   1  2   0   0   2         .       .       .  50
   1  3   1   2   1         .       .       .  50
   1  4   0   0   2      mild       .       .  50
   1  5   1   2   1         .       .       .  50
   1  6   0   0   2    nonaff       .       .  50
   1  7   3   4   1    severe     het       .  50
   1  8   0   0   2    nonaff       .       .  50
   1  9   5   6   1      mild       .       .  50
   1 10   0   0   2    nonaff       .       .  50
   1 11   7   8   1    nonaff       .       .  50
   1 12   9  10   2      mild     het       .  50
   1 13  11  12   1    severe     het       1  50
   1 14  11  12   2      mild     het       .  50
   1 15  11  12   2    nonaff     neg       .  50
```

<img src={require("./img/ex3-ped.png").default} style={{maxHeight:"350px"}} />

There are two disease phenotypes in this family: one relatively mild and
common and one more rare and severe. It is hypothesized that the rare
autosomal variant found could be related to an increased risks for both.

## One phenotype {#one-phenotype}

A basic approach would be to ignore this fact and treat both as the same
“affected” phenotype. For instance, we may use a conservative phenocopy
rate (`neg risk`) of 20% and a penetrance (`het/hom risk`) of 70% for
all pedigree members:

<img src={require("./img/ex3-lclass1-tab.png").default} style={{maxHeight:"62px"}} />

With this setup, we would obtain moderate evidence for pathogenicity, as
shown below. The sensitivity analysis also indicates that reducing the
phenocopy rate would greatly increase the evidence. However, doing so
may result in overestimation, since we know the mild phenotype is
relatively common.

<img src={require("./img/ex3-lclass1-flb.png").default} style={{maxHeight:"500px"}} />

## Two phenotypes {#two-phenotypes-1}

Luckily, we can easily account for this fact by declaring a lower
phenocopy rate for only the severe phenotype, let’s say 1%. This can be
done by clicking on **ADD** to add more liability classes.

<img src={require("./img/ex3-lclass2-tab.png").default} style={{maxHeight:"105px"}} />

And with that, the FLB doubles to 41.5, resulting in strong evidence for
pathogenicity.

<img src={require("./img/ex3-lclass2-flb.png").default} style={{maxHeight:"100px"}} />

