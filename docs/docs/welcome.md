---
output: docusaurus-md
sidebar_position: 1
slug: /
# generated from welcome.qmd
---

# Welcome to shinyseg {#welcome-to-shinyseg}

shinyseg is an interactive web application for clinical cosegregation
analysis, a method used to ascertain disease-associated genetic variants
from family data. More specifically, it calculates the full-likelihood
Bayes factor (FLB) for a variant’s pathogenicity ([Thompson et al.,
2003](https://doi.org/10.1086/378100)), which can be readily used within
the ACMG-AMP framework for clinical variant interpretation ([Richards et
al., 2015](https://doi.org/10.1038%2Fgim.2015.30); [Jarvik and Browning,
2016](https://doi.org/10.1016%2Fj.ajhg.2016.04.003)).

<img src={require("./img/welcome.png").default} style={{maxHeight:"500px"}} />

shinyseg is powered by the [ped
suite](https://magnusdv.github.io/pedsuite/),
[Shiny](https://shiny.posit.co/), and
[rhandsontable](https://CRAN.R-project.org/package=rhandsontable).

