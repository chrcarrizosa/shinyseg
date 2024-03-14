"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[814],{3744:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>o,contentTitle:()=>r,default:()=>h,frontMatter:()=>a,metadata:()=>d,toc:()=>l});var t=i(4848),s=i(8453);const a={output:"docusaurus-md",sidebar_position:2},r="Quick start {#quick-start}",d={id:"quick-start",title:"Quick start",description:"quick-start}",source:"@site/docs/quick-start.md",sourceDirName:".",slug:"/quick-start",permalink:"/shinyseg/quick-start",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:2,frontMatter:{output:"docusaurus-md",sidebar_position:2},sidebar:"tutorialSidebar",previous:{title:"Welcome to shinyseg",permalink:"/shinyseg/"},next:{title:"How to\u2026",permalink:"/shinyseg/how-to/"}},o={},l=[{value:"Uploading a pedigree",id:"uploading-a-pedigree",level:2},{value:"Genetic and clinical data",id:"genetic-and-clinical-data",level:2},{value:"Penetrance model",id:"penetrance-model",level:2},{value:"1) Relative risk",id:"relative-risk",level:3},{value:"2) Liability class",id:"liability-class",level:3},{value:"Cosegregation evidence",id:"cosegregation-evidence",level:2}];function c(e){const n={a:"a",admonition:"admonition",code:"code",h1:"h1",h2:"h2",h3:"h3",img:"img",li:"li",p:"p",pre:"pre",strong:"strong",ul:"ul",...(0,s.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.h1,{id:"quick-start",children:"Quick start"}),"\n",(0,t.jsxs)(n.p,{children:["Analyses with shinyseg are easy and highly flexible. All you need to do\nis upload a pedigree file, enter the genetic/clinical data, and specify\na suitable penetrance model. The following sections briefly explain how\nto do so with an example case, but you can find detailed guidance within\nthe app or in the ",(0,t.jsx)(n.a,{href:"/how-to",children:"documentation"}),"."]}),"\n",(0,t.jsx)(n.h2,{id:"uploading-a-pedigree",children:"Uploading a pedigree"}),"\n",(0,t.jsxs)(n.p,{children:["The application works with pedigree files in ped format, like those\ncreated by ",(0,t.jsx)(n.a,{href:"https://magnusdv.shinyapps.io/quickped",children:"QuickPed"}),". Here is\nan example of such a file and the corresponding family:"]}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-text",children:" id fid mid sex\n  1   0   0   1\n  2   0   0   2\n  3   1   2   1\n  4   0   0   1\n  5   1   2   2\n  6   4   5   1\n  7   4   5   1\n"})}),"\n",(0,t.jsx)(n.p,{children:(0,t.jsx)(n.img,{src:i(4045).A+"",width:"1350",height:"1350"})}),"\n",(0,t.jsxs)(n.p,{children:["Note that other columns may be present, but you will at least need: ",(0,t.jsx)(n.code,{children:"id"}),"\n(individual ID), ",(0,t.jsx)(n.code,{children:"fid"})," (father\u2019s ID, 0 if not included in the pedigree),\n",(0,t.jsx)(n.code,{children:"mid"})," (mother\u2019s ID, 0 if not included in the pedigree), and ",(0,t.jsx)(n.code,{children:"sex"})," (1 =\nmale; 2 = female)."]}),"\n",(0,t.jsx)(n.admonition,{type:"tip",children:(0,t.jsxs)(n.p,{children:["You can even upload multiple families at the same time. Read\n",(0,t.jsx)(n.a,{href:"/how-to/pedigree#multiple-families",children:"here"})," how to do it!"]})}),"\n",(0,t.jsx)(n.h2,{id:"genetic-and-clinical-data",children:"Genetic and clinical data"}),"\n",(0,t.jsx)(n.p,{children:"Once a pedigree is loaded, users are presented with a table to enter the\nanalysis data:"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"phenotype:"})," free-text field to specify disease phenotypes\nrelevant to the analysis, e.g.\xa0",(0,t.jsx)(n.code,{children:"affected"}),", ",(0,t.jsx)(n.code,{children:"breast cancer"}),", etc.\n",(0,t.jsx)(n.code,{children:"nonaff"})," for unaffected individuals, empty if unknown."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"carrier:"})," ",(0,t.jsx)(n.code,{children:"neg"})," for non-carriers, ",(0,t.jsx)(n.code,{children:"het"})," for heterozygous (or\nhemizygous) carriers, ",(0,t.jsx)(n.code,{children:"hom"})," for homozygous carriers. Empty if\nunknown."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"proband:"})," checkbox indicating the proband or index case."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"age:"})," integer between 1-100 specifying the age of disease onset\nor censoring."]}),"\n"]}),"\n",(0,t.jsx)(n.p,{children:"For instance:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-text",children:" id fid mid sex phenotype carrier proband age\n  1   0   0   1         .       .       .  80\n  2   0   0   2         .       .       .  80\n  3   1   2   1  affected     het       .  40\n  4   0   0   1    nonaff     neg       .  60\n  5   1   2   2    nonaff     het       .  60\n  6   4   5   1  affected     het       1  40\n  7   4   5   1  affected     het       .  40\n"})}),"\n",(0,t.jsx)(n.p,{children:(0,t.jsx)(n.img,{src:i(9080).A+"",width:"1350",height:"1350"})}),"\n",(0,t.jsx)(n.admonition,{type:"warning",children:(0,t.jsxs)(n.p,{children:["The app will signal if some information is wrong. Look at the\nrequirements ",(0,t.jsx)(n.a,{href:"/how-to/pedigree#clinical-and-genetic-data",children:"here"}),"!"]})}),"\n",(0,t.jsx)(n.h2,{id:"penetrance-model",children:"Penetrance model"}),"\n",(0,t.jsxs)(n.p,{children:["The inheritance pattern combines choices for chromosome (",(0,t.jsx)(n.code,{children:"A"}),": autosomal,\n",(0,t.jsx)(n.code,{children:"X"}),": X-linked) and dominance (",(0,t.jsx)(n.code,{children:"D"}),": dominant, ",(0,t.jsx)(n.code,{children:"R"}),": recessive, ",(0,t.jsx)(n.code,{children:"I"}),":\nincomplete dominance). In this example we select X-linked recessive\n(",(0,t.jsx)(n.code,{children:"XR"}),"), which will adjust other inputs accordingly."]}),"\n",(0,t.jsx)(n.p,{children:"Next, it is time to assign the probabilities of observing the analysis\nphenotypes conditional on carriership status. These will be hereafter\nreferred to as penetrances, and their specification may be done in two\nways:"}),"\n",(0,t.jsx)(n.h3,{id:"relative-risk",children:"1) Relative risk"}),"\n",(0,t.jsxs)(n.p,{children:["A parametric version of the survival penetrances described in ",(0,t.jsx)(n.a,{href:"https://doi.org/10.1038/s41436-020-0920-4",children:"Belman et\nal.\xa0(2020)"}),". It is based on:"]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"Baseline lifetime risk, mean and SD:"})," the lifetime risk, mean,\nand standard deviation of disease onset in non-carriers and\nheterozygous carriers in recessive inheritance."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"Hazard ratios:"})," the relative risks in homo-, hemi-, and\nheterozygous carriers in dominant inheritance, compared to the\nbaseline. They can be either constant or age-dependent and may also\nbe specified through a ",(0,t.jsx)(n.strong,{children:"variant-associated lifetime risk"}),"."]}),"\n"]}),"\n",(0,t.jsxs)(n.p,{children:["For example, the following describes a constant relative risk model in\nwhich heterozygous women (",(0,t.jsx)(n.code,{children:"\u2640het"}),") and non-carriers (",(0,t.jsx)(n.code,{children:"neg"}),") have a\nlifetime risk (phenocopy rate) of 1% with onset at 70\xb115 years of age,\nwhile hemizygous men (",(0,t.jsx)(n.code,{children:"\u2642het"}),") and homozygous women (",(0,t.jsx)(n.code,{children:"hom"}),") have a\nlifetime risk of 75%."]}),"\n",(0,t.jsx)("img",{src:i(6232).A,style:{maxHeight:"300px"}}),"\n",(0,t.jsx)(n.admonition,{type:"tip",children:(0,t.jsxs)(n.p,{children:["To facilitate the specification, shinyseg can also optimize these\nparameters based on user-provided cumulative incidence data. See\n",(0,t.jsx)(n.a,{href:"/how-to/penetrance#optimal-parameters",children:"here"})," how to do it!"]})}),"\n",(0,t.jsx)(n.h3,{id:"liability-class",children:"2) Liability class"}),"\n",(0,t.jsx)(n.p,{children:"Alternatively, users can manually define the penetrances using a table.\nThe following represents the simplest case where there are no\nphenocopies, and the chance of disease onset in hemizygous men and\nhomozygous women is 100%."}),"\n",(0,t.jsx)("img",{src:i(3671).A,style:{maxHeight:"61px"}}),"\n",(0,t.jsx)(n.p,{children:"More detailed specifications, dependent on sex, phenotype, and age, can\nbe created by adding more rows and filling in these columns accordingly."}),"\n",(0,t.jsx)(n.h2,{id:"cosegregation-evidence",children:"Cosegregation evidence"}),"\n",(0,t.jsxs)(n.p,{children:["Calculating the FLB is then as easy as clicking a button. For instance,\nwith a population variant frequency of 0.001 and the simple liability\nclass model from before, we get an ",(0,t.jsx)(n.code,{children:"FLB = 8.00"}),". shinyseg reports this\nas supporting evidence for pathogenicity based on ",(0,t.jsx)(n.a,{href:"https://doi.org/10.1016%2Fj.ajhg.2016.04.003",children:"Jarvik and Browning\u2019s\n(2016)"})," thresholds."]}),"\n",(0,t.jsx)("img",{src:i(9153).A,style:{maxHeight:"70px"}}),"\n",(0,t.jsx)(n.p,{children:"Afterward, the app opens up more possibilities, including performing\nsensitivity analyses to assess the robustness of your results."}),"\n",(0,t.jsx)(n.admonition,{type:"tip",children:(0,t.jsxs)(n.p,{children:["Take a look at the ",(0,t.jsx)(n.a,{href:"/how-to/flb#sensitivity-analyses",children:"documentation"})," to\nexplore this feature!"]})})]})}function h(e={}){const{wrapper:n}={...(0,s.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(c,{...e})}):c(e)}},9153:(e,n,i)=>{i.d(n,{A:()=>t});const t="data:image/png;base64,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"},3671:(e,n,i)=>{i.d(n,{A:()=>t});const t="data:image/png;base64,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"},6232:(e,n,i)=>{i.d(n,{A:()=>t});const t=i.p+"assets/images/rrisk-tab-9fcb2069e0b697905958e36f5210e14e.png"},4045:(e,n,i)=>{i.d(n,{A:()=>t});const t=i.p+"assets/images/ped-empty-1-16607fa3a710ffc84b5e8125d19f68e4.png"},9080:(e,n,i)=>{i.d(n,{A:()=>t});const t=i.p+"assets/images/ped-filled-1-b4fedcb3957e5c7d4c86f41f8aa2cecc.png"},8453:(e,n,i)=>{i.d(n,{R:()=>r,x:()=>d});var t=i(6540);const s={},a=t.createContext(s);function r(e){const n=t.useContext(a);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function d(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:r(e.components),t.createElement(a.Provider,{value:n},e.children)}}}]);