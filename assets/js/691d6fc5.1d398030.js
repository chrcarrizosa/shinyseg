"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[119],{2079:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>c,contentTitle:()=>r,default:()=>h,frontMatter:()=>t,metadata:()=>o,toc:()=>l});var i=s(4848),a=s(8453);const t={output:"docusaurus-md",sidebar_position:1,description:"A simple case to showcase the relative risk mode and the importance of accounting for age of onset."},r="1) Constant relative risk {#constant-relative-risk}",o={id:"examples/example1",title:"1) Constant relative risk",description:"A simple case to showcase the relative risk mode and the importance of accounting for age of onset.",source:"@site/docs/examples/example1.md",sourceDirName:"examples",slug:"/examples/example1",permalink:"/shinyseg/examples/example1",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{output:"docusaurus-md",sidebar_position:1,description:"A simple case to showcase the relative risk mode and the importance of accounting for age of onset."},sidebar:"tutorialSidebar",previous:{title:"Examples",permalink:"/shinyseg/examples/"},next:{title:"2) X-linked inheritance",permalink:"/shinyseg/examples/example2"}},c={},l=[{value:"Pedigree table",id:"pedigree-table",level:2},{value:"A single liability class",id:"a-single-liability-class",level:2},{value:"More liability classes",id:"more-liability-classes",level:2},{value:"Relative risk mode",id:"relative-risk-mode",level:2}];function d(e){const n={code:"code",em:"em",h1:"h1",h2:"h2",p:"p",pre:"pre",strong:"strong",...(0,a.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.h1,{id:"constant-relative-risk",children:"1) Constant relative risk"}),"\n",(0,i.jsx)(n.p,{children:"This example pertains to a family suffering from a rare connective\ntissue disorder in which a rare autosomal variant was identified. The\ncase primarily focuses on accounting for unaffected carriers who are\nrelatively young compared to the expected onset of the disease."}),"\n",(0,i.jsx)(n.h2,{id:"pedigree-table",children:"Pedigree table"}),"\n",(0,i.jsx)(n.p,{children:"The data corresponding to the case is shown below:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-text",children:" ped id fid mid sex phenotype carrier proband age\n   1  1   0   0   1         .       .       .  80\n   1  2   0   0   2         .       .       .  80\n   1  3   1   2   1    nonaff       .       .  40\n   1  4   0   0   2    nonaff       .       .  80\n   1  5   1   2   1  affected     het       .  60\n   1  6   0   0   2    nonaff       .       .  80\n   1  7   3   4   1    nonaff     neg       .  60\n   1  8   3   4   1  affected     het       .  50\n   1  9   0   0   2    nonaff       .       .  60\n   1 10   5   6   1  affected     het       .  30\n   1 11   0   0   2    nonaff       .       .  50\n   1 12   5   6   1    nonaff     neg       .  50\n   1 13   8   9   2  affected     het       1  30\n   1 14   8   9   2    nonaff     neg       .  30\n   1 15   8   9   1    nonaff     het       .  30\n   1 16  10  11   1    nonaff     het       .  30\n"})}),"\n",(0,i.jsx)("img",{src:s(5093).A,style:{maxHeight:"350px"}}),"\n",(0,i.jsx)(n.p,{children:"In brief, there are four affected family members with disease onset\noccurring between the ages of 30 and 60 years, all of whom are carriers.\nAdditionally, five unaffected individuals have been tested, revealing\nthat two individuals from the youngest generation (aged 30 years) are\ncarriers. Moreover, the proband\u2019s grandfather is likely an obligate\ncarrier, although he remained asymptomatic until his death in his 40s."}),"\n",(0,i.jsx)(n.h2,{id:"a-single-liability-class",children:"A single liability class"}),"\n",(0,i.jsxs)(n.p,{children:["The simplest way to analyze these data is to consider a single risk\nparameter for noncarriers (phenocopy rate) and another for carriers\n(penetrance). This can be achieved by switching the mode to\n",(0,i.jsx)(n.code,{children:"Liability class"})," and then setting a single liability class, which will\napply to all cases regardless of phenotype, sex, and age. For example,\nthe following will set a phenocopy rate of 0.001 and a penetrance of\n0.90, which may be viewed as somewhat conservative values."]}),"\n",(0,i.jsx)("img",{src:s(5129).A,style:{maxHeight:"150px"}}),"\n",(0,i.jsxs)(n.p,{children:["After completing these steps, the ",(0,i.jsx)(n.em,{children:"FLB"})," panel becomes available. Upon\nclicking on ",(0,i.jsx)(n.strong,{children:"Calculate"}),", a value of 3.2 is generated, indicating\ninconclusive evidence regarding the variant\u2019s pathogenicity (FLB < 8)."]}),"\n",(0,i.jsx)("img",{src:s(2324).A,style:{maxHeight:"150px"}}),"\n",(0,i.jsxs)(n.p,{children:["Now, let\u2019s conduct a sensitivity analysis to assess the robustness of\nthis result. Click on ",(0,i.jsx)(n.strong,{children:"Sensitivity"})," and select ",(0,i.jsx)(n.code,{children:"het/hom risk"}),"\n(penetrance) as the first parameter and ",(0,i.jsx)(n.code,{children:"neg risk"})," (phenocopy rate) as\nthe second. Adjust the ranges of values to test to ",(0,i.jsx)(n.code,{children:"0.5\u20130.9999"})," and\n",(0,i.jsx)(n.code,{children:"0.0001\u20130.10"}),", respectively, and then click on ",(0,i.jsx)(n.strong,{children:"Plot"}),"."]}),"\n",(0,i.jsx)("img",{src:s(6350).A,style:{maxHeight:"350px"}}),"\n",(0,i.jsx)(n.p,{children:"We can observe that reducing the carriers\u2019 risk enhances the evidence,\nprimarily due to the presence of unaffected carriers. However, it\u2019s\nimportant to note that in this case, these carriers are also young.\nAccounting for this fact may significantly increase the FLB value."}),"\n",(0,i.jsx)(n.h2,{id:"more-liability-classes",children:"More liability classes"}),"\n",(0,i.jsxs)(n.p,{children:["The easiest method to achieve this is by adding more liability classes.\nThis can be accomplished effortlessly by clicking on ",(0,i.jsx)(n.strong,{children:"ADD"})," and\ncompleting the new rows in the liability class table. For instance, the\nfollowing configuration specifies that the penetrance in individuals\naged 1-40 will be 0.45, which is half the value for the older\nindividuals."]}),"\n",(0,i.jsx)("img",{src:s(3538).A,style:{maxHeight:"175px"}}),"\n",(0,i.jsxs)(n.p,{children:["With this simple adjustment, the FLB jumps to 34.3, indicating strong\nevidence for pathogenicity (FLB > 32). However, conducting a\nsensitivity analysis by comparing the penetrances in the young\n(",(0,i.jsx)(n.code,{children:"1-40 | het/hom risk"}),") and old (",(0,i.jsx)(n.code,{children:"41-100 | het/hom risk"}),") age groups\nreveals that this result is not robust."]}),"\n",(0,i.jsx)("img",{src:s(9381).A,style:{maxHeight:"500px"}}),"\n",(0,i.jsx)(n.p,{children:"It\u2019s possible that different conclusions may arise if we implement a\nmore detailed age group classification. However, doing so makes\nsensitivity analyses increasingly cumbersome, as each new liability\nclass introduces two additional parameters."}),"\n",(0,i.jsx)(n.h2,{id:"relative-risk-mode",children:"Relative risk mode"}),"\n",(0,i.jsxs)(n.p,{children:["Alternatively, we can revert to the default ",(0,i.jsx)(n.code,{children:"Relative risk"})," mode. This\nmode simplifies the specification of age-specific penetrances while\nensuring that modeling assumptions remain transparent and manageable\nwithin a reasonable number of parameters."]}),"\n",(0,i.jsxs)(n.p,{children:["For instance, in the following model, we define lifetime risks of 0.001\nfor non-carriers (",(0,i.jsx)(n.code,{children:"neg risk"}),") and 0.90 for carriers (",(0,i.jsx)(n.code,{children:"het/hom risk"}),").\nAdditionally, we specify that phenocopies have an age of onset around\n60\xb115 years of age (",(0,i.jsx)(n.code,{children:"neg mean"})," and ",(0,i.jsx)(n.code,{children:"neg SD"}),"). The last parameter,\n",(0,i.jsx)(n.code,{children:"hazard ratio(s)"}),", is automatically updated by the app. Since it\ncontains a single value, it implies that we are assuming a constant\nrelative risk model. The plots below illustrate how these inputs\ntranslate to the expected cumulative incidences in carriers (solid\nlines) and non-carriers (dashed lines) of the variant."]}),"\n",(0,i.jsx)("img",{src:s(1851).A,style:{maxHeight:"400px"}}),"\n",(0,i.jsx)(n.p,{children:"With the updated parameters, we now obtain an FLB score of 30.3,\nindicating evidence for pathogenicity (FLB > 16). Conducting a\nsensitivity analysis comparing both lifetime risks demonstrates that\nthis conclusion is robust."}),"\n",(0,i.jsx)("img",{src:s(9804).A,style:{maxHeight:"500px"}})]})}function h(e={}){const{wrapper:n}={...(0,a.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(d,{...e})}):d(e)}},2324:(e,n,s)=>{s.d(n,{A:()=>i});const i=s.p+"assets/images/ex1-lclass1-flb-3c24f29d3901ebf041c21972fbf96690.png"},6350:(e,n,s)=>{s.d(n,{A:()=>i});const i=s.p+"assets/images/ex1-lclass1-sen-ffe8b741ee4b8f94766db1c3b4e199c8.png"},5129:(e,n,s)=>{s.d(n,{A:()=>i});const i=s.p+"assets/images/ex1-lclass1-tab-5ea9b16bab115fc28427fd27621b0fec.png"},9381:(e,n,s)=>{s.d(n,{A:()=>i});const i=s.p+"assets/images/ex1-lclass2-sen-ceda231ef2be9bb45d7c5e492e44d7c1.png"},3538:(e,n,s)=>{s.d(n,{A:()=>i});const i=s.p+"assets/images/ex1-lclass2-tab-308ecc1fa7fd26b1b74a8dd170ee4889.png"},5093:(e,n,s)=>{s.d(n,{A:()=>i});const i=s.p+"assets/images/ex1-ped-2ed31045b32515bc1112192d6627dd2d.png"},9804:(e,n,s)=>{s.d(n,{A:()=>i});const i=s.p+"assets/images/ex1-rrisk-sen-c8979969e13e65fc5e08ea805dbab36d.png"},1851:(e,n,s)=>{s.d(n,{A:()=>i});const i=s.p+"assets/images/ex1-rrisk-tab-29568c907c4b1b19f6949d4a1adbf18a.png"},8453:(e,n,s)=>{s.d(n,{R:()=>r,x:()=>o});var i=s(6540);const a={},t=i.createContext(a);function r(e){const n=i.useContext(t);return i.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function o(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:r(e.components),i.createElement(t.Provider,{value:n},e.children)}}}]);