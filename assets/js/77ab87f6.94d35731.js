"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[3],{7653:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>l,contentTitle:()=>i,default:()=>c,frontMatter:()=>o,metadata:()=>r,toc:()=>d});var s=t(4848),a=t(8453);const o={output:"docusaurus-md",sidebar_position:3,description:"An example with two phenotypes in a consanguineous family."},i="3) Two phenotypes {#two-phenotypes}",r={id:"examples/example3",title:"3) Two phenotypes",description:"An example with two phenotypes in a consanguineous family.",source:"@site/docs/examples/example3.md",sourceDirName:"examples",slug:"/examples/example3",permalink:"/shinyseg/examples/example3",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:3,frontMatter:{output:"docusaurus-md",sidebar_position:3,description:"An example with two phenotypes in a consanguineous family."},sidebar:"tutorialSidebar",previous:{title:"2) X-linked inheritance",permalink:"/shinyseg/examples/example2"},next:{title:"4) Breast cancer and BRCA1",permalink:"/shinyseg/examples/example4"}},l={},d=[{value:"Pedigree table",id:"pedigree-table",level:2},{value:"One phenotype",id:"one-phenotype",level:2},{value:"Two phenotypes",id:"two-phenotypes-1",level:2}];function p(e){const n={code:"code",h1:"h1",h2:"h2",p:"p",pre:"pre",strong:"strong",...(0,a.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(n.h1,{id:"two-phenotypes",children:"3) Two phenotypes"}),"\n",(0,s.jsx)(n.p,{children:"This example demonstrates how to incorporate multiple phenotypes into\nthe cosegregation analysis. It also showcases how the app can deal with\nconsanguineous families."}),"\n",(0,s.jsx)(n.h2,{id:"pedigree-table",children:"Pedigree table"}),"\n",(0,s.jsx)(n.p,{children:"The data corresponding to the case is shown below:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-text",children:" ped id fid mid sex phenotype carrier proband age\n   1  1   0   0   1         .       .       .  50\n   1  2   0   0   2         .       .       .  50\n   1  3   1   2   1         .       .       .  50\n   1  4   0   0   2      mild       .       .  50\n   1  5   1   2   1         .       .       .  50\n   1  6   0   0   2    nonaff       .       .  50\n   1  7   3   4   1    severe     het       .  50\n   1  8   0   0   2    nonaff       .       .  50\n   1  9   5   6   1      mild       .       .  50\n   1 10   0   0   2    nonaff       .       .  50\n   1 11   7   8   1    nonaff       .       .  50\n   1 12   9  10   2      mild     het       .  50\n   1 13  11  12   1    severe     het       1  50\n   1 14  11  12   2      mild     het       .  50\n   1 15  11  12   2    nonaff     neg       .  50\n"})}),"\n",(0,s.jsx)("img",{src:t(7575).A,style:{maxHeight:"350px"}}),"\n",(0,s.jsx)(n.p,{children:"There are two disease phenotypes in this family: one relatively mild and\ncommon and one more rare and severe. It is hypothesized that the rare\nautosomal variant found could be related to an increased risks for both."}),"\n",(0,s.jsx)(n.h2,{id:"one-phenotype",children:"One phenotype"}),"\n",(0,s.jsxs)(n.p,{children:["A basic approach would be to ignore this fact and treat both as the same\n\u201caffected\u201d phenotype. For instance, we may use a conservative phenocopy\nrate (",(0,s.jsx)(n.code,{children:"neg risk"}),") of 20% and a penetrance (",(0,s.jsx)(n.code,{children:"het/hom risk"}),") of 70% for\nall pedigree members:"]}),"\n",(0,s.jsx)("img",{src:t(9595).A,style:{maxHeight:"62px"}}),"\n",(0,s.jsx)(n.p,{children:"With this setup, we would obtain moderate evidence for pathogenicity, as\nshown below. The sensitivity analysis also indicates that reducing the\nphenocopy rate would greatly increase the evidence. However, doing so\nmay result in overestimation, since we know the mild phenotype is\nrelatively common."}),"\n",(0,s.jsx)("img",{src:t(8954).A,style:{maxHeight:"500px"}}),"\n",(0,s.jsx)(n.h2,{id:"two-phenotypes-1",children:"Two phenotypes"}),"\n",(0,s.jsxs)(n.p,{children:["Luckily, we can easily account for this fact by declaring a lower\nphenocopy rate for only the severe phenotype, let\u2019s say 1%. This can be\ndone by clicking on ",(0,s.jsx)(n.strong,{children:"ADD"})," to add more liability classes."]}),"\n",(0,s.jsx)("img",{src:t(4824).A,style:{maxHeight:"105px"}}),"\n",(0,s.jsx)(n.p,{children:"And with that, the FLB doubles to 41.5, resulting in strong evidence for\npathogenicity."}),"\n",(0,s.jsx)("img",{src:t(5169).A,style:{maxHeight:"100px"}})]})}function c(e={}){const{wrapper:n}={...(0,a.R)(),...e.components};return n?(0,s.jsx)(n,{...e,children:(0,s.jsx)(p,{...e})}):p(e)}},8954:(e,n,t)=>{t.d(n,{A:()=>s});const s=t.p+"assets/images/ex3-lclass1-flb-cbd46bd07436dddb79e397274a5a132d.png"},9595:(e,n,t)=>{t.d(n,{A:()=>s});const s="data:image/png;base64,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"},5169:(e,n,t)=>{t.d(n,{A:()=>s});const s=t.p+"assets/images/ex3-lclass2-flb-a1f992145c24674615d62f33c5d579ab.png"},4824:(e,n,t)=>{t.d(n,{A:()=>s});const s=t.p+"assets/images/ex3-lclass2-tab-a4241ff7a34a4151073487bd3939e734.png"},7575:(e,n,t)=>{t.d(n,{A:()=>s});const s=t.p+"assets/images/ex3-ped-61a161d4347a67f6434a054353fb43d4.png"},8453:(e,n,t)=>{t.d(n,{R:()=>i,x:()=>r});var s=t(6540);const a={},o=s.createContext(a);function i(e){const n=s.useContext(o);return s.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function r(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:i(e.components),s.createElement(o.Provider,{value:n},e.children)}}}]);