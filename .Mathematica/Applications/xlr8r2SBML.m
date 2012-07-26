(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



BeginPackage["xlr8r2SBML`"];


(* turn off message writing - don't echo a message if you have to load mathsbml or xlr8r *) 

If[Length[Names["FLAGS`ECHOLOAD"]]>0,
saveflag=ToExpression["FLAGS`ECHOLOAD"],
saveflag=True;
];
FLAGS`ECHOLOAD=False;

(* Load mathsbml if it is not already in memory *)

If[Length[Names["MathSBML`$MathSBML$Version"]]==0,
<<mathsbml.m;
]; 

(* Load xlr8r if it is not already in memory *)

If[Length[Names["xlr8r`$xlr8r$Version"]]==0,
<<xlr8r.m;
];

(* Put package-load echoing back into its previous mode *)
FLAGS`ECHOLOAD=saveflag;

(* clean up *)

Remove[saveflag];

(* set a flag to indicate whether or not some version of this package has already been loaded. This flag will again be checked at the very end of loading the package *)

versions=Names["xlr8r2SBMLVersion"];

Off[TEMP$OLDVERSION::"shdw"];
Off[TEMP$FIRST::"shdw"];
If[Length[versions]>0,
TEMP$OLDVERSION=xlr8r2SBMLVersion; 
TEMP$FIRST=False;,
TEMP$FIRST=True];
Remove[versions];

(* THE FOLLOWING FLAG SHOULD UNIQUELY IDENTIFY THIS S/W VERSION AND SHOULD BE CHANGED EACH TIME THE PROGRAM IS MODIFIED *)
 
xlr8r2SBMLVersion="0.2 alpha (02 July 2007)";




convertToSBML::usage=" convertToSBML[network, ic, rates, id, name, compartment]\n\nwhere\n\nnetwork = an xlr8r network, identical to input to interpret\nic = initial conditions in format {x1\[Rule]value1, x2\[Rule]value2,...}\nrates = list of constants, e.g., {a\[Rule]23, b\[Rule]4, ...}\nid = desired SBML model id\nname = desired SBML model name\ncompartment = desired SBML compartment id";


Begin["`Private`"];


(* ::Input:: *)
convertToSBML[network_,initialConditions_, rates_, modelid_, modelname_, compartmentname_]:= Module[{debug=True,low,addNextReaction,mybind,nosubscript,kinlaw, handleMassAction,handleMM,handleHill,handleGRN,handleSSystem,handleNHCA,handleGMWC,species,sids,svalues,rids,rvalues, noEmptySet, notes},

(* Handles formation of intermediate complexes *)
mybind[A_,B_]:=Module[{names,name},names=Sort[{A,B}];
name=ToString[names[[1]]]<>"\[UnderBracket]"<>ToString[names[[2]]];
Return[Symbol[name]];
];(* End myBind[] *)

(* Handles cases of subscripts *)
nosubscript[A_,B_]:=Module[{names,name},
names={A,B};
name=ToString[names[[1]]]<>"\[UnderBracket]"<>ToString[names[[2]]];
Return[Symbol[name]];
];(* End noSubscript[] *)

kinlaw[str_]:=Module[{fst,lefths,pntr,tokenlist,tokenhold,const,kl},

fst=First[str];
fst=fst/.{ShortRightArrow->Rule};
fst=ToString[fst];
lefths=StringDrop[fst,{First[First[StringPosition[fst,"->"]]],StringLength[fst]}];

pntr=1;
tokenlist=StringSplit[lefths];

While[pntr<Length[tokenlist],

If[DigitQ[Part[tokenlist,pntr]],

tokenhold=Part[tokenlist,pntr];
tokenlist=Drop[tokenlist,{pntr}];
tokenlist=Insert[tokenlist,tokenhold,pntr+1];
tokenlist=Insert[tokenlist,"^",pntr+1];
pntr=pntr+3;,

If[Part[tokenlist,pntr]=="+",(* Else If case *)

tokenlist=ReplacePart[tokenlist,"*",pntr];
pntr++;,

pntr++;(* Else case *)

](* End Else If case *)
];(* End If *)
];(* End While *)

fst=StringJoin[tokenlist];
const=ToString[Last[str]];
kl=StringJoin[const,"*",fst];
kl=ToExpression[kl];

Return[kl];(* Returning kinetic law *)

];(* End kinLaw[] *)

handleMassAction[reaction_]:=
Module[{kLaw,react},

(* Getting kinetic law formula for the current reaction from list *) 
kLaw=kinlaw[reaction];

(* Editing the current reaction to fit the SBML spec
 This will turn the xCellerator Mass Action arrow into the SBML accepted Rule arrow *)
react=First[reaction]/.{ShortRightArrow->Rule}; 

(* Adding the current reaction to the model *)
MathSBML`addReaction[react, Global`kineticLaw->kLaw];

];(* End handleMassAction[] *)

handleMM[reaction_]:=
Module[{odes,kLaw,react,modi,reactprod,reactant,product},

(* A list of kinetic laws for the reaction *)
odes=xlr8r`Private`parseArrowForm[reaction]; 
odes=odes/.{xlr8r`Bind->mybind};
(* Removing any cases of 0's *)
odes=Select[odes, FreeQ[#,0]&];(* Getting kinetic law formula for the current reaction from list *) 
kLaw=Last[First[odes]];

(* Adjusting xCellerator formula to fit SBML spec *)
(* This will get rid of xCellerator's [t]'s *)
kLaw=ToExpression[StringReplace[ToString[InputForm[kLaw]],{"[t]"-> ""}]];
(* Removing negative if applicable *)
If[First[kLaw]<0,
kLaw=kLaw*-1;
];

react=First[reaction]; 

If[StringCount[ToString[FullForm[react]],"Overscript"] >=1,

(* If overscript Michaelis-Menten arrow *)

modi=Last[react];
reactprod=First[react];
reactant=First[reactprod];
product = Last[reactprod];

(* Editing the current reaction to fit the SBML spec
 This will turn the xCellerator Michaelis-Menten arrow into the SBML accepted Rule arrow *)
react=First[react]/.{DoubleLongRightArrow->Rule, DoubleLongLeftRightArrow->Rule}; (* Getting rid of overscripts *)


(* Adding the current reaction to the model *)
MathSBML`addReaction[react, Global`kineticLaw->kLaw, Global`modifiers->{modi},Global`products->{product},Global`reactants->{reactant} ];

,(* Else no overscript Michaelis-Menten arrow *)

(* Finding modifiers, products, and reactants *)
reactant=First[react];
product=Last[react];

(* Editing the current reaction to fit the SBML spec
 This will turn the xCellerator Michaelis-Menten arrow into the SBML accepted Rule arrow *)
react=react/.{DoubleLongRightArrow->Rule};


(* Adding the current reaction to the model *)
MathSBML`addReaction[react, Global`kineticLaw->kLaw, Global`products->{product},Global`reactants->{reactant} ];

];(* End If[] *)

];(* End handleMM[] *)

handleHill[reaction_]:=
Module[{odes,kLaw,reactant,modi,product },

(* A list of kinetic laws for the reaction *)
odes=xlr8r`Private`parseArrowForm[reaction]; 
odes=odes/.{xlr8r`Bind->mybind};
(* Removing any cases of 0's *)
odes=Select[odes, FreeQ[#,0]&];(* Getting kinetic law formula for the current reaction from list *) 
kLaw=Last[First[odes]];

(* Adjusting xCellerator formula to fit SBML spec *)
(* This will get rid of xCellerator's [t]'s *)
kLaw=ToExpression[StringReplace[ToString[InputForm[kLaw]],{"[t]"-> ""}]];
(* Removing negative if applicable *)
	If[First[kLaw]<0,
		kLaw=kLaw*-1;
	];

react=First[reaction];

If[StringCount[ToString[FullForm[react]],"Overscript"] >=1,

(* If Catalytic Hill reaction *)

(* Finding modifiers, products, and reactants *)
(* Reactants *)
reactant=First[First[react]];

If[StringCount[ToString[reactant], ","]>=1,(* If {A1, A2, A3} case *)

reactant=reactant;

,(* Else *)

If[StringCount[ToString[reactant], "+"]>=1,(* If A1+A2+A3 case *)

reactant=StringReplace[ToString[reactant],"+"-> ","];
reactant=StringInsert[reactant, "{",1];
reactant=ToExpression[reactant=StringInsert[reactant,"}", StringLength[reactant]+1]];

,(* Else *)

If[StringCount[ToString[reactant], "{"]>=1,(* If {A1 A2 A3} case *)

reactant=ToExpression[StringReplace[ToString[reactant]," "->","]]

,(* Else *)

If[StringCount[ToString[FullForm[reactant]], "Times["]>=1, (* If A1 A2 A3 case *)

reactant=ToString[FullForm[reactant]];
reactant=StringReplace[ToString[reactant],"Times["->"{"];
reactant=StringReplace[ToString[reactant],"]"->"}"];
reactant=ToExpression[reactant];,(* Else A case *)

reactant=reactant;

];(* End A1 A2 A3 If[] *)

];(* End {A1 A2 A3} If[] *)

];(* End A1+A2+A3 If[] *)

];(* End {A1, A2, A3} If[] *)

(* Modifiers *)
modi=Last[react];

(* Editing the current reaction to fit the SBML spec
 This will turn the xCellerator Hill arrow into the SBML accepted Rule arrow *)
react=First[react]/.{RightTeeArrow->Rule}; (* Getting rid of overscripts *)

(* Products *)
product=Last[react];

(* Adding the current reaction to the model *)
MathSBML`addReaction[react, Global`kineticLaw->kLaw, Global`modifiers->{modi},Global`products->{product}, Global`reactants->{reactant}];

,(* Else Regulatory Hill reaction *)

(* Finding Reg Hill case *)

(* If {{A1\[RightTeeArrow]B,hill[...]},{A2\[RightTeeArrow]B,hill[...]},...} case *)
If[StringCount[ToString[reaction], "hill"]>= 2,

For[i=1,i<=Length[reaction],i++,

react=Part[reaction,i];
react=First[react];
modi=First[react];
product=Last[react];

(* Editing the current reaction to fit the SBML spec
 This will turn the xCellerator Hill arrow into the SBML accepted Rule arrow *)
react=First[react]/.{RightTeeArrow->Rule}; (* Getting rid of overscripts *)

(* Adding the current reaction to the model *)
MathSBML`addReaction[react, Global`kineticLaw->kLaw, Global`modifiers->{modi},Global`products->{product}];

];(* End For loop *)

,(* Else If {{A1,A2,...}\[RightTeeArrow]B,hill[...]} case *)
If[StringCount[ToString[First[reaction]],"{"]==1,

react=First[reaction];
modilist=First[react];
product=Last[react];

For[i=1,i<=Length[modilist],i++,

modi=modilist;
react=ToExpression[ToString[modi]<>"\[Rule]"<>ToString[product]];

(* Adding the current reaction to the model *)
MathSBML`addReaction[react,Global`kineticLaw->kLaw, Global`modifiers->{modi},Global`products->{product}];

];(* End For loop *)

,(* Else {A\[RightTeeArrow]B,hill[...]} case *)

react=First[reaction];
modi=First[react];
product=Last[react];

(* Editing the current reaction to fit the SBML spec *)
 (* This will turn the xCellerator Hill arrow into the SBML accepted Rule arrow *)
react=react/.{RightTeeArrow->Rule};

(* Adding the current reaction to the model *)
MathSBML`addReaction[react, Global`kineticLaw->kLaw, Global`modifiers->{modi},Global`products->{product}];

];(* End {{A1,A2,...}\[RightTeeArrow]B,hill[...]} If *)

];(* End {{A1\[RightTeeArrow]B,hill[...]},{A2\[RightTeeArrow]B,hill[...]},...} If *)

];

];(* End handleHill[] *)

handleGRN[reaction_]:=
Module[{odes,kLaw,react,modi,product,reactant},

(* A list of kinetic laws for the reaction *)
odes=xlr8r`Private`parseArrowForm[reaction]; 
odes=odes/.{xlr8r`Bind->mybind};
(* Removing any cases of 0's *)
odes=Select[odes, FreeQ[#,0]&];(* Getting kinetic law formula for the current reaction from list *) 
kLaw=Last[First[odes]];

(* Adjusting xCellerator formula to fit SBML spec *)
(* This will get rid of xCellerator's [t]'s *)
kLaw=ToExpression[StringReplace[ToString[InputForm[kLaw]],{"[t]"-> ""}]];
(* Removing negative if applicable *)
If[First[kLaw]<0,
kLaw=kLaw*-1;
];

Print["The kinetic law for this reaction is: ",kLaw]; (* Print *)

react=First[reaction];

(* Finding modifiers, products, and reactants *)

modi=First[First[reaction]];

If[StringCount[ToString[modi], "+" ]>=1,(* If A1+A2+A3 case *)

modi=StringReplace[ToString[modi],"+"-> ","];
modi=StringInsert[modi, "{",1];
ToExpression[modi=StringInsert[modi,"}", StringLength[modi]+1]];

];

product=Last[react];
reactant=Last[reaction];

(* GRN[] -> { } *)
reactant=ToExpression[StringReplace[ToString[reactant], {"GRN["-> "{", "]"-> "}"}]];
reactant=Complement[reactant,rids]; (* Taking out parameters *)


(* Editing the current reaction to fit the SBML spec
 This will turn the xCellerator GRN arrow into the SBML accepted Rule arrow *)
react=First[reaction];/.{RightTeeArrow->Rule};

(* Adding the current reaction to the model *)
MathSBML`addReaction[react, Global`kineticLaw->kLaw, Global`modifiers->{modi},Global`products->{product},Global`reactants->{reactant} ];

];(* End handleGRN[] *)

handleSSystem[reaction_]:=
Module[{odes,kLaw,react,modi,product,reactant},

(* A list of kinetic laws for the reaction *)
odes=xlr8r`Private`parseArrowForm[reaction]; 
odes=odes/.{xlr8r`Bind->mybind};
(* Removing any cases of 0's *)
odes=Select[odes, FreeQ[#,0]&];(* Getting kinetic law formula for the current reaction from list *) 
kLaw=Last[First[odes]];

(* Adjusting xCellerator formula to fit SBML spec *)
(* This will get rid of xCellerator's [t]'s *)
kLaw=ToExpression[StringReplace[ToString[InputForm[kLaw]],{"[t]"-> ""}]];

react=First[reaction];

(* Finding modifiers, products, and reactants *)
modi=First[First[reaction]];

If[StringCount[ToString[modi], "+" ]>=1,(* If A1+A2+A3 case *)

modi=StringReplace[ToString[modi],"+"-> ","];
modi=StringInsert[modi, "{",1];
modi=ToExpression[modi=StringInsert[modi,"}", StringLength[modi]+1]];

];

product=Last[react];
reactant=Last[reaction];

(* SSystem[] -> { } *)
reactant=ToExpression[StringReplace[ToString[reactant], {"SSystem["-> "{", "]"-> "}"}]];
reactant=Flatten[reactant];
reactant=Complement[reactant,rids]; (* Taking out parameters *)

(* Editing the current reaction to fit the SBML spec
 This will turn the xCellerator S-System arrow into the SBML accepted Rule arrow *)
react=ToExpression[StringReplace[ToString[react],{","-> "+","{"->"","}"->""}]];
react=react/.{RightTeeArrow->Rule};

(* Adding the current reaction to the model *)
MathSBML`addReaction[react, Global`kineticLaw->kLaw, Global`modifiers->{modi},Global`products->{product},Global`reactants->{reactant} ];

];(* End handleSSystem[] *)

handleNHCA[reaction_]:=
Module[{odes,kLaw,react,modi,product,reactant},

(* A list of kinetic laws for the reaction *)
odes=xlr8r`Private`parseArrowForm[reaction]; 
odes=odes/.{xlr8r`Bind->mybind};
(* Removing any cases of 0's *)
odes=Select[odes, FreeQ[#,0]&];(* Getting kinetic law formula for the current reaction from list *) 
kLaw=Last[First[odes]];

(* Adjusting xCellerator formula to fit SBML spec *)
(* This will get rid of xCellerator's [t]'s *)
kLaw=ToExpression[StringReplace[ToString[InputForm[kLaw]],{"[t]"-> ""}]];

react=First[reaction];

(* Finding modifiers, products, and reactants *)
modi=First[First[reaction]];

If[StringCount[ToString[modi], "+" ]>=1,(* If A1+A2+A3 case *)

modi=StringReplace[ToString[modi],"+"-> ","];
modi=StringInsert[modi, "{",1];
modi=ToExpression[modi=StringInsert[modi,"}", StringLength[modi]+1]];

];

product=Last[react];

reactant=Last[reaction];

(* NHCA[] -> { } *)
reactant=ToExpression[StringReplace[ToString[reactant], {"NHCA["-> "{", "]"-> "}"}]];
reactant=Flatten[reactant];
reactant=Complement[reactant,rids]; (* Taking out parameters *)
reactant=Flatten[reactant];

If[StringCount[ToString[react],","]>=1, (* {A1,A2,A3} case *)

react=ToExpression[StringReplace[ToString[react], {"{"-> "","}"->"",","-> "+"}]];

];

(* Editing the current reaction to fit the SBML spec
 This will turn the xCellerator NHCA arrow into the SBML accepted Rule arrow *)
react=react/.{RightTeeArrow->Rule};

(* Adding the current reaction to the model *)
MathSBML`addReaction[react, Global`kineticLaw->kLaw, Global`modifiers->{modi},Global`products->{product},Global`reactants->{reactant} ];

];(* End handleNHCA[] *)

handleGMWC[reaction_]:=
Module[{odes,kLaw,react,modi,product,reactant},

(* A list of kinetic laws for the reaction *)
odes=xlr8r`Private`parseArrowForm[reaction]; 
odes=odes/.{xlr8r`Bind->mybind};
(* Removing any cases of 0's *)
odes=Select[odes, FreeQ[#,0]&];(* Getting kinetic law formula for the current reaction from list *) 
kLaw=Last[First[odes]];

(* Adjusting xCellerator formula to fit SBML spec *)
(* This will get rid of xCellerator's [t]'s *)
kLaw=ToExpression[StringReplace[ToString[InputForm[kLaw]],{"[t]"-> ""}]];

(* Removing negative if applicable *)
If[First[kLaw]<0,
kLaw=kLaw*-1;
];

(* Editing the current reaction to fit the SBML spec
 This will turn the xCellerator MWC/GMWC arrow into the SBML accepted Rule arrow *)
react=First[reaction];

(* Finding modifiers, products, and reactants *)
(* Modifiers *)
modi=Last[react];
modi=Union[{modi},Last[First[react]]];
modi=Flatten[modi];

(* Products *)
product=Last[First[First[react]]];

(* Reactants *)
reactant=First[First[First[react]]];

react=First[First[react]]/.{DoubleLongRightArrow->Rule}; (* Getting rid of overscripts *)

(* Adding the current reaction to the model *)
MathSBML`addReaction[react,Global`kineticLaw->kLaw, Global`modifiers->{modi},Global`products->{product},Global`reactants->{reactant} ];

];(* End handleGMWC[] *)

addNextReaction[reaction_]:= Module[{odes},

odes=xlr8r`interpret[{reaction}]; 
odes=odes/.{xlr8r`Bind->mybind};



(* If Mass Action reaction *)
If[StringCount[ToString[reaction], "\[ShortRightArrow]"]>=1,

handleMassAction[reaction];

,(* Else If Michaelis-Menten reaction *)

If[StringCount[ToString[reaction], "MM"]>=1,

handleMM[reaction];

,(* Else If Hill reaction *)

If[StringCount[ToString[reaction], {"hill","Hill"}]>=1,

handleHill[reaction];

,(* Else If GRN reaction *)

If[StringCount[ToString[reaction], {"GRN", "grn"}]>=1,

handleGRN[reaction];

,(* Else If S-System reaction *)

If[StringCount[ToString[reaction], "SSystem"]>=1,

handleSSystem[reaction];

,(* Else If NHCA reaction *)

If[StringCount[ToString[reaction], "NHCA"]>=1,

handleNHCA[reaction];

,(* Else If GMWC reaction *)

If[StringCount[ToString[reaction], {"GMWC"}]>=1,

handleGMWC[reaction];

];(* End GMWC If[] *)

];(* End NHCA If[] *)

];(* End S-System If[] *)

];(* End GRN If[] *)

];(* End Hill If[] *)

];(* End Michaelis-Menten If[] *)

]; (* End Mass Action If[] *)

];(* End addNextReaction[] *)

low=xlr8r`lowLevelReactions[network];
low = low/.{xlr8r`Bind->mybind};
low=low/.{Subscript->nosubscript};
low=low/.{Global`\[EmptySet]-> Global`\[UnderBracket]EmptySet};

(* Initialize MathSBML *)
notes= "This model was auto-generated from xlr8r reactions by xlr8r2SBML version "<>xlr8r2SBML`xlr8r2SBMLVersion<>" at "<>MathSBML`Private`now[]; 
MathSBML`newModel[modelid, Global`name->modelname,
Global`notes->notes ];
(* Creating compartment *)
MathSBML`addCompartment[compartmentname, Global`size-> 1];

rids=Map[First,rates];
rids=rids/.{Subscript->nosubscript};
rids=rids/.{\[EmptySet]-> Global`\[UnderBracket]EmptySet};

rvalues=Map[Last,rates];

For[i=1,i<=Length[rates],i++,
(* Adding parameters *)
MathSBML`addParameter[Global`id->Part[rids,i], Global`value->Part[rvalues,i]];
];

species = Last[xlr8r`interpret[network]];
species=species/.{xlr8r`Bind->mybind};

sids=Map[First, initialConditions];
sids=sids/.{Subscript->nosubscript};
sids=sids/.{\[EmptySet]-> Global`\[UnderBracket]EmptySet};

svalues=Map[Last, initialConditions];

newspecies=Complement[species,sids];

For[i=1, i<=Length[initialConditions],i++,
(* Adding species *)
MathSBML`addSpecies[Global`id->Part[sids,i],Global`initialConcentration->Part[svalues,i]];
];

For[i=1, i<=Length[newspecies],i++,
(* Adding new species *)
MathSBML`addSpecies[Global`id->Part[newspecies,i],Global`initialConcentration->0];
];

(* Adding reactions *)
Map[addNextReaction, low];

Return[MathSBML`createModel[]];

]


now=MathSBML`Private`now[];
If[Length[Names["FLAGS`ECHOLOAD"]]>0,
msgflag=ToExpression["FLAGS`ECHOLOAD"],
msgflag=True;
];


If[msgflag,
If[TEMP$FIRST \[Or] (!TEMP$FIRST \[And] !Equal[xlr8r2SBMLVersion, TEMP$OLDVERSION]),
Print["xlr8r2SBML Version "<>xlr8r2SBMLVersion<>" using Mathematica Version "<>$Version<>" loaded "<>now];,

Print["xlr8r2SBML Version "<>xlr8r2SBMLVersion<>" using Mathematica Version "<>$Version<>" reloaded "<>now];

];
];
If[!TEMP$FIRST \[And] !Equal[xlr8r2SBMLVersion, TEMP$OLDVERSION],Print[
Style["xlr8r2SBML: warning: loading a different version of this package may cause unexpected results.\ncurrent version: "<>xlr8r2SBMLVersion<>"\nprevious version:"<>TEMP$OLDVERSION,FontWeight-> Bold, FontColor-> Red]
]]; 

If[$VersionNumber<6, Print["Warning: This version of xlr8r2SBML requires Mathematica Version 6. You are using an earlier veriosn. Some translations will not work properly."]; 
]; 

Remove[msgflag];


End[];

Remove[TEMP$OLDVERSION];
Remove[TEMP$FIRST];

EndPackage[];
