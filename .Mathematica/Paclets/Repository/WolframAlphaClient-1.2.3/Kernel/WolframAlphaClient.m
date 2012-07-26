(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package header*)


(* $Id: WolframAlphaClient.m,v 1.363.2.74 2012/02/06 20:13:27 lou Exp $ *)

(* :Summary: Support for using the Wolfram|Alpha webservice API from within Mathematica. *)

(* :Mathematica Version: Mathematica 8 *)

(* :Author: Louis J. D'Andria *)

(* :Keywords: *)

(* :Discussion: *)

(* :Warning: *)

(* :Sources: *)




Unprotect[{
	AlphaIntegration`AlphaQuery,
	AlphaIntegration`AlphaQueryInputs,
	AlphaIntegration`ExtrusionEvaluate,
	AlphaIntegration`FormatAlphaResults,
	AlphaIntegration`LinguisticAssistant,
	AlphaIntegration`LinguisticAssistantBoxes,
	System`Asynchronous,
	System`ExcludePods,
	System`IncludePods,
	System`InputAssumptions,
	System`NamespaceBox,
	System`PodStates,
	System`PodWidth,
	System`WolframAlpha,
	System`WolframAlphaDate,
	System`WolframAlphaQuantity,
	System`WolframAlphaResult}]
	

(* Safeguard against package reloading *)
Block[{list, msgs},
	list = {
		"AlphaIntegration`AlphaQuery",
		"AlphaIntegration`AlphaQueryInputs",
		"AlphaIntegration`ExtrusionEvaluate",
		"AlphaIntegration`FormatAlphaResults",
		"AlphaIntegration`LinguisticAssistant",
		"AlphaIntegration`LinguisticAssistantBoxes",
		"WolframAlpha",
		"WolframAlphaDate",
		"WolframAlphaQuantity",
		"WolframAlphaResult"
	};
	Unprotect @@ list;
	(* Preserve Messages[WolframAlpha], since they are defined elsewhere *)
	msgs = Messages[WolframAlpha];
	ClearAll @@ list;
	ClearAll["WolframAlphaClient`Private`*"];
	Messages[WolframAlpha] = msgs;
]


BeginPackage["WolframAlphaClient`"]


Begin["`Private`"];



(* ::Section::Closed:: *)
(*Implementation*)


(* ::Subsection::Closed:: *)
(*Managing FrontEnd preferences*)


(*
Preference settings managed by setPreference etc below are stored in the front end,
usually as a global option, that is, as an option to $FrontEnd. The options are
collectively stored in PrivateFrontEndOptions -> "WolframAlphaSettings" -> {...}

Since we want WolframAlpha[] to work even when a front end is not present, we do two
things. First, the set/get utilities themselves call Quiet to avoid the typical "you need
a front end to do that" message. Second, all the utilities that call these preference
management functions should make sure that the return value is sensible, and provide a
default in case it's not. An alternate solution would be to use UsingFrontEnd to guarantee
a front end is present, but that defeats one of the biggest benefits of working in a raw
kernel.
*)


dynamicPreference[opt_] := Dynamic[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}]]

dynamicPreference[opt_, default_] := Dynamic[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}, default]]

dynamicPreference[target_, opt_, default_] := Dynamic[CurrentValue[target, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}, default]]


getPreferences[] := Quiet[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings"}]]

getPreference[opt_] := Quiet[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}]]

getPreference[opt_, default_] := Quiet[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}, default]]

getPreference[target_, opt_, default_] := Quiet[CurrentValue[target, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}, default]]


setPreference[opt_, val_] := Quiet[CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}] = val]

setPreference[target_, opt_, val_] := Quiet[CurrentValue[target, {PrivateFrontEndOptions, "WolframAlphaSettings", opt}] = val]


clearPreferences[] := Quiet[
	CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings"}] = {};
	$AlphaQuerySendMathematicaSessionInfo = Automatic;
]

clearPreference[opt_] := setPreference[opt, Inherited]



cachedFrontEndResource[args__] := cachedFrontEndResource[args] = FrontEndResource[args]


(* ::Subsection::Closed:: *)
(*Global variables*)


$AlphaQueryClientLocation = System`Private`$InputFileName


$AlphaQueryBaseURLs = {
	"http://api.devel.wolframalpha.com/v1/" -> "Devel",
	"http://api.test.wolframalpha.com/v1/" -> "Test",
	"http://www.current.wolframalpha.com/api/v1/" -> "Current",
	"http://preview.wolframalpha.com/api/v1/" -> "Preview",
	"http://api.wolframalpha.com/v1/" -> "Production"
};


$AlphaQueryBaseURL[] := $AlphaQueryBaseURL[getPreference["BaseURL", "Automatic"]]

$AlphaQueryBaseURL[Automatic] := $AlphaQueryBaseURL[getPreference["BaseURL", "Automatic"]]

$AlphaQueryBaseURL[url_] :=
	Block[{baseurl},
		baseurl = If[StringQ[url], url, "production"];
		baseurl = Switch[ ToLowerCase[baseurl],
				"devel", "Devel" /. Map[Reverse, $AlphaQueryBaseURLs],
				"test", "Test" /. Map[Reverse, $AlphaQueryBaseURLs],
				"current", "Current" /. Map[Reverse, $AlphaQueryBaseURLs],
				"preview", "Preview" /. Map[Reverse, $AlphaQueryBaseURLs],
				"production" | "public" | "api" | "automatic", "Production" /. Map[Reverse, $AlphaQueryBaseURLs],
				_, baseurl (* any other string is assumed to be valid base url *)
		]
	];


$AlphaQueryJSP = "query.jsp";


$AlphaParseJSP = "mparse.jsp";


$AlphaValidateJSP = "validatequery";


$AlphaQueryAppID[] := With[{id = getPreference["AppID", Automatic]}, If[StringQ[id] && id =!= "Automatic", id, Automatic]];


$AlphaQueryMClient = "2";


$AlphaQueryMMode = "function";


$AlphaQueryPrefixQ = False;


$AlphaQueryPrefix = "devmode: ";


$AllAlphaQueryAppearanceElements = {"Extrusion", "Warnings", "CDFWarnings", "Assumptions", "CDFAssumptions", "Brand", "CDFBrand", "ContentPadding", "Pods", "UseInput", "UseInputLeft", "TearOffButtons", "PodMenus", "CDFPodMenus", "PodCornerCurl", "SubpodMenus", "Unsuccessful", "Sources"};


$AutomaticAlphaQueryAppearanceElements = {"Warnings", "Assumptions", "Brand", "Pods", "PodMenus", "Unsuccessful", "Sources"};


$AlphaQueryAppearanceElements = $AutomaticAlphaQueryAppearanceElements;


(* Disable these interfaces, now that W|A can return interfaces when given interactive=true *)
(*
$AlphaQueryInterfaceAddedToMInput["N"] = "Manipulate"
$AlphaQueryInterfaceAddedToMInput["ContinuedFraction"] = "Manipulate"
$AlphaQueryInterfaceAddedToMInput["Series"] = "Manipulate"
$AlphaQueryInterfaceAddedToMInput["Plot"] = "Explore"
$AlphaQueryInterfaceAddedToMInput["Plot3D"] = "Manipulate"
$AlphaQueryInterfaceAddedToMInput["ContourPlot"] = "Manipulate"
*)


$AlphaQueryAsync[] := With[{async = getPreference["Asynchronous", True]}, If[MatchQ[async, True | False | All | _?NumericQ], async, True]]


$AlphaQueryInteractive[] := With[{int = getPreference["Interactive", True]}, If[MemberQ[{True, False}, int], int, True]];


$AlphaQuerySimpleRecalculate[] := With[{re = getPreference["Recalculate", True]}, If[MemberQ[{True, False}, re], re, True]];


$AlphaQueryRecalculateSpace = 30;


$AlphaQueryReinterpret[] := With[{re = getPreference["Reinterpret", True]}, If[MemberQ[{True, False}, re], re, True]];


$AlphaQueryFormats = {"cell", "minput", "msound", "dataformats"};


$AlphaQueryDataFormats = {"ComputableData", "FormattedData", "FormulaData", "NumberData", "QuantityData", "SoundData", "TimeSeriesData"};


$AlphaQueryTextFormats = {"Plaintext", "Input", "Output"};


$AlphaQueryPodWidth = Automatic;


$AlphaQueryExtrusionShowMOutputs = True;


$AlphaQueryExtrusionClickClose[] := With[{close = getPreference["ExtrusionClickClose", False]}, If[MemberQ[{True, False}, close], close, False]];


$AlphaQueryExtrusionClickEvaluate[] := With[{eval = getPreference["ExtrusionClickEvaluate", False]}, If[MemberQ[{True, False}, eval], eval, False]];


$AlphaQueryExtrusionEvaluateCachedMForms = True;


$AlphaQueryExtrusionWidth = 560;


$AlphaQueryQuiet = Automatic;


$AlphaQueryLogNotebook = Automatic;


$AlphaQueryAsynchoronousTimeout = Automatic;


$AlphaQueryTimeConstraint = 30;


$AlphaQueryScanTimeout = Automatic;


$AlphaQueryPodTimeout = Automatic;


$AlphaQueryFormatTimeout = Automatic;


$AlphaQuerySendMathematicaSessionInfo = Automatic;


$AlphaQueryShowLinksByDefault = True;


$AlphaQueryAdditionalInformation = False;


$AlphaQueryGlobalSendMathematicaSessionInfo[] :=
	With[{send = getPreference[$FrontEnd, "SendMathematicaSessionInfo", Automatic]}, If[MemberQ[{True, False}, send], send, Automatic] ];


$AlphaQueryRawParameters = {};


$AlphaQueryMathematicaFormsFormatType[] := 
	With[{fmt = getPreference["MathematicaFormsFormatType", "MostlyInputForm"]},
		If[MemberQ[{"InputForm", "StandardForm", "TraditionalForm"}, fmt], fmt, "MostlyInputForm"] ]




$MostlyInputFormTypesetHeads = "Placeholder" | "Out" | "WolframAlphaClient`Private`rawCompressedBoxes";


$MostlyInputFormTypesetInnerHeads = "Graphics" | "Graphics3D";


$MostlyInputFormTypesetInnerThreshhold = 15000;


SetAttributes[AlphaQueryMakeBoxes, HoldAllComplete];

AlphaQueryMakeBoxes[expr_] := With[{fmt = $AlphaQueryMathematicaFormsFormatType[]}, AlphaQueryMakeBoxes[expr, fmt]]

AlphaQueryMakeBoxes[expr_, fmt:("InputForm" | "StandardForm" | "TraditionalForm" | "MostlyInputForm")] :=
	alphaQueryMakeBoxes[HoldComplete[expr] /.
		(* Contractually, Uncompress expressions should always be expanded during formatting *)
		HoldPattern[Uncompress][a_String] :> Block[{}, Uncompress[a] /; True] /.
		(*
			This rule makes Images renderable in typeset forms, including MostlyInputForm.
			The rawCompressedBoxes wrapper is used so that the result of evaluating the Image
			will survive the coercion to an InputForm string in MostlyInputForm, below.
		*)
		If[fmt === "InputForm",
			{},
			HoldPattern[Image][a_, b___] /; Or[
				MatchQ[Unevaluated[a], _RawArray],
				MatrixQ[Unevaluated[a]] && Depth[Unevaluated[a]] == 3 && FreeQ[Unevaluated[a], Except[List, _Symbol], {2}]
			] :> With[{image = rawCompressedBoxes[Compress[ToBoxes[Image[a, b]]]]}, image /; True]
		],
		fmt
	]

AlphaQueryMakeBoxes[expr_, other_] := AlphaQueryMakeBoxes[expr, "InputForm"]


alphaQueryMakeBoxes[HoldComplete[expr_], "InputForm"] := ToString[Unevaluated[expr], InputForm]
alphaQueryMakeBoxes[HoldComplete[expr_], "StandardForm"] := BoxForm`MakeBoxesWithTextFormatting[expr, StandardForm]
alphaQueryMakeBoxes[HoldComplete[expr_], "TraditionalForm"] := BoxForm`MakeBoxesWithTextFormatting[expr, TraditionalForm]
alphaQueryMakeBoxes[HoldComplete[expr_], "MostlyInputForm"] := 
	MathLink`CallFrontEnd[FrontEnd`ReparseBoxStructurePacket[ToString[Unevaluated[expr], InputForm]]] /. {
		(* Some exprs should always be typeset as if they were in StandardForm *)
		boxes:RowBox[{$MostlyInputFormTypesetHeads, "[", _, "]"}] :>
			Replace[BoxForm`ConvertForm[boxes, StandardForm, StandardForm, "Output"], BoxData[{a_}, ___] :> a],
		(* Other exprs should be typeset only if they are inside of something else, and even then only if they are sufficienty verbose *)
		box_[x___, gboxes:RowBox[{$MostlyInputFormTypesetInnerHeads, "[", _, "]"}], y___] /; (ByteCount[gboxes] > $MostlyInputFormTypesetInnerThreshhold) :>
			box[x, Replace[BoxForm`ConvertForm[gboxes, StandardForm, StandardForm, "Output"], BoxData[{a_}, ___] :> a], y]
	}


rawCompressedBoxes /: MakeBoxes[rawCompressedBoxes[str_String], fmt_] := Uncompress[str]


(* ::Subsection::Closed:: *)
(*AlphaQueryPreferences[]*)


(*
Note that this dialog affects the behavior of AlphaQuery[] (aka '=='), not the general
behavior of WolframAlpha[]. WolframAlpha[] respects the dialog's "Server" setting, but
nothing else.
*)


setServer["other"] := Block[{other = InputString["Enter a URL:"]}, If[StringQ[other], setPreference["BaseURL", other], other]];

setBehavior["default"] := (
	$AlphaQueryAppearanceElements = $AutomaticAlphaQueryAppearanceElements;
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"cell", "minput", "msound", "dataformats"};
	$AlphaQueryPodWidth = Automatic;
	$AlphaQueryRawParameters = {};
)
setBehavior["CDF"] := (
	$AlphaQueryAppearanceElements = {"CDFWarnings", "CDFAssumptions", "CDFBrand", "ContentPadding", "Pods", "CDFPodMenus", "PodCornerCurl", "Unsuccessful", "Sources"};
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"cell", "minput", "msound", "dataformats", "imagemap"};
	$AlphaQueryPodWidth = Automatic;
	$AlphaQueryRawParameters = {};
)
setBehavior["no inputs"] := (
	$AlphaQueryAppearanceElements = {"Warnings", "Assumptions", "Brand", "Pods", "PodMenus", "Unsuccessful", "Sources"};
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"cell", "msound", "dataformats"};
	$AlphaQueryPodWidth = Automatic;
	$AlphaQueryRawParameters = {};
)
setBehavior["only inputs"] := (
	$AlphaQueryAppearanceElements = {"UseInputLeft"};
	setPreference["Asynchronous", False];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"minput"};
	$AlphaQueryPodWidth = Automatic;
	$AlphaQueryRawParameters = {};
)
setBehavior["images"] := (
	$AlphaQueryAppearanceElements = {"Warnings", "Assumptions", "Brand", "Pods", "Unsuccessful", "Sources"};
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"image", "msound"};
	$AlphaQueryPodWidth = Automatic;
	$AlphaQueryRawParameters = {};
)
setBehavior["iPhone images"] := (
	$AlphaQueryAppearanceElements = {"Warnings", "Assumptions", "Brand", "Pods", "Unsuccessful", "Sources"};
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"image"};
	$AlphaQueryPodWidth = {308, 468};
	$AlphaQueryRawParameters = {};
)
setBehavior["iPhone landscape images"] := (
	$AlphaQueryAppearanceElements = {"Warnings", "Assumptions", "Brand", "Pods", "Unsuccessful", "Sources"};
	setPreference["Asynchronous", True];
	setPreference["Interactive", True];
	setPreference["Reinterpret", True];
	$AlphaQueryFormats = {"image"};
	$AlphaQueryPodWidth = {468, 468};
	$AlphaQueryRawParameters = {};
)
setBehavior["tear off buttons"] := (
	$AlphaQueryAppearanceElements = Flatten[{DeleteCases[$AutomaticAlphaQueryAppearanceElements, "UseInput" | "UseInputLeft" | "PodMenus" | "SubpodMenus"], "TearOffButtons"}];
)
setBehavior["pod menus"] := (
	$AlphaQueryAppearanceElements = Flatten[{DeleteCases[$AutomaticAlphaQueryAppearanceElements, "UseInput" | "UseInputLeft" | "TearOffButtons" | "SubpodMenus"], "PodMenus"}];
)
setBehavior["pod and subpod menus"] := (
	$AlphaQueryAppearanceElements = Flatten[{DeleteCases[$AutomaticAlphaQueryAppearanceElements, "UseInput" | "UseInputLeft" | "TearOffButtons"], "PodMenus", "SubpodMenus"}];
)
setBehavior["no tear offs"] := (
	$AlphaQueryAppearanceElements = Flatten[{DeleteCases[$AutomaticAlphaQueryAppearanceElements, "UseInput" | "UseInputLeft" | "TearOffButtons" | "PodMenus" | "SubpodMenus"]}];
)



AlphaQueryPreferences[] :=
CreateDialog[{
	Panel[
		Grid[{
			{"Wolfram|Alpha server", InputField[dynamicPreference["BaseURL", "Automatic"], String, FieldSize -> {{20,20},{1,Infinity}}]},
			{"AppID", InputField[dynamicPreference["AppID", "Automatic"], String, FieldSize -> {{20,20},{1,Infinity}}]},
			{"", Row[{Checkbox[dynamicPreference["Asynchronous", True]], "Asynchronous"}]},
			{"", Row[{Checkbox[dynamicPreference["Interactive", True]], "Interactive"}]},
			{"", Row[{Checkbox[dynamicPreference["Reinterpret", True]], "Reinterpret"}]},
			{"", Row[{Checkbox[dynamicPreference["Recalculate", True]], "Recalculate"}]},
			{"", Row[{Checkbox[dynamicPreference["MathematicaFormsFormatType", "MostlyInputForm"], {"MostlyInputForm", "StandardForm"}], "Typeset Mathematica forms"}]},
			{"Clicking in the extrusion", Row[{Checkbox[dynamicPreference["ExtrusionClickClose", False]], "closes the extrusion  ", Checkbox[dynamicPreference["ExtrusionClickEvaluate", False]], "evaluates the cell"}]},
			{"Mathematica Session Info",
				ActionMenu[
					Dynamic[Switch[{
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}, Automatic],
							$AlphaQuerySendMathematicaSessionInfo},
						{True, _}, "Always send",
						{False, _}, "Never send",
						{_, True}, "Send for this session",
						{_, False}, "Don't send for this session",
						{_, _}, "Ask before sending"
					]],
					{
						"Ask before sending" :> (
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}] = Automatic;
							$AlphaQuerySendMathematicaSessionInfo = Automatic;),
						Delimiter,
						"Never send" :> (
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}] = False;
							$AlphaQuerySendMathematicaSessionInfo = False;),
						"Don't send for this session" :> (
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}] = Automatic;
							$AlphaQuerySendMathematicaSessionInfo = False;),
						"Send for this session" :> (
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}] = Automatic;
							$AlphaQuerySendMathematicaSessionInfo = True;),
						"Always send" :> (
							CurrentValue[$FrontEnd, {PrivateFrontEndOptions, "WolframAlphaSettings", "SendMathematicaSessionInfo"}] = True;
							$AlphaQuerySendMathematicaSessionInfo = True;)
					},
					Appearance -> "PopupMenu"
				]
			},			
			{"", Item[Row[{Button["Help\[Ellipsis]", SystemOpen["paclet:ref/WolframAlpha"]], Button["Reset", clearPreferences[]]}], Alignment -> Right]}
		}, Alignment ->{{Right, Left}}],
		"Front End Preferences"
	],
	Panel[
		Grid[{
			(* {"Query JSP", InputField[Dynamic[$AlphaQueryJSP], String, FieldSize -> {{40,40},{1,Infinity}}]}, *)
			{Row[{Checkbox[Dynamic[$AlphaQueryPrefixQ]], "Query prefix"}], InputField[Dynamic[$AlphaQueryPrefix], String, FieldSize -> {{40,40},{1,Infinity}}, Enabled -> Dynamic[$AlphaQueryPrefixQ]]},
			{"", OpenerView[{"Timeouts",
				Panel @ Grid[{
					{"scan timeout", InputField[Dynamic[$AlphaQueryScanTimeout], Expression, FieldSize -> {{20,20},{1,Infinity}}]},
					{"pod timeout", InputField[Dynamic[$AlphaQueryPodTimeout], Expression, FieldSize -> {{20,20},{1,Infinity}}]},
					{"format timeout", InputField[Dynamic[$AlphaQueryFormatTimeout], Expression, FieldSize -> {{20,20},{1,Infinity}}]},
					{"async timeout", InputField[Dynamic[$AlphaQueryAsynchoronousTimeout], Expression, FieldSize -> {{20,20},{1,Infinity}}]}}, Alignment ->{{Right, Left}}]}, False]},
			{"", Row[{Checkbox[Dynamic[$AlphaQueryExtrusionEvaluateCachedMForms]], "Evaluate cached minputs and moutputs"}]},
			{"", OpenerView[{"Formats", Panel @ CheckboxBar[Dynamic[$AlphaQueryFormats], {"image", "cell", "minput", "moutput", "sound", "msound", "dataformats", "computabledata", "formatteddata", "imagemap"}, Appearance -> "Vertical"]}, False]},
			{"", OpenerView[{"AppearanceElements", Panel @ CheckboxBar[Dynamic[$AlphaQueryAppearanceElements], $AllAlphaQueryAppearanceElements, Appearance -> "Vertical"]}, False]},
			{"", Row[{Checkbox[Dynamic[$AlphaQueryExtrusionShowMOutputs]], "Show moutputs in the extrusion"}]},
			{"", Row[{Checkbox[Dynamic[$AlphaQueryShowLinksByDefault]], "Show links by default"}]},
			{"", Row[{Checkbox[Dynamic[$AlphaQueryAdditionalInformation]], "Show additional information"}]},
			{"Quiet", SetterBar[Dynamic[$AlphaQueryQuiet], {True, False, "Log", Automatic}]},
			{"Pod width", InputField[Dynamic[$AlphaQueryPodWidth], Expression, FieldSize -> {{40,40},{1,Infinity}}]},
			{"Time constraint", InputField[Dynamic[$AlphaQueryTimeConstraint], Expression, FieldSize -> {{40,40},{1,Infinity}}]},
			{"Other parameters", InputField[Dynamic[$AlphaQueryRawParameters], Expression, FieldSize -> {{40,40},{1,Infinity}}]},
			{"", Button["Sample URL\[Ellipsis]", CreateDocument[ExpressionCell[WolframAlpha["sinx", "URL", alphaQueryOptions[]], "Input"]], Method -> "Queued", ImageSize -> Automatic]}
		}, Alignment ->{{Right, Left}}],
		"Kernel Preferences"
	],
	Panel[
		Row[{
			SetterBar[dynamicPreference["BaseURL", "Automatic"], Last /@ $AlphaQueryBaseURLs, BaseStyle -> "ControlStyle"],
			Button["Other\[Ellipsis]", setServer["other"], Method -> "Queued"]
		}],
		"Server"
	],
	Panel[
		Row[{
			Button["Default", setBehavior["default"]],
			Button["CDF", setBehavior["CDF"]],
			Button["No inputs", setBehavior["no inputs"]],
			Button["Only inputs", setBehavior["only inputs"]], 
			Button["Use images", setBehavior["images"]],
			Button["images, iPhone", setBehavior["iPhone images"]],
			Button["images, iPhone landscape", setBehavior["iPhone landscape images"]]
		}],
		"Display behavior"
	],
	Panel[
		Row[{
			Button["Default", setBehavior["pod menus"]],
			Button["Pod and subpod menus", setBehavior["pod and subpod menus"]],
			Button["Buttons", setBehavior["tear off buttons"]],
			Button["None", setBehavior["no tear offs"]]
		}],
		"Tear off behavior"
	]
	},
	WindowTitle -> "AlphaQuery Preferences",
	NotebookEventActions -> {},
	WindowElements -> {"VerticalScrollBar"}
]


alphaQueryOptions[] := Sequence @@ {
	AppearanceElements -> $AlphaQueryAppearanceElements,
	Asynchronous -> Block[{async, asynctimeout},
		async = $AlphaQueryAsync[];
		asynctimeout = $AlphaQueryAsynchoronousTimeout;	
		(* Interpret True as All *)
		If[ async === True, async = All];
		(* Interpret All as $AlphaQueryAsynchoronousTimeout if it's set *)
		If[ async === All && NumericQ[$AlphaQueryAsynchoronousTimeout],
			async = $AlphaQueryAsynchoronousTimeout
		];
		async
	],
	TimeConstraint -> {$AlphaQueryTimeConstraint, $AlphaQueryScanTimeout, $AlphaQueryPodTimeout, $AlphaQueryFormatTimeout},
	Method -> {
		"Server" -> $AlphaQueryBaseURL[],
		"Formats" -> $AlphaQueryFormats,
		"RawParameters" -> $AlphaQueryRawParameters
	}
}


alphaQueryInputsOptions[] := Sequence @@ {
		AppearanceElements -> {"UseInputLeft"},
		Asynchronous -> False,
		Method -> {
			"Server" -> $AlphaQueryBaseURL[],
			"Formats" -> {"minput"}
		}
	}


(* ::Subsection::Closed:: *)
(*WolframAlpha[]*)


(* ::Subsubsection::Closed:: *)
(*Options[WolframAlpha]*)


Options[WolframAlpha] = Options[WolframAlphaResult] = {
	AppearanceElements -> Automatic,
	Asynchronous -> False,
	ExcludePods -> None,
	IgnoreCase -> False,
	IncludePods -> All,
	InputAssumptions -> {},
	Method -> {},
	PodStates -> {},
	PodWidth -> Automatic,
	TimeConstraint -> 30
}


$WolframAlphaMethodOptions = {
	"AdditionalInformation" :> $AlphaQueryAdditionalInformation,
	"CellOptions" -> {},
	"DelayedBoxes" -> False,
	"ExpandAsyncContent" -> Automatic,
	"ExpandImageURLs" -> Automatic,
	"ExpandRecalculateContent" -> Automatic,
	"ExtrusionChosen" -> Automatic,
	"ExtrusionOpen" -> False,
	"Formats" -> Automatic,
	"Interactive" -> Automatic,
	"NewQuery" -> None,
	"NotebookOptions" -> {},
	"OriginalURL" -> None,
	"RawParameters" -> {},
	"Reinterpret" -> Automatic,
	"Server" -> Automatic,
	"UserAgentString" -> Automatic
};


$WolframAlphaDefaultOptions = Options[WolframAlpha] /. (Method -> {}) -> (Sequence @@ $WolframAlphaMethodOptions);


(* 
The resolveOptions[] utility takes a sequence of option settings and returns the sequence
of all WolframAlpha[] options that are set to something other than their default value.
This is redundant for uses of WolframAlpha[] that go through "URL", "RawXML", or
"ProcessedXML", but these options are also used by the formatQueryResults[] utility that
takes the "ProcessedXML" result and decides how to format it.

Thus, we only use this utility when calling formatQueryResults[].
*)


resolveOptions[opts___] := 
Block[{mainOptions, methodOptions},

	mainOptions = First[Cases[Flatten[{opts, Options[WolframAlpha]}], _[#, _]]]& /@ 
		{AppearanceElements, Asynchronous, ExcludePods, IncludePods, InputAssumptions, PodStates, PodWidth, TimeConstraint};
	
	(* Don't include "CellOptions" or "NotebookOptions" here, since they're already being stored in the cell or notebook. *)
	methodOptions = First[Cases[allMethodOptions[opts], _[#, _]]]& /@ {
		"AdditionalInformation",
		"ExpandAsyncContent",
		"ExpandImageURLs",
		"ExpandRecalculateContent",
		"ExtrusionChosen",
		"ExtrusionOpen",
		"Formats",
		"NewQuery",
		"RawParameters",
		"Server",
		"UserAgentString"
	};

	mainOptions = DeleteCases[mainOptions, Alternatives @@ $WolframAlphaDefaultOptions];
	
	methodOptions = DeleteCases[methodOptions, Alternatives @@ $WolframAlphaDefaultOptions];
	
	Sequence @@ Flatten[{mainOptions, If[methodOptions === {}, {}, Method -> methodOptions]}]
]


allMethodOptions[opts___] :=
	Cases[
		Flatten[{
			Cases[Flatten[{opts}], _[Method, a_] :> a],
			Cases[Options[WolframAlpha], _[Method, a_] :> a],
			$WolframAlphaMethodOptions
		}],
		_Rule | _RuleDelayed
	];



(* ::Subsubsection::Closed:: *)
(*No arguments*)


WolframAlpha[OptionsPattern[]] := Null /; Message[WolframAlpha::argt, WolframAlpha, 0, 1, 2]


(* ::Subsubsection::Closed:: *)
(*First argument contains only whitespace*)


WolframAlpha["", rest___] := (Message[WolframAlpha::blnulst, ToString["", InputForm], 1]; $Failed)


WolframAlpha[str_String, rest___] := (Message[WolframAlpha::blnulst, ToString[str, InputForm], 1]; $Failed) /; StringTrim[str] === ""


(* ::Subsubsection::Closed:: *)
(*One-argument form*)


$DefaultWolframAlphaFormat := If[$Notebooks, "FullOutput", {All, {"Plaintext", "Input"}}]


WolframAlpha[str_String, opts:OptionsPattern[]] := WolframAlpha[str, $DefaultWolframAlphaFormat, opts]


(* ::Subsubsection::Closed:: *)
(*"FullResult"*)


WolframAlpha[str_String, "FullResult" | "FullOutput", opts:OptionsPattern[]] := 
	formatAlphaXML[
		WolframAlpha[str, "ProcessedXML", opts, AppearanceElements -> {"Warnings", "Assumptions", "Brand", "Pods", "PodMenus", "Unsuccessful", "Sources"}],
		str, opts, AppearanceElements -> {"Warnings", "Assumptions", "Brand", "Pods", "PodMenus", "Unsuccessful", "Sources"}
	]


(* ::Subsubsection::Closed:: *)
(*"FullInput"*)


WolframAlpha[str_String, "FullInput", opts:OptionsPattern[]] :=
	Block[{replaceEvaluationCell},
		replaceEvaluationCell[expr_] := CellPrint[queryBlobCell[expr]];
		AlphaIntegration`ExtrusionEvaluate[str, InputForm, opts]
	]


(* ::Subsubsection::Closed:: *)
(*"Result"*)


WolframAlpha[str_String, "Result", opts:OptionsPattern[]] := 
	Block[{replaceEvaluationCell, errorBlobQ},
		errorBlobQ[True, ___] := (Message[WolframAlpha::conopen]; False);
		AlphaIntegration`ExtrusionEvaluate[str, InputForm, opts]
	]


WolframAlpha[str_String, "HeldResult", opts:OptionsPattern[]] :=
	Block[{replaceEvaluationCell, errorBlobQ, suppressPlaceholderMInputs, minputResult, cell},
		replaceEvaluationCell[expr_] := (cell = queryBlobCell[expr]);
		errorBlobQ[True, ___] := (Message[WolframAlpha::conopen]; False);
		(*
			"HeldResult" returns even those expressions that contain Placeholders, even
			though they're not typically evaluated by AlphaIntegration`ExtrusionEvaluate.
		*)
		suppressPlaceholderMInputs[heldexpr_] := heldexpr;
		minputResult[string_] := HoldComplete @@ minputInput[string];

		AlphaIntegration`ExtrusionEvaluate[str, InputForm, opts];
		MakeExpression[First[cell], StandardForm]
	]


(* ::Subsubsection::Closed:: *)
(*"Boxes"*)


WolframAlpha[str_String, "Boxes", opts:OptionsPattern[]] := 
	Switch["DelayedBoxes" /. allMethodOptions[opts],
		True,
		DynamicBox[WolframAlpha[str, "Boxes", Method -> {"DelayedBoxes" -> False}, opts],
			System`DestroyAfterEvaluation -> True,
			SynchronousUpdating -> False,
			Evaluate[CachedValue -> ToBoxes[Column[{
				Style["Computing", FontSize -> 24, FontFamily -> "Times", FontColor -> GrayLevel[0.65]],
				Animator[Appearance -> "Necklace"]},
				Alignment -> Center,
				ItemSize -> Scaled[1],
				Spacings -> 1]]
			]
		],
		"PaneSelector",
		(* When saving to a file, we need to avoid using CachedValue. That's what this does. *)
		DynamicModule[{done = False, content = "None"},
			PaneSelector[{
				False ->
					Column[{
						Style["Computing", FontSize -> 24, FontFamily -> "Times", FontColor -> GrayLevel[0.65]],
						Animator[Appearance -> "Necklace"]},
						Alignment -> Center,
						ItemSize -> Scaled[1],
						Spacings -> 1
					],
				True -> Dynamic[RawBoxes[content], System`DestroyAfterEvaluation -> True]
				},
				Dynamic[done],
				ImageSize -> Automatic
			],
			Initialization :> (content = WolframAlpha[str, "Boxes", Method -> {"DelayedBoxes" -> False}, opts]; done = True),
			SynchronousInitialization -> False
		] // ToBoxes,
		(* else *)
		_,
		formatAlphaXMLWithGeneralization[str, opts] // ToBoxes
	]


formatAlphaXMLWithGeneralization[str_, opts___] :=
	Block[{xml, generalization, values, topic, desc, url, async, delayed},
		xml = WolframAlpha[str, "ProcessedXML", opts];		
		generalization = Replace[xml, {
			XMLObject["Document"][_, XMLElement["queryresult", _, {___, g:XMLElement["generalization", _, _], ___}], _] :> g,
			_ -> None}
		];
		
		If[generalization === None, Return @ formatAlphaXML[xml, str, opts]];

		values = Replace[generalization, {XMLElement["generalization", values_, data_] :> values, _ -> $Failed}];
		topic = "topic" /. Flatten[{values, "topic" -> "unknown"}];
		desc = "desc" /. Flatten[{values, "desc" -> "General results for:"}];
		url = "url" /. Flatten[{values, "url" -> $Failed}];
		If[url === $Failed, Return @ formatAlphaXML[xml, str, opts]];
		
		(*
		In the future, we might want to have ontological promotion handled in the main
		formatAlphaXML or formatQueryResults utilities. So as a bet against future compatibility, we
		remove the "generalization" element from the first round of xml results before formatting
		them.
		*)
		xml = Replace[xml, 
			XMLObject["Document"][a_, XMLElement["queryresult", values_, {x___, XMLElement["generalization", _, _], y___}], b_] :>
			XMLObject["Document"][a, XMLElement["queryresult", values, {x, y}], b]
		];
		
		(* delay the new output only if Asynchronous is set to All or a number *)
		async = Asynchronous /. Flatten[{opts, Options[WolframAlpha]}];		
		delayed = If[(async === All) || NumericQ[async], "PaneSelector", False];
		
		Column[{
			formatAlphaXML[xml, str, opts],
			innerFrame[
				Row[{desc, " ", Style[topic, Bold]}],
				Background -> RGBColor[1., 1., 0.92],
				BaseStyle -> "DialogStyle",
				FrontEnd`BoxFrame -> 2,
				FrameMargins -> 15,
				FrameStyle -> RGBColor[0.98, 0.52, 0.33],
				RoundingRadius -> 8
			],
			RawBoxes[WolframAlpha[topic, "Boxes", opts,  Method -> {"OriginalURL" -> url, "DelayedBoxes" -> delayed}]]
			},
			Spacings -> {1.5, 1.5}
		]
	]


(* ::Subsubsection::Closed:: *)
(*"Cell", "Notebook"*)

WolframAlpha[str_String, "Cell", opts:OptionsPattern[]] := 
	Block[{cellopts},
		cellopts = "CellOptions" /. allMethodOptions[opts];
		cellopts = Sequence @@ Flatten[{cellopts}];
		Cell[
			BoxData[WolframAlpha[str, "Boxes", opts]],
			"Output",
			cellopts,
			CellMargins -> {{20, 10}, {Inherited, Inherited}}
		]
	]


WolframAlpha[str_String, "Notebook", opts:OptionsPattern[]] :=
	Block[{nbopts},
		nbopts = "NotebookOptions" /. allMethodOptions[opts];
		nbopts = Sequence @@ Flatten[{nbopts}];
		Notebook[{
			WolframAlpha[str, "Cell", opts]
			},
			nbopts
		]
	]


WolframAlpha[str_String, {"Notebook", nbfile_}, opts:OptionsPattern[]] := 
	Put[WolframAlpha[str, "Notebook", opts], nbfile]


(* ::Subsubsection::Closed:: *)
(*"ServerNotebook"*)


$CDFNotebookOptions = Sequence[
	Editable -> False,
	Evaluatable -> False,
	Selectable -> False,
	ScrollingOptions -> {"VerticalScrollRange" -> Fit},
	ShowCellBracket -> False,
	ShowSelection -> False
]

$CDFCellOptions = Sequence[
	CellEditDuplicate -> False,
	CellMargins -> {{4, 4}, {4, 4}},
	Evaluatable -> False,
	Selectable -> True,
	ShowSelection -> True
]


WolframAlpha[str_String, "ServerNotebook", opts:OptionsPattern[]] := 
	Block[{nbopts},
		nbopts = "NotebookOptions" /. allMethodOptions[opts];
		nbopts = Sequence @@ Flatten[{nbopts}];
		Notebook[{
			WolframAlpha[str, "Cell", opts,
				AppearanceElements -> {"CDFWarnings", "CDFAssumptions", "CDFBrand", "ContentPadding", "Pods", "CDFPodMenus", "PodCornerCurl", "Unsuccessful", "Sources"},
				Asynchronous -> All,
				Method -> {"CellOptions" -> {$CDFCellOptions}, "Formats" -> {"cell", "msound", "dataformats", "imagemap"}}
			]},
			nbopts,
			$CDFNotebookOptions
		]
	]


WolframAlpha[str_String, {"ServerNotebook", nbfile_}, opts:OptionsPattern[]] := 
	WolframAlpha[str, {"ServerNotebook", nbfile, False}, opts]	


WolframAlpha[str_String, {"ServerNotebook", nbfile_, cacheQ:False}, opts:OptionsPattern[]] := 
	Put[WolframAlpha[str, "ServerNotebook", opts], nbfile]


WolframAlpha[str_String, {"ServerNotebook", nbfile_, cacheQ:True}, opts:OptionsPattern[]] := 
	exportNotebook[nbfile, WolframAlpha[str, "ServerNotebook", opts]]


WolframAlpha[str_String, {"ServerNotebook", nbfile_, context_String}, opts:OptionsPattern[]] := 
	exportNotebook[nbfile, WolframAlpha[str, "ServerNotebook", opts], context]


WolframAlpha[str_String, {"ServerNotebook", nbfile_, cacheQ_, pathToSigningBinary_}, opts:OptionsPattern[]] := 
	Block[{link},
		WolframAlpha[str, {"ServerNotebook", nbfile, cacheQ}, opts];
		link = Install[ pathToSigningBinary ];
		NotebookSign`PlayerProSecure[nbfile];
		Uninstall[link]
	]



exportNotebook[args__] :=
	Block[{dynamicupdating},
		UsingFrontEnd[
			dynamicupdating = CurrentValue[$FrontEndSession, DynamicUpdating];
			CurrentValue[$FrontEndSession, DynamicUpdating] = False;
			doExportNotebook[args];
			CurrentValue[$FrontEndSession, DynamicUpdating] = dynamicupdating;
		]
	];


doExportNotebook[nbfile_, nbexpr_] := Export[nbfile, nbexpr]

doExportNotebook[nbfile_, nbexpr_, context_] :=
	If[TrueQ[$LocalServerWithContext] || useWithContext[],
		Block[{result, stm},
			result = Block[
				{$Context = "hiddenContext`", $ContextPath = {"hiddenContext`", "System`"}},
				MathLink`CallFrontEnd[FrontEnd`ExportPacket[FEPrivate`WithContext[context, nbexpr], "NotebookString"]]
			];
			If[MatchQ[result, {_String, ___}],
				stm = OpenWrite[nbfile];
				WriteString[stm, First[result]];
				Close[stm];
				nbfile,
				(* else *)
				$Failed
			]
		],
		Export[nbfile, nbexpr]
	]



useWithContext[] := useWithContext[] =
Block[{version, release, minorrelease},
	version = MathLink`CallFrontEnd[FrontEnd`Value["$NotebookVersionNumber"]];
	release = MathLink`CallFrontEnd[FrontEnd`Value["$NotebookReleaseNumber"]];
	minorrelease = MathLink`CallFrontEnd[FrontEnd`Value["$NotebookMinorReleaseNumber"]];
	(* The required FEPrivate`WithContext behavior was added in version 8.0.1.1 *)
	Or[
		version > 8.0,
		version === 8.0 && release > 1,
		version === 8.0 && release === 1 && minorrelease > 0
	] // TrueQ
]



(* ::Subsubsection::Closed:: *)
(*"FormattedPods"*)


WolframAlpha[str_String, "FormattedElements", opts:OptionsPattern[]] :=
	Block[{xml, pods, nonpods, elements, style},
		xml = WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"cell"}, "ExpandRecalculateContent" -> True, "ExpandAsyncContent" -> True}];
		If[!MatchQ[xml, XMLObject["Document"][_, XMLElement["queryresult", _, _], _]], Return[$Failed]];
		
		pods = Cases[xml[[2,3]], XMLElement["pod", _, _]];
		nonpods = DeleteCases[xml[[2,3]], XMLElement["pod", _, _]];
		
		elements = OptionValue[AppearanceElements];
		Switch[elements,
			None, elements = {},
			{__String}, Null,
			All, elements = $AllAlphaQueryAppearanceElements,
			Automatic | _, elements = $AutomaticAlphaQueryAppearanceElements
		];
		
		style[list_List] := style /@ list;
		style[expr_] := Style[expr, "DialogStyle", AutoItalicWords -> {"Mathematica"}, Enabled -> False];
		
		Flatten[{
			If[!MemberQ[elements, "Warnings"], {}, "Warnings" -> style[FormatIndependentWarnings[nonpods, str, opts]]],
			If[!MemberQ[elements, "Assumptions"], {}, "Assumptions" -> style[FormatIndependentAssumptions[nonpods, str, opts]]],
			If[!MemberQ[elements, "Pods"], {}, "Pods" -> ( ("id" /. Part[#, 2]) -> style[FormatIndependentPod[#, str, opts]]& /@ pods )],
			If[!MemberQ[elements, "Pods"], {}, "PodStates" -> getPodAndSubpodStates[pods]],
			If[!MemberQ[elements, "Unsuccessful"], {}, "Unsuccessful" -> If[pods =!= {}, {}, style[FormatIndependentUnsuccesses[nonpods, str, opts]]]]
		}]
	]


getPodAndSubpodStates[pods_] := Flatten[{getPodStates[pods], getSubpodStates[pods]}]

getPodStates[pods_List] := 
	Cases[pods, XMLElement["pod", {___, "id" -> id_, ___}, {___, states:XMLElement["states", _, _], ___}] :> (id -> states)]

getSubpodStates[pods_List] := DeleteCases[Flatten[getSubpodStates /@ pods], _ -> {}]

getSubpodStates[XMLElement["pod", {___, "id" -> id_, ___}, data_]] :=
	MapIndexed[{id -> First[#2]} -> # &, Cases[data, subpod:XMLElement["subpod", _, _] :> getSubpodStates[subpod]]];

getSubpodStates[XMLElement["subpod", _, {___, states:XMLElement["states", _, _], ___}]] := states

getSubpodStates[other_] := {}



(* FormatIndependentWarnings[] needs to reach deeper than FormatAllWarnings[] does to get the warnings separately *) 
FormatIndependentWarnings[nonpods_, query_, opts___] :=
	DeleteCases[
		Map[
			DynamicModule[{Typeset`q = query, Typeset`newq = None},
				formatWarning[#, "Warnings", Dynamic[Typeset`newq], Dynamic[Typeset`q], opts],
				Initialization :> Quiet[WolframAlpha[]] (* trigger autoloading *)
			]&,
			Flatten[Cases[nonpods, XMLElement["warnings", values_, data_] :> data]]
		],
		_[_, {}]
	]


FormatIndependentUnsuccesses[nonpods_, query_, opts___] := {
	(* there's always exactly one of these, which is disincluded if there are any pods *)
	DynamicModule[{Typeset`q = query, Typeset`newq = None, Typeset`opts = {opts}},
		Dynamic[First[FormatAllUnsuccesses[nonpods, Dynamic[Typeset`newq], Dynamic[Typeset`q], Dynamic[Typeset`opts]]]],
		Initialization :> Quiet[WolframAlpha[]] (* trigger autoloading *)
	]
}


(* FormatIndependentAssumptions[] is based closely on FormatAllAssumptions[] *)
FormatIndependentAssumptions[nonpods_, query_, opts___] :=
Block[{assumptions, nonformula, formula},
	assumptions = Cases[nonpods, XMLElement["assumptions", values_, list_] :> list] // Flatten;
	formula = Cases[assumptions,
		XMLElement["assumption", {___, "type" -> (Alternatives @@ $FormulaAssumptionTypes), ___}, _] ];
	nonformula = DeleteCases[assumptions, Alternatives @@ formula];
	
	With[{nonformula = nonformula, formula = formula},
		Flatten[{
			If[nonformula === {}, {},
				DynamicModule[{Typeset`q = query, Typeset`newq = None, Typeset`opts = {opts}},
					Dynamic[formatNonFormulaAssumptions[nonformula, "Assumptions", Dynamic[Typeset`newq], Dynamic[Typeset`q], Dynamic[Typeset`opts]]],
					Initialization :> Quiet[WolframAlpha[]] (* trigger autoloading *)
				]
			],
			If[formula === {}, {},
				DynamicModule[{Typeset`q = query, Typeset`newq = None, Typeset`opts = {opts}},
					Dynamic[formatFormulaAssumptions[formula, "Assumptions", Dynamic[Typeset`newq], Dynamic[Typeset`q], Dynamic[Typeset`opts]]],
					Initialization :> Quiet[WolframAlpha[]] (* trigger autoloading *)
				]
			]
		}]
	]
]







(* ::Subsubsection::Closed:: *)
(*"PodCells"*)


WolframAlpha[str_String, "PodCells", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"cell"}}],
		XMLElement["subpod", _, {___, XMLElement["cell", _, {cell_}], ___}] :> prepareSubpodCell[cell, Automatic, Identity],
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodContents"*)


WolframAlpha[str_String, "PodContents", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"cell"}}],
		XMLElement["subpod", _, {___, XMLElement["cell", _, {cell_}], ___}] :> prepareSubpodCellContent[cell, Automatic, Identity],
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodImages"*)


WolframAlpha[str_String, "PodImages", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"image"}}],
		XMLElement["subpod", _, {___, XMLElement["img", {___, "ImportedData" -> img_,___}, _], ___}] :> img,
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodPlaintext"*)


WolframAlpha[str_String, "PodPlaintext", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
		XMLElement["subpod", _, {___, XMLElement["plaintext", _, {text_}], ___}] :> text,
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodTitles"*)


WolframAlpha[str_String, "PodTitles", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
		XMLElement["pod", {___, "title" -> title_, ___}, _] :> title,
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodIDs"*)


WolframAlpha[str_String, "PodIDs", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
		XMLElement["pod", {___, "id" -> id_, ___}, _] :> id,
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PodStates"*)


WolframAlpha[str_String, "PodStates", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"plaintext"}}],
		XMLElement["pod", {___, "id" -> id_, ___}, {___, XMLElement["states", _, states_], ___}] :>
			(id -> Flatten[Map[getStates, states]]),
		Infinity
	]


getStates[XMLElement["state", {___, "input" -> input_, ___}, _]] := input

getStates[XMLElement["statelist", _, states_]] := Map[getStates, states]

getStates[other_] := {}



(* ::Subsubsection::Closed:: *)
(*"PodInformation"*)


WolframAlpha[str_String, "PodInformation" | "PropertyRules", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "ProcessedXML", opts],
		pod:XMLElement["pod", {___, "id" -> id_, ___}, _] :> podXMLToPodProperties[id, pod],
		Infinity
	] // Flatten;


podXMLToPodProperties[id_, XMLElement["pod", values_, data_]] := Flatten[{
	{{id, 0}, "Title"} -> getValue["title", values],
	{{id, 0}, "Scanner"} -> getValue["scanner", values],
	{{id, 0}, "ID"} -> getValue["id", values],
	{{id, 0}, "Position"} -> getValue["position", values],
	(* {{id, 0}, "PodStates"} -> getData["states", data], *)
	(* {{id, 0}, "PodInfos"} -> getData["infos", data], *)
	(* {{id, 0}, "HTML"} -> getData["markup", data], *)
	(* {{id, 0}, "XML"} -> XML["pod", values, data], *)
	MapIndexed[subpodXMLToPodProperties[id, First[#2], #1]&, Cases[data, subpod:XMLElement["subpod", _, _]]]
}] // DeleteCases[#, _ -> (_Missing | "")]&


subpodXMLToPodProperties[id_, c_, xml:XMLElement["subpod", values_, data_]] := {
	(* {{id, c}, "XML"} -> xml, *)
	{{id, c}, "Title"} -> getValue["title", values],
	{{id, c}, "Plaintext"} -> getData["plaintext", data],
	{{id, c}, "Input"} -> getData["minput", data],
	{{id, c}, "Output"} -> getData["moutput", data],
	{{id, c}, "MathML"} -> getData["mathml", data],
	{{id, c}, "Cell"} -> getData["cell", data],
	{{id, c}, "Content"} -> getData["cellcontent", data],
	{{id, c}, "Image"} -> getData["img", data],
	{{id, c}, "Sound"} -> getData["sound", data],
	{{id, c}, "MSound"} -> getData["msound", data],
	{{id, c}, "DataFormats"} -> getData["dataformats", data],
	{{id, c}, "ComputableData"} -> getData["computabledata", data],
	{{id, c}, "FormattedData"} -> getData["formatteddata", data],
	{{id, c}, "FormulaData"} -> getData["formuladata", data],
	{{id, c}, "NumberData"} -> getData["numberdata", data],
	{{id, c}, "QuantityData"} -> getData["quantitydata", data],
	{{id, c}, "SoundData"} -> getData["sounddata", data],
	{{id, c}, "TimeSeriesData"} -> getData["timeseriesdata", data]
}


getValue[name_, values_] := name /. values /. name -> Missing["NotAvailable"]


getData["cell", {___, XMLElement["cell", _, {cell_}], ___}] := prepareSubpodCell[cell, Automatic, Identity]
getData["cellcontent", {___, XMLElement["cell", _, {cell_}], ___}] := prepareSubpodCellContent[cell, Automatic, Identity]
getData["minput", {___, XMLElement["minput", _, {minput_String}], ___}] := ToExpression[minput, InputForm, HoldComplete]
getData["moutput", {___, XMLElement["moutput", _, {moutput_String}], ___}] := ToExpression[moutput, InputForm, HoldComplete]
getData["mathml", {___, XMLElement["mathml", _, {mathml_}], ___}] := mathml
getData["img", {___, XMLElement["img", {___, "ImportedData" -> img_, ___}, _], ___}] := img
getData["img", {___, XMLElement["img", {___, "src" -> src_, ___}, _], ___}] := src
getData["msound", {___, XMLElement["sound", _, {sound_String}], ___}] := ToExpression[sound]
getData["sound", {___, XMLElement["sound", {___, "ImportedData" -> sound_, ___}, _], ___}] := sound
getData["sound", {___, XMLElement["sound", {___, "src" -> src_, ___}, _], ___}] := src
getData["dataformats", {___, XMLElement["dataformats", _, {data_String}], ___}] := Join[parseTextFormatsString[data], parseDataFormatsString[data]]
getData["dataformats", {___, XMLElement["dataformats", _, {}], ___}] := {}
getData["computabledata", {___, XMLElement["computabledata", {}, {data_String}], ___}] := ToExpression[data]
getData["formatteddata", {___, XMLElement["formatteddata", {}, {data_String}], ___}] := ToExpression[data]
getData["formuladata", {___, XMLElement["formuladata", {}, {data_String}], ___}] := ToExpression[data]
getData["numberdata", {___, XMLElement["numberdata", {}, {data_String}], ___}] := ToExpression[data]
getData["quantitydata", {___, XMLElement["quantitydata", {}, {data_String}], ___}] := ToExpression[data]
getData["sounddata", {___, XMLElement["sounddata", {}, {data_String}], ___}] := ToExpression[data]
getData["timeseriesdata", {___, XMLElement["timeseriesdata", {}, {data_String}], ___}] := ToExpression[data]
getData[name_, {___, XMLElement[name_, values_, {data_}], ___}] := data
getData[name_, data_] := Missing["NotAvailable"]


parseDataFormatsString[str_] :=
	DeleteDuplicates[Flatten[StringSplit[str, ","]]] /.
		Flatten[{
			Thread[ToLowerCase[$AlphaQueryDataFormats] -> $AlphaQueryDataFormats],
			_String :> Unevaluated[Sequence[]]
		}]

parseTextFormatsString[str_] := 
	DeleteDuplicates[Flatten[StringSplit[str, ","]]] /.
		Flatten[{
			Thread[ToLowerCase[$AlphaQueryTextFormats] -> $AlphaQueryTextFormats],
			"minput" -> "Input",
			"moutput" -> "Output",
			_String :> Unevaluated[Sequence[]]
		}]


(* ::Subsubsection::Closed:: *)
(*"PodProperties"*)


WolframAlpha[str_String, "PodProperties" | "Properties", opts:OptionsPattern[]] :=
	First /@ WolframAlpha[str, "PodInformation", opts]


(* ::Subsubsection::Closed:: *)
(*"DataRules"*)


WolframAlpha[str_String, "DataRules", opts:OptionsPattern[]] := 
	WolframAlpha[str, {All, $AlphaQueryDataFormats}, opts]


WolframAlpha[str_String, "DataFormats", opts:OptionsPattern[]] :=
	WolframAlpha[str, {All, "DataFormats"}, opts]


WolframAlpha[str_String, "ComputableData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "computabledata"}],
		XMLElement["computabledata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "FormattedData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "formatteddata"}],
		XMLElement["formatteddata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "FormulaData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "formuladata"}],
		XMLElement["formuladata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "NumberData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "numberdata"}],
		XMLElement["numberdata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "QuantityData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "quantitydata"}],
		XMLElement["quantitydata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "SoundData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "sounddata"}],
		XMLElement["sounddata", _, {data_}] :> ToExpression[data],
		Infinity
	]


WolframAlpha[str_String, "TimeSeriesData", opts:OptionsPattern[]] :=
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> "timeseriesdata"}],
		XMLElement["timeseriesdata", _, {data_}] :> ToExpression[data],
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"InputAssumptions"*)


WolframAlpha[str_String, "InputAssumptions" | "Assumptions", opts:OptionsPattern[]] := 
	Block[{assumptions},
		assumptions =
			Cases[
				WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
				XMLElement["assumptions", _, list_] :> list,
				Infinity
			];
		Map[getAssumptions, Flatten[assumptions]]
	]


getAssumptions[XMLElement["value", {___, "input" -> input_, ___}, _]] := input

getAssumptions[XMLElement["assumption", {___, "type" -> type_, ___}, values_]] := Map[getAssumptions, values]

getAssumptions[other_] := {}


(* ::Subsubsection::Closed:: *)
(*"Sources"*)


WolframAlpha[str_String, "Sources", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"plaintext"}}],
		XMLElement["sources", _, sources_List] :> Map[getSource, sources],
		Infinity
	] // Flatten


getSource[XMLElement["source", {___, "url" -> url_String, ___, "text" -> text_, ___}, _]] := text -> url

getSource[other_] := {}


(* ::Subsubsection::Closed:: *)
(*"MathematicaResult"*)


WolframAlpha[str_String, "MathematicaResult", opts:OptionsPattern[]] := 
	ReleaseHold[ WolframAlpha[str, "HeldMathematicaResult", opts] ]


WolframAlpha[str_String, "HeldMathematicaResult", opts:OptionsPattern[]] := 
	Block[{result},
		result = Cases[
			WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"mresult"}}],
			XMLElement["mresult", _, {mresult_String}] :> ToExpression[mresult, InputForm, HoldComplete],
			Infinity
		];
		result = DeleteCases[result, HoldComplete[None]];
		If[result === {}, Missing["NoResult"], First[result]]
	]


(* ::Subsubsection::Closed:: *)
(*"MathematicaForms"*)


WolframAlpha[str_String, "MathematicaForms", opts:OptionsPattern[]] := 
	ToExpression[#, InputForm, HoldComplete]& /@ WolframAlpha[str, "MathematicaStrings", opts]


WolframAlpha[str_String, "MathematicaStrings", opts:OptionsPattern[]] := 
	Cases[
		WolframAlpha[str, "RawXML", opts, Method -> {"Formats" -> {"minput", "moutput"}}],
		XMLElement["minput" | "moutput", _, {mform_String}] :> mform,
		Infinity
	]


(* ::Subsubsection::Closed:: *)
(*"PrimaryMathematicaForm"*)


WolframAlpha[str_String, "PrimaryMathematicaForm", opts:OptionsPattern[]] :=
	If[# === $Failed, $Failed, ToExpression[#, InputForm, HoldComplete]]& @
		WolframAlpha[str, "PrimaryMathematicaString", opts]


WolframAlpha[str_String, "PrimaryMathematicaString", opts:OptionsPattern[]] :=
	Block[{xml, result},
		xml = WolframAlpha[str, "ProcessedXML", opts, Method -> {"Formats" -> {"minput", "moutput"}}];
		(* If there is a primary result, use its minput or moutput, in that order *)
		result = Flatten[Cases[xml, XMLElement["pod", {___, "primary" -> "true", ___}, data_] :> extractMathematicaForms[data], Infinity]];
		If[result =!= {}, Return[First[result]]];
		(* If there's no primary result, use the first pod that has an minput or moutput, in pod order *)
		result = Flatten[Cases[xml, XMLElement["pod", _, data_] :> extractMathematicaForms[data], Infinity]];
		If[result =!= {}, Return[First[result]]];
		(* Otherwise, return $Failed *)
		$Failed
	]


extractMathematicaForms[data_] := Flatten[{
	Cases[data, XMLElement["minput", _, {minput_String}] :> minput, Infinity],
	Cases[data, XMLElement["moutput", _, {moutput_String}] :> moutput, Infinity]}]




(* ::Subsubsection::Closed:: *)
(*"SessionInfo"*)


WolframAlpha[str_String, "SessionInfo", opts:OptionsPattern[]] :=
	mathematicaSessionInfo[str]


(* ::Subsubsection::Closed:: *)
(*"XML", "RawXML", "ProcessedXML"*)


WolframAlpha[str_String, "XML", opts:OptionsPattern[]] := WolframAlpha[str, "RawXML", opts]


WolframAlpha[str_String, "RawXML", ___, Method -> {___, "RawXML" -> xml_, ___}, ___] := xml


WolframAlpha[str_String, "ProcessedXML", ___, Method -> {___, "ProcessedXML" -> xml_, ___}, ___] := xml


WolframAlpha[str_String, type:( "RawXML" | "ProcessedXML" ), opts:OptionsPattern[]] := 
	Block[{url, xml, recalculate, expandrecalculate, async, expandasync, expandimage, additionalinformation, timecontraint},
		timeconstraint = First[N[Flatten[{ OptionValue[TimeConstraint] }]]];
		If[!TrueQ[timeconstraint > 0], timeconstraint = TimeConstraint /. $WolframAlphaDefaultOptions];
		TimeConstrained[
			(
				xml = qImport[url = WolframAlpha[str, "URL", opts], "XML"];
				
				async = OptionValue[Asynchronous];
				{additionalinformation, expandrecalculate, expandasync, expandimage} =
					{"AdditionalInformation", "ExpandRecalculateContent", "ExpandAsyncContent", "ExpandImageURLs"} /. allMethodOptions[opts];

				If[additionalinformation,
					xml = Replace[xml,
						XMLObject["Document"][a_, XMLElement["queryresult", {values__}, data_], b_] :>
						XMLObject["Document"][a, XMLElement["queryresult", {values, "queryurl" -> url}, data], b]
					]
				];
				
				(*
				Interpret "ExpandRecalculateContent" -> Automatic, "ExpandAsyncContent" ->
				Automatic, and "ExpandImageURLs" -> Automatic to mean always do the expansion,
				unless Asynchronous -> All or Asynchronous -> number. That gives us a simple switch
				to throw to get un-expanded results.
				*)

				If[expandrecalculate === True ||
					( expandrecalculate === Automatic && $AlphaQuerySimpleRecalculate[] && (async =!= All && !NumericQ[async]) ),
					recalculate = DeleteCases[Cases[xml, XMLElement["queryresult", {___, "recalculate" -> url_, ___}, _] :> url, Infinity], ""];
					If[Length[recalculate] > 0,
						recalculate = Cases[qImport[extendedParams @@ splitURL[First[recalculate]], "XML"], XMLElement["pod", _, _], Infinity];
						xml = Replace[xml,
							XMLObject["Document"][a_, XMLElement["queryresult", values_, data_List], b_] :>
							XMLObject["Document"][a, XMLElement["queryresult", addRecalculateValues[values], addRecalculateData[data, recalculate]], b]
						]
					]
				];
		
				If[ expandasync === True || ( expandasync === Automatic && (async =!= All && !NumericQ[async]) ),
					xml = xml /. XMLElement["pod", {___, "async" -> url_, ___}, _] :> expandAsyncPod[url, False]
				];
				If[type === "RawXML",	
					(* For "RawXML", don't expand images or cells *)
					xml,
					(* For "ProcessedXML", only expand images when the settings call for it *)
					If[ expandimage === True || ( expandimage === Automatic && (async =!= All && !NumericQ[async]) ),
						xml = expandData[xml],
						(* even when not expanding other data, always expand the cells *)
						xml = expandCells[xml]
					];
					xml
				]
			),
			timeconstraint,
			Message[WolframAlpha::timeout, HoldForm[WolframAlpha[str]], timeconstraint];
			$TimedOut
		]
			
	] // dontRefresh


expandAsyncPod[url_String, expandDataQ_] := 
	Block[{pods},
		pods = Cases[qImport[url, "XML"], XMLElement["pod", _, _], Infinity];
		If[pods === {}, Unevaluated[Sequence[]], If[expandDataQ, expandData[First[pods]], First[pods]]]
	]



addRecalculateValues[values_] := values /. ("recalculate" -> url_) :> ("recalculated" -> url)


addRecalculateData[olddata_, {}] := olddata

addRecalculateData[olddata_, newdata_] := 
Block[{oldpods, nonpods, newpods, joinedpods},
	oldpods = Cases[olddata, XMLElement["pod", _, _]];
	nonpods = DeleteCases[olddata, XMLElement["pod", _, _]];
	newpods = Cases[newdata, XMLElement["pod", _, _]];

	joinedpods = {If[DigitQ[#], ToExpression[#], #]&["position" /. #[[2]]], #}& /@ Join[newpods, oldpods];
	Join[#[[1,2]]& /@ SplitBy[SortBy[joinedpods, {First}], First], nonpods]
]


addRecalculateQueryInfo[oldinfo_, newinfo_] := {oldinfo, newinfo}

addRecalculateNonPods[olddata_, newdata_] := Flatten[{olddata, newdata}]


(* ::Subsubsection::Closed:: *)
(*"URL"*)


WolframAlpha[str_String, "URL", opts:OptionsPattern[]] := 
	Block[{baseurl, baseparams, async, format, assumption, includepodid, excludepodid, podstate, podwidth,
			ignorecase, timeouts, msessinfo, reinterpret, interactive, rawparameters, params, methodOptions},

		methodOptions = allMethodOptions[opts];
		
		baseurl = "OriginalURL" /. methodOptions;
		If[StringQ[baseurl],
			{baseurl, baseparams} = splitURL[baseurl],
			(* else *)			
			baseurl = "Server" /. methodOptions;
			baseurl = $AlphaQueryBaseURL[baseurl] <> $AlphaQueryJSP;
			baseparams = {};
		];
		
		async = OptionValue[Asynchronous];
		async = Switch[async,
			_Integer, "async" -> ToString[async, InputForm],
			_?NumericQ, "async" -> ToString[Re[N[async]], InputForm],
			False, "async" -> "false",
			True | All | _, "async" -> "true"
		];
		
		assumption = OptionValue[InputAssumptions];
		assumption = Switch[assumption,
			_String, "assumption" -> assumption,
			{__String}, Map["assumption" -> # &, assumption],
			_, {}
		];
		
		includepodid = OptionValue[IncludePods];
		includepodid = Switch[includepodid,
			_String, "includepodid" -> urlencode[includepodid],
			{__String}, Map["includepodid" -> urlencode[#] &, includepodid],
			_, {}
		];
		
		excludepodid = OptionValue[ExcludePods];
		excludepodid = Switch[excludepodid,
			_String, "excludepodid" -> urlencode[excludepodid],
			{__String}, Map["excludepodid" -> urlencode[#] &, excludepodid],
			_, {}
		];
		
		If[includepodid =!= {} && excludepodid =!= {},
			Message[WolframAlpha::notboth, includepodid, excludepodid];
			excludepodid = {}
		];
		
		podstate = OptionValue[PodStates];
		podstate = Switch[podstate,
			_String, "podstate" -> urlencode[podstate],
			{__String}, "podstate" -> StringJoin[Riffle[urlencode /@ podstate, ","]],
			_, {}
		];
		
		podwidth = Flatten[{ OptionValue[PodWidth] }];
		podwidth = MapThread[If[NumericQ[#], #, #2]&,
			{ PadRight[podwidth, 4, Automatic], PadRight[Flatten[{$AlphaQueryPodWidth}], 4, Automatic] }
		];
		podwidth = Flatten[MapThread[
			Which[
				IntegerQ[#], #2 -> ToString[#, InputForm],
				NumericQ[#], #2 -> ToString[Re[N[#]], InputForm],
				True, {}
			]&,
			{ podwidth, {"width", "maxwidth", "plotwidth", "infowidth"} }
		]];
		
		format = "Formats" /. methodOptions;
		If[format === Automatic, format = {"cell", "minput", "msound", "dataformats"}];
		format = Switch[format,
			_String, "format" -> format,
			{__String}, "format" -> StringJoin[Riffle[format, ","]],
			_, {}
		];
		
		reinterpret = "Reinterpret" /. methodOptions;
		If[reinterpret === Automatic, reinterpret = $AlphaQueryReinterpret[]];
		reinterpret = Switch[reinterpret,
			True, "reinterpret" -> "true",
			False, "reinterpret" -> "false",
			_, {}
		];
		
		interactive = "Interactive" /. methodOptions;
		If[interactive === Automatic, interactive = $AlphaQueryInteractive[]];
		interactive = Switch[interactive,
			True, "interactive" -> "true",
			False, "interactive" -> "false",
			_, {}
		];
		
		ignorecase = OptionValue[IgnoreCase];
		ignorecase = Switch[ignorecase,
			True, "ignorecase" -> "true",
			_, {}
		];
		
		timeouts = Flatten[{ OptionValue[TimeConstraint] }];
		timeouts =
			Which[
				IntegerQ[#], ToString[#, InputForm],
				NumericQ[#], ToString[Re[N[#]], InputForm],
				True, Automatic
			]& /@ PadRight[timeouts, 4, Automatic];
		timeouts = Cases[Thread[{Automatic, "scantimeout", "podtimeout", "formattimeout"} -> timeouts], _[_String, _String]];
		
		msessinfo = getMathematicaSessionInfo[str, True];
		msessinfo = Switch[msessinfo,
			{__Rule}, "msessinfo" -> urlencode @ ToString[msessinfo, InputForm],
			_, {}
		];

		rawparameters = "RawParameters" /. methodOptions;
		rawparameters = DeleteCases[Cases[Flatten[{rawparameters}], _String], ""];
		rawparameters = Flatten[StringSplit[#, "&"]& /@ rawparameters];
		rawparameters = StringSplit[#, "="]& /@ rawparameters;
		rawparameters = Flatten[If[Length[#] === 2, Rule @@ #, {}]& /@ rawparameters];
		
		params = Flatten[{
			baseparams,
			"input" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			ignorecase,
			async,
			timeouts,
			format,
			assumption,
			includepodid,
			excludepodid,
			podstate,
			reinterpret,
			podwidth,
			interactive,
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]],
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]],
			"systemid" -> urlencode[$SystemID],
			"mclient" -> $AlphaQueryMClient,
			"mmode" -> $AlphaQueryMMode,
			msessinfo,
			rawparameters
		}];
		
		extendedParams[baseurl, params]
	]



extendedParams[baseurl_, params_] :=
	Block[{id, newparams},
		id = If[TrueQ[$LocalServer], $AlphaQueryLocalServerID, If[StringQ[#], #, None]& @ $AlphaQueryAppID[]];
		If[!StringQ[id], Return[Internal`HouseKeep[baseurl, params]]];
		newparams = Flatten[{"?appid" -> id, params, "mlicense" -> $ActivationKey, "machineid" -> $MachineID}];
		StringJoin[baseurl, Riffle[{First[#], "=", Last[#]}& /@ Cases[newparams, _[_String, _String]], "&"]]
	]


(* ::Subsubsection::Closed:: *)
(*"CDFURL"*)


WolframAlpha[str_String, "CDFURL", opts:OptionsPattern[]] := 
	Block[{baseurl, assumption, rawparameters, params, methodOptions},

		methodOptions = allMethodOptions[opts];
				
		baseurl = "Server" /. methodOptions;
		If[!StringQ[baseurl], baseurl = getPreference["BaseURL", "Automatic"]];
		If[!StringQ[baseurl], baseurl = "production"];
		baseurl = baseurl /. $AlphaQueryBaseURLs;
		baseurl = Switch[ ToLowerCase[baseurl],
				"devel", "http://www.devel.wolframalpha.com/cdf/",
				"test", "http://www.test.wolframalpha.com/cdf/",
				"current", "http://www.current.wolframalpha.com/cdf/",
				"preview", "http://preview.wolframalpha.com/cdf/",
				"production" | "public" | "api" | "automatic", "http://www.wolframalpha.com/cdf/",
				_, "http://www.wolframalpha.com/cdf/" (* any other string is assumed to be invalid *)
		];
		
		assumption = OptionValue[InputAssumptions];
		assumption = Switch[assumption,
			_String, "a" -> assumption,
			{__String}, Map["a" -> # &, assumption],
			_, {}
		];

		rawparameters = "RawParameters" /. methodOptions;
		rawparameters = DeleteCases[Cases[Flatten[{rawparameters}], _String], ""];
		rawparameters = Flatten[StringSplit[#, "&"]& /@ rawparameters];
		rawparameters = StringSplit[#, "="]& /@ rawparameters;
		rawparameters = Flatten[If[Length[#] === 2, Rule @@ #, {}]& /@ rawparameters];
		
		params = Flatten[{
			"i" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			assumption,
			rawparameters
		}];
		
		StringJoin[baseurl, "?", Riffle[{First[#], "=", Last[#]}& /@ Cases[params, _[_String, _String]], "&"]]
	]




(* ::Subsubsection::Closed:: *)
(*"MathematicaParse"*)


(*
The set of calls for parsing are much like those for querying, but they
call a different jsp and have different import characteristics.
*)


WolframAlpha[str_String, "MathematicaParse", opts:OptionsPattern[]] := 
	Block[{result},
		result = WolframAlpha[str, "RawMathematicaParse", opts];
		Switch[result,
			{___Rule, "Parse" -> _String, ___Rule},
			ToExpression["Parse" /. result, InputForm, HoldComplete],
			_,
			$Failed
		]
	]


WolframAlpha[str_String, "RawMathematicaParse", opts:OptionsPattern[]] := 
	Block[{result, timeconstraint},
		timeconstraint = First[N[Flatten[{ OptionValue[TimeConstraint] }]]];
		If[!TrueQ[timeconstraint > 0], timeconstraint = TimeConstraint /. $WolframAlphaDefaultOptions];
		TimeConstrained[
			(
				result = qImport[WolframAlpha[str, "MathematicaParseURL", opts], "Text"];
				If[StringQ[result], ToExpression[StringTrim[result]], $Failed]
			),
			timeconstraint,
			Message[WolframAlpha::timeout, HoldForm[WolframAlpha[str, "MathematicaParse", opts]], timeconstraint];
			$TimedOut
		]
	] // dontRefresh


WolframAlpha[str_String, "MathematicaParseURL", opts:OptionsPattern[]] := 
	Block[{baseurl, assumption, msessinfo, params},

		baseurl = "Server" /. allMethodOptions[opts];
		baseurl = $AlphaQueryBaseURL[baseurl] <> $AlphaParseJSP;
		
		assumption = OptionValue[InputAssumptions];
		assumption = Switch[assumption,
			_String, "assumption" -> assumption,
			{__String}, Map["assumption" -> # &, assumption],
			_, {}
		];
		
		msessinfo = getMathematicaSessionInfo[str, False];
		msessinfo = Switch[msessinfo,
			{__Rule}, "msessinfo" -> urlencode @ ToString[msessinfo, InputForm],
			_, {}
		];
		
		params = Flatten[{
			"input" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			assumption,
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]],
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]],
			"systemid" -> urlencode[$SystemID],
			"mclient" -> $AlphaQueryMClient,
			"mmode" -> $AlphaQueryMMode,
			msessinfo
		}];
		
		extendedParams[baseurl, params]
	]



(* ::Subsubsection::Closed:: *)
(*"LinguisticAssistant"*)


WolframAlpha[str_String, "LinguisticAssistant" | "InlineInput", OptionsPattern[]] := AlphaIntegration`LinguisticAssistant[str, OptionValue[InputAssumptions]]


(* ::Subsubsection::Closed:: *)
(*"Validate"*)


WolframAlpha[str_String, "Validate", opts:OptionsPattern[]] := 
	Switch[
		WolframAlpha[str, "ValidateXML", opts],
		XMLObject["Document"][_, XMLElement["validatequeryresult", {___, "success" -> "true", ___}, _], _],
		True,
		XMLObject["Document"][_, XMLElement["validatequeryresult", {___, "success" -> "false", ___}, _], _],
		False,
		_,
		$Failed
	]


WolframAlpha[str_String, "ValidateXML", opts:OptionsPattern[]] := 
	Block[{timeconstraint},
		timeconstraint = First[N[Flatten[{ OptionValue[TimeConstraint] }]]];
		If[!TrueQ[timeconstraint > 0], timeconstraint = TimeConstraint /. $WolframAlphaDefaultOptions];
		TimeConstrained[
			qImport[WolframAlpha[str, "ValidateURL", opts], "XML"],
			timeconstraint,
			Message[WolframAlpha::timeout, HoldForm[WolframAlpha[str, "Validate", opts]], timeconstraint];
			$TimedOut
		]
	] // dontRefresh


WolframAlpha[str_String, "ValidateURL", OptionsPattern[]] :=
	Block[{baseurl, params},
		
		baseurl = "Server" /. allMethodOptions[opts];
		baseurl = $AlphaQueryBaseURL[baseurl] <> $AlphaValidateJSP;
		
		params = {
			"input" -> urlencode[If[$AlphaQueryPrefixQ, $AlphaQueryPrefix, ""] <> str],
			"releaseid" -> urlencode[Internal`CachedSystemInformation["Kernel", "ReleaseID"]],
			"patchlevel" -> urlencode[Internal`CachedSystemInformation["Kernel", "PatchLevel"]],
			"systemid" -> urlencode[$SystemID],
			"mclient" -> $AlphaQueryMClient,
			"mmode" -> $AlphaQueryMMode
		};
		
		extendedParams[baseurl, params]
	]


(* ::Subsubsection::Closed:: *)
(*Notes for when the second argument is a list*)


(*
When the second argument to WolframAlpha[] is a list, it overrides any settings
given for IncludePods and ExcludePods.

This list can have one or two elements.

The first element is a podid, {podid, subpodid}, or list of such. Podids can be
strings, lists of strings, or All. Subpodids can be integers, lists of integers,
or All. The 0th subpod is used to indicate pod-level properties.

The second element is a property name, a list of property names, or All.

When the given list has only a first element (that is, only has a podid/subpodid
or a list of such), the default second argument is "Properties". The return
value in this case is a list of LHSs only; no actual data is returned.

When the given list has a unique first element and a unique second element other
than "Properties", the return value is just the single piece of data being
requested. If that piece of data doesn't exist, the return value is
Missing["NotAvailable"].

In the remaining cases, where either there is more than one podid/subpodid or
more than one property given, the result will be of the form {prop -> val, prop
-> val, ...}.

Any Missing["NotAvailable"] results will be removed if they came from a
speculative specification of All, and will be left alone if they came from a
literal request for a specific piece of information. The goal is for the result
to contain a rule for every specifically requested property. So this:

WolframAlpha["foobar", { {{"Result", {1,2}}, "fish"}, "Plaintext"}]

might return something like this (note the lack of "fish" in the result):

{
	{{"Result", 1}, "Plaintext"} -> "This is foobar",
	{{"Result", 2}, "Plaintext"} -> Missing["NotAvailable"]
}

*)



(*
The expandIDsAndProperties utility canonicalizes the user's second argument to
its fully expanded form, where the first element is a list of pairs of the form
{All | _String, All | _Integer}, and the second element is All or a list of
property names.
*)

expandIDsAndProperties[{a_}] := expandIDsAndProperties[{a, "Properties"}]

expandIDsAndProperties[{a_, b_}] := {expandIDs[a], expandProperties[Flatten[{b}]]}

expandIDsAndProperties[other_] := InvalidIDAndProperty[other]


expandIDs[a_String] := {{a, All}}

expandIDs[All] := {{All, All}}

expandIDs[{a : (All | _String), b : (All | _Integer)}] := {{a, b}}

expandIDs[{a : (All | _String), b : {__Integer}}] := Map[{a, #}&, b]

expandIDs[{a : {__String}, b : (All | _Integer)}] := Map[{#, b}&, a]

expandIDs[{a : {__String}, b : {__Integer}}] := Tuples[{a, b}]

expandIDs[list_List] := Join @@ Map[expandIDs, list]

expandIDs[other_] := {{InvalidPodID[other], InvalidSubpodID[]}}


expandProperties[{All}] := All

expandProperties[{a__String}] := {a}

expandProperties[other_] := InvalidProperty[other]


validExpandedIDsAndPropertiesQ[expr_] :=
	MatchQ[expr, { {{All | _String, All | _Integer}..}, All | {__String}}]


(*
Note that every list that ends in an actual property makes targeted, efficient
use of the API, requesting only those formats related to that property.
*)


propertyToFormat[a_List] := Union[Flatten[propertyToFormat /@ a]]
propertyToFormat["Plaintext"] = "plaintext"
propertyToFormat["Cell"] = "cell"
propertyToFormat["Content"] = "cell"
propertyToFormat["Input"] = "minput"
propertyToFormat["Output"] = "moutput"
propertyToFormat["MathML"] = "mathml"
propertyToFormat["HTML"] = "html"
propertyToFormat["Image"] = "image"
propertyToFormat["Sound"] = "msound"
propertyToFormat["DataFormats"] = "dataformats"
propertyToFormat["ComputableData"] = "computabledata"
propertyToFormat["FormattedData"] = "formatteddata"
propertyToFormat["FormulaData"] = "formuladata"
propertyToFormat["NumberData"] = "numberdata"
propertyToFormat["QuantityData"] = "quantitydata"
propertyToFormat["SoundData"] = "sounddata"
propertyToFormat["TimeSeriesData"] = "timeseriesdata"
propertyToFormat["Properties"] := $allFormats
propertyToFormat[All] := $allFormats
propertyToFormat[other_] = "plaintext"


$allFormats = {"plaintext", "minput", "moutput", (* "mathml", "html", *) "cell", "image",
	"sound", "msound", "dataformats", "computabledata", "formatteddata", "formuladata", "numberdata", "quantitydata", "sounddata", "timeseriesdata"};


(* ::Subsubsection::Closed:: *)
(*Second argument is a list of podids + subpodids + properties*)


WolframAlpha[str_String, list_List, opts:OptionsPattern[]] :=
	Block[{idp = expandIDsAndProperties[list]},
		getPropertyInformation[str, idp, opts] /; validExpandedIDsAndPropertiesQ[idp]
	]


getPropertyInformation[str_String, {ids:{{_,_}..}, props_}, opts___] :=
	Block[{includepods, result, fmt},

		fmt = If[props === {"Properties"}, "PodProperties", "PodInformation"];
		includepods = DeleteDuplicates[First /@ ids];
		includepods = If[MatchQ[includepods, {__String}], IncludePods -> includepods, Unevaluated[Sequence[]]];

		result = WolframAlpha[str, fmt, Method -> {"Formats" -> propertyToFormat[props]}, includepods, opts];

		extractProperties[result, ids, props]
	];


(* One specific result *)
extractProperties[result_, {{podid:Except[All], subpodid:Except[All]}}, {prop:Except["Properties"]}] := 
	{{podid, subpodid}, prop} /. result /. {{podid, subpodid}, prop} -> Missing["NotAvailable"]


(* List of LHSs, no actual data *)
extractProperties[result_, ids_, {"Properties"}] := Cases[result, {idsToPattern[ids], _}]


(* List of rules *)
extractProperties[result_, ids_, props_] := 
	Map[
		If[
			FreeQ[#, All],
			(* If the request is for a specific piece of information, return Missing[...] if it's missing *)
			# -> (# /. result /. # -> Missing["NotAvailable"]),
			(* If the request contains an All, then allow it to be absent from the return value *)
			Cases[result, _[{idToPattern[First[#]], propToPattern[Last[#]]}, _]]
		]&,
		Tuples[{ids, Flatten[{props}]}]
	] // Flatten


(* Utilities *)
idsToPattern[ids_] := Alternatives @@ Map[idToPattern, ids]
idToPattern[{podid:All, subpodid:All}] := {_, _}
idToPattern[{podid:All, subpodid_}] := {_, subpodid}
idToPattern[{podid_, subpodid:All}] := {podid, _}
idToPattern[{podid_, subpodid_}] := {podid, subpodid}

propToPattern[All] := _String
propToPattern[prop_String] := prop

propsToPattern[All] := _String
propsToPattern[{props__String}] := Alternatives[props]


(* ::Subsubsection::Closed:: *)
(*Held first argument*)


WolframAlpha[Hold[expr_], rest___] := WolframAlpha[ToString[Unevaluated[expr], InputForm], rest]


WolframAlpha[HoldPattern[Unevaluated][expr_], rest___] := WolframAlpha[ToString[Unevaluated[expr], InputForm], rest]


(* ::Subsubsection::Closed:: *)
(*Non-string first argument*)


(* Emit a message for now, although it may someday be possible to let W|A handle these natively *)


WolframAlpha[expr : Except[_String], rest___] := Null /; Message[WolframAlpha::string, 1, HoldForm[WolframAlpha[expr, rest]]]


(* ::Subsubsection::Closed:: *)
(*Unsupported second argument*)


WolframAlpha[str_String, fmt_, OptionsPattern[]] := Null /; Message[WolframAlpha::format, HoldForm[fmt]]




(* ::Subsubsection::Closed:: *)
(*Utility: mathematicaSessionInfo*)


getMathematicaSessionInfo[query_String, tryDialog_:True] := (
	If[tryDialog, mathematicaSessionInfoPermissionDialogIfNecessary[query, {}]];
	Switch[
		{$AlphaQueryGlobalSendMathematicaSessionInfo[], $AlphaQuerySendMathematicaSessionInfo, $Notebooks},
		
		(* In a kernel-only session, always opt out *)
		{_, _, False},
		{"Allowed" -> False},
		
		(* Otherwise, we're in a front end session. *)
		(* If the user has opted in or out globally, respect that setting *)
		{False, _, _},
		{"Allowed" -> False},

		{True, _, _},
		Join[{"Allowed" -> True}, mathematicaSessionInfo[query]],
		
		(* Otherwise, if the user has opted in or out locally, respect that setting *)
		{_, False, _},
		{"Allowed" -> False},
		
		{_, True, _},
		Join[{"Allowed" -> True}, mathematicaSessionInfo[query]],
		
		(* Otherwise, the user hasn't opted in or out yet. *)
		(* Don't get the session info for now. *)
		{_, _, _},
		{"Allowed" -> "Unknown"}
	]
)


(*
Put up the permission dialog if and only if at least one of the following is true:
- There is a variable name in the user's query is a symbol with OwnValues in their session, or
- There is an MInput Scanner result which contains an minput string which contains a 
	subexpression with head Out or head Placeholder.

Never send msessinfo from a kernel-only session.
*)


mathematicaSessionInfoPermissionDialogIfNecessary[query_String, xml_] :=
	If[mathematicaSessionInfoPermissionDialogNecessaryQ[query, xml], mathematicaSessionInfoPermissionDialog[query], None]

mathematicaSessionInfoPermissionDialogNecessaryQ[query_String, xml_] :=
Block[{percentlist, symbolnames, minputs},
	Which[
		(* In a kernel-only session, don't ask. *)
		$Notebooks === False,
		False,
		
		(* If the user has opted in or out globally, don't ask again *)
		MemberQ[{True, False}, $AlphaQueryGlobalSendMathematicaSessionInfo[]],
		False,
		
		(* If the user has opted in or out locally, don't ask again *)
		MemberQ[{True, False}, $AlphaQuerySendMathematicaSessionInfo],
		False,
		
		(* If the user is running in the plugin or in player, don't ask, just opt out *)
		CurrentValue["PluginEnabled"] || MemberQ[$ProductInformation, "ProductIDName" -> "MathematicaPlayer"],
		False,
		
		(* If the query string contains a symbol in the current context that has OwnValues, ask *)
		symbolnames = extractSymbolNames[query];
		symbolnames = Select[symbolnames, MatchQ[ToExpression[#, InputForm, OwnValues], {_RuleDelayed}]&];
		symbolnames =!= {},
		True,
		
		(* If the query string contains explicit references to %, ask *)
		percentlist = extractPercentReferences[query];
		percentlist =!= {},
		True,
		
		(*
		If the xml query results contain an MInput Scanner pod which contains an minput string which
		contains a subexpression with head Out or Placeholder, ask.
		*)
		minputs = Cases[xml, XMLElement["pod", {___, "scanner" -> "MInput", ___}, data_] :> data, Infinity];
		minputs = Cases[minputs, XMLElement["minput", _, {str_String}] :> str, Infinity];
		minputs = !FreeQ[ToExpression[#, InputForm, HoldComplete]& /@ minputs, _Out | _Placeholder];
		minputs,
		True,
		
		(* otherwise, don't ask *)
		True,
		False
	]
];


mathematicaSessionInfoPermissionDialog[query_] :=
DialogInput[{
	TextCell[cachedFrontEndResource["WAStrings", "SessionInfoQuestion"]],
	ExpressionCell[
		Grid[{{
			Button[Dynamic[FEPrivate`FrontEndResource["WAStrings", "SessionInfoNo"]], 
				$AlphaQuerySendMathematicaSessionInfo = False;
				setPreference[$FrontEnd, "SendMathematicaSessionInfo", Automatic];
				DialogReturn[False],
				Appearance -> "CancelButton"
			],
			Button[Dynamic[FEPrivate`FrontEndResource["WAStrings", "SessionInfoYesAlways"]],
				$AlphaQuerySendMathematicaSessionInfo = True;
				setPreference[$FrontEnd, "SendMathematicaSessionInfo", True];
				DialogReturn[True],
				Appearance -> "DialogBox"
			],
			Button[Dynamic[FEPrivate`FrontEndResource["WAStrings", "SessionInfoYesSession"]],
				$AlphaQuerySendMathematicaSessionInfo = True;
				setPreference[$FrontEnd, "SendMathematicaSessionInfo", Automatic];
				DialogReturn[True],
				Appearance -> "DefaultButton"
			]
		}}],
		TextAlignment -> Right
	],
	ExpressionCell[
		OpenerView[{
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "SessionInfoDetails"]],
			Panel[Grid[List @@@ mathematicaSessionInfo[query], Alignment -> {{Right, Left}, Top}]]
		}]
	]
	},
	WindowSize -> {500, FitAll},
	WindowTitle -> cachedFrontEndResource["WAStrings", "SessionInfoWindowTitle"]
]


mathematicaSessionInfo[query_String : ""] :=
Block[{percentlist, symbolnames},

	percentlist = Union[Select[{$Line - 1}, Positive], extractPercentReferences[query]];

	symbolnames = extractSymbolNames[query];

	Flatten[{
		"$Line" -> $Line,
		"$VersionNumber" -> $VersionNumber,
		If[MatchQ[#, _Missing], {}, "$GeoLocation" -> #]& @ ToExpression["System`$GeoLocation"],
		getSkeleton[percentlist, "Inputs"],
		getSkeleton[percentlist, "Outputs"],
		getSkeleton[symbolnames, "Symbols"]
	}]
]


extractSymbolNames[query_String] := 
	Select[
		StringCases[" " <> query <> " ",
			StringExpression[
				Except[{LetterCharacter, "$"}],
				a:{LetterCharacter, "$"},
				b:{WordCharacter, "$"}...,
				Except[{WordCharacter, "$"}]
			] :> StringJoin[a, b],
			Overlaps -> True
		],
		NameQ[#] && Context[#] === $Context &
	];


extractPercentReferences[query_String] := 
Block[{percent, percentpercent, percentn},
	percentpercent = StringCases[
		" " <> query <> " ",
		Except["%"] ~~ a:("%"..) ~~ Except["%" | DigitCharacter] :> StringLength[a]];
	percentn = StringCases[
		" " <> query <> " ",
		Except["%"] ~~ "%" ~~ a:(DigitCharacter..) ~~ Except[DigitCharacter] :> a];
	Select[Union[Flatten[{$Line - percentpercent, ToExpression /@ percentn}]], Positive]
]


getSkeleton[list_, type_] := 
Block[{result, f},
	f = Switch[type, "Inputs", toInputSkeleton, "Outputs", toOutputSkeleton, "Symbols", toSymbolSkeleton];
	result = (# -> f[#])& /@ list;
	result = Flatten[DeleteCases[result, _ -> None]];
	If[result === {} || result === None, {}, type -> result]
]


toInputSkeleton[n_] :=
	Replace[DownValues[In], { {___, _[_[n]] :> val_, ___} :> toSkeleton[val], _ -> None }]


toOutputSkeleton[n_] :=
	Replace[DownValues[Out], { {___, _[_[n]] :> val_, ___} :> toSkeleton[val], _ -> None }]


SetAttributes[toSymbolSkeleton, HoldFirst];

toSymbolSkeleton[a_String] := ToExpression[a, InputForm, toSymbolSkeleton]

(* Only return information for symbols that have a single OwnValue. Other values are not handled. *)

toSymbolSkeleton[a_Symbol] := 
	Replace[OwnValues[a], { {_ :> val_} :> toSkeleton[val], _ -> None }]


$MaxByteCount = 5000;
$MaxStringLength = 78;


SetAttributes[toSkeleton, HoldAllComplete]

toSkeleton[expr_] := 
	DeleteCases[
		{
			"Dimensions" -> Dimensions[Unevaluated[expr]],
			"Head" -> Replace[HoldComplete[expr], {
				HoldComplete[a_Symbol[___]] :> ToString[Unevaluated[a], InputForm],
				HoldComplete[a_ /; AtomQ[Unevaluated[a]]] :> ToString[Head[Unevaluated[a]], InputForm],
				_ -> None}],
			"StringLength" -> If[StringQ[Unevaluated[expr]], StringLength[expr], None],
			"PlotRange" -> Replace[HoldComplete[expr], {
				HoldComplete[(Graphics|Graphics3D)[__, PlotRange -> a_, ___]] |
				HoldComplete[(Graphics|Graphics3D)[_, {___, PlotRange -> a_, ___}]] :> ToString[Unevaluated[a], InputForm], _ -> None}],
			"Value" -> Block[{str},
				If[ (* Send the whole value if it's small, and converts to a small string *)
					ByteCount[Unevaluated[expr]] <= $MaxByteCount &&
					StringLength[str = ToString[Unevaluated[expr], InputForm]] <= $MaxStringLength,
					str,
					None
				]
			]
		},
		_ -> None
	]



(* ::Subsubsection::Closed:: *)
(*Utility: urlToWolframAlpha*)


(* This utility is basically the inverse of WolframAlpha[query, "URL", opts] *)



splitURL[url_String] := 
Block[{list},
	list = Flatten[MapAt[StringSplit[#, "?"] &, StringSplit[url, "&"], 1]];
	{First[list], Cases[StringSplit[#, "="]& /@ Rest[list], {param_String, value_String} :> param -> value]}
]


urlToWolframAlpha[url_String] := 
Block[{baseurl, list},

	list = Flatten[MapAt[StringSplit[#,"?"]&, StringSplit[url,"&"], 1]];
	baseurl = StringReplace[First[list], $AlphaQueryJSP -> ""];
	baseurl = If[baseurl === $AlphaQueryBaseURL[], {}, {"Server" -> baseurl /. $AlphaQueryBaseURLs}];

	list = Rest[list];
	list = StringSplit[list, "="];

	list = queryParameterToAlphaOption @@@ list;
	list = joinDuplicateOptions[list];

	list = Flatten[{
		"Input" /. list,
		"URL",
		Cases[list, _[_Symbol, _]],
		Method -> Join[baseurl, DeleteCases[Cases[list, _[_String, _]], _["Input", _]]]
	}];

	Hold[WolframAlpha[##]]& @@ list
]


(* Some parameters have no analog in Options[WolframAlpha], so discard them. *)

queryParameterToAlphaOption["appid", a_String] := Sequence[]

queryParameterToAlphaOption["machineid", a_String] := Sequence[]

queryParameterToAlphaOption["mclient", a_String] := Sequence[]

queryParameterToAlphaOption["mlicense", a_String] := Sequence[]

queryParameterToAlphaOption["mmode", a_String] := Sequence[]

queryParameterToAlphaOption["patchlevel", a_String] := Sequence[]

queryParameterToAlphaOption["releaseid", a_String] := Sequence[]

queryParameterToAlphaOption["sig", a_String] := Sequence[]

queryParameterToAlphaOption["systemid", a_String] := Sequence[]


(* msessinfo is useful to see, but it doesn't belong in the final WolframAlpha[] input, so just decode and print it *)

queryParameterToAlphaOption["msessinfo", a_String] := Sequence[] /; (Print[urldecode @ a]; True)


(* the remaining parameters to map to something sensible *)

queryParameterToAlphaOption["assumption", a_String] := InputAssumptions -> {a}

queryParameterToAlphaOption["async", a_String] := Asynchronous -> Switch[a, "true", True, "false", False, other_, ToExpression[a]]

queryParameterToAlphaOption["excludepodid", a_String] := ExcludePods -> {urldecode @ a}

queryParameterToAlphaOption["format", a_String] := "Formats" -> StringSplit[a, ","]

queryParameterToAlphaOption["formattimeout", a_String] := TimeConstraint -> {Automatic, Automatic, Automatic, ToExpression[a]}

queryParameterToAlphaOption["ignorecase", a_String] := IgnoreCase -> If[a === "true", True, False]

queryParameterToAlphaOption["includepodid", a_String] := IncludePods -> {urldecode @ a}

queryParameterToAlphaOption["infowidth", a_String] := PodWidth -> {Automatic, Automatic, Automatic, ToExpression[a]}

queryParameterToAlphaOption["input", a_String] := "Input" -> urldecode[a]

queryParameterToAlphaOption["interactive", a_String] := "Interactive" -> If[a === "true", True, False]

queryParameterToAlphaOption["maxwidth", a_String] := PodWidth -> {Automatic, ToExpression[a]}

queryParameterToAlphaOption["plotwidth", a_String] := PodWidth -> {Automatic, Automatic, ToExpression[a]}

queryParameterToAlphaOption["podstate", a_String] := PodStates -> urldecode /@ StringSplit[a, ","]

queryParameterToAlphaOption["podtimeout", a_String] := TimeConstraint -> {Automatic, Automatic, ToExpression[a]}

queryParameterToAlphaOption["reinterpret", a_String] := "Reinterpret" -> If[a === "true", True, False]

queryParameterToAlphaOption["scantimeout", a_String] := TimeConstraint -> {Automatic, ToExpression[a]}

queryParameterToAlphaOption["width", a_String] := PodWidth -> {ToExpression[a]}

queryParameterToAlphaOption[other_, value_] := "RawParameters" -> StringJoin[other, "=", value]

queryParameterToAlphaOption[other___] := Sequence[]


joinDuplicateOptions[opts_List] := Map[joindup[#, Cases[opts, _[#, val_] :> val]]&, Union[First /@ opts]]

joindup[opt_, {val_}] := opt -> val

joindup[Asynchronous, vals_] := Asynchronous -> False (* this is what the API does *)

joindup[PodWidth, vals_] := PodWidth -> ( First[Sort[#]]& /@ Flatten[vals, {{2}, {1}}] )

joindup[TimeConstraint, vals_] := TimeConstraint -> ( First[Sort[#]]& /@ Flatten[vals, {{2}, {1}}] )

(* default case *)
joindup[opt_, vals_] := opt -> Flatten[vals]


(* ::Subsubsection::Closed:: *)
(*Utility: qImport*)


dontRefresh = Function[{expr}, Refresh[expr, None], HoldFirst]

qImport[args___] := 
	Switch[$AlphaQueryQuiet,
		True, Quiet[Import[args]],
		False, Import[args],
		"Log", loggingImport[args],
		Automatic | _,
		(* Replace the FetchURL::conopen message with the WolframAlpha::conopen message *)
		redirectMessages[
			Import[args],
			Utilities`URLTools`FetchURL::conopen,
			Message[WolframAlpha::conopen]
		]
	] // dontRefresh



SetAttributes[redirectMessages, HoldAll]

redirectMessages[expr_, msgs_, new_] :=
	Module[{result}, Quiet[Check[result = expr, new; result, msgs], msgs]]


loggingImport[url_, args___] := 
	Block[{t0, t1, result},
		t0 = AbsoluteTime[];
		result = Import[url, args];
		t1 = AbsoluteTime[];
		
		writeToLog[
			Column[
				Flatten[{
					Style[ToDate[t0]],
					Defer[Import[url, args]],
					If[StringMatchQ[url, "*?*"],
						OpenerView[{"decoded", Column[urldecode /@ StringSplit[url, {"?", "&"}]]}, False],
						{}
					],
					Row[{If[# < 10000, Mouseover[Style[#, "Hyperlink"], result], #]& @ ByteCount[result], " bytes in ", t1 - t0, " seconds"}]
				}],
				Dividers -> {False, Center},
				FrameStyle -> Dotted,
				Spacings -> 1.5,
				BaselinePosition -> {1,1}
			],
			CellDingbat -> Which[
				StringMatchQ[url, "*query.jsp*"], StyleBox["\[FilledCircle]", FontSize -> 18, FontColor -> RGBColor[0,0,1]],
				StringMatchQ[url, "*mparse.jsp*"], StyleBox["\[FilledDiamond]", FontSize -> 18, FontColor -> RGBColor[0,1,0]],
				StringMatchQ[url, "*recalc.jsp*"], StyleBox["\[Star]", FontSize -> 36, FontColor -> RGBColor[1,0,0]],
				True, StyleBox["\[EmptyCircle]", FontSize -> 18, FontColor -> RGBColor[0,0,1]]
			]
		];
		result
	]



writeToLog[expr_, opts___] :=
(
	If[!MemberQ[Notebooks[], $AlphaQueryLogNotebook],
		$AlphaQueryLogNotebook = NotebookPut[
			Notebook[{},
				WindowTitle -> "Alpha Query Log",
				WindowMargins -> {{Automatic,0},{Automatic,0}},
				DockedCells -> {Cell[BoxData[ButtonBox["clear",
					Appearance -> "DialogBox",
					ButtonFunction :> FrontEndExecute[{
						FrontEnd`SelectionMove[FrontEnd`ButtonNotebook[], All, Notebook],
						FrontEnd`NotebookDelete[FrontEnd`ButtonNotebook[]]}]]], "DockedCell"]},
				Background -> GrayLevel[0.95],
				Saveable -> False
			]
		]
	];
	SelectionMove[$AlphaQueryLogNotebook, After, Notebook];
	NotebookWrite[$AlphaQueryLogNotebook, Cell[BoxData[ToBoxes[expr]], "Output", Background -> White, opts]]
);




(* ::Subsection::Closed:: *)
(*AlphaQuery[]*)


doQuerySideEffects[query_String] :=
	If[ToLowerCase[query] === "developer preferences", AlphaQueryPreferences[]; True];


queryBoxesToQueryString[str_String, fmt_] := str

queryBoxesToQueryString[BoxData[boxes_] | boxes_, fmt_] :=
	First[ MathLink`CallFrontEnd[ FrontEnd`ExportPacket[BoxData[boxes], "InputText"] ] ]


returnNull = Null

returnNull /: MakeBoxes[returnNull, fmt_] := MakeBoxes[Null, fmt]


AlphaIntegration`AlphaQuery[boxes_, fmt_] := 
	Block[{opts = alphaQueryOptions[], query, xml, returnNull, conopen=False, $AlphaQueryMMode = "query"},
		query = StringTrim[queryBoxesToQueryString[boxes, fmt]];
		If[query === "", Return[Null]];
		If[doQuerySideEffects[query], Return[Null]];
		redirectMessages[
			xml = WolframAlpha[query, "ProcessedXML", opts];
			If[mathematicaSessionInfoPermissionDialogIfNecessary[query, xml], xml = WolframAlpha[query, "ProcessedXML", opts]],
			WolframAlpha::conopen,
			conopen = True
		];
		CellPrint[
			If[errorBlobQ[conopen, xml, query, opts],
				formatErrorBlob[conopen, xml, query, opts],
				formatAlphaXMLCell[xml, query, opts]
			]
		];
		returnNull
	]


AlphaIntegration`AlphaQueryInputs[boxes_, fmt_] :=
	Block[{opts = alphaQueryInputsOptions[], query, xml, returnNull},
		query = StringTrim[queryBoxesToQueryString[boxes, fmt]];
		If[query === "", Return[Null]];
		xml = WolframAlpha[query, "RawXML", opts];
		If[ mathematicaSessionInfoPermissionDialogIfNecessary[query, xml], xml = WolframAlpha[query, "RawXML", opts]];
		CellPrint[formatAlphaXMLCell[xml, query, opts]];
		returnNull
	]


formatAlphaXMLCell[xml_, query_, opts___] :=
	Cell[
		BoxData[ToBoxes[formatAlphaXML[xml, query, opts]]],
		"Print",
		CellMargins -> {{20, 10}, {Inherited, Inherited}}
	]


formatAlphaXML[XMLObject["Document"][docdata_, XMLElement["queryresult", values_, data_], _], query_, opts___] :=
	System`DynamicNamespace[
		"WolframAlphaQueryResults",
		formatQueryResults[values, data, query, resolveOptions[opts]],
		BaseStyle -> {Deployed -> True},
		Editable -> False,
		DeleteWithContents -> True,
		SelectWithContents -> True
	]

formatAlphaXML[other_, query_, opts___] := $Failed


errorBlobQ[True, xml_, query_, opts___] := True;

errorBlobQ[_, XMLObject["Document"][docdata_, XMLElement["queryresult", values_, data:{___, error:XMLElement["error", _, _], ___}], _], query_, opts___] := True

errorBlobQ[___] := False


formatErrorBlob[True, xml_, query_, opts___] := 
	formatErrorBlob[
		cachedFrontEndResource["WAStrings", "NoInternetTitle"],
		cachedFrontEndResource["WAStrings", "NoInternetBody"]
	]

formatErrorBlob[conopen_, XMLObject["Document"][docdata_, XMLElement["queryresult", values_, data:{___, error:XMLElement["error", _, _], ___}], _], query_, opts___] :=
	formatErrorBlob[conopen, error, query, opts]

formatErrorBlob[conopen_, error:XMLElement["error", _, {___, XMLElement["code", _, {code_}], ___}], query_, opts___] := 
	If[DigitQ[code] && 3000 <= ToExpression[code] <= 3999,
		formatErrorBlob[
			cachedFrontEndResource["WAStrings", "BlockedTitle"] <> " (" <> code <> ")",
			errorBlobMessage[code, error]
		],
		formatErrorBlob[
			cachedFrontEndResource["WAStrings", "ErrorTitle"] <> " (" <> code <> ")",
			errorBlobMessage[code, error]
		]
	]

formatErrorBlob[title_, msg_] := 
	Cell[
		BoxData[ToBoxes[
			innerFrame[
				Column[{
					Grid[{{Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "WAErrorIcon"]]], TextCell[title, Bold, StripOnInput -> True]}}],
					TextCell[msg, StripOnInput -> True]
					},
					Spacings -> 1.5
				],
				Background -> GrayLevel[0.96],
				ImageSize -> Automatic,
				FrameMargins -> {{20,20},{15,15}},
				BaseStyle -> {FontFamily -> "Helvetica"}
			]
		]],
		"Print",
		Deployed->True	
	]

formatErrorBlob[___] := $Failed


errorBlobMessage[code_, XMLElement["error", _, {___, XMLElement["mathematicamsg", _, {mathematicamsg_}], ___}]] :=
Block[{params, suffix, result},

	params = Flatten[{
		"error" -> code,
	
		buildid=Internal`$CreationID;
		If[StringQ[buildid], "buildid" -> buildid, {}],
	
		actkey=$ActivationKey;
		If[StringQ[actkey], "actkey" -> actkey, {}],
	
		machineid=$MachineID;
		If[StringQ[machineid], "machineid" -> machineid, {}]
	}];
	
	suffix = StringJoin["?", Riffle[{First[#], "=", Last[#]}& /@ params, "&"]];
	
	result = StringReplace[mathematicamsg, ("$$[" ~~ (text__) ~~ "][" ~~ (url__) ~~ "]$$") :>
		With[{fullurl = StringJoin[url, suffix]}, TraditionalForm[Hyperlink[text, fullurl]]]
	];
	
	Switch[Head[result],
		String, result,
		StringExpression, Row[List @@ result],
		_, result
	]
]

errorBlobMessage[code_, XMLElement["error", _, {___, XMLElement["msg", _, {msg_}], ___}]] :=
	errorBlobMessage[code, XMLElement["error", {}, {XMLElement["mathematicamsg", {}, {msg}]}]]

errorBlobMessage[code_, other_] := "Unknown error"







formatQueryResults[values:{___, "error" -> "true", ___}, data_, query_, opts___] :=
	formatFallThrough["error", values, data, query, opts]


(* "success" -> "false" is handled by the main formatQueryResults rule *)


formatQueryResults[values_, data_List, query_, opts___] := 
Block[{numpods, podvars, auxvars, expandedData, recalculateURL, boxversion = 1},
	numpods = Count[data, XMLElement["pod", _, _]];
	
	(* If we're going to be doing a recalculate, proactively add space for extra pods *)
	recalculateURL = "recalculate" /. values /. "recalculate" -> "";
	If[recalculateURL =!= "", numpods += $AlphaQueryRecalculateSpace; boxversion = 2];
	
	podvars = Thread[ToExpression["Typeset`pod" <> ToString[#], InputForm, Hold]& /@ Range[numpods], Hold];
	auxvars = Thread[ToExpression["Typeset`aux" <> ToString[#], InputForm, Hold]& /@ Range[numpods], Hold];
	If[podvars === {}, podvars = Hold[{}]];
	If[auxvars === {}, auxvars = Hold[{}]];
	If[ TrueQ[(AppearanceElements /. {opts} /. Options[WolframAlpha]) === {"UseInputLeft"}] ||
		MatchQ[Asynchronous /. {opts} /. Options[WolframAlpha], True | All | _?NumericQ],
		expandedData = data,
		expandedData = expandData[data]
	];
	buildDynamicModule[boxversion, values, expandedData, recalculateURL, podvars, auxvars, query, opts]
]


buildDynamicModule[boxversion_, values_, data_List, recalculateURL_, Hold[{podvarseq___}], Hold[{auxvarseq___}], query_, opts___] := 
DynamicModule[{ Typeset`q = query, Typeset`opts = {opts}, Typeset`elements, podvarseq, auxvarseq,
				Typeset`asyncpods, Typeset`nonpods, Typeset`initdone = False, Typeset`queryinfo,
				Typeset`sessioninfo, Typeset`showpods, Typeset`failedpods, Typeset`chosen, Typeset`open,
				Typeset`newq},
	
	Typeset`queryinfo = values;
	Typeset`sessioninfo = {"TimeZone" -> $TimeZone, "Date" -> DateList[], "Line" -> $Line, "SessionID" -> $SessionID};

	Typeset`showpods = Cases[data, XMLElement["pod", _, _]];
	
	Evaluate[Take[{podvarseq}, Length[Typeset`showpods]]] = Typeset`showpods;
	{auxvarseq} = {
		True (* opener state *),
		False (* whether a podstate is actively changing *),
		{False} (* whether each subpod state is actively changing *),
		$AlphaQueryShowLinksByDefault (* whether to show internal links as links *) }& /@ {podvarseq};
	Typeset`nonpods = DeleteCases[data, XMLElement["pod", _, _]];
	
	Typeset`showpods = Range[Length[Typeset`showpods]];
	Typeset`failedpods = Complement[Range[Length[{auxvarseq}]], Typeset`showpods];
	
	If[recalculateURL =!= "",
		(* add a temporary pod whose only purpose is to trigger the display of the async pod animator *)
		Evaluate[First[Cases[{podvarseq}, _Symbol]]] = XMLElement["pod", {"recalculate" -> "true"}, {"recalculate"}];
		AppendTo[Typeset`showpods, First[Typeset`failedpods]];
		Typeset`failedpods = Rest[Typeset`failedpods]
	];
	
	{Typeset`open, Typeset`chosen, Typeset`newq} = {"ExtrusionOpen", "ExtrusionChosen", "NewQuery"} /. allMethodOptions[opts];
	If[ !ListQ[Typeset`chosen], Typeset`chosen = {}];
	If[ !StringQ[Typeset`newq], Typeset`newq = query];
	
	
	Typeset`asyncpods = Cases[
		MapIndexed[Function[{x, y}, {First[y], Hold[x], x}, HoldAllComplete], Unevaluated[{podvarseq}]],
		{index_, Hold[var_], XMLElement["pod", {___, "async" -> url_, ___}, _]} :> {index, Hold[var], url}
	];
	
	Typeset`elements = OptionValue[WolframAlpha, {opts}, AppearanceElements];
	Switch[Typeset`elements,
		None, Typeset`elements = {},
		{__String}, Null,
		All, Typeset`elements = $AllAlphaQueryAppearanceElements,
		Automatic | _, Typeset`elements = $AutomaticAlphaQueryAppearanceElements
	];
	
	Dynamic[
		AlphaIntegration`FormatAlphaResults[Dynamic[{
			boxversion,
			{podvarseq}, {auxvarseq}, Typeset`chosen, Typeset`open, Typeset`elements, Typeset`q, Typeset`opts,
			Typeset`nonpods, Typeset`queryinfo, Typeset`sessioninfo, Typeset`showpods, Typeset`failedpods, Typeset`newq
		}]],
		TrackedSymbols :> {Typeset`showpods, Typeset`failedpods}
	],
	
	If[boxversion === 1,
		Initialization :> If[
			Not[Typeset`initdone],
			doAsyncUpdates[Hold[{podvarseq}], Typeset`asyncpods, Dynamic[Typeset`failedpods]];
			Typeset`asyncpods = {};
			Typeset`initdone = True
		],
		Initialization :> If[
			Not[Typeset`initdone],
			AlphaIntegration`DoAsyncInitialization[Hold[{
				boxversion,
				{podvarseq}, {auxvarseq}, Typeset`chosen, Typeset`open, Typeset`elements, Typeset`q, Typeset`opts,
				Typeset`nonpods, Typeset`queryinfo, Typeset`sessioninfo, Typeset`showpods, Typeset`failedpods, Typeset`newq,
				recalculateURL, Typeset`asyncpods
			}]];
			Typeset`asyncpods = {};
			Typeset`initdone = True
		]
	] // Evaluate,
	SynchronousInitialization -> False

]




(* Version 1 *)

AlphaIntegration`FormatAlphaResults[arg:Dynamic[{1|2, {podvarseq___}, {auxvarseq___}, chosen_, open_, elements_, q_, opts_, nonpods_, qinfo_, info_, showpods_, failedpods_, newq_}]] := 
	If[!MemberQ[elements, "Extrusion"], 
		#,
		extrusionOpener[#, nonpods, Dynamic[q], Dynamic[opts], Dynamic[chosen], Dynamic[open], Dynamic[newq]] 
	]& @
	If[!MemberQ[elements, "CDFWarnings" | "CDFAssumptions"],
		#,
		Column[Flatten[{
			If[!MemberQ[elements, "CDFWarnings"], {}, FormatAllWarnings["CDFWarnings", nonpods, Dynamic[newq], Dynamic[q], Sequence @@ opts]],
			If[!MemberQ[elements, "CDFAssumptions"], {}, FormatAllAssumptions["CDFAssumptions", nonpods, Dynamic[newq], Dynamic[q], Dynamic[opts]]],
			#
			}],
			BaseStyle -> {"DialogStyle", AutoItalicWords -> {"Mathematica"}},
			Spacings -> 2
		]
	]& @ 
	If[!TrueQ["AdditionalInformation" /. allMethodOptions[opts]],
		#,
		Column[Flatten[{
			#,
			FormatAdditionalInformation[arg]
			}],
			BaseStyle -> {"DialogStyle", AutoItalicWords -> {"Mathematica"}},
			Spacings -> 2
		]
	]& @
	outerFrame[
		Column[Flatten[{
			If[!MemberQ[elements, "Warnings"], {}, FormatAllWarnings["Warnings", nonpods, Dynamic[newq], Dynamic[q], Sequence @@ opts]],
			If[!MemberQ[elements, "Assumptions"], {}, FormatAllAssumptions["Assumptions", nonpods, Dynamic[newq], Dynamic[q], Dynamic[opts]]],
			Which[
				elements === {"UseInputLeft"},
				FormatAllMInputs[{podvarseq}],

				MemberQ[elements, "Pods"] && {podvarseq} =!= {},
				FormatAllPods[Dynamic[{podvarseq}], Dynamic[{auxvarseq}], Dynamic[chosen], Dynamic[open], Dynamic[showpods], Dynamic[failedpods], Dynamic[newq], Dynamic[q], Dynamic[opts], elements, info],

				MemberQ[elements, "Unsuccessful"],
				FormatAllUnsuccesses[nonpods, Dynamic[newq], Dynamic[q], Dynamic[opts]],

				True,
				{}
			],
			BrandingStripe[elements, nonpods, Dynamic[q], Dynamic[opts]]
			}],
			BaseStyle -> {"DialogStyle", AutoItalicWords -> {"Mathematica"}},
			Spacings -> If[MemberQ[elements, "ContentPadding"], 1.1, 0.9]
		],
		MemberQ[elements, "ContentPadding"]
	]


(* Version unknown *)

AlphaIntegration`FormatAlphaResults[Dynamic[{unknown_, ___}]] := 
	Framed[
		Grid[{{
			Item[TextCell[Row[{
				"Displaying this content requires a more recent version of ",
				Style["Mathematica", "IT"],
				" or ",
				Style["Mathematica Player", "IT"],
				". ",
				Hyperlink["\[RightSkeleton]", "http://www.wolfram.com/"]}]],
				Background -> GrayLevel[1],
				Frame -> 1,
				FrameStyle -> LightGray
			]}}],
		BaseStyle -> "DialogStyle",
		RoundingRadius -> 5,
		FrameStyle -> LightGray,
		Background -> GrayLevel[0.965],
		FrameMargins -> 5
	]





(*
The formatting of the main column of frames follows the following conventions.

The outer FrameBox has ImageSize -> Full and RoundingRadius -> 8.

All the inner FrameBoxes have ImageSize -> Full and RoundingRadius -> 5.

The overall effect is something like this:

Framed[Column[Table[
	Framed[i, ImageSize -> Full, RoundingRadius -> 5],
	{i, 10}]], ImageSize -> Full, RoundingRadius -> 8]

Then there are GridBoxes within each inner frame. Those use an ItemSize of Scaled[0.998] or
Scaled[0.499], depending on whether they are full-width or half-width.

Framed[Column[Table[
	Framed[Grid[{{i}}, Alignment -> Left, ItemSize -> Scaled[0.998]], ImageSize -> Full, RoundingRadius -> 5],
	{i, 10}]], ImageSize -> Full, RoundingRadius -> 8]
 
 or
 
Framed[Column[Table[
	Framed[Grid[{{i, i + 1}}, Alignment -> {{Left, Right}, Automatic}, ItemSize -> Scaled[0.499]], ImageSize -> Full, RoundingRadius -> 5],
	{i, 10}]], ImageSize -> Full, RoundingRadius -> 8]

*)


outerFrame[expr_, padding_] := 
	Framed[expr,
		RoundingRadius -> 8,
		FrameStyle -> LightGray,
		Background -> GrayLevel[0.965],
		FrameMargins -> If[padding, 9, 7]
	]


innerFrame[expr_, opts___] := 
	Framed[expr,
		opts,
		RoundingRadius -> 5,
		FrameStyle -> LightGray,
		Background -> White,
		ImageSize -> Full,
		Alignment -> Top
	]


BrandingStripe[elements_, nonpods_, Dynamic[q_], Dynamic[opts_]] := 
Block[{left, right, itemsize},

	Switch[
		MemberQ[elements, #]& /@ {"CDFBrand", "Sources", "Brand"},
		{False, False, False}, left = {}; right = {},
		{False, False, True }, left = {}; right = webBrand[True, {}, Dynamic[q], Dynamic[opts]],
		{False, True,  False}, left = {}; right = webBrand[False, FormatAllSourcesActions[nonpods], Dynamic[q], Dynamic[opts]],
		{False, True,  True }, left = {}; right = webBrand[True, FormatAllSourcesActions[nonpods], Dynamic[q], Dynamic[opts]],
		{True,  False, False}, left = computedByLink[]; right = {},
		{True,  False, True }, left = computedByLink[]; right = webBrand[True, {}, Dynamic[q], Dynamic[opts]],
		{True,  True,  False}, left = computedByLink[]; right = browserSourceInfoButton[AllSourceURLs[nonpods], Dynamic[q], Dynamic[opts]],
		{True,  True,  True }, left = computedByLink[]; right = browserSourceInfoButton[AllSourceURLs[nonpods], Dynamic[q], Dynamic[opts]]
	];		

	If[left === {} && right === {}, Return[{}]];

	If[left =!= {}, left = Item[left, Alignment -> {Left, Bottom}]];
	If[right =!= {}, right = Item[Row[right, "   "], Alignment -> {Right, Bottom}]];	

	itemsize = If[left === {} || right === {}, Scaled[0.998], Scaled[0.499]];
	
	innerFrame[
		Grid[{Flatten[{left, right}]}, ItemSize -> itemsize],
		Background -> None,
		FrameStyle -> None,
		FrameMargins -> {{Automatic, Automatic}, {0,0}}
	]
]


computedByLink[] := 
	Hyperlink[
		Grid[{{Dynamic[FEPrivate`FrontEndResource["WAStrings", "ComputedBy"]]}},
			Alignment -> Baseline,
			BaselinePosition -> {1,1},
			BaseStyle -> {FontColor -> Black},
			ItemSize -> Full,
			Spacings -> {0,0}
		],
		"http://www.wolfram.com/mathematica",
		ActiveStyle -> {},
		Alignment -> Left,
		ButtonFunction :> (FrontEndExecute[{NotebookLocate[#2, "OpenInNewWindow" -> CurrentValue["HyperlinkModifierKey"]]}]&)
	]



browserSourceInfoButton[sourceurls:{__}, Dynamic[q_], Dynamic[opts_]] := 
Block[{agent, url},
	agent = Cases[Flatten[{opts}], (Method -> {___, "UserAgentString" -> a_String, ___}) :> a];
	agent = If[Length[agent] === 0, "", First[agent]];
	If[StringMatchQ[agent, "*MSIE *"],
		(* IE does not respond to javascript urls, so we use a plain web link instead *)
		url = StringJoin[
			"http://www.wolframalpha.com/input/sources.jsp?i=", urlencode[q],
			Map[{"&sources=", #} &, StringReplace[FileNameTake /@ sourceurls, "SourceInformationNotes.html" -> ""]],
			"&back=1"
		],
		(* otherwise, javascript is better *)
		url = StringJoin["javascript:openCdfSources(new Array(", Riffle[{"\"", #, "\""}& /@ sourceurls, ","], "))"]
	];
	{Hyperlink[
		Row[{Dynamic[FEPrivate`FrontEndResource["WAStrings", "SourceInfo"]], " \[RightGuillemet]"}],
		url,
		BaseStyle -> {"Hyperlink", FontColor -> Orange},
		ActiveStyle -> {"HyperlinkActive", FontColor -> Red},
		ButtonNote -> ""
	]}
];


browserSourceInfoButton[_, _, _] := {}





webBrand[brandQ_, sourceactions_, Dynamic[q_], Dynamic[opts_]] :=
Block[{button, menuactions, menu},
	button = If[brandQ,
		Hyperlink[
			Dynamic[RawBoxes[FEPrivate`FrontEndResource["WALocalizableBitmaps", "WolframAlpha"]]],
			"http://www.wolframalpha.com",
			Alignment -> Left
		],
		{}
	];
	
	menuactions = If[brandQ,
		Flatten[{
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "Feedback"]] :> (NotebookLocate[{URL[#], None}]&[
				StringJoin[
					"mailto:feedback@wolframalpha.com?",
					(* ExternalService`EncodeString[] works better in testing mail clients than the Java-based urlencode *)
					"subject=", ExternalService`EncodeString["Feedback for Wolfram|Alpha in Mathematica"],
					"&body=",
					With[{q=q, assumptions = Cases[Flatten[{InputAssumptions /. Flatten[{opts}] /. InputAssumptions -> {}}], _String]},
						If[assumptions === {},
							ExternalService`EncodeString[ToString[Unevaluated[WolframAlpha[q]],InputForm]],
							ExternalService`EncodeString[ToString[Unevaluated[WolframAlpha[q, InputAssumptions -> assumptions]],InputForm]]
						]
					]
				]
			]),
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "WebVersion"]] :> (
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				NotebookLocate[{URL[#], None}]&[
					"http://www.wolframalpha.com/input/?i=" <> urlencode[q] <> 
					With[{assumptions = Cases[Flatten[{InputAssumptions /. Flatten[{opts}] /. InputAssumptions -> {}}], _String]},
						If[assumptions === {}, "", Map[{"&a=", #}&, assumptions]]
					]
				]
			),
			If[sourceactions === {}, {}, Delimiter],
			sourceactions
		}],
		sourceactions
	];
		
	menu = If[menuactions === {}, {},
		Tooltip[
			ActionMenu[
				Dynamic[RawBoxes[FEPrivate`FrontEndResource["FEBitmaps", "CirclePlusIcon"]]],
				menuactions,
				Appearance -> None
			],
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "Links"]]
		]
	];
	
	Flatten[{button, menu}]
]




AlphaIntegration`DoAsyncInitialization[Hold[{1|2, {podvarseq___}, {auxvarseq___}, chosen_, open_, elements_, q_, opts_, nonpods_, qinfo_, info_, showpods_, failedpods_, newq_, recalculateURL_, asyncpods_}]] :=
Block[{},
	doAsyncUpdates[Hold[{podvarseq}], asyncpods, Dynamic[failedpods]];
	asyncpods = {};
	doAsyncRecalculate[recalculateURL, Hold[{podvarseq}], Hold[qinfo], Hold[nonpods], Hold[asyncpods], Dynamic[showpods], Dynamic[failedpods]];
	doAsyncUpdates[Hold[{podvarseq}], asyncpods, Dynamic[failedpods]];
]



doAsyncRecalculate[recalculateURL_, Hold[{podvarseq___}], Hold[queryinfo_], Hold[nonpods_], Hold[asyncpods_], Dynamic[showpods_], Dynamic[failedpods_]] :=
Block[{xml, queryresultvalues, queryresultdata, oldpods, newpods, n, r},
	CheckAbort[
		If[recalculateURL === "", Return[]];
		
		(* otherwise, there is a recalculate URL *)
		xml = qImport[extendedParams @@ splitURL[recalculateURL], "XML"];

		{queryresultvalues, queryresultdata} = {Part[#, 2], Part[#, 3]}& @
			First[Flatten[{Cases[xml, XMLElement["queryresult", _, _], Infinity], XMLElement["queryresult", {}, {}]}]];
		queryinfo = addRecalculateQueryInfo[queryinfo, queryresultvalues];
		nonpods = addRecalculateNonPods[nonpods, DeleteCases[queryresultdata, XMLElement["pod", _, _]]];
		
		newpods = Cases[queryresultdata, XMLElement["pod", _, _]];
		If[Length[newpods] === 0,
			(* delete the recalc loading pod *)
			r = Last[showpods];
			showpods = DeleteCases[showpods, r];
			failedpods = Union[Flatten[{r, failedpods}]];
			ClearAll @@ Part[Hold /@ Unevaluated[{podvarseq}], r];
			Return[]
		];
		
		(* otherwise, recalculate did return some pods. Most[] avoids the one showing the async animator *)
		oldpods = Most[Cases[{podvarseq}, _XMLElement]];
		newpods = addRecalculateData[oldpods, newpods];
		n = Length[newpods];

		ClearAll[podvarseq];
		Evaluate[Take[{podvarseq}, n]] = newpods;

		asyncpods = Cases[
			MapIndexed[Function[{x, y}, {First[y], Hold[x], x}, HoldAllComplete], Unevaluated[{podvarseq}]],
			{index_, Hold[var_], XMLElement["pod", {___, "async" -> url_, ___}, _]} :> {index, Hold[var], url}
		];
		showpods = Range[n];
		failedpods = Complement[Range[Length[{podvarseq}]], showpods]
		,
		(* if aborted, make sure to remove the recalc pod showing the async animator *)
		If[
			r = Position[{podvarseq}, XMLElement["pod", {"recalculate" -> "true"}, {"recalculate"}], 1];
			MatchQ[r, {{_Integer}}],
			r = r[[1,1]];
			showpods = DeleteCases[showpods, r];
			failedpods = Union[Flatten[{r, failedpods}]];
			ClearAll @@ Part[Hold /@ Unevaluated[{podvarseq}], r];
		];
	]
]


(* Support for legacy notebooks *)
AlphaIntegration`Dump`doAsyncUpdates[args___] := doAsyncUpdates[args]

doAsyncUpdates[Hold[{podvarseq__}], asyncpods_, Dynamic[failedpods_]] :=
	CheckAbort[
		Scan[expandPod, Thread[Hold[{podvarseq}]]];
		getAsyncPods[asyncpods, Dynamic[failedpods]]
		,
		Scan[markAsyncFailure[#, "aborted", Dynamic[failedpods]]&, asyncpods]
	]


expandPod[Hold[var_]] := Block[{expanded = expandData[var]}, If[var =!= expanded, var = expanded]]


getAsyncPods[asyncpods:{__}, Dynamic[failedpods_]] := 
Block[{indexes, vars, urls},
	{indexes, vars, urls} = Transpose[asyncpods];
	multifetchURL[urls, MapThread[Function[{index, var}, getAsyncPodCallback[index, var, Dynamic[failedpods], ##]&], {indexes, vars}]]
];


getAsyncPodCallback[index_, Hold[var_], Dynamic[failedpods_], url_String, filename_String, exception_] :=
Block[{pods},
	If[ exception === Null,	
		(* multifetchURL indicated success *)
		pods = Cases[qImport[filename, "XML"], XMLElement["pod", _, _], Infinity];
		pods = DeleteCases[pods, XMLElement["pod", {___, "error" -> "true", ___}, _]];
		If[pods === {}, markAsyncFailure[{index, Hold[var], url}, "data not found", Dynamic[failedpods]], var = expandData[First[pods]]],
		(* otherwise, multifetchURL indicated failure *)
		markAsyncFailure[{index, Hold[var], url}, "data not found", Dynamic[failedpods]]	
	]
]


markAsyncFailure[{index_, Hold[var_], url_}, msg_, Dynamic[failedpods_]] := 
(
	failedpods = Union[Flatten[{failedpods, index}]];
	If[MatchQ[var, XMLElement["pod", {___, "async" -> _, ___}, _]],
		var = Insert[var, "asyncfailed" -> msg, {2, -1}]
	];
)



expandCells[xml_] := xml /. {
	e:XMLElement["cell", _, {_String}] :> expandCell[e]
}


expandData[xml_] := xml /. {
	e:XMLElement["cell", _, {_String}] :> expandCell[e],
	e:XMLElement["img", values_, _] :> expandImage[e] /; FreeQ[values, "ImportedData"],
	e:XMLElement["sound", values:{___, "type" -> "audio/x-wav", ___}, _] :> expandSound[e] /; FreeQ[values, "ImportedData"]
}



(*
The cell expression string returned by the API will be the result of 
one of these:

	Compress[cellexpr]
		or
	ToString[FullForm[cellexpr]]
*)

expandCell[XMLElement["cell", values_, {str_String}]] :=
Block[{cellexpr, c, s},
	cellexpr = If[Head[#] === Uncompress, c=False; str, c=True; #]& @ Quiet[Uncompress[str]];
	If[StringQ[cellexpr], cellexpr = ToExpression[cellexpr]; s=True, s=False];
	If[Head[cellexpr] =!= Cell, Return[Sequence[]]];
	cellexpr = DeleteCases[
		cellexpr /. TagBox[a__, TagBoxNote -> Except[_String], b___] -> TagBox[a, b],
		((FontSize | FontFamily | NumberSeparator | LineSpacing | PageWidth | ShowCellBracket | CellMargins) -> _) | (Magnification -> 1 | 1.)
	];	
	XMLElement["cell", {"compressed" -> c, "string" -> s}, {cellexpr}]
]


expandImage[XMLElement["img", values_, data_]] :=
Block[{src, importedData},
	src = "src" /. values /. "src" -> $Failed;
	importedData = If[src === $Failed, $Failed, qImport[src]];
	If[Head[importedData] === Image, importedData = Image[importedData, Magnification -> 1]];
	XMLElement["img", Append[values, "ImportedData" -> importedData], data]
]


expandSound[XMLElement["sound", values_, data_]] := 
Block[{url, importedData},
	url = "url" /. values /. "url" -> $Failed;
	importedData = If[url === $Failed, $Failed, qImport[url]];
	XMLElement["sound", Append[values, "ImportedData" -> importedData], data]
]


(* ::Subsubsection::Closed:: *)
(*multifetchURL*)


(* MultiFetchURL utility from Todd Gayley *)

(* Notes *)

(*
The MultiFetchURL function takes a list of URLs and a list of callback
functions. It downloads the content of each URL in parallel, and when each one
finishes it calls the callback function. The return value is the list of the
results from the callback functions.

The callback functions are called with three arguments: the original URL, the
filename of the downloaded file, and the Java exception object (or Null if
success).

MultiFetchURL does no work other than performing the download, calling the
callback function, and cleaning up. The callback func has to perform the
assignment to the symbol and any other work. Its return value is what
MultiFetchURL returns for that URL. MultiFetchURL deletes the file after the
callback function is called, and it releases the exception object, so don't try
to cache it for later inspection.

Note that MultiFetchURL doesn't issue any messages itself, as it's considered
an internal utility in this package. Examining the exceptions returned by each
fetcher (third argument to the callback function) would a pretty thorough way of
detecting and/or reporting errors, if needed.
*)


multifetchURL[urls : {__String}, userFunctions_List, socketTimeout : _Integer : 10000] :=
Module[{fetcherTable, pm, fetchers, exc, file, expr, newRow},
	Needs["JLink`"];
	pm = Symbol["PacletManager`Package`getPacletManager"][];
	fetchers = pm@createURLFetcher[#, Close[OpenTemporary[]], False] & /@ urls;
	#@setSocketTimeout[socketTimeout] & /@ fetchers;
	#@start[] & /@ fetchers;
	fetcherTable = Thread[{fetchers, urls, userFunctions}];	
	(*
	Loop over fetcherTable, waiting for each fetcher to finish. When
	it does, replace its row in fetcherTable with a "result" row:
	{Null, callbackResult}.
	*)
	While[! MatchQ[fetcherTable, {{Null, __} ...}],
		fetcherTable = Function[
			{fetcher, url, func},
			If[fetcher =!= Null && fetcher@isFinished[],
				exc = fetcher@getException[];
				file = fetcher@getFilename[];
				Symbol["JLink`ReleaseJavaObject"][fetcher];
				expr = func[url, file, exc];
				(*
				Replace the fetcher with Null to indicate this url is
				done, and put the result in the last slot. We don't
				need the url part any more, but we can't change the
				length of each row.
				*)
				newRow = {Null, url, expr};
				Symbol["JLink`ReleaseJavaObject"][exc];
				Quiet[DeleteFile[file]],
				(* else *)
				(* This fetcher not finished. Leave row unmodified. *)
				newRow = {fetcher, url, func}
				];
				newRow
		] @@@ fetcherTable;
		Pause[.05]
	];
	(* The results from the callback functions are the last argument of each row in the table. *)
	Last /@ fetcherTable
]


(* ::Subsection::Closed:: *)
(*FallThroughs*)


suggestionsButtonOpener[content_, type:("CDFNoResults" | "CDFReinterpret")] := 
	DynamicModule[{tip = False, display},
		display = content /. suggestionsDialogButtonPlaceholder[] :> 
			Tooltip[
				Button[
					Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "QuestionMarkSmallIcon"]]],
					tip = Not[tip],
					Appearance -> None,
					BaseStyle -> {ShowStringCharacters -> False}
				],
				cachedFrontEndResource["WAStrings", "SuggestionsTooltip"]
			];
		
		PaneSelector[{
			False -> Dynamic[display],
			True ->
				Column[{
					Dynamic[display],
					Style[
						If[type === "CDFReinterpret",
							Column[{
								FrontEndResource["WAStrings", "SuggestionsTitleReinterpret"],
								FrontEndResource["WAStrings", "SuggestionsBody"]
								}
							],
							FrontEndResource["WAStrings", "SuggestionsBody"]
						],
						FontColor -> Gray
					]},
					Dividers -> Center,
					FrameStyle -> LightGray,
					ItemSize -> Scaled[0.998],
					Spacings -> 3
				]
			},
			Dynamic[tip],
			ImageSize -> Automatic
		]
	]


suggestionsDialogButton[type_, args___] := 
	Tooltip[
		Button[
			Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "QuestionMarkSmallIcon"]]],
			Quiet[WolframAlpha[]]; (* trigger autoloading *)
			openSuggestionsDialog[type, args],
			Appearance -> None,
			BaseStyle -> {ShowStringCharacters -> False}
		],
		cachedFrontEndResource["WAStrings", "SuggestionsTooltip"]
	]


openSuggestionsDialog[type_, query_, args___] := 
MessageDialog[
	Grid[{
		{"  ",""},
		{"", TextCell[
			Switch[type,
				"Reinterpret", cachedFrontEndResource["WAStrings", "SuggestionsTitleReinterpret"],
				"NoTranslations", cachedFrontEndResource["WAStrings", "SuggestionsTitleNoTranslations"],
				"NoResults" | _, cachedFrontEndResource["WAStrings", "SuggestionsTitleNoResults"]
			], "Text", Bold]
		},
		{"", ""},
		Switch[type,
			"NoTranslations",
			Sequence @@ {
				{"", TextCell[cachedFrontEndResource["WAStrings", "SuggestionsBodyNoTranslations"], "Text"]},
				{"", Button[
					Hyperlink[cachedFrontEndResource["WAStrings", "SuggestionsBodyTryIt"]], 
					Block[{nb},
						nb = CreateDocument[TextCell[query, "WolframAlphaLong", FormatType -> "TextForm"]];
						FrontEndTokenExecute[nb, "EvaluateNotebook"]
					],
					ImageSize -> Automatic,
					Appearance -> None,
					BaseStyle -> {},
					DefaultBaseStyle -> {}
				]}
			},
			_,
			{"", TextCell[cachedFrontEndResource["WAStrings", "SuggestionsBody"], "Text"]}
		]
		},
		Alignment -> Left
	],
	{
		cachedFrontEndResource["WAStrings", "SuggestionsFeedbackButton"] :> sendFeedback[query, args]
	},
	WindowSize -> {500, FitAll},
	WindowTitle -> cachedFrontEndResource["WAStrings", "SuggestionsWindowTitle"]
]


sendFeedback[args___] :=
	NotebookLocate[{URL[#], None}]&[
		StringJoin[
			"mailto:feedback@wolframalpha.com?",
			(* ExternalService`EncodeString[] works better in testing mail clients than the Java-based urlencode *)
			"subject=", ExternalService`EncodeString["Feedback for Wolfram|Alpha in Mathematica"],
			"&body=", ExternalService`EncodeString[ToString[Unevaluated[WolframAlpha[args]],InputForm]]
		]
	]


formatFallThrough["error", values_, data_, query_, opts___] := (
	Message[WolframAlpha::interr,
		Row[Riffle[Cases[data, error:XMLElement["error", _, _] :> formatErrorString[error]], ";"]]
	];
	$Failed
)


formatErrorString[error:XMLElement["error", _, {___, XMLElement["code", _, {code_}], ___}]] :=
	Row[{errorBlobMessage[code, error], " (", code, ")"}]


formatErrorString[XMLElement["error", _, {___, XMLElement["msg", _, {msg_}], ___}]] := msg


formatErrorString[XMLElement["error", _, other_]] := "unknown error"



formatFallThroughElement[XMLElement["warnings", _, data_], q_, opts___] := formatFallThroughElement[#, q, opts]& /@ data


formatFallThroughElement[XMLElement["relatedexamples", _, data_], q_, opts___] := formatFallThroughElement[#, q, opts]& /@ data


formatFallThroughElement[XMLElement["didyoumeans", _, data_], q_, opts___] := formatFallThroughElement[#, q, opts]& /@ data


(* "spellcheck" elements are handled as a warning, not as a fall-through *)
(* formatFallThroughElement[XMLElement["spellcheck", {___, "text"-> text_, ___}, _], q_, opts___] := text *)


formatFallThroughElement[XMLElement["relatedexample", values_, data_], q_, opts___] :=
	Row[Flatten[{
		"\[Bullet] Related example: ",
		replaceQueryResultsButton["input" /. values, "input" /. values, opts],
		If[("desc" /. values) === "", {}, {" (", "desc" /. values, ")"}]
	}]]


formatFallThroughElement[XMLElement["didyoumean", values_, {suggestion_}], q_, opts___] :=
	Row[{"Did you mean ", replaceQueryResultsButton[suggestion, suggestion, opts], "?"}]


formatFallThroughElement[XMLElement["futuretopic", values_, data_], q_, opts___] :=
	Row[{"\[Bullet] ", "topic" /. values, ":  ", "msg" /. values}]


formatFallThroughElement[XMLElement["examplepage", values_, data_], q_, opts___] :=
Block[{url, xml},
	url = "url" /. values;
	url = StringReplace[url, "-content.html" -> ".xml"];
	xml = qImport[url, "XML"];
	formatExample[Flatten[Cases[xml, XMLElement["example", _, a_] :> a, Infinity]], q, opts]
]

formatExample[list_List, q_, opts___] := If[# === {}, {}, Column[Last /@ #, Spacings -> {Automatic, First /@ #}]]&[ formatExampleElement[#, q, opts]& /@ list ]

formatExampleElement[XMLElement["category", _, {category_}], q_, opts___] := {2, Style[category, "Subsection", FontColor -> Red]}

formatExampleElement[XMLElement["section-title", {}, {XMLElement["link", {"ref" -> ref_}, {title_String}]}], q_, opts___] :=
	{2, replaceQueryResultsButton[Row[{Style[title, Bold, FontColor -> Black], " \[RightGuillemet]"}], title, opts]}

formatExampleElement[XMLElement["section-title", {}, {title_String}], q_, opts___] := {2, Style[title, Bold, FontColor -> Black]}

formatExampleElement[XMLElement["caption", {}, {caption_}], q_, opts___] := {1.2, caption}

formatExampleElement[XMLElement["input", {}, {input_}], q_, opts___] := {0.4, replaceQueryResultsButton[input <> " \[RightGuillemet]", input, opts]}

formatExampleElement[other_, q_, opts___] := Sequence[]


formatFallThroughElement[XMLElement["languagemsg", values:{___, "english" -> english_, ___}, _], q_, opts___] :=
	If[MatchQ[values, {___, "other" -> _String, ___}],
		Grid[{{"\[Bullet] ", Style["other" /. values, FontSize -> Larger]}, {"", english}}, Alignment -> Left],
		Row[{"\[Bullet] ", english}]
	]


formatFallThroughElement[other_, q_, opts___] := {}



replaceQueryResultsButton[label_, query_, opts___] := 
Button[
	Style[Mouseover[Style[label, FontColor -> Orange], Style[label, FontColor -> Red]], "DialogStyle"],
	Quiet[WolframAlpha[]]; (* trigger autoloading *)
	If[MemberQ[AppearanceElements /. Flatten[{opts}] /. AppearanceElements -> $AutomaticAlphaQueryAppearanceElements, "CDFWarnings"],
		NotebookLocate[{URL[WolframAlpha[query, "CDFURL", opts]], None}, "OpenInNewWindow" -> CurrentValue["HyperlinkModifierKey"]],
		replaceQueryResults[ButtonNotebook[], query, opts]
	],
	Appearance -> None,
	Method -> "Queued"
]


replaceQueryResults[nb_, query_, opts___] :=
	MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[
		FE`BoxReference[MathLink`CallFrontEnd[FrontEnd`Value[FEPrivate`Self[]]], {FE`Parent["WolframAlphaQueryResults"]}],
		formatAlphaXML[WolframAlpha[query, "ProcessedXML", opts], query, opts] // ToBoxes,
		AutoScroll -> False
	]];


(* ::Subsection::Closed:: *)
(*success -> false*)


FormatAllUnsuccesses[nonpods_List, Dynamic[newq_], Dynamic[q_], Dynamic[opts_]] := 
	Block[{list},
		list = Flatten[formatFallThroughElement[#, q, Sequence @@ Flatten[{opts}]]& /@ nonpods];
		{innerFrame[
			Column[
				If[list === {},
					If[MemberQ[AppearanceElements /. Flatten[{opts}] /. AppearanceElements -> $AutomaticAlphaQueryAppearanceElements, "CDFWarnings"],
						{suggestionsButtonOpener[
							Row[{
								Style[Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoResults"]], Italic],
								"  ",
								suggestionsDialogButtonPlaceholder[]
							}],
							"CDFNoResults"
						]},
						{Row[{
							Style[Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoResults"]], Italic],
							"  ",
							suggestionsDialogButton["NoResults", q]
						}]}
					],
					list
				],
				Dividers -> Center,
				FrameStyle -> LightGray,
				BaseStyle -> {"DialogStyle", FontColor -> Gray},
				Spacings -> 2,
				ItemSize -> Scaled[0.998]
			],
			FrameMargins -> 15,
			Background -> queryBlobBackground[Dynamic[q], Dynamic[newq], GrayLevel[0.965]]
		]}
	]


(* ::Subsection::Closed:: *)
(*warnings*)


FormatAllWarnings[ type_, data_, args___] := formatWarnings[#, type, args]& /@ data

formatWarnings[XMLElement["warnings", values_, data_], type_, args___] := formatWarning[#, type, args]& /@ data

formatWarnings[other_, ___] := {}

formatWarning[XMLElement["spellcheck", values:{___, "text" -> text_, ___}, data_], type_, Dynamic[newq_], Dynamic[q_], opts___] := 
	warningFrame[type, text, Dynamic[newq], Dynamic[q]]

formatWarning[XMLElement["delimiters", values:{___, "text" -> text_, ___}, data_], type_, Dynamic[newq_], Dynamic[q_], opts___] :=
	warningFrame[type, text, Dynamic[newq], Dynamic[q]]

formatWarning[XMLElement["translation", values:{___, "text" -> text_, ___}, data_], type_, Dynamic[newq_], Dynamic[q_], opts___] := 
	warningFrame[type, text, Dynamic[newq], Dynamic[q]]

formatWarning[XMLElement["reinterpret", values:{___, "new" -> new_, ___}, data_], type_, Dynamic[newq_], Dynamic[q_], opts___] := 
	Block[{alternatives, rows},
		rows = {If[type === "CDFWarnings",
			suggestionsButtonOpener[
				Grid[{{
						Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "ReinterpretationIndicator"]]],
						Row[{Style["text" /. values, FontColor -> Gray], " ", Style[new, Bold]}],
						suggestionsDialogButtonPlaceholder[]
					}},
					Alignment -> Left,
					Spacings -> 0.5
				],
				"CDFReinterpret"
			],
			Grid[{{
					Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "ReinterpretationIndicator"]]],
					Row[{Style["text" /. values, FontColor -> Gray], " ", Style[new, Bold]}],
					suggestionsDialogButton["Reinterpret", q]
				}},
				Alignment -> Left,
				Spacings -> 0.5
			]
		]};

		alternatives = Cases[data, XMLElement["alternative", _, {alternative_String}] :> alternative];
		alternatives = replaceQueryResultsButton[#, #, opts]& /@ alternatives;
		If[alternatives =!= {},
			AppendTo[rows, Row[Flatten[{Style["More interpretations:", FontColor -> Gray], " ", Riffle[alternatives, $verticalBar1]}]]]
		];
		
		warningFrame[
			type,
			Grid[List /@ rows,
				Alignment -> Left,
				Dividers -> Center,
				FrameStyle -> LightGray,
				ItemSize -> Scaled[0.998],
				Spacings -> {0,2}
			],
			Dynamic[newq],
			Dynamic[q]
		]
	]


formatWarning[unknownWarning_, args___] := {}


warningFrame["Warnings", expr_, Dynamic[newq_], Dynamic[q_]] :=
	innerFrame[expr,
		FrameStyle -> Orange,
		FrameMargins -> 15,
		Background -> queryBlobBackground[Dynamic[q], Dynamic[newq], GrayLevel[0.965]]
	]

warningFrame["CDFWarnings", expr_, Dynamic[newq_], Dynamic[q_]] :=
	innerFrame[expr,
		FrameStyle -> RGBColor[0.98, 0.52, 0.33],
		FrontEnd`BoxFrame -> 2,
		FrameMargins -> 15,
		Background -> queryBlobBackground[Dynamic[q], Dynamic[newq], GrayLevel[0.965]],
		RoundingRadius -> 8
	]



(* ::Subsection::Closed:: *)
(*assumptions*)


$FormulaAssumptionTypes = {
	"FormulaSelect",
	"FormulaSolve",
	"FormulaVariable",
	"FormulaVariableOption",
	"FormulaVariableInclude"
};


$NonFormulaAssumptionTypes = {
	"Clash",
	"Unit",
	"AngleUnit",
	"Function",
	"MultiClash",
	"SubCategory",
	"Attribute",
	"TimeAMOrPM",
	"DateOrder",
	"ListOrTimes",
	"ListOrNumber",
	"CoordinateSystem",
	"I",
	"NumberBase",
	"MixedFraction",
	"MortalityYearDOB",
	"TideStation"
};


$AssumptionTypes = Sort[Join[$FormulaAssumptionTypes, $NonFormulaAssumptionTypes]];


FormatAllAssumptions[ type_, data_, Dynamic[newq_], Dynamic[q_], Dynamic[opts_]] := 
Block[{assumptions, nonformula, formula},
	assumptions = Cases[data, XMLElement["assumptions", values_, list_] :> list] // Flatten;
	formula = Cases[assumptions,
		XMLElement["assumption", {___, "type" -> (Alternatives @@ $FormulaAssumptionTypes), ___}, _] ];
	nonformula = DeleteCases[assumptions, Alternatives @@ formula];
	
	{
		If[nonformula === {}, {}, formatNonFormulaAssumptions[nonformula, type, Dynamic[newq], Dynamic[q], Dynamic[opts]]],
		If[formula === {}, {}, formatFormulaAssumptions[formula, type, Dynamic[newq], Dynamic[q], Dynamic[opts]]]
	}
]


formatAssumption[e:XMLElement["assumption", values:{___, "type" -> type_, ___}, data_], Dynamic[q_], Dynamic[opts_]] :=
	formatAssumption[type, values, data, Dynamic[q], Dynamic[opts]]


(* ::Subsubsection::Closed:: *)
(*Assumption utilities*)


$verticalBar1 = Style[" | ", "DialogStyle", FontSize -> Larger, FontColor -> GrayLevel[0.7]]

$verticalBar2 = Style["  |  ", "DialogStyle", FontSize -> Larger, FontColor -> GrayLevel[0.7]]

$verticalBar3 = Style[" ", "DialogStyle"]

assumptionDesc[XMLElement["value", {___, "desc" -> desc_, ___}, _]] := desc


assumptionInput[XMLElement["value", {___, "input" -> input_, ___}, _]] := input


assumptionWord[XMLElement["value", {___, "word" -> word_, ___}, _]] := word


assumptionControls[list:{__}, Dynamic[q_], Dynamic[opts_]] := 
Block[{controls},
	controls = If[
		Length[list] <= 3,
		assumptionButton[#, Dynamic[q], Dynamic[opts]]& /@ list,
		Flatten[{
			assumptionButton[#, Dynamic[q], Dynamic[opts]]& /@ Take[list, 2],
			assumptionMenu["more", Drop[list, 2], Dynamic[q], Dynamic[opts]]
		}]
	];
	Riffle[controls, " or "]
]


assumptionButton[value_, Dynamic[q_], Dynamic[opts_]] := 
Button[
	Style[Mouseover[Style[assumptionDesc[value], FontColor -> Orange], Style[assumptionDesc[value], FontColor -> Red]], "DialogStyle"],
	Quiet[WolframAlpha[]]; (* trigger autoloading *)
	updateWithAssumptions[ButtonNotebook[], {assumptionInput[value]}, Dynamic[q], Dynamic[opts]],
	Appearance -> None,
	Method -> "Queued"]


assumptionMenu[label_, values_, Dynamic[q_], Dynamic[opts_]] :=
ActionMenu[
	lightFrameActionMenuBase[label],
	Map[(assumptionDesc[#] :> (
			Quiet[WolframAlpha[]]; (* trigger autoloading *)
			updateWithAssumptions[ButtonNotebook[], {assumptionInput[#]}, Dynamic[q], Dynamic[opts]]))&,
		values
	],
	Appearance -> None,
	BaselinePosition -> Baseline,
	Method -> "Queued"
]


updateWithAssumptions[nb_, assumptioninputs:{__}, Dynamic[q_], Dynamic[opts_]] := 
Block[{newopts, oldassumptions, newassumptions},
	oldassumptions = Cases[Flatten[{InputAssumptions /. Flatten[{opts}] /. InputAssumptions -> {}}], _String];
	newassumptions = removeDuplicateAssumptions[oldassumptions, assumptioninputs];
	newopts = Sequence @@ Flatten[{DeleteCases[Flatten[{opts}], _[InputAssumptions, _]], InputAssumptions -> newassumptions}];
	
	If[MemberQ[AppearanceElements /. Flatten[{opts}] /. AppearanceElements -> $AutomaticAlphaQueryAppearanceElements, "CDFAssumptions"],
		NotebookLocate[{URL[WolframAlpha[q, "CDFURL", newopts]], None}, "OpenInNewWindow" -> CurrentValue["HyperlinkModifierKey"]],
		replaceQueryResults[nb, q, newopts]
	]
]


removeDuplicateAssumptions[old_List, new_List] :=
	Last /@ Fold[removeDuplicateAssumption, {assumptionToPrefix[#], #}& /@ old, {assumptionToPrefix[#], #}& /@ new]

removeDuplicateAssumption[old_List, new:{newprefix_, _}] := Append[DeleteCases[old, {newprefix, _}], new]




assumptionFrame["Assumptions", expr_, Dynamic[newq_], Dynamic[q_], opts___] := 
	innerFrame[expr,
		opts,
		Background -> queryBlobBackground[Dynamic[q], Dynamic[newq], GrayLevel[0.965]],
		FrameMargins -> 15
	]

assumptionFrame["CDFAssumptions", expr_, Dynamic[newq_], Dynamic[q_], opts___] := 
	innerFrame[expr,
		opts,
		Background -> queryBlobBackground[Dynamic[q], Dynamic[newq], GrayLevel[0.965]],
		FrameMargins -> 15,
		RoundingRadius -> 8
	]




(* ::Subsubsection::Closed:: *)
(*Formula assumptions*)


formatFormulaAssumptions[list_, type_, Dynamic[newq_], Dynamic[q_], Dynamic[opts_]] :=
Block[{fselects, fsolves, fvariables, fvariableoptions, fvariableincludes},

	fselects          = Cases[list, XMLElement["assumption", {___, "type" -> "FormulaSelect", ___}, _]];
	fsolves           = Cases[list, XMLElement["assumption", {___, "type" -> "FormulaSolve", ___}, _]];
	fvariables        = Cases[list, XMLElement["assumption", {___, "type" -> "FormulaVariable", ___}, _]];
	fvariableoptions  = Cases[list, XMLElement["assumption", {___, "type" -> "FormulaVariableOption", ___}, _]];
	fvariableincludes = Cases[list, XMLElement["assumption", {___, "type" -> "FormulaVariableInclude", ___}, _]];

	assumptionFrame[
		type,
		Column[
			Join[
				fselects          = Flatten[{formatAssumption["FormulaSelect", #2, #3, Dynamic[q], Dynamic[opts]]& @@@ fselects}],
				fsolves           = Flatten[{formatAssumption["FormulaSolve", #2, #3, Dynamic[q], Dynamic[opts]]& @@@ fsolves}],
				fvariables        = Flatten[{formatVariableGrid[fvariables, Dynamic[q], Dynamic[opts]]}],
				fvariableoptions  = Flatten[{formatAssumption["FormulaVariableOption", #2, #3, Dynamic[q], Dynamic[opts]]& @@@ fvariableoptions}],
				fvariableincludes = Flatten[{formatAssumption["FormulaVariableInclude", #2, #3, Dynamic[q], Dynamic[opts]]& @@@ fvariableincludes}]
			],
			Spacings -> {1, 1},
			FrameStyle -> LightGray,
			Dividers -> {
				None,
				If[
					Length[fselects] + Length[fsolves] + Length[fvariables] > 0 && 
					Length[fvariableoptions] + Length[fvariableincludes] > 0,
					(* Draw a divider between the top three categories and the bottom two categories, if both exist *)
					(Length[fselects] + Length[fsolves] + Length[fvariables] + 1) -> True,
					(* Otherwise, we don't need a divider *)
					None
				]
			}
		],
		Dynamic[newq],
		Dynamic[q],
		FrameStyle -> Orange
	] // Deploy
]


formatVariableGrid[{}, Dynamic[q_], Dynamic[opts_]] := {}


formatVariableGrid[fvariables_, Dynamic[q_], Dynamic[opts_]] :=
DynamicModule[{labels, inputs, prefixes, returnkey},

	{labels, inputs, prefixes} = Transpose[getFormulaVariableInfo /@ fvariables];
	
	Grid[{{
		Grid[
			Table[With[{i=i},
				{
					Dynamic[labels[[i]]],
					formulaControl[Dynamic[inputs[[i]]], fvariables[[i]], Dynamic[returnkey]]
				}],
				{i, 1, Length[fvariables]}
			],
			Alignment -> {Left, Baseline}
		],
		(* If there's more than one variable, draw a bracket *)
		If[Length[fvariables] > 1,
			Item["", Frame -> {{False, True}, {True, True}}, FrameStyle -> Orange],
			Unevaluated[Sequence[]]
		],
		Tooltip[
			Button[
				Mouseover[
					Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "Equal"]]],
					Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "EqualHot"]]]
				],
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				setFormulaVariables[prefixes, inputs, Dynamic[q], Dynamic[opts]],
				Appearance -> None,
				FrameMargins -> 0,
				Method -> "Queued"
			],
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "ApplyAssumptions"]]
		],
		PaneSelector[{
			False -> "",
			True ->
				DynamicModule[{},
					"",
					SynchronousInitialization -> False,
					Initialization :> (
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						setFormulaVariables[prefixes, inputs, Dynamic[q], Dynamic[opts]]
					)
				]
			},
			Dynamic[returnkey],
			ImageSize -> {1,1}
		]
		}},
		Alignment -> Center
	],
	UnsavedVariables :> {returnkey}
]





formulaControl[Dynamic[var_], XMLElement["assumption", values_, data:{_}], Dynamic[returnkey_]] :=
	EventHandler[InputField[Dynamic[var], String], {"ReturnKeyDown" :> (returnkey = True)}, PassEventsDown -> True]


formulaControl[Dynamic[var_], XMLElement["assumption", values_, data:{_, __}], Dynamic[returnkey_]] :=
	With[{rules = (First[assumptionToInputAndPrefix[#]] -> assumptionDesc[#])& /@ data},
		PopupMenu[
			Dynamic[var],
			rules,
			Dynamic[var /. rules],
			lightFrameActionMenuBase[Dynamic[var /. rules]],

			Appearance -> None,
			BaselinePosition -> Baseline,
			BaseStyle -> "DialogStyle",
			FrameMargins -> 0
		]
	]


setFormulaVariables[prefixes_, inputs_, Dynamic[q_], Dynamic[opts_]] := 
	updateWithAssumptions[
		ButtonNotebook[], 
		MapThread[assumptionFromInputAndPrefix, {inputs, prefixes}],
		Dynamic[q], Dynamic[opts]]


(* return {label, currentvalue, prefix} *)
getFormulaVariableInfo[XMLElement["assumption", values_, data_]] :=
	Flatten[{
		Row[{Style["\[Bullet] ", Orange], "desc"/. values, ":"}],
		Block[{n},
			n = "current" /. values;
			If[DigitQ[n], n = ToExpression[n], n = 1];
			assumptionToInputAndPrefix[data[[n]]]
		]
	}]


assumptionToInputAndPrefix[input_String] := 
Block[{result},
	If[
		(* if there is an instance of "-_", split at the last one *)
		result = StringSplit[input, "-_"];
		Length[result] > 1,
		result = {StringJoin[Riffle[Most[result], "-_"]] <> "-", Last[result]},
		(* otherwise, split at the last "_" *)
		result = StringSplit[input, "_"];
		result = {StringJoin[Riffle[Most[result], "_"]], Last[result]}
	];
	{urldecode[Last[result]], First[result]}
]

assumptionToInputAndPrefix[XMLElement["value", {___, "input" -> input_String, ___}, _]] :=
	assumptionToInputAndPrefix[input]


assumptionToPrefix[input_String] := Last[assumptionToInputAndPrefix[input]]


assumptionFromInputAndPrefix[input_, prefix_] := prefix <> "_" <> urlencode[input]


formatAssumption["FormulaSelect", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["FormulaSolve", values_, data_, Dynamic[q_], Dynamic[opts_]] :=
	Row[{"Calculate ", assumptionMenu[assumptionDesc[First[data]], Rest[data], Dynamic[q], Dynamic[opts]]}]


formatAssumption["FormulaVariableOption", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["FormulaVariableInclude", values_, data_, Dynamic[q_], Dynamic[opts_]] :=
	Row[{"Also include: ", Row[Map[assumptionButton[#, Dynamic[q], Dynamic[opts]]&, data], $verticalBar1]}]


(* ::Subsubsection::Closed:: *)
(*Non-formula assumptions*)


formatNonFormulaAssumptions[list_, type_, Dynamic[newq_], Dynamic[q_], Dynamic[opts_]] := 
	Block[{formattedAssumptions},
		formattedAssumptions = Flatten[formatAssumption[#, Dynamic[q], Dynamic[opts]]& /@ list];
		If[formattedAssumptions === {}, Return[{}]];
		assumptionFrame[
			type,
			Column[formattedAssumptions, ItemSize -> Scaled[0.998]],
			Dynamic[newq],
			Dynamic[q]
		]
	]
	

formatAssumption["Clash", {___, "word" -> word_, ___}, data:{_,__}, Dynamic[q_], Dynamic[opts_]] := 
	Row[Flatten[{"Assuming \[OpenCurlyDoubleQuote]", word, "\[CloseCurlyDoubleQuote] is ", assumptionDesc[First[data]], $verticalBar2, "Use as ", 
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["Unit", {___, "word" -> word_, ___}, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", assumptionDesc[First[data]], " for \[OpenCurlyDoubleQuote]", word, "\[CloseCurlyDoubleQuote]", $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["AngleUnit", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming trigonometric arguments are in ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["Function", {___, "word" -> word_, ___}, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming \[OpenCurlyDoubleQuote]", word, "\[CloseCurlyDoubleQuote] is ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["MultiClash", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Block[{quoted, word, result, valuelist, otherwords},
		quoted["the input"] := "the input";
		quoted[str_] := {"\[OpenCurlyDoubleQuote]", str, "\[CloseCurlyDoubleQuote]"};
	
		word = assumptionWord[First[data]];
		result = {"Assuming ", quoted[word], " is ", assumptionDesc[First[data]]};
		
		(* If there are clashes for this same word, display them next *)
		valuelist = Select[Rest[data], assumptionWord[#] === word &];
		If[valuelist =!= {},
			result = Flatten[{result, $verticalBar2, "Use as ", assumptionControls[valuelist, Dynamic[q], Dynamic[opts]], " instead"}]
		];

		(* If there are clashes for other words, display them last *)
		otherwords = DeleteCases[DeleteDuplicates[assumptionWord /@ data], word];
		valuelist = Select[Rest[data], Function[x, assumptionWord[x] === #]]& /@ otherwords;
		result = Flatten[{result,
			MapThread[
				If[#2 === {}, {}, {$verticalBar2, "Use ", quoted[#1], " as ", assumptionControls[#2, Dynamic[q], Dynamic[opts]], " instead"}]&,
				{otherwords, valuelist}
			]
		}];
		
		Row[result]
	]


formatAssumption["SubCategory", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["Attribute", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["TimeAMOrPM", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["DateOrder", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["ListOrTimes", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["ListOrNumber", {___, "word" -> word_, ___}, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", word, " is a ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["CoordinateSystem", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Using ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["I", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["NumberBase", {___, "word" -> word_, ___}, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", word, " is ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["MixedFraction", {___, "word" -> word_, ___}, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", word, " is a ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["MortalityYearDOB", {___, "word" -> word_, ___}, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Assuming ", word, " is ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption["TideStation", values_, data:{_,__}, Dynamic[q_], Dynamic[opts_]] :=
	Row[Flatten[{"Using ", assumptionDesc[First[data]], $verticalBar2, "Use ",
		assumptionControls[Rest[data], Dynamic[q], Dynamic[opts]], " instead"}]]


formatAssumption[unknownType_, values_, data_, Dynamic[q_], Dynamic[opts_]] := (
	(* Message[WolframAlpha::invasmp, unknownType, values, data]; *)
	{}
)


(* ::Subsection::Closed:: *)
(*pods*)


(* The DynamicModule created by FormatIndependentPod is based closely on the one built by buildDynamicModule[] *)
FormatIndependentPod[pod:XMLElement["pod", _, _], query_, opts___] :=
	DynamicModule[{
			Typeset`q = query, Typeset`opts = {opts}, Typeset`elements,
			Typeset`sessioninfo, Typeset`chosen, Typeset`open,
			Typeset`newq, Typeset`pod, Typeset`aux},
				
		Typeset`pod = pod;
		Typeset`aux = {True, False, {False}, True};

		Typeset`sessioninfo = {"TimeZone" -> $TimeZone, "Date" -> DateList[], "Line" -> $Line, "SessionID" -> $SessionID};

		{Typeset`open, Typeset`chosen, Typeset`newq} = {"ExtrusionOpen", "ExtrusionChosen", "NewQuery"} /. allMethodOptions[opts];
		If[ !ListQ[Typeset`chosen], Typeset`chosen = {}];
		If[ !StringQ[Typeset`newq], Typeset`newq = query];

		Typeset`elements = OptionValue[WolframAlpha, {opts}, AppearanceElements];
		Switch[Typeset`elements,
			None, Typeset`elements = {},
			{__String}, Null,
			All, Typeset`elements = $AllAlphaQueryAppearanceElements,
			Automatic | _, Typeset`elements = $AutomaticAlphaQueryAppearanceElements
		];

		Dynamic[
			FormatPod[Typeset`pod, Dynamic[Typeset`pod], Dynamic[Typeset`aux], Dynamic[Typeset`chosen], Dynamic[Typeset`open], Dynamic[Typeset`newq], Dynamic[Typeset`q], Dynamic[Typeset`opts], Typeset`elements, Typeset`sessioninfo],
			TrackedSymbols :> {Typeset`pod}
		],

		BaseStyle -> {"DialogStyle", AutoItalicWords -> {"Mathematica"}},
		Initialization :> Quiet[WolframAlpha[]] (* trigger autoloading *)
	]


FormatAllPods[Dynamic[{podvarseq___}], Dynamic[{auxvarseq___}], Dynamic[chosen_], Dynamic[open_], Dynamic[showpods_], Dynamic[failedpods_], Dynamic[newq_], Dynamic[q_], Dynamic[opts_], elements_, info_] := 
	Extract[#, List /@ Complement[showpods, failedpods]]& @ 
	MapThread[
		Function[{var, aux},
			Dynamic[
				FormatPod[var, Dynamic[var], Dynamic[aux], Dynamic[chosen], Dynamic[open], Dynamic[newq], Dynamic[q], Dynamic[opts], elements, info],
				TrackedSymbols :> {var}
			],
			HoldAllComplete
		],
		Unevaluated[{{podvarseq}, {auxvarseq}}]
	]


(* async failed *)
FormatPod[e:XMLElement["pod", values:{___, "asyncfailed" -> msg_}, data_], Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[newq_], Dynamic[q_], Dynamic[opts_], elements_, info_] :=
	innerFrame[
		Grid[{{
			Item[podTitle[values, ""], Alignment -> Left],
			Item[Style[msg, Italic, FontColor -> LightGray], Alignment -> Right]
			}},
			Alignment -> Top,
			ItemSize -> Scaled[0.499]
		]
	]


(* async content is available *)
FormatPod[e:XMLElement["pod", values:{___, "async" -> url_, ___}, data_], Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[newq_], Dynamic[q_], Dynamic[opts_], elements_, info_] :=
	asyncPodAnimator[]


(* recalculate content is available *)
FormatPod[e:XMLElement["pod", values:{___, "recalculate" -> "true", ___}, data:{"recalculate"}], Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[newq_], Dynamic[q_], Dynamic[opts_], elements_, info_] :=
	asyncPodAnimator[]


(* main case: no async content *)
FormatPod[XMLElement["pod", values_, data_], Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[newq_], Dynamic[q_], Dynamic[opts_], elements_, info_] :=
Block[{numberOfSubpods, subpods, podinfos, poddefinitions, podnotes, $subpodCounter = 0},
	numberOfSubpods = Count[data, XMLElement["subpod", _, _]];
	subpods = formatSubpods["id" /. values, "title" /. values, data, Dynamic[var], Dynamic[aux], Dynamic[chosen], Dynamic[open], Dynamic[q], Dynamic[opts], Hold[$subpodCounter], numberOfSubpods, info, AppearanceElements /. opts /. Options[WolframAlpha]];
	podinfos = Item[#, Alignment -> Right]& /@ podInfos[data];
	poddefinitions = Item[#, Alignment -> Right]& /@ podDefinitions[data];
	podnotes = Item[#, Alignment -> Right]& /@ podNotes[data];
	(* If there are both "Definitions" and "Notes" elements, combine them into a single "Definitions and notes" element *)	
	If[poddefinitions =!= {} && podnotes =!= {},
		poddefinitions = Item[podDefinitionsAndNotes[data], Alignment -> Right];
		podnotes = {}
	];

	If[subpods === {}, Return[""]];
	
	dogEarPanel[
		innerFrame[
			podOpener[{
				Column[Flatten[{
					Grid[{{
						Item[
							Pane[
								podOpenerButton[podTitle[values, ":"], Dynamic[aux[[1]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "HidePod"]], elements],
								ImageMargins -> {{0,0},{0,1}}
							],
							Alignment -> Left
						],
						Item[
							PaneSelector[{
								True -> necklaceAnimator[$necklaceAnimatorSize],
								False -> podStates["id" /. values, data, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts]]
								},
								Dynamic[TrueQ[aux[[2]]]],
								ImageSize -> All,
								Alignment -> {Right, Top}
							],
							Alignment -> Right
						]
						}},
						Alignment -> Top,
						(* If there are no pod state or sound buttons, give more space to the title *)
						ItemSize -> If[MemberQ[data, XMLElement["states"|"sounds", _, _]], Scaled[0.499], {{Scaled[0.9], Scaled[0.098]}, Automatic}]
					],
					subpods,
					podinfos,
					poddefinitions,
					podnotes
					}],
					ItemSize -> Scaled[0.998],
					(*
					If there's exactly one subpod, don't add delimiters, even if there are infos.
					If there's more than one subpod, add a delimiter between each one, and before the infos if any.
					*)
					Dividers -> (
						subpods = Length[Flatten[subpods]];
						podinfos = Length[Flatten[{podinfos, poddefinitions, podnotes}]];
						If[subpods === 1,
							None,
							{None, Table[i -> True, {i, 3, 3 + subpods -2 + Sign[podinfos]}]}
						]
					),
					Spacings -> If[MemberQ[elements, "ContentPadding"],
						{Automatic, Flatten[{Automatic, .9, If[subpods === 1, {}, Table[1.1, {subpods}]]}]},
						Automatic
					],
					FrameStyle -> GrayLevel[0.95]
				],
				podTitle[values, " \[RightGuillemet]"]
				},
				Dynamic[aux[[1]]],
				elements
			],
			Background -> queryBlobBackground[Dynamic[q], Dynamic[newq], GrayLevel[0.965]],
			FrameMargins -> If[MemberQ[elements, "ContentPadding"], {{13,10},{6, 6}}, Automatic]
		],
		"id" /. values,
		data,
		Dynamic[var],
		Dynamic[aux],
		Dynamic[q],
		Dynamic[opts],
		elements
	]
]


podOpener[{pod_, title_}, Dynamic[open_], elements_] := 
	If[MemberQ[elements, "PodCornerCurl"],
		pod,
		PaneSelector[{
				True -> pod,
				False -> Column[{podOpenerButton[title, Dynamic[open], Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowPod"]], elements]}, ItemSize -> Scaled[0.998]]
			},
			Dynamic[open],
			ImageSize -> Automatic
		]
	]


podOpenerButton[label_, Dynamic[open_], tip_, elements_] :=
	If[MemberQ[elements, "PodCornerCurl"],
		label,
		Tooltip[
			Button[
				label,
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				open = Not @ open,
				Appearance -> None,
				BaseStyle -> {},
				DefaultBaseStyle -> {},
				If[$VersionNumber > 7, ContentPadding -> False, FrameMargins -> 0]
			],
			tip
		]
	]


asyncPodAnimator[] := 
DynamicModule[{linecolor, darkfill, lightfill, roundedcorners, v = 0},
	linecolor = RGBColor[0.8161745632104982, 0.8162966353856718, 0.8161135271229114];
	darkfill = RGBColor[0.873182269016556, 0.8733043411917296, 0.8731212329289693];
	lightfill = RGBColor[0.9752040894178683, 0.9753566796368353, 0.9751277943083848]; 
	roundedcorners = {{0., 9.}, {2., 8.464101615137753}, {3.4641016151377544, 7.}, {4., 5.},
		{3.4641016151377544, 3.}, {2., 1.5358983848622456}, {0., 1.}, {-2., 1.5358983848622456},
		{-3.4641016151377544, 3.}, {-4., 5.}, {-3.4641016151377544, 7.}, {-2., 8.464101615137753},
		{0., 9.}
	};

	Grid[{{
		
		Animator[Dynamic[v], {0, 1}, AppearanceElements -> {}, ImageSize -> {1, 1}, DefaultDuration -> 15],
		
		Graphics[{darkfill, EdgeForm[linecolor], Polygon[roundedcorners]},
			PlotRange -> {{-4.5, 1}, {-1, 9}},
			ImageSize -> {Automatic, 9},
			ImagePadding -> 0,
			PlotRangePadding -> 0
		],
  
		Graphics[{
			EdgeForm[],
			darkfill, Rectangle[{0, 0}, {Dynamic@v, 9}],
			lightfill, Rectangle[{Dynamic@v, 0}, {1, 9}],
			linecolor, Line[{{0, 1}, {1, 1}}], Line[{{0, 9}, {1, 9}}]
			},
			ImageSize -> {Full, 9},
			AspectRatio -> Full,
			ImagePadding -> 0,
			PlotRangePadding -> 0
		],
  
		Graphics[{lightfill, EdgeForm[linecolor], Polygon[roundedcorners]},
			PlotRange -> {{0, 4.5}, {-1, 9}},
			ImageSize -> {Automatic, 9},
			ImagePadding -> 0,
			PlotRangePadding -> 0
		]

		}},
		Spacings -> 0
	]
]


$necklaceAnimatorSize = 13;

necklaceAnimator[size_] := Animator[Appearance -> "Necklace", ImageSize -> size]





dogEarClosed[] := Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "DogEarClosed"]]]


dogEarOpen[podid_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_]] :=
	With[{linksQ = TrueQ @ aux[[4]],
			label = If[TrueQ @ aux[[4]],
						Dynamic[FEPrivate`FrontEndResource["WAStrings", "HideLinks"]],
						Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowLinks"]]
					]
		},
		Overlay[{
			Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "DogEarOpen"]]],
			Pane[
				Row[Flatten[{
					Button[
						Style[Mouseover[ Style[label, FontColor -> Orange], Style[label, FontColor -> Red] ], "DialogStyle"],
						(Quiet[WolframAlpha[]]; (* trigger autoloading *) changeLinkState[Dynamic[var], Dynamic[aux], Not @ linksQ]),
						Appearance -> None,
						BaseStyle -> {},
						DefaultBaseStyle -> {},
						Method -> "Queued"
					],
					cdfPodMenu[podid, data, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts]]
					}],
					$verticalBar2
				],
				ImageMargins -> {{15, 3}, {3, 0}}
			]
			},
			{1,2},
			2,
			Alignment -> {Left, Bottom}
		]
	]


dogEarPanel[innerframe_, podid_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], elements_] :=
	Block[{},
		If[Not @ MemberQ[elements, "PodCornerCurl"], Return[innerframe]];
				
		DynamicModule[{layers = {1}, currentlayer = 1},
			EventHandler[
				Overlay[{
					innerframe,
					dogEarClosed[],
					dogEarOpen[podid, data, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts]]
					},
					Dynamic[layers],
					Dynamic[currentlayer],
					ImageSize -> All,
					Alignment -> Bottom
				],
				{
					"MouseEntered" :> (FEPrivate`Set[layers, {1,2}]; FEPrivate`Set[currentlayer, 1]),
					"MouseMoved" :>
						FEPrivate`Which[
							FEPrivate`And[
								FEPrivate`Less[FEPrivate`Part[FrontEnd`CurrentValue[{"MousePosition","EventHandlerAbsolute"}],1], 24],
								FEPrivate`Less[FEPrivate`Part[FrontEnd`CurrentValue[{"MousePosition","EventHandlerAbsolute"}],2], 25]
							],
							FEPrivate`Set[layers, {1,3}]; FEPrivate`Set[currentlayer, 3],
							FEPrivate`Greater[FEPrivate`Part[FrontEnd`CurrentValue[{"MousePosition","EventHandlerAbsolute"}],2], 50],
							FEPrivate`Set[layers, {1,2}]; FEPrivate`Set[currentlayer, 1]
						],
					"MouseExited" :> (FEPrivate`Set[layers, {1}]; FEPrivate`Set[currentlayer, 1])
				}
			]
		]
	]






FormatAllMInputs[{podvarseq___}] := 
	Block[{list},
		list = Flatten[Map[formatPodMInputsOnly, {podvarseq}]];
		If[ list === {}, {Style["No inputs were found", Italic, Gray]}, list]
	]


formatPodMInputsOnly[XMLElement["pod", values_, data_]] :=
Block[{subpods},
	subpods = Flatten[Cases[data, XMLElement["subpod", _, d_] :> d]];
	subpods = Flatten[formatSubpodMInputLeft /@ subpods];

	If[subpods === {}, Return[{}]];
	
	innerFrame[
		Column[Flatten[{
			podTitle[values, ":"],
			subpods
			}],
			ItemSize -> Scaled[0.998],
			Spacings -> .8
		]
	]
]


podTitle[{___, "title" -> str_, ___}, suffix_String] := Style[str <> suffix, FontColor -> RGBColor[0.411764, 0.352941, 0.494117], AutoItalicWords -> {"Mathematica"}]

podTitle[{___, "title" -> str_, ___}, {c_Integer, 0 | 1, "Input" | "Output" | "Content"}] := podTitle[{"title" -> str}, "  "]

podTitle[{___, "title" -> str_, ___}, {c_Integer, 0 | 1, datatype_String}] := podTitle[{"title" -> str}, " \[LongDash] " <> datatype]

podTitle[{___, "title" -> str_, ___}, {c_Integer, n_Integer, "Input" | "Output" | "Content"}] :=
	Style[Row[{str,
		Style[Row[{" (", ToString[c], " of ", ToString[n], ")  "}], FontColor -> GrayLevel[0.75], PrivateFontOptions -> {"OperatorSubstitution" -> False}]
		}],
		FontColor -> RGBColor[0.411764, 0.352941, 0.494117],
		AutoItalicWords -> {"Mathematica"}
	]

podTitle[{___, "title" -> str_, ___}, {c_Integer, n_Integer, datatype_String}] :=
	Style[Row[{
		str <> " \[LongDash] " <> datatype <> "  ",
		Style[Row[{" (", ToString[c], " of ", ToString[n], ")  "}], FontColor -> GrayLevel[0.75], PrivateFontOptions -> {"OperatorSubstitution" -> False}]
		}],
		FontColor -> RGBColor[0.411764, 0.352941, 0.494117],
		AutoItalicWords -> {"Mathematica"}
	]


podInfos[data_] := Flatten[podInfo /@ data]

podInfo[XMLElement["infos", values_, data:{__}]] := formatPodInfo /@ Cases[data, _XMLElement]

podInfo[other_] := {}


formatPodInfo[XMLElement["info", values_, data:{___, Except[_XMLElement], ___}]] :=
	formatPodInfo[XMLElement["info", values, Cases[data, _XMLElement]]]

formatPodInfo[XMLElement["info", values_, {XMLElement["units", _, data_]}]] :=
	formatPodInfoUnits[data]

formatPodInfo[info:XMLElement["info", {___, "text" -> text_, ___}, data:{___, XMLElement["link", _, _], ___}]] :=
	With[{base = formatPodInfoBase[info]},
		Mouseover[
			Row[{base, Style[" \[RightGuillemet]", Orange]}],
			Row[Flatten[{base, formatLink /@ Cases[data, XMLElement["link", _, _]]}], " | "],
			ImageSize -> {Automatic, All}
		]
	]

formatPodInfo[info:XMLElement["info", {___, "text" -> text_, ___}, data_]] :=
	formatPodInfoBase[info]

formatPodInfo[XMLElement["info", {}, {onelink : XMLElement["link", _, _]}]] :=
	formatLink[onelink, " \[RightGuillemet]"]

formatPodInfo[XMLElement["info", values_, data_]] :=
	Style[Row[data /. elem : XMLElement["link", _, _] :> formatLink[elem], " | "], FontColor -> Gray]

formatPodInfo[other_] := {}


formatPodInfoUnits[{___, XMLElement["cell", _, {cell_}], ___}] :=
	podInfoOpener[
		"Units",
		Style[prepareSubpodCellContent[cell, Automatic, ResetAndUndeploy], Background -> White]
	]

formatPodInfoUnits[{___, XMLElement["img", {___, "ImportedData" -> image_, ___}, _], ___}] :=
	podInfoOpener[
		"Units",
		image
	]

formatPodInfoUnits[data : {___, XMLElement["unit", _, _], ___}] := 
	podInfoOpener[
		"Units",
		Grid[
			Cases[data, XMLElement["unit", {___, "short" -> short_, ___, "long" -> long_, ___}, _] :> {short, long}],
			Frame -> All,
			FrameStyle -> LightGray,
			Alignment -> Left,
			Background -> White,
			Spacings -> {1,1},
			ItemStyle -> {{FontColor -> Black, FontColor -> Gray}, None}
		]
	]

formatPodInfoUnits[other_] := {}


podInfoOpener[label_, content_] :=
	DynamicModule[{open = False, button},
		button = Button[
			Row[{
				PaneSelector[{
					True -> Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "OrangeMinus"]]],
					False -> Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "OrangePlus"]]]},
					Dynamic[open]
				],
				" ",
				Mouseover[Style[label, FontColor -> Orange], Style[label, FontColor -> Red], ImageSize -> All]
			}],
			open = Not[open],
			Appearance -> None,
			BaseStyle -> {}
		];
		PaneSelector[{
				False -> Dynamic[button],
				True -> Grid[{
					{Dynamic[button]},
					{Overlay[{
						Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "PodInfoBackground"]]],
						Pane[content, ImageMargins -> {{30, 30}, {10, 15}}]
						},
						Alignment -> {Left, Top}
					]}
					}, Alignment -> Right]
			},
			Dynamic[open],
			ImageSize -> Automatic
		]
	]





formatPodInfoBase[XMLElement["info", {___, "text" -> text_, ___}, {___, XMLElement["cell", _, {cell_Cell}], ___}]] := formatPodInfoCell[cell]

formatPodInfoBase[XMLElement["info", {___, "text" -> text_, ___}, {___, img:XMLElement["img", _, _], ___}]] := formatPodInfoImage[img]

formatPodInfoBase[XMLElement["info", {___, "text" -> text_, ___}, _]] := text


formatPodInfoCell[cell_Cell] := RawBoxes[cell //. StyleBox[x_, a___, _Integer | (FontSize -> _) | (FontFamily -> _), b___] :> StyleBox[x, a, b]]

formatPodInfoCell[other_] := ""


formatPodInfoImage[XMLElement["img", {___, "ImportedData" -> image_, ___}, _]] := image

formatPodInfoImage[XMLElement["img", values:{___, "alt" -> alt_, ___}, _]] := 
	Framed["" (* alt *), ImageSize -> {
		ToExpression["width" /. values /. "width" -> "Automatic"],
		ToExpression["height" /. values /. "height" -> "Automatic"]},
		FrameStyle -> GrayLevel[0.95],
		Alignment -> {Left, Top}
	]

formatPodInfoImage[other_] := ""






podDefinitions[data_] := Flatten[podDefinition /@ data]

podDefinition[XMLElement["definitions", values_, data:{___, XMLElement["definition", _, _], ___}]] :=
	podInfoOpener[
		"Definitions",
		Style[ Column[Flatten[formatPodDefinition /@ data], Spacings -> 1.2], FontColor -> GrayLevel[0.4] ]
	]

podDefinition[other_] := {}

formatPodDefinition[XMLElement["definition", values_, data_]] :=
	TextCell[
		Row[{
			Style[("word" /. values) <> ":", Bold],
			"\[IndentingNewLine]" <> ("desc" /. values)
		}],
		ParagraphIndent -> -10
	]

formatPodDefinition[other_] := {}



podNotes[data_] := Flatten[podNote /@ data]

podNote[XMLElement["notes", values_, data:{__}]] := 
	podInfoOpener[
		"Notes",
		Style[ Column[Flatten[formatPodNote /@ data], Spacings -> 1.2], FontColor -> GrayLevel[0.4] ]
	]

podNote[other_] := {}

formatPodNote[XMLElement["note", values_, {note_String}]] := TextCell[note]

formatPodNote[other_] := {}



podDefinitionsAndNotes[list_] := 
	Block[{defs, notes},
		defs = Flatten[Cases[list, XMLElement["definitions", values_, data_] :> data]];
		defs = Cases[defs, XMLElement["definition", _, _]];
		notes = Flatten[Cases[list, XMLElement["notes", values_, data:{__}] :> data]];
		notes = Cases[notes, XMLElement["note", _, {_String}]];
		podInfoOpener[
			"Definitions and notes",
			Style[ Column[Flatten[{formatPodDefinition /@ defs, formatPodNote /@ notes}], Spacings -> 1.2], FontColor -> GrayLevel[0.4]]
		]
	]



podStates[podid_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_]] :=
	If[# === {}, "", Row[#, "  "]]& @ Flatten[{
		podState[podid, #, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], {2}]& /@ data,
		podTearOffButton[podid, Dynamic[q], Dynamic[opts]],
		podTearOffMenu[podid, data, Dynamic[var], Dynamic[q], Dynamic[opts]]
	}]

subpodStates[podid_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], c_] :=
	If[# === {}, "", Row[#, "  "]]& @ Flatten[
		podState[podid, #, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], {3, c}]& /@ data
	]

podState[podid_, XMLElement["states", values_, data_], Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], auxpart_] :=
	Row[Flatten[podStateControl[podid, #, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], auxpart]& /@ data], $verticalBar3]

podState[podid_, XMLElement["sounds", values_, data_], Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], auxpart_] :=
	Row[Flatten[podSoundControls /@ data], $verticalBar3]

podState[podid_, other_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], auxpart_] := {}


podStateControl[podid_, e:XMLElement["state", {___, "name" -> name_, ___}, {}], Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], {auxpart__}] :=
	Button[
		lightFrameButtonBase[name],
		CheckAbort[
			Quiet[WolframAlpha[]]; (* trigger autoloading *)
			changePodState[podid, stateInput[e], Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], {auxpart}],
			aux[[auxpart]] = False
		],
		ImageSize -> Automatic,
		Method -> "Queued",
		Appearance -> None
	]

podStateControl[podid_, XMLElement["statelist", {___, "value" -> value_, ___}, states:{XMLElement["state", {___, "name" -> _, ___}, {}]..}],
							Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], {auxpart__}] :=
	ActionMenu[
		lightFrameActionMenuBase[value],
		Map[(stateName[#] :> CheckAbort[
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				changePodState[podid, stateInput[#], Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], {auxpart}],
				aux[[auxpart]] = False
			])&,
			states
		],
		Appearance -> None,
		Method -> "Queued"
	]

podStateControl[other___] := {}


lightFrameButtonBase[label_] := 
	Style[
		Framed[
			Mouseover[
				Style[label, FontColor -> Orange],
				Style[label, FontColor -> Red],
				BaselinePosition -> Baseline,
				ImageSize -> All
			],
			Background -> GrayLevel[0.98],
			BaselinePosition -> Baseline,
			FrameMargins -> {{5,5},{1,1}},
			FrameStyle -> GrayLevel[0.9],
			RoundingRadius -> 3
		],
		"DialogStyle"
	]

lightFrameActionMenuBase[label_] := 
	Style[
		Framed[
			Mouseover[
				Row[{Style[label, FontColor -> Orange], $verticalBar1, Style["\[DownPointer]", 14]}],
				Row[{Style[label, FontColor -> Red], $verticalBar1, Style["\[DownPointer]", 14]}],
				BaselinePosition -> Baseline,
				ImageSize -> All
			],
			Background -> GrayLevel[0.98],
			BaselinePosition -> Baseline,
			FrameMargins -> {{5,5},{1,1}},
			FrameStyle -> GrayLevel[0.9],
			RoundingRadius -> 3
		],
		"DialogStyle"
	]



podTearOffButton[podid_, Dynamic[q_], Dynamic[opts_]] :=
If[ !MemberQ[AppearanceElements /. opts /. Options[WolframAlpha], "TearOffButtons"],
	{},
	Tooltip[
		Button[
			Dynamic[RawBoxes[FEPrivate`FrontEndResource["FEBitmaps", "ManipulatePasteIcon"]]],
			Quiet[WolframAlpha[]]; (* trigger autoloading *)
			doPodTearOffButton[q, IncludePods -> podid, AppearanceElements -> {"Pods"},
				Sequence @@ DeleteCases[opts, _[IncludePods | AppearanceElements | Asynchronous | Method, _]]],
			Appearance -> None,
			BaseStyle -> {}
		],
		Dynamic[FEPrivate`FrontEndResource["WAStrings", "ReplaceCellPod"]]
	]
]


doPodTearOffButton[q_, opts___] := (
	SelectionMove[EvaluationNotebook[], All, ButtonCell];
	NotebookWrite[EvaluationNotebook[],
		Cell[BoxData[ToBoxes[Defer[WolframAlpha][q, opts]]], "Input"],
		All
	];
	FrontEndTokenExecute[EvaluationNotebook[], "HandleShiftReturn"]
)


podTearOffMenu[podid_, data_, Dynamic[var_], Dynamic[q_], Dynamic[opts_]] := 
	With[{n = Count[data, XMLElement["subpod", _, _]]},
		Block[{dataformats, textformats},
			If[ !MemberQ[AppearanceElements /. opts /. Options[WolframAlpha], "PodMenus"],
				{},
				dataformats = First[Flatten[{Cases[data, XMLElement["dataformats", _, {dataformats_String}] :> dataformats, Infinity], ""}]];
				{dataformats, textformats} = {parseDataFormatsString[dataformats], parseTextFormatsString[dataformats]};
			
				Tooltip[
					ActionMenu[
						Dynamic[RawBoxes[FEPrivate`FrontEndResource["FEBitmaps", "CirclePlusIcon"]]],
						Flatten[{
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "FormattedPod"]] :>
								(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodTearOffMenu["Pod", podid, var, q, opts]),
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "SubpodContent"]] :>
								(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodTearOffMenu["Content", podid, n, var, q, opts]), 
							If[textformats === {}, {}, Delimiter],
							Map[(Dynamic[FEPrivate`FrontEndResource["WAStrings", #]] :>
									(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodTearOffMenu[#, podid, n, var, q, opts]))&,
								textformats
							],
							If[dataformats === {}, {}, Delimiter],
							Map[(Dynamic[FEPrivate`FrontEndResource["WAStrings", #]] :>
									(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodTearOffMenu[#, podid, n, var, q, opts]))&,
								dataformats
							]
						}],
						Appearance -> None,
						BaselinePosition -> (Top -> Top)
					],
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "PasteRelatedInputs"]]
				]
			]
		]
	]


doPodTearOffMenu["Pod", podid_, var_, q_, opts_] :=
	tearOffPaste[Defer[WolframAlpha][q, IncludePods -> podid, AppearanceElements -> {"Pods"},
		Sequence @@ DeleteCases[opts, _[IncludePods | AppearanceElements | Asynchronous | Method, _]]]]

doPodTearOffMenu[type_, podid_, 1, var_, q_, opts_] :=
	tearOffPaste[Defer[WolframAlpha][q, {{podid, 1}, type}, Sequence @@ Cases[opts, _[InputAssumptions | PodStates, _]]]]

doPodTearOffMenu[type_, podid_, n_Integer, var_, q_, opts_] :=
	tearOffPaste[Map[Defer[WolframAlpha][q, {{podid, #}, type}, Sequence @@ Cases[opts, _[InputAssumptions | PodStates, _]]]&, Range[n]]]


tearOffPaste[expr_] := (
	SelectionMove[EvaluationNotebook[], After, ButtonCell];
	NotebookWrite[EvaluationNotebook[], Cell[BoxData[ToBoxes[expr]], "Input"], All];
	FrontEndTokenExecute[EvaluationNotebook[], "HandleShiftReturn"]
)





stateName[XMLElement["state", {___, "name" -> name_, ___}, _]] := name

stateInput[XMLElement["state", {___, "input" -> input_, ___}, _]] := input

stateInput[XMLElement["state", {___, "name" -> name_, ___}, _]] := name


changePodState[podid_, newstate_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_], {auxpart__}] := 
Block[{pods, podstates, newopts},

	aux[[auxpart]] = True;
	
	podstates = PodStates /. opts /. PodStates -> {};
	podstates = Flatten[{podstates, newstate}];

	newopts = Flatten[{DeleteCases[opts, _[PodStates, _]], PodStates -> podstates}];
	
	pods = Cases[
		WolframAlpha[q, "ProcessedXML", IncludePods -> {podid}, Asynchronous -> False, TimeConstraint -> {$AlphaQueryTimeConstraint, None, None, None}, Sequence @@ newopts],
		XMLElement["pod", _, _],
		Infinity
	];
	If[pods =!= {}, var = expandData[First[pods]]; opts = newopts, Message[WolframAlpha::nopst, newstate, podid]; (* Beep[] *)];

	aux[[auxpart]] = False;
]


podSoundControls[XMLElement["sound", {___, "ImportedData" -> sound_, ___}, _]] :=
Block[{},
	If[ Head[sound] =!= Sound, Return[{}]];
	Button[
		lightFrameButtonBase["Play sound"],
		EmitSound[sound],
		ImageSize -> Automatic,
		Appearance -> None
	]
]


(* MIDI sends the full Sound object through, provided you've asked for msound *)
podSoundControls[XMLElement["sound", _, {a_String}]] := 
With[{sound = ToExpression[a]},
	If[ Head[sound] =!= Sound, Return[{}]];
	Button[
		lightFrameButtonBase["Play sound"],
		EmitSound[sound],
		ImageSize -> Automatic,
		Appearance -> None
	]
]


podSoundControls[other_] := {}



cdfPodMenu[podid_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[q_], Dynamic[opts_]] := 
		Block[{dataformats, textformats},
			If[ !MemberQ[AppearanceElements /. opts /. Options[WolframAlpha], "CDFPodMenus"],
				{},
				dataformats = First[Flatten[{Cases[data, XMLElement["dataformats", _, {dataformats_String}] :> dataformats, Infinity], ""}]];
				{dataformats, textformats} = {parseDataFormatsString[dataformats], parseTextFormatsString[dataformats]};
				dataformats = {};

				Tooltip[
					ActionMenu[
						Style[Mouseover[
							Style[Row[{Dynamic[FEPrivate`FrontEndResource["WAStrings", "CopyAs"]], " \[RightGuillemet]"}], FontColor -> Orange],
							Style[Row[{Dynamic[FEPrivate`FrontEndResource["WAStrings", "CopyAs"]], " \[RightGuillemet]"}], FontColor -> Red]
							],
							"DialogStyle"
						],
						Flatten[{
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "FormattedPod"]] :>
								(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodCopyAs["Pod", podid, var, q, opts]),
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "CDF"]] :>
								(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodCopyAs["CDF", podid, var, q, opts]),
							(*
							Dynamic[FEPrivate`FrontEndResource["WAStrings", "SubpodContent"]] :>
								(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodCopyAs["Content", podid, var, q, opts]), 
							*)
							If[textformats === {}, {}, Delimiter],
							Map[(Dynamic[FEPrivate`FrontEndResource["WAStrings", #]] :>
									(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodCopyAs[#, podid, var, q, opts]))&,
								textformats
							],
							If[dataformats === {}, {}, Delimiter],
							Map[(Dynamic[FEPrivate`FrontEndResource["WAStrings", #]] :>
									(Quiet[WolframAlpha[]]; (* trigger autoloading *) doPodCopyAs[#, podid, var, q, opts]))&,
								dataformats
							]
						}],
						Appearance -> None (* ,
						Method -> "Queued"*)
					],
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "CopyPod"]]
				]
			]
		]


changeLinkState[Dynamic[var_], Dynamic[aux_], value_] := 
	Block[{},
		(* Change the value of var to coerce dynamic updating *)
		var = {var};
		aux[[4]] = value;
		var = First[var];
	]


(*
Rasterize currently trouble rendering Dynamic content in the Plugin. So we
resolve Dynamic content before rasterizing.
*)

doPodCopyAs["Pod", podid_, var_, q_, opts_] :=
	CopyToClipboard[Rasterize[WolframAlpha[q, IncludePods -> podid, AppearanceElements -> {"Pods"},
		Sequence @@ DeleteCases[opts, _[IncludePods | AppearanceElements | Asynchronous | Method, _]]] //.
		HoldPattern[Dynamic][x_, ___] :> x, "Image"]]

doPodCopyAs["CDF", podid_, var_, q_, opts_] :=
	CopyToClipboard[
		WolframAlpha[q,
			AppearanceElements -> {"Brand", "CDFBrand", "ContentPadding", "Pods"},
			IncludePods -> podid,
			Sequence @@ Cases[opts, _[InputAssumptions | PodStates, _]]
		]
	]

doPodCopyAs[type_, podid_, var_, q_, opts_] :=
	Block[{result},
		result = WolframAlpha[q, {{podid, All}, type}, Sequence @@ Cases[opts, _[InputAssumptions | PodStates, _]]];
		CopyToClipboard[If[MatchQ[result, {__Rule}], Last /@ result, result]]
	]







formatLink[XMLElement["link", {___, "url" -> url_, ___, "text" -> text_, ___}, {}]] :=
	Hyperlink[text, url, 
		BaseStyle -> {"Hyperlink", FontColor -> Orange},
		ActiveStyle -> {"HyperlinkActive", FontColor -> Red},
		ButtonFunction :> (FrontEndExecute[{NotebookLocate[#2, "OpenInNewWindow" -> CurrentValue["HyperlinkModifierKey"]]}]&)
	]


formatLink[XMLElement["link", {___, "url" -> url_, ___, "text" -> text_, ___}, {}], suffix_String] :=
	formatLink[XMLElement["link", {"url" -> url, "text" -> (text <> suffix)}, {}]]


$subpodIndent = "   "


formatSubpods[ podid_, title_, data_, Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[q_], Dynamic[opts_], Hold[$subpodCounter_], numberOfSubpods_, info_, appearanceElements_] :=
	Flatten[formatSubpod[podid, title, #, Dynamic[var], Dynamic[aux], Dynamic[chosen], Dynamic[open], Dynamic[q], Dynamic[opts], Hold[$subpodCounter], numberOfSubpods, info, appearanceElements]& /@ data]

formatSubpod[podid_, title_, e:XMLElement["subpod", values_, data_], Dynamic[var_], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[q_], Dynamic[opts_], Hold[$subpodCounter_], numberOfSubpods_, info_, appearanceElements_] := 
Block[{list, mforms, dataformats, textformats, subpodTitle},
	++$subpodCounter;
	If[Length[aux] === 2, AppendTo[aux, {False}]];
	If[Length[aux] === 3, AppendTo[aux, False]];
	If[Length[aux[[3]]] < $subpodCounter, aux[[3]] = PadRight[aux[[3]], $subpodCounter, False]];
	dataformats = First[Flatten[{Cases[data, XMLElement["dataformats", _, {dataformats_String}] :> dataformats], ""}]];
	{dataformats, textformats} = {parseDataFormatsString[dataformats], parseTextFormatsString[dataformats]};
	
	subpodTitle = If[
		MemberQ[data, XMLElement["states", _, _]],
		With[{c = $subpodCounter},
			Grid[{{
				Item[If[# === {}, "", #]& @ formatSubpodTitle[values], Alignment -> Left],
				Item[
					PaneSelector[{
						True -> necklaceAnimator[$necklaceAnimatorSize],
						False -> subpodStates[podid, data, Dynamic[var], Dynamic[aux], Dynamic[q], Dynamic[opts], c]
						},
						Dynamic[TrueQ[aux[[3, c]]]],
						ImageSize -> All,
						Alignment -> {Right, Top}
					],
					Alignment -> Right
				]
				}},
				Alignment -> Top,
				ItemSize -> Scaled[0.499]
			]
		],
		formatSubpodTitle[values]
	];

	list = Flatten[{
		subpodTitle,
		If[MemberQ[appearanceElements, "UseInputLeft"],
			{formatSubpodMInputLeft /@ data, formatSubpodMOutput /@ data},
			{}
		],
		If[MemberQ[appearanceElements, "UseInput"],
			formatSubpodMInputRight /@ data,
			{}
		],
		If[MemberQ[appearanceElements, "Extrusion"],
			mforms = formatMathematicaForm[#, "minput", "Input", Dynamic[chosen], Dynamic[open], Dynamic[q], Dynamic[opts], podid, title, $subpodCounter, numberOfSubpods]& /@ data;
			If[Flatten[mforms] === {},
				mforms = formatMathematicaForm[#, "moutput", "Output", Dynamic[chosen], Dynamic[open], Dynamic[q], Dynamic[opts], podid, title, $subpodCounter, numberOfSubpods]& /@ data;
			];
			{
				mforms,
				formatSubpodCell[#, Dynamic[aux], Dynamic[chosen], Dynamic[open], Dynamic[q], Dynamic[opts], podid, title, $subpodCounter, numberOfSubpods, textformats, dataformats]& /@ data
			},
			formatSubpodCell[#, Dynamic[aux], Dynamic[q], Dynamic[opts]]& /@ data
		],
		formatSubpodImage /@ data,
		subpodTearOffMenu[podid, $subpodCounter, numberOfSubpods, e, Dynamic[var], Dynamc[q], Dynamic[opts], appearanceElements]
	}];
	If[list === {}, {}, Column[list]]
]

formatSubpod[podid_, other_, ___] := {}

formatSubpodTitle[{___, "title" -> title:Except[""], ___}] :=
	Style["   " <> title <> ":", FontColor -> RGBColor[0.411764, 0.352941, 0.494117], AutoItalicWords -> {"Mathematica"}]

formatSubpodTitle[other_] := {}


subpodTearOffMenu[podid_, c_, n_, e:XMLElement["subpod", values_, data_], Dynamic[var_], Dynamc[q_], Dynamic[opts_], appearanceElements_] := 
If[ !MemberQ[appearanceElements, "SubpodMenus"],
	{},
	Item[
		Tooltip[
			ActionMenu[
				Dynamic[RawBoxes[FEPrivate`FrontEndResource["FEBitmaps", "CirclePlusIcon"]]],
				{
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "SubpodContent"]] :> 
						(Quiet[WolframAlpha[]]; (* trigger autoloading *) doSubpodTearOffMenu["Content", podid, c, e, q, opts]),
					Delimiter,
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "Plaintext"]] :>
						(Quiet[WolframAlpha[]]; (* trigger autoloading *) doSubpodTearOffMenu["Plaintext", podid, c, e, q, opts]),
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "Input"]] :> 
						(Quiet[WolframAlpha[]]; (* trigger autoloading *) doSubpodTearOffMenu["Input", podid, c, e, q, opts])
				},
				Appearance -> None
			],
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "PasteRelatedInputsSubpod"]]
		] // Grid[{{$subpodIndent, #}}]&,
		Alignment -> Left
	]
]



doSubpodTearOffMenu["Content", podid_, c_, e_, q_, opts_] :=
	subpodTearOffPaste[Defer[WolframAlpha][q, {{podid, c}, "Content"}, Sequence @@ Cases[opts, _[Assumption | PodStates, _]]]]

doSubpodTearOffMenu["Plaintext", podid_, c_, e_, q_, opts_] :=
	subpodTearOffPaste[Defer[WolframAlpha][q, {{podid, c}, "Plaintext"}, Sequence @@ Cases[opts, _[Assumption | PodStates, _]]]]

doSubpodTearOffMenu["Input", podid_, c_, e_, q_, opts_] :=
	subpodTearOffPaste[Defer[WolframAlpha][q, {{podid, c}, "Input"}, Sequence @@ Cases[opts, _[Assumption | PodStates, _]]]]



subpodTearOffPaste[expr_] := (
	SelectionMove[EvaluationNotebook[], After, ButtonCell];
	NotebookWrite[EvaluationNotebook[], Cell[BoxData[ToBoxes[expr]], "Input"], All];
	FrontEndTokenExecute[EvaluationNotebook[], "HandleShiftReturn"]
)


formatSubpodMInputAndOutput[XMLElement["minput", _, {text_}]] :=
	Grid[{{
		Item[Style["In[]:=", "CellLabel"], Alignment -> Right],
		formatSubpodUtility["minput content", text],
		Item[formatSubpodUtility["minput link", text], Alignment -> Right]
		},
		{
		Item[Style["Out[]=", "CellLabel"], Alignment -> Right],
		Style[
			Dynamic[TimeConstrained[ToExpression[text, InputForm], 3, $TimedOut], System`DestroyAfterEvaluation -> True, SynchronousUpdating -> False],
			"StandardForm", "Output", 12, ShowStringCharacters -> False, ScriptLevel -> 0],
		SpanFromLeft
		}},
		ItemSize -> {{Full, Fit, Full}},
		Alignment -> Left
	]

formatSubpodMInputAndOutput[other_] := {}

formatSubpodMInputLeft[XMLElement["minput", _, {text_}]] :=
	Grid[{{
		$subpodIndent,
		Item[formatSubpodUtility["minput content", text], Alignment -> Left],
		Item[formatSubpodUtility["minput link", text], Alignment -> Right]}},
		ItemSize -> {{Full, Fit, Full}}
	]

formatSubpodMInputLeft[other_] := {}

formatSubpodMInputRight[XMLElement["minput", _, {text_}]] := 
	Grid[{{formatSubpodUtility["minput right link", text]}}, ItemSize -> Fit, Alignment -> Right]

formatSubpodMInputRight[other_] := {}


formatSubpodMOutput[XMLElement["moutput", _, {text_}]] :=
	Grid[
		{{$subpodIndent, formatSubpodUtility["moutput", text]}},
		Alignment -> {Left, Baseline}]

formatSubpodMOutput[other_] := {}


formatSubpodCell[XMLElement["cell", _, {cell_}], Dynamic[aux_], Dynamic[q_], Dynamic[opts_]] :=
	Grid[{{
		$subpodIndent,
		prepareSubpodCellContent[cell, If[TrueQ @ aux[[4]], "AutomaticWithLinks", Automatic], ResetAndUndeploy, Dynamic[q], Dynamic[opts]]
		}},
		Alignment -> {Left, Baseline}]

formatSubpodCell[XMLElement["cell", _, {cell_}], Dynamic[aux_], Dynamic[chosen_], Dynamic[open_], Dynamic[q_], Dynamic[opts_], podid_, title_, c_, n_, textformats_, dataformats_] :=
	Grid[{{
		chosenIndent[Dynamic[chosen], {podid, c, {"Content", "ComputableData", "FormattedData", "FormulaData", "NumberData", "QuantityData", "SoundData", "TimeSeriesData"}}, None],
		Tooltip[
			Button[
				Framed[
					prepareSubpodCellContent[cell, If[TrueQ @ aux[[4]], "AutomaticWithLinks", Automatic], ResetAndDeploy, Dynamic[q], Dynamic[opts]],
					FrameStyle -> chosenFrameColor[Dynamic[chosen], {podid, c}, RGBColor[1., 0.723, 0.093], GrayLevel[0.85], None],
					Background -> Dynamic[FEPrivate`If[FrontEnd`CurrentValue["MouseOver"], RGBColor[1., 0.955, 0.836], None]],
					RoundingRadius -> 5
				],
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				chosen = {podid, title, c, n, "Content"};
				opts = updateOptionSetting[opts, Method, "ExtrusionChosen" -> chosen];
				SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
				If[$AlphaQueryExtrusionClickClose[] || MemberQ[CurrentValue["ModifierKeys"], "Command" | "Alt"],
					open = False;
					opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open]

				];
				If[$AlphaQueryExtrusionClickEvaluate[],
					SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
					FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
				],
				Appearance -> None,
				ImageSize -> Automatic,
				Alignment -> Left
			],
			chosenTooltip[Dynamic[chosen], {podid, c, {"Content", "ComputableData", "FormattedData", "FormulaData", "NumberData", "QuantityData", "SoundData", "TimeSeriesData"}}, None,
				PaneSelector[{
					"Content" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedSubpod"]],
					"ComputableData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedComputableData"]],
					"FormattedData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedFormattedData"]],
					"FormulaData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedFormulaData"]],
					"NumberData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedNumberData"]],
					"QuantityData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedQuantityData"]],
					"SoundData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedSoundData"]],
					"TimeSeriesData" -> Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedTimeSeriesData"]]
					},
					Dynamic[FEPrivate`Part[chosen, 5]],
					ImageSize -> Automatic
				],
				Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectThisSubpod"]]
			],
			BaseStyle -> {
				ContextMenu -> Flatten[{
					KernelMenuItem[cachedFrontEndResource["WAStrings", "CopySubpodContent"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						CopyToClipboard[ExpressionCell[prepareSubpodCellContent[cell, Automatic, ResetAndUndeploy], "Input"]]
					],
					KernelMenu[cachedFrontEndResource["WAStrings", "CopyAs"], Flatten[{
						Map[
							KernelMenuItem[cachedFrontEndResource["WAStrings", #], CopyToClipboard[ExpressionCell[WolframAlpha[q, {{podid, c}, #}], "Input"]]]&,
							textformats
						],
						If[textformats === {}, {}, Delimiter],
						Map[
							KernelMenuItem[cachedFrontEndResource["WAStrings", #], CopyToClipboard[ExpressionCell[WolframAlpha[q, {{podid, c}, #}], "Input"]]]&,
							dataformats
						],
						If[dataformats === {}, {}, Delimiter],
						KernelMenuItem[cachedFrontEndResource["WAStrings", "FormattedPod"], CopyToClipboard[ExpressionCell[WolframAlpha[q, AppearanceElements -> {"Pods"}, IncludePods -> podid], "Input"]]]
					}]],
					Delimiter,
					KernelMenu[cachedFrontEndResource["WAStrings", "PasteInputFor"], Flatten[{
						Map[
							KernelMenuItem[cachedFrontEndResource["WAStrings", #], CellPrint[ExpressionCell[Defer[WolframAlpha][q, {{podid, c}, #}], "Input", GeneratedCell -> False, CellAutoOverwrite -> False]]]&,
							textformats
						],
						If[textformats === {}, {}, Delimiter],
						Map[
							KernelMenuItem[cachedFrontEndResource["WAStrings", #], CellPrint[ExpressionCell[Defer[WolframAlpha][q, {{podid, c}, #}], "Input", GeneratedCell -> False, CellAutoOverwrite -> False]]]&,
							dataformats
						],
						If[dataformats === {}, {}, Delimiter],
						KernelMenuItem[cachedFrontEndResource["WAStrings", "SubpodContent"], CellPrint[ExpressionCell[Defer[WolframAlpha][q, {{podid, c}, "Content"}], "Input", GeneratedCell -> False, CellAutoOverwrite -> False]]],
						KernelMenuItem[cachedFrontEndResource["WAStrings", "FormattedPod"], CellPrint[ExpressionCell[Defer[WolframAlpha][q, AppearanceElements -> {"Pods"}, IncludePods -> podid], "Input", GeneratedCell -> False, CellAutoOverwrite -> False]]]
					}]],
					Delimiter,
					KernelMenuItem[cachedFrontEndResource["WAStrings", "Collapse"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						open = False;
						opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open]
					],
					KernelMenuItem[cachedFrontEndResource["WAStrings", "Evaluate"],
						SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
						FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
					],
					If[dataformats === {}, {}, KernelMenu[cachedFrontEndResource["WAStrings", "EvaluateAs"], 
						Map[
							KernelMenuItem[cachedFrontEndResource["WAStrings", #],
								Quiet[WolframAlpha[]]; (* trigger autoloading *)
								chosen = {podid, title, c, n, #, With[{q=q}, ToString[Unevaluated[WolframAlpha[q, {{podid, c}, #}]], InputForm]]};
								opts = updateOptionSetting[opts, Method, "ExtrusionChosen" -> chosen];
								SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
								FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
							]&,
							dataformats
						]
					]],
					KernelMenuItem[cachedFrontEndResource["WAStrings", "CollapseAndEvaluate"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						open = False;
						opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open];
						SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
						FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
					]
				}]
			}		
		]
		}},
		Alignment -> {Left, Baseline},
		Spacings -> 0.4
	]
		

formatSubpodCell[other_, ___] := {}


(*
prop: either "minput" or "moutput"
type: either "Input" or "Output"
mform: either the minput string or the moutput string
*)

formatMathematicaForm[XMLElement[prop_, _, {mform_String}], prop_, type_, Dynamic[chosen_], Dynamic[open_], Dynamic[q_], Dynamic[opts_], podid_, title_, c_, n_] := 
	Grid[{{
		chosenIndent[Dynamic[chosen], {podid, c, {"Input", "Output"}}, mform],
		Tooltip[
			Button[
				Framed[
					Style[
						shortenMathematicaBoxes[ ToExpression[mform, InputForm, AlphaQueryMakeBoxes], {"StandardForm", 12}, Scrollbars -> {False, False} ],
						If[type === "Input", Bold, Plain]
					],
					FrameStyle -> {Dynamic[FEPrivate`If[FrontEnd`CurrentValue["MouseOver"], RGBColor[1., 0.723, 0.093], GrayLevel[0.85]]]},
					Background -> Dynamic[FEPrivate`If[FrontEnd`CurrentValue["MouseOver"], RGBColor[1., 0.955, 0.836], RGBColor[0.973, 0.978, 0.98]]],
					RoundingRadius -> 5,
					FrameMargins -> {{8,8},{5,5}}
				],
				Quiet[WolframAlpha[]]; (* trigger autoloading *)
				chosen = {podid, title, c, n, type, mform};
				opts = updateOptionSetting[opts, Method, "ExtrusionChosen" -> chosen];
				SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
				If[$AlphaQueryExtrusionClickClose[] || MemberQ[CurrentValue["ModifierKeys"], "Command" | "Alt"],
					open = False;
					opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open]
				];
				If[$AlphaQueryExtrusionClickEvaluate[],
					SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
					FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
				],
				Appearance -> None,
				ImageSize -> Automatic,
				Alignment -> Left
			],
			chosenTooltip[Dynamic[chosen], {podid, c, {"Input", "Output"}}, mform,
				Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectedInput"]],
				Dynamic[FEPrivate`FrontEndResource["WAStrings", "SelectThisInput"]]
			],
			BaseStyle -> {
				ContextMenu -> {
					KernelMenuItem[cachedFrontEndResource["WAStrings", "CopyInput"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						CopyToClipboard[Cell[BoxData[ToExpression[mform, InputForm, AlphaQueryMakeBoxes]], "Input"]]
					],
					Delimiter,
					KernelMenuItem[cachedFrontEndResource["WAStrings", "Collapse"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						open = False;
						opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open]
					],
					KernelMenuItem[cachedFrontEndResource["WAStrings", "Evaluate"],
						SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
						FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
					],
					KernelMenuItem[cachedFrontEndResource["WAStrings", "CollapseAndEvaluate"],
						Quiet[WolframAlpha[]]; (* trigger autoloading *)
						open = False;
						opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open];
						SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
						FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"]
					],
					Delimiter,
					KernelMenuItem[cachedFrontEndResource["WAStrings", "RemoveResults"],
						SelectionMove[ButtonNotebook[], All, ButtonCell];
						NotebookWrite[ButtonNotebook[], Cell[BoxData[mform], "Input"], All];
						SelectionMove[ButtonNotebook[], After, CellContents],
						Method -> "Queued" (* without this, the SelectionMoves don't happen properly *)
					]
				}
			}		
		]
		}},
		Alignment -> {Left, Baseline},
		Spacings -> 0.4
	]

formatMathematicaForm[other_, ___] := {}



SetAttributes[KernelMenuItem, HoldRest];

KernelMenu[label_, list_List] := System`Menu[label, list]

KernelMenuItem[label_, action_, opts___] := System`MenuItem[label, System`KernelExecute[action], System`MenuEvaluator -> Automatic, opts]



chosenIndent[Dynamic[chosen_], {podid_, c_, types_}, mform_] :=
	PaneSelector[{
		True -> Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "ChosenSubpodIndicator"]]],
		False -> ""
		},
		Dynamic[FEPrivate`Or[
			FEPrivate`And[
				FEPrivate`SameQ[FEPrivate`Part[chosen, 1], podid],
				FEPrivate`SameQ[FEPrivate`Part[chosen, 3], c],
				FEPrivate`MemberQ[types, FEPrivate`Part[chosen, 5]]
			],
			FEPrivate`SameQ[FEPrivate`Part[chosen, 6], mform]
		]],
		ImageSize -> All
	]


chosenTooltip[Dynamic[chosen_], {podid_, c_, types_}, mform_, chosenLabel_, otherLabel_] :=
	PaneSelector[
		{ True -> chosenLabel, False -> otherLabel },
		Dynamic[FEPrivate`Or[
			FEPrivate`And[
				FEPrivate`SameQ[FEPrivate`Part[chosen, 1], podid],
				FEPrivate`SameQ[FEPrivate`Part[chosen, 3], c],
				FEPrivate`MemberQ[types, FEPrivate`Part[chosen, 5]]
			],
			FEPrivate`SameQ[FEPrivate`Part[chosen, 6], mform]
		]],
		ImageSize -> Automatic
	]


chosenFrameColor[Dynamic[chosen_], {podid_, c_}, mouseoverColor_, chosenColor_, otherColor_] :=
	Dynamic[
		FEPrivate`If[
			FrontEnd`CurrentValue["MouseOver"],
			mouseoverColor,
			FEPrivate`If[
				FEPrivate`And[
					FEPrivate`SameQ[FEPrivate`Part[chosen, 1], podid],
					FEPrivate`SameQ[FEPrivate`Part[chosen, 3], c],
					FEPrivate`MemberQ[
						{"Content", "ComputableData", "FormattedData", "FormulaData", "NumberData", "QuantityData", "SoundData", "TimeSeriesData"},
						FEPrivate`Part[chosen, 5]
					]
				],
				chosenColor,
				otherColor
			]
		]
	]


(* two-arg, support for regular options *)

updateOptionSetting[{a___, _[name_, _], b___}, opt:(_[name_, _])] := {a, opt, b}

updateOptionSetting[{a___}, opt_] := {a, opt}

(* three-arg, support for Method options *)

updateOptionSetting[{a___, Method -> method_, b___}, Method, opt_] := {a, Method -> updateOptionSetting[Flatten[{method}], opt], b}

updateOptionSetting[{a___}, Method, opt_] := {a, Method -> {opt}}


getOptionSetting[{a___, _[name_, val_], b___}, name_] := val

getOptionSetting[{a___, Method -> method_, b___}, Method, name_] := getOptionSetting[Flatten[{method}], name]

getOptionSetting[{a___}, name_] := None





formatSubpodImage[XMLElement["img", {___, "ImportedData" -> image_, ___}, _]] :=
	Grid[{{$subpodIndent, ResetAndUndeploy @ image}}, ItemSize -> Full]

formatSubpodImage[XMLElement["img", values:{___, "alt" -> alt_, ___}, _]] := 
	Grid[{{"     ", 
		Framed["" (* alt *), ImageSize -> {
			ToExpression["width" /. values /. "width" -> "Automatic"],
			ToExpression["height" /. values /. "height" -> "Automatic"]},
			FrameStyle -> GrayLevel[0.95],
			Alignment -> {Left, Top}
		]}}, ItemSize -> Full]

formatSubpodImage[other_] := {}


formatSubpodCellAndData[data_] := 
Block[{cell, mdata},
	cell = Flatten[Cases[data, XMLElement["cell", _, {cell_}] :> cell]];
	mdata = Flatten[Cases[data, XMLElement["formatteddata" | "computabledata", _, {_}]]];

	If[Length[cell] > 0,
		cell = First[cell];
		If[Length[mdata] > 0,
			formatSubpodCellAndDataGrid[cell, mdata],
			formatSubpodCellAndDataGrid[cell, None]
		],
		(* else *)
		If[Length[mdata] > 0,
			formatSubpodCellAndDataGrid[None, mdata],
			{}
		]
	]
]

formatSubpodCellAndDataGrid[None, mdata_List] := 
	Grid[{{
		$subpodIndent,
		"",
		Item[formatSubpodUtility["data link", mdata], Alignment -> {Right, Top}]}},
		ItemSize -> {{Full, Fit, Full}}
	]

formatSubpodCellAndDataGrid[cell_, None] := 
	Grid[{{$subpodIndent, prepareSubpodCellContent[cell, Automatic, ResetAndUndeploy]}},
		Alignment -> {Left, Baseline}
	]

formatSubpodCellAndDataGrid[cell_, mdata_List] := 
	Grid[{{
		$subpodIndent,
		Item[prepareSubpodCellContent[cell, Automatic, ResetAndUndeploy], Alignment -> Left],
		Item[formatSubpodUtility["data link", mdata], Alignment -> {Right, Top}]
		}},
		ItemSize -> {{Full, Fit, Full}}
	]



parsePartialURL[url_String] := 
	Block[{input, assumptions, tmp},
		tmp = StringSplit[#, "="]& /@ StringSplit[url, {"/input/?", "&"}];
		input = Cases[tmp, {"i", s_String} :> s];
		If[input === {}, Return[$Failed]];
		{urldecode[First @ input], InputAssumptions -> Cases[tmp, {"a", s_String} :> s]}
	]


internalLink[boxes_, {input_, newopts___}, Dynamic[q_], Dynamic[opts_]] :=
	Button[
		Framed[RawBoxes[boxes],
			BaselinePosition -> Baseline,
			FrontEnd`BoxFrame -> {{0, 0}, {1, 0}},
			ContentPadding -> False,
			FrameMargins -> {{0, 0}, {1, 0}},
			FrameStyle -> Dynamic[FEPrivate`If[FrontEnd`CurrentValue["MouseOver"],
				(* RGB: 123, 171, 208 *) RGBColor[0.4824, 0.6706, 0.8157],
				(* RGB: 236, 236, 236 *) GrayLevel[0.9255]
			]]
		],
		If[MemberQ[AppearanceElements /. Flatten[{opts}] /. AppearanceElements -> {}, "CDFAssumptions"],
			NotebookLocate[{URL[WolframAlpha[input, "CDFURL", newopts, Sequence @@ DeleteCases[opts, _[InputAssumptions | PodStates, _]]]], None}, "OpenInNewWindow" -> CurrentValue["HyperlinkModifierKey"]],
			replaceQueryResults[ButtonNotebook[], input, newopts, Sequence @@ DeleteCases[opts, _[InputAssumptions | PodStates, _]]]
		],
		Method -> "Queued",
		BaseStyle -> {},
		DefaultBaseStyle -> {},
		Appearance -> None,
		ButtonNote -> input,
		ContentPadding -> False
	]


externalLink[boxes_, "URL"] := RawBoxes[boxes]

externalLink[boxes_, url_] := 
	Button[
		Grid[{{RawBoxes[boxes]}},
			BaselinePosition -> {1,1},
			FrameStyle -> Dynamic[FEPrivate`If[FrontEnd`CurrentValue["MouseOver"],
				RGBColor[0.4868, 0.50295, 0.736775],
				Directive[Dashing[{0.5, 0}], GrayLevel[0.5]]
			]],
			Dividers -> {None, 2 -> True},
			Spacings -> {0,0}
		],
		FrontEndExecute[{NotebookLocate[{URL[url], None}, "OpenInNewWindow" -> True]}],
		Evaluator -> None,
		BaseStyle -> {},
		DefaultBaseStyle -> {},
		Appearance -> None,
		ButtonNote -> url,
		ContentPadding -> False
	]


prepareSubpodCell[cell_Cell, simplifiers_, wrapper_] :=
	wrapper @ RawBoxes @ boxSimplify[#, simplifiers]& @ 
		Join[cell, Cell[FontFamily -> "Times", FontSize -> 14, ScriptLevel -> 0, Background -> None]]


prepareSubpodCellContent[cell_Cell, simplifiers_, wrapper_, args___] :=
	Block[{opts},
		
		opts = List @@ Rest[cell];
		opts = Flatten[{opts, FontFamily -> "Times", FontSize -> 14, ScriptLevel -> 0, Background -> None}];

		wrapper @ RawBoxes @ boxSimplify[#, simplifiers, args]& @ 
		If[MatchQ[cell, Cell[BoxData[_], ___]],
			StyleBox[First @ First @ cell, Sequence @@ opts],
			Cell[First @ cell, Sequence @@ opts]
		]
	]



ResetAndDeploy /: MakeBoxes[ResetAndDeploy[expr_], fmt_] := 
	MakeBoxes[Deploy[resetImageSizeLevel[expr]], fmt]

ResetAndUndeploy /: MakeBoxes[ResetAndUndeploy[expr_], fmt_] :=
	MakeBoxes[BoxForm`Undeploy[resetImageSizeLevel[expr]], fmt]

resetImageSizeLevel /: MakeBoxes[resetImageSizeLevel[expr_], fmt_] :=
	PaneBox[MakeBoxes[expr, fmt], BaselinePosition -> Baseline, FrameMargins -> 0]



formatSubpodUtility["minput content", minput_String] := 
	Block[{button, interactive},
		button = minputButton[
			Style[RawBoxes[minput], "StandardForm", "Input", 12, ShowStringCharacters -> True],
			minput
		];

		interactive = ToExpression[minput, InputForm, HoldComplete];
		interactive = minputToInteractive[interactive];
		If[interactive === None, Return[button]];

		interactive = Column[{
			minputButton[
				Style[RawBoxes[interactive], "StandardForm", "Input", 12, ShowStringCharacters -> True],
				interactive
			],
			ToExpression[interactive]
		}];
		OpenerView[{button, interactive}]
	]


minputButton[label_, inputboxes_] :=
	Button[
		Tooltip[label, "Paste input below"],
		If[CurrentValue["OptionKey"] || CurrentValue["AltKey"],
			NotebookPut[Notebook[{Cell[BoxData[inputboxes], "Input"]}]],
			SelectionMove[ButtonNotebook[], All, ButtonCell];
			SelectionMove[ButtonNotebook[], After, Cell];
			NotebookWrite[ButtonNotebook[], Cell[BoxData[inputboxes], "Input"], All];
			(* SelectionMove[ButtonNotebook[], All, CellContents]; *)
			(* FrontEndTokenExecute[ButtonNotebook[], "HandleShiftReturn"] *)
		],
		DefaultBaseStyle -> {},
		BaseStyle -> {},
		Appearance -> None
	]
	

(*
The toPlainString utility makes sure that literal occurrences of AlphaIntegration`Dump`
or Global` don't appear in the resulting string.
*)

toPlainString[expr_] :=
	StringReplace[ ToString[expr, InputForm], {$Context -> "", "AlphaIntegration`Dump`" -> ""}]


addQ[head_, interface_] := $AlphaQueryInterfaceAddedToMInput[head] === interface


minputToInteractive[HoldComplete[N[expr_, k_]]] /; addQ["N", "Manipulate"] := toPlainString @ 
	Manipulate[N[expr, n], {{n, k}, Floor[k / 2], 10 k, 1, Appearance -> "Labeled"}]

minputToInteractive[HoldComplete[ContinuedFraction[expr_, k_]]] /; addQ["ContinuedFraction", "Manipulate"] := toPlainString @ 
	Manipulate[ContinuedFraction[expr, n], {{n, k}, Floor[k / 2], 10 k, 1, Appearance -> "Labeled"}]

minputToInteractive[HoldComplete[Series[f_, {x_, x0_, k_}, opts___]]] /; addQ["Series", "Manipulate"] := toPlainString @ 
	Manipulate[Series[f, {x, x0, n}], {{n, k}, Floor[k / 2], 5 k, 1, Appearance -> "Labeled"}]

minputToInteractive[HoldComplete[Plot[f_, {x_, xmin_, xmax_}, opts___]]] /; addQ["Plot", "Manipulate"] := toPlainString @ 
	Manipulate[Plot[f, {x, xmin - n, xmax + n}, opts], {n, 0, (xmax - xmin)/2}]

minputToInteractive[HoldComplete[plot:Plot[f_, {x_, xmin_, xmax_}, opts___]]] /; addQ["Plot", "Explore"] := toPlainString @ 
	Experimental`Explore[plot, PlotRange]

minputToInteractive[HoldComplete[Plot3D[f_, {x_, xmin_, xmax_}, {y_, ymin_, ymax_}, opts___]]]  /; addQ["Plot3D", "Manipulate"] := toPlainString @ 
	Manipulate[Plot3D[f, {x, xmin - n, xmax + n}, {y, ymin - n, ymax + n}, opts], {n, 0, (xmax - xmin)/2}]

minputToInteractive[HoldComplete[ContourPlot[f_, {x_, xmin_, xmax_}, {y_, ymin_, ymax_}, opts___]]]  /; addQ["ContourPlot", "Manipulate"] := toPlainString @ 
	Manipulate[ContourPlot[f, {x, xmin - n, xmax + n}, {y, ymin - n, ymax + n}, opts], {n, 0, (xmax - xmin)/2}]

minputToInteractive[other_] := None



formatSubpodUtility["minput link", minput_String] :=
	minputButton[
		Style[Mouseover[
			Style["Use input \[RightGuillemet]", FontColor -> Purple],
			Style["Use input \[RightGuillemet]", FontColor -> Red]], "DialogStyle"],
		minput
	]


$UseInputLength = 40;

formatSubpodUtility["minput right link", minput_String] :=
Block[{label},
	label = If[StringLength[minput] < $UseInputLength+2, minput, StringTake[minput, $UseInputLength-2] <> " \[Ellipsis]"];
	minputButton[
		Style[Mouseover[
			Row[{
				Style[RawBoxes[label], "StandardForm", "Input", FontSize -> (Inherited+1), ShowStringCharacters -> True, Plain, ShowAutoStyles -> False],
				$verticalBar2,
				"Use input \[RightGuillemet]"
				},
				BaseStyle -> {FontColor -> GrayLevel[0.65]}
			],
			Row[{
				Style[RawBoxes[minput], "StandardForm", "Input", FontSize -> (Inherited+1), ShowStringCharacters -> True, Plain, FontColor -> Black],
				$verticalBar2,
				Style["Use input \[RightGuillemet]", FontColor -> Red]
				},
				BaseStyle -> {FontColor -> GrayLevel[0.65]}
			]],
			"DialogStyle"
		],
		minput
	]
]


$ErrorHighlightingOff = AutoStyleOptions -> {
	"HighlightSyntaxErrors" -> False, 
	"HighlightUnknownOptions" -> False, 
	"HighlightExcessArguments" -> False, 
	"HighlightMissingArguments" -> False, 
	"HighlightUnwantedAssignments" -> False, 
	"HighlightSymbolShadowing" -> False
	};



formatSubpodUtility["moutput", moutput_String] :=
	Style[TraditionalForm[ToExpression[moutput] /. HoldComplete -> Defer], "TraditionalForm", "TR", "Output", Larger, ScriptLevel -> 0]


formatSubpodUtility["data link", mdata_List] :=
	With[{data = dataView[mdata]},
		Tooltip[
			Button[
				Style[Mouseover[
					Style["Use data \[RightGuillemet]", FontColor -> Purple],
					Style["Use data \[RightGuillemet]", FontColor -> Red]], "DialogStyle"],
				If[CurrentValue["OptionKey"] || CurrentValue["AltKey"],
					NotebookPut[Notebook[{Cell[BoxData[ToBoxes[data]], "Input"]}]],
					SelectionMove[ButtonNotebook[], All, ButtonCell];
					SelectionMove[ButtonNotebook[], After, Cell];
					NotebookWrite[ButtonNotebook[], Cell[BoxData[ToBoxes[data]], "Input"], All];
				],
				DefaultBaseStyle -> {},
				BaseStyle -> {},
				Appearance -> None
			],
			"Paste data below"
		]
	]

dataName["computabledata"] := "ComputableData"
dataName["formatteddata"] := "FormattedData"
dataName[_] := "Unknown Data"

dataView[mdata:{XMLElement[_, _, {_}]..}] := dataName[First[#]] -> RawBoxes[First[Last[#]]]& /@ mdata







boxSimplify[boxes_, types_, args___] := boxes //. Flatten[boxSimplifyRules[#, args]& /@ Flatten[{types}]]


$BoxSimplifyAll = {
	"AllTemplateBoxDisplayFunctions",
	"AllTagBoxes",
	"AllButtonBoxes",
	"AllStyleBoxes",
	"AllFormBoxes"
}

$BoxSimplifyAutomatic = {
	"RowTemplateBoxes",
	"StripInternalLinks",
	"FixExternalLinks",
	"StringTagBoxes",
	"TagBoxWrappers",
	"IdentityTagBoxes"
}

$BoxSimplifyAutomaticWithLinks = {
	"RowTemplateBoxes",
	"CombineInternalLinks",
	"FixInternalLinks",
	"FixExternalLinks",
	"StringTagBoxes",
	"TagBoxWrappers",
	"IdentityTagBoxes"
}




boxSimplifyRules[None, args___] := {}

boxSimplifyRules[All, args___] := boxSimplifyRules[#, args]& /@ $BoxSimplifyAll

boxSimplifyRules[Automatic, args___] := boxSimplifyRules[#, args]& /@ $BoxSimplifyAutomatic

boxSimplifyRules["AutomaticWithLinks", args___] := boxSimplifyRules[#, args]& /@ $BoxSimplifyAutomaticWithLinks



boxSimplifyRules["AllTemplateBoxDisplayFunctions", ___] :=
	TemplateBox[{args___}, ___, DisplayFunction -> f_, ___] :> Block[{}, f[args] /; True]

boxSimplifyRules["RowTemplateBoxes", ___] :=
	TemplateBox[{args___}, "Row", ___, DisplayFunction -> f_, ___] :> Block[{}, f[args] /; True]

boxSimplifyRules["AllTagBoxes", ___] := {
	TagBox[arg_, tag_String, ___] :> StyleBox[arg, tag],
	TagBox[arg_, ___] :> arg
}

boxSimplifyRules["StringTagBoxes", ___] := TagBox[arg_, tag:Except["ResetImageSizeLevel" | "SkipImageSizeLevel" | "DynamicName", _String], ___] :> StyleBox[arg, tag]

boxSimplifyRules["TagBoxWrappers", ___] := TagBox[arg_, sym_Symbol[___], ___] :> arg /; SymbolName[Unevaluated[sym]] === "TagBoxWrapper"

boxSimplifyRules["IdentityTagBoxes", ___] := TagBox[arg_, Identity | (# &), ___] :> arg

boxSimplifyRules["StripInternalLinks", ___] := 
	(TagBox[ButtonBox[content_, ___] | content_, Annotation[#, s_String, "Hyperlink"]&] /; StringMatchQ[s, "/input/*"]) :> content

boxSimplifyRules["CombineInternalLinks", ___] := {
	(RowBox[list:{TagBox[ButtonBox[_, ___], Annotation[#, s_String, "Hyperlink"]&]..}] /; StringMatchQ[s, "/input/*"]) :> 
		Block[{}, ReplacePart[First[list], {1,1} -> RowBox[#[[1,1]]& /@ list]] /; True],
	(RowBox[list:{TagBox[_, Annotation[#, s_String, "Hyperlink"]&]..}] /; StringMatchQ[s, "/input/*"]) :> 
		Block[{}, ReplacePart[First[list], {1} -> RowBox[#[[1]]& /@ list]] /; True]
}

boxSimplifyRules["FixInternalLinks", args___] := 
	(TagBox[ButtonBox[content_, ___] | content_, Annotation[#, s_String, "Hyperlink"]&] /; StringMatchQ[s, "/input/*"]) :>
			Block[{}, ToBoxes[internalLink[content, parsePartialURL[s], args]] /; True]

boxSimplifyRules["StripExternalLinks", args___] := 
	TagBox[content_, Annotation[#, info_List, "ExternalLink"]&, ___] :> content

boxSimplifyRules["FixExternalLinks", args___] := 
	TagBox[content_, Annotation[#, info_List, "ExternalLink"]&, ___] :>
			Block[{}, ToBoxes[externalLink[content, "URL" /. info]] /; True]

boxSimplifyRules["AllButtonBoxes", ___] := ButtonBox[arg_, ___] :> arg

boxSimplifyRules["AllStyleBoxes", ___] := StyleBox[arg_, ___] :> arg

boxSimplifyRules["AllFormBoxes", ___] := FormBox[arg_, ___] :> arg

boxSimplifyRules[other___] := {}


(* ::Subsection::Closed:: *)
(*sources*)


AllSourceURLs[data_] := DeleteDuplicates[Flatten[sourceURLs /@ data]]

sourceURLs[XMLElement["sources", values_, data_]] :=
	Cases[data, XMLElement["source", {___, "url" -> url_String, ___}, _] :> url]

sourceURLs[other_] := {}



FormatAllSources[data_] := Flatten[formatSources /@ data]

formatSources[XMLElement["sources", values_, data_]] := 
	If[# === {}, {},
		ActionMenu[
			Mouseover[
				Style["Source Information \[RightGuillemet]", "DialogStyle", Gray],
				Style["Source Information \[RightGuillemet]", "DialogStyle", Red]
			],
			#,
			Appearance -> None
		]
	]& @ Map[formatSource, data]

formatSources[other_] := {}

formatSource[XMLElement["source", {___, "url" -> url_String, ___, "text" -> text_, ___}, _]] :=
	With[{newurl = transformSourceURL[url]}, text :> NotebookLocate[{URL[newurl], None}]]

formatSource[other_] := {}


FormatAllSourcesActions[data_] := DeleteDuplicates[Flatten[formatSourcesActions /@ data]]

formatSourcesActions[XMLElement["sources", values_, data_]] := Map[formatSourceAction, data]

formatSourcesActions[other_] := {}

formatSourceAction[XMLElement["source", {___, "url" -> url_String, ___, "text" -> text_, ___}, _]] :=
	With[{newurl = transformSourceURL[url]}, Row[{Dynamic[FEPrivate`FrontEndResource["WAStrings", "SourceInfo"]], ": " <> text}] :> NotebookLocate[{URL[newurl], None}] ]

formatSourceAction[other_] := {}


(*
The API currently points to completely unstyled HTML pages for source information. The
transformSourceURL utility changes the url reported by the API to one which points to a highly
styled page. Ideally, these urls would be reported by the API.
*)

transformSourceURL[url_String] :=
	If[StringMatchQ[url, "http://www.wolframalpha.com/sources/*SourceInformationNotes.html"],
		"http://www.wolframalpha.com/input/sources.jsp?sources=" <> StringTake[url, {37, -28}],
		url
	]


(* ::Subsection::Closed:: *)
(*additional information*)


FormatAdditionalInformation[Dynamic[{boxversion_, {podvarseq___}, {auxvarseq___}, chosen_, open_, elements_, q_, opts_, nonpods_, qinfo_, info_, showpods_, failedpods_, newq_}]] := 
	OpenerView[
		{
			"Additional Information",
			Dynamic[
				outerFrame[
					Column[Flatten[{
						Style["Query: " <> q, Bold],
						Grid[List @@@ Flatten[{opts, Cases[qinfo, _["queryurl", _], Infinity]}], Alignment -> Left],
						If[MatchQ[qinfo, {_List, _List}],
							{
								Style["Query result", Bold],
								Grid[List @@@ DeleteCases[First[qinfo], _[_, ""] | _["queryurl", _]], Alignment -> Left],
								Style["Recalculate result", Bold],
								Grid[List @@@ DeleteCases[Last[qinfo], _[_, ""] | _["queryurl", _]], Alignment -> Left]
							},
							{
								Style["Query result", Bold],
								Grid[List @@@ DeleteCases[qinfo, _[_, ""] | _["queryurl", _]], Alignment -> Left]
							}
						],
						Style["Client information", Bold],
						Grid[{
								{"product", ("ProductIDName" /. $ProductInformation)},
								{"kernel", SystemInformation["Kernel", "ReleaseID"]},
								{"", SystemInformation["Kernel", "Version"]},
								{"front end", SystemInformation["FrontEnd", "ReleaseID"]},
								{"", SystemInformation["FrontEnd", "Version"]},
								{"plugin version", SystemInformation["FrontEnd", "BrowserPlugin"]},
								{"plugin enabled", CurrentValue["PluginEnabled"]},
								{"client", $AlphaQueryClientLocation},
								{"box version", boxversion}
							},
							Alignment -> Left
						],
						Grid[List @@@ FE`Evaluate[FEPrivate`$StartTimes], Alignment -> Left],
						If[ListQ[#], Grid[List @@@ Sort[#], Alignment -> Left], {}]& @ FE`Evaluate[FEPrivate`TaskTimes[]]
						}],
						Spacings -> {1.5,1.5}
					],
					True
				]
			]
		},
		False
	]

			


(* ::Subsection::Closed:: *)
(*ExtrusionEvaluate[]*)


extrusionOpener[content_, nonpods_, Dynamic[q_], Dynamic[opts_], Dynamic[chosen_], Dynamic[open_], Dynamic[newq_]] := 
	Block[{grid, assumptionsQ = MatchQ[nonpods, {___, XMLElement["assumptions", _, _], ___}]},
		grid =
			DynamicModule[{show = False, reinterpret},
				reinterpret = Cases[nonpods, XMLElement["reinterpret", values:{___, "new" -> new_, ___}, _] :> values, Infinity];
				If[reinterpret =!= {}, reinterpret = {"new", "text"} /. First[reinterpret]];
				EventHandler[
					innerFrame[
						Grid[{{
								Dynamic[
									If[MatchQ[reinterpret, {_String, _String}],
										Grid[{{
												Style[queryBlobInputField[Dynamic[newq]], Plain, Gray],
												SpanFromLeft,
												SpanFromLeft
											},
											{
												Tooltip[
													Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "ReinterpretationIndicator"]]],
													Last[reinterpret]
												],
												queryBlobButton[First[reinterpret]],
												suggestionsDialogButton["Reinterpret", q]
											}},
											Alignment -> Left,
											BaselinePosition -> {1,1},
											Spacings -> 0.5
										],
										queryBlobInputField[Dynamic[newq]]
									]
								],
								If[assumptionsQ,
									Button[
										Tooltip[
											Style["\[RightGuillemet]", Orange],
											Dynamic[FEPrivate`FrontEndResource["WAStrings", "InterpretationAssumptions"]]
										],
										Quiet[WolframAlpha[]]; (* trigger autoloading *)
										open = !open;
										opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open],
										Appearance -> None,
										BaselinePosition -> Baseline,
										ContentPadding -> False
									],
									Unevaluated[Sequence[]]
								],
								Item[
									Button[
										PaneSelector[{
											True -> Tooltip[Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "OrangeSquareMinus"]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "HideAllResults"]]],
											False -> PaneSelector[{
												True -> Tooltip[Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "OrangeSquarePlus"]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowAllResults"]]],
												False -> Tooltip[Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "GraySquarePlus"]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowAllResults"]]]
												},
												Dynamic[show],
												ImageSize -> All
											]
											},
											Dynamic[open],
											ImageSize -> All
										],
										Quiet[WolframAlpha[]]; (* trigger autoloading *)
										If[CurrentValue["OptionKey"] || CurrentValue["AltKey"],
											SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
											NotebookWrite[ButtonNotebook[], Cell[newq, "WolframAlphaShort", FormatType -> "TextForm"], All],
											(* else *)
											open = !open;
											opts = updateOptionSetting[opts, Method, "ExtrusionOpen" -> open]
										],
										Appearance -> None
									],
									Alignment -> {Right, Top}
								]
							},
							{
								Dynamic[Quiet[Switch[{chosen[[1]], chosen[[5]]},
									{"Fast parse", "Input" | "Output"},
									queryBlobMathematicaForm[ToExpression[chosen[[6]], InputForm, Defer]],
									{_, "Content"},
									Grid[{{
											Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "PodTitleIndicator"]]],
											Style[podTitle[{"title" -> chosen[[2]]}, Part[chosen, {3, 4, 5}]], "DialogStyle"]
										}},
										Spacings -> 0.3
									],
									{_, "Input" | "Output" | "ComputableData" | "FormattedData" | "FormulaData" | "NumberData" | "QuantityData" | "SoundData" | "TimeSeriesData"},
									Grid[{{
											Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "PodTitleIndicator"]]],
											Style[podTitle[{"title" -> chosen[[2]]}, Part[chosen, {3, 4, 5}]], "DialogStyle"],
											SpanFromLeft
										},
										{
											"",
											queryBlobMathematicaForm[ToExpression[chosen[[6]], InputForm, Defer]],
											" "
										}},
										Alignment -> Left,
										Spacings -> 0.3
									],
									_,
									Style[Row[{"(", Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoSelection"]], ")"}], "DialogStyle", FontColor -> Gray]
								]]],
								If[assumptionsQ, SpanFromLeft, Unevaluated[Sequence[]]],
								SpanFromLeft
							}},
							Alignment -> Left,
							Dividers -> {False, Center},
							FrameStyle -> LightGray,
							BaselinePosition -> {1,1}
						],
						ImageSize -> Automatic,
						BaselinePosition -> Baseline,
						Background -> queryBlobBackground[Dynamic[q], Dynamic[newq]]
					],
					{"MouseEntered" :> FEPrivate`Set[show, True], "MouseExited" :> FEPrivate`Set[show, False]}
				]
			];
			
		PaneSelector[{False -> grid, True -> Column[{grid, Pane[content, $AlphaQueryExtrusionWidth]}, BaselinePosition -> {1,1}]},
			Dynamic[open],
			ImageSize -> Automatic,
			BaseStyle -> {Deployed -> True}
		]
	]



queryBlobInputField[Dynamic[newq_]] := 
		InputField[Dynamic[newq],
			String,
			Appearance -> None,
			BaseStyle -> {"CalculateInput"},
			Enabled -> True,
			ContinuousAction -> True,
			FieldSize -> {{1,40},{1,Infinity}},
			If[TrueQ[BoxForm`sufficientVersionQ[8.0, 1]], System`TrapSelection -> False, Unevaluated[Sequence[]]]
		]

queryBlobButton[query_String] := 
	Tooltip[
		Button[
			Mouseover[query, Style[query, FontColor -> Orange]],
			SelectionMove[ButtonNotebook[], All, ButtonCell];
			NotebookWrite[ButtonNotebook[], Cell[query, "WolframAlphaShort", FormatType -> "TextForm"], All];
			SelectionMove[ButtonNotebook[], After, CellContents],
			Alignment -> Left,
			Appearance -> None,
			BaseStyle -> {"CalculateInput"}
		],
		Dynamic[FEPrivate`FrontEndResource["WAStrings", "RemoveResults"]]
	]


queryBlobBackground[Dynamic[q_], Dynamic[newq_], color_:RGBColor[1., 0.975, 0.95]] := 
	Dynamic[
		FEPrivate`If[
			FEPrivate`And[
				FEPrivate`SameQ[FEPrivate`Head[newq], String],
				FEPrivate`UnsameQ[q, newq]
			],
			color,
			GrayLevel[1]
		]
	]


queryBlobMathematicaForm[Defer[expr_]] := 
	With[{boxes = AlphaQueryMakeBoxes[expr]},
		shortenMathematicaBoxes[
			Tooltip[
				Button[
					Mouseover[
						Style[RawBoxes[boxes], NumberMarks -> False],
						Style[RawBoxes[boxes], NumberMarks -> False, FontColor -> Orange, ShowSyntaxStyles -> False],
						BaseStyle -> {ShowStringCharacters -> True}
					],
					SelectionMove[ButtonNotebook[], All, ButtonCell];
					NotebookWrite[ButtonNotebook[], Cell[BoxData[boxes], "Input"], All];
					SelectionMove[ButtonNotebook[], After, CellContents],
					Alignment -> Left,
					Appearance -> None,
					BaseStyle -> {},
					ImageSize -> Automatic
				],
				Dynamic[FEPrivate`FrontEndResource["WAStrings", "RemoveResults"]]
			] // ToBoxes,
			{}
		]
	]


shortenMathematicaBoxes[boxes_, {basestyleopts___}, opts___] :=
	Pane[
		RawBoxes[boxes],
		opts,
		ImageSize -> {Automatic, {1,100}},
		Scrollbars -> {False, Automatic},
		AppearanceElements -> {},
		BaseStyle -> {basestyleopts, ShowStringCharacters -> True, ScriptLevel -> 0}
	]




AlphaIntegration`ExtrusionEvaluate[query_String, fmt_, opts___] :=
Block[{result, rawparse, parseassumptions, xml, options, previouslyChosen, oldid, monitor, starttime = AbsoluteTime[], conopen=False,
		$AlphaQueryMMode = "input"},

	If[TrueQ[$AlphaQueryExtrusionMonitor],
		monitor[args___] := Print[Style[Row[{AbsoluteTime[]-starttime, ": ", args}], "Message", "MSG"]],
		monitor[args___] := Null
	];

	If[doQuerySideEffects[query], Return[Null]];
	If[StringTrim[query] === "", Return[Null]];
	
	(* If the query contains symbols with OwnValues, ask for permission to send msessinfo *)
	mathematicaSessionInfoPermissionDialogIfNecessary[query, {}];
	
	(* The choice procedure needs to take into account the previous choice, if there was one.*)
	previouslyChosen = "ExtrusionChosen" /. (Method /. {opts} /. Method -> {});
	If[!MatchQ[previouslyChosen, {_, _, _, _, __}], previouslyChosen = None,  oldid = First[previouslyChosen]];
	
	(* RETURNING THE FAST PARSE *)
	(* if there's a fast parse, return it as the input *)
	monitor["Checking fast parse..."];
	rawparse = If[previouslyChosen === None, Quiet[WolframAlpha[query, "RawMathematicaParse"], WolframAlpha::conopen], {"Parse" -> None}];
	result = Replace[rawparse, {{___, "Parse" -> a_String, ___} :> ToExpression[a, InputForm, HoldComplete], _ -> $Failed}];
	If[MatchQ[result, _HoldComplete],
		parseassumptions = MatchQ[rawparse, {___, "Assumptions" -> {__}, ___}];
		replaceEvaluationCell[formatQueryBlob[query, Defer @@ result, "Fast parse", parseassumptions]];
		Return @ fastParseResult[result]
	];
	
	(* otherwise, we'll be examining the full results *)
	monitor["Getting full results..."];
	options = resolveOptions[
		Asynchronous -> All,
		AppearanceElements -> {"Extrusion", "Warnings", "Assumptions", "Pods"},
		Method -> {
			"Formats" -> Flatten[{"cell", "minput", If[$AlphaQueryExtrusionShowMOutputs, "moutput", {}, {}], "msound", "dataformats"}],
			"NewQuery" -> None
		},
		opts
	];
	redirectMessages[
		xml = WolframAlpha[query, "ProcessedXML", options] // expandData,
		WolframAlpha::conopen,
		conopen = True
	];
	If[errorBlobQ[conopen, xml, query, options],
		CellPrint[formatErrorBlob[conopen, xml, query, options]];
		Return[Null]
	];
	
	(* If the xml contains appropriate MInput scanner results, ask for permission to send msessinfo *)
	monitor["Ask permission for msessinfo if necessary..."];
	If[ mathematicaSessionInfoPermissionDialogIfNecessary[query, xml] === True,
		(* If the user gave permission, redo the query *)
			monitor["msessinfo permissions changed. Getting full results again..."];
			xml = WolframAlpha[query, "ProcessedXML", options] // expandData;		
	];
	
	(* RETURNING A RESULT THAT MATCHES A PREVIOUSLY CHOSEN RESULT *)
	(* if the previously chosen podid also exists in the new results, try to return it *)
	monitor["Checking previously chosen podid..."];
	If[previouslyChosen =!= None,
		result = Cases[xml, XMLElement["pod", {___, "id" -> oldid, ___}, _], Infinity];
		If[result =!= {},
			(* load any async pods with that podid *)
			xml = xml /. XMLElement["pod", {___, "id" -> oldid, ___, "async" -> url_, ___}, _] :> expandAsyncPod[url, True];
			result = Cases[xml, XMLElement["pod", {___, "id" -> oldid, ___}, _], Infinity]
		];
		If[result =!= {}, Return @ chooseAndReturnAValue[query, xml, previouslyChosen, options]]
	];
	
	(* RETURNING A RESULT THAT HAS BEEN TAGGED AS A PRIMARY RESULT *)
	monitor["Checking for a primary result tag..."];
	(* load any async pods that look like primary result pods *)
	xml = xml /. XMLElement["pod", {___, "async" -> url_, ___, "primary" -> "true", ___}, _] :> expandAsyncPod[url, True];
	(* if there are still primary result pods, return information from the first one *)
	result = Cases[xml, XMLElement["pod", {___, "title" -> title_String, ___, "id" -> id_String, ___, "primary" -> "true", ___}, _] :> {id, title}, Infinity];
	If[result =!= {}, Return @ chooseAndReturnAValue[query, xml, First @ result, options] ];
		
	(* RETURNING AN MINPUT-SCANNER RESULT *)
	monitor["Checking for MInput Scanner results..."];
	(* load any async pods from the "MInput" scanner *)
	xml = xml /. XMLElement["pod", {___, "scanner" -> "MInput", ___, "async" -> url_, ___}, _] :> expandAsyncPod[url, True];
	(* if there are any pods from the "MInput" Scanner, use the first one *)
	result = Cases[xml, XMLElement["pod", {___, "scanner" -> "MInput", ___}, _], Infinity];
	result = Cases[result, XMLElement["pod", {___, "title" -> title_String, ___, "id" -> id_String, ___}, _] :> {id, title}, Infinity];
	If[result =!= {}, Return @ chooseAndReturnAValue[query, xml, First @ result, options] ];
	
	(* RETURNING A RESULT FROM A RESULT POD *)
	monitor["Checking for a \"result\" pod..."];
	(* load any async pods that look like results pods *)
	xml = xml /. XMLElement["pod", {___, "title" -> (title_String /; StringMatchQ[title, "*Result*"]), ___, "async" -> url_, ___}, _] :> expandAsyncPod[url, True];
	(* if there are results pods, return information from the first one *)
	result = Cases[xml, XMLElement["pod", {___, "title" -> title_String, ___, "id" -> id_String, ___}, _] :> {id, title}, Infinity];
	result = Select[result, StringMatchQ[#[[2]], "*Result*"]&];	
	If[result =!= {}, Return @ chooseAndReturnAValue[query, xml, First @ result, options] ];
	
	(* RETURNING A RESULT FROM SOME OTHER POD *)
	(* otherwise, if there are any pods, focus on the first couple *)
	monitor["No results pods found. Trying other pods..."];
	result = Cases[xml, XMLElement["pod", {___, "title" -> title_String, ___, "id" -> id_String, ___}, _] :> {id, title}, Infinity];
	result = Take[result, Min[2, Length[result]]];
	If[MatchQ[result, {__List}],
		xml = xml /. XMLElement["pod", {___, "id" -> (Alternatives @@ First /@ result), ___, "async" -> url_, ___}, _] :> expandAsyncPod[url, True];
		result = Cases[xml, XMLElement["pod", {___, "title" -> title_, ___, "id" -> id:(Alternatives @@ First /@ result), ___}, _] :> {id, title}, Infinity];
		result = Take[result, Min[2, Length[result]]];
		If[MatchQ[result, {__List}], Return @ chooseAndReturnAValue[query, xml, Last @ result, options] ];
		(* in the rare instance that there are any pods left, load the remaining async content before proceeding *)
		xml = xml /. XMLElement["pod", {___, "async" -> url_, ___}, _] :> expandAsyncPod[url, True];
		result = Cases[xml, XMLElement["pod", {___, "title" -> title_String, ___, "id" -> id_String, ___}, _] :> {id, title}, Infinity];
		If[result =!= {}, Return @ chooseAndReturnAValue[query, xml, First @ result, options] ];
	];
	
	(* RETURNING NOTHING *)
	(* otherwise, there's nothing to do *)
	monitor["No pods found."];
	replaceEvaluationCell[formatQueryBlob[query, None, Row[{"(", Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoInterpretations"]], ")"}]]];
	Return[Null];
]

(*
When the first argument to ExtrusionEvaluate is boxes rather than a string, check to see
whether it contains a known result structure. If it does, those structures have untypeset
rules that take care of interpretation, so just call MakeExpression.

If they don't -- that is, if a user has somehow gotten a "WolframAlphaShort" cell that
contains boxes which aren't W|A result boxes -- then try to intepret those boxes as a
query string, using the same mechanism that AlphaIntegration`AlphaQuery uses.
*)

AlphaIntegration`ExtrusionEvaluate[boxes_, fmt:(StandardForm | TraditionalForm)] := 
Block[{heldexpr, $AlphaQueryMMode = "input"},
	heldexpr = If[
		!FreeQ[boxes, NamespaceBox["WolframAlphaQueryResults" | "WolframAlphaQueryNoResults" | "WolframAlphaQueryParseResults", ___]],
		MakeExpression[boxes, fmt],
		With[{query = queryBoxesToQueryString[boxes, fmt]},
			HoldComplete[AlphaIntegration`ExtrusionEvaluate[query, InputForm]]
		]
	];
	If[MatchQ[heldexpr, _HoldComplete],
		ReleaseHold[heldexpr],
		Null
	]
]

AlphaIntegration`ExtrusionEvaluate[boxes_, fmt_] := Null (* otherwise, do nothing *)




chooseAndReturnAValue[query_, xml_, {id_, title_}, opts___] :=
	chooseAndReturnAValue[query, xml, {id, title, Automatic, Automatic, Automatic, Automatic}, opts]


chooseAndReturnAValue[query_, xml_, {id_, title_, subpodid_, numberOfSubpods_, type_}, opts___] := 
	chooseAndReturnAValue[query, xml, {id, title, subpodid, numberOfSubpods, type, Automatic}, opts]


chooseAndReturnAValue[query_, xml_, {podid_, title_, subpodid_, numberOfSubpods_, type_, mform_}, opts___] := 
Block[{podxml, subpodxml, primarysubpods, c, n, minputs, moutputs, returntype, str},
	podxml = First[Cases[xml, XMLElement["pod", {___, "id" -> podid, ___}, _], Infinity]];
	subpodxml = Cases[podxml, XMLElement["subpod", _, _], Infinity];
	n = Length[subpodxml];
	c = Which[
		IntegerQ[subpodid] && 1 <= subpodid <= n, subpodid,
		(* If subpodid is Automatic, look for a primary result subpod *)
		(primarysubpods = First /@ Position[subpodxml, XMLElement["subpod", {___, "primary" -> "true", ___}, _]]) =!= {}, First[primarysubpods],
		True, 1
	];
	subpodxml = Part[subpodxml, c];
	
	minputs = Cases[subpodxml, XMLElement["minput", _, {minput_String}] :> minput, Infinity];
	moutputs = Cases[subpodxml, XMLElement["moutput", _, {moutput_String}] :> moutput, Infinity];
	
	returntype = Which[
		(* if the requested type is available, choose it *)
		type === "Content", "Content",
		type === "Input" && minputs =!= {}, "Input",
		type === "Output" && moutputs =!= {}, "Output",
		MemberQ[$AlphaQueryDataFormats, type], type,
		(* otherwise, the requested type is not available, so we have to pick one *)
		(* use minput or moutput if they exist, and plain content otherwise *)
		minputs =!= {}, "Input",
		moutputs =!= {}, "Output",
		True, "Content"
	];
	
	(* Now that the return type has been chosen, time to actually return it *)
	Switch[returntype,
		"Input",
		replaceEvaluationCell[formatQueryBlob[query, xml, {podid, title, c, n, "Input", First @ minputs}, opts]];
		Return[minputResult[First @ minputs]],
		"Output",
		replaceEvaluationCell[formatQueryBlob[query, xml, {podid, title, c, n, "Output", First @ moutputs}, opts]];
		Return[minputResult[First @ moutputs]],
		"Content",
		replaceEvaluationCell[formatQueryBlob[query, xml, {podid, title, c, n, "Content"}, opts]];
		Return[WolframAlphaResult[query, {{podid, c}, "Content"}]],
		_ (* data exposure *),
		str = With[{c=c, returntype=returntype}, ToString[Unevaluated[WolframAlpha[query, {{podid, c}, returntype}]], InputForm]];
		replaceEvaluationCell[formatQueryBlob[query, xml, {podid, title, c, n, returntype, str}, opts]];
		Return[ToExpression[str]]
	]
];


replaceEvaluationCell[expr_] := LinkWrite[$ParentLink, FrontEnd`RewriteExpressionPacket[queryBlobCell[expr]]]


extrudeFromFastParse[query_String, Defer[expr_], label_String] :=
Block[{results},
	results =
		WolframAlpha[query,
			Asynchronous -> All,
			AppearanceElements -> {"Extrusion", "Warnings", "Assumptions", "Pods", "Unsuccessful"},
			Method -> {
				"ExtrusionChosen" -> {label, label, 1, 0, "Input", ToString[Unevaluated[expr], InputForm]},
				"ExtrusionOpen" -> True,
				"Formats" -> Flatten[{"cell", "minput", If[$AlphaQueryExtrusionShowMOutputs, "moutput", {}, {}], "msound", "dataformats"}] }
			];
	If[results =!= $Failed,
		SelectionMove[ButtonNotebook[], All, ButtonCell];
		NotebookWrite[ButtonNotebook[], queryBlobCell[results], All];
	];
]


queryBlobCell[a_Cell] := a;


queryBlobCell[expr_] := Cell[BoxData[ToBoxes[expr]], "WolframAlphaShortInput"]


formatQueryBlob[query_String, None, label_] := 
	Block[{grid},
		grid[Dynamic[q_], Dynamic[newq_]] := innerFrame[
			Grid[{{
					queryBlobInputField[Dynamic[newq]],
					SpanFromLeft
				},
				{
					Style[label, "DialogStyle", FontColor -> Gray],
					suggestionsDialogButton["NoResults", query]
				}},
				Alignment -> Left,
				Dividers -> {False, Center},
				FrameStyle -> LightGray,
				BaselinePosition -> {1,1}
			],
			RoundingRadius -> 5,
			FrameStyle -> LightGray,
			BaselinePosition -> Baseline,
			ImageSize -> Automatic,
			Background -> queryBlobBackground[Dynamic[q], Dynamic[newq]]
		];

		System`DynamicNamespace[
			"WolframAlphaQueryNoResults",
			DynamicModule[{Typeset`q = query, Typeset`newq = query},
				grid[Dynamic[Typeset`q], Dynamic[Typeset`newq]]
			],
			BaseStyle -> {Deployed -> True},
			Editable -> False,
			DeleteWithContents -> True,
			SelectWithContents -> True
		]
	]

formatQueryBlob[query_String, Defer[expr_], label_String, parseassumptions_] :=
	Block[{grid},
		grid[Dynamic[q_], Dynamic[newq_], Dynamic[open_]] := 
			DynamicModule[{show = False, assumptionsQ = TrueQ[parseassumptions]},
				EventHandler[
					innerFrame[
						Grid[{{
								queryBlobInputField[Dynamic[newq]],
								If[assumptionsQ,
									Button[
										Tooltip[
											Style["\[RightGuillemet]", Orange],
											Dynamic[FEPrivate`FrontEndResource["WAStrings", "InterpretationAssumptions"]]
										],
										Quiet[WolframAlpha[]]; (* trigger autoloading *)
										open = True;
										extrudeFromFastParse[query, Defer[expr], label];
										open = False,
										Appearance -> None,
										Method -> "Queued",
										BaselinePosition -> Baseline,
										ContentPadding -> False
									],
									Unevaluated[Sequence[]]
								],
								Item[
									Button[
										PaneSelector[{
											True -> Tooltip[Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "OrangeSquarePlus"]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowAllResults"]]],
											False -> Tooltip[Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "GraySquarePlus"]]], Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowAllResults"]]]
											},
											Dynamic[show],
											ImageSize -> All
										],
										Quiet[WolframAlpha[]]; (* trigger autoloading *)
										If[CurrentValue["OptionKey"] || CurrentValue["AltKey"],
											SelectionMove[ButtonNotebook[], All, ButtonCell, AutoScroll -> False];
											NotebookWrite[ButtonNotebook[], Cell[newq, "WolframAlphaShort", FormatType -> "TextForm"], All],
											(* else *)
											open = True;
											extrudeFromFastParse[query, Defer[expr], label];
											open = False
										],
										Appearance -> None,
										Method -> "Queued"
									],
									Alignment -> {Right, Top}
								]
							},
							{
								queryBlobMathematicaForm[Defer[expr]],
								If[assumptionsQ, SpanFromLeft, Unevaluated[Sequence[]]],
								SpanFromLeft
							}},
							Alignment -> Left,
							Dividers -> {False, Center},
							FrameStyle -> LightGray,
							BaselinePosition -> {1,1}
						],
						RoundingRadius -> 5,
						FrameStyle -> LightGray,
						BaselinePosition -> Baseline,
						ImageSize -> Automatic,
						Background -> queryBlobBackground[Dynamic[q], Dynamic[newq]]
					],
					{"MouseEntered" :> FEPrivate`Set[show, True], "MouseExited" :> FEPrivate`Set[show, False]}
				]
			];

		System`DynamicNamespace[
			"WolframAlphaQueryParseResults",
			DynamicModule[{Typeset`q = query, Typeset`newq = query, Typeset`chosen = ToString[Unevaluated[expr], InputForm], Typeset`open = False},
				PaneSelector[{
						False -> grid[Dynamic[Typeset`q], Dynamic[Typeset`newq], Dynamic[Typeset`open]],
						True -> Column[{grid[Dynamic[Typeset`q], Dynamic[Typeset`newq], Dynamic[Typeset`open]], necklaceAnimator[20]}, BaselinePosition -> {1,1}]
					},
					Dynamic[TrueQ[Typeset`open]], (* Dynamic made purposely complex so it will update during queued evaluations *)
					ImageSize -> Automatic,
					BaseStyle -> {Deployed -> True}
				]
			],
			BaseStyle -> {Deployed -> True},
			Editable -> False,
			DeleteWithContents -> True,
			SelectWithContents -> True
		]
	]


formatQueryBlob[query_String, xml_, chosen_, opts___] :=
	formatAlphaXML[xml, query, Method -> {"ExtrusionChosen" -> chosen}, opts]


(*
The fastParseResult utility might someday be expanded to heuristically either evaluate
locally or not, depending on what kind of things are in expr.
*)
fastParseResult[HoldComplete[expr_]] := If[FreeQ[HoldComplete[expr], _Placeholder], expr, Null]


(*
This input may return literal references to % or Placeholder, when appropriate
*)
minputInput[str_] := 
	Apply[
		Defer,
		ToExpression[str, InputForm, HoldComplete] /. {
			Hold[a_] :> a,
			HoldForm[a_] :> a,
			Defer[a_] :> a
		}
	]

minputResult[str_] := First[minputInput[str] // suppressPlaceholderMInputs]


WolframAlphaResult /:
MakeBoxes[ WolframAlphaResult[q_, {{podid_, subpodid_}, type_}, opts___], fmt_] :=
	ToBoxes[
		Interpretation[
			Framed[
				WolframAlpha[q, {{podid, subpodid}, type}, opts],
				FrameStyle -> LightGray,
				RoundingRadius -> 5,
				FrameMargins -> 10,
				BaseStyle -> {Plain}
			],
			WolframAlphaResult[q, {{podid, subpodid}, type}, opts]
		],
		fmt
	]


(* ::Subsection::Closed:: *)
(*encoding*)


(*
urlencode[str_] := ExternalService`EncodeString[str]

urldecode[str_] := ExternalService`DecodeString[str]

ExternalService`DecodeString[] doesn't properly decode all characters,
like the '+' character. Thus, we universally rely on the more robust
JLink versions, below.
*)


urlencode[str_] := (
	Needs["JLink`"];
	Symbol["JLink`InstallJava"][];
	Symbol["JLink`LoadJavaClass"]["java.net.URLEncoder"];
	Symbol["java`net`URLEncoder`encode"][str, "UTF-8"]
)

urldecode[str_] := (
	Needs["JLink`"];
	Symbol["JLink`InstallJava"][];
	Symbol["JLink`LoadJavaClass"]["java.net.URLDecoder"];
	Symbol["java`net`URLDecoder`decode"][str, "UTF-8"]
)


(* ::Subsection::Closed:: *)
(*attaching to '='*)


(* 
GetFEKernelInit.tr sets up the bindings for
FrontEnd`Private`EvaluationModeEvaluate[_, _, "WolframAlphaShort"], aka Single-Equal, and
FrontEnd`Private`EvaluationModeEvaluate[_, _, "WolframAlphaLong"], aka Double-Equal.
*)



(* ::Subsection::Closed:: *)
(*Interpretation rules*)


(* What about version numbers ??? *)


Unprotect[NamespaceBox]; (* Autoload code has a tendency to reset attributes during package loading *)


NamespaceBox /: MakeExpression[NamespaceBox["WolframAlphaQueryNoResults", boxes_, ___?OptionQ], fmt_] := 
Block[{expr, q, newq},
	expr = MakeExpression[boxes, fmt];
	If[!MatchQ[expr, HoldComplete[DynamicModule[{___}, __]]], Return[expr]];
	
	expr = Extract[expr, {1, 1}, HoldComplete];
	q = Replace[expr, {HoldComplete[{___, _[Typeset`q, q_String], ___}] :> q, _ -> $Failed}];
	newq = Replace[expr, {HoldComplete[{___, _[Typeset`newq, newq_String], ___}] :> newq, _ -> $Failed}];
	
	Switch[StringQ[newq] && newq =!= q,
		True, returnExtrusionEvaluate[newq, {}, {}],
		_, HoldComplete[Null]
	]
]


NamespaceBox /: MakeExpression[NamespaceBox["WolframAlphaQueryParseResults", boxes_, ___?OptionQ], fmt_] :=
Block[{expr, q, newq},
	expr = MakeExpression[boxes, fmt];
	If[!MatchQ[expr, HoldComplete[DynamicModule[{___}, __]]], Return[expr]];
	
	expr = Extract[expr, {1, 1}, HoldComplete];
	q = Replace[expr, {HoldComplete[{___, _[Typeset`q, q_String], ___}] :> q, _ -> $Failed}];
	newq = Replace[expr, {HoldComplete[{___, _[Typeset`newq, newq_String], ___}] :> newq, _ -> $Failed}];
	chosen = Replace[expr, {HoldComplete[{___, _[Typeset`chosen, chosen_String], ___}] :> chosen, _ -> $Failed}];
	
	Switch[{StringQ[newq] && newq =!= q, chosen},
		{True, _}, returnExtrusionEvaluate[newq, {}, {}],
		{_, mform_String}, ToExpression[chosen, InputForm, HoldComplete],
		{_, _}, HoldComplete[Null]
	]
]


NamespaceBox /: MakeExpression[NamespaceBox["WolframAlphaQueryResults", boxes_, ___?OptionQ], fmt_] :=
Block[{expr, q, opts, chosen, newq},
	expr = MakeExpression[boxes, fmt];
	If[!MatchQ[expr, HoldComplete[DynamicModule[{___}, __]]], Return[expr]];
	
	expr = Extract[expr, {1, 1}, HoldComplete];
	q = Replace[expr, {HoldComplete[{___, _[Typeset`q, q_String], ___}] :> q, _ -> $Failed}];
	opts = Replace[expr, {HoldComplete[{___, _[Typeset`opts, opts_List], ___}] :> opts, _ -> $Failed}];
	chosen = Replace[expr, {HoldComplete[{___, _[Typeset`chosen, chosen_List], ___}] :> chosen, _ -> $Failed}];
	newq = Replace[expr, {HoldComplete[{___, _[Typeset`newq, newq_String], ___}] :> newq, _ -> $Failed}];
	
	Switch[{StringQ[newq] && newq =!= q, chosen},
		{True, _}, returnExtrusionEvaluate[newq, opts, chosen],
		{_, {podid_, title_, c_, n_, "Input" | "Output", mform_String}}, returnMathematicaForm[q, opts, chosen],
		{_, {podid_, title_, c_, n_, dataexposure_, mform_String}}, ToExpression[chosen[[6]], InputForm, HoldComplete],
		{_, {podid_, title_, c_, n_, "Content"}}, returnSingleResult[q, opts, chosen],
		{_, {} | _}, returnWolframAlphaExpression[q, opts]
	]
]



(*
The contract stipulates: Any input that contains a Placeholder expression is deemed unfit for
evaluation, and so should be suppressed so that it doesn't trigger gratuitous error messages.
*)
suppressPlaceholderMInputs[heldexpr_] := heldexpr /; FreeQ[heldexpr, _Placeholder]
suppressPlaceholderMInputs[other_] := HoldComplete[Null]


returnMathematicaForm[q_, opts_, chosen_] /; $AlphaQueryExtrusionEvaluateCachedMForms :=
	ToExpression[chosen[[6]], InputForm, HoldComplete] // suppressPlaceholderMInputs


returnMathematicaForm[q_, opts_, chosen_] := 
	With[{options = Sequence @@ Cases[opts, _[InputAssumptions | PodStates | Method, _]]},
		WolframAlpha[q, chosen[[{1,3,5}]], options] // suppressPlaceholderMInputs
	]


returnExtrusionEvaluate[q_, opts_, chosen_] :=
	With[{options = Sequence @@ Cases[opts, _[InputAssumptions | PodStates | Method, _]]},
		HoldComplete[AlphaIntegration`ExtrusionEvaluate[q, InputForm, options]]
	]


returnSingleResult[q_, opts_, chosen:{podid_, title_, subpodid_, n_, type_, ___}] :=
	With[{options = Sequence @@ Cases[opts, _[InputAssumptions | PodStates, _]]},
		HoldComplete[WolframAlphaResult[q, {{podid, subpodid}, type}, options]]
	]


returnWolframAlphaExpression[q_, {opts___}] := HoldComplete[WolframAlpha[q, opts]]






(* ::Subsection::Closed:: *)
(*LinguisticAssistant[], aka Control-Equal*)


(*
When the argument is a notebook object, we decide whether to insert a 
new linguistic assistant at the insertion point.
*)


AlphaIntegration`LinguisticAssistant[nb_NotebookObject] := 
Block[{info},
	info = MathLink`CallFrontEnd[FrontEnd`CellInformation[nb]];
	(* If there are too many cells in the selection, don't do anything *)
	If[Length[info] > 1,
		Message[WolframAlpha::noiffi];
		Return @ $Failed
	];
	(* If the insertion point is horizontal, insert a typeset input cell and go into Alpha mode *)
	If[Length[info] === 0,
		NotebookWrite[nb, Cell[BoxData[""], "Input"], All];
		SelectionMove[nb, Before, CellContents];
		Return @ doEnterLinguisticAssistant[nb]
	];
	
	info = First[info];

	(* If we're already in a Linguistic Assistant, do nothing, silently *)
	If[
		!FreeQ[MathLink`CallFrontEnd[FrontEnd`GetBoxIDs[nb]], "LinguisticAssistantID"],
		Return @ Null
	];

	(* If we're inside a typeset cell and nothing is selected, enter a new Linguistic Assistant *)
	If[
		MemberQ[info, "ContentData" -> BoxData] &&
		MemberQ[info, "Formatted" -> True] &&
		MemberQ[info, "CursorPosition" -> {n_, n_}],
		Return @ doEnterLinguisticAssistant[nb]
	];

	(* Otherwise, the selection is not supported *)
	Message[WolframAlpha::noiffi];
	(* Beep[] *)
]


doEnterLinguisticAssistant[nb_] := (
	NotebookWrite[nb, ToBoxes[AlphaIntegration`LinguisticAssistant["", {}]], After];
	FrontEndTokenExecute[nb, "Tab"];
)


AlphaIntegration`LinguisticAssistant[str_String, chosenAssumptions:{___String}] :=
	System`DynamicNamespace[
		"LinguisticAssistant",
		DynamicModule[{query = str, boxes = "None", allassumptions = {}, assumptions = chosenAssumptions, open = {1}},
			doNewFastParse[query, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open]];
			Dynamic[
				AlphaIntegration`LinguisticAssistantBoxes[str, 1, Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open]]
			]

		],
		BaseStyle -> {Deployed -> True},
		Editable -> False,
		DeleteWithContents -> True,
		SelectWithContents -> True
	]



AlphaIntegration`LinguisticAssistantBoxes[str_String, version:1, Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_]] :=
	Overlay[
		{
			Framed[
				Which[
					open === {1} && boxes === "None" && MatchQ[allassumptions, {} | $Failed],
					linguisticTopInputField[Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open]],
			
					open === {1},
					Grid[{{
						linguisticTopInputField[Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open]],
						linguisticAssumptions[Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open]],
						linguisticOpener["Top", Dynamic[open]]
					}}, Spacings -> {{0, 0.5, 1}, 1}],
			
					open === {2},
					Grid[{{
						linguisticBottomInputField[Dynamic[boxes]],
						linguisticOpener["Bottom", Dynamic[open]]
					}}, Spacings -> {{0, 0.5, 1}, 1}],
			
					open === {1, 2},
					Grid[{
						{
							linguisticTopInputField[Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open]],
							linguisticAssumptions[Dynamic[query], Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open]],
							linguisticCloser["Top", Dynamic[open]]
			
						},
						{
							If[boxes === "None",
								(* Beep[]; *)
								Style[
									Row[{
										"(",
										If[allassumptions === {},
											Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoTranslations"]],
											Dynamic[FEPrivate`FrontEndResource["WAStrings", "NoTranslation"]]
										],
										")",
										"  ",
										suggestionsDialogButton["NoTranslations", query, "MathematicaParse"]	
									}],
									"DialogStyle",
									Gray
								],
								linguisticBottomInputField[Dynamic[boxes]]
							],
							SpanFromLeft,
							linguisticCloser["Bottom", Dynamic[open]]
						}},
						Dividers -> {None, Center},
						FrameStyle -> LightGray,
						Alignment -> {Left, Baseline},
						Spacings -> {{0, 0.5, 1}, 1}
					]
				],
				FrameStyle -> LightGray,
				RoundingRadius -> 5,
				ImageMargins -> {{5, 0}, {0, 0}},
				FrameMargins -> {{6, 4}, {4, 4}}
			],
			Dynamic[RawBoxes[FEPrivate`FrontEndResource["WABitmaps", "EqualSmall"]]]
		},
		{1, 2},
		1,
		Alignment -> {Left, Center}
	]


linguisticAssumptions[Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_]] := 
	RawBoxes[""] /; allassumptions === {}


linguisticAssumptions[Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_]] :=
	Tooltip[
		Button[
			Mouseover[Style["\[RightGuillemet]", Orange], Style["\[RightGuillemet]", Red]],
			Quiet[WolframAlpha[]]; (* trigger autoloading *)
			CreateDocument[{
					TextCell["", CellMargins -> 0],
					ExpressionCell[
						DynamicModule[{},
							(* replace the button- and actionmenu-function with the LinguisticAssistant version *)
							Column[Flatten[FormatAllAssumptions["Assumptions", assumptionsListToXML[allassumptions], Dynamic[query], Dynamic[query], Dynamic[{}]]]] /.
								HoldPattern[updateWithAssumptions][nb_, assumptioninputs_, Dynamic[q_], Dynamic[opts_]] :> 
									updateWithAssumptionsLinguisticAssistant[query, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open], assumptioninputs],
							InheritScope -> True
						],
						NotebookDefault,
						"DialogStyle", 
						Deployed -> True,
						ShowSelection -> True,
						CellMargins -> {{10, 10}, {0, 0}}
					],
					TextCell["", CellMargins -> 0]
				},
				WindowFrame -> "Palette",
				WindowSize -> {600, FitAll},
				WindowTitle -> (cachedFrontEndResource["WAStrings", "AssumptionsFor"] <> ": " <> query),
				WindowElements -> {},
				WindowFrameElements -> {"CloseBox"},
				WindowFloating -> True,
				ShowCellBracket -> False,
				Background -> Lighter[Gray, 0.9],
				ShowSelection -> False,
				Deployed -> True,
				Saveable -> False,
				WindowMargins -> {{#1, Automatic}, {Automatic, #2}} & @@ MousePosition[]
			],
			Appearance -> None,
			BaselinePosition -> (Axis -> Axis)
		],
		Dynamic[FEPrivate`FrontEndResource["WAStrings", "InterpretationAssumptions"]]
	]
 

assumptionsListToXML[a_] := If[# === {}, {}, {XMLElement["assumptions", {"count" -> ToString[Length[#]]}, #]}]& @ assumptionsListToXMLElements[a]

assumptionsListToXMLElements[a_List] := Flatten[assumptionListToXMLElement /@ a]

assumptionListToXMLElement[a_List] := XMLElement["assumption", Cases[a, _[_String, _String]], valueListToXMLElements["Values" /. a]]

valueListToXMLElements[a_List] := Flatten[valueListToXMLElement /@ a]

valueListToXMLElement[a:{__Rule}] := XMLElement["value", a, {}]

valueListToXMLElement[other_] := {}


updateWithAssumptionsLinguisticAssistant[query_, Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_], assumptioninputs:{__}] := 
(
	assumptions = removeDuplicateAssumptions[assumptions, assumptioninputs];
	doNewFastParse[query, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open]];
	NotebookClose[ButtonNotebook[]];
)


linguisticOpener[location_, Dynamic[open_]] :=
	Tooltip[
		Button[
			Mouseover[
				Style["\[FilledDiamond]", Smaller, Orange],
				Style["\[FilledDiamond]", Smaller, Red],
				ContentPadding -> False
			],
			open = {1,2},
			Appearance -> None,
			ContentPadding -> False,
			BaseStyle -> {}
		],
		If[location === "Top",
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowMathematicaForm"]],
			Dynamic[FEPrivate`FrontEndResource["WAStrings", "ShowLinguisticForm"]]
		]
	]


linguisticCloser[location_, Dynamic[open_]] := 
	Block[{label, labelhot},
		{label, labelhot} = If[location === "Top",
			Style["\[FilledDownTriangle]", Smaller, #]& /@ {Orange, Red},
			Style["\[FilledUpTriangle]", Smaller, #]& /@ {Orange, Red}
		];
		Item[
			Tooltip[
				Button[
					Mouseover[
						label,
						labelhot,
						ContentPadding -> False
					],
					Switch[location,
						"Top", open = {2},
						"Bottom" | _, open = {1}
					],
					Appearance -> None,
					ContentPadding -> False,
					BaseStyle -> {},
					Alignment -> If[location === "Top", Bottom, Top]
				],
				If[location === "Top",
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "HideLinguisticForm"]],
					Dynamic[FEPrivate`FrontEndResource["WAStrings", "HideMathematicaForm"]]
				]
			],
			Alignment -> If[location === "Top", Bottom, Top]
		]
	]



NamespaceBox /: MakeExpression[NamespaceBox["LinguisticAssistant", theboxes_, ___?OptionQ], fmt_] :=
		(* If the boxes variable contains a box expression, use it as the interpretation *)
		Switch[
			theboxes,
			DynamicModuleBox[{_[_,query_], _[_,boxes_], ___}, ___],
			If[theboxes[[1,2,2]] === "None", $Failed, MakeExpression[ theboxes[[1,2,2]] ]],
			_,
			HoldComplete[$Failed]
		]


(* Fix the ordering of NamespaceBox rules, which occasionally puts the new rules after the generic ones. *)
FormatValues[NamespaceBox] = With[{formatValues = FormatValues[NamespaceBox]}, Join[
	Select[formatValues, !FreeQ[#, "WolframAlphaQueryNoResults" | "WolframAlphaQueryParseResults" | "WolframAlphaQueryResults" | "LinguisticAssistant"]&],
	Select[formatValues,  FreeQ[#, "WolframAlphaQueryNoResults" | "WolframAlphaQueryParseResults" | "WolframAlphaQueryResults" | "LinguisticAssistant"]&]
]]


linguisticTopInputField[Dynamic[query_], Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_]] := 
	EventHandler[
		InputField[
			Dynamic[query, (query = #; doNewFastParse[query, Dynamic[boxes], Dynamic[allassumptions], Dynamic[assumptions], Dynamic[open]])&],
			String,
			Appearance -> None,
			BaseStyle -> {"CalculateInput"},
			BoxID :> "LinguisticAssistantID",
			FieldSize -> {{1,40},{1,Infinity}},
			If[TrueQ[BoxForm`sufficientVersionQ[8.0, 1]], System`TrapSelection -> False, Unevaluated[Sequence[]]]
		],
		{ "ReturnKeyDown" :> 
			(
				MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[
					FE`BoxReference[MathLink`CallFrontEnd[FrontEnd`Value[FEPrivate`Self[]]], {FE`Parent["LinguisticAssistant"]}],
					AutoScroll -> True
				]];
				FrontEndExecute[FrontEnd`FrontEndToken["MoveNext"]];
			)
		}
	]


(* This approach would be preferable, if we could manage the insertion point better. *)
(*
linguisticBottomInputField[Dynamic[boxes_]] :=
	InputField[
		Dynamic[boxes,
			MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[
				FE`BoxReference[MathLink`CallFrontEnd[FrontEnd`Value[FEPrivate`Self[]]], {FE`Parent["LinguisticAssistant"]}],
				#,
				AutoScroll -> True
			]]&
		],
		Boxes,
		Appearance -> None,
		BaseStyle -> {ScriptLevel -> 0, ShowStringCharacters -> True},
		ContinuousAction -> True,
		Enabled -> True,
		FieldSize -> {{1,40},{1,Infinity}}
	]
*)


linguisticBottomInputField[Dynamic[boxes_]] :=
	Button[
		Mouseover[RawBoxes[boxes], Style[RawBoxes[boxes], Orange, ShowSyntaxStyles -> False], BaseStyle -> {ShowStringCharacters -> True}], 
		MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[
			FE`BoxReference[MathLink`CallFrontEnd[FrontEnd`Value[FEPrivate`Self[]]], {FE`Parent["LinguisticAssistant"]}],
			boxes,
			AutoScroll -> True
		]],
		Appearance -> None,
		BaseStyle -> {ScriptLevel -> 0, ShowStringCharacters -> True}
	]


(* An empty query should clear everything *)
doNewFastParse["", Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_]] :=
	(
		boxes = "None";
		allassumptions = {};
		assumptions = {};
		open = {1};
	)

(* The general case calls mparse.jsp, followed by query.jsp if necessary *)
doNewFastParse[query_, Dynamic[boxes_], Dynamic[allassumptions_], Dynamic[assumptions_], Dynamic[open_]] :=
	Block[{rawparse, parse, newassumptions, $AlphaQueryMMode = "inline"},
		rawparse = TimeConstrained[WolframAlpha[query, "RawMathematicaParse", InputAssumptions -> assumptions], 3, $TimedOut];
		Switch[rawparse,
			{__Rule},
			parse = "Parse" /. rawparse /. "Parse" -> None;
			newassumptions = "Assumptions" /. rawparse /. {"$Failed" -> {}, $Failed -> {}, "Assumptions" -> {}},
			_,
			parse = None;
			newassumptions = {}
		];
		
		If[parse === None,
			rawparse = TimeConstrained[WolframAlpha[query, "PrimaryMathematicaString", InputAssumptions -> assumptions], 2, $TimedOut];
			Switch[rawparse,
				_String,
				parse = rawparse;
				newassumptions = {},
				_,
				parse = None;
				newassumptions = {}
			]
		];
		
		If[parse === None, boxes = "None", boxes = ToExpression[parse, InputForm, AlphaQueryMakeBoxes]];
		open = {1,2};
		allassumptions = DeleteCases[newassumptions, {___, "type" -> (Alternatives @@ $FormulaAssumptionTypes), ___}];
		assumptions = Select[assumptions, !FreeQ[allassumptions, #]&]
	]


(* ::Subsection::Closed:: *)
(*WolframAlphaDate*)


Options[WolframAlphaDate] = {
	TimeZone -> Automatic };


WolframAlphaDate /: MakeBoxes[WolframAlphaDate[date_List, opts___], fmt_] /; (1 <= Length[Unevaluated[date]] <= 6) :=
With[{boxes = ToBoxes[Framed[
		Switch[Length[Unevaluated[date]],
			1, DateString[date, "Year"],
			2, DateString[date, {"MonthNameShort", " ", "Year"}],
			3, DateString[date, "DateShort"],
			4, DateString[date, {"DateShort", " ", "Hour12Short", " ", "AMPMLowerCase"}],
			5, DateString[date, {"DateShort", " ", "Hour12Short", ":", "Minute", " ", "AMPMLowerCase"}],
			6, DateString[date, {"DateShort", " ", "Hour12Short", ":", "Minute", ":", "Second", " ", "AMPMLowerCase"}] ],
		RoundingRadius -> 4,
		FrameStyle -> RGBColor[1, 0.9, 0.8],
		FrameMargins -> 3], fmt]},
	InterpretationBox[boxes, WolframAlphaDate[date, opts], BaseStyle -> {FontFamily -> "Helvetica", ShowStringCharacters -> False}] ]	



(* ::Subsection::Closed:: *)
(*WolframAlphaQuantity*)


Options[WolframAlphaQuantity] = {
	Appearance -> Automatic,
	"Accuracy" -> Automatic,
	"Precision" -> Automatic };


WolframAlphaQuantity /: MakeBoxes[WolframAlphaQuantity[amts_List, units_List, opts___], fmt_] /; (Length[amts] === Length[units]) :=
With[{
	boxes = Map[Function[x, MakeBoxes[x, fmt], HoldAllComplete], amts],
	rowbox = RowBox[Flatten[Riffle[Map[{Slot @@ {#}, "  ", StyleBox[ToBoxes[units[[#]], fmt],
		FontColor -> GrayLevel[0.5], FontWeight -> "Plain", ShowStringCharacters -> False]}&, Range[Length[units]]], "  "]]],
	rest = Sequence @@ Flatten[Function[x, {",", MakeBoxes[x, fmt]}, HoldAllComplete] /@ Unevaluated[{units, opts}]]},

	TemplateBox[boxes, "WolframAlphaQuantities",
		DisplayFunction -> (FrameBox[rowbox, RoundingRadius -> 4, FrameStyle -> RGBColor[1, 0.9, 0.8], FrameMargins -> 3]&),
		InterpretationFunction -> (RowBox[{"WolframAlphaQuantity", "[",
			RowBox[{ RowBox[{"{", RowBox[{TemplateSlotSequence[1, ","]}], "}"}], rest }], "]"}]&),
		BaseStyle -> {FontFamily -> "Helvetica"}] ]


WolframAlphaQuantity /: MakeBoxes[WolframAlphaQuantity[amt_, unit_, opts___], fmt_] :=
With[{
	boxes = {MakeBoxes[amt, fmt]},
	rowbox = RowBox[{Slot[1], "  ", StyleBox[ToBoxes[unit, fmt], FontColor -> GrayLevel[0.5], FontWeight -> "Plain", ShowStringCharacters -> False]}],
	rest = Sequence @@ Flatten[Function[x, {",", MakeBoxes[x, fmt]}, HoldAllComplete] /@ Unevaluated[{unit, opts}]]},

	TemplateBox[boxes, "WolframAlphaQuantity",
		DisplayFunction -> (FrameBox[rowbox, RoundingRadius -> 4, FrameStyle -> RGBColor[1, 0.9, 0.8], FrameMargins -> 3]&),
		InterpretationFunction -> (RowBox[{"WolframAlphaQuantity", "[", RowBox[{#, rest}], "]"}]&),
		BaseStyle -> {FontFamily -> "Helvetica"}] ]


(* ::Section::Closed:: *)
(*Package footer*)


SetAttributes[{WolframAlphaDate, WolframAlphaQuantity}, NHoldFirst]


SetAttributes[{WolframAlphaResult}, NHoldRest]


SetAttributes[{
	AlphaIntegration`AlphaQuery,
	AlphaIntegration`AlphaQueryInputs,
	AlphaIntegration`ExtrusionEvaluate,
	AlphaIntegration`FormatAlphaResults,
	AlphaIntegration`LinguisticAssistant,
	AlphaIntegration`LinguisticAssistantBoxes,
	Asynchronous,
	ExcludePods,
	IncludePods,
	InputAssumptions,
	NamespaceBox,
	PodStates,
	PodWidth,
	WolframAlpha,
	WolframAlphaDate,
	WolframAlphaQuantity,
	WolframAlphaResult}, {Protected, ReadProtected}]



End[] (* `Private` *)


EndPackage[] (* WolframAlphaClient` *)
