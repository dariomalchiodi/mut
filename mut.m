(* ::Package:: *)

(* : Title : mut *)
(* : Context : mut` *)
(* : 
    Author : Dario Malchiodi *)
(* : 
    Summary : simple unit testing *)
(* : 
    Package Version : 1.0 *)
(* : Mathematica Version : 8 *)
(* : 
    Keywords : unit testing *)

BeginPackage["mut`"]

mutNewTest::usage = "mutNewTest[] creates and returns a new test; mutNewTest[n] creates and returns a new test with name n.";\

mutSetName::usage = "mutSetName[tag, n] sets to n the name of an existing test identified by tag.";\

mutGetName::usage = "nutGetName[tag] returns the name of an existing test identified by tag.";\

mutSetBeforeAll::usage = "mutSetBeforeAll[tag, f] sets to f the function which will be called before starting the evaluation of all test cases associated to the test identified by tag.";\

mutGetBeforeAll::usage = "mutGetBeforeAll[tag, f] returns the function which will be called before starting the evaluation of all test cases associated to the test identified by tag.";\

mutSetAfterAll::usage = "mutSetAfterAll[tag, f] sets to f the function which will be called after the evaluation of all test cases associated to the test identified by tag is finished.";\

mutGetAfterAll::usage = "mutGetAfterAll[tag, f] returns the function which will be called after the evaluation of all test cases associated to the test identified by tag is finished.";\

mutSetBefore::usage = "mutSetBefore[tag, f] sets to f the function which will be called before starting the evaluation of any test case associated to the test identified by tag.";\

mutGetBefore::usage = "mutGetBefore[tag] returns the function which will be called before starting the evaluation of any test case associated to the test identified by tag.";\

mutSetAfter::usage = "mutSetAfter[tag, f] sets to f the function which will be called after the evaluation of any of the test cases associated to the test identified by tag is finished.";\

mutGetAfter::usage = "mutGetAfter[tag] returns the function which will be called after the evaluation of any of the test cases associated to the test identified by tag is finished.";\

mutAddTestCase::usage = "mutAddTestCase[tag, f] adds a test case whose execution is described in f to the test identified by tag.";\

mutGetTestCases::usage = "mutGetTestCases[tag] returns all test cases associated to the test identified by tag.";\

mutTestRun::usage = "mutTestRun[tag] runs all test cases associated to the test identified by tag.";\

mutTest::usage = "mutTest[tag] represents a test, identified by tag.";\

getName::usage = "";\

setName::usage = "";\

setBeforeAll::usage = "";\

getBeforeAll::usage = "";\

setAfterAll::usage = "";\

getAfterAll::usage = "";\

setBefore::usage = "";\

getBefore::usage = "";\

setAfter::usage = "";\

getAfter::usage = "";\

addTestCase::usage = "";\

getTestCases::usage = "";\

run::usage = "";\

mutAssertEquals::usage = "";\

mutAssertEquals::usage = "";\

mutAssertSame::usage = "";\

mutAssertNotSame::usage = "";\

mutAssertTrue::usage = "";\

mutAssertFalse::usage = "";\

mutAssertNull::usage = "";\

mutAssertNotNull::usage = "";\

mutAssertMatches::usage = "";\

mutAssertThrows::usage = "";\

mutAssertFails::usage = "";\

mutAssertEndsWithin::usage = "";\

Begin["`Private`"]







(* ::Section:: *)
(*Package variables*)


mutTests={};

mutNullFunction[]:=Block[{},
  Null
];

mutDefaultTest={
  "tag" -> 0,
  "name" -> "", 
  "before" -> mutNullFunction,
  "after" -> mutNullFunction,
  "beforeAll" -> mutNullFunction,
  "afterAll" -> mutNullFunction,
  "testCases" -> {}
};

MUTTAGPOS=Position[mutDefaultTest,"tag"->_][[1,1]];
MUTNAMEPOS=Position[mutDefaultTest,"name"->_][[1,1]];
MUTBEFOREPOS=Position[mutDefaultTest,"before"->_][[1,1]];
MUTAFTERPOS=Position[mutDefaultTest,"after"->_][[1,1]];
MUTBEFOREALLPOS=Position[mutDefaultTest,"beforeAll"->_][[1,1]];
MUTAFTERALLPOS=Position[mutDefaultTest,"afterAll"->_][[1,1]];
MUTTESTCASESPOS=Position[mutDefaultTest,"testCases"->_][[1,1]];


(* ::Section:: *)
(*Helper functions*)


mutGetPosition[tag_]:=Position[mutTests, "tag"->tag][[1, 1]];


(* ::Section:: *)
(*Public functions*)


(* ::Subsection:: *)
(*Test creation*)


mutNewTest[name_:""]:= Block[{tag, t},
  tag = $ModuleNumber;
  t = {};
  Do[
    t = Append[t, e],
  {e, mutDefaultTest}];
  t[[MUTTAGPOS]][[2]] = tag;
  If[name != "",
    t[[MUTNAMEPOS]][[2]] = name;
  ];
  mutTests=Append[mutTests, t];
  Return[mutTest[tag]];
]


(* ::Subsection:: *)
(*Accessors*)


mutTest/: mutTest[tag_].setName[name_]:= mutSetName[tag, name];
mutSetName[tag_, name_]:= Block[{p},
  p = mutGetPosition[tag];
  mutTests[[p]][[MUTNAMEPOS]][[2]] = name;
];

mutTest/: mutTest[tag_].getName[]:= mutGetName[tag];
mutGetName[tag_]:= Block[{p},
  p = mutGetPosition[tag];
  Return["name" /. mutTests[[p]]];
];

mutTest/: mutTest[tag_].setBeforeAll[beforeAll_]:= mutSetBeforeAll[tag, beforeAll];
mutSetBeforeAll[tag_, beforeAll_]:= Block[{p},
  p = mutGetPosition[tag];
  mutTests[[p]][[MUTBEFOREALLPOS]][[2]] = beforeAll;
];

mutTest/: mutTest[tag_].getBeforeAll[]:= mutGetBeforeAll[tag];
mutGetBeforeAll[tag_]:= Block[{p},
  p = mutGetPosition[tag];
  Return["beforeAll" /. mutTests[[p]]];
];

mutTest/: mutTest[tag_].setAfterAll[afterAll_]:= mutSetAfterAll[tag, afterAll];
mutSetAfterAll[tag_, afterAll_]:= Block[{p},
  p = mutGetPosition[tag];
  mutTests[[p]][[MUTAFTERALLPOS]][[2]] = afterAll;
];

mutTest/: mutTest[tag_].getAfterAll[]:= mutGetAfterAll[tag];
mutGetAfterAll[tag_]:= Block[{p},
  p = mutGetPosition[tag];
  Return["afterAll" /. mutTests[[p]]];
];

mutTest/: mutTest[tag_].setBefore[before_]:= mutSetBefore[tag, before];
mutSetBefore[tag_, before_]:= Block[{p},
  p = mutGetPosition[tag];
  mutTests[[p]][[MUTBEFOREPOS]][[2]] = before;
];

mutTest/: mutTest[tag_].getBefore[]:= mutGetBefore[tag];
mutGetBefore[tag_]:= Block[{p},
  p = mutGetPosition[tag];
  Return["before" /. mutTests[[p]]];
];

mutTest/: mutTest[tag_].setAfter[after_]:= mutSetAfter[tag, after];
mutSetAfter[tag_, after_]:= Block[{p},
  p = mutGetPosition[tag];
  mutTests[[p]][[MUTAFTERPOS]][[2]] = after;
];

mutTest/: mutTest[tag_].getAfter[]:= mutGetAfter[tag];
mutGetAfter[tag_]:= Block[{p},
  p = mutGetPosition[tag];
  Return["after" /. mutTests[[p]]];
];

mutTest/: mutTest[tag_].addTestCase[testCase_]:= mutAddTestCase[tag,testCase];
mutAddTestCase[tag_, testCase_]:= Block[{p},
  p = mutGetPosition[tag];
  mutTests[[p]][[MUTTESTCASESPOS]][[2]] = Append[mutTests[[p]][[MUTTESTCASESPOS]][[2]], testCase];
];

mutTest/: mutTest[tag_].getTestCases[]:= mutGetTestCases[tag];
mutGetTestCases[tag_]:= Block[{p},
  p = mutGetPosition[tag];
  Return["testCases" /. mutTests[[p]]];
];


(* ::Subsection:: *)
(*Test execution*)


mutTest/: mutTest[tag_].run[opts___]:= mutTestRun[tag, opts];
Options[mutTestRun] = {Verbose -> False};
mutTestRun[tag_, opts___]:= Block[{isVerbose, result, results},
  isVerbose = Verbose /. {opts} /. Options[mutTestRun];
  If[isVerbose, Print["Run beforeAll..."]];
  (mutTest[tag].getBeforeAll[])[];
  If[isVerbose, Print["Done"]];
  results = Reap[
    Do[
      If[isVerbose, Print["Running before..."]];
      (mutTest[tag].getBefore[])[];
      If[isVerbose, Print["Done."]];
      If[isVerbose, Print["Running test case..."]];
      result = testCase[];
      If[isVerbose, Print["Done. Result is ", result]];
      Sow[result];
      If[isVerbose, Print["Running after..."]];
      (mutTest[tag].getAfter[])[];
      If[isVerbose, Print["Done."]];
    ,{testCase, mutTest[tag].getTestCases[]}];
    ][[2, 1]];
  If[isVerbose, Print["Running afterAll..."]];
  (mutTest[tag].getAfterAll[])[];
  If[isVerbose, Print["Done."]];

  Return[Count[results, True] == Length[results]];
];


(* ::Subsection:: *)
(*Assertions*)


mutAssertEquals[val_, target_]:= Return[target == val];
mutAssertEquals[val_, target_, tolerance_]:= Return[Abs[target - val] < tolerance];

mutAssertSame[val_, target_]:= Return[target === val];
mutAssertNotSame[val_, target_]:= Return[target =!= val];

mutAssertTrue[val_]:= Return[val];
mutAssertFalse[val_]:= Return[Not[val]];

mutAssertNull[val_]:= Return[val == Null];
mutAssertNotNull[val_]:= Return[val != Null];

mutAssertMatches[val_, pattern_]:= Return[MatchQ[val, pattern]];

mutAssertThrows[expr_, exception_]:= Return[Catch[expr; $Failed] == exception];
mutAssertFails[expr_, failExpr_]:= Return[Check[expr, failExpr, $Failed] == $Failed];

mutAssertEndsWithin[expr_, timeout_]:= Block[{result},
  result = TimeConstrained[expr, timeout];
  Return[If[result != $Aborted, result, False]];
];


End[]

EndPackage[]
