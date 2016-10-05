This is the top-level directory for collaboration between the University of
Melbourne and Oracle Labs on security verification (ARC linkage project
LP140100437).

This repository contains the string abstract domains we currently implemented in SAFE in this [folder](src/main/scala/kr/ac/kaist/jsaf/analysis/typing/domain/), i.e.:

|File|Description|
|---|---|
| AbsString.scala|Base class for all the abstract string domains |
| AbsStringAutomata.scala|DFA domain (to be implemented) |
| AbsStringCharIncl.scala|Character Inclusion (CI) domain |
| AbsStringConst.scala|Constant String (CS) domain |
| AbsStringHash.scala|String Hash domain, see [1] |
| AbsStringJSAI.scala|JSAI default string domain, see [2] |
| AbsStringNumOth.scala|Numeric/Other domain, used by SAFE |
| AbsStringNumSplOth.scala|Numeric/Special/Other domain, used by JSAI |
| AbsStringPrefSuff.scala|Prefix-Suffix (PS) domain |
| AbsStringProd.scala|Cartesian (actually, direct) product of domains |
| AbsStringSAFE.scala|SAFE default string domain |
| AbsStringSet.scala|String Set domain, see [1] |

[1] Magnus Madsen and Esben Andreasen| String analysis for dynamic field access.
    In A| Cohen, editor, Compiler Construction, volume 8409 of Lecture Notes in
    Computer Science, pages 197–217| Springer, 2014.

[2] Vineeth Kashyap, Kyle Dewey, Ethan A| Kuefner, John Wagner, Kevin Gibbons,
    John Sarracino, Ben Wiedermann, and Ben Hardekopf| JSAI: A static analysis
    platform for JavaScript| In Proceedings of the 22nd ACM SIGSOFT International
    Symposium on Foundations of Software Engineering, pages 121–132| ACM Publ., 2014.

