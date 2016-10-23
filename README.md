
SAFE (with string domains)
====

### Preamble

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

### Qickstart

Building and testing SAFE (in sbt):  
```bash
# launch SBT with Java 7 runtime
sbt -java-home /usr/lib/jvm/java-7-oracle
 
 # set proxy for SBT to download software packages
-Dhttps.proxyHost=your.https.proxy.com -Dhttps.proxyPort=80
 
# compiling in sbt should work by just using:
> compile
 
# if there are problems with the ant compile task that `compile` depends on, try:
> antRun clean compile
> antRun compile

Running SAFE analysis using the `neosafe` command line tool :  
```bash
# default analysis
bin/neosafe foo.js

# HTML (DOM-enabled) analysis
bin/neosafe --html foo.html

# configure string domains
bin/neosafe --max-strset-size 32 --strdom=ss,ci,no foo.js

# show help
% ~/work/safe/bin/neosafe --help
Guessing JS_HOME=/home/aljordan/work/safe
  -b, --benchmark                benchmark default settings (KAIST-lsa/regex)
  -c, --cfgdump
      --debug-after  <arg>       start debug after a certain iteration
  -d, --dom                      dom mode
  -e, --exitdump
      --heap-verbose  <arg>
  -h, --html  <arg>              html file for analysis
  -j, --jquery                   enable jQuery model
  -l, --max-iter  <arg>          stop after n iterations max
  -m, --max-strset-size  <arg>   max string set size
  -n, --no-imprecision-log       do not ouput imprecision logging
      --no-imprecision-stop      do not stop on catastrophic imprecision
  -q, --quiet                    less debug output
  -s, --stats                    dump full statistics
      --strdom  <arg>            enable advanced string domains
      --test                     expose abstract types for testing
      --timeout  <arg>           timeout in seconds
  -t, --trace                    trace output for AI semantics
      --help                     Show help message

```


[1] Magnus Madsen and Esben Andreasen| String analysis for dynamic field access.
    In A| Cohen, editor, Compiler Construction, volume 8409 of Lecture Notes in
    Computer Science, pages 197–217| Springer, 2014.

[2] Vineeth Kashyap, Kyle Dewey, Ethan A| Kuefner, John Wagner, Kevin Gibbons,
    John Sarracino, Ben Wiedermann, and Ben Hardekopf| JSAI: A static analysis
    platform for JavaScript| In Proceedings of the 22nd ACM SIGSOFT International
    Symposium on Foundations of Software Engineering, pages 121–132| ACM Publ., 2014.
