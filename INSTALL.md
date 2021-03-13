Welcome to HyperBird Play application
=====================================

This file is packaged with the current Play application 
when using `play dist`.


## Managed dependencies

Check build.sbt, project/plugins.sbt, and project/Build.scala


## Unmanaged dependencies

XQJ driver: A standard Java interface to XML DataSources which support XQuery 1.0.

XQJ driver files for eXist XML database http://www.exist-db.org downloaded from http://xqj.net/exist/
are located in project at: 

lib/exist-xqj-1.0.1.jar
lib/xqj2-0.0.1.jar
lib/xqjapi.jar


XQS XQuery for Scala

XQS is found at https://github.com/fancellu/xqs and is a single file.
This file XQS.scala is included in the source code "as is" and should
only be used by objects or classes located in ch.bsisa.hyperbird.dao 
package.
Note that XQS.scala depends on lib/xqjapi.jar at compile time.


## How to run

Prerequisites are:

* Play 2.2.6 available here: https://www.playframework.com/releases
* Java
* SBT
* wkhtmltopdf (see http://wkhtmltopdf.org/downloads.html) to get the latest version
Other dependencies should follow automatically (Play, Scala,...).


Tested environment: 
* Open JDK java version "1.7.0_25"
* SBT version 0.13.0

If SBT is not yet installed check:

SBT install
http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html

For MaxOSX there are homebrew and macport packages, c.f. above link.


Then in the hb-api project type:
sbt
play
run

http://localhost:9000/
should direct you to the login page

