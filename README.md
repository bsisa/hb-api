hb-api
======

HyperBird API services


# Building the distribution #


## Prerequisites tools ##

* Java JDK - (Java Development Kit)
  If not already install check for it at: http://openjdk.java.net/ and  http://java.oracle.com
* SBT - Simple Build Tool
  http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html
* JNotify 
  http://jnotify.sourceforge.net/


## Build procedure ##

Go to hb-api/ and type: 

    sbt dist

The distribution will be available at: 
    hb-api/target/universal/hb-api-X.Y.Z.zip

## Running in dev

If you're running on Linux, make sure you have jnotify installed and add it to the java.library.path env. Example:

    sbt -Djava.library.path=/home/guy/bin/jnotify/64bit_linux/ play run    


# Running the distribution #

In production mode from the packaged distribution: 

    unzip hb-api-X.Y.Z.zip

Go to hb-api-X.Y.Z/bin directory:

    cd hb-api-X.Y.Z/bin

Run the application (Linux, MacOSX):

    ./hb-api 

Run the application (Windows):

    hb-api.bat

Then using a Web browser go to: 

    http://localhost:9000/



