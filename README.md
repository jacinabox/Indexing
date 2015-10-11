Indexing
========

A desktop search application for Windows. It is able to search within gzip and bzip2 archives.

At present indexing is done manually, by using index -i followed by a directory. Command line output is achieved by entering keywords on the command prompt. Giving no keywords opens the GUI.

An overview of the components of this project:

* UI.hs - a Win32 GUI, including the entry point.
* Indexing.hs - the main indexing and searching code.
* IndexDirector.hs - controls indexing and searching.
* Unpacks.hs - a module that does unpacking of archive files for searching. Also does conversion to plain text.
* Driveletters.hs - a module that interfaces with Win32, to determine the letters of the fixed drives on the system, for indexing.
* Normalize.hs - a function that converts accents to unaccented characters, to make searching in European languages easier.
* Subclass.hs - a function for subclassing a window in Win32.

To build the application, do these steps;

1. Install the Displayable package at https://github.com/jacinabox/Hoss/tree/master/Displayable

2. Install the Swizzling package from http://alkalisoftware.net/Swizzling-0.1.2.0.tar.gz

3. Get this repository and do

ghc UI.hs -o Index -O1 -DWIN32

on Windows or

ghc UI.hs -o Index -O1

on Linux.

Hard problems to solve with this project:

* Writing a component (Linux Security Modules, or minifilter drivers on Windows) that monitors file writes on the system, and queues those files for indexing.
* Add more functions to the unpacks list in Unpacks.hs, to convert things like Word documents to plain text for indexing.

Contributions are appreciated. (info@alkalisoftware.net)
