Indexing
========

A desktop search application for Windows. It is able to search within gzip and bzip2 archives.

At present indexing is done manually, by using index -i followed by a directory. Command line output is achieved by entering keywords on the command prompt. Giving no keywords opens the GUI.

An overview of the components of this project:

* UI.hs - a Win32 GUI, including the entry point.
* Indexing.hs - the main indexing and searching code.
* Unpacks.hs - a module that does unpacking of archive files for searching. Also does conversion to plain text.
* Driveletters.hs - a module that interfaces with Win32, to determine the letters of the fixed drives on the system, for indexing.
* FileCons.hs - the low-level file access code used by Indexing.hs. Also stands on its own as a library.
* Normalize.hs - a function that converts accents to unaccented characters, to make searching in European languages easier.

If you do not have the Win32 package, install it by doing

cabal install win32

To build the application, use

ghc UI.hs -o Index -O2 -DWIN32

Hard problems to solve with this project:

* Writing a component (Linux Security Modules, or minifilter drivers on Windows) that monitors file writes on the system, and queues those files for indexing.
* Add more functions to the unpacks list in Unpacks.hs, to convert things like Word documents to plain text for indexing.

Contributions are appreciated. (info@alkalisoftware.net)
