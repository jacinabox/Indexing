Indexing
========

A desktop search application for Windows. It is able to search within gzip and bzip2 archives.

At present indexing is done manually, by using index -i followed by a directory. Command line output is achieved by entering keywords on the command prompt. Giving no keywords opens the GUI. Follow any actual parameters to the program, with +RTS -N, to get parallelism in the executable.

An overview of the components of this project:

* UI.hs - a Win32 GUI, including the entry point.
* Indexing.hs - the main indexing and searching code.
* Unpacks.hs - a module that does unpacking of archive files for searching. Also does conversion to plain text.
* Driveletters.hs - a module that interfaces with Win32, to determine the letters of the fixed drives on the system, for indexing.
* FileCons.hs - the low-level file access code used by Indexing.hs. Also stands on its own as a library.

To build the application, use

ghc UI.hs -o Index -O2 -DWIN32 -threaded

Hard problems to solve with this project:

* Writing a component (Linux Security Modules, or minifilter drivers on Windows) that monitors file writes on the system, and queues those files for indexing.
* Add more functions to the unpacks list in Unpacks.hs, to convert things like Word documents to plain text for indexing. (Use the treatment of HTML in Unpacks.hs as an example.)
* End-to-end support for character sets other than ANSI.

Contributions are appreciated. (info@alkalisoftware.net)
