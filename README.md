Indexing
========

A cross-platform, command-line desktop search application. It is able to search within gzip and bzip2 archives.

At present indexing is done manually, by using index -i followed by a directory.

The command to search the index is "index" followed by keywords.

An overview of the components of this project:

* Indexing.hs - the main indexing and searching code.
* Unpacks.hs - a module that does unpacking of archive files for searching. Also does conversion to plain text.
* FileCons.hs - the low-level file access code used by Indexing.hs. Also stands on its own as a library.
* The driveletters project - a Windows program called driveletters.exe is used to generate the letters of fixed drives on the system. This component is built by Visual Studio 6.

To build the Haskell part of the application, use

ghc Indexing.hs -o Index -O2

on Linux and

ghc Indexing.hs -o Index.exe -O2 -DWIN32

on Windows.

Hard problems to solve with this project:

* Writing a component (Linux Security Modules, or minifilter drivers on Windows) that monitors file writes on the system, and queues those files for indexing.
* Add more functions to the unpacks list in Unpacks.hs, to convert things like Word documents to plain text for indexing. (Use the treatment of HTML in Unpacks.hs as an example.)

Contributions are appreciated. (info@alkalisoftware.net)
