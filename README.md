XDS Modula-2/Oberon-2 Development System for Windows
====================================================

The XDS development system facilitate software development in the Modula-2 and Oberon-2 languages. The unique feature of XDS is seamless integration between the two, which enables you to mix Modula-2 and Oberon-2 modules in one project freely. It includes two toolchains: Native XDS-x86 and XDS-C.

#### Native XDS-x86
Native XDS-x86 is an optimizing ISO Modula-2 and Oberon-2 compiler for 32-bit Intel x86 systems running Windows. It comes with additional suite of tools including debugger, linker, etc.

#### XDS-C
XDS-C is a Modula-2/Oberon-2 "via C" compiler: its output is ANSI C, K&R C, or C++ source code, which may then be compiled by a third-party C/C++ compiler for any target platform. This technique allows you to cross program in Modula-2 and/or Oberon-2 for virtually any target environment. XDS-C itself runs on Windows.

Building from source
====================

To build the XDS development system, you will need:

*   Git
*   Microsoft Visual Studio Community 2015 or higher
*   NASM - the Netwide Assembler
*   MinGW - Minimalist GNU for Windows

Download the sources:

    git clone https://github.com/excelsior-oss/xds.git

Go to the 'bin' repository directory and unpack the binary version of the XDS system:

    cd xds\bin 
    unzip xds-x86-2.51.win32.zip

Create your building environment:
- Go to the '.config' repository directory, 
- Rename the '.config\.env-COMPUTERNAME.bsc' according to the name of your workstation,
- Set path to external tools inside this file.

Go to the root of repository and build everything using 'xdswork.bat' script:

    xdswork.bat

The XDS development system will be in the 'XDS' directory.

Released under the Apache 2.0 license, see LICENSE.

Copyright © 2019 [Excelsior LLC](https://excelsior.ru)
