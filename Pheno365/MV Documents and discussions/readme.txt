This directory contains the current version (as of February
2010) of the codes and documentation relative to the programs to
investigate the phenological evolution of vegetation over land.
These files and codes are upgrades and revised versions of the
earlier growing seasons IDL codes (version 6), as well as of the
phenot project as it evolved up to September 2008. All routines
have been ported to the new Eclipse Workbench development
environment of IDL 7.x.

The contents of this directory are organized as follows:

- Subdirectory 'Codes' includes all IDL source files to process
the data and generate the output.

- Subdirectory 'Data' contains the input data files used for
phenological studies, in the new '.xdat' format. Data files in
the original format ('.dat') are located in /home2/data/SeaWiFS.

- Subdirectory 'Documentation' assembles all the documentation
relative to this project, including the definition of the new
'.xdat' format, the Algorithm Tehoretical Basis Document describing
the algorithm, and the papers or reports relative to the project.

To start IDL 7.x and the Workbench environment, open a terminal
and issue the command

idlde &

then select the IDLWorkspace 'Science/Pheno/Codes'. To also start
the on-line help in a separate window, select 'Help content' from
the 'Help' menu in the main IDL window.

This environment currently hosts three projects:

- xdat involves all routines required to convert the original
FAPAR files from the basis SAS file format to the updated xdat
file format. Routine read_xdat is required by phenot; the other
routines are useful only when new data files need to be
processed.

- utils contains generic routines used by phenot but potentially
also useful in other contexts.

- phenot is the main project to compute the phenological
statistics of FAPAR time series.

To operate the software, it is necessary to
1 build the xdat project
2 build the utils project
3 build the phenot project
4 open the phenot.pro file and 'Run' it
