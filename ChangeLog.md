2020-07-06  Nicklas Botö  <nicklas.boto@gmail.com>

	gitignore

	Expanded prelude

	Updat test

	Added format langdef, and beginnings of some abstraction naming sugar

	Added nice Unlambda interpreter output

2020-07-04  Nicklas Botö  <nicklas.boto@gmail.com>

	Update README.md
	It was gettin' old

	restructure

	Style changes

	added church numeral library file

	Updated TODO

	Added formatting function

	Unlambda functions can now be applied!

	Style changes

	Added string in compiled files

2020-07-03  Nicklas Botö  <nicklas.boto@gmail.com>

	More ideas

2020-07-03  NicklasBoto  <nicklas.boto@gmail.com>

	Updated vim syntax highlighting

2020-07-02  NicklasBoto  <nicklas.boto@gmail.com>

	New file in cabal

	Added omega combinator

	Fixed stupid I-combinator printing

	Better import parsing

2020-07-02  Nicklas Botö  <nicklas.boto@gmail.com>

	Added vim syntax file

2020-07-02  NicklasBoto  <nicklas.boto@gmail.com>

	Documentation

	Added adder example

2020-07-01  NicklasBoto  <nicklas.boto@gmail.com>

	Updated prelude

	Added support for some greek/math characters

	Added config file

	new redditex!

	more style...

	Style changes

	rename

	Updated prelude

2020-06-30  NicklasBoto  <nicklas.boto@gmail.com>

	Added library files

	Improved application parsing

	New, smaller beer art

2020-06-29  NicklasBoto  <nicklas.boto@gmail.com>

	Added run and gen commands

	Added langDef-macro parser, and improved function parsing

	Using FilePath instead of String

	Moved some type synonyms, added show instance

2020-06-27  NicklasBoto  <nicklas.boto@gmail.com>

	Updated version month

	Added wayne command

	name conflict

	Using FilePath

	Added some index escape delimiter handling

2020-06-22  NicklasBoto  <nicklas.boto@gmail.com>

	Updated yaml

	Simplified README

	Updated yaml

	Changed terminal sugar

	Updated commands

	Updated broken installation script

2020-06-21  NicklasBoto  <nicklas.boto@gmail.com>

	Added bruc section

	Looks nicer

	Updated verison number in yaml

	Added installation section

	Added install file

	Added compile command to bruce! Finally

2020-06-20  NicklasBoto  <nicklas.boto@gmail.com>

	Added banner

	updated yaml

2020-06-19  NicklasBoto  <nicklas.boto@gmail.com>

	Added basic commands for compiler

	Updated dependencies, added turtle

2020-06-18  Nicklas Botö  <nicklas.boto@gmail.com>

	Updated show instance for BLambda

	Moved test files

	Added generation and execution of files

	Import statements are changed to take filepaths

	Changed comment convension

	Syntax error in Prelude

2020-06-14  Nicklas Botö  <nicklas.boto@gmail.com>

	Added a TODO/idea file

2020-06-06  Nicklas Botö  <nicklas.boto@gmail.com>

	Missed a few comment conversions

2020-06-05  Nicklas Botö  <nicklas.boto@gmail.com>

	Updated readme and spec. some refreshers.

2020-06-04  Nicklas Botö  <nicklas.boto@gmail.com>

	Fixed parse error in issue. Closes #2

	Added with-utf8 dependency

2020-05-31  Nicklas Botö  <nicklas.boto@gmail.com>

	Restructure of project directory to use stack

2020-04-17  NicklasBoto  <nicklas.boto@gmail.com>

	Fixed overapplication in functions. Added support for some syntactic sugar functions. Added import support. Some debug functions (genString, runString, genUnl, comb)

	Fixed import statement bug

	Added prelude library folder. Combinator library example.

	Changed import statement

	AST now supports two types of church encodings

2020-04-13  NicklasBoto  <nicklas.boto@gmail.com>

	Added import statements. Non-evaluating files (libraries) can now be imported with the "import" keyword.

	Foundation of symbol table generation done, BruSKI files can now be compiled (given that they use pure lambda and function application). Yay!

	Swapped Names

	Changed error styling

	Removed Sequent Bλ constructor, now using type synonym Sequence for a list of Bλ. Makes code simpler.

2020-04-11  NicklasBoto  <nicklas.boto@gmail.com>

	Merge branch 'master' of https://github.com/NicklasBoto/BruSKI

	Added Symbol datatype and some rudimentary table generators.

	Changed Show instances, for windows stupid terminal

	Moved translation to new module

2020-04-11  Nicklas Botö  <nicklas.boto@gmail.com>

	Added intermediate AST

2020-04-09  NicklasBoto  <nicklas.boto@gmail.com>

	Parser now ensures assigned arity does not exceed the number of binders of an expression. Might move this to generator later.

2020-04-07  NicklasBoto  <nicklas.boto@gmail.com>

	Fixed syntactic sugar bug in parser. Index escapde delimiters in (skidmarks) are now ok in synsugar.

	Checked workflow

	Oops, moved it.

	Added outline of error handler.

	General λ to SKI translator defined and debugged. Comments for other lost souls. Added index decrementing function. Reduced scope function. Intermediate conversions added. Added Unlambda code generator.

2020-04-03  NicklasBoto  <nicklas.boto@gmail.com>

	Merge branch 'master' of https://github.com/NicklasBoto/BruSKI

	Reworked intermediate AST, added conversion functions. Added tx1, wip lambda to SKI translator.

2020-03-31  Nicklas Botö  <nicklas.boto@gmail.com>

	Fleshed out readme

2020-03-27  Nicklas Botö  <nicklas.boto@gmail.com>

	Update SPEC.md

	Clarifications
	tack hugo

2020-03-24  NicklasBoto  <nicklas.boto@gmail.com>

	Removed clutter

2020-03-24  NicBot  <asasdfasdf@,asdfasdf>

	Added function for parsing DeBruijn expressions, without general syntax. For debugging

	Code generator. Now includes datatype for index binding, and functions for calculating index scope and binding type.

2020-03-20  NicklasBoto  <nicklas.boto@gmail.com>

	Check

2020-03-12  NicklasBoto  <nicklas.boto@gmail.com>

	Added more parser checks

	Added PRT syntactic sugar function

2020-03-11  NicklasBoto  <nicklas.boto@gmail.com>

	Modulize parser, added terminal greeting. Improved function parsing. Improved file parsing

2020-03-10  NicklasBoto  <nicklas.boto@gmail.com>

	AST change, and parser helpers

	merge

	New expression parser in-file. Also added call-by-name functions, index escape delimiters (skidmarks) to parser. Improved application. Improved arity parsing. Added automatic arity calculation

2020-03-07  Nicklas Botö  <nicklas.boto@gmail.com>

	closing comments is good

	Update from readme

	!! operator, clarification

2020-03-06  NicklasBoto  <nicklas.boto@gmail.com>

	Commenting is now handled in the general syntax parser

	General syntax parser!

	Moved index parsing to new function, with error handling

2020-03-04  NicklasBoto  <nicklas.boto@gmail.com>

	Changed folder name

2020-03-04  Nicklas Botö  <nicklas.boto@gmail.com>

	Merge pull request #1 from SofiSjoberg/master
	Change name from SKID to BruSKI

2020-03-04  SofiSjoberg  <61803406+SofiSjoberg@users.noreply.github.com>

	pronounciation

	Same as Readme

	Updated name

2020-03-03  NicklasBoto  <nicklas.boto@gmail.com>

	shorthand parse, for debug

2020-03-01  NicklasBoto  <nicklas.boto@gmail.com>

	removed old file

	Added CHR and INT Church encondings

	Added function Enc for encodings

	Added Unlambda expression parsing

	Added unlambda terms in DeB datatype

	Moved Unl interpreter, import reasons

2020-02-29  NicklasBoto  <nicklas.boto@gmail.com>

	Merge

	Added DeBruijn parser (that works)

	Added AST for DeBruijn expressions

	Added my unlambda interpreter

2020-02-26  Nicklas Botö  <nicklas.boto@gmail.com>

	Spec in readme

	Small errors

2020-02-26  NicklasBoto  <nicklas.boto@gmail.com>

	comment change

	changed eval section

	section about comments

	?

	Changed the arity spec

	Updated spec

	Updated spec

	Short language spec

2020-02-26  Nicklas Botö  <nicklas.boto@gmail.com>

	Initial commit
