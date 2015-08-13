# OpenLogicConvert
Conversion toolchain to produce OLT in vanilla LaTeX/XML/HTML/etc

To compile the converter, type the following in a terminal while in the repository directory:

    > ghc --make Replacer.hs -o convert

Next, copy the compiled executable (in this case "convert") and the file "book-configuration.opt" to the top level of the book directory (probably called "OpenLogic")

Finally, run the executable with ./convert. By default, it targets the file "open-logic-complete.tex", and outputs to the file "processed-book.tex". You can compile "processed-book.tex" with pdflatex as normal. 

The program has four command line options. --input=infile.tex will target "infile.tex" instead of "open-logic-complete.tex". --output and --config change the outputfile and configuration file similarly. --help explains the command line options (-h also works).

