
# macrowave

Macrowave is a Scala library, which brings scanner and parser generators to the 21st century.

Tools like [Lex], [Flex], [Bison] and [YACC] are well known among compiler engineers. 
One disadvantage of those tools is that the creation (generation) of scanners and parsers doesn't belong to the actual 
process of compilation.
Using macros, the generation of the scanners and parsers can be achieved via meta-programming. 
Thus, the disadvantage will disappear.

Macrowave allows to define a parsing expression grammar, which gets transformed into a regular automaton for 
tokenization and a table driven LALR(1)-parser.

## Building macrowave
The project macrowave is built with [SBT] 0.13.12 or later and its master branch with [Scala] 2.11.8.

To build the project:
- Install [SBT] and [Scala].
- Clone this project and `cd` into the root directory of the cloned repository.
- Run
    - `sbt compile` to build the project
    - `sbt test` to run the unit tests

## Using macrowave
- TODO: Nothing to use, yet!

## Trivia
The name "macrowave" is obviously a play on "macros" and "microwave".

## Authors
- [keddelzz](https://github.com/keddelzz)
- [kuchenkruste](https://github.com/kuchenkruste)

[Bison]: http://dinosaur.compilertools.net/bison/
[Flex]: http://dinosaur.compilertools.net/flex/
[Lex]: http://dinosaur.compilertools.net/lex/
[SBT]: http://www.scala-sbt.org/
[Scala]: https://www.scala-lang.org/
[YACC]: http://dinosaur.compilertools.net/yacc/
