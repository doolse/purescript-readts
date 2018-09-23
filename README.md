# purescript-readts

A library which uses the Typescript Compiler API to read the types in Typescript source files + functions and data structures for converting them into Purescript source files.

## The goal

If you're like me, then you like type safety and Purescript is your transpiled JS language of choice. The downside of using a language with a smaller user base is that a large number of useful JS libraries will require effort to create bindings. 

Thankfully for those of us who like type safety, a large number of JS libraries are either written in Typescript or have Typescript declaration files, so provided we have a way to convert Typescript definitions into equivalent Purescript types. That's the goal of this library in conjunction with the typechecking support library [purescript-tscompat](http://github.com/doolse/purescript-tscompat).

## Current status

* Focus on types of React properties
* Used to generate [purescript-react-mui](http://github.com/doolse/purescript-react-mui)
* Only converts top level interface declarations currently