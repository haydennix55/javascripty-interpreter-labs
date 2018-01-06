# javascripty-interpreter-labs
PPPL Labs using scala and functional programming to create an imperative javascript-ish language interpreter

**All projects are set up as intellij projects and can be built using sbt**

**If you wish to see the abstract syntax or inference rules, they are in the lab writeups (eg. labX.pdf) inside each lab folder.**

----

## Lab 1
The purpose of this first assignment is to warm-up with Scala and to refresh preliminaries that will be necessary for creating the javascripty interpreter

----

## Lab 2
In this lab, we create a big step interpreter for our JAVASCRIPTY language that has, at this point, 4 values, unary, and binary operators. We also impliment type conversion, a (often flawed) property of some imperative languages. The goal was to mimic javascript, including its idiosyncrasies like short-cutting and/or logic.

The primary learning goals of this lab are to understand the following:

* how grammars are used to specify the syntax of programming languages
* the distinction between concrete and abstract syntax
* the basics of inductive definitions via judgments, judgment forms, and inference rules and
* variable binding and variable environments.

Functional Programming: recursion and mapping

----

## Lab 3
In this lab, we will extend JAVASCRIPTY with recursive functions and implement two interpreters. The first will be a big-step interpreter that is an extension of Lab 2 but implements dynamic scoping “by accident.” The second will be a small-step interpreter that exposes evaluation order by iterating a single-step transition relation and implements static scoping by sub- stitution.

The primary learning goals of this lab are to understand the following:

* how to read a formal specification of a language semantics
* how dynamic scoping arises
* the distinction between a big-step and a small-step operational semantics
* evaluation order and
* substitution and program transformation

Functional Programming: Iteration. Introduction to higher-order functions.

----

## Lab 4
In this lab, we will extend JAVASCRIPTY with immutable objects and types and extend our small-step interpreter from Lab 3. Unlike all prior language constructs, object expressions do not have an a priori bound on the number of sub-expressions because an object can have any number of fields. To represent objects, we will use collections from the Scala library and thus will need to get used to working with the Scala collection API.

Parameters are always passed by value in JavaScript/TypeScript, so the parameter passing modes in JAVASCRIPTY is an extension beyond JavaScript/TypeScript. In particular, we consider parameter passing modes primarily to illustrate a language design decision and how the design decision manifests in the operational semantics

The primary learning goals of this lab are to understand the following:

* static type checking and the interplay between typechecking and evaluation
* parameter passing modes: call-by-value versus call-by-name
* capture-avoiding substitution and
* programming with higher-order functions

Functional Programming: Higher-order functions. Collections and callbacks.

----

## Lab 5
In this lab, we will update our type checker and small-step interpreter from Lab 4 with imperative variables, memory access, memory threading, addresses, and the operations associated with them and see that mutation (the non-functional idea that data can be changed at runtime) forces a global refactoring of our interpreter. To minimize the impact of this refactoring, we will be explore the functional programming idea of encapsulating effects in a data structure (known as a monad). We will also consider the idea of transforming code to a “lowered” form to make it easier to implement interpretation.

Extending our discussion about parameter passing modes to illustrate language design de- cisions and how design decisions manifest in the operational semantics. Call-by-value with addresses and call-by-reference are often confused, but with the operational semantics, we can see clearly the distinction.

The primary learning goals of this lab are to understand the following:

* imperative computation
* mutation and aliasing
* casting and type safety
* recursive types and
* programming with encapsulated effects

Functional Programming: Encapsulating computation as a datastructure (as opposed to evaluation).

----

## Lab 6
In this lab, we will consider regular expressions. We write construct a parser for a language of regular expressions and implement a regular expression matcher in Scala. For fun, we extend our Lab 5 interpreters with regular expression literals and regular expression matching (like JavaScript) using your parser and expression matcher.

The primary learning goals of this lab are to understand the following:

* thinking inductively and continuations and
* recursive descent parsing.

Functional Programming: Continuations (incontinuation-passingstyle). Thinking inductively.

----


















