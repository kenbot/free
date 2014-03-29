Free Monad examples
===============

Presented to the Melbourne Scala User Group on March 24th 2014, of which you can [find the slides on SlideShare](http://www.slideshare.net/kenbot/running-free-with-the-monads).

Tank Game
---------------
The Tank Game uses Free Monads to drive the AIs of the tanks.  Each Tank holds a Free[TankMove, Unit] (aka AI[Unit]), which gets consumed one instruction at a time per game frame.

The "game" consists of a number of tanks with hard-wired AI scripts; there is no user input.

There are two interpreters, a Hard interpreter, and an Easy interpreter which shoots slightly to the right.


KVS
----------------
A simple example of an ADT/Functor that can generate a useful Free Monad, based on Runar Bjarnason's [Dead Simple Dependency Injection](http://www.youtube.com/watch?v=ZasXwtTRkio) talk.

This illustrates the steps to convert a conceptual DSL to a Free Monad script, and has an example of both a pure and effectful interpreter.


Trampoline
---------------
Simple example of a mutually tail-recursive algorithm that cannot be optimised by scalac, based on Runar's [Stackless Scala](http://blog.higher-order.com/assets/trampolines.pdf) paper. There is a naive example that will blow the stack, and a Trampoline translation of the same code that lifts the recursion onto the heap.

NIHFree
-------------
Minimal implementation of Free, from first principles.  This was an effective exercise for me to learn the structure, and may be otherwise useful as a reference with less noise than scalaz.Free or Control.Monad.Free.
