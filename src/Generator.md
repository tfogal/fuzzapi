Creating Generators
----

A Generator is an iterator that creates code fragments in some related class of
type[s].  Generators know how to both declare variables of their type as well
as how to mutate the state of that variable in interesting ways.  For example,
an integer generator knows how to declare an integer:
	int foo = 0;
That generator also knows how to assign "interesting" values to integer-typed
variables, such as the foo it just generated:
	foo = 42;
The idea is that a Generator can somehow make an intelligent reduction of the
class of variables it represents.  The integer generator could conceivably
generate any of 2^32 values.  However, if you test an API with the value '7',
it is unlikely to exhibit vastly different results with the value '6'.  Thus
the default integer generator has a small finite number of regimes: negative
numbers far from 0; negative numbers near 0; 0; positive numbers near 0;
positive numbers far from 0.  By testing with values from all of these regimes,
one has *not* *guaranteed* that the API is safe against overflow or underflow or
crazy things like interpreting the integer as a pointer---yet nonetheless the
approach is *likely* to find issues of those types if they exist.

-----

The idea is that there are a set of properties one can set in a graph.
Let's say it's a computation tree for a simple calculator-like language.
One "property" would be to print a single child node.  Another might be
"add".  Another might be define-variable.  Another might be to add an
integer constant node, where the actual integer constant itself comes from a
generator.  And so on.

One can then think of HotFuzz as generating the test program:
  graph* g = create_graph(); // the decl.
  { g->add_child(g_print(null)); } // property 0, print.
as well as the program:
  graph* g = create_graph(); // the decl.
  { g->add_child(g_add(nullptr, nullptr)) } // property 1, print.
and so on for the other properties.  But it should also generate
combinations.  Now omitting the decl:
  { graph* ic = g_intconst(42); // property 4, integer constant node
    g->add_child(g_print(ic)); } // property 0 mixed with property 4
and:
  { graph* ic = g_intconst(4billion); // property 4
    graph* add = g_add(nullptr, ic); // property 2 mixed with property 4
    g->add_child(g_print(add)); } // 1 mixed with 2 mixed with 4
note that the last one could also be:
  { graph* ic = g_intconst(4billion); // property 4
    graph* add = g_add(nullptr, ic); // property 2 mixed with property 4
    g->add_child(g_print(nullptr)); } // 1 mixed with 2 mixed with 4
i.e. it sets up a bunch of things working in tandem but then decides NOT to
use it.  This could still be a valid test, by verifying that there is no
interaction between the "add" tree and the other tree.

We might also consider various mixin verification stages.  For example,
maybe the client wants to assert there are no memory leaks.  But maybe that
assertion only makes sense if the API is used properly, and some mixes will
use it properly whilst some will not.

This type of scheme means that:
  1. Properties need to work both independently as well as in tandem with
     one another.  The "output" of one property could be the "input" to
     another.  Can properties have multiple outputs?
  2. We need to be able to generate the set of all "mixes" of properties.
  3. We need to describe invariants in a language that can reference each
     property's value and/or variable.
     3.a. option 1: the generator for the invariant wants to ask,
          "in this run, is property foo set?  if so, then I want a
           conditional on ...", or maybe a constraint, or whatever.
     3.b. option 2: the generator for the invariant wants to ask,
          "in this run, assert somefunc(blah), where 'blah' is the output
           variable from property p."
     3.c. this need is recursive!  In the integer constant case, for
          example, the invariant might decide it wants a constraint that all
          generated constants are greater than 0.  It thus needs to query
          the generator that's embedded into the constant.
