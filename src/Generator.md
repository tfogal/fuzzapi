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

Generators need to extend beyond more than just a single value.  Consider a
graph or tree API that implements a computation tree for a simple
calculator-like language:

	typedef void graph;
	graph* g_intconst(int);
	graph* g_add(graph*, graph*);
	void g_print(graph*, FILE*);
	g_destroy(graph*);

One might compute the expression "5 + 6 + 7" with the series of calls:

	graph* five = g_intconst(5);
	graph* six = g_intconst(6);
	graph* seven = g_intconst(7);
	graph* lhs = g_add(five, six);
	graph* expr = g_add(lhs, seven);
	g_print(expr, stdio);
	g_destroy(expr);

Verifying this is complex, as the combinations of possible nodes grows quickly
with the set of APIs involved---this was just adding integers, imagine a range
of math ops and a range of types.

From HF's point of view, this is complex due to the fact that multiple
generated states can interact.  If we consider a Generator that creates the
line:

	graph* five = g_intconst(5);

then the issue is that we need to be able to test subsequent lines both in
isolation *and* by taking into account the five we just created.  That is, we
need to be able to generate both:

	graph* foo = g_add(NULL, NULL);

and

	graph* foo = g_add(five, NULL);

etc.  For practical testing one would want the arguments to 'g_add' to be any
combination of NULL and any subgraph that warrant testing on its own.

Thus we cannot assume that a Generator's current state generates a complete
instance of a test: the state of a Generator includes the state of other
Generators.  We support this by nesting Generators.

This means Generators must be extended such that they can query the state of
sub-generators at a finer granularity.  For example, if a sub-generator
creates:
	graph* six = g_intconst(6);
then the Generator that houses it needs to be able to query the sub-generator
and ask it for the list of variables it created---in this case: ["six"].  Then
we can nest Generators ad infinitum.

For example, one Generator might create graph*'s of all the different supported
types.  Another might generate over all the supported math operations (add,
subtract, multiply, etc.).  Another may generate nulls and intconsts.  Another
generator may simply aggregate sub-generators and generate all combinations.

Verification after this may be dependent on that full state.  Null pointers
might be invalid in the tree; maybe your program simply asserts that any
programs with null pointers as input succeed if the test executes without
segfaulting.  But it could be more complex, such as wanting to assert that in
the absence of subtract operations, and when a final print is present, the
final print exceeds the value of any leaf node in the tree.  That kind of
conditional will need to know how the tree was generated to know whether the
assertion can be made.

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
