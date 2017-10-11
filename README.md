HF is a WIP API fuzzing tool.

Prof. Regehr has the best 'quick intro' to API fuzzing:

  https://blog.regehr.org/archives/1269

There is a dearth of tools available for API fuzzing of real-world software
today.  Haskell's QuickCheck is good:

  https://wiki.haskell.org/Introduction_to_QuickCheck1

but few developers live in that world.  Furthermore, QuickCheck is oriented
towards checking a simple procedure: if there are properties to verify on a
data structure built up over many API calls, it is less suited to the task.

HF allows one to write an "abstract" test program along with a set
of 'Generators'.  The Generators produce one of any number of values
and are intended to sample the space of possible inputs to your API.
The Generators are user-driven, however, so you can create only those
inputs that make sense for your specific API.

(For those familiar with QuickCheck, 'Generators' are rather similar to
'Property' functions.)
