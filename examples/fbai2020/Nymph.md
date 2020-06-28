Nymph is a theorem prover that uses natural language to state and write proofs of logical and mathematical theorems instead of the symbolic language of propositional and first-order logic. Nymph provides a natural language interface to the Sylph theorem prover and is designed to make
theorem proving using computers more inclusive and accessible to mathematics students without computer programming or TeX experience.

The language used in discussing and teaching mathematics is usually a combination of unambiguous symbolic expressions with informal statements and instructions where ambiguity exists and many surface forms may map to the same meaning. Nymph uses Wit.ai to understand the roles of the different consitituents of a natural language statement, and to extract the intent of the sentence as it relates to proofs and theorems together with any symbolic expressions as entities which which can then be parsed and interpreted by the Sylph theorem prover.

## Motivation
The 2020 coronavirus pandemic has accelerated the need for online teaching tools. Many students now access classes remotely. However remote evaluate
of mathematical problems is still problematic. Many times the only way for math assignments can be evaluated. Languages like LaTex and its variants can help studentd


Sylph is a languah

Sylph-N uses Wit.ai to translate sentences into proof rules which are then passed to the Sylph theorem prover.
Since Wit.ai has a built-in entity for math expressions, we can use Wit.ai to tell us what roles different expressions play in different senteces 

e.g Show that logical implication is defined by 
and translates 

in an argument and uses the
Sylph theorem prover

Sylph-N can also prove textual entailment between sentences by using Wit.ai to extract the logical form of natural language sentences and sending the extracted logical formula to Sylph where it can be proved according to the axioms and rules of a particualar thory. 

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/allisterb/nymph-notebooks/master)