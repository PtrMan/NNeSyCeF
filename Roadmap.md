Roadmap
===

Q&A
====

* implement basic Q&A
* find better answers
* (advanced) structural queries with SDR's

"Q&A Set-comprehension"
=====

**Problem**:<br />
Q&A of sets takes to long in OpenNARS 3.0.0 and before

**Impl**:
* set comprehension ex: https://github.com/opennars/opennars2/blob/master/src/nal/rules.clj#L204 has to be done in the Q&A functionality
* fast and efficient handling of sets with parent-questions

Attention
====

* ~~implement basic attributes for tasks~~
* implement concept with custom strategy

Inference
====

* implement most rules
* unifier

Procedural
====

* implement decision making
* implement procedural learning and representation from ANSNA

* implement sequence mapping and query with SDR's
   * <(&/, a, +5) =/> b> and <(&/, a, +15) =/> b> are specializations of a =/> b
     this can be represented in a SDR (for a fast lookup) and a symbolic representation (for symbolic manipulation)