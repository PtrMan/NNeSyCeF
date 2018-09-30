# NNeSyCeF
NAL and neuro symbolic based core framework

An (aspiring) Artificial General Intelligence framework based on
* "Non axiomatic logic"(NAL) as the logic core and it's truth values for reasoning about uncertainty under "Assumption of Insufficient Knowledge and resources"(AIKR)
* Procedural learning and decision making from ANSNA

## Why was Python chosen as a programming language?

* code is dense enough
* easy to read and change
* fast enough for prototyping
* provides a path for conversion to fast native code with Cython or Shedskin
* allows introduction of C/C++ modules to accelerate core functionality
* mature ecosystem (IDE, Debugger, Profiler, ...)

## Requirements
* python3 environment (anaconda is recommended)
    * cython for "ahead of time" compilation of code which has to be fast without the need of some python features
    * numba for "just in time"(JIT) compilation of code which might change (at runtime) and doesn't have to be fast
    * (numpy) for numerical processing  not jet required
