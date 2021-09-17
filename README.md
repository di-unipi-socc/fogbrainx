<p><img align="left"  src="http://pages.di.unipi.it/forti/FogBrainX/img/logo.png" width="100"> <h1>FogBrainXX</h1></p>

_continuous reasoning for managing next-gen Cloud-IoT applications in continuity with the CI/CD pipeline_

<br></br>

FogBrainX methodology is described in the following article:

> [Stefano Forti](http://pages.di.unipi.it/forti), [Giuseppe Bisicchia](), [Antonio Brogi](http://pages.di.unipi.it/brogi)<br>
> [**Declarative Continuous Reasoning in the Cloud-IoT Continuum**](), <br>	
> *Under revision*

If you wish to reuse source code in this repo, please consider citing it.

## Background & Prerequisites

FogBrainX is written in Prolog. Prolog programs are finite sets of *clauses* of the form:

```prolog
a :- b1, ... , bn.
```

stating that `a` holds when `b1` and ... and `bn` holds, where `n >= 0` and `a`, `b1` ..., `bn` are atomic literals. Clauses with empty condition are also called *facts*. Prolog variables begin with upper-case letters, lists are denoted by square brackets, and negation by `\+`.

Before using **FogBrainXX** you need to install the latest stable release of [SWI-Prolog](https://www.swi-prolog.org/download/stable).