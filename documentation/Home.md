Welcome to the

<img src="https://repository-images.githubusercontent.com/609092087/f88a3982-702d-4282-834c-d2eebf404426" width="640">

wiki!

## What is QMGsurvey?

QMGsurvey is a _Mathematica_ based program that can be used to analyse quantum matrix geometries, based on the algorithms from [arXiv:2301.10206](https://arxiv.org/abs/2301.10206) (from now on [[1]](https://arxiv.org/abs/2301.10206)).

This Wiki assumes familiarity with the _theory_ and constructions from [[1]](https://arxiv.org/abs/2301.10206). This especially includes the sections 2.2, 2.4 and 3. For further reference, see for example [arXiv:2009.03400](https://arxiv.org/abs/2009.03400) by Harold Steinacker on which the constructions from [[1]](https://arxiv.org/abs/2301.10206) are based.

_"Matrix configurations coming from matrix models comprise many important aspects of modern physics. They represent special quantum spaces and are thus strongly related to noncommutative geometry."_ [[1]](https://arxiv.org/abs/2301.10206).
This package allows to analyze and visualize arbitrary matrix configurations, while a main aspect is the numerical implementation of a quantization map.

## Usage

In order to use QMGsurvey, [Mathematica](https://www.wolfram.com/mathematica/) has to be installed.
QMGsurvey has been developed using Mathematica 13.1.
Download the files QMGsurvey.wl and QMGxX.wl and create a new Mathematica notebook.
Import both packages via

```mathematica
<< "\path\to\file\\QMGsurvey.wl"
<< "\path\to\file\\QMGxX.wl"
```

where "\path\to\file\" is the directory conaining the files.

Alternatively, by downloading the whole package, you can directly execute the **examples** in the folder "examples".
These demonstrate many usecases and give additional information and introduction!

## Documentation


QMGsurvey has been developed using Mathematica 13.1.



The following _data types_ are frequently used:

| Type | Description |
| --- | --- |
| Bool | a boolean (True or False) |
| Int | an integer number|
| Real | a real number |
| Complex | a complex number |
| Bool(k_1,...,k_n) | a boolean array with dimensions {k_1,...,k_n} |
| Int(k_1,...,k_n) | an integer array with dimensions {k_1,...,k_n} |
| Real(k_1,...,k_n) | a real array with dimensions {k_1,...,k_n} |
| Complex(k_1,...,k_n) | a complex array with dimensions {k_1,...,k_n} |
| MatConf(Dim,Nim) | a matrix configuration: X in MatConf(Dim,Nim) equivalent to X in Complex(Dim,Nim,Nim) with X(a,b,c)=X(a,c,b) for all indices a,b,c |
| Fx | a function |
| Color | a Mathematica color |

Sometimes, mixed data types are in use as for example {1,{0.,0.,0.}}. For this we would write {Int,Real(3)}.

The following _standard variables_ occur throughout the package and are therefore collectively described here:
| Variable | Type | Description |
| --- | --- | --- |
| Dim | Real | the (real) dimension of target space |
| Nim | Real | the (complex) dimension of the Hilbert space |
| l | Real | the (even) effective dimension of the quantum manifold |
| x | Real(Dim) | the chosen point in target space |
| X | MatConf(Dim,Nim) | the chosen matrix configuration |

_Optional arguments_ are indicated as in var=1.23 where 1.23 is the default value of var.

For better readability, list specifiers like in
```mathematica
list={0.1,0.2,0.3};
list[[2]]
```
are written with round brackets as in list(2).

The Mathematica package QMGsurvey.wl contains functions presented in:
* [The Functions in QMGsurvey I](https://github.com/laurinjfelder/QMGsurvey/wiki/The-Functions-in-QMGsurvey-I)
* [The Functions in QMGsurvey II](https://github.com/laurinjfelder/QMGsurvey/wiki/The-Functions-in-QMGsurvey-II)
* [The Functions in QMGsurvey III](https://github.com/laurinjfelder/QMGsurvey/wiki/The-Functions-in-QMGsurvey-III)
* [The Functions in QMGsurvey IV](https://github.com/laurinjfelder/QMGsurvey/wiki/The-Functions-in-QMGsurvey-IV)

The last are either internal functions or functions that are mainly there for other functions to build uppon.

This package contains the most important functions that build the core of QMGsurvey.
The Mathematica package QMGxX.wl contains functions presented in:
* [The Functions in QMGxX](https://github.com/laurinjfelder/QMGsurvey/wiki/The-Functions-in-QMGxX)

The functions in this package allow for the construction of certain matrix configurations and points in target space.
The general scheme of compiled functions is explained in:
* [Using Compiled Functions](https://github.com/laurinjfelder/QMGsurvey/wiki/Using-Compiled-Functions)