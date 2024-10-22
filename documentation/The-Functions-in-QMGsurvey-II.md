# Compiled Basic Calculations

## cqmgLightweight\*

### cqmgLightweightINIT
_Initialization function of compiled version of qmgLightweight; cqmgLightweight calculates the lowest eigenstate of the Hamiltonian and the correpsonding eigenvalue at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |

**Output**

The output is cqmgLightweight.

| Output | Type | Description |
| --- | --- | --- |
| cqmgLightweight | Fx | the compiled function |

### cqmgLightweight
_Initialized with qmgLightweightINIT; cqmgLightweight calculates the lowest eigenstate of the Hamiltonian and the correpsonding eigenvalue at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |

cqmgLightweight is _parallelizable_ in the variable x.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(1+2\*Nim) | compressed output. With respect to the output of cqmgLightweightEXTR out=Join[{EigFixed(1)},Re[EigFixed(2)],Im[EigFixed(2)]] |

### cqmgLightweightEXTR
_Extraction function of compiled version of qmgLightweight; cqmgLightweight calculates the lowest eigenstate of the Hamiltonian and the correpsonding eigenvalue at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(1+2\*Nim) | the output of cqmgLightweight |

**Output**

The output is EigFixed.

| Output | Type | Description |
| --- | --- | --- |
| EigFixed | {Real,Complex(Nim)} | EigFixed(1) is the eigenvalue of the Hamiltonian at x. EigFixed(2) is the corresponding normalized eigenvector |


**Description**

cqmgLightweight is a compiled version of qmgLightweight (attention: EigFixed has a different shape for practical reasons).
cqmgLightweight calculates the lowest eigenvalue of the Hamiltonian for a given matrix configuration X at the point x and the corresponding eigenvector.
The eigenvector is normalized and its complex phase is fixed such that the last component (that is Eig(2,Nim,1)) is greater or equal 0.
Attention: In the cases where the last component vanishes, this choice is not unique.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
cqmgLightweight=cqmgLightweightINIT[X];
out=cqmgLightweight[x];
cqmgLightweightEXTR[out]
```

An example using parallelization:
```mathematica
X=qmgXsu2[4];
x1={0,0,1};
x2={0,1,0};
cqmgLightweight=cqmgLightweightINIT[X];
out=cqmgLightweight[{x1,x2}]
```









## cqmgBasic\*

### cqmgBasicINIT
_Initialization function of compiled version of cqmgBasic; cqmgBasic calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and many more important quantities like g, omega, the disperion and others at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |

**Output**

The output is cqmgBasic.

| Output | Type | Description |
| --- | --- | --- |
| cqmgBasic | Fx | the compiled function |

### cqmgBasic
_Initialized with cqmgBasicINIT; cqmgBasic calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and many more important quantities like g, omega, the disperion and others at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |

cqmgBasic is _parallelizable_ in the variable x.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(1+2\*Nim+4\*Dim^2+Dim) | compressed output. With respect to the output of cqmgBasicEXTR out=Join[{EigUnfixed(1)},Re[EigUnfixed(2)],Im[EigUnfixed(2)],Flatten[Re[h]],Flatten[Im[h]],Flatten[theta],xBold,Flatten[xBoldPartial]] |


### cqmgBasicEXTR
_Extraction function of compiled version of cqmgBasic; cqmgBasic calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and many more important quantities like g, omega, the disperion and others at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(1+2\*Nim+4\*Dim^2+Dim) | the output of cqmgBasic |

**Output**

The output is {EigUnfixed,{h,g,omega,theta},{xBold,xBoldPartial}}.

| Output | Type | Description |
| --- | --- | --- |
| EigUnfixed | {Real,Complex(Nim)} | EigUnfixed(1) is the eigenvalue of the Hamiltonian at x. EigUnfixed(2) is the corresponding normalized eigenvector |
| h | Complex(Dim,Dim) | the hermitian form at x |
| g | Real(Dim,Dim) | the quantum metric at x |
| omega | Real(Dim,Dim) | the (would be) symplectic form at x |
| theta | Real(Dim,Dim) | theta at x |
| xBold | Real(Dim) | the embedded point at x, that is the expectation value of the matrix configuration in the quasi-coherent state at x |
| xBoldPartial | Real(Dim,Dim) | A list of all partial derivatives of the embedded point at x (the first index is the index of the derivative) |


**Description**

cqmgBasic is a compiled version of qmgBasic (attention: EigUnfixed has a different shape than EigFixed for practical reasons, further the U(1) phase is not fixed here).
cqmgBasic calculates the most important quantities for a given matrix configuration X at the point x.
We have the relations h=ConjugateTranspose[h] and h=(g-I\*omega)/2 (note that this slightly deviates from [[1]](https://arxiv.org/abs/2301.10206), look at comment 21).
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
cqmgBasic=cqmgBasicINIT[X];
out=cqmgBasic[x];
cqmgBasicEXTR[out]
```












## cqmgEmbedding\*

### cqmgEmbeddingINIT
_Initialization function of compiled version of cqmgEmbedding; cqmgEmbedding calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and xBold at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |

**Output**

The output is cqmgEmbedding.

| Output | Type | Description |
| --- | --- | --- |
| cqmgEmbedding| Fx | the compiled function |

### cqmgEmbedding
_Initialized with cqmgEmbeddingINIT; cqmgEmbedding calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and xBold at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |

cqmgEmbedding is _parallelizable_ in the variable x.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(1+2\*Nim+Dim) | compressed output. With respect to the output of cqmgEmbeddingEXTR out=Join[{EigFixed(1)},Re[EigFixed(2)],Im[EigFixed(2)],xBold] |

### cqmgEmbeddingEXTR
_Extraction function of compiled version of cqmgEmbedding; cqmgEmbedding calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and xBold at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(1+2\*Nim+Dim) | the output of cqmgEmbedding |

**Output**

The output is {EigFixed,xBold}.

| Output | Type | Description |
| --- | --- | --- |
| EigFixed | {Real,Complex(Nim)} | EigFixed(1) is the eigenvalue of the Hamiltonian at x. EigFixed(2) is the corresponding normalized eigenvector |
| xBold | Real(Dim) | the embedded point at x, that is the expectation value of the matrix configuration in the quasi-coherent state at x |


**Description**

cqmgEmbedding is a compiled version of qmgEmbedding (attention: EigFixed has a different shape for practical reasons).
cqmgEmbedding calculates the lowest eigenvalue of the Hamiltonian for a given matrix configuration X at the point x, the corresponding eigenvector and xBold.
The eigenvector is normalized and its complex phase is fixed such that the last component (that is Eig(2,Nim,1)) is greater or equal 0.
Attention: In the cases where the last component vanishes, this choice is not unique.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
cqmgEmbedding=cqmgEmbeddingINIT[X];
out=cqmgEmbedding[x];
cqmgEmbeddingEXTR[out]
```











# Compiled Presentation


## cqmgProject\*

### cqmgProjectINIT
_For internal use; Initialization function of cqmgProject; cqmgProject performs the projection in to R^3 for points in target space, the quantum manifold and the embedded quantum space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| PcalTS | Real(3,Dim) | a matrix of rank three being the composition of a truncated orthogonal matrix and a projector of rank three |
| PcalQM | Real(3,2*Nim) | a matrix of rank three being the composition of a truncated orthogonal matrix and a projector of rank three |

**Output**

The output is cqmgProject.

| Output | Type | Description |
| --- | --- | --- |
| cqmgProject | Fx | the compiled function |

### cqmgProject
_For internal use; Initialized with cqmgProjectINIT; cqmgProject performs the projection in to R^3 for points in target space, the quantum manifold and the embedded quantum space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |
| EigStateVec | Complex(Nim) | a quasi-coherent state, being calculated for x |
| xBold | Real(Dim) | an embedded point, being calculated for x |

cqmgProject is _parallelizable_ in the variables x, EigStateVec and xBold.

**Output**

The output is {PointTS,PointQM,PointEQS}.

| Output | Type | Description |
| --- | --- | --- |
| PointTS | Real(3) | the processed input point in target space |
| PointQM | Real(3) | the processed input point in the quantum manifold |
| PointEQS | Real(3) | the processed input point in the embedded quantum space |


**Description**

cqmgProject projects points in target space, the quantum manifold and the embedded quantum space into $\mathbb{R}^3$. This is very similar to the internal process in the functions qmgPlotTS, qmgPlotQM and qmgPlotEQS. Therefore, PcalTS should be thought of as the composition of Orth and P from qmgPlotTS and PcalQM should be thought of as the composition of Orth and P from qmgPlotQM.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.2.


**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
Dim=Length[X];
Nim=Length[X[[1]]];
(PcalTS=ConstantArray[0,{3,Dim}];PcalTS[[1,1]]=1;PcalTS[[2,2]]=1;PcalTS[[3,3]]=1);
(PcalQM=ConstantArray[0,{3,2*Nim}];PcalQM[[1,1]]=1;PcalQM[[2,2]]=1;PcalQM[[3,Nim+1]]=1);
x={0,0,1};
cqmgEmbedding=cqmgEmbeddingINIT[PcalTS,PcalQM];
out=cqmgEmbedding[x];
{EigFixed,xBold}=cqmgEmbeddingEXTR[out];
cqmgProject=cqmgProjectINIT[PcalTS,PcalQM];
cqmgProject[x,EigFixed[[2]],xBold]
```













## cqmgPlot
_Compiled unified version of qmgPlotTS, qmgPlotQM and qmgPlotEQS. Plots points in target space, the quantum manifold and the embedded quantum space, represented by points in target space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix of rank three being the composition of a truncated orthogonal matrix and a projector of rank three |
| xsets | {{Real(k_1,Dim),Color},...,{Real(k_n,Dim),Color}} | a list of length n consisting of lists. Each of these consists of a list of k_n points in target space (the first index specifies a point) and a color that is used for visualization |
| projectionmode="default" | Str | the mode used to construct the plots. Either "default" or "project". For more information see the description |
| projectionData={} | if projectionmode=="default": {};<br> if projection mode=="project": {Real(3,Dim),Real(3,2*Nim)} | a list of twice a matrix of rank three being the composition of a truncated orthogonal matrix and a projector of rank three. These matrices are called PcalTS and PcalQM |



**Output**

The output is {{PlotTS,PlotQM,PlotEQS},PlotEQS}.

| Output | Type | Description |
| --- | --- | --- |
| PlotTS | Mathematica Plot | a plot in target space of the processed input points |
| PlotQM | Mathematica Plot | a plot in the quantum manifold of the processed input points |
| PlotEQS | Mathematica Plot | a plot in the embedded quantum space of the processed input points |
| PlotEQS | {Real(k_1,3,3),...,Real(k_n,3,3)} | a list of the processed input points, being the result of a parallel computation of cqmgEmbedding composed with cqmgProject |



**Description**


cqmgPlot is a compiled and combined version of qmgPlotTS, qmgPlotQM and qmgPlotEQS. It constructs plots for the target space, the quantum manifold and the embedded quantum space. Two modes are available.
The mode "default" is the same as "project" with
```mathematica
(PcalTS=ConstantArray[0,{3,Dim}];PcalTS[[1,1]]=1;PcalTS[[2,2]]=1;PcalTS[[3,3]]=1);
(PcalQM=ConstantArray[0,{3,2*Nim}];PcalQM[[1,1]]=1;PcalQM[[2,2]]=1;PcalQM[[3,Nim+1]]=1);
```
The mode "project" at first calculates cqmgEmbedding and cqmgProject for each point. The result is then mapped into $\mathbb{R}^3$ using PcalTS, PcalQM and PcalTS respectively.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.2.


**Example(s)**

An example for a "default" plot:
```mathematica
X=qmgXsu2[4];
xs1=qmgxRandomBall[3,1,5000];
xs2=Table[{0,0,1+a/100},{a,10}];
color1=Red;
color2=Green;
xsets={{xs1,color1},{xs2,color2}};
cqmgPlot[X,xsets]
```

An example for a "project" plot:
```mathematica
X=qmgXsu2[4];
xs1=qmgxRandomBall[3,1,5000];
xs2=Table[{0,0,1+a/100},{a,10}];
color1=Red;
color2=Green;
xsets={{xs1,color1},{xs2,color2}};
projectionmode="project";
(PcalTS=ConstantArray[0,{3,Dim}];PcalTS[[1,1]]=1;PcalTS[[2,2]]=1;PcalTS[[3,3]]=1);
(PcalQM=ConstantArray[0,{3,2*Nim}];PcalQM[[1,1]]=1;PcalQM[[2,2]]=1;PcalQM[[3,Nim+1]]=1);
projectiondata={PcalTS,PcalQM};
cqmgPlot[X,xsets,projectionmode,projectiondata]
```








# Compiled Basic Calculations Specialized to Leaves and for Kähler Cost

## cqmgTSleafBasic\*

### cqmgTSleafBasicINIT
_Initialization function of cqmgTSleafBasic, a specialized version of cqmgBasic; cqmgTSleafBasic calculates the lowest eigenstate of the Hamiltonian, theta and xBold for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |

**Output**

The output is cqmgTSleafBasic.

| Output | Type | Description |
| --- | --- | --- |
| cqmgTSleafBasic | Fx | the compiled function |

### cqmgTSleafBasic
_Initialized with cqmgTSleafBasicINIT, a specialized version of cqmgBasic; cqmgTSleafBasic calculates the lowest eigenstate of the Hamiltonian, theta and xBold for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |

cqmgTSleafBasic is _parallelizable_ in the variable x.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2\*Nim+Dim^2+Dim) | compressed output. With respect to the output of cqmgTSleafBasicEXTR out=Join[Re[EigState],Im[EigState],Flatten[theta],xBold] |

### cqmgTSleafBasicEXTR
_Extraction function of cqmgTSleafBasic, a specialized version of cqmgBasic; cqmgTSleafBasic calculates the lowest eigenstate of the Hamiltonian, theta and xBold for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2\*Nim+Dim^2+Dim) | the output of cqmgTSleafBasic |

**Output**

The output is {EigState,theta,xBold}.

| Output | Type | Description |
| --- | --- | --- |
| EigState | Complex(Nim) | EigState is the quasi-coherent state at x |
| theta | Real(Dim,Dim) | theta at x |
| xBold | Real(Dim) | the embedded point at x, that is the expectation value of the matrix configuration in the quasi-coherent state at x |


**Description**

cqmgTSleafBasic is a reduced version of cqmgBasic that is well adapted to calculations in target space and the embedded quantum space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
cqmgTSleafBasic=cqmgTSleafBasicINIT[X];
out=cqmgTSleafBasic[x];
cqmgTSleafBasicEXTR[out]
```












## cqmgQMleafBasic\*

### cqmgQMleafBasicINIT
_Initialization function of cqmgQMleafBasic, a specialized version of cqmgBasic; cqmgQMleafBasic calculates the lowest eigenstate of the Hamiltonian, omega and xBold for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |

**Output**

The output is cqmgQMleafBasic.

| Output | Type | Description |
| --- | --- | --- |
| cqmgQMleafBasic | Fx | the compiled function |

### cqmgQMleafBasic
_Initialized with cqmgQMleafBasicINIT, a specialized version of cqmgBasic; cqmgQMleafBasic calculates the lowest eigenstate of the Hamiltonian, omega and xBold for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |

cqmgQMleafBasic is _parallelizable_ in the variable x.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2\*Nim+Dim^2+Dim) | compressed output. With respect to the output of cqmgQMleafBasicEXTR out=Join[Re[EigState],Im[EigState],Flatten[omega],xBold] |

### cqmgQMleafBasicEXTR
_Extraction function of cqmgQMleafBasic, a specialized version of cqmgBasic; cqmgQMleafBasic calculates the lowest eigenstate of the Hamiltonian, omega and xBold for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2\*Nim+Dim^2+Dim) | the output of cqmgQMleafBasic |

**Output**

The output is {EigState,omega,xBold}.

| Output | Type | Description |
| --- | --- | --- |
| EigState | Complex(Nim) | EigState is the quasi-coherent state at x |
| omega | Real(Dim,Dim) | the (would be) symplectic form at x |
| xBold | Real(Dim) | the embedded point at x, that is the expectation value of the matrix configuration in the quasi-coherent state at x |


**Description**

cqmgQMleafBasic is a reduced version of cqmgBasic that is well adapted to calculations in the quantum manifold.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
cqmgQMleafBasic=cqmgQMleafBasicINIT[X];
out=cqmgQMleafBasic[x];
cqmgQMleafBasicEXTR[out]
```













## cqmgGQMleafBasic\*

### cqmgGQMleafBasicINIT
_Initialization function of cqmgGQMleafBasic, a specialized version of cqmgBasic; cqmgGQMleafBasic calculates the lowest eigenstate of the Hamiltonian, g and xBold for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |

**Output**

The output is cqmgGQMleafBasic.

| Output | Type | Description |
| --- | --- | --- |
| cqmgGQMleafBasic | Fx | the compiled function |

### cqmgGQMleafBasic
_Initialized with cqmgGQMleafBasicINIT, a specialized version of cqmgBasic; cqmgGQMleafBasic calculates the lowest eigenstate of the Hamiltonian, g and xBold for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |

cqmgGQMleafBasic is _parallelizable_ in the variable x.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2\*Nim+Dim^2+Dim) | compressed output. With respect to the output of cqmgGQMleafBasic out=Join[Re[EigState],Im[EigState],Flatten[g],xBold] |

### cqmgGQMleafBasicEXTR
_Extraction function of cqmgGQMleafBasic, a specialized version of cqmgBasic; cqmgGQMleafBasic calculates the lowest eigenstate of the Hamiltonian, g and xBold for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2\*Nim+Dim^2+Dim) | the output of cqmgGQMleafBasic |

**Output**

The output is {EigState,g,xBold}.

| Output | Type | Description |
| --- | --- | --- |
| EigState | Complex(Nim) | EigState is the quasi-coherent state at x |
| g | Real(Dim,Dim) | the quantum metric at x |
| xBold | Real(Dim) | the embedded point at x, that is the expectation value of the matrix configuration in the quasi-coherent state at x |


**Description**

cqmgGQMleafBasic is a reduced version of cqmgBasic that is well adapted to calculations in the quantum manifold.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
cqmgGQMleafBasic=cqmgGQMleafBasicINIT[X];
out=cqmgGQMleafBasic[x];
cqmgGQMleafBasicEXTR[out]
```












## cqmgGQMleafBasic\*

### cqmgKaehlerBasicINIT
_Initialization function of cqmgKaehlerBasic, a specialized version of cqmgBasic; cqmgKaehlerBasic calculates the lowest eigenstate of the Hamiltonian and CovDer for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |

**Output**

The output is cqmgGQMleafBasic.

| Output | Type | Description |
| --- | --- | --- |
| cqmgKaehlerBasic | Fx | the compiled function |

### cqmgKaehlerBasic
_Initialized with cqmgKaehlerBasicINIT, a specialized version of cqmgBasic; cqmgKaehlerBasic calculates the lowest eigenstate of the Hamiltonian and CovDer for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |

cqmgKaehlerBasic is _parallelizable_ in the variable x.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2\*Nim+2\*Dim\*Nim) | compressed output. With respect to the output of cqmgKaehlerBasic out=Join[Re[EigState],Im[EigState],Flatten[Re[CovDer]],Flatten[Im[CovDer]]] |

### cqmgKaehlerBasicEXTR
_Extraction function of cqmgKaehlerBasic, a specialized version of cqmgBasic; cqmgKaehlerBasic calculates the lowest eigenstate of the Hamiltonian and CovDer for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2\*Nim+2\*Dim\*Nim) | the output of cqmgKaehlerBasic |

**Output**

The output is {EigState,CovDer}.

| Output | Type | Description |
| --- | --- | --- |
| EigState | Complex(Nim) | EigState is the quasi-coherent state at x |
| CovDer | Complex(Dim,Nim) | the covariant derivative of the quasi-coherent state (the first index is the index of the derivative) |

**Description**

cqmgKaehlerBasic is a reduced version of cqmgBasic that is well adapted to calculations of the Kähler cost.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
cqmgKaehlerBasic=cqmgKaehlerBasicINIT[X];
out=cqmgKaehlerBasic[x];
cqmgKaehlerBasicEXTR[out]
```









