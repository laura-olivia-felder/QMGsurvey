# Compiled Distributions

## cqmgTSleafDistribution\*

### cqmgTSleafDistributionINIT
_Initialization function of cqmgTSleafDistribution; cqmgTSleafDistribution calculates the target space based distribution for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgTSleafDistribution.

| Output | Type | Description |
| --- | --- | --- |
| cqmgTSleafDistribution | Fx | the compiled function |

### cqmgTSleafDistribution
_Initialized with cqmgTSleafDistributionINIT; cqmgTSleafDistribution calculates the target space based distribution for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |

cqmgTSleafDistribution is _parallelizable_ in the variable x.

**Output**

The output is dist.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(l,Dim) | compressed output. With respect to the output of cqmgTSleafDistributionEXTR out=dist |

### cqmgTSleafDistributionEXTR
_Extraction function of cqmgTSleafDistribution; cqmgTSleafDistribution calculates the target space based distribution for a given point._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(l,Dim) | the output of cqmgTSleafDistribution |

**Output**

The output is dist.

| Output | Type | Description |
| --- | --- | --- |
| dist | Real(l,Dim) | dist is a list containing vectors that span the distribution of the hybrid leaf (using theta) in target space |


**Description**

cqmgTSleafDistribution calculates the distribution of the hybrid leaf (using theta) in target space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
cqmgTSleafDistribution=cqmgTSleafDistributionINIT[X,l];
out=cqmgTSleafDistribution[x];
cqmgTSleafDistributionEXTR[out]
```













## cqmgQMleafDistribution\*

### cqmgQMleafDistributionINIT
_Initialization function of cqmgQMleafDistribution; cqmgQMleafDistribution calculates the quantum manifold based distribution for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgQMleafDistribution.

| Output | Type | Description |
| --- | --- | --- |
| cqmgQMleafDistribution | Fx | the compiled function |

### cqmgQMleafDistribution
_Initialized with cqmgQMleafDistributionINIT; cqmgQMleafDistribution calculates the quantum manifold based distribution for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |

cqmgQMleafDistribution is _parallelizable_ in the variable x.

**Output**

The output is dist.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(l,Dim) | compressed output. With respect to the output of cqmgQMleafDistributionEXTR out=dist |

### cqmgQMleafDistributionEXTR
_Extraction function of cqmgQMleafDistribution; cqmgQMleafDistribution calculates the quantum manifold based distribution for a given point._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(l,Dim) | the output of cqmgQMleafDistribution |

**Output**

The output is dist.

| Output | Type | Description |
| --- | --- | --- |
| dist | Real(l,Dim) | dist is a list containing vectors that span the distribution of the hybrid leaf using omega in target space |


**Description**

cqmgQMleafDistribution calculates the distribution of the hybrid leaf using omega in target space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
cqmgQMleafDistribution=cqmgQMleafDistributionINIT[X,l];
out=cqmgQMleafDistribution[x];
cqmgQMleafDistributionEXTR[out]
```














## cqmgGQMleafDistribution\*

### cqmgGQMleafDistributionINIT
_Initialization function of cqmgGQMleafDistribution; cqmgGQMleafDistribution calculates the quantum manifold and g based distribution for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgGQMleafDistribution.

| Output | Type | Description |
| --- | --- | --- |
| cqmgGQMleafDistribution | Fx | the compiled function |

### cqmgGQMleafDistribution
_Initialized with cqmgGQMleafDistributionINIT; cqmgGQMleafDistribution calculates the quantum manifold and g based distribution for a given point._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |

cqmgGQMleafDistribution is _parallelizable_ in the variable x.

**Output**

The output is dist.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(l,Dim) | compressed output. With respect to the output of cqmgGQMleafDistributionEXTR out=dist |

### cqmgGQMleafDistributionEXTR
_Extraction function of cqmgGQMleafDistribution; cqmgGQMleafDistribution calculates the quantum manifold and g based distribution for a given point._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(l,Dim) | the output of cqmgGQMleafDistribution |

**Output**

The output is dist.

| Output | Type | Description |
| --- | --- | --- |
| dist | Real(l,Dim) | dist is a list containing vectors that span the distribution of the hybrid leaf using omega and g in target space |


**Description**

cqmgQMleafDistribution calculates the distribution of the hybrid leaf using omega and g in target space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
cqmgGQMleafDistribution=cqmgGQMleafDistributionINIT[X,l];
out=cqmgGQMleafDistribution[x];
cqmgGQMleafDistributionEXTR[out]
```














## cqmgDistribution
_Unification of cqmgTSleafDistribution, cqmgQMleafDistribution and cqmgGQMleafDistribution; calculates the distribution for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | a point in target space |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |

cqmgDistribution is _parallelizable_ in the variable x.

**Output**

The output is dist.

| Output | Type | Description |
| --- | --- | --- |
| dist | Real(l,Dim) | dist is a list containing vectors that span the distribution of the chosen leaf in target space |


**Description**

cqmgDistribution is a unification of cqmgTSleafDistribution\*, cqmgQMleafDistribution\* and cqmgGQMleafDistribution\*.
cqmgDistribution calculates the distribution of the chosen leaf in target space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
leaf="QMleaf";
cqmgDistribution[X,x,l,leaf]
```














# Compiled Curve Integration

## cqmgTSleafCurveIteration\*

### cqmgTSleafCurveIterationINIT
_Initialization function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the target space based distribution._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgTSleafCurveIteration.

| Output | Type | Description |
| --- | --- | --- |
| cqmgTSleafCurveIteration | Fx | the compiled function |

### cqmgTSleafCurveIteration
_Initialized with cqmgTSleafCurveIterationINIT; cqmgTSleafCurveIteration calculates a step in the target space based distribution._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite step length |

cqmgTSleafCurveIteration is _parallelizable_ in the variables x, v and delta.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | compressed output. With respect to the output of cqmgTSleafCurveIterationEXTR out={vPrime,xNew} |

### cqmgTSleafCurveIterationEXTR
_Extraction function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the target space based distribution._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | the output of cqmgTSleafCurveIteration |

**Output**

The output is {vPrime,xNew}.

| Output | Type | Description |
| --- | --- | --- |
| vPrime | Real(Dim) | vPrime is the projection of the initial tangent vector v into the distribution, normalized to length delta |
| xNew | Real(Dim) | xNew=x+vPrime is the new point after a finite step with vPrime |


**Description**

cqmgTSleafCurveIteration calculates a finite step in the hybrid leaf (using theta) in target space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=0.01;
cqmgTSleafCurveIteration=cqmgTSleafCurveIterationINIT[X,l];
out=cqmgTSleafCurveIteration[x,v,delta];
cqmgTSleafCurveIterationEXTR[out]
```
















## cqmgQMleafCurveIteration\*

### cqmgQMleafCurveIterationINIT
_Initialization function of cqmgQMleafCurveIteration; cqmgQMleafCurveIteration calculates a step in the quantum manifold based distribution._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgQMleafCurveIteration.

| Output | Type | Description |
| --- | --- | --- |
| cqmgQMleafCurveIteration | Fx | the compiled function |

### cqmgQMleafCurveIteration
_Initialized with cqmgQMleafCurveIterationINIT; cqmgQMleafCurveIteration calculates a step in the quantum manifold based distribution._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite step length |

cqmgQMleafCurveIteration is _parallelizable_ in the variables x, v and delta.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | compressed output. With respect to the output of cqmgQMleafCurveIterationEXTR out={vPrime,xNew} |

### cqmgQMleafCurveIterationEXTR
_Extraction function of cqmgQMleafCurveIteration; cqmgQMleafCurveIteration calculates a step in the quantum manifold based distribution._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | the output of cqmgQMleafCurveIteration |

**Output**

The output is {vPrime,xNew}.

| Output | Type | Description |
| --- | --- | --- |
| vPrime | Real(Dim) | vPrime is the projection of the initial tangent vector v into the distribution, normalized to length delta |
| xNew | Real(Dim) | xNew=x+vPrime is the new point after a finite step with vPrime |


**Description**

cqmgQMleafCurveIteration calculates a finite step in the hybrid leaf using omega in target space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=0.01;
cqmgQMleafCurveIteration=cqmgQMleafCurveIterationINIT[X,l];
out=cqmgQMleafCurveIteration[x,v,delta];
cqmgQMleafCurveIterationEXTR[out]
```














## cqmgGQMleafCurveIteration\*

### cqmgGQMleafCurveIterationINIT
_Initialization function of cqmgGQMleafCurveIteration; cqmgGQMleafCurveIteration calculates a step in the quantum manifold and g based distribution._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgGQMleafCurveIteration.

| Output | Type | Description |
| --- | --- | --- |
| cqmgGQMleafCurveIteration | Fx | the compiled function |

### cqmgGQMleafCurveIteration
_Initialized with cqmgGQMleafCurveIterationINIT; cqmgGQMleafCurveIteration calculates a step in the quantum manifold and g based distribution._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite step length |

cqmgGQMleafCurveIteration is _parallelizable_ in the variables x, v and delta.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | compressed output. With respect to the output of cqmgGQMleafCurveIterationEXTR out={vPrime,xNew} |

### cqmgGQMleafCurveIterationEXTR
_Extraction function of cqmgGQMleafCurveIteration; cqmgGQMleafCurveIteration calculates a step in the quantum manifold and g based distribution._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | the output of cqmgGQMleafCurveIteration |

**Output**

The output is {vPrime,xNew}.

| Output | Type | Description |
| --- | --- | --- |
| vPrime | Real(Dim) | vPrime is the projection of the initial tangent vector v into the distribution, normalized to length delta |
| xNew | Real(Dim) | xNew=x+vPrime is the new point after a finite step with vPrime |


**Description**

cqmgGQMleafCurveIteration calculates a finite step in the hybrid leaf using omega and g in target space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=0.01;
cqmgGQMleafCurveIteration=cqmgGQMleafCurveIterationINIT[X,l];
out=cqmgGQMleafCurveIteration[x,v,delta];
cqmgGQMleafCurveIterationEXTR[out]
```








## cqmgCurveIntegration
_Unified function; calculates a curve in the chosen distribution._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite step length |
| n | Int | the (positive) number of steps (initial point excluded) |
| m | Int | the (positive) number of intermediate steps (zero means no intermediate steps). These increase the numerical precision, they do not affect the length of the curve |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |
| vFix=False | Bool | True: use the initial v at each step; False: use vPrime from the last step as |

cqmgCurveIntegration is partially _parallelizable_ in the variables x and v (this means lists of initial points and initial tangent vectors but no higher nesting can be handled).

**Output**

The output is xs.

| Output | Type | Description |
| --- | --- | --- |
| xs | Real(n+1,Dim) | xs is a list of all points of the discrete curve in the chosen leaf, xs(1) is the initial point x |


**Description**

cqmgCurveIntegration is an iterated unification of cqmgTSleafCurveIteration\*, cqmgQMleafCurveIteration\* and cqmgGQMleafCurveIteration\*.
cqmgDistribution calculates a discrete curve with initial point x, initial tangent vector v and step length delta in the chosen leaf.
This is done by iterating cqmgTSleafCurveIteration\*, cqmgQMleafCurveIteration\* or cqmgGQMleafCurveIteration\*.
If vFix=False the iteration uses vPrime from the last step as the new v in every step; if vFix=False v is used allways.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=0.01;
n=1000;
m=3;
leaf="QMleaf";
cqmgCurveIntegration[X,x,v,delta,n,m,l,leaf]
```

An example for the fuzzy sphere with fixed initial tangent vector:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=0.01;
n=1000;
m=3;
leaf="QMleaf";
cqmgCurveIntegration[X,x,v,delta,n,m,l,leaf,True]
```

An example with parallelization:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v1={0,1,0};
v2={1,0,0};
vs={v1,v2};
delta=0.01;
n=1000;
m=3;
leaf="QMleaf";
cqmgCurveIntegration[X,x,vs,delta,n,m,l,leaf,True]
```







# Compiled Null Space Curve Integration

## cqmgTSleafCurveIterationNull\*

### cqmgTSleafCurveIterationNullINIT
_Initialization function of cqmgTSleafCurveIterationNull; cqmgTSleafCurveIterationNull calculates a step in the target space based distribution in orthogonal (null) direction._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgTSleafCurveIterationNull.

| Output | Type | Description |
| --- | --- | --- |
| cqmgTSleafCurveIterationNull | Fx | the compiled function |

### cqmgTSleafCurveIterationNull
_Initialized with cqmgTSleafCurveIterationNullINIT; cqmgTSleafCurveIterationNull calculates a step in the target space based distribution in orthogonal (null) direction._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite step length |

cqmgTSleafCurveIterationNull is _parallelizable_ in the variables x, v and delta.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | compressed output. With respect to the output of cqmgTSleafCurveIterationNullEXTR out={vPrime,xNew} |

### cqmgTSleafCurveIterationNullEXTR
_Extraction function of cqmgTSleafCurveIterationNull; cqmgTSleafCurveIterationNull calculates a step in the target space based distribution in orthogonal (null) direction._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | the output of cqmgTSleafCurveIterationNull |

**Output**

The output is {vPrime,xNew}.

| Output | Type | Description |
| --- | --- | --- |
| vPrime | Real(Dim) | vPrime is the projection of the initial tangent vector v into the orthogonal distribution, normalized to length delta |
| xNew | Real(Dim) | xNew=x+vPrime is the new point after a finite step with vPrime |


**Description**

cqmgTSleafCurveIterationNull calculates a finite step in the orthognal direction to the hybrid leaf (using theta) (thus the null leaf) in target space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=0.01;
cqmgTSleafCurveIterationNull=cqmgTSleafCurveIterationNullINIT[X,l];
out=cqmgTSleafCurveIterationNull[x,v,delta];
cqmgTSleafCurveIterationNullEXTR[out]
```














## cqmgQMleafCurveIterationNull\*

### cqmgQMleafCurveIterationNullINIT
_Initialization function of cqmgQMleafCurveIterationNull; cqmgQMleafCurveIterationNull calculates a step in the quantum manifold based distribution in orthogonal (null) direction._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgQMleafCurveIterationNull.

| Output | Type | Description |
| --- | --- | --- |
| cqmgQMleafCurveIterationNull | Fx | the compiled function |

### cqmgQMleafCurveIterationNull
_Initialized with cqmgQMleafCurveIterationNullINIT; cqmgQMleafCurveIterationNull calculates a step in the quantum manifold based distribution in orthogonal (null) direction._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite step length |

cqmgQMleafCurveIterationNull is _parallelizable_ in the variables x, v and delta.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | compressed output. With respect to the output of cqmgQMleafCurveIterationNullEXTR out={vPrime,xNew} |

### cqmgTSleafCurveIterationNullEXTR
_Extraction function of cqmgQMleafCurveIterationNull; cqmgQMleafCurveIterationNull calculates a step in the quantum manifold based distribution in orthogonal (null) direction._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | the output of cqmgQMleafCurveIterationNull |

**Output**

The output is {vPrime,xNew}.

| Output | Type | Description |
| --- | --- | --- |
| vPrime | Real(Dim) | vPrime is the projection of the initial tangent vector v into the orthogonal distribution, normalized to length delta |
| xNew | Real(Dim) | xNew=x+vPrime is the new point after a finite step with vPrime |


**Description**

cqmgQMleafCurveIterationNull calculates a finite step in the orthognal direction to the hybrid leaf using omega (thus the null leaf) in target space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=0.01;
cqmgQMleafCurveIterationNull=cqmgQMleafCurveIterationNullINIT[X,l];
out=cqmgQMleafCurveIterationNull[x,v,delta];
cqmgQMleafCurveIterationNullEXTR[out]
```


















## cqmgGQMleafCurveIterationNull\*

### cqmgGQMleafCurveIterationNullINIT
_Initialization function of cqmgGQMleafCurveIterationNull; cqmgGQMleafCurveIterationNull calculates a step in the quantum manifold and g based distribution in orthogonal (null) direction._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgGQMleafCurveIterationNull.

| Output | Type | Description |
| --- | --- | --- |
| cqmgGQMleafCurveIterationNull | Fx | the compiled function |

### cqmgGQMleafCurveIterationNull
_Initialized with cqmgGQMleafCurveIterationNullINIT; cqmgGQMleafCurveIterationNull calculates a step in the quantum manifold and g based distribution in orthogonal (null) direction._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite step length |

cqmgGQMleafCurveIterationNull is _parallelizable_ in the variables x, v and delta.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | compressed output. With respect to the output of cqmgGQMleafCurveIterationNullEXTR out={vPrime,xNew} |

### cqmgGQMleafCurveIterationNullEXTR
_Extraction function of cqmgGQMleafCurveIterationNull; cqmgGQMleafCurveIterationNull calculates a step in the quantum manifold and g based distribution in orthogonal (null) direction._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | the output of cqmgGQMleafCurveIterationNull |

**Output**

The output is {vPrime,xNew}.

| Output | Type | Description |
| --- | --- | --- |
| vPrime | Real(Dim) | vPrime is the projection of the initial tangent vector v into the orthogonal distribution, normalized to length delta |
| xNew | Real(Dim) | xNew=x+vPrime is the new point after a finite step with vPrime |


**Description**

cqmgGQMleafCurveIterationNull calculates a finite step in the orthognal direction to the hybrid leaf using omega and g (thus the null leaf) in target space.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=0.01;
cqmgGQMleafCurveIterationNull=cqmgGQMleafCurveIterationNullINIT[X,l];
out=cqmgGQMleafCurveIterationNull[x,v,delta];
cqmgGQMleafCurveIterationNullEXTR[out]
```















## cqmgCurveIntegrationNull
_Unified function; calculates a curve in the chosen distribution in orthogonal (null) direction._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite step length |
| n | Int | the (positive) number of steps (initial point excluded) |
| m | Int | the (positive) number of intermediate steps (zero means no intermediate steps). These increase the numerical precision, they do not affect the length of the curve |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |
| vFix=False | Bool | True: use the initial v at each step; False: use vPrime from the last step as |

cqmgCurveIntegrationNull is partially _parallelizable_ in the variables x and v (this means lists of initial points and initial tangent vectors but no higher nesting can be handled).

**Output**

The output is xs.

| Output | Type | Description |
| --- | --- | --- |
| xs | Real(n+1,Dim) | xs is a list of all points of the discrete curve in the null leaf of the chosen leaf, xs(1) is the initial point x |


**Description**

cqmgCurveIntegrationNull is an iterated unification of cqmgTSleafCurveIterationNull\*, cqmgQMleafCurveIterationNull\* and cqmgGQMleafCurveIterationNull\*.
cqmgCurveIntegrationNull calculates a discrete curve with initial point x, initial tangent vector v and step length delta in the null leaf corresponding to the chosen leaf.
This is done by iterating cqmgTSleafCurveIterationNull\*, cqmgQMleafCurveIterationNull\* or cqmgGQMleafCurveIterationNull\*.
If vFix=False the iteration uses vPrime from the last step as the new v in every step; if vFix=False v is used allways.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=0.01;
n=1000;
m=3;
leaf="QMleaf";
cqmgCurveIntegrationNull[X,x,v,delta,n,m,l,leaf]
```

An example for the fuzzy sphere with fixed initial tangent vector:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=0.01;
n=1000;
m=3;
leaf="QMleaf";
cqmgCurveIntegrationNull[X,x,v,delta,n,m,l,leaf,True]
```

An example with parallelization:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v1={0,1,0};
v2={1,0,0};
vs={v1,v2};
delta=0.01;
n=1000;
m=3;
leaf="QMleaf";
cqmgCurveIntegrationNull[X,x,vs,delta,n,m,l,leaf,True]
```











# Compiled Minimization of Lambda

## cqmgTSleafCurveIterationAdaptiveNull\*

### cqmgTSleafCurveIterationAdaptiveNullINIT
_Initialization function of cqmgTSleafCurveIterationAdaptiveNull; cqmgTSleafCurveIterationAdaptiveNull calculates a step in the target space based distribution in orthogonal (null) direction and with variable length._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgTSleafCurveIterationAdaptiveNull.

| Output | Type | Description |
| --- | --- | --- |
| cqmgTSleafCurveIterationAdaptiveNull | Fx | the compiled function |

### cqmgTSleafCurveIterationAdaptiveNull
_Initialized with cqmgTSleafCurveIterationAdaptiveNullINIT; cqmgTSleafCurveIterationAdaptiveNull calculates a step in the target space based distribution in orthogonal (null) direction and with variable length._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite relative step length |

cqmgTSleafCurveIterationAdaptiveNull is _parallelizable_ in the variables x, v and delta.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | compressed output. With respect to the output of cqmgTSleafCurveIterationAdaptiveNullEXTR out={vPrime,xNew} |

### cqmgTSleafCurveIterationAdaptiveNullEXTR
_Extraction function of cqmgTSleafCurveIterationAdaptiveNull; cqmgTSleafCurveIterationAdaptiveNull calculates a step in the target space based distribution in orthogonal (null) direction and with variable length._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | the output of cqmgTSleafCurveIterationAdaptiveNull |

**Output**

The output is {vPrime,xNew}.

| Output | Type | Description |
| --- | --- | --- |
| vPrime | Real(Dim) | vPrime is the projection of the initial tangent vector v into the orthogonal distribution, multiplied with delta |
| xNew | Real(Dim) | xNew=x+vPrime is the new point after a finite step with vPrime |


**Description**

cqmgTSleafCurveIterationAdaptiveNull calculates a finite step in the orthognal direction to the hybrid leaf (using theta) (thus the null leaf) in target space. Here, vPrime is not normalized to delta but is the projection of v multiplied with delta.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=1;
cqmgTSleafCurveIterationAdaptiveNull=cqmgTSleafCurveIterationAdaptiveNullINIT[X,l];
out=cqmgTSleafCurveIterationAdaptiveNull[x,v,delta];
cqmgTSleafCurveIterationAdaptiveNullEXTR[out]
```














## cqmgQMleafCurveIterationAdaptiveNull\*

### cqmgQMleafCurveIterationAdaptiveNullINIT
_Initialization function of cqmgQMleafCurveIterationAdaptiveNull; cqmgQMleafCurveIterationAdaptiveNull calculates a step in the quantum manifold based distribution in orthogonal (null) direction and with variable length._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgQMleafCurveIterationAdaptiveNull.

| Output | Type | Description |
| --- | --- | --- |
| cqmgQMleafCurveIterationAdaptiveNull | Fx | the compiled function |

### cqmgQMleafCurveIterationAdaptiveNull
_Initialized with cqmgQMleafCurveIterationAdaptiveNullINIT; cqmgQMleafCurveIterationAdaptiveNullcalculates a step in the quantum manifold based distribution in orthogonal (null) direction and with variable length._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite relative step length |

cqmgQMleafCurveIterationAdaptiveNull is _parallelizable_ in the variables x, v and delta.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | compressed output. With respect to the output of cqmgQMleafCurveIterationAdaptiveNullEXTR out={vPrime,xNew} |

### cqmgQMleafCurveIterationAdaptiveNullEXTR
_Extraction function of cqmgQMleafCurveIterationAdaptiveNull; cqmgQMleafCurveIterationAdaptiveNull calculates a step in the quantum manifold based distribution in orthogonal (null) direction and with variable length._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | the output of cqmgQMleafCurveIterationAdaptiveNull |

**Output**

The output is {vPrime,xNew}.

| Output | Type | Description |
| --- | --- | --- |
| vPrime | Real(Dim) | vPrime is the projection of the initial tangent vector v into the orthogonal distribution, multiplied with delta |
| xNew | Real(Dim) | xNew=x+vPrime is the new point after a finite step with vPrime |


**Description**

cqmgQMleafCurveIterationAdaptiveNull calculates a finite step in the orthognal direction to the hybrid leaf using omega (thus the null leaf) in target space. Here, vPrime is not normalized to delta but is the projection of v multiplied with delta.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=1;
cqmgQMleafCurveIterationAdaptiveNull=cqmgQMleafCurveIterationAdaptiveNullINIT[X,l];
out=cqmgQMleafCurveIterationAdaptiveNull[x,v,delta];
cqmgQMleafCurveIterationAdaptiveNullEXTR[out]
```
















## cqmgGQMleafCurveIterationAdaptiveNull\*

### cqmgGQMleafCurveIterationAdaptiveNullINIT
_Initialization function of cqmgGQMleafCurveIterationAdaptiveNull; cqmgGQMleafCurveIterationAdaptiveNull calculates a step in the quantum manifold and g based distribution in orthogonal (null) direction and with variable length._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgGQMleafCurveIterationAdaptiveNull.

| Output | Type | Description |
| --- | --- | --- |
| cqmgGQMleafCurveIterationAdaptiveNull | Fx | the compiled function |

### cqmgGQMleafCurveIterationAdaptiveNull
_Initialized with cqmgGQMleafCurveIterationAdaptiveNullINIT; cqmgGQMleafCurveIterationAdaptiveNull calculates a step in the quantum manifold and g based distribution in orthogonal (null) direction and with variable length._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | an initial point in target space |
| v | Real(Dim) | an initial tangent vector in target space |
| delta | Real | a positive finite relative step length |

cqmgGQMleafCurveIterationAdaptiveNull is _parallelizable_ in the variables x, v and delta.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | compressed output. With respect to the output of cqmgGQMleafCurveIterationAdaptiveNullEXTR out={vPrime,xNew} |

### cqmgGQMleafCurveIterationAdaptiveNullEXTR
_Extraction function of cqmgGQMleafCurveIterationAdaptiveNull; cqmgGQMleafCurveIterationAdaptiveNull calculates a step in the quantum manifold and g based distribution in orthogonal (null) direction and with variable length._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real(2,Dim) | the output of cqmgGQMleafCurveIterationAdaptiveNull |

**Output**

The output is {vPrime,xNew}.

| Output | Type | Description |
| --- | --- | --- |
| vPrime | Real(Dim) | vPrime is the projection of the initial tangent vector v into the orthogonal distribution, multiplied with delta |
| xNew | Real(Dim) | xNew=x+vPrime is the new point after a finite step with vPrime |


**Description**

cqmgGQMleafCurveIterationAdaptiveNull calculates a finite step in the orthognal direction to the hybrid leaf using omega and g (thus the null leaf) in target space. Here, vPrime is not normalized to delta but is the projection of v multiplied with delta.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
v={0,1,0};
delta=1;
cqmgGQMleafCurveIterationAdaptiveNull=cqmgGQMleafCurveIterationAdaptiveNullINIT[X,l];
out=cqmgGQMleafCurveIterationAdaptiveNull[x,v,delta];
cqmgGQMleafCurveIterationAdaptiveNullEXTR[out]
```















## cqmgMinimizeLambda
_Unified function; finds the minimum of lambda in the orthogonal direction to the chosen distribution._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | an initial point in target space |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |
| delta=1 | Real | a positive finite relative step length |
| nMax=100 | Int | the (positive) maximal number of steps (initial point excluded) |
| epsilon=10^-8 | Real | the (positive) numerical tolerance for the minimum |


cqmgMinimizeLambda is _not parallelizable_.


**Output**

The output is {xNew,Successful,i}.

| Output | Type | Description |
| --- | --- | --- |
| xNew | Real(Dim) | the point that (locally) minimizes the lowest eigenvalue of the Hamiltonian in the null leaf of the chosen leaf |
| Successful | Bool | True: a local minimum has been found up to tolerance epsilon; Flase: no local minimum has been found within nMax steps |
| i | Int | the number of steps the were needed for success |


**Description**

cqmgCurveIntegrationNull finds a local minimum of lambda in the null leaf of the chosen leaf.
This is done via a gradient descent method, using that $\partial_a\lambda=-(\mathbf{x}^a-x^a)$. delta is multiplied to the projection of the current gradient of lambda in every step. The method stops when $\vert \partial_a\lambda\vert<\epsilon$.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
leaf="QMleaf";
cqmgMinimizeLambda[X,x,l,leaf]
```

