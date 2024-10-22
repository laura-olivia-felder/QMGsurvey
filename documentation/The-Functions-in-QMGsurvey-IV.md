# Compiled Scan

## cqmgScan
_SeedRandom in advance recommended; Scans the chosen leaf via random curves._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | an initial point in target space |
| delta | Real | a positive finite step length in the integration of discrete curves |
| nPrime | Int | the (positive) number of random curves |
| n | Int | the (positive) number of steps (initial point excluded) in the integration of discrete curves |
| m | Int | the (positive) number of intermediate steps (zero means no intermediate steps) in the integration of discrete curves |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |


cqmgScan is _not parallelizable_.


**Output**

The output is xsScan.

| Output | Type | Description |
| --- | --- | --- |
| xsScan | Real(nPrime\*n+1,Dim) | the points in the scan of the chosen leaf, xsScan(1) is the initial point x |


**Description**

cqmgScan constructs a scan of the chosen leaf through x.
This is done via curves with random initial tangent vector and an initial point that is randomly chosen from the previously calculated points.
Since random numbers are involved, it is recomended to _seed a random state in advance_ to maintain reproducability.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
delta={0.01};
nPrime=10;
n=1000;
m=3;
leaf="QMleaf";
SeedRandom[1];
cqmgScan[X,x,delta,nPrime,n,m,l,leaf]
```












# Compiled Coordinates

## cqmgCoordinates
_Constructs local coordinates for the chosen leaf._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | an initial point in target space |
| delta | Real | a positive finite step length in the integration of discrete curves |
| n | Int | the (positive) number of steps (initial point excluded) in the integration of discrete curves |
| m | Int | the (positive) number of intermediate steps (zero means no intermediate steps) in the integration of discrete curves |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |
| calculateJaccobians=True | Bool | True: the Jaccobians are calculated; False: the Jaccobians are not calculated |
| epsilon=10^-8 | Real | the (positive) finite difference in the calculation of the Jaccobians |


cqmgCoordinates is _not parallelizable_.


**Output**

The output is {xssCoordinates,xssJaccobians} if calculateJaccobians=True; the output is {xssCoordinates} if calculateJaccobians=False.

| Output | Type | Description |
| --- | --- | --- |
| xssCoordinates | Real(2\*n+1,...l times...,2\*n+1,Dim) | the constructed local coordinates. The first l indices parametrize the new coordinates, specifying a point in target space, xssCoordinates(n+1,...l times...,n+1) is the initial point x). The last index corresponds to the point in target space |
| xssJaccobians | Real(2\*n+1,...l times...,2\*n+1,l,Dim) | the constructe Jaccobians. The first l indices parametrize the new coordinates, specifying a point in target space, xssJaccobians(k_1,...,k_l) is the Jaccobian at the point xssCoordinates(k_1,...,k_l). The last two indices correspond to the Jaccobi matrix. The second but last corresponds to the new coordinates, while the last corresponds to the target space |


**Description**

cqmgCoordinates constructs descrete local coordinates in the chosen leaf through x. This is done by integrating discrete curves.
The output xssCoordinates should be thought of as a discretization of a map $f:U\subset\mathbb{R}^l\to\mathbb{R}^D$ where $U$ is an open subset of $\mathbb{R}^l$. Here, $(q\circ f)^{-1}$ defines local coordinates for the chosen leaf where $q$ is the map $x\mapsto U(1)\vert x\rangle$.
xssJaccobians should be thought of as a discretization of the map $(\partial_l f)$.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
delta={0.1};
n=10;
m=3;
leaf="QMleaf";
cqmgCoordinates[X,x,delta,n,m,l,leaf]
```

An example for the fuzzy sphere wthout calculating Jaccobians:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
delta={0.1};
n=10;
m=3;
leaf="QMleaf";
cqmgCoordinates[X,x,delta,n,m,l,leaf,False]
```














# Compiled Integration

## cqmgPointToolsINIT
_For internal use; Initialzation function of cqmgPointInTileQ, cqmgPointsInTile, cqmgFindOptimalPointInTile and cqmgFilledTileQ; these perform important calculations in the context of tilings._

## cqmgPointTools\*

### cqmgPointToolsINIT
_For internal use; Initialzation function of cqmgPointInTileQ, cqmgPointsInTile, cqmgFindOptimalPointInTile and cqmgFilledTileQ; these perform important calculations in the context of tilings._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is {cqmgPointInTileQ,cqmgPointsInTile,cqmgFindOptimalPointInTile,cqmgFilledTileQ}.

| Output | Type | Description |
| --- | --- | --- |
| cqmgPointInTileQ | Fx | the compiled function |
| cqmgPointsInTile | Fx | a function based on cqmgPointInTileQ |
| cqmgFindOptimalPointInTile | Fx | a function based on cqmgPointInTileQ |
| cqmgFilledTileQ | Fx | a function based on cqmgPointInTileQ |

### cqmgPointInTileQ
_Initialized with cqmgPointInTileQINIT; cqmgPointInTileQ checks if a point lies within a tile._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | a point in target space |
| Tile | Real(Dim,2) | a tile in target space. Tile(k,1) defines the lower bound in direction k, Tile(k,2) defines the upper bound in direction a  |

cqmgPointInTileQ is _parallelizable_ in the variables x and Tile.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Bool | True if for all k Tile(k,1) is smaller or equal x(k) and x(k) is smaller Tile(k,2); else False |

### cqmgPointsInTile
_Initialized with cqmgPointsInTileINIT; cqmgPointsInTile selects the points of a list that lie within a tile._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| xs | Real(k,Dim) | a list of length k consisting of points in target space |
| Tile | Real(Dim,2) | a tile in target space. Tile(k,1) defines the lower bound in direction k, Tile(k,2) defines the upper bound in direction a  |


cqmgPointsInTile is _not parallelizable_.

**Output**

The output is xsInTile.

| Output | Type | Description |
| --- | --- | --- |
| xsInTile | Real(kPrime,Dim) | a list of length kPrime consisting of points in target space. This is the list of points from xs that lie in the Tile |

### cqmgFindOptimalPointInTile
_Initialized with cqmgFindOptimalPointInTileINIT; cqmgFindOptimalPointInTile chooses from a list of points in a tile the point that lies most centered in the tile._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| xsInTile | Bool(k,Dim) | a list of length k consisting of points in target space that lie in the Tile |
| Tile | Real(Dim,2) | a tile in target space. Tile(k,1) defines the lower bound in direction k, Tile(k,2) defines the upper bound in direction a  |

cqmgFindOptimalPointInTile is _not parallelizable_.

**Output**

The output is x.

| Output | Type | Description |
| --- | --- | --- |
| x | Real(Dim) | the point from xsInTile that is most centered |
| Tile | Real(Dim,2) | a tile in target space. Tile(k,1) defines the lower bound in direction k, Tile(k,2) defines the upper bound in direction a  |


### cqmgFilledTileQ
_Initialized with cqmgFilledTileQINIT; cqmgFilledTileQ checks if some local coordinates filled a Tile._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| CoordinatesInTileQ  | Bool(2\*n+1,...l times...,2\*n+1) | This is the output of cqmgPointInTileQ[xssCoordinates,Tile] for some local coordinates xssCoordinates, being themselves part of the output of cqmgCoordinates |
| Tile | Real(Dim,2) | a tile in target space. Tile(k,1) defines the lower bound in direction k, Tile(k,2) defines the upper bound in direction a  |


cqmgFilledTileQ is _not parallelizable_.

**Output**

The output is Filled.

| Output | Type | Description |
| --- | --- | --- |
| Filled | Bool | True if CoordinatesInTileQ(k_1,...,k_l)=False in all cases where at least one k_j is 1 or 2\*n+1; else False. This means that all border points of xssCoordinates lie outside of the Tile, thus the coordinates fill the Tile |


**Description**

cqmgPointInTileQ checks if a point lies within a given Tile.
cqmgPointsInTile selects all points from a list of points that lie within a given Tile.
cqmgFindOptimalPointInTile chooses from a list of points in a tile the point that lies most centered in the Tile.
This is done by minimizing $\vert x-y\vert_\infty$ over all points x in xsInTile. Here y=(Tile(1)+Tile(2))/2 is the center of the Tile.
cqmgFilledTileQ can be used to check if given discrete local coordinates fill a given Tile in the sense that all border points of the the local coordinates lie outside the tile, assuming that at least one coordinate point lies within.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for cqmgPointInTileQ using parallelization:
```mathematica
X=qmgXsu2[4];
l=2;
Tile={{0,1},{-1,0},{-1,3}}
x1={0,0,1};
x2={0,1,0};
x3={0,0,1/2}
xs={x1,x2,x3}
{cqmgPointInTileQ,cqmgPointsInTile,cqmgFindOptimalPointInTile,cqmgFilledTileQ}=cqmgPointToolsINIT[X,l];
cqmgPointInTileQ[xs,Tile]
```

An example for cqmgPointsInTile:
```mathematica
X=qmgXsu2[4];
l=2;
Tile={{0,1},{-1,0},{-1,3}}
x1={0,0,1};
x2={0,1,0};
x3={0,0,1/2}
xs={x1,x2,x3}
{cqmgPointInTileQ,cqmgPointsInTile,cqmgFindOptimalPointInTile,cqmgFilledTileQ}=cqmgPointToolsINIT[X,l];
cqmgPointsInTile[xs,Tile]
```

An example for cqmgFindOptimalPointInTile:
```mathematica
X=qmgXsu2[4];
l=2;
Tile={{0,1},{-1,0},{-1,3}}
x1={0,0,1};
x2={0,1,0};
x3={0,0,1/2}
xs={x1,x2,x3}
{cqmgPointInTileQ,cqmgPointsInTile,cqmgFindOptimalPointInTile,cqmgFilledTileQ}=cqmgPointToolsINIT[X,l];
xsInTile=cqmgPointsInTile[xs,Tile];
cqmgPointsInTile[xsInTile,Tile]
```

An example for cqmgFilledTileQ:
```mathematica
X=qmgXsu2[4];
l=2;
Tile={{0,1},{-1,0},{-1,3}}
x={1/2,-1/2,0};
delta={0.3};
n=10;
m=3;
leaf="QMleaf";
xssCoordinates=cqmgCoordinates[X,x,delta,n,m,l,leaf,False];
{cqmgPointInTileQ,cqmgPointsInTile,cqmgFindOptimalPointInTile,cqmgFilledTileQ}=cqmgPointToolsINIT[X,l];
CoordinatesInTileQ=cqmgPointInTileQ[xssCoordinates,Tile];
cqmgFilledTileQ[CoordinatesInTileQ,Tile]
```










## cqmgIntegrands\*

### cqmgIntegrandsINIT
_For internal use; Initialzation function of cqmgIntegrands; cqmgIntegrands calculates the integrands for integration over the chosen leaf._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |

**Output**

The output is cqmgIntegrands.

| Output | Type | Description |
| --- | --- | --- |
| cqmgIntegrands | Fx | the compiled function |

### cqmgIntegrands
_For internal use; Initialized with cqmgIntegrandsINIT; cqmgIntegrands calculates the integrands for integration over the chosen leaf._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Jaccobi | Real(l,Dim) | the Jaccobain matrix of local coordinates at a point (as in the output of cqmgCoordinates) |
| EigState | Complex(Nim,1) | the quasi-coherent state at a point (as in the output of cqmgBasic) |
| MeasureForm | Real(Dim,Dim) | a measore form at a given point, eather omega or g (as in the output of cqmgBasic) |
| xBold | Real(Dim) | xBold at a point (as in the output of cqmgBasic) |

cqmgIntegrands is _parallelizable_ in the variables Jaccobi, EigState, MeasureForm and xBold.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Complex(Nim\*Nim+Dim\*Nim\*Nim) | compressed output. With respect to the output of cqmgIntegrandsEXTR out=Join[Integrand,xBoldTensorIntegrand] |


### cqmgIntegrandsEXTR
_For internal use; Extraction function of cqmgIntegrands; cqmgIntegrands calculates the integrands for integration over the chosen leaf._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Complex(1+Dim,Nim,Nim) | the output of cqmgIntegrands |

**Output**

The output is {Integrand,xBoldTensorIntegrand}.

| Output | Type | Description |
| --- | --- | --- |
| Integrand | Complex(1+Dim,Nim,Nim) | the integrand at the point |
| xBoldTensorIntegrand | Complex(Dim,Nim,Nim) | the tensor product of xBold with the Integrand at a point |


**Description**

cqmgIntegrands calculates the integrand and its tensor product with xBold at a point.
At the point x Integrand is $m \vert x\rangle\langle x\vert$ where $m$ is the measure at x either based on omega or g.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
delta={0.3};
n=10;
m=3;
leaf="QMleaf";
{xssCoordinates,xssJaccobians]=cqmgCoordinates[X,x,delta,n,m,l,leaf];
xChosen=xssCoordinates[[2,3]];
Jaccobi=xssJaccobians[[2,3]];
cqmgQMleafBasic=cqmgQMleafBasicINIT[X];
out=cqmgQMleafBasic[xChosen];
{EigState,omega,xBold}=cqmgQMleafBasicEXTR[out];
cqmgIntegrands=cqmgIntegrandsINIT[X,l];
out=cqmgIntegrands[Jaccobi,EigState,omega,xBold]
```
















## cqmgIntegrateTile
_Integrates over a given tile._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| xCollection | Real(k,Dim) | a list of length k, consising of points in target space that lie in the chosen leaf. Usually, this is the output of qmgScan |
| Tile | Real(Dim,2) | a tile in target space. Tile(k,1) defines the lower bound in direction k, Tile(k,2) defines the upper bound in direction a  |
| delta | Real | a positive finite step length in the construction of discrete local coordinates |
| n | Int | the (positive) number of steps (initial point excluded) in the construction of discrete local coordinates |
| m | Int | the (positive) number of intermediate steps (zero means no intermediate steps) in the construction of discrete local coordinates |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |
| measure="omega" | Str | the chosen measure. Either "omega" for omega or "g" for g |
| epsilon=10^-8 | Real | the (positive) finite difference in the calculation of the Jaccobians in the construction of discrete local coordinates |


cqmgIntegrateTile is _not parallelizable_.


**Output**

The output is {{CompletenessTile,xBoldQuantizationTile},{xsCoordinatesInTile,xsEigStatInTile,xsIntegrandInTile},{TileNonempty,xsInTileNumber,FilledTileQ}}.

| Output | Type | Description |
| --- | --- | --- |
| CompletenessTile | Complex(Nim,Nim) | the local preliminary quantization of the 1 function (if TileNonempty=False: ConstantArray[0,{Nim,Nim}]) |
| xBoldQuantizationTile | Complex(Dim,Nim,Nim) | the local preliminary quantization of xBold (if TileNonempty=False: ConstantArray[0,{Dim,Nim,Nim}]) |
| xsCoordinatesInTile | Real(k,Dim) | a list of length k consisting of the constructed corrdinate points that lie within the Tile (if TileNonempty=False: {}) |
| xsEigStatInTile | Complex(k,Nim) | a list of length k consisting of the quasi-coherent states at the constructed corrdinate points that lie within the Tile (if TileNonempty=False: {}) |
| xsIntegrandInTile | a list of length k consisting of the Integrands at the constructed corrdinate points that lie within the Tile (if TileNonempty=False: {}) |
| TileNonempty | Bool | True if at least one of the points in xCollection lies within the Tile; else False |
| xsInTileNumber |Int | the number of points in xCollection that lie within the Tile (if TileNonempty=False: 0) |
| FilledTileQ | Bool | True if the constructe discrete local coordinates filled the Tile (this means that all border points lie outside of the Tile) (if TileNonempty=False: True, this is for better compatibility with other functions) |

**Description**

cqmgIntegrateTile chooses the optimally centered point in a given Tile from points coming from cqmgScan.
Arround that point, discrete local coordinates are constructed. Further, the maps $x\mapsto m \vert x\rangle\langle x\vert$ and $x\mapsto \mathbf{x}^a m \vert x\rangle\langle x\vert$ (where $m$ is the chosen measure) are integrate over the local coordinates, resulting in CompletenessTile and xBoldQuantizationTile.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
Tile={{0,1},{-1,0},{-1,3}};
x={0,0,1};
deltaScan={0.01};
nPrime=10;
nScan=1000;
mScan=3;
deltaInt={0.1};
nInt=25;
mInt=1;
leaf="QMleaf";
measure="omega";
SeedRandom[1];
xCollection=cqmgScan[X,x,deltaScan,nPrime,nScan,mScan,l,leaf];
cqmgIntegrateTile[X,xCollection,Tile,deltaInt,nInt,mInt,l,leaf,measure]
```














## cqmgIntegrateTiling
_Integrates over a tiling._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| xCollection | Real(k,Dim) | a list of length k, consising of points in target space that lie in the chosen leaf. Usually, this is the output of qmgScan |
| Tiling | Real(r,Dim,2) | a tiling in target space, that is a collection of r tiles that intersect at least on a set of measure zero. They should better be chosen such that they at least cover all points in xCollection |
| delta | Real | a positive finite step length in the construction of discrete local coordinates |
| n | Int | the (positive) number of steps (initial point excluded) in the construction of discrete local coordinates |
| m | Int | the (positive) number of intermediate steps (zero means no intermediate steps) in the construction of discrete local coordinates |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |
| measure="omega" | Str | the chosen measure. Either "omega" for omega or "g" for g |
| epsilon=10^-8 | Real | the (positive) finite difference in the calculation of the Jaccobians in the construction of discrete local coordinates |
| parallelTable=True | Bool | If True, cqmgIntegrateTile is called in parallel for each Tile, else this is done subsequently  |


cqmgIntegrateTiling is _not parallelizable_.


**Output**

The output is {{Completeness,xBoldQuantization},xsCoordinatesCollection,{NoTileEmpty,AllPointsInTile,AllTilesFilled},out}.

| Output | Type | Description |
| --- | --- | --- |
| Completeness | Complex(Nim,Nim) | the sum of CompletenessTile over all tiles in Tiling |
| xBoldQuantization | Complex(Dim,Nim,Nim) | the sum of xBoldQuantizationTile over all tiles in Tiling |
| xsCoordinatesCollection | {Real(s_1,Dim),...,Real(s_r,Dim)} | a list of length r consisting of the xsCoordinatesInTile for each tile in Tiling |
| NoTileEmpty | Bool | True if TileNonempty is True for each tile in Tiling; else False. This means that every Tile contains at least one point of xCollection |
| AllPointsInTile | Bool | True if the sum over xsInTileNumber for each tile in Tiling equals k; else False. This means that Tiling covers all points in xCollection |
| AllTilesFilled | Bool | True if FilledTileQ is True for each tile in Tiling; else False. This means that all constructed discrete local coordinates in non empty coordinates fill the corresponding tile |
| out | {out_1,...out_r} | a list of length r consisting of the output of cqmgIntegrateTile for each tile in Tiling |

**Description**

cqmgIntegrateTiling calculates cqmgIntegrateTile for each Tile in Tiling and patches the results together.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
Tiling=Tuples[{{0, 10}, {-10, 0}}, 3];
x={0,0,1};
deltaScan={0.01};
nPrime=10;
nScan=1000;
mScan=3;
deltaInt={0.1};
nInt=25;
mInt=1;
leaf="QMleaf";
measure="omega";
SeedRandom[1];
xCollection=cqmgScan[X,x,deltaScan,nPrime,nScan,mScan,l,leaf];
cqmgIntegrateTiling[X,xCollection,Tiling,deltaInt,nInt,mInt,l,leaf,measure]
```














## cqmgQuantization
_Processes the output of cqmgIntegrateTiling in the context of quantization._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| outIntegrateTiling |  | the output of cqmgIntegrateTiling |


cqmgQuantization is _not parallelizable_.


**Output**

The output is {{Vol,alpha,nCorr},{Completeness,xBoldQuantizationCorr}}.

| Output | Type | Description |
| --- | --- | --- |
| Vol | Real | the volume of the leaf with respect to the chosen measure |
| alpha | Real | the correction factor alpha in the quantization map |
| nCorr | Real | the correction factor for the quantization of xBold |
| Completeness | the quantization of the 1 function |
| xBoldQuantization | Complex(Dim,Nim,Nim) | the corrected quantization of xBold |

**Description**

cqmgQuantization further processes the output of cqmgIntegrateTiling, related to the quantization map.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
Tiling=Tuples[{{0, 10}, {-10, 0}}, 3];
x={0,0,1};
deltaScan={0.01};
nPrime=10;
nScan=1000;
mScan=3;
deltaInt={0.1};
nInt=25;
mInt=1;
leaf="QMleaf";
measure="omega";
SeedRandom[1];
xCollection=cqmgScan[X,x,deltaScan,nPrime,nScan,mScan,l,leaf];
outIntegrateTiling=cqmgIntegrateTiling[X,xCollection,Tiling,deltaInt,nInt,mInt,l,leaf,measure];
cqmgQuantization[X,l,outIntegrateTiling]
```











# Integration Preview


## cqmgIntegrateTilePreview
_Preview of cqmgIntegrateTile; only calculates local coordinates._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| xCollection | Real(k,Dim) | a list of length k, consising of points in target space that lie in the chosen leaf. Usually, this is the output of qmgScan |
| Tile | Real(Dim,2) | a tile in target space. Tile(k,1) defines the lower bound in direction k, Tile(k,2) defines the upper bound in direction a  |
| delta | Real | a positive finite step length in the construction of discrete local coordinates |
| n | Int | the (positive) number of steps (initial point excluded) in the construction of discrete local coordinates |
| m | Int | the (positive) number of intermediate steps (zero means no intermediate steps) in the construction of discrete local coordinates |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |

cqmgIntegrateTilePreview is _not parallelizable_.


**Output**

The output is {{},{xsCoordinatesInTile},{TileNonempty,xsInTileNumber,FilledTileQ}}.

| Output | Type | Description |
| --- | --- | --- |
| xsCoordinatesInTile | Real(k,Dim) | a list of length k consisting of the constructed corrdinate points that lie within the Tile (if TileNonempty=False: {}) |
| TileNonempty | Bool | True if at least one of the points in xCollection lies within the Tile; else False |
| xsInTileNumber |Int | the number of points in xCollection that lie within the Tile (if TileNonempty=False: 0) |
| FilledTileQ | Bool | True if the constructe discrete local coordinates filled the Tile (this means that all border points lie outside of the Tile) (if TileNonempty=False: True, this is for better compatibility with other functions) |

**Description**

cqmgIntegrateTilePreview is a faster version of cqmgIntegrateTile that only constructs discrete local coordinates and checks their quality but does not perform any integration. This method can be used to find the right parameters for cqmgIntegrateTile without always having to perform the full calculation.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
Tile={{0,1},{-1,0},{-1,3}};
x={0,0,1};
deltaScan={0.01};
nPrime=10;
nScan=1000;
mScan=3;
deltaInt={0.1};
nInt=25;
mInt=1;
leaf="QMleaf";
SeedRandom[1];
xCollection=cqmgScan[X,x,deltaScan,nPrime,nScan,mScan,l,leaf];
cqmgIntegrateTilePreview[X,xCollection,Tile,deltaInt,nInt,mInt,l,leaf]
```










## cqmgIntegrateTilingPreview
_Preview of cqmgIntegrateTiling; only calculates a covering with local coordinates._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| xCollection | Real(k,Dim) | a list of length k, consising of points in target space that lie in the chosen leaf. Usually, this is the output of qmgScan |
| Tiling | Real(r,Dim,2) | a tiling in target space, that is a collection of r tiles that intersect at least on a set of measure zero. They should better be chosen such that they at least cover all points in xCollection |
| delta | Real | a positive finite step length in the construction of discrete local coordinates |
| n | Int | the (positive) number of steps (initial point excluded) in the construction of discrete local coordinates |
| m | Int | the (positive) number of intermediate steps (zero means no intermediate steps) in the construction of discrete local coordinates |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |
| measure="omega" | Str | the chosen measure. Either "omega" for omega or "g" for g |

cqmgIntegrateTilingPreview is _not parallelizable_.


**Output**

The output is {{},xsCoordinatesCollection,{NoTileEmpty,AllPointsInTile,AllTilesFilled},out}.

| Output | Type | Description |
| --- | --- | --- |
| xsCoordinatesCollection | {Real(s_1,Dim),...,Real(s_r,Dim)} | a list of length r consisting of the xsCoordinatesInTile for each tile in Tiling |
| NoTileEmpty | Bool | True if TileNonempty is True for each tile in Tiling; else False. This means that every Tile contains at least one point of xCollection |
| AllPointsInTile | Bool | True if the sum over xsInTileNumber for each tile in Tiling equals k; else False. This means that Tiling covers all points in xCollection |
| AllTilesFilled | Bool | True if FilledTileQ is True for each tile in Tiling; else False. This means that all constructed discrete local coordinates in non empty coordinates fill the corresponding tile |
| out | {out_1,...out_r} | a list of length r consisting of the output of cqmgIntegrateTilePreview for each tile in Tiling |

**Description**

cqmgIntegrateTiling Preview is a faster version of cqmgIntegrateTiling  that only constructs discrete local coordinates and checks their quality but does not perform any integration. This method can be used to find the right parameters for cqmgIntegrateTiling without always having to perform the full calculation.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
Tiling=Tuples[{{0, 10}, {-10, 0}}, 3];
x={0,0,1};
deltaScan={0.01};
nPrime=10;
nScan=1000;
mScan=3;
deltaInt={0.1};
nInt=25;
mInt=1;
leaf="QMleaf";
SeedRandom[1];
xCollection=cqmgScan[X,x,deltaScan,nPrime,nScan,mScan,l,leaf];
cqmgIntegrateTilingPreview[X,xCollection,Tiling,deltaInt,nInt,mInt,l,leaf]
```












# Integrate and Quantize Custom Function

## cqmgCustomIntegrateTiling
_Integrates custom functions over a tiling via the output of cqmgIntegrateTiling._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| outIntegrateTiling |  | the output of cqmgIntegrateTiling |
| fxCustom | Fx | a custom map to be integrated. For more information see below |
| parallelTable=True | Bool | If True, cqmgIntegrateTile is called in parallel for each Tile, else this is done subsequently  |


cqmgCustomIntegrateTiling is _not parallelizable_.


**Output**

The output is {Integral,IntegralReduced,Quantization}.

| Output | Type | Description |
| --- | --- | --- |
| Integral | Complex(Nim,Nim,k_1,...k_n) | the preliminary quantization of fxCustom. The first two indices correspond to the operator structure, the remaining indices correspond to the tensor structure of the output of fxCustom |
| IntegralReduced | Complex(k_1,...k_n) | the trace of Integral in the operator indices, this is simply the integral of fxCustom over the chosen leaf |
| Quantization | Complex(Nim,Nim,k_1,...k_n) | the quantization of fxCustom. The first two indices correspond to the operator structure, the remaining indices correspond to the tensor structure of the output of fxCustom |


### fxCustom
_One of the arguments of cqmgCustomIntegrateTiling, a custom function to be quantized._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Vec | Complex(Dim) | a normalized vector, for this a quasi-coherent state will be inserted. The function fxCustom has to be invariant under the multiplication of Vec with a complex phase |

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Complex(k_1,...k_n) | a complex array of arbitrary shape |


**Description**

cqmgCustomIntegrateTiling performs calculations corresponding to the quantization map for a custom function fxCustom (for this we now write f), based on the output of cqmgIntegrateTiling.
Integral is then the integral of the map $x\mapsto m f(x) \vert x\rangle\langle x\vert$ and IntegralReduced the integral of the map $x\mapsto m f(x)$ (where $m$ is the chosen measure). Quantization is the quantization of $f$.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
Tiling=Tuples[{{0, 10}, {-10, 0}}, 3];
x={0,0,1};
deltaScan={0.01};
nPrime=10;
nScan=1000;
mScan=3;
deltaInt={0.1};
nInt=25;
mInt=1;
leaf="QMleaf";
measure="omega";
SeedRandom[1];
fxCustom[Vec_]:={{1, 2}, {-1, 0}};
xCollection=cqmgScan[X,x,deltaScan,nPrime,nScan,mScan,l,leaf];
outIntegrateTiling=cqmgIntegrateTiling[X,xCollection,Tiling,deltaInt,nInt,mInt,l,leaf,measure];
cqmgCustomIntegrateTiling[X,l,outIntegrateTiling,fxCustom]
```













# Compiled Kähler Cost

## cqmgKaehlerCost\*

### cqmgKaehlerCostINIT
_Initialization function of cqmgKaehlerCost; cqmgKaehlerCost calculates the Kähler cost for a given linear subspace of the tangent space._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |


**Output**

The output is cqmgKaehlerCost.

| Output | Type | Description |
| --- | --- | --- |
| cqmgKaehlerCost | Fx | the compiled function |

### cqmgKaehlerCost
_Initialized with cqmgKaehlerCostINIT; cqmgKaehlerCost calculates the Kähler cost for a given linear subspace of the tangent space._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| CovDer | Complex(Dim,Nim) | a point in target space |
| W | Real(Dim,l) | the transpose of a list of length k consisting of vectors that span a subspace of dimension l of target space |

cqmgKaehlerCost is _parallelizable_ in the variables CovDer and W.

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real | compressed output. With respect to the output of cqmgKaehlerCostEXTR out=c |

### cqmgKaehlerCostEXTR
_Extraction function of cqmgKaehlerCost; cqmgKaehlerCost calculates the Kähler cost for a given linear subspace of the tangent space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| out | Real | the output of cqmgKaehlerCost |

**Output**

The output is c.

| Output | Type | Description |
| --- | --- | --- |
| c | Real | the Kähler cost of the subspace spanned by the vectors in Transpose[W] |

**Description**

cqmgKaehlerCost calculates the Kähler cost of the subspace spanned by the vectors in Transpose[W].
The Kähler cost is a measure for how far away a subspace (to be precise, its pushforward to the quantum manifold) is from being closed under multiplication with i (c=0 means that it is closed).
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
W=Transpose[{{1,0,0},{0,0,1}}];
cqmgKaehlerBasic=cqmgKaehlerBasicINIT[X];
{EigState,CovDer}=cqmgKaehlerBasic[x];
cqmgKaehlerCost=cqmgKaehlerCostINIT[X,l];
out=cqmgKaehlerCost[CovDer,W];
cqmgKaehlerCostEXTR[out]
```


















## cqmgKaehler
_Simplified function built on cqmgKaehlerCost; calculates the Kähler cost for a given linear subspace of the tangent space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | an initial point in target space |
| W | Real(Dim,l) | the transpose of a list of length k consisting of vectors that span a subspace of dimension l of target space |

cqmgKaehler is _not parallelizable_.


**Output**

The output is c.

| Output | Type | Description |
| --- | --- | --- |
| c | Real | the Kähler cost of the subspace spanned by the vectors in Transpose[W] |

**Description**

cqmgKaehler calculates the Kähler cost of the subspace spanned by the vectors in Transpose[W].
The Kähler cost is a measure for how far away a subspace (to be precise, its pushforward to the quantum manifold) is from being closed under multiplication with i (c=0 means that it is closed).
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
W=Transpose[{{1,0,0},{0,0,1}}];
cqmgKaehler[X,x,W]
```













## cqmgKaehlerForLeaf
_Calculates the Kähler cost for the chosen leaf._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | an initial point in target space |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| leaf="TSleaf" | Str | the chosen leaf, can be "TSleaf", "QMleaf" or "GQMleaf". "TSleaf" is the hybrid leaf (using theta), "QMleaf" is the hybrid leaf using omega and "GQMleaf" is the hybrid leaf using omega and g |

cqmgKaehlerForLeaf is _not parallelizable_.


**Output**

The output is c.

| Output | Type | Description |
| --- | --- | --- |
| c | Real | the Kähler cost of the subspace corresponding to the distribution at x of the chosen leaf |

**Description**

cqmgKaehlerForLeaf calculates the Kähler cost of the subspace corresponding to the distribution at x of the chosen leaf.
The Kähler cost is a measure for how far away a subspace (to be precise, its pushforward to the quantum manifold) is from being closed under multiplication with i (c=0 means that it is closed).
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
leaf="QMleaf";
cqmgKaehlerForLeaf[X,x,l,leaf]
```









## cqmgKaehlerForRandom
_SeedRandom in advance recommended; calculates the Kähler cost for random subspaces._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | an initial point in target space |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| n | Int | a positive integer that specifies for how many random subspaces the Kähler cost is calculated |

cqmgKaehlerForRandom is _not parallelizable_.


**Output**

The output is {cAverage,cDev,cMin,cMax}.

| Output | Type | Description |
| --- | --- | --- |
| cAverage | Real | the average of the Kähler costs of all random subspaces |
| cDev | Real | the standard deviation of the Kähler costs of all random subspaces |
| cMin | Real | the minimum of the Kähler costs over of random subspaces |
| cMax | Real | the maximum of the Kähler costs over of random subspaces |

**Description**

cqmgKaehlerForRandom calculates the Kähler cost for n random subspaces of dimension l of target space. Since random numbers are involved, it is recomended to seed a random state in advance to maintain reproducability.
The Kähler cost is a measure for how far away a subspace (to be precise, its pushforward to the quantum manifold) is from being closed under multiplication with i (c=0 means that it is closed).
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
x={0,0,1};
n=1000;
SeedRandom[1];
cqmgKaehlerForLeaf[X,x,l,n]
```














# Compiled Poisson Structures

## cqmgComparePoissonStructures
_Calculates the Poisson structure induced by omega in the form of theta for a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | an initial point in target space |
| tolerance=10^-8 | Real | the finite positive tolerance used in the calculation of a PseudoInverse |

cqmgComparePoissonStructures is _not parallelizable_.


**Output**

The output is {theta,thetaInducedByomega}.

| Output | Type | Description |
| --- | --- | --- |
| theta | Real(Dim,Dim) | theta |
| thetaInducedByomega | Real(Dim,Dim) | the Poisson structure induced by omega |

**Description**

cqmgComparePoissonStructures calculates theta and the Poisson tensor corresponding to the Poisson structure induced by omega.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 2.2.7.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
cqmgComparePoissonStructures[X,x]
```












# Compiled Quantization Validation

## cqmgQuantizationValidation
_SeedRandom in advance recommended; performs various checks that validate the quality of the semiclassical limit based on the output of cqmgIntegrateTiling._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | an initial point in target space |
| l | Int | a positive even integer, the effective dimension of the quantum manifold |
| outIntegrateTiling |  | the output of cqmgIntegrateTiling |
| n | Int | a positive integer that specifies for how many random subspaces the Kähler cost is calculated |
| tolerance=10^-8 | Real | the finite positive tolerance used in the calculation of a PseudoInverse |


cqmgQuantizationValidation is _not parallelizable_.


**Output**

The output is {Vol,dCompleteness,dxBoldQuantizationCorr,nCorr,dPoisson,cLeaf,cRandom,cRandomDev,cMin,cMax,NoTileEmpty,AllPointsInTile,AllTilesFilled}.

| Output | Type | Description |
| --- | --- | --- |
| Vol | Real | the volume of the leaf with respect to the chosen measure |
| dCompleteness | Real | the relative deviation of the completeness relation |
| dxBoldQuantizationCorr | Real | the relative deviation the corrected quantization of xBold to X |
| nCorr | Real | the correction factor for the quantization of xBold |
| dPoisson | Real | the relative deviation of theta and thetaInducedByomega |
| c | Real | the Kähler cost of the subspace corresponding to the distribution at x of the chosen leaf |
| cRandom | Real | the average of the Kähler costs of all random subspaces |
| cRandomDev | Real | the standard deviation of the Kähler costs of all random subspaces |
| cMin | Real | the minimum of the Kähler costs over of random subspaces |
| cMax | Real | the maximum of the Kähler costs over of random subspaces |
| NoTileEmpty | Bool | NoTileEmpty from the output of cqmgIntegrateTiling |
| AllPointsInTile | Bool | AllPointsInTile from the output of cqmgIntegrateTiling |
| AllTilesFilled | Bool | AllTilesFilled from the output of cqmgIntegrateTiling |


**Description**

cqmgQuantizationValidation compactly unifies the results of cqmgQuantization, cqmgComparePoissonStructures, cqmgKaehlerForLeaf and cqmgKaehlerForRandom
and provides a few verifications in the context of the quantization map.
Since random numbers are involved, it is recomended to _seed a random state in advance_ to maintain reproducability.
Compare to [[1]](https://arxiv.org/abs/2301.10206) sections 2.2.7 and 3.3.


**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
Tiling=Tuples[{{0, 10}, {-10, 0}}, 3];
x={0,0,1};
deltaScan={0.01};
nPrime=10;
nScan=1000;
mScan=3;
deltaInt={0.1};
nInt=25;
mInt=1;
leaf="QMleaf";
measure="omega";
nKaehler=1000;
SeedRandom[1];
xCollection=cqmgScan[X,x,deltaScan,nPrime,nScan,mScan,l,leaf];
outIntegrateTiling=cqmgIntegrateTiling[X,xCollection,Tiling,deltaInt,nInt,mInt,l,leaf,measure];
cqmgQuantizationValidation[X,x,l,outIntegrateTiling,nKaehler,leaf]
```











## cqmgQuantizationValidationPresent
_SeedRandom in advance recommended; constructs a table with the most interesting output of cqmgQuantizationValidation._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Xxs | {{MathConf[Dim_1,Nim_1],Real[Dim_1],Str,Int,outIntegrateTiling,Int,Str},...,{MathConf[Dim_1,Nim_1],Real[Dim_1],Str,Int,outIntegrateTiling,Int,Str}} | a list of length k consisting of lists consisting of a matrix configuration (X), a point in target space (x), a name for the row in the table (Name), the effective dimension of the corresponding quantum manifold (l), an output of cqmgIntegrate (outIntegrateTiling), a positive number of random subspace for the calculation of the Kähler cost (n) and a leaf (leaf, either "TSleaf", "QMleaf" or "GQMleaf") |
| tolerance=10^-8 | Real | the finite positive tolerance used in the calculation of a PseudoInverse |



cqmgQuantizationValidationPresent is _not parallelizable_.


**Output**

The output is table.

| Output | Type | Description |
| --- | --- | --- |
| table | Mathematica Table | a table with row i+1 showing the results of cqmgQuantizationValidation for X=Xxs(i,1), x=Xxs(i,2), l=Xxs(i,4), outIntegrateTiling=Xxs(i,5), n=Xxs(i,6), leaf=Xxs(i,7) and tolerance=tolerance |



**Description**

cqmgQuantizationValidationPresent calculates cqmgQuantizationValidation for multiple matrix configurations etc. and presents (selected) output in a table.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
l=2;
Tiling=Tuples[{{0, 10}, {-10, 0}}, 3];
x={0,0,1};
deltaScan={0.01};
nPrime=10;
nScan=1000;
mScan=3;
deltaInt={0.1};
nInt=25;
mInt=1;
leaf="QMleaf";
measure="omega";
nKaehler=1000;
SeedRandom[1];
xCollection=cqmgScan[X,x,deltaScan,nPrime,nScan,mScan,l,leaf];
outIntegrateTiling=cqmgIntegrateTiling[X,xCollection,Tiling,deltaInt,nInt,mInt,l,leaf,measure];
Name="N=4";
Xxs={{X,x,Name,l,outIntegrateTiling,nKaehler,leaf}};
cqmgQuantizationValidationPresent[Xxs]
```


