# Matrix Configurations

## qmgXsu2
_Generates the matrix configuration of the fuzzy sphere._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Nim | Int | the dimension of the representation |
| Normalized=True | Bool | True: the matrices are normalized such that the sum over all squares is the identity; False: the matrices are not normalized  |


**Output**

The output is X.

| Output | Type | Description |
| --- | --- | --- |
| X | MathConf(3,Nim) | the matrix configuration of the fuzzy sphere |


**Description**

qmgXsu2 generates the matrix configuration of the fuzzy sphere.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 2.3.2.

**Example(s)**

An example for Nim=4:
```mathematica
X=qmgXsu2[4]
```










## qmgXsu3
_Generates the matrix configuration of the fuzzy CP2._

The following function has been derived from [https://arxiv.org/abs/0908.3864](https://arxiv.org/abs/0908.3864) by Richard Shurtleff, [licensed under CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/legalcode.txt).


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Pin | Int | Pin as in the (Pin,Qin) irreducible representation of SU(3) |
| Qin | Int | Qin as in the (Pin,Qin) irreducible representation of SU(3) |
| Normalized=True | Bool | True: the matrices are normalized such that the sum over all squares is the identity; False: the matrices are not normalized  |


**Output**

The output is X.

| Output | Type | Description |
| --- | --- | --- |
| X | MathConf(8,(Pin+1)(Qin+1)(Pin+Qin+2)/2) | the matrix configuration of the fuzzy CP2 |


**Description**

qmgXsu3 generates the matrix configuration of the fuzzy $\mathbb{C}P^2$.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 2.3.5.

**Example(s)**

An example for (Pin,Qin)=(3,0):
```mathematica
X=qmgXsu5[3,0]
```











## qmgXcs
_Generates the matrix configuration of the fuzzy torus._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Nim | Int | the dimension of the representation |


**Output**

The output is X.

| Output | Type | Description |
| --- | --- | --- |
| X | MathConf(4,Nim) | the matrix configuration of the fuzzy torus |


**Description**

qmgXcs generates the matrix configuration of the fuzzy torus.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 4.5.

**Example(s)**

An example for Nim=4:
```mathematica
X=qmgXcs[4]
```










## qmgXrand
_SeedRandom in advance recommended; generates a random matrix configuration._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Dim | Int | the dimension of the target space |
| Nim | Int | the dimension of the Hilbert space |
| componentMax | Real | the maximal magnitude of the components |


**Output**

The output is X.

| Output | Type | Description |
| --- | --- | --- |
| X | MathConf(Dim,Nim) | a random mantrix configuration |


**Description**

qmgXrand generates a random matrix configuration. Since random numbers are involved, it is recomended to seed a random state in advance to maintain reproducability. Compare to [[1]](https://arxiv.org/abs/2301.10206) section 4.2.

**Example(s)**

An example for Dim=5 and Nim=4:
```mathematica
SeedRandom[1];
X=qmgXrand[5,4,0.5]
```







# Sets in Target Space



## qmgxCartesianCoordinates
_Generates Cartesian coordinate lines in target space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Dim | Int | the dimension of target space |
| directionP | Int | an integer between 1 and Dim. The direction in which the coordinate lines point |
| directionsT | Int(k) | a list of length k consisting of distinct integers between 1 and Dim, directionP excluded. The directions in which the coordinate lines are stacked |
| lengthP | Real | the step length between two points in the coordinate line |
| lengthT | Real | the step length between two different coordinate lines in each direction in directionsT |
| nP | Int | the number of points in the coordinate lines |
| nT | Int | the number of coordinate lines in each direction in directionsT  |
| center={} | Either {} or Real(Dim) | the center of the coordinate lines, either {} (then {0,...,0} is used as center) or a point in target space |


**Output**

The output is xs.

| Output | Type | Description |
| --- | --- | --- |
| xs | Real((2\*nP+1)\*(2\*nT)^k,Dim) | a list of points in target space |


**Description**

qmgxCartesianCoordinates generates Cartesian coordinate lines in target space.

**Example(s)**

An example for Dim=4 and main direction 2:
```mathematica
xs=qmgxCartesianCoordinates[4,2,{1,3},5,5,100,3,{0,0,1}]
```











## qmgx3DSphericalCoordinates
_Generates three dimensional spherical coordinate lines in target space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| rMin | Int | the minimal radius |
| rMax | Int | the maximal radius |
| nr | Int | the number of points in the radial direction |
| ntheta | Int | the number of poins in the polar direction |
| nphi | Int | the number of points in the azimuthal direction |

**Output**

The output is xs.

| Output | Type | Description |
| --- | --- | --- |
| xs | Real((nr+1)\*(ntheta+1)\*phi,3) | a list of points in target space |


**Description**

qmgx3DSphericalCoordinates generates spherical coordinate lines in $\mathbb{R}^3$.

**Example(s)**

An example for radial coordinate lines:
```mathematica
xs=qmgx3DSphericalCoordinates[1,3,100,4,4]
```

An example for polar coordinate lines:
```mathematica
xs=qmgx3DSphericalCoordinates[1,3,4,100,4]
```

An example for azimuthal coordinate lines:
```mathematica
xs=qmgx3DSphericalCoordinates[1,3,4,4,100]
```















## qmgx3DSphericalCoordinatesSector
_Generates a sector of three dimensional spherical coordinate lines in target space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| rBord={0.5,1.5} | Real(2) | a list consisting of the minimal radius and the maximal radius |
| thetaBord={0.2Pi,0.8Pi} | Real(2) | a list consisting of the minimal polar angle and the maximal polar angle |
| phiBord={-0.015Pi,0.015Pi} | Real(2) | a list consisting of the minimal azimuthal angle and the maximal azimuthal angle|
| nr | Int | the number of poins in the radial direction |
| ntheta | Int | the number of points in the polar direction |
| nphi | Int | the number of points in the azimuthal direction |

**Output**

The output is xs.

| Output | Type | Description |
| --- | --- | --- |
| xs | Real((nr+1)\*(ntheta+1)\*(phi+1),3) | a list of points in target space |


**Description**

qmgx3DSphericalCoordinatesSector generates a sector of spherical coordinate lines in $\mathbb{R}^3$.

**Example(s)**

An example for radial coordinate lines:
```mathematica
xs=qmgx3DSphericalCoordinatesSector[100,4,4]
```

An example for azimuthal coordinate lines and custom bounds:
```mathematica
xs=qmgx3DSphericalCoordinatesSector[{0.1,0.5},{0.3,1},{0.2,0.4},100,4,4]
```












## qmgxSplitHemispheres
_Splits given points in target space into the upper and lower hemisphere._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| xs | Real(k,Dim) | a list of length k consisting of points in target space |

**Output**

The output is {xsUpper,xsLower}.

| Output | Type | Description |
| --- | --- | --- |
| xsUpper | Real(r,Dim) | the points x in xs with x(Dim)>0 |
| xsLower | Real(k-r,Dim) | the points x in xs with x(Dim)≤0 |


**Description**

qmgxSplitHemispheres splits the points in the upper hemisphere of $\mathbb{R}^{Dim}$ from the points in the lower hemisphere.

**Example(s)**

An example for Dim=3:
```mathematica
xs=qmgx3DSphericalCoordinates[1,3,100,4,4];
qmgxSplitHemispheres[xs]
```








## qmgxRandomBall
_SeedRandom in advance recommend; generates random points in a ball in target space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Dim | Int | the dimension of target space |
| radius | Real | the radius of the ball |
| n | Int | the number of random points |
| center={} | Either {} or Real(Dim) | the center of the ball, either {} (then {0,...,0} is used as center) or a point in target space |


**Output**

The output is xs.

| Output | Type | Description |
| --- | --- | --- |
| xs | Real(n,Dim) | a list of points in target space |


**Description**

qmgxRandomBall generates random points in a ball in target space. Since random numbers are involved, it is recomended to seed a random state in advance to maintain reproducability.

**Example(s)**

An example for Dim=4:
```mathematica
SeedRandom[1]
xs=qmgxRandomBall[4,3.5,1000,{0,0,0,1}]
```










## qmgxRandomCube
_SeedRandom in advance recommend; generates random points in a cube in target space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Dim | Int | the dimension of target space |
| length | Real | the side length of the cube |
| n | Int | the number of random points |
| center={} | Either {} or Real(Dim) | the center of the cube, either {} (then {0,...,0} is used as center) or a point in target space |


**Output**

The output is xs.

| Output | Type | Description |
| --- | --- | --- |
| xs | Real(n,Dim) | a list of points in target space |


**Description**

qmgxRandomCube generates random points in a cube in target space. Since random numbers are involved, it is recomended to seed a random state in advance to maintain reproducability.

**Example(s)**

An example for Dim=4:
```mathematica
SeedRandom[1]
xs=qmgxRandomCube[4,2.5,1000,{0,1,0,1}]
```


