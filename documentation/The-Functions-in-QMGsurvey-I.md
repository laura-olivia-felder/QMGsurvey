# Preparation

## MatrixRankWORKAROUND
_For internal use; provides a workaround for MatrixRank that gives zero if the matrix almost vanishes._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| M | Real(m,n) | a real matrix of arbitrary dimensions |
| tolerance | Real | the numerical tolerance in the calculation of the rank |

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Int | the rank of M |

**Description**

MatrixRankWORKAROUND calculates the matrix rank of matrix M with a numerical tolerance.
In comparison to MatrixRank, it always returns 0 if the magnitudes of all components are smaller than the tolerance.

**Example(s)**

An example where the tolerance is important:
```mathematica
M={{1.,2.},{1.,2.0001}};
tolreance=0.01;
MatrixRankWORKAROUND[M,tolerance]
```

An example where MatrixRankWORKAROUND and MatrixRank are different:
```mathematica
M={{0.001.,0.},{0.,0.001}};
tolreance=0.01;
{MatrixRankWORKAROUND[M,tolerance],MatrixRank[M,tolerance]}
```




## NullSpaceWORKAROUND
_For internal use; provides a workaround for NullSpace that gives the full space if the matrix almost vanishes._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| M | Real(m,m) | a real square matrix of arbitrary dimensions |
| tolerance | Real | the numerical tolerance in the calculation |

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(n,m) | the NullSpace (=kernel) of M, where out(a) is a basis vector for each index a and n=MatrixRankWORKAROUND[M,tolerance] |

**Description**

NullSpaceWORKAROUND calculates the kernel of matrix M (represented by a basis) with a numerical tolerance.
In comparison to MatrixRank, it always returns 0 if the magnitudes of all components are smaller than the tolerance.

**Example(s)**

An example where the tolerance is important:
```mathematica
M={{1.,2.},{1.,2.0001}};
tolreance=0.01;
NullSpaceWORKAROUND[M,tolerance]
```

An example where NullSpaceWORKAROUNDand NullSpaceare different:
```mathematica
M={{0.001.,0.},{0.,0.001}};
tolreance=0.01;
{NullSpaceWORKAROUND[M,tolerance],NullSpace[M,tolerance]}
```





## RealMatrixRank
_For internal use; calculates the real rank of a complex matrix M._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| M | Complex(m,n) | a complex matrix of arbitrary dimensions |
| tolerance | Real | the numerical tolerance in the calculation |

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Int | the real rank of M |

**Description**

RealMatrixRank calculates the real matrix rank of the complex matrix M with a numerical tolerance.
This is the dimension of the real kernel of M, the maximal real subspace of the kernel of M.
This is calculated by calculating MatrixRankWORKAROUND of Join[Re[M],Im[M]].
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example where RealMatrixRank and MatrixRank are not the same:
```mathematica
M={{1.,1.},{I,I}};
tolreance=0.01;
{RealMatrixRank[M,tolerance],MatrixRank[M,tolerance]}
```









## RealNullSpace
_For internal use; calculates the real kernel of a complex matrix M._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| M | Complex(m,m) | a complex square matrix of arbitrary dimensions |
| tolerance | Real | the numerical tolerance in the calculation |

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Real(n,m) | the real NullSpace (=real kernel) of M, where out(a) is a basis vector for each index a and n=RealMatrixRank[M,tolerance] |

**Description**

RealNullSpace calculates the real kernel of the complex matrix M (represented by a basis) with a numerical tolerance.
This is calculated by calculating NullSpaceWORKAROUND of Join[Re[M],Im[M]] (what works although Join[Re[M],Im[M]] is not a square matrix).
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example where RealNullSpace  and NullSpace are not the same:
```mathematica
M={{1.,1.},{I,I}};
tolreance=0.01;
{RealNullSpace[M,tolerance],NullSpace[M,tolerance]}
```







## CheckSubspace
_For internal use; checks if the linear span of v is a linear subspace of the linear span of w._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| v | Real(nv,m) | a subspace of R^m of dimension nv (represented by a basis), where v(a) is a basis vector for each index a |
| w | Real(nw,m) | a subspace of R^m of dimension nw (represented by a basis), where w(a) is a basis vector for each index a |
| tolerance | Real | the numerical tolerance in the calculation |

**Output**

The output is out.

| Output | Type | Description |
| --- | --- | --- |
| out | Bool | True if v is a linear subspace of w, else False |

**Description**

CheckSubspace checks if the vectorspace V represented by v is a linear subspace of the vector space W represented by w up to the given tolerance.
Note that V is a subspace of W if and only if the dimension of the span of all basis vectors of W is the same as the dimension of the span of all basis vectors of V and W together.
Therefore, it suffices to check if the matrix rank of w is the same as the matrix rank of Join[v,w] as the dimension of the span of basis vectors is the same as the matrix rank of the matrix built from the basis vectors.
This is implemented in CheckSubspace.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An basic example:
```mathematica
v={{1.,0.}};
w={{1.,0.},{0.,1.}};
tolreance=0.01;
CheckSubspace[v,w,tolerance]
```

An example where the tolerance is important:
```mathematica
v={{1.,0.0001}};
w={{1.,0.}};
tolreance=0.01;
CheckSubspace[v,w,tolerance]
```

An example where v is not a subspace of w:
```mathematica
v={{1.,0.}};
w={};
tolreance=0.01;
CheckSubspace[v,w,tolerance]
```





# Basic Calculations

## qmgNondegeneracyProbe
_Checks if the lowest eigenspace of the Hamiltonian is nondegenerate at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | a point in target space |

**Output**

The output is {Nondegeneracy,degree}.

| Output | Type | Description |
| --- | --- | --- |
| Nondegeneracy | Bool | True if the lowest eigenspace of the Hamiltonian is non degenerate at x, else False |
| degree | Int| the degree of the degenerace, i.e. the dimension of the lowest eigenspace of the Hamiltonian at x |

**Description**

qmgNondegeneracyProbe checks if the lowest eigenspace of the Hamiltonian is non degenerate at a given point x and returns the degree of degeneracy.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example where the lowest eigenspace is not degenerate:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
qmgNondegeneracyProbe[X,x]
```

An example where the lowest eigenspace is degenerate:
```mathematica
X=qmgXsu2[4];
x={0,0,0};
qmgNondegeneracyProbe[X,x]
```







## qmgLightweight
_Calculates the lowest eigenstate of the Hamiltonian and the correpsonding eigenvalue at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | a point in target space |

**Output**

The output is EigFixed.

| Output | Type | Description |
| --- | --- | --- |
| EigFixed | {Real,Complex(Nim,1)} | EigFixed(1) (also called lambda) is the eigenvalue of the Hamiltonian at x. EigFixed(2) (also called EigState) is the corresponding normalized eigenvector |

**Description**

qmgLightweight calculates the lowest eigenvalue of the Hamiltonian for a given matrix configuration X at the point x and the corresponding eigenvector.
The eigenvector is normalized and its complex phase is fixed such that the last component (that is Eig(2,Nim,1)) is greater or equal 0.
Attention: In the cases where the last component vanishes, this choice is not unique.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
qmgLightweight[X,x]
```








## qmgLightweightAsymptotic
_Calculates the lowest eigenstate of the (asymptotic) Hamiltonian and the correpsonding eigenvalue at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | a point in target space |
| Asymptotic=True | Bool | whether or not to use the asymptotic Hamiltonian |

**Output**

The output is EigFixed.

| Output | Type | Description |
| --- | --- | --- |
| EigFixed | {Real,Complex(Nim,1)} | EigFixed(1) is the eigenvalue of the (asymptotic) Hamiltonian at x. EigFixed(2) is the corresponding normalized eigenvector |

**Description**

qmgLightweightAsymptotic calculates the lowest eigenvalue of the (asymptotic) Hamiltonian for a given matrix configuration X at the point x and the corresponding eigenvector.
If Asymptotic=False, qmgLightweightAsymptotic  agrees with qmgLightweight.
The eigenvector is normalized and its complex phase is fixed such that the last component (that is Eig(2,Nim,1)) is greater or equal 0.
Attention: In the cases where the last component vanishes, this choice is not unique.
Compare to [[1]](https://arxiv.org/abs/2301.10206) sections 3.1 and 3.2.

**Example(s)**

Calculation of an asymptotic quasi-coherent state:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
qmgLightweightAsymptotic[X,x]
```

Check that for Asymptotic=False qmgLightweightAsymptotic is the same as qmgLightweight:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
qmgLightweightAsymptotic[X,x,False]==qmgLightweight[X,x]
```








## qmgBasic
_Calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and many more important quantities like g, omega, the disperion and others at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | a point in target space |

**Output**

The output is {EigFixed,{h,g,omega,theta},{xBold,xBoldPartial},{Displacement,Dispersion},{H,HPseudoInverse,Xfrac,CovDer}}.

| Output | Type | Description |
| --- | --- | --- |
| EigFixed | {Real,Complex(Nim,1)} | EigFixed(1) is the eigenvalue of the Hamiltonian at x. EigFixed(2) is the corresponding normalized eigenvector |
| h | Complex(Dim,Dim) | the hermitian form at x |
| g | Real(Dim,Dim) | the quantum metric at x |
| omega | Real(Dim,Dim) | the (would be) symplectic form at x |
| theta | Real(Dim,Dim) | theta at x |
| xBold | Real(Dim) | the embedded point at x, that is the expectation value of the matrix configuration in the quasi-coherent state at x |
| xBoldPartial | Real(Dim,Dim) | A list of all partial derivatives of the embedded point at x (the first index is the index of the derivative) |
| Displacement | Real | the displacement at x |
| Dispersion | Real | the dispersion at x |
| H | Complex(Nim,Nim) | the Hamiltonian at x |
| HPseudoInverse | Complex(Nim,Nim) | the pseudo inverse of H-EigFixed(1)|
| Xfrac | Complex(Nim,Nim) | the operator from [[1]](https://arxiv.org/abs/2301.10206) equation (49) that is used in the calculations |
| CovDer | Complex(Dim,Nim) | the covariant derivative of the quasi-coherent state (the first index is the index of the derivative) |

**Description**

qmgBasic calculates the most important quantities for a given matrix configuration X at the point x.
EigFixed is the same as the output of qmgLightweight.
We have the relations h=ConjugateTranspose[h] and h=(g-I\*omega)/2 (note that this slightly deviates from [[1]](https://arxiv.org/abs/2301.10206), look at comment 21).
CovDer, h and xBoldPartial are calculated using Xfrac, avoiding the use of differential quotients.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
qmgBasic[X,x]
```







## qmgBasicHighPrecision
_The same as qmgBasic but to arbitrary precision; calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and many more important quantities like g, omega, the disperion and others at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Xp | MatConf(Dim,Nim) | a matrix configuration of arbitrary precision |
| xp | Real(Dim) | a point in target space of arbitrary precision |
| precision | Int | the desired precision used in all calculations, compare with [SetPrecision](https://reference.wolfram.com/language/ref/SetPrecision.html) |

**Output**

The output is {EigFixed,{h,g,omega,theta},{xBold,xBoldPartial},{Displacement,Dispersion},{H,HPseudoInverse,Xfrac,CovDer}}.

| Output | Type | Description |
| --- | --- | --- |
| EigFixed | {Real,Complex(Nim,1)} | EigFixed(1) is the eigenvalue of the Hamiltonian at xp. EigFixed(2) is the corresponding normalized eigenvector |
| h | Complex(Dim,Dim) | the hermitian form at xp |
| g | Real(Dim,Dim) | the quantum metric at xp |
| omega | Real(Dim,Dim) | the (would be) symplectic form at xp |
| theta | Real(Dim,Dim) | theta at xp |
| xBold | Real(Dim) | the embedded point at xp, that is the expectation value of the matrix configuration in the quasi-coherent state at xp |
| xBoldPartial | Real(Dim,Dim) | a list of all partial derivatives of the embedded point at xp (the first index is the index of the derivative) |
| Displacement | Real | the displacement at xp |
| Dispersion | Real | the dispersion at xp |
| H | Complex(Nim,Nim) | the Hamiltonian at xp |
| HPseudoInverse | Complex(Nim,Nim) | the pseudo inverse of H-EigFixed(1)|
| Xfrac | Complex(Nim,Nim) | the operator from [[1]](https://arxiv.org/abs/2301.10206) equation (49) that is used in the calculations |
| CovDer | Complex(Dim,Nim) | the covariant derivative of the quasi-coherent state (the first index is the index of the derivative) |

**Description**

qmgBasicHighPrecision is essentially the same as qmgBasic, but works with chosen precision.
This allows one to control the accuracy of the result.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
Xp=qmgXsu2[4];
xp={0.,0.,1.+10.^-90};
qmgBasicHighPrecision[Xp,xp,100]
```









## qmgEmbedding
_Calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and xBold at a given point._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | a point in target space |
| Asymptotic=False | Bool | whether or not to use the asymptotic Hamiltonian |

**Output**

The output is {EigFixed,xBold}.

| Output | Type | Description |
| --- | --- | --- |
| EigFixed | {Real,Complex(Nim,1)} | EigFixed(1) is the eigenvalue of the Hamiltonian at x. EigFixed(2) is the corresponding normalized eigenvector |
| xBold | Real(Dim) | the embedded point at x, that is the expectation value of the matrix configuration in the quasi-coherent state at x |


**Description**

qmgEmbedding is essentially the same as qmgLightWeight, but additionally also calculates xBold as in the output of qmgBasic.
Attention: Here, the default for Asymptotic is False.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
qmgEmbedding[X,x]
```











## qmgGeom
_Calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue, xBold, the displacement and the dispersion at a given point._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | a point in target space |

**Output**

The output is {EigFixed,xBold,Displacement,Dispersion}.

| Output | Type | Description |
| --- | --- | --- |
| EigFixed | {Real,Complex(Nim,1)} | EigFixed(1) is the eigenvalue of the Hamiltonian at x. EigFixed(2) is the corresponding normalized eigenvector |
| xBold | Real(Dim) | the embedded point at x, that is the expectation value of the matrix configuration in the quasi-coherent state at x |
| Displacement | Real | the displacement at x |
| Dispersion | Real | the dispersion at x |

**Description**

qmgGeom is essentially the same as qmgBasic, but only calculates EigFixed, xBold, Displacement and Dispersion.
Here, we have the relation 2\*lambda=Displacement+Dispersion where lambda=EigFixed(1).
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
qmgGeom[X,x]
```









## qmgProbe
_Calculates qmgBasic and checks many important properties at a given point like the dimension of the quantum manifold, and the vector space structure of the kernels of g, omega and more._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| x | Real(Dim) | a point in target space |
| tolerance=10^-8 | Real | the tolerance used in calculations of ranks and kernels |


**Output**

The output is {EigFixed,{h,g,omega,theta},{xBold,xBoldPartial},{Displacement,Dispersion},{H,HPseudoInverse,Xfrac,CovDer},{kernels,ranks,kernelsComparison}}
.

| Output | Type | Description |
| --- | --- | --- |
| EigFixed | {Real,Complex(Nim,1)} | EigFixed(1) is the eigenvalue of the Hamiltonian at x. EigFixed(2) is the corresponding normalized eigenvector |
| h | Complex(Dim,Dim) | the hermitian form at x |
| g | Real(Dim,Dim) | the quantum metric at x |
| omega | Real(Dim,Dim) | the (would be) symplectic form at x |
| theta | Real(Dim,Dim) | theta at x |
| xBold | Real(Dim) | the embedded point at x, that is the expectation value of the matrix configuration in the quasi-coherent state at x |
| xBoldPartial | Real(Dim,Dim) | a list of all partial derivatives of the embedded point at x (the first index is the index of the derivative) |
| Displacement | Real | the displacement at x |
| Dispersion | Real | the dispersion at x |
| H | Complex(Nim,Nim) | the Hamiltonian at x |
| HPseudoInverse | Complex(Nim,Nim) | the pseudo inverse of H-EigFixed(1) |
| Xfrac | Complex(Nim,Nim) | the operator from [[1]](https://arxiv.org/abs/2301.10206) equation (49) that is used in the calculations |
| CovDer | Complex(Dim,Nim) | the covariant derivative of the quasi-coherent state (the first index is the index of the derivative) |
| kernels | {Real(k_1,Dim),Real(k_2,Dim),Real(k_3,Dim),Real(k_4,Dim)} | contains a list of the (real) kernels with respect to the tolerance of Txq, g, omega and theta. These are each represented by a list of basis vectors, thus k_i is the dimension of the kernel (the first index is the index of the lernel, the second index is the index of the basis element and the third index is the index of the vector component) |
| ranks | {Int,Int,Int,Int} | a list of the (real) ranks with respect to the tolerance of Txq, g, omega and theta |
| kernelsComparison | {Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool} | Contains a list of subspace comparisons with respect to the tolerance. Each element of the list is True if the (real) kernel of A is a linear subspace of the (real) kernel of B, else False and that for all tuples (A,B): {(Txq,g),(Txq,omega),(g,omega),(omega,g),(Txq,theta),(g,theta),(theta,g),(omega,theta),(theta,omega)} |


**Description**

qmgProbe calculates all outputs of qmgBasic and calculates the (real) kernels and (real) ranks of the tangent map of q (that is Txq), g, omega and theta
and further compares their kernels.
These comparisons have a nice interpretation: If for example kernelsComparison(1) is True that means that g is zero on all vectors outside the range of Txq (what is clearly expected); if for example kernelsComparison(2) is true that means that omega is zero at least on all vectors that are in the kernel of g.
A few non trivial relations are expekted: k_i+ranks(i)=Dim, ranks(1)=ranks(2)≥ranks(3), kernelsComparison(1)=kernelsComparison(2)=True.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.1.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
qmgProbe[X,x]
```











## qmgCurveLength
_Calculates the length of a discrete curve in target space with respect to the Euclidian and quantum metric._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| DiscreteCurve | Real(k,Dim) | a descrete curve in target space, consisting of k points in target space |

**Output**

The output is {EuklidLength,QuantumLength}.

| Output | Type | Description |
| --- | --- | --- |
| EuklidLength | Real | the length of the curve with respect to the Euclidean metric (represented by the identity matrix) |
| QuantumLength | Real | the length of the curve with respect to the quantum metric g |

**Description**

qmgCurveLength treats every element DiscreteCurve(i) as a point $x_i=\gamma(i)$ of a smooth curve $\gamma$ in target space.
The output consists of the Euclidean legth - length with respect to the Euclidean metric and the quantum length - the length with respect to the quantum metric.
These two can deviate strongly.
Compare to [[1]](https://arxiv.org/abs/2301.10206) section 3.3.2.

**Example(s)**

An example for the fuzzy sphere:
```mathematica
DiscreteCurve=Table[{0,0,1+a/100},{a,10}];
qmgCurveLength[X,DiscreteCurve]
```
































# Presentation

## qmgPresent
_Constructs a table with the most interesting output of qmgProbe._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| Xxs | {{MathConf[Dim_1,Nim_1],Real[Dim_1],Str},...,{MathConf[Dim_n,Nim_n],Real[Dim_n],Str}} | a list of length n consisting of lists consisting of a matrix configuration (X), a point in target space (x) and a name for the row in the table (Name) |
| tolerance=10^-8 | Real | the numerical tolerance in the calculations of qmgProbe |

**Output**

The output is table.

| Output | Type | Description |
| --- | --- | --- |
| table | Mathematica Table | a table with row i+1 showing the results of qmgProbe for X=Xxs(i,1) and x=Xxs(i,2) |

**Description**

qmgPresent calculates qmgProbe for multiple matrix configurations and points in target space and presents (selected) output in a table.

**Example(s)**

An example for the fuzzy sphere with different choices for Nim and x:
```mathematica
X1=qmgXsu2[4];
X2=qmgXsu2[9];
x1={0,0,1};
x2={1,0,0};
Name1="N=4";
Name2="N=9";
Xxs={{X1,x1,Name1},{X2,x2,Name2}};
qmgPresent[Xxs]
```








## qmgPlotTS
_Plots points in target space._

**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| xsets | {{Real(k_1,Dim),Color},...,{Real(k_n,Dim),Color}} | a list of length n consisting of lists. Each of these consists of a list of k_n points in target space (the first index specifies a point) and a color that is used for visualization |
| projectionmode="default" | Str | the mode used to construct the plots. Either "default", "project" or "slice". For more information see the description |
| projectionData={} | if projectionmode=="default": {};<br> if projection mode=="project": {Real(3,Dim),Real(Dim,Dim)};<br> if projection mode=="slice": {Real(3,Dim),Real(Dim,Dim),Real(Dim),Real} | the data that is needed for construction of the plot <br> in the "default" case, nothing is needed;<br> in the "project" case this is a list consisting of a truncated orthogonal matrix Orth and projector P of rank three such that the composition of Orth and P also has rank three;<br> in the "slice" case the latter list is suplemented by a vector v specifying the slice and a slice tolerance epsilon |

**Output**

The output is {plot,pointset,Colors}.

| Output | Type | Description |
| --- | --- | --- |
| plot | Mathematica Plot | a plot of the processed input points |
| pointset | {Real(k_1,3),...,Real(k_n,3)} | a list of length n consisting of the processed input points |
| Colors | {Color,...,Color} | a list of length n consisting of the input colors |

**Description**

qmgPlotTS plots points in target space for a given matrix configuration.
For that purpose, three modes are available.<br>
The mode "default" is the same as "project" with
```mathematica
(Orth=ConstantArray[0,{3,Dim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,3]]=1);
P=DiagonalMatrix[Join[{1,1,1},ConstantArray[0,Dim-3]]];
```
The mode "project" uses the composition of Orth and P to map the input points into the three dimensional Euclidean space. These are then plotted according to the chosen colors.<br>
The mode "slice" at first selects points $x$ such that $\vert(\mathbb{1}-P)\cdot x-v\vert<\epsilon$ (for epsilon=0 this condition defines a three dimensional plane through v in target space). These are then processed as in the mode "project".
<br>
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
qmgPlotTS[X,xsets]
```

An example for a "project" plot:
```mathematica
X=qmgXsu2[4];
xs1=qmgxRandomBall[3,1,5000];
xs2=Table[{0,0,1+a/100},{a,10}];
color1=Red;
color2=Green;
projectionmode="project";
Dim=Length[X];
Nim=Length[X[[1]]];
(Orth=ConstantArray[0,{3,Dim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,3]]=1);
P=DiagonalMatrix[Join[{1,1,1},ConstantArray[0,Dim-3]]];
projectionData={Orth,P};
xsets={{xs1,color1},{xs2,color2}};
qmgPlotTS[X,xsets,projectionmode,projectionData]
```

An example for a "slice" plot:
```mathematica
X=qmgXsu2[4];
xs1=qmgxRandomBall[3,1,5000];
xs2=Table[{0,0,1+a/100},{a,10}];
color1=Red;
color2=Green;
projectionmode="slice";
Dim=Length[X];
Nim=Length[X[[1]]];
(Orth=ConstantArray[0,{3,Dim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,3]]=1);
P=DiagonalMatrix[Join[{1,1,1},ConstantArray[0,Dim-3]]];
v={0,0,1};
epsilon=0.3;
projectionData={Orth,P,v,epsilon};
xsets={{xs1,color1},{xs2,color2}};
qmgPlotTS[X,xsets,projectionmode,projectionData]
```









## qmgPlotQM
_Plots points in the quantum manifold, represented by points in target space._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| xsets | {{Real(k_1,Dim),Color},...,{Real(k_n,Dim),Color}} | a list of length n consisting of lists. Each of these consists of a list of k_n points in target space (the first index specifies a point) and a color that is used for visualization |
| Asymptotic=False | Bool | whether or not to use the asymptotic Hamiltonian |
| projectionmode="default" | Str | the mode used to construct the plots. Either "default", "project" or "slice". For more information see the description |
| projectionData={} | if projectionmode=="default": {};<br> if projection mode=="project": {Real(3,2\*Nim),Real(2\*Nim,2\*Nim)};<br> if projection mode=="slice": {Real(3,2\*Nim),Real(2\*Nim,2\*Nim),Real(2\*Nim),Real} | the data that is needed for construction of the plot. Compared with qmgPlotTS, we work with $\mathbb{C}^{Nim}\cong\mathbb{R}^{2Nim}$ instead of $\mathbb{R}^{Dim}$ <br> in the "default" case, nothing is needed;<br> in the "project" case this is a list consisting of a truncated orthogonal matrix Orth and projector P of rank three such that the composition of Orth and P also has rank three;<br> in the "slice" case the latter list is suplemented by a vector v specifying the slice and a slice tolerance epsilon |

**Output**

The output is {plot,pointset,Colors}.

| Output | Type | Description |
| --- | --- | --- |
| plot | Mathematica Plot | a plot of the processed input points |
| pointset | {Real(k_1,3),...,Real(k_n,3)} | a list of length n consisting of the processed input points |
| Colors | {Color,...,Color} | a list of length n consisting of the input colors |

**Description**

qmgPlotQM plots quasi-coherent states coming from points in target space for a given matrix configuration (thus this should be thought of plots of the quantum manifold).
For that purpose, three modes are available.<br>
The mode "default" is the same as "project" with
```mathematica
(Orth=ConstantArray[0,{3,2*Nim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,Nim+1]]=1);
P=DiagonalMatrix[Join[{1,1},ConstantArray[0,Nim-2],{1},ConstantArray[0,Nim-1]]];
```
The mode "project" at first calculates qmgLightweightAsymptotic[X,x,Asymptotic] for each point.
The resulting quasi-coherent state is then split into real and imaginary part (then denoted by z) and with the composition of Orth and P mapped into the three dimensional Euclidean space. The outcome is then plotted according to the chosen colors.<br>
The mode "slice" at first selects quasi-coherent states $z$ such that $\vert(\mathbb{1}-P)\cdot z-v\vert<\epsilon$ (for epsilon=0 this condition defines a three dimensional plane through v in $\mathbb{R}^{2Nim}$). These are then processed as in the mode "project".
<br>
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
qmgPlotQM[X,xsets]
```


An example for an asymptotic "default" plot:
```mathematica
X=qmgXsu2[4];
xs1=qmgxRandomBall[3,1,5000];
xs2=Table[{0,0,1+a/100},{a,10}];
color1=Red;
color2=Green;
xsets={{xs1,color1},{xs2,color2}};
qmgPlotQM[X,xsets,True]
```

An example for a "project" plot:
```mathematica
X=qmgXsu2[4];
xs1=qmgxRandomBall[3,1,5000];
xs2=Table[{0,0,1+a/100},{a,10}];
color1=Red;
color2=Green;
projectionmode="project";
Dim=Length[X];
Nim=Length[X[[1]]];
(Orth=ConstantArray[0,{3,2*Nim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,3]]=1;Orth);
P=DiagonalMatrix[Join[{1,1,1},ConstantArray[0,2*Nim-3]]];
projectionData={Orth,P};
xsets={{xs1,color1},{xs2,color2}};
qmgPlotQM[X,xsets,False,projectionmode,projectionData]
```

An example for a "slice" plot:
```mathematica
X=qmgXsu2[4];
xs1=qmgxRandomBall[3,1,5000];
xs2=Table[{0,0,1+a/100},{a,10}];
color1=Red;
color2=Green;
projectionmode="slice";
Dim=Length[X];
Nim=Length[X[[1]]];
(Orth=ConstantArray[0,{3,2*Nim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,3]]=1;Orth);
P=DiagonalMatrix[Join[{1,1,1},ConstantArray[0,2*Nim-3]]];
xv={0,0,1};
(v=Transpose[qmgLightweight[Xsfz4,xv][[2]]][[1]];v=Join[Re[v],Im[v]]);
epsilon=0.3;
projectionData={Orth,P,v,epsilon};
xsets={{xs1,color1},{xs2,color2}};
qmgPlotQM[X,xsets,False,projectionmode,projectionData]
```









## qmgPlotEQS
_Plots points in the embedded quantum space, represented by points in target space._


**Arguments**

| Argument | Type | Description |
| --- | --- | --- |
| X | MatConf(Dim,Nim) | a matrix configuration |
| xsets | {{Real(k_1,Dim),Color},...,{Real(k_n,Dim),Color}} | a list of length n consisting of lists. Each of these consists of a list of k_n points in target space (the first index specifies a point) and a color that is used for visualization |
| Asymptotic=False | Bool | whether or not to use the asymptotic Hamiltonian |
| projectionmode="default" | Str | the mode used to construct the plots. Either "default", "project" or "slice". For more information see the description |
| projectionData={} | if projectionmode=="default": {};<br> if projection mode=="project": {Real(3,Dim),Real(Dim,Dim)};<br> if projection mode=="slice": {Real(3,Dim),Real(Dim,Dim),Real(Dim),Real} | the data that is needed for construction of the plot <br> in the "default" case, nothing is needed;<br> in the "project" case this is a list consisting of a truncated orthogonal matrix Orth and projector P of rank three such that the composition of Orth and P also has rank three;<br> in the "slice" case the latter list is suplemented by a vector v specifying the slice and a slice tolerance epsilon |

**Output**

The output is {plot,pointset,Colors}.

| Output | Type | Description |
| --- | --- | --- |
| plot | Mathematica Plot | a plot of the processed input points |
| pointset | {Real(k_1,3),...,Real(k_n,3)} | a list of length n consisting of the processed input points |
| Colors | {Color,...,Color} | a list of length n consisting of the input colors |

**Description**

qmgPlotEQS plots embedded points for a given matrix configuration.
For that purpose, three modes are available.<br>
The mode "default" is the same as "project" with
```mathematica
(Orth=ConstantArray[0,{3,Dim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,3]]=1);
P=DiagonalMatrix[Join[{1,1,1},ConstantArray[0,Dim-3]]];
```
The mode "project" at first calculates qmgEmbedding[X,x,Asymptotic] for each point.
The resulting xBold (denoted by $\mathbf{x}$) is mapped into the three dimensional Euclidean space with the composition of Orth and P. The outcome is then plotted according to the chosen colors.<br>
The mode "slice" at first selects embedded points $\mathbf{x}$ such that $\vert(\mathbb{1}-P)\cdot \mathbf{x}-v\vert<\epsilon$ (for epsilon=0 this condition defines a three dimensional plane through v in $\mathbb{R}^{Dim}$). These are then processed as in the mode "project".
<br>
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
qmgPlotEQS[X,xsets]
```

An example for an asymptotic "default" plot:
```mathematica
X=qmgXsu2[4];
xs1=qmgxRandomBall[3,1,5000];
xs2=Table[{0,0,1+a/100},{a,10}];
color1=Red;
color2=Green;
xsets={{xs1,color1},{xs2,color2}};
qmgPlotEQS[X,xsets,True]
```

An example for a "project" plot:
```mathematica
X=qmgXsu2[4];
xs1=qmgxRandomBall[3,1,5000];
xs2=Table[{0,0,1+a/100},{a,10}];
color1=Red;
color2=Green;
projectionmode="project";
Dim=Length[X];
Nim=Length[X[[1]]];
(Orth=ConstantArray[0,{3,Dim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,3]]=1);
P=DiagonalMatrix[Join[{1,1,1},ConstantArray[0,Dim-3]]];
projectionData={Orth,P};
xsets={{xs1,color1},{xs2,color2}};
qmgPlotEQS[X,xsets,False,projectionmode,projectionData]
```

An example for a "slice" plot:
```mathematica
X=qmgXsu2[4];
xs1=qmgxRandomBall[3,1,5000];
xs2=Table[{0,0,1+a/100},{a,10}];
color1=Red;
color2=Green;
projectionmode="slice";
Dim=Length[X];
Nim=Length[X[[1]]];
(Orth=ConstantArray[0,{3,Dim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,3]]=1);
P=DiagonalMatrix[Join[{1,1,1},ConstantArray[0,Dim-3]]];
v={0,0,1};
epsilon=0.3;
projectionData={Orth,P,v,epsilon};
xsets={{xs1,color1},{xs2,color2}};
qmgPlotEQS[X,xsets,False,projectionmode,projectionData]
```
