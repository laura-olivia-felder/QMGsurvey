(* ::Package:: *)

(*
   Copyright 2023 Laurin J. Felder

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


(*
This file is part of the software package QMGsurvey (released at https://github.com/laurinjfelder/QMGsurvey).
See https://github.com/laurinjfelder/QMGsurvey/wiki for more information and documentation.
QMGsurvey is a Mathematica based program that can be used to analyse quantum matrix geometries, based on the algorithms from https://arxiv.org/abs/2301.10206.
*)


(* ::Section:: *)
(*Beginning*)


(* ::Text:: *)
(*[1] refers to https://arxiv.org/pdf/2301.10206.pdf*)


BeginPackage["QMG`"];


MatrixRankWORKAROUND::usage="For internal use; provides a workaround for MatrixRank that gives zero if the matrix almost vanishes.";


NullSpaceWORKAROUND::usage="For internal use; provides a workaround for NullSpace that gives the full space if the matrix almost vanishes.";


RealMatrixRank::usage="For internal use; calculates the real rank of a complex matrix M.";


RealNullSpace::usage="For internal use; calculates the real kernel of a complex matrix M.";


CheckSubspace::usage="For internal use; checks if the linear span of v is a linear subspace of the linear span of w.";


qmgNondegeneracyProbe::usage="Checks if the lowest eigenspace of the Hamiltonian is nondegenerate at a given point.";


qmgLightweight::usage="Calculates the lowest eigenstate of the Hamiltonian and the correpsonding eigenvalue at a given point.";


qmgLightweightAsymptotic::usage="Calculates the lowest eigenstate of the (asymptotic) Hamiltonian and the correpsonding eigenvalue at a given point.";


qmgBasic::usage="Calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and many more important quantities like g, omega, the disperion and others at a given point.";


qmgBasicHighPrecision::usage="The same as qmgBasic but to arbitrary precision; calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and many more important quantities like g, omega, the disperion and others at a given point.";


qmgEmbedding::usage="Calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and xBold at a given point.";


qmgGeom::usage="Calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue, xBold, the displacement and the dispersion at a given point.";


qmgProbe::usage="Calculates qmgBasic and checks many important properties at a given point like the dimension of the quantum manifold, and the vector space structure of the kernels of g, omega and more.";


qmgCurveLength::usage="Calculates the length of a discrete curve in target space with respect to the Euclidian and quantum metric.";


qmgPresent::usage="Constructs a table with the most interesting output of qmgProbe.";


qmgPlotTS::usage="Plots points in target space.";


qmgPlotQM::usage="Plots points in the quantum manifold, represented by points in target space.";


qmgPlotEQS::usage="Plots points in the embedded quantum space, represented by points in target space.";


cqmgLightweightINIT::usage="Initialization function of compiled version of qmgLightweight; cqmgLightweight calculates the lowest eigenstate of the Hamiltonian and the correpsonding eigenvalue at a given point.";


cqmgLightweightEXTR::usage="Extraction function of compiled version of qmgLightweight; cqmgLightweight calculates the lowest eigenstate of the Hamiltonian and the correpsonding eigenvalue at a given point.";


cqmgBasicINIT::usage="Initialization function of compiled version of cqmgBasic; cqmgBasic calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and many more important quantities like g, omega, the disperion and others at a given point.";


cqmgBasicEXTR::usage="Extraction function of compiled version of cqmgBasic; cqmgBasic calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and many more important quantities like g, omega, the disperion and others at a given point.";


cqmgEmbeddingINIT::usage="Initialization function of compiled version of cqmgEmbedding; cqmgEmbedding calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and xBold at a given point.";


cqmgEmbeddingEXTR::usage="Extraction function of compiled version of cqmgEmbedding; cqmgEmbedding calculates the lowest eigenstate of the Hamiltonian, the correpsonding eigenvalue and xBold at a given point.";


cqmgProjectINIT::usage="For internal use; Initialization function of cqmgProject; cqmgProject performs the projection in to R^3 for points in target space, the quantum manifold and the embedded quantum space.";


cqmgPlot::usage="Compiled unified version of qmgPlotTS, qmgPlotQM and qmgPlotEQS. Plots points in target space, the quantum manifold and the embedded quantum space, represented by points in target space.";


cqmgTSleafBasicINIT::usage="Initialization function of cqmgTSleafBasic, a specialized version of cqmgBasic; cqmgTSleafBasic calculates the lowest eigenstate of the Hamiltonian, theta and xBold for a given point.";


cqmgTSleafBasicEXTR::usage="Extraction function of cqmgTSleafBasic, a specialized version of cqmgBasic; cqmgTSleafBasic calculates the lowest eigenstate of the Hamiltonian, theta and xBold for a given point.";


cqmgQMleafBasicINIT::usage="Initialization function of cqmgQMleafBasic, a specialized version of cqmgBasic; cqmgQMleafBasic calculates the lowest eigenstate of the Hamiltonian, omega and xBold for a given point.";


cqmgQMleafBasicEXTR::usage="Extraction function of cqmgQMleafBasic, a specialized version of cqmgBasic; cqmgQMleafBasic calculates the lowest eigenstate of the Hamiltonian, omega and xBold for a given point.";


cqmgGQMleafBasicINIT::usage="Initialization function of cqmgGQMleafBasic, a specialized version of cqmgBasic; cqmgGQMleafBasic calculates the lowest eigenstate of the Hamiltonian, g and xBold for a given point.";


cqmgGQMleafBasicEXTR::usage="Extraction function of cqmgGQMleafBasic, a specialized version of cqmgBasic; cqmgGQMleafBasic calculates the lowest eigenstate of the Hamiltonian, g and xBold for a given point.";


cqmgKaehlerBasicINIT::usage="Initialization function of cqmgKaehlerBasic, a specialized version of cqmgBasic; cqmgKaehlerBasic calculates the lowest eigenstate of the Hamiltonian and CovDer for a given point.";


cqmgKaehlerBasicEXTR::usage="Extraction function of cqmgKaehlerBasic, a specialized version of cqmgBasic; cqmgKaehlerBasic calculates the lowest eigenstate of the Hamiltonian and CovDer for a given point.";


cqmgTSleafDistributionINIT::usage="Initialization function of cqmgTSleafDistribution; cqmgTSleafDistribution calculates the target space based distribution for a given point.";


cqmgTSleafDistributionEXTR::usage="Extraction function of cqmgTSleafDistribution; cqmgTSleafDistribution calculates the target space based distribution for a given point.";


cqmgQMleafDistributionINIT::usage="Initialization function of cqmgQMleafDistribution; cqmgQMleafDistribution calculates the quantum manifold based distribution for a given point.";


cqmgQMleafDistributionEXTR::usage="Extraction function of cqmgQMleafDistribution; cqmgQMleafDistribution calculates the quantum manifold based distribution for a given point.";


cqmgGQMleafDistributionINIT::usage="Initialization function of cqmgGQMleafDistribution; cqmgGQMleafDistribution calculates the quantum manifold and g based distribution for a given point.";


cqmgGQMleafDistributionEXTR::usage="Extraction function of cqmgGQMleafDistribution; cqmgGQMleafDistribution calculates the quantum manifold and g based distribution for a given point.";


cqmgDistribution::usage="Unification of cqmgTSleafDistribution, cqmgQMleafDistribution and cqmgGQMleafDistribution; calculates the distribution for a given point.";


cqmgTSleafCurveIterationINIT::usage="Initialization function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the target space based distribution.";


cqmgTSleafCurveIterationEXTR::usage="Extraction function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the target space based distribution.";


cqmgQMleafCurveIterationINIT::usage="Initialization function of cqmgQMleafCurveIteration; cqmgQMleafCurveIteration calculates a step in the quantum manifold based distribution.";


cqmgQMleafCurveIterationEXTR::usage="Extraction function of cqmgQMleafCurveIteration; cqmgQMleafCurveIteration calculates a step in the quantum manifold based distribution.";


cqmgGQMleafCurveIterationINIT::usage="Initialization function of cqmgGQMleafCurveIteration; cqmgGQMleafCurveIteration calculates a step in the quantum manifold and g based distribution.";


cqmgGQMleafCurveIterationEXTR::usage="Extraction function of cqmgGQMleafCurveIteration; cqmgGQMleafCurveIteration calculates a step in the quantum manifold and g based distribution.";


cqmgCurveIntegration::usage="Unified function; calculates a curve in the chosen distribution.";


cqmgTSleafCurveIterationNullINIT::usage="Initialization function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the target space based distribution in orthogonal (null) direction.";


cqmgTSleafCurveIterationNullEXTR::usage="Extraction function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the target space based distribution in orthogonal (null) direction.";


cqmgQMleafCurveIterationNullINIT::usage="Initialization function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the quantum manifold based distribution in orthogonal (null) direction.";


cqmgQMleafCurveIterationNullEXTR::usage="Extraction function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the quantum manifold based distribution in orthogonal (null) direction.";


cqmgGQMleafCurveIterationNullINIT::usage="Initialization function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the quantum manifold and g based distribution in orthogonal (null) direction.";


cqmgGQMleafCurveIterationNullEXTR::usage="Extraction function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the quantum manifold and g based distribution in orthogonal (null) direction.";


cqmgCurveIntegrationNull::usage="Unified function; calculates a curve in the chosen distribution in orthogonal (null) direction.";


cqmgTSleafCurveIterationAdaptiveNullINIT::usage="Initialization function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the target space based distribution in orthogonal (null) direction and with variable length.";


cqmgTSleafCurveIterationAdaptiveNullEXTR::usage="Extraction function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the target space based distribution in orthogonal (null) direction and with variable length.";


cqmgQMleafCurveIterationAdaptiveNullINIT::usage="Initialization function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the quantum manifold based distribution in orthogonal (null) direction and with variable length.";


cqmgQMleafCurveIterationAdaptiveNullEXTR::usage="Extraction function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the quantum manifold based distribution in orthogonal (null) direction and with variable length.";


cqmgGQMleafCurveIterationAdaptiveNullINIT::usage="Initialization function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the quantum manifold and g based distribution in orthogonal (null) direction and with variable length.";


cqmgGQMleafCurveIterationAdaptiveNullEXTR::usage="Extraction function of cqmgTSleafCurveIteration; cqmgTSleafCurveIteration calculates a step in the quantum manifold and g based distribution in orthogonal (null) direction and with variable length.";


cqmgMinimizeLambda::usage="Unified function; finds the minimum of lambda in the orthogonal direction to the chosen distribution.";


cqmgScan::usage="SeedRandom in advance recommended; Scans the chosen leaf via random curves.";


cqmgCoordinates::usage="Constructs local coordinates for the chosen leaf.";


cqmgPointToolsINIT::usage="For internal use; Initialzation function of cqmgPointInTileQ, cqmgPointsInTile, cqmgFindOptimalPointInTile and cqmgFilledTileQ; these perform important calculations in the context of tilings.";


cqmgIntegrandsINIT::usage="For internal use; Initialzation function of cqmgIntegrands; cqmgIntegrands calculates the integrands for integration over the chosen leaf.";


cqmgIntegrateTile::usage="Integrates over a given tile.";


cqmgIntegrateTiling::usage="Integrates over a tiling.";


cqmgQuantization::usage="Processes the output of cqmgIntegrateTiling in the context of quantization.";


cqmgIntegrateTilePreview::usage="Preview of cqmgIntegrateTile; only calculates local coordinates.";


cqmgIntegrateTilingPreview::usage="Preview of cqmgIntegrateTiling; only calculates a covering with local coordinates.";


cqmgCustomIntegrateTiling::usage="Integrates custom functions over a tiling via the output of cqmgIntegrateTiling.";


cqmgKaehlerCostINIT::usage="Initialization function of cqmgKaehlerCost; cqmgKaehlerCost calculates the K\[ADoubleDot]hler cost for a given linear subspace of the tangent space.";


cqmgKaehlerCostEXTR::usage="Extraction function of cqmgKaehlerCost; cqmgKaehlerCost calculates the K\[ADoubleDot]hler cost for a given linear subspace of the tangent space.";


cqmgKaehler::usage="Simplified function built on cqmgKaehlerCost; calculates the K\[ADoubleDot]hler cost for a given linear subspace of the tangent space.";


cqmgKaehlerForLeaf::usage="Calculates the K\[ADoubleDot]hler cost for the chosen leaf.";


cqmgKaehlerForRandom::usage="SeedRandom in advance recommended; calculates the K\[ADoubleDot]hler cost for random subspaces.";


cqmgComparePoissonStructures::usage="Calculates the Poisson structure induced by omega in the form of theta for a given point.";


cqmgQuantizationValidation::usage="SeedRandom in advance recommended; performs various checks that validate the quality of the semiclassical limit based on the output of cqmgIntegrateTiling.";


cqmgQuantizationValidationPresent::usage="SeedRandom in advance recommended; constructs a table with the most interesting output of cqmgQuantizationValidation.";


Begin["`Private`"];


(* ::Section:: *)
(*Preparation*)


MatrixRankWORKAROUND[M_,tolerance_]:=If[Norm[M,Infinity]<tolerance,0,MatrixRank[M,Tolerance->tolerance]](*WORKAROUND: ensures that the rank is zero if M is approximately zero*)


NullSpaceWORKAROUND[M_,tolerance_]:=If[Norm[M,Infinity]<tolerance,Table[UnitVector[Length[M],i],{i,1,Length[M]}],NullSpace[M,Tolerance->tolerance]](*WORKAROUND: ensures that the kernel is the full space if M is approximately zero*)


RealMatrixRank[M_,tolerance_]:=MatrixRankWORKAROUND[Join[Re[M],Im[M]],tolerance](*see [1] equation (114)*)


RealNullSpace[M_,tolerance_]:=NullSpaceWORKAROUND[Join[Re[M],Im[M]],tolerance]


CheckSubspace[v_,w_,tolerance_]:=If[v=={},True,If[w=={},False,MatrixRankWORKAROUND[w,tolerance]==MatrixRankWORKAROUND[Join[v,w],tolerance]]](*see [1] equation (115)]*)


(* ::Section:: *)
(*Basic Calculations*)


qmgNondegeneracyProbe[X_,x_]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
Id=IdentityMatrix[Nim];
H=N[1/2*Sum[(X[[a]]-x[[a]]*Id) . (X[[a]]-x[[a]]*Id),{a,Dim}]];(*see [1] equation (33)*)
EigVals=Eigenvalues[H];
degree=Length[Position[EigVals,Min[EigVals]]];(*dimension of lowest eigenspace*)
Nondegeneracy=(degree==1);(*lowest eigenspace nondegenereate <-> Degree==1*)
{Nondegeneracy,degree}
)


qmgLightweight[X_,x_]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
Id=IdentityMatrix[Nim];
H=N[1/2*Sum[(X[[a]]-x[[a]]*Id) . (X[[a]]-x[[a]]*Id),{a,Dim}]];(*see [1] equation (33)*)
Eig=If[Nim<3,Eigensystem[H,-1],Eigensystem[H,-1,Method->{"Arnoldi","Shift"->0}]];
EigFixed={Eig[[1,1]],Transpose[{Eig[[2,1]]/Norm[Eig[[2,1]]]*If[Eig[[2,1,-1]]==0,1,(Norm[Eig[[2,1,-1]]]/Eig[[2,1,-1]])]}]};(*calculates quasi-coherent state and its corresponding eigenvalue, see [1] section 3.1*)
EigFixed
)


qmgLightweightAsymptotic[X_,x_,Asymptotic_:True]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
Id=IdentityMatrix[Nim];
If[Asymptotic,(H=N[-Sum[x[[a]]*X[[a]],{a,Dim}]];H=H+Norm[H]*Id),H=N[1/2*Sum[(X[[a]]-x[[a]]*Id) . (X[[a]]-x[[a]]*Id),{a,Dim}]]];(*see [1] equation (87) while dropping the first term respectively (33)*)
Eig=Eigensystem[H,-1];
EigFixed={Eig[[1,1]],Transpose[{Eig[[2,1]]/Norm[Eig[[2,1]]]*If[Eig[[2,1,-1]]==0,1,(Norm[Eig[[2,1,-1]]]/Eig[[2,1,-1]])]}]};(*calculates quasi-coherent state and its corresponding eigenvalue, see [1] section 3.1*)
EigFixed
)


qmgBasic[X_,x_]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
Id=IdentityMatrix[Nim];
H=N[1/2*Sum[(X[[a]]-x[[a]]*Id) . (X[[a]]-x[[a]]*Id),{a,Dim}]];(*see [1] equation (33)*)
EigFull=Eigensystem[H];
EigFixed={EigFull[[1,-1]],Transpose[{EigFull[[2,-1]]/Norm[EigFull[[2,-1]]]*If[EigFull[[2,-1,-1]]==0,1,(Norm[EigFull[[2,-1,-1]]]/EigFull[[2,-1,-1]])]}]};(*calculates quasi-coherent state and its corresponding eigenvalue, see [1] section 3.1*)
HPseudoInverse=Sum[1/(EigFull[[1,a]]-EigFull[[1,-1]])*Transpose[{EigFull[[2,a]]}] . Conjugate[{EigFull[[2,a]]}],{a,Nim-1}];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
Xfrac=Table[HPseudoInverse . X[[a]],{a,Dim}];(*see [1] equation (49)*)
CovDer=Table[Xfrac[[a]] . EigFixed[[2]],{a,Dim}];(*see [1] equation (36) and (49)*)
h=Table[(ConjugateTranspose[CovDer[[a]]] . CovDer[[b]])[[1,1]],{a,Dim},{b,Dim}];(*see [1] equation (41)*)
g=Re[h+Transpose[h]];(*see [1] equation (41) and comment 21*)
omega=Re[I*(h-Transpose[h])];(*see [1] equation (41) and comment 21*)
theta=Table[Re[-I*(ConjugateTranspose[EigFixed[[2]]] . (X[[a]] . X[[b]]-X[[b]] . X[[a]]) . EigFixed[[2]])[[1,1]]],{a,Dim},{b,Dim}];(*see [1] equation (69)*)
xBold=Table[Re[(ConjugateTranspose[EigFixed[[2]]] . X[[a]] . EigFixed[[2]])[[1,1]]],{a,Dim}];(*see [1] equation (58)*)
xBoldPartial=Table[Re[(ConjugateTranspose[EigFixed[[2]]] . X[[a]] . Xfrac[[b]] . EigFixed[[2]]+ConjugateTranspose[EigFixed[[2]]] . X[[a]] . Xfrac[[b]] . EigFixed[[2]])[[1,1]]],{a,Dim},{b,Dim}];(*see [1] equation (59)*)
Displacement=Re[Sum[(xBold[[a]]-x[[a]])^2,{a,Dim}]];(*see [1] equation (60)*)
Dispersion=Re[Sum[(ConjugateTranspose[EigFixed[[2]]] . (X[[a]]-xBold[[a]]*Id) . (X[[a]]-xBold[[a]]*Id) . EigFixed[[2]])[[1,1]],{a,Dim}]];(*see [1] equation (61)*)
{EigFixed,{h,g,omega,theta},{xBold,xBoldPartial},{Displacement,Dispersion},{H,HPseudoInverse,Xfrac,CovDer}}
)


qmgBasicHighPrecision[Xp_,xp_,precision_]:=(
X=If[Precision[Xp]<precision,SetPrecision[Xp,precision],Xp];
x=If[Precision[xp]<precision,SetPrecision[xp,precision],xp];
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
Id=IdentityMatrix[Nim];
H=N[1/2*Sum[(X[[a]]-x[[a]]*Id) . (X[[a]]-x[[a]]*Id),{a,Dim}],precision];(*see [1] equation (33)*)
EigFull=Eigensystem[H];
EigFixed={EigFull[[1,-1]],Transpose[{EigFull[[2,-1]]/Norm[EigFull[[2,-1]]]*If[EigFull[[2,-1,-1]]==0,1,(Norm[EigFull[[2,-1,-1]]]/EigFull[[2,-1,-1]])]}]};(*calculates quasi-coherent state and its corresponding eigenvalue, see [1] section 3.1*)
HPseudoInverse=Sum[1/(EigFull[[1,a]]-EigFull[[1,-1]])*Transpose[{EigFull[[2,a]]}] . Conjugate[{EigFull[[2,a]]}],{a,Nim-1}];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
Xfrac=Table[HPseudoInverse . X[[a]],{a,Dim}];(*see [1] equation (49)*)
CovDer=Table[Xfrac[[a]] . EigFixed[[2]],{a,Dim}];(*see [1] equation (36) and (49)*)
h=Table[(ConjugateTranspose[CovDer[[a]]] . CovDer[[b]])[[1,1]],{a,Dim},{b,Dim}];(*see [1] equation (41)*)
g=Re[h+Transpose[h]];(*see [1] equation (41) and comment 21*)
omega=Re[I*(h-Transpose[h])];(*see [1] equation (41) and comment 21*)
theta=Table[Re[-I*(ConjugateTranspose[EigFixed[[2]]] . (X[[a]] . X[[b]]-X[[b]] . X[[a]]) . EigFixed[[2]])[[1,1]]],{a,Dim},{b,Dim}];(*see [1] equation (69)*)
xBold=Table[Re[(ConjugateTranspose[EigFixed[[2]]] . X[[a]] . EigFixed[[2]])[[1,1]]],{a,Dim}];(*see [1] equation (58)*)
xBoldPartial=Table[Re[(ConjugateTranspose[EigFixed[[2]]] . X[[a]] . Xfrac[[b]] . EigFixed[[2]]+ConjugateTranspose[EigFixed[[2]]] . X[[a]] . Xfrac[[b]] . EigFixed[[2]])[[1,1]]],{a,Dim},{b,Dim}];(*see [1] equation (59)*)
Displacement=Re[Sum[(xBold[[a]]-x[[a]])^2,{a,Dim}]];(*see [1] equation (60)*)
Dispersion=Re[Sum[(ConjugateTranspose[EigFixed[[2]]] . (X[[a]]-xBold[[a]]*Id) . (X[[a]]-xBold[[a]]*Id) . EigFixed[[2]])[[1,1]],{a,Dim}]];(*see [1] equation (61)*)
{EigFixed,{h,g,omega,theta},{xBold,xBoldPartial},{Displacement,Dispersion},{H,HPseudoInverse,Xfrac,CovDer}}
)


qmgEmbedding[X_,x_,Asymptotic_:False]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
EigFixed=qmgLightweightAsymptotic[X,x,Asymptotic];
xBold=Table[Re[(ConjugateTranspose[EigFixed[[2]]] . X[[a]] . EigFixed[[2]])[[1,1]]],{a,Dim}];(*see [1] equation (58)*)
{EigFixed,xBold}
)


qmgGeom[X_,x_]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
EigFixed=qmgLightweightAsymptotic[X,x,Asymptotic];
xBold=Table[Re[(ConjugateTranspose[EigFixed[[2]]] . X[[a]] . EigFixed[[2]])[[1,1]]],{a,Dim}];(*see [1] equation (58)*)
Displacement=Re[Sum[(xBold[[a]]-x[[a]])^2,{a,Dim}]];(*see [1] equation (60)*)
Dispersion=Re[Sum[(ConjugateTranspose[EigFixed[[2]]] . (X[[a]]-xBold[[a]]*Id) . (X[[a]]-xBold[[a]]*Id) . EigFixed[[2]])[[1,1]],{a,Dim}]];(*see [1] equation (61)*)
{EigFixed,xBold,Displacement,Dispersion}
)


qmgProbe[X_,x_,tolerance_:10^-8]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
{EigFixed,{h,g,omega,theta},{xBold,xBoldPartial},{Displacement,Dispersion},{H,HPseudoInverse,Xfrac,CovDer}}=qmgBasic[X,x];
kernels={RealNullSpace[Transpose[CovDer,1<->3][[1]],tolerance],NullSpaceWORKAROUND[g,tolerance],NullSpaceWORKAROUND[omega,tolerance],NullSpaceWORKAROUND[theta,tolerance]};(*calculate kernels of TxQ (see [1] equation (56)), g, omega, theta*)
ranks={RealMatrixRank[Transpose[CovDer,1<->3][[1]],tolerance],MatrixRankWORKAROUND[g,tolerance],MatrixRankWORKAROUND[omega,tolerance],MatrixRankWORKAROUND[theta,tolerance]};(*calculate ranks of TxQ (see [1] equation (56)), g, omega, theta*)
kernelsComparison={CheckSubspace[kernels[[1]],kernels[[2]],tolerance],CheckSubspace[kernels[[1]],kernels[[3]],tolerance],CheckSubspace[kernels[[2]],kernels[[3]],tolerance],CheckSubspace[kernels[[3]],kernels[[2]],tolerance],CheckSubspace[kernels[[1]],kernels[[4]],tolerance],CheckSubspace[kernels[[2]],kernels[[4]],tolerance],CheckSubspace[kernels[[4]],kernels[[2]],tolerance],CheckSubspace[kernels[[3]],kernels[[4]],tolerance],CheckSubspace[kernels[[4]],kernels[[3]],tolerance]};(*calculate whitch kernels contain whitch*)
{EigFixed,{h,g,omega,theta},{xBold,xBoldPartial},{Displacement,Dispersion},{H,HPseudoInverse,Xfrac,CovDer},{kernels,ranks,kernelsComparison}}
)


qmgCurveLength[X_,DiscreteCurve_]:=(
EuklidLength=0;(*initialize*)
QuantumLength=0;(*initialize*)
For[i=1,i<Length[DiscreteCurve],i++,(
x=DiscreteCurve[[i]];(*pick basepoint*)
v=DiscreteCurve[[i+1]]-DiscreteCurve[[i]];(*calculate approximate tangent vector of curve*)
g=qmgBasic[X,x][[2,2]];(*calculate g at basepoint*)
EuklidLength=EuklidLength+Norm[v];(*calculate Euklidian length of step*)
QuantumLength=QuantumLength+Sqrt[Abs[v . g . v]];(*calculate length of step with respect to g*)
)];
{EuklidLength,QuantumLength}
)


(* ::Section:: *)
(*Presentation*)


qmgPresent[Xxs_,tolerance_:10^-8]:=(
heading={"Name","x","D","N","dim(M)","rank(g)","rank(\[Omega])","rank(\[Theta])","TxNx in ker(g)","TxNx in ker(\[Omega])","ker(g) in ker(\[Omega])","ker(\[Omega]) in ker(g)","TxNx in ker(\[Theta])","ker(g) in ker(\[Theta])","ker(\[Theta]) in ker(g)","ker(\[Omega]) in ker(\[Theta])","ker(\[Theta]) in ker(\[Omega])","Re(spec(g))","Im(spec(\[Omega]))"};(*prepare heading for table*)
data={heading};(*initialize*)
For[i=1,i<=Length[Xxs],i++,((*loop over all sets*)
X=Xxs[[i,1]];(*extract X*)
x=Xxs[[i,2]];(*extract x*)
Name=Xxs[[i,3]];(*extract name*)
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
{EigFixed,{h,g,omega,theta},{xBold,xBoldPartial},{Displacement,Dispersion},{H,HPseudoInverse,Xfrac,CovDer},{kernels,ranks,kernelsComparison}}=qmgProbe[X,x,tolerance];(*calculate data*)
AppendTo[data,Join[{Name,MatrixForm[{N[Round[x,0.01]]}],Dim,Nim},ranks,kernelsComparison,{MatrixForm[{Re[N[Round[Eigenvalues[g],tolerance]]]}]},{MatrixForm[{Im[N[Round[Eigenvalues[omega],tolerance]]]}]}]];(*add data to table*)
)];
table=Grid[data,Alignment->Left,Spacings->{2,1},Frame->All,ItemStyle->"Text",Background->{{LightOrange,White,LightGray,LightGray,White,White,White,White,LightGray,LightGray,LightGray,LightGray,LightGray,LightGray,LightGray,LightGray,LightGray,White,White},{Orange,None}}];(*generate table*)
table
)


qmgPlotTS[X_,xsets_,projectionmode_:"default",projectionData_:{}]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
If[projectionmode=="default",((*distinct projection modes*)
(Orth=ConstantArray[0,{3,Dim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,3]]=1);(*sets default O, see [1] section 3.2*)
P=DiagonalMatrix[Join[{1,1,1},ConstantArray[0,Dim-3]]](*sets default P, see [1] section 3.2*)
),(
Orth=projectionData[[1]];(*sets provided O, see [1] section 3.2*)
P=projectionData[[2]](*sets provided P, see [1] section 3.2*)
)];
Pcal=Orth . P;(*sets Pcal, see [1] section 3.2*)
If[projectionmode=="slice",((*distinct projection modes*)
antiP=IdentityMatrix[Dim]-P;
v=projectionData[[3]];(*sets v, see [1] section 3.2*)
epsilon=projectionData[[4]](*sets epsilon, see [1] section 3.2*)
)];
pointset={};(*initialize list of point lists*)
Colors={};(*initialize list of coloring schemes*)
For[i=1,i<=Length[xsets],i++,((*loop over all sets*)
xs=xsets[[i,1]];(*define curent xs*)
Color=xsets[[i,2]];(*define curent Color*)
points={};(*initialize curent point list*)
For[j=1,j<=Length[xs],j++,((*loop over all sets*)
x=xs[[j]];(*define current x*)
pointRaw=x;(*use x itself as point*)
If[projectionmode!="slice",((*distinct projection modes*)
point=Pcal . pointRaw;(*apply projection scheme, see [1] section 3.2*)
points=AppendTo[points,point](*append Point to list*)
),(
If[antiP . (pointRaw-v)>epsilon,Continue[]];(*check if x in slice, see [1] section 3.2*)
point=Pcal . pointRaw;(*apply projection scheme, see [1] section 3.2*)
points=AppendTo[points,point](*append Point to list*)
)];
)];
pointset=AppendTo[pointset,points];(*append Points to list*)
Colors=AppendTo[Colors,Color];(*append Color to list*)
)];
plot=ListPointPlot3D[pointset,PlotStyle->Colors,PlotRange->All];
{plot,pointset,Colors}
)


qmgPlotQM[X_,xsets_,Asymptotic_:False,projectionmode_:"default",projectionData_:{}]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
If[projectionmode=="default",((*distinct projection modes*)
(Orth=ConstantArray[0,{3,2*Nim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,Nim+1]]=1);(*sets default O, see [1] section 3.2*)
P=DiagonalMatrix[Join[{1,1},ConstantArray[0,Nim-2],{1},ConstantArray[0,Nim-1]]](*sets default P, see [1] section 3.2*)
),(
Orth=projectionData[[1]];(*sets provided O, see [1] section 3.2*)
P=projectionData[[2]](*sets provided P, see [1] section 3.2*)
)];
Pcal=Orth . P;(*sets Pcal, see [1] section 3.2*)
If[projectionmode=="slice",((*distinct projection modes*)
antiP=IdentityMatrix[2*Nim]-P;
v=projectionData[[3]];(*sets v, see [1] section 3.2*)
epsilon=projectionData[[4]](*sets epsilon, see [1] section 3.2*)
)];
pointset={};(*initialize list of point lists*)
Colors={};(*initialize list of coloring schemes*)
For[i=1,i<=Length[xsets],i++,((*loop over all sets*)
xs=xsets[[i,1]];(*define curent xs*)
Color=xsets[[i,2]];(*define curent Color*)
points={};(*initialize curent point list*)
For[j=1,j<=Length[xs],j++,((*loop over all sets*)
x=xs[[j]];(*define current x*)
pointRaw=Transpose[qmgLightweightAsymptotic[X,x,Asymptotic][[2]]][[1]];(*calculate the quasi-coherent state at x*)
If[projectionmode!="slice",((*distinct projection modes*)
point=Pcal . Join[Re[pointRaw],Im[pointRaw]];(*apply projection scheme, see [1] section 3.2*)
points=AppendTo[points,point](*append Point to list*)
),(
pointRaw=Join[Re[pointRaw],Im[pointRaw]];
If[(Norm[antiP . (pointRaw-v)]>epsilon),Continue[]];(*check if x in slice, see [1] section 3.2*)
point=Pcal . pointRaw;(*apply projection scheme, see [1] section 3.2*)
points=AppendTo[points,point](*append Point to list*)
)];
)];
pointset=AppendTo[pointset,points];(*append Points to list*)
Colors=AppendTo[Colors,Color];(*append Color to list*)
)];
plot=ListPointPlot3D[pointset,PlotStyle->Colors,PlotRange->All];
{plot,pointset,Colors}
)


qmgPlotEQS[X_,xsets_,Asymptotic_:False,projectionmode_:"default",projectionData_:{}]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
If[projectionmode=="default",((*distinct projection modes*)
(Orth=ConstantArray[0,{3,Dim}];Orth[[1,1]]=1;Orth[[2,2]]=1;Orth[[3,3]]=1);(*sets default O, see [1] section 3.2*)
P=DiagonalMatrix[Join[{1,1,1},ConstantArray[0,Dim-3]]](*sets default P, see [1] section 3.2*)
),(
Orth=projectionData[[1]];(*sets provided O, see [1] section 3.2*)
P=projectionData[[2]](*sets provided P, see [1] section 3.2*)
)];
Pcal=Orth . P;(*sets Pcal, see [1] section 3.2*)
If[projectionmode=="slice",((*distinct projection modes*)
antiP=IdentityMatrix[Dim]-P;
v=projectionData[[3]];(*sets v, see [1] section 3.2*)
epsilon=projectionData[[4]](*sets epsilon, see [1] section 3.2*)
)];
pointset={};(*initialize list of point lists*)
Colors={};(*initialize list of coloring schemes*)
For[i=1,i<=Length[xsets],i++,((*loop over all sets*)
xs=xsets[[i,1]];(*define curent xs*)
Color=xsets[[i,2]];(*define curent Color*)
points={};(*initialize curent point list*)
For[j=1,j<=Length[xs],j++,((*loop over all sets*)
x=xs[[j]];(*define current x*)
pointRaw=qmgEmbedding[X,x,Asymptotic][[2]];(*calculate xBold at x*)
If[projectionmode!="slice",((*distinct projection modes*)
point=Pcal . pointRaw;(*apply projection scheme, see [1] section 3.2*)
points=AppendTo[points,point](*append Point to list*)
),(
If[Norm[antiP . (pointRaw-v)]>epsilon,Continue[]];(*check if x in slice, see [1] section 3.2*)
point=Pcal . pointRaw;(*apply projection scheme, see [1] section 3.2*)
points=AppendTo[points,point](*append Point to list*)
)];
)];
pointset=AppendTo[pointset,points];(*append Points to list*)
Colors=AppendTo[Colors,Color];(*append Color to list*)
)];
plot=ListPointPlot3D[pointset,PlotStyle->Colors,PlotRange->All];
{plot,pointset,Colors}
)


(* ::Section:: *)
(*Compiled Basic Calculations*)


(* ::Text:: *)
(*Note the general structure:*)
(*-cqmgFunc is the compiled analogue of qmgFunc*)
(*-cqmgFunc can be compiled via the function cqmgFuncINIT (having cqmgFunc as output)*)
(*-cqmgFuncSTAT collects the arguments with which cqmgFunc has been compiled*)
(*-cqmgFuncEXTR takes the output of cqmgFunc and generates a more nicely structured output comparable to the output of qmgFunc*)


cqmgLightweightINIT[X_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx1[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input,-1],{Nim+1}]];(*prepare function that calculates eigensystem of H*)
cqmgLightweight=Compile[{{x,_Real,1}},(*compile cqmgLightweight, comparing to qmgLightweight but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,H,Eig,EigLambda,EigState,EigStateFixed},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
Eig=fx1[H];(*calculation of Eig using predefined function*)
EigLambda=Re[Eig[[1]]];(*extract lambda*)
EigState=Eig[[2;;mNim+1]];(*extract quasi-coherent state*)
EigStateFixed=EigState/Norm[EigState]*If[EigState[[-1]]==0,1,(Norm[EigState[[-1]]]/EigState[[-1]])];(*calculates quasi-coherent state and its corresponding eigenvalue, see [1] section 3.1*)
Join[{EigLambda},Re[EigStateFixed],Im[EigStateFixed]](*join everething for output*)
],
{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{Eig,_Complex,1},{EigState,_Complex},{EigStateFixed,_Complex,1},{fx1[_],_Complex,1}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgLightweightSTAT={X};(*this variable indicates with which input cqmgLightweight has been compiled*)
cqmgLightweight
)
(**)
cqmgLightweightEXTR[out_]:=(
Dim=Length[cqmgLightweightSTAT[[1]]];(*=D*)
Nim=Length[cqmgLightweightSTAT[[1,1]]];(*=N*)
EigFixed={out[[1]],out[[2;;Nim+1]]+I*out[[Nim+2;;2*Nim+1]]};(*extract the results of qmgLightweight from the output of cqmgLightweight*)
EigFixed
)


cqmgBasicINIT[X_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
varB=Table[Xnum[[a]] . Xnum[[b]]-Xnum[[b]] . Xnum[[a]],{a,Dim},{b,Dim}];(*prepare commutators matrix configuration for theta*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
cqmgBasic=Compile[{{x,_Real,1}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,mvarB=varB,a,b,H,EigFull,EigLambda,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,theta,xBold,xBoldPartial},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigLambda=EigFull[[1,mNim]];(*extract lambda*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
theta=Im[Table[(EigStateCon . mvarB[[a,b]] . EigState)[[1,1]],{a,mDim},{b,mDim}]];(*calculates theta as in qmgBasic, but in a compressed form*)
xBold=Re[Table[(EigStateCon . mXnum[[a]] . EigState)[[1,1]],{a,mDim}]];(*calculates xBold as in qmgBasic, but in a compressed form*)
xBoldPartial=Re[Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverse . mXnum[[b]] . EigState+EigStateCon . mXnum[[b]] . HPseudoInverse . mXnum[[a]] . EigState)[[1,1]],{b,mDim}],{a,mDim}]];(*calculates xBoldPartial as in qmgBasic, but in a compressed form*)
Join[{Re[EigLambda]},Re[EigStateCon[[1]]],-Im[EigStateCon[[1]]],Flatten[Re[h]],Flatten[Im[h]],Flatten[theta],xBold,Flatten[xBoldPartial]](*join everething for output*)
],
{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{mvarB,_Complex,4},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigLambda,_Real},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{theta,_Real,2},{xBold,_Real,1},{xBoldPartial,_Real,2},{fx2[_],_Complex,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgBasicSTAT={X};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgBasic
)
(**)
cqmgBasicEXTR[out_]:=(
Dim=Length[cqmgBasicSTAT[[1]]];(*=D*)
Nim=Length[cqmgBasicSTAT[[1,1]]];(*=N*)
EigUnfixed={out[[1]],out[[2;;Nim+1]]+I*out[[Nim+2;;2*Nim+1]]};
h=ArrayReshape[out[[2*Nim+2;;2*Nim+Dim^2+1]],{Dim,Dim}]+I*ArrayReshape[out[[2*Nim+Dim^2+2;;2*Nim+2*Dim^2+1]],{Dim,Dim}];
g=Re[h+Transpose[h]];(*see [1] equation (41) and comment 21*)
omega=Re[I*(h-Transpose[h])];(*see [1] equation (41) and comment 21*)
theta=ArrayReshape[out[[2*Nim+2*Dim^2+2;;2*Nim+3*Dim^2+1]],{Dim,Dim}];
xBold=out[[2*Nim+3*Dim^2+2;;2*Nim+3*Dim^2+Dim+1]];
xBoldPartial=ArrayReshape[out[[2*Nim+3*Dim^2+Dim+2;;2*Nim+4*Dim^2+Dim+1]],{Dim,Dim}];
{EigUnfixed,{h,g,omega,theta},{xBold,xBoldPartial}}
)


cqmgEmbeddingINIT[X_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx1[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input,-1],{Nim+1}]];(*prepare function that calculates eigensystem of H*)
cqmgEmbedding=Compile[{{x,_Real,1}},(*compile cqmgEmbedding, comparing to qmgEmbedding but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,H,Eig,EigLambda,EigState,EigStateFixed,EigStateVec,EigStateVecCon,xBold},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
Eig=fx1[H];(*calculation of Eig using predefined function*)
EigLambda=Re[Eig[[1]]];(*extract lambda*)
EigState=Eig[[2;;mNim+1]];(*extract quasi-coherent state*)
EigStateFixed=EigState/Norm[EigState]*If[EigState[[-1]]==0,1,(Norm[EigState[[-1]]]/EigState[[-1]])];(*calculates quasi-coherent state and its corresponding eigenvalue, see [1] section 3.1*)
EigStateVec=Transpose[{EigState}];(*extract quasi-coherent state*)
EigStateVecCon=ConjugateTranspose[EigStateVec];(*prepare complex conjugate of EigState*)
xBold=Re[Table[(EigStateVecCon . mXnum[[a]] . EigStateVec)[[1,1]],{a,mDim}]];(*calculates xBold as in qmgBasic, but in a compressed form*)
Join[{EigLambda},Re[EigStateFixed],Im[EigStateFixed],Flatten[xBold]](*join everething for output*)
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{H,_Complex,2},{Eig,_Complex,1},{EigLambda,_Real},{EigState,_Complex,1},{EigStateFixed,_Complex,1},{EigStateVec,_Complex,2},{EigStateVecCon,_Complex,2},{xBold,_Real,1},{fx1[_],_Complex,1}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgEmbeddingSTAT={X};(*this variable indicates with which input cqmgLightweight has been compiled*)
cqmgEmbedding
)
(**)
cqmgEmbeddingEXTR[out_]:=(
Dim=Length[cqmgEmbeddingSTAT[[1]]];(*=D*)
Nim=Length[cqmgEmbeddingSTAT[[1,1]]];(*=N*)
EigFixed={out[[1]],out[[2;;Nim+1]]+I*out[[Nim+2;;2*Nim+1]]};
xBold=out[[2*Nim+2;;2*Nim+Dim+1]];
{EigFixed,xBold}
)


(* ::Section:: *)
(*Compiled Presentation*)


cqmgProjectINIT[PcalTS_,PcalQM_]:=(
PcalTSNum=N[PcalTS];
PcalQMNum=N[PcalQM];
cqmgProject=Compile[{{x,_Real,1},{EigStateVec,_Complex,1},{xBold,_Real,1}},
Module[{mPcalTS=PcalTSNum,mPcalQM=PcalQMNum,PointTS,PointQM,PointEQS},(*create module for improved compilation*)
PointTS=mPcalTS . x;(*apply projection scheme, see [1] section 3.2*)
PointQM=mPcalQM . Join[Re[EigStateVec],Im[EigStateVec]];(*apply projection scheme, see [1] section 3.2*)
PointEQS=mPcalTS . xBold;(*apply projection scheme, see [1] section 3.2*)
{PointTS,PointQM,PointEQS}
],{{mPcalTS,_Real,2},{mPcalQM,_Real,2},{PointTS,_Real,1},{PointQM,_Real,1},{PointEQS,_Real,1}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgProjectSTAT={PcalTS,PcalQM};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgProject
)


cqmgPlot[X_,xsets_,projectionmode_:"default",projectionData_:{}]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
If[projectionmode=="default",((*distinct projection modes*)
(PcalTS=ConstantArray[0,{3,Dim}];PcalTS[[1,1]]=1;PcalTS[[2,2]]=1;PcalTS[[3,3]]=1);(*sets default mPcalTS, see [1] section 3.2*)
(PcalQM=ConstantArray[0,{3,2*Nim}];PcalQM[[1,1]]=1;PcalQM[[2,2]]=1;PcalQM[[3,Nim+1]]=1);(*sets default mPcalQM, see [1] section 3.2*)
),(
PcalTS=projectionData[[1]];(*sets provided mPcalTS, see [1] section 3.2*)
PcalQM=projectionData[[2]](*sets provided mPcalQM, see [1] section 3.2*)
)];
If[!ValueQ[cqmgProjectSTAT],cqmgProjectINIT[PcalTS,PcalQM],If[cqmgProjectSTAT!={PcalTS,PcalQM},cqmgProjectINIT[PcalTS,PcalQM],cqmgProject]];(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[!ValueQ[cqmgEmbeddingSTAT],cqmgEmbeddingINIT[X],If[cqmgEmbeddingSTAT!={X},cqmgEmbeddingINIT[X],cqmgEmbedding]];(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
xsetsReduced=Select[xsets, #[[1]]!={} &];(*remove empty parts*)
{xs,Colors}=Transpose[xsetsReduced,1<->2];(*extract points and coloring*)
out=cqmgEmbedding[xs];(*calculate the quasi-coherent states and the xBolds; here we use parallelization*)
xsEigStateVec=out[[;;,;;,2;;Nim+1]]+I*out[[;;,;;,Nim+2;;2*Nim+1]];(*extract xsEigStateVec*)
xsxBold=out[[;;,;;,2*Nim+2;;2*Nim+Dim+1]];(*extract xsxBold*)
out=cqmgProject[xs,xsEigStateVec,xsxBold];(*project according to the scheme*)
PointsTS=out[[;;,;;,1]];(*extract PointsTS*)
PointsQM=out[[;;,;;,2]];(*extract PointsQM*)
PointsEQS=out[[;;,;;,3]];(*extract PointsEQS*)
{{ListPointPlot3D[PointsTS,PlotStyle->Colors,PlotRange->All],ListPointPlot3D[PointsQM,PlotStyle->Colors,PlotRange->All],ListPointPlot3D[PointsEQS,PlotStyle->Colors,PlotRange->All]},out}
)


(* ::Section:: *)
(*Compiled Basic Calculations Specialized to Leaves and for K\[ADoubleDot]hler Cost*)


(* ::Text:: *)
(*TSleaf stands for the target space based leaf using theta, see [1] section 2.4.3 and section 3.3.1 hybrid leaf*)
(*QMleaf stands for the quantum manifold based leaf using  omega, see [1] section 2.4.3 and section 3.3.1 hybrid leaf*)
(*GQMleaf stands for the quantum manifold based leaf using omega and g, see [1] section 2.4.3 and section 3.3.1 hybrid leaf*)
(*Note that cqmgGQMleafBasicINIT=cqmgQMleafBasicINIT as at this stage there is no difference between the two*)


cqmgTSleafBasicINIT[X_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
varB=Table[Xnum[[a]] . Xnum[[b]]-Xnum[[b]] . Xnum[[a]],{a,Dim},{b,Dim}];(*prepare commutators matrix configuration for theta*)
fx3[input_]:=Developer`ToPackedArray[Eigenvectors[input,-1]];(*prepare function that calculates eigensystem of H*)
cqmgTSleafBasic=Compile[{{x,_Real,1}},
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,mvarB=varB,a,b,H,EigState,EigStateCon,theta,xBold},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigState=Transpose[fx3[H]];(*calculation of EigState using predefined function*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
theta=Im[Table[(EigStateCon . mvarB[[a,b]] . EigState)[[1,1]],{a,mDim},{b,mDim}]];(*calculates theta as in qmgBasic, but in a compressed form*)
xBold=Re[Table[(EigStateCon . mXnum[[a]] . EigState)[[1,1]],{a,mDim}]];(*calculates xBold as in qmgBasic, but in a compressed form*)
Join[Re[EigStateCon[[1]]],-Im[EigStateCon[[1]]],Flatten[theta],xBold](*join everething for output*)
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{mvarB,_Complex,4},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{theta,_Real,2},{xBold,_Real,1},{fx3[_],_Complex,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgTSleafBasicSTAT={X};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgTSleafBasic
)
(**)
cqmgTSleafBasicEXTR[out_]:=(
Dim=Length[cqmgTSleafBasicSTAT[[1]]];(*=D*)
Nim=Length[cqmgTSleafBasicSTAT[[1,1]]];(*=N*)
EigState=out[[1;;Nim]]+I*out[[Nim+1;;2*Nim]];
theta=ArrayReshape[out[[2*Nim+1;;2*Nim+Dim^2]],{Dim,Dim}];
xBold=out[[2*Nim+Dim^2+1;;2*Nim+Dim^2+Dim]];
{EigState,theta,xBold}
)


cqmgQMleafBasicINIT[X_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
cqmgQMleafBasic=Compile[{{x,_Real,1}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,b,H,EigFull,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,xBold},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
xBold=Re[Table[(EigStateCon . mXnum[[a]] . EigState)[[1,1]],{a,mDim}]];(*calculates xBold as in qmgBasic, but in a compressed form*)
Join[Re[EigStateCon[[1]]],-Im[EigStateCon[[1]]],Flatten[Re[I*(h-Transpose[h])]],xBold](*join everething for output*)
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{xBold,_Real,1},{fx2[_],_Complex,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgQMleafBasicSTAT={X};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgQMleafBasic
)
(**)
cqmgQMleafBasicEXTR[out_]:=(
Dim=Length[cqmgQMleafBasicSTAT[[1]]];(*=D*)
Nim=Length[cqmgQMleafBasicSTAT[[1,1]]];(*=N*)
EigStat=out[[1;;Nim]]+I*out[[Nim+1;;2*Nim]];
omega=ArrayReshape[out[[2*Nim+1;;2*Nim+Dim^2]],{Dim,Dim}];
xBold=out[[2*Nim+Dim^2+1;;2*Nim+Dim^2+Dim]];
{EigStat,omega,xBold}
)


cqmgGQMleafBasicINIT[X_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
cqmgGQMleafBasic=Compile[{{x,_Real,1}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,b,H,EigFull,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,xBold},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
xBold=Re[Table[(EigStateCon . mXnum[[a]] . EigState)[[1,1]],{a,mDim}]];(*calculates xBold as in qmgBasic, but in a compressed form*)
Join[Re[EigStateCon[[1]]],-Im[EigStateCon[[1]]],Flatten[Re[h+Transpose[h]]],xBold](*join everething for output*)
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{xBold,_Real,1},{fx2[_],_Complex,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgGQMleafBasicSTAT={X};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgGQMleafBasic
)
(**)
cqmgGQMleafBasicEXTR[out_]:=(
Dim=Length[cqmgGQMleafBasicSTAT[[1]]];(*=D*)
Nim=Length[cqmgGQMleafBasicSTAT[[1,1]]];(*=N*)
EigStat=out[[1;;Nim]]+I*out[[Nim+1;;2*Nim]];
g=ArrayReshape[out[[2*Nim+1;;2*Nim+Dim^2]],{Dim,Dim}];
xBold=out[[2*Nim+Dim^2+1;;2*Nim+Dim^2+Dim]];
{EigStat,g,xBold}
)


cqmgKaehlerBasicINIT[X_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
cqmgKaehlerBasic=Compile[{{x,_Real,1}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,mvarB=varB,a,b,H,EigFull,EigLambda,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,CovDer},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigLambda=EigFull[[1,mNim]];(*extract lambda*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
CovDer=Table[HPseudoInverse . mXnum[[a]] . EigState,{a,mDim}];(*calculates CovDer as in qmgBasic, but in a compressed form*)
Join[Re[EigStateCon[[1]]],-Im[EigStateCon[[1]]],Flatten[Re[CovDer]],Flatten[Im[CovDer]]](*join everething for output*)
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{mvarB,_Complex,4},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigLambda,_Real},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{CovDer,_Complex,2},{fx2[_],_Complex,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgKaehlerBasicSTAT={X};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgKaehlerBasic
)
(**)
cqmgKaehlerBasicEXTR[out_]:=(
Dim=Length[cqmgKaehlerBasicSTAT[[1]]];(*=D*)
Nim=Length[cqmgKaehlerBasicSTAT[[1,1]]];(*=N*)
EigState=out[[1;;Nim]]+I*out[[Nim+1;;2*Nim]];
CovDer=ArrayReshape[out[[2*Nim+1;;2*Nim+Dim*Nim]],{Dim,Nim,1}]+I*ArrayReshape[out[[2*Nim+Dim*Nim+1;;2*Nim+2*Dim*Nim]],{Dim,Nim,1}];
{EigState,CovDer}
)


(* ::Section:: *)
(*Compiled Distributions*)


cqmgTSleafDistributionINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
varB=Table[Xnum[[a]] . Xnum[[b]]-Xnum[[b]] . Xnum[[a]],{a,Dim},{b,Dim}];(*prepare commutators matrix configuration for theta*)
fx3[input_]:=Developer`ToPackedArray[Eigenvectors[input,-1]];(*prepare function that calculates eigensystem of H*)
fx4[input_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input,l][[1;;l;;2]],1]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized*)
cqmgTSleafDistribution=Compile[{{x,_Real,1}},
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,mvarB=varB,a,b,H,EigState,EigStateCon,theta,dist},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigState=Transpose[fx3[H]];(*calculation of EigState using predefined function*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
theta=Im[Table[(EigStateCon . mvarB[[a,b]] . EigState)[[1,1]],{a,mDim},{b,mDim}]];(*calculates theta as in qmgBasic, but in a compressed form*)
dist=fx4[theta];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf*)
dist
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{mvarB,_Complex,4},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{theta,_Real,2},{dist,_Real,2},{fx3[_],_Complex,2},{fx4[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgTSleafDistributionSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgTSleafDistribution
)
(**)
cqmgTSleafDistributionEXTR[out_]:=(
Dim=Length[cqmgTSleafDistributionSTAT[[1]]];(*=D*)
Nim=Length[cqmgTSleafDistributionSTAT[[1,1]]];(*=N*)
l=cqmgTSleafDistributionSTAT[[2]];(*=l*)
dist=out;
dist
)


cqmgQMleafDistributionINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
fx5[input_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input,l][[1;;l;;2]],1]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized*)
cqmgQMleafDistribution=Compile[{{x,_Real,1}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,b,H,EigFull,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,dist},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
dist=fx5[Im[h]];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf using omega; note that Im[h] is proportional to omega*)
dist
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{dist,_Real,2},{fx2[_],_Complex,2},{fx5[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgQMleafDistributionSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgQMleafDistribution
)
(**)
cqmgQMleafDistributionEXTR[out_]:=(
Dim=Length[cqmgQMleafDistributionSTAT[[1]]];(*=D*)
Nim=Length[cqmgQMleafDistributionSTAT[[1,1]]];(*=N*)
l=cqmgQMleafDistributionSTAT[[2]];(*=l*)
dist=out;
dist
)


cqmgGQMleafDistributionINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
fx6[input1_,input2_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input1,l][[1;;l;;2]],1],Function[{u, v}, u . input2 . v]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized w.r.t. g*)
cqmgGQMleafDistribution=Compile[{{x,_Real,1}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,b,H,EigFull,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,dist},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
dist=fx6[Im[h],2*Re[h]];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf using omega and g; note that Im[h] is proportional to omega*)
dist
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{dist,_Real,2},{fx2[_],_Complex,2},{fx6[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgGQMleafDistributionSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgGQMleafDistribution
)
(**)
cqmgGQMleafDistributionEXTR[out_]:=(
Dim=Length[cqmgGQMleafDistributionSTAT[[1]]];(*=D*)
Nim=Length[cqmgGQMleafDistributionSTAT[[1,1]]];(*=N*)
l=cqmgQMleafDistributionSTAT[[2]];(*=l*)
dist=out;
dist
)


cqmgDistribution[X_,x_,l_,leaf_:"TSleaf"]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
cqmgDistributionInternal=If[leaf=="TSleaf",(*pick the appropriate function depending on the chosen leaf*)
If[!ValueQ[cqmgTSleafDistributionSTAT],cqmgTSleafDistributionINIT[X,l],If[cqmgTSleafDistributionSTAT!={X,l},cqmgTSleafDistributionINIT[X,l],cqmgTSleafDistribution]],(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[leaf=="QMleaf",
If[!ValueQ[cqmgQMleafDistributionSTAT],cqmgQMleafDistributionINIT[X,l],If[cqmgQMleafDistributionSTAT!={X,l},cqmgQMleafDistributionINIT[X,l],cqmgQMleafDistribution]],(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[!ValueQ[cqmgGQMleafDistributionSTAT],cqmgGQMleafDistributionINIT[X,l],If[cqmgGQMleafDistributionSTAT!={X,l},cqmgGQMleafDistributionINIT[X,l],cqmgGQMleafDistribution]](*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
]];
dist=cqmgDistributionInternal[x];(*calculate the disrtibution; here we (possibly) use parallelization*)
dist
)


(* ::Section:: *)
(*Compiled Curve Integration*)


cqmgTSleafCurveIterationINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
varB=Table[Xnum[[a]] . Xnum[[b]]-Xnum[[b]] . Xnum[[a]],{a,Dim},{b,Dim}];(*prepare commutators matrix configuration for theta*)
fx3[input_]:=Developer`ToPackedArray[Eigenvectors[input,-1]];(*prepare function that calculates eigensystem of H*)
fx4[input_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input,l][[1;;l;;2]],1]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized*)
cqmgTSleafCurveIteration=Compile[{{x,_Real,1},{v,_Real,1},{delta,_Real,0}},
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,mvarB=varB,a,b,H,EigState,EigStateCon,theta,dist,Px,vPrime},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigState=Transpose[fx3[H]];(*calculation of EigState using predefined function*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
theta=Im[Table[(EigStateCon . mvarB[[a,b]] . EigState)[[1,1]],{a,mDim},{b,mDim}]];(*calculates theta as in qmgBasic, but in a compressed form*)
dist=fx4[theta];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf*)
Px=Transpose[dist] . dist;(*calculate the projector Px on the leave, see [1] section 3.3.1 hybrid leaf*)
vPrime=Px . v;
vPrime=(delta/Norm[vPrime])*vPrime;(*calculates the step, see [1] equation (125)*)
{vPrime,x+vPrime}
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{mvarB,_Complex,4},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{theta,_Real,2},{dist,_Real,2},{Px,_Real,2},{vPrime,_Real,1},{fx3[_],_Complex,2},{fx4[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgTSleafCurveIterationSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgTSleafCurveIteration
)
(**)
cqmgTSleafCurveIterationEXTR[out_]:=(
Dim=Length[cqmgTSleafCurveIterationSTAT[[1]]];(*=D*)
Nim=Length[cqmgTSleafCurveIterationSTAT[[1,1]]];(*=N*)
l=cqmgTSleafCurveIterationSTAT[[2]];(*=l*)
vPrime=out[[1]];
xNew=out[[2]];
{vPrime,xNew}
)


cqmgQMleafCurveIterationINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
fx5[input_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input,l][[1;;l;;2]],1]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized*)
cqmgQMleafCurveIteration=Compile[{{x,_Real,1},{v,_Real,1},{delta,_Real,0}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,b,H,EigFull,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,dist,Px,vPrime},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
dist=fx5[Im[h]];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf using omega; note that Im[h] is proportional to omega*)
Px=Transpose[dist] . dist;(*calculate the projector Px on the leave, see [1] section 3.3.1 hybrid leaf using omega*)
vPrime=Px . v;
vPrime=(delta/Norm[vPrime])*vPrime;(*calculates the step, see [1] equation (125)*)
{vPrime,x+vPrime}
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{dist,_Real,2},{Px,_Real,2},{vPrime,_Real,1},{fx2[_],_Complex,2},{fx5[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgQMleafCurveIterationSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgQMleafCurveIteration
)
(**)
cqmgQMleafCurveIterationEXTR[out_]:=(
Dim=Length[cqmgQMleafCurveIterationSTAT[[1]]];(*=D*)
Nim=Length[cqmgQMleafCurveIterationSTAT[[1,1]]];(*=N*)
l=cqmgQMleafCurveIterationSTAT[[2]];(*=l*)
vPrime=out[[1]];
xNew=out[[2]];
{vPrime,xNew}
)


cqmgGQMleafCurveIterationINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
fx6[input1_,input2_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input1,l][[1;;l;;2]],1],Function[{u, v}, u . input2 . v]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized w.r.t. g*)
cqmgGQMleafCurveIteration=Compile[{{x,_Real,1},{v,_Real,1},{delta,_Real,0}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,b,H,EigFull,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,dist,Px,vPrime},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
dist=fx6[Im[h],2*Re[h]];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf using omega and g; note that Im[h] is proportional to omega*)
Px=Transpose[dist] . dist . (2* Re[h]);(*calculate the projector Px on the leave, see [1] section 3.3.1 hybrid leaf using omega and g*)
vPrime=Px . v;
vPrime=(delta/Norm[vPrime])*vPrime;(*calculates the step, see [1] equation (125)*)
{vPrime,x+vPrime}
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{dist,_Real,2},{Px,_Real,2},{vPrime,_Real,1},{fx2[_],_Complex,2},{fx6[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgGQMleafCurveIterationSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgGQMleafCurveIteration
)
(**)
cqmgGQMleafCurveIterationEXTR[out_]:=(
Dim=Length[cqmgGQMleafCurveIterationSTAT[[1]]];(*=D*)
Nim=Length[cqmgGQMleafCurveIterationSTAT[[1,1]]];(*=N*)
l=cqmgGQMleafCurveIterationSTAT[[2]];(*=l*)
vPrime=out[[1]];
xNew=out[[2]];
{vPrime,xNew}
)


cqmgCurveIntegration[X_,x_,v_,delta_,n_,m_,l_,leaf_:"TSleaf",vFix_:False]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
cqmgCurveIteration=If[leaf=="TSleaf",(*pick the appropriate function depending on the chosen leaf*)
If[!ValueQ[cqmgTSleafCurveIterationSTAT],cqmgTSleafCurveIterationINIT[X,l],If[cqmgTSleafCurveIterationSTAT!={X,l},cqmgTSleafCurveIterationINIT[X,l],cqmgTSleafCurveIteration]],(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[leaf=="QMleaf",
If[!ValueQ[cqmgQMleafCurveIterationSTAT],cqmgQMleafCurveIterationINIT[X,l],If[cqmgQMleafCurveIterationSTAT!={X,l},cqmgQMleafCurveIterationINIT[X,l],cqmgQMleafCurveIteration]],(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[!ValueQ[cqmgGQMleafCurveIterationSTAT],cqmgGQMleafCurveIterationINIT[X,l],If[cqmgGQMleafCurveIterationSTAT!={X,l},cqmgGQMleafCurveIterationINIT[X,l],cqmgGQMleafCurveIteration]](*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
]];
If[vFix,(*prepares a function that fixes v if vFix==True and gives back the input if vFix==False*)
fx7[input_]:=v,
fx7[input_]:=input
];
MimicParallel=(Max[TensorRank[x],TensorRank[v]]==2);(*check if x and/or v is actually a list of points respectively initial tangent vectors, allowing for rudimentary parallelization*)
If[Not[MimicParallel],((*distinguish cases for organizational reasons*)
xs={x};(*initialize curve*)
xNew=xs[[1]];(*sets x0, see [1] section 3.3.2*)
vNew=v;(*sets v0, see [1] section 3.3.2*)
If[m==0,(*distinguish cases for efficiency*)
For[i=1,i<=n,i++,({vNew,xNew}=cqmgCurveIteration[xNew,fx7[vNew],delta];xs={xs,{xNew}})],(*iterate the curve, see [1] equation (125); the scheme xs={xs,{xNew}} is adopted for higher numerical efficiency*)
For[i=1,i<=(m+1)*n,i++,({vNew,xNew}=cqmgCurveIteration[xNew,fx7[vNew],delta/(m+1.)];If[IntegerQ[i/(m+1)],xs={xs,{xNew}}])];(*iterate the curve but only keep main points, see [1] section 3.3.2 and especially equation (125)*)
];
xs=ArrayReshape[Flatten[xs],{n+1,Dim}];(*bring array in right shape*)
),(
If[TensorRank[x]==2,(ParallelLength=Length[x];xs={x}),(ParallelLength=Length[v];xs={Table[x,ParallelLength]})];(*initialize curve while ensuring appropriate shape of array*)
xNew=xs[[1]];(*initialize curve*)
vNew=v;(*sets v0, see [1] section 3.3.2*)
If[m==0,(*distinguish cases for efficiency*)
For[i=1,i<=n,i++,({vNew,xNew}=Transpose[cqmgCurveIteration[xNew,fx7[vNew],delta]];xs={xs,{xNew}})],(*iterate the curve while ensuring appropriate shape of array, see [1] equation (125); here we use parallelization*)
For[i=1,i<=(m+1)*n,i++,({vNew,xNew}=Transpose[cqmgCurveIteration[xNew,fx7[vNew],delta/(m+1.)]];If[IntegerQ[i/(m+1)],xs={xs,{xNew}}])];(*iterate the curve while ensuring appropriate shape of array but only keep main points, see [1] section 3.3.2 and especially equation (125); here we use parallelization*)
];
xs=Transpose[ArrayReshape[Flatten[xs],{n+1,ParallelLength,Dim}],1<->2](*bring array in right shape*)
)];
xs
)


(* ::Section:: *)
(*Compiled Null Space Curve Integration*)


cqmgTSleafCurveIterationNullINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
varB=Table[Xnum[[a]] . Xnum[[b]]-Xnum[[b]] . Xnum[[a]],{a,Dim},{b,Dim}];(*prepare commutators matrix configuration for theta*)
fx3[input_]:=Developer`ToPackedArray[Eigenvectors[input,-1]];(*prepare function that calculates eigensystem of H*)
fx4[input_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input,l][[1;;l;;2]],1]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized*)
cqmgTSleafCurveIterationNull=Compile[{{x,_Real,1},{v,_Real,1},{delta,_Real,0}},
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,mvarB=varB,a,b,H,EigState,EigStateCon,theta,dist,Px,vPrime},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigState=Transpose[fx3[H]];(*calculation of EigState using predefined function*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
theta=Im[Table[(EigStateCon . mvarB[[a,b]] . EigState)[[1,1]],{a,mDim},{b,mDim}]];(*calculates theta as in qmgBasic, but in a compressed form*)
dist=fx4[theta];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf*)
Px=Transpose[dist] . dist;(*calculate the projector Px on the leave, see [1] section 3.3.1 hybrid leaf*)
vPrime=v-Px . v;(*compare to [1] section 3.3.7]*)
vPrime=(delta/Norm[vPrime])*vPrime;(*calculates the step, see [1] equation (125)*)
{vPrime,x+vPrime}
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{mvarB,_Complex,4},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{theta,_Real,2},{dist,_Real,2},{Px,_Real,2},{vPrime,_Real,1},{fx3[_],_Complex,2},{fx4[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgTSleafCurveIterationNullSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgTSleafCurveIterationNull
)
(**)
cqmgTSleafCurveIterationNullEXTR[out_]:=(
Dim=Length[cqmgTSleafCurveIterationNullSTAT[[1]]];(*=D*)
Nim=Length[cqmgTSleafCurveIterationNullSTAT[[1,1]]];(*=N*)
l=cqmgTSleafCurveIterationNullSTAT[[2]];(*=l*)
vPrime=out[[1]];
xNew=out[[2]];
{vPrime,xNew}
)


cqmgQMleafCurveIterationNullINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
fx5[input_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input,l][[1;;l;;2]],1]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized*)
cqmgQMleafCurveIterationNull=Compile[{{x,_Real,1},{v,_Real,1},{delta,_Real,0}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,b,H,EigFull,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,dist,Px,vPrime},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
dist=fx5[Im[h]];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf using omega; note that Im[h] is proportional to omega*)
Px=Transpose[dist] . dist;(*calculate the projector Px on the leave, see [1] section 3.3.1 hybrid leaf using omega*)
vPrime=v-Px . v;(*compare to [1] section 3.3.7]*)
vPrime=(delta/Norm[vPrime])*vPrime;(*calculates the step, see [1] equation (125)*)
{vPrime,x+vPrime}
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{dist,_Real,2},{Px,_Real,2},{vPrime,_Real,1},{fx2[_],_Complex,2},{fx5[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgQMleafCurveIterationNullSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgQMleafCurveIterationNull
)
(**)
cqmgQMleafCurveIterationNullEXTR[out_]:=(
Dim=Length[cqmgQMleafCurveIterationNullSTAT[[1]]];(*=D*)
Nim=Length[cqmgQMleafCurveIterationNullSTAT[[1,1]]];(*=N*)
l=cqmgQMleafCurveIterationNullSTAT[[2]];(*=l*)
vPrime=out[[1]];
xNew=out[[2]];
{vPrime,xNew}
)


cqmgGQMleafCurveIterationNullINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
fx6[input1_,input2_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input1,l][[1;;l;;2]],1],Function[{u, v}, u . input2 . v]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized w.r.t. g*)
cqmgGQMleafCurveIterationNull=Compile[{{x,_Real,1},{v,_Real,1},{delta,_Real,0}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,b,H,EigFull,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,dist,Px,vPrime},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
dist=fx6[Im[h],2*Re[h]];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf using omega and g; note that Im[h] is proportional to omega*)
Px=Transpose[dist] . dist . (2* Re[h]);(*calculate the projector Px on the leave, see [1] section 3.3.1 hybrid leaf using omega*)
vPrime=v-Px . v;(*compare to [1] section 3.3.7]*)
vPrime=(delta/Norm[vPrime])*vPrime;(*calculates the step, see [1] equation (125)*)
{vPrime,x+vPrime}
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{dist,_Real,2},{Px,_Real,2},{vPrime,_Real,1},{fx2[_],_Complex,2},{fx6[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgGQMleafCurveIterationNullSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgGQMleafCurveIterationNull
)
(**)
cqmgGQMleafCurveIterationNullEXTR[out_]:=(
Dim=Length[cqmgGQMleafCurveIterationNullSTAT[[1]]];(*=D*)
Nim=Length[cqmgGQMleafCurveIterationNullSTAT[[1,1]]];(*=N*)
l=cqmgGQMleafCurveIterationNullSTAT[[2]];(*=l*)
vPrime=out[[1]];
xNew=out[[2]];
{vPrime,xNew}
)


cqmgCurveIntegrationNull[X_,x_,v_,delta_,n_,m_,l_,leaf_:"TSleaf",vFix_:False]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
cqmgCurveIterationNull=If[leaf=="TSleaf",(*pick the appropriate function depending on the chosen leaf*)
If[!ValueQ[cqmgTSleafCurveIterationNullSTAT],cqmgTSleafCurveIterationNullINIT[X,l],If[cqmgTSleafCurveIterationNullSTAT!={X,l},cqmgTSleafCurveIterationNullINIT[X,l],cqmgTSleafCurveIterationNull]],(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[leaf=="QMleaf",
If[!ValueQ[cqmgQMleafCurveIterationNullSTAT],cqmgQMleafCurveIterationNullINIT[X,l],If[cqmgQMleafCurveIterationNullSTAT!={X,l},cqmgQMleafCurveIterationNullINIT[X,l],cqmgQMleafCurveIterationNull]],(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[!ValueQ[cqmgGQMleafCurveIterationNullSTAT],cqmgGQMleafCurveIterationNullINIT[X,l],If[cqmgGQMleafCurveIterationNullSTAT!={X,l},cqmgGQMleafCurveIterationNullINIT[X,l],cqmgGQMleafCurveIterationNull]](*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
]];
If[vFix,(*prepares a function that fixes v if vFix==True and gives back the input if vFix==False*)
fx7[input_]:=v,
fx7[input_]:=input
];
MimicParallel=(Max[TensorRank[x],TensorRank[v]]==2);(*check if x and/or v is actually a list of points respectively initial tangent vectors, allowing for rudimentary parallelization*)
If[Not[MimicParallel],((*distinguish cases for organizational reasons*)
xs={x};(*initialize curve*)
xNew=xs[[1]];(*sets x0, see [1] section 3.3.2*)
vNew=v;(*sets v0, see [1] section 3.3.2*)
If[m==0,(*distinguish cases for efficiency*)
For[i=1,i<=n,i++,({vNew,xNew}=cqmgCurveIterationNull[xNew,fx7[vNew],delta];xs={xs,{xNew}})],(*iterate the curve, see [1] equation (125) and section 3.3.7; the scheme xs={xs,{xNew}} is adopted for higher numerical efficiency*)
For[i=1,i<=(m+1)*n,i++,({vNew,xNew}=cqmgCurveIterationNull[xNew,fx7[vNew],delta/(m+1.)];If[IntegerQ[i/(m+1)],xs={xs,{xNew}}])];(*iterate the curve but only keep main points, see [1] section 3.3.2 and especially equation (125) and section 3.3.7*)
];
xs=ArrayReshape[Flatten[xs],{n+1,Dim}];(*bring array in right shape*)
),(
If[TensorRank[x]==2,(ParallelLength=Length[x];xs={x}),(ParallelLength=Length[v];xs={Table[x,ParallelLength]})];(*initialize curve while ensuring appropriate shape of array*)
xNew=xs[[1]];(*initialize curve*)
vNew=v;(*sets v0, see [1] section 3.3.2*)
If[m==0,(*distinguish cases for efficiency*)
For[i=1,i<=n,i++,({vNew,xNew}=Transpose[cqmgCurveIterationNull[xNew,fx7[vNew],delta]];xs={xs,{xNew}})],(*iterate the curve while ensuring appropriate shape of array, see [1] equation (125) and section 3.3.7; here we use parallelization*)
For[i=1,i<=(m+1)*n,i++,({vNew,xNew}=Transpose[cqmgCurveIterationNull[xNew,fx7[vNew],delta/(m+1.)]];If[IntegerQ[i/(m+1)],xs={xs,{xNew}}])];(*iterate the curve while ensuring appropriate shape of array but only keep main points, see [1] section 3.3.2 and especially equation (125) and section 3.3.7; here we use parallelization*)
];
xs=Transpose[ArrayReshape[Flatten[xs],{n+1,ParallelLength,Dim}],1<->2](*bring array in right shape*)
)];
xs
)


(* ::Section:: *)
(*Compiled Minimization of Lambda*)


cqmgTSleafCurveIterationAdaptiveNullINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
varB=Table[Xnum[[a]] . Xnum[[b]]-Xnum[[b]] . Xnum[[a]],{a,Dim},{b,Dim}];(*prepare commutators matrix configuration for theta*)
fx3[input_]:=Developer`ToPackedArray[Eigenvectors[input,-1]];(*prepare function that calculates eigensystem of H*)
fx4[input_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input,l][[1;;l;;2]],1]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized*)
cqmgTSleafCurveIterationAdaptiveNull=Compile[{{x,_Real,1},{v,_Real,1},{delta,_Real,0}},
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,mvarB=varB,a,b,H,EigState,EigStateCon,theta,dist,Px,vPrime},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigState=Transpose[fx3[H]];(*calculation of EigState using predefined function*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
theta=Im[Table[(EigStateCon . mvarB[[a,b]] . EigState)[[1,1]],{a,mDim},{b,mDim}]];(*calculates theta as in qmgBasic, but in a compressed form*)
dist=fx4[theta];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf*)
Px=Transpose[dist] . dist;(*calculate the projector Px on the leave, see [1] section 3.3.1 hybrid leaf*)
vPrime=v-Px . v;(*compare to [1] section 3.3.7]*)
vPrime=delta*vPrime;(*calculates the step, see [1] equation (125); note that here we do not normalize vPrime*)
{vPrime,x+vPrime}
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{mvarB,_Complex,4},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{theta,_Real,2},{dist,_Real,2},{Px,_Real,2},{vPrime,_Real,1},{fx3[_],_Complex,2},{fx4[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgTSleafCurveIterationAdaptiveNullSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgTSleafCurveIterationAdaptiveNull
)
(**)
cqmgTSleafCurveIterationAdaptiveNullEXTR[out_]:=(
Dim=Length[cqmgTSleafCurveIterationAdaptiveNullSTAT[[1]]];(*=D*)
Nim=Length[cqmgTSleafCurveIterationAdaptiveNullSTAT[[1,1]]];(*=N*)
l=cqmgTSleafCurveIterationAdaptiveNullSTAT[[2]];(*=l*)
vPrime=out[[1]];
xNew=out[[2]];
{vPrime,xNew}
)


cqmgQMleafCurveIterationAdaptiveNullINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
fx5[input_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input,l][[1;;l;;2]],1]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized*)
cqmgQMleafCurveIterationAdaptiveNull=Compile[{{x,_Real,1},{v,_Real,1},{delta,_Real,0}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,b,H,EigFull,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,dist,Px,vPrime},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
dist=fx5[Im[h]];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf using omega; note that Im[h] is proportional to omega*)
Px=Transpose[dist] . dist;(*calculate the projector Px on the leave, see [1] section 3.3.1 hybrid leaf using omega*)
vPrime=v-Px . v;(*compare to [1] section 3.3.7]*)
vPrime=delta*vPrime;(*calculates the step, see [1] equation (125); note that here we do not normalize vPrime*)
{vPrime,x+vPrime}
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{dist,_Real,2},{Px,_Real,2},{vPrime,_Real,1},{fx2[_],_Complex,2},{fx5[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgQMleafCurveIterationAdaptiveNullSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgQMleafCurveIterationAdaptiveNull
)
(**)
cqmgQMleafCurveIterationAdaptiveNullEXTR[out_]:=(
Dim=Length[cqmgQMleafCurveIterationAdaptiveNullSTAT[[1]]];(*=D*)
Nim=Length[cqmgQMleafCurveIterationAdaptiveNullSTAT[[1,1]]];(*=N*)
l=cqmgQMleafCurveIterationAdaptiveNullSTAT[[2]];(*=l*)
vPrime=out[[1]];
xNew=out[[2]];
{vPrime,xNew}
)


cqmgGQMleafCurveIterationAdaptiveNullINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
Id=IdentityMatrix[Nim]/2;(*prepare modified identity matrix for H*)
varA=Sum[Xnum[[a]] . Xnum[[a]],{a,1,Dim}]/2;(*prepare modified square sum over matrix configuration for H*)
fx2[input_]:=Developer`ToPackedArray[ArrayReshape[Eigensystem[input],{Nim+1,Nim}]];(*prepare function that calculates eigensystem of H*)
fx6[input1_,input2_]:=Developer`ToPackedArray[Orthogonalize[Flatten[{Re[#],Im[#]}&/@Eigenvectors[input1,l][[1;;l;;2]],1],Function[{u, v}, u . input2 . v]]];(*prepare function that calculates orthonormal basis of the distribution for given theta; [[1;;l;;2]] selects the even indices smaller or equal l; for these we calculate the real and imaginary part according to [1] section 3.3.1 hybrid leaf; finally the resulting basis is orthonormalized w.r.t. g*)
cqmgGQMleafCurveIterationAdaptiveNull=Compile[{{x,_Real,1},{v,_Real,1},{delta,_Real,0}},(*compile cqmgBasic, comparing to qmgBasic but for fixed X*)
Module[{mDim=Dim,mNim=Nim,mId=Id,mXnum=Xnum,mvarA=varA,a,b,H,EigFull,EigState,EigStateCon,HPseudoInverse,HPseudoInverseSquared,h,dist,Px,vPrime},(*create module for improved compilation*)
H=mvarA-Sum[x[[a]]*mXnum[[a]],{a,1,mDim}]+Norm[x]^2*mId;(*see [1] equation (33), using prepared quantities*)
EigFull=fx2[H];(*calculation of Eig using predefined function*)
EigState=Transpose[{EigFull[[mNim+1]]}];(*extract quasi-coherent state*)
EigStateCon=ConjugateTranspose[EigState];(*prepare complex conjugate of EigState*)
HPseudoInverse=Total[Table[(1/(EigFull[[1,a-1]]-EigFull[[1,mNim]]))*(Transpose[{EigFull[[a]]}] . Conjugate[{EigFull[[a]]}]),{a,2,mNim}]];(*see [1] equation (64), note that Mathematica sorts the eigenvalues descendingly*)
HPseudoInverseSquared=HPseudoInverse . HPseudoInverse;(*prepare square of HPseudoInverse*)
h=Table[Table[(EigStateCon . mXnum[[a]] . HPseudoInverseSquared . mXnum[[b]] . EigState)[[1,1]],{b,mDim}],{a,mDim}];(*calculates h as in qmgBasic, but in a compressed form*)
dist=fx6[Im[h],2*Re[h]];(*calculates an orthonormal basis of the distribution using predefined function, see [1] section 3.3.1 hybrid leaf using omega and g; note that Im[h] is proportional to omega*)
Px=Transpose[dist] . dist . (2* Re[h]);(*calculate the projector Px on the leave, see [1] section 3.3.1 hybrid leaf using omega*)
vPrime=v-Px . v;(*compare to [1] section 3.3.7]*)
vPrime=delta*vPrime;(*calculates the step, see [1] equation (125); note that here we do not normalize vPrime*)
{vPrime,x+vPrime}
],{{mDim,_Integer},{mNim,_Integer},{mId,_Complex,2},{mXnum,_Complex,3},{mvarA,_Complex,2},{a,_Integer},{b,_Integer},{H,_Complex,2},{EigFull,_Complex,2},{EigState,_Complex,2},{EigStateCon,_Complex,2},{HPseudoInverse,_Complex,2},{HPseudoInverseSquared,_Complex,2},{h,_Complex,2},{dist,_Real,2},{Px,_Real,2},{vPrime,_Real,1},{fx2[_],_Complex,2},{fx6[_],_Real,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgGQMleafCurveIterationAdaptiveNullSTAT={X,l};(*this variable indicates with which input qmgBasicOptimized has been compiled*)
cqmgGQMleafCurveIterationAdaptiveNull
)
(**)
cqmgGQMleafCurveIterationAdaptiveNullEXTR[out_]:=(
Dim=Length[cqmgGQMleafCurveIterationAdaptiveNullSTAT[[1]]];(*=D*)
Nim=Length[cqmgGQMleafCurveIterationAdaptiveNullSTAT[[1,1]]];(*=N*)
l=cqmgGQMleafCurveIterationAdaptiveNullSTAT[[2]];(*=l*)
vPrime=out[[1]];
xNew=out[[2]];
{vPrime,xNew}
)


cqmgMinimizeLambda[X_,x_,l_,leaf_:"TSleaf",delta_:1,nMax_:100,epsilon_:10^-8]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
cqmgCurveIterationAdaptiveNull=If[leaf=="TSleaf",(*pick the appropriate function depending on the chosen leaf*)
If[!ValueQ[cqmgTSleafCurveIterationAdaptiveNullSTAT],cqmgTSleafCurveIterationAdaptiveNullINIT[X,l],If[cqmgTSleafCurveIterationAdaptiveNullSTAT!={X,l},cqmgTSleafCurveIterationAdaptiveNullINIT[X,l],cqmgTSleafCurveIterationAdaptiveNull]],(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[leaf=="QMleaf",
If[!ValueQ[cqmgQMleafCurveIterationAdaptiveNullSTAT],cqmgQMleafCurveIterationAdaptiveNullINIT[X,l],If[cqmgQMleafCurveIterationAdaptiveNullSTAT!={X,l},cqmgQMleafCurveIterationAdaptiveNullINIT[X,l],cqmgQMleafCurveIterationAdaptiveNull]],(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[!ValueQ[cqmgGQMleafCurveIterationAdaptiveNullSTAT],cqmgGQMleafCurveIterationAdaptiveNullINIT[X,l],If[cqmgGQMleafCurveIterationAdaptiveNullSTAT!={X,l},cqmgGQMleafCurveIterationAdaptiveNullINIT[X,l],cqmgGQMleafCurveIterationAdaptiveNull]](*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
]];
If[!ValueQ[cqmgEmbeddingSTAT],cqmgEmbeddingINIT[X],If[cqmgEmbeddingSTAT!={X},cqmgEmbeddingINIT[X],cqmgEmbeddingNull]];(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
Successful=False;(*initialize success variable*)
xNew=x;(*initialize current position*)
For[i=1,i<=nMax,i++,((*begin gradient descent*)
xBoldNew=cqmgEmbedding[xNew][[2*Nim+2;;2*Nim+Dim+1]];(*calculate xBold*)
xGradNew=xBoldNew-xNew;(*calculate the gradient of lambda, see [1] equation (44)*)
{xGradNewInNull,xNewPrelim}=cqmgCurveIterationAdaptiveNull[xNew,xGradNew,delta];(*calculate step, see [1] section 3.3.7*)
If[Norm[xGradNewInNull]<epsilon,(Successful=True;Break[])];(*stop if the norm of the projected gradient is smaller than tolerance*)
xNew=xNewPrelim
)];
{xNew,Successful,i}
)


(* ::Section:: *)
(*Compiled Scan*)


cqmgScan[X_,x_,delta_,nPrime_,n_,m_,l_,leaf_:"TSleaf"]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
xss={x};(*initialize set of points*)
For[j=1,j<=nPrime,j++,((*iterate over the different curves, see [1] section 3.3.3*)
xNew=xss[[RandomInteger[{1,Length[xss]}]]];(*select a random point xNew out of the points calculated so far, see [1] section 3.3.3*)
distNew=cqmgDistribution[X,xNew,l,leaf];
vNew=Transpose[distNew] . RandomPoint[Ball[ConstantArray[0,l],1]];(*randomly choos an initial v in the distribution, see [1] section 3.3.3; this is done with respect to an orthonormal basis*)
xssNew=cqmgCurveIntegration[X,xNew,vNew,delta,n,m,l,leaf,False];(*integrate a new curve corresponding to the initial data, see [1] section 3.3.3*)
xssNew=Drop[xssNew,1];(*drop the initial point*)
xss=Join[xss,xssNew];(*add the new points to the old ones*)
)];
xsScan=xss;
xsScan
)


(* ::Section:: *)
(*Compiled Coordinates*)


cqmgCoordinates[X_,x_,delta_,n_,m_,l_,leaf_:"TSleaf",calculateJaccobians_:True,epsilon_:10^-8]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
dist=cqmgDistribution[X,x,l,leaf];(*calculate distribution at x*)
xss={x};(*initialize set of coordinate points*)
For[s=1,s<=l,s++,((*iterate over the directions*)
xssNewPlus=cqmgCurveIntegration[X,xss,dist[[s]],delta,n,m,l,leaf,True];(*parallely integrate curve in the direction s for all already calculated coordinate lines, see [1] section 3.3.4; here we use parallelization*)
xssNewMinus=cqmgCurveIntegration[X,xss,-dist[[s]],delta,n,m,l,leaf,True];(*parallely integrate curve in the -(direction s) for all already calculated coordinate lines, see [1] section 3.3.4; here we use parallelization*)
xss=Join[Flatten[Reverse[Transpose[xssNewMinus,1<->2],1],1],Flatten[Drop[Transpose[xssNewPlus,1<->2],1],1]];(*patch both of the latter together accordingly; the complicated structure is for bettter efficiency*)
)];
xssCoordinates=Transpose[ArrayReshape[xss,Join[Table[2*n+1,l],{Dim}]],Join[Table[l+1-s,{s,l}],{l+1}]];(*reshape in such a manner that the index structure fits to [1] section 3.3.4*)
If[calculateJaccobians,((*optionally calculate Jaccobians*)
xssDerivatives={};(*initialize set of derivatives*)
For[s=1,s<=l,s++,((*iterate over directions*)
xssNewPlus=cqmgCurveIntegration[X,xss,dist[[s]],epsilon*delta,1,0,l,leaf,True];(*calculate on infinitesimal step in direction s, see [1] section 3.3.4; here we use parallelization*)
xssDerivative=(Flatten[Drop[Transpose[xssNewPlus,1<->2],1],1]-xss)/epsilon;(*build differential quotient, see [1] section 3.3.4; the complicated structure is for bettter efficiency*)
xssDerivatives={xssDerivatives,xssDerivative};(*add to list; the complicated structure is for bettter efficiency*)
)];
xssDerivatives=Flatten[xssDerivatives,1];(*first reshape*)
xssJaccobians=Transpose[ArrayReshape[xssDerivatives,Join[{l},Table[2*n+1,l],{Dim}]],Join[{l+1},Table[l+1-c,{c,l}],{l+2}]];(*reshape in such a manner that the index structure fits to [1] section 3.3.4*)
Result={xssCoordinates,xssJaccobians}),
Result={xssCoordinates}
];
Result
)


(* ::Section:: *)
(*Compiled Integration*)


cqmgPointToolsINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
cqmgPointInTileQ=Compile[{{x,_Real,1},{Tile,_Real,2}},
Module[{mDim=Dim,Inside,a},(*create module for improved compilation*)
Inside=True;(*initialize Inside*)
For[a=1,a<=mDim,a++,Inside=(Inside&&(Tile[[a,1]]<=x[[a]]<Tile[[a,2]]))];(*x is in Tile if and only if every coordinate lies within the borders defined by the given tile, see [1] section 3.3.5*)
Inside
],{{mDim,_Integer},{a,_Integer},{Inside,True|False}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
(**)
cqmgPointsInTile[xs_,Tile_]:=Pick[xs,cqmgPointInTileQ[xs,Tile]];(*selects all points in xs that lie within the given tile; here we use parallelization*)
(**)
cqmgFindOptimalPointInTile[xsInTile_,Tile_]:=(
Module[{TileT,center,Deviations,NormDevitations,xOpt},
TileT=Transpose[Tile];(*intermediate step*)
center=(TileT[[1]]+TileT[[2]])/2;(*calculate the center of a given tile*)
Deviations=Transpose[Transpose[xsInTile]-center];(*calculate the deviation of each x in xsInTile and the center*)
NormDevitations=Map[Norm[#,Infinity]&,Deviations];(*calculate the infinity norm of each deviation*)
xOpt=xsInTile[[Position[NormDevitations,Min[NormDevitations]][[1,1]]]];(*choose the (first) point that minimizes the deviation, see [1] section 3.3.5*)
xOpt
]);
(**)
cqmgFilledTileQ[CoordinatesInTileQ_,Tile_]:=(
Module[{md=l,Filled,a,row},
Filled=True;(*initialize Filled*)
For[a=1,a<=md,a++,((*loop over all directions; the tile is filled if and only if in every direction all border points lie without the given tile, see [1] section 3.3.5*)
row=Flatten[Transpose[CoordinatesInTileQ,RotateRight[Range[md],a]][[{1,-1}]]];(*select all border points in the current direction*)
Filled=Filled&&AllTrue[row,(!#)&](*check if all border points in the current direction lie without the given tile*)
)];
Filled
]);
(**)
cqmgPointToolsSTAT={X,l};
{cqmgPointInTileQ,cqmgPointsInTile,cqmgFindOptimalPointInTile,cqmgFilledTileQ}
)


cqmgIntegrandsINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
cqmgIntegrands=Compile[{{Jaccobi,_Real,2},{EigState,_Complex,2},{MeasureForm,_Real,2},{xBold,_Real,1}},
Module[{mDim=Dim,mNim=Nim,a,Measure,Integrand},(*create module for improved compilation*)
Measure=Sqrt[Abs[Det[Jaccobi . MeasureForm . Transpose[Jaccobi]]]];(*calculate the squareroot of the determinant of the pullback of the MeasureForm, see [1] section 3.3.6*)
Integrand=Measure*(EigState . ConjugateTranspose[EigState]);(*calculate the integrand as the measure times the projector optained from the quasi-coherent state, see [1] section 3.3.6*)
Join[{Integrand},Table[xBold[[a]]*Integrand,{a,mDim}]](*the second part is the tensor product of xBold with the Integrand, see [1] section 3.3.6*)
],{{mDim,_Integer},{mNim,_Integer},{a,_Integer},{Measure,_Complex,2},{Integrand,_Complex,2}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgIntegrandsSTAT={X,l};
cqmgIntegrands
)
(**)
cqmgIntegrandsEXTR[out_]:=(
Dim=Length[cqmgIntegrandsSTAT[[1]]];(*=D*)
Nim=Length[cqmgIntegrandsSTAT[[1,1]]];(*=N*)
l=cqmgIntegrandsCost[[2]];(*=l*)
Integrand=out[[1]];
xBoldTensorIntegrand=out[[2;;Dim+1]];
{Integrand,xBoldTensorIntegrand}
)


cqmgIntegrateTile[X_,xCollection_,Tile_,delta_,n_,m_,l_,leaf_:"TSleaf",measure_:"omega",epsilon_:10^-8]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
cqmgBasicReduced=If[measure=="omega",(*pick the appropriate function depending on the chosen measure*)
If[!ValueQ[cqmgQMleafBasicSTAT],cqmgQMleafBasicINIT[X],If[cqmgQMleafBasicSTAT!={X},cqmgQMleafBasicINIT[X],cqmgQMleafBasic]](*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
,
If[!ValueQ[cqmgGQMleafBasicSTAT],cqmgGQMleafBasicINIT[X],If[cqmgGQMleafBasicSTAT!={X},cqmgGQMleafBasicINIT[X],cqmgGQMleafBasic]](*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
];
If[!ValueQ[cqmgPointToolsSTAT],cqmgPointToolsINIT[X,l],If[cqmgPointToolsSTAT!={X,l},cqmgPointToolsINIT[X,l],{cqmgPointInTileQ,cqmgPointsInTile,cqmgFindOptimalPointInTile,cqmgFilledTileQ}]];(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[!ValueQ[cqmgIntegrandsSTAT],cqmgIntegrandsINIT[X,l],If[cqmgIntegrandsSTAT!={X,l},cqmgIntegrandsINIT[X,l],cqmgIntegrands]];(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
xsInTile=cqmgPointsInTile[xCollection,Tile];(*select the points of xs that lie within the given tile, see [1] section 3.3.5*)
xsInTileNumber=Length[xsInTile];(*calculate the number of these points, see [1] section 3.3.5*)
If[xsInTileNumber>0,((*if non lie within calculations will be trivial, therefor we distinguish cases, see [1] section 3.3.5*)
TileNonempty=True;(*in this cas the given tile is not empty, see [1] section 3.3.5*)
xOptimal=cqmgFindOptimalPointInTile[xsInTile,Tile];(*find the optimally located point within the given tile, see [1] section 3.3.5*)
{xsCoordinates,xsJaccobians}=cqmgCoordinates[X,xOptimal,delta,n,m,l,leaf,True,epsilon];(*construct local coordinates, see [1] section 3.3.5*)
xsCoordinates=Flatten[xsCoordinates,l-1];(*reshape for better processability*)
xsJaccobians=Flatten[xsJaccobians,l-1];(*reshape for better processability*)
xsCoordinatesInTileQ=cqmgPointInTileQ[xsCoordinates,Tile];(*for each coordinate point check if it lies within the given tile, see [1] section 3.3.5; here we use parallelization*)
FilledTileQ=cqmgFilledTileQ[ArrayReshape[xsCoordinatesInTileQ,Table[2*n+1,l]],Tile];(*use the latter calculation to check if the given tile is filled by the constructed coordinates, see [1] section 3.3.5*)
xsCoordinatesInTile=Pick[xsCoordinates,xsCoordinatesInTileQ];(*we restrict ourselves to points within the given tile, see [1] section 3.3.5*)
xsJaccobiansInTile=Pick[xsJaccobians,xsCoordinatesInTileQ];(*we restrict ourselves to points within the given tile, see [1] section 3.3.5*)
PointsInTileNumber=Length[xsCoordinatesInTile];(*for practical reasons we count the number of coordinate points within the given tile*)
out=Transpose[cqmgBasicReduced[xsCoordinatesInTile]];(*calculate the relevant quantities for all coordinate points within the given tile; here we use parallelization*)
xsEigStatInTile=ArrayReshape[Transpose[out[[1;;Nim]]+I*out[[Nim+1;;2*Nim]]],{PointsInTileNumber,Nim,1}];(*extract the quasi-coherent states*)
xsMeasureFormInTile=ArrayReshape[Transpose[out[[2*Nim+1;;2*Nim+Dim^2]]],{PointsInTileNumber,Dim,Dim}];(*extract the omegas/thetas*)
xsxBoldInTile=ArrayReshape[Transpose[out[[2*Nim+Dim^2+1;;2*Nim+Dim^2+Dim]]],{PointsInTileNumber,Dim}];(*extract the xBolds*)
xsIntegrandInTile=cqmgIntegrands[xsJaccobiansInTile,xsEigStatInTile,xsMeasureFormInTile,xsxBoldInTile];(*calculates the corresponding integrands, see [1] section 3.3.6; here we use parallelization*)
JointIntegral=Total[xsIntegrandInTile,1];(*sum, see [1] section 3.3.6*)
CompletenessTile=JointIntegral[[1]];(*extract the completeness relation of the given tile, see [1] section 3.3.6*)
xBoldQuantizationTile=JointIntegral[[2;;Dim+1]](*extract the quantization of xBold over the given tile, see [1] section 3.3.6*)
),(
TileNonempty=False;(*the given tile is empty*)
FilledTileQ=True;(*for compatibility we set this to True eventhough it does not make sense conceptually*)
xsCoordinatesInTile={};(*therefore we can not calculate coordinate points*)
CompletenessTile=ConstantArray[0,{Nim,Nim}];(*for compatibility we set this to zero in the appropriate shape*)
xBoldQuantizationTile=ConstantArray[0,{Dim,Nim,Nim}](*for compatibility we set this to zero in the appropriate shape*)
)];
{{CompletenessTile,xBoldQuantizationTile},{xsCoordinatesInTile,xsEigStatInTile,xsIntegrandInTile},{TileNonempty,xsInTileNumber,FilledTileQ}}
)


cqmgIntegrateTiling[X_,xCollection_,Tiling_,delta_,n_,m_,l_,leaf_:"TSleaf",measure_:"omega",epsilon_:10^-8,parallelTable_:True]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
TilesNumber=Length[Tiling];(*calculate the number of given tiles*)
If[parallelTable,
out=ParallelTable[cqmgIntegrateTile[X,xCollection,Tiling[[alpha]],delta,n,m,l,leaf,measure,epsilon],{alpha,TilesNumber}],(*integrate over each given tile; here we use parallelization*)
out=Table[cqmgIntegrateTile[X,xCollection,Tiling[[alpha]],delta,n,m,l,leaf,measure,epsilon],{alpha,TilesNumber}](*integrate over each given tile*)
];
Completeness=Sum[out[[alpha,1,1]],{alpha,TilesNumber}];(*extract the overall completeness relation, see [1] section 3.3.6*)
xBoldQuantization=Sum[out[[alpha,1,2]],{alpha,TilesNumber}];(*extract the overall quantization of xBold, see [1] section 3.3.6*)
xsCoordinatesCollection=Table[out[[alpha,2,1]],{alpha,TilesNumber}];(*collect all coordinates that we used*)
NoTileEmpty=AllTrue[Table[out[[alpha,3,1]],{alpha,TilesNumber}],#&];(*checks if no given tile is empty*)
AllPointsInTile=(Sum[out[[alpha,3,2]],{alpha,TilesNumber}]==Length[xCollection]);(*checks if ever point of xs belongs to some given tile*)
AllTilesFilled=AllTrue[Table[out[[alpha,3,3]],{alpha,TilesNumber}],#&];(*checks if all given tiles are filled, empty given tiles are ignored as they are always set to true*)
{{Completeness,xBoldQuantization},xsCoordinatesCollection,{NoTileEmpty,AllPointsInTile,AllTilesFilled},out}
)


cqmgQuantization[X_,l_,outIntegrateTiling_]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
TilesNumber=Length[outIntegrateTiling];(*calculate the number of given tiles*)
{CompletenessPreliminary,xBoldQuantizationPreliminary}=outIntegrateTiling[[1]];(*extract CompletenessPreliminary and xBoldQuantizationPreliminary*)
Vol=Re[Tr[CompletenessPreliminary]];(*calculate the symplectic volume, see [1] equation (132)*)
alpha=(2Pi)^(l/2)/Vol;(*calculate alpha, see [1] equation (133)*)
Completeness=(Nim/Vol)*CompletenessPreliminary;(*use this to calculate Completeness, see [1] section 3.3.6*)
xBoldQuantization=(Nim/Vol)*xBoldQuantizationPreliminary;(*use this to calculate xBoldQuantization, see [1] section 3.3.6*)
nCorr=Norm[Flatten[X],2]/Norm[Flatten[xBoldQuantization],2];(*use this to calculate the correction factor, see [1] section 3.3.6*)
xBoldQuantizationCorr=nCorr*xBoldQuantization;(*use this to calculate the corrected xBoldQuantization, see [1] section 3.3.6*)
{{Vol,alpha,nCorr},{Completeness,xBoldQuantizationCorr}}
)


(* ::Section:: *)
(*Integration Preview*)


cqmgIntegrateTilePreview[X_,xCollection_,Tile_,delta_,n_,m_,l_,leaf_:"TSleaf"]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
If[!ValueQ[cqmgPointToolsSTAT],cqmgPointToolsINIT[X,l],If[cqmgPointToolsSTAT!={X,l},cqmgPointToolsINIT[X,l],{cqmgPointInTileQ,cqmgPointsInTile,cqmgFindOptimalPointInTile,cqmgFilledTileQ}]];(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
xsInTile=cqmgPointsInTile[xCollection,Tile];(*select the points of xs that lie within the given tile, see [1] section 3.3.5*)
xsInTileNumber=Length[xsInTile];(*calculate the number of these points, see [1] section 3.3.5*)
If[xsInTileNumber>0,((*if non lie within calculations will be trivial, therefor we distinguish cases, see [1] section 3.3.5*)
TileNonempty=True;(*in this cas the given tile is not empty, see [1] section 3.3.5*)
xOptimal=cqmgFindOptimalPointInTile[xsInTile,Tile];(*find the optimally located point within the given tile, see [1] section 3.3.5*)
{xsCoordinates}=cqmgCoordinates[X,xOptimal,delta,n,m,l,leaf,False];(*construct local coordinates, see [1] section 3.3.5*)
xsCoordinates=Flatten[xsCoordinates,l-1];(*reshape for better processability*)
xsCoordinatesInTileQ=cqmgPointInTileQ[xsCoordinates,Tile];(*for each coordinate point check if it lies within the given tile, see [1] section 3.3.5; here we use parallelization*)
FilledTileQ=cqmgFilledTileQ[ArrayReshape[xsCoordinatesInTileQ,Table[2*n+1,l]],Tile];(*use the latter calculation to check if the given tile is filled by the constructed coordinates, see [1] section 3.3.5*)
xsCoordinatesInTile=Pick[xsCoordinates,xsCoordinatesInTileQ];(*we restrict ourselves to points within the given tile, see [1] section 3.3.5*)
PointsInTileNumber=Length[xsCoordinatesInTile];(*for practical reasons we count the number of coordinate points within the given tile*)
),(
TileNonempty=False;(*the given tile is empty*)
FilledTileQ=True;(*for compatibility we set this to True eventhough it does not make sense conceptually*)
xsCoordinatesInTile={};(*therefore we can not calculate coordinate points*)
)];
{{},{xsCoordinatesInTile},{TileNonempty,xsInTileNumber,FilledTileQ}}
)


cqmgIntegrateTilingPreview[X_,xCollection_,Tiling_,delta_,n_,m_,l_,leaf_:"TSleaf",measure_:"omega",epsilon_:10^-8,parallelTable_:True]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
TilesNumber=Length[Tiling];(*calculate the number of given tiles*)
If[parallelTable,
out=ParallelTable[cqmgIntegrateTilePreview[X,xCollection,Tiling[[alpha]],delta,n,m,l,leaf],{alpha,TilesNumber}],(*integrate over each given tile; here we use parallelization*)
out=Table[cqmgIntegrateTilePreview[X,xCollection,Tiling[[alpha]],delta,n,m,l,leaf],{alpha,TilesNumber}](*integrate over each given tile*)
];
xsCoordinatesCollection=Table[out[[alpha,2,1]],{alpha,TilesNumber}];(*collect all coordinates that we used*)
NoTileEmpty=AllTrue[Table[out[[alpha,3,1]],{alpha,TilesNumber}],#&];(*checks if no given tile is empty*)
AllPointsInTile=(Sum[out[[alpha,3,2]],{alpha,TilesNumber}]==Length[xCollection]);(*checks if ever point of xs belongs to some given tile*)
AllTilesFilled=AllTrue[Table[out[[alpha,3,3]],{alpha,TilesNumber}],#&];(*checks if all given tiles are filled, empty given tiles are ignored as they are always set to true*)
{{},xsCoordinatesCollection,{NoTileEmpty,AllPointsInTile,AllTilesFilled},out}
)


(* ::Section:: *)
(*Integrate and Quantize Custom Function*)


cqmgCustomIntegrateTiling[X_,l_,outIntegrateTiling_,fxCustom_,parallelTable_:True]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
out=cqmgQuantization[X,l,outIntegrateTiling];
Vol=out[[1,1]];(*extract volume*)
TilesNumber=Length[outIntegrateTiling[[4]]];(*calculate the number of given tiles*)
If[parallelTable,
out=ParallelTable[(
{xsCoordinatesInTile,xsEigStatInTile,xsIntegrandInTile}=outIntegrateTiling[[4,alpha]];(*extract xsCoordinatesInTile, xsEigStatInTile and xsIntegrandInTile*)
xsIntegrandInTileReduced=Transpose[xsIntegrandInTile][[1]];(*reduce the integrand as the part with xBold is no longer interesting*)
xsEvaluationInTile=Map[fxCustom,xsEigStatInTile];(*calculate fxCustom at every point xsEigStatInTile*)
Total[Outer[TensorProduct,xsIntegrandInTileReduced,xsEvaluationInTile,1],1](*sum to integrate, while taking the tensorproduct of the integrand with the output of fxCustom at every point*)
),{alpha,TilesNumber}],(*integrate over each given tile; here we use parallelization*)
out=Table[(
{xsCoordinatesInTile,xsEigStatInTile,xsIntegrandInTile}=outIntegrateTiling[[4,alpha,2]];(*extract xsCoordinatesInTile, xsEigStatInTile and xsIntegrandInTile*)
xsIntegrandInTileReduced=Transpose[xsIntegrandInTile][[1]];(*reduce the integrand as the part with xBold is no longer interesting*)
xsEvaluationInTile=Map[fxCustom,xsEigStatInTile,1];(*calculate fxCustom at every point xsEigStatInTile*)
Total[Map[TensorProduct[#[[1]],#[[2]]]&,Transpose[{xsIntegrandInTileReduced,xsEvaluationInTile},1<->2],{1}],1](*sum to integrate, while taking the tensorproduct of the integrand with the output of fxCustom at every point*)
),{alpha,TilesNumber}](*integrate over each given tile*)
];
Integral=Sum[out[[alpha]],{alpha,TilesNumber}];(*ectract the overall integral*)
IntegralReduced=Tr[Integral,Plus,2];(*take the trace in order to remove the projector on the quasi-coherent state from the integral, compare to [1] equation (132)*)
Quantization=(Nim/Vol)*Integral;(*normalize the integral, see [1] equation (64) and equation (133)*)
{Integral,IntegralReduced,Quantization}
)


(* ::Section:: *)
(*Compiled K\[ADoubleDot]hler Cost*)


cqmgKaehlerCostINIT[X_,l_]:=(
Xnum=N[X];(*ensure that X is numerical*)
Dim=Length[Xnum];(*=D*)
Nim=Length[Xnum[[1]]];(*=N*)
J=N[ArrayFlatten[{{ConstantArray[0,{Nim,Nim}],IdentityMatrix[Nim]},{-1*IdentityMatrix[Nim],ConstantArray[0,{Nim,Nim}]}}]];(*defines J identifying C^N with R^(2N), see [1] section 3.3.1 K\[ADoubleDot]hler leaf*)
cqmgKaehlerCost=Compile[{{CovDer,_Complex,2},{W,_Real,2}},
Module[{md=l,mDim=Dim,mNim=Nim,mJ=J,a,b,CovDerReal,Vprelim,V,Vprime,c},(*create module for improved compilation*)
CovDerReal=Join[Re[Transpose[CovDer]],Im[Transpose[CovDer]]];(*identify C^N with R^(2N)*)
Vprelim=Transpose[CovDerReal . W];(*apply Txq to W, see [1] section 3.3.1 K\[ADoubleDot]hler leaf*)
V={Vprelim[[1]]};(*initialize Gram-Schmidt, see [1] section 3.3.1 K\[ADoubleDot]hler leaf*)
For[a=2,a<=md,a++,V=Join[V,{Vprelim[[a]]-Sum[(V[[b]] . Vprelim[[a]]/V[[b]] . V[[b]])*V[[b]],{b,a-1}]}]];(*Gram-Schmidt orthogonalize, see [1] section 3.3.1 K\[ADoubleDot]hler leaf*)
For[a=1,a<=md,a++,V[[a]]=V[[a]]/Sqrt[V[[a]] . V[[a]]]];(*Gram-Schmidt normalize, see [1] section 3.3.1 K\[ADoubleDot]hler leaf*)
Vprime=Transpose[mJ . Transpose[V]];(*calculate Vprime, see [1] section 3.3.1 K\[ADoubleDot]hler leaf*)
c=Sqrt[Norm[md-Sum[(V[[a]] . Vprime[[b]])^2,{a,md},{b,md}]]];(*calculate the K\[ADoubleDot]hler cost, see [1] section 3.3.1 K\[ADoubleDot]hler leaf i.e. equation (124)*)
c
],{{md,_Integer},{mDim,_Integer},{mNim,_Integer},{mJ,_Real,2},{a,_Integer},{b,_Integer},{CovDerReal,_Complex,2},{Vprelim,_Real,2},{V,_Real,2},{Vprime,_Real,2},{c,_Real}},(*specify used variables*)
CompilationOptions->{"InlineExternalDefinitions"->True},
RuntimeAttributes->{Listable}];(*allow for parallelization*)
cqmgKaehlerCostSTAT={X,l};(*this variable indicates with which input cqmgLightweight has been compiled*)
cqmgKaehlerCost
)
(**)
cqmgKaehlerCostEXTR[out_]:=(
Dim=Length[cqmgKaehlercqmgKaehlerCostSTAT[[1]]];(*=D*)
Nim=Length[cqmgKaehlercqmgKaehlerCostSTAT[[1,1]]];(*=N*)
l=cqmgKaehlerCost[[2]];(*=l*)
c=out;
c
)


cqmgKaehler[X_,x_,W_]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
l=Length[W[[1]]];(*=l*)
If[!ValueQ[cqmgKaehlerBasicSTAT],cqmgKaehlerBasicINIT[X],If[cqmgKaehlerBasicSTAT!={X},cqmgKaehlerBasicINIT[X],cqmgKaehlerBasic]];(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[!ValueQ[cqmgKaehlerCostSTAT],cqmgKaehlerCostINIT[X,l],If[cqmgKaehlerCostSTAT!={X,l},cqmgKaehlerCostINIT[X,l],cqmgKaehlerCost]];(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
If[!ValueQ[cqmgKaehlerSTAT],calculate=True,If[cqmgKaehlerSTAT!={X,x},calculate=True,calculate=False]];(*if not yet compiled calculate, if calculated for different input calculate, else reuse already calculated cqmgKaehlerBasic*)
If[calculate,((*calculate cqmgKaehlerBasic*)
out=cqmgKaehlerBasic[x];
CovDerK=ArrayReshape[out[[2*Nim+1;;2*Nim+Dim*Nim]],{Dim,Nim}]+I*ArrayReshape[out[[2*Nim+Dim*Nim+1;;2*Nim+2*Dim*Nim]],{Dim,Nim}];(*the K in CovDerK is there in order to prevent unaware overwrighting of the variable*)
)];
cqmgKaehlerSTAT={X,x};(*set status*)
c=cqmgKaehlerCost[CovDerK,W];(*calculate the K\[ADoubleDot]hler cost*)
c
)


cqmgKaehlerForLeaf[X_,x_,l_,leaf_:"TSleaf"]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
dist=cqmgDistribution[X,x,l,leaf];(*calculate the distribution*)
c=cqmgKaehler[X,x,Transpose[dist]];(*calculate the K\[ADoubleDot]hler cost*)
c
)


cqmgKaehlerForRandom[X_,x_,l_,n_]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
RandomSubspaces=RandomReal[{-1,1},{n,Dim,l}];(*choose random subspaces*)
cs=Table[cqmgKaehler[X,x,RandomSubspaces[[k]]],{k,n}];(*calculate the corresponding K\[ADoubleDot]hler costs*)
cAverage=Mean[cs];(*calculate the average*)
cDev=StandardDeviation[cs];(*calculate the standard deviation*)
cMin=Min[cs];
cMax=Max[cs];
{cAverage,cDev,cMin,cMax}
)


(* ::Section:: *)
(*Compiled Poisson Structures*)


cqmgComparePoissonStructures[X_,x_,tolerance_:10^-8]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
If[!ValueQ[cqmgBasicSTAT],cqmgBasicINIT[X],If[cqmgBasicSTAT!={X},cqmgBasicINIT[X],cqmgBasic]];(*if not yet compiled compile, if compiled for different input compile, else reuse already compiled version*)
out=cqmgBasic[x];(*calculate the basic quantites*)
h=ArrayReshape[out[[2*Nim+2;;2*Nim+Dim^2+1]],{Dim,Dim}]+I*ArrayReshape[out[[2*Nim+Dim^2+2;;2*Nim+2*Dim^2+1]],{Dim,Dim}];
omega=Re[I*(h-Transpose[h])];(*extract omega, see [1] equation (41) and comment 21*)
theta=ArrayReshape[out[[2*Nim+2*Dim^2+2;;2*Nim+3*Dim^2+1]],{Dim,Dim}];(*extract theta*)
xBoldPartial=ArrayReshape[out[[2*Nim+3*Dim^2+Dim+2;;2*Nim+4*Dim^2+Dim+1]],{Dim,Dim}];(*extract xBoldPartial*)
thetaInducedByomega=xBoldPartial . PseudoInverse[omega,Tolerance->tolerance] . xBoldPartial;(*calculate the Poissonstructure induced by omega, see [1] section 2.2.7 i.e. point 7*)
thetaInducedByomega=(thetaInducedByomega-Transpose[thetaInducedByomega])/2;
{theta,thetaInducedByomega}
)


(* ::Section:: *)
(*Compiled Quantization Validation*)


cqmgQuantizationValidation[X_,x_,l_,outIntegrateTiling_,n_,leaf_:"TSleaf",tolerance_:10^-8]:=(
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
{NoTileEmpty,AllPointsInTile,AllTilesFilled}=outIntegrateTiling[[3]];
{{Vol,alpha,nCorr},{Completeness,xBoldQuantizationCorr}}=cqmgQuantization[X,l,outIntegrateTiling];
{theta,thetaInducedByomega}=cqmgComparePoissonStructures[X,x,tolerance];
cLeaf=cqmgKaehlerForLeaf[X,x,l,leaf];
{cRandom,cRandomDev,cMin,cMax}=cqmgKaehlerForRandom[X,x,l,n];
dCompleteness=N[Norm[Flatten[Completeness-IdentityMatrix[Nim]],2]]/N[Norm[Flatten[IdentityMatrix[Nim]],2]]/Sqrt[Nim];
dxBoldQuantizationCorr=N[Norm[Flatten[xBoldQuantizationCorr-X],2]]/N[Norm[Flatten[X],2]]/Sqrt[Nim];
dPoisson=Norm[Flatten[thetaInducedByomega-theta],2]/Norm[Flatten[theta],2];
{Vol,dCompleteness,dxBoldQuantizationCorr,nCorr,dPoisson,cLeaf,cRandom,cRandomDev,cMin,cMax,NoTileEmpty,AllPointsInTile,AllTilesFilled}
)


cqmgQuantizationValidationPresent[Xxs_,tolerance_:10^-8]:=(
heading={"Name","D","N","l","x","|x|","SymVol","dComp","dxBold","nCorr","dPoisson","cLeaf","cRandom","cRandomDev","cMin","cMax","NoTileEmpty","AllPointsInTile","AllTilesFilled"};(*prepare heading for table*)
data={heading};(*initialize*)
For[i=1,i<=Length[Xxs],i++,((*loop over all sets*)
X=Xxs[[i,1]];(*extract X*)
x=Xxs[[i,2]];(*extract x*)
Name=Xxs[[i,3]];(*extract name*)
Dim=Length[X];(*=D*)
Nim=Length[X[[1]]];(*=N*)
l=Xxs[[i,4]];
outIntegrateTiling=Xxs[[i,5]];
n=Xxs[[i,6]];
leaf=Xxs[[i,7]];
{Vol,dCompleteness,dxBoldQuantizationCorr,nCorr,dPoisson,cLeaf,cRandom,cRandomDev,cMin,cMax,NoTileEmpty,AllPointsInTile,AllTilesFilled}=cqmgQuantizationValidation[X,x,l,outIntegrateTiling,n,leaf,10^-8];
AppendTo[data,{Name,Dim,Nim,l,MatrixForm[{N[Round[x,0.01]]}],N[Norm[x]],Vol,dCompleteness,dxBoldQuantizationCorr,nCorr,dPoisson,cLeaf,cRandom,cRandomDev,cMin,cMax,NoTileEmpty,AllPointsInTile,AllTilesFilled}];(*add data to table*)
)];
table=Grid[data,Alignment->Left,Spacings->{2,1},Frame->All,ItemStyle->"Text",Background->{{LightOrange,White,White,White,LightGray,LightGray,White,White,White,White,LightGray,White,White,White,White,White,LightGray,LightGray,LightGray},{Orange,None}}];(*generate table*)
table
)


(* ::Section:: *)
(*End*)


End[];


EndPackage[];
