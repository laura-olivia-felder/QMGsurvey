(* ::Package:: *)

(*
   Copyright 2023 Laurin J. Felder

   This work is licensed under the Creative Commons Attribution 4.0 International License.
   To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
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


BeginPackage["QMGxW`"];


qmgXsu2::usage="Generates the matrix configuration of the fuzzy sphere.";


qmgXsu3::usage="Generates the matrix configuration of the fuzzy CP2.";


qmgXcs::usage="Generates the matrix configuration of the fuzzy torus.";


qmgXrand::usage="SeedRandom in advance recommended; generates a random matrix configuration.";


qmgxCartesianCoordinates::usage="Generates Cartesian coordinate lines in target space.";


qmgx3DSphericalCoordinates::usage="Generates three dimensional spherical coordinate lines in target space.";


qmgx3DSphericalCoordinatesSector::usage="Generates a sector of three dimensional spherical coordinate lines in target space.";


qmgxSplitHemispheres::usage="Splits given points in target space into the upper and lower hemisphere.";


qmgxRandomBall::usage="SeedRandom in advance recommend; generates random points in a ball in target space.";


qmgxRandomCube::usage="SeedRandom in advance recommend; generates random points in a cube in target space.";


Begin["`Private`"];


(* ::Section:: *)
(*Matrix Configurations*)


(* ::Subsection:: *)
(*SU(2)*)


qmgXsu2[Nim_,Normalized_:True]:=(
j=(Nim-1)/2;
CN=Sqrt[1/4*(Nim^2-1)];
J3=DiagonalMatrix[Table[k,{k,-j,j}],0];
Jp=DiagonalMatrix[Table[Sqrt[j*(j+1)-k*(k+1)],{k,-j,j-1}],-1];
Jm=DiagonalMatrix[Table[Sqrt[j*(j+1)-k*(k-1)],{k,-j+1,j}],1];
J1=1/2*(Jp+Jm);
J2=1/(2I)*(Jp-Jm);
X=1/CN*{J1,J2,J3};
If[Normalized,X,{J1,J2,J3}]
)


(* ::Subsection:: *)
(*SU(3)*)


(*
The following function has been derived from https://arxiv.org/abs/0908.3864 by Richard Shurtleff, licensed under CC-BY-4.0 (see https://creativecommons.org/licenses/by/4.0/legalcode.txt).
*)


qmgXsu3[Pin_,Qin_,Normalized_:True]:=(
p=Pin;
q=Qin;
Cpq=(p^2+q^2+3p+3q+p*q)/3;
If[p>=q,pqOK=True,pqOK=False];
If[pqOK,"sweet",p0=p];
If[pqOK,"sweet",q0=q];
If[pqOK,"sweet",p=q0];
If[pqOK,"sweet",q=p0];
numTspins=(p+1)(q+1);
dimREP=1/2 (p+1)(q+1)(p+q+2);
n0[q_]:=n0[q]=(q(q-1))/2+1;
mod[n_,q_]:=mod[n,q]=Mod[n-n0[q],q+1];
floor[n_,q_]:=floor[n,q]=Floor[(n-n0[q])/(q+1)];
Unprotect[Up];
rp[s_,\[Sigma]_]:= rp[s,\[Sigma]]=Sqrt[(s-\[Sigma])(s+\[Sigma]+1)];
rm[s_,\[Sigma]_]:= rm[s,\[Sigma]]=Sqrt[(s+\[Sigma])(s-\[Sigma]+1)];
Tspins=1/2 Flatten[{Table[s2-1,{s2,q},{m,s2}],Table[s2,{s2,q,p},{m,q+1}],Table[s2,{s2,p+1,p+q},{m,q+1-s2+p}]}];
Y3s=Flatten[{Table[-2(p-q)-3( i-1)+6 (j-1),{i,q},{j,i}],Table[Table[-2 p-q+3 i+6 j,{j,0,q}],{i,0,p-q}],
Table[3+p-4 q+3(i-1)+6(j-1),{i,q},{j,q-i+1}]}];
U3firsts=1/2 Flatten[{Table[-(p-q)-2(i-1)+3(j-1),{i,q},{j,i}],Table[Table[(-p-q+i+3j),{j,0,q}],{i,0,p-q}],
Table[(1-2q+(i-1)+3(j-1)),{i,q},{j,q-i+1}]}];
Tplus[s_]:=Tplus[s]=Table[If[\[Sigma]==\[Sigma]1+1,rp[s,\[Sigma]1],0,ah],{\[Sigma],s,-s,-1},{\[Sigma]1,s,-s,-1}];
Tminus[s_]:=Tminus[s]=Table[If[\[Sigma]==\[Sigma]1-1,rm[s,\[Sigma]1],0,ah],{\[Sigma],s,-s,-1},{\[Sigma]1,s,-s,-1}];
Tthree[s_]:=Tthree[s]=Table[If[\[Sigma]==\[Sigma]1,\[Sigma],0,ah],{\[Sigma],s,-s,-1},{\[Sigma]1,s,-s,-1}];
Tp=Table[0,{i,dimREP},{j,dimREP}];
For[s=1,s<=numTspins,s++,Table[Tp[[i+Sum[2Tspins[[si]]+1,{si,s-1}],j+Sum[2Tspins[[si]]+1,{si,s-1}]]]=Tplus[Tspins[[s]]][[i,j]],{i,2Tspins[[s]]+1},{j,2Tspins[[s]]+1}]];
Tm=Table[0,{i,dimREP},{j,dimREP}];
For[s=1,s<=numTspins,s++,Table[Tm[[i+Sum[2Tspins[[si]]+1,{si,s-1}],j+Sum[2Tspins[[si]]+1,{si,s-1}]]]=Tminus[Tspins[[s]]][[i,j]],{i,2Tspins[[s]]+1},{j,2Tspins[[s]]+1}]];
T3=Table[0,{i,dimREP},{j,dimREP}];
For[s=1,s<=numTspins,s++,Table[T3[[i+Sum[2Tspins[[si]]+1,{si,s-1}],j+Sum[2Tspins[[si]]+1,{si,s-1}]]]=Tthree[Tspins[[s]]][[i,j]],{i,2Tspins[[s]]+1},{j,2Tspins[[s]]+1}]];
U3=Table[0,{i,dimREP},{j,dimREP}];
For[s=1,s<=numTspins,s++,Table[U3[[i+Sum[2Tspins[[si]]+1,{si,s-1}],i+Sum[2Tspins[[si]]+1,{si,s-1}]]]=U3firsts[[s]]+1/2 (i-1),{i,2Tspins[[s]]+1}]];
uppertopcap=Table[Table[{upc2[i,i+q1]->(n0[q1+1]-i)/(q1+1) (p+(n0[q1+1]-i)+1)(q-(n0[q1+1]-i)+1)},{i,n0[q1],n0[q1+1]}],{q1,q-1}];
lowertopcap=Table[Table[{upc2[j+q1+1,j]->((j-n0[q1])+1)/(q1(q1+1)) (p-(j-n0[q1]))(q+(j-n0[q1])+2)},{j,n0[q1],n0[q1+1]-1}],{q1,q-1}];
n0rowlowertopcap=Table[Table[upc2[n0[q1],m]->0,{m,n0[q1]}],{q1,q+1}];
upperdiagonal=Table[{upc2[i,i+q]->((q-mod[i,q])/(q+1+floor[i,q]) (p+q+1-mod[i,q])(1+mod[i,q]))},{i,n0[q],numTspins-(q(q+1))/2}];
(lowerdiagonal[p_,q_]:=Flatten[{Table[{upc2[j+q+1,j]->-1+upc2[j,j-q+If[j<n0[q+1],0,-1,jah]]+1/(2Tspins[[j]]) upc2[j-q+1+If[j<n0[q+1],0,-1,jah],j]-1/(2Tspins[[j]]+1) upc2[j,j+q]},{j,n0[q]+If[q==1,1,0,ahk],n0[q]+(p-q+2)(q+1)-3}]}]/;q>=1;lowerdiagonal[p_,0]:=Flatten[{Table[{upc2[j+1,j]->p-j+1},{j,1,p}]}];);
upperbottomcap=Table[Table[{upc2[i,i+q1]->((n0[q1+1]+i-numTspins+q1-1)(q-q1-n0[q1+1]+numTspins+2-i)(p+q-q1-n0[q1+1]+numTspins+3-i))/(p+q-q1+2)},{i,numTspins-n0[q1+1]-q1+1,numTspins-n0[q1]-q1+1}],{q1,q-1}];
lowerbottomcap=Table[Table[{upc2[j+q1+1,j]->((+numTspins-q1-n0[q1]+1-j)(p-numTspins+q1+n0[q1]+j)(p+q+j-numTspins+q1+n0[q1]+1))/((p+q-q1+1)(p+q-q1+2))},{j,numTspins-n0[q1+1]+1-q1,numTspins-n0[q1]-q1}],{q1,q-1}];
miscSUBS={upc2[a_,0]->0,If[q==1,upc2[3,1]->1/2 (p)(q+2),upc2[0,b_]->0]};
upc2SUBS=Flatten[{uppertopcap,lowertopcap,upperdiagonal,lowerdiagonal[p,q],n0rowlowertopcap,upperbottomcap,lowerbottomcap,miscSUBS}];
uplus[s_,t_,\[Sigma]_]:=uplus[s,t,\[Sigma]]=If[s>t,up[s,t] Sqrt[s-\[Sigma]],If[s<t,up[s,t]Sqrt[ ((1+s+\[Sigma])/(1+2 s))],ahu],huhu];
Upa=Table[0,{i,dimREP},{j,dimREP}];
Uplus[s_,t_]:=Uplus[s,t]=Table[If[\[Sigma]==\[Rho]-1/2,uplus[s,t,\[Sigma]],0,ah],{\[Sigma],s,-s,-1},{\[Rho],t,-t,-1}];
For[s=1,s<=numTspins,s++,For[t=1,t<=numTspins,t++,If[(Tspins[[t]]==Tspins[[s]]+1/2)&&(U3firsts[[t]]-U3firsts[[s]]==-1),Table[Upa[[i+Sum[2Tspins[[si]]+1,{si,s-1}],j+Sum[2Tspins[[ti]]+1,{ti,t-1}]]]=Uplus[Tspins[[s]],Tspins[[t]]][[i,j]]/.{up[Tspins[[s]],Tspins[[t]]]->upc[s,t]},{i,2Tspins[[s]]+1},{j,2Tspins[[t]]+1}]]]];
For[s=1,s<=numTspins,s++,For[t=1,t<=numTspins,t++,If[(Tspins[[t]]==Tspins[[s]]-1/2)&&(U3firsts[[s]]-U3firsts[[t]]==1/2),Table[Upa[[i+Sum[2Tspins[[si]]+1,{si,s-1}],j+Sum[2Tspins[[ti]]+1,{ti,t-1}]]]=Uplus[Tspins[[s]],Tspins[[t]]][[i,j]]/.{up[Tspins[[s]],Tspins[[t]]]->upc[s,t]},{i,2Tspins[[s]]+1},{j,2Tspins[[t]]+1}]]]];
VarUpa=Variables[Upa];
upcSUBS=Flatten[{Table[VarUpa[[i]]->\[Sqrt]((VarUpa[[i]]^2/.Flatten[Table[upc[i,j]^2->upc2[i,j],{i,numTspins},{j,numTspins}]])//.upc2SUBS),{i,Length[VarUpa]}]}];
Up=Upa//.upcSUBS;
vplus[s_,t_,\[Sigma]_]:=vplus[s,t,\[Sigma]]=If[s>t,up[s,t]Sqrt[s+\[Sigma]],If[s<t,- up[s,t] Sqrt[(1+s-\[Sigma])/(1+2 s)],ahv],huhv];
Vplus[s_,t_]:=Vplus[s,t]=Table[If[\[Sigma]==\[Rho]+1/2,vplus[s,t,\[Sigma]],0,ah],{\[Sigma],s,-s,-1},{\[Rho],t,-t,-1}];
Vpa=Table[0,{i,dimREP},{j,dimREP}];
For[s=1,s<=numTspins,s++,For[t=1,t<=numTspins,t++,If[(Tspins[[t]]==Tspins[[s]]+1/2)&&(U3firsts[[t]]-U3firsts[[s]]==-1),Table[Vpa[[i+Sum[2Tspins[[si]]+1,{si,s-1}],j+Sum[2Tspins[[ti]]+1,{ti,t-1}]]]=Vplus[Tspins[[s]],Tspins[[t]]][[i,j]]/.{up[Tspins[[s]],Tspins[[t]]]->upc[s,t]},{i,2Tspins[[s]]+1},{j,2Tspins[[t]]+1}]]]];
For[s=1,s<=numTspins,s++,For[t=1,t<=numTspins,t++,If[(Tspins[[t]]==Tspins[[s]]-1/2)&&(U3firsts[[s]]-U3firsts[[t]]==1/2),Table[Vpa[[i+Sum[2Tspins[[si]]+1,{si,s-1}],j+Sum[2Tspins[[ti]]+1,{ti,t-1}]]]=Vplus[Tspins[[s]],Tspins[[t]]][[i,j]]/.{up[Tspins[[s]],Tspins[[t]]]->upc[s,t]},{i,2Tspins[[s]]+1},{j,2Tspins[[t]]+1}]]]];
Vp=Vpa//.upcSUBS;
Um=Table[0,{i,dimREP},{j,dimREP}];Table[Um[[i,j]]=Up[[j,i]],{i,dimREP},{j,dimREP}];Vm=Table[0,{i,dimREP},{j,dimREP}];Table[Vm[[i,j]]=Vp[[j,i]],{i,dimREP},{j,dimREP}];
If[pqOK,"sweet",Tp=-Transpose[Tp]];
If[pqOK,"sweet",Tm=-Transpose[Tm]];
If[pqOK,"sweet",T3=-Transpose[T3]];
If[pqOK,"sweet",Up=-Transpose[Up]];
If[pqOK,"sweet",Um=-Transpose[Um]];
If[pqOK,"sweet",U3=-Transpose[U3]];
If[pqOK,"sweet",Vp=-Transpose[Vp]];
If[pqOK,"sweet",Vm=-Transpose[Vm]];
If[pqOK,"sweet",p=p0];
If[pqOK,"sweet",q=q0];
Fi={1/2 (Tp+Tm),-I/2 (Tp-Tm),T3,1/2 (Vp+Vm),-I/2 (Vp-Vm),1/2 (Up+Um),-I/2 (Up-Um),Sqrt[4/3]U3+1/Sqrt[3] T3};
X=Fi/Sqrt[Cpq];
If[Normalized,X,Fi]
)


(* ::Subsection:: *)
(*Clock & Shift Matrices*)


qmgXcs[Nim_]:=(
q=Exp[2*Pi*I/Nim];
U=DiagonalMatrix[ConstantArray[1,Nim-1],1];
U[[Nim,1]]=1;
V=DiagonalMatrix[Table[q^(a-1),{a,Nim}]];
X={1/2(U+ConjugateTranspose[U]),1/(2I)(U-ConjugateTranspose[U]),1/2(V+ConjugateTranspose[V]),1/(2I)(V-ConjugateTranspose[V])};
X
)


(* ::Subsection:: *)
(*Random*)


qmgXrand[Dim_,Nim_,componentMax_]:=(
raw=RandomComplex[{-componentMax-componentMax*I,componentMax+componentMax*I},{Dim,Nim,Nim}];
X=Table[1/2(raw[[l]]+ConjugateTranspose[raw[[l]]]),{l,1,Dim}]
)


(* ::Section:: *)
(*Sets in Target Space*)


(* ::Subsection:: *)
(*Cartesian Coordinates*)


qmgxCartesianCoordinates[Dim_,directionP_,directionsT_,lengthP_,lengthT_,nP_,nT_,center_:{}]:=(
If[center=={},centerInternal=ConstantArray[0,Dim],centerInternal=center];
xs={centerInternal};
directions=Join[{directionP},directionsT];
For[a=1,a<=Length[directions],a++,(
If[directions[[a]]==directionP,xs=Table[Transpose[xs]+b*lengthP/nP*UnitVector[Dim,directions[[a]]],{b,-nP,nP}],xs=Table[Transpose[xs]+b*lengthT/nT*UnitVector[Dim,directions[[a]]],{b,-nT,nT}]];
xs=Flatten[Transpose[xs,2<->3],1];
)];
xs
)


(* ::Subsection:: *)
(*Spherical Coordinates*)


qmgx3DSphericalCoordinates[rMin_,rMax_,nr_,ntheta_,nphi_]:=(
xs=Flatten[Table[N[(rMin+r*(rMax-rMin)/nr)*{Sin[theta*Pi/ntheta]*Sin[phi*2*Pi/nphi],Sin[theta*Pi/ntheta]*Cos[phi*2*Pi/nphi],Cos[theta*Pi/ntheta]}],{r,0,nr},{theta,0,ntheta},{phi,1,nphi}],2]
)


qmgx3DSphericalCoordinatesSector[rBord_:{0.5,1.5},thetaBord_:{0.2Pi,0.8Pi},phiBord_:{-0.015Pi,0.015Pi},nr_,ntheta_,nphi_]:=(
Flatten[Table[N[(rBord[[1]]+r*(rBord[[2]]-rBord[[1]])/nr)*{Sin[(thetaBord[[1]]+theta*(thetaBord[[2]]-thetaBord[[1]])/ntheta)]*Sin[(phiBord[[1]]+phi*(phiBord[[2]]-phiBord[[1]])/nphi)],Sin[(thetaBord[[1]]+theta*(thetaBord[[2]]-thetaBord[[1]])/ntheta)]*Cos[(phiBord[[1]]+phi*(phiBord[[2]]-phiBord[[1]])/nphi)],Cos[(thetaBord[[1]]+theta*(thetaBord[[2]]-thetaBord[[1]])/ntheta)]}],{r,0,nr},{theta,0,ntheta},{phi,0,nphi}],2]
)


qmgxSplitHemispheres[xs_]:=(
xsUpper={};
xsLower={};
For[k=1,k<=Length[xs],k++,If[xs[[k,-1]]>0,AppendTo[xsUpper,xs[[k]]],AppendTo[xsLower,xs[[k]]]]];
{xsUpper,xsLower})


(* ::Subsection:: *)
(*Random Ball*)


qmgxRandomBall[Dim_,radius_,n_,center_:{}]:=(
If[center=={},centerInternal=ConstantArray[0,Dim],centerInternal=center];
xs=RandomPoint[Ball[centerInternal,radius],n];
xs
)


(* ::Subsection:: *)
(*Random Cube*)


qmgxRandomCube[Dim_,length_,n_,center_:{}]:=(
If[center=={},centerInternal=ConstantArray[0,Dim],centerInternal=center];
xs=RandomPoint[Cuboid[Table[centerInternal[[i]]-length,{i,Length[centerInternal]}],Table[centerInternal[[i]]+length,{i,Length[centerInternal]}]],n];
xs
)


(* ::Section:: *)
(*End*)


End[];


EndPackage[];
