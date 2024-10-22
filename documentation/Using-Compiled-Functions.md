## Why Compile Functions?

Often, compilde functions are much faster as some computational overhead can be omitted.
This comes to the price that their handling can become more complicated.
Therefore, here the concepts and best practice are explained.

If we work with compiled functions, we usually have a family of three elements
dummyINIT, dummy and dummyEXTR. If we mean the whole faimily, we write dummy*.

## The dummyINIT Function

Before we can use a compiled function (for example dummy), we have to compile it.
Here, this is always done with the function dummyINIT.

Let's look at an example:
```mathematica
X=qmgXsu2[4];
cqmgLightweight=cqmgLightweightINIT[X]
```


## The dummy Function

Having compilled dummy, we can use it as a normal function as in:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
cqmgLightweight=cqmgLightweightINIT[X];
cqmgLightweight[x]
```

## The dummyEXTR Function

Yet, the output of dummy is most likely in a compressed form.
To solve that, we can use dummyEXTR:
```mathematica
X=qmgXsu2[4];
x={0,0,1};
cqmgLightweight=cqmgLightweightINIT[X];
out=cqmgLightweight[x];
cqmgLightweightEXTR[out]
```

## Special Cases

There are functions that lack a dummyEXTR function. This is usually the case when dummy is made for internal use.

Further, a complete special case is the function cqmgPointToolsINIT. It does not return cqmgPointTools but rather {cqmgPointInTileQ,cqmgPointsInTile,cqmgFindOptimalPointInTile,cqmgFilledTileQ} where cqmgPointInTileQ is compiled and cqmgPointsInTile, cqmgFindOptimalPointInTile and cqmgFilledTileQ are built on the latter.

Yet not all functions beginning with cqmg have to be compiled. Some are simply built on compiled functions and do the job on their own as for example cqmgPlot. These can interfere with manual compilation if one chooses different inputs, so it is good practice to always compile after changing the input.
