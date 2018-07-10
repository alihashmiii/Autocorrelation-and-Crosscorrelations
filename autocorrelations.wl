
(* autocorrelation for a 1D vector *)
autoCorrelation1D[vector_]:= CorrelationFunction[vector,{Length@vector - 1}];

(* generating Gaussian Images *)
ClearAll@gaussianImage;
Options[gaussianImage] = {"intensity" -> "random"};
gaussianImage[pts : {{_Real, _Real} ..}, num_: 50, boundX_Integer: 100, boundY_Integer: 1024, sX_: 5.1, sY_: 5.1, 
  OptionsPattern[]] := Block[{sx = sX, sy = sY, maxI, mx, my, func},
   If[OptionValue["intensity"] === "random", func := RandomReal[], func = OptionValue@"intensity"];
   ParallelTable[
    maxI = func;
    {mx, my} = {First@#, Last@#} &@pts[[i]];
    Table[(maxI*Exp[-((x - mx)^2/(2 sx^2)) - ((y - my)^2/(2*sy^2))]), {x, 1, boundX}, {y, 1, boundY}],{i, num}]
   ];
   
(* 
computing 2D auto/cross-correlations using ImageCorrelate (output is the same dimension as the input) and ListCorrelate (output size is
larger than the input) 
*)

(* different methods can be used for ImageCorrelate: Dot, EuclideanDistance, SquaredEuclideanDistance, NormalizedSquaredEuclideanDistance,
CosineDistance etc. *)

imageCorrelation[input_Image]:= ImageAdjust@ImageCorrelate[input,input,PerformanceGoal -> "Quality"];

autoCorr2D[input:{{_?NumberQ, _?NumberQ}..} | _Image]:= Module[{mat},
mat = If[Head@input === Image, ImageData@input, input];
ListCorrelate[mat,mat,{-1,1},0]
];

