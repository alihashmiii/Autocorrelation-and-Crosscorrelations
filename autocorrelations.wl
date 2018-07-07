
(* autocorrelation for a 1D vector *)
autoCorrelation1D[vector_]:= CorrelationFunction[vector,{Length@vector - 1}];
